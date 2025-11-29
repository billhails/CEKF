/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2024  Bill Hails
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <math.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include "common.h"
#include "value.h"
#include "cekf.h"
#include "cekfs.h"
#include "utf8.h"
#include "symbol.h"
#include "builtin_io.h"
#include "tc_analyze.h"

#ifdef DEBUG_IO
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static void registerPutc(BuiltIns *registry);
static void registerFPutc(BuiltIns *registry);
static void registerGetc(BuiltIns *registry);
static void registerFGetc(BuiltIns *registry);

static void registerPutn(BuiltIns *registry);
static void registerFPutn(BuiltIns *registry);

static void registerPutv(BuiltIns *registry);
static void registerFPutv(BuiltIns *registry);

static void registerPuts(BuiltIns *registry);
static void registerFPuts(BuiltIns *registry);
static void registerGets(BuiltIns *registry);
static void registerFGets(BuiltIns *registry);

static void registerOpen(BuiltIns *registry);
static void registerOpenMemstream(BuiltIns *registry);
static void registerClose(BuiltIns *registry);

static void registerOpenDir(BuiltIns *registry);
static void registerReadDir(BuiltIns *registry);
static void registerCloseDir(BuiltIns *registry);

static void registerFType(BuiltIns *registry);

static BuiltInMemBufHash *memBufs = NULL;

void markMemBufs() {
    if (memBufs != NULL) {
        markHashTable((HashTable *) memBufs);
    }
}

static BuiltInMemBufHash *getMemBufs(void) {
    if (memBufs == NULL) {
        memBufs = newBuiltInMemBufHash();
    }
    return memBufs;
}

void registerIO(BuiltIns *registry) {
    registerPutc(registry);
    registerFPutc(registry);
    registerGetc(registry);
    registerFGetc(registry);
    registerPutn(registry);
    registerFPutn(registry);
    registerPutv(registry);
    registerFPutv(registry);
    registerPuts(registry);
    registerFPuts(registry);
    registerGets(registry);
    registerFGets(registry);
    registerOpen(registry);
    registerOpenMemstream(registry);
    registerClose(registry);
    registerOpenDir(registry);
    registerReadDir(registry);
    registerCloseDir(registry);
    registerFType(registry);
}

static HashSymbol *fileSymbol(void) {
    return newSymbol("file");
}

static HashSymbol *dirSymbol(void) {
    return newSymbol("dir");
}

static TcType *makeFileType(void) {
    return newTcType_Opaque(fileSymbol());
}

static TcType *makeDirType(void) {
    return newTcType_Opaque(dirSymbol());
}

static Value errnoToTry() {
    Value errMsg = utf8ToList(strerror(errno));
    int save = protectValue(errMsg);
    Value result = makeTryResult(0, errMsg);
    UNPROTECT(save);
    return result;
}

static void private_fputc(FILE *fh, Character character) {
    char buf[16];
    char *end = (char *) writeChar((unsigned char *)buf, character);
    *end = 0;
    fprintf(fh, "%s", buf);
}

static Value builtin_putc(Vec *args) {
#ifdef SAFETY_CHECKS
    if (args->entries[0].type != VALUE_TYPE_CHARACTER) {
        cant_happen("unexpected %s", valueTypeName(args->entries[0].type));
    }
#endif
    private_fputc(stdout, args->entries[0].val.character);
    return args->entries[0];
}

static Value builtin_fputc(Vec *args) {
#ifdef SAFETY_CHECKS
    if (args->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(args->entries[0].type));
    }
#endif
    Opaque *data = args->entries[0].val.opaque;
    if (data == NULL || data->data == NULL) {
        cant_happen("fputc to closed file handle");
    }
    private_fputc(data->data, args->entries[1].val.character);
    return args->entries[1];
}

static Value builtin_fputs(Vec *args) {
    Opaque *data = args->entries[0].val.opaque;
    char * buf = listToUtf8(args->entries[1]);
    fprintf((FILE *) data->data, "%s", buf);
    FREE_ARRAY(char, buf, strlen(buf) + 1);
    return args->entries[1];
}

static Value builtin_puts(Vec *args) {
    char * buf = listToUtf8(args->entries[0]);
    printf("%s", buf);
    FREE_ARRAY(char, buf, strlen(buf) + 1);
    return args->entries[0];
}

#define IO_MODE_READ 0
#define IO_MODE_WRITE 1
#define IO_MODE_APPEND 2

static HashSymbol *fileHandleToKey(FILE *file) {
    static char buf[128];
    sprintf(buf, "%p", file);
    return newSymbol(buf);
}

static void opaque_io_close(Opaque *data) {
    if (data == NULL) return;
    if (data->data == NULL) return;
    fclose(data->data);
    HashSymbol *key = fileHandleToKey(data->data);
    BuiltInMemBuf *memBuf = NULL;
    if (getBuiltInMemBufHash(getMemBufs(), key, &memBuf)) {
        if (memBuf->buffer != NULL) {
            free(memBuf->buffer);
            memBuf->buffer = NULL;
        }
    }
    data->data = NULL;
}

static void opaque_io_closedir(Opaque *data) {
    if (data == NULL) return;
    if (data->data == NULL) return;
    DEBUG("closing dir %p", data->data);
    closedir((DIR *)data->data);
    data->data = NULL;
}

static Value builtin_open(Vec *args) {
    char *filename = listToUtf8(args->entries[0]);
    int mode = args->entries[1].val.stdint;
    FILE *file = NULL;
    switch (mode) {
        case IO_MODE_READ:
            file = fopen(filename, "r");
            break;
        case IO_MODE_WRITE:
            file = fopen(filename, "w");
            break;
        case IO_MODE_APPEND:
            file = fopen(filename, "a");
            break;
        default:
            cant_happen("unexpected %d", mode);
    }
    FREE_ARRAY(char, filename, strlen(filename) + 1);
    if (file == NULL) {
        return errnoToTry();
    }
    DEBUG("io open %p", file);
    Opaque *wrapper = newOpaque(file, opaque_io_close, NULL);
    Value opaque = value_Opaque(wrapper);
    int save = protectValue(opaque);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(save);
    return result;
}

static Value builtin_open_memstream(Vec *args __attribute__((unused))) {
    BuiltInMemBuf *memBuf = newBuiltInMemBuf();
    int save = PROTECT(memBuf);
    FILE *file = open_memstream(&memBuf->buffer, &memBuf->size);
    BuiltInMemBufHash *memBufs = getMemBufs();
    HashSymbol *key = fileHandleToKey(file);
    setBuiltInMemBufHash(memBufs, key, memBuf);
    Opaque *wrapper = newOpaque(file, opaque_io_close, NULL);
    Value opaque = value_Opaque(wrapper);
    protectValue(opaque);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(save);
    return result;
}

static Value builtin_opendir(Vec *args) {
    char *dirname = listToUtf8(args->entries[0]);
    DIR *dir = opendir(dirname);
    FREE_ARRAY(char, dirname, strlen(dirname) + 1);
    if (dir == NULL) {
        return errnoToTry();
    }
    DEBUG("io opendir %p", dir);
    Opaque *wrapper = newOpaque(dir, opaque_io_closedir, NULL);
    Value opaque = value_Opaque(wrapper);
    int save = protectValue(opaque);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(save);
    return result;
}

#define FTYPE_SOCKET 0
#define FTYPE_SYMLINK 1
#define FTYPE_REGULAR 2
#define FTYPE_BLOCK 3
#define FTYPE_DIR 4
#define FTYPE_CHAR 5
#define FTYPE_FIFO 6

static Value builtin_ftype(Vec *args) {
    struct stat statbuf;
    char *dirname = listToUtf8(args->entries[0]);
    int status = stat(dirname, &statbuf);
    FREE_ARRAY(char, dirname, strlen(dirname) + 1);
    if (status == -1) {
        return errnoToTry();
    }
    switch (statbuf.st_mode & S_IFMT) {
        case S_IFSOCK:
            return makeSome(value_Stdint(FTYPE_SOCKET));
        case S_IFLNK:
            return makeSome(value_Stdint(FTYPE_SYMLINK));
        case S_IFREG:
            return makeSome(value_Stdint(FTYPE_REGULAR));
        case S_IFBLK:
            return makeSome(value_Stdint(FTYPE_BLOCK));
        case S_IFDIR:
            return makeSome(value_Stdint(FTYPE_DIR));
        case S_IFCHR:
            return makeSome(value_Stdint(FTYPE_CHAR));
        case S_IFIFO:
            return makeSome(value_Stdint(FTYPE_FIFO));
        default:
            cant_happen("unrecognised file type %u", statbuf.st_mode & S_IFMT);
    }
}

static Value builtin_close(Vec *args) {
#ifdef SAFETY_CHECKS
    if (args->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(args->entries[0].type));
    }
#endif
    Opaque *data = args->entries[0].val.opaque;
    opaque_io_close(data);
    args->entries[0].val.opaque = NULL;
    return value_Stdint(1);
}

static Value builtin_closedir(Vec *args) {
#ifdef SAFETY_CHECKS
    if (args->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(args->entries[0].type));
    }
#endif
    Opaque *data = args->entries[0].val.opaque;
    opaque_io_closedir(data);
    args->entries[0].val.opaque = NULL;
    return value_Stdint(1);
}

static Value builtin_readdir(Vec *args) {
#ifdef SAFETY_CHECKS
    if (args->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(args->entries[0].type));
    }
#endif
    Opaque *data = args->entries[0].val.opaque;
    if (data->data == NULL) {
        return makeNothing();
    }
    DIR *dir = (DIR *) data->data;
    struct dirent *entry = readdir(dir);
    if (entry == NULL) {
        return makeNothing();
    }
    Value string = utf8ToList(entry->d_name);
    int save = protectValue(string);
    Value someString = makeSome(string);
    UNPROTECT(save);
    return someString;
}

void fputValue(FILE *fh, Value x) {
    switch (x.type) {
        case VALUE_TYPE_NONE:
            fprintf(fh, "<void>");
            break;
        case VALUE_TYPE_STDINT:
            fprintf(fh, "%d", x.val.stdint);
            break;
        case VALUE_TYPE_STDINT_IMAG:
            fprintf(fh, "%di", x.val.stdint);
            break;
        case VALUE_TYPE_BIGINT:
            fprintBigInt(fh, x.val.bigint);
            break;
        case VALUE_TYPE_BIGINT_IMAG:
            fprintBigInt(fh, x.val.bigint);
            fprintf(fh, "i");
            break;
        case VALUE_TYPE_RATIONAL:
            fputValue(fh, x.val.vec->entries[0]);
            fprintf(fh, "/");
            fputValue(fh, x.val.vec->entries[1]);
            break;
        case VALUE_TYPE_RATIONAL_IMAG:
            fprintf(fh, "(");
            fputValue(fh, x.val.vec->entries[0]);
            fprintf(fh, "/");
            fputValue(fh, x.val.vec->entries[1]);
            fprintf(fh, ")i");
            break;
        case VALUE_TYPE_IRRATIONAL:
            if(fmod(x.val.irrational, 1) == 0)
                fprintf(fh, "%.1f", x.val.irrational);
            else
                fprintf(fh, "%g", x.val.irrational);
            break;
        case VALUE_TYPE_IRRATIONAL_IMAG:
            if( fmod(x.val.irrational, 1) == 0)
                fprintf(fh, "%.1fi", x.val.irrational);
            else
                fprintf(fh, "%gi", x.val.irrational);
            break;
        case VALUE_TYPE_COMPLEX:
            fprintf(fh, "(");
            fputValue(fh, x.val.vec->entries[0]);
            fprintf(fh, "+");
            fputValue(fh, x.val.vec->entries[1]);
            fprintf(fh, ")");
            break;
        case VALUE_TYPE_CHARACTER:
            fprintf(fh, "%s", charRep(x.val.character));
            break;
        case VALUE_TYPE_CLO:
            fprintf(fh, "<closure>");
            break;
        case VALUE_TYPE_KONT:
            fprintf(fh, "<continuation>");
            break;
        case VALUE_TYPE_VEC:
            fputVec(fh, x.val.vec);
            break;
        default:
            cant_happen("unrecognised value type %s", valueTypeName(x.type));
    }
}

void putValue(Value x) {
    fputValue(stdout, x);
}

void fputVec(FILE *fh, Vec *x) {
    fprintf(fh, "#[");
    for (Index i = 0; i < x->size; i++) {
        fputValue(fh, x->entries[i]);
        if (i + 1 < x->size) {
            fprintf(fh, " ");
        }
    }
    fprintf(fh, "]");
}

void putVec(Vec *x) {
    fputVec(stdout, x);
}

static Value private_fgets(FILE *fh) {
    ByteArray *bytes = newByteArray();
    int save = PROTECT(bytes);
    HashSymbol *key = fileHandleToKey(fh);
    BuiltInMemBuf *buf = NULL;
    if (getBuiltInMemBufHash(getMemBufs(), key, &buf)) {
        fflush(fh);
        if (buf->buffer == NULL) {
            cant_happen("fgets on null memstream");
        }
        do { pushByteArray(bytes, (Byte) buf->buffer[buf->index]); } while (buf->buffer[buf->index++]);
        buf->index--; // point back at '\0' for next time
    } else {
        int c;
        while ((c = fgetc(fh)) != EOF) {
            if (c == '\n') break;
            if (c == 0) break;
            pushByteArray(bytes, (Byte) c);
        }
        pushByteArray(bytes, 0);
    }
    Value string = utf8ToList((char *) bytes->entries);
    UNPROTECT(save);
    return string;
}

static Value builtin_gets() {
    return private_fgets(stdin);
}

static Value private_fgetc(FILE *fh) {
    HashSymbol *key = fileHandleToKey(fh);
    if (getBuiltInMemBufHash(getMemBufs(), key, NULL)) {
        cant_happen("getc on memory buffers not supported yet");
    }
    Character c = utf8Fgetc(fh);
    return value_Character(c);
}

static Value builtin_getc() {
    return private_fgetc(stdin);
}

static Value builtin_fgetc(Vec *args) {
    Opaque *data = args->entries[0].val.opaque;
    if (data == NULL || data->data == NULL) {
        cant_happen("fgets on closed file handle");
    }
    return private_fgetc((FILE *) data->data);
}

static Value builtin_fgets(Vec *args) {
    Opaque *data = args->entries[0].val.opaque;
    if (data == NULL || data->data == NULL) {
        cant_happen("fgets on closed file handle");
    }
    return private_fgets((FILE *) data->data);
}

static Value builtin_putv(Vec *args) {
    putValue(args->entries[0]);
    return args->entries[0];
}

static Value builtin_fputv(Vec *args) {
    Opaque *data = args->entries[0].val.opaque;
    if (data == NULL || data->data == NULL) {
        cant_happen("fput on closed file handle");
    }
    fputValue((FILE *) data->data, args->entries[1]);
    return args->entries[1];
}

static TcType *pushFileArg(BuiltInArgs *args) {
    TcType *fileType = makeFileType();
    int save = PROTECT(fileType);
    pushBuiltInArgs(args, fileType);
    UNPROTECT(save);
    return fileType;
}

static TcType *pushDirArg(BuiltInArgs *args) {
    TcType *dirType = makeDirType();
    int save = PROTECT(dirType);
    pushBuiltInArgs(args, dirType);
    UNPROTECT(save);
    return dirType;
}

static TcType *pushIoArg(BuiltInArgs *args) {
    TcType *ioType = makeIOType();
    int save = PROTECT(ioType);
    pushBuiltInArgs(args, ioType);
    UNPROTECT(save);
    return ioType;
}

static TcType *makeTryFileType(TcType *errorType) {
    TcType *fileType = makeFileType();
    int save = PROTECT(fileType);
    TcType *tryFileType = makeTryType(errorType, fileType);
    UNPROTECT(save);
    return tryFileType;
}

static TcType *makeTryDirType(TcType *errorType) {
    TcType *dirType = makeDirType();
    int save = PROTECT(dirType);
    TcType *tryDirType = makeTryType(errorType, dirType);
    UNPROTECT(save);
    return tryDirType;
}

static TcType *makeTryFTypeType(TcType *errorType) {
    TcType *ftypeType = makeFTypeType();
    int save = PROTECT(ftypeType);
    TcType *tryFTypeType = makeTryType(errorType, ftypeType);
    UNPROTECT(save);
    return tryFTypeType;
}

// char -> char
static void registerPutc(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *charType = pushCharacterArg(args);
    pushNewBuiltIn(registry, "putc", charType, args, (void *)builtin_putc);
    UNPROTECT(save);
}

// opaque(file) -> char -> char
static void registerFPutc(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileArg(args);
    TcType *charType = pushCharacterArg(args);
    pushNewBuiltIn(registry, "fputc", charType, args, (void *)builtin_fputc);
    UNPROTECT(save);
}

// number -> number
static void registerPutn(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *numberType = pushIntegerArg(args);
    pushNewBuiltIn(registry, "putn", numberType, args, (void *)builtin_putv); // re-use putv
    UNPROTECT(save);
}

// opaque(file) -> number -> number
static void registerFPutn(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileArg(args);
    TcType *numberType = pushIntegerArg(args);
    pushNewBuiltIn(registry, "fputn", numberType, args, (void *)builtin_fputv); // re-use putv
    UNPROTECT(save);
}

// #t -> #t
static void registerPutv(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *anyType = pushAnyArg(args);
    pushNewBuiltIn(registry, "putv", anyType, args, (void *)builtin_putv); // re-use putv
    UNPROTECT(save);
}

// opaque(file) -> #t -> #t
static void registerFPutv(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileArg(args);
    TcType *anyType = pushAnyArg(args);
    pushNewBuiltIn(registry,"fputv", anyType, args, (void *)builtin_fputv); // re-use putv
    UNPROTECT(save);
}


// string -> string
static void registerPuts(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = pushStringArg(args);
    pushNewBuiltIn(registry, "puts", stringType, args, (void *)builtin_puts);
    UNPROTECT(save);
}

// string -> mode -> try(string, opaque(file))
static void registerOpen(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = pushStringArg(args);
    pushIoArg(args);
    TcType *tryFileType = makeTryFileType(stringType);
    PROTECT(tryFileType);
    pushNewBuiltIn(registry, "open", tryFileType, args, (void *)builtin_open);
    UNPROTECT(save);
}

// try(string, opaque(file))
static void registerOpenMemstream(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    TcType *tryFileType = makeTryFileType(stringType);
    PROTECT(tryFileType);
    pushNewBuiltIn(registry, "openmem", tryFileType, args, (void *)builtin_open_memstream);
    UNPROTECT(save);
}

// string -> try(string, opaque(dir))
static void registerOpenDir(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = pushStringArg(args);
    TcType *tryDirType = makeTryDirType(stringType);
    PROTECT(tryDirType);
    pushNewBuiltIn(registry, "opendir", tryDirType, args, (void *)builtin_opendir);
    UNPROTECT(save);
}

static void registerFType(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = pushStringArg(args);
    TcType *tryFTypeType = makeTryFTypeType(stringType);
    PROTECT(tryFTypeType);
    pushNewBuiltIn(registry, "ftype", tryFTypeType, args, (void *)builtin_ftype);
    UNPROTECT(save);
}

// opaque(file) -> bool
static void registerClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "close", b, args, (void *)builtin_close);
    UNPROTECT(save);
}

// opaque(file) -> bool
static void registerCloseDir(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushDirArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "closedir", b, args, (void *)builtin_closedir);
    UNPROTECT(save);
}

// opaque(dir) -> maybe(string)
static void registerReadDir(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushDirArg(args);
    TcType *maybeStringType = makeMaybeStringType();
    PROTECT(maybeStringType);
    pushNewBuiltIn(registry, "readdir", maybeStringType, args, (void *)builtin_readdir);
    UNPROTECT(save);
}

// () -> string
static void registerGets(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    pushNewBuiltIn(registry, "gets", stringType, args, (void *)builtin_gets);
    UNPROTECT(save);
}

// () -> char
static void registerGetc(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *charType = newTcType_Character();
    PROTECT(charType);
    pushNewBuiltIn(registry, "getc", charType, args, (void *)builtin_getc);
    UNPROTECT(save);
}

// opaque(file) -> char
static void registerFGetc(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileArg(args);
    TcType *charType = newTcType_Character();
    PROTECT(charType);
    pushNewBuiltIn(registry, "fgetc", charType, args, (void *)builtin_fgetc);
    UNPROTECT(save);
}

// opaque(file) -> string -> string
static void registerFPuts(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileArg(args);
    TcType *stringType = pushStringArg(args);
    pushNewBuiltIn(registry, "fputs", stringType, args, (void *)builtin_fputs);
    UNPROTECT(save);
}

// opaque(file) -> string
static void registerFGets(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileArg(args);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    pushNewBuiltIn(registry, "fgets", stringType, args, (void *)builtin_fgets);
    UNPROTECT(save);
}
