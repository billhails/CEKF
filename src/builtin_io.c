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
static void registerClose(BuiltIns *registry);

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
    registerClose(registry);
}

static HashSymbol *fileSymbol(void) {
    return newSymbol("file");
}

static TcType *makeFileType(void) {
    return newTcType_Opaque(fileSymbol());
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


static void opaque_io_close(Opaque *data) {
    if (data == NULL) return;
    if (data->data == NULL) return;
    DEBUG("closing io %p", data->data);
    fclose(data->data);
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
        Value errMsg = utf8ToList(strerror(errno));
        int save = protectValue(errMsg);
        Value result = makeTryResult(0, errMsg);
        UNPROTECT(save);
        return result;
    }
    DEBUG("io open %p", file);
    Opaque *wrapper = newOpaque(file, opaque_io_close, NULL);
    Value opaque = value_Opaque(wrapper);
    int save = protectValue(opaque);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(save);
    return result;
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
            cant_happen("unrecognised value type in fputValue");
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
    int c;
    while ((c = fgetc(fh)) != EOF) {
        if (c == '\n') break;
        if (c == 0) break;
        pushByteArray(bytes, (Byte) c);
    }
    pushByteArray(bytes, 0);
    Value string = utf8ToList((char *) bytes->entries);
    UNPROTECT(save);
    return string;
}

static Value builtin_gets() {
    return private_fgets(stdin);
}

static Value private_fgetc(FILE *fh) {
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
    fputValue((FILE *) data->data, args->entries[0]);
    return args->entries[0];
}

static TcType *pushCharType(BuiltInArgs *args) {
    TcType *charType = newTcType_Character();
    int save = PROTECT(charType);
    pushBuiltInArgs(args, charType);
    UNPROTECT(save);
    return charType;
}

static TcType *pushStringType(BuiltInArgs *args) {
    TcType *stringType = makeStringType();
    int save = PROTECT(stringType);
    pushBuiltInArgs(args, stringType);
    UNPROTECT(save);
    return stringType;
}

static TcType *pushFileType(BuiltInArgs *args) {
    TcType *fileType = makeFileType();
    int save = PROTECT(fileType);
    pushBuiltInArgs(args, fileType);
    UNPROTECT(save);
    return fileType;
}

static TcType *pushNumberType(BuiltInArgs *args) {
    TcType *numberType = newTcType_Biginteger();
    int save = PROTECT(numberType);
    pushBuiltInArgs(args, numberType);
    UNPROTECT(save);
    return numberType;
}

static TcType *pushAnyType(BuiltInArgs *args) {
    TcType *anyType = makeFreshVar("any");
    int save = PROTECT(anyType);
    pushBuiltInArgs(args, anyType);
    UNPROTECT(save);
    return anyType;
}

static TcType *pushIOType(BuiltInArgs *args) {
    TcType *modeType = makeIOType();
    int save = PROTECT(modeType);
    pushBuiltInArgs(args, modeType);
    UNPROTECT(save);
    return modeType;
}

static TcType *makeTryFileType(TcType *errorType) {
    TcType *fileType = makeFileType();
    int save = PROTECT(fileType);
    TcType *tryFileType = makeTryType(errorType, fileType);
    UNPROTECT(save);
    return tryFileType;
}

static void pushNewBuiltIn(BuiltIns *registry, char *name, TcType *returnType, BuiltInArgs *args, void *implementation) {
    BuiltIn *decl = newBuiltIn(newSymbol(name), returnType, args, implementation);
    int save = PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

// char -> char
static void registerPutc(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *charType = pushCharType(args);
    pushNewBuiltIn(registry, "putc", charType, args, (void *)builtin_putc);
    UNPROTECT(save);
}

// opaque(file) -> char -> char
static void registerFPutc(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileType(args);
    TcType *charType = pushCharType(args);
    pushNewBuiltIn(registry, "fputc", charType, args, (void *)builtin_fputc);
    UNPROTECT(save);
}

// number -> number
static void registerPutn(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *numberType = pushNumberType(args);
    pushNewBuiltIn(registry, "putn", numberType, args, (void *)builtin_putv); // re-use putv
    UNPROTECT(save);
}

// opaque(file) -> number -> number
static void registerFPutn(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileType(args);
    TcType *numberType = pushNumberType(args);
    pushNewBuiltIn(registry, "fputn", numberType, args, (void *)builtin_fputv); // re-use putv
    UNPROTECT(save);
}

// #t -> #t
static void registerPutv(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *anyType = pushAnyType(args);
    pushNewBuiltIn(registry, "putv", anyType, args, (void *)builtin_putv); // re-use putv
    UNPROTECT(save);
}

// opaque(file) -> #t -> #t
static void registerFPutv(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileType(args);
    TcType *anyType = pushAnyType(args);
    pushNewBuiltIn(registry,"fputv", anyType, args, (void *)builtin_fputv); // re-use putv
    UNPROTECT(save);
}


// string -> string
static void registerPuts(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = pushStringType(args);
    pushNewBuiltIn(registry, "puts", stringType, args, (void *)builtin_puts);
    UNPROTECT(save);
}

// string -> mode -> try(string, opaque(file))
static void registerOpen(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = pushStringType(args);
    pushIOType(args);
    TcType *tryFileType = makeTryFileType(stringType);
    PROTECT(tryFileType);
    pushNewBuiltIn(registry, "open", tryFileType, args, (void *)builtin_open);
    UNPROTECT(save);
}

// opaque(file) -> bool
static void registerClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileType(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "close", b, args, (void *)builtin_close);
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
    pushFileType(args);
    TcType *charType = newTcType_Character();
    PROTECT(charType);
    pushNewBuiltIn(registry, "fgetc", charType, args, (void *)builtin_fgetc);
    UNPROTECT(save);
}

// opaque(file) -> string -> string
static void registerFPuts(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileType(args);
    TcType *stringType = pushStringType(args);
    pushNewBuiltIn(registry, "fputs", stringType, args, (void *)builtin_fputs);
    UNPROTECT(save);
}

// opaque(file) -> string
static void registerFGets(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFileType(args);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    pushNewBuiltIn(registry, "fgets", stringType, args, (void *)builtin_fgets);
    UNPROTECT(save);
}
