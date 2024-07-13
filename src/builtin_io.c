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
static void registerPutn(BuiltIns *registry);
static void registerPutv(BuiltIns *registry);
static void registerPuts(BuiltIns *registry);
static void registerFPuts(BuiltIns *registry);
static void registerGets(BuiltIns *registry);
static void registerFGets(BuiltIns *registry);
static void registerOpen(BuiltIns *registry);
static void registerClose(BuiltIns *registry);

void registerIO(BuiltIns *registry) {
    registerPutc(registry);
    registerPutn(registry);
    registerPutv(registry);
    registerPuts(registry);
    registerFPuts(registry);
    registerGets(registry);
    registerFGets(registry);
    registerOpen(registry);
    registerClose(registry);
}

static Value builtin_putc(Vec *args) {
#ifdef SAFETY_CHECKS
    if (args->entries[0].type != VALUE_TYPE_CHARACTER) {
        cant_happen("unexpected %s", valueTypeName(args->entries[0].type));
    }
#endif
    char buf[16];
    char *end = (char *) writeChar((unsigned char *)buf, args->entries[0].val.character);
    *end = 0;
    printf("%s", buf);
    return args->entries[0];
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

void putValue(Value x) {
    switch (x.type) {
        case VALUE_TYPE_NONE:
            printf("<void>");
            break;
        case VALUE_TYPE_STDINT:
            printf("%d", x.val.stdint);
            break;
        case VALUE_TYPE_STDINT_IMAG:
            printf("%di", x.val.stdint);
            break;
        case VALUE_TYPE_BIGINT:
            fprintBigInt(stdout, x.val.bigint);
            break;
        case VALUE_TYPE_BIGINT_IMAG:
            fprintBigInt(stdout, x.val.bigint);
            printf("i");
            break;
        case VALUE_TYPE_RATIONAL:
            putValue(x.val.vec->entries[0]);
            printf("/");
            putValue(x.val.vec->entries[1]);
            break;
        case VALUE_TYPE_RATIONAL_IMAG:
            printf("(");
            putValue(x.val.vec->entries[0]);
            printf("/");
            putValue(x.val.vec->entries[1]);
            printf(")i");
            break;
        case VALUE_TYPE_IRRATIONAL:
            if(fmod(x.val.irrational, 1) == 0)
                printf("%.1f", x.val.irrational);
            else
                printf("%g", x.val.irrational);
            break;
        case VALUE_TYPE_IRRATIONAL_IMAG:
            if( fmod(x.val.irrational, 1) == 0)
                printf("%.1fi", x.val.irrational);
            else
                printf("%gi", x.val.irrational);
            break;
        case VALUE_TYPE_COMPLEX:
            printf("(");
            putValue(x.val.vec->entries[0]);
            printf(" + ");
            putValue(x.val.vec->entries[1]);
            printf(")");
            break;
        case VALUE_TYPE_CHARACTER:
            printf("%s", charRep(x.val.character));
            break;
        case VALUE_TYPE_CLO:
            printf("<closure>");
            break;
        case VALUE_TYPE_KONT:
            printf("<continuation>");
            break;
        case VALUE_TYPE_VEC:
            putVec(x.val.vec);
            break;
        default:
            cant_happen("unrecognised value type in putValue");
    }
}

void putVec(Vec *x) {
    printf("#[");
    for (Index i = 0; i < x->size; i++) {
        putValue(x->entries[i]);
        if (i + 1 < x->size) {
            printf(" ");
        }
    }
    printf("]");
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

// char -> char
static void registerPutc(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *charType = newTcType_Character();
    PROTECT(charType);
    pushBuiltInArgs(args, charType);
    BuiltIn *decl = newBuiltIn(newSymbol("putc"), charType, args, (void *)builtin_putc);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

// number -> number
static void registerPutn(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *numberType = newTcType_Biginteger();
    PROTECT(numberType);
    pushBuiltInArgs(args, numberType);
    BuiltIn *decl = newBuiltIn(newSymbol("putn"), numberType, args, (void *)builtin_putv);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

// #t -> #t
static void registerPutv(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *anyType = makeFreshVar("any");
    PROTECT(anyType);
    pushBuiltInArgs(args, anyType);
    BuiltIn *decl = newBuiltIn(newSymbol("putv"), anyType, args, (void *)builtin_putv);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}


// #t -> #t
static void registerPuts(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    pushBuiltInArgs(args, stringType);
    BuiltIn *decl = newBuiltIn(newSymbol("puts"), stringType, args, (void *)builtin_puts);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static HashSymbol *fileSymbol(void) {
    return newSymbol("file");
}

static TcType *makeFileType(void) {
    return newTcType_Opaque(fileSymbol());
}

// string -> char -> try(string, opaque(file))
static void registerOpen(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    TcType *modeType = makeIOType();
    PROTECT(modeType);
    TcType *fileType = makeFileType();
    PROTECT(fileType);
    TcType *tryFileType = makeTryType(stringType, fileType);
    PROTECT(tryFileType);
    pushBuiltInArgs(args, stringType);
    pushBuiltInArgs(args, modeType);
    BuiltIn *decl = newBuiltIn(newSymbol("open"), tryFileType, args, (void *)builtin_open);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

// opaque(file) -> bool
static void registerClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *fileType = makeFileType();
    PROTECT(fileType);
    pushBuiltInArgs(args, fileType);
    TcType *b = makeBoolean();
    PROTECT(b);
    BuiltIn *decl = newBuiltIn(newSymbol("close"), b, args, (void *)builtin_close);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

// () -> string
static void registerGets(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    BuiltIn *decl = newBuiltIn(newSymbol("gets"), stringType, args, (void *)builtin_gets);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

// opaque(file) -> string -> string
static void registerFPuts(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *fileType = makeFileType();
    PROTECT(fileType);
    pushBuiltInArgs(args, fileType);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    pushBuiltInArgs(args, stringType);
    BuiltIn *decl = newBuiltIn(newSymbol("fputs"), stringType, args, (void *)builtin_fputs);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

// opaque(file) -> string
static void registerFGets(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *fileType = makeFileType();
    PROTECT(fileType);
    pushBuiltInArgs(args, fileType);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    BuiltIn *decl = newBuiltIn(newSymbol("fgets"), stringType, args, (void *)builtin_fgets);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}
