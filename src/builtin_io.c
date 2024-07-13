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

void registerIO(BuiltIns *registry) {
    registerPutc(registry);
    registerPutn(registry);
    registerPutv(registry);
    registerPuts(registry);
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

static Value builtin_puts(Vec *args) {
    char * buf = listToUtf8(args->entries[0]);
    printf("%s", buf);
    FREE_ARRAY(char, buf, strlen(buf));
    return args->entries[0];
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

