/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2023  Bill Hails
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

// print function generator and compiler code

#include <stdio.h>
#include "print.h"
#include "cekf.h"
#include "common.h"
#include "lambda.h"
#include "symbol.h"

#ifdef DEBUG_PRINT
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static void putVec(Vec *x);

#ifdef NOTDEF
static LamLetRecBindings *makePrintFunction(LamTypeDef *typeDef, LamLetRecBindings *next);
static HashSymbol *makePrintName(LamType *type);

LamLetRecBindings *makePrintFunctions(LamTypeDefList *typeDefs, LamLetRecBindings *next) {
    ENTER(makePrintFunctions);
    if (typeDefs == NULL) {
        LEAVE(makePrintFunctions);
        return next;
    }
    next = makePrintFunctions(typeDefs->next, next, env);
    int save = PROTECT(next);
    
    next = makePrintFunction(typeDefs->typeDef, next, env);

    UNPROTECT(save);
    LEAVE(makePrintFunctions);
    return next;
}

static bool isListType(LamType *type) {
    return type->name == listSymbol();
}

static HashSymbol *makePrintName(char *name) {
    const char *prefix = "print$";
    HashSymbol *res = NULL;
    int size = strlen(name) + strlen(prefix) + 1;
    if (size < 128) {
        static char buf[128];
        sprintf(buf, "%s%s", prefix, name);
        res = newSymbol(buf);
    } else {
        char *buf = NEW_ARRAY(char, size);
        sprintf(buf, "%s%s", prefix, name);
        res = newSymbol(buf);
        FREE_ARRAY(char, buf, name);
    }
    return res;
}

static LamLetRecBindings *injectPrintListFunctions(LamLetRecBindings *next) {
    ENTER(injectPrintListFunctions);
    next = makePrintGenericFunction(next);
    int save = PROTECT(next);
    next = makePrintStringFunction(next);
    REPLACE_PROTECT(save, next);
    next = makePrintListFunction(next);
    UNPROTECT(save);
    LEAVE(injectPrintListFunctions);
    return next;
}

static LamLetRecBindings *makePrintTypeFunction(LamTypeDef *typeDef, LamLetRecBindings *next) {
}

static LamLetRecBindings *makePrintFunction(LamTypeDef *typeDef, LamLetRecBindings *next) {
    if (isListType(typeDef->type)) {
        return injectPrintListFunctions(next);
    } else {
        return makePrintTypeFunction(typeDef, next);
    }
}
#endif

void putValue(Value x) {
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printf("<void>");
            break;
        case VALUE_TYPE_STDINT:
            printf("%d", x.val.z);
            break;
        case VALUE_TYPE_BIGINT:
            fprintBigInt(stdout, x.val.b);
            break;
        case VALUE_TYPE_CHARACTER:
            switch (x.val.c) {
                case '\t':
                    printf("'\\t'");
                    break;
                case '\n':
                    printf("'\\n'");
                    break;
                default:
                    printf("'%c'", x.val.c);
                    break;
            }
            break;
        case VALUE_TYPE_CLO:
            printf("<closure>");
            break;
        case VALUE_TYPE_CONT:
            printf("<continuation>");
            break;
        case VALUE_TYPE_VEC:
            putVec(x.val.vec);
            break;
        default:
            cant_happen("unrecognised value type in putValue");
    }
}

static void putVec(Vec *x) {
    printf("#[");
    for (int i = 0; i < x->size; i++) {
        putValue(x->values[i]);
        if (i + 1 < x->size) {
            printf(" ");
        }
    }
    printf("]");
}
