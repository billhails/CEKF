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
 *
 * print function compiler
 * invoked during type checking
 * computes a print function that will print the given type
 */

#include <stdio.h>
#include "print_compiler.h"
#include "print_generator.h"
#include "cekf.h"
#include "common.h"
#include "lambda.h"
#include "lambda_helper.h"
#include "symbol.h"
#include "symbols.h"

#ifdef DEBUG_PRINT_COMPILER
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static LamExp *compilePrinterForFunction(TcFunction *function);
static LamExp *compilePrinterForPair(TcPair *pair);
static LamExp *compilePrinterForVar(TcVar *var, TcEnv *env);
static LamExp *compilePrinterForInt();
static LamExp *compilePrinterForChar();
static LamExp *compilePrinterForUserType(TcUserType *userType, TcEnv *env);

LamExp *compilePrinterForType(TcType *type, TcEnv *env) {
    LamExp *res = NULL;
    switch (type->type) {
        case TCTYPE_TYPE_FUNCTION:
            res = compilePrinterForFunction(type->val.function);
            break;
        case TCTYPE_TYPE_PAIR:
            res = compilePrinterForPair(type->val.pair);
            break;
        case TCTYPE_TYPE_VAR:
            res = compilePrinterForVar(type->val.var, env);
            break;
        case TCTYPE_TYPE_SMALLINTEGER:
        case TCTYPE_TYPE_BIGINTEGER:
            res = compilePrinterForInt();
            break;
        case TCTYPE_TYPE_CHARACTER:
            res = compilePrinterForChar();
            break;
        case TCTYPE_TYPE_USERTYPE:
            res = compilePrinterForUserType(type->val.userType, env);
            break;
        default:
            cant_happen("unrecognised TcType %d in compilePrinterForType",
                        type->type);
    }
    return res;
}

static LamExp *compilePrinterForFunction(TcFunction *function
                                         __attribute__((unused))) {
    return makeSymbolExpr("print$fn");
}

static LamExp *compilePrinterForPair(TcPair *pair __attribute__((unused))) {
    cant_happen("compilePrinterForPair not implemented yet");
}

static LamExp *compilePrinterForVar(TcVar *var, TcEnv *env) {
    if (var->instance == NULL) {
        return makeSymbolExpr("print$");
    }
    return compilePrinterForType(var->instance, env);
}

static LamExp *compilePrinterForInt() {
    return makePrintInt();
}

static LamExp *compilePrinterForChar() {
    return makePrintChar();
}

static LamList *compilePrinterForUserTypeArgs(TcUserTypeArgs *args,
                                              TcEnv *env) {
    if (args == NULL)
        return NULL;
    LamList *next = compilePrinterForUserTypeArgs(args->next, env);
    int save = PROTECT(next);
    LamExp *this = compilePrinterForType(args->type, env);
    PROTECT(this);
    LamList *res = newLamList(this, next);
    UNPROTECT(save);
    return res;
}

static LamExp *compilePrinterForString() {
    HashSymbol *name = newSymbol("print$string");
    return newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
}

static LamExp *compilePrinterForUserType(TcUserType *userType, TcEnv *env) {
    if (userType->name == listSymbol()) {
        if (userType->args
            && userType->args->type->type == TCTYPE_TYPE_CHARACTER) {
            return compilePrinterForString();
        }
    }
    HashSymbol *name = makePrintName("print$", userType->name->name);
    if (!getFromTcEnv(env, name, NULL)) {
        return makeSymbolExpr("print$");
    }
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    int save = PROTECT(exp);
    LamList *args = compilePrinterForUserTypeArgs(userType->args, env);
    PROTECT(args);
    int nargs = countLamList(args);
    if (nargs == 0) {
        UNPROTECT(save);
        return exp;
    }
    LamApply *apply = newLamApply(exp, nargs, args);
    PROTECT(apply);
    LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return res;
}
