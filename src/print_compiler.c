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
#include "lambda_pp.h"
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
static LamExp *compilePrinterForTuple(TcTypeArray *tuple, TcEnv *env);
static LamExp *compilePrinter(TcType *type, TcEnv *env);

LamExp *compilePrinterForType(TcType *type, TcEnv *env) {
    // (lambda (x) (begin (printer x) (putc '\n') x)
    LamExp *printer = compilePrinter(type, env);
    int save = PROTECT(printer);
    // x)
    HashSymbol *name = genSym("x$");
    LamExp *var = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    PROTECT(var);
    LamSequence *seq = newLamSequence(var, NULL);
    PROTECT(seq);

    // (putc '\n') x)
    LamExp *newline = newLamExp(LAMEXP_TYPE_CHARACTER, LAMEXP_VAL_CHARACTER('\n'));
    PROTECT(newline);
    LamUnaryApp *app = newLamUnaryApp(LAMUNARYOP_TYPE_PUTC, newline);
    PROTECT(app);
    LamExp *exp = newLamExp(LAMEXP_TYPE_UNARY, LAMEXP_VAL_UNARY(app));
    PROTECT(exp);
    seq = newLamSequence(exp, seq);
    PROTECT(seq);

    // (printer x) (putc '\n') x)
    LamList *args = newLamList(var, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(printer, 1, args);
    PROTECT(apply);
    LamExp *applyExp = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    PROTECT(applyExp);
    seq = newLamSequence(applyExp, seq);
    PROTECT(seq);

    // (lambda (x) (begin (printer x) (putc '\n') x)
    LamVarList *fargs = newLamVarList(name, NULL);
    PROTECT(fargs);
    LamExp *body = newLamExp(LAMEXP_TYPE_LIST, LAMEXP_VAL_LIST(seq));
    PROTECT(body);
    LamLam *lambda = newLamLam(1, fargs, body);
    PROTECT(lambda);
    LamExp *res = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(lambda));
    UNPROTECT(save);
    return res;
}

static LamExp *compilePrinter(TcType *type, TcEnv *env) {
    ENTER(compilePrinter);
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
        case TCTYPE_TYPE_TUPLE:
            res = compilePrinterForTuple(type->val.tuple, env);
            break;
        default:
            cant_happen("unrecognised TcType %s", tcTypeTypeName(type->type));
    }
    LEAVE(compilePrinter);
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
    return compilePrinter(var->instance, env);
}

static LamExp *compilePrinterForInt() {
    return makePrintInt();
}

static LamExp *compilePrinterForChar() {
    return makePrintChar();
}

static LamList *compilePrinterForUserTypeArgs(TcUserTypeArgs *args,
                                              TcEnv *env) {
    ENTER(compilePrinterForUserTypeArgs);
    if (args == NULL) {
        LEAVE(compilePrinterForUserTypeArgs);
        return NULL;
    }
    LamList *next = compilePrinterForUserTypeArgs(args->next, env);
    int save = PROTECT(next);
    LamExp *this = compilePrinter(args->type, env);
    PROTECT(this);
    LamList *res = newLamList(this, next);
    UNPROTECT(save);
    LEAVE(compilePrinterForUserTypeArgs);
    return res;
}

static LamList *compilePrinterForTupleArgs(TcTypeArray *tuple, TcEnv *env) {
    LamList *res = NULL;
    int save = PROTECT(res);
    for (int i = tuple->size; i > 0; i--) {
        int index = i - 1;
        LamExp *this = compilePrinter(tuple->entries[index], env);
        PROTECT(this);
        res = newLamList(this, res);
        PROTECT(res);
    }
    UNPROTECT(save);
    return res;
}

static LamExp *compilePrinterForString() {
    HashSymbol *name = newSymbol("print$string");
    return newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
}

static LamExp *compilePrinterForUserType(TcUserType *userType, TcEnv *env) {
    IFDEBUG(printTcUserType(userType, 0));
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

static LamExp *compilePrinterForTuple(TcTypeArray *tuple, TcEnv *env) {
    ENTER(compilePrinterForTuple);
    if (tuple->size < 5) {
        char buf[64];
        sprintf(buf, "print$tuple$%d", tuple->size);
        LamExp *exp = makeSymbolExpr(buf);
        if (tuple->size == 0) {
            LEAVE(compilePrinterForTuple);
            return exp;
        }
        int save = PROTECT(exp);
        LamList *args = compilePrinterForTupleArgs(tuple, env);
        PROTECT(args);
        LamApply *apply = newLamApply(exp, tuple->size, args);
        PROTECT(apply);
        LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
        UNPROTECT(save);
        IFDEBUG(ppLamExp(res));
        LEAVE(compilePrinterForTuple);
        return res;
    } else {
        LEAVE(compilePrinterForTuple);
        return makeSymbolExpr("print$");
    }
}
