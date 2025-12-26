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
#include "tc_analyze.h"

#ifdef DEBUG_PRINT_COMPILER
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static LamExp *compilePrinterForFunction(ParserInfo I, TcFunction *function);
static LamExp *compilePrinterForPair(ParserInfo I, TcPair *pair);
static LamExp *compilePrinterForThunk(ParserInfo I, TcThunk *thunk);
static LamExp *compilePrinterForVar(ParserInfo I, TcVar *var, TcEnv *env);
static LamExp *compilePrinterForInt(ParserInfo I);
static LamExp *compilePrinterForChar(ParserInfo I);
static LamExp *compilePrinterForTypeSig(ParserInfo I, TcTypeSig *typeSig, TcEnv *env);
static LamExp *compilePrinterForTuple(ParserInfo I, TcTypeArray *tuple, TcEnv *env);
static LamExp *compilePrinter(ParserInfo I, TcType *type, TcEnv *env);

static LamExp *makePutcExp(ParserInfo I, char c) {
    LamExp *character = newLamExp_Character(I, c);
    int save = PROTECT(character);
    LamArgs *putcArgs = newLamArgs(I, character, NULL);
    PROTECT(putcArgs);
    LamExp *putc = newLamExp_Var(I, newSymbol("putc"));
    PROTECT(putc);
    LamApply *applyPutc = newLamApply(I, putc, putcArgs);
    PROTECT(applyPutc);
    LamExp *putcExp = newLamExp_Apply(I, applyPutc);
    UNPROTECT(save);
    return putcExp;
}

LamExp *compilePrinterForType(ParserInfo I, TcType *type, TcEnv *env) {
    // (lambda (x) (begin (printer x) (putc '\n') x)
    LamExp *printer = compilePrinter(I, type, env);
    int save = PROTECT(printer);
    // x)
    HashSymbol *name = genSym("x$");
    LamExp *var = newLamExp_Var(I, name);
    PROTECT(var);
    LamSequence *seq = newLamSequence(I, var, NULL);
    PROTECT(seq);

    // (putc '\n') x)
    LamExp *putcExp = makePutcExp(I, '\n');
    PROTECT(putcExp);
    seq = newLamSequence(I, putcExp, seq);
    PROTECT(seq);

    // (printer x) (putc '\n') x)
    LamArgs *args = newLamArgs(I, var, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(I, printer, args);
    PROTECT(apply);
    LamExp *applyExp = newLamExp_Apply(I, apply);
    PROTECT(applyExp);
    seq = newLamSequence(I, applyExp, seq);
    PROTECT(seq);

    // (lambda (x) (begin (printer x) (putc '\n') x)
    LamVarList *fargs = newLamVarList(I, name, NULL);
    PROTECT(fargs);
    LamExp *body = newLamExp_Sequence(I, seq);
    PROTECT(body);
    LamLam *lambda = newLamLam(I, fargs, body);
    PROTECT(lambda);
    LamExp *res = newLamExp_Lam(I, lambda);
    UNPROTECT(save);
    return res;
}

static LamExp *compilePrinterForOpaque(ParserInfo I) {
    return makeVarExpr(I, "print$opaque");
}

static LamExp *compilePrinter(ParserInfo I, TcType *type, TcEnv *env) {
    ENTER(compilePrinter);
    LamExp *res = NULL;
    switch (type->type) {
        case TCTYPE_TYPE_FUNCTION:
            res = compilePrinterForFunction(I, type->val.function);
            break;
        case TCTYPE_TYPE_PAIR:
            res = compilePrinterForPair(I, type->val.pair);
            break;
        case TCTYPE_TYPE_THUNK:
            res = compilePrinterForThunk(I, type->val.thunk);
            break;
        case TCTYPE_TYPE_VAR:
            res = compilePrinterForVar(I, type->val.var, env);
            break;
        case TCTYPE_TYPE_SMALLINTEGER:
        case TCTYPE_TYPE_BIGINTEGER:
            res = compilePrinterForInt(I);
            break;
        case TCTYPE_TYPE_CHARACTER:
            res = compilePrinterForChar(I);
            break;
        case TCTYPE_TYPE_OPAQUE:
            res = compilePrinterForOpaque(I);
            break;
        case TCTYPE_TYPE_TYPESIG:
            res = compilePrinterForTypeSig(I, type->val.typeSig, env);
            break;
        case TCTYPE_TYPE_TUPLE:
            res = compilePrinterForTuple(I, type->val.tuple, env);
            break;
        default:
            cant_happen("unrecognised TcType %s", tcTypeTypeName(type->type));
    }
    LEAVE(compilePrinter);
    return res;
}

static LamExp *compilePrinterForFunction(ParserInfo I, TcFunction *function
                                         __attribute__((unused))) {
    return makeVarExpr(I, "print$fn");
}

static LamExp *compilePrinterForPair(ParserInfo I __attribute__((unused)), TcPair *pair __attribute__((unused))) {
    cant_happen("compilePrinterForPair not implemented yet");
}

static LamExp *compilePrinterForThunk(ParserInfo I, TcThunk *thunk __attribute__((unused))) {
    // Thunks are functions, so use the function printer
    return makeVarExpr(I, "print$fn");
}

static LamExp *compilePrinterForVar(ParserInfo I, TcVar *var, TcEnv *env) {
    if (var->instance == NULL) {
        return makeVarExpr(I, "__print__");
    }
    return compilePrinter(I, var->instance, env);
}

static LamExp *compilePrinterForInt(ParserInfo I) {
    return makePrintInt(I);
}

static LamExp *compilePrinterForChar(ParserInfo I) {
    return makePrintChar(I);
}

static LamArgs *compilePrinterForTypeSigArgs(ParserInfo I, TcTypeSigArgs *args,
                                              TcEnv *env) {
    ENTER(compilePrinterForTypeSigArgs);
    if (args == NULL) {
        LEAVE(compilePrinterForTypeSigArgs);
        return NULL;
    }
    LamArgs *next = compilePrinterForTypeSigArgs(I, args->next, env);
    int save = PROTECT(next);
    LamExp *this = compilePrinter(I, args->type, env);
    PROTECT(this);
    LamArgs *res = newLamArgs(I, this, next);
    UNPROTECT(save);
    LEAVE(compilePrinterForTypeSigArgs);
    return res;
}

static LamArgs *compilePrinterForTupleArgs(ParserInfo I, TcTypeArray *tuple, TcEnv *env) {
    LamArgs *res = NULL;
    int save = PROTECT(res);
    for (int i = tuple->size; i > 0; i--) {
        int index = i - 1;
        LamExp *this = compilePrinter(I, tuple->entries[index], env);
        PROTECT(this);
        res = newLamArgs(I, this, res);
        PROTECT(res);
    }
    UNPROTECT(save);
    return res;
}

static LamExp *compilePrinterForString(ParserInfo I) {
    HashSymbol *name = newSymbol("print$string");
    return newLamExp_Var(I, name);
}

static TcEnv *getNsEnv(int index, TcEnv *env) {
    if (index == NS_GLOBAL) {
        return env;
    }
    TcType *currentNs = NULL;
    getFromTcEnv(env, nameSpaceSymbol(), &currentNs);
#ifdef SAFETY_CHECKS
    if (currentNs == NULL) {
        cant_happen("cannot find current nameSpace");
    }
#endif
    if (currentNs->val.nsid == index) {
        return env;
    }
    TcType *res = lookupNsRef(index, env);
    return res->val.env;
}

static LamExp *compilePrinterForTypeSig(ParserInfo I, TcTypeSig *typeSig, TcEnv *env) {
    IFDEBUG(printTcTypeSig(typeSig, 0));
    if (typeSig->name == listSymbol()) {
        if (typeSig->args
            && typeSig->args->type->type == TCTYPE_TYPE_CHARACTER) {
            return compilePrinterForString(I);
        }
    }
    HashSymbol *name = makePrintName("print$", typeSig->name->name);
    TcEnv *nsEnv = getNsEnv(typeSig->ns, env);
    if (!getFromTcEnv(nsEnv, name, NULL)) {
        return makeVarExpr(I, "__print__");
    }
    LamExp *exp = newLamExp_Var(I, name);
    int save = PROTECT(exp);
    if (env != nsEnv) {
        LamLookup *lookup = newLamLookup(I, typeSig->ns, NULL, exp);
        PROTECT(lookup);
        exp = newLamExp_Lookup(I, lookup);
        PROTECT(exp);
    }
    LamArgs *args = compilePrinterForTypeSigArgs(I, typeSig->args, env);
    PROTECT(args);
    int nargs = countLamArgs(args);
    if (nargs == 0) {
        UNPROTECT(save);
        return exp;
    }
    LamApply *apply = newLamApply(I, exp, args);
    PROTECT(apply);
    LamExp *res = newLamExp_Apply(I, apply);
    UNPROTECT(save);
    return res;
}

static LamExp *compilePrinterForTuple(ParserInfo I, TcTypeArray *tuple, TcEnv *env) {
    ENTER(compilePrinterForTuple);
    if (tuple->size < 5) {
        char buf[64];
        sprintf(buf, "print$tuple_%d", tuple->size);
        LamExp *exp = makeVarExpr(I, buf);
        if (tuple->size == 0) {
            LEAVE(compilePrinterForTuple);
            return exp;
        }
        int save = PROTECT(exp);
        LamArgs *args = compilePrinterForTupleArgs(I, tuple, env);
        PROTECT(args);
        LamApply *apply = newLamApply(I, exp, args);
        PROTECT(apply);
        LamExp *res = newLamExp_Apply(I, apply);
        UNPROTECT(save);
        IFDEBUG(ppLamExp(res));
        LEAVE(compilePrinterForTuple);
        return res;
    } else {
        LEAVE(compilePrinterForTuple);
        return makeVarExpr(I, "__print__");
    }
}
