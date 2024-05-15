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
 * print function generator
 * invoked during lambda conversion
 * generates a print function for each typedef
 */

// print function generator, compiler and run-time code

#include <stdio.h>
#include "print_generator.h"
#include "cekf.h"
#include "common.h"
#include "lambda.h"
#include "lambda_helper.h"
#include "symbol.h"
#include "symbols.h"

#ifdef DEBUG_PRINT_GENERATOR
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static LamLetRecBindings *makePrintFunction(LamTypeDef *typeDef,
                                            LamLetRecBindings *next,
                                            LamContext *env, bool inPreamble);

LamLetRecBindings *makePrintFunctions(LamTypeDefList *typeDefs,
                                      LamLetRecBindings *next,
                                      LamContext *env, bool inPreamble) {
    ENTER(makePrintFunctions);
    if (typeDefs == NULL) {
        LEAVE(makePrintFunctions);
        return next;
    }
    next = makePrintFunctions(typeDefs->next, next, env, inPreamble);
    int save = PROTECT(next);

    next = makePrintFunction(typeDefs->typeDef, next, env, inPreamble);

    UNPROTECT(save);
    LEAVE(makePrintFunctions);
    return next;
}

static bool isListType(LamType *type) {
    return type->name == listSymbol();
}

HashSymbol *makePrintName(char *prefix, char *name) {
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
        FREE_ARRAY(char, buf, size);
    }
    return res;
}

static HashSymbol *printArgSymbol(void) {
    static HashSymbol *res = NULL;
    if (res == NULL)
        res = newSymbol("thing");
    return res;
}

static LamExp *printArgVar() {
    HashSymbol *name = printArgSymbol();
    return newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
}

static LamVarList *makeLastArg(void) {
    HashSymbol *name = printArgSymbol();
    return newLamVarList(name, NULL);
}

static LamVarList *makePrintTypeFunctionArgs(LamTypeArgs *args) {
    if (args == NULL)
        return makeLastArg();
    LamVarList *next = makePrintTypeFunctionArgs(args->next);
    int save = PROTECT(next);
    HashSymbol *name = makePrintName("print", args->name->name);
    LamVarList *res = newLamVarList(name, next);
    UNPROTECT(save);
    return res;
}

static LamExp *makeNullList() {
    LamConstruct *nil = newLamConstruct(nilSymbol(), 0, NULL);
    int save = PROTECT(nil);
    LamExp *res = newLamExp(LAMEXP_TYPE_CONSTRUCT, LAMEXP_VAL_CONSTRUCT(nil));
    UNPROTECT(save);
    return res;
}

static LamExp *makeCharList(char c, LamExp *tail) {
    LamExp *character =
        newLamExp(LAMEXP_TYPE_CHARACTER, LAMEXP_VAL_CHARACTER(c));
    int save = PROTECT(character);
    LamList *args = newLamList(tail, NULL);
    PROTECT(args);
    args = newLamList(character, args);
    PROTECT(args);
    LamConstruct *cons = newLamConstruct(consSymbol(), 1, args);
    PROTECT(cons);
    LamExp *res =
        newLamExp(LAMEXP_TYPE_CONSTRUCT, LAMEXP_VAL_CONSTRUCT(cons));
    UNPROTECT(save);
    return res;
}

static LamExp *stringToList(char *name) {
    if (*name == 0) {
        return makeNullList();
    }
    LamExp *next = stringToList(name + 1);
    int save = PROTECT(next);
    LamExp *this = makeCharList(*name, next);
    UNPROTECT(save);
    return this;
}

static LamExp *putsExp(LamExp *string) {
    HashSymbol *sym = putsSymbol();
    LamExp *puts = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(sym));
    int save = PROTECT(puts);
    LamList *args = newLamList(string, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(puts, args);
    PROTECT(apply);
    LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return res;
}

static LamExp *makePutsString(char *str) {
    LamExp *string = stringToList(str);
    int save = PROTECT(string);
    LamExp *res = putsExp(string);
    UNPROTECT(save);
    return res;
}

static LamExp *makePlainMatchBody(LamTypeConstructor *constructor) {
    LamExp *string = stringToList(constructor->name->name);
    int save = PROTECT(string);
    LamExp *puts = putsExp(string);
    UNPROTECT(save);
    return puts;
}

static LamExp *makePrintAccessor(int index, LamTypeConstructorInfo *info) {
    LamExp *printArg = printArgVar();
    int save = PROTECT(printArg);
    LamDeconstruct *dec =
        newLamDeconstruct(info->type->name, index, printArg);
    PROTECT(dec);
    LamExp *res =
        newLamExp(LAMEXP_TYPE_DECONSTRUCT, LAMEXP_VAL_DECONSTRUCT(dec));
    UNPROTECT(save);
    return res;
}

LamExp *makeSymbolExpr(char *name) {
    HashSymbol *symbol = newSymbol(name);
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(symbol));
    return exp;
}

LamExp *makePrintInt() {
    return makeSymbolExpr("print$int");
}

LamExp *makePrintChar() {
    return makeSymbolExpr("print$char");
}

static LamExp *makePrintVar(HashSymbol *var) {
    HashSymbol *name = makePrintName("print", var->name);
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    return exp;
}

static LamExp *makePrinter(LamTypeConstructorType *arg);

static LamList *makePrintArgs(LamTypeConstructorArgs *args) {
    if (args == NULL)
        return NULL;
    LamList *next = makePrintArgs(args->next);
    int save = PROTECT(next);
    LamExp *printer = makePrinter(args->arg);
    PROTECT(printer);
    LamList *this = newLamList(printer, next);
    UNPROTECT(save);
    return this;
}

static LamExp *makePrintType(LamTypeFunction *function) {
    if (function->name == listSymbol()) {
        if (function->args
            && function->args->arg->type ==
            LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER) {
            HashSymbol *name = newSymbol("print$string");
            return newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
        }
    }
    HashSymbol *name = makePrintName("print$", function->name->name);
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    int save = PROTECT(exp);
    LamList *args = makePrintArgs(function->args);
    PROTECT(args);
    int nargs = countLamList(args);
    if (nargs == 0) {
        UNPROTECT(save);
        return exp;
    }
    LamApply *apply = newLamApply(exp, args);
    PROTECT(apply);
    LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return res;
}

static LamExp *makePrintTuple(LamTypeConstructorArgs *tuple) {
    int size = countLamTypeConstructorArgs(tuple);
    HashSymbol *name = NULL;
    if (size <= 4) {
        char buf[64];
        sprintf(buf, "%d", size);
        name = makePrintName("print$tuple$", buf);
    } else {
        name = newSymbol("print$");
        LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
        return exp;
    }
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    int save = PROTECT(exp);
    LamList *args = makePrintArgs(tuple);
    PROTECT(args);
    LamApply *apply = newLamApply(exp, args);
    PROTECT(apply);
    LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return res;
}

static LamExp *makePrinter(LamTypeConstructorType *arg) {
    LamExp *printer = NULL;
    switch (arg->type) {
        case LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER:
            printer = makePrintInt();
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER:
            printer = makePrintChar();
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_VAR:
            printer = makePrintVar(arg->val.var);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
            printer = makePrintType(arg->val.function);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_TUPLE:
            printer = makePrintTuple(arg->val.tuple);
            break;
        default:
            cant_happen("unrecognised type %s in makePrinter", lamTypeConstructorTypeTypeName(arg->type));
    }
    return printer;
}

static LamExp *makePrintConstructorArg(LamTypeConstructorType *arg,
                                       LamTypeConstructorInfo *info,
                                       int index) {
    LamExp *accessor = makePrintAccessor(index, info);
    int save = PROTECT(accessor);
    LamExp *printer = makePrinter(arg);
    PROTECT(printer);
    LamList *args = newLamList(accessor, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(printer, args);
    PROTECT(apply);
    LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return res;
}

static LamSequence *makeVecMatchParts(int index, LamTypeConstructorArgs *args,
                                      LamTypeConstructorInfo *info,
                                      LamSequence *tail) {
    if (args == NULL)
        return tail;
    LamSequence *next = makeVecMatchParts(index + 1, args->next, info, tail);
    int save = PROTECT(next);
    LamExp *exp = makePrintConstructorArg(args->arg, info, index + 1);
    PROTECT(exp);
    if (next != tail) {
        LamExp *comma = makePutsString(", ");
        PROTECT(comma);
        next = newLamSequence(comma, next);
        PROTECT(next);
    }
    LamSequence *res = newLamSequence(exp, next);
    UNPROTECT(save);
    return res;
}

static LamExp *makeVecMatchBody(LamTypeConstructorInfo *info) {
    LamTypeConstructor *constructor = info->type;
    LamExp *header = makePlainMatchBody(constructor);
    int save = PROTECT(header);
    LamExp *open = makePutsString("(");
    PROTECT(open);
    LamExp *close = makePutsString(")");
    PROTECT(close);
    LamSequence *seq = newLamSequence(close, NULL);
    int save2 = PROTECT(seq);
    seq = makeVecMatchParts(0, constructor->args, info, seq);
    REPLACE_PROTECT(save2, seq);
    seq = newLamSequence(open, seq);
    REPLACE_PROTECT(save2, seq);
    seq = newLamSequence(header, seq);
    REPLACE_PROTECT(save2, seq);
    LamExp *res = newLamExp(LAMEXP_TYPE_LIST, LAMEXP_VAL_LIST(seq));
    UNPROTECT(save);
    return res;
}

static LamMatchList *makePlainMatchList(LamTypeConstructorList *constructors,
                                        LamContext *env) {
    if (constructors == NULL)
        return NULL;
    LamMatchList *next = makePlainMatchList(constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info =
        lookupConstructorInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen
            ("cannot find info for type constructor %s in makePlainMatchList",
             constructors->constructor->name->name);
    }
    LamIntList *matches = newLamIntList(info->index, info->type->name, NULL);
    PROTECT(matches);
    LamExp *body = makePlainMatchBody(constructors->constructor);
    PROTECT(body);
    LamMatchList *res = newLamMatchList(matches, body, next);
    UNPROTECT(save);
    return res;
}

static LamMatchList *makeTagMatchList(LamTypeConstructorList *constructors,
                                      LamContext *env) {
    if (constructors == NULL)
        return NULL;
    LamMatchList *next = makeTagMatchList(constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info =
        lookupConstructorInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen
            ("cannot find info for type constructor %s in makeTagMatchList",
             constructors->constructor->name->name);
    }
    LamIntList *matches = newLamIntList(info->index, info->type->name, NULL);
    PROTECT(matches);
    LamExp *body = NULL;
    if (info->arity > 0) {
        body = makeVecMatchBody(info);
    } else {
        body = makePlainMatchBody(constructors->constructor);
    }
    PROTECT(body);
    LamMatchList *res = newLamMatchList(matches, body, next);
    UNPROTECT(save);
    return res;
}

static LamMatch *makePlainMatch(LamTypeConstructorList *constructors,
                                LamContext *env) {
    LamMatchList *cases = makePlainMatchList(constructors, env);
    int save = PROTECT(cases);
    LamExp *var = printArgVar();
    PROTECT(var);
    LamMatch *res = newLamMatch(var, cases);
    UNPROTECT(save);
    return res;
}

static LamMatch *makeTagMatch(LamTypeConstructorList *constructors,
                              LamContext *env) {
    LamMatchList *cases = makeTagMatchList(constructors, env);
    int save = PROTECT(cases);
    LamExp *var = printArgVar();
    PROTECT(var);
    LamExp *prim = newLamExp(LAMEXP_TYPE_TAG, LAMEXP_VAL_TAG(var));
    PROTECT(prim);
    LamMatch *res = newLamMatch(prim, cases);
    UNPROTECT(save);
    return res;
}

static LamExp *makeFunctionBody(LamTypeConstructorList *constructors,
                                LamContext *env) {
    LamTypeConstructorInfo *info =
        lookupConstructorInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen
            ("cannot find info for type constructor %s in makeFunctionBody",
             constructors->constructor->name->name);
    }
    LamMatch *match = NULL;
    if (info->needsVec) {
        match = makeTagMatch(constructors, env);
    } else {
        match = makePlainMatch(constructors, env);
    }
    int save = PROTECT(match);
    LamExp *res = newLamExp(LAMEXP_TYPE_MATCH, LAMEXP_VAL_MATCH(match));
    PROTECT(res);
    // print functions should all return their argument
    LamExp *ret = printArgVar();
    PROTECT(ret);
    LamSequence *seq = newLamSequence(ret, NULL);
    PROTECT(seq);
    seq = newLamSequence(res, seq);
    PROTECT(seq);
    res = newLamExp(LAMEXP_TYPE_LIST, LAMEXP_VAL_LIST(seq));
    UNPROTECT(save);
    return res;
}

static LamLetRecBindings *makePrintTypeFunction(LamTypeDef *typeDef,
                                                LamContext *env,
                                                LamLetRecBindings *next) {
    HashSymbol *name = makePrintName("print$", typeDef->type->name->name);
    LamVarList *args = makePrintTypeFunctionArgs(typeDef->type->args);
    int save = PROTECT(args);
    LamExp *body = makeFunctionBody(typeDef->constructors, env);
    PROTECT(body);
    LamLam *lam = newLamLam(args, body);
    PROTECT(lam);
    LamExp *val = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(lam));
    PROTECT(val);
    LamLetRecBindings *res = newLamLetRecBindings(name, val, next);
    UNPROTECT(save);
    return res;
}

static LamLetRecBindings *makePrintFunction(LamTypeDef *typeDef,
                                            LamLetRecBindings *next,
                                            LamContext *env,
                                            bool inPreamble) {
    if (inPreamble && isListType(typeDef->type)) {
        // print$list is hand-coded in the preamble
        return next;
    } else {
        return makePrintTypeFunction(typeDef, env, next);
    }
}
