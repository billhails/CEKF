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

// print function generator, compiler and run-time code

#include <stdio.h>
#include "print.h"
#include "cekf.h"
#include "common.h"
#include "lambda.h"
#include "lambda_helper.h"
#include "symbol.h"
#include "symbols.h"

#ifdef DEBUG_PRINT
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static void putVec(Vec *x);

static LamLetRecBindings *makePrintFunction(LamTypeDef *typeDef, LamLetRecBindings *next, LamContext *env, bool inPreamble);
static HashSymbol *makePrintName(char *prefix, char *name);

/****************************************************************
 * print function generator
 * invoked during lambda conversion
 * generates a print function for each typedef
 ****************************************************************/

LamLetRecBindings *makePrintFunctions(LamTypeDefList *typeDefs, LamLetRecBindings *next, LamContext *env, bool inPreamble) {
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

static HashSymbol *makePrintName(char *prefix, char *name) {
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

static int countLamVarList(LamVarList *list) {
    int res = 0;
    while (list != NULL) {
        list = list->next;
        res++;
    }
    return res;
}

static int countLamList(LamList *list) {
    int res = 0;
    while (list != NULL) {
        list = list->next;
        res++;
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
    if (args == NULL) return makeLastArg();
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
    LamExp *character = newLamExp(LAMEXP_TYPE_CHARACTER, LAMEXP_VAL_CHARACTER(c));
    int save = PROTECT(character);
    LamList *args = newLamList(tail, NULL);
    PROTECT(args);
    args = newLamList(character, args);
    PROTECT(args);
    LamConstruct *cons = newLamConstruct(consSymbol(), 1, args);
    PROTECT(cons);
    LamExp *res = newLamExp(LAMEXP_TYPE_CONSTRUCT, LAMEXP_VAL_CONSTRUCT(cons));
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
    LamApply *apply = newLamApply(puts, 1, args);
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
    LamDeconstruct *dec = newLamDeconstruct(info->type->name, index, printArg);
    PROTECT(dec);
    LamExp *res = newLamExp(LAMEXP_TYPE_DECONSTRUCT, LAMEXP_VAL_DECONSTRUCT(dec));
    UNPROTECT(save);
    return res;
}

static LamExp *makeSymbolExpr(char *name) {
    HashSymbol *symbol = newSymbol(name);
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(symbol));
    return exp;
}

static LamExp *makePrintInt() {
    return makeSymbolExpr("print$int");
}

static LamExp *makePrintChar() {
    return makeSymbolExpr("print$char");
}

static LamExp *makePrintVar(HashSymbol *var) {
    HashSymbol *name = makePrintName("print", var->name);
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    return exp;
}

static LamExp *makePrinter(LamTypeConstructorType *arg);

static LamList *makePrintArgs(LamTypeConstructorArgs *args) {
    if (args == NULL) return NULL;
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
        if (function->args && function->args->arg->type == LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER) {
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
    LamApply *apply = newLamApply(exp, nargs, args);
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
        default:
            cant_happen("unrecognised type %d in makePrinter", arg->type);
    }
    return printer;
}

static LamExp *makePrintConstructorArg(LamTypeConstructorType *arg, LamTypeConstructorInfo *info, int index) {
    LamExp *accessor = makePrintAccessor(index, info);
    int save = PROTECT(accessor);
    LamExp *printer = makePrinter(arg);
    PROTECT(printer);
    LamList *args = newLamList(accessor, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(printer, 1, args);
    PROTECT(apply);
    LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return res;
}

static LamSequence *makeVecMatchParts(int index, LamTypeConstructorArgs *args, LamTypeConstructorInfo *info, LamSequence *tail) {
    if (args == NULL) return tail;
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

static LamMatchList *makePlainMatchList(LamTypeConstructorList *constructors, LamContext *env) {
    if (constructors == NULL) return NULL;
    LamMatchList *next = makePlainMatchList(constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info = lookupInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen("cannot find info for type constructor %s in makePlainMatchList",
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

static LamMatchList *makeTagMatchList(LamTypeConstructorList *constructors, LamContext *env) {
    if (constructors == NULL) return NULL;
    LamMatchList *next = makeTagMatchList(constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info = lookupInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen("cannot find info for type constructor %s in makeTagMatchList",
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

static LamMatch *makePlainMatch(LamTypeConstructorList *constructors, LamContext *env) {
    LamMatchList *cases = makePlainMatchList(constructors, env);
    int save = PROTECT(cases);
    LamExp *var = printArgVar();
    PROTECT(var);
    LamMatch *res = newLamMatch(var, cases);
    UNPROTECT(save);
    return res;
}

static LamMatch *makeTagMatch(LamTypeConstructorList *constructors, LamContext *env) {
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

static LamExp *makeFunctionBody(LamTypeConstructorList *constructors, LamContext *env) {
    LamTypeConstructorInfo *info = lookupInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen("cannot find info for type constructor %s in makeFunctionBody",
                    constructors->constructor->name->name);
    }
    LamMatch *match = NULL;
    if (info->vec) {
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

static LamLetRecBindings *makePrintTypeFunction(LamTypeDef *typeDef, LamContext *env, LamLetRecBindings *next) {
    HashSymbol *name = makePrintName("print$", typeDef->type->name->name);
    LamVarList *args = makePrintTypeFunctionArgs(typeDef->type->args);
    int save = PROTECT(args);
    LamExp *body = makeFunctionBody(typeDef->constructors, env);
    PROTECT(body);
    LamLam *lam = newLamLam(countLamVarList(args), args, body);
    PROTECT(lam);
    LamExp *val = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(lam));
    PROTECT(val);
    LamLetRecBindings *res = newLamLetRecBindings(name, val, next);
    UNPROTECT(save);
    return res;
}

static LamLetRecBindings *makePrintFunction(LamTypeDef *typeDef, LamLetRecBindings *next, LamContext *env, bool inPreamble) {
    if (inPreamble && isListType(typeDef->type)) {
        return next;
    } else {
        return makePrintTypeFunction(typeDef, env, next);
    }
}

/****************************************************************
 * print function compiler
 * invoked during type checking
 * computes a print function that will print the given type
 ****************************************************************/
static LamExp *compilePrinterForFunction(TcFunction *function);
static LamExp *compilePrinterForPair(TcPair *pair);
static LamExp *compilePrinterForVar(TcVar *var, TcEnv *env);
static LamExp *compilePrinterForInt();
static LamExp *compilePrinterForChar();
static LamExp *compilePrinterForTypeDef(TcTypeDef *typeDef, TcEnv *env);

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
        case TCTYPE_TYPE_TYPEDEF:
            res = compilePrinterForTypeDef(type->val.typeDef, env);
            break;
        default:
            cant_happen("unrecognised TcType %d in compilePrinterForType", type->type);
    }
    return res;
}

static LamExp *compilePrinterForFunction(TcFunction *function __attribute__((unused))) {
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

static LamList *compilePrinterForTypeDefArgs(TcTypeDefArgs *args, TcEnv *env) {
    if (args == NULL) return NULL;
    LamList *next = compilePrinterForTypeDefArgs(args->next, env);
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

static LamExp *compilePrinterForTypeDef(TcTypeDef *typeDef, TcEnv *env) {
    if (typeDef->name == listSymbol()) {
        if (typeDef->args && typeDef->args->type->type == TCTYPE_TYPE_CHARACTER) {
            return compilePrinterForString();
        }
    }
    HashSymbol *name = makePrintName("print$", typeDef->name->name);
    if (!getFromTcEnv(env, name, NULL)) {
        return makeSymbolExpr("print$");
    }
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    int save = PROTECT(exp);
    LamList *args = compilePrinterForTypeDefArgs(typeDef->args, env);
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


/****************************************************************
 * run-time print code supporting putv
 * used when the type cannot be detrmined.
 ****************************************************************/

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
