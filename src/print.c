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

static LamExp *makePrintAccessor(int index) {
    LamExp *printArg = printArgVar();
    int save = PROTECT(printArg);
    LamExp *indexVal = newLamExp(LAMEXP_TYPE_STDINT, LAMEXP_VAL_STDINT(index + 1));
    PROTECT(indexVal);
    LamPrimApp *primApp = newLamPrimApp(LAMPRIMOP_TYPE_VEC, indexVal, printArg);
    PROTECT(primApp);
    LamExp *res = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(primApp));
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
            cant_happen("unrecognised type %d in makePrintConstructorArg", arg->type);
    }
    return printer;
}

static LamExp *makePrintConstructorArg(LamTypeConstructorType *arg, int index) {
    LamExp *accessor = makePrintAccessor(index);
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
    LamExp *exp = makePrintConstructorArg(args->arg, index);
    PROTECT(exp);
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

static LamMatchList *makeVecMatchList(LamTypeConstructorList *constructors, LamContext *env) {
    if (constructors == NULL) return NULL;
    LamMatchList *next = makeVecMatchList(constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info = lookupInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen("cannot find info for type constructor %s in makeVecMatchList",
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

static LamMatch *makeVecMatch(LamTypeConstructorList *constructors, LamContext *env) {
    LamMatchList *cases = makeVecMatchList(constructors, env);
    int save = PROTECT(cases);
    LamExp *zero = newLamExp(LAMEXP_TYPE_STDINT, LAMEXP_VAL_STDINT(0));
    PROTECT(zero);
    LamExp *var = printArgVar();
    PROTECT(var);
    LamPrimApp *vec = newLamPrimApp(LAMPRIMOP_TYPE_VEC, zero, var);
    PROTECT(vec);
    LamExp *prim = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(vec));
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
        match = makeVecMatch(constructors, env);
    } else {
        match = makePlainMatch(constructors, env);
    }
    int save = PROTECT(match);
    LamExp *res = newLamExp(LAMEXP_TYPE_MATCH, LAMEXP_VAL_MATCH(match));
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
