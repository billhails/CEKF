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

static LamLetRecBindings *makePrintFunction(ParserInfo I, LamTypeDef *typeDef,
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

    next = makePrintFunction(CPI(typeDefs->typeDef), typeDefs->typeDef, next, env, inPreamble);

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

static LamExp *printArgVar(ParserInfo I) {
    HashSymbol *name = printArgSymbol();
    return newLamExp_Var(I, name);
}

static LamVarList *makeLastArg(ParserInfo I) {
    HashSymbol *name = printArgSymbol();
    return newLamVarList(I, name, NULL);
}

static LamVarList *makePrintTypeFunctionArgs(ParserInfo I, LamTypeArgs *args) {
    if (args == NULL)
        return makeLastArg(I);
    LamVarList *next = makePrintTypeFunctionArgs(I, args->next);
    int save = PROTECT(next);
    HashSymbol *name = makePrintName("print", args->name->name);
    LamVarList *res = newLamVarList(I, name, next);
    UNPROTECT(save);
    return res;
}

static LamExp *makeNullList(ParserInfo I) {
    LamConstruct *nil = newLamConstruct(I, nilSymbol(), 0, NULL);
    int save = PROTECT(nil);
    LamExp *res = newLamExp_Construct(I, nil);
    UNPROTECT(save);
    return res;
}

static LamExp *makeCharList(ParserInfo I, char c, LamExp *tail) {
    LamExp *character =
        newLamExp_Character(I, c);
    int save = PROTECT(character);
    LamList *args = newLamList(I, tail, NULL);
    PROTECT(args);
    args = newLamList(I, character, args);
    PROTECT(args);
    LamConstruct *cons = newLamConstruct(I, consSymbol(), 1, args);
    PROTECT(cons);
    LamExp *res =
        newLamExp_Construct(I, cons);
    UNPROTECT(save);
    return res;
}

LamExp *stringToLamList(ParserInfo I, char *name) {
    if (*name == 0) {
        return makeNullList(I);
    }
    LamExp *next = stringToLamList(I, name + 1);
    int save = PROTECT(next);
    LamExp *this = makeCharList(I, *name, next);
    UNPROTECT(save);
    return this;
}

static LamExp *putsExp(ParserInfo I, LamExp *string) {
    HashSymbol *sym = putsSymbol();
    LamExp *puts = newLamExp_Var(I, sym);
    int save = PROTECT(puts);
    LamList *args = newLamList(I, string, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(I, puts, args);
    PROTECT(apply);
    LamExp *res = newLamExp_Apply(I, apply);
    UNPROTECT(save);
    return res;
}

static LamExp *makePutsString(ParserInfo I, char *str) {
    LamExp *string = stringToLamList(I, str);
    int save = PROTECT(string);
    LamExp *res = putsExp(I, string);
    UNPROTECT(save);
    return res;
}

static LamExp *makePlainMatchBody(ParserInfo I, LamTypeConstructor *constructor) {
    LamExp *string = stringToLamList(I, constructor->name->name);
    int save = PROTECT(string);
    LamExp *puts = putsExp(I, string);
    UNPROTECT(save);
    return puts;
}

static LamExp *makePrintAccessor(ParserInfo I, int index, LamTypeConstructorInfo *info) {
    LamExp *printArg = printArgVar(I);
    int save = PROTECT(printArg);
    LamDeconstruct *dec =
        newLamDeconstruct(I, info->type->name, info->nsid, index, printArg);
    PROTECT(dec);
    LamExp *res =
        newLamExp_Deconstruct(I, dec);
    UNPROTECT(save);
    return res;
}

LamExp *makeSymbolExpr(ParserInfo I, char *name) {
    HashSymbol *symbol = newSymbol(name);
    LamExp *exp = newLamExp_Var(I, symbol);
    return exp;
}

LamExp *makePrintInt(ParserInfo I) {
    return makeSymbolExpr(I, "print$int");
}

LamExp *makePrintChar(ParserInfo I) {
    return makeSymbolExpr(I, "print$char");
}

static LamExp *makePrintVar(ParserInfo I, HashSymbol *var) {
    HashSymbol *name = makePrintName("print", var->name);
    LamExp *exp = newLamExp_Var(I, name);
    return exp;
}

static LamExp *makePrinter(ParserInfo I, LamTypeConstructorType *arg);

static LamList *makePrintArgs(ParserInfo I, LamTypeConstructorArgs *args) {
    if (args == NULL)
        return NULL;
    LamList *next = makePrintArgs(I, args->next);
    int save = PROTECT(next);
    LamExp *printer = makePrinter(I, args->arg);
    PROTECT(printer);
    LamList *this = newLamList(I, printer, next);
    UNPROTECT(save);
    return this;
}

static bool functionIsList(LamLookupOrSymbol *los) {
    switch (los->type) {
        case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
            return los->val.symbol == listSymbol();
        case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:
            return false;
        default:
            cant_happen("unrecognized %s", lamLookupOrSymbolTypeName(los->type));
    }
}

static char *getUnderlyingFunctionName(LamLookupOrSymbol *los) {
    switch (los->type) {
        case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
            return los->val.symbol->name;
        case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:
            return los->val.lookup->symbol->name;
        default:
            cant_happen("unrecognized %s", lamLookupOrSymbolTypeName(los->type));
    }
}

static LamExp *wrapTypeFunction(ParserInfo I, LamExp *res, LamLookupOrSymbol *los) {
    if (los->type == LAMLOOKUPORSYMBOL_TYPE_LOOKUP) {
        LamLookupSymbol *ls = los->val.lookup;
        LamLookup *llu = newLamLookup(I, ls->nsid, ls->nsSymbol, res);
        int save = PROTECT(llu);
        res = newLamExp_Lookup(I, llu);
        UNPROTECT(save);
    }
    return res;
}

static LamExp *makePrintType(ParserInfo I, LamTypeFunction *function) {
    if (functionIsList(function->name)) {
        if (function->args
            && function->args->arg->type ==
            LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER) {
            HashSymbol *name = newSymbol("print$string");
            return newLamExp_Var(I, name);
        }
    }
    HashSymbol *name = makePrintName("print$", getUnderlyingFunctionName(function->name));
    LamExp *exp = newLamExp_Var(I, name);
    int save = PROTECT(exp);
    exp = wrapTypeFunction(I, exp, function->name);
    REPLACE_PROTECT(save, exp);
    LamList *args = makePrintArgs(I, function->args);
    PROTECT(args);
    int nargs = countLamList(args);
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

static LamExp *makePrintTuple(ParserInfo I, LamTypeConstructorArgs *tuple) {
    int size = countLamTypeConstructorArgs(tuple);
    HashSymbol *name = NULL;
    if (size <= 4) {
        char buf[64];
        sprintf(buf, "%d", size);
        name = makePrintName("print$tuple$", buf);
    } else {
        name = newSymbol("print$");
        LamExp *exp = newLamExp_Var(I, name);
        return exp;
    }
    LamExp *exp = newLamExp_Var(I, name);
    int save = PROTECT(exp);
    LamList *args = makePrintArgs(I, tuple);
    PROTECT(args);
    LamApply *apply = newLamApply(I, exp, args);
    PROTECT(apply);
    LamExp *res = newLamExp_Apply(I, apply);
    UNPROTECT(save);
    return res;
}

static LamExp *makePrinter(ParserInfo I, LamTypeConstructorType *arg) {
    LamExp *printer = NULL;
    switch (arg->type) {
        case LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER:
            printer = makePrintInt(I);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER:
            printer = makePrintChar(I);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_VAR:
            printer = makePrintVar(I, arg->val.var);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
            printer = makePrintType(I, arg->val.function);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_TUPLE:
            printer = makePrintTuple(I, arg->val.tuple);
            break;
        default:
            cant_happen("unrecognised type %s in makePrinter", lamTypeConstructorTypeTypeName(arg->type));
    }
    return printer;
}

static LamExp *makePrintConstructorArg(ParserInfo I, LamTypeConstructorType *arg,
                                       LamTypeConstructorInfo *info,
                                       int index) {
    LamExp *accessor = makePrintAccessor(I, index, info);
    int save = PROTECT(accessor);
    LamExp *printer = makePrinter(I, arg);
    PROTECT(printer);
    LamList *args = newLamList(I, accessor, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(I, printer, args);
    PROTECT(apply);
    LamExp *res = newLamExp_Apply(I, apply);
    UNPROTECT(save);
    return res;
}

static HashSymbol *findNthTag(int index, LamTypeTags *tags) {
    if (tags == NULL) {
        cant_happen("reached end of tags");
    }
    if (index == 0) {
        return tags->tag;
    }
    return findNthTag(index - 1, tags->next);
}

static LamSequence *makeVecMatchParts(ParserInfo I, int index, LamTypeConstructorArgs *args,
                                      LamTypeConstructorInfo *info,
                                      LamSequence *tail) {
    if (args == NULL)
        return tail;
    LamSequence *next = makeVecMatchParts(I, index + 1, args->next, info, tail);
    int save = PROTECT(next);
    LamExp *exp = makePrintConstructorArg(I, args->arg, info, index + 1);
    PROTECT(exp);
    if (next != tail) {
        LamExp *comma = makePutsString(I, ", ");
        PROTECT(comma);
        next = newLamSequence(I, comma, next);
        PROTECT(next);
    }
    LamSequence *res = newLamSequence(I, exp, next);
    PROTECT(res);
    if (info->tags != NULL) {
        HashSymbol *tag = findNthTag(index, info->tags);
        LamExp *colon = makePutsString(I, ": ");
        PROTECT(colon);
        res = newLamSequence(I, colon, res);
        PROTECT(res);
        LamExp *name = makePutsString(I, tag->name);
        PROTECT(name);
        res = newLamSequence(I, name, res);
        PROTECT(next);
    }
    UNPROTECT(save);
    return res;
}

static LamExp *makeVecMatchBody(ParserInfo I, LamTypeConstructorInfo *info) {
    LamTypeConstructor *constructor = info->type;
    LamExp *header = makePlainMatchBody(I, constructor);
    int save = PROTECT(header);
    bool isStruct = info->tags != NULL;
    LamExp *open = makePutsString(I, isStruct ? "{ " : "(");
    PROTECT(open);
    LamExp *close = makePutsString(I, isStruct ? " }" : ")");
    PROTECT(close);
    LamSequence *seq = newLamSequence(I, close, NULL);
    PROTECT(seq);
    seq = makeVecMatchParts(I, 0, constructor->args, info, seq);
    PROTECT(seq);
    seq = newLamSequence(I, open, seq);
    PROTECT(seq);
    seq = newLamSequence(I, header, seq);
    PROTECT(seq);
    LamExp *res = newLamExp_List(I, seq);
    UNPROTECT(save);
    return res;
}

static LamMatchList *makePlainMatchList(ParserInfo I, LamTypeConstructorList *constructors,
                                        LamContext *env) {
    if (constructors == NULL)
        return NULL;
    LamMatchList *next = makePlainMatchList(I, constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info =
        lookupConstructorInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen
            ("cannot find info for type constructor %s in makePlainMatchList",
             constructors->constructor->name->name);
    }
    LamIntList *matches = newLamIntList(I, info->index, info->type->name, info->nsid, NULL);
    PROTECT(matches);
    LamExp *body = makePlainMatchBody(I, constructors->constructor);
    PROTECT(body);
    LamMatchList *res = newLamMatchList(I, matches, body, next);
    UNPROTECT(save);
    return res;
}

static LamMatchList *makeTagMatchList(ParserInfo I, LamTypeConstructorList *constructors,
                                      LamContext *env) {
    if (constructors == NULL)
        return NULL;
    LamMatchList *next = makeTagMatchList(I, constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info =
        lookupConstructorInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen
            ("cannot find info for type constructor %s in makeTagMatchList",
             constructors->constructor->name->name);
    }
    LamIntList *matches = newLamIntList(I, info->index, info->type->name, info->nsid, NULL);
    PROTECT(matches);
    LamExp *body = NULL;
    if (info->arity > 0) {
        body = makeVecMatchBody(I, info);
    } else {
        body = makePlainMatchBody(I, constructors->constructor);
    }
    PROTECT(body);
    LamMatchList *res = newLamMatchList(I, matches, body, next);
    UNPROTECT(save);
    return res;
}

static LamMatch *makePlainMatch(ParserInfo I, LamTypeConstructorList *constructors,
                                LamContext *env) {
    LamMatchList *cases = makePlainMatchList(I, constructors, env);
    int save = PROTECT(cases);
    LamExp *var = printArgVar(I);
    PROTECT(var);
    LamMatch *res = newLamMatch(I, var, cases);
    UNPROTECT(save);
    return res;
}

static LamMatch *makeTagMatch(ParserInfo I, LamTypeConstructorList *constructors,
                              LamContext *env) {
    LamMatchList *cases = makeTagMatchList(I, constructors, env);
    int save = PROTECT(cases);
    LamExp *var = printArgVar(I);
    PROTECT(var);
    LamExp *prim = newLamExp_Tag(I, var);
    PROTECT(prim);
    LamMatch *res = newLamMatch(I, prim, cases);
    UNPROTECT(save);
    return res;
}

static LamExp *makeFunctionBody(ParserInfo I, LamTypeConstructorList *constructors,
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
        match = makeTagMatch(I, constructors, env);
    } else {
        match = makePlainMatch(I, constructors, env);
    }
    int save = PROTECT(match);
    LamExp *res = newLamExp_Match(I, match);
    PROTECT(res);
    // print functions should all return their argument
    LamExp *ret = printArgVar(I);
    PROTECT(ret);
    LamSequence *seq = newLamSequence(I, ret, NULL);
    PROTECT(seq);
    seq = newLamSequence(I, res, seq);
    PROTECT(seq);
    res = newLamExp_List(I, seq);
    UNPROTECT(save);
    return res;
}

static bool userDefined(HashSymbol *printName, LamLetRecBindings *bindings) {
    if (bindings == NULL) return false;
    if (bindings->var == printName) return true;
    return userDefined(printName, bindings->next);
}

static LamLetRecBindings *makePrintTypeFunction(ParserInfo I, LamTypeDef *typeDef,
                                                LamContext *env,
                                                LamLetRecBindings *next) {
    HashSymbol *name = makePrintName("print$", typeDef->type->name->name);
    if (userDefined(name, next)) {
        return next;
    }
    LamVarList *args = makePrintTypeFunctionArgs(I, typeDef->type->args);
    int save = PROTECT(args);
    LamExp *body = makeFunctionBody(I, typeDef->constructors, env);
    PROTECT(body);
    LamLam *lam = newLamLam(I, args, body);
    PROTECT(lam);
    LamExp *val = newLamExp_Lam(I, lam);
    PROTECT(val);
    LamLetRecBindings *res = newLamLetRecBindings(I, name, val, next);
    UNPROTECT(save);
    return res;
}

static LamLetRecBindings *makePrintFunction(ParserInfo I, LamTypeDef *typeDef,
                                            LamLetRecBindings *next,
                                            LamContext *env,
                                            bool inPreamble) {
    if (inPreamble && isListType(typeDef->type)) {
        // print$list is hand-coded in the preamble
        return next;
    }
    return makePrintTypeFunction(I, typeDef, env, next);
}
