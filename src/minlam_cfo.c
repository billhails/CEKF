/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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
 * Minimal AST after desugaring
 * Generated from src/minlam.yaml by tools/generate.py
 */

#include "minlam_cfo.h"
#include "memory.h"
#include "minlam_helper.h"
#include "utils_helper.h"

#ifdef DEBUG_MINLAM_COUNT
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

typedef struct VisitorContext {
    SymbolSet *symbols;
    IntMap *counts;
} VisitorContext;

static void cfoMinLam(MinLam *, VisitorContext);
static void cfoMinAnnotatedVar(MinAnnotatedVar *, VisitorContext);
static void cfoMinExprList(MinExprList *, VisitorContext);
static void cfoMinPrimApp(MinPrimApp *, VisitorContext);
static void cfoMinApply(MinApply *, VisitorContext);
static void cfoMinIff(MinIff *, VisitorContext);
static void cfoMinCond(MinCond *, VisitorContext);
static void cfoMinIntCondCases(MinIntCondCases *, VisitorContext);
static void cfoMinCharCondCases(MinCharCondCases *, VisitorContext);
static void cfoMinMatch(MinMatch *, VisitorContext);
static void cfoMinMatchList(MinMatchList *, VisitorContext);
static void cfoMinLetRec(MinLetRec *, VisitorContext);
static void cfoMinBindings(MinBindings *, VisitorContext);
static void cfoMinAmb(MinAmb *, VisitorContext);
static void cfoMinExp(MinExp *, VisitorContext);
static void cfoMinCondCases(MinCondCases *, VisitorContext);

///////
// API
///////

IntMap *countFreeOccurences(MinExp *node, SymbolSet *symbols) {
    VisitorContext context = {.counts = newIntMap(), .symbols = symbols};
    int save = PROTECT(context.counts);
    if (countSymbolSet(symbols) > 0) {
        Index i = 0;
        HashSymbol *symbol = NULL;
        while ((symbol = iterateSymbolSet(symbols, &i)) != NULL) {
            setIntMap(context.counts, symbol, 0);
        }
        cfoMinExp(node, context);
    }
    UNPROTECT(save);
    return context.counts;
}

///////////////////
// Helper Routines
///////////////////

static void testAndAdd(HashSymbol *symbol, VisitorContext context) {
    if (getSymbolSet(context.symbols, symbol)) {
        int count = 0;
        if (!getIntMap(context.counts, symbol, &count)) {
            cant_happen("unregistered var %s", symbol->name);
        }
        setIntMap(context.counts, symbol, count + 1);
    }
}

///////////////////////////
// Visitor implementations
///////////////////////////

static void cfoMinLam(MinLam *node, VisitorContext context) {
    ENTER(cfoMinLam);
    SymbolSet *args = symbolListToSet(node->args);
    int save = PROTECT(args);
    SymbolSet *newSymbols = differenceSymbolSet(context.symbols, args);
    PROTECT(newSymbols);
    context.symbols = newSymbols;
    cfoMinExp(node->exp, context);
    UNPROTECT(save);
    LEAVE(cfoMinLam);
}

static void cfoMinAnnotatedVar(MinAnnotatedVar *node, VisitorContext context) {
    ENTER(cfoMinAnnotatedVar);
    testAndAdd(node->var, context);
    LEAVE(cfoMinAnnotatedVar);
}

static void cfoMinExprList(MinExprList *node, VisitorContext context) {
    if (node == NULL)
        return;
    ENTER(cfoMinExprList);
    cfoMinExp(node->exp, context);
    cfoMinExprList(node->next, context);
    LEAVE(cfoMinExprList);
}

static void cfoMinPrimApp(MinPrimApp *node, VisitorContext context) {
    ENTER(cfoMinPrimApp);
    cfoMinExp(node->exp1, context);
    cfoMinExp(node->exp2, context);
    LEAVE(cfoMinPrimApp);
}

static void cfoMinApply(MinApply *node, VisitorContext context) {
    ENTER(cfoMinApply);
    cfoMinExp(node->function, context);
    cfoMinExprList(node->args, context);
    LEAVE(cfoMinApply);
}

static void cfoMinIff(MinIff *node, VisitorContext context) {
    ENTER(cfoMinIff);
    cfoMinExp(node->condition, context);
    cfoMinExp(node->consequent, context);
    cfoMinExp(node->alternative, context);
    LEAVE(cfoMinIff);
}

static void cfoMinCond(MinCond *node, VisitorContext context) {
    if (node == NULL)
        return;
    ENTER(cfoMinCond);
    cfoMinExp(node->value, context);
    cfoMinCondCases(node->cases, context);
    LEAVE(cfoMinCond);
}

static void cfoMinIntCondCases(MinIntCondCases *node, VisitorContext context) {
    if (node == NULL)
        return;
    ENTER(cfoMinIntCondCases);
    cfoMinExp(node->body, context);
    cfoMinIntCondCases(node->next, context);
    LEAVE(cfoMinIntCondCases);
}

static void cfoMinCharCondCases(MinCharCondCases *node,
                                VisitorContext context) {
    if (node == NULL)
        return;
    ENTER(cfoMinCharCondCases);
    cfoMinExp(node->body, context);
    cfoMinCharCondCases(node->next, context);
    LEAVE(cfoMinCharCondCases);
}

static void cfoMinMatch(MinMatch *node, VisitorContext context) {
    ENTER(cfoMinMatch);
    cfoMinExp(node->index, context);
    cfoMinMatchList(node->cases, context);
    LEAVE(cfoMinMatch);
}

static void cfoMinMatchList(MinMatchList *node, VisitorContext context) {
    if (node == NULL)
        return;
    ENTER(cfoMinMatchList);
    cfoMinExp(node->body, context);
    cfoMinMatchList(node->next, context);
    LEAVE(cfoMinMatchList);
}

static void cfoMinLetRec(MinLetRec *node, VisitorContext context) {
    ENTER(cfoMinLetRec);
    SymbolSet *bindings = getAllKeys(node->bindings);
    int save = PROTECT(bindings);
    context.symbols = differenceSymbolSet(context.symbols, bindings);
    REPLACE_PROTECT(save, context.symbols);
    cfoMinBindings(node->bindings, context);
    cfoMinExp(node->body, context);
    UNPROTECT(save);
    LEAVE(cfoMinLetRec);
}

static void cfoMinBindings(MinBindings *node, VisitorContext context) {
    if (node == NULL)
        return;
    ENTER(cfoMinBindings);
    cfoMinExp(node->val, context);
    cfoMinBindings(node->next, context);
    LEAVE(cfoMinBindings);
}

static void cfoMinAmb(MinAmb *node, VisitorContext context) {
    ENTER(cfoMinAmb);
    cfoMinExp(node->left, context);
    cfoMinExp(node->right, context);
    LEAVE(cfoMinAmb);
}

static void cfoMinExp(MinExp *node, VisitorContext context) {
    if (node == NULL)
        return;
    ENTER(cfoMinExp);
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        cfoMinAmb(variant, context);
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        cfoMinApply(variant, context);
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        MinExprList *variant = getMinExp_Args(node);
        cfoMinExprList(variant, context);
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
        MinAnnotatedVar *variant = getMinExp_Avar(node);
        cfoMinAnnotatedVar(variant, context);
        break;
    }
    case MINEXP_TYPE_BACK: {
        // void_ptr
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        // MaybeBigInt
        break;
    }
    case MINEXP_TYPE_BINDINGS: {
        // MinBindings
        MinBindings *variant = getMinExp_Bindings(node);
        cfoMinBindings(variant, context);
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        cfoMinExp(variant, context);
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        // character
        break;
    }
    case MINEXP_TYPE_COND: {
        // MinCond
        MinCond *variant = getMinExp_Cond(node);
        cfoMinCond(variant, context);
        break;
    }
    case MINEXP_TYPE_DONE: {
        // int
        break;
    }
    case MINEXP_TYPE_IFF: {
        // MinIff
        MinIff *variant = getMinExp_Iff(node);
        cfoMinIff(variant, context);
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        cfoMinLam(variant, context);
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        cfoMinLetRec(variant, context);
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        cfoMinExprList(variant, context);
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        cfoMinMatch(variant, context);
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        cfoMinPrimApp(variant, context);
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        cfoMinExprList(variant, context);
        break;
    }
    case MINEXP_TYPE_STDINT: {
        // int
        break;
    }
    case MINEXP_TYPE_VAR: {
        // HashSymbol
        testAndAdd(getMinExp_Var(node), context);
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %s", minExpTypeName(node->type));
    }
    LEAVE(cfoMinExp);
}

static void cfoMinCondCases(MinCondCases *node, VisitorContext context) {
    ENTER(cfoMinCondCases);
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        cfoMinIntCondCases(variant, context);
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        cfoMinCharCondCases(variant, context);
        break;
    }
    default:
        cant_happen("unrecognized MinCondCases type %s",
                    minCondCasesTypeName(node->type));
    }
    LEAVE(cfoMinCondCases);
}