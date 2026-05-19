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

#include "memory.h"
#include "minlam_helper.h"
#include "utils_helper.h"

#include "minlam_isSimple.h"

#ifdef DEBUG_MINLAM_ISSIMPLE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

typedef struct {
    SymbolSet *symbols;
    bool outer;
} Context;

static bool isSimpleMinLam(MinLam *, Context);
static bool isSimpleMinExprList(MinExprList *, Context);
static bool isSimpleMinPrimApp(MinPrimApp *, Context);
static bool isSimpleMinApply(MinApply *, Context);
static bool isSimpleMinIff(MinIff *, Context);
static bool isSimpleMinCond(MinCond *, Context);
static bool isSimpleMinIntCondCases(MinIntCondCases *, Context);
static bool isSimpleMinCharCondCases(MinCharCondCases *, Context);
static bool isSimpleMinMatch(MinMatch *, Context);
static bool isSimpleMinMatchList(MinMatchList *, Context);
static bool isSimpleMinLetRec(MinLetRec *, Context);
static bool isSimpleMinBindings(MinBindings *, Context);
static bool isSimpleMinAmb(MinAmb *, Context);
static bool isSimpleMinExp(MinExp *, Context);
static bool isSimpleMinCondCases(MinCondCases *, Context);
static bool isSimpleMinCs(MinExp *, Context);

///////////////////////////
// Visitor implementations
///////////////////////////

static bool isSimpleMinLam(MinLam *node, Context context) {
    if (node == NULL)
        return true;
    if (context.outer) {
        context.outer = false;
        return isSimpleMinExp(node->exp, context);
    } else {
        SymbolSet *bindings = symbolListToSet(node->args);
        int save = PROTECT(bindings);
        context.symbols = unionSymbolSet(context.symbols, bindings);
        PROTECT(context.symbols);
        bool result = isSimpleMinExp(node->exp, context);
        UNPROTECT(save);
        return result;
    }
}

static bool isSimpleMinExprList(MinExprList *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->exp, context) &&
           isSimpleMinExprList(node->next, context);
}

static bool isSimpleMinPrimApp(MinPrimApp *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->exp1, context) &&
           isSimpleMinExp(node->exp2, context);
}

static bool isSimpleMinApply(MinApply *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinCs(node->function, context) &&
           isSimpleMinExprList(node->args, context);
}

static bool isSimpleMinIff(MinIff *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->condition, context) &&
           isSimpleMinExp(node->consequent, context) &&
           isSimpleMinExp(node->alternative, context);
}

static bool isSimpleMinCond(MinCond *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->value, context) &&
           isSimpleMinCondCases(node->cases, context);
}

static bool isSimpleMinIntCondCases(MinIntCondCases *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->body, context) &&
           isSimpleMinIntCondCases(node->next, context);
}

static bool isSimpleMinCharCondCases(MinCharCondCases *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->body, context) &&
           isSimpleMinCharCondCases(node->next, context);
}

static bool isSimpleMinMatch(MinMatch *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->index, context) &&
           isSimpleMinMatchList(node->cases, context);
}

static bool isSimpleMinMatchList(MinMatchList *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->body, context) &&
           isSimpleMinMatchList(node->next, context);
}

static bool isSimpleMinLetRec(MinLetRec *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinBindings(node->bindings, context) &&
           isSimpleMinExp(node->body, context);
}

static bool isSimpleMinBindings(MinBindings *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->val, context) &&
           isSimpleMinBindings(node->next, context);
}

static bool isSimpleMinAmb(MinAmb *node, Context context) {
    if (node == NULL)
        return true;
    return isSimpleMinExp(node->left, context) &&
           isSimpleMinExp(node->right, context);
}

static bool isSimpleMinExp(MinExp *node, Context context) {
    if (node == NULL)
        return true;
    switch (node->type) {
    case MINEXP_TYPE_AMB:
        return isSimpleMinAmb(getMinExp_Amb(node), context);
    case MINEXP_TYPE_APPLY:
        return isSimpleMinApply(getMinExp_Apply(node), context);
    case MINEXP_TYPE_ARGS:
        return isSimpleMinExprList(getMinExp_Args(node), context);
    case MINEXP_TYPE_BINDINGS:
        return isSimpleMinBindings(getMinExp_Bindings(node), context);
    case MINEXP_TYPE_CALLCC:
        return isSimpleMinExp(getMinExp_CallCC(node), context);
    case MINEXP_TYPE_CUT:
        return isSimpleMinExp(getMinExp_Cut(node), context);
    case MINEXP_TYPE_COND:
        return isSimpleMinCond(getMinExp_Cond(node), context);
    case MINEXP_TYPE_IFF:
        return isSimpleMinIff(getMinExp_Iff(node), context);
    case MINEXP_TYPE_LAM:
        return isSimpleMinLam(getMinExp_Lam(node), context);
    case MINEXP_TYPE_LETREC:
        return isSimpleMinLetRec(getMinExp_LetRec(node), context);
    case MINEXP_TYPE_MAKEVEC:
        return isSimpleMinExprList(getMinExp_MakeVec(node), context);
    case MINEXP_TYPE_MATCH:
        return isSimpleMinMatch(getMinExp_Match(node), context);
    case MINEXP_TYPE_PRIM:
        return isSimpleMinPrimApp(getMinExp_Prim(node), context);
    case MINEXP_TYPE_SEQUENCE:
        return isSimpleMinExprList(getMinExp_Sequence(node), context);
    case MINEXP_TYPE_DONE:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_VAR:
    case MINEXP_TYPE_AVAR:
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
        return true;
    default:
        cant_happen("unrecognized MinExp type %s", minExpTypeName(node->type));
    }
}

static bool isSimpleMinCondCases(MinCondCases *node, Context context) {
    if (node == NULL)
        return true;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        return isSimpleMinIntCondCases(variant, context);
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        return isSimpleMinCharCondCases(variant, context);
    }
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
}

static bool isSimpleMinCs(MinExp *callSite, Context context) {
    if (callSite == NULL)
        return true;
    if (isMinExp_Var(callSite) &&
        getSymbolSet(context.symbols, getMinExp_Var(callSite))) {
        return false;
    } else {
        return true;
    }
}

bool simpleMinExp(MinExp *node) {
    Context context = {.outer = true, .symbols = newSymbolSet()};
    int save = PROTECT(context.symbols);
    bool result = isSimpleMinExp(node, context);
    UNPROTECT(save);
    return result;
}