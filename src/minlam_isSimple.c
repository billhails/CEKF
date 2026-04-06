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
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        return isSimpleMinAmb(variant, context);
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        return isSimpleMinApply(variant, context);
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        MinExprList *variant = getMinExp_Args(node);
        return isSimpleMinExprList(variant, context);
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
        return true;
    }
    case MINEXP_TYPE_BACK: {
        // void_ptr
        return true;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        // MaybeBigInt
        return true;
    }
    case MINEXP_TYPE_BINDINGS: {
        // MinBindings
        MinBindings *variant = getMinExp_Bindings(node);
        return isSimpleMinBindings(variant, context);
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        return isSimpleMinExp(variant, context);
    }
    case MINEXP_TYPE_CHARACTER: {
        // character
        return true;
    }
    case MINEXP_TYPE_COND: {
        // MinCond
        MinCond *variant = getMinExp_Cond(node);
        return isSimpleMinCond(variant, context);
    }
    case MINEXP_TYPE_DONE: {
        // int
        return true;
    }
    case MINEXP_TYPE_IFF: {
        // MinIff
        MinIff *variant = getMinExp_Iff(node);
        return isSimpleMinIff(variant, context);
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        return isSimpleMinLam(variant, context);
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        return isSimpleMinLetRec(variant, context);
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        return isSimpleMinExprList(variant, context);
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        return isSimpleMinMatch(variant, context);
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        return isSimpleMinPrimApp(variant, context);
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        return isSimpleMinExprList(variant, context);
    }
    case MINEXP_TYPE_STDINT: {
        // int
        return true;
    }
    case MINEXP_TYPE_VAR: {
        // HashSymbol
        return true;
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
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