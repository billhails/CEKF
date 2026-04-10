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
 * Minimal AST after desugaring - dead binding elimination (tree shaking).
 * Generated from src/minlam.yaml by tools/generate.py
 */

#include "minlam_shake.h"
#include "memory.h"
#include "minlam_freeVars.h"
#include "minlam_helper.h"
#include "utils.h"
#include "utils_helper.h"

#ifdef DEBUG_MINLAM_SHAKE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinLam *shakeMinLam(MinLam *);
static MinExprList *shakeMinExprList(MinExprList *);
static MinPrimApp *shakeMinPrimApp(MinPrimApp *);
static MinApply *shakeMinApply(MinApply *);
static MinIff *shakeMinIff(MinIff *);
static MinCond *shakeMinCond(MinCond *);
static MinIntCondCases *shakeMinIntCondCases(MinIntCondCases *);
static MinCharCondCases *shakeMinCharCondCases(MinCharCondCases *);
static MinMatch *shakeMinMatch(MinMatch *);
static MinMatchList *shakeMinMatchList(MinMatchList *);
static MinExp *shakeMinLetRec(MinExp *);
static MinBindings *shakeMinBindings(MinBindings *);
static MinAmb *shakeMinAmb(MinAmb *);
static MinCondCases *shakeMinCondCases(MinCondCases *);

///////////////////////////
// Visitor implementations
///////////////////////////

static MinLam *shakeMinLam(MinLam *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinLam);
    bool changed = false;
    MinExp *new_exp = shakeMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinLam *result = node;
    if (changed) {
        result = newMinLam(CPI(node), node->args, new_exp);
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(shakeMinLam);
    return result;
}

static MinExprList *shakeMinExprList(MinExprList *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinExprList);
    bool changed = false;
    MinExp *new_exp = shakeMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = shakeMinExprList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }
    UNPROTECT(save);
    LEAVE(shakeMinExprList);
    return result;
}

static MinPrimApp *shakeMinPrimApp(MinPrimApp *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinPrimApp);
    bool changed = false;
    MinExp *new_exp1 = shakeMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = shakeMinExp(node->exp2);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);
    MinPrimApp *result = node;
    if (changed) {
        result = newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
    }
    UNPROTECT(save);
    LEAVE(shakeMinPrimApp);
    return result;
}

static MinApply *shakeMinApply(MinApply *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinApply);
    bool changed = false;
    MinExp *new_function = shakeMinExp(node->function);
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = shakeMinExprList(node->args);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);
    MinApply *result = node;
    if (changed) {
        result = newMinApply(CPI(node), new_function, new_args);
        result->isBuiltin = node->isBuiltin;
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(shakeMinApply);
    return result;
}

static MinIff *shakeMinIff(MinIff *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinIff);
    bool changed = false;
    MinExp *new_condition = shakeMinExp(node->condition);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = shakeMinExp(node->consequent);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = shakeMinExp(node->alternative);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);
    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }
    UNPROTECT(save);
    LEAVE(shakeMinIff);
    return result;
}

static MinCond *shakeMinCond(MinCond *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinCond);
    bool changed = false;
    MinExp *new_value = shakeMinExp(node->value);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = shakeMinCondCases(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }
    UNPROTECT(save);
    LEAVE(shakeMinCond);
    return result;
}

static MinIntCondCases *shakeMinIntCondCases(MinIntCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinIntCondCases);
    bool changed = false;
    MinExp *new_body = shakeMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = shakeMinIntCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(shakeMinIntCondCases);
    return result;
}

static MinCharCondCases *shakeMinCharCondCases(MinCharCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinCharCondCases);
    bool changed = false;
    MinExp *new_body = shakeMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = shakeMinCharCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        result->isDefault = node->isDefault;
    }
    UNPROTECT(save);
    LEAVE(shakeMinCharCondCases);
    return result;
}

static MinMatch *shakeMinMatch(MinMatch *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinMatch);
    bool changed = false;
    MinExp *new_index = shakeMinExp(node->index);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = shakeMinMatchList(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }
    UNPROTECT(save);
    LEAVE(shakeMinMatch);
    return result;
}

static MinMatchList *shakeMinMatchList(MinMatchList *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinMatchList);
    bool changed = false;
    MinExp *new_body = shakeMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = shakeMinMatchList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), node->matches, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(shakeMinMatchList);
    return result;
}

// Where it all happens
static MinExp *shakeMinLetRec(MinExp *exp) {
    ENTER(shakeMinLetRec);
    MinLetRec *node = getMinExp_LetRec(exp);
    bool changed = false;
    // post-traversal processing, recurse into the body
    // and the bindings first.
    MinExp *new_body = shakeMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinBindings *new_bindings = shakeMinBindings(node->bindings);
    PROTECT(new_bindings);

    // main algorithm
    SymbolSet *keys = getAllKeys(node->bindings);
    PROTECT(keys);
    SymbolSetMap *deps = buildDependencyGraph(node->bindings, keys);
    PROTECT(deps);
    SymbolSet *rootSet = computeRoots(keys, new_body);
    PROTECT(rootSet);
    SymbolSet *live = computeLiveBindings(deps, rootSet);
    PROTECT(live);
    new_bindings = retainOnlyLive(new_bindings, live);
    PROTECT(new_bindings);

    changed = changed || (new_bindings != node->bindings);
    MinExp *result = exp;
    if (changed) {
        if (new_bindings == NULL)
            result = new_body;
        else
            result = makeMinExp_LetRec(CPI(node), new_bindings, new_body);
    } else if (new_bindings == NULL) {
        result = new_body;
    }
    UNPROTECT(save);
    LEAVE(shakeMinLetRec);
    return result;
}

static MinBindings *shakeMinBindings(MinBindings *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinBindings);
    bool changed = false;
    MinExp *new_val = shakeMinExp(node->val);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = shakeMinBindings(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
    }
    UNPROTECT(save);
    LEAVE(shakeMinBindings);
    return result;
}

static MinAmb *shakeMinAmb(MinAmb *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinAmb);
    bool changed = false;
    MinExp *new_left = shakeMinExp(node->left);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = shakeMinExp(node->right);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);
    MinAmb *result = node;
    if (changed) {
        result = newMinAmb(CPI(node), new_left, new_right);
    }
    UNPROTECT(save);
    LEAVE(shakeMinAmb);
    return result;
}

//////////////
// Public API
//////////////

MinExp *shakeMinExp(MinExp *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinExp);
    int save = PROTECT(NULL);
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = shakeMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = shakeMinApply(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = shakeMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Args(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
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
        MinBindings *new_variant = shakeMinBindings(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = shakeMinExp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_CallCC(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        // character
        break;
    }
    case MINEXP_TYPE_COND: {
        // MinCond
        MinCond *variant = getMinExp_Cond(node);
        MinCond *new_variant = shakeMinCond(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_DONE: {
        // void_ptr
        break;
    }
    case MINEXP_TYPE_IFF: {
        // MinIff
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = shakeMinIff(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = shakeMinLam(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        // can return just the body if the bindings are empty
        result = shakeMinLetRec(node);
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = shakeMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = shakeMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = shakeMinPrimApp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = shakeMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Sequence(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_STDINT: {
        // int
        break;
    }
    case MINEXP_TYPE_VAR: {
        // HashSymbol
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }
    UNPROTECT(save);
    LEAVE(shakeMinExp);
    return result;
}

static MinCondCases *shakeMinCondCases(MinCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(shakeMinCondCases);
    int save = PROTECT(NULL);
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = shakeMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = shakeMinCharCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Characters(CPI(node), new_variant);
        }
        break;
    }
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
    UNPROTECT(save);
    LEAVE(shakeMinCondCases);
    return result;
}