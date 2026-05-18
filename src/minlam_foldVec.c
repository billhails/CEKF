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
#include "minlam.h"

#include "minlam_foldVec.h"

#ifdef DEBUG_MINLAM_FOLDVEC
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

// (vec 0 (make-vec (vec 0 (make-vec 1 2)) 3)) => 1

static MinLam *foldVecMinLam(MinLam *node);
static MinAnnotatedVar *foldVecMinAnnotatedVar(MinAnnotatedVar *node);
static MinExprList *foldVecMinExprList(MinExprList *node);
static MinExp *foldVecMinPrimApp(MinExp *node);
static MinApply *foldVecMinApply(MinApply *node);
static MinIff *foldVecMinIff(MinIff *node);
static MinCond *foldVecMinCond(MinCond *node);
static MinIntCondCases *foldVecMinIntCondCases(MinIntCondCases *node);
static MinCharCondCases *foldVecMinCharCondCases(MinCharCondCases *node);
static MinMatch *foldVecMinMatch(MinMatch *node);
static MinMatchList *foldVecMinMatchList(MinMatchList *node);
static MinIntList *foldVecMinIntList(MinIntList *node);
static MinLetRec *foldVecMinLetRec(MinLetRec *node);
static MinBindings *foldVecMinBindings(MinBindings *node);
static MinAmb *foldVecMinAmb(MinAmb *node);
static MinCondCases *foldVecMinCondCases(MinCondCases *node);

///////////////////////////
// Visitor implementations
///////////////////////////

static MinLam *foldVecMinLam(MinLam *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinLam);
    bool changed = false;
    MinExp *new_exp = foldVecMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinLam *result = node;
    if (changed) {
        result = newMinLam(CPI(node), node->args, new_exp);
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(foldVecMinLam);
    return result;
}

static MinAnnotatedVar *foldVecMinAnnotatedVar(MinAnnotatedVar *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinAnnotatedVar);
    MinAnnotatedVar *result = node;
    LEAVE(foldVecMinAnnotatedVar);
    return result;
}

static MinExprList *foldVecMinExprList(MinExprList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinExprList);
    bool changed = false;
    MinExp *new_exp = foldVecMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = foldVecMinExprList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinExprList);
    return result;
}

static MinExp *nthMinExp(int index, MinExprList *list) {
    while (index > 0) {
#ifdef SAFETY_CHECKS
        if (list == NULL) {
            cant_happen("list exhausted[1]");
        }
#endif
        list = list->next;
        index--;
    }
#ifdef SAFETY_CHECKS
    if (list == NULL) {
        cant_happen("list exhausted[2]");
    }
#endif
    return list->exp;
}

static MinExp *foldVecMinPrimApp(MinExp *exp) {
    if (exp == NULL)
        return NULL;
    MinPrimApp *node = getMinExp_Prim(exp);
    ENTER(foldVecMinPrimApp);
    bool changed = false;
    MinExp *new_exp1 = foldVecMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = foldVecMinExp(node->exp2);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);
    MinExp *result = exp;
    if (node->type == MINPRIMOP_TYPE_VEC &&
        new_exp2->type == MINEXP_TYPE_MAKEVEC &&
        new_exp1->type == MINEXP_TYPE_STDINT) {
        result = nthMinExp(new_exp1->val.stdint, new_exp2->val.makeVec);
    } else if (changed) {
        result = makeMinExp_Prim(CPI(node), node->type, new_exp1, new_exp2);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinPrimApp);
    return result;
}

static MinApply *foldVecMinApply(MinApply *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinApply);
    bool changed = false;
    MinExp *new_function = foldVecMinExp(node->function);
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = foldVecMinExprList(node->args);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);
    MinApply *result = node;
    if (changed) {
        result = newMinApply(CPI(node), new_function, new_args);
        result->isBuiltin = node->isBuiltin;
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(foldVecMinApply);
    return result;
}

static MinIff *foldVecMinIff(MinIff *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinIff);
    bool changed = false;
    MinExp *new_condition = foldVecMinExp(node->condition);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = foldVecMinExp(node->consequent);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = foldVecMinExp(node->alternative);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);
    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinIff);
    return result;
}

static MinCond *foldVecMinCond(MinCond *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinCond);
    bool changed = false;
    MinExp *new_value = foldVecMinExp(node->value);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = foldVecMinCondCases(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinCond);
    return result;
}

static MinIntCondCases *foldVecMinIntCondCases(MinIntCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinIntCondCases);
    bool changed = false;
    MinExp *new_body = foldVecMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = foldVecMinIntCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinIntCondCases);
    return result;
}

static MinCharCondCases *foldVecMinCharCondCases(MinCharCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinCharCondCases);
    bool changed = false;
    MinExp *new_body = foldVecMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = foldVecMinCharCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        result->isDefault = node->isDefault;
    }
    UNPROTECT(save);
    LEAVE(foldVecMinCharCondCases);
    return result;
}

static MinMatch *foldVecMinMatch(MinMatch *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinMatch);
    bool changed = false;
    MinExp *new_index = foldVecMinExp(node->index);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = foldVecMinMatchList(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinMatch);
    return result;
}

static MinMatchList *foldVecMinMatchList(MinMatchList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinMatchList);
    bool changed = false;
    MinIntList *new_matches = foldVecMinIntList(node->matches);
    int save = PROTECT(new_matches);
    changed = changed || (new_matches != node->matches);
    MinExp *new_body = foldVecMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = foldVecMinMatchList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), new_matches, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinMatchList);
    return result;
}

static MinIntList *foldVecMinIntList(MinIntList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinIntList);
    bool changed = false;
    MinIntList *new_next = foldVecMinIntList(node->next);
    int save = PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntList *result = node;
    if (changed) {
        result = newMinIntList(CPI(node), node->item, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinIntList);
    return result;
}

static MinLetRec *foldVecMinLetRec(MinLetRec *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinLetRec);
    bool changed = false;
    MinBindings *new_bindings = foldVecMinBindings(node->bindings);
    int save = PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = foldVecMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinLetRec *result = node;
    if (changed) {
        result = newMinLetRec(CPI(node), new_bindings, new_body);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinLetRec);
    return result;
}

static MinBindings *foldVecMinBindings(MinBindings *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinBindings);
    bool changed = false;
    MinExp *new_val = foldVecMinExp(node->val);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = foldVecMinBindings(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
    }
    UNPROTECT(save);
    LEAVE(foldVecMinBindings);
    return result;
}

static MinAmb *foldVecMinAmb(MinAmb *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinAmb);
    bool changed = false;
    MinExp *new_left = foldVecMinExp(node->left);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = foldVecMinExp(node->right);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);
    MinAmb *result = node;
    if (changed) {
        result = newMinAmb(CPI(node), new_left, new_right);
    }
    UNPROTECT(save);
    LEAVE(foldVecMinAmb);
    return result;
}

MinExp *foldVecMinExp(MinExp *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinExp);
    int save = STARTPROTECT();
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = foldVecMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = foldVecMinApply(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = foldVecMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Args(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
        MinAnnotatedVar *variant = getMinExp_Avar(node);
        MinAnnotatedVar *new_variant = foldVecMinAnnotatedVar(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Avar(CPI(node), new_variant);
        }
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
        MinBindings *new_variant = foldVecMinBindings(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = foldVecMinExp(variant);
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
        MinCond *new_variant = foldVecMinCond(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_DONE: {
        // int
        break;
    }
    case MINEXP_TYPE_IFF: {
        // MinIff
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = foldVecMinIff(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = foldVecMinLam(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = foldVecMinLetRec(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = foldVecMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = foldVecMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        result = foldVecMinPrimApp(node);
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = foldVecMinExprList(variant);
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
    LEAVE(foldVecMinExp);
    return result;
}

static MinCondCases *foldVecMinCondCases(MinCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldVecMinCondCases);
    int save = STARTPROTECT();
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = foldVecMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = foldVecMinCharCondCases(variant);
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
    LEAVE(foldVecMinCondCases);
    return result;
}
