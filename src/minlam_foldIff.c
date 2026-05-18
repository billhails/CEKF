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

#include "minlam_foldIff.h"

#ifdef DEBUG_MINLAM_FOLDIFF
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinLam *foldIffMinLam(MinLam *node);
static MinAnnotatedVar *foldIffMinAnnotatedVar(MinAnnotatedVar *node);
static MinExprList *foldIffMinExprList(MinExprList *node);
static MinPrimApp *foldIffMinPrimApp(MinPrimApp *node);
static MinApply *foldIffMinApply(MinApply *node);
static MinExp *foldIffMinIff(MinExp *exp);
static MinCond *foldIffMinCond(MinCond *node);
static MinIntCondCases *foldIffMinIntCondCases(MinIntCondCases *node);
static MinCharCondCases *foldIffMinCharCondCases(MinCharCondCases *node);
static MinMatch *foldIffMinMatch(MinMatch *node);
static MinMatchList *foldIffMinMatchList(MinMatchList *node);
static MinIntList *foldIffMinIntList(MinIntList *node);
static MinLetRec *foldIffMinLetRec(MinLetRec *node);
static MinBindings *foldIffMinBindings(MinBindings *node);
static MinAmb *foldIffMinAmb(MinAmb *node);
static MinCondCases *foldIffMinCondCases(MinCondCases *node);

///////////////////////////
// Visitor implementations
///////////////////////////

static MinLam *foldIffMinLam(MinLam *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinLam);
    bool changed = false;
    MinExp *new_exp = foldIffMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinLam *result = node;
    if (changed) {
        result = newMinLam(CPI(node), node->args, new_exp);
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(foldIffMinLam);
    return result;
}

static MinAnnotatedVar *foldIffMinAnnotatedVar(MinAnnotatedVar *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinAnnotatedVar);
    MinAnnotatedVar *result = node;
    LEAVE(foldIffMinAnnotatedVar);
    return result;
}

static MinExprList *foldIffMinExprList(MinExprList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinExprList);
    bool changed = false;
    MinExp *new_exp = foldIffMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = foldIffMinExprList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinExprList);
    return result;
}

static MinPrimApp *foldIffMinPrimApp(MinPrimApp *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinPrimApp);
    bool changed = false;
    MinExp *new_exp1 = foldIffMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = foldIffMinExp(node->exp2);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);
    MinPrimApp *result = node;
    if (changed) {
        result = newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinPrimApp);
    return result;
}

static MinApply *foldIffMinApply(MinApply *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinApply);
    bool changed = false;
    MinExp *new_function = foldIffMinExp(node->function);
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = foldIffMinExprList(node->args);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);
    MinApply *result = node;
    if (changed) {
        result = newMinApply(CPI(node), new_function, new_args);
        result->isBuiltin = node->isBuiltin;
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(foldIffMinApply);
    return result;
}

static MinExp *foldIffMinIff(MinExp *exp) {
    if (exp == NULL)
        return NULL;
    ENTER(foldIffMinIff);
    MinIff *node = getMinExp_Iff(exp);
    bool changed = false;
    MinExp *new_condition = foldIffMinExp(node->condition);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = foldIffMinExp(node->consequent);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = foldIffMinExp(node->alternative);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);
    MinExp *result = exp;
    if (isMinExp_Stdint(new_condition)) {
        if (getMinExp_Stdint(new_condition)) {
            result = new_consequent;
        } else {
            result = new_alternative;
        }
    } else if (changed) {
        result = makeMinExp_Iff(CPI(node), new_condition, new_consequent,
                                new_alternative);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinIff);
    return result;
}

static MinCond *foldIffMinCond(MinCond *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinCond);
    bool changed = false;
    MinExp *new_value = foldIffMinExp(node->value);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = foldIffMinCondCases(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinCond);
    return result;
}

static MinIntCondCases *foldIffMinIntCondCases(MinIntCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinIntCondCases);
    bool changed = false;
    MinExp *new_body = foldIffMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = foldIffMinIntCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinIntCondCases);
    return result;
}

static MinCharCondCases *foldIffMinCharCondCases(MinCharCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinCharCondCases);
    bool changed = false;
    MinExp *new_body = foldIffMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = foldIffMinCharCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        result->isDefault = node->isDefault;
    }
    UNPROTECT(save);
    LEAVE(foldIffMinCharCondCases);
    return result;
}

static MinMatch *foldIffMinMatch(MinMatch *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinMatch);
    bool changed = false;
    MinExp *new_index = foldIffMinExp(node->index);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = foldIffMinMatchList(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinMatch);
    return result;
}

static MinMatchList *foldIffMinMatchList(MinMatchList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinMatchList);
    bool changed = false;
    MinIntList *new_matches = foldIffMinIntList(node->matches);
    int save = PROTECT(new_matches);
    changed = changed || (new_matches != node->matches);
    MinExp *new_body = foldIffMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = foldIffMinMatchList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), new_matches, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinMatchList);
    return result;
}

static MinIntList *foldIffMinIntList(MinIntList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinIntList);
    bool changed = false;
    MinIntList *new_next = foldIffMinIntList(node->next);
    int save = PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntList *result = node;
    if (changed) {
        result = newMinIntList(CPI(node), node->item, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinIntList);
    return result;
}

static MinLetRec *foldIffMinLetRec(MinLetRec *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinLetRec);
    bool changed = false;
    MinBindings *new_bindings = foldIffMinBindings(node->bindings);
    int save = PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = foldIffMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinLetRec *result = node;
    if (changed) {
        result = newMinLetRec(CPI(node), new_bindings, new_body);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinLetRec);
    return result;
}

static MinBindings *foldIffMinBindings(MinBindings *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinBindings);
    bool changed = false;
    MinExp *new_val = foldIffMinExp(node->val);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = foldIffMinBindings(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
    }
    UNPROTECT(save);
    LEAVE(foldIffMinBindings);
    return result;
}

static MinAmb *foldIffMinAmb(MinAmb *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinAmb);
    bool changed = false;
    MinExp *new_left = foldIffMinExp(node->left);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = foldIffMinExp(node->right);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);
    MinAmb *result = node;
    if (changed) {
        result = newMinAmb(CPI(node), new_left, new_right);
    }
    UNPROTECT(save);
    LEAVE(foldIffMinAmb);
    return result;
}

MinExp *foldIffMinExp(MinExp *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinExp);
    int save = STARTPROTECT();
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = foldIffMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = foldIffMinApply(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = foldIffMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Args(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
        MinAnnotatedVar *variant = getMinExp_Avar(node);
        MinAnnotatedVar *new_variant = foldIffMinAnnotatedVar(variant);
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
        MinBindings *new_variant = foldIffMinBindings(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = foldIffMinExp(variant);
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
        MinCond *new_variant = foldIffMinCond(variant);
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
        result = foldIffMinIff(node);
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = foldIffMinLam(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = foldIffMinLetRec(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = foldIffMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = foldIffMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = foldIffMinPrimApp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = foldIffMinExprList(variant);
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
    LEAVE(foldIffMinExp);
    return result;
}

static MinCondCases *foldIffMinCondCases(MinCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldIffMinCondCases);
    int save = STARTPROTECT();
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = foldIffMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = foldIffMinCharCondCases(variant);
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
    LEAVE(foldIffMinCondCases);
    return result;
}