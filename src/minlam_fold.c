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
 */

#include "memory.h"
#include "minlam.h"

#include "minlam_fold.h"
#include "minlam_simplify.h"

#ifdef DEBUG_MINLAM_FOLD
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinLam *foldMinLam(MinLam *node);
static MinExprList *foldMinExprList(MinExprList *node);
static MinPrimApp *foldMinPrimApp(MinPrimApp *node);
static MinApply *foldMinApply(MinApply *node);
static MinIff *foldMinIff(MinIff *node);
static MinCond *foldMinCond(MinCond *node);
static MinIntCondCases *foldMinIntCondCases(MinIntCondCases *node);
static MinCharCondCases *foldMinCharCondCases(MinCharCondCases *node);
static MinMatch *foldMinMatch(MinMatch *node);
static MinMatchList *foldMinMatchList(MinMatchList *node);
static MinIntList *foldMinIntList(MinIntList *node);
static MinLetRec *foldMinLetRec(MinLetRec *node);
static MinBindings *foldMinBindings(MinBindings *node);
static MinAmb *foldMinAmb(MinAmb *node);
static MinCondCases *foldMinCondCases(MinCondCases *node);

static bool primOpIsArithmetic(MinPrimOp op) {
    switch (op) {
    case MINPRIMOP_TYPE_ADD:
    case MINPRIMOP_TYPE_SUB:
    case MINPRIMOP_TYPE_MUL:
    case MINPRIMOP_TYPE_DIV:
    case MINPRIMOP_TYPE_MOD:
    case MINPRIMOP_TYPE_GCD:
    case MINPRIMOP_TYPE_LCM:
    case MINPRIMOP_TYPE_POW:
        return true;
    default:
        return false;
    }
}

static MinLam *foldMinLam(MinLam *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_exp = foldMinExp(node->exp);
    if (new_exp != node->exp)
        return newMinLam(CPI(node), node->args, new_exp);

    return node;
}

static MinExprList *foldMinExprList(MinExprList *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_exp = foldMinExp(node->exp);
    int save = PROTECT(new_exp);
    MinExprList *new_next = foldMinExprList(node->next);
    PROTECT(new_next);

    if (new_exp != node->exp || new_next != node->next) {
        MinExprList *result = newMinExprList(CPI(node), new_exp, new_next);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinPrimApp *foldMinPrimApp(MinPrimApp *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_exp1 = foldMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    MinExp *new_exp2 = foldMinExp(node->exp2);
    PROTECT(new_exp2);

    if (new_exp1 != node->exp1 || new_exp2 != node->exp2) {
        MinPrimApp *result =
            newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinApply *foldMinApply(MinApply *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_function = foldMinExp(node->function);
    int save = PROTECT(new_function);
    MinExprList *new_args = foldMinExprList(node->args);
    PROTECT(new_args);

    if (new_function != node->function || new_args != node->args) {
        MinApply *result = newMinApply(CPI(node), new_function, new_args);
        result->isBuiltin = node->isBuiltin;
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinIff *foldMinIff(MinIff *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_condition = foldMinExp(node->condition);
    int save = PROTECT(new_condition);
    MinExp *new_consequent = foldMinExp(node->consequent);
    PROTECT(new_consequent);
    MinExp *new_alternative = foldMinExp(node->alternative);
    PROTECT(new_alternative);

    if (new_condition != node->condition ||
        new_consequent != node->consequent ||
        new_alternative != node->alternative) {
        MinIff *result = newMinIff(CPI(node), new_condition, new_consequent,
                                   new_alternative);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinCond *foldMinCond(MinCond *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_value = foldMinExp(node->value);
    int save = PROTECT(new_value);
    MinCondCases *new_cases = foldMinCondCases(node->cases);
    PROTECT(new_cases);

    if (new_value != node->value || new_cases != node->cases) {
        MinCond *result = newMinCond(CPI(node), new_value, new_cases);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinIntCondCases *foldMinIntCondCases(MinIntCondCases *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_body = foldMinExp(node->body);
    int save = PROTECT(new_body);
    MinIntCondCases *new_next = foldMinIntCondCases(node->next);
    PROTECT(new_next);

    if (new_body != node->body || new_next != node->next) {
        MinIntCondCases *result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinCharCondCases *foldMinCharCondCases(MinCharCondCases *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_body = foldMinExp(node->body);
    int save = PROTECT(new_body);
    MinCharCondCases *new_next = foldMinCharCondCases(node->next);
    PROTECT(new_next);

    if (new_body != node->body || new_next != node->next) {
        MinCharCondCases *result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinMatch *foldMinMatch(MinMatch *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_index = foldMinExp(node->index);
    int save = PROTECT(new_index);
    MinMatchList *new_cases = foldMinMatchList(node->cases);
    PROTECT(new_cases);

    if (new_index != node->index || new_cases != node->cases) {
        MinMatch *result = newMinMatch(CPI(node), new_index, new_cases);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinMatchList *foldMinMatchList(MinMatchList *node) {
    if (node == NULL)
        return NULL;

    MinIntList *new_matches = foldMinIntList(node->matches);
    int save = PROTECT(new_matches);
    MinExp *new_body = foldMinExp(node->body);
    PROTECT(new_body);
    MinMatchList *new_next = foldMinMatchList(node->next);
    PROTECT(new_next);

    if (new_matches != node->matches || new_body != node->body ||
        new_next != node->next) {
        MinMatchList *result =
            newMinMatchList(CPI(node), new_matches, new_body, new_next);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinIntList *foldMinIntList(MinIntList *node) {
    if (node == NULL)
        return NULL;

    MinIntList *new_next = foldMinIntList(node->next);
    if (new_next != node->next)
        return newMinIntList(CPI(node), node->item, new_next);

    return node;
}

static MinLetRec *foldMinLetRec(MinLetRec *node) {
    if (node == NULL)
        return NULL;

    MinBindings *new_bindings = foldMinBindings(node->bindings);
    int save = PROTECT(new_bindings);
    MinExp *new_body = foldMinExp(node->body);
    PROTECT(new_body);

    if (new_bindings != node->bindings || new_body != node->body) {
        MinLetRec *result = newMinLetRec(CPI(node), new_bindings, new_body);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinBindings *foldMinBindings(MinBindings *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_val = foldMinExp(node->val);
    int save = PROTECT(new_val);
    MinBindings *new_next = foldMinBindings(node->next);
    PROTECT(new_next);

    if (new_val != node->val || new_next != node->next) {
        MinBindings *result =
            newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

static MinAmb *foldMinAmb(MinAmb *node) {
    if (node == NULL)
        return NULL;

    MinExp *new_left = foldMinExp(node->left);
    int save = PROTECT(new_left);
    MinExp *new_right = foldMinExp(node->right);
    PROTECT(new_right);

    if (new_left != node->left || new_right != node->right) {
        MinAmb *result = newMinAmb(CPI(node), new_left, new_right);
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return node;
}

MinExp *foldMinExp(MinExp *node) {
    if (node == NULL)
        return NULL;

    int save = PROTECT(NULL);
    MinExp *result = node;

    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = foldMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = foldMinApply(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // args is a compatibility node for the alternative normalize_2 path
        // and is not a valid expression in the main MinExp pipeline.
        cant_happen("MINEXP_TYPE_ARGS should not appear in foldMinExp");
        break;
    }
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_VAR:
        break;
    case MINEXP_TYPE_BINDINGS: {
        MinBindings *variant = getMinExp_Bindings(node);
        MinBindings *new_variant = foldMinBindings(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = foldMinExp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_CallCC(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_COND: {
        MinCond *variant = getMinExp_Cond(node);
        MinCond *new_variant = foldMinCond(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = foldMinIff(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = foldMinLam(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = foldMinLetRec(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = foldMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = foldMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = foldMinPrimApp(variant);
        MinExp *candidate = node;
        if (new_variant != variant) {
            PROTECT(new_variant);
            candidate = newMinExp_Prim(CPI(node), new_variant);
        }
        MinPrimApp *candidatePrim = getMinExp_Prim(candidate);
        bool hasArgsOperand = candidatePrim->exp1->type == MINEXP_TYPE_ARGS ||
                              candidatePrim->exp2->type == MINEXP_TYPE_ARGS;
        if (primOpIsArithmetic(candidatePrim->type) && !hasArgsOperand) {
            result = simplifyMinExp(candidate);
        } else {
            result = candidate;
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = foldMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Sequence(CPI(node), new_variant);
        }
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }

    UNPROTECT(save);
    return result;
}

static MinCondCases *foldMinCondCases(MinCondCases *node) {
    if (node == NULL)
        return NULL;

    int save = PROTECT(NULL);
    MinCondCases *result = node;

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = foldMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = foldMinCharCondCases(variant);
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
    return result;
}