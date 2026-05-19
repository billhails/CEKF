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

#include "arithmetic_next.h"
#include "cekfs.h"
#include "memory.h"
#include "minlam.h"

#include "minlam_foldCmp.h"

#ifdef DEBUG_MINLAM_FOLDCMP
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinLam *foldCmpMinLam(MinLam *node);
static MinAnnotatedVar *foldCmpMinAnnotatedVar(MinAnnotatedVar *node);
static MinExprList *foldCmpMinExprList(MinExprList *node);
static MinExp *foldCmpMinPrimApp(MinExp *exp);
static MinApply *foldCmpMinApply(MinApply *node);
static MinIff *foldCmpMinIff(MinIff *node);
static MinCond *foldCmpMinCond(MinCond *node);
static MinIntCondCases *foldCmpMinIntCondCases(MinIntCondCases *node);
static MinCharCondCases *foldCmpMinCharCondCases(MinCharCondCases *node);
static MinMatch *foldCmpMinMatch(MinMatch *node);
static MinMatchList *foldCmpMinMatchList(MinMatchList *node);
static MinIntList *foldCmpMinIntList(MinIntList *node);
static MinLetRec *foldCmpMinLetRec(MinLetRec *node);
static MinBindings *foldCmpMinBindings(MinBindings *node);
static MinAmb *foldCmpMinAmb(MinAmb *node);
static MinCondCases *foldCmpMinCondCases(MinCondCases *node);

///////////////////////////
// Visitor implementations
///////////////////////////

static MinLam *foldCmpMinLam(MinLam *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinLam);
    bool changed = false;
    MinExp *new_exp = foldCmpMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinLam *result = node;
    if (changed) {
        result = newMinLam(CPI(node), node->args, new_exp);
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinLam);
    return result;
}

static MinAnnotatedVar *foldCmpMinAnnotatedVar(MinAnnotatedVar *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinAnnotatedVar);
    MinAnnotatedVar *result = node;
    LEAVE(foldCmpMinAnnotatedVar);
    return result;
}

static MinExprList *foldCmpMinExprList(MinExprList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinExprList);
    bool changed = false;
    MinExp *new_exp = foldCmpMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = foldCmpMinExprList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinExprList);
    return result;
}

static bool isComparison(MinPrimOp op) {
    switch (op) {
    case MINPRIMOP_TYPE_EQ:
    case MINPRIMOP_TYPE_NE:
    case MINPRIMOP_TYPE_GT:
    case MINPRIMOP_TYPE_LT:
    case MINPRIMOP_TYPE_GE:
    case MINPRIMOP_TYPE_LE:
    case MINPRIMOP_TYPE_CMP:
        return true;
    default:
        return false;
    }
}

static bool isConstant(MinExp *exp) {
    switch (exp->type) {
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_STDINT:
        return true;
    default:
        return false;
    }
}

static Value constToValue(MinExp *c) {
    if (isMinExp_BigInteger(c)) {
        MaybeBigInt *maybe = getMinExp_BigInteger(c);
        switch (maybe->type) {
        case BI_BIG: {
            bigint dst;
            bigint_init(&dst);
            bigint_cpy(&dst, &maybe->big);
            BigInt *big = newBigInt(dst);
            return value_Bigint(big);
        }
        case BI_SMALL:
            return value_Stdint(maybe->small);
        case BI_IRRATIONAL:
            return value_Irrational(maybe->irrational);
        default:
            cant_happen("unexpected bigint type %d", maybe->type);
        }
    } else {
        return value_Stdint(getMinExp_Stdint(c));
    }
}

static int compareConsts(MinPrimOp op, MinExp *e1, MinExp *e2) {
    Cmp cmp = 0;
    int save = STARTPROTECT();
    if (isMinExp_Character(e1)) {
        Character c1 = getMinExp_Character(e1);
        Character c2 = getMinExp_Character(e2);
        if (c1 < c2) {
            cmp = CMP_LT;
        } else if (c1 == c2) {
            cmp = CMP_EQ;
        } else {
            cmp = CMP_GT;
        }
    } else {
        Value v1 = constToValue(e1);
        protectValue(v1);
        Value v2 = constToValue(e2);
        protectValue(v2);
        cmp = ncmp(v1, v2);
    }
    UNPROTECT(save);
    switch (op) {
    case MINPRIMOP_TYPE_EQ:
        return cmp == CMP_EQ ? 1 : 0;
    case MINPRIMOP_TYPE_NE:
        return cmp != CMP_EQ ? 1 : 0;
    case MINPRIMOP_TYPE_GT:
        return cmp == CMP_GT ? 1 : 0;
    case MINPRIMOP_TYPE_LT:
        return cmp == CMP_LT ? 1 : 0;
    case MINPRIMOP_TYPE_GE:
        return cmp != CMP_LT ? 1 : 0;
    case MINPRIMOP_TYPE_LE:
        return cmp != CMP_GT ? 1 : 0;
    case MINPRIMOP_TYPE_CMP:
        return cmp == CMP_LT ? 0 : cmp == CMP_EQ ? 1 : 2;
    default:
        cant_happen("unexpected op type %s", minPrimOpName(op));
    }
}

static MinExp *foldCmpMinPrimApp(MinExp *exp) {
    if (exp == NULL)
        return NULL;
    ENTER(foldCmpMinPrimApp);
    MinPrimApp *node = getMinExp_Prim(exp);
    bool changed = false;
    MinExp *new_exp1 = foldCmpMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = foldCmpMinExp(node->exp2);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);
    MinExp *result = exp;
    if (isComparison(node->type) && isConstant(new_exp1) &&
        isConstant(new_exp2)) {
        result = newMinExp_Stdint(
            CPI(node), compareConsts(node->type, new_exp1, new_exp2));
    } else if (changed) {
        result = makeMinExp_Prim(CPI(node), node->type, new_exp1, new_exp2);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinPrimApp);
    return result;
}

static MinApply *foldCmpMinApply(MinApply *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinApply);
    bool changed = false;
    MinExp *new_function = foldCmpMinExp(node->function);
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = foldCmpMinExprList(node->args);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);
    MinApply *result = node;
    if (changed) {
        result = newMinApply(CPI(node), new_function, new_args);
        result->isBuiltin = node->isBuiltin;
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinApply);
    return result;
}

static MinIff *foldCmpMinIff(MinIff *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinIff);
    bool changed = false;
    MinExp *new_condition = foldCmpMinExp(node->condition);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = foldCmpMinExp(node->consequent);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = foldCmpMinExp(node->alternative);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);
    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinIff);
    return result;
}

static MinCond *foldCmpMinCond(MinCond *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinCond);
    bool changed = false;
    MinExp *new_value = foldCmpMinExp(node->value);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = foldCmpMinCondCases(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinCond);
    return result;
}

static MinIntCondCases *foldCmpMinIntCondCases(MinIntCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinIntCondCases);
    bool changed = false;
    MinExp *new_body = foldCmpMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = foldCmpMinIntCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinIntCondCases);
    return result;
}

static MinCharCondCases *foldCmpMinCharCondCases(MinCharCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinCharCondCases);
    bool changed = false;
    MinExp *new_body = foldCmpMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = foldCmpMinCharCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        result->isDefault = node->isDefault;
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinCharCondCases);
    return result;
}

static MinMatch *foldCmpMinMatch(MinMatch *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinMatch);
    bool changed = false;
    MinExp *new_index = foldCmpMinExp(node->index);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = foldCmpMinMatchList(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinMatch);
    return result;
}

static MinMatchList *foldCmpMinMatchList(MinMatchList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinMatchList);
    bool changed = false;
    MinIntList *new_matches = foldCmpMinIntList(node->matches);
    int save = PROTECT(new_matches);
    changed = changed || (new_matches != node->matches);
    MinExp *new_body = foldCmpMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = foldCmpMinMatchList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), new_matches, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinMatchList);
    return result;
}

static MinIntList *foldCmpMinIntList(MinIntList *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinIntList);
    bool changed = false;
    MinIntList *new_next = foldCmpMinIntList(node->next);
    int save = PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntList *result = node;
    if (changed) {
        result = newMinIntList(CPI(node), node->item, new_next);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinIntList);
    return result;
}

static MinLetRec *foldCmpMinLetRec(MinLetRec *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinLetRec);
    bool changed = false;
    MinBindings *new_bindings = foldCmpMinBindings(node->bindings);
    int save = PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = foldCmpMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinLetRec *result = node;
    if (changed) {
        result = newMinLetRec(CPI(node), new_bindings, new_body);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinLetRec);
    return result;
}

static MinBindings *foldCmpMinBindings(MinBindings *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinBindings);
    bool changed = false;
    MinExp *new_val = foldCmpMinExp(node->val);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = foldCmpMinBindings(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinBindings);
    return result;
}

static MinAmb *foldCmpMinAmb(MinAmb *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinAmb);
    bool changed = false;
    MinExp *new_left = foldCmpMinExp(node->left);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = foldCmpMinExp(node->right);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);
    MinAmb *result = node;
    if (changed) {
        result = newMinAmb(CPI(node), new_left, new_right);
    }
    UNPROTECT(save);
    LEAVE(foldCmpMinAmb);
    return result;
}

MinExp *foldCmpMinExp(MinExp *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinExp);
    int save = STARTPROTECT();
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = foldCmpMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = foldCmpMinApply(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = foldCmpMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Args(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
        MinAnnotatedVar *variant = getMinExp_Avar(node);
        MinAnnotatedVar *new_variant = foldCmpMinAnnotatedVar(variant);
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
        MinBindings *new_variant = foldCmpMinBindings(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = foldCmpMinExp(variant);
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
        MinCond *new_variant = foldCmpMinCond(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CUT: {
        // MinExp
        MinExp *variant = getMinExp_Cut(node);
        MinExp *new_variant = foldCmpMinExp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cut(CPI(node), new_variant);
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
        MinIff *new_variant = foldCmpMinIff(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = foldCmpMinLam(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = foldCmpMinLetRec(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = foldCmpMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = foldCmpMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        result = foldCmpMinPrimApp(node);
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = foldCmpMinExprList(variant);
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
    LEAVE(foldCmpMinExp);
    return result;
}

static MinCondCases *foldCmpMinCondCases(MinCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(foldCmpMinCondCases);
    int save = STARTPROTECT();
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = foldCmpMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = foldCmpMinCharCondCases(variant);
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
    LEAVE(foldCmpMinCondCases);
    return result;
}
