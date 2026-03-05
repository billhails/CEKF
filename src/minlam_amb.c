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

#include "minlam_amb.h"
#include "memory.h"
#include "symbol.h"

#ifdef DEBUG_MINLAM_AMB
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinExp *ambMinAmb(MinAmb *, MinExp *);

static MinLam *ambMinLam(MinLam *, MinExp *);
static MinExprList *ambMinExprList(MinExprList *, MinExp *);
static MinPrimApp *ambMinPrimApp(MinPrimApp *, MinExp *);
static MinApply *ambMinApply(MinApply *, MinExp *);
static MinIff *ambMinIff(MinIff *, MinExp *);
static MinCond *ambMinCond(MinCond *, MinExp *);
static MinIntCondCases *ambMinIntCondCases(MinIntCondCases *, MinExp *);
static MinCharCondCases *ambMinCharCondCases(MinCharCondCases *, MinExp *);
static MinMatch *ambMinMatch(MinMatch *, MinExp *);
static MinMatchList *ambMinMatchList(MinMatchList *, MinExp *);
static MinLetRec *ambMinLetRec(MinLetRec *, MinExp *);
static MinBindings *ambMinBindings(MinBindings *, MinExp *);
static MinCondCases *ambMinCondCases(MinCondCases *, MinExp *);

// append f to the end of l
static SymbolList *ambSymbolList(SymbolList *l, SymbolList *f) {
    if (l == NULL) {
        return f;
    }
    SymbolList *rest = ambSymbolList(l->next, f);
    int save = PROTECT(rest);
    SymbolList *this = newSymbolList(CPI(l), l->symbol, rest);
    UNPROTECT(save);
    return this;
}

static MinExprList *appendExpr(MinExprList *l, MinExp *f) {
    if (l == NULL) {
        return newMinExprList(CPI(f), f, NULL);
    }
    MinExprList *next = appendExpr(l->next, f);
    int save = PROTECT(next);
    MinExprList *this = newMinExprList(CPI(l), l->exp, next);
    UNPROTECT(save);
    return this;
}

///////////////////////////
// Visitor implementations
///////////////////////////

static MinLam *ambMinLam(MinLam *node, MinExp *fail __attribute__((unused))) {
    ENTER(ambMinLam);
    if (node == NULL) {
        LEAVE(ambMinLam);
        return NULL;
    }
    HashSymbol *f = genSymDollar("f");
    SymbolList *fl = newSymbolList(CPI(node), f, NULL);
    int save = PROTECT(fl);
    SymbolList *new_args = ambSymbolList(node->args, fl);
    PROTECT(new_args);
    MinExp *fail2 = newMinExp_Var(CPI(node), f);
    PROTECT(fail2);
    MinExp *new_exp = ambMinExp(node->exp, fail2);
    PROTECT(new_exp);
    MinLam *result = newMinLam(CPI(node), new_args, new_exp);
    result->cc = node->cc;
    UNPROTECT(save);
    LEAVE(ambMinLam);
    return result;
}

static MinExprList *ambMinExprList(MinExprList *node, MinExp *fail) {
    ENTER(ambMinExprList);
    if (node == NULL) {
        LEAVE(ambMinExprList);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp = ambMinExp(node->exp, fail);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = ambMinExprList(node->next, fail);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }

    UNPROTECT(save);
    LEAVE(ambMinExprList);
    return result;
}

static MinPrimApp *ambMinPrimApp(MinPrimApp *node, MinExp *fail) {
    ENTER(ambMinPrimApp);
    if (node == NULL) {
        LEAVE(ambMinPrimApp);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp1 = ambMinExp(node->exp1, fail);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = ambMinExp(node->exp2, fail);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);

    MinPrimApp *result = node;
    if (changed) {
        result = newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
    }

    UNPROTECT(save);
    LEAVE(ambMinPrimApp);
    return result;
}

static MinApply *ambMinApply(MinApply *node, MinExp *fail) {
    ENTER(ambMinApply);
    if (node == NULL) {
        LEAVE(ambMinApply);
        return NULL;
    }

    MinExp *new_function = ambMinExp(node->function, fail);
    int save = PROTECT(new_function);
    MinExprList *new_args = ambMinExprList(node->args, fail);
    PROTECT(new_args);
    new_args = appendExpr(new_args, fail);
    PROTECT(new_args);
    MinApply *result = newMinApply(CPI(node), new_function, new_args);
    result->isBuiltin = node->isBuiltin;
    result->cc = node->cc;
    UNPROTECT(save);
    LEAVE(ambMinApply);
    return result;
}

static MinIff *ambMinIff(MinIff *node, MinExp *fail) {
    ENTER(ambMinIff);
    if (node == NULL) {
        LEAVE(ambMinIff);
        return NULL;
    }

    bool changed = false;
    MinExp *new_condition = ambMinExp(node->condition, fail);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = ambMinExp(node->consequent, fail);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = ambMinExp(node->alternative, fail);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);

    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }

    UNPROTECT(save);
    LEAVE(ambMinIff);
    return result;
}

static MinCond *ambMinCond(MinCond *node, MinExp *fail) {
    ENTER(ambMinCond);
    if (node == NULL) {
        LEAVE(ambMinCond);
        return NULL;
    }

    bool changed = false;
    MinExp *new_value = ambMinExp(node->value, fail);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = ambMinCondCases(node->cases, fail);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }

    UNPROTECT(save);
    LEAVE(ambMinCond);
    return result;
}

static MinIntCondCases *ambMinIntCondCases(MinIntCondCases *node,
                                           MinExp *fail) {
    ENTER(ambMinIntCondCases);
    if (node == NULL) {
        LEAVE(ambMinIntCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = ambMinExp(node->body, fail);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = ambMinIntCondCases(node->next, fail);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }

    UNPROTECT(save);
    LEAVE(ambMinIntCondCases);
    return result;
}

static MinCharCondCases *ambMinCharCondCases(MinCharCondCases *node,
                                             MinExp *fail) {
    ENTER(ambMinCharCondCases);
    if (node == NULL) {
        LEAVE(ambMinCharCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = ambMinExp(node->body, fail);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = ambMinCharCondCases(node->next, fail);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
    }

    UNPROTECT(save);
    LEAVE(ambMinCharCondCases);
    return result;
}

static MinMatch *ambMinMatch(MinMatch *node, MinExp *fail) {
    ENTER(ambMinMatch);
    if (node == NULL) {
        LEAVE(ambMinMatch);
        return NULL;
    }

    bool changed = false;
    MinExp *new_index = ambMinExp(node->index, fail);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = ambMinMatchList(node->cases, fail);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }

    UNPROTECT(save);
    LEAVE(ambMinMatch);
    return result;
}

static MinMatchList *ambMinMatchList(MinMatchList *node, MinExp *fail) {
    ENTER(ambMinMatchList);
    if (node == NULL) {
        LEAVE(ambMinMatchList);
        return NULL;
    }

    bool changed = false;
    MinIntList *new_matches = node->matches;
    int save = PROTECT(new_matches);
    changed = changed || (new_matches != node->matches);
    MinExp *new_body = ambMinExp(node->body, fail);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = ambMinMatchList(node->next, fail);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), new_matches, new_body, new_next);
    }

    UNPROTECT(save);
    LEAVE(ambMinMatchList);
    return result;
}

static MinLetRec *ambMinLetRec(MinLetRec *node, MinExp *fail) {
    ENTER(ambMinLetRec);
    if (node == NULL) {
        LEAVE(ambMinLetRec);
        return NULL;
    }

    bool changed = false;
    MinBindings *new_bindings = ambMinBindings(node->bindings, fail);
    int save = PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = ambMinExp(node->body, fail);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);

    MinLetRec *result = node;
    if (changed) {
        result = newMinLetRec(CPI(node), new_bindings, new_body);
    }

    UNPROTECT(save);
    LEAVE(ambMinLetRec);
    return result;
}

static MinBindings *ambMinBindings(MinBindings *node, MinExp *fail) {
    ENTER(ambMinBindings);
    if (node == NULL) {
        LEAVE(ambMinBindings);
        return NULL;
    }

    bool changed = false;
    MinExp *new_val = ambMinExp(node->val, fail);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = ambMinBindings(node->next, fail);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
    }

    UNPROTECT(save);
    LEAVE(ambMinBindings);
    return result;
}

static MinExp *ambMinAmb(MinAmb *node, MinExp *fail) {
    ENTER(ambMinAmb);
    if (node == NULL) {
        LEAVE(ambMinAmb);
        return NULL;
    }
    MinExp *new_right = ambMinExp(node->right, fail);
    int save = PROTECT(new_right);
    // create a new failure continuation
    MinExp *fail2 = makeMinExp_Lam(CPI(new_right), NULL, new_right);
    PROTECT(fail2);
    MinExp *result = ambMinExp(node->left, fail2);
    UNPROTECT(save);
    LEAVE(ambMinAmb);
    return result;
}

MinExp *ambMinExp(MinExp *node, MinExp *fail) {
    ENTER(ambMinExp);
    if (node == NULL) {
        LEAVE(ambMinExp);
        return NULL;
    }
    int save = PROTECT(NULL);
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        result = ambMinAmb(getMinExp_Amb(node), fail);
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = ambMinApply(variant, fail);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_BACK: {
        result = makeMinExp_Apply(CPI(node), fail, NULL);
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        // MaybeBigInt
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = ambMinExp(variant, fail);
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
        MinCond *new_variant = ambMinCond(variant, fail);
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
        MinIff *new_variant = ambMinIff(variant, fail);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = ambMinLam(variant, fail);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = ambMinLetRec(variant, fail);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = ambMinExprList(variant, fail);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = ambMinMatch(variant, fail);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = ambMinPrimApp(variant, fail);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = ambMinExprList(variant, fail);
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
    LEAVE(ambMinExp);
    return result;
}

static MinCondCases *ambMinCondCases(MinCondCases *node, MinExp *fail) {
    ENTER(ambMinCondCases);
    if (node == NULL) {
        LEAVE(ambMinCondCases);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinCondCases *result = node;

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = ambMinIntCondCases(variant, fail);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = ambMinCharCondCases(variant, fail);
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
    LEAVE(ambMinCondCases);
    return result;
}
