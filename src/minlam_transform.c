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

#include "minlam_transform.h"

#ifdef DEBUG_MINLAM_TRANSFORM
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

// (MinExp -> MinExp) -> MinExp -> MinExp
typedef MinExp *(*MinExpTransform)(MinExpFn, MinExp *);

// Context here is like a curry of t(a) as a closure.
typedef struct Context {
    MinExpTransform t;
    MinExpFn a;
} Context;

static MinExp *transformMinLam(MinLam *node, Context *c);
static MinExp *transformMinApply(MinApply *node, Context *c);
static MinExp *transformMinIff(MinIff *node, Context *c);
static MinExp *transformMinCond(MinCond *node, Context *c);
static MinExp *transformMinMatch(MinMatch *node, Context *c);
static MinExp *transformMinLetRec(MinLetRec *node, Context *c);
static MinExp *transformMinAmb(MinAmb *node, Context *c);
static MinExp *transformMinMakeVec(MinExprList *node, Context *c);
static MinExp *transformMinSequence(MinExprList *node, Context *c);
static MinExp *transformMinExp(MinExp *node, Context *c);

static MinExprList *transformMinExprList(MinExprList *node, Context *c);
static MinIntCondCases *transformMinIntCondCases(MinIntCondCases *node,
                                                 Context *c);
static MinCharCondCases *transformMinCharCondCases(MinCharCondCases *node,
                                                   Context *c);
static MinMatchList *transformMinMatchList(MinMatchList *node, Context *c);
static MinBindings *transformMinBindings(MinBindings *node, Context *c);
static MinCondCases *transformMinCondCases(MinCondCases *node, Context *c);

//////////////////////////
// Support and public API
//////////////////////////

//  fn top_down(f, exp) {
//      _transform(top_down, f, f(exp))
//  }
MinExp *topDownMinExp(MinExpFn f, MinExp *exp) {
    MinExp *new_exp = f(exp);
    int save = PROTECT(new_exp);
    Context c = {topDownMinExp, f};
    MinExp *result = transformMinExp(new_exp, &c);
    UNPROTECT(save);
    return result;
}

//  fn bottom_up(f, exp) {
//      f(_transform(bottom_up, f, exp))
//  }
MinExp *bottomUpMinExp(MinExpFn f, MinExp *exp) {
    Context c = {bottomUpMinExp, f};
    MinExp *new_exp = transformMinExp(exp, &c);
    int save = PROTECT(new_exp);
    MinExp *result = f(new_exp);
    UNPROTECT(save);
    return result;
}

static inline MinExp *apply(Context *c, MinExp *node) {
    return c->t(c->a, node);
}

///////////////////////////
// Visitor implementations
///////////////////////////

static MinExp *transformMinLam(MinLam *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinLam);
    MinExp *new_exp = apply(c, node->exp);
    int save = PROTECT(new_exp);
    MinExp *result = makeMinExp_Lam(CPI(node), node->args, new_exp);
    UNPROTECT(save);
    LEAVE(transformMinLam);
    return result;
}

static MinExprList *transformMinExprList(MinExprList *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinExprList);
    MinExp *new_exp = apply(c, node->exp);
    int save = PROTECT(new_exp);
    MinExprList *new_next = transformMinExprList(node->next, c);
    PROTECT(new_next);
    MinExprList *result = newMinExprList(CPI(node), new_exp, new_next);
    UNPROTECT(save);
    LEAVE(transformMinExprList);
    return result;
}

static MinExp *transformMinApply(MinApply *node, Context *c) {
    if (node == NULL)
        return NULL;
    if (node->isBuiltin)
        return newMinExp_Apply(CPI(node), node);
    ENTER(transformMinApply);
    MinExp *new_function = apply(c, node->function);
    int save = PROTECT(new_function);
    MinExprList *new_args = transformMinExprList(node->args, c);
    PROTECT(new_args);
    MinExp *result = makeMinExp_Apply(CPI(node), new_function, new_args);
    getMinExp_Apply(result)->isBuiltin = node->isBuiltin;
    UNPROTECT(save);
    LEAVE(transformMinApply);
    return result;
}

static MinExp *transformMinIff(MinIff *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinIff);
    MinExp *new_condition = apply(c, node->condition);
    int save = PROTECT(new_condition);
    MinExp *new_consequent = apply(c, node->consequent);
    PROTECT(new_consequent);
    MinExp *new_alternative = apply(c, node->alternative);
    PROTECT(new_alternative);
    MinExp *result = makeMinExp_Iff(CPI(node), new_condition, new_consequent,
                                    new_alternative);
    UNPROTECT(save);
    LEAVE(transformMinIff);
    return result;
}

static MinExp *transformMinCond(MinCond *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinCond);
    MinExp *new_value = apply(c, node->value);
    int save = PROTECT(new_value);
    MinCondCases *new_cases = transformMinCondCases(node->cases, c);
    PROTECT(new_cases);
    MinExp *result = makeMinExp_Cond(CPI(node), new_value, new_cases);
    UNPROTECT(save);
    LEAVE(transformMinCond);
    return result;
}

static MinCondCases *transformMinCondCases(MinCondCases *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinCondCases);
    int save = PROTECT(NULL);
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *new_variant =
            transformMinIntCondCases(getMinCondCases_Integers(node), c);
        PROTECT(new_variant);
        result = newMinCondCases_Integers(CPI(node), new_variant);
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *new_variant =
            transformMinCharCondCases(getMinCondCases_Characters(node), c);
        PROTECT(new_variant);
        result = newMinCondCases_Characters(CPI(node), new_variant);
        break;
    }
    default:
        cant_happen("unrecognized MinCondCases type %s",
                    minCondCasesTypeName(node->type));
    }
    UNPROTECT(save);
    LEAVE(transformMinCondCases);
    return result;
}

static MinIntCondCases *transformMinIntCondCases(MinIntCondCases *node,
                                                 Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinIntCondCases);
    MinExp *new_body = apply(c, node->body);
    int save = PROTECT(new_body);
    MinIntCondCases *new_next = transformMinIntCondCases(node->next, c);
    PROTECT(new_next);
    MinIntCondCases *result =
        newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    UNPROTECT(save);
    LEAVE(transformMinIntCondCases);
    return result;
}

static MinCharCondCases *transformMinCharCondCases(MinCharCondCases *node,
                                                   Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinCharCondCases);
    MinExp *new_body = apply(c, node->body);
    int save = PROTECT(new_body);
    MinCharCondCases *new_next = transformMinCharCondCases(node->next, c);
    PROTECT(new_next);
    MinCharCondCases *result =
        newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
    UNPROTECT(save);
    LEAVE(transformMinCharCondCases);
    return result;
}

static MinExp *transformMinMatch(MinMatch *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinMatch);
    MinExp *new_index = apply(c, node->index);
    int save = PROTECT(new_index);
    MinMatchList *new_cases = transformMinMatchList(node->cases, c);
    PROTECT(new_cases);
    MinExp *result = makeMinExp_Match(CPI(node), new_index, new_cases);
    UNPROTECT(save);
    LEAVE(transformMinMatch);
    return result;
}

static MinMatchList *transformMinMatchList(MinMatchList *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinMatchList);
    MinExp *new_body = apply(c, node->body);
    int save = PROTECT(new_body);
    MinMatchList *new_next = transformMinMatchList(node->next, c);
    PROTECT(new_next);
    MinMatchList *result =
        newMinMatchList(CPI(node), node->matches, new_body, new_next);
    UNPROTECT(save);
    LEAVE(transformMinMatchList);
    return result;
}

static MinExp *transformMinLetRec(MinLetRec *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinLetRec);
    MinBindings *new_bindings = transformMinBindings(node->bindings, c);
    int save = PROTECT(new_bindings);
    MinExp *new_body = apply(c, node->body);
    PROTECT(new_body);
    MinExp *result = makeMinExp_LetRec(CPI(node), new_bindings, new_body);
    UNPROTECT(save);
    LEAVE(transformMinLetRec);
    return result;
}

static MinBindings *transformMinBindings(MinBindings *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinBindings);
    MinExp *new_val = apply(c, node->val);
    int save = PROTECT(new_val);
    MinBindings *new_next = transformMinBindings(node->next, c);
    PROTECT(new_next);
    MinBindings *result =
        newMinBindings(CPI(node), node->var, new_val, new_next);
    result->arity = node->arity;
    UNPROTECT(save);
    LEAVE(transformMinBindings);
    return result;
}

static MinExp *transformMinAmb(MinAmb *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinAmb);
    MinExp *new_left = apply(c, node->left);
    int save = PROTECT(new_left);
    MinExp *new_right = apply(c, node->right);
    PROTECT(new_right);
    MinExp *result = makeMinExp_Amb(CPI(node), new_left, new_right);
    UNPROTECT(save);
    LEAVE(transformMinAmb);
    return result;
}

static MinExp *transformMinMakeVec(MinExprList *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinMakeVec);
    MinExprList *new_exprs = transformMinExprList(node, c);
    int save = PROTECT(new_exprs);
    MinExp *result = newMinExp_MakeVec(CPI(node), new_exprs);
    UNPROTECT(save);
    LEAVE(transformMinMakeVec);
    return result;
}

static MinExp *transformMinSequence(MinExprList *node, Context *c) {
    if (node == NULL)
        return NULL;
    ENTER(transformMinSequence);
    MinExprList *new_exprs = transformMinExprList(node, c);
    int save = PROTECT(new_exprs);
    MinExp *result = newMinExp_Sequence(CPI(node), new_exprs);
    UNPROTECT(save);
    LEAVE(transformMinSequence);
    return result;
}

static MinExp *transformMinExp(MinExp *node, Context *c) {
    ENTER(transformMinExp);
    if (node == NULL) {
        LEAVE(transformMinExp);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinExp *result = node;

    switch (node->type) {
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_VAR:
    case MINEXP_TYPE_PRIM:
        break;

    case MINEXP_TYPE_AMB:
        result = transformMinAmb(getMinExp_Amb(node), c);
        break;

    case MINEXP_TYPE_APPLY:
        result = transformMinApply(getMinExp_Apply(node), c);
        break;

    case MINEXP_TYPE_CALLCC:
        // MinExp
        result = transformMinExp(getMinExp_CallCC(node), c);
        break;

    case MINEXP_TYPE_COND:
        result = transformMinCond(getMinExp_Cond(node), c);
        break;

    case MINEXP_TYPE_IFF:
        result = transformMinIff(getMinExp_Iff(node), c);
        break;

    case MINEXP_TYPE_LAM:
        result = transformMinLam(getMinExp_Lam(node), c);
        break;

    case MINEXP_TYPE_LETREC:
        result = transformMinLetRec(getMinExp_LetRec(node), c);
        break;

    case MINEXP_TYPE_MAKEVEC:
        // MinExprList
        result = transformMinMakeVec(getMinExp_MakeVec(node), c);
        break;

    case MINEXP_TYPE_MATCH:
        result = transformMinMatch(getMinExp_Match(node), c);
        break;

    case MINEXP_TYPE_SEQUENCE:
        // MinExprList
        result = transformMinSequence(getMinExp_Sequence(node), c);
        break;

    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }

    UNPROTECT(save);
    LEAVE(transformMinExp);
    return result;
}
