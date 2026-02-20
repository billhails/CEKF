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
#include "utils.h"

#include "minlam_freeVars.h"

#ifdef DEBUG_MINLAM_FREEVARS
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

// Forward declarations
static SymbolSet *freeVarsMinLam(MinLam *node, SymbolEnv *context);
static SymbolSet *freeVarsMinExprList(MinExprList *node, SymbolEnv *context);
static SymbolSet *freeVarsMinPrimApp(MinPrimApp *node, SymbolEnv *context);
static SymbolSet *freeVarsMinApply(MinApply *node, SymbolEnv *context);
static SymbolSet *freeVarsMinLookUp(MinLookUp *node, SymbolEnv *context);
static SymbolSet *freeVarsMinIff(MinIff *node, SymbolEnv *context);
static SymbolSet *freeVarsMinCond(MinCond *node, SymbolEnv *context);
static SymbolSet *freeVarsMinIntCondCases(MinIntCondCases *node,
                                          SymbolEnv *context);
static SymbolSet *freeVarsMinCharCondCases(MinCharCondCases *node,
                                           SymbolEnv *context);
static SymbolSet *freeVarsMinMatch(MinMatch *node, SymbolEnv *context);
static SymbolSet *freeVarsMinMatchList(MinMatchList *node, SymbolEnv *context);
static SymbolSet *freeVarsMinLetRec(MinLetRec *node, SymbolEnv *context);
static SymbolSet *freeVarsMinBindings(MinBindings *node, SymbolEnv *context);
static SymbolSet *freeVarsMinAmb(MinAmb *node, SymbolEnv *context);
static SymbolSet *freeVarsMinCondCases(MinCondCases *node, SymbolEnv *context);

static SymbolSet *mergeSymbolSets(SymbolSet *a, SymbolSet *b) {
    if (a == NULL)
        return b;
    if (b == NULL)
        return a;

    SymbolSet *result = newSymbolSet();
    int save = PROTECT(result);
    // Add all elements from set a
    HashSymbol *symbol = NULL;
    Index i = 0;
    while ((symbol = iterateSymbolSet(a, &i)) != NULL) {
        setSymbolSet(result, symbol);
    }
    i = 0;
    while ((symbol = iterateSymbolSet(b, &i)) != NULL) {
        setSymbolSet(result, symbol);
    }
    UNPROTECT(save);
    return result;
}

static bool isSymbolInEnv(SymbolEnv *env, HashSymbol *symbol) {
    while (env != NULL) {
        if (getSymbolSet(env->bindings, symbol)) {
            return true;
        }
        env = env->parent;
    }
    return false;
}

// Visitor implementations

static SymbolSet *freeVarsMinLam(MinLam *node, SymbolEnv *context) {
    ENTER(freeVarsMinLam);
    if (node == NULL) {
        LEAVE(freeVarsMinLam);
        return NULL;
    }

    SymbolEnv *new_context = newSymbolEnv(context);
    int save = PROTECT(new_context);
    // Add parameters to the new context
    for (SymbolList *param = node->args; param != NULL; param = param->next) {
        setSymbolSet(new_context->bindings, param->symbol);
    }
    SymbolSet *result = freeVarsMinExp(node->exp, new_context);
    UNPROTECT(save);
    LEAVE(freeVarsMinLam);
    return result;
}

static SymbolSet *freeVarsMinExprList(MinExprList *node, SymbolEnv *context) {
    ENTER(freeVarsMinExprList);
    if (node == NULL) {
        LEAVE(freeVarsMinExprList);
        return NULL;
    }

    SymbolSet *this = freeVarsMinExp(node->exp, context);
    int save = PROTECT(this);
    SymbolSet *rest = freeVarsMinExprList(node->next, context);
    PROTECT(rest);
    SymbolSet *result = mergeSymbolSets(this, rest);
    UNPROTECT(save);
    LEAVE(freeVarsMinExprList);
    return result;
}

static SymbolSet *freeVarsMinPrimApp(MinPrimApp *node, SymbolEnv *context) {
    ENTER(freeVarsMinPrimApp);
    if (node == NULL) {
        LEAVE(freeVarsMinPrimApp);
        return NULL;
    }

    SymbolSet *exp1 = freeVarsMinExp(node->exp1, context);
    int save = PROTECT(exp1);
    SymbolSet *exp2 = freeVarsMinExp(node->exp2, context);
    PROTECT(exp2);
    SymbolSet *result = mergeSymbolSets(exp1, exp2);

    UNPROTECT(save);
    LEAVE(freeVarsMinPrimApp);
    return result;
}

static SymbolSet *freeVarsMinApply(MinApply *node, SymbolEnv *context) {
    ENTER(freeVarsMinApply);
    if (node == NULL) {
        LEAVE(freeVarsMinApply);
        return NULL;
    }

    SymbolSet *fnSet = freeVarsMinExp(node->function, context);
    int save = PROTECT(fnSet);
    SymbolSet *argSet = freeVarsMinExprList(node->args, context);
    PROTECT(argSet);
    SymbolSet *result = mergeSymbolSets(fnSet, argSet);

    UNPROTECT(save);
    LEAVE(freeVarsMinApply);
    return result;
}

static SymbolSet *freeVarsMinLookUp(MinLookUp *node, SymbolEnv *context) {
    ENTER(freeVarsMinLookUp);
    if (node == NULL) {
        LEAVE(freeVarsMinLookUp);
        return NULL;
    }

    // this is ok because the program has already been alpha-renamed
    SymbolSet *expSet = freeVarsMinExp(node->exp, context);
    LEAVE(freeVarsMinLookUp);
    return expSet;
}

static SymbolSet *freeVarsMinIff(MinIff *node, SymbolEnv *context) {
    ENTER(freeVarsMinIff);
    if (node == NULL) {
        LEAVE(freeVarsMinIff);
        return NULL;
    }

    SymbolSet *conditionSet = freeVarsMinExp(node->condition, context);
    int save = PROTECT(conditionSet);
    SymbolSet *consequentSet = freeVarsMinExp(node->consequent, context);
    PROTECT(consequentSet);
    SymbolSet *alternativeSet = freeVarsMinExp(node->alternative, context);
    PROTECT(alternativeSet);

    SymbolSet *result = mergeSymbolSets(conditionSet, consequentSet);
    PROTECT(result);
    result = mergeSymbolSets(result, alternativeSet);

    UNPROTECT(save);
    LEAVE(freeVarsMinIff);
    return result;
}

static SymbolSet *freeVarsMinCond(MinCond *node, SymbolEnv *context) {
    ENTER(freeVarsMinCond);
    if (node == NULL) {
        LEAVE(freeVarsMinCond);
        return NULL;
    }

    SymbolSet *valueSet = freeVarsMinExp(node->value, context);
    int save = PROTECT(valueSet);
    SymbolSet *casesSet = freeVarsMinCondCases(node->cases, context);
    PROTECT(casesSet);
    SymbolSet *result = mergeSymbolSets(valueSet, casesSet);

    UNPROTECT(save);
    LEAVE(freeVarsMinCond);
    return result;
}

static SymbolSet *freeVarsMinIntCondCases(MinIntCondCases *node,
                                          SymbolEnv *context) {
    ENTER(freeVarsMinIntCondCases);
    if (node == NULL) {
        LEAVE(freeVarsMinIntCondCases);
        return NULL;
    }

    SymbolSet *bodySet = freeVarsMinExp(node->body, context);
    int save = PROTECT(bodySet);
    SymbolSet *nextSet = freeVarsMinIntCondCases(node->next, context);
    PROTECT(nextSet);
    SymbolSet *result = mergeSymbolSets(bodySet, nextSet);

    UNPROTECT(save);
    LEAVE(freeVarsMinIntCondCases);
    return result;
}

static SymbolSet *freeVarsMinCharCondCases(MinCharCondCases *node,
                                           SymbolEnv *context) {
    ENTER(freeVarsMinCharCondCases);
    if (node == NULL) {
        LEAVE(freeVarsMinCharCondCases);
        return NULL;
    }

    SymbolSet *bodySet = freeVarsMinExp(node->body, context);
    int save = PROTECT(bodySet);
    SymbolSet *nextSet = freeVarsMinCharCondCases(node->next, context);
    PROTECT(nextSet);
    SymbolSet *result = mergeSymbolSets(bodySet, nextSet);

    UNPROTECT(save);
    LEAVE(freeVarsMinCharCondCases);
    return result;
}

static SymbolSet *freeVarsMinMatch(MinMatch *node, SymbolEnv *context) {
    ENTER(freeVarsMinMatch);
    if (node == NULL) {
        LEAVE(freeVarsMinMatch);
        return NULL;
    }

    SymbolSet *indexSet = freeVarsMinExp(node->index, context);
    int save = PROTECT(indexSet);
    SymbolSet *casesSet = freeVarsMinMatchList(node->cases, context);
    PROTECT(casesSet);
    SymbolSet *result = mergeSymbolSets(indexSet, casesSet);

    UNPROTECT(save);
    LEAVE(freeVarsMinMatch);
    return result;
}

static SymbolSet *freeVarsMinMatchList(MinMatchList *node, SymbolEnv *context) {
    ENTER(freeVarsMinMatchList);
    if (node == NULL) {
        LEAVE(freeVarsMinMatchList);
        return NULL;
    }

    SymbolSet *bodySet = freeVarsMinExp(node->body, context);
    int save = PROTECT(bodySet);
    SymbolSet *nextSet = freeVarsMinMatchList(node->next, context);
    PROTECT(nextSet);
    SymbolSet *result = mergeSymbolSets(bodySet, nextSet);

    UNPROTECT(save);
    LEAVE(freeVarsMinMatchList);
    return result;
}

static SymbolSet *freeVarsMinLetRec(MinLetRec *node, SymbolEnv *context) {
    ENTER(freeVarsMinLetRec);
    if (node == NULL) {
        LEAVE(freeVarsMinLetRec);
        return NULL;
    }

    SymbolEnv *new_context = newSymbolEnv(context);
    int save = PROTECT(new_context);
    for (MinBindings *binding = node->bindings; binding != NULL;
         binding = binding->next) {
        setSymbolSet(new_context->bindings, binding->var);
    }
    SymbolSet *bindingsSet = freeVarsMinBindings(node->bindings, new_context);
    PROTECT(bindingsSet);
    SymbolSet *bodySet = freeVarsMinExp(node->body, new_context);
    PROTECT(bodySet);
    SymbolSet *result = mergeSymbolSets(bindingsSet, bodySet);

    UNPROTECT(save);
    LEAVE(freeVarsMinLetRec);
    return result;
}

static SymbolSet *freeVarsMinBindings(MinBindings *node, SymbolEnv *context) {
    ENTER(freeVarsMinBindings);
    if (node == NULL) {
        LEAVE(freeVarsMinBindings);
        return NULL;
    }

    SymbolSet *this = freeVarsMinExp(node->val, context);
    int save = PROTECT(this);
    SymbolSet *next = freeVarsMinBindings(node->next, context);
    PROTECT(next);
    SymbolSet *result = mergeSymbolSets(this, next);

    UNPROTECT(save);
    LEAVE(freeVarsMinBindings);
    return result;
}

static SymbolSet *freeVarsMinAmb(MinAmb *node, SymbolEnv *context) {
    ENTER(freeVarsMinAmb);
    if (node == NULL) {
        LEAVE(freeVarsMinAmb);
        return NULL;
    }

    SymbolSet *leftSet = freeVarsMinExp(node->left, context);
    int save = PROTECT(leftSet);
    SymbolSet *rightSet = freeVarsMinExp(node->right, context);
    PROTECT(rightSet);
    SymbolSet *result = mergeSymbolSets(leftSet, rightSet);

    UNPROTECT(save);
    LEAVE(freeVarsMinAmb);
    return result;
}

SymbolSet *freeVarsMinExp(MinExp *node, SymbolEnv *context) {
    ENTER(freeVarsMinExp);
    if (node == NULL) {
        LEAVE(freeVarsMinExp);
        return NULL;
    }

    switch (node->type) {
    case MINEXP_TYPE_AMB:
        return freeVarsMinAmb(getMinExp_Amb(node), context);
    case MINEXP_TYPE_APPLY:
        return freeVarsMinApply(getMinExp_Apply(node), context);
    case MINEXP_TYPE_ARGS:
        cant_happen("encountered MinArgs");
        break;
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_ENV:
    case MINEXP_TYPE_ERROR:
    case MINEXP_TYPE_STDINT:
        return NULL;
    case MINEXP_TYPE_BINDINGS:
        cant_happen("encountered MinBindings");
    case MINEXP_TYPE_CALLCC:
        return freeVarsMinExp(getMinExp_CallCC(node), context);
    case MINEXP_TYPE_COND:
        return freeVarsMinCond(getMinExp_Cond(node), context);
    case MINEXP_TYPE_IFF:
        return freeVarsMinIff(getMinExp_Iff(node), context);
    case MINEXP_TYPE_LAM:
        return freeVarsMinLam(getMinExp_Lam(node), context);
    case MINEXP_TYPE_LETREC:
        return freeVarsMinLetRec(getMinExp_LetRec(node), context);
    case MINEXP_TYPE_LOOKUP:
        return freeVarsMinLookUp(getMinExp_LookUp(node), context);
    case MINEXP_TYPE_MAKEVEC:
        return freeVarsMinExprList(getMinExp_MakeVec(node), context);
    case MINEXP_TYPE_MATCH:
        return freeVarsMinMatch(getMinExp_Match(node), context);
    case MINEXP_TYPE_NAMESPACES:
        return NULL;
    case MINEXP_TYPE_PRIM:
        return freeVarsMinPrimApp(getMinExp_Prim(node), context);
    case MINEXP_TYPE_SEQUENCE:
        return freeVarsMinExprList(getMinExp_Sequence(node), context);
    case MINEXP_TYPE_VAR: {
        if (isSymbolInEnv(context, getMinExp_Var(node))) {
            return NULL;
        } else {
            SymbolSet *result = newSymbolSet();
            int save = PROTECT(result);
            setSymbolSet(result, getMinExp_Var(node));
            UNPROTECT(save);
            return result;
        }
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }
}

static SymbolSet *freeVarsMinCondCases(MinCondCases *node, SymbolEnv *context) {
    ENTER(freeVarsMinCondCases);
    if (node == NULL) {
        LEAVE(freeVarsMinCondCases);
        return NULL;
    }

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        return freeVarsMinIntCondCases(getMinCondCases_Integers(node), context);
    case MINCONDCASES_TYPE_CHARACTERS:
        return freeVarsMinCharCondCases(getMinCondCases_Characters(node),
                                        context);
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
}