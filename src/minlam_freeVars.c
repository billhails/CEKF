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
static void freeVarsMinLam(MinLam *node, SymbolSet *result, SymbolEnv *context);
static void freeVarsMinExprList(MinExprList *node, SymbolSet *result,
                                SymbolEnv *context);
static void freeVarsMinPrimApp(MinPrimApp *node, SymbolSet *result,
                               SymbolEnv *context);
static void freeVarsMinApply(MinApply *node, SymbolSet *result,
                             SymbolEnv *context);
static void freeVarsMinIff(MinIff *node, SymbolSet *result, SymbolEnv *context);
static void freeVarsMinCond(MinCond *node, SymbolSet *result,
                            SymbolEnv *context);
static void freeVarsMinIntCondCases(MinIntCondCases *node, SymbolSet *result,
                                    SymbolEnv *context);
static void freeVarsMinCharCondCases(MinCharCondCases *node, SymbolSet *result,
                                     SymbolEnv *context);
static void freeVarsMinMatch(MinMatch *node, SymbolSet *result,
                             SymbolEnv *context);
static void freeVarsMinMatchList(MinMatchList *node, SymbolSet *result,
                                 SymbolEnv *context);
static void freeVarsMinLetRec(MinLetRec *node, SymbolSet *result,
                              SymbolEnv *context);
static void freeVarsMinBindings(MinBindings *node, SymbolSet *result,
                                SymbolEnv *context);
static void freeVarsMinAmb(MinAmb *node, SymbolSet *result, SymbolEnv *context);
static void freeVarsMinCondCases(MinCondCases *node, SymbolSet *result,
                                 SymbolEnv *context);

static bool isSymbolInEnv(SymbolEnv *env, HashSymbol *symbol) {
    while (env != NULL) {
        if (getSymbolSet(env->bindings, symbol)) {
            return true;
        }
        env = env->parent;
    }
    return false;
}

static void freeVarsMinLam(MinLam *node, SymbolSet *result,
                           SymbolEnv *context) {
    ENTER(freeVarsMinLam);
    if (node == NULL) {
        LEAVE(freeVarsMinLam);
        return;
    }

    SymbolEnv *newContext = newSymbolEnv(context);
    int save = PROTECT(newContext);
    for (SymbolList *param = node->args; param != NULL; param = param->next) {
        setSymbolSet(newContext->bindings, param->symbol);
    }
    freeVarsMinExp(node->exp, result, newContext);
    UNPROTECT(save);
    LEAVE(freeVarsMinLam);
}

static void freeVarsMinExprList(MinExprList *node, SymbolSet *result,
                                SymbolEnv *context) {
    ENTER(freeVarsMinExprList);
    if (node == NULL) {
        LEAVE(freeVarsMinExprList);
        return;
    }

    freeVarsMinExp(node->exp, result, context);
    freeVarsMinExprList(node->next, result, context);
    LEAVE(freeVarsMinExprList);
}

static void freeVarsMinPrimApp(MinPrimApp *node, SymbolSet *result,
                               SymbolEnv *context) {
    ENTER(freeVarsMinPrimApp);
    if (node == NULL) {
        LEAVE(freeVarsMinPrimApp);
        return;
    }

    freeVarsMinExp(node->exp1, result, context);
    freeVarsMinExp(node->exp2, result, context);
    LEAVE(freeVarsMinPrimApp);
}

static void freeVarsMinApply(MinApply *node, SymbolSet *result,
                             SymbolEnv *context) {
    ENTER(freeVarsMinApply);
    if (node == NULL) {
        LEAVE(freeVarsMinApply);
        return;
    }

    freeVarsMinExp(node->function, result, context);
    freeVarsMinExprList(node->args, result, context);
    LEAVE(freeVarsMinApply);
}

static void freeVarsMinIff(MinIff *node, SymbolSet *result,
                           SymbolEnv *context) {
    ENTER(freeVarsMinIff);
    if (node == NULL) {
        LEAVE(freeVarsMinIff);
        return;
    }

    freeVarsMinExp(node->condition, result, context);
    freeVarsMinExp(node->consequent, result, context);
    freeVarsMinExp(node->alternative, result, context);
    LEAVE(freeVarsMinIff);
}

static void freeVarsMinCond(MinCond *node, SymbolSet *result,
                            SymbolEnv *context) {
    ENTER(freeVarsMinCond);
    if (node == NULL) {
        LEAVE(freeVarsMinCond);
        return;
    }

    freeVarsMinExp(node->value, result, context);
    freeVarsMinCondCases(node->cases, result, context);
    LEAVE(freeVarsMinCond);
}

static void freeVarsMinIntCondCases(MinIntCondCases *node, SymbolSet *result,
                                    SymbolEnv *context) {
    ENTER(freeVarsMinIntCondCases);
    if (node == NULL) {
        LEAVE(freeVarsMinIntCondCases);
        return;
    }

    freeVarsMinExp(node->body, result, context);
    freeVarsMinIntCondCases(node->next, result, context);
    LEAVE(freeVarsMinIntCondCases);
}

static void freeVarsMinCharCondCases(MinCharCondCases *node, SymbolSet *result,
                                     SymbolEnv *context) {
    ENTER(freeVarsMinCharCondCases);
    if (node == NULL) {
        LEAVE(freeVarsMinCharCondCases);
        return;
    }

    freeVarsMinExp(node->body, result, context);
    freeVarsMinCharCondCases(node->next, result, context);
    LEAVE(freeVarsMinCharCondCases);
}

static void freeVarsMinMatch(MinMatch *node, SymbolSet *result,
                             SymbolEnv *context) {
    ENTER(freeVarsMinMatch);
    if (node == NULL) {
        LEAVE(freeVarsMinMatch);
        return;
    }

    freeVarsMinExp(node->index, result, context);
    freeVarsMinMatchList(node->cases, result, context);
    LEAVE(freeVarsMinMatch);
}

static void freeVarsMinMatchList(MinMatchList *node, SymbolSet *result,
                                 SymbolEnv *context) {
    ENTER(freeVarsMinMatchList);
    if (node == NULL) {
        LEAVE(freeVarsMinMatchList);
        return;
    }

    freeVarsMinExp(node->body, result, context);
    freeVarsMinMatchList(node->next, result, context);
    LEAVE(freeVarsMinMatchList);
}

static void freeVarsMinLetRec(MinLetRec *node, SymbolSet *result,
                              SymbolEnv *context) {
    ENTER(freeVarsMinLetRec);
    if (node == NULL) {
        LEAVE(freeVarsMinLetRec);
        return;
    }

    SymbolEnv *newContext = newSymbolEnv(context);
    int save = PROTECT(newContext);
    for (MinBindings *binding = node->bindings; binding != NULL;
         binding = binding->next) {
        setSymbolSet(newContext->bindings, binding->var);
    }

    freeVarsMinBindings(node->bindings, result, newContext);
    freeVarsMinExp(node->body, result, newContext);

    UNPROTECT(save);
    LEAVE(freeVarsMinLetRec);
}

static void freeVarsMinBindings(MinBindings *node, SymbolSet *result,
                                SymbolEnv *context) {
    ENTER(freeVarsMinBindings);
    if (node == NULL) {
        LEAVE(freeVarsMinBindings);
        return;
    }

    freeVarsMinExp(node->val, result, context);
    freeVarsMinBindings(node->next, result, context);
    LEAVE(freeVarsMinBindings);
}

static void freeVarsMinAmb(MinAmb *node, SymbolSet *result,
                           SymbolEnv *context) {
    ENTER(freeVarsMinAmb);
    if (node == NULL) {
        LEAVE(freeVarsMinAmb);
        return;
    }

    freeVarsMinExp(node->left, result, context);
    freeVarsMinExp(node->right, result, context);
    LEAVE(freeVarsMinAmb);
}

void freeVarsMinExp(MinExp *node, SymbolSet *result, SymbolEnv *context) {
    ENTER(freeVarsMinExp);
    if (node == NULL) {
        LEAVE(freeVarsMinExp);
        return;
    }

    switch (node->type) {
    case MINEXP_TYPE_AMB:
        freeVarsMinAmb(getMinExp_Amb(node), result, context);
        break;
    case MINEXP_TYPE_APPLY:
        freeVarsMinApply(getMinExp_Apply(node), result, context);
        break;
    case MINEXP_TYPE_ARGS:
        cant_happen("encountered MinArgs");
        break;
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_ERROR:
    case MINEXP_TYPE_STDINT:
        break;
    case MINEXP_TYPE_BINDINGS:
        cant_happen("encountered MinBindings");
        break;
    case MINEXP_TYPE_CALLCC:
        freeVarsMinExp(getMinExp_CallCC(node), result, context);
        break;
    case MINEXP_TYPE_COND:
        freeVarsMinCond(getMinExp_Cond(node), result, context);
        break;
    case MINEXP_TYPE_IFF:
        freeVarsMinIff(getMinExp_Iff(node), result, context);
        break;
    case MINEXP_TYPE_LAM:
        freeVarsMinLam(getMinExp_Lam(node), result, context);
        break;
    case MINEXP_TYPE_LETREC:
        freeVarsMinLetRec(getMinExp_LetRec(node), result, context);
        break;
    case MINEXP_TYPE_MAKEVEC:
        freeVarsMinExprList(getMinExp_MakeVec(node), result, context);
        break;
    case MINEXP_TYPE_MATCH:
        freeVarsMinMatch(getMinExp_Match(node), result, context);
        break;
    case MINEXP_TYPE_PRIM:
        freeVarsMinPrimApp(getMinExp_Prim(node), result, context);
        break;
    case MINEXP_TYPE_SEQUENCE:
        freeVarsMinExprList(getMinExp_Sequence(node), result, context);
        break;
    case MINEXP_TYPE_VAR:
        if (!isSymbolInEnv(context, getMinExp_Var(node))) {
            setSymbolSet(result, getMinExp_Var(node));
        }
        break;
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }

    LEAVE(freeVarsMinExp);
}

static void freeVarsMinCondCases(MinCondCases *node, SymbolSet *result,
                                 SymbolEnv *context) {
    ENTER(freeVarsMinCondCases);
    if (node == NULL) {
        LEAVE(freeVarsMinCondCases);
        return;
    }

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        freeVarsMinIntCondCases(getMinCondCases_Integers(node), result,
                                context);
        break;
    case MINCONDCASES_TYPE_CHARACTERS:
        freeVarsMinCharCondCases(getMinCondCases_Characters(node), result,
                                 context);
        break;
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }

    LEAVE(freeVarsMinCondCases);
}
