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

#include "minlam.h"

#include "minlam_inSafe.h"

#ifdef DEBUG_MINLAM_INSAFE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static bool inSafeMinLam(MinLam *node);
static bool inSafeMinExprList(MinExprList *node);
static bool inSafeMinPrimApp(MinPrimApp *node);
static bool inSafeMinApply(MinApply *node);
static bool inSafeMinIff(MinIff *node);
static bool inSafeMinCond(MinCond *node);
static bool inSafeMinIntCondCases(MinIntCondCases *node);
static bool inSafeMinCharCondCases(MinCharCondCases *node);
static bool inSafeMinMatch(MinMatch *node);
static bool inSafeMinMatchList(MinMatchList *node);
static bool inSafeMinBindings(MinBindings *node);
static bool inSafeMinExpInternal(MinExp *node);
static bool inSafeMinCondCases(MinCondCases *node);

static bool inSafeMinLam(MinLam *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->exp);
}

static bool inSafeMinExprList(MinExprList *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->exp) && inSafeMinExprList(node->next);
}

static bool inSafeMinPrimApp(MinPrimApp *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->exp1) && inSafeMinExpInternal(node->exp2);
}

static bool inSafeMinApply(MinApply *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->function) &&
           inSafeMinExprList(node->args);
}

static bool inSafeMinIff(MinIff *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->condition) &&
           inSafeMinExpInternal(node->consequent) &&
           inSafeMinExpInternal(node->alternative);
}

static bool inSafeMinCond(MinCond *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->value) && inSafeMinCondCases(node->cases);
}

static bool inSafeMinIntCondCases(MinIntCondCases *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->body) &&
           inSafeMinIntCondCases(node->next);
}

static bool inSafeMinCharCondCases(MinCharCondCases *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->body) &&
           inSafeMinCharCondCases(node->next);
}

static bool inSafeMinMatch(MinMatch *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->index) && inSafeMinMatchList(node->cases);
}

static bool inSafeMinMatchList(MinMatchList *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->body) && inSafeMinMatchList(node->next);
}

static bool inSafeMinBindings(MinBindings *node) {
    if (node == NULL) {
        return true;
    }
    return inSafeMinExpInternal(node->val) && inSafeMinBindings(node->next);
}

static bool inSafeMinExpInternal(MinExp *node) {
    if (node == NULL) {
        return true;
    }
    switch (node->type) {
    case MINEXP_TYPE_AMB:
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_CALLCC:
    case MINEXP_TYPE_DONE:
    case MINEXP_TYPE_LETREC:
        return false;
    case MINEXP_TYPE_APPLY:
        return inSafeMinApply(getMinExp_Apply(node));
    case MINEXP_TYPE_ARGS:
        return inSafeMinExprList(getMinExp_Args(node));
    case MINEXP_TYPE_AVAR:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_VAR:
        return true;
    case MINEXP_TYPE_BINDINGS:
        return inSafeMinBindings(getMinExp_Bindings(node));
    case MINEXP_TYPE_COND:
        return inSafeMinCond(getMinExp_Cond(node));
    case MINEXP_TYPE_IFF:
        return inSafeMinIff(getMinExp_Iff(node));
    case MINEXP_TYPE_LAM:
        return inSafeMinLam(getMinExp_Lam(node));
    case MINEXP_TYPE_MAKEVEC:
        return inSafeMinExprList(getMinExp_MakeVec(node));
    case MINEXP_TYPE_MATCH:
        return inSafeMinMatch(getMinExp_Match(node));
    case MINEXP_TYPE_PRIM:
        return inSafeMinPrimApp(getMinExp_Prim(node));
    case MINEXP_TYPE_SEQUENCE:
        return inSafeMinExprList(getMinExp_Sequence(node));
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }
}

static bool inSafeMinCondCases(MinCondCases *node) {
    if (node == NULL) {
        return true;
    }
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        return inSafeMinIntCondCases(getMinCondCases_Integers(node));
    case MINCONDCASES_TYPE_CHARACTERS:
        return inSafeMinCharCondCases(getMinCondCases_Characters(node));
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
}

bool inSafeMinExp(MinExp *node) { return inSafeMinExpInternal(node); }
