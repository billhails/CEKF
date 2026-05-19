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

#include "minlam_size.h"

#ifdef DEBUG_MINLAM_SIZE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static int sizeMinLam(MinLam *);
static int sizeMinAnnotatedVar(MinAnnotatedVar *);
static int sizeMinExprList(MinExprList *);
static int sizeMinPrimApp(MinPrimApp *);
static int sizeMinApply(MinApply *);
static int sizeMinIff(MinIff *);
static int sizeMinCond(MinCond *);
static int sizeMinIntCondCases(MinIntCondCases *);
static int sizeMinCharCondCases(MinCharCondCases *);
static int sizeMinMatch(MinMatch *);
static int sizeMinMatchList(MinMatchList *);
static int sizeMinIntList(MinIntList *);
static int sizeMinLetRec(MinLetRec *);
static int sizeMinBindings(MinBindings *);
static int sizeMinAmb(MinAmb *);
static int sizeMinCondCases(MinCondCases *);

///////////////////////////
// Visitor implementations
///////////////////////////

static int sizeMinLam(MinLam *node) {
    if (node == NULL)
        return 0;
    return (int)countSymbolList(node->args) + sizeMinExp(node->exp) + 1;
}

static int sizeMinAnnotatedVar(MinAnnotatedVar *node) {
    if (node == NULL)
        return 0;
    return 1;
}

static int sizeMinExprList(MinExprList *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->exp) + sizeMinExprList(node->next) + 1;
}

static int sizeMinPrimApp(MinPrimApp *node) {
    if (node == NULL)
        return 0;

    return sizeMinExp(node->exp1) + sizeMinExp(node->exp2) + 1;
}

static int sizeMinApply(MinApply *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->function) + sizeMinExprList(node->args) + 1;
}

static int sizeMinIff(MinIff *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->condition) + sizeMinExp(node->consequent) +
           sizeMinExp(node->alternative) + 1;
}

static int sizeMinCond(MinCond *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->value) + sizeMinCondCases(node->cases) + 1;
}

static int sizeMinIntCondCases(MinIntCondCases *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->body) + sizeMinIntCondCases(node->next) + 1;
}

static int sizeMinCharCondCases(MinCharCondCases *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->body) + sizeMinCharCondCases(node->next) + 1;
}

static int sizeMinMatch(MinMatch *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->index) + sizeMinMatchList(node->cases) + 1;
}

static int sizeMinMatchList(MinMatchList *node) {
    if (node == NULL)
        return 0;
    return sizeMinIntList(node->matches) + sizeMinExp(node->body) +
           sizeMinMatchList(node->next) + 1;
}

static int sizeMinIntList(MinIntList *node) {
    if (node == NULL)
        return 0;
    return sizeMinIntList(node->next) + 1;
}

static int sizeMinLetRec(MinLetRec *node) {
    if (node == NULL)
        return 0;
    return sizeMinBindings(node->bindings) + sizeMinExp(node->body) + 1;
}

static int sizeMinBindings(MinBindings *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->val) + sizeMinBindings(node->next) + 1;
}

static int sizeMinAmb(MinAmb *node) {
    if (node == NULL)
        return 0;
    return sizeMinExp(node->left) + sizeMinExp(node->right) + 1;
}

int sizeMinExp(MinExp *node) {
    if (node == NULL)
        return 0;
    switch (node->type) {
    case MINEXP_TYPE_AMB:
        return sizeMinAmb(getMinExp_Amb(node));
    case MINEXP_TYPE_APPLY:
        return sizeMinApply(getMinExp_Apply(node));
    case MINEXP_TYPE_ARGS:
        return sizeMinExprList(getMinExp_Args(node));
    case MINEXP_TYPE_AVAR:
        return sizeMinAnnotatedVar(getMinExp_Avar(node));
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_DONE:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_VAR:
        return 1;
    case MINEXP_TYPE_BINDINGS:
        return sizeMinBindings(getMinExp_Bindings(node));
    case MINEXP_TYPE_CALLCC:
        return sizeMinExp(getMinExp_CallCC(node));
    case MINEXP_TYPE_CUT:
        return sizeMinExp(getMinExp_Cut(node));
    case MINEXP_TYPE_COND:
        return sizeMinCond(getMinExp_Cond(node));
    case MINEXP_TYPE_IFF:
        return sizeMinIff(getMinExp_Iff(node));
    case MINEXP_TYPE_LAM:
        return sizeMinLam(getMinExp_Lam(node));
    case MINEXP_TYPE_LETREC:
        return sizeMinLetRec(getMinExp_LetRec(node));
    case MINEXP_TYPE_MAKEVEC:
        return sizeMinExprList(getMinExp_MakeVec(node));
    case MINEXP_TYPE_MATCH:
        return sizeMinMatch(getMinExp_Match(node));
    case MINEXP_TYPE_PRIM:
        return sizeMinPrimApp(getMinExp_Prim(node));
    case MINEXP_TYPE_SEQUENCE:
        return sizeMinExprList(getMinExp_Sequence(node));
    default:
        cant_happen("unrecognized MinExp type %s", minExpTypeName(node->type));
    }
    return 0;
}

static int sizeMinCondCases(MinCondCases *node) {
    if (node == NULL)
        return 0;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        return sizeMinIntCondCases(getMinCondCases_Integers(node));
    case MINCONDCASES_TYPE_CHARACTERS:
        return sizeMinCharCondCases(getMinCondCases_Characters(node));
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
    return 0;
}