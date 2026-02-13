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

#include "minlam_occurs.h"

#ifdef DEBUG_MINLAM_OCCURS
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

// Forward declarations
static bool occursMinLam(MinLam *node, SymbolSet *targets);
static bool occursMinExprList(MinExprList *node, SymbolSet *targets);
static bool occursMinPrimApp(MinPrimApp *node, SymbolSet *targets);
static bool occursMinApply(MinApply *node, SymbolSet *targets);
static bool occursMinIff(MinIff *node, SymbolSet *targets);
static bool occursMinCond(MinCond *node, SymbolSet *targets);
static bool occursMinIntCondCases(MinIntCondCases *node, SymbolSet *targets);
static bool occursMinCharCondCases(MinCharCondCases *node, SymbolSet *targets);
static bool occursMinMatch(MinMatch *node, SymbolSet *targets);
static bool occursMinMatchList(MinMatchList *node, SymbolSet *targets);
static bool occursMinLetRec(MinLetRec *node, SymbolSet *targets);
static bool occursMinBindings(MinBindings *node, SymbolSet *targets);
static bool occursMinAmb(MinAmb *node, SymbolSet *targets);
static bool occursMinCondCases(MinCondCases *node, SymbolSet *targets);
static bool occursSymbolList(SymbolList *node, SymbolSet *targets);
static bool occursMinNameSpaceArray(MinNameSpaceArray *node,
                                    SymbolSet *targets);
static bool occursMinBindingsVals(MinBindings *node, SymbolSet *targets);
static bool occursMinBindingsVars(MinBindings *node, SymbolSet *targets);

// Visitor implementations

static bool occursMinLam(MinLam *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursSymbolList(node->args, targets)) {
        return false; // bound here, so does not occur
    }

    return occursMinExp(node->exp, targets);
}

static bool occursMinExprList(MinExprList *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->exp, targets)) {
        return true;
    }

    return occursMinExprList(node->next, targets);
}

static bool occursMinPrimApp(MinPrimApp *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->exp1, targets)) {
        return true;
    }

    return occursMinExp(node->exp2, targets);
}

static bool occursMinApply(MinApply *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->function, targets)) {
        return true;
    }

    return occursMinExprList(node->args, targets);
}

static bool occursMinIff(MinIff *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->condition, targets)) {
        return true;
    }

    if (occursMinExp(node->consequent, targets)) {
        return true;
    }

    if (occursMinExp(node->alternative, targets)) {
        return true;
    }
    return false;
}

static bool occursMinCond(MinCond *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->value, targets)) {
        return true;
    }

    if (occursMinCondCases(node->cases, targets)) {
        return true;
    }

    return false;
}

static bool occursMinIntCondCases(MinIntCondCases *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->body, targets)) {
        return true;
    }

    if (occursMinIntCondCases(node->next, targets)) {
        return true;
    }

    return false;
}

static bool occursMinCharCondCases(MinCharCondCases *node, SymbolSet *targets) {
    ENTER(occursMinCharCondCases);
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->body, targets)) {
        return true;
    }

    if (occursMinCharCondCases(node->next, targets)) {
        return true;
    }

    return false;
}

static bool occursMinMatch(MinMatch *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->index, targets)) {
        return true;
    }

    if (occursMinMatchList(node->cases, targets)) {
        return true;
    }

    return false;
}

static bool occursMinMatchList(MinMatchList *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->body, targets)) {
        return true;
    }
    if (occursMinMatchList(node->next, targets)) {
        return true;
    }

    return false;
}

static bool occursMinLetRec(MinLetRec *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinBindings(node->bindings, targets)) {
        return true;
    }
    if (occursMinBindingsVars(node->bindings, targets)) {
        return false; // bound here, so does not occur
    }
    return occursMinExp(node->body, targets);
}

static bool occursMinBindings(MinBindings *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinBindingsVars(node, targets)) {
        return false; // bound here, so does not occur
    }

    return occursMinBindingsVals(node, targets);
}

static bool occursMinBindingsVars(MinBindings *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (getSymbolSet(targets, node->var)) {
        return true;
    }

    return occursMinBindingsVars(node->next, targets);
}

static bool occursMinBindingsVals(MinBindings *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (occursMinExp(node->val, targets)) {
        return true;
    }

    return occursMinBindingsVals(node->next, targets);
}

static bool occursMinAmb(MinAmb *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    return occursMinExp(node->left, targets) ||
           occursMinExp(node->right, targets);
}

/**
 * Main entry point for MinExp visitor
 *
 * Return true if any symbol in targets occurs free in node.
 */
bool occursMinExp(MinExp *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        MinAmb *variant = getMinExp_Amb(node);
        return occursMinAmb(variant, targets);
    }
    case MINEXP_TYPE_APPLY: {
        MinApply *variant = getMinExp_Apply(node);
        return occursMinApply(variant, targets);
    }
    case MINEXP_TYPE_ARGS: {
        MinExprList *variant = getMinExp_Args(node);
        return occursMinExprList(variant, targets);
    }
    case MINEXP_TYPE_BACK: {
        return false;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        return false;
        break;
    }
    case MINEXP_TYPE_BINDINGS: {
        MinBindings *variant = getMinExp_Bindings(node);
        return occursMinBindings(variant, targets);
    }
    case MINEXP_TYPE_CALLCC: {
        MinExp *variant = getMinExp_CallCC(node);
        return occursMinExp(variant, targets);
    }
    case MINEXP_TYPE_CHARACTER: {
        return false;
    }
    case MINEXP_TYPE_COND: {
        MinCond *variant = getMinExp_Cond(node);
        return occursMinCond(variant, targets);
    }
    case MINEXP_TYPE_ENV: {
        return false;
    }
    case MINEXP_TYPE_ERROR: {
        return false;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        return occursMinIff(variant, targets);
    }
    case MINEXP_TYPE_LAM: {
        MinLam *variant = getMinExp_Lam(node);
        return occursMinLam(variant, targets);
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        return occursMinLetRec(variant, targets);
    }
    case MINEXP_TYPE_LOOKUP: {
        return false;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *variant = getMinExp_MakeVec(node);
        return occursMinExprList(variant, targets);
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        return occursMinMatch(variant, targets);
    }
    case MINEXP_TYPE_NAMESPACES: {
        MinNameSpaceArray *variant = getMinExp_NameSpaces(node);
        return occursMinNameSpaceArray(variant, targets);
    }
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *variant = getMinExp_Prim(node);
        return occursMinPrimApp(variant, targets);
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        return occursMinExprList(variant, targets);
    }
    case MINEXP_TYPE_STDINT: {
        return false;
    }
    case MINEXP_TYPE_VAR: {
        HashSymbol *var = getMinExp_Var(node);
        return getSymbolSet(targets, var);
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }
}

static bool occursMinCondCases(MinCondCases *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        return occursMinIntCondCases(variant, targets);
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        return occursMinCharCondCases(variant, targets);
    }
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
}

static bool occursSymbolList(SymbolList *node, SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    if (getSymbolSet(targets, node->symbol)) {
        return true;
    }

    return occursSymbolList(node->next, targets);
}

static bool occursMinNameSpaceArray(MinNameSpaceArray *node,
                                    SymbolSet *targets) {
    if (node == NULL) {
        return false;
    }

    for (Index i = 0; i < node->size; i++) {
        struct MinExp *element = peeknMinNameSpaceArray(node, i);
        if (occursMinExp(element, targets)) {
            return true;
        }
    }
    return false;
}