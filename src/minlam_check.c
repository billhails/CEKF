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

#include "minlam_check.h"
#include "memory.h"
#include "minlam_helper.h"
#include "utils_helper.h"

static void checkMinLam(MinLam *, SymbolSet *);
static void checkMinAnnotatedVar(MinAnnotatedVar *, SymbolSet *);
static void checkMinExprList(MinExprList *, SymbolSet *);
static void checkMinPrimApp(MinPrimApp *, SymbolSet *);
static void checkMinApply(MinApply *, SymbolSet *);
static void checkMinIff(MinIff *, SymbolSet *);
static void checkMinCond(MinCond *, SymbolSet *);
static void checkMinIntCondCases(MinIntCondCases *, SymbolSet *);
static void checkMinCharCondCases(MinCharCondCases *, SymbolSet *);
static void checkMinMatch(MinMatch *, SymbolSet *);
static void checkMinMatchList(MinMatchList *, SymbolSet *);
static void checkMinLetRec(MinLetRec *, SymbolSet *);
static void checkMinBindings(MinBindings *, SymbolSet *);
static void checkMinAmb(MinAmb *, SymbolSet *);
static void checkMinExpI(MinExp *, SymbolSet *);
static void checkMinCondCases(MinCondCases *, SymbolSet *);

///////
// API
///////

void checkMinExp(MinExp *node, BuiltIns *builtIns) {
    SymbolSet *context = newSymbolSet();
    int save = PROTECT(context);
    for (Index i = 0; i < builtIns->size; i++) {
        setSymbolSet(context, builtIns->entries[i]->internalName);
    }
    checkMinExpI(node, context);
    UNPROTECT(save);
}

///////////////////
// Helper Routines
///////////////////

static SymbolSet *addSymbolListToContext(SymbolSet *set, SymbolList *list) {
    SymbolSet *new = copySymbolSet(set);
    int save = PROTECT(new);
    while (list != NULL) {
        setSymbolSet(new, list->symbol);
        list = list->next;
    }
    UNPROTECT(save);
    return new;
}

static SymbolSet *addBindingsToContext(MinBindings *bindings,
                                       SymbolSet *context) {
    SymbolList *keys = minBindingsToSymbolList(bindings);
    int save = PROTECT(keys);
    SymbolSet *new = addSymbolListToContext(context, keys);
    UNPROTECT(save);
    return new;
}

static void assertBound(HashSymbol *symbol, SymbolSet *context) {
    if (!getSymbolSet(context, symbol)) {
        cant_happen("unbound variable %s", symbol->name);
    }
}

///////////////////////////
// Visitor implementations
///////////////////////////

static void checkMinLam(MinLam *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinLam");
    SymbolSet *newContext = addSymbolListToContext(context, node->args);
    int save = PROTECT(newContext);
    checkMinExpI(node->exp, newContext);
    UNPROTECT(save);
}

static void checkMinAnnotatedVar(MinAnnotatedVar *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinAnnotatedVar");
    assertBound(node->var, context);
}

static void checkMinExprList(MinExprList *node, SymbolSet *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->exp, context);
    checkMinExprList(node->next, context);
}

static void checkMinPrimApp(MinPrimApp *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinPrimApp");
    checkMinExpI(node->exp1, context);
    checkMinExpI(node->exp2, context);
}

static void checkMinApply(MinApply *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinApply");
    checkMinExpI(node->function, context);
    checkMinExprList(node->args, context);
}

static void checkMinIff(MinIff *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinIff");
    checkMinExpI(node->condition, context);
    checkMinExpI(node->consequent, context);
    checkMinExpI(node->alternative, context);
}

static void checkMinCond(MinCond *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinCond");
    checkMinExpI(node->value, context);
    checkMinCondCases(node->cases, context);
}

static void checkMinIntCondCases(MinIntCondCases *node, SymbolSet *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->body, context);
    checkMinIntCondCases(node->next, context);
}

static void checkMinCharCondCases(MinCharCondCases *node, SymbolSet *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->body, context);
    checkMinCharCondCases(node->next, context);
}

static void checkMinMatch(MinMatch *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinMatch");
    checkMinExpI(node->index, context);
    checkMinMatchList(node->cases, context);
}

static void checkMinMatchList(MinMatchList *node, SymbolSet *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->body, context);
    checkMinMatchList(node->next, context);
}

static void checkMinLetRec(MinLetRec *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinLetRec");
    SymbolSet *newContext = addBindingsToContext(node->bindings, context);
    int save = PROTECT(newContext);
    checkMinBindings(node->bindings, newContext);
    checkMinExpI(node->body, newContext);
    UNPROTECT(save);
}

static void checkMinBindings(MinBindings *node, SymbolSet *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->val, context);
    checkMinBindings(node->next, context);
}

static void checkMinAmb(MinAmb *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinAmb");
    checkMinExpI(node->left, context);
    checkMinExpI(node->right, context);
}

static void checkMinExpI(MinExp *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinExp");
    switch (node->type) {
    case MINEXP_TYPE_AMB:
        checkMinAmb(getMinExp_Amb(node), context);
        break;
    case MINEXP_TYPE_APPLY:
        checkMinApply(getMinExp_Apply(node), context);
        break;
    case MINEXP_TYPE_ARGS:
        checkMinExprList(getMinExp_Args(node), context);
        break;
    case MINEXP_TYPE_AVAR:
        checkMinAnnotatedVar(getMinExp_Avar(node), context);
        break;
    case MINEXP_TYPE_BINDINGS:
        checkMinBindings(getMinExp_Bindings(node), context);
        break;
    case MINEXP_TYPE_CALLCC:
        checkMinExpI(getMinExp_CallCC(node), context);
        break;
    case MINEXP_TYPE_COND:
        checkMinCond(getMinExp_Cond(node), context);
        break;
    case MINEXP_TYPE_IFF:
        checkMinIff(getMinExp_Iff(node), context);
        break;
    case MINEXP_TYPE_LAM:
        checkMinLam(getMinExp_Lam(node), context);
        break;
    case MINEXP_TYPE_LETREC:
        checkMinLetRec(getMinExp_LetRec(node), context);
        break;
    case MINEXP_TYPE_MAKEVEC:
        checkMinExprList(getMinExp_MakeVec(node), context);
        break;
    case MINEXP_TYPE_MATCH:
        checkMinMatch(getMinExp_Match(node), context);
        break;
    case MINEXP_TYPE_PRIM:
        checkMinPrimApp(getMinExp_Prim(node), context);
        break;
    case MINEXP_TYPE_SEQUENCE:
        checkMinExprList(getMinExp_Sequence(node), context);
        break;
    case MINEXP_TYPE_VAR:
        assertBound(getMinExp_Var(node), context);
        break;
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_DONE:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
        break;
    default:
        cant_happen("unrecognized MinExp type %s", minExpTypeName(node->type));
    }
}

static void checkMinCondCases(MinCondCases *node, SymbolSet *context) {
    if (node == NULL)
        cant_happen("NULL MinCondCases");
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        checkMinIntCondCases(getMinCondCases_Integers(node), context);
        break;
    case MINCONDCASES_TYPE_CHARACTERS:
        checkMinCharCondCases(getMinCondCases_Characters(node), context);
        break;
    default:
        cant_happen("unrecognized MinCondCases type %s",
                    minCondCasesTypeName(node->type));
    }
}