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
#include "minlam_pp.h"
#include "utils_helper.h"

typedef struct Context {
    SymbolSet *symbols;
    bool *error;
} Context;

static void checkMinLam(MinLam *, Context *);
static void checkMinAnnotatedVar(MinAnnotatedVar *, Context *);
static void checkMinExprList(MinExprList *, Context *);
static void checkMinPrimApp(MinPrimApp *, Context *);
static void checkMinApply(MinApply *, Context *);
static void checkMinIff(MinIff *, Context *);
static void checkMinCond(MinCond *, Context *);
static void checkMinIntCondCases(MinIntCondCases *, Context *);
static void checkMinCharCondCases(MinCharCondCases *, Context *);
static void checkMinMatch(MinMatch *, Context *);
static void checkMinMatchList(MinMatchList *, Context *);
static void checkMinLetRec(MinLetRec *, Context *);
static void checkMinBindings(MinBindings *, Context *);
static void checkMinAmb(MinAmb *, Context *);
static void checkMinExpI(MinExp *, Context *);
static void checkMinCondCases(MinCondCases *, Context *);

///////
// API
///////

void checkMinExp(MinExp *node, BuiltIns *builtIns) {
    bool errors = false;
    Context context = {.symbols = newSymbolSet(), .error = &errors};
    int save = PROTECT(context.symbols);
    for (Index i = 0; i < builtIns->size; i++) {
        setSymbolSet(context.symbols, builtIns->entries[i]->internalName);
    }
    checkMinExpI(node, &context);
    if (errors) {
        cant_happen("checkMinExp detected errors");
    }
    UNPROTECT(save);
}

///////////////////
// Helper Routines
///////////////////

static Context addSymbolListToContext(Context *context, SymbolList *list) {
    Context new = {.symbols = copySymbolSet(context->symbols),
                   .error = context->error};
    int save = PROTECT(new.symbols);
    while (list != NULL) {
        setSymbolSet(new.symbols, list->symbol);
        list = list->next;
    }
    UNPROTECT(save);
    return new;
}

static Context addBindingsToContext(MinBindings *bindings, Context *context) {
    SymbolList *keys = minBindingsToSymbolList(bindings);
    int save = PROTECT(keys);
    Context new = addSymbolListToContext(context, keys);
    UNPROTECT(save);
    return new;
}

static void assertBound(HashSymbol *symbol, Context *context) {
    if (!getSymbolSet(context->symbols, symbol)) {
        cant_happen("unbound variable %s", symbol->name);
    }
}

///////////////////////////
// Visitor implementations
///////////////////////////

static void checkMinLam(MinLam *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinLam");
    Context newContext = addSymbolListToContext(context, node->args);
    int save = PROTECT(newContext.symbols);
    checkMinExpI(node->exp, &newContext);
    UNPROTECT(save);
}

static void checkMinAnnotatedVar(MinAnnotatedVar *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinAnnotatedVar");
    assertBound(node->var, context);
}

static void checkMinExprList(MinExprList *node, Context *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->exp, context);
    checkMinExprList(node->next, context);
}

static void checkMinPrimApp(MinPrimApp *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinPrimApp");
    checkMinExpI(node->exp1, context);
    checkMinExpI(node->exp2, context);
}

static void checkMinApply(MinApply *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinApply");
    if (isMinExp_Apply(node->function)) {
        eprintf("nested application detected: ");
        ppMinApply(stderr, node);
        eprintf("\n");
        *(context->error) = true;
    }
    checkMinExpI(node->function, context);
    checkMinExprList(node->args, context);
}

static void checkMinIff(MinIff *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinIff");
    checkMinExpI(node->condition, context);
    checkMinExpI(node->consequent, context);
    checkMinExpI(node->alternative, context);
}

static void checkMinCond(MinCond *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinCond");
    checkMinExpI(node->value, context);
    checkMinCondCases(node->cases, context);
}

static void checkMinIntCondCases(MinIntCondCases *node, Context *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->body, context);
    checkMinIntCondCases(node->next, context);
}

static void checkMinCharCondCases(MinCharCondCases *node, Context *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->body, context);
    checkMinCharCondCases(node->next, context);
}

static void checkMinMatch(MinMatch *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinMatch");
    checkMinExpI(node->index, context);
    checkMinMatchList(node->cases, context);
}

static void checkMinMatchList(MinMatchList *node, Context *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->body, context);
    checkMinMatchList(node->next, context);
}

static void checkMinLetRec(MinLetRec *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinLetRec");
    Context newContext = addBindingsToContext(node->bindings, context);
    int save = PROTECT(newContext.symbols);
    checkMinBindings(node->bindings, &newContext);
    checkMinExpI(node->body, &newContext);
    UNPROTECT(save);
}

static void checkMinBindings(MinBindings *node, Context *context) {
    if (node == NULL)
        return;
    checkMinExpI(node->val, context);
    checkMinBindings(node->next, context);
}

static void checkMinAmb(MinAmb *node, Context *context) {
    if (node == NULL)
        cant_happen("NULL MinAmb");
    checkMinExpI(node->left, context);
    checkMinExpI(node->right, context);
}

static void checkMinExpI(MinExp *node, Context *context) {
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
    case MINEXP_TYPE_CUT:
        checkMinExpI(getMinExp_Cut(node), context);
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

static void checkMinCondCases(MinCondCases *node, Context *context) {
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