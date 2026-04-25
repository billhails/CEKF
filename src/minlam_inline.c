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

#include "minlam_inline.h"
#include "memory.h"
#include "minlam_helper.h"
#include "minlam_inSafe.h"
#include "minlam_isSimple.h"
#include "minlam_pp.h"
#include "minlam_size.h"
#include "minlam_substCs.h"

#ifdef DEBUG_MINLAM_INLINE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

#define INLINE_SIZE_LIMIT 40

static MinLam *inlineMinLam(MinLam *);
static MinAnnotatedVar *inlineMinAnnotatedVar(MinAnnotatedVar *);
static MinExprList *inlineMinExprList(MinExprList *);
static MinPrimApp *inlineMinPrimApp(MinPrimApp *);
static MinApply *inlineMinApply(MinApply *);
static MinIff *inlineMinIff(MinIff *);
static MinCond *inlineMinCond(MinCond *);
static MinIntCondCases *inlineMinIntCondCases(MinIntCondCases *);
static MinCharCondCases *inlineMinCharCondCases(MinCharCondCases *);
static MinMatch *inlineMinMatch(MinMatch *);
static MinMatchList *inlineMinMatchList(MinMatchList *);
static MinLetRec *inlineMinLetRec(MinLetRec *);
static MinBindings *inlineMinBindings(MinBindings *);
static MinAmb *inlineMinAmb(MinAmb *);
static MinCondCases *inlineMinCondCases(MinCondCases *);

///////////////////////////
// Visitor implementations
///////////////////////////

static MinLam *inlineMinLam(MinLam *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinLam);
    bool changed = false;
    MinExp *new_exp = inlineMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinLam *result = node;
    if (changed) {
        result = newMinLam(CPI(node), node->args, new_exp);
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(inlineMinLam);
    return result;
}

static MinAnnotatedVar *inlineMinAnnotatedVar(MinAnnotatedVar *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinAnnotatedVar);
    MinAnnotatedVar *result = node;
    LEAVE(inlineMinAnnotatedVar);
    return result;
}

static MinExprList *inlineMinExprList(MinExprList *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinExprList);
    bool changed = false;
    MinExp *new_exp = inlineMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = inlineMinExprList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }
    UNPROTECT(save);
    LEAVE(inlineMinExprList);
    return result;
}

static MinPrimApp *inlineMinPrimApp(MinPrimApp *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinPrimApp);
    bool changed = false;
    MinExp *new_exp1 = inlineMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = inlineMinExp(node->exp2);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);
    MinPrimApp *result = node;
    if (changed) {
        result = newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
    }
    UNPROTECT(save);
    LEAVE(inlineMinPrimApp);
    return result;
}

static MinApply *inlineMinApply(MinApply *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinApply);
    bool changed = false;
    MinExp *new_function = inlineMinExp(node->function);
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = inlineMinExprList(node->args);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);
    MinApply *result = node;
    if (changed) {
        result = newMinApply(CPI(node), new_function, new_args);
        result->isBuiltin = node->isBuiltin;
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(inlineMinApply);
    return result;
}

static MinIff *inlineMinIff(MinIff *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinIff);
    bool changed = false;
    MinExp *new_condition = inlineMinExp(node->condition);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = inlineMinExp(node->consequent);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = inlineMinExp(node->alternative);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);
    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }
    UNPROTECT(save);
    LEAVE(inlineMinIff);
    return result;
}

static MinCond *inlineMinCond(MinCond *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinCond);
    bool changed = false;
    MinExp *new_value = inlineMinExp(node->value);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = inlineMinCondCases(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }
    UNPROTECT(save);
    LEAVE(inlineMinCond);
    return result;
}

static MinIntCondCases *inlineMinIntCondCases(MinIntCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinIntCondCases);
    bool changed = false;
    MinExp *new_body = inlineMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = inlineMinIntCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(inlineMinIntCondCases);
    return result;
}

static MinCharCondCases *inlineMinCharCondCases(MinCharCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinCharCondCases);
    bool changed = false;
    MinExp *new_body = inlineMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = inlineMinCharCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        result->isDefault = node->isDefault;
    }
    UNPROTECT(save);
    LEAVE(inlineMinCharCondCases);
    return result;
}

static MinMatch *inlineMinMatch(MinMatch *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinMatch);
    bool changed = false;
    MinExp *new_index = inlineMinExp(node->index);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = inlineMinMatchList(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }
    UNPROTECT(save);
    LEAVE(inlineMinMatch);
    return result;
}

static MinMatchList *inlineMinMatchList(MinMatchList *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinMatchList);
    bool changed = false;
    MinExp *new_body = inlineMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = inlineMinMatchList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), node->matches, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(inlineMinMatchList);
    return result;
}

static MinLetRec *inlineMinLetRec(MinLetRec *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinLetRec);
    bool changed = false;
    MinBindings *new_bindings = inlineMinBindings(node->bindings);
    int save = PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = inlineMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);

    // all names bound by this letrec
    SymbolSet *keys = getAllKeys(node->bindings);
    PROTECT(keys);
    // all their direct references to other names bound in this letrec
    SymbolSetMap *deps = buildDependencyGraph(node->bindings, keys);
    PROTECT(deps);
    // faster lookup
    MinExpTable *map = minBindingsToMap(node->bindings);
    PROTECT(map);
    // things we'll be replacing
    MinExpTable *replacements = newMinExpTable();
    PROTECT(replacements);

    HashSymbol *f = NULL;
    Index i = 0;
    while ((f = iterateSymbolSet(keys, &i)) != NULL) {
        SymbolSet *fDeps = NULL;
        bool isRecursive = false;
        if (getSymbolSetMap(deps, f, &fDeps)) {
            SymbolSet *transitive = computeLiveBindings(deps, fDeps);
            int save2 = PROTECT(transitive);
            isRecursive = getSymbolSet(transitive, f);
            UNPROTECT(save2);
        }
        DEBUG("%s is%s recursive\n", f->name, isRecursive ? "" : " not");
        if (isRecursive)
            continue;

        MinExp *body = NULL;
        if (!getMinExpTable(map, f, &body)) {
            cant_happen("cannot find %s in map", f->name);
        }

        if (!isMinExp_Lam(body)) {
            eprintf("non-lamda %s :", f->name);
            ppMinExp(stderr, body);
            cant_happen("found non-lambda in letrec");
        }

        int size = sizeMinExp(body);
        DEBUG("%s is size %d\n", f->name, size);

        if (size > INLINE_SIZE_LIMIT)
            continue;

        bool isSimple = simpleMinExp(body);
        bool isSafe = inSafeMinExp(body);
        DEBUG("%s is%s simple\n", f->name, isSimple ? "" : " not");
        DEBUG("%s is%s inline-safe\n", f->name, isSafe ? "" : " not");

        if (!isSimple && !isSafe)
            continue;

#ifdef DEBUG_MINLAM_INLINE
        eprintf("INLINING %s: ", f->name);
        ppMinExp(stderr, body);
        eprintf("\n");
#endif
        // add to our list
        body = copyMinExp(body);
        int save2 = PROTECT(body);
        setMinExpTable(replacements, f, body);
        UNPROTECT(save2);
    }

    if (countMinExpTable(replacements) > 0) {
        new_bindings = substCsMinBindings(new_bindings, replacements);
        PROTECT(new_bindings);
        changed = changed || (new_bindings != node->bindings);

        new_body = substCsMinExp(new_body, replacements);
        PROTECT(new_body);
        changed = changed || (new_body != node->body);
    }

    MinLetRec *result = node;
    if (changed) {
        result = newMinLetRec(CPI(node), new_bindings, new_body);
    }
    UNPROTECT(save);
    LEAVE(inlineMinLetRec);
    return result;
}

static MinBindings *inlineMinBindings(MinBindings *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinBindings);
    bool changed = false;
    MinExp *new_val = inlineMinExp(node->val);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = inlineMinBindings(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
    }
    UNPROTECT(save);
    LEAVE(inlineMinBindings);
    return result;
}

static MinAmb *inlineMinAmb(MinAmb *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinAmb);
    bool changed = false;
    MinExp *new_left = inlineMinExp(node->left);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = inlineMinExp(node->right);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);
    MinAmb *result = node;
    if (changed) {
        result = newMinAmb(CPI(node), new_left, new_right);
    }
    UNPROTECT(save);
    LEAVE(inlineMinAmb);
    return result;
}

MinExp *inlineMinExp(MinExp *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinExp);
    int save = STARTPROTECT();
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = inlineMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = inlineMinApply(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = inlineMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Args(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
        MinAnnotatedVar *variant = getMinExp_Avar(node);
        MinAnnotatedVar *new_variant = inlineMinAnnotatedVar(variant);
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
        MinBindings *new_variant = inlineMinBindings(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = inlineMinExp(variant);
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
        MinCond *new_variant = inlineMinCond(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
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
        MinIff *new_variant = inlineMinIff(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = inlineMinLam(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = inlineMinLetRec(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = inlineMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = inlineMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = inlineMinPrimApp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = inlineMinExprList(variant);
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
    LEAVE(inlineMinExp);
    return result;
}

static MinCondCases *inlineMinCondCases(MinCondCases *node) {
    if (node == NULL)
        return NULL;
    ENTER(inlineMinCondCases);
    int save = STARTPROTECT();
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = inlineMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = inlineMinCharCondCases(variant);
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
    LEAVE(inlineMinCondCases);
    return result;
}