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

#include "minlam_annotate.h"

#ifdef DEBUG_MINLAM_ANNOTATE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinLam *annotateMinLam(MinLam *node, IntMap *context);
static MinExprList *annotateMinExprList(MinExprList *node, IntMap *context);
static MinPrimApp *annotateMinPrimApp(MinPrimApp *node, IntMap *context);
static MinApply *annotateMinApply(MinApply *node, IntMap *context);
static MinIff *annotateMinIff(MinIff *node, IntMap *context);
static MinCond *annotateMinCond(MinCond *node, IntMap *context);
static MinIntCondCases *annotateMinIntCondCases(MinIntCondCases *node,
                                                IntMap *context);
static MinCharCondCases *annotateMinCharCondCases(MinCharCondCases *node,
                                                  IntMap *context);
static MinMatch *annotateMinMatch(MinMatch *node, IntMap *context);
static MinMatchList *annotateMinMatchList(MinMatchList *node, IntMap *context);
static MinLetRec *annotateMinLetRec(MinLetRec *node, IntMap *context);
static MinBindings *annotateMinBindings(MinBindings *node, IntMap *context);
static MinAmb *annotateMinAmb(MinAmb *node, IntMap *context);
static MinExp *annotateMinExp(MinExp *node, IntMap *context);
static MinCondCases *annotateMinCondCases(MinCondCases *node, IntMap *context);

///////////
// Helpers
///////////

static IntMap *newIntMapFromSymbolList(SymbolList *list) {
    IntMap *map = newIntMap();
    int save = PROTECT(map);
    int i = 0;
    while (list != NULL) {
        setIntMap(map, list->symbol, i);
        i++;
        list = list->next;
    }
    UNPROTECT(save);
    return map;
}

// Copy existing context and add bindings starting at next available position.
// After closure conversion, letrec bodies can reference enclosing lambda
// params, so we must extend (not replace) the context.
static IntMap *extendIntMapWithBindings(IntMap *context,
                                        MinBindings *bindings) {
    IntMap *map = newIntMap();
    int save = PROTECT(map);
    // copy existing context
    HashSymbol *key = NULL;
    Index idx = 0;
    Integer val = 0;
    while ((key = iterateIntMap(context, &idx, &val)) != NULL) {
        setIntMap(map, key, val);
    }
    // add bindings starting at next position
    int i = (int)countIntMap(context);
    while (bindings != NULL) {
        setIntMap(map, bindings->var, i);
        i++;
        bindings = bindings->next;
    }
    UNPROTECT(save);
    return map;
}

///////////////////////////
// Visitor implementations
///////////////////////////

static MinLam *annotateMinLam(MinLam *node,
                              IntMap *context __attribute__((unused))) {
    ENTER(annotateMinLam);
    if (node == NULL) {
        LEAVE(annotateMinLam);
        return NULL;
    }

    IntMap *newContext = newIntMapFromSymbolList(node->args);
    int save = PROTECT(newContext);
    bool changed = false;
    MinExp *new_exp = annotateMinExp(node->exp, newContext);
    PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);

    MinLam *result = node;
    if (changed) {
        result = newMinLam(CPI(node), node->args, new_exp);
        result->cc = node->cc;
    }

    UNPROTECT(save);
    LEAVE(annotateMinLam);
    return result;
}

static MinExprList *annotateMinExprList(MinExprList *node, IntMap *context) {
    ENTER(annotateMinExprList);
    if (node == NULL) {
        LEAVE(annotateMinExprList);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp = annotateMinExp(node->exp, context);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = annotateMinExprList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }

    UNPROTECT(save);
    LEAVE(annotateMinExprList);
    return result;
}

static MinPrimApp *annotateMinPrimApp(MinPrimApp *node, IntMap *context) {
    ENTER(annotateMinPrimApp);
    if (node == NULL) {
        LEAVE(annotateMinPrimApp);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp1 = annotateMinExp(node->exp1, context);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = annotateMinExp(node->exp2, context);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);

    MinPrimApp *result = node;
    if (changed) {
        result = newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
    }

    UNPROTECT(save);
    LEAVE(annotateMinPrimApp);
    return result;
}

static MinApply *annotateMinApply(MinApply *node, IntMap *context) {
    ENTER(annotateMinApply);
    if (node == NULL) {
        LEAVE(annotateMinApply);
        return NULL;
    }
    bool changed = false;
    MinExp *new_function = node->function;
    if (!node->isBuiltin) {
        new_function = annotateMinExp(node->function, context);
    }
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = annotateMinExprList(node->args, context);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);

    MinApply *result = node;
    if (changed) {
        result = newMinApply(CPI(node), new_function, new_args);
        result->isBuiltin = node->isBuiltin;
        result->cc = node->cc;
    }

    UNPROTECT(save);
    LEAVE(annotateMinApply);
    return result;
}

static MinIff *annotateMinIff(MinIff *node, IntMap *context) {
    ENTER(annotateMinIff);
    if (node == NULL) {
        LEAVE(annotateMinIff);
        return NULL;
    }

    bool changed = false;
    MinExp *new_condition = annotateMinExp(node->condition, context);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = annotateMinExp(node->consequent, context);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = annotateMinExp(node->alternative, context);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);

    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }

    UNPROTECT(save);
    LEAVE(annotateMinIff);
    return result;
}

static MinCond *annotateMinCond(MinCond *node, IntMap *context) {
    ENTER(annotateMinCond);
    if (node == NULL) {
        LEAVE(annotateMinCond);
        return NULL;
    }

    bool changed = false;
    MinExp *new_value = annotateMinExp(node->value, context);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = annotateMinCondCases(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }

    UNPROTECT(save);
    LEAVE(annotateMinCond);
    return result;
}

static MinIntCondCases *annotateMinIntCondCases(MinIntCondCases *node,
                                                IntMap *context) {
    ENTER(annotateMinIntCondCases);
    if (node == NULL) {
        LEAVE(annotateMinIntCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = annotateMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = annotateMinIntCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }

    UNPROTECT(save);
    LEAVE(annotateMinIntCondCases);
    return result;
}

static MinCharCondCases *annotateMinCharCondCases(MinCharCondCases *node,
                                                  IntMap *context) {
    ENTER(annotateMinCharCondCases);
    if (node == NULL) {
        LEAVE(annotateMinCharCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = annotateMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = annotateMinCharCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
    }

    UNPROTECT(save);
    LEAVE(annotateMinCharCondCases);
    return result;
}

static MinMatch *annotateMinMatch(MinMatch *node, IntMap *context) {
    ENTER(annotateMinMatch);
    if (node == NULL) {
        LEAVE(annotateMinMatch);
        return NULL;
    }

    bool changed = false;
    MinExp *new_index = annotateMinExp(node->index, context);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = annotateMinMatchList(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }

    UNPROTECT(save);
    LEAVE(annotateMinMatch);
    return result;
}

static MinMatchList *annotateMinMatchList(MinMatchList *node, IntMap *context) {
    ENTER(annotateMinMatchList);
    if (node == NULL) {
        LEAVE(annotateMinMatchList);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = annotateMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = annotateMinMatchList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), node->matches, new_body, new_next);
    }

    UNPROTECT(save);
    LEAVE(annotateMinMatchList);
    return result;
}

static MinLetRec *annotateMinLetRec(MinLetRec *node, IntMap *context) {
    ENTER(annotateMinLetRec);
    if (node == NULL) {
        LEAVE(annotateMinLetRec);
        return NULL;
    }

    IntMap *newContext = extendIntMapWithBindings(context, node->bindings);
    int save = PROTECT(newContext);
    bool changed = false;
    MinBindings *new_bindings = annotateMinBindings(node->bindings, newContext);
    PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = annotateMinExp(node->body, newContext);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);

    MinLetRec *result = node;
    if (changed) {
        result = newMinLetRec(CPI(node), new_bindings, new_body);
    }

    UNPROTECT(save);
    LEAVE(annotateMinLetRec);
    return result;
}

static MinBindings *annotateMinBindings(MinBindings *node, IntMap *context) {
    ENTER(annotateMinBindings);
    if (node == NULL) {
        LEAVE(annotateMinBindings);
        return NULL;
    }

    bool changed = false;
    MinExp *new_val = annotateMinExp(node->val, context);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = annotateMinBindings(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
    }

    UNPROTECT(save);
    LEAVE(annotateMinBindings);
    return result;
}

static MinAmb *annotateMinAmb(MinAmb *node, IntMap *context) {
    ENTER(annotateMinAmb);
    if (node == NULL) {
        LEAVE(annotateMinAmb);
        return NULL;
    }

    bool changed = false;
    MinExp *new_left = annotateMinExp(node->left, context);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = annotateMinExp(node->right, context);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);

    MinAmb *result = node;
    if (changed) {
        result = newMinAmb(CPI(node), new_left, new_right);
    }

    UNPROTECT(save);
    LEAVE(annotateMinAmb);
    return result;
}

static MinExp *annotateMinExp(MinExp *node, IntMap *context) {
    ENTER(annotateMinExp);
    if (node == NULL) {
        LEAVE(annotateMinExp);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinExp *result = node;

    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = annotateMinAmb(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = annotateMinApply(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = annotateMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Args(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
        MinAnnotatedVar *variant = getMinExp_Avar(node);
        cant_happen("annotate encountered annotated var %s<%d>",
                    variant->var->name, variant->position);
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
        MinBindings *new_variant = annotateMinBindings(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = annotateMinExp(variant, context);
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
        MinCond *new_variant = annotateMinCond(variant, context);
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
        MinIff *new_variant = annotateMinIff(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = annotateMinLam(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = annotateMinLetRec(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = annotateMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = annotateMinMatch(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = annotateMinPrimApp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = annotateMinExprList(variant, context);
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
        HashSymbol *var = getMinExp_Var(node);
        Integer i = 0;
        if (getIntMap(context, var, &i)) {
            result = makeMinExp_Avar(CPI(node), var, i);
        } else {
            cant_happen("unbound variable %s", var->name);
        }
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }

    UNPROTECT(save);
    LEAVE(annotateMinExp);
    return result;
}

static MinCondCases *annotateMinCondCases(MinCondCases *node, IntMap *context) {
    ENTER(annotateMinCondCases);
    if (node == NULL) {
        LEAVE(annotateMinCondCases);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinCondCases *result = node;

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant =
            annotateMinIntCondCases(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant =
            annotateMinCharCondCases(variant, context);
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
    LEAVE(annotateMinCondCases);
    return result;
}

////////////////////
// Public Interface
////////////////////

MinExp *indexMinExp(MinExp *exp) {
    IntMap *context = newIntMap();
    int save = PROTECT(context);
    exp = annotateMinExp(exp, context);
    UNPROTECT(save);
    return exp;
}