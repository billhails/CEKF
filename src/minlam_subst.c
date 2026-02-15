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
#include "utils_helper.h"

#include "minlam_subst.h"

#ifdef DEBUG_MINLAM_SUBST
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

// Forward declarations
static MinLam *substMinLam(MinLam *node, MinExpTable *context);
static MinExprList *substMinExprList(MinExprList *node, MinExpTable *context);
static MinPrimApp *substMinPrimApp(MinPrimApp *node, MinExpTable *context);
static MinApply *substMinApply(MinApply *node, MinExpTable *context);
static MinLookUp *substMinLookUp(MinLookUp *node, MinExpTable *context);
static MinIff *substMinIff(MinIff *node, MinExpTable *context);
static MinCond *substMinCond(MinCond *node, MinExpTable *context);
static MinIntCondCases *substMinIntCondCases(MinIntCondCases *node,
                                             MinExpTable *context);
static MinCharCondCases *substMinCharCondCases(MinCharCondCases *node,
                                               MinExpTable *context);
static MinMatch *substMinMatch(MinMatch *node, MinExpTable *context);
static MinMatchList *substMinMatchList(MinMatchList *node,
                                       MinExpTable *context);
static MinIntList *substMinIntList(MinIntList *node, MinExpTable *context);
static MinLetRec *substMinLetRec(MinLetRec *node, MinExpTable *context);
static MinBindings *substMinBindings(MinBindings *node, MinExpTable *context);
static MinAmb *substMinAmb(MinAmb *node, MinExpTable *context);
static MinAlphaEnv *substMinAlphaEnv(MinAlphaEnv *node, MinExpTable *context);
static MinCondCases *substMinCondCases(MinCondCases *node,
                                       MinExpTable *context);
static SymbolMap *substSymbolMap(SymbolMap *node, MinExpTable *context);
static MinNameSpaceArray *substMinNameSpaceArray(MinNameSpaceArray *node,
                                                 MinExpTable *context);
static MinAlphaEnvArray *substMinAlphaEnvArray(MinAlphaEnvArray *node,
                                               MinExpTable *context);

static MinExpTable *excludeBoundVars(MinExpTable *context, SymbolList *vars) {
    MinExpTable *new = newMinExpTable();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    MinExp *exp;
    while ((current = iterateMinExpTable(context, &i, &exp)) != NULL) {
        if (!symbolInList(current, vars)) {
            setMinExpTable(new, current, exp);
        }
    }
    UNPROTECT(save);
    return new;
}

static SymbolList *getBindingVars(MinBindings *bindings) {
    SymbolList *vars = NULL;
    int save = PROTECT(vars);
    while (bindings != NULL) {
        vars = newSymbolList(CPI(bindings), bindings->var, vars);
        PROTECT(vars);
        bindings = bindings->next;
    }
    UNPROTECT(save);
    return vars;
}

// Visitor implementations

static MinLam *substMinLam(MinLam *node, MinExpTable *context) {
    ENTER(substMinLam);
    if (node == NULL) {
        LEAVE(substMinLam);
        return NULL;
    }

    bool changed = false;
    MinExpTable *reducedContext = excludeBoundVars(context, node->args);
    int save = PROTECT(reducedContext);
    MinExp *new_exp = substMinExp(node->exp, reducedContext);
    PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);

    if (changed) {
        // Create new node with modified fields
        MinLam *result = newMinLam(CPI(node), node->args, new_exp);
        UNPROTECT(save);
        LEAVE(substMinLam);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinLam);
    return node;
}

static MinExprList *substMinExprList(MinExprList *node, MinExpTable *context) {
    ENTER(substMinExprList);
    if (node == NULL) {
        LEAVE(substMinExprList);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp = substMinExp(node->exp, context);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = substMinExprList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        // Create new node with modified fields
        MinExprList *result = newMinExprList(CPI(node), new_exp, new_next);
        UNPROTECT(save);
        LEAVE(substMinExprList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinExprList);
    return node;
}

static MinPrimApp *substMinPrimApp(MinPrimApp *node, MinExpTable *context) {
    ENTER(substMinPrimApp);
    if (node == NULL) {
        LEAVE(substMinPrimApp);
        return NULL;
    }

    bool changed = false;
    // Pass through type (type: MinPrimOp, not memory-managed)
    MinExp *new_exp1 = substMinExp(node->exp1, context);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = substMinExp(node->exp2, context);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);

    if (changed) {
        // Create new node with modified fields
        MinPrimApp *result =
            newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
        UNPROTECT(save);
        LEAVE(substMinPrimApp);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinPrimApp);
    return node;
}

static MinApply *substMinApply(MinApply *node, MinExpTable *context) {
    ENTER(substMinApply);
    if (node == NULL) {
        LEAVE(substMinApply);
        return NULL;
    }

    bool changed = false;
    MinExp *new_function = substMinExp(node->function, context);
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = substMinExprList(node->args, context);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);

    if (changed) {
        // Create new node with modified fields
        MinApply *result = newMinApply(CPI(node), new_function, new_args);
        UNPROTECT(save);
        LEAVE(substMinApply);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinApply);
    return node;
}

static MinLookUp *substMinLookUp(MinLookUp *node, MinExpTable *context) {
    ENTER(substMinLookUp);
    if (node == NULL) {
        LEAVE(substMinLookUp);
        return NULL;
    }

    bool changed = false;
    // Pass through nsId (type: int, not memory-managed)
    MinExp *new_exp = substMinExp(node->exp, context);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);

    if (changed) {
        // Create new node with modified fields
        MinLookUp *result = newMinLookUp(CPI(node), node->nsId, new_exp);
        UNPROTECT(save);
        LEAVE(substMinLookUp);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinLookUp);
    return node;
}

static MinIff *substMinIff(MinIff *node, MinExpTable *context) {
    ENTER(substMinIff);
    if (node == NULL) {
        LEAVE(substMinIff);
        return NULL;
    }

    bool changed = false;
    MinExp *new_condition = substMinExp(node->condition, context);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = substMinExp(node->consequent, context);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = substMinExp(node->alternative, context);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);

    if (changed) {
        // Create new node with modified fields
        MinIff *result = newMinIff(CPI(node), new_condition, new_consequent,
                                   new_alternative);
        UNPROTECT(save);
        LEAVE(substMinIff);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinIff);
    return node;
}

static MinCond *substMinCond(MinCond *node, MinExpTable *context) {
    ENTER(substMinCond);
    if (node == NULL) {
        LEAVE(substMinCond);
        return NULL;
    }

    bool changed = false;
    MinExp *new_value = substMinExp(node->value, context);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = substMinCondCases(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    if (changed) {
        // Create new node with modified fields
        MinCond *result = newMinCond(CPI(node), new_value, new_cases);
        UNPROTECT(save);
        LEAVE(substMinCond);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinCond);
    return node;
}

static MinIntCondCases *substMinIntCondCases(MinIntCondCases *node,
                                             MinExpTable *context) {
    ENTER(substMinIntCondCases);
    if (node == NULL) {
        LEAVE(substMinIntCondCases);
        return NULL;
    }

    bool changed = false;
    // Pass through constant (type: MaybeBigInt, not memory-managed)
    MinExp *new_body = substMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = substMinIntCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        // Create new node with modified fields
        MinIntCondCases *result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        LEAVE(substMinIntCondCases);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinIntCondCases);
    return node;
}

static MinCharCondCases *substMinCharCondCases(MinCharCondCases *node,
                                               MinExpTable *context) {
    ENTER(substMinCharCondCases);
    if (node == NULL) {
        LEAVE(substMinCharCondCases);
        return NULL;
    }

    bool changed = false;
    // Pass through constant (type: character, not memory-managed)
    MinExp *new_body = substMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = substMinCharCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        // Create new node with modified fields
        MinCharCondCases *result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        LEAVE(substMinCharCondCases);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinCharCondCases);
    return node;
}

static MinMatch *substMinMatch(MinMatch *node, MinExpTable *context) {
    ENTER(substMinMatch);
    if (node == NULL) {
        LEAVE(substMinMatch);
        return NULL;
    }

    bool changed = false;
    MinExp *new_index = substMinExp(node->index, context);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = substMinMatchList(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    if (changed) {
        // Create new node with modified fields
        MinMatch *result = newMinMatch(CPI(node), new_index, new_cases);
        UNPROTECT(save);
        LEAVE(substMinMatch);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinMatch);
    return node;
}

static MinMatchList *substMinMatchList(MinMatchList *node,
                                       MinExpTable *context) {
    ENTER(substMinMatchList);
    if (node == NULL) {
        LEAVE(substMinMatchList);
        return NULL;
    }

    bool changed = false;
    MinIntList *new_matches = substMinIntList(node->matches, context);
    int save = PROTECT(new_matches);
    changed = changed || (new_matches != node->matches);
    MinExp *new_body = substMinExp(node->body, context);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = substMinMatchList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        // Create new node with modified fields
        MinMatchList *result =
            newMinMatchList(CPI(node), new_matches, new_body, new_next);
        UNPROTECT(save);
        LEAVE(substMinMatchList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinMatchList);
    return node;
}

static MinIntList *substMinIntList(MinIntList *node, MinExpTable *context) {
    ENTER(substMinIntList);
    if (node == NULL) {
        LEAVE(substMinIntList);
        return NULL;
    }

    bool changed = false;
    // Pass through item (type: int, not memory-managed)
    MinIntList *new_next = substMinIntList(node->next, context);
    int save = PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        // Create new node with modified fields
        MinIntList *result = newMinIntList(CPI(node), node->item, new_next);
        UNPROTECT(save);
        LEAVE(substMinIntList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinIntList);
    return node;
}

static MinLetRec *substMinLetRec(MinLetRec *node, MinExpTable *context) {
    ENTER(substMinLetRec);
    if (node == NULL) {
        LEAVE(substMinLetRec);
        return NULL;
    }

    SymbolList *vars = getBindingVars(node->bindings);
    int save = PROTECT(vars);
    MinExpTable *reducedContext = excludeBoundVars(context, vars);
    PROTECT(reducedContext);
    bool changed = false;
    MinBindings *new_bindings =
        substMinBindings(node->bindings, reducedContext);
    PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = substMinExp(node->body, reducedContext);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);

    if (changed) {
        // Create new node with modified fields
        MinLetRec *result = newMinLetRec(CPI(node), new_bindings, new_body);
        UNPROTECT(save);
        LEAVE(substMinLetRec);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinLetRec);
    return node;
}

static MinBindings *substMinBindings(MinBindings *node, MinExpTable *context) {
    ENTER(substMinBindings);
    if (node == NULL) {
        LEAVE(substMinBindings);
        return NULL;
    }

    bool changed = false;
    // Pass through var (type: HashSymbol, not memory-managed)
    MinExp *new_val = substMinExp(node->val, context);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = substMinBindings(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        // Create new node with modified fields
        MinBindings *result =
            newMinBindings(CPI(node), node->var, new_val, new_next);
        UNPROTECT(save);
        LEAVE(substMinBindings);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinBindings);
    return node;
}

static MinAmb *substMinAmb(MinAmb *node, MinExpTable *context) {
    ENTER(substMinAmb);
    if (node == NULL) {
        LEAVE(substMinAmb);
        return NULL;
    }

    bool changed = false;
    MinExp *new_left = substMinExp(node->left, context);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = substMinExp(node->right, context);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);

    if (changed) {
        // Create new node with modified fields
        MinAmb *result = newMinAmb(CPI(node), new_left, new_right);
        UNPROTECT(save);
        LEAVE(substMinAmb);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinAmb);
    return node;
}

static MinAlphaEnv *substMinAlphaEnv(MinAlphaEnv *node, MinExpTable *context) {
    ENTER(substMinAlphaEnv);
    if (node == NULL) {
        LEAVE(substMinAlphaEnv);
        return NULL;
    }

    bool changed = false;
    SymbolMap *new_alphaTable = substSymbolMap(node->alphaTable, context);
    int save = PROTECT(new_alphaTable);
    changed = changed || (new_alphaTable != node->alphaTable);
    MinAlphaEnv *new_next = substMinAlphaEnv(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinAlphaEnvArray *new_nameSpaces =
        substMinAlphaEnvArray(node->nameSpaces, context);
    PROTECT(new_nameSpaces);
    changed = changed || (new_nameSpaces != node->nameSpaces);

    if (changed) {
        // Create new node with modified fields
        MinAlphaEnv *result = newMinAlphaEnv(new_next);
        result->alphaTable = new_alphaTable;
        result->nameSpaces = new_nameSpaces;
        UNPROTECT(save);
        LEAVE(substMinAlphaEnv);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinAlphaEnv);
    return node;
}

MinExp *substMinExp(MinExp *node, MinExpTable *context) {
    ENTER(substMinExp);
    if (node == NULL) {
        LEAVE(substMinExp);
        return NULL;
    }

    if (countMinExpTable(context) == 0) {
        LEAVE(substMinExp);
        return node;
    }

    int save = PROTECT(NULL);
    MinExp *result = node;

    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = substMinAmb(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = substMinApply(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
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
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = substMinExp(variant, context);
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
        MinCond *new_variant = substMinCond(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ENV: {
        // void_ptr
        break;
    }
    case MINEXP_TYPE_ERROR: {
        // void_ptr
        break;
    }
    case MINEXP_TYPE_IFF: {
        // MinIff
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = substMinIff(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = substMinLam(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = substMinLetRec(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LOOKUP: {
        // MinLookUp
        MinLookUp *variant = getMinExp_LookUp(node);
        MinLookUp *new_variant = substMinLookUp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LookUp(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = substMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = substMinMatch(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_NAMESPACES: {
        // MinNameSpaceArray
        MinNameSpaceArray *variant = getMinExp_NameSpaces(node);
        MinNameSpaceArray *new_variant =
            substMinNameSpaceArray(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_NameSpaces(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = substMinPrimApp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = substMinExprList(variant, context);
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
        MinExp *res = NULL;
        if (getMinExpTable(context, getMinExp_Var(node), &res)) {
            result = res;
        }
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }

    UNPROTECT(save);
    LEAVE(substMinExp);
    return result;
}

static MinCondCases *substMinCondCases(MinCondCases *node,
                                       MinExpTable *context) {
    ENTER(substMinCondCases);
    if (node == NULL) {
        LEAVE(substMinCondCases);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinCondCases *result = node;

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = substMinIntCondCases(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = substMinCharCondCases(variant, context);
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
    LEAVE(substMinCondCases);
    return result;
}

static SymbolMap *substSymbolMap(SymbolMap *node, MinExpTable *context) {
    ENTER(substSymbolMap);
    if (node == NULL) {
        LEAVE(substSymbolMap);
        return NULL;
    }

    (void)context; // Values are HashSymbol (not memory-managed)
#ifdef NOTDEF
    // Iterate over all entries for inspection/logging
    Index i = 0;
    struct HashSymbol *value;
    HashSymbol *key;
    while ((key = iterateSymbolMap(node, &i, &value)) != NULL) {
        // Inspect/log key and value here
    }
#endif
    LEAVE(substSymbolMap);
    return node;
}

static MinNameSpaceArray *substMinNameSpaceArray(MinNameSpaceArray *node,
                                                 MinExpTable *context) {
    ENTER(substMinNameSpaceArray);
    if (node == NULL) {
        LEAVE(substMinNameSpaceArray);
        return NULL;
    }

    bool changed = false;
    MinNameSpaceArray *result = newMinNameSpaceArray();
    int save = PROTECT(result);

    // Iterate over all elements
    for (Index i = 0; i < node->size; i++) {
        struct MinExp *element = peeknMinNameSpaceArray(node, i);
        struct MinExp *new_element = substMinExp(element, context);
        PROTECT(new_element);
        changed = changed || (new_element != element);
        pushMinNameSpaceArray(result, new_element);
    }

    if (changed) {
        UNPROTECT(save);
        LEAVE(substMinNameSpaceArray);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinNameSpaceArray);
    return node;
}

static MinAlphaEnvArray *substMinAlphaEnvArray(MinAlphaEnvArray *node,
                                               MinExpTable *context) {
    ENTER(substMinAlphaEnvArray);
    if (node == NULL) {
        LEAVE(substMinAlphaEnvArray);
        return NULL;
    }

    bool changed = false;
    MinAlphaEnvArray *result = newMinAlphaEnvArray();
    int save = PROTECT(result);

    // Iterate over all elements
    for (Index i = 0; i < node->size; i++) {
        struct MinAlphaEnv *element = peeknMinAlphaEnvArray(node, i);
        struct MinAlphaEnv *new_element = substMinAlphaEnv(element, context);
        PROTECT(new_element);
        changed = changed || (new_element != element);
        pushMinAlphaEnvArray(result, new_element);
    }

    if (changed) {
        UNPROTECT(save);
        LEAVE(substMinAlphaEnvArray);
        return result;
    }

    UNPROTECT(save);
    LEAVE(substMinAlphaEnvArray);
    return node;
}
