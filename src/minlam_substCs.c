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

#include "minlam_substCs.h"

#ifdef DEBUG_MINLAM_SUBSTCS
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinLam *substCsMinLam(MinLam *, MinExpTable *);
static MinAnnotatedVar *substCsMinAnnotatedVar(MinAnnotatedVar *,
                                               MinExpTable *);
static MinExprList *substCsMinExprList(MinExprList *, MinExpTable *);
static MinPrimApp *substCsMinPrimApp(MinPrimApp *, MinExpTable *);
static MinApply *substCsMinApply(MinApply *, MinExpTable *);
static MinIff *substCsMinIff(MinIff *, MinExpTable *);
static MinCond *substCsMinCond(MinCond *, MinExpTable *);
static MinIntCondCases *substCsMinIntCondCases(MinIntCondCases *,
                                               MinExpTable *);
static MinCharCondCases *substCsMinCharCondCases(MinCharCondCases *,
                                                 MinExpTable *);
static MinMatch *substCsMinMatch(MinMatch *, MinExpTable *);
static MinMatchList *substCsMinMatchList(MinMatchList *, MinExpTable *);
static MinIntList *substCsMinIntList(MinIntList *, MinExpTable *);
static MinLetRec *substCsMinLetRec(MinLetRec *, MinExpTable *);
static MinAmb *substCsMinAmb(MinAmb *, MinExpTable *);
static MinCondCases *substCsMinCondCases(MinCondCases *, MinExpTable *);
static MinExp *substCs(MinExp *, MinExpTable *);

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

///////////////////////////
// Visitor implementations
///////////////////////////

static MinLam *substCsMinLam(MinLam *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinLam);
    MinExpTable *reducedContext = excludeBoundVars(context, node->args);
    int save = PROTECT(reducedContext);
    bool changed = false;
    MinExp *new_exp = substCsMinExp(node->exp, reducedContext);
    PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinLam *result = node;
    if (changed) {
        result = newMinLam(CPI(node), node->args, new_exp);
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(substCsMinLam);
    return result;
}

static MinAnnotatedVar *substCsMinAnnotatedVar(MinAnnotatedVar *node,
                                               MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinAnnotatedVar);
    MinAnnotatedVar *result = node;
    (void)context; // Unused parameter - all fields are pass-through
    LEAVE(substCsMinAnnotatedVar);
    return result;
}

static MinExprList *substCsMinExprList(MinExprList *node,
                                       MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinExprList);
    bool changed = false;
    MinExp *new_exp = substCsMinExp(node->exp, context);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = substCsMinExprList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }
    UNPROTECT(save);
    LEAVE(substCsMinExprList);
    return result;
}

static MinPrimApp *substCsMinPrimApp(MinPrimApp *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinPrimApp);
    bool changed = false;
    MinExp *new_exp1 = substCsMinExp(node->exp1, context);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = substCsMinExp(node->exp2, context);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);
    MinPrimApp *result = node;
    if (changed) {
        result = newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
    }
    UNPROTECT(save);
    LEAVE(substCsMinPrimApp);
    return result;
}

static MinApply *substCsMinApply(MinApply *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinApply);
    bool changed = false;
    MinExp *new_function = substCs(node->function, context);
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = substCsMinExprList(node->args, context);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);
    MinApply *result = node;
    if (changed) {
        result = newMinApply(CPI(node), new_function, new_args);
        result->isBuiltin = node->isBuiltin;
        result->cc = node->cc;
    }
    UNPROTECT(save);
    LEAVE(substCsMinApply);
    return result;
}

static MinIff *substCsMinIff(MinIff *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinIff);
    bool changed = false;
    MinExp *new_condition = substCsMinExp(node->condition, context);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = substCsMinExp(node->consequent, context);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = substCsMinExp(node->alternative, context);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);
    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }
    UNPROTECT(save);
    LEAVE(substCsMinIff);
    return result;
}

static MinCond *substCsMinCond(MinCond *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinCond);
    bool changed = false;
    MinExp *new_value = substCsMinExp(node->value, context);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = substCsMinCondCases(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }
    UNPROTECT(save);
    LEAVE(substCsMinCond);
    return result;
}

static MinIntCondCases *substCsMinIntCondCases(MinIntCondCases *node,
                                               MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinIntCondCases);
    bool changed = false;
    MinExp *new_body = substCsMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = substCsMinIntCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(substCsMinIntCondCases);
    return result;
}

static MinCharCondCases *substCsMinCharCondCases(MinCharCondCases *node,
                                                 MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinCharCondCases);
    bool changed = false;
    MinExp *new_body = substCsMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = substCsMinCharCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        result->isDefault = node->isDefault;
    }
    UNPROTECT(save);
    LEAVE(substCsMinCharCondCases);
    return result;
}

static MinMatch *substCsMinMatch(MinMatch *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinMatch);
    bool changed = false;
    MinExp *new_index = substCsMinExp(node->index, context);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = substCsMinMatchList(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }
    UNPROTECT(save);
    LEAVE(substCsMinMatch);
    return result;
}

static MinMatchList *substCsMinMatchList(MinMatchList *node,
                                         MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinMatchList);
    bool changed = false;
    MinIntList *new_matches = substCsMinIntList(node->matches, context);
    int save = PROTECT(new_matches);
    changed = changed || (new_matches != node->matches);
    MinExp *new_body = substCsMinExp(node->body, context);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = substCsMinMatchList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), new_matches, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(substCsMinMatchList);
    return result;
}

static MinIntList *substCsMinIntList(MinIntList *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinIntList);
    bool changed = false;
    MinIntList *new_next = substCsMinIntList(node->next, context);
    int save = PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntList *result = node;
    if (changed) {
        result = newMinIntList(CPI(node), node->item, new_next);
    }
    UNPROTECT(save);
    LEAVE(substCsMinIntList);
    return result;
}

static MinLetRec *substCsMinLetRec(MinLetRec *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinLetRec);
    SymbolList *vars = getBindingVars(node->bindings);
    int save = PROTECT(vars);
    MinExpTable *reducedContext = excludeBoundVars(context, vars);
    PROTECT(reducedContext);
    bool changed = false;
    MinBindings *new_bindings =
        substCsMinBindings(node->bindings, reducedContext);
    PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = substCsMinExp(node->body, reducedContext);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinLetRec *result = node;
    if (changed) {
        result = newMinLetRec(CPI(node), new_bindings, new_body);
    }
    UNPROTECT(save);
    LEAVE(substCsMinLetRec);
    return result;
}

MinBindings *substCsMinBindings(MinBindings *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinBindings);
    bool changed = false;
    MinExp *new_val = substCsMinExp(node->val, context);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = substCsMinBindings(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = node->arity;
    }
    UNPROTECT(save);
    LEAVE(substCsMinBindings);
    return result;
}

static MinAmb *substCsMinAmb(MinAmb *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinAmb);
    bool changed = false;
    MinExp *new_left = substCsMinExp(node->left, context);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = substCsMinExp(node->right, context);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);
    MinAmb *result = node;
    if (changed) {
        result = newMinAmb(CPI(node), new_left, new_right);
    }
    UNPROTECT(save);
    LEAVE(substCsMinAmb);
    return result;
}

MinExp *substCsMinExp(MinExp *node, MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinExp);
    int save = STARTPROTECT();
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = substCsMinAmb(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = substCsMinApply(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = substCsMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Args(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_AVAR: {
        MinAnnotatedVar *variant = getMinExp_Avar(node);
        MinAnnotatedVar *new_variant = substCsMinAnnotatedVar(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Avar(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_BINDINGS: {
        MinBindings *variant = getMinExp_Bindings(node);
        MinBindings *new_variant = substCsMinBindings(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = substCsMinExp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_CallCC(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CUT: {
        MinExp *variant = getMinExp_Cut(node);
        MinExp *new_variant = substCsMinExp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cut(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_COND: {
        MinCond *variant = getMinExp_Cond(node);
        MinCond *new_variant = substCsMinCond(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = substCsMinIff(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = substCsMinLam(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = substCsMinLetRec(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = substCsMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = substCsMinMatch(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = substCsMinPrimApp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = substCsMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Sequence(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_DONE:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_VAR:
        break;
    default:
        cant_happen("unrecognized MinExp type %s", minExpTypeName(node->type));
    }
    UNPROTECT(save);
    LEAVE(substCsMinExp);
    return result;
}

static MinCondCases *substCsMinCondCases(MinCondCases *node,
                                         MinExpTable *context) {
    if (node == NULL)
        return NULL;
    ENTER(substCsMinCondCases);
    int save = STARTPROTECT();
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = substCsMinIntCondCases(variant, context);
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
            substCsMinCharCondCases(variant, context);
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
    LEAVE(substCsMinCondCases);
    return result;
}

static MinExp *substCs(MinExp *callSite, MinExpTable *context) {
    if (isMinExp_Var(callSite)) {
        MinExp *replacement = NULL;
        if (getMinExpTable(context, getMinExp_Var(callSite), &replacement)) {
            return replacement;
        } else {
            return callSite;
        }
    } else {
        return substCsMinExp(callSite, context);
    }
}