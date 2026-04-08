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

#include "minlam_beta.h"
#include "memory.h"
#include "minlam.h"
#include "minlam_cfo.h"
#include "minlam_pp.h"
#include "minlam_subst.h"
#include "symbol.h"
#include "utils_helper.h"

#ifdef DEBUG_MINLAM_BETA
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinExprList *betaMinExprList(MinExprList *node, ObjectMap *context);
static MinPrimApp *betaMinPrimApp(MinPrimApp *node, ObjectMap *context);
static MinExp *betaMinApply(MinExp *node, ObjectMap *context);
static MinIff *betaMinIff(MinIff *node, ObjectMap *context);
static MinCond *betaMinCond(MinCond *node, ObjectMap *context);
static MinIntCondCases *betaMinIntCondCases(MinIntCondCases *node,
                                            ObjectMap *context);
static MinCharCondCases *betaMinCharCondCases(MinCharCondCases *node,
                                              ObjectMap *context);
static MinMatch *betaMinMatch(MinMatch *node, ObjectMap *context);
static MinMatchList *betaMinMatchList(MinMatchList *node, ObjectMap *context);
static MinLetRec *betaMinLetRec(MinLetRec *node, ObjectMap *context);
static MinBindings *betaMinBindings(MinBindings *node, ObjectMap *context);
static MinAmb *betaMinAmb(MinAmb *node, ObjectMap *context);
static MinCondCases *betaMinCondCases(MinCondCases *node, ObjectMap *context);
static SymbolList *betaSymbolList(SymbolList *node, ObjectMap *context);
static bool isAexp(MinExp *exp);
static bool areAexpList(MinExprList *args);
static bool isIdentityLam(MinLam *lam);
static bool isCheapToDuplicate(MinExp *exp);
static MinLam *betaMinLam(MinLam *node, ObjectMap *context);
static MinExp *_betaMinExp(MinExp *node, ObjectMap *context);

char *beta_conversion_function = NULL;

static int curryDepth(MinExp *exp) {
    int depth = 0;
    while (isMinExp_Lam(exp)) {
        MinLam *lam = getMinExp_Lam(exp);
        depth += countSymbolList(lam->args);
        exp = lam->exp;
    }
    return depth;
}

static MinExpTable *makeSubstitutionTable(SymbolList *fargs,
                                          MinExprList *aargs) {
    MinExpTable *table = newMinExpTable();
    int save = PROTECT(table);
    while (fargs != NULL && aargs != NULL) {
        setMinExpTable(table, fargs->symbol, aargs->exp);
        fargs = fargs->next;
        aargs = aargs->next;
    }
    UNPROTECT(save);
    return table;
}

static bool areAexpList(MinExprList *args) {
    while (args != NULL) {
        if (!isAexp(args->exp)) {
            return false;
        }
        args = args->next;
    }
    return true;
}

static bool isAexp(MinExp *exp) {
    if (exp == NULL) {
        return false;
    }
    switch (exp->type) {
    case MINEXP_TYPE_LAM:
    case MINEXP_TYPE_VAR:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
        return true;
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *prim = getMinExp_Prim(exp);
        return isAexp(prim->exp1) && isAexp(prim->exp2);
    }
    case MINEXP_TYPE_MAKEVEC:
        return areAexpList(getMinExp_MakeVec(exp));
    case MINEXP_TYPE_SEQUENCE:
        return areAexpList(getMinExp_Sequence(exp));
    default:
        return false;
    }
}

static bool isIdentityLam(MinLam *lam) {
    if (lam == NULL || lam->args == NULL || lam->args->next != NULL ||
        lam->exp == NULL) {
        return false;
    }
    if (lam->exp->type != MINEXP_TYPE_VAR) {
        return false;
    }
    return getMinExp_Var(lam->exp) == lam->args->symbol;
}

static bool isCheapToDuplicate(MinExp *exp) {
    if (exp == NULL) {
        return false;
    }
    switch (exp->type) {
    case MINEXP_TYPE_VAR:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
        return true;
    default:
        return false;
    }
}

//////////////////////////
// Visitor implementations
//////////////////////////

static MinLam *betaMinLam(MinLam *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinLam);
    bool changed = false;
    SymbolList *new_args = betaSymbolList(node->args, context);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);
    MinExp *new_exp = _betaMinExp(node->exp, context);
    PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinLam *result = changed ? newMinLam(CPI(node), new_args, new_exp) : node;
    UNPROTECT(save);
    LEAVE(betaMinLam);
    return result;
}

static MinExprList *betaMinExprList(MinExprList *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinExprList);
    bool changed = false;
    MinExp *new_exp = _betaMinExp(node->exp, context);
    PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = betaMinExprList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }
    UNPROTECT(save);
    LEAVE(betaMinExprList);
    return result;
}

static MinPrimApp *betaMinPrimApp(MinPrimApp *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinPrimApp);
    bool changed = false;
    MinExp *new_exp1 = _betaMinExp(node->exp1, context);
    PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = _betaMinExp(node->exp2, context);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);
    MinPrimApp *result = node;
    if (changed) {
        result = newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
    }
    UNPROTECT(save);
    LEAVE(betaMinPrimApp);
    return result;
}

// too many args
// ((λ (f1) body) a1 a2) => reduce(body[f1/a1] a2)
static MinExp *betaMinOverApply(MinExp *body, SymbolList *fargs,
                                MinExprList *aargs, bool isBuiltin,
                                ObjectMap *context) {
    // implicitly creates the short list [f1/a1]
    MinExp *result = NULL;
    if (result)
        return result;
    MinExpTable *table = makeSubstitutionTable(fargs, aargs);
    int save = PROTECT(table);
    result = substMinExp(body, table);
    PROTECT(result);
    for (int i = countSymbolList(fargs); i > 0; i--) {
        aargs = aargs->next;
    }
    result = makeMinExp_Apply(CPI(result), result, aargs);
    getMinExp_Apply(result)->isBuiltin = isBuiltin;
    PROTECT(result);
    result = _betaMinExp(result, context);
    PROTECT(result);
    UNPROTECT(save);
    return result;
}

// too few args
// ((λ (f1 f2) body) a1) => (λ (f2) body[f1/a1])
static MinExp *betaMinUnderApply(MinExp *body, SymbolList *fargs,
                                 MinExprList *aargs, ObjectMap *context) {
    int save = PROTECT(context);
    MinExp *result = NULL;
    if (result)
        return result;
    MinExpTable *table = makeSubstitutionTable(fargs, aargs);
    PROTECT(table);
    result = substMinExp(body, table);
    PROTECT(result);
    // step past the substituted fargs to f2
    for (int i = countMinExprList(aargs); i > 0; i--) {
        fargs = fargs->next;
    }
    result = makeMinExp_Lam(CPI(result), fargs, result);
    PROTECT(result);
    UNPROTECT(save);
    return result;
}

// same number of args
// ((λ (f1 f2) body) a1 a2) => reduce(body[f1/a1, f2/a2])
static MinExp *betaMinSimpleApply(MinExp *body, SymbolList *fargs,
                                  MinExprList *aargs, ObjectMap *context) {
    MinExp *result = NULL;
    if (result)
        return result;
    int save = PROTECT(context);
    MinExpTable *table = makeSubstitutionTable(fargs, aargs);
    PROTECT(table);
    result = substMinExp(body, table);
    PROTECT(result);
    result = _betaMinExp(result, context);
    PROTECT(result);
    UNPROTECT(save);
    return result;
}

static MinExp *betaMinApplyLambda(MinLam *lam, MinExprList *aargs,
                                  bool isBuiltin, ObjectMap *context) {
    int save = PROTECT(context);
    int num_aargs = countMinExprList(aargs);
    SymbolList *fargs = lam->args;
    int num_fargs = countSymbolList(fargs);
    // Exact nullary application is safe to collapse directly: there are no
    // arguments to substitute, so no risk of duplicating/reordering argument
    // evaluation.
    if (num_aargs == 0 && num_fargs == 0) {
        MinExp *res = _betaMinExp(lam->exp, context);
        UNPROTECT(save);
        return res;
    }
    if (num_aargs <= 0 || num_fargs <= 0) {
        UNPROTECT(save);
        return NULL;
    }
    // Conservative extension: exact identity application can be reduced even
    // when the argument is not an A-expression because it does not duplicate
    // or discard the argument.
    if (num_fargs == 1 && num_aargs == 1 && isIdentityLam(lam)) {
        UNPROTECT(save);
        return aargs->exp;
    }
    // Safety rule for this pass: only beta-reduce when each substituted
    // argument is an A-expression.
    //
    // This compiler evaluates function arguments before call (call-by-value).
    // Unrestricted substitution can duplicate or delay non-value arguments,
    // changing observable behavior for effects and control operators such as
    // amb/backtracking, call/cc, I/O, and namespace lookups.
    //
    // For now we only substitute a prefix of arguments when all substituted
    // arguments are A-expressions. Otherwise we leave the APPLY shape
    // intact and only recurse into children.
    int substitutions = num_fargs < num_aargs ? num_fargs : num_aargs;
    MinExprList *cur = aargs;
    for (int i = 0; i < substitutions; i++) {
        if (cur == NULL || !isAexp(cur->exp)) {
            UNPROTECT(save);
            return NULL;
        }
        cur = cur->next;
    }
    SymbolSet *fargset = symbolListToSet(fargs);
    PROTECT(fargset);
    IntMap *counts = countFreeOccurences(lam->exp, fargset);
    PROTECT(counts);
    SymbolList *formal = fargs;
    cur = aargs;
    for (int i = 0; i < substitutions; i++) {
        int count = 0;
        if (formal == NULL || cur == NULL) {
            cant_happen("missing formal/actual during beta reduction");
        }
        if (getIntMap(counts, formal->symbol, &count) && count > 1 &&
            !isCheapToDuplicate(cur->exp)) {
            UNPROTECT(save);
            return NULL;
        }
        formal = formal->next;
        cur = cur->next;
    }
    MinExp *res = NULL;
    if (num_fargs < num_aargs) {
        res = betaMinOverApply(lam->exp, fargs, aargs, isBuiltin, context);
    } else if (num_fargs > num_aargs) {
        res = betaMinUnderApply(lam->exp, fargs, aargs, context);
    } else {
        res = betaMinSimpleApply(lam->exp, fargs, aargs, context);
    }
    UNPROTECT(save);
    return res;
}

// N.B. MinExp not MinApply so it can return a different type.
static MinExp *betaMinApply(MinExp *exp, ObjectMap *context) {
    if (exp == NULL)
        return NULL;
    int save = PROTECT(context);

    ENTER(betaMinApply);
    MinApply *node = getMinExp_Apply(exp);
    bool changed = false;
    MinExprList *redaargs = betaMinExprList(node->args, context);
    PROTECT(redaargs);
    changed = changed || (redaargs != node->args);
    MinExp *new_function = _betaMinExp(node->function, context);
    PROTECT(new_function);
    changed = changed || (new_function != node->function);
    if (new_function->type == MINEXP_TYPE_LAM) {
        MinExp *result = betaMinApplyLambda(getMinExp_Lam(new_function),
                                            redaargs, node->isBuiltin, context);
        if (result != NULL) {
            UNPROTECT(save);
            LEAVE(betaMinApply);
            return result;
        }
    }
    MinExp *result = exp;
    if (changed) {
        result = makeMinExp_Apply(CPI(node), new_function, redaargs);
        getMinExp_Apply(result)->isBuiltin = node->isBuiltin;
    }
    UNPROTECT(save);
    LEAVE(betaMinApply);
    return result;
}

static MinIff *betaMinIff(MinIff *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinIff);
    bool changed = false;
    MinExp *new_condition = _betaMinExp(node->condition, context);
    PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = _betaMinExp(node->consequent, context);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = _betaMinExp(node->alternative, context);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);
    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }
    UNPROTECT(save);
    LEAVE(betaMinIff);
    return result;
}

static MinCond *betaMinCond(MinCond *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinCond);
    bool changed = false;
    MinExp *new_value = _betaMinExp(node->value, context);
    PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = betaMinCondCases(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }
    UNPROTECT(save);
    LEAVE(betaMinCond);
    return result;
}

static MinIntCondCases *betaMinIntCondCases(MinIntCondCases *node,
                                            ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinIntCondCases);
    bool changed = false;
    MinExp *new_body = _betaMinExp(node->body, context);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = betaMinIntCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    if (changed) {
        MinIntCondCases *result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        LEAVE(betaMinIntCondCases);
        return result;
    }
    UNPROTECT(save);
    LEAVE(betaMinIntCondCases);
    return node;
}

static MinCharCondCases *betaMinCharCondCases(MinCharCondCases *node,
                                              ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinCharCondCases);
    bool changed = false;
    MinExp *new_body = _betaMinExp(node->body, context);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = betaMinCharCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    if (changed) {
        MinCharCondCases *result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        result->isDefault = node->isDefault;
        UNPROTECT(save);
        LEAVE(betaMinCharCondCases);
        return result;
    }
    UNPROTECT(save);
    LEAVE(betaMinCharCondCases);
    return node;
}

static MinMatch *betaMinMatch(MinMatch *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinMatch);
    bool changed = false;
    MinExp *new_index = _betaMinExp(node->index, context);
    PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = betaMinMatchList(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    if (changed) {
        MinMatch *result = newMinMatch(CPI(node), new_index, new_cases);
        UNPROTECT(save);
        LEAVE(betaMinMatch);
        return result;
    }
    UNPROTECT(save);
    LEAVE(betaMinMatch);
    return node;
}

static MinMatchList *betaMinMatchList(MinMatchList *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinMatchList);
    bool changed = false;
    MinExp *new_body = _betaMinExp(node->body, context);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = betaMinMatchList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    if (changed) {
        MinMatchList *result =
            newMinMatchList(CPI(node), node->matches, new_body, new_next);
        UNPROTECT(save);
        LEAVE(betaMinMatchList);
        return result;
    }
    UNPROTECT(save);
    LEAVE(betaMinMatchList);
    return node;
}

static MinLetRec *betaMinLetRec(MinLetRec *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinLetRec);
    bool changed = false;
    MinBindings *new_bindings = betaMinBindings(node->bindings, context);
    PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = _betaMinExp(node->body, context);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    if (changed) {
        MinLetRec *result = newMinLetRec(CPI(node), new_bindings, new_body);
        UNPROTECT(save);
        LEAVE(betaMinLetRec);
        return result;
    }
    UNPROTECT(save);
    LEAVE(betaMinLetRec);
    return node;
}

static MinBindings *betaMinBindings(MinBindings *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinBindings);
    bool changed = false;
    MinExp *new_val = _betaMinExp(node->val, context);
    PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = betaMinBindings(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinBindings *result = node;
    if (changed) {
        result = newMinBindings(CPI(node), node->var, new_val, new_next);
        result->arity = curryDepth(new_val);
    }
    if (beta_conversion_function != NULL &&
        strcmp(beta_conversion_function, result->var->name) == 0) {
        ppMinExp(stdout, new_val);
        exit(0);
    }
    UNPROTECT(save);
    LEAVE(betaMinBindings);
    return result;
}

static MinAmb *betaMinAmb(MinAmb *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinAmb);
    bool changed = false;
    MinExp *new_left = _betaMinExp(node->left, context);
    PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = _betaMinExp(node->right, context);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);
    if (changed) {
        MinAmb *result = newMinAmb(CPI(node), new_left, new_right);
        UNPROTECT(save);
        LEAVE(betaMinAmb);
        return result;
    }
    UNPROTECT(save);
    LEAVE(betaMinAmb);
    return node;
}

MinExp *betaMinExp(MinExp *node) {
    ObjectMap *context = newObjectMap();
    int save = PROTECT(context);
    MinExp *res = _betaMinExp(node, context);
    UNPROTECT(save);
    return res;
}

static MinExp *_betaMinExp(MinExp *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    ENTER(_betaMinExp);
    MinExp *result = node;
    int save = PROTECT(context);
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = betaMinAmb(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        result = betaMinApply(node, context);
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = _betaMinExp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_CallCC(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_COND: {
        MinCond *variant = getMinExp_Cond(node);
        MinCond *new_variant = betaMinCond(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = betaMinIff(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = betaMinLam(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = betaMinLetRec(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = betaMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = betaMinMatch(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = betaMinPrimApp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = betaMinExprList(variant, context);
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
    LEAVE(_betaMinExp);
    return result;
}

static MinCondCases *betaMinCondCases(MinCondCases *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaMinCondCases);
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = betaMinIntCondCases(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = betaMinCharCondCases(variant, context);
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
    LEAVE(betaMinCondCases);
    return result;
}

static SymbolList *betaSymbolList(SymbolList *node, ObjectMap *context) {
    if (node == NULL)
        return NULL;
    int save = PROTECT(context);
    ENTER(betaSymbolList);
    bool changed = false;
    SymbolList *new_next = betaSymbolList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    if (changed) {
        SymbolList *result = newSymbolList(CPI(node), node->symbol, new_next);
        UNPROTECT(save);
        LEAVE(betaSymbolList);
        return result;
    }
    UNPROTECT(save);
    LEAVE(betaSymbolList);
    return node;
}
