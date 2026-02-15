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

#include "minlam_beta.h"
#include "minlam_pp.h"
#include "minlam_subst.h"

#ifdef DEBUG_MINLAM_BETA
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinLam *betaMinLam(MinLam *node);
static MinExprList *betaMinExprList(MinExprList *node);
static MinPrimApp *betaMinPrimApp(MinPrimApp *node);
static MinExp *betaMinApply(MinExp *node);
static MinLookUp *betaMinLookUp(MinLookUp *node);
static MinIff *betaMinIff(MinIff *node);
static MinCond *betaMinCond(MinCond *node);
static MinIntCondCases *betaMinIntCondCases(MinIntCondCases *node);
static MinCharCondCases *betaMinCharCondCases(MinCharCondCases *node);
static MinMatch *betaMinMatch(MinMatch *node);
static MinMatchList *betaMinMatchList(MinMatchList *node);
static MinIntList *betaMinIntList(MinIntList *node);
static MinLetRec *betaMinLetRec(MinLetRec *node);
static MinBindings *betaMinBindings(MinBindings *node);
static MinAmb *betaMinAmb(MinAmb *node);
static MinAlphaEnv *betaMinAlphaEnv(MinAlphaEnv *node);
static MinCondCases *betaMinCondCases(MinCondCases *node);
static SymbolMap *betaSymbolMap(SymbolMap *node);
static SymbolList *betaSymbolList(SymbolList *node);
static MinNameSpaceArray *betaMinNameSpaceArray(MinNameSpaceArray *node);
static MinAlphaEnvArray *betaMinAlphaEnvArray(MinAlphaEnvArray *node);
static bool isAexp(MinExp *exp);
static bool areAexpList(MinExprList *args);
static bool isIdentityLam(MinLam *lam);

char *beta_conversion_function = NULL;

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
    case MINEXP_TYPE_NAMESPACES:
        return true;
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *prim = getMinExp_Prim(exp);
        return isAexp(prim->exp1) && isAexp(prim->exp2);
    }
    case MINEXP_TYPE_MAKEVEC:
        return areAexpList(getMinExp_MakeVec(exp));
    case MINEXP_TYPE_SEQUENCE:
        return areAexpList(getMinExp_Sequence(exp));
    case MINEXP_TYPE_LOOKUP:
        return isAexp(getMinExp_LookUp(exp)->exp);
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

//////////////////////////
// Visitor implementations
//////////////////////////

static MinLam *betaMinLam(MinLam *node) {
    ENTER(betaMinLam);
    if (node == NULL) {
        LEAVE(betaMinLam);
        return NULL;
    }

    bool changed = false;
    SymbolList *new_args = betaSymbolList(node->args);
    int save = PROTECT(new_args);
    changed = changed || (new_args != node->args);
    MinExp *new_exp = betaMinExp(node->exp);
    PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);

    if (changed) {
        MinLam *result = newMinLam(CPI(node), new_args, new_exp);
        UNPROTECT(save);
        LEAVE(betaMinLam);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinLam);
    return node;
}

static MinExprList *betaMinExprList(MinExprList *node) {
    ENTER(betaMinExprList);
    if (node == NULL) {
        LEAVE(betaMinExprList);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp = betaMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = betaMinExprList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinExprList *result = newMinExprList(CPI(node), new_exp, new_next);
        UNPROTECT(save);
        LEAVE(betaMinExprList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinExprList);
    return node;
}

static MinPrimApp *betaMinPrimApp(MinPrimApp *node) {
    ENTER(betaMinPrimApp);
    if (node == NULL) {
        LEAVE(betaMinPrimApp);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp1 = betaMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = betaMinExp(node->exp2);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);

    if (changed) {
        MinPrimApp *result =
            newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
        UNPROTECT(save);
        LEAVE(betaMinPrimApp);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinPrimApp);
    return node;
}

// too many args
// ((λ (f1) body) a1 a2) => reduce(body[f1/a1] a2)
static MinExp *betaMinOverApply(MinExp *body, SymbolList *fargs,
                                MinExprList *aargs) {
#if 0
    body = betaMinExp(body);
    int save = PROTECT(body);
    MinExp *lam = makeMinExp_Lam(CPI(body), fargs, body);
    PROTECT(lam);
    MinExp *result = makeMinExp_Apply(CPI(body), lam, aargs);
    UNPROTECT(save);
    return result;
#else
    // implicitly creates the short list [f1/a1]
    MinExpTable *table = makeSubstitutionTable(fargs, aargs);
    int save = PROTECT(table);
    body = substMinExp(body, table);
    PROTECT(body);

    for (int i = countSymbolList(fargs); i > 0; i--) {
        aargs = aargs->next;
    }

    MinExp *result = makeMinExp_Apply(CPI(body), body, aargs);
    PROTECT(result);
    result = betaMinExp(result);
    UNPROTECT(save);
    return result;
#endif
}

// too few args
// ((λ (f1 f2) body) a1) => (λ (f2) body[f1/a1])
static MinExp *betaMinUnderApply(MinExp *body, SymbolList *fargs,
                                 MinExprList *aargs) {
#if 0
    body = betaMinExp(body);
    int save = PROTECT(body);
    MinExp *lam = makeMinExp_Lam(CPI(body), fargs, body);
    PROTECT(lam);
    MinExp *result = makeMinExp_Apply(CPI(body), lam, aargs);
    UNPROTECT(save);
    return result;
#else
    MinExpTable *table = makeSubstitutionTable(fargs, aargs);
    int save = PROTECT(table);
    body = substMinExp(body, table);
    PROTECT(body);

    // step past the substituted fargs to f2
    for (int i = countMinExprList(aargs); i > 0; i--) {
        fargs = fargs->next;
    }

    MinExp *result = makeMinExp_Lam(CPI(body), fargs, body);
    UNPROTECT(save);
    return result;
#endif
}

// same number of args
// ((λ (f1 f2) body) a1 a2) => reduce(body[f1/a1, f2/a2])
static MinExp *betaMinSimpleApply(MinExp *body, SymbolList *fargs,
                                  MinExprList *aargs) {
    MinExpTable *table = makeSubstitutionTable(fargs, aargs);
    int save = PROTECT(table);
    body = substMinExp(body, table);
    PROTECT(body);
    body = betaMinExp(body);
    UNPROTECT(save);
    return body;
}

static MinExp *betaMinApplyLambda(MinLam *lam, MinExprList *aargs) {
    int num_aargs = countMinExprList(aargs);
    SymbolList *fargs = lam->args;
    int num_fargs = countSymbolList(fargs);

    // Exact nullary application is safe to collapse directly: there are no
    // arguments to substitute, so no risk of duplicating/reordering argument
    // evaluation.
    if (num_aargs == 0 && num_fargs == 0) {
        return betaMinExp(lam->exp);
    }

    if (num_aargs <= 0 || num_fargs <= 0) {
        return NULL;
    }

    // Conservative extension: exact identity application can be reduced even
    // when the argument is not an A-expression because it does not duplicate
    // or discard the argument.
    if (num_fargs == 1 && num_aargs == 1 && isIdentityLam(lam)) {
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
            return NULL;
        }
        cur = cur->next;
    }

    if (num_fargs < num_aargs) {
        return betaMinOverApply(lam->exp, fargs, aargs);
    } else if (num_fargs > num_aargs) {
        return betaMinUnderApply(lam->exp, fargs, aargs);
    } else {
        return betaMinSimpleApply(lam->exp, fargs, aargs);
    }
}

// N.B. MinExp not MinApply so it can return a different type.
static MinExp *betaMinApply(MinExp *exp) {
    ENTER(betaMinApply);
    if (exp == NULL) {
        LEAVE(betaMinApply);
        return NULL;
    }

    MinApply *node = getMinExp_Apply(exp);

    bool changed = false;
    MinExprList *redaargs = betaMinExprList(node->args);
    int save = PROTECT(redaargs);
    changed = changed || (redaargs != node->args);

    MinExp *new_function = betaMinExp(node->function);
    PROTECT(new_function);
    changed = changed || (new_function != node->function);

    if (new_function->type == MINEXP_TYPE_LAM) {
        MinExp *result =
            betaMinApplyLambda(getMinExp_Lam(new_function), redaargs);
        if (result != NULL) {
            UNPROTECT(save);
            LEAVE(betaMinApply);
            return result;
        }
    }

    if (changed) {
        MinExp *result = makeMinExp_Apply(CPI(node), new_function, redaargs);
        UNPROTECT(save);
        LEAVE(betaMinApply);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinApply);
    return exp;
}

static MinLookUp *betaMinLookUp(MinLookUp *node) {
    ENTER(betaMinLookUp);
    if (node == NULL) {
        LEAVE(betaMinLookUp);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp = betaMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);

    if (changed) {
        MinLookUp *result = newMinLookUp(CPI(node), node->nsId, new_exp);
        UNPROTECT(save);
        LEAVE(betaMinLookUp);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinLookUp);
    return node;
}

static MinIff *betaMinIff(MinIff *node) {
    ENTER(betaMinIff);
    if (node == NULL) {
        LEAVE(betaMinIff);
        return NULL;
    }

    bool changed = false;
    MinExp *new_condition = betaMinExp(node->condition);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = betaMinExp(node->consequent);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = betaMinExp(node->alternative);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);

    if (changed) {
        MinIff *result = newMinIff(CPI(node), new_condition, new_consequent,
                                   new_alternative);
        UNPROTECT(save);
        LEAVE(betaMinIff);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinIff);
    return node;
}

static MinCond *betaMinCond(MinCond *node) {
    ENTER(betaMinCond);
    if (node == NULL) {
        LEAVE(betaMinCond);
        return NULL;
    }

    bool changed = false;
    MinExp *new_value = betaMinExp(node->value);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = betaMinCondCases(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    if (changed) {
        MinCond *result = newMinCond(CPI(node), new_value, new_cases);
        UNPROTECT(save);
        LEAVE(betaMinCond);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinCond);
    return node;
}

static MinIntCondCases *betaMinIntCondCases(MinIntCondCases *node) {
    ENTER(betaMinIntCondCases);
    if (node == NULL) {
        LEAVE(betaMinIntCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = betaMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = betaMinIntCondCases(node->next);
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

static MinCharCondCases *betaMinCharCondCases(MinCharCondCases *node) {
    ENTER(betaMinCharCondCases);
    if (node == NULL) {
        LEAVE(betaMinCharCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = betaMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = betaMinCharCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinCharCondCases *result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        LEAVE(betaMinCharCondCases);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinCharCondCases);
    return node;
}

static MinMatch *betaMinMatch(MinMatch *node) {
    ENTER(betaMinMatch);
    if (node == NULL) {
        LEAVE(betaMinMatch);
        return NULL;
    }

    bool changed = false;
    MinExp *new_index = betaMinExp(node->index);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = betaMinMatchList(node->cases);
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

static MinMatchList *betaMinMatchList(MinMatchList *node) {
    ENTER(betaMinMatchList);
    if (node == NULL) {
        LEAVE(betaMinMatchList);
        return NULL;
    }

    bool changed = false;
    MinIntList *new_matches = betaMinIntList(node->matches);
    int save = PROTECT(new_matches);
    changed = changed || (new_matches != node->matches);
    MinExp *new_body = betaMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = betaMinMatchList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinMatchList *result =
            newMinMatchList(CPI(node), new_matches, new_body, new_next);
        UNPROTECT(save);
        LEAVE(betaMinMatchList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinMatchList);
    return node;
}

static MinIntList *betaMinIntList(MinIntList *node) {
    ENTER(betaMinIntList);
    if (node == NULL) {
        LEAVE(betaMinIntList);
        return NULL;
    }

    bool changed = false;
    MinIntList *new_next = betaMinIntList(node->next);
    int save = PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinIntList *result = newMinIntList(CPI(node), node->item, new_next);
        UNPROTECT(save);
        LEAVE(betaMinIntList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinIntList);
    return node;
}

static MinLetRec *betaMinLetRec(MinLetRec *node) {
    ENTER(betaMinLetRec);
    if (node == NULL) {
        LEAVE(betaMinLetRec);
        return NULL;
    }

    bool changed = false;
    MinBindings *new_bindings = betaMinBindings(node->bindings);
    int save = PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = betaMinExp(node->body);
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

static MinBindings *betaMinBindings(MinBindings *node) {
    ENTER(betaMinBindings);
    if (node == NULL) {
        LEAVE(betaMinBindings);
        return NULL;
    }

    bool changed = false;
    MinExp *new_val = betaMinExp(node->val);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = betaMinBindings(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        node = newMinBindings(CPI(node), node->var, new_val, new_next);
    }

    if (beta_conversion_function != NULL &&
        strcmp(beta_conversion_function, node->var->name) == 0) {
        ppMinExp(new_val);
    }

    UNPROTECT(save);
    LEAVE(betaMinBindings);
    return node;
}

static MinAmb *betaMinAmb(MinAmb *node) {
    ENTER(betaMinAmb);
    if (node == NULL) {
        LEAVE(betaMinAmb);
        return NULL;
    }

    bool changed = false;
    MinExp *new_left = betaMinExp(node->left);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = betaMinExp(node->right);
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

static MinAlphaEnv *betaMinAlphaEnv(MinAlphaEnv *node) {
    ENTER(betaMinAlphaEnv);
    if (node == NULL) {
        LEAVE(betaMinAlphaEnv);
        return NULL;
    }

    bool changed = false;
    SymbolMap *new_alphaTable = betaSymbolMap(node->alphaTable);
    int save = PROTECT(new_alphaTable);
    changed = changed || (new_alphaTable != node->alphaTable);
    MinAlphaEnv *new_next = betaMinAlphaEnv(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinAlphaEnvArray *new_nameSpaces = betaMinAlphaEnvArray(node->nameSpaces);
    PROTECT(new_nameSpaces);
    changed = changed || (new_nameSpaces != node->nameSpaces);

    if (changed) {
        MinAlphaEnv *result = newMinAlphaEnv(new_next);
        result->alphaTable = new_alphaTable;
        result->nameSpaces = new_nameSpaces;
        UNPROTECT(save);
        LEAVE(betaMinAlphaEnv);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinAlphaEnv);
    return node;
}

MinExp *betaMinExp(MinExp *node) {
    ENTER(betaMinExp);
    if (node == NULL) {
        LEAVE(betaMinExp);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinExp *result = node;

    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = betaMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        result = betaMinApply(node);
        break;
    }
    case MINEXP_TYPE_BACK: {
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = betaMinExp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_CallCC(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        break;
    }
    case MINEXP_TYPE_COND: {
        MinCond *variant = getMinExp_Cond(node);
        MinCond *new_variant = betaMinCond(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ENV: {
        break;
    }
    case MINEXP_TYPE_ERROR: {
        break;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = betaMinIff(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = betaMinLam(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = betaMinLetRec(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LOOKUP: {
        MinLookUp *variant = getMinExp_LookUp(node);
        MinLookUp *new_variant = betaMinLookUp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LookUp(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = betaMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = betaMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_NAMESPACES: {
        MinNameSpaceArray *variant = getMinExp_NameSpaces(node);
        MinNameSpaceArray *new_variant = betaMinNameSpaceArray(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_NameSpaces(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = betaMinPrimApp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = betaMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Sequence(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_STDINT: {
        break;
    }
    case MINEXP_TYPE_VAR: {
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }

    UNPROTECT(save);
    LEAVE(betaMinExp);
    return result;
}

static MinCondCases *betaMinCondCases(MinCondCases *node) {
    ENTER(betaMinCondCases);
    if (node == NULL) {
        LEAVE(betaMinCondCases);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinCondCases *result = node;

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = betaMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = betaMinCharCondCases(variant);
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

static SymbolMap *betaSymbolMap(SymbolMap *node) {
    ENTER(betaSymbolMap);
    if (node == NULL) {
        LEAVE(betaSymbolMap);
        return NULL;
    }

#ifdef NOTDEF
    Index i = 0;
    struct HashSymbol *value;
    HashSymbol *key;
    while ((key = iterateSymbolMap(node, &i, &value)) != NULL) {
        // Inspect/log key and value here
    }
#endif
    LEAVE(betaSymbolMap);
    return node;
}

static SymbolList *betaSymbolList(SymbolList *node) {
    ENTER(betaSymbolList);
    if (node == NULL) {
        LEAVE(betaSymbolList);
        return NULL;
    }

    bool changed = false;
    SymbolList *new_next = betaSymbolList(node->next);
    int save = PROTECT(new_next);
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

static MinNameSpaceArray *betaMinNameSpaceArray(MinNameSpaceArray *node) {
    ENTER(betaMinNameSpaceArray);
    if (node == NULL) {
        LEAVE(betaMinNameSpaceArray);
        return NULL;
    }

    bool changed = false;
    MinNameSpaceArray *result = newMinNameSpaceArray();
    int save = PROTECT(result);

    for (Index i = 0; i < node->size; i++) {
        struct MinExp *element = peeknMinNameSpaceArray(node, i);
        struct MinExp *new_element = betaMinExp(element);
        PROTECT(new_element);
        changed = changed || (new_element != element);
        pushMinNameSpaceArray(result, new_element);
    }

    if (changed) {
        UNPROTECT(save);
        LEAVE(betaMinNameSpaceArray);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinNameSpaceArray);
    return node;
}

static MinAlphaEnvArray *betaMinAlphaEnvArray(MinAlphaEnvArray *node) {
    ENTER(betaMinAlphaEnvArray);
    if (node == NULL) {
        LEAVE(betaMinAlphaEnvArray);
        return NULL;
    }

    bool changed = false;
    MinAlphaEnvArray *result = newMinAlphaEnvArray();
    int save = PROTECT(result);

    for (Index i = 0; i < node->size; i++) {
        struct MinAlphaEnv *element = peeknMinAlphaEnvArray(node, i);
        struct MinAlphaEnv *new_element = betaMinAlphaEnv(element);
        PROTECT(new_element);
        changed = changed || (new_element != element);
        pushMinAlphaEnvArray(result, new_element);
    }

    if (changed) {
        UNPROTECT(save);
        LEAVE(betaMinAlphaEnvArray);
        return result;
    }

    UNPROTECT(save);
    LEAVE(betaMinAlphaEnvArray);
    return node;
}
