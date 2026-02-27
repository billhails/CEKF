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

#include "minlam_eta.h"
#include "memory.h"
#include "minlam.h"
#include "minlam_occurs.h"
#include "utils_helper.h"

#ifdef DEBUG_MINLAM_ETA
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinExp *etaMinLam(MinExp *node);
static MinExprList *etaMinExprList(MinExprList *node);
static MinPrimApp *etaMinPrimApp(MinPrimApp *node);
static MinApply *etaMinApply(MinApply *node);
static MinIff *etaMinIff(MinIff *node);
static MinCond *etaMinCond(MinCond *node);
static MinIntCondCases *etaMinIntCondCases(MinIntCondCases *node);
static MinCharCondCases *etaMinCharCondCases(MinCharCondCases *node);
static MinMatch *etaMinMatch(MinMatch *node);
static MinMatchList *etaMinMatchList(MinMatchList *node);
static MinLetRec *etaMinLetRec(MinLetRec *node);
static MinBindings *etaMinBindings(MinBindings *node);
static MinAmb *etaMinAmb(MinAmb *node);
static MinCondCases *etaMinCondCases(MinCondCases *node);
static bool etaSafeFunction(MinExp *exp);
static MinExp *etaMinBindingValue(MinExp *exp);
static SymbolSet *etaBindingSymbols(MinBindings *node);

static SymbolSet *etaLetRecSymbols = NULL;

// true if aargs are all symbols and the same symbols as fargs.
static bool fargsEqAargs(SymbolList *fargs, MinExprList *aargs) {
    if (fargs == NULL) {
        return aargs == NULL;
    } else if (aargs == NULL) {
        return false;
    } else {
        if (!isMinExp_Var(aargs->exp)) {
            return false;
        }
        return fargs->symbol == getMinExp_Var(aargs->exp) &&
               fargsEqAargs(fargs->next, aargs->next);
    }
}

static bool etaSafeFunction(MinExp *exp) { return exp != NULL; }

static SymbolSet *etaBindingSymbols(MinBindings *node) {
    ENTER(etaBindingSymbols);
    SymbolSet *symbols = newSymbolSet();
    int save = PROTECT(symbols);
    while (node != NULL) {
        setSymbolSet(symbols, node->var);
        node = node->next;
    }
    UNPROTECT(save);
    LEAVE(etaBindingSymbols);
    return symbols;
}

static MinExp *etaMinBindingValue(MinExp *exp) {
    ENTER(etaMinBindingValue);
    if (exp == NULL) {
        LEAVE(etaMinBindingValue);
        return NULL;
    }

    if (!isMinExp_Lam(exp)) {
        LEAVE(etaMinBindingValue);
        return exp;
    }

    MinLam *lambda = getMinExp_Lam(exp);
    MinExp *new_body = etaMinExp(lambda->exp);

    if (new_body != lambda->exp) {
        int save = PROTECT(new_body);
        MinExp *result = makeMinExp_Lam(CPI(lambda), lambda->args, new_body);
        UNPROTECT(save);
        LEAVE(etaMinBindingValue);
        return result;
    }

    LEAVE(etaMinBindingValue);
    return exp;
}

//////////////////////////
// Visitor implementations
//////////////////////////

// N.B. MinExp not MinLam
static MinExp *etaMinLam(MinExp *exp) {
    ENTER(etaMinLam);
    MinLam *lambda = getMinExp_Lam(exp);

    if (lambda == NULL) {
        LEAVE(etaMinLam);
        return NULL;
    }

    // η(λ.x (f x))) => ηf, where x is not free in f
    if (lambda->exp != NULL && isMinExp_Apply(lambda->exp)) {
        MinApply *apply = getMinExp_Apply(lambda->exp); // (f x)
        if (etaSafeFunction(apply->function) &&
            fargsEqAargs(lambda->args, apply->args)) {
            SymbolSet *symbols = symbolListToSet(lambda->args);
            int save = PROTECT(symbols);
            bool touchesLetRec =
                etaLetRecSymbols != NULL &&
                occursMinExp(apply->function, etaLetRecSymbols);
            if (!touchesLetRec && !occursMinExp(apply->function, symbols)) {
                MinExp *result = etaMinExp(apply->function); // ηf
                UNPROTECT(save);
                LEAVE(etaMinLam);
                return result;
            }
            UNPROTECT(save);
        }
    }

    // η(λ.x (f x))) => (λ.x (ηf x)) otherwise
    MinExp *body = etaMinExp(lambda->exp);
    if (body != lambda->exp) {
        int save = PROTECT(body);
        MinExp *result = makeMinExp_Lam(CPI(lambda), lambda->args, body);
        UNPROTECT(save);
        LEAVE(etaMinLam);
        return result;
    }

    LEAVE(etaMinLam);
    return exp;
}

static MinExprList *etaMinExprList(MinExprList *node) {
    ENTER(etaMinExprList);
    if (node == NULL) {
        LEAVE(etaMinExprList);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp = etaMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = etaMinExprList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinExprList *result = newMinExprList(CPI(node), new_exp, new_next);
        UNPROTECT(save);
        LEAVE(etaMinExprList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinExprList);
    return node;
}

static MinPrimApp *etaMinPrimApp(MinPrimApp *node) {
    ENTER(etaMinPrimApp);
    if (node == NULL) {
        LEAVE(etaMinPrimApp);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp1 = etaMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = etaMinExp(node->exp2);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);

    if (changed) {
        MinPrimApp *result =
            newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
        UNPROTECT(save);
        LEAVE(etaMinPrimApp);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinPrimApp);
    return node;
}

static MinApply *etaMinApply(MinApply *node) {
    ENTER(etaMinApply);
    if (node == NULL) {
        LEAVE(etaMinApply);
        return NULL;
    }

    bool changed = false;
    MinExp *new_function = etaMinExp(node->function);
    int save = PROTECT(new_function);
    changed = changed || (new_function != node->function);
    MinExprList *new_args = etaMinExprList(node->args);
    PROTECT(new_args);
    changed = changed || (new_args != node->args);

    if (changed) {
        MinApply *result = newMinApply(CPI(node), new_function, new_args);
        UNPROTECT(save);
        LEAVE(etaMinApply);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinApply);
    return node;
}

static MinIff *etaMinIff(MinIff *node) {
    ENTER(etaMinIff);
    if (node == NULL) {
        LEAVE(etaMinIff);
        return NULL;
    }

    bool changed = false;
    MinExp *new_condition = etaMinExp(node->condition);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = etaMinExp(node->consequent);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = etaMinExp(node->alternative);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);

    if (changed) {
        MinIff *result = newMinIff(CPI(node), new_condition, new_consequent,
                                   new_alternative);
        UNPROTECT(save);
        LEAVE(etaMinIff);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinIff);
    return node;
}

static MinCond *etaMinCond(MinCond *node) {
    ENTER(etaMinCond);
    if (node == NULL) {
        LEAVE(etaMinCond);
        return NULL;
    }

    bool changed = false;
    MinExp *new_value = etaMinExp(node->value);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = etaMinCondCases(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    if (changed) {
        MinCond *result = newMinCond(CPI(node), new_value, new_cases);
        UNPROTECT(save);
        LEAVE(etaMinCond);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinCond);
    return node;
}

static MinIntCondCases *etaMinIntCondCases(MinIntCondCases *node) {
    ENTER(etaMinIntCondCases);
    if (node == NULL) {
        LEAVE(etaMinIntCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = etaMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = etaMinIntCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinIntCondCases *result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        LEAVE(etaMinIntCondCases);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinIntCondCases);
    return node;
}

static MinCharCondCases *etaMinCharCondCases(MinCharCondCases *node) {
    ENTER(etaMinCharCondCases);
    if (node == NULL) {
        LEAVE(etaMinCharCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = etaMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = etaMinCharCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinCharCondCases *result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        LEAVE(etaMinCharCondCases);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinCharCondCases);
    return node;
}

static MinMatch *etaMinMatch(MinMatch *node) {
    ENTER(etaMinMatch);
    if (node == NULL) {
        LEAVE(etaMinMatch);
        return NULL;
    }

    bool changed = false;
    MinExp *new_index = etaMinExp(node->index);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = etaMinMatchList(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    if (changed) {
        MinMatch *result = newMinMatch(CPI(node), new_index, new_cases);
        UNPROTECT(save);
        LEAVE(etaMinMatch);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinMatch);
    return node;
}

static MinMatchList *etaMinMatchList(MinMatchList *node) {
    ENTER(etaMinMatchList);
    if (node == NULL) {
        LEAVE(etaMinMatchList);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = etaMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = etaMinMatchList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinMatchList *result =
            newMinMatchList(CPI(node), node->matches, new_body, new_next);
        UNPROTECT(save);
        LEAVE(etaMinMatchList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinMatchList);
    return node;
}

static MinLetRec *etaMinLetRec(MinLetRec *node) {
    ENTER(etaMinLetRec);
    if (node == NULL) {
        LEAVE(etaMinLetRec);
        return NULL;
    }

    int save = PROTECT(NULL);
    SymbolSet *previousLetRecSymbols = etaLetRecSymbols;
    SymbolSet *bindingSymbols = etaBindingSymbols(node->bindings);
    PROTECT(bindingSymbols);
    if (previousLetRecSymbols != NULL) {
        bindingSymbols = unionSymbolSet(previousLetRecSymbols, bindingSymbols);
        PROTECT(bindingSymbols);
    }
    etaLetRecSymbols = bindingSymbols;

    bool changed = false;
    MinBindings *new_bindings = etaMinBindings(node->bindings);
    PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = etaMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);

    etaLetRecSymbols = previousLetRecSymbols;

    if (changed) {
        MinLetRec *result = newMinLetRec(CPI(node), new_bindings, new_body);
        UNPROTECT(save);
        LEAVE(etaMinLetRec);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinLetRec);
    return node;
}

static MinBindings *etaMinBindings(MinBindings *node) {
    ENTER(etaMinBindings);
    if (node == NULL) {
        LEAVE(etaMinBindings);
        return NULL;
    }

    bool changed = false;
    // LetRec bindings are scope-sensitive for later normalization/annotation.
    // Preserve binding shape, but allow eta reduction inside lambda bodies.
    MinExp *new_val = etaMinBindingValue(node->val);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = etaMinBindings(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinBindings *result =
            newMinBindings(CPI(node), node->var, new_val, new_next);
        UNPROTECT(save);
        LEAVE(etaMinBindings);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinBindings);
    return node;
}

static MinAmb *etaMinAmb(MinAmb *node) {
    ENTER(etaMinAmb);
    if (node == NULL) {
        LEAVE(etaMinAmb);
        return NULL;
    }

    bool changed = false;
    MinExp *new_left = etaMinExp(node->left);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = etaMinExp(node->right);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);

    if (changed) {
        MinAmb *result = newMinAmb(CPI(node), new_left, new_right);
        UNPROTECT(save);
        LEAVE(etaMinAmb);
        return result;
    }

    UNPROTECT(save);
    LEAVE(etaMinAmb);
    return node;
}

MinExp *etaMinExp(MinExp *node) {
    ENTER(etaMinExp);
    if (node == NULL) {
        LEAVE(etaMinExp);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinExp *result = node;

    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = etaMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        MinApply *variant = getMinExp_Apply(node);
        MinApply *new_variant = etaMinApply(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Apply(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_ARGS: {
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = etaMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Args(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_BACK: {
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        break;
    }
    case MINEXP_TYPE_BINDINGS: {
        MinBindings *variant = getMinExp_Bindings(node);
        MinBindings *new_variant = etaMinBindings(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = etaMinExp(variant);
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
        MinCond *new_variant = etaMinCond(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = etaMinIff(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        result = etaMinLam(node);
        break;
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = etaMinLetRec(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = etaMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = etaMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = etaMinPrimApp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = etaMinExprList(variant);
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
    LEAVE(etaMinExp);
    return result;
}

static MinCondCases *etaMinCondCases(MinCondCases *node) {
    ENTER(etaMinCondCases);
    if (node == NULL) {
        LEAVE(etaMinCondCases);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinCondCases *result = node;

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = etaMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = etaMinCharCondCases(variant);
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
    LEAVE(etaMinCondCases);
    return result;
}