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
 * Currying pass: transforms multi-arg lambdas to nested single-arg
 * lambdas, and multi-arg applications to nested single-arg applications.
 *
 *     (λ (x y z) body)  -->  (λ (x) (λ (y) (λ (z) body)))
 *     (f a b c)         -->  (((f a) b) c)
 */

#include "minlam_curry.h"
#include "memory.h"
#include "minlam.h"

#ifdef DEBUG_MINLAM_CURRY
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinExp *curryMinLam(MinExp *exp);
static MinExp *curryMinApply(MinExp *exp);
static MinExprList *curryMinExprList(MinExprList *node);
static MinPrimApp *curryMinPrimApp(MinPrimApp *node);
static MinIff *curryMinIff(MinIff *node);
static MinCond *curryMinCond(MinCond *node);
static MinIntCondCases *curryMinIntCondCases(MinIntCondCases *node);
static MinCharCondCases *curryMinCharCondCases(MinCharCondCases *node);
static MinMatch *curryMinMatch(MinMatch *node);
static MinMatchList *curryMinMatchList(MinMatchList *node);
static MinLetRec *curryMinLetRec(MinLetRec *node);
static MinBindings *curryMinBindings(MinBindings *node);
static MinAmb *curryMinAmb(MinAmb *node);
static MinCondCases *curryMinCondCases(MinCondCases *node);

// λ(x, y, z) body  -->  λ(x) (λ(y) (λ(z) curry(body)))
static MinExp *curryMinLam(MinExp *exp) {
    ENTER(curryMinLam);
    MinLam *node = getMinExp_Lam(exp);

    SymbolList *args = node->args;

    if (args == NULL || args->next == NULL) {
        // 0 or 1 args: just curry the body
        MinExp *new_body = curryMinExp(node->exp);
        if (new_body != node->exp) {
            int save = PROTECT(new_body);
            MinExp *result = makeMinExp_Lam(CPI(node), args, new_body);
            UNPROTECT(save);
            LEAVE(curryMinLam);
            return result;
        }
        LEAVE(curryMinLam);
        return exp;
    }

    // 2+ args: λ([first], curry(λ(rest, body)))
    SymbolList *firstArg = newSymbolList(CPI(args), args->symbol, NULL);
    int save = PROTECT(firstArg);
    MinExp *innerLam = makeMinExp_Lam(CPI(node), args->next, node->exp);
    PROTECT(innerLam);
    MinExp *curriedInner = curryMinExp(innerLam);
    PROTECT(curriedInner);
    MinExp *result = makeMinExp_Lam(CPI(node), firstArg, curriedInner);
    UNPROTECT(save);
    LEAVE(curryMinLam);
    return result;
}

// (f a b c)  -->  curry(((f a) b) c)
static MinExp *curryMinApply(MinExp *exp) {
    ENTER(curryMinApply);
    MinApply *node = getMinExp_Apply(exp);

    MinExprList *args = node->args;

    // If function is a builtin, don't restructure, just curry args
    if (node->isBuiltin) {
        MinExp *new_function = curryMinExp(node->function);
        int save = PROTECT(new_function);
        MinExprList *new_args = curryMinExprList(args);
        PROTECT(new_args);
        if (new_function != node->function || new_args != args) {
            MinExp *result =
                makeMinExp_Apply(CPI(node), new_function, new_args);
            UNPROTECT(save);
            LEAVE(curryMinApply);
            return result;
        }
        UNPROTECT(save);
        LEAVE(curryMinApply);
        return exp;
    }

    // 0 or 1 args: curry function and args
    if (args == NULL || args->next == NULL) {
        MinExp *new_function = curryMinExp(node->function);
        int save = PROTECT(new_function);
        MinExprList *new_args = curryMinExprList(args);
        PROTECT(new_args);
        if (new_function != node->function || new_args != args) {
            MinExp *result =
                makeMinExp_Apply(CPI(node), new_function, new_args);
            UNPROTECT(save);
            LEAVE(curryMinApply);
            return result;
        }
        UNPROTECT(save);
        LEAVE(curryMinApply);
        return exp;
    }

    // 2+ args: curry(apply(apply(e, [first_arg]), rest))
    MinExprList *firstArgList = newMinExprList(CPI(args), args->exp, NULL);
    int save = PROTECT(firstArgList);
    MinExp *innerApply =
        makeMinExp_Apply(CPI(node), node->function, firstArgList);
    PROTECT(innerApply);
    MinExp *outerApply = makeMinExp_Apply(CPI(node), innerApply, args->next);
    PROTECT(outerApply);
    MinExp *result = curryMinExp(outerApply);
    UNPROTECT(save);
    LEAVE(curryMinApply);
    return result;
}

static MinExprList *curryMinExprList(MinExprList *node) {
    ENTER(curryMinExprList);
    if (node == NULL) {
        LEAVE(curryMinExprList);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp = curryMinExp(node->exp);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = curryMinExprList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinExprList *result = newMinExprList(CPI(node), new_exp, new_next);
        UNPROTECT(save);
        LEAVE(curryMinExprList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinExprList);
    return node;
}

static MinPrimApp *curryMinPrimApp(MinPrimApp *node) {
    ENTER(curryMinPrimApp);
    if (node == NULL) {
        LEAVE(curryMinPrimApp);
        return NULL;
    }

    bool changed = false;
    MinExp *new_exp1 = curryMinExp(node->exp1);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = curryMinExp(node->exp2);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);

    if (changed) {
        MinPrimApp *result =
            newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
        UNPROTECT(save);
        LEAVE(curryMinPrimApp);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinPrimApp);
    return node;
}

static MinIff *curryMinIff(MinIff *node) {
    ENTER(curryMinIff);
    if (node == NULL) {
        LEAVE(curryMinIff);
        return NULL;
    }

    bool changed = false;
    MinExp *new_condition = curryMinExp(node->condition);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = curryMinExp(node->consequent);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = curryMinExp(node->alternative);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);

    if (changed) {
        MinIff *result = newMinIff(CPI(node), new_condition, new_consequent,
                                   new_alternative);
        UNPROTECT(save);
        LEAVE(curryMinIff);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinIff);
    return node;
}

static MinCond *curryMinCond(MinCond *node) {
    ENTER(curryMinCond);
    if (node == NULL) {
        LEAVE(curryMinCond);
        return NULL;
    }

    bool changed = false;
    MinExp *new_value = curryMinExp(node->value);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = curryMinCondCases(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    if (changed) {
        MinCond *result = newMinCond(CPI(node), new_value, new_cases);
        UNPROTECT(save);
        LEAVE(curryMinCond);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinCond);
    return node;
}

static MinIntCondCases *curryMinIntCondCases(MinIntCondCases *node) {
    ENTER(curryMinIntCondCases);
    if (node == NULL) {
        LEAVE(curryMinIntCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = curryMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = curryMinIntCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinIntCondCases *result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        LEAVE(curryMinIntCondCases);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinIntCondCases);
    return node;
}

static MinCharCondCases *curryMinCharCondCases(MinCharCondCases *node) {
    ENTER(curryMinCharCondCases);
    if (node == NULL) {
        LEAVE(curryMinCharCondCases);
        return NULL;
    }

    bool changed = false;
    MinExp *new_body = curryMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = curryMinCharCondCases(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinCharCondCases *result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
        UNPROTECT(save);
        LEAVE(curryMinCharCondCases);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinCharCondCases);
    return node;
}

static MinMatch *curryMinMatch(MinMatch *node) {
    ENTER(curryMinMatch);
    if (node == NULL) {
        LEAVE(curryMinMatch);
        return NULL;
    }

    bool changed = false;
    MinExp *new_index = curryMinExp(node->index);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = curryMinMatchList(node->cases);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);

    if (changed) {
        MinMatch *result = newMinMatch(CPI(node), new_index, new_cases);
        UNPROTECT(save);
        LEAVE(curryMinMatch);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinMatch);
    return node;
}

static MinMatchList *curryMinMatchList(MinMatchList *node) {
    ENTER(curryMinMatchList);
    if (node == NULL) {
        LEAVE(curryMinMatchList);
        return NULL;
    }

    bool changed = false;
    // matches is a MinIntList (just ints), no currying needed
    MinExp *new_body = curryMinExp(node->body);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = curryMinMatchList(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinMatchList *result =
            newMinMatchList(CPI(node), node->matches, new_body, new_next);
        UNPROTECT(save);
        LEAVE(curryMinMatchList);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinMatchList);
    return node;
}

static MinLetRec *curryMinLetRec(MinLetRec *node) {
    ENTER(curryMinLetRec);
    if (node == NULL) {
        LEAVE(curryMinLetRec);
        return NULL;
    }

    bool changed = false;
    MinBindings *new_bindings = curryMinBindings(node->bindings);
    int save = PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = curryMinExp(node->body);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);

    if (changed) {
        MinLetRec *result = newMinLetRec(CPI(node), new_bindings, new_body);
        UNPROTECT(save);
        LEAVE(curryMinLetRec);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinLetRec);
    return node;
}

static MinBindings *curryMinBindings(MinBindings *node) {
    ENTER(curryMinBindings);
    if (node == NULL) {
        LEAVE(curryMinBindings);
        return NULL;
    }

    bool changed = false;
    MinExp *new_val = curryMinExp(node->val);
    int save = PROTECT(new_val);
    changed = changed || (new_val != node->val);
    MinBindings *new_next = curryMinBindings(node->next);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);

    if (changed) {
        MinBindings *result =
            newMinBindings(CPI(node), node->var, new_val, new_next);
        UNPROTECT(save);
        LEAVE(curryMinBindings);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinBindings);
    return node;
}

static MinAmb *curryMinAmb(MinAmb *node) {
    ENTER(curryMinAmb);
    if (node == NULL) {
        LEAVE(curryMinAmb);
        return NULL;
    }

    bool changed = false;
    MinExp *new_left = curryMinExp(node->left);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = curryMinExp(node->right);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);

    if (changed) {
        MinAmb *result = newMinAmb(CPI(node), new_left, new_right);
        UNPROTECT(save);
        LEAVE(curryMinAmb);
        return result;
    }

    UNPROTECT(save);
    LEAVE(curryMinAmb);
    return node;
}

MinExp *curryMinExp(MinExp *node) {
    ENTER(curryMinExp);
    if (node == NULL) {
        LEAVE(curryMinExp);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinExp *result = node;

    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = curryMinAmb(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        result = curryMinApply(node);
        break;
    }
    case MINEXP_TYPE_ARGS: {
        MinExprList *variant = getMinExp_Args(node);
        MinExprList *new_variant = curryMinExprList(variant);
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
        MinBindings *new_variant = curryMinBindings(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Bindings(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = curryMinExp(variant);
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
        MinCond *new_variant = curryMinCond(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = curryMinIff(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        result = curryMinLam(node);
        break;
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = curryMinLetRec(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = curryMinExprList(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = curryMinMatch(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = curryMinPrimApp(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = curryMinExprList(variant);
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
    LEAVE(curryMinExp);
    return result;
}

static MinCondCases *curryMinCondCases(MinCondCases *node) {
    ENTER(curryMinCondCases);
    if (node == NULL) {
        LEAVE(curryMinCondCases);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinCondCases *result = node;

    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = curryMinIntCondCases(variant);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant = curryMinCharCondCases(variant);
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
    LEAVE(curryMinCondCases);
    return result;
}