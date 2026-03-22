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

#include "minlam_closureConvert.h"
#include "memory.h"
#include "minlam.h"
#include "minlam_freeVars.h"
#include "minlam_subst.h"
#include "minlam_transform.h"
#include "symbol.h"
#include "utils_helper.h"

#ifdef DEBUG_MINLAM_CLOSURECONVERT
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinExp *closureConvertMinLam(MinLam *node);
static MinExp *closureConvertMinApply(MinApply *node);
static MinExp *closureConvertMinExp(MinExp *node);

//////////////
// Public API
//////////////

MinExp *flatClosureConvert(MinExp *exp) {
    return bottomUpMinExp(closureConvertMinExp, exp);
}

MinExp *sharedClosureConvert(MinExp *exp) {
    return topDownMinExp(closureConvertMinExp, exp);
}

////////////////////
// Helper utilities
////////////////////

static MinExp *makeMinEnvRef(ParserInfo I, HashSymbol *senv, int i) {
    MinExp *index = newMinExp_Stdint(I, i);
    int save = PROTECT(index);
    MinExp *env = newMinExp_Var(I, senv);
    PROTECT(env);
    MinExp *result = makeMinExp_Prim(I, MINPRIMOP_TYPE_VEC, index, env);
    UNPROTECT(save);
    return result;
}

static void populateEnvReferences(MinExpTable *references, HashSymbol *env,
                                  SymbolList *list) {
    int i = 0;
    int save = PROTECT(references); // claim a stack slot
    while (list != NULL) {
        MinExp *ref = makeMinEnvRef(CPI(list), env, i++);
        REPLACE_PROTECT(save, ref);
        setMinExpTable(references, list->symbol, ref);
        list = list->next;
    }
    UNPROTECT(save);
}

static MinExprList *makeExprList(SymbolList *symbols) {
    if (symbols == NULL)
        return NULL;
    MinExprList *rest = makeExprList(symbols->next);
    int save = PROTECT(rest);
    MinExp *var = newMinExp_Var(CPI(symbols), symbols->symbol);
    PROTECT(var);
    MinExprList *this = newMinExprList(CPI(var), var, rest);
    UNPROTECT(save);
    return this;
}

static MinExp *makeEnv(ParserInfo PI, SymbolList *symbols) {
    MinExprList *list = makeExprList(symbols);
    int save = PROTECT(list);
    MinExp *env = newMinExp_MakeVec(PI, list);
    UNPROTECT(save);
    return env;
}

static MinExprList *makeClosure(MinExp *lambdac, MinExp *env) {
    MinExprList *b = newMinExprList(CPI(env), env, NULL);
    int save = PROTECT(b);
    MinExprList *a = newMinExprList(CPI(lambdac), lambdac, b);
    UNPROTECT(save);
    return a;
}

static SymbolSet *freeVarsMinLam(MinLam *node) {
    MinExp *lam = newMinExp_Lam(CPI(node), node);
    int save = PROTECT(lam);
    SymbolSet *fv = newSymbolSet();
    PROTECT(fv);
    freeVarsMinExp(lam, fv, NULL);
    UNPROTECT(save);
    return fv;
}

///////////////////////////
// Visitor implementations
///////////////////////////

//  (exp=M.lambda(params, body)) {
//      let
//          senv = GS.genstring("$env");
//          fv = FV.free(exp);
//          venv = fv |> fn (v) { #(v, M.var(v)) };
//          sub = DICT.make(fv, fv |> fn (v) { M.env_ref(M.var(senv), v) });
//          vbody = SUBST.substitute(sub, body);
//      in
//          M.make_closure(M.lambdac(senv @ params, vbody), M.make_env(venv))
//  }
static MinExp *closureConvertMinLam(MinLam *node) {
    ENTER(closureConvertMinLam);
    if (node->cc) {
        LEAVE(closureConvertMinLam);
        return newMinExp_Lam(CPI(node), node);
    }
    HashSymbol *envName = genSymDollar("env");
    // get the free variables in the body of the lambda
    SymbolSet *fv = freeVarsMinLam(node);
    int save = PROTECT(fv);
    // create an arbitrary fixed ordering for the free variables
    SymbolList *indexes = symbolSetToList(CPI(node), fv);
    PROTECT(indexes);
    // map from vars to index lookups
    MinExpTable *sub = newMinExpTable();
    PROTECT(sub);
    populateEnvReferences(sub, envName, indexes);
    // replace free variables in the body with vec refs into env
    MinExp *vbody = substMinExp(node->exp, sub);
    PROTECT(vbody);
    // construct the env from a vec
    MinExp *env = makeEnv(CPI(node), indexes);
    PROTECT(env);
    // prepend env onto the lambda args
    SymbolList *sparams = newSymbolList(CPI(node), envName, node->args);
    PROTECT(sparams);
    // build the new lambda
    MinExp *lambdac = makeMinExp_Lam(CPI(node), sparams, vbody);
    PROTECT(lambdac);
    // set the lambdac flag
    getMinExp_Lam(lambdac)->cc = true;
    // wrap the lambda and the env in a 2-vec closure
    MinExprList *closureArgs = makeClosure(lambdac, env);
    PROTECT(closureArgs);
    MinExp *result = newMinExp_MakeVec(CPI(closureArgs), closureArgs);
    UNPROTECT(save);
    LEAVE(closureConvertMinLam);
    return result;
}

// newMinApply(newMinPrimApp(MINPRIMOP_TYPE_VEC, 0, vec),
//             newMinExprList(newMinPrimApp(MINPRIMOP_TYPE_VEC, 1, vec),
//             args))
static MinExp *closureConvertMinApply(MinApply *node) {
    if (node == NULL)
        return NULL;
    ENTER(closureConvertMinApply);
    if (node->isBuiltin) {
        LEAVE(closureConvertMinApply);
        return newMinExp_Apply(CPI(node), node);
    }
    if (node->cc) {
        LEAVE(closureConvertMinApply);
        return newMinExp_Apply(CPI(node), node);
    }
    MinExp *result = makeMinExp_Apply(CPI(node), node->function, node->args);
    getMinExp_Apply(result)->cc = true;
    LEAVE(closureConvertMinApply);
    return result;
}

static MinExp *closureConvertMinExp(MinExp *node) {
    if (node == NULL)
        return NULL;
    ENTER(closureConvertMinExp);
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_APPLY:
        result = closureConvertMinApply(getMinExp_Apply(node));
        break;
    case MINEXP_TYPE_LAM:
        result = closureConvertMinLam(getMinExp_Lam(node));
        break;
    default:
        break;
    }
    LEAVE(closureConvertMinExp);
    return result;
}