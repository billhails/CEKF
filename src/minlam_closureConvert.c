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

static MinExprList *makeMakeVec(SymbolList *symbols) {
    if (symbols == NULL)
        return NULL;
    MinExprList *rest = makeMakeVec(symbols->next);
    int save = PROTECT(rest);
    MinExp *var = newMinExp_Var(CPI(symbols), symbols->symbol);
    PROTECT(var);
    MinExprList *this = newMinExprList(CPI(var), var, rest);
    UNPROTECT(save);
    return this;
}

static MinExprList *make2Expr(MinExp *first, MinExp *second) {
    MinExprList *b = newMinExprList(CPI(second), second, NULL);
    int save = PROTECT(b);
    MinExprList *a = newMinExprList(CPI(first), first, b);
    UNPROTECT(save);
    return a;
}

///////////////////////////
// Visitor implementations
///////////////////////////

static MinExp *closureConvertMinLam(MinLam *node) {
    ENTER(closureConvertMinLam);
    //  (exp=M.lambda(params, body)) {
    //      let
    //          senv = GS.genstring("$env");
    //          sparams = senv @ params;
    //          fv = FV.free(exp);
    //          venv = fv |> fn (v) { #(v, M.var(v)) };
    //          sub = DICT.make(fv, fv |> fn (v) { M.env_ref(M.var(senv), v) });
    //          vbody = SUBST.substitute(sub, body);
    //      in
    //          M.make_closure(M.lambdac(sparams, vbody), M.make_env(venv))
    //  }

    //          senv = GS.genstring("$env");
    HashSymbol *senv = genSymDollar("env");
    //          sparams = senv @ params;
    SymbolList *new_args = newSymbolList(CPI(node), senv, node->args);
    int save = PROTECT(new_args);
    //          fv = FV.free(exp);
    SymbolSet *fv = newSymbolSet();
    PROTECT(fv);
    MinExp *lam = newMinExp_Lam(CPI(node), node);
    PROTECT(lam);
    freeVarsMinExp(lam, fv, NULL);
    //          venv = fv |> fn (v) { #(v, M.var(v)) };
    SymbolList *indexes = symbolSetToList(CPI(node), fv);
    PROTECT(indexes);
    //          sub = DICT.make(fv, fv |> fn (v) { M.env_ref(M.var(senv), v) });
    MinExpTable *sub = newMinExpTable();
    PROTECT(sub);
    populateEnvReferences(sub, senv, indexes);
    //          vbody = SUBST.substitute(sub, body);
    MinExp *vbody = substMinExp(node->exp, sub);
    PROTECT(vbody);
    //          M.make_closure(M.lambdac(sparams, vbody), M.make_env(venv))
    MinExprList *elements = makeMakeVec(indexes);
    PROTECT(elements);
    MinExp *makeVec = newMinExp_MakeVec(CPI(node), elements);
    PROTECT(makeVec);
    MinExp *lambdac = makeMinExp_Lam(CPI(node), new_args, vbody);
    PROTECT(lambdac);
    MinExprList *closureArgs = make2Expr(lambdac, makeVec);
    PROTECT(closureArgs);
    MinExp *result = newMinExp_MakeVec(CPI(closureArgs), closureArgs);
    UNPROTECT(save);
    LEAVE(closureConvertMinLam);
    return result;
}

static MinExp *closureConvertMinApply(MinApply *node) {
    if (node == NULL)
        return NULL;
    ENTER(closureConvertMinApply);
    if (node->isBuiltin) {
        LEAVE(closureConvertMinApply);
        return newMinExp_Apply(CPI(node), node);
    }
    // newMinApply(newMinPrimApp(MINPRIMOP_TYPE_VEC, 0, vec),
    //             newMinExprList(newMinPrimApp(MINPRIMOP_TYPE_VEC, 1, vec),
    //             args))
    MinExp *vec = node->function;
    MinExprList *args = node->args;
    MinExp *zero = newMinExp_Stdint(CPI(node), 0);
    int save = PROTECT(zero);
    MinExp *function =
        makeMinExp_Prim(CPI(node), MINPRIMOP_TYPE_VEC, zero, vec);
    PROTECT(function);
    MinExp *one = newMinExp_Stdint(CPI(node), 1);
    PROTECT(one);
    MinExp *vec_ref = makeMinExp_Prim(CPI(node), MINPRIMOP_TYPE_VEC, one, vec);
    PROTECT(vec_ref);
    MinExprList *new_args = newMinExprList(CPI(node), vec_ref, args);
    PROTECT(new_args);
    MinExp *result = makeMinExp_Apply(CPI(node), function, new_args);
    UNPROTECT(save);
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