/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2025  Bill Hails
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
 * ANF Normalization - Continuation-Passing Style Implementation
 * 
 * This is a rewrite of anf_normalize.c using continuation scaffolding
 * generated from tools/anf_continuations.yaml. The old implementation
 * remains untouched for reference.
 */

#include <stdio.h>
#include "common.h"
#include "lambda.h"
#include "anf_kont.h"
#include "symbol.h"

// Include generated continuation scaffolding (all static)
#include "anf_kont_impl.inc"

#ifdef DEBUG_ANF
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static LamExp *normalize(LamExp *exp, AnfKont *k);

static inline LamExp *INVOKE(AnfKont *k, LamExp *arg) {
    return k->wrapper(arg, k->env);  // Calls wrapper, which calls specific continuation with specific env
}

static LamExp *makeSingleLet(HashSymbol *y, LamExp *e, LamExp *body) {
    LamLetBindings *bindings = newLamLetBindings(CPI(e), y, e, NULL);
    int save = PROTECT(bindings);
    LamExp *res = makeLamExp_Let(CPI(e), bindings, body);
    UNPROTECT(save);
    return res;
}

static bool isValueExp(LamExp *exp) {
    switch (exp->type) {
        case LAMEXP_TYPE_BACK:
        case LAMEXP_TYPE_BIGINTEGER:
        case LAMEXP_TYPE_CHARACTER:
        case LAMEXP_TYPE_CONSTANT:
        case LAMEXP_TYPE_ERROR:
        case LAMEXP_TYPE_STDINT:
        case LAMEXP_TYPE_VAR:
            return true;
        default:
            return false;
    }
}

// (define (normalize-term e)
//     (normalize e (λ (x) x)))

static LamExp *normalize_term(LamExp *e) {
    AnfKont *termKont = makeKont_normalizeTerm(); // Identity continuation
    int save = PROTECT(termKont);
    LamExp *result = normalize(e, termKont);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeTermKont(LamExp *exp, NormalizeTermKontEnv *k __attribute__((unused))) {
    return exp;
}

// (define (normalize-name e k)
//  (normalize e (λ (x)
//    (if (value? x)
//        (k x)
//        (let ((y (gensym)))
//             `(let (,y ,x) ,(k y)))))))

static LamExp *normalize_name(LamExp *e, AnfKont *k) {
    AnfKont *nameKont = makeKont_normalizeName(k);
    int save = PROTECT(nameKont);
    LamExp *result = normalize(e, nameKont);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeNameKont(LamExp *exp, NormalizeNameKontEnv *env) {
    if (isValueExp(exp)) {
        return INVOKE(env->k, exp);
    } else {
        LamExp *y = newLamExp_Var(CPI(exp), genSymDollar(""));
        int save = PROTECT(y);
        LamExp *body = INVOKE(env->k, y);
        PROTECT(body);
        LamExp *letExp = makeSingleLet(y->val.var, exp, body);
        UNPROTECT(save);
        return letExp;
    }
}
// (define (normalize-names Ms k)
//     (if (null? Ms)
//         (k '())
//         (normalize-name (car Ms) [λ (t)
//             (normalize-names (cdr Ms) [λ (ts)
//                 (k `(,t . ,ts))])]))))

static LamExp *normalize_names(LamArgs *args, AnfKont *k) {
    if (args == NULL) {
        LamExp *nilExp = newLamExp_Args((struct ParserInfo) {.lineno = 0, .filename = ""},  NULL);
        int save = PROTECT(nilExp);
        LamExp *result = INVOKE(k, nilExp);
        UNPROTECT(save);
        return result;
    }
    AnfKont *namesKont = makeKont_normalizeNamesOuter(args->next, k);
    int save = PROTECT(namesKont);
    LamExp *result = normalize_name(args->exp, namesKont);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeNamesOuterKont(LamExp *t, NormalizeNamesOuterKontEnv *env) {
    AnfKont *k = makeKont_normalizeNamesInner(t, env->k);
    int save = PROTECT(k);
    LamExp *result = normalize_names(env->Ms->next, k);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeNamesInnerKont(LamExp *ts, NormalizeNamesInnerKontEnv *env) {
    LamArgs *tts = getLamExp_Args(ts);
    LamExp *newArgs = makeLamExp_Args(CPI(env->t), env->t, tts);
    int save = PROTECT(newArgs);
    LamExp *result = INVOKE(env->k, newArgs);
    UNPROTECT(save);
    return result;
}


//     (`(if ,e0 ,e1 ,e2)
//         (normalize-name e0
//             [λ (test)
//                 (k `(if ,test
//                         ,(normalize-term e1)
//                         ,(normalize-term e2)))]))

static LamExp *normalize_iff(LamExp *exp, AnfKont *k) {
    LamIff *iff = getLamExp_Iff(exp);
    AnfKont *iffKont = makeKont_normalizeIff(k, iff->consequent, iff->alternative);
    int save = PROTECT(iffKont);
    LamExp *result = normalize_name(iff->condition, iffKont);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeIffKont(LamExp *test, NormalizeIffKontEnv *env) {
    LamExp *consequent = normalize_term(env->e1);
    int save = PROTECT(consequent);
    LamExp *alternative = normalize_term(env->e2);
    PROTECT(alternative);
    LamExp *ifExp = makeLamExp_Iff(CPI(test), test, consequent, alternative);
    PROTECT(ifExp);
    LamExp *result = INVOKE(env->k, ifExp);
    UNPROTECT(save);
    return result;
}

// (`(,Fn . ,Ms)
//   (normalize-name Fn
//         [λ (t)
//            (normalize-names Ms
//                 [λ (ts) (k `(,t . ,ts))])]))

static LamExp *normalize_apply(LamExp *exp, AnfKont *k) {
    LamApply *apply = getLamExp_Apply(exp);
    AnfKont *k2 = makeKont_normalizeApplyOuter(apply->args, k);
    int save = PROTECT(k2);
    LamExp *result = normalize_name(apply->function, k2);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeApplyOuterKont(LamExp *t, NormalizeApplyOuterKontEnv *env) {
    AnfKont *k = makeKont_normalizeApplyInner(t, env->k);
    int save = PROTECT(k);
    LamExp *result = normalize_names(env->Ms, k);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeApplyInnerKont(LamExp *ts, NormalizeApplyInnerKontEnv *env) {
    LamArgs *tts = getLamExp_Args(ts);
    LamExp *newApply = makeLamExp_Apply(CPI(env->t), env->t, tts);
    int save = PROTECT(newApply);
    LamExp *result = INVOKE(env->k, newApply);
    UNPROTECT(save);
    return result;
}

static LamExp *normalize(LamExp *exp, AnfKont *k) {
    switch (exp->type) {
        case LAMEXP_TYPE_IFF:
            return normalize_iff(exp, k);
        case LAMEXP_TYPE_APPLY:
            return normalize_apply(exp, k);
        // Other cases to be implemented...
        default:
            cant_happen("normalize: unhandled LamExp type %s", lamExpTypeName(exp->type));
    }
}

/**
 * Public entry point.
 */
LamExp *anfNormalize2(LamExp *exp) {
    return normalize_term(exp);
}
