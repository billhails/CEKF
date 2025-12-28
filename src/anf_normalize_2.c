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
 * 
 * The rationale for this rewrite is that although mimicking continuations
 * in C is somewhat cumbersome, it allows the algorithm to be written
 * in a form that closely matches the original scheme specification. This
 * makes it much easier to verify, maintain and extend.
 * 
 * See the proposals in the docs folder: ANF-REWRITE.md and ANF-KONT.md
 */

#include <stdio.h>
#include "common.h"
#include "lambda.h"
#include "lambda_pp.h"
#include "anf_kont.h"
#include "symbol.h"

// Include generated continuation scaffolding (all static)
#include "anf_kont_impl.inc"

// #define DEBUG_ANF2

#ifdef DEBUG_ANF2
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static ParserInfo NULLPI = (ParserInfo) {.lineno = 0, .filename = ""};

static LamExp *normalize(LamExp *exp, AnfKont *k);

static inline LamExp *INVOKE(AnfKont *k, LamExp *arg) {
    return k->wrapper(arg, k->env);
}

static LamExp *makeSingleLet(HashSymbol *y, LamExp *e, LamExp *body) {
    LamBindings *bindings = newLamBindings(CPI(e), y, e, NULL);
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
        case LAMEXP_TYPE_CONSTRUCTOR:
        case LAMEXP_TYPE_ENV:
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
    ENTER(normalize_term);
    AnfKont *termKont = makeKont_normalizeTerm(); // Identity continuation
    int save = PROTECT(termKont);
    LamExp *result = normalize(e, termKont);
    UNPROTECT(save);
    LEAVE(normalize_term);
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
    ENTER(normalize_name);
    AnfKont *nameKont = makeKont_normalizeName(k);
    int save = PROTECT(nameKont);
    LamExp *result = normalize(e, nameKont);
    UNPROTECT(save);
    LEAVE(normalize_name);
    return result;
}

static LamExp *normalizeNameKont(LamExp *x, NormalizeNameKontEnv *env) {
    if (isValueExp(x)) {
        return INVOKE(env->k, x);
    } else {
        LamExp *y = newLamExp_Var(CPI(x), genSymDollar(""));
        int save = PROTECT(y);
        LamExp *body = INVOKE(env->k, y);
        PROTECT(body);
        LamExp *letExp = makeSingleLet(y->val.var, x, body);
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

static LamExp *normalize_names(LamArgs *Ms, AnfKont *k) {
    ENTER(normalize_names);
    if (Ms == NULL) {
        LamExp *nullArgs = newLamExp_Args(NULLPI,  NULL);
        int save = PROTECT(nullArgs);
        LamExp *result = INVOKE(k, nullArgs);
        UNPROTECT(save);
        LEAVE(normalize_names);
        return result;
    }
    AnfKont *k1 = makeKont_normalizeNamesOuter(Ms, k);
    int save = PROTECT(k1);
    LamExp *result = normalize_name(Ms->exp, k1);
    UNPROTECT(save);
    LEAVE(normalize_names);
    return result;
}

static LamExp *normalizeNamesOuterKont(LamExp *t, NormalizeNamesOuterKontEnv *env) {
    AnfKont *k2 = makeKont_normalizeNamesInner(t, env->k);
    int save = PROTECT(k2);
    LamExp *result = normalize_names(env->Ms->next, k2);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeNamesInnerKont(LamExp *ts, NormalizeNamesInnerKontEnv *env) {
    LamExp *newArgs = makeLamExp_Args(CPI(env->t), env->t, getLamExp_Args(ts));
    int save = PROTECT(newArgs);
    LamExp *result = INVOKE(env->k, newArgs);
    UNPROTECT(save);
    return result;
}

// (`(if ,e0 ,e1 ,e2)
//     (normalize-name e0
//         [λ (test)
//             (k `(if ,test
//                     ,(normalize-term e1)
//                     ,(normalize-term e2)))]))

static LamExp *normalize_Iff(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Iff);
    LamIff *iff = getLamExp_Iff(exp);
    AnfKont *iffKont = makeKont_normalizeIff(k, iff->consequent, iff->alternative);
    int save = PROTECT(iffKont);
    LamExp *result = normalize_name(iff->condition, iffKont);
    UNPROTECT(save);
    LEAVE(normalize_Iff);
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
//     [λ (t)
//        (normalize-names Ms
//             [λ (ts) (k `(,t . ,ts))])]))

static LamExp *normalize_Apply(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Apply);
    AnfKont *k1 = makeKont_normalizeApplyOuter(getLamExp_Apply(exp)->args, k);
    int save = PROTECT(k1);
    LamExp *result = normalize_name(getLamExp_Apply(exp)->function, k1);
    UNPROTECT(save);
    LEAVE(normalize_Apply);
    return result;
}

static LamExp *normalizeApplyOuterKont(LamExp *t, NormalizeApplyOuterKontEnv *env) {
    AnfKont *k2 = makeKont_normalizeApplyInner(t, env->k);
    int save = PROTECT(k2);
    LamExp *result = normalize_names(env->Ms, k2);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeApplyInnerKont(LamExp *ts, NormalizeApplyInnerKontEnv *env) {
    LamExp *newApply = makeLamExp_Apply(CPI(env->t), env->t, getLamExp_Args(ts));
    int save = PROTECT(newApply);
    LamExp *result = INVOKE(env->k, newApply);
    UNPROTECT(save);
    return result;
}

// (define (normalize-bindings bindings k)
//   (match bindings
//     ('()
//        (k '()))
//     (`((,x ,val) . ,next)
//         (normalize val
//             [λ (anfval)
//               (normalize-bindings next
//                 [λ (anfrest)
//                     (k `((,x ,anfval) . ,anfrest))])]))))

static LamExp *normalize_bindings(LamBindings *bindings, AnfKont *k) {
    ENTER(normalize_bindings);
    if (bindings == NULL) {
        LamExp *nilExp = newLamExp_Bindings(NULLPI, NULL);
        int save = PROTECT(nilExp);
        LamExp *result = INVOKE(k, nilExp);
        UNPROTECT(save);
        LEAVE(normalize_bindings);
        return result;
    }
    AnfKont *bindingsKont = makeKont_normalizeBindingsOuter(bindings->var, bindings->next, k);
    int save = PROTECT(bindingsKont);
    LamExp *result = normalize(bindings->val, bindingsKont);
    UNPROTECT(save);
    LEAVE(normalize_bindings);
    return result;
}

static LamExp *normalizeBindingsOuterKont(LamExp *anfVal, NormalizeBindingsOuterKontEnv *env) {
    AnfKont *k = makeKont_normalizeBindingsInner(env->x, anfVal, env->k);
    int save = PROTECT(k);
    LamExp *result = normalize_bindings(env->rest, k);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeBindingsInnerKont(LamExp *anfrest, NormalizeBindingsInnerKontEnv *env) {
    LamBindings *rest = getLamExp_Bindings(anfrest);
    LamExp *bindingsExp = makeLamExp_Bindings(CPI(env->anfVal), env->x, env->anfVal, rest);
    int save = PROTECT(bindingsExp);
    LamExp *result = INVOKE(env->k, bindingsExp);
    UNPROTECT(save);
    return result;
}

// (`(letrec ,bindings ,body)
//     (normalize-bindings bindings
//         [λ (anfbindings)
//             `(letrec ,anfbindings
//                      ,(normalize body k))])))

static LamExp *normalize_LetRec(LamExp *exp, AnfKont *k) {
    ENTER(normalize_LetRec);
    LamLetRec *letrec = getLamExp_LetRec(exp);
    AnfKont *k1 = makeKont_normalizeLetRec(letrec->body, k);
    int save = PROTECT(k1);
    LamExp *result = normalize_bindings(letrec->bindings, k1);
    UNPROTECT(save);
    LEAVE(normalize_LetRec);
    return result;
}

static LamExp *normalizeLetRecKont(LamExp *anfbindings, NormalizeLetRecKontEnv *env) {
    LamExp *body = normalize(env->body, env->k);
    int save = PROTECT(body);
    LamExp *result = makeLamExp_LetRec(CPI(env->body), getLamExp_Bindings(anfbindings), body);
    UNPROTECT(save);
    return result;
}

// (`(let ,bindings ,body)
//     (normalize-bindings bindings
//         [λ (anfbindings)
//             `(let ,anfbindings
//                   ,(normalize body k))])))

static LamExp *normalize_Let(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Let);
    AnfKont *k1 = makeKont_normalizeLet(getLamExp_Let(exp)->body, k);
    int save = PROTECT(k1);
    LamExp *result = normalize_bindings(getLamExp_Let(exp)->bindings, k1);
    UNPROTECT(save);
    LEAVE(normalize_Let);
    return result;
}

static LamExp *normalizeLetKont(LamExp *anfbindings, NormalizeLetKontEnv *env) {
    LamExp *body = normalize(env->body, env->k);
    int save = PROTECT(body);
    LamExp *result = makeLamExp_Let(CPI(env->body), getLamExp_Bindings(anfbindings), body);
    UNPROTECT(save);
    return result;
}

// (`(construct ,name ,tag . ,Ms)
//     (normalize-names Ms
//         [λ (ts) (k `(construct ,name ,tag . ,ts))]))

static LamExp *normalize_Construct(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Construct);
    LamConstruct *construct = getLamExp_Construct(exp);
    AnfKont *k1 = makeKont_normalizeConstruct(construct->name, construct->tag, k);
    int save = PROTECT(k1);
    LamExp *result = normalize_names(construct->args, k1);
    UNPROTECT(save);
    LEAVE(normalize_Construct);
    return result;
}

static LamExp *normalizeConstructKont(LamExp *ets, NormalizeConstructKontEnv *env) {
    LamArgs *ts = getLamExp_Args(ets);
    LamExp *newConstruct = makeLamExp_Construct(CPI(ets), env->name, env->tag, ts);
    int save = PROTECT(newConstruct);
    LamExp *result = INVOKE(env->k, newConstruct);
    UNPROTECT(save);
    return result;
}

// (`(make-tuple . ,Ms)
//     (normalize-names Ms
//         [λ (ts) (k `(make-tuple . ,ts))]))

static LamExp *normalize_MakeTuple(LamExp *exp, AnfKont *k) {
    ENTER(normalize_MakeTuple);
    LamArgs *lamArgs = getLamExp_MakeTuple(exp);
    AnfKont *k2 = makeKont_normalizeMakeTuple(k);
    int save = PROTECT(k2);
    LamExp *result = normalize_names(lamArgs, k2);
    UNPROTECT(save);
    LEAVE(normalize_MakeTuple);
    return result;
}

static LamExp *normalizeMakeTupleKont(LamExp *ts, NormalizeMakeTupleKontEnv *env) {
    LamArgs *tts = getLamExp_Args(ts);
    LamExp *newMakeTuple = newLamExp_MakeTuple(CPI(ts), tts);
    int save = PROTECT(newMakeTuple);
    LamExp *result = INVOKE(env->k, newMakeTuple);
    UNPROTECT(save);
    return result;
}

// (`(make-vec ,nargs . ,Ms)
//     (normalize-names Ms
//         [λ (ts) (k `(make-vec ,nargs . ,ts))]))

static LamExp *normalize_MakeVec(LamExp *exp, AnfKont *k) {
    ENTER(normalize_MakeVec);
    LamMakeVec *makeVec = getLamExp_MakeVec(exp);
    AnfKont *k2 = makeKont_normalizeMakeVec(makeVec->nargs, k);
    int save = PROTECT(k2);
    LamExp *result = normalize_names(makeVec->args, k2);
    UNPROTECT(save);
    LEAVE(normalize_MakeVec);
    return result;
}

static LamExp *normalizeMakeVecKont(LamExp *ts, NormalizeMakeVecKontEnv *env) {
    LamArgs *tts = getLamExp_Args(ts);
    LamExp *newMakeVec = makeLamExp_MakeVec(CPI(ts), env->nargs, tts);
    int save = PROTECT(newMakeVec);
    LamExp *result = INVOKE(env->k, newMakeVec);
    UNPROTECT(save);
    return result;
}

// (`(deconstruct ,name ,nsid ,vec ,e0)
//     (normalize-name e0
//         [λ (t) (k `(deconstruct ,name ,nsid ,vec ,t))]))

static LamExp *normalize_Deconstruct(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Deconstruct);
    LamDeconstruct *deconstruct = getLamExp_Deconstruct(exp);
    AnfKont *k2 = makeKont_normalizeDeconstruct(deconstruct->name, deconstruct->nsid, deconstruct->vec, k);
    int save = PROTECT(k2);
    LamExp *result = normalize_name(deconstruct->exp, k2);
    UNPROTECT(save);
    LEAVE(normalize_Deconstruct);
    return result;
}

static LamExp *normalizeDeconstructKont(LamExp *t, NormalizeDeconstructKontEnv *env) {
    LamExp *newDeconstruct = makeLamExp_Deconstruct(CPI(t), env->name, env->nsid, env->vec, t);
    int save = PROTECT(newDeconstruct);
    LamExp *result = INVOKE(env->k, newDeconstruct);
    UNPROTECT(save);
    return result;
}

// (`(cond ,e0 ,cases)
//     (normalize-name e0
//         [λ (t) (k `(cond ,t ,(normalize-cases cases)))]))

static LamExp *normalize_Cond(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Cond);
    LamCond *cond = getLamExp_Cond(exp);
    AnfKont *k1 = makeKont_normalizeCond(cond->cases, k);
    int save = PROTECT(k1);
    LamExp *result = normalize_name(cond->value, k1);
    UNPROTECT(save);
    LEAVE(normalize_Cond);
    return result;
}

static LamCharCondCases *normalize_char_cases(LamCharCondCases *cases) {
    if (cases == NULL) {
        return NULL;
    }
    LamCharCondCases *rest = normalize_char_cases(cases->next);
    int save = PROTECT(rest);
    LamExp *newExp = normalize_term(cases->body);
    PROTECT(newExp);
    LamCharCondCases *newCases = newLamCharCondCases(CPI(cases), cases->constant, newExp, rest);
    UNPROTECT(save);
    return newCases;
}

static LamIntCondCases *normalize_int_cases(LamIntCondCases *cases) {
    if (cases == NULL) {
        return NULL;
    }
    LamIntCondCases *rest = normalize_int_cases(cases->next);
    int save = PROTECT(rest);
    LamExp *newExp = normalize_term(cases->body);
    PROTECT(newExp);
    LamIntCondCases *newCases = newLamIntCondCases(CPI(cases), cases->constant, newExp, rest);
    UNPROTECT(save);
    return newCases;
}

static LamCondCases *normalize_cases(LamCondCases *cases) {
    if (cases == NULL) {
        return NULL;
    }
    switch (cases->type) {
        case LAMCONDCASES_TYPE_CHARACTERS: {
            LamCharCondCases *ccases = normalize_char_cases(getLamCondCases_Characters(cases));
            int save = PROTECT(ccases);
            LamCondCases *newCases = newLamCondCases_Characters(CPI(cases), ccases);
            UNPROTECT(save);
            return newCases;
        }
        case LAMCONDCASES_TYPE_INTEGERS: {
            LamIntCondCases *icases = normalize_int_cases(getLamCondCases_Integers(cases));
            int save = PROTECT(icases);
            LamCondCases *newCases = newLamCondCases_Integers(CPI(cases), icases);
            UNPROTECT(save);
            return newCases;
        }
        default:
            cant_happen("normalize_cases: unhandled LamCondCases type %s", lamCondCasesTypeName(cases->type));
    }
}

static LamExp *normalizeCondKont(LamExp *t, NormalizeCondKontEnv *env) {
    LamCondCases *cases = normalize_cases(env->cases);
    int save = PROTECT(cases);
    LamExp *newCond = makeLamExp_Cond(CPI(t), t, cases);
    PROTECT(newCond);
    LamExp *result = INVOKE(env->k, newCond);
    UNPROTECT(save);
    return result;
}

// (`(match-expr ,e0 ,cases)
//     (normalize-name e0
//         [λ (t)
//             (k `(match-expr ,t
//                             ,(normalize-match-cases cases)))])))

static LamExp *normalize_Match(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Match);
    LamMatch *match = getLamExp_Match(exp);
    AnfKont *k1 = makeKont_normalizeMatch(match->cases, k);
    int save = PROTECT(k1);
    LamExp *result = normalize_name(match->index, k1);
    UNPROTECT(save);
    LEAVE(normalize_Match);
    return result;
}

static LamMatchList *normalize_match_cases(LamMatchList *cases) {
    if (cases == NULL) {
        return NULL;
    }
    LamMatchList *rest = normalize_match_cases(cases->next);
    int save = PROTECT(rest);
    LamExp *newExp = normalize_term(cases->body);
    PROTECT(newExp);
    LamMatchList *newCases = newLamMatchList(CPI(cases), cases->matches, newExp, rest);
    UNPROTECT(save);
    return newCases;
}

static LamExp *normalizeMatchKont(LamExp *t, NormalizeMatchKontEnv *env) {
    LamMatchList *cases = normalize_match_cases(env->cases);
    int save = PROTECT(cases);
    LamExp *newMatch = makeLamExp_Match(CPI(t), t, cases);
    PROTECT(newMatch);
    LamExp *result = INVOKE(env->k, newMatch);
    UNPROTECT(save);
    return result;
}

// (`(primitive-apply ,type ,e1 ,e2)
//     (normalize-name e1
//         [λ (anfE1)
//             (normalize-name e2
//                 (λ (anfE2)
//                     (k `(primitive-apply ,type ,anfE1 ,anfE2))))]))

static LamExp *normalize_PrimApp(LamExp *exp, AnfKont *k) {
    ENTER(normalize_PrimApp);
    LamPrimApp *primApp = getLamExp_Prim(exp);
    AnfKont *k1 = makeKont_normalizePrimappOuter(primApp->type, primApp->exp2, k);
    int save = PROTECT(k1);
    LamExp *result = normalize_name(primApp->exp1, k1);
    UNPROTECT(save);
    LEAVE(normalize_PrimApp);
    return result;
}

static LamExp *normalizePrimappOuterKont(LamExp *anfE0, NormalizePrimappOuterKontEnv *env) {
    AnfKont *k2 = makeKont_normalizePrimappInner(env->type, anfE0, env->k);
    int save = PROTECT(k2);
    LamExp *result = normalize_name(env->e2, k2);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizePrimappInnerKont(LamExp *anfE2, NormalizePrimappInnerKontEnv *env) {
    LamExp *newPrimApp = makeLamExp_Prim(CPI(anfE2), env->type, env->anfE1, anfE2);
    int save = PROTECT(newPrimApp);
    LamExp *result = INVOKE(env->k, newPrimApp);
    UNPROTECT(save);
    return result;
}

// (`(print ,e0)
//     (normalize-name e0
//         [λ (anfE0) (k `(print ,anfE0))]))

static LamExp *normalize_Print(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Print);
    LamPrint *printExp = getLamExp_Print(exp);
    AnfKont *k1 = makeKont_normalizePrint(k);
    int save = PROTECT(k1);
    LamExp *result = normalize_name(printExp->exp, k1);
    UNPROTECT(save);
    LEAVE(normalize_Print);
    return result;
}

static LamExp *normalizePrintKont(LamExp *anfE0, NormalizePrintKontEnv *env) {
    LamExp *newPrint = makeLamExp_Print(CPI(anfE0), anfE0);
    int save = PROTECT(newPrint);
    LamExp *result = INVOKE(env->k, newPrint);
    UNPROTECT(save);
    return result;
}

// (`(typeOf ,e0)
//     (normalize-name e0
//         [λ (anfE0) (k `(typeOf ,anfE0))]))

static LamExp *normalize_TypeOf(LamExp *exp, AnfKont *k) {
    ENTER(normalize_TypeOf);
    LamTypeOf *typeOfExp = getLamExp_TypeOf(exp);
    AnfKont *k2 = makeKont_normalizeTypeOf(k);
    int save = PROTECT(k2);
    LamExp *result = normalize_name(typeOfExp->exp, k2);
    UNPROTECT(save);
    LEAVE(normalize_TypeOf);
    return result;
}

static LamExp *normalizeTypeOfKont(LamExp *anfE0, NormalizeTypeOfKontEnv *env) {
    LamExp *newTypeOf = makeLamExp_TypeOf(CPI(anfE0), anfE0);
    int save = PROTECT(newTypeOf);
    LamExp *result = INVOKE(env->k, newTypeOf);
    UNPROTECT(save);
    return result;
}

// (`(tuple-index ,vec ,size ,e0)
//     (normalize-name e0
//         [λ (t0) (k `(tuple-index ,vec ,size ,t0))]))

static LamExp *normalize_TupleIndex(LamExp *exp, AnfKont *k) {
    ENTER(normalize_TupleIndex);
    LamTupleIndex *tupleIndexExp = getLamExp_TupleIndex(exp);
    AnfKont *k2 = makeKont_normalizeTupleIndex(tupleIndexExp->vec, tupleIndexExp->size, k);
    int save = PROTECT(k2);
    LamExp *result = normalize_name(tupleIndexExp->exp, k2);
    UNPROTECT(save);
    LEAVE(normalize_TupleIndex);
    return result;
}

static LamExp *normalizeTupleIndexKont(LamExp *t0, NormalizeTupleIndexKontEnv *env) {
    LamExp *newTupleIndex = makeLamExp_TupleIndex(CPI(t0), env->vec, env->size, t0);
    int save = PROTECT(newTupleIndex);
    LamExp *result = INVOKE(env->k, newTupleIndex);
    UNPROTECT(save);
    return result;
}

// (`(tag ,e0)
//     (normalize-name e0
//         [λ (t0) (k `(tag ,t0))]))

static LamExp *normalize_Tag(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Tag);
    LamExp *tagExp = getLamExp_Tag(exp);
    AnfKont *k2 = makeKont_normalizeTag(k);
    int save = PROTECT(k2);
    LamExp *result = normalize_name(tagExp, k2);
    UNPROTECT(save);
    LEAVE(normalize_Tag);
    return result;
}

static LamExp *normalizeTagKont(LamExp *t0, NormalizeTagKontEnv *env) {
    LamExp *newTag = newLamExp_Tag(CPI(t0), t0);
    int save = PROTECT(newTag);
    LamExp *result = INVOKE(env->k, newTag);
    UNPROTECT(save);
    return result;
}

// (`(amb ,e0, e1)
//    (k `(amb ,(normalize-term e0)
//             ,(normalize-term e1))))

static LamExp *normalize_Amb(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Amb);
    LamAmb *ambExp = getLamExp_Amb(exp);
    LamExp *e0 = normalize_term(ambExp->left);
    int save = PROTECT(e0);
    LamExp *e1 = normalize_term(ambExp->right);
    PROTECT(e1);
    LamExp *newAmb = makeLamExp_Amb(CPI(exp), e0, e1);
    PROTECT(newAmb);
    LamExp *result = INVOKE(k, newAmb);
    UNPROTECT(save);
    LEAVE(normalize_Amb);
    return result;
}

// (`(λ ,params ,body)
//   (k `(λ ,params ,(normalize-term body))))

static LamExp *normalize_Lam(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Lam);
    LamLam *lamExp = getLamExp_Lam(exp);
    LamExp *newBody = normalize_term(lamExp->exp);
    int save = PROTECT(newBody);
    LamExp *newLam = makeLamExp_Lam(CPI(exp), lamExp->args, newBody);
    PROTECT(newLam);
    getLamExp_Lam(newLam)->isMacro = lamExp->isMacro;
    LamExp *result = INVOKE(k, newLam);
    UNPROTECT(save);
    LEAVE(normalize_Lam);
    return result;
}

// (`(lookup ,name ,id ,expr)
//     (k `(lookup ,name ,id ,(normalize-term expr))))

static LamExp *normalize_Lookup(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Lookup);
    LamLookup *lookupExp = getLamExp_Lookup(exp);
    LamExp *newExpr = normalize_term(lookupExp->exp);
    int save = PROTECT(newExpr);
    LamExp *newLookup = makeLamExp_Lookup(CPI(exp), lookupExp->nsid, lookupExp->nsSymbol, newExpr);
    PROTECT(newLookup);
    LamExp *result = INVOKE(k, newLookup);
    UNPROTECT(save);
    LEAVE(normalize_Lookup);
    return result;
}

// (`(nameSpaces . ,Ms)
//     (k `(nameSpaces . ,(normalize-terms Ms))))

static LamNameSpaceArray *normalize_array(LamNameSpaceArray *nsArray) {
    LamNameSpaceArray *newNsArray = newLamNameSpaceArray();
    int save = PROTECT(newNsArray);
    for (Index i = 0; i < nsArray->size; i++) {
        LamExp *normNs = normalize_term(nsArray->entries[i]);
        int save = PROTECT(normNs);
        pushLamNameSpaceArray(newNsArray, normNs);
        UNPROTECT(save);
    }
    UNPROTECT(save);
    return newNsArray;
}

static LamExp *normalize_NameSpaces(LamExp *exp, AnfKont *k) {
    ENTER(normalize_NameSpaces);
    LamNameSpaceArray *nsExp = normalize_array(getLamExp_NameSpaces(exp));
    int save = PROTECT(nsExp);
    LamExp *newNsExp = newLamExp_NameSpaces(CPI(exp), nsExp);
    PROTECT(newNsExp);
    LamExp *result = INVOKE(k, newNsExp);;
    UNPROTECT(save);
    LEAVE(normalize_NameSpaces);
    return result;
}

// (`(callcc ,e0)
//     (normalize-name e0
//         [λ (t0) (k `(callcc ,t0))]))

static LamExp *normalize_CallCC(LamExp *exp, AnfKont *k) {
    ENTER(normalize_CallCC);
    LamExp *e0 = getLamExp_CallCC(exp);
    AnfKont *k2 = makeKont_normalizeCallCC(k);
    int save = PROTECT(k2);
    LamExp *result = normalize_name(e0, k2);
    UNPROTECT(save);
    LEAVE(normalize_CallCC);
    return result;
}

static LamExp *normalizeCallCCKont(LamExp *t0, NormalizeCallCCKontEnv *env) {
    LamExp *newCallCC = newLamExp_CallCC(CPI(t0), t0);
    int save = PROTECT(newCallCC);
    LamExp *result = INVOKE(env->k, newCallCC);
    UNPROTECT(save);
    return result;
}

// (`(sequence . ,Ms)
//     (k `(sequence . ,(normalize-terms Ms))))

static LamSequence *_normalizeSquence(LamSequence *seq);

static LamExp *normalize_Sequence(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Sequence);
    LamSequence *normSeq = _normalizeSquence(getLamExp_Sequence(exp));
    int save = PROTECT(normSeq);
    LamExp *newSeqExp = newLamExp_Sequence(CPI(exp), normSeq);
    PROTECT(newSeqExp);
    LamExp *result = INVOKE(k, newSeqExp);
    UNPROTECT(save);
    LEAVE(normalize_Sequence);
    return result;
}

static LamSequence *_normalizeSquence(LamSequence *seq) {
    if (seq == NULL) return NULL;
    LamSequence *rest = _normalizeSquence(seq->next);
    int save = PROTECT(rest);
    LamExp *newExp = normalize_term(seq->exp);
    PROTECT(newExp);
    LamSequence *newSeq = newLamSequence(CPI(seq), newExp, rest);
    UNPROTECT(save);
    return newSeq;
}

// (`(typedefs ,defs ,body)
//   (k `(typedefs ,defs ,(normalize-term body))))

static LamExp *normalize_Typedefs(LamExp *exp, AnfKont *k) {
    ENTER(normalize_Typedefs);
    LamTypeDefs *typedefsExp = getLamExp_Typedefs(exp);
    LamExp *newBody = normalize_term(typedefsExp->body);
    int save = PROTECT(newBody);
    LamExp *newTypedefs = makeLamExp_Typedefs(CPI(exp), typedefsExp->typeDefs, newBody);
    PROTECT(newTypedefs);
    LamExp *result = INVOKE(k, newTypedefs);
    UNPROTECT(save);
    LEAVE(normalize_Typedefs);
    return result;
}

// (define (normalize e k)
//    (match e
//       ...))

static LamExp *normalize(LamExp *exp, AnfKont *k) {
    ENTER(normalize);
    LamExp *res = NULL;
    if (exp == NULL) {
        res = NULL;
    } else if (isValueExp(exp)) {
        res = INVOKE(k, exp);
    } else {
        switch (exp->type) {
            case LAMEXP_TYPE_AMB:
                res = normalize_Amb(exp, k);
                break;
            case LAMEXP_TYPE_APPLY:
                res = normalize_Apply(exp, k);
                break;
            case LAMEXP_TYPE_CALLCC:
                res = normalize_CallCC(exp, k);
                break;
            case LAMEXP_TYPE_COND:
                res = normalize_Cond(exp, k);
                break;
            case LAMEXP_TYPE_CONSTRUCT:
                res = normalize_Construct(exp, k);
                break;
            case LAMEXP_TYPE_DECONSTRUCT:
                res = normalize_Deconstruct(exp, k);
                break;
            case LAMEXP_TYPE_IFF:
                res = normalize_Iff(exp, k);
                break;
            case LAMEXP_TYPE_LAM:
                res = normalize_Lam(exp, k);
                break;
            case LAMEXP_TYPE_LETREC:
                res = normalize_LetRec(exp, k);
                break;
            case LAMEXP_TYPE_LET:
                res = normalize_Let(exp, k);
                break;
            case LAMEXP_TYPE_LOOKUP:
                res = normalize_Lookup(exp, k);
                break;
            case LAMEXP_TYPE_MAKETUPLE:
                res = normalize_MakeTuple(exp, k);
                break;
            case LAMEXP_TYPE_MAKEVEC:
                res = normalize_MakeVec(exp, k);
                break;
            case LAMEXP_TYPE_MATCH:
                res = normalize_Match(exp, k);
                break;
            case LAMEXP_TYPE_NAMESPACES:
                res = normalize_NameSpaces(exp, k);
                break;
            case LAMEXP_TYPE_PRIM:
                res = normalize_PrimApp(exp, k);
                break;
            case LAMEXP_TYPE_PRINT:
                res = normalize_Print(exp, k);
                break;
            case LAMEXP_TYPE_SEQUENCE:
                res = normalize_Sequence(exp, k);
                break;
            case LAMEXP_TYPE_TAG:
                res = normalize_Tag(exp, k);
                break;
            case LAMEXP_TYPE_TUPLEINDEX:
                res = normalize_TupleIndex(exp, k);
                break;
            case LAMEXP_TYPE_TYPEDEFS:
                res = normalize_Typedefs(exp, k);
                break;
            case LAMEXP_TYPE_TYPEOF:
                res = normalize_TypeOf(exp, k);
                break;
            default:
                cant_happen("normalize: unhandled LamExp type %s", lamExpTypeName(exp->type));
        }
    }
    LEAVE(normalize);
    return res;
}

/**
 * Public entry point.
 */
LamExp *anfNormalize2(LamExp *exp) {
    return normalize_term(exp);
}
