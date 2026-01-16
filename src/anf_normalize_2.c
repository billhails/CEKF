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

#include "anf_kont.h"
#include "common.h"
#include "minlam.h"
#include "minlam_pp.h"
#include "symbol.h"
#include <stdio.h>

// Include generated continuation scaffolding (all static)
#include "anf_kont_impl.inc"

// #define DEBUG_ANF2

#ifdef DEBUG_ANF2
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static ParserInfo NULLPI = (ParserInfo){.lineNo = 0, .fileName = ""};

static MinExp *normalize(MinExp *exp, AnfKont *k);

static inline MinExp *INVOKE(AnfKont *k, MinExp *arg) {
    return k->wrapper(arg, k->env);
}

static MinExp *makeSingleLambda(HashSymbol *y, MinExp *e, MinExp *body) {
    MinVarList *fargs = newMinVarList(CPI(e), y, NULL);
    int save = PROTECT(fargs);
    MinExp *lam = makeMinExp_Lam(CPI(body), fargs, body);
    PROTECT(lam);
    MinArgs *aargs = newMinArgs(CPI(e), e, NULL);
    PROTECT(aargs);
    MinExp *res = makeMinExp_Apply(CPI(e), lam, aargs);
    UNPROTECT(save);
    return res;
}

static bool isValueExp(MinExp *exp) {
    switch (exp->type) {
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_CONSTANT:
    case MINEXP_TYPE_CONSTRUCTOR:
    case MINEXP_TYPE_ENV:
    case MINEXP_TYPE_ERROR:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_VAR:
        return true;
    default:
        return false;
    }
}

// (define (normalize-term e)
//     (normalize e (λ (x) x)))

static MinExp *normalize_term(MinExp *e) {
    ENTER(normalize_term);
    AnfKont *termKont = makeKont_normalizeTerm(); // Identity continuation
    int save = PROTECT(termKont);
    MinExp *result = normalize(e, termKont);
    UNPROTECT(save);
    LEAVE(normalize_term);
    return result;
}

static MinExp *normalizeTermKont(MinExp *exp, NormalizeTermKontEnv *k
                                 __attribute__((unused))) {
    return exp;
}

// (define (normalize-name e k)
//  (normalize e (λ (x)
//    (if (value? x)
//        (k x)
//        (let ((y (gensym)))
//             `(let (,y ,x) ,(k y)))))))

static MinExp *normalize_name(MinExp *e, AnfKont *k) {
    ENTER(normalize_name);
    AnfKont *nameKont = makeKont_normalizeName(k);
    int save = PROTECT(nameKont);
    MinExp *result = normalize(e, nameKont);
    UNPROTECT(save);
    LEAVE(normalize_name);
    return result;
}

static MinExp *normalizeNameKont(MinExp *x, NormalizeNameKontEnv *env) {
    if (isValueExp(x)) {
        return INVOKE(env->k, x);
    } else {
        MinExp *y = newMinExp_Var(CPI(x), genSymDollar(""));
        int save = PROTECT(y);
        MinExp *body = INVOKE(env->k, y);
        PROTECT(body);
        MinExp *letExp = makeSingleLambda(y->val.var, x, body);
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

static MinExp *normalize_names(MinArgs *Ms, AnfKont *k) {
    ENTER(normalize_names);
    if (Ms == NULL) {
        MinExp *nullArgs = newMinExp_Args(NULLPI, NULL);
        int save = PROTECT(nullArgs);
        MinExp *result = INVOKE(k, nullArgs);
        UNPROTECT(save);
        LEAVE(normalize_names);
        return result;
    }
    AnfKont *k1 = makeKont_normalizeNamesOuter(Ms, k);
    int save = PROTECT(k1);
    MinExp *result = normalize_name(Ms->exp, k1);
    UNPROTECT(save);
    LEAVE(normalize_names);
    return result;
}

static MinExp *normalizeNamesOuterKont(MinExp *t,
                                       NormalizeNamesOuterKontEnv *env) {
    AnfKont *k2 = makeKont_normalizeNamesInner(t, env->k);
    int save = PROTECT(k2);
    MinExp *result = normalize_names(env->Ms->next, k2);
    UNPROTECT(save);
    return result;
}

static MinExp *normalizeNamesInnerKont(MinExp *ts,
                                       NormalizeNamesInnerKontEnv *env) {
    MinExp *newArgs = makeMinExp_Args(CPI(env->t), env->t, getMinExp_Args(ts));
    int save = PROTECT(newArgs);
    MinExp *result = INVOKE(env->k, newArgs);
    UNPROTECT(save);
    return result;
}

// (`(if ,e0 ,e1 ,e2)
//     (normalize-name e0
//         [λ (test)
//             (k `(if ,test
//                     ,(normalize-term e1)
//                     ,(normalize-term e2)))]))

static MinExp *normalize_Iff(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Iff);
    MinIff *iff = getMinExp_Iff(exp);
    AnfKont *iffKont =
        makeKont_normalizeIff(k, iff->consequent, iff->alternative);
    int save = PROTECT(iffKont);
    MinExp *result = normalize_name(iff->condition, iffKont);
    UNPROTECT(save);
    LEAVE(normalize_Iff);
    return result;
}

static MinExp *normalizeIffKont(MinExp *test, NormalizeIffKontEnv *env) {
    MinExp *consequent = normalize_term(env->e1);
    int save = PROTECT(consequent);
    MinExp *alternative = normalize_term(env->e2);
    PROTECT(alternative);
    MinExp *ifExp = makeMinExp_Iff(CPI(test), test, consequent, alternative);
    PROTECT(ifExp);
    MinExp *result = INVOKE(env->k, ifExp);
    UNPROTECT(save);
    return result;
}

// (`(,Fn . ,Ms)
//   (normalize-name Fn
//     [λ (t)
//        (normalize-names Ms
//             [λ (ts) (k `(,t . ,ts))])]))

static MinExp *normalize_Apply(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Apply);
    AnfKont *k1 = makeKont_normalizeApplyOuter(getMinExp_Apply(exp)->args, k);
    int save = PROTECT(k1);
    MinExp *result = normalize_name(getMinExp_Apply(exp)->function, k1);
    UNPROTECT(save);
    LEAVE(normalize_Apply);
    return result;
}

static MinExp *normalizeApplyOuterKont(MinExp *t,
                                       NormalizeApplyOuterKontEnv *env) {
    AnfKont *k2 = makeKont_normalizeApplyInner(t, env->k);
    int save = PROTECT(k2);
    MinExp *result = normalize_names(env->Ms, k2);
    UNPROTECT(save);
    return result;
}

static MinExp *normalizeApplyInnerKont(MinExp *ts,
                                       NormalizeApplyInnerKontEnv *env) {
    MinExp *newApply =
        makeMinExp_Apply(CPI(env->t), env->t, getMinExp_Args(ts));
    int save = PROTECT(newApply);
    MinExp *result = INVOKE(env->k, newApply);
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

static MinExp *normalize_bindings(MinBindings *bindings, AnfKont *k) {
    ENTER(normalize_bindings);
    if (bindings == NULL) {
        MinExp *nilExp = newMinExp_Bindings(NULLPI, NULL);
        int save = PROTECT(nilExp);
        MinExp *result = INVOKE(k, nilExp);
        UNPROTECT(save);
        LEAVE(normalize_bindings);
        return result;
    }
    AnfKont *bindingsKont =
        makeKont_normalizeBindingsOuter(bindings->var, bindings->next, k);
    int save = PROTECT(bindingsKont);
    MinExp *result = normalize(bindings->val, bindingsKont);
    UNPROTECT(save);
    LEAVE(normalize_bindings);
    return result;
}

static MinExp *normalizeBindingsOuterKont(MinExp *anfVal,
                                          NormalizeBindingsOuterKontEnv *env) {
    AnfKont *k = makeKont_normalizeBindingsInner(env->x, anfVal, env->k);
    int save = PROTECT(k);
    MinExp *result = normalize_bindings(env->rest, k);
    UNPROTECT(save);
    return result;
}

static MinExp *normalizeBindingsInnerKont(MinExp *anfrest,
                                          NormalizeBindingsInnerKontEnv *env) {
    MinBindings *rest = getMinExp_Bindings(anfrest);
    MinExp *bindingsExp =
        makeMinExp_Bindings(CPI(env->anfVal), env->x, env->anfVal, rest);
    int save = PROTECT(bindingsExp);
    MinExp *result = INVOKE(env->k, bindingsExp);
    UNPROTECT(save);
    return result;
}

// (`(letrec ,bindings ,body)
//     (normalize-bindings bindings
//         [λ (anfbindings)
//             `(letrec ,anfbindings
//                      ,(normalize body k))])))

static MinExp *normalize_LetRec(MinExp *exp, AnfKont *k) {
    ENTER(normalize_LetRec);
    MinLetRec *letrec = getMinExp_LetRec(exp);
    AnfKont *k1 = makeKont_normalizeLetRec(letrec->body, k);
    int save = PROTECT(k1);
    MinExp *result = normalize_bindings(letrec->bindings, k1);
    UNPROTECT(save);
    LEAVE(normalize_LetRec);
    return result;
}

static MinExp *normalizeLetRecKont(MinExp *anfbindings,
                                   NormalizeLetRecKontEnv *env) {
    MinExp *body = normalize(env->body, env->k);
    int save = PROTECT(body);
    MinExp *result = makeMinExp_LetRec(CPI(env->body),
                                       getMinExp_Bindings(anfbindings), body);
    UNPROTECT(save);
    return result;
}

// (`(make-tuple . ,Ms)
//     (normalize-names Ms
//         [λ (ts) (k `(make-tuple . ,ts))]))

static MinExp *normalize_MakeTuple(MinExp *exp, AnfKont *k) {
    ENTER(normalize_MakeTuple);
    MinArgs *minArgs = getMinExp_MakeTuple(exp);
    AnfKont *k2 = makeKont_normalizeMakeTuple(k);
    int save = PROTECT(k2);
    MinExp *result = normalize_names(minArgs, k2);
    UNPROTECT(save);
    LEAVE(normalize_MakeTuple);
    return result;
}

static MinExp *normalizeMakeTupleKont(MinExp *ts,
                                      NormalizeMakeTupleKontEnv *env) {
    MinArgs *tts = getMinExp_Args(ts);
    MinExp *newMakeTuple = newMinExp_MakeTuple(CPI(ts), tts);
    int save = PROTECT(newMakeTuple);
    MinExp *result = INVOKE(env->k, newMakeTuple);
    UNPROTECT(save);
    return result;
}

// (`(make-vec ,nArgs . ,Ms)
//     (normalize-names Ms
//         [λ (ts) (k `(make-vec ,nArgs . ,ts))]))

static MinExp *normalize_MakeVec(MinExp *exp, AnfKont *k) {
    ENTER(normalize_MakeVec);
    MinMakeVec *makeVec = getMinExp_MakeVec(exp);
    AnfKont *k2 = makeKont_normalizeMakeVec(makeVec->nArgs, k);
    int save = PROTECT(k2);
    MinExp *result = normalize_names(makeVec->args, k2);
    UNPROTECT(save);
    LEAVE(normalize_MakeVec);
    return result;
}

static MinExp *normalizeMakeVecKont(MinExp *ts, NormalizeMakeVecKontEnv *env) {
    MinArgs *tts = getMinExp_Args(ts);
    MinExp *newMakeVec = makeMinExp_MakeVec(CPI(ts), env->nArgs, tts);
    int save = PROTECT(newMakeVec);
    MinExp *result = INVOKE(env->k, newMakeVec);
    UNPROTECT(save);
    return result;
}

// (`(deconstruct ,name ,nsId ,vec ,e0)
//     (normalize-name e0
//         [λ (t) (k `(deconstruct ,name ,nsId ,vec ,t))]))

static MinExp *normalize_Deconstruct(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Deconstruct);
    MinDeconstruct *deconstruct = getMinExp_Deconstruct(exp);
    AnfKont *k2 = makeKont_normalizeDeconstruct(
        deconstruct->name, deconstruct->nsId, deconstruct->vec, k);
    int save = PROTECT(k2);
    MinExp *result = normalize_name(deconstruct->exp, k2);
    UNPROTECT(save);
    LEAVE(normalize_Deconstruct);
    return result;
}

static MinExp *normalizeDeconstructKont(MinExp *t,
                                        NormalizeDeconstructKontEnv *env) {
    MinExp *newDeconstruct =
        makeMinExp_Deconstruct(CPI(t), env->name, env->nsId, env->vec, t);
    int save = PROTECT(newDeconstruct);
    MinExp *result = INVOKE(env->k, newDeconstruct);
    UNPROTECT(save);
    return result;
}

// (`(cond ,e0 ,cases)
//     (normalize-name e0
//         [λ (t) (k `(cond ,t ,(normalize-cases cases)))]))

static MinExp *normalize_Cond(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Cond);
    MinCond *cond = getMinExp_Cond(exp);
    AnfKont *k1 = makeKont_normalizeCond(cond->cases, k);
    int save = PROTECT(k1);
    MinExp *result = normalize_name(cond->value, k1);
    UNPROTECT(save);
    LEAVE(normalize_Cond);
    return result;
}

static MinCharCondCases *normalize_char_cases(MinCharCondCases *cases) {
    if (cases == NULL) {
        return NULL;
    }
    MinCharCondCases *rest = normalize_char_cases(cases->next);
    int save = PROTECT(rest);
    MinExp *newExp = normalize_term(cases->body);
    PROTECT(newExp);
    MinCharCondCases *newCases =
        newMinCharCondCases(CPI(cases), cases->constant, newExp, rest);
    UNPROTECT(save);
    return newCases;
}

static MinIntCondCases *normalize_int_cases(MinIntCondCases *cases) {
    if (cases == NULL) {
        return NULL;
    }
    MinIntCondCases *rest = normalize_int_cases(cases->next);
    int save = PROTECT(rest);
    MinExp *newExp = normalize_term(cases->body);
    PROTECT(newExp);
    MinIntCondCases *newCases =
        newMinIntCondCases(CPI(cases), cases->constant, newExp, rest);
    UNPROTECT(save);
    return newCases;
}

static MinCondCases *normalize_cases(MinCondCases *cases) {
    if (cases == NULL) {
        return NULL;
    }
    switch (cases->type) {
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *ccases =
            normalize_char_cases(getMinCondCases_Characters(cases));
        int save = PROTECT(ccases);
        MinCondCases *newCases = newMinCondCases_Characters(CPI(cases), ccases);
        UNPROTECT(save);
        return newCases;
    }
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *icases =
            normalize_int_cases(getMinCondCases_Integers(cases));
        int save = PROTECT(icases);
        MinCondCases *newCases = newMinCondCases_Integers(CPI(cases), icases);
        UNPROTECT(save);
        return newCases;
    }
    default:
        cant_happen("normalize_cases: unhandled MinCondCases type %s",
                    minCondCasesTypeName(cases->type));
    }
}

static MinExp *normalizeCondKont(MinExp *t, NormalizeCondKontEnv *env) {
    MinCondCases *cases = normalize_cases(env->cases);
    int save = PROTECT(cases);
    MinExp *newCond = makeMinExp_Cond(CPI(t), t, cases);
    PROTECT(newCond);
    MinExp *result = INVOKE(env->k, newCond);
    UNPROTECT(save);
    return result;
}

// (`(match-expr ,e0 ,cases)
//     (normalize-name e0
//         [λ (t)
//             (k `(match-expr ,t
//                             ,(normalize-match-cases cases)))])))

static MinExp *normalize_Match(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Match);
    MinMatch *match = getMinExp_Match(exp);
    AnfKont *k1 = makeKont_normalizeMatch(match->cases, k);
    int save = PROTECT(k1);
    MinExp *result = normalize_name(match->index, k1);
    UNPROTECT(save);
    LEAVE(normalize_Match);
    return result;
}

static MinMatchList *normalize_match_cases(MinMatchList *cases) {
    if (cases == NULL) {
        return NULL;
    }
    MinMatchList *rest = normalize_match_cases(cases->next);
    int save = PROTECT(rest);
    MinExp *newExp = normalize_term(cases->body);
    PROTECT(newExp);
    MinMatchList *newCases =
        newMinMatchList(CPI(cases), cases->matches, newExp, rest);
    UNPROTECT(save);
    return newCases;
}

static MinExp *normalizeMatchKont(MinExp *t, NormalizeMatchKontEnv *env) {
    MinMatchList *cases = normalize_match_cases(env->cases);
    int save = PROTECT(cases);
    MinExp *newMatch = makeMinExp_Match(CPI(t), t, cases);
    PROTECT(newMatch);
    MinExp *result = INVOKE(env->k, newMatch);
    UNPROTECT(save);
    return result;
}

// (`(primitive-apply ,type ,e1 ,e2)
//     (normalize-name e1
//         [λ (anfE1)
//             (normalize-name e2
//                 (λ (anfE2)
//                     (k `(primitive-apply ,type ,anfE1 ,anfE2))))]))

static MinExp *normalize_PrimApp(MinExp *exp, AnfKont *k) {
    ENTER(normalize_PrimApp);
    MinPrimApp *primApp = getMinExp_Prim(exp);
    AnfKont *k1 =
        makeKont_normalizePrimappOuter(primApp->type, primApp->exp2, k);
    int save = PROTECT(k1);
    MinExp *result = normalize_name(primApp->exp1, k1);
    UNPROTECT(save);
    LEAVE(normalize_PrimApp);
    return result;
}

static MinExp *normalizePrimappOuterKont(MinExp *anfE0,
                                         NormalizePrimappOuterKontEnv *env) {
    AnfKont *k2 = makeKont_normalizePrimappInner(env->type, anfE0, env->k);
    int save = PROTECT(k2);
    MinExp *result = normalize_name(env->e2, k2);
    UNPROTECT(save);
    return result;
}

static MinExp *normalizePrimappInnerKont(MinExp *anfE2,
                                         NormalizePrimappInnerKontEnv *env) {
    MinExp *newPrimApp =
        makeMinExp_Prim(CPI(anfE2), env->type, env->anfE1, anfE2);
    int save = PROTECT(newPrimApp);
    MinExp *result = INVOKE(env->k, newPrimApp);
    UNPROTECT(save);
    return result;
}

// (`(tuple-index ,vec ,size ,e0)
//     (normalize-name e0
//         [λ (t0) (k `(tuple-index ,vec ,size ,t0))]))

static MinExp *normalize_TupleIndex(MinExp *exp, AnfKont *k) {
    ENTER(normalize_TupleIndex);
    MinTupleIndex *tupleIndexExp = getMinExp_TupleIndex(exp);
    AnfKont *k2 = makeKont_normalizeTupleIndex(tupleIndexExp->vec,
                                               tupleIndexExp->size, k);
    int save = PROTECT(k2);
    MinExp *result = normalize_name(tupleIndexExp->exp, k2);
    UNPROTECT(save);
    LEAVE(normalize_TupleIndex);
    return result;
}

static MinExp *normalizeTupleIndexKont(MinExp *t0,
                                       NormalizeTupleIndexKontEnv *env) {
    MinExp *newTupleIndex =
        makeMinExp_TupleIndex(CPI(t0), env->vec, env->size, t0);
    int save = PROTECT(newTupleIndex);
    MinExp *result = INVOKE(env->k, newTupleIndex);
    UNPROTECT(save);
    return result;
}

// (`(tag ,e0)
//     (normalize-name e0
//         [λ (t0) (k `(tag ,t0))]))

static MinExp *normalize_Tag(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Tag);
    MinExp *tagExp = getMinExp_Tag(exp);
    AnfKont *k2 = makeKont_normalizeTag(k);
    int save = PROTECT(k2);
    MinExp *result = normalize_name(tagExp, k2);
    UNPROTECT(save);
    LEAVE(normalize_Tag);
    return result;
}

static MinExp *normalizeTagKont(MinExp *t0, NormalizeTagKontEnv *env) {
    MinExp *newTag = newMinExp_Tag(CPI(t0), t0);
    int save = PROTECT(newTag);
    MinExp *result = INVOKE(env->k, newTag);
    UNPROTECT(save);
    return result;
}

// (`(amb ,e0, e1)
//    (k `(amb ,(normalize-term e0)
//             ,(normalize-term e1))))

static MinExp *normalize_Amb(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Amb);
    MinAmb *ambExp = getMinExp_Amb(exp);
    MinExp *e0 = normalize_term(ambExp->left);
    int save = PROTECT(e0);
    MinExp *e1 = normalize_term(ambExp->right);
    PROTECT(e1);
    MinExp *newAmb = makeMinExp_Amb(CPI(exp), e0, e1);
    PROTECT(newAmb);
    MinExp *result = INVOKE(k, newAmb);
    UNPROTECT(save);
    LEAVE(normalize_Amb);
    return result;
}

// (`(λ ,params ,body)
//   (k `(λ ,params ,(normalize-term body))))

static MinExp *normalize_Lam(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Lam);
    MinLam *minExp = getMinExp_Lam(exp);
    MinExp *newBody = normalize_term(minExp->exp);
    int save = PROTECT(newBody);
    MinExp *newMin = makeMinExp_Lam(CPI(exp), minExp->args, newBody);
    PROTECT(newMin);
    getMinExp_Lam(newMin)->isMacro = minExp->isMacro;
    MinExp *result = INVOKE(k, newMin);
    UNPROTECT(save);
    LEAVE(normalize_Lam);
    return result;
}

// (`(lookUp ,name ,id ,expr)
//     (k `(lookUp ,name ,id ,(normalize-term expr))))

static MinExp *normalize_LookUp(MinExp *exp, AnfKont *k) {
    ENTER(normalize_LookUp);
    MinLookUp *lookUpExp = getMinExp_LookUp(exp);
    MinExp *newExpr = normalize_term(lookUpExp->exp);
    int save = PROTECT(newExpr);
    MinExp *newLookUp = makeMinExp_LookUp(CPI(exp), lookUpExp->nsId,
                                          lookUpExp->nsSymbol, newExpr);
    PROTECT(newLookUp);
    MinExp *result = INVOKE(k, newLookUp);
    UNPROTECT(save);
    LEAVE(normalize_LookUp);
    return result;
}

// (`(nameSpaces . ,Ms)
//     (k `(nameSpaces . ,(normalize-terms Ms))))

static MinNameSpaceArray *normalize_array(MinNameSpaceArray *nsArray) {
    MinNameSpaceArray *newNsArray = newMinNameSpaceArray();
    int save = PROTECT(newNsArray);
    for (Index i = 0; i < nsArray->size; i++) {
        MinExp *normNs = normalize_term(nsArray->entries[i]);
        int save = PROTECT(normNs);
        pushMinNameSpaceArray(newNsArray, normNs);
        UNPROTECT(save);
    }
    UNPROTECT(save);
    return newNsArray;
}

static MinExp *normalize_NameSpaces(MinExp *exp, AnfKont *k) {
    ENTER(normalize_NameSpaces);
    MinNameSpaceArray *nsExp = normalize_array(getMinExp_NameSpaces(exp));
    int save = PROTECT(nsExp);
    MinExp *newNsExp = newMinExp_NameSpaces(CPI(exp), nsExp);
    PROTECT(newNsExp);
    MinExp *result = INVOKE(k, newNsExp);
    ;
    UNPROTECT(save);
    LEAVE(normalize_NameSpaces);
    return result;
}

// (`(callcc ,e0)
//     (normalize-name e0
//         [λ (t0) (k `(callcc ,t0))]))

static MinExp *normalize_CallCC(MinExp *exp, AnfKont *k) {
    ENTER(normalize_CallCC);
    MinExp *e0 = getMinExp_CallCC(exp);
    AnfKont *k2 = makeKont_normalizeCallCC(k);
    int save = PROTECT(k2);
    MinExp *result = normalize_name(e0, k2);
    UNPROTECT(save);
    LEAVE(normalize_CallCC);
    return result;
}

static MinExp *normalizeCallCCKont(MinExp *t0, NormalizeCallCCKontEnv *env) {
    MinExp *newCallCC = newMinExp_CallCC(CPI(t0), t0);
    int save = PROTECT(newCallCC);
    MinExp *result = INVOKE(env->k, newCallCC);
    UNPROTECT(save);
    return result;
}

// (`(sequence . ,Ms)
//     (k `(sequence . ,(normalize-terms Ms))))

static MinSequence *_normalizeSquence(MinSequence *seq);

static MinExp *normalize_Sequence(MinExp *exp, AnfKont *k) {
    ENTER(normalize_Sequence);
    MinSequence *normSeq = _normalizeSquence(getMinExp_Sequence(exp));
    int save = PROTECT(normSeq);
    MinExp *newSeqExp = newMinExp_Sequence(CPI(exp), normSeq);
    PROTECT(newSeqExp);
    MinExp *result = INVOKE(k, newSeqExp);
    UNPROTECT(save);
    LEAVE(normalize_Sequence);
    return result;
}

static MinSequence *_normalizeSquence(MinSequence *seq) {
    if (seq == NULL)
        return NULL;
    MinSequence *rest = _normalizeSquence(seq->next);
    int save = PROTECT(rest);
    MinExp *newExp = normalize_term(seq->exp);
    PROTECT(newExp);
    MinSequence *newSeq = newMinSequence(CPI(seq), newExp, rest);
    UNPROTECT(save);
    return newSeq;
}

// (`(typeDefs ,defs ,body)
//   (k `(typeDefs ,defs ,(normalize-term body))))

static MinExp *normalize_TypeDefs(MinExp *exp, AnfKont *k) {
    ENTER(normalize_TypeDefs);
    MinTypeDefs *typeDefsExp = getMinExp_TypeDefs(exp);
    MinExp *newBody = normalize_term(typeDefsExp->body);
    int save = PROTECT(newBody);
    MinExp *newTypeDefs =
        makeMinExp_TypeDefs(CPI(exp), typeDefsExp->typeDefs, newBody);
    PROTECT(newTypeDefs);
    MinExp *result = INVOKE(k, newTypeDefs);
    UNPROTECT(save);
    LEAVE(normalize_TypeDefs);
    return result;
}

// (define (normalize e k)
//    (match e
//       ...))

static MinExp *normalize(MinExp *exp, AnfKont *k) {
    ENTER(normalize);
    MinExp *res = NULL;
    if (exp == NULL) {
        res = NULL;
    } else if (isValueExp(exp)) {
        res = INVOKE(k, exp);
    } else {
        switch (exp->type) {
        case MINEXP_TYPE_AMB:
            res = normalize_Amb(exp, k);
            break;
        case MINEXP_TYPE_APPLY:
            res = normalize_Apply(exp, k);
            break;
        case MINEXP_TYPE_CALLCC:
            res = normalize_CallCC(exp, k);
            break;
        case MINEXP_TYPE_COND:
            res = normalize_Cond(exp, k);
            break;
        case MINEXP_TYPE_DECONSTRUCT:
            res = normalize_Deconstruct(exp, k);
            break;
        case MINEXP_TYPE_IFF:
            res = normalize_Iff(exp, k);
            break;
        case MINEXP_TYPE_LAM:
            res = normalize_Lam(exp, k);
            break;
        case MINEXP_TYPE_LETREC:
            res = normalize_LetRec(exp, k);
            break;
        case MINEXP_TYPE_LOOKUP:
            res = normalize_LookUp(exp, k);
            break;
        case MINEXP_TYPE_MAKETUPLE:
            res = normalize_MakeTuple(exp, k);
            break;
        case MINEXP_TYPE_MAKEVEC:
            res = normalize_MakeVec(exp, k);
            break;
        case MINEXP_TYPE_MATCH:
            res = normalize_Match(exp, k);
            break;
        case MINEXP_TYPE_NAMESPACES:
            res = normalize_NameSpaces(exp, k);
            break;
        case MINEXP_TYPE_PRIM:
            res = normalize_PrimApp(exp, k);
            break;
        case MINEXP_TYPE_SEQUENCE:
            res = normalize_Sequence(exp, k);
            break;
        case MINEXP_TYPE_TAG:
            res = normalize_Tag(exp, k);
            break;
        case MINEXP_TYPE_TUPLEINDEX:
            res = normalize_TupleIndex(exp, k);
            break;
        case MINEXP_TYPE_TYPEDEFS:
            res = normalize_TypeDefs(exp, k);
            break;
        default:
            cant_happen("normalize: unhandled MinExp type %s",
                        minExpTypeName(exp->type));
        }
    }
    LEAVE(normalize);
    return res;
}

/**
 * Public entry point.
 */
MinExp *anfNormalize2(MinExp *exp) { return normalize_term(exp); }
