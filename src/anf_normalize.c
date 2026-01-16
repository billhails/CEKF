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
 */

#include "anf_normalize.h"
#include "bigint.h"
#include "common.h"
#include "hash.h"
#include "memory.h"
#include "minlam_helper.h"
#include "symbol.h"

#ifdef DEBUG_ANF
#include "anf_debug.h"
#include "debug.h"
#include "debugging_on.h"
#include "minlam_pp.h"
#include <stdio.h>
#include <unistd.h>
#else
#include "debugging_off.h"
#endif

static AnfExp *normalize(MinExp *minExp, AnfExp *tail);
static AnfExp *normalizeMin(MinLam *minMin, AnfExp *tail);
static AnfExp *normalizeNameSpaces(ParserInfo I, MinNameSpaceArray *nsArray,
                                   AnfExp *tail);
static AnfExp *normalizeVar(ParserInfo I, HashSymbol *var, AnfExp *tail);
static AnfExp *normalizeMaybeBigInteger(ParserInfo I, MaybeBigInt *integer,
                                        AnfExp *tail);
static AnfExp *normalizeStdInteger(ParserInfo I, int integer, AnfExp *tail);
static AnfExp *normalizeCharacter(ParserInfo I, Character character,
                                  AnfExp *tail);
static AnfExp *normalizeAmb(MinAmb *app, AnfExp *tail);
static AnfExp *normalizeSequence(MinSequence *sequence, AnfExp *tail);
static AnfExp *normalizePrim(MinPrimApp *app, AnfExp *tail);
static AnfExp *normalizeApply(MinApply *minApply, AnfExp *tail);
static AnfExp *normalizeBack(ParserInfo I, AnfExp *tail);
static AnfExp *normalizeError(ParserInfo I, AnfExp *tail);
static HashSymbol *freshSymbol();
static Aexp *replaceMinExp(MinExp *minExp, MinExpTable *replacements);
static AnfExp *letBind(AnfExp *body, MinExpTable *replacements);
static AexpPrimOp mapPrimOp(MinPrimOp op);
static Aexp *aexpNormalizeVar(ParserInfo I, HashSymbol *var);
static Aexp *aexpNormalizeMaybeBigInteger(ParserInfo I, MaybeBigInt *integer);
static Aexp *aexpNormalizeStdInteger(ParserInfo I, int integer);
static Aexp *aexpNormalizeCharacter(ParserInfo I, Character character);
static Aexp *aexpNormalizeLam(MinLam *minMin);
static AexpNameSpaceArray *aexpNormalizeNameSpaces(ParserInfo I,
                                                   MinNameSpaceArray *nsArray);
static AexpVarList *convertVarList(MinVarList *args);
static AexpList *replaceMinArgs(MinArgs *, MinExpTable *);
static Aexp *replaceMinPrim(MinPrimApp *minPrimApp, MinExpTable *replacements);
static Aexp *replaceMinMakeVec(MinMakeVec *makeVec, MinExpTable *replacements);
static Aexp *replaceMinCexp(MinExp *apply, MinExpTable *replacements);
static AnfExp *normalizeMakeVec(MinMakeVec *makeVec, AnfExp *tail);
static AnfExp *wrapTail(AnfExp *exp, AnfExp *tail);
static AnfExp *normalizeIff(MinIff *minIff, AnfExp *tail);
static AnfExp *normalizeCallCc(MinExp *callCC, AnfExp *tail);
static AnfExp *normalizeLetRec(MinLetRec *minLetRec, AnfExp *tail);
static AnfExp *normalizeMatch(MinMatch *match, AnfExp *tail);
static AnfMatchList *normalizeMatchList(MinMatchList *matchList);
static AexpIntList *convertIntList(MinIntList *list);
static AnfExp *normalizeCond(MinCond *cond, AnfExp *tail);
static CexpCondCases *normalizeCondCases(MinCondCases *cases);
static CexpLetRec *normalizeLetRecBindings(CexpLetRec *, MinBindings *);
static AnfExp *normalizeMakeTuple(ParserInfo, MinArgs *, AnfExp *);
static AnfExp *normalizeTupleIndex(MinTupleIndex *construct, AnfExp *tail);
static AnfExp *normalizeDeconstruct(MinDeconstruct *deconstruct, AnfExp *tail);
static AnfExp *normalizeTag(MinExp *tag, AnfExp *tail);
static AnfExp *normalizeEnv(ParserInfo I, AnfExp *tail);
static AnfExp *normalizeMinLookUp(MinLookUp *, AnfExp *);

AnfExp *anfNormalize(MinExp *minExp) { return normalize(minExp, NULL); }

static AnfExp *normalize(MinExp *minExp, AnfExp *tail) {
    if (minExp == NULL) {
        return NULL;
    }
    ENTER(normalize);
    IFDEBUG(ppMinExp(minExp));
    switch (minExp->type) {
    case MINEXP_TYPE_LAM:
        return normalizeMin(getMinExp_Lam(minExp), tail);
    case MINEXP_TYPE_VAR:
        return normalizeVar(CPI(minExp), getMinExp_Var(minExp), tail);
    case MINEXP_TYPE_STDINT:
        return normalizeStdInteger(CPI(minExp), getMinExp_Stdint(minExp), tail);
    case MINEXP_TYPE_BIGINTEGER:
        return normalizeMaybeBigInteger(CPI(minExp),
                                        getMinExp_BigInteger(minExp), tail);
    case MINEXP_TYPE_PRIM:
        return normalizePrim(getMinExp_Prim(minExp), tail);
    case MINEXP_TYPE_AMB:
        return normalizeAmb(getMinExp_Amb(minExp), tail);
    case MINEXP_TYPE_SEQUENCE:
        return normalizeSequence(getMinExp_Sequence(minExp), tail);
    case MINEXP_TYPE_MAKEVEC:
        return normalizeMakeVec(getMinExp_MakeVec(minExp), tail);
    case MINEXP_TYPE_TYPEDEFS:
        return normalize(getMinExp_TypeDefs(minExp)->body, tail);
    case MINEXP_TYPE_APPLY:
        return normalizeApply(getMinExp_Apply(minExp), tail);
    case MINEXP_TYPE_IFF:
        return normalizeIff(getMinExp_Iff(minExp), tail);
    case MINEXP_TYPE_CALLCC:
        return normalizeCallCc(getMinExp_CallCC(minExp), tail);
    case MINEXP_TYPE_LETREC:
        return normalizeLetRec(getMinExp_LetRec(minExp), tail);
    case MINEXP_TYPE_TUPLEINDEX:
        return normalizeTupleIndex(getMinExp_TupleIndex(minExp), tail);
    case MINEXP_TYPE_DECONSTRUCT:
        return normalizeDeconstruct(getMinExp_Deconstruct(minExp), tail);
    case MINEXP_TYPE_TAG:
        return normalizeTag(getMinExp_Tag(minExp), tail);
    case MINEXP_TYPE_CONSTANT:
        return normalizeStdInteger(CPI(minExp), getMinExp_Constant(minExp)->tag,
                                   tail);
    case MINEXP_TYPE_MATCH:
        return normalizeMatch(getMinExp_Match(minExp), tail);
    case MINEXP_TYPE_COND:
        return normalizeCond(getMinExp_Cond(minExp), tail);
    case MINEXP_TYPE_CHARACTER:
        return normalizeCharacter(CPI(minExp), getMinExp_Character(minExp),
                                  tail);
    case MINEXP_TYPE_BACK:
        return normalizeBack(CPI(minExp), tail);
    case MINEXP_TYPE_ERROR:
        return normalizeError(CPI(minExp), tail);
    case MINEXP_TYPE_MAKETUPLE:
        return normalizeMakeTuple(CPI(minExp), getMinExp_MakeTuple(minExp),
                                  tail);
    case MINEXP_TYPE_NAMESPACES:
        return normalizeNameSpaces(CPI(minExp), getMinExp_NameSpaces(minExp),
                                   tail);
    case MINEXP_TYPE_ENV:
        return normalizeEnv(CPI(minExp), tail);
    case MINEXP_TYPE_LOOKUP:
        return normalizeMinLookUp(getMinExp_LookUp(minExp), tail);
    default:
        cant_happen("unrecognized type %s", minExpTypeName(minExp->type));
    }
    LEAVE(normalize);
}

static AnfExp *wrapAexp(Aexp *aexp) { return newAnfExp_Aexp(CPI(aexp), aexp); }

static AnfExp *wrapCexp(Cexp *cexp) { return newAnfExp_Cexp(CPI(cexp), cexp); }

static AnfExp *normalizeCond(MinCond *cond, AnfExp *tail) {
    ENTER(normalizeCond);
    MinExpTable *replacements = newMinExpTable();
    int save = PROTECT(replacements);
    Aexp *value = replaceMinExp(cond->value, replacements);
    int save2 = PROTECT(value);
    CexpCondCases *cases = normalizeCondCases(cond->cases);
    PROTECT(cases);
    CexpCond *cexpCond = newCexpCond(CPI(value), value, cases);
    UNPROTECT(save2);
    save2 = PROTECT(cexpCond);
    Cexp *cexp = newCexp_Cond(CPI(cexpCond), cexpCond);
    REPLACE_PROTECT(save2, cexp);
    AnfExp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    exp = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeCond);
    return exp;
}

static AnfExp *normalizeMatch(MinMatch *match, AnfExp *tail) {
    ENTER(normalizeMatch);
    MinExpTable *replacements = newMinExpTable();
    int save = PROTECT(replacements);
    Aexp *index = replaceMinExp(match->index, replacements);
    int save2 = PROTECT(index);
    AnfMatchList *matchList = normalizeMatchList(match->cases);
    PROTECT(matchList);
    CexpMatch *cexpMatch = newCexpMatch(CPI(index), index, matchList);
    UNPROTECT(save2);
    save2 = PROTECT(cexpMatch);
    Cexp *cexp = newCexp_Match(CPI(cexpMatch), cexpMatch);
    REPLACE_PROTECT(save2, cexp);
    AnfExp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    AnfExp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeMatch);
    return res;
}

static AnfMatchList *normalizeMatchList(MinMatchList *matchList) {
    ENTER(normalizeMatchList);
    if (matchList == NULL) {
        LEAVE(normalizeMatchList);
        return NULL;
    }
    AnfMatchList *next = normalizeMatchList(matchList->next);
    int save = PROTECT(next);
    AexpIntList *matches = convertIntList(matchList->matches);
    PROTECT(matches);
    AnfExp *body = normalize(matchList->body, NULL);
    PROTECT(body);
    AnfMatchList *this = newAnfMatchList(CPI(matches), matches, body, next);
    UNPROTECT(save);
    LEAVE(normalizeMatchList);
    return this;
}

static MinPrimApp *deconstructToPrimApp(MinDeconstruct *deconstruct) {
    MinExp *index = newMinExp_Stdint(CPI(deconstruct), deconstruct->vec);
    int save = PROTECT(index);
    MinPrimApp *res = newMinPrimApp(CPI(deconstruct), MINPRIMOP_TYPE_VEC, index,
                                    deconstruct->exp);
    UNPROTECT(save);
    return res;
}

static MinPrimApp *tagToPrimApp(MinExp *tagged) {
    MinExp *index = newMinExp_Stdint(CPI(tagged), 0);
    int save = PROTECT(index);
    MinPrimApp *res =
        newMinPrimApp(CPI(tagged), MINPRIMOP_TYPE_VEC, index, tagged);
    UNPROTECT(save);
    return res;
}

static AnfExp *normalizeDeconstruct(MinDeconstruct *deconstruct, AnfExp *tail) {
    ENTER(noramaalizeDeconstruct);
    MinPrimApp *primApp = deconstructToPrimApp(deconstruct);
    int save = PROTECT(primApp);
    AnfExp *res = normalizePrim(primApp, tail);
    UNPROTECT(save);
    LEAVE(noramaalizeDeconstruct);
    return res;
}

static MinPrimApp *tupleIndexToPrimApp(MinTupleIndex *tupleIndex) {
    MinExp *index = newMinExp_Stdint(CPI(tupleIndex), tupleIndex->vec);
    int save = PROTECT(index);
    MinPrimApp *res = newMinPrimApp(CPI(tupleIndex), MINPRIMOP_TYPE_VEC, index,
                                    tupleIndex->exp);
    UNPROTECT(save);
    return res;
}

static AnfExp *normalizeTupleIndex(MinTupleIndex *index, AnfExp *tail) {
    ENTER(noramaalizeTupleIndex);
    MinPrimApp *primApp = tupleIndexToPrimApp(index);
    int save = PROTECT(primApp);
    AnfExp *res = normalizePrim(primApp, tail);
    UNPROTECT(save);
    LEAVE(noramaalizeTupleIndex);
    return res;
}

static AnfExp *normalizeTag(MinExp *tagged, AnfExp *tail) {
    ENTER(noramaalizeTag);
    MinPrimApp *primApp = tagToPrimApp(tagged);
    int save = PROTECT(primApp);
    AnfExp *res = normalizePrim(primApp, tail);
    UNPROTECT(save);
    LEAVE(noramaalizeTag);
    return res;
}

static AnfExp *normalizeLetRec(MinLetRec *minLetRec, AnfExp *tail) {
    ENTER(normalizeLetRec);
    IFDEBUG(ppMinLetRec(minLetRec));
    AnfExp *body = normalize(minLetRec->body, tail);
    int save = PROTECT(body);
    CexpLetRec *cexpLetRec = newCexpLetRec(CPI(body), 0, NULL, body);
    PROTECT(cexpLetRec);
    cexpLetRec = normalizeLetRecBindings(cexpLetRec, minLetRec->bindings);
    PROTECT(cexpLetRec);
    if (cexpLetRec->bindings == NULL) {
        UNPROTECT(save);
        validateLastAlloc();
        LEAVE(normalizeLetRec);
        return cexpLetRec->body;
    }
    Cexp *cexp = newCexp_LetRec(CPI(cexpLetRec), cexpLetRec);
    PROTECT(cexp);
    AnfExp *exp = wrapCexp(cexp);
    UNPROTECT(save);
    LEAVE(normalizeLetRec);
    return exp;
}

static AnfExp *normalizeError(ParserInfo I, AnfExp *tail) {
    ENTER(normalizeError);
    Cexp *error = newCexp_Error(I);
    int save = PROTECT(error);
    AnfExp *exp = wrapCexp(error);
    REPLACE_PROTECT(save, exp);
    exp = wrapTail(exp, tail);
    UNPROTECT(save);
    LEAVE(normalizeError);
    return exp;
}

static AnfExp *normalizeBack(ParserInfo I, AnfExp *tail) {
    ENTER(normalizeBack);
    Cexp *back = newCexp_Back(I);
    int save = PROTECT(back);
    AnfExp *exp = wrapCexp(back);
    REPLACE_PROTECT(save, exp);
    exp = wrapTail(exp, tail);
    UNPROTECT(save);
    LEAVE(normalizeBack);
    return exp;
}

static AnfExp *normalizeCallCc(MinExp *minExp, AnfExp *tail) {
    ENTER(normalizeCallCc);
    MinExpTable *replacements = newMinExpTable();
    int save = PROTECT(replacements);
    Aexp *aexp = replaceMinExp(minExp, replacements);
    int save2 = PROTECT(aexp);
    Cexp *cexp = newCexp_CallCC(CPI(aexp), aexp);
    REPLACE_PROTECT(save2, cexp);
    AnfExp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    AnfExp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeCallCc);
    return res;
}

static AnfExp *normalizeIff(MinIff *minIff, AnfExp *tail) {
    ENTER(normalizeIff);
    MinExpTable *replacements = newMinExpTable();
    int save = PROTECT(replacements);
    Aexp *condition = replaceMinExp(minIff->condition, replacements);
    int save2 = PROTECT(condition);
    AnfExp *consequent = normalize(minIff->consequent, NULL);
    PROTECT(consequent);
    AnfExp *alternative = normalize(minIff->alternative, NULL);
    PROTECT(alternative);
    Cexp *cexp = makeCexp_Iff(CPI(minIff), condition, consequent, alternative);
    REPLACE_PROTECT(save2, cexp);
    AnfExp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    AnfExp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeIff);
    return res;
}

static AnfExp *normalizeMakeVec(MinMakeVec *minMakeVec, AnfExp *tail) {
    ENTER(normalizeMakeVec);
    MinExpTable *replacements = newMinExpTable();
    int save = PROTECT(replacements);
    DEBUG("calling replaceMinArgs");
    AexpList *args = replaceMinArgs(minMakeVec->args, replacements);
    int save2 = PROTECT(args);
    AexpMakeVec *aexpMakeVec =
        newAexpMakeVec(CPI(minMakeVec), countAexpList(args), args);
    REPLACE_PROTECT(save2, aexpMakeVec);
    Aexp *aexp = newAexp_MakeVec(CPI(aexpMakeVec), aexpMakeVec);
    REPLACE_PROTECT(save2, aexp);
    AnfExp *exp = wrapAexp(aexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    AnfExp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeMakeVec);
    return res;
}

static MinMakeVec *tupleToMakeVec(ParserInfo PI, MinArgs *tuple) {
    int nArgs = countMinArgs(tuple);
    MinMakeVec *res = newMinMakeVec(PI, nArgs, tuple);
    return res;
}

static AnfExp *normalizeMakeTuple(ParserInfo PI, MinArgs *tuple, AnfExp *tail) {
    MinMakeVec *makeVec = tupleToMakeVec(PI, tuple);
    int save = PROTECT(makeVec);
    AnfExp *res = normalizeMakeVec(makeVec, tail);
    UNPROTECT(save);
    return res;
}

// sequences are not covered by the algorithm
// however the algorithm states that "All non-atomic
// (complex) expressions must be let-bound or appear
// in tail position". The working scheme ANF for the
// liars puzzle has the expressions in a sequence
// let-bound, so we basically need to translate
// ((exp1) (exp2) (exp3))
// to
// (let (f1 (exp1)) (let (f2 (exp2)) (exp3)))
// not forgetting that those exp* themselves may have
// let-bindings of their own, and that a let nested
// within a let value like:
// (let (a (let (b (c)) b)) d)
// is not legal, `let` is an exp, but the value in a let
// expression can only be a cexp.
// so the algorithm should be right recursive, normalizing
// the last expression in the sequence then binding prior
// expressions within a let. The only remaining problem is
// to flatten the result, i.e. after normalizing the penultimate
// expression, we have a form like
// (let (anf$123 (expr1)) (expr2 anf$123))
// and a tail <expr3> and we want to build
// (let (anf$123 (expr1)) (let (f (expr2 anf$123)) <expr3>))
static AnfExp *normalizeSequence(MinSequence *sequence, AnfExp *tail) {
    ENTER(normalizeSequence);
    if (sequence == NULL) {
        cant_happen("empty sequence in normalizeSequence");
    }
    if (sequence->next == NULL) {
        return normalize(sequence->exp, tail);
    }
    AnfExp *next = normalizeSequence(sequence->next, tail);
    int save = PROTECT(next);
    AnfExp *this = normalize(sequence->exp, next);
    UNPROTECT(save);
    LEAVE(normalizeSequence);
    return this;
}

static AnfExp *wrapTail(AnfExp *exp, AnfExp *tail) {
    ENTER(wrapTail);
    if (tail == NULL) {
        LEAVE(wrapTail);
        return exp;
    }
    HashSymbol *s = freshSymbol();
    AnfExpLet *let = newAnfExpLet(CPI(exp), s, exp, tail);
    int save = PROTECT(let);
    exp = newAnfExp_Let(CPI(let), let);
    UNPROTECT(save);
    LEAVE(wrapTail);
    return exp;
}

static AnfExp *normalizePrim(MinPrimApp *app, AnfExp *tail) {
    ENTER(normalizePrim);
    MinExpTable *replacements = newMinExpTable();
    int save = PROTECT(replacements);
    Aexp *exp1 = replaceMinExp(app->exp1, replacements);
    PROTECT(exp1);
    Aexp *exp2 = replaceMinExp(app->exp2, replacements);
    PROTECT(exp2);
    Aexp *aexp = makeAexp_Prim(CPI(app), mapPrimOp(app->type), exp1, exp2);
    PROTECT(aexp);
    AnfExp *exp = wrapAexp(aexp);
    PROTECT(exp);
    exp = wrapTail(exp, tail);
    PROTECT(exp);
    AnfExp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizePrim);
    return res;
}

static AnfExp *normalizeAmb(MinAmb *app, AnfExp *tail) {
    AnfExp *left = normalize(app->left, NULL);
    int save = PROTECT(left);
    AnfExp *right = normalize(app->right, NULL);
    PROTECT(right);
    CexpAmb *amb = newCexpAmb(CPI(app), left, right);
    PROTECT(amb);
    Cexp *cexp = newCexp_Amb(CPI(amb), amb);
    PROTECT(cexp);
    AnfExp *exp = wrapCexp(cexp);
    PROTECT(exp);
    exp = wrapTail(exp, tail);
    UNPROTECT(save);
    return exp;
}

static AnfExp *normalizeVar(ParserInfo I, HashSymbol *var, AnfExp *tail) {
    ENTER(normalizeVar);
    if (tail != NULL) {
        LEAVE(normalizeVar);
        return tail;
    }
    Aexp *aexp = aexpNormalizeVar(I, var);
    int save = PROTECT(aexp);
    AnfExp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeVar);
    return exp;
}

static AnfExp *normalizeCharacter(ParserInfo I, Character character,
                                  AnfExp *tail) {
    ENTER(normalizeCharacter);
    if (tail != NULL) {
        LEAVE(normalizeCharacter);
        return tail;
    }
    Aexp *aexp = aexpNormalizeCharacter(I, character);
    int save = PROTECT(aexp);
    AnfExp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeCharacter);
    return exp;
}

static AnfExp *normalizeMaybeBigInteger(ParserInfo I, MaybeBigInt *integer,
                                        AnfExp *tail) {
    ENTER(normalizeMaybeBigInteger);
    if (tail != NULL) {
        LEAVE(normalizeMaybeBigInteger);
        return tail;
    }
    Aexp *aexp = aexpNormalizeMaybeBigInteger(I, integer);
    int save = PROTECT(aexp);
    AnfExp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeMaybeBigInteger);
    return exp;
}

static AnfExp *normalizeStdInteger(ParserInfo I, int integer, AnfExp *tail) {
    ENTER(normalizeStdInteger);
    if (tail != NULL) {
        LEAVE(normalizeStdInteger);
        return tail;
    }
    Aexp *aexp = aexpNormalizeStdInteger(I, integer);
    int save = PROTECT(aexp);
    AnfExp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeStdInteger);
    return exp;
}

static AnfExp *normalizeNameSpaces(ParserInfo I, MinNameSpaceArray *nsArray,
                                   AnfExp *tail) {
    ENTER(normalizeNameSpaces);
    AexpNameSpaceArray *nsa = aexpNormalizeNameSpaces(I, nsArray);
    int save = PROTECT(nsa);
    AexpNameSpaces *nso = newAexpNameSpaces(I, nsa, tail);
    PROTECT(nso);
    Aexp *aexp = newAexp_NameSpaces(I, nso);
    PROTECT(aexp);
    AnfExp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeNameSpaces);
    return exp;
}

static AexpNameSpaceArray *aexpNormalizeNameSpaces(ParserInfo I,
                                                   MinNameSpaceArray *nsArray) {
    ENTER(aexpNormalizeNameSpaces);
    AexpNameSpaceArray *res = newAexpNameSpaceArray();
    int save = PROTECT(res);
    for (Index i = 0; i < nsArray->size; i++) {
        AnfExp *nsExp = normalize(nsArray->entries[i], NULL);
        int save2 = PROTECT(nsExp);
        AexpNameSpace *ns = newAexpNameSpace(I, nsExp);
        PROTECT(ns);
        pushAexpNameSpaceArray(res, ns);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    LEAVE(aexpNormalizeNameSpaces);
    return res;
}

static AnfExp *normalizeEnv(ParserInfo I, AnfExp *tail) {
    ENTER(normalizeEnv);
    if (tail != NULL) {
        LEAVE(normalizeEnv);
        return tail;
    }
    AnfExp *exp = newAnfExp_Env(I);
    LEAVE(normalizeMin);
    return exp;
}

static AnfExp *normalizeMinLookUp(MinLookUp *lookUp, AnfExp *tail) {
    AnfExp *rest = normalize(lookUp->exp, tail);
    int save = PROTECT(rest);
    AnfExpLookUp *exp = newAnfExpLookUp(CPI(lookUp), lookUp->nsId, rest);
    PROTECT(exp);
    AnfExp *res = newAnfExp_LookUp(CPI(exp), exp);
    UNPROTECT(save);
    return res;
}

static AnfExp *normalizeMin(MinLam *minMin, AnfExp *tail) {
    ENTER(normalizeMin);
    if (tail != NULL) {
        LEAVE(normalizeMin);
        return tail;
    }
    Aexp *aexp = aexpNormalizeLam(minMin);
    int save = PROTECT(aexp);
    AnfExp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeMin);
    return exp;
}

static Aexp *aexpNormalizeLam(MinLam *minMin) {
    ENTER(aexpNormalizeLam);
    AexpVarList *varList = convertVarList(minMin->args);
    int save = PROTECT(varList);
    AnfExp *body = normalize(minMin->exp, NULL);
    PROTECT(body);
    Aexp *aexp =
        makeAexp_Lam(CPI(minMin), countAexpVarList(varList), 0, varList, body);
    UNPROTECT(save);
    LEAVE(aexpNormalizeLam);
    return aexp;
}

static AexpIntList *convertIntList(MinIntList *list) {
    ENTER(convertIntList);
    if (list == NULL) {
        LEAVE(convertIntList);
        return NULL;
    }
    AexpIntList *next = convertIntList(list->next);
    int save = PROTECT(next);
    AexpIntList *this = newAexpIntList(CPI(list), list->item, next);
    UNPROTECT(save);
    LEAVE(convertIntList);
    return this;
}

static AexpVarList *convertVarList(MinVarList *args) {
    ENTER(convertVarList);
    if (args == NULL) {
        LEAVE(convertVarList);
        return NULL;
    }
    AexpVarList *next = convertVarList(args->next);
    int save = PROTECT(next);
    AexpVarList *this = newAexpVarList(CPI(args), args->var, next);
    UNPROTECT(save);
    LEAVE(convertVarList);
    return this;
}

static AnfExp *normalizeApply(MinApply *minApply, AnfExp *tail) {
    ENTER(normalizeApply);
    MinExpTable *replacements = newMinExpTable();
    int save = PROTECT(replacements);
    Aexp *function = replaceMinExp(minApply->function, replacements);
    int save2 = PROTECT(function);
    DEBUG("calling replaceMinArgs");
    AexpList *args = replaceMinArgs(minApply->args, replacements);
    PROTECT(args);
    DEBUG("back from replaceMinArgs");
    IFDEBUG(printMinExpTable(replacements, 0));
    Cexp *cexp =
        makeCexp_Apply(CPI(minApply), function, countAexpList(args), args);
    REPLACE_PROTECT(save2, cexp);
    AnfExp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    DEBUG("calling wrapTail");
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    AnfExp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeApply);
    return res;
}

static AnfExp *letBind(AnfExp *body, MinExpTable *replacements) {
    ENTER(letBind);
    // DEBUG("sleep %d", sleep(1));
    IFDEBUG(printExp(body, 0));
    IFDEBUG(printMinExpTable(replacements, 0));
    if (countMinExpTable(replacements) == 0) {
        LEAVE(letBind);
        return body;
    }
    int save = PROTECT(body);
    MinExp *minExpVal = NULL;
    Index i = 0;
    HashSymbol *key = NULL;
    while ((key = iterateMinExpTable(replacements, &i, &minExpVal)) != NULL) {
        DEBUG("letBind iteration %d", i);
        AnfExp *exp = normalize(minExpVal, NULL);
        int save2 = PROTECT(exp);
        AnfExpLet *let = newAnfExpLet(CPI(exp), key, exp, body);
        PROTECT(let);
        body = newAnfExp_Let(CPI(let), let);
        UNPROTECT(save2);
        REPLACE_PROTECT(save, body);
    }
    UNPROTECT(save);
    LEAVE(letBind);
    return body;
}

static Aexp *aexpNormalizeVar(ParserInfo I, HashSymbol *var) {
    DEBUG("aexpNormalizeVar %s", var->name);
    return newAexp_Var(I, var);
}

static Aexp *aexpNormalizeMaybeBigInteger(ParserInfo I, MaybeBigInt *integer) {
    return newAexp_BigInteger(I, integer);
}

static Aexp *aexpNormalizeStdInteger(ParserInfo I, int integer) {
    return newAexp_LittleInteger(I, integer);
}

static Aexp *aexpNormalizeCharacter(ParserInfo I, Character character) {
    return newAexp_Character(I, character);
}

static CexpIntCondCases *normalizeIntCondCases(MinIntCondCases *cases) {
    if (cases == NULL)
        return NULL;
    CexpIntCondCases *next = normalizeIntCondCases(cases->next);
    int save = PROTECT(next);
    AnfExp *body = normalize(cases->body, NULL);
    PROTECT(body);
    CexpIntCondCases *this =
        newCexpIntCondCases(CPI(cases), cases->constant, body, next);
    UNPROTECT(save);
    return this;
}

static CexpCharCondCases *normalizeCharCondCases(MinCharCondCases *cases) {
    if (cases == NULL)
        return NULL;
    CexpCharCondCases *next = normalizeCharCondCases(cases->next);
    int save = PROTECT(next);
    AnfExp *body = normalize(cases->body, NULL);
    PROTECT(body);
    CexpCharCondCases *this =
        newCexpCharCondCases(CPI(cases), cases->constant, body, next);
    UNPROTECT(save);
    return this;
}

static CexpCondCases *normalizeCondCases(MinCondCases *cases) {
    ENTER(normalizeCondCases);
    if (cases == NULL) {
        LEAVE(normalizeCondCases);
        return NULL;
    }
    CexpCondCases *res = NULL;
    int save = PROTECT(NULL);
    switch (cases->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        CexpIntCondCases *intCases =
            normalizeIntCondCases(getMinCondCases_Integers(cases));
        PROTECT(intCases);
        res = newCexpCondCases_IntCases(CPI(cases), intCases);
    } break;
    case MINCONDCASES_TYPE_CHARACTERS: {
        CexpCharCondCases *charCases =
            normalizeCharCondCases(getMinCondCases_Characters(cases));
        PROTECT(charCases);
        res = newCexpCondCases_CharCases(CPI(cases), charCases);
    } break;
    default:
        cant_happen("unrecognized type %d in normlizeCondCases", cases->type);
    }
    UNPROTECT(save);
    LEAVE(normalizeCondCases);
    return res;
}

static Aexp *replaceMinDeconstruct(MinDeconstruct *minDeconstruct,
                                   MinExpTable *replacements) {
    MinPrimApp *primApp = deconstructToPrimApp(minDeconstruct);
    int save = PROTECT(primApp);
    Aexp *res = replaceMinPrim(primApp, replacements);
    UNPROTECT(save);
    return res;
}

static Aexp *replaceMinTag(MinExp *tagged, MinExpTable *replacements) {
    MinPrimApp *primApp = tagToPrimApp(tagged);
    int save = PROTECT(primApp);
    Aexp *res = replaceMinPrim(primApp, replacements);
    UNPROTECT(save);
    return res;
}

static Aexp *replaceMinTupleIndex(MinTupleIndex *tupleIndex,
                                  MinExpTable *replacements) {
    MinPrimApp *primApp = tupleIndexToPrimApp(tupleIndex);
    int save = PROTECT(primApp);
    Aexp *res = replaceMinPrim(primApp, replacements);
    UNPROTECT(save);
    return res;
}

static Aexp *replaceMinExp(MinExp *minExp, MinExpTable *replacements) {
    ENTER(replaceMinExp);
    Aexp *res = NULL;
    switch (minExp->type) {
    case MINEXP_TYPE_TUPLEINDEX:
        res = replaceMinTupleIndex(getMinExp_TupleIndex(minExp), replacements);
        break;
    case MINEXP_TYPE_LAM:
        res = aexpNormalizeLam(getMinExp_Lam(minExp));
        break;
    case MINEXP_TYPE_VAR:
        res = aexpNormalizeVar(CPI(minExp), getMinExp_Var(minExp));
        break;
    case MINEXP_TYPE_BIGINTEGER:
        res = aexpNormalizeMaybeBigInteger(CPI(minExp),
                                           getMinExp_BigInteger(minExp));
        break;
    case MINEXP_TYPE_STDINT:
        res = aexpNormalizeStdInteger(CPI(minExp), getMinExp_Stdint(minExp));
        break;
    case MINEXP_TYPE_PRIM:
        res = replaceMinPrim(getMinExp_Prim(minExp), replacements);
        break;
    case MINEXP_TYPE_MAKEVEC:
        res = replaceMinMakeVec(getMinExp_MakeVec(minExp), replacements);
        break;
    case MINEXP_TYPE_TAG:
        res = replaceMinTag(getMinExp_Tag(minExp), replacements);
        break;
    case MINEXP_TYPE_CONSTANT:
        res = aexpNormalizeStdInteger(CPI(minExp),
                                      getMinExp_Constant(minExp)->tag);
        break;
    case MINEXP_TYPE_TYPEDEFS:
        res = replaceMinCexp(getMinExp_TypeDefs(minExp)->body, replacements);
        break;
    case MINEXP_TYPE_DECONSTRUCT:
        res =
            replaceMinDeconstruct(getMinExp_Deconstruct(minExp), replacements);
        break;
    case MINEXP_TYPE_CHARACTER:
        res = aexpNormalizeCharacter(CPI(minExp), getMinExp_Character(minExp));
        break;
    case MINEXP_TYPE_LOOKUP:
    case MINEXP_TYPE_SEQUENCE:
    case MINEXP_TYPE_APPLY:
    case MINEXP_TYPE_IFF:
    case MINEXP_TYPE_CALLCC:
    case MINEXP_TYPE_LETREC:
    case MINEXP_TYPE_MATCH:
    case MINEXP_TYPE_COND:
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_ERROR:
    case MINEXP_TYPE_AMB:
    case MINEXP_TYPE_MAKETUPLE:
        res = replaceMinCexp(minExp, replacements);
        break;
    default:
        cant_happen("unrecognised type %s", minExpTypeName(minExp->type));
    }
    LEAVE(replaceMinExp);
    return res;
}

static bool minExpIsMinbda(MinExp *val) {
    switch (val->type) {
    case MINEXP_TYPE_LAM:
        return true;
    case MINEXP_TYPE_VAR:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_CONSTANT:
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_ERROR:
    case MINEXP_TYPE_AMB:
    case MINEXP_TYPE_PRIM:
    case MINEXP_TYPE_SEQUENCE:
    case MINEXP_TYPE_APPLY:
    case MINEXP_TYPE_IFF:
    case MINEXP_TYPE_CALLCC:
    case MINEXP_TYPE_LETREC:
    case MINEXP_TYPE_TYPEDEFS:
    case MINEXP_TYPE_MATCH:
    case MINEXP_TYPE_COND:
    case MINEXP_TYPE_MAKEVEC:
    case MINEXP_TYPE_LOOKUP:
    case MINEXP_TYPE_MAKETUPLE:
    case MINEXP_TYPE_TUPLEINDEX:
        return false;
    default:
        cant_happen("unrecognised MinExp type %s in minExpIsMinbda",
                    minExpTypeName(val->type));
    }
}

static CexpLetRec *normalizeLetRecBindings(CexpLetRec *cexpLetRec,
                                           MinBindings *minLetRecBindings) {
    if (minLetRecBindings == NULL) {
        return cexpLetRec;
    }
    cexpLetRec = normalizeLetRecBindings(cexpLetRec, minLetRecBindings->next);
    int save = PROTECT(cexpLetRec);
    if (minExpIsMinbda(minLetRecBindings->val)) {
        Aexp *val = replaceMinExp(minLetRecBindings->val, NULL);
        PROTECT(val);
        cexpLetRec->bindings =
            newAnfLetRecBindings(CPI(minLetRecBindings), minLetRecBindings->var,
                                 val, cexpLetRec->bindings);
        cexpLetRec->nBindings++;
    } else {
        AnfExp *val = normalize(minLetRecBindings->val, NULL);
        PROTECT(val);
        AnfExp *exp = NULL;
        if (cexpLetRec->bindings != NULL) {
            Cexp *cexp = newCexp_LetRec(CPI(cexpLetRec), cexpLetRec);
            PROTECT(cexp);
            exp = wrapCexp(cexp);
            PROTECT(exp);
        } else {
            exp = cexpLetRec->body;
        }
        AnfExpLet *expLet = newAnfExpLet(CPI(minLetRecBindings),
                                         minLetRecBindings->var, val, exp);
        PROTECT(expLet);
        exp = newAnfExp_Let(CPI(expLet), expLet);
        PROTECT(exp);
        cexpLetRec = newCexpLetRec(CPI(exp), 0, NULL, exp);
    }
    UNPROTECT(save);
    return cexpLetRec;
}

static Aexp *replaceMinMakeVec(MinMakeVec *makeVec, MinExpTable *replacements) {
    ENTER(replaceMinMakeVec);
    DEBUG("calling replaceMinArgs");
    AexpList *aexpList = replaceMinArgs(makeVec->args, replacements);
    int save = PROTECT(aexpList);
    AexpMakeVec *aexpMakeVec =
        newAexpMakeVec(CPI(makeVec), countAexpList(aexpList), aexpList);
    PROTECT(aexpMakeVec);
    Aexp *res = newAexp_MakeVec(CPI(makeVec), aexpMakeVec);
    UNPROTECT(save);
    LEAVE(replaceMinMakeVec);
    return res;
}

static AexpList *replaceMinArgs(MinArgs *list, MinExpTable *replacements) {
    ENTER(replaceMinArgs);
    if (list == NULL) {
        LEAVE(replaceMinArgs);
        return NULL;
    }
    DEBUG("calling replaceMinArgs");
    AexpList *next = replaceMinArgs(list->next, replacements);
    int save = PROTECT(next);
    Aexp *val = replaceMinExp(list->exp, replacements);
    PROTECT(val);
    AexpList *res = newAexpList(CPI(list), val, next);
    UNPROTECT(save);
    LEAVE(replaceMinArgs);
    return res;
}

static Aexp *replaceMinPrim(MinPrimApp *minPrimApp, MinExpTable *replacements) {
    ENTER(replaceMinPrim);
    Aexp *exp1 = replaceMinExp(minPrimApp->exp1, replacements);
    int save = PROTECT(exp1);
    Aexp *exp2 = replaceMinExp(minPrimApp->exp2, replacements);
    PROTECT(exp2);
    AexpPrimApp *prim = newAexpPrimApp(CPI(minPrimApp),
                                       mapPrimOp(minPrimApp->type), exp1, exp2);
    PROTECT(prim);
    Aexp *res = newAexp_Prim(CPI(prim), prim);
    UNPROTECT(save);
    LEAVE(replaceMinPrim);
    return res;
}

static AexpPrimOp mapPrimOp(MinPrimOp op) {
    switch (op) {
    case MINPRIMOP_TYPE_ADD:
        return AEXPPRIMOP_TYPE_ADD;
    case MINPRIMOP_TYPE_SUB:
        return AEXPPRIMOP_TYPE_SUB;
    case MINPRIMOP_TYPE_MUL:
        return AEXPPRIMOP_TYPE_MUL;
    case MINPRIMOP_TYPE_DIV:
        return AEXPPRIMOP_TYPE_DIV;
    case MINPRIMOP_TYPE_POW:
        return AEXPPRIMOP_TYPE_POW;
    case MINPRIMOP_TYPE_EQ:
        return AEXPPRIMOP_TYPE_EQ;
    case MINPRIMOP_TYPE_NE:
        return AEXPPRIMOP_TYPE_NE;
    case MINPRIMOP_TYPE_GT:
        return AEXPPRIMOP_TYPE_GT;
    case MINPRIMOP_TYPE_LT:
        return AEXPPRIMOP_TYPE_LT;
    case MINPRIMOP_TYPE_GE:
        return AEXPPRIMOP_TYPE_GE;
    case MINPRIMOP_TYPE_LE:
        return AEXPPRIMOP_TYPE_LE;
    case MINPRIMOP_TYPE_VEC:
        return AEXPPRIMOP_TYPE_VEC;
    case MINPRIMOP_TYPE_MOD:
        return AEXPPRIMOP_TYPE_MOD;
    case MINPRIMOP_TYPE_CMP:
        return AEXPPRIMOP_TYPE_CMP;
    default:
        cant_happen("unrecognised op type %d in mapPrimOp", op);
    }
}

static HashSymbol *freshSymbol() {
    HashSymbol *res = genSym("anf$");
    DEBUG("freshHashSymbol %s", res->name);
    return res;
}

static Aexp *replaceMinCexp(MinExp *apply, MinExpTable *replacements) {
    ENTER(replaceMinCexp);
    if (replacements == NULL) {
        cant_happen("replaceMinCexp called with null replacements");
    }
    HashSymbol *subst = freshSymbol();
    setMinExpTable(replacements, subst, apply);
    IFDEBUG(printMinExpTable(replacements, 0));
    LEAVE(replaceMinCexp);
    return newAexp_Var(CPI(apply), subst);
}
