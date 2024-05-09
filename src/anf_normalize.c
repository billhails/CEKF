/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2023  Bill Hails
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
#include "common.h"
#include "hash.h"
#include "symbol.h"
#include "memory.h"
#include "lambda_helper.h"
#include "bigint.h"

#ifdef DEBUG_ANF
#  include <stdio.h>
#  include <unistd.h>
#  include "debug.h"
#  include "lambda_pp.h"
#  include "anf_debug.h"
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static Exp *normalize(LamExp *lamExp, Exp *tail);
static Exp *normalizeLam(LamLam *lamLam, Exp *tail);
static Exp *normalizeNameSpaces(LamNameSpaceArray *nsArray, Exp *tail);
static Exp *normalizeVar(HashSymbol *var, Exp *tail);
static Exp *normalizeMaybeBigInteger(MaybeBigInt *integer, Exp *tail);
static Exp *normalizeStdInteger(int integer, Exp *tail);
static Exp *normalizeCharacter(char character, Exp *tail);
static Exp *normalizeUnary(LamUnaryApp *app, Exp *tail);
static Exp *normalizeAnd(LamAnd *app, Exp *tail);
static Exp *normalizeOr(LamOr *app, Exp *tail);
static Exp *normalizeAmb(LamAmb *app, Exp *tail);
static Exp *normalizeSequence(LamSequence *sequence, Exp *tail);
static Exp *normalizePrim(LamPrimApp *app, Exp *tail);
static Exp *normalizeApply(LamApply *lamApply, Exp *tail);
static Exp *normalizeBack(Exp *tail);
static Exp *normalizeError(Exp *tail);
static HashSymbol *freshSymbol();
static LamExpTable *makeLamExpHashTable();
static Aexp *replaceLamExp(LamExp *lamExp, LamExpTable *replacements);
static AexpUnaryOp mapUnaryOp(LamUnaryOp op);
static Exp *letBind(Exp *body, LamExpTable *replacements);
static AexpPrimOp mapPrimOp(LamPrimOp op);
static Aexp *aexpNormalizeVar(HashSymbol *var);
static Aexp *aexpNormalizeMaybeBigInteger(MaybeBigInt *integer);
static Aexp *aexpNormalizeStdInteger(int integer);
static Aexp *aexpNormalizeCharacter(char character);
static Aexp *aexpNormalizeLam(LamLam *lamLam);
static AexpNameSpaceArray *aexpNormalizeNameSpaces(LamNameSpaceArray *nsArray);
static AexpVarList *convertVarList(LamVarList *args);
static AexpList *replaceLamList(LamList *list, LamExpTable *replacements);
static Aexp *replaceLamPrim(LamPrimApp *lamPrimApp,
                            LamExpTable *replacements);
static Aexp *replaceLamUnary(LamUnaryApp *lamUnaryApp,
                             LamExpTable *replacements);
static Aexp *replaceLamMakeVec(LamMakeVec *makeVec,
                               LamExpTable *replacements);
static Aexp *replaceLamConstruct(LamConstruct *construct,
                                 LamExpTable *replacements);
static Aexp *replaceLamPrint(LamPrint *print, LamExpTable *replacements);
static Aexp *replaceLamCexp(LamExp *apply, LamExpTable *replacements);
static Exp *normalizeMakeVec(LamMakeVec *makeVec, Exp *tail);
static Exp *wrapTail(Exp *exp, Exp *tail);
static Exp *normalizeIff(LamIff *lamIff, Exp *tail);
static Exp *normalizeCallCc(LamExp *callcc, Exp *tail);
static Exp *normalizePrint(LamPrint *print, Exp *tail);
static Exp *normalizeLetRec(LamLetRec *lamLetRec, Exp *tail);
static Exp *normalizeLet(LamLet *lamLet, Exp *tail);
static Exp *normalizeMatch(LamMatch *match, Exp *tail);
static MatchList *normalizeMatchList(LamMatchList *matchList);
static AexpIntList *convertIntList(LamIntList *list);
static Exp *normalizeCond(LamCond *cond, Exp *tail);
static CexpCondCases *normalizeCondCases(LamCondCases *cases);
static CexpLetRec *replaceCexpLetRec(CexpLetRec *cexpLetRec,
                                     LamLetRecBindings *lamLetRecBindings);
static Exp *normalizeConstruct(LamConstruct *construct, Exp *tail);
static Exp *normalizeMakeTuple(LamList *tuple, Exp *tail);
static Exp *normalizeTupleIndex(LamTupleIndex *construct, Exp *tail);
static Exp *normalizeDeconstruct(LamDeconstruct *deconstruct, Exp *tail);
static Exp *normalizeTag(LamExp *tag, Exp *tail);
static Exp *normalizeEnv(Exp *tail);
static Exp *normalizeLamLookUp(LamLookUp *, Exp *);

Exp *anfNormalize(LamExp *lamExp) {
    return normalize(lamExp, NULL);
}

static Exp *normalize(LamExp *lamExp, Exp *tail) {
    ENTER(normalize);
    IFDEBUG(ppLamExp(lamExp));
    switch (lamExp->type) {
        case LAMEXP_TYPE_LAM:
            return normalizeLam(lamExp->val.lam, tail);
        case LAMEXP_TYPE_VAR:
            return normalizeVar(lamExp->val.var, tail);
        case LAMEXP_TYPE_STDINT:
            return normalizeStdInteger(lamExp->val.stdint, tail);
        case LAMEXP_TYPE_BIGINTEGER:
            return normalizeMaybeBigInteger(lamExp->val.biginteger, tail);
        case LAMEXP_TYPE_PRIM:
            return normalizePrim(lamExp->val.prim, tail);
        case LAMEXP_TYPE_UNARY:
            return normalizeUnary(lamExp->val.unary, tail);
        case LAMEXP_TYPE_AND:
            return normalizeAnd(lamExp->val.and, tail);
        case LAMEXP_TYPE_OR:
            return normalizeOr(lamExp->val.or, tail);
        case LAMEXP_TYPE_AMB:
            return normalizeAmb(lamExp->val.amb, tail);
        case LAMEXP_TYPE_LIST:
            return normalizeSequence(lamExp->val.list, tail);
        case LAMEXP_TYPE_MAKEVEC:
            return normalizeMakeVec(lamExp->val.makeVec, tail);
        case LAMEXP_TYPE_TYPEDEFS:
            return normalize(lamExp->val.typedefs->body, tail);
        case LAMEXP_TYPE_APPLY:
            return normalizeApply(lamExp->val.apply, tail);
        case LAMEXP_TYPE_IFF:
            return normalizeIff(lamExp->val.iff, tail);
        case LAMEXP_TYPE_CALLCC:
            return normalizeCallCc(lamExp->val.callcc, tail);
        case LAMEXP_TYPE_PRINT:
            return normalizePrint(lamExp->val.print, tail);
        case LAMEXP_TYPE_LETREC:
            return normalizeLetRec(lamExp->val.letrec, tail);
        case LAMEXP_TYPE_TUPLE_INDEX:
            return normalizeTupleIndex(lamExp->val.tuple_index, tail);
        case LAMEXP_TYPE_DECONSTRUCT:
            return normalizeDeconstruct(lamExp->val.deconstruct, tail);
        case LAMEXP_TYPE_CONSTRUCT:
            return normalizeConstruct(lamExp->val.construct, tail);
        case LAMEXP_TYPE_TAG:
            return normalizeTag(lamExp->val.tag, tail);
        case LAMEXP_TYPE_CONSTANT:
            return normalizeStdInteger(lamExp->val.constant->tag, tail);
        case LAMEXP_TYPE_LET:
            return normalizeLet(lamExp->val.let, tail);
        case LAMEXP_TYPE_MATCH:
            return normalizeMatch(lamExp->val.match, tail);
        case LAMEXP_TYPE_COND:
            return normalizeCond(lamExp->val.cond, tail);
        case LAMEXP_TYPE_CHARACTER:
            return normalizeCharacter(lamExp->val.character, tail);
        case LAMEXP_TYPE_BACK:
            return normalizeBack(tail);
        case LAMEXP_TYPE_ERROR:
            return normalizeError(tail);
        case LAMEXP_TYPE_MAKE_TUPLE:
            return normalizeMakeTuple(lamExp->val.make_tuple, tail);
        case LAMEXP_TYPE_NAMESPACES:
            return normalizeNameSpaces(lamExp->val.namespaces, tail);
        case LAMEXP_TYPE_ENV:
            return normalizeEnv(tail);
        case LAMEXP_TYPE_LOOKUP:
            return normalizeLamLookUp(lamExp->val.lookUp, tail);
        case LAMEXP_TYPE_COND_DEFAULT:
            cant_happen("normalize encountered cond default");
        default:
            cant_happen("unrecognized type %s", lamExpTypeName(lamExp->type));
    }
    LEAVE(normalize);
}

static Exp *wrapAexp(Aexp *aexp) {
    return newExp(EXP_TYPE_AEXP, EXP_VAL_AEXP(aexp));
}

static Exp *wrapCexp(Cexp *cexp) {
    return newExp(EXP_TYPE_CEXP, EXP_VAL_CEXP(cexp));
}

static Exp *normalizeCond(LamCond *cond, Exp *tail) {
    ENTER(normalizeCond);
    LamExpTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *value = replaceLamExp(cond->value, replacements);
    int save2 = PROTECT(value);
    CexpCondCases *cases = normalizeCondCases(cond->cases);
    PROTECT(cases);
    CexpCond *cexpCond = newCexpCond(value, cases);
    UNPROTECT(save2);
    save2 = PROTECT(cexpCond);
    Cexp *cexp = newCexp(CEXP_TYPE_COND, CEXP_VAL_COND(cexpCond));
    REPLACE_PROTECT(save2, cexp);
    Exp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    exp = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeCond);
    return exp;
}

static Exp *normalizeMatch(LamMatch *match, Exp *tail) {
    ENTER(normalizeMatch);
    LamExpTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *index = replaceLamExp(match->index, replacements);
    int save2 = PROTECT(index);
    MatchList *matchList = normalizeMatchList(match->cases);
    PROTECT(matchList);
    CexpMatch *cexpMatch = newCexpMatch(index, matchList);
    UNPROTECT(save2);
    save2 = PROTECT(cexpMatch);
    Cexp *cexp = newCexp(CEXP_TYPE_MATCH, CEXP_VAL_MATCH(cexpMatch));
    REPLACE_PROTECT(save2, cexp);
    Exp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeMatch);
    return res;
}

static MatchList *normalizeMatchList(LamMatchList *matchList) {
    ENTER(normalizeMatchList);
    if (matchList == NULL) {
        LEAVE(normalizeMatchList);
        return NULL;
    }
    MatchList *next = normalizeMatchList(matchList->next);
    int save = PROTECT(next);
    AexpIntList *matches = convertIntList(matchList->matches);
    PROTECT(matches);
    Exp *body = normalize(matchList->body, NULL);
    PROTECT(body);
    MatchList *this = newMatchList(matches, body, next);
    UNPROTECT(save);
    LEAVE(normalizeMatchList);
    return this;
}

static Exp *normalizeLet(LamLet *lamLet, Exp *tail) {
    ENTER(normalizeLet);
    Exp *value = normalize(lamLet->value, NULL);
    int save = PROTECT(value);
    Exp *body = normalize(lamLet->body, tail);
    PROTECT(body);
    ExpLet *expLet = newExpLet(lamLet->var, value, body);
    PROTECT(expLet);
    Exp *exp = newExp(EXP_TYPE_LET, EXP_VAL_LET(expLet));
    UNPROTECT(save);
    LEAVE(normalizeLet);
    return exp;
}

static LamPrimApp *deconstructToPrimApp(LamDeconstruct *deconstruct) {
    LamExp *index =
        newLamExp(LAMEXP_TYPE_STDINT, LAMEXP_VAL_STDINT(deconstruct->vec));
    int save = PROTECT(index);
    LamPrimApp *res =
        newLamPrimApp(LAMPRIMOP_TYPE_VEC, index, deconstruct->exp);
    UNPROTECT(save);
    return res;
}

static LamPrimApp *tagToPrimApp(LamExp *tagged) {
    LamExp *index = newLamExp(LAMEXP_TYPE_STDINT, LAMEXP_VAL_STDINT(0));
    int save = PROTECT(index);
    LamPrimApp *res = newLamPrimApp(LAMPRIMOP_TYPE_VEC, index, tagged);
    UNPROTECT(save);
    return res;
}

static Exp *normalizeDeconstruct(LamDeconstruct *deconstruct, Exp *tail) {
    ENTER(noramaalizeDeconstruct);
    LamPrimApp *primApp = deconstructToPrimApp(deconstruct);
    int save = PROTECT(primApp);
    Exp *res = normalizePrim(primApp, tail);
    UNPROTECT(save);
    LEAVE(noramaalizeDeconstruct);
    return res;
}

static LamPrimApp *tupleIndexToPrimApp(LamTupleIndex *tupleIndex) {
    LamExp *index =
        newLamExp(LAMEXP_TYPE_STDINT, LAMEXP_VAL_STDINT(tupleIndex->vec));
    int save = PROTECT(index);
    LamPrimApp *res =
        newLamPrimApp(LAMPRIMOP_TYPE_VEC, index, tupleIndex->exp);
    UNPROTECT(save);
    return res;
}

static Exp *normalizeTupleIndex(LamTupleIndex *index, Exp *tail) {
    ENTER(noramaalizeTupleIndex);
    LamPrimApp *primApp = tupleIndexToPrimApp(index);
    int save = PROTECT(primApp);
    Exp *res = normalizePrim(primApp, tail);
    UNPROTECT(save);
    LEAVE(noramaalizeTupleIndex);
    return res;
}


static Exp *normalizeTag(LamExp *tagged, Exp *tail) {
    ENTER(noramaalizeTag);
    LamPrimApp *primApp = tagToPrimApp(tagged);
    int save = PROTECT(primApp);
    Exp *res = normalizePrim(primApp, tail);
    UNPROTECT(save);
    LEAVE(noramaalizeTag);
    return res;
}

static Exp *normalizeLetRec(LamLetRec *lamLetRec, Exp *tail) {
    ENTER(normalizeLetRec);
    IFDEBUG(ppLamLetRec(lamLetRec));
    Exp *body = normalize(lamLetRec->body, tail);
    int save = PROTECT(body);
    CexpLetRec *cexpLetRec = newCexpLetRec(0, NULL, body);
    PROTECT(cexpLetRec);
    cexpLetRec = replaceCexpLetRec(cexpLetRec, lamLetRec->bindings);
    PROTECT(cexpLetRec);
    if (cexpLetRec->bindings == NULL) {
        UNPROTECT(save);
        validateLastAlloc();
        LEAVE(normalizeLetRec);
        return cexpLetRec->body;
    }
    Cexp *cexp = newCexp(CEXP_TYPE_LETREC, CEXP_VAL_LETREC(cexpLetRec));
    PROTECT(cexp);
    Exp *exp = wrapCexp(cexp);
    UNPROTECT(save);
    LEAVE(normalizeLetRec);
    return exp;
}

static Exp *normalizeError(Exp *tail) {
    ENTER(normalizeError);
    Cexp *error = newCexp(CEXP_TYPE_ERROR, CEXP_VAL_ERROR());
    int save = PROTECT(error);
    Exp *exp = wrapCexp(error);
    REPLACE_PROTECT(save, exp);
    exp = wrapTail(exp, tail);
    UNPROTECT(save);
    LEAVE(normalizeError);
    return exp;
}

static Exp *normalizeBack(Exp *tail) {
    ENTER(normalizeBack);
    Cexp *back = newCexp(CEXP_TYPE_BACK, CEXP_VAL_BACK());
    int save = PROTECT(back);
    Exp *exp = wrapCexp(back);
    REPLACE_PROTECT(save, exp);
    exp = wrapTail(exp, tail);
    UNPROTECT(save);
    LEAVE(normalizeBack);
    return exp;
}

static Exp *normalizeCallCc(LamExp *lamExp, Exp *tail) {
    ENTER(normalizeCallCc);
    LamExpTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *aexp = replaceLamExp(lamExp, replacements);
    int save2 = PROTECT(aexp);
    Cexp *cexp = newCexp(CEXP_TYPE_CALLCC, CEXP_VAL_CALLCC(aexp));
    REPLACE_PROTECT(save2, cexp);
    Exp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeCallCc);
    return res;
}

static LamApply *printToApply(LamPrint *lamPrint) {
    LamList *args = newLamList(lamPrint->exp, NULL);
    int save = PROTECT(args);
    LamApply *lamApply = newLamApply(lamPrint->printer, 1, args);
    UNPROTECT(save);
    return lamApply;
}

static Exp *normalizePrint(LamPrint *lamPrint, Exp *tail) {
    ENTER(normalizePrint);
    LamApply *lamApply = printToApply(lamPrint);
    int save = PROTECT(lamApply);
    Exp *res = normalizeApply(lamApply, tail);
    UNPROTECT(save);
    LEAVE(normalizePrint);
    return res;
}

static Exp *normalizeIff(LamIff *lamIff, Exp *tail) {
    ENTER(normalizeIff);
    LamExpTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *condition = replaceLamExp(lamIff->condition, replacements);
    int save2 = PROTECT(condition);
    Exp *consequent = normalize(lamIff->consequent, NULL);
    PROTECT(consequent);
    Exp *alternative = normalize(lamIff->alternative, NULL);
    PROTECT(alternative);
    CexpIf *cexpIf = newCexpIf(condition, consequent, alternative);
    UNPROTECT(save2);
    save2 = PROTECT(cexpIf);
    Cexp *cexp = newCexp(CEXP_TYPE_IFF, CEXP_VAL_IFF(cexpIf));
    REPLACE_PROTECT(save2, cexp);
    Exp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeIff);
    return res;
}

static Exp *normalizeMakeVec(LamMakeVec *lamMakeVec, Exp *tail) {
    ENTER(normalizeMakeVec);
    LamExpTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    DEBUG("calling replaceLamList");
    AexpList *args = replaceLamList(lamMakeVec->args, replacements);
    int save2 = PROTECT(args);
    AexpMakeVec *aexpMakeVec = newAexpMakeVec(countAexpList(args), args);
    REPLACE_PROTECT(save2, aexpMakeVec);
    Aexp *aexp = newAexp(AEXP_TYPE_MAKEVEC, AEXP_VAL_MAKEVEC(aexpMakeVec));
    REPLACE_PROTECT(save2, aexp);
    Exp *exp = wrapAexp(aexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeMakeVec);
    return res;
}

static LamMakeVec *constructToMakeVec(LamConstruct *construct) {
    int nargs = countLamList(construct->args);
    LamExp *newArg =
        newLamExp(LAMEXP_TYPE_STDINT, LAMEXP_VAL_STDINT(construct->tag));
    int save = PROTECT(newArg);
    LamList *extraItem = newLamList(newArg, construct->args);
    PROTECT(extraItem);
    LamMakeVec *res = newLamMakeVec(nargs + 1, extraItem);
    UNPROTECT(save);
    return res;
}

static LamMakeVec *tupleToMakeVec(LamList *tuple) {
    int nargs = countLamList(tuple);
    LamMakeVec *res = newLamMakeVec(nargs, tuple);
    return res;
}

static Exp *normalizeConstruct(LamConstruct *construct, Exp *tail) {
    ENTER(normalizeConstruct);
    LamMakeVec *makeVec = constructToMakeVec(construct);
    int save = PROTECT(makeVec);
    Exp *res = normalizeMakeVec(makeVec, tail);
    UNPROTECT(save);
    LEAVE(normalizeConstruct);
    return res;
}

static Exp *normalizeMakeTuple(LamList *tuple, Exp *tail) {
    LamMakeVec *makeVec = tupleToMakeVec(tuple);
    int save = PROTECT(makeVec);
    Exp *res = normalizeMakeVec(makeVec, tail);
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
static Exp *normalizeSequence(LamSequence *sequence, Exp *tail) {
    ENTER(normalizeSequence);
    if (sequence == NULL) {
        cant_happen("empty sequence in normalizeSequence");
    }
    if (sequence->next == NULL) {
        return normalize(sequence->exp, tail);
    }
    Exp *next = normalizeSequence(sequence->next, tail);
    int save = PROTECT(next);
    Exp *this = normalize(sequence->exp, next);
    UNPROTECT(save);
    LEAVE(normalizeSequence);
    return this;
}

static Exp *wrapTail(Exp *exp, Exp *tail) {
    ENTER(wrapTail);
    if (tail == NULL) {
        LEAVE(wrapTail);
        return exp;
    }
    HashSymbol *s = freshSymbol();
    ExpLet *let = newExpLet(s, exp, tail);
    int save = PROTECT(let);
    exp = newExp(EXP_TYPE_LET, EXP_VAL_LET(let));
    UNPROTECT(save);
    LEAVE(wrapTail);
    return exp;
}

static Exp *normalizeUnary(LamUnaryApp *app, Exp *tail) {
    ENTER(normalizeUnary);
    LamExpTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *aexp = replaceLamExp(app->exp, replacements);
    int save2 = PROTECT(aexp);
    AexpUnaryApp *aexpUnaryApp = newAexpUnaryApp(mapUnaryOp(app->type), aexp);
    UNPROTECT(save2);
    save2 = PROTECT(aexpUnaryApp);
    Aexp *aexp2 = newAexp(AEXP_TYPE_UNARY, AEXP_VAL_UNARY(aexpUnaryApp));
    REPLACE_PROTECT(save2, aexp2);
    Exp *exp = wrapAexp(aexp2);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeUnary);
    return res;
}

static Exp *normalizePrim(LamPrimApp *app, Exp *tail) {
    ENTER(normalizePrim);
    LamExpTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *exp1 = replaceLamExp(app->exp1, replacements);
    int save2 = PROTECT(exp1);
    Aexp *exp2 = replaceLamExp(app->exp2, replacements);
    PROTECT(exp2);
    AexpPrimApp *aexpPrimApp =
        newAexpPrimApp(mapPrimOp(app->type), exp1, exp2);
    UNPROTECT(save2);
    save2 = PROTECT(aexpPrimApp);
    Aexp *aexp = newAexp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(aexpPrimApp));
    REPLACE_PROTECT(save2, aexp);
    Exp *exp = wrapAexp(aexp);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizePrim);
    return res;
}

static Exp *normalizeAnd(LamAnd *app, Exp *tail) {
    Exp *left = normalize(app->left, NULL);
    int save = PROTECT(left);
    Exp *right = normalize(app->right, NULL);
    PROTECT(right);
    CexpBool *and = newCexpBool(CEXPBOOLTYPE_TYPE_AND, left, right);
    PROTECT(and);
    Cexp *cexp = newCexp(CEXP_TYPE_BOOLEAN, CEXP_VAL_BOOLEAN(and));
    PROTECT(cexp);
    Exp *exp = wrapCexp(cexp);
    PROTECT(exp);
    exp = wrapTail(exp, tail);
    UNPROTECT(save);
    return exp;
}

static Exp *normalizeOr(LamOr *app, Exp *tail) {
    Exp *left = normalize(app->left, NULL);
    int save = PROTECT(left);
    Exp *right = normalize(app->right, NULL);
    PROTECT(right);
    CexpBool *or = newCexpBool(CEXPBOOLTYPE_TYPE_OR, left, right);
    PROTECT(or);
    Cexp *cexp = newCexp(CEXP_TYPE_BOOLEAN, CEXP_VAL_BOOLEAN(or));
    PROTECT(cexp);
    Exp *exp = wrapCexp(cexp);
    PROTECT(exp);
    exp = wrapTail(exp, tail);
    UNPROTECT(save);
    return exp;
}

static Exp *normalizeAmb(LamAmb *app, Exp *tail) {
    Exp *left = normalize(app->left, NULL);
    int save = PROTECT(left);
    Exp *right = normalize(app->right, NULL);
    PROTECT(right);
    CexpAmb *amb = newCexpAmb(left, right);
    PROTECT(amb);
    Cexp *cexp = newCexp(CEXP_TYPE_AMB, CEXP_VAL_AMB(amb));
    PROTECT(cexp);
    Exp *exp = wrapCexp(cexp);
    PROTECT(exp);
    exp = wrapTail(exp, tail);
    UNPROTECT(save);
    return exp;
}

static Exp *normalizeVar(HashSymbol *var, Exp *tail) {
    ENTER(normalizeVar);
    if (tail != NULL) {
        LEAVE(normalizeVar);
        return tail;
    }
    Aexp *aexp = aexpNormalizeVar(var);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeVar);
    return exp;
}

static Exp *normalizeCharacter(char character, Exp *tail) {
    ENTER(normalizeCharacter);
    if (tail != NULL) {
        LEAVE(normalizeCharacter);
        return tail;
    }
    Aexp *aexp = aexpNormalizeCharacter(character);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeCharacter);
    return exp;
}

static Exp *normalizeMaybeBigInteger(MaybeBigInt *integer, Exp *tail) {
    ENTER(normalizeMaybeBigInteger);
    if (tail != NULL) {
        LEAVE(normalizeMaybeBigInteger);
        return tail;
    }
    Aexp *aexp = aexpNormalizeMaybeBigInteger(integer);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeMaybeBigInteger);
    return exp;
}

static Exp *normalizeStdInteger(int integer, Exp *tail) {
    ENTER(normalizeStdInteger);
    if (tail != NULL) {
        LEAVE(normalizeStdInteger);
        return tail;
    }
    Aexp *aexp = aexpNormalizeStdInteger(integer);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeStdInteger);
    return exp;
}

static Exp *normalizeNameSpaces(LamNameSpaceArray *nsArray, Exp *tail) {
    ENTER(normalizeNameSpaces);
    AexpNameSpaceArray *nsa = aexpNormalizeNameSpaces(nsArray);
    int save = PROTECT(nsa);
    AexpNameSpaces *nso = newAexpNameSpaces(nsa, tail);
    PROTECT(nso);
    Aexp *aexp = newAexp(AEXP_TYPE_NAMESPACES, AEXP_VAL_NAMESPACES(nso));
    PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeNameSpaces);
    return exp;
}

static AexpNameSpaceArray *aexpNormalizeNameSpaces(LamNameSpaceArray *nsArray) {
    ENTER(aexpNormalizeNameSpaces);
    AexpNameSpaceArray *res = newAexpNameSpaceArray();
    int save = PROTECT(res);
    for (Index i = 0; i < nsArray->size; i++) {
        Exp *nsExp = normalize(nsArray->entries[i], NULL);
        int save2 = PROTECT(nsExp);
        AexpNameSpace *ns = newAexpNameSpace(nsExp);
        PROTECT(ns);
        pushAexpNameSpaceArray(res, ns);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    LEAVE(aexpNormalizeNameSpaces);
    return res;
}

static Exp *normalizeEnv(Exp *tail) {
    ENTER(normalizeEnv);
    if (tail != NULL) {
        LEAVE(normalizeEnv);
        return tail;
    }
    Exp *exp = newExp(EXP_TYPE_ENV, EXP_VAL_ENV());
    LEAVE(normalizeLam);
    return exp;
}

static Exp *normalizeLamLookUp(LamLookUp *lookUp, Exp *tail) {
    Exp *rest = normalize(lookUp->exp, tail);
    int save = PROTECT(rest);
    ExpLookUp *exp = newExpLookUp(lookUp->namespace, rest);
    PROTECT(exp);
    Exp *res = newExp(EXP_TYPE_LOOKUP, EXP_VAL_LOOKUP(exp));
    UNPROTECT(save);
    return res;
}

static Exp *normalizeLam(LamLam *lamLam, Exp *tail) {
    ENTER(normalizeLam);
    if (tail != NULL) {
        LEAVE(normalizeLam);
        return tail;
    }
    Aexp *aexp = aexpNormalizeLam(lamLam);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeLam);
    return exp;
}

static Aexp *aexpNormalizeLam(LamLam *lamLam) {
    ENTER(aexpNormalizeLam);
    AexpVarList *varList = convertVarList(lamLam->args);
    int save = PROTECT(varList);
    Exp *body = normalize(lamLam->exp, NULL);
    PROTECT(body);
    AexpLam *aexpLam =
        newAexpLam(countAexpVarList(varList), 0, varList, body);
    PROTECT(aexpLam);
    Aexp *aexp = newAexp(AEXP_TYPE_LAM, AEXP_VAL_LAM(aexpLam));
    UNPROTECT(save);
    LEAVE(aexpNormalizeLam);
    return aexp;
}

static AexpIntList *convertIntList(LamIntList *list) {
    ENTER(convertIntList);
    if (list == NULL) {
        LEAVE(convertIntList);
        return NULL;
    }
    AexpIntList *next = convertIntList(list->next);
    int save = PROTECT(next);
    AexpIntList *this = newAexpIntList(list->item, next);
    UNPROTECT(save);
    LEAVE(convertIntList);
    return this;
}

static AexpVarList *convertVarList(LamVarList *args) {
    ENTER(convertVarList);
    if (args == NULL) {
        LEAVE(convertVarList);
        return NULL;
    }
    AexpVarList *next = convertVarList(args->next);
    int save = PROTECT(next);
    AexpVarList *this = newAexpVarList(args->var, next);
    UNPROTECT(save);
    LEAVE(convertVarList);
    return this;
}

static LamExpTable *makeLamExpHashTable() {
    return newLamExpTable();
}

static Exp *normalizeApply(LamApply *lamApply, Exp *tail) {
    ENTER(normalizeApply);
    LamExpTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *function = replaceLamExp(lamApply->function, replacements);
    int save2 = PROTECT(function);
    DEBUG("calling replaceLamList");
    AexpList *args = replaceLamList(lamApply->args, replacements);
    PROTECT(args);
    DEBUG("back from replaceLamList");
    IFDEBUG(printLamExpTable(replacements, 0));
    CexpApply *cexpApply = newCexpApply(function, countAexpList(args), args);
    UNPROTECT(save2);
    save2 = PROTECT(cexpApply);
    Cexp *cexp = newCexp(CEXP_TYPE_APPLY, CEXP_VAL_APPLY(cexpApply));
    REPLACE_PROTECT(save2, cexp);
    Exp *exp = wrapCexp(cexp);
    REPLACE_PROTECT(save2, exp);
    DEBUG("calling wrapTail");
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeApply);
    return res;
}

static Exp *letBind(Exp *body, LamExpTable *replacements) {
    ENTER(letBind);
    // DEBUG("sleep %d", sleep(1));
    IFDEBUG(printExp(body, 0));
    IFDEBUG(printLamExpTable(replacements, 0));
    if (countLamExpTable(replacements) == 0) {
        LEAVE(letBind);
        return body;
    }
    int save = PROTECT(body);
    LamExp *lamExpVal = NULL;
    Index i = 0;
    HashSymbol *key = NULL;
    while ((key = iterateLamExpTable(replacements, &i, &lamExpVal)) != NULL) {
        DEBUG("letBind iteration %d", i);
        Exp *exp = normalize(lamExpVal, NULL);
        int save2 = PROTECT(exp);
        ExpLet *let = newExpLet(key, exp, body);
        PROTECT(let);
        body = newExp(EXP_TYPE_LET, EXP_VAL_LET(let));
        UNPROTECT(save2);
        REPLACE_PROTECT(save, body);
    }
    UNPROTECT(save);
    LEAVE(letBind);
    return body;
}

static Aexp *aexpNormalizeVar(HashSymbol *var) {
    DEBUG("aexpNormalizeVar %s", var->name);
    return newAexp(AEXP_TYPE_VAR, AEXP_VAL_VAR(var));
}

static Aexp *aexpNormalizeMaybeBigInteger(MaybeBigInt *integer) {
    return newAexp(AEXP_TYPE_BIGINTEGER, AEXP_VAL_BIGINTEGER(integer));
}

static Aexp *aexpNormalizeStdInteger(int integer) {
    return newAexp(AEXP_TYPE_LITTLEINTEGER, AEXP_VAL_LITTLEINTEGER(integer));
}

static Aexp *aexpNormalizeCharacter(char character) {
    return newAexp(AEXP_TYPE_CHARACTER, AEXP_VAL_CHARACTER(character));
}

static CexpIntCondCases *normalizeIntCondCases(LamIntCondCases *cases) {
    if (cases == NULL)
        return NULL;
    CexpIntCondCases *next = normalizeIntCondCases(cases->next);
    int save = PROTECT(next);
    Exp *body = normalize(cases->body, NULL);
    PROTECT(body);
    CexpIntCondCases *this = newCexpIntCondCases(cases->constant, body, next);
    UNPROTECT(save);
    return this;
}

static CexpCharCondCases *normalizeCharCondCases(LamCharCondCases *cases) {
    if (cases == NULL)
        return NULL;
    CexpCharCondCases *next = normalizeCharCondCases(cases->next);
    int save = PROTECT(next);
    Exp *body = normalize(cases->body, NULL);
    PROTECT(body);
    CexpCharCondCases *this =
        newCexpCharCondCases(cases->constant, body, next);
    UNPROTECT(save);
    return this;
}

static CexpCondCases *normalizeCondCases(LamCondCases *cases) {
    ENTER(normalizeCondCases);
    if (cases == NULL) {
        LEAVE(normalizeCondCases);
        return NULL;
    }
    CexpCondCases *res = NULL;
    int save = PROTECT(NULL);
    switch (cases->type) {
        case LAMCONDCASES_TYPE_INTEGERS:{
                CexpIntCondCases *intCases =
                    normalizeIntCondCases(cases->val.integers);
                PROTECT(intCases);
                res =
                    newCexpCondCases(CEXPCONDCASES_TYPE_INTCASES,
                                     CEXPCONDCASES_VAL_INTCASES(intCases));
            }
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:{
                CexpCharCondCases *charCases =
                    normalizeCharCondCases(cases->val.characters);
                PROTECT(charCases);
                res =
                    newCexpCondCases(CEXPCONDCASES_TYPE_CHARCASES,
                                     CEXPCONDCASES_VAL_CHARCASES(charCases));
            }
            break;
        default:
            cant_happen("unrecognized type %d in normlizeCondCases",
                        cases->type);
    }
    UNPROTECT(save);
    LEAVE(normalizeCondCases);
    return res;
}

static Aexp *replaceLamDeconstruct(LamDeconstruct *lamDeconstruct,
                                   LamExpTable *replacements) {
    LamPrimApp *primApp = deconstructToPrimApp(lamDeconstruct);
    int save = PROTECT(primApp);
    Aexp *res = replaceLamPrim(primApp, replacements);
    UNPROTECT(save);
    return res;
}

static Aexp *replaceLamTag(LamExp *tagged, LamExpTable *replacements) {
    LamPrimApp *primApp = tagToPrimApp(tagged);
    int save = PROTECT(primApp);
    Aexp *res = replaceLamPrim(primApp, replacements);
    UNPROTECT(save);
    return res;
}

static Aexp *replaceLamExp(LamExp *lamExp, LamExpTable *replacements) {
    ENTER(replaceLamExp);
    Aexp *res = NULL;
    switch (lamExp->type) {
        case LAMEXP_TYPE_LAM:
            res = aexpNormalizeLam(lamExp->val.lam);
            break;
        case LAMEXP_TYPE_VAR:
            res = aexpNormalizeVar(lamExp->val.var);
            break;
        case LAMEXP_TYPE_BIGINTEGER:
            res = aexpNormalizeMaybeBigInteger(lamExp->val.biginteger);
            break;
        case LAMEXP_TYPE_STDINT:
            res = aexpNormalizeStdInteger(lamExp->val.stdint);
            break;
        case LAMEXP_TYPE_PRIM:
            res = replaceLamPrim(lamExp->val.prim, replacements);
            break;
        case LAMEXP_TYPE_UNARY:
            res = replaceLamUnary(lamExp->val.unary, replacements);
            break;
        case LAMEXP_TYPE_PRINT:
            res = replaceLamPrint(lamExp->val.print, replacements);
            break;
        case LAMEXP_TYPE_MAKEVEC:
            res = replaceLamMakeVec(lamExp->val.makeVec, replacements);
            break;
        case LAMEXP_TYPE_CONSTRUCT:
            res = replaceLamConstruct(lamExp->val.construct, replacements);
            break;
        case LAMEXP_TYPE_TAG:
            res = replaceLamTag(lamExp->val.tag, replacements);
            break;
        case LAMEXP_TYPE_CONSTANT:
            res = aexpNormalizeStdInteger(lamExp->val.constant->tag);
            break;
        case LAMEXP_TYPE_TYPEDEFS:
            res = replaceLamCexp(lamExp->val.typedefs->body, replacements);
            break;
        case LAMEXP_TYPE_DECONSTRUCT:
            res =
                replaceLamDeconstruct(lamExp->val.deconstruct, replacements);
            break;
        case LAMEXP_TYPE_CHARACTER:
            res = aexpNormalizeCharacter(lamExp->val.character);
            break;
        case LAMEXP_TYPE_LOOKUP:
        case LAMEXP_TYPE_LIST:
        case LAMEXP_TYPE_APPLY:
        case LAMEXP_TYPE_IFF:
        case LAMEXP_TYPE_CALLCC:
        case LAMEXP_TYPE_LETREC:
        case LAMEXP_TYPE_LET:
        case LAMEXP_TYPE_MATCH:
        case LAMEXP_TYPE_COND:
        case LAMEXP_TYPE_BACK:
        case LAMEXP_TYPE_ERROR:
        case LAMEXP_TYPE_AND:
        case LAMEXP_TYPE_OR:
        case LAMEXP_TYPE_AMB:
        case LAMEXP_TYPE_MAKE_TUPLE:
            res = replaceLamCexp(lamExp, replacements);
            break;
        case LAMEXP_TYPE_COND_DEFAULT:
            cant_happen("replaceLamExp encountered cond default");
        default:
            cant_happen("unrecognised type %s", lamExpTypeName(lamExp->type));
    }
    LEAVE(replaceLamExp);
    return res;
}

static bool lamExpIsLambda(LamExp *val) {
    switch (val->type) {
        case LAMEXP_TYPE_LAM:
            return true;
        case LAMEXP_TYPE_VAR:
        case LAMEXP_TYPE_BIGINTEGER:
        case LAMEXP_TYPE_CHARACTER:
        case LAMEXP_TYPE_CONSTANT:
        case LAMEXP_TYPE_CONSTRUCT:
        case LAMEXP_TYPE_BACK:
        case LAMEXP_TYPE_ERROR:
        case LAMEXP_TYPE_AMB:
        case LAMEXP_TYPE_PRIM:
        case LAMEXP_TYPE_UNARY:
        case LAMEXP_TYPE_LIST:
        case LAMEXP_TYPE_APPLY:
        case LAMEXP_TYPE_IFF:
        case LAMEXP_TYPE_CALLCC:
        case LAMEXP_TYPE_LETREC:
        case LAMEXP_TYPE_TYPEDEFS:
        case LAMEXP_TYPE_LET:
        case LAMEXP_TYPE_MATCH:
        case LAMEXP_TYPE_COND:
        case LAMEXP_TYPE_AND:
        case LAMEXP_TYPE_OR:
        case LAMEXP_TYPE_MAKEVEC:
            return false;
        case LAMEXP_TYPE_COND_DEFAULT:
            cant_happen("lamExpIsLambda encountered cond default");
        default:
            cant_happen("unrecognised LamExp type %s in lamExpIsLambda",
                        lamExpTypeName(val->type));
    }
}

static CexpLetRec *replaceCexpLetRec(CexpLetRec *cexpLetRec,
                                     LamLetRecBindings *lamLetRecBindings) {
    if (lamLetRecBindings == NULL) {
        return cexpLetRec;
    }
    cexpLetRec = replaceCexpLetRec(cexpLetRec, lamLetRecBindings->next);
    int save = PROTECT(cexpLetRec);
    if (lamExpIsLambda(lamLetRecBindings->val)) {
        Aexp *val = replaceLamExp(lamLetRecBindings->val, NULL);
        PROTECT(val);
        cexpLetRec->bindings =
            newLetRecBindings(lamLetRecBindings->var, val,
                              cexpLetRec->bindings);
        cexpLetRec->nbindings++;
    } else {
        Exp *val = normalize(lamLetRecBindings->val, NULL);
        PROTECT(val);
        Exp *exp = NULL;
        if (cexpLetRec->bindings != NULL) {
            Cexp *cexp =
                newCexp(CEXP_TYPE_LETREC, CEXP_VAL_LETREC(cexpLetRec));
            PROTECT(cexp);
            exp = wrapCexp(cexp);
            PROTECT(exp);
        } else {
            exp = cexpLetRec->body;
        }
        ExpLet *expLet = newExpLet(lamLetRecBindings->var, val, exp);
        PROTECT(expLet);
        exp = newExp(EXP_TYPE_LET, EXP_VAL_LET(expLet));
        PROTECT(exp);
        cexpLetRec = newCexpLetRec(0, NULL, exp);
    }
    UNPROTECT(save);
    return cexpLetRec;
}

static Aexp *replaceLamConstruct(LamConstruct *construct,
                                 LamExpTable *replacements) {
    LamMakeVec *makeVec = constructToMakeVec(construct);
    int save = PROTECT(makeVec);
    Aexp *res = replaceLamMakeVec(makeVec, replacements);
    UNPROTECT(save);
    return res;
}

static Aexp *replaceLamMakeVec(LamMakeVec *makeVec, LamExpTable *replacements) {
    ENTER(replaceLamMakeVec);
    DEBUG("calling replaceLamList");
    AexpList *aexpList = replaceLamList(makeVec->args, replacements);
    int save = PROTECT(aexpList);
    AexpMakeVec *aexpMakeVec =
        newAexpMakeVec(countAexpList(aexpList), aexpList);
    PROTECT(aexpMakeVec);
    Aexp *res = newAexp(AEXP_TYPE_MAKEVEC, AEXP_VAL_MAKEVEC(aexpMakeVec));
    UNPROTECT(save);
    LEAVE(replaceLamMakeVec);
    return res;
}

static Aexp *replaceLamPrint(LamPrint *print, LamExpTable *replacements) {
    ENTER(replaceLamPrint);
    LamApply *lamApply = printToApply(print);
    int save = PROTECT(lamApply);
    LamExp *lamExp = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(lamApply));
    PROTECT(lamExp);
    Aexp *res = replaceLamExp(lamExp, replacements);
    UNPROTECT(save);
    LEAVE(replaceLamPrint);
    return res;
}

static AexpList *replaceLamList(LamList *list, LamExpTable *replacements) {
    ENTER(replaceLamList);
    if (list == NULL) {
        LEAVE(replaceLamList);
        return NULL;
    }
    DEBUG("calling replaceLamList");
    AexpList *next = replaceLamList(list->next, replacements);
    int save = PROTECT(next);
    Aexp *val = replaceLamExp(list->exp, replacements);
    PROTECT(val);
    AexpList *res = newAexpList(val, next);
    UNPROTECT(save);
    LEAVE(replaceLamList);
    return res;
}

static Aexp *replaceLamPrim(LamPrimApp *lamPrimApp, LamExpTable *replacements) {
    ENTER(replaceLamPrim);
    Aexp *exp1 = replaceLamExp(lamPrimApp->exp1, replacements);
    int save = PROTECT(exp1);
    Aexp *exp2 = replaceLamExp(lamPrimApp->exp2, replacements);
    PROTECT(exp2);
    AexpPrimApp *prim =
        newAexpPrimApp(mapPrimOp(lamPrimApp->type), exp1, exp2);
    PROTECT(prim);
    Aexp *res = newAexp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(prim));
    UNPROTECT(save);
    LEAVE(replaceLamPrim);
    return res;
}

static Aexp *replaceLamUnary(LamUnaryApp *lamUnaryApp,
                             LamExpTable *replacements) {
    ENTER(replaceLamUnary);
    Aexp *exp = replaceLamExp(lamUnaryApp->exp, replacements);
    int save = PROTECT(exp);
    AexpUnaryApp *unary = newAexpUnaryApp(mapUnaryOp(lamUnaryApp->type), exp);
    PROTECT(unary);
    Aexp *res = newAexp(AEXP_TYPE_UNARY, AEXP_VAL_UNARY(unary));
    UNPROTECT(save);
    LEAVE(replaceLamUnary);
    return res;
}

static AexpUnaryOp mapUnaryOp(LamUnaryOp op) {
    switch (op) {
        case LAMUNARYOP_TYPE_NEG:
            return AEXPUNARYOP_TYPE_NEG;
        case LAMUNARYOP_TYPE_NOT:
            return AEXPUNARYOP_TYPE_NOT;
        case LAMUNARYOP_TYPE_PUTC:
            return AEXPUNARYOP_TYPE_PUTC;
        case LAMUNARYOP_TYPE_PUTN:
            return AEXPUNARYOP_TYPE_PUTN;
        case LAMUNARYOP_TYPE_PUTV:
            return AEXPUNARYOP_TYPE_PUTV;
        default:
            cant_happen("unrecognised type %d in mapUnaryOp", op);
    }
}

static AexpPrimOp mapPrimOp(LamPrimOp op) {
    switch (op) {
        case LAMPRIMOP_TYPE_ADD:
            return AEXPPRIMOP_TYPE_ADD;
        case LAMPRIMOP_TYPE_SUB:
            return AEXPPRIMOP_TYPE_SUB;
        case LAMPRIMOP_TYPE_MUL:
            return AEXPPRIMOP_TYPE_MUL;
        case LAMPRIMOP_TYPE_DIV:
            return AEXPPRIMOP_TYPE_DIV;
        case LAMPRIMOP_TYPE_POW:
            return AEXPPRIMOP_TYPE_POW;
        case LAMPRIMOP_TYPE_EQ:
            return AEXPPRIMOP_TYPE_EQ;
        case LAMPRIMOP_TYPE_NE:
            return AEXPPRIMOP_TYPE_NE;
        case LAMPRIMOP_TYPE_GT:
            return AEXPPRIMOP_TYPE_GT;
        case LAMPRIMOP_TYPE_LT:
            return AEXPPRIMOP_TYPE_LT;
        case LAMPRIMOP_TYPE_GE:
            return AEXPPRIMOP_TYPE_GE;
        case LAMPRIMOP_TYPE_LE:
            return AEXPPRIMOP_TYPE_LE;
        case LAMPRIMOP_TYPE_VEC:
            return AEXPPRIMOP_TYPE_VEC;
        case LAMPRIMOP_TYPE_XOR:
            return AEXPPRIMOP_TYPE_XOR;
        case LAMPRIMOP_TYPE_MOD:
            return AEXPPRIMOP_TYPE_MOD;
        case LAMPRIMOP_TYPE_CMP:
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

static Aexp *replaceLamCexp(LamExp *apply, LamExpTable *replacements) {
    ENTER(replaceLamCexp);
    if (replacements == NULL) {
        cant_happen("replaceLamCexp called with null replacements");
    }
    HashSymbol *subst = freshSymbol();
    setLamExpTable(replacements, subst, apply);
    IFDEBUG(printLamExpTable(replacements, 0));
    LEAVE(replaceLamCexp);
    return newAexp(AEXP_TYPE_VAR, AEXP_VAL_VAR(subst));
}
