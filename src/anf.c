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

#include "anf.h"
#include "common.h"
#include "hash.h"
#include "symbol.h"
#include "memory.h"
#include "lambda_helper.h"

#ifdef DEBUG_ANF
#include <stdio.h>
#include <unistd.h>
#include "debug.h"
#include "lambda_pp.h"
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static Exp *normalize(LamExp *lamExp, Exp *tail);
static Exp *normalizeLam(LamLam *lamLam, Exp *tail);
static Exp *normalizeVar(HashSymbol *var, Exp *tail);
static Exp *normalizeInteger(int integer, Exp *tail);
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
static HashTable *makeLamExpHashTable();
static Aexp *replaceLamExp(LamExp *lamExp, HashTable *replacements);
static AexpUnaryOp mapUnaryOp(LamUnaryOp op);
static Exp *letBind(Exp *body, HashTable *replacements);
static AexpPrimOp mapPrimOp(LamPrimOp op);
static Aexp *aexpNormalizeVar(HashSymbol *var);
static Aexp *aexpNormalizeInteger(int integer);
static Aexp *aexpNormalizeCharacter(char character);
static Aexp *aexpNormalizeLam(LamLam *lamLam);
static AexpVarList *convertVarList(LamVarList *args);
static AexpList *replaceLamList(LamList *list, HashTable *replacements);
static Aexp *replaceLamPrim(LamExp *lamExp, HashTable *replacements);
static Aexp *replaceLamUnary(LamUnaryApp *lamUnaryApp, HashTable *replacements);
static Aexp *replaceLamMakeVec(LamMakeVec *makeVec, HashTable *replacements);
static Aexp *replaceLamCexp(LamExp *apply, HashTable *replacements);
static Exp *normalizeMakeVec(LamMakeVec *makeVec, Exp *tail);
static Exp *wrapTail(Exp *exp, Exp *tail);
static Exp *normalizeIff(LamIff *lamIff, Exp *tail);
static Exp *normalizeCallCc(LamExp *callcc, Exp *tail);
static Exp *normalizeLetRec(LamLetRec *lamLetRec, Exp *tail);
static LetRecBindings *replaceLetRecBindings(LamLetRecBindings *lamLetRecBindings, HashTable *replacements);
static Exp *normalizeLet(LamLet *lamLet, Exp *tail);
static Exp *normalizeMatch(LamMatch *match, Exp *tail);
static MatchList *normalizeMatchList(LamMatchList *matchList);
static AexpList *convertIntList(LamIntList *list);
static Exp *normalizeCond(LamCond *cond, Exp *tail);
static Exp *replaceCond(Aexp *value, LamCondCases *cases, HashTable *replacements);
static CexpLetRec *replaceCexpLetRec(CexpLetRec *cexpLetRec, LamLetRecBindings *lamLetRecBindings);

Exp *anfNormalize(LamExp *lamExp) {
    return normalize(lamExp, NULL);
}

static Exp *normalize(LamExp *lamExp, Exp *tail) {
    ENTER(normalize);
#ifdef DEBUG_ANF
    ppLamExp(lamExp);
    NEWLINE();
    // printLamExp(lamExp, 0);
    // NEWLINE();
#endif
    switch (lamExp->type) {
        case LAMEXP_TYPE_LAM:
            return normalizeLam(lamExp->val.lam, tail);
        case LAMEXP_TYPE_VAR:
            return normalizeVar(lamExp->val.var, tail);
        case LAMEXP_TYPE_INTEGER:
            return normalizeInteger(lamExp->val.integer, tail);
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
        case LAMEXP_TYPE_APPLY:
            return normalizeApply(lamExp->val.apply, tail);
        case LAMEXP_TYPE_IFF:
            return normalizeIff(lamExp->val.iff, tail);
        case LAMEXP_TYPE_CALLCC:
            return normalizeCallCc(lamExp->val.callcc, tail);
        case LAMEXP_TYPE_LETREC:
            return normalizeLetRec(lamExp->val.letrec, tail);
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
        case LAMEXP_TYPE_COND_DEFAULT:
            cant_happen("normalize encountered cond default");
        default:
            cant_happen("unrecognized type %d in normalize", lamExp->type);
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
    HashTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *value = replaceLamExp(cond->value, replacements);
    int save2 = PROTECT(value);
    Exp *exp = replaceCond(value, cond->cases, replacements);
    REPLACE_PROTECT(save2, exp);
    exp = wrapTail(exp, tail);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    LEAVE(normalizeCond);
    return res;
}

static Exp *normalizeMatch(LamMatch *match, Exp *tail) {
    ENTER(normalizeMatch);
    HashTable *replacements = makeLamExpHashTable();
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
    AexpList *matches = convertIntList(matchList->matches);
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

static Exp *normalizeLetRec(LamLetRec *lamLetRec, Exp *tail) {
    ENTER(normalizeLetRec);
#ifdef DEBUG_ANF
    ppLamLetRec(lamLetRec);
    NEWLINE();
#endif
    Exp* body = normalize(lamLetRec->body, tail);
    int save = PROTECT(body);
    CexpLetRec *cexpLetRec = newCexpLetRec(NULL, body);
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

/*
static Exp *normalizeLetRec(LamLetRec *lamLetRec, Exp *tail) {
    ENTER(normalizeLetRec);
#ifdef DEBUG_ANF
    ppLamLetRec(lamLetRec);
    NEWLINE();
#endif
    HashTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    LetRecBindings *bindings = replaceLetRecBindings(lamLetRec->bindings, replacements);
    PROTECT(bindings);
    Exp* body = normalize(lamLetRec->body, tail);
    PROTECT(body);
    body = letBind(body, replacements);
    PROTECT(body);
    CexpLetRec *cexpLetRec = newCexpLetRec(bindings, body);
    PROTECT(cexpLetRec);
    Cexp *cexp = newCexp(CEXP_TYPE_LETREC, CEXP_VAL_LETREC(cexpLetRec));
    PROTECT(cexp);
    Exp *exp = wrapCexp(cexp);
    UNPROTECT(save);
    LEAVE(normalizeLetRec);
    return exp;
}
*/

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
    HashTable *replacements = makeLamExpHashTable();
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

static Exp *normalizeIff(LamIff *lamIff, Exp *tail) {
    ENTER(normalizeIff);
    HashTable *replacements = makeLamExpHashTable();
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
    Cexp *cexp = newCexp(CEXP_TYPE_IF, CEXP_VAL_IF(cexpIf));
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
    HashTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    DEBUG("calling replaceLamList");
    AexpList *args = replaceLamList(lamMakeVec->args, replacements);
    int save2 = PROTECT(args);
    AexpMakeVec *aexpMakeVec = newAexpMakeVec(args);
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
    HashTable *replacements = makeLamExpHashTable();
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
    HashTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *exp1 = replaceLamExp(app->exp1, replacements);
    int save2 = PROTECT(exp1);
    Aexp *exp2 = replaceLamExp(app->exp2, replacements);
    PROTECT(exp2);
    AexpPrimApp *aexpPrimApp = newAexpPrimApp(mapPrimOp(app->type), exp1, exp2);
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
    CexpBool *and = newCexpBool(BOOL_TYPE_AND, left, right);
    PROTECT(and);
    Cexp *cexp = newCexp(CEXP_TYPE_BOOL, CEXP_VAL_BOOL(and));
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
    CexpBool *or = newCexpBool(BOOL_TYPE_OR, left, right);
    PROTECT(or);
    Cexp *cexp = newCexp(CEXP_TYPE_BOOL, CEXP_VAL_BOOL(or));
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

static Exp *normalizeInteger(int integer, Exp *tail) {
    ENTER(normalizeInteger);
    if (tail != NULL) {
        LEAVE(normalizeInteger);
        return tail;
    }
    Aexp *aexp = aexpNormalizeInteger(integer);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    LEAVE(normalizeInteger);
    return exp;
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
    AexpLam *aexpLam = newAexpLam(varList, body);
    PROTECT(aexpLam);
    Aexp *aexp = newAexp(AEXP_TYPE_LAM, AEXP_VAL_LAM(aexpLam));
    UNPROTECT(save);
    LEAVE(aexpNormalizeLam);
    return aexp;
}

static AexpList *convertIntList(LamIntList *list) {
    ENTER(convertIntList);
    if (list == NULL) {
        LEAVE(convertIntList);
        return NULL;
    }
    AexpList *next = convertIntList(list->next);
    int save = PROTECT(next);
    Aexp *aexp = newAexp(AEXP_TYPE_INT, AEXP_VAL_INT(list->item));
    PROTECT(aexp);
    AexpList *this = newAexpList(next, aexp);
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
    AexpVarList *this = newAexpVarList(next, args->var);
    UNPROTECT(save);
    LEAVE(convertVarList);
    return this;
}

static HashTable *makeLamExpHashTable() {
    return newHashTable(sizeof(LamExp *), markLamExpFn, printLamExpFn);
}

static Exp *normalizeApply(LamApply *lamApply, Exp *tail) {
    ENTER(normalizeApply);
    HashTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *function = replaceLamExp(lamApply->function, replacements);
    int save2 = PROTECT(function);
    DEBUG("calling replaceLamList");
    AexpList *args = replaceLamList(lamApply->args, replacements);
    PROTECT(args);
    DEBUG("back from replaceLamList");
#ifdef DEBUG_ANF
    printHashTable(replacements, 0);
    NEWLINE();
#endif
    CexpApply *cexpApply = newCexpApply(function, args);
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

static Exp *letBind(Exp *body, HashTable *replacements) {
    ENTER(letBind);
    // DEBUG("sleep %d", sleep(1));
#ifdef DEBUG_ANF
    printExp(body);
    NEWLINE();
    printHashTable(replacements, 0);
    NEWLINE();
#endif
    if (replacements->count == 0) {
        LEAVE(letBind);
        return body;
    }
    int save = PROTECT(body);
    LamExp *lamExpVal = NULL;
    int i = 0;
    HashSymbol *key = NULL;
    while ((key = iterateHashTable(replacements, &i, &lamExpVal)) != NULL) {
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

static Aexp *aexpNormalizeInteger(int integer) {
    return newAexp(AEXP_TYPE_INT, AEXP_VAL_INT(integer));
}

static Aexp *aexpNormalizeCharacter(char character) {
    return newAexp(AEXP_TYPE_CHAR, AEXP_VAL_CHAR(character));
}

static Aexp *cloneAexp(Aexp *orig) {
    if (orig->type == AEXP_TYPE_VAR) {
        return newAexp(AEXP_TYPE_VAR, AEXP_VAL_VAR(orig->val.var));
    }
    return orig;
}

static Exp *replaceCond(Aexp *value, LamCondCases *cases, HashTable *replacements) {
    ENTER(replaceCond);
    if (cases == NULL) {
        cant_happen("empty cases in replaceCond");
    }
    Aexp *constant = replaceLamExp(cases->constant, replacements);
    int save = PROTECT(constant);
    Exp *consequent = normalize(cases->body, NULL);
    PROTECT(consequent);
    if (constant->type == AEXP_TYPE_DEFAULT) {
        if (cases->next != NULL) {
            cant_happen("cond cases after default");
        }
        UNPROTECT(save);
        LEAVE(replaceCond);
        return consequent;
    }
    if (cases->next == NULL) {
        cant_happen("cond cases with no default");
    }
    Exp *alternative = replaceCond(value, cases->next, replacements);
    PROTECT(alternative);
    Aexp *clone = cloneAexp(value);
    PROTECT(clone);
    AexpPrimApp *eq = newAexpPrimApp(AEXP_PRIM_EQ, clone, constant);
    PROTECT(eq);
    Aexp *condition = newAexp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(eq));
    PROTECT(condition);
    CexpIf *iff = newCexpIf(condition, consequent, alternative);
    PROTECT(iff);
    Cexp *cexp = newCexp(CEXP_TYPE_IF, CEXP_VAL_IF(iff));
    PROTECT(cexp);
    Exp *exp = wrapCexp(cexp);
    UNPROTECT(save);
    LEAVE(replaceCond);
    return exp;
}


static Aexp *replaceCondDefault() {
    return newAexp(AEXP_TYPE_DEFAULT, AEXP_VAL_DEFAULT());
}


static Aexp *replaceLamExp(LamExp *lamExp, HashTable *replacements) {
    ENTER(replaceLamExp);
    Aexp *res = NULL;
    switch (lamExp->type) {
        case LAMEXP_TYPE_LAM:
            res = aexpNormalizeLam(lamExp->val.lam);
            break;
        case LAMEXP_TYPE_VAR:
            res = aexpNormalizeVar(lamExp->val.var);
            break;
        case LAMEXP_TYPE_INTEGER:
            res = aexpNormalizeInteger(lamExp->val.integer);
            break;
        case LAMEXP_TYPE_PRIM:
            res = replaceLamPrim(lamExp, replacements); // prim needs lamExp
            break;
        case LAMEXP_TYPE_UNARY:
            res = replaceLamUnary(lamExp->val.unary, replacements);
            break;
        case LAMEXP_TYPE_MAKEVEC:
            res = replaceLamMakeVec(lamExp->val.makeVec, replacements);
            break;
        case LAMEXP_TYPE_CHARACTER:
            res = aexpNormalizeCharacter(lamExp->val.character);
            break;
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
            res = replaceLamCexp(lamExp, replacements);
            break;
        case LAMEXP_TYPE_COND_DEFAULT:
            res = replaceCondDefault();
            break;
        default:
            cant_happen("unrecognised type %d in replaceLamExp", lamExp->type);
    }
    LEAVE(replaceLamExp);
    return res;
}

static bool lamExpIsConst(LamExp *val) {
    switch (val->type) {
        case LAMEXP_TYPE_LAM:
            return true;
        case LAMEXP_TYPE_VAR:
        case LAMEXP_TYPE_INTEGER:
        case LAMEXP_TYPE_CHARACTER:
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
        case LAMEXP_TYPE_LET:
        case LAMEXP_TYPE_MATCH:
        case LAMEXP_TYPE_COND:
        case LAMEXP_TYPE_AND:
        case LAMEXP_TYPE_OR:
        case LAMEXP_TYPE_MAKEVEC:
            return false;
        case LAMEXP_TYPE_COND_DEFAULT:
            cant_happen("lamExpIsConst encountered cond default");
        default:
            cant_happen("unrecognised LamExp type %d in lamExpIsConst", val->type);
    }
}

static CexpLetRec *replaceCexpLetRec(CexpLetRec *cexpLetRec, LamLetRecBindings *lamLetRecBindings) {
    if (lamLetRecBindings == NULL) {
        return cexpLetRec;
    }
    cexpLetRec = replaceCexpLetRec(cexpLetRec, lamLetRecBindings->next);
    int save = PROTECT(cexpLetRec);
    if (lamExpIsConst(lamLetRecBindings->val)) {
        Aexp *val = replaceLamExp(lamLetRecBindings->val, NULL);
        PROTECT(val);
        cexpLetRec->bindings = newLetRecBindings(cexpLetRec->bindings, lamLetRecBindings->var, val);
        cexpLetRec->nbindings++;
    } else {
        Exp *val = normalize(lamLetRecBindings->val, NULL);
        PROTECT(val);
        Exp *exp = NULL;
        if (cexpLetRec->bindings != NULL) {
            Cexp *cexp = newCexp(CEXP_TYPE_LETREC, CEXP_VAL_LETREC(cexpLetRec));
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
        cexpLetRec = newCexpLetRec(NULL, exp);
    }
    UNPROTECT(save);
    return cexpLetRec;
}

/*
static LetRecBindings *replaceLetRecBindings(LamLetRecBindings *lamLetRecBindings, HashTable *replacements) {
    ENTER(replaceLetRecBindings);
    if (lamLetRecBindings == NULL) {
        LEAVE(replaceLetRecBindings);
        return NULL;
    }
    LetRecBindings *next = replaceLetRecBindings(lamLetRecBindings->next, replacements);
    int save = PROTECT(next);
    Aexp *val = replaceLamExp(lamLetRecBindings->val, replacements);
    PROTECT(val);
    LetRecBindings *this = newLetRecBindings(next, lamLetRecBindings->var, val);
    UNPROTECT(save);
    LEAVE(replaceLetRecBindings);
    return this;
}
*/

static Aexp *replaceLamMakeVec(LamMakeVec *makeVec, HashTable *replacements) {
    ENTER(replaceLamMakeVec);
    DEBUG("calling replaceLamList");
    AexpList *aexpList = replaceLamList(makeVec->args, replacements);
    int save = PROTECT(aexpList);
    AexpMakeVec *aexpMakeVec = newAexpMakeVec(aexpList);
    PROTECT(aexpMakeVec);
    Aexp *res = newAexp(AEXP_TYPE_MAKEVEC, AEXP_VAL_MAKEVEC(aexpMakeVec));
    UNPROTECT(save);
    LEAVE(replaceLamMakeVec);
    return res;
}

static AexpList *replaceLamList(LamList *list, HashTable *replacements) {
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
    AexpList *res = newAexpList(next, val);
    UNPROTECT(save);
    LEAVE(replaceLamList);
    return res;
}

static Aexp *replaceLamPrim(LamExp *lamExp, HashTable *replacements) {
    ENTER(replaceLamPrim);
    LamPrimApp *lamPrimApp = lamExp->val.prim;
    Aexp *exp1 = replaceLamExp(lamPrimApp->exp1, replacements);
    int save = PROTECT(exp1);
    Aexp *exp2 = replaceLamExp(lamPrimApp->exp2, replacements);
    PROTECT(exp2);
    AexpPrimApp *prim = newAexpPrimApp(mapPrimOp(lamPrimApp->type), exp1, exp2);
    PROTECT(prim);
    Aexp *res = newAexp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(prim));
    UNPROTECT(save);
    LEAVE(replaceLamPrim);
    return res;
}

static Aexp *replaceLamUnary(LamUnaryApp *lamUnaryApp, HashTable *replacements) {
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
    switch(op) {
        case LAMUNARYOP_TYPE_LAM_UNARY_NOT:
            return AEXP_UNARY_NOT;
        case LAMUNARYOP_TYPE_LAM_UNARY_PRINT:
            return AEXP_UNARY_PRINT;
        default:
            cant_happen("unrecognised type %d in mapUnaryOp", op);
    }
}

static AexpPrimOp mapPrimOp(LamPrimOp op) {
    switch (op) {
        case LAMPRIMOP_TYPE_LAM_PRIM_ADD:
            return AEXP_PRIM_ADD;
        case LAMPRIMOP_TYPE_LAM_PRIM_SUB:
            return AEXP_PRIM_SUB;
        case LAMPRIMOP_TYPE_LAM_PRIM_MUL:
            return AEXP_PRIM_MUL;
        case LAMPRIMOP_TYPE_LAM_PRIM_DIV:
            return AEXP_PRIM_DIV;
        case LAMPRIMOP_TYPE_LAM_PRIM_EQ:
            return AEXP_PRIM_EQ;
        case LAMPRIMOP_TYPE_LAM_PRIM_NE:
            return AEXP_PRIM_NE;
        case LAMPRIMOP_TYPE_LAM_PRIM_GT:
            return AEXP_PRIM_GT;
        case LAMPRIMOP_TYPE_LAM_PRIM_LT:
            return AEXP_PRIM_LT;
        case LAMPRIMOP_TYPE_LAM_PRIM_GE:
            return AEXP_PRIM_GE;
        case LAMPRIMOP_TYPE_LAM_PRIM_LE:
            return AEXP_PRIM_LE;
        case LAMPRIMOP_TYPE_LAM_PRIM_VEC:
            return AEXP_PRIM_VEC;
        case LAMPRIMOP_TYPE_LAM_PRIM_XOR:
            return AEXP_PRIM_XOR;
        case LAMPRIMOP_TYPE_LAM_PRIM_MOD:
            return AEXP_PRIM_MOD;
        default:
            cant_happen("unrecognised op type %d in mapPrimOp", op);
    }
}

static HashSymbol *freshSymbol() {
    HashSymbol *res = genSym("anf$");
#ifdef DEBUG_ANF
    DEBUG("freshHashSymbol %s", res->name);
#endif
    return res;
}

static Aexp *replaceLamCexp(LamExp *apply, HashTable *replacements) {
    ENTER(replaceLamCexp);
    if (replacements == NULL) {
        cant_happen("replaceLamCexp called with null replacements");
    }
    HashSymbol *subst = freshSymbol();
    hashSet(replacements, subst, &apply);
#ifdef DEBUG_ANF
    printHashTable(replacements, 0);
    NEWLINE();
#endif
    LEAVE(replaceLamCexp);
    return newAexp(AEXP_TYPE_VAR, AEXP_VAL_VAR(subst));
}

