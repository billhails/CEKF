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

#ifdef DEBUG_ANF
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

Exp *anfNormalize(LamExp *lamExp) {
    return normalize(lamExp);
}

static Exp *normalize(LamExp *lamExp) {
    switch (lamExp->type) {
        case LAMEXP_TYPE_LAM:
            return normalizeLam(lamexp->val.lam);
        case LAMEXP_TYPE_VAR:
            return normalizeVar(lamexp->val.var);
        case LAMEXP_TYPE_INTEGER:
            return normalizeInteger(lamexp->val.integer);
        case LAMEXP_TYPE_PRIM:
            return normalizePrim(lamexp->val.prim);
        case LAMEXP_TYPE_UNARY:
            return normalizeUnary(lamexp->val.unary);
        case LAMEXP_TYPE_LIST:
            return normalizeList(lamexp->val.list);
        case LAMEXP_TYPE_MAKEVEC:
            break;
        case LAMEXP_TYPE_APPLY:
            break;
        case LAMEXP_TYPE_IFF:
            break;
        case LAMEXP_TYPE_CALLCC:
            break;
        case LAMEXP_TYPE_LETREC:
            break;
        case LAMEXP_TYPE_LET:
            break;
        case LAMEXP_TYPE_MATCH:
            break;
        case LAMEXP_TYPE_COND:
            break;
        case LAMEXP_TYPE_CHARACTER:
            break;
        case LAMEXP_TYPE_BACK:
            break;
        case LAMEXP_TYPE_ERROR:
            break;
        case LAMEXP_TYPE_COND_DEFAULT:
            break;
        default:
            cant_happen("unrecognized type %d in anfNormalize", lamExp->type);
    }
}

static Exp *wrapAexp(Aexp *aexp) {
    return newExp(EXP_TYPE_AEXP, EXP_VAL_AEXP(aexp));
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
// so the algorithm should be
static Exp *normalizeList(LamSequence *list) {
}

static Exp *normalizeUnary(LamUnaryApp *app) {
    HashTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *exp = replaceLamExp(app->exp, replacements);
    int save2 = PROTECT(exp1);
    AexpUnaryApp *aexpUnaryApp = newAexpUnaryApp(mapUnaryOp(app->type), exp);
    UNPROTECT(save2);
    save2 = PROTECT(aexpUnaryApp);
    Aexp *aexp = newAexp(AEXP_TYPE_UNARY, AEXP_VAL_UNARY(aexpUnaryApp));
    REPLACE_PROTECT(save2, aexp);
    Exp *exp = wrapAexp(aexp);
    REPLACE_PROTECT(save2, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    return res;
}

static Exp *normalizePrim(LamPrimApp *app) {
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
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    return res;
}

static Exp *normalizeVar(HashSymbol *var) {
    Aexp *aexp = aexpNormalizeVar(var);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    return exp;
}

static Exp *normalizeInteger(int integer) {
    Aexp *aexp = aexpNormalizeInteger(integer);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save);
    return exp;
}

static Exp *normalizeLam(LamLam *lamLam) {
    Aexp *aexp = aexpNormalizeLam(lamLam);
    int save = PROTECT(aexp);
    Exp *exp = wrapAexp(aexp);
    UNPROTECT(save):
    return exp;
}

static Aexp *aexpNormalizeLam(LamLam *lamLam) {
    AexpVarList *varList = convertVarList(lamLam->args);
    int save = PROTECT(varList);
    Exp *body = normalize(lamLam->exp);
    PROTECT(body);
    AexpLam *aexpLam = newAexpLam(varList, body);
    PROTECT(aexpLam);
    Aexp *aexp = newAexp(AEXP_TYPE_LAM, AEXP_VAL_LAM(aexpLam));
    UNPROTECT(save);
    return aexp;
}

static HashTable *makeLamExpHashTable() {
    return newHashTable(sizeof(LamExp *), markLamExpFn, printLamExpFn);
}

static Exp *normalizeApply(LamApply *lamApply) {
    HashTable *replacements = makeLamExpHashTable();
    int save = PROTECT(replacements);
    Aexp *function = replaceLamExp(lamApply->function, replacements);
    PROTECT(function);
    AexpList *args = replaceLamList(lamApply->args, replacements);
    PROTECT(args);
    CexpApply *cexpApply = newCexpApply(function, args);
    REPLACE_PROTECT(save, cexpApply);
    Cexp *cexp = newCexp(CEXP_TYPE_APPLY, CEXP_VAL_APPLY(cexpApply));
    REPLACE_PROTECT(save, cexp);
    Exp *exp = newExp(EXP_TYPE_CEXP, EXP_VAL_CEXP(cexp));
    REPLACE_PROTECT(save, exp);
    Exp *res = letBind(exp, replacements);
    UNPROTECT(save);
    return res;
}

static Exp letBind(Exp *body, HashTable *replacements) {
    if (replacements->count == 0) {
        return body;
    }
    HashTable *nextReplacements = makeLamExpHashTable();
    int save = PROTECT(nextReplacements);
    int save2 = PROTECT(body);
    LamExp *lamExpVal = NULL;
    int i = 0;
    HashSymbol *key = NULL;
    while ((key = iterateHashTable(replacements, &i, &lamExpVal)) != NULL) {
        Aexp *aexpVal = replaceLamExp(lamExpVal, nextReplacements);
        int save3 PROTECT(aexpVal);
        Exp *expVal = newExp(EXP_TYPE_AEXP, EXP_VAL_AEXP(aexpVal));
        REPLACE_PROTECT(save3, expVal);
        ExpLet *let = newExpLet(key, expVal, body);
        REPLACE_PROTECT(save3, let);
        body = newExp(EXP_TYPE_LET, EXP_VAL_LET(let));
        REPLACE_PROTECT(save2, body);
        UNPROTECT(save3);
    }
    body = letBind(body, nextReplacements);
    UNPROTECT(save);
    return body;
}

static Aexp *aexpNormalizeVar(HashSymbol *var) {
    return newAexp(AEXP_TYPE_VAR, AEXP_VAL_VAR(var));
}

static Aexp *aexpNormalizeInteger(int integer) {
    return newAexp(AEXP_TYPE_INT, AEXP_VAL_INT(integer));
}

static Aexp *aexpNormalizeChar(char character) {
    return newAexp(AEXP_TYPE_CHAR, AEXP_VAL_CHAR(character));
}

static Aexp *replaceLamExp(LamExp *lamexp, HashTable *replacements) {
    switch (lamexp->type) {
        case LAMEXP_TYPE_LAM:
            return aexpNormalizeLam(lamexp->val.lam);
        case LAMEXP_TYPE_VAR:
            return aexpNormalizeVar(lamexp->val.var);
        case LAMEXP_TYPE_INTEGER:
            return aexpNormalizeInteger(lamexp->val.integer);
        case LAMEXP_TYPE_PRIM:
            return replaceLamPrim(lamexp, replacements); // prim needs lamExp
        case LAMEXP_TYPE_UNARY:
            return replaceLamUnary(lamexp->val.unary, replacements);
        case LAMEXP_TYPE_LIST:
            return replaceLamListAexp(lamexp->val.list, replacements);
        case LAMEXP_TYPE_MAKEVEC:
            return replaceLamMakeVec(lamexp->val.makeVec, replacements);
        case LAMEXP_TYPE_CHARACTER:
            return aexpNormalizeChar(lamexp->val.character);
        case LAMEXP_TYPE_APPLY:
        case LAMEXP_TYPE_IFF:
        case LAMEXP_TYPE_CALLCC:
        case LAMEXP_TYPE_LETREC:
        case LAMEXP_TYPE_LET:
        case LAMEXP_TYPE_MATCH:
        case LAMEXP_TYPE_COND:
        case LAMEXP_TYPE_BACK:
        case LAMEXP_TYPE_ERROR:
            return replaceLamCexp(lamexp, replacements);
        case LAMEXP_TYPE_COND_DEFAULT:
            return replaceCondDefault(replacements);
        default:
            cant_happen("unrecognised type %d in replaceLamExp", lamexp->type);
    }
}

static Aexp *replaceLamMakeVec(LamMakeVec *makeVec, HashTable *replacements) {
    AexpList *aexpList = replaceAexpList(makeVec->args, replacements);
    int save = PROTECT(aexpList);
    AexpMakeVec *aexpMakeVec = newAexpMakeVec(makeVec->nargs, aexpList);
    PROTECT(aexpMakeVec);
    Aexp *res = newAexp(AEXP_TYPE_MAKEVEC, AEXP_VAL_MAKEVEC(aexpMakeVec));
    UNPROTECT(save);
    return res;
}

static Aexp *replaceLamListAexp(LamSequence *list, HashTable *replacements) {
    AexpList *aexpList = replaceLamList(list, replacements);
    int save = PROTECT(aexpList);
    Aexp *res = newAexp(AEXP_TYPE_LIST, AEXP_VAL_LIST(aexpList));
    UNPROTECT(save);
    return res;
}

static AexpList *replaceLamList(LamSequence *list, HashTable *replacements) {
    if (list == NULL) {
        return NULL;
    }
    AexpList *next = replaceLamList(list->next, replacemets);
    int save = PROTECT(next);
    Aexp *val = replaceLamExp(list->exp, replacements);
    PROTECT(val);
    AexpList *res = newAexpList(val, next);
    UNPROTECT(save);
    return res;
}

static Aexp *replaceLamPrim(LamExp *lamExp, HashTable *replacements) {
    LamPrimApp *lamPrimApp = lamExp->val.prim;
    if (opIsCexp(lamPrimApp->type)) {
        return replaceLamCexp(lamExp, replacements);
    }
    Aexp *exp1 = replaceLamExp(lamPrimApp->exp1, replacements);
    int save = PROTECT(exp1);
    Aexp *exp2 = replaceLamExp(lamPrimApp->exp2, replacements);
    PROTECT(exp2);
    AexpPrimApp *prim = newAexpPrimApp(mapPrimOp(lamPrimApp->type), exp1, exp2);
    PROTECT(prim);
    Aexp *res = newAexp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(prim));
    UNPROTECT(save);
    return res;
}

static Aexp *replaceLamUnary(LamUnaryApp *lamUnaryApp, HashTable *replacements) {
    Aexp *exp = replaceLamExp(lamUnaryApp->exp, replacements);
    int save = PROTECT(exp);
    AexpUnaryApp *unary = newAexpUnaryApp(mapUnaryOp(lamUnaryApp->type), exp);
    PROTECT(unary);
    Aexp *res = newAexp(AEXP_TYPE_UNARY, AEXP_VAL_UNARY(unary));
    UNPROTECT(save);
    return res;
}

static AexpUnaryOp mapUnaryOp(LamUnaryOp *op) {
    switch(op) {
        case LAMUNARYOP_TYPE_LAM_UNARY_NOT:
            return AEXP_UNARY_NOT;
        case LAMUNARYOP_TYPE_LAM_UNARY_PRINT:
            return AEXP_UNARY_PRINT;
        default:
            cant_happen("unrecognised type %d in mapUnaryOp", op);
    }
}

static bool opIsCexp(LamPrimOp op) {
    switch (op) {
        case LAMPRIMOP_TYPE_LAM_PRIM_AND:
        case LAMPRIMOP_TYPE_LAM_PRIM_OR:
        case LAMPRIMOP_TYPE_LAM_PRIM_AMB:
            return true;
        default:
            return false;
    }
}

static AexpPrimOp *mapPrimOp(LamPrimOp op) {
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
        case LAMPRIMOP_TYPE_LAM_PRIM_AND:
            cant_happen("mapPrimOp got bool 'AND' (cexp)");
        case LAMPRIMOP_TYPE_LAM_PRIM_OR:
            cant_happen("mapPrimOp got bool 'OR' (cexp)");
        case LAMPRIMOP_TYPE_LAM_PRIM_AMB:
            cant_happen("mapPrimOp got amb (cexp)");
        default:
            cant_happen("unrecognised op type %d in mapPrimOp", op);
    }
}

static Aexp *replaceLamCexp(LamExp *apply, HashTable *replacements) {
    HashSymbol *subst = genSym("anf$");
    hashSet(replacements, subst, &apply);
    return newAexp(AEXP_TYPE_VAR, AEXP_VAL_VAR(subst));
}
