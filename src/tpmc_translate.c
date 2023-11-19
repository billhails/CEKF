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
 *
 * Term Pattern Matching Compiler stage 4. code generation
 */

#include <stdio.h>
#include <string.h>
#include "lambda.h"
#include "tpmc.h"
#include "symbol.h"
#include "common.h"

#ifdef DEBUG_TPMC_TRANSLATE
#include "debug_tpmc.h"
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static LamExp *translateStateToInlineCode(TpmcState *dfa, HashTable *lambdaCache);
static LamExp *translateState(TpmcState *dfa, HashTable *lambdaCache);

static void markLamExpFn(void *ptr) {
    markLamExp(*((LamExp **) ptr));
}

static void printLamExpFn(void *ptr, int depth) {
    ppLamExpD(*((LamExp **) ptr), depth);
}

static HashSymbol *makeLambdaName(TpmcState *state) {
    char buf[80];
    sprintf(buf, "$tpmc%d", state->stamp);
    return newSymbol(buf);
}

static LamVarList *makeCanonicalArgs(HashTable *freeVariables) {
    ENTER(makeCanonicalArgs);
    if (freeVariables->count == 0) {
        LEAVE(makeCanonicalArgs);
        return NULL;
    }
    TpmcVariableArray *sorted = newTpmcVariableArray();
    int save = PROTECT(sorted);
    int i = 0;
    HashSymbol *key;
    while ((key = iterateHashTable(freeVariables, &i, NULL)) != NULL) {
        pushTpmcVariableArray(sorted, key);
        for (int i = sorted->size - 1; i > 0; i--) {
            if (strcmp(sorted->entries[i-1]->name, sorted->entries[i]->name) > 0) {
                key = sorted->entries[i];
                sorted->entries[i] = sorted->entries[i-1];
                sorted->entries[i-1] = key;
            } else {
                break;
            }
        }
    }
    // claim an extra slot
    int save2 = PROTECT(sorted);
    LamVarList *res = NULL;
    for (int i = 0; i < sorted->size; i++) {
        res = newLamVarList(sorted->entries[i], res);
        REPLACE_PROTECT(save2, res);
    }
    UNPROTECT(save);
    LEAVE(makeCanonicalArgs);
    return res;
}

static LamList *convertVarListToList(LamVarList *vars) {
    ENTER(convertVarListToList);
    if (vars == NULL) {
        LEAVE(convertVarListToList);
        return NULL;
    }
    LamList *next = convertVarListToList(vars->next);
    int save = PROTECT(next);
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(vars->var));
    DEBUG("[newLamExp]");
    PROTECT(exp);
    LamList *this = newLamList(exp, next);
    UNPROTECT(save);
    LEAVE(convertVarListToList);
    return this;
}

static LamExp *translateToApply(HashSymbol *name, TpmcState *dfa) {
    ENTER(translateToApply);
    LamExp *function = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    DEBUG("[newLamExp]");
    int save = PROTECT(function);
    LamVarList *cargs = makeCanonicalArgs(dfa->freeVariables);
    PROTECT(cargs);
    LamList *args = convertVarListToList(cargs);
    PROTECT(args);
    LamApply *apply = newLamApply(function, dfa->freeVariables->count, args);
    PROTECT(apply);
    LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    DEBUG("[newLamExp]");
    UNPROTECT(save);
    LEAVE(translateToApply);
    return res;
}

static LamExp *translateToLambda(TpmcState *dfa, HashTable *lambdaCache) {
    ENTER(translateToLambda);
    LamExp *exp = translateStateToInlineCode(dfa, lambdaCache);
    int save = PROTECT(exp);
    LamVarList *args = makeCanonicalArgs(dfa->freeVariables);
    PROTECT(args);
    LamLam *lambda = newLamLam(dfa->freeVariables->count, args, exp);
    PROTECT(lambda);
    LamExp *res = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(lambda));
    DEBUG("[newLamExp]");
    UNPROTECT(save);
    LEAVE(translateToLambda);
    return res;
}

#ifdef DEBUG_LEAK
extern bool hash_debug_flag;
#endif

static LamExp *storeLambdaAndTranslateToApply(TpmcState *dfa, HashTable *lambdaCache) {
    ENTER(storeLambdaAndTranslateToApply);
    HashSymbol *name = makeLambdaName(dfa);
    if (!hashGet(lambdaCache, name, NULL)) {
        LamExp *lambda = translateToLambda(dfa, lambdaCache);
        int save = PROTECT(lambda);
        hashSet(lambdaCache, name, &lambda);
        UNPROTECT(save);
    }
    LamExp *res = translateToApply(name, dfa);
    LEAVE(storeLambdaAndTranslateToApply);
    return res;
}

static LamExp *translateComparisonArcToTest(TpmcArc *arc) {
    ENTER(translateComparisonArcToTest);
    if (arc->test->pattern->type != TPMCPATTERNVALUE_TYPE_COMPARISON) {
        cant_happen("translateComparisonArcToTest ecncountered non-comparison type %d", arc->test->pattern->type);
    }
    TpmcComparisonPattern *pattern = arc->test->pattern->val.comparison;
    LamExp *a = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(pattern->previous->path));
    DEBUG("[newLamExp]");
    int save = PROTECT(a);
    LamExp *b = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(pattern->current->path));
    DEBUG("[newLamExp]");
    PROTECT(b);
    LamPrimApp *eq = newLamPrimApp(LAMPRIMOP_TYPE_LAM_PRIM_EQ, a, b);
    PROTECT(eq);
    LamExp *res = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(eq));
    DEBUG("[newLamExp]");
    UNPROTECT(save);
    LEAVE(translateComparisonArcToTest);
    return res;
}

static LamExp *prependLetBindings(TpmcPattern *test, HashTable *freeVariables, LamExp *body) {
    ENTER(prependLetBindings);
    if (test->pattern->type != TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        cant_happen("prependLetBindings passed non-constructor %d", test->pattern->type);
    }
    TpmcConstructorPattern *constructor = test->pattern->val.constructor;
    if (constructor->components->size == 0) {
        return body;
    }
    int save = PROTECT(body);
    for (int i = 0; i < constructor->components->size; i++) {
        // (let (var (vec i+1 base)) body)
        HashSymbol *path = constructor->components->entries[i]->path;
        if (hashGet(freeVariables, path, NULL)) {
            LamExp *index = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(i + 1));
            DEBUG("[newLamExp]");
            int save2 = PROTECT(index);
            LamExp *base = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(test->path));
            DEBUG("[newLamExp]");
            PROTECT(base);
            LamPrimApp *vec = newLamPrimApp(LAMPRIMOP_TYPE_LAM_PRIM_VEC, index, base);
            PROTECT(vec);
            LamExp *vecExp = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(vec));
            DEBUG("[newLamExp]");
            PROTECT(vecExp);
            LamLet *let = newLamLet(path, vecExp, body);
            PROTECT(let);
            body = newLamExp(LAMEXP_TYPE_LET, LAMEXP_VAL_LET(let));
            DEBUG("[newLamExp]");
            REPLACE_PROTECT(save, body);
            UNPROTECT(save2);
        }
    }
    LEAVE(prependLetBindings);
    return body;
}

static LamExp *translateArcToCode(TpmcArc *arc, HashTable *lambdaCache) {
    ENTER(translateArcToCode);
    LamExp *res = translateState(arc->state, lambdaCache);
    if (arc->test->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        int save = PROTECT(res);
        res = prependLetBindings(arc->test, arc->state->freeVariables, res);
        UNPROTECT(save);
    }
    LEAVE(translateArcToCode);
    return res;
}

static LamExp *translateComparisonArcAndAlternativeToIf(TpmcArc *arc, HashTable *lambdaCache, LamExp *alternative) {
    ENTER(translateComparisonArcAndAlternativeToIf);
    // (if (eq p$0 p$1) ... ...) ; both variables
    LamExp *test = translateComparisonArcToTest(arc);
    int save = PROTECT(test);
    LamExp *consequent = translateArcToCode(arc, lambdaCache);
    DEBUG("translateArcToCode returned %p", consequent);
    PROTECT(consequent);
    LamIff *iff = newLamIff(test, consequent, alternative);
    PROTECT(iff);
    LamExp *res = newLamExp(LAMEXP_TYPE_IFF, LAMEXP_VAL_IFF(iff));
    DEBUG("[newLamExp]");
    UNPROTECT(save);
    LEAVE(translateComparisonArcAndAlternativeToIf);
    return res;
}

static TpmcArcList *_arcArrayToList(TpmcArcArray *arcArray, int index) {
    if (index >= arcArray->size) {
        return NULL;
    }
    TpmcArcList *next = _arcArrayToList(arcArray, index + 1);
    int save = PROTECT(next);
    TpmcArcList *this = newTpmcArcList(arcArray->entries[index], next);
    UNPROTECT(save);
    return this;
}

static TpmcArcList *arcArrayToList(TpmcArcArray *arcArray) {
    return _arcArrayToList(arcArray, 0);
}

// may need a context i.e. the type of test this is contained in for index > 0
//
// The state consists of a variable to be tested, and an ordered list of arcs to other states.
// Each arc has a pattern to match and a result state.
// The pattern can be a comparison, a constant, a constructor or a wildcard.
// Any one state cannot have arcs with both constructor and constant patterns.
// Only the last arc may have a wildcard pattern.
//
// So in the absence of comparisons, everything would be simple.
// either a list of constants followed by a wildcard, or a list of constructors, optionally
// followed by a wildcard if the list of constructors is not exhaustive.
// we would either construct a (cond ... (default ...)) test for constants, or a
// (match ... ((n m) ...)) test for constructors, wher (n m) is the unexhausted constructor indices.
//
// The presence of a comparison test forces us to interject a premature default clause for the
// constants or constructors so far encountered, followed by a new cond or match clause for the remainder.
//
// example:
// fn awkward {
//   (_, 'a') { 'A' }
//   (_, 'b') { 'B' }
//   (x, x)   { '!' }
//   (_, 'c') { 'C' }
//   (_, _)   { 'D' }
// }
//
// (cond p$1 ('a' 'A')
//           ('b' 'B')
//           (default
//             (if (eq p$0 p$1)
//                 '!'
//                 (cond p$1 ('c' 'C')
//                           (default 'D')))))
// 
static LamExp *translateArcList(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache);
static LamCondCases *translateConstantArcList(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache);
static LamMatchList *translateConstructorArcList(TpmcArcList *arcList, LamExp *testVar, LamIntList *unexhaustedIndices, HashTable *lambdaCache);

static LamExp *translateTestState(TpmcTestState *testState, HashTable *lambdaCache) {
    ENTER(translateTestState);
    TpmcArcList *arcList = arcArrayToList(testState->arcs);
    int save = PROTECT(arcList);
    LamExp *testVar = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(testState->path));
    DEBUG("[newLamExp]");
    PROTECT(testVar);
    LamExp *res = translateArcList(arcList, testVar, lambdaCache);
    UNPROTECT(save);
    LEAVE(translateTestState);
    return res;
}

static LamIntList *makeUnexhaustedIndices(LamTypeConstructorInfo *info) {
    ENTER(makeUnexhaustedIndices);
    LamIntList *res = NULL;
    int save = PROTECT(res);
    for (int i = 0; i < info->size; ++i) {
        res = newLamIntList(i, res);
        PROTECT(res);
    }
    UNPROTECT(save);
    LEAVE(makeUnexhaustedIndices);
    return res;
}

static LamIntList *removeIndex(int index, LamIntList *indices) {
    ENTER(removeIndices);
    LamIntList *res;
    // assumes only one occurrence
    if (indices == NULL) {
        res = NULL;
    } else if (indices->item == index) {
        res = indices->next;
    } else {
        indices->next = removeIndex(index, indices->next);
        res = indices;
    }
    LEAVE(removeIndices);
    return res;
}

static LamExp *translateComparisonArcListToIf(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache) {
    LamExp *rest = translateArcList(arcList->next, testVar, lambdaCache);
    int save = PROTECT(rest);
    LamExp *res = translateComparisonArcAndAlternativeToIf(arcList->arc, lambdaCache, rest);
    UNPROTECT(save);
    return res;
}

static LamExp *translateArcList(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache) {
    ENTER(translateArcList);
    if (arcList == NULL) {
        cant_happen("ran out of arcs in translateArcList");
    }
    LamExp *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON: {
            res = translateComparisonArcListToIf(arcList, testVar, lambdaCache);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
        case TPMCPATTERNVALUE_TYPE_INTEGER: {
            LamCondCases *cases = translateConstantArcList(arcList, testVar, lambdaCache);
            int save = PROTECT(cases);
            LamCond *cond = newLamCond(testVar, cases);
            PROTECT(cond);
            res = newLamExp(LAMEXP_TYPE_COND, LAMEXP_VAL_COND(cond));
            DEBUG("[newLamExp]");
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_WILDCARD: {
            LamExp *res = translateState(arcList->arc->state, lambdaCache);
            return res;
        }
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR: {
            LamTypeConstructorInfo *info = arcList->arc->test->pattern->val.constructor->info;
            LamIntList *unexhaustedIndices = makeUnexhaustedIndices(info);
            int save = PROTECT(unexhaustedIndices);
            LamMatchList *matches = translateConstructorArcList(arcList, testVar, unexhaustedIndices, lambdaCache);
            PROTECT(matches);
            LamExp *testExp = NULL;
            if (info->vec) {
                LamExp *zero = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(0));
                DEBUG("[newLamExp]");
                PROTECT(zero);
                LamPrimApp *vec = newLamPrimApp(LAMPRIMOP_TYPE_LAM_PRIM_VEC, zero, testVar);
                PROTECT(vec);
                testExp = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(vec));
                DEBUG("[newLamExp]");
                PROTECT(testExp);
            } else {
                testExp = testVar;
            }
            LamMatch *match = newLamMatch(testExp, matches);
            PROTECT(match);
            res = newLamExp(LAMEXP_TYPE_MATCH, LAMEXP_VAL_MATCH(match));
            DEBUG("[newLamExp]");
            UNPROTECT(save);
            break;
        }
        default:
            cant_happen("unrecognized pattern type %d in translateArcList", arcList->arc->test->pattern->type);
    }
    LEAVE(translateArcList);
    return res;
}

static LamCondCases *makeConstantCondCase(TpmcArcList *arcList, LamExp *constant, LamExp *testVar, HashTable *lambdaCache) {
    LamCondCases *next = translateConstantArcList(arcList->next, testVar, lambdaCache);
    int save = PROTECT(next);
    LamExp *body = translateState(arcList->arc->state, lambdaCache);
    PROTECT(body);
    LamCondCases *res = newLamCondCases(constant, body, next);
    UNPROTECT(save);
    return res;
}

static LamCondCases *makeCondDefault(LamExp *action) {
    LamExp *cond_default = newLamExp(LAMEXP_TYPE_COND_DEFAULT, LAMEXP_VAL_COND_DEFAULT());
    DEBUG("[newLamExp]");
    int save = PROTECT(cond_default);
    LamCondCases *res = newLamCondCases(cond_default, action, NULL);
    UNPROTECT(save);
    return res;
}

static LamCondCases *translateConstantArcList(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache) {
    if (arcList == NULL) {
        cant_happen("ran out of arcs in translateConstantArcList");
    }
    ENTER(translateConstantArcList);
    LamCondCases *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON: {
            // (default ...
            LamExp *iff = translateComparisonArcListToIf(arcList, testVar, lambdaCache);
            int save = PROTECT(iff);
            res = makeCondDefault(iff);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_CHARACTER: {
            LamExp *character = newLamExp(LAMEXP_TYPE_CHARACTER, LAMEXP_VAL_CHARACTER(arcList->arc->test->pattern->val.character));
            DEBUG("[newLamExp]");
            int save = PROTECT(character);
            res = makeConstantCondCase(arcList, character, testVar, lambdaCache);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_INTEGER: {
            LamExp *integer = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(arcList->arc->test->pattern->val.integer));
            DEBUG("[newLamExp]");
            int save = PROTECT(integer);
            res = makeConstantCondCase(arcList, integer, testVar, lambdaCache);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_WILDCARD: {
            LamExp *body = translateState(arcList->arc->state, lambdaCache);
            int save = PROTECT(body);
            res = makeCondDefault(body);
            UNPROTECT(save);
            break;
        }
        default:
            cant_happen("unrecognized pattern type %d in translateConstantArcList", arcList->arc->test->pattern->type);
    }
    LEAVE(translateConstantArcList);
    return res;
}

#ifdef DEBUG_TPMC_TRANSLATE
static int arcListLength(TpmcArcList *list) {
    int i = 0;
    while (list != NULL) {
        list = list->next;
        i++;
    }
    return i;
}

static int intListLength(LamIntList *list) {
    int i = 0;
    while (list != NULL) {
        list = list->next;
        i++;
    }
    return i;
}
#endif

static LamMatchList *translateConstructorArcList(TpmcArcList *arcList, LamExp *testVar, LamIntList *unexhaustedIndices, HashTable *lambdaCache) {
    ENTER(translateConstructorArcList);
    if (arcList == NULL) {
        if (unexhaustedIndices == NULL) {
            return NULL;
        } else {
            cant_happen("ran out of arcs with unexhausted indices in translateConstructorArcList");
        }
    }
    if (unexhaustedIndices == NULL) {
        cant_happen("all indices exhausted with arcs remaining");
    }
    LamMatchList *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON: {
            LamExp *iff = translateComparisonArcListToIf(arcList, testVar, lambdaCache);
            int save = PROTECT(iff);
            res = newLamMatchList(unexhaustedIndices, iff, NULL);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_WILDCARD: {
            LamExp *body = translateState(arcList->arc->state, lambdaCache);
            int save = PROTECT(body);
            res = newLamMatchList(unexhaustedIndices, body, NULL);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR: {
            // remove this constructor's index from the list we pass downstream
            LamTypeConstructorInfo *info = arcList->arc->test->pattern->val.constructor->info;
            unexhaustedIndices = removeIndex(info->index, unexhaustedIndices);
            LamMatchList *next = translateConstructorArcList(arcList->next, testVar, unexhaustedIndices, lambdaCache);
            int save = PROTECT(next);
            LamExp *body = translateArcToCode(arcList->arc, lambdaCache);
            DEBUG("translateArcToCode returned %p", body);
            PROTECT(body);
            LamIntList *index = newLamIntList(info->index, NULL);
            PROTECT(index);
            res = newLamMatchList(index, body, next);
            UNPROTECT(save);
            break;
        }
        default:
            cant_happen("unrecognized pattern type %d in translateConstructorArcList", arcList->arc->test->pattern->type);
    }
    LEAVE(translateConstructorArcList);
    return res;
}

static LamExp *translateStateToInlineCode(TpmcState *dfa, HashTable *lambdaCache) {
    ENTER(translateStateToInlineCode);
    LamExp *res = NULL;
    switch (dfa->state->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            res = translateTestState(dfa->state->val.test, lambdaCache);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            res = dfa->state->val.final->action;
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            res = newLamExp(LAMEXP_TYPE_ERROR, LAMEXP_VAL_ERROR());
            DEBUG("[newLamExp]");
            break;
        default:
            cant_happen("unrecognised state type %d in tpmcTranslate", dfa->state->type);
    }
    LEAVE(translateStateToInlineCode);
    return res;
}

static LamExp *translateState(TpmcState *dfa, HashTable *lambdaCache) {
    ENTER(translateState);
    LamExp *res = NULL;
    if (dfa->refcount > 1) {
        res = storeLambdaAndTranslateToApply(dfa, lambdaCache);
    } else {
        res = translateStateToInlineCode(dfa, lambdaCache);
    }
    LEAVE(translateState);
    return res;
}

static void resetStateRefCountsToZero(TpmcState *dfa) {
    dfa->refcount = 0;
    switch (dfa->state->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            TpmcArcArray *arcs = dfa->state->val.test->arcs;
            for (int i = 0; i < arcs->size; ++i) {
                resetStateRefCountsToZero(arcs->entries[i]->state);
            }
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
        case TPMCSTATEVALUE_TYPE_ERROR:
            break;
        default:
            cant_happen("unrecognised type %d in resetStateRefCountToZero", dfa->state->type);
    }
}

static void incrementStateRefCounts(TpmcState *dfa) {
    dfa->refcount++;
    if (dfa->refcount == 1) {
        switch (dfa->state->type) {
            case TPMCSTATEVALUE_TYPE_TEST:
                TpmcArcArray *arcs = dfa->state->val.test->arcs;
                for (int i = 0; i < arcs->size; ++i) {
                    incrementStateRefCounts(arcs->entries[i]->state);
                }
                break;
            case TPMCSTATEVALUE_TYPE_FINAL:
            case TPMCSTATEVALUE_TYPE_ERROR:
                break;
            default:
                cant_happen("unrecognised type %d in resetStateRefCountToZero", dfa->state->type);
        }
    }
}

static void recalculateRefCounts(TpmcState *dfa) {
    resetStateRefCountsToZero(dfa);
    incrementStateRefCounts(dfa);
}

static LamExp *prependLetRec(HashTable *lambdaCache, LamExp *body) {
    ENTER(prependLetRec);
    int nbindings = 0;
    LamLetRecBindings *bindings = NULL;
    int save = -1;
    HashSymbol *key;
    int i = 0;
    LamExp *val = NULL;
    while ((key = iterateHashTable(lambdaCache, &i, &val)) != NULL) {
        nbindings++;
        bindings = newLamLetRecBindings(key, val, bindings);
        if (save == -1) {
            save = PROTECT(bindings);
        } else {
            REPLACE_PROTECT(save, bindings);
        }
    }
    LamLetRec *letrec = newLamLetRec(nbindings, bindings, body);
    PROTECT(letrec);
    LamExp *res = newLamExp(LAMEXP_TYPE_LETREC, LAMEXP_VAL_LETREC(letrec));
    DEBUG("[newLamExp]");
    UNPROTECT(save);
    LEAVE(prependLetRec);
    return res;
}

LamExp *tpmcTranslate(TpmcState *dfa) {
    ENTER(tpmcTranslate);
    HashTable *lambdaCache = newHashTable(sizeof(LamExp *), markLamExpFn, printLamExpFn);
    int save = PROTECT(lambdaCache);
    recalculateRefCounts(dfa);
    if (dfa->state->type == TPMCSTATEVALUE_TYPE_FINAL) {
        validateLastAlloc();
    }
    LamExp *result = translateState(dfa, lambdaCache);
    if (lambdaCache->count > 0) {
        PROTECT(result);
        result = prependLetRec(lambdaCache, result);
    }
    UNPROTECT(save);
    LEAVE(tpmcTranslate);
    return result;
}
