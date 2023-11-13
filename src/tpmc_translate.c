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

static int debugInvocationId = 0;
#define ENTER(name) int debugMyId = debugInvocationId++; fprintf(stderr, "**** "); fprintf(stderr, "enter " #name " %d\n", debugMyId)
#define LEAVE(name) fprintf(stderr, "**** "); fprintf(stderr, "leave " #name " %d\n", debugMyId)
#define DEBUG(...) do { fprintf(stderr, "**** "); fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); } while(0)
#define NEWLINE() fprintf(stderr, "\n")
#else
#define ENTER(n) 0
#define LEAVE(n) 0
#define DEBUG(...) 0
#define NEWLINE() 0
#endif

typedef enum ArcContext {
    ARC_CONTEXT_NONE,
    ARC_CONTEXT_CONSTRUCTOR,
    ARC_CONTEXT_CONSTANT
} ArcContext;

static LamExp *tpmcTranslateToCode(TpmcState *dfa, HashTable *lambdaCache);
static LamExp *_tpmcTranslate(TpmcState *dfa, HashTable *lambdaCache);

static void markLamExpFn(void *ptr) {
    markLamExp((LamExp *) ptr);
}

static void printLamExpFn(void *ptr, int depth) {
    ppLamExpD((LamExp *) ptr, depth);
}

static HashSymbol *lambdaName(TpmcState *state) {
    char buf[80];
    sprintf(buf, "$tpmc%d", state->stamp);
    return newSymbol(buf);
}

static LamVarList *canonicalArgs(HashTable *freeVariables) {
    ENTER(canonicalArgs);
    if (freeVariables->count == 0) {
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
    for (int i = sorted->size; i > 0; --i) {
        res = newLamVarList(sorted->entries[i-1], res);
        REPLACE_PROTECT(save2, res);
    }
    UNPROTECT(save);
    LEAVE(canonicalArgs);
    return res;
}

static LamSequence *_convertVarsToSequence(LamVarList *vars) {
    ENTER(_convertVarsToSequence);
    if (vars == NULL) {
        LEAVE(_convertVarsToSequence);
        return NULL;
    }
    LamSequence *next = _convertVarsToSequence(vars->next);
    int save = PROTECT(next);
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(vars->var));
    PROTECT(exp);
    LamSequence *this = newLamSequence(exp, next);
    UNPROTECT(save);
    LEAVE(_convertVarsToSequence);
    return this;
}

static LamExp *tpmcCallLambda(HashSymbol *name, TpmcState *dfa) {
    ENTER(tpmcCallLambda);
    LamExp *function = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(name));
    int save = PROTECT(function);
    LamVarList *cargs = canonicalArgs(dfa->freeVariables);
    PROTECT(cargs);
    LamSequence *args = _convertVarsToSequence(cargs);
    PROTECT(args);
    LamApply *apply = newLamApply(function, dfa->freeVariables->count, args);
    PROTECT(apply);
    LamExp *res = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
#ifdef DEBUG_TPMC_TRANSLATE
    fprintf(stderr, "tpmcCallLambda returning ");
    ppLamExp(res);
    NEWLINE();
#endif
    UNPROTECT(save);
    LEAVE(tpmcCallLambda);
    return res;
}

static LamExp *tpmcTranslateToLambda(TpmcState *dfa, HashTable *lambdaCache) {
    ENTER(tpmcTranslateToLambda);
    LamExp *exp = tpmcTranslateToCode(dfa, lambdaCache);
    int save = PROTECT(exp);
    LamVarList *args = canonicalArgs(dfa->freeVariables);
    PROTECT(args);
    LamLam *lambda = newLamLam(dfa->freeVariables->count, args, exp);
    PROTECT(lambda);
    LamExp *res = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(lambda));
    UNPROTECT(save);
    LEAVE(tpmcTranslateToLambda);
    return res;
}

static LamExp *tpmcStoreLambda(TpmcState *dfa, HashTable *lambdaCache) {
    ENTER(tpmcStoreLambda);
    HashSymbol *name = lambdaName(dfa);
    if (!hashGet(lambdaCache, name, NULL)) {
        LamExp *lambda = tpmcTranslateToLambda(dfa, lambdaCache);
#ifdef DEBUG_TPMC_TRANSLATE
        fprintf(stderr, "tpmcStoreLambda ");
        ppHashSymbol(name);
        fprintf(stderr, " = ");
        ppLamExp(lambda);
        NEWLINE();
#endif
        int save = PROTECT(lambda);
        hashSet(lambdaCache, name, &lambda);
        UNPROTECT(save);
    }
    LEAVE(tpmcStoreLambda);
    return tpmcCallLambda(name, dfa);
}

static LamExp *tpmcTranslateArcToComparisonTest(TpmcArc *arc) {
    ENTER(tpmcTranslateArcToComparisonTest);
    if (arc->test->pattern->type != TPMCPATTERNVALUE_TYPE_COMPARISON) {
        cant_happen("tpmcTranslateArcToComparisonTest ecncountered non-comparison type %d", arc->test->pattern->type);
    }
    TpmcComparisonPattern *pattern = arc->test->pattern->val.comparison;
    LamExp *a = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(pattern->previous->path));
    int save = PROTECT(a);
    LamExp *b = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(pattern->current->path));
    PROTECT(b);
    LamPrimApp *eq = newLamPrimApp(LAMPRIMOP_TYPE_LAM_PRIM_EQ, a, b);
    PROTECT(eq);
    LamExp *res = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(eq));
    UNPROTECT(save);
    LEAVE(tpmcTranslateArcToComparisonTest);
    return res;
}

static LamExp *tpmcWrapConstructorBindings(TpmcPattern *test, HashTable *freeVariables, LamExp *body) {
    ENTER(tpmcWrapConstructorBindings);
    if (test->pattern->type != TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        cant_happen("tpmcWrapConstructorBindings passed non-constructor %d", test->pattern->type);
    }
    int save = PROTECT(body);
    TpmcConstructorPattern *constructor = test->pattern->val.constructor;
    LamExp *base = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(test->path));
    PROTECT(base);
    for (int i = 0; i < constructor->components->size; i++) {
        // (let (var (vec i+1 base)) body)
        HashSymbol *path = constructor->components->entries[i]->path;
        if (hashGet(freeVariables, path, NULL)) {
            LamExp *index = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(i + 1));
            int save2 = PROTECT(index);
            LamPrimApp *vec = newLamPrimApp(LAMPRIMOP_TYPE_LAM_PRIM_VEC, index, base);
            PROTECT(vec);
            LamExp *vecExp = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(vec));
            PROTECT(vecExp);
            LamLet *let = newLamLet(path, vecExp, body);
            PROTECT(let);
            body = newLamExp(LAMEXP_TYPE_LET, LAMEXP_VAL_LET(let));
            REPLACE_PROTECT(save, body);
            UNPROTECT(save2);
        }
    }
    LEAVE(tpmcWrapConstructorBindings);
    return body;
}

static LamExp *tpmcTranslateArcToCode(TpmcArc *arc, HashTable *lambdaCache) {
    ENTER(tpmcTranslateArcToCode);
    LamExp *res = NULL;
    switch (arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
        case TPMCPATTERNVALUE_TYPE_INTEGER:
            res = _tpmcTranslate(arc->state, lambdaCache);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            LamExp *body = _tpmcTranslate(arc->state, lambdaCache);
            int save = PROTECT(body);
            LamExp *letBindings = tpmcWrapConstructorBindings(arc->test, arc->state->freeVariables, body);
            UNPROTECT(save);
            res = letBindings;
            break;
        default:
            cant_happen("unrecognized type %d in tpmcTranslateArcToCode", arc->test->pattern->type);
    }
    LEAVE(tpmcTranslateArcToCode);
    return res;
}

static LamExp *tpmcTranslateComparisonTestToCode(TpmcArc *arc, HashTable *lambdaCache, LamExp *alternative) {
    ENTER(tpmcTranslateComparisonTestToCode);
    // (if (eq p$0 p$1) ... ...) ; both variables
    LamExp *test = tpmcTranslateArcToComparisonTest(arc);
    int save = PROTECT(test);
    LamExp *consequent = tpmcTranslateArcToCode(arc, lambdaCache);
    PROTECT(consequent);
    LamIff *iff = newLamIff(test, consequent, alternative);
    PROTECT(iff);
    LamExp *res = newLamExp(LAMEXP_TYPE_IFF, LAMEXP_VAL_IFF(iff));
    UNPROTECT(save);
    LEAVE(tpmcTranslateComparisonTestToCode);
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
static LamExp *translateTestState(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache);
static LamCondCases *translateConstantCases(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache);
static LamMatchList *translateConstructorCases(TpmcArcList *arcList, LamExp *testVar, LamIntList *unexhaustedIndices, HashTable *lambdaCache);

static LamExp *tpmcTranslateTestState(TpmcTestState *testState, HashTable *lambdaCache) {
    ENTER(tpmcTranslateTestState);
    TpmcArcList *arcList = arcArrayToList(testState->arcs);
    int save = PROTECT(arcList);
    LamExp *testVar = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(testState->path));
    PROTECT(testVar);
    LamExp *res = translateTestState(arcList, testVar, lambdaCache);
    UNPROTECT(save);
    LEAVE(tpmcTranslateTestState);
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

static LamExp *translateTestState(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache) {
    ENTER(translateTestState);
    if (arcList == NULL) {
        cant_happen("ran out of arcs in translateTestState");
    }
    LamExp *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON: {
            LamExp *rest = translateTestState(arcList->next, testVar, lambdaCache);
            int save = PROTECT(rest);
            res = tpmcTranslateComparisonTestToCode(arcList->arc, lambdaCache, rest);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
        case TPMCPATTERNVALUE_TYPE_INTEGER: {
            LamCondCases *cases = translateConstantCases(arcList, testVar, lambdaCache);
            int save = PROTECT(cases);
            LamCond *cond = newLamCond(testVar, cases);
            PROTECT(cond);
            res = newLamExp(LAMEXP_TYPE_COND, LAMEXP_VAL_COND(cond));
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            return _tpmcTranslate(arcList->arc->state, lambdaCache);
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR: {
            LamTypeConstructorInfo *info = arcList->arc->test->pattern->val.constructor->info;
            LamIntList *unexhaustedIndices = makeUnexhaustedIndices(info);
            int save = PROTECT(unexhaustedIndices);
            LamMatchList *matches = translateConstructorCases(arcList, testVar, unexhaustedIndices, lambdaCache);
            PROTECT(matches);
            LamExp *testExp = NULL;
            if (info->vec) {
                LamExp *zero = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(0));
                PROTECT(zero);
                LamPrimApp *vec = newLamPrimApp(LAMPRIMOP_TYPE_LAM_PRIM_VEC, zero, testVar);
                PROTECT(vec);
                testExp = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(vec));
                PROTECT(testExp);
            } else {
                testExp = testVar;
            }
            LamMatch *match = newLamMatch(testExp, matches);
            PROTECT(match);
            res = newLamExp(LAMEXP_TYPE_MATCH, LAMEXP_VAL_MATCH(match));
            UNPROTECT(save);
            break;
        }
        default:
            cant_happen("unrecognized pattern type %d in translateTestState", arcList->arc->test->pattern->type);
    }
    LEAVE(translateTestState);
    return res;
}

static LamCondCases *translateConstantCases(TpmcArcList *arcList, LamExp *testVar, HashTable *lambdaCache) {
    if (arcList == NULL) {
        cant_happen("ran out of arcs in translateConstantCases");
    }
    ENTER(translateConstantCases);
    LamCondCases *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON: {
            // (default ...
            LamExp *rest = translateTestState(arcList->next, testVar, lambdaCache);
            int save = PROTECT(rest);
            LamExp *iff = tpmcTranslateComparisonTestToCode(arcList->arc, lambdaCache, rest);
            PROTECT(iff);
            LamExp *cond_default = newLamExp(LAMEXP_TYPE_COND_DEFAULT, LAMEXP_VAL_COND_DEFAULT());
            PROTECT(cond_default);
            res = newLamCondCases(cond_default, iff, NULL);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_CHARACTER: {
            LamCondCases *next = translateConstantCases(arcList->next, testVar, lambdaCache);
            int save = PROTECT(next);
            LamExp *character = newLamExp(LAMEXP_TYPE_CHARACTER, LAMEXP_VAL_CHARACTER(arcList->arc->test->pattern->val.character));
            PROTECT(character);
            LamExp *body = _tpmcTranslate(arcList->arc->state, lambdaCache);
            PROTECT(body);
            res = newLamCondCases(character, body, next);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_INTEGER: {
            LamCondCases *next = translateConstantCases(arcList->next, testVar, lambdaCache);
            int save = PROTECT(next);
            LamExp *integer = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(arcList->arc->test->pattern->val.integer));
            PROTECT(integer);
            LamExp *body = _tpmcTranslate(arcList->arc->state, lambdaCache);
            PROTECT(body);
            res = newLamCondCases(integer, body, next);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_WILDCARD: {
            LamExp *body = _tpmcTranslate(arcList->arc->state, lambdaCache);
            int save = PROTECT(body);
            LamExp *cond_default = newLamExp(LAMEXP_TYPE_COND_DEFAULT, LAMEXP_VAL_COND_DEFAULT());
            PROTECT(cond_default);
            res = newLamCondCases(cond_default, body, NULL);
            UNPROTECT(save);
            break;
        }
        default:
            cant_happen("unrecognized pattern type %d in translateConstantCases", arcList->arc->test->pattern->type);
    }
    LEAVE(translateConstantCases);
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

static LamMatchList *translateConstructorCases(TpmcArcList *arcList, LamExp *testVar, LamIntList *unexhaustedIndices, HashTable *lambdaCache) {
    ENTER(translateConstructorCases);
    DEBUG("translateConstructorCases arcs: %d, indices %d", arcListLength(arcList), intListLength(unexhaustedIndices));
    if (arcList == NULL) {
        if (unexhaustedIndices == NULL) {
            return NULL;
        } else {
            cant_happen("ran out of arcs with unexhausted indices in translateConstructorCases");
        }
    }
    if (unexhaustedIndices == NULL) {
        cant_happen("all indices exhausted with arcs remaining");
    }
    LamMatchList *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON: {
            LamExp *rest = translateTestState(arcList->next, testVar, lambdaCache);
            int save = PROTECT(rest);
            LamExp *iff = tpmcTranslateComparisonTestToCode(arcList->arc, lambdaCache, rest);
            PROTECT(iff);
            res = newLamMatchList(unexhaustedIndices, iff, NULL);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_WILDCARD: {
            LamExp *body = _tpmcTranslate(arcList->arc->state, lambdaCache);
            int save = PROTECT(body);
            res = newLamMatchList(unexhaustedIndices, body, NULL);
            UNPROTECT(save);
            break;
        }
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR: {
            // remove this constructor's index from the list we pass downstream
            LamTypeConstructorInfo *info = arcList->arc->test->pattern->val.constructor->info;
            unexhaustedIndices = removeIndex(info->index, unexhaustedIndices);
            LamMatchList *next = translateConstructorCases(arcList->next, testVar, unexhaustedIndices, lambdaCache);
            int save = PROTECT(next);
            LamExp *_body = _tpmcTranslate(arcList->arc->state, lambdaCache);
            PROTECT(_body);
            LamExp *body = tpmcWrapConstructorBindings(arcList->arc->test, arcList->arc->state->freeVariables, _body);
            PROTECT(body);
            LamIntList *index = newLamIntList(info->index, NULL);
            PROTECT(index);
            res = newLamMatchList(index, body, next);
            UNPROTECT(save);
            break;
        }
        default:
            cant_happen("unrecognized pattern type %d in translateConstructorCases", arcList->arc->test->pattern->type);
    }
    LEAVE(translateConstructorCases);
    return res;
}

static LamExp *tpmcTranslateToCode(TpmcState *dfa, HashTable *lambdaCache) {
    ENTER(tpmcTranslateToCode);
    LamExp *res = NULL;
    switch (dfa->state->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            res = tpmcTranslateTestState(dfa->state->val.test, lambdaCache);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            res = dfa->state->val.final->action;
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            res = newLamExp(LAMEXP_TYPE_ERROR, LAMEXP_VAL_ERROR());
            break;
        default:
            cant_happen("unrecognised state type %d in tpmcTranslate", dfa->state->type);
    }
    LEAVE(tpmcTranslateToCode);
    return res;
}

static LamExp *_tpmcTranslate(TpmcState *dfa, HashTable *lambdaCache) {
    ENTER(_tpmcTranslate);
    LamExp *res = NULL;
    if (dfa->refcount > 1) {
        res = tpmcStoreLambda(dfa, lambdaCache);
    } else {
        res = tpmcTranslateToCode(dfa, lambdaCache);
    }
    LEAVE(_tpmcTranslate);
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
    int save = PROTECT(bindings);
    HashSymbol *key;
    int i = 0;
    LamExp *val = NULL;
    while ((key = iterateHashTable(lambdaCache, &i, &val)) != NULL) {
#ifdef DEBUG_TPMC_TRANSLATE
        ppHashSymbol(key);
        fprintf(stderr, " = ");
        ppLamExp(val);
        fprintf(stderr, "\n");
#endif
        nbindings++;
        bindings = newLamLetRecBindings(key, val, bindings);
        REPLACE_PROTECT(save, bindings);
    }
    LamLetRec *letrec = newLamLetRec(nbindings, bindings, body);
    PROTECT(letrec);
    LamExp *res = newLamExp(LAMEXP_TYPE_LETREC, LAMEXP_VAL_LETREC(letrec));
    UNPROTECT(save);
    LEAVE(prependLetRec);
    return res;
}

LamExp *tpmcTranslate(TpmcState *dfa) {
    ENTER(tpmcConvert);
    HashTable *lambdaCache = newHashTable(sizeof(LamExp *), markLamExpFn, printLamExpFn);
    int save = PROTECT(lambdaCache);
    recalculateRefCounts(dfa);
    LamExp *result = _tpmcTranslate(dfa, lambdaCache);
    if (lambdaCache->count > 0) {
        PROTECT(result);
        result = prependLetRec(lambdaCache, result);
    }
    UNPROTECT(save);
    LEAVE(tpmcConvert);
    return result;
}
