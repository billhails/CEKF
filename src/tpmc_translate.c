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
#include <stdlib.h>
#include "lambda.h"
#include "lambda_helper.h"
#include "tpmc.h"
#include "tpmc_pp.h"
#include "symbol.h"
#include "common.h"

#ifdef DEBUG_TPMC_TRANSLATE
#  include "tpmc_debug.h"
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static ParserInfo I;

static LamExp *translateStateToInlineCode(TpmcState *dfa,
                                          LamExpTable *lambdaCache);
static LamExp *translateState(TpmcState *dfa, LamExpTable *lambdaCache);

static HashSymbol *makeLambdaName(TpmcState *state) {
    char buf[80];
    sprintf(buf, "$tpmc%d", state->stamp);
    return newSymbol(buf);
}

static LamVarList *makeCanonicalArgs(TpmcVariableTable *freeVariables) {
    ENTER(makeCanonicalArgs);
    if (countTpmcVariableTable(freeVariables) == 0) {
        LEAVE(makeCanonicalArgs);
        return NULL;
    }
    TpmcVariableArray *sorted = newTpmcVariableArray();
    int save = PROTECT(sorted);
    Index i = 0;
    HashSymbol *key;
    while ((key = iterateTpmcVariableTable(freeVariables, &i)) != NULL) {
        pushTpmcVariableArray(sorted, key);
        for (int i = sorted->size - 1; i > 0; i--) {
            if (strcmp(sorted->entries[i - 1]->name, sorted->entries[i]->name)
                > 0) {
                key = sorted->entries[i];
                sorted->entries[i] = sorted->entries[i - 1];
                sorted->entries[i - 1] = key;
            } else {
                break;
            }
        }
    }
    // claim an extra slot
    int save2 = PROTECT(sorted);
    LamVarList *res = NULL;
    for (Index i = 0; i < sorted->size; i++) {
        res = newLamVarList(I, sorted->entries[i], res);
        REPLACE_PROTECT(save2, res);
    }
    UNPROTECT(save);
    LEAVE(makeCanonicalArgs);
    return res;
}

static LamArgs *convertVarListToList(LamVarList *vars) {
    ENTER(convertVarListToList);
    if (vars == NULL) {
        LEAVE(convertVarListToList);
        return NULL;
    }
    LamArgs *next = convertVarListToList(vars->next);
    int save = PROTECT(next);
    LamExp *exp = newLamExp_Var(I, vars->var);
    PROTECT(exp);
    LamArgs *this = newLamArgs(I, exp, next);
    UNPROTECT(save);
    LEAVE(convertVarListToList);
    return this;
}

static LamExp *translateToApply(HashSymbol *name, TpmcState *dfa) {
    ENTER(translateToApply);
    LamExp *function = newLamExp_Var(I, name);
    int save = PROTECT(function);
    LamVarList *cargs = makeCanonicalArgs(dfa->freeVariables);
    PROTECT(cargs);
    LamArgs *args = convertVarListToList(cargs);
    PROTECT(args);
    LamExp *res = makeLamExp_Apply(I, function, args);
    UNPROTECT(save);
    LEAVE(translateToApply);
    return res;
}

static LamExp *translateToLambda(TpmcState *dfa, LamExpTable *lambdaCache) {
    ENTER(translateToLambda);
    LamExp *exp = translateStateToInlineCode(dfa, lambdaCache);
    int save = PROTECT(exp);
    LamVarList *args = makeCanonicalArgs(dfa->freeVariables);
    PROTECT(args);
    LamExp *res = makeLamExp_Lam(I, args, exp);
    UNPROTECT(save);
    LEAVE(translateToLambda);
    return res;
}

#ifdef DEBUG_LEAK
extern bool hash_debug_flag;
#endif

static LamExp *storeLambdaAndTranslateToApply(TpmcState *dfa,
                                              LamExpTable *lambdaCache) {
    ENTER(storeLambdaAndTranslateToApply);
    HashSymbol *name = makeLambdaName(dfa);
    if (!getLamExpTable(lambdaCache, name, NULL)) {
        LamExp *lambda = translateToLambda(dfa, lambdaCache);
        int save = PROTECT(lambda);
        setLamExpTable(lambdaCache, name, lambda);
        UNPROTECT(save);
    }
    LamExp *res = translateToApply(name, dfa);
    LEAVE(storeLambdaAndTranslateToApply);
    return res;
}

static LamExp *translateComparisonArcToTest(TpmcArc *arc) {
    ENTER(translateComparisonArcToTest);
#ifdef SAFETY_CHECKS
    if (arc->test->pattern->type != TPMCPATTERNVALUE_TYPE_COMPARISON) {
        cant_happen
            ("translateComparisonArcToTest encountered non-comparison type %d",
             arc->test->pattern->type);
    }
#endif
    TpmcComparisonPattern *pattern = getTpmcPatternValue_Comparison(arc->test->pattern);
    LamExp *a =
        newLamExp_Var(I, pattern->previous->path);
    int save = PROTECT(a);
    LamExp *b =
        newLamExp_Var(I, pattern->current->path);
    PROTECT(b);
    LamPrimApp *eq = newLamPrimApp(I, LAMPRIMOP_TYPE_EQ, a, b);
    PROTECT(eq);
    LamExp *res = newLamExp_Prim(I, eq);
    UNPROTECT(save);
    LEAVE(translateComparisonArcToTest);
    return res;
}

static LamExp *prependLetBindings(TpmcPattern *test,
                                  TpmcVariableTable *freeVariables,
                                  LamExp *body) {
    ENTER(prependLetBindings);
    int save = PROTECT(body);
    switch (test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR: {
            TpmcConstructorPattern *constructor = getTpmcPatternValue_Constructor(test->pattern);
            TpmcPatternArray *components = constructor->components;
            HashSymbol *name = constructor->info->type->name;
            DEBUG("constructor %s has size %d", name->name, components->size);
            IFDEBUG(ppTpmcConstructorPattern(constructor));
            for (Index i = 0; i < components->size; i++) {
                HashSymbol *path = components->entries[i]->path;
                DEBUG("considering variable %s", path->name);
                if (getTpmcVariableTable(freeVariables, path)) {
                    DEBUG("%s is free", path->name);
                    LamExp *base =
                        newLamExp_Var(I, test->path);
                    int save2 = PROTECT(base);
                    LamDeconstruct *deconstruct =
                        newLamDeconstruct(I, name, constructor->info->nsid, i + 1, base);
                    PROTECT(deconstruct);
                    LamExp *deconstructExp = newLamExp_Deconstruct(I, deconstruct);
                    PROTECT(deconstructExp);
                    LamBindings *bindings = newLamBindings(I, path, deconstructExp, NULL);
                    PROTECT(bindings);
                    LamLet *let = newLamLet(I, bindings, body);
                    PROTECT(let);
                    body = newLamExp_Let(I, let);
                    REPLACE_PROTECT(save, body);
                    UNPROTECT(save2);
                } else {
                    DEBUG("%s is not free", path->name);
                }
            }
        }
        break;
        case TPMCPATTERNVALUE_TYPE_TUPLE: {
            TpmcPatternArray *components = getTpmcPatternValue_Tuple(test->pattern);
            int size = components->size;
            for (int i = 0; i < size; i++) {
                HashSymbol *path = components->entries[i]->path;
                if (getTpmcVariableTable(freeVariables, path)) {
                    LamExp *base =
                        newLamExp_Var(I, test->path);
                    int save2 = PROTECT(base);
                    LamTupleIndex *index = newLamTupleIndex(I, i, size, base);
                    PROTECT(index);
                    LamExp *tupleIndex = newLamExp_TupleIndex(I, index);
                    PROTECT(tupleIndex);
                    LamBindings *bindings =
                        newLamBindings(I, path, tupleIndex, NULL);
                    PROTECT(bindings);
                    LamLet *let = newLamLet(I, bindings, body);
                    PROTECT(let);
                    body = newLamExp_Let(I, let);
                    REPLACE_PROTECT(save, body);
                    UNPROTECT(save2);
                }
            }
        }
        break;
        default:
            cant_happen("prependLetBindings passed non-constructor %s",
                        tpmcPatternValueTypeName(test->pattern->type));
    }
    LEAVE(prependLetBindings);
    UNPROTECT(save);
    return body;
}

static LamExp *translateArcToCode(TpmcArc *arc, LamExpTable *lambdaCache) {
    ENTER(translateArcToCode);
    LamExp *res = translateState(arc->state, lambdaCache);
    switch (arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
        case TPMCPATTERNVALUE_TYPE_TUPLE: {
            int save = PROTECT(res);
            res = prependLetBindings(arc->test, arc->state->freeVariables, res);
            UNPROTECT(save);
        }
        break;
        default:
            break;
    }
    LEAVE(translateArcToCode);
    return res;
}

static LamExp *translateComparisonArcAndAlternativeToIf(TpmcArc *arc, LamExpTable
                                                        *lambdaCache,
                                                        LamExp *alternative) {
    ENTER(translateComparisonArcAndAlternativeToIf);
    // (if (eq p$0 p$1) ... ...) ; both variables
    LamExp *test = translateComparisonArcToTest(arc);
    int save = PROTECT(test);
    LamExp *consequent = translateArcToCode(arc, lambdaCache);
    DEBUG("translateArcToCode returned %p", consequent);
    PROTECT(consequent);
    LamExp *res = makeLamExp_Iff(I, test, consequent, alternative);
    UNPROTECT(save);
    LEAVE(translateComparisonArcAndAlternativeToIf);
    return res;
}

static TpmcArcList *_arcArrayToList(TpmcArcArray *arcArray, Index index) {
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
static LamExp *translateArcList(TpmcArcList *arcList, LamExp *testVar,
                                LamExpTable *lambdaCache);
static LamIntCondCases *translateConstantIntArcList(TpmcArcList *arcList,
                                                    LamExp *testVar,
                                                    LamExpTable *lambdaCache);
static LamCharCondCases *translateConstantCharArcList(TpmcArcList *arcList,
                                                      LamExp *testVar,
                                                      LamExpTable
                                                      *lambdaCache);
static LamMatchList *translateConstructorArcList(TpmcArcList *arcList,
                                                 LamExp *testVar, LamIntList
                                                 *unexhaustedIndices,
                                                 LamExpTable *lambdaCache);

static LamExp *translateTestState(TpmcTestState *testState,
                                  LamExpTable *lambdaCache) {
    ENTER(translateTestState);
    TpmcArcList *arcList = arcArrayToList(testState->arcs);
    int save = PROTECT(arcList);
    LamExp *testVar =
        newLamExp_Var(I, testState->path);
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
        res = newLamIntList(I, i, info->type->name, info->nsid, res);
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

static LamExp *translateComparisonArcListToIf(TpmcArcList *arcList,
                                              LamExp *testVar,
                                              LamExpTable *lambdaCache) {
    LamExp *rest = translateArcList(arcList->next, testVar, lambdaCache);
    int save = PROTECT(rest);
    LamExp *res =
        translateComparisonArcAndAlternativeToIf(arcList->arc, lambdaCache,
                                                 rest);
    UNPROTECT(save);
    return res;
}

static LamExp *translateArcList(TpmcArcList *arcList, LamExp *testVar,
                                LamExpTable *lambdaCache) {
    ENTER(translateArcList);
#ifdef SAFETY_CHECKS
    if (arcList == NULL) {
        cant_happen("ran out of arcs in translateArcList");
    }
#endif
    LamExp *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON:{
                res =
                    translateComparisonArcListToIf(arcList, testVar,
                                                   lambdaCache);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_CHARACTER:{
                LamCharCondCases *charCases =
                    translateConstantCharArcList(arcList, testVar,
                                                 lambdaCache);
                int save = PROTECT(charCases);
                LamCondCases *cases = newLamCondCases_Characters(I, charCases);
                PROTECT(cases);
                res = makeLamExp_Cond(I, testVar, cases);
                UNPROTECT(save);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:{
                LamIntCondCases *intCases =
                    translateConstantIntArcList(arcList, testVar,
                                                lambdaCache);
                int save = PROTECT(intCases);
                LamCondCases *cases = newLamCondCases_Integers(I, intCases);
                PROTECT(cases);
                res = makeLamExp_Cond(I, testVar, cases);
                UNPROTECT(save);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_WILDCARD:{
                LamExp *res =
                    translateState(arcList->arc->state, lambdaCache);
                return res;
            }
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:{
                LamTypeConstructorInfo *info =
                    getTpmcPatternValue_Constructor(arcList->arc->test->pattern)->info;
                LamIntList *unexhaustedIndices = makeUnexhaustedIndices(info);
                int save = PROTECT(unexhaustedIndices);
                LamMatchList *matches =
                    translateConstructorArcList(arcList, testVar,
                                                unexhaustedIndices,
                                                lambdaCache);
                PROTECT(matches);
                LamExp *testExp = NULL;
                if (info->needsVec) {
                    testExp =
                        newLamExp_Tag(I, testVar);
                    PROTECT(testExp);
                } else {
                    testExp = testVar;
                }
                LamMatch *match = newLamMatch(I, testExp, matches);
                PROTECT(match);
                res = newLamExp_Match(I, match);
                UNPROTECT(save);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_TUPLE:{
            res = translateArcToCode(arcList->arc, lambdaCache);
            break;
        }
        default:
            cant_happen("unrecognised type %s", tpmcPatternValueTypeName(arcList->arc->test->pattern->type));
    }
    LEAVE(translateArcList);
    return res;
}

static LamIntCondCases *makeConstantIntCondCase(TpmcArcList *arcList,
                                                MaybeBigInt *constant,
                                                LamExp *testVar,
                                                LamExpTable *lambdaCache) {
    LamIntCondCases *next =
        translateConstantIntArcList(arcList->next, testVar, lambdaCache);
    int save = PROTECT(next);
    LamExp *body = translateState(arcList->arc->state, lambdaCache);
    PROTECT(body);
    LamIntCondCases *res = newLamIntCondCases(I, constant, body, next);
    UNPROTECT(save);
    return res;
}

static LamIntCondCases *makeIntCondDefault(LamExp *action) {
    LamIntCondCases *res = newLamIntCondCases(I, NULL, action, NULL);
    return res;
}

static LamCharCondCases *makeConstantCharCondCase(TpmcArcList *arcList,
                                                  int constant,
                                                  LamExp *testVar,
                                                  LamExpTable *lambdaCache) {
    LamCharCondCases *next =
        translateConstantCharArcList(arcList->next, testVar, lambdaCache);
    int save = PROTECT(next);
    LamExp *body = translateState(arcList->arc->state, lambdaCache);
    PROTECT(body);
    LamCharCondCases *res = newLamCharCondCases(I, constant, body, next);
    UNPROTECT(save);
    return res;
}

static LamCharCondCases *makeCharCondDefault(LamExp *action) {
    LamCharCondCases *res = newLamCharCondCases(I, 0, action, NULL);
    return res;
}

static LamIntCondCases *translateConstantIntArcList(TpmcArcList *arcList,
                                                    LamExp *testVar,
                                                    LamExpTable *lambdaCache) 
{
#ifdef SAFETY_CHECKS
    if (arcList == NULL) {
        cant_happen("ran out of arcs in translateConstantIntArcList");
    }
#endif
    ENTER(translateConstantIntArcList);
    LamIntCondCases *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON:{
                // (default ...
                LamExp *iff = translateComparisonArcListToIf(arcList, testVar,
                                                             lambdaCache);
                int save = PROTECT(iff);
                res = makeIntCondDefault(iff);
                UNPROTECT(save);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_CHARACTER:{
                cant_happen
                    ("encountered character case when cinstructing an integer cond");
            }
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:{
                MaybeBigInt *integer = getTpmcPatternValue_BigInteger(arcList->arc->test->pattern);
                res =
                    makeConstantIntCondCase(arcList, integer, testVar,
                                            lambdaCache);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_WILDCARD:{
                LamExp *body =
                    translateState(arcList->arc->state, lambdaCache);
                int save = PROTECT(body);
                res = makeIntCondDefault(body);
                UNPROTECT(save);
                break;
            }
        default:
            cant_happen
                ("unrecognized pattern type %d in translateConstantArcList",
                 arcList->arc->test->pattern->type);
    }
    LEAVE(translateConstantArcList);
    return res;
}

static LamCharCondCases *translateConstantCharArcList(TpmcArcList *arcList,
                                                      LamExp *testVar,
                                                      LamExpTable
                                                      *lambdaCache) {
#ifdef SAFETY_CHECKS
    if (arcList == NULL) {
        cant_happen("ran out of arcs in translateConstantCharArcList");
    }
#endif
    ENTER(translateConstantCharArcList);
    LamCharCondCases *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON:{
                // (default ...
                LamExp *iff = translateComparisonArcListToIf(arcList, testVar,
                                                             lambdaCache);
                int save = PROTECT(iff);
                res = makeCharCondDefault(iff);
                UNPROTECT(save);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_CHARACTER:{
                int character = getTpmcPatternValue_Character(arcList->arc->test->pattern);
                res =
                    makeConstantCharCondCase(arcList, character, testVar,
                                             lambdaCache);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:{
                cant_happen
                    ("encountered integer case when constructing a character cond");
            }
        case TPMCPATTERNVALUE_TYPE_WILDCARD:{
                LamExp *body =
                    translateState(arcList->arc->state, lambdaCache);
                int save = PROTECT(body);
                res = makeCharCondDefault(body);
                UNPROTECT(save);
                break;
            }
        default:
            cant_happen
                ("unrecognized pattern type %d in translateConstantArcList",
                 arcList->arc->test->pattern->type);
    }
    LEAVE(translateConstantArcList);
    return res;
}

static LamMatchList *translateConstructorArcList(TpmcArcList *arcList,
                                                 LamExp *testVar, LamIntList
                                                 *unexhaustedIndices,
                                                 LamExpTable *lambdaCache) {
    ENTER(translateConstructorArcList);
    if (arcList == NULL) {
        if (unexhaustedIndices == NULL) {
            LEAVE(translateConstructorArcList);
            return NULL;
        } else {
            cant_happen
                ("ran out of arcs with unexhausted indices in translateConstructorArcList");
        }
    }
#ifdef SAFETY_CHECKS
    if (unexhaustedIndices == NULL) {
        cant_happen("all indices exhausted with arcs remaining");
    }
#endif
    LamMatchList *res = NULL;
    switch (arcList->arc->test->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON:{
                LamExp *iff = translateComparisonArcListToIf(arcList, testVar,
                                                             lambdaCache);
                int save = PROTECT(iff);
                res = newLamMatchList(I, unexhaustedIndices, iff, NULL);
                UNPROTECT(save);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_WILDCARD:{
                LamExp *body =
                    translateState(arcList->arc->state, lambdaCache);
                int save = PROTECT(body);
                res = newLamMatchList(I, unexhaustedIndices, body, NULL);
                UNPROTECT(save);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:{
                // remove this constructor's index from the list we pass downstream
                LamTypeConstructorInfo *info =
                    getTpmcPatternValue_Constructor(arcList->arc->test->pattern)->info;
                unexhaustedIndices =
                    removeIndex(info->index, unexhaustedIndices);
                LamMatchList *next =
                    translateConstructorArcList(arcList->next, testVar,
                                                unexhaustedIndices,
                                                lambdaCache);
                int save = PROTECT(next);
                LamExp *body = translateArcToCode(arcList->arc, lambdaCache);
                DEBUG("translateArcToCode returned %p", body);
                PROTECT(body);
                LamIntList *index =
                    newLamIntList(I, info->index, info->type->name, info->nsid, NULL);
                PROTECT(index);
                res = newLamMatchList(I, index, body, next);
                UNPROTECT(save);
                break;
            }
        default:
            cant_happen
                ("unrecognized pattern type %d in translateConstructorArcList",
                 arcList->arc->test->pattern->type);
    }
    LEAVE(translateConstructorArcList);
    return res;
}

static LamExp *translateStateToInlineCode(TpmcState *dfa,
                                          LamExpTable *lambdaCache) {
    ENTER(translateStateToInlineCode);
    LamExp *res = NULL;
    switch (dfa->state->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            res = translateTestState(getTpmcStateValue_Test(dfa->state), lambdaCache);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            res = getTpmcStateValue_Final(dfa->state)->action;
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            res = newLamExp_Error(I);
            break;
        default:
            cant_happen("unrecognised state type %d in tpmcTranslate",
                        dfa->state->type);
    }
    LEAVE(translateStateToInlineCode);
    return res;
}

static LamExp *translateState(TpmcState *dfa, LamExpTable *lambdaCache) {
    ENTER(translateState);
    LamExp *res = NULL;
    if (dfa->refCount > 1) {
        res = storeLambdaAndTranslateToApply(dfa, lambdaCache);
    } else {
        res = translateStateToInlineCode(dfa, lambdaCache);
    }
    LEAVE(translateState);
    return res;
}

static void resetStateRefCountsToZero(TpmcState *dfa) {
    dfa->refCount = 0;
    switch (dfa->state->type) {
        case TPMCSTATEVALUE_TYPE_TEST:{
            TpmcArcArray *arcs = getTpmcStateValue_Test(dfa->state)->arcs;
            for (Index i = 0; i < arcs->size; ++i) {
                resetStateRefCountsToZero(arcs->entries[i]->state);
            }
        }
        break;
        case TPMCSTATEVALUE_TYPE_FINAL:
        case TPMCSTATEVALUE_TYPE_ERROR:
            break;
        default:
            cant_happen("unrecognised type %d in resetStateRefCountToZero",
                        dfa->state->type);
    }
}

static void incrementStateRefCounts(TpmcState *dfa) {
    dfa->refCount++;
    if (dfa->refCount == 1) {
        switch (dfa->state->type) {
            case TPMCSTATEVALUE_TYPE_TEST:{
                TpmcArcArray *arcs = getTpmcStateValue_Test(dfa->state)->arcs;
                for (Index i = 0; i < arcs->size; ++i) {
                    incrementStateRefCounts(arcs->entries[i]->state);
                }
            }
            break;
            case TPMCSTATEVALUE_TYPE_FINAL:
            case TPMCSTATEVALUE_TYPE_ERROR:
                break;
            default:
                cant_happen
                    ("unrecognised type %d in resetStateRefCountToZero",
                     dfa->state->type);
        }
    }
}

static void recalculateRefCounts(TpmcState *dfa) {
    resetStateRefCountsToZero(dfa);
    incrementStateRefCounts(dfa);
}

static LamExp *prependLetRec(LamExpTable *lambdaCache, LamExp *body) {
    ENTER(prependLetRec);
    LamBindings *bindings = NULL;
    int save = -1;
    HashSymbol *key;
    Index i = 0;
    LamExp *val = NULL;
    while ((key = iterateLamExpTable(lambdaCache, &i, &val)) != NULL) {
        bindings = newLamBindings(I, key, val, bindings);
        if (save == -1) {
            save = PROTECT(bindings);
        } else {
            REPLACE_PROTECT(save, bindings);
        }
    }
    LamExp *res = makeLamExp_LetRec(I, bindings, body);
    UNPROTECT(save);
    LEAVE(prependLetRec);
    return res;
}

LamExp *tpmcTranslate(ParserInfo PI, TpmcState *dfa) {
    // IFDEBUG(system("clear"));
    ENTER(tpmcTranslate);
    I = PI;
    IFDEBUG(ppTpmcState(dfa));
    LamExpTable *lambdaCache = newLamExpTable();
    int save = PROTECT(lambdaCache);
    recalculateRefCounts(dfa);
    if (dfa->state->type == TPMCSTATEVALUE_TYPE_FINAL) {
        validateLastAlloc();
    }
    LamExp *result = translateState(dfa, lambdaCache);
    if (countLamExpTable(lambdaCache) > 0) {
        PROTECT(result);
        result = prependLetRec(lambdaCache, result);
    }
    UNPROTECT(save);
    LEAVE(tpmcTranslate);
    return result;
}
