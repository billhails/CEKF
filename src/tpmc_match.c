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
 * Term Pattern Matching Compiler match algorithm
 */

#include <stdio.h>
#include <assert.h>
#include "common.h"
#include "tpmc_match.h"
#include "tpmc_compare.h"
#include "tpmc_debug.h"
#include "tpmc_pp.h"
#include "lambda_debug.h"
#include "lambda_helper.h"
#include "symbol.h"

#ifdef DEBUG_TPMC_MATCH
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

TpmcState *tpmcMakeState(TpmcStateValue *val) {
    static int counter = 0;
    return newTpmcState(counter++, val);
}

// this is definately a wildcard
// TPMCPATTERNVALUE_TYPE_WILDCARD
//
// TPMCPATTERNVALUE_TYPE_COMPARISON
//
// these are all constructors:
// TPMCPATTERNVALUE_TYPE_CHARACTER
// TPMCPATTERNVALUE_TYPE_INTEGER
// TPMCPATTERNVALUE_TYPE_CONSTRUCTOR

static bool patternIsWildcard(TpmcPattern *pattern) {
    bool res = pattern->pattern->type == TPMCPATTERNVALUE_TYPE_WILDCARD;
    return res;
}

static bool noRemainingTests(TpmcMatrix *matrix) {
    for (int x = 0; x < matrix->width; x++) {
        if (!patternIsWildcard(getTpmcMatrixIndex(matrix, x, 0))) {
            return false;
        }
    }
    return true;
}

static int findFirstConstructorColumn(TpmcMatrix *matrix) {
    for (int x = 0; x < matrix->width; x++) {
        if (!patternIsWildcard(getTpmcMatrixIndex(matrix, x, 0))) {
            return x;
        }
    }
    cant_happen("findFirstConstructorColumn failed");
}

static TpmcState *makeEmptyTestState(HashSymbol *path) {
    TpmcArcArray *arcs = newTpmcArcArray();
    int save = PROTECT(arcs);
    TpmcTestState *test = newTpmcTestState(path, arcs);
    PROTECT(test);
    TpmcStateValue *val = newTpmcStateValue(TPMCSTATEVALUE_TYPE_TEST,
                                            TPMCSTATEVALUE_VAL_TEST(test));
    PROTECT(val);
    TpmcState *state = tpmcMakeState(val);
    UNPROTECT(save);
    return state;
}

static bool patternMatches(TpmcPattern *constructor, TpmcPattern *pattern) {
    bool isComparison =
        (constructor->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON);
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            cant_happen("patternMatches ennncountered var");
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            return eqTpmcPattern(constructor, pattern);
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            cant_happen("patternMatches encountered assignment");
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            return true;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:{
                bool res = isComparison
                    || (constructor->pattern->type ==
                        TPMCPATTERNVALUE_TYPE_CHARACTER
                        && constructor->pattern->val.character ==
                        pattern->pattern->val.character);
                return res;
            }
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:{
                bool res = isComparison
                    || (constructor->pattern->type ==
                        TPMCPATTERNVALUE_TYPE_BIGINTEGER
                        && cmpBigInt(constructor->pattern->val.biginteger,
                                     pattern->pattern->val.biginteger) == 0);
                return res;
            }
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:{
                // remember the "constructor" is really just "not a wildcard"
                bool res =
                    (constructor->pattern->type ==
                     TPMCPATTERNVALUE_TYPE_CONSTRUCTOR &&
                     // pointer equivalence works for hash symbols
                     constructor->pattern->val.constructor->tag ==
                     pattern->pattern->val.constructor->tag) || isComparison;
                return res;
            }
        default:
            cant_happen("unrecognized pattern type %d in patternMatches",
                        pattern->pattern->type);
    }
}

TpmcIntArray *findPatternsMatching(TpmcPattern *c, TpmcPatternArray *N) {
    TpmcIntArray *res = newTpmcIntArray();
    int save = PROTECT(res);
    int i = 0;
    TpmcPattern *candidate;
    while (iterateTpmcPatternArray(N, &i, &candidate, NULL)) {
        if (patternMatches(c, candidate)) {
            pushTpmcIntArray(res, i - 1);
        }
    }
    UNPROTECT(save);
    return res;
}

static TpmcPatternArray *extractColumnSubset(TpmcPatternArray *N,
                                             TpmcIntArray *ys) {
    TpmcPatternArray *res = newTpmcPatternArray("extractColumnSubset");
    int save = PROTECT(res);
    int i = 0;
    int y;
    while (iterateTpmcIntArray(ys, &i, &y, NULL)) {
        pushTpmcPatternArray(res, N->entries[y]);
    }
    UNPROTECT(save);
    return res;
}

static TpmcPatternArray *extractMatrixColumn(TpmcMatrix *matrix, int x) {
    TpmcPatternArray *res = newTpmcPatternArray("extractMatrixColumn");
    int save = PROTECT(res);
    for (int y = 0; y < matrix->height; y++) {
        pushTpmcPatternArray(res, getTpmcMatrixIndex(matrix, x, y));
    }
    UNPROTECT(save);
    return res;
}

static TpmcMatrix *discardMatrixColumn(TpmcMatrix *matrix, int column) {
    TpmcMatrix *res = newTpmcMatrix(matrix->width - 1, matrix->height);
    int save = PROTECT(res);
    for (int x = 0; x < matrix->width; x++) {
        for (int y = 0; y < matrix->height; y++) {
            if (x < column) {
                setTpmcMatrixIndex(res, x, y,
                                   getTpmcMatrixIndex(matrix, x, y));
            } else if (x > column) {
                setTpmcMatrixIndex(res, x - 1, y,
                                   getTpmcMatrixIndex(matrix, x, y));
            } else {
                // no-op
            }
        }
    }
    UNPROTECT(save);
    return res;
}

static TpmcMatrix *extractMatrixRows(TpmcMatrix *matrix,
                                     TpmcIntArray *indices) {
    TpmcMatrix *res = newTpmcMatrix(matrix->width, indices->size);
    int save = PROTECT(res);
    int resy = 0;
    int iy = 0;
    int i = 0;
    while (iterateTpmcIntArray(indices, &i, &iy, NULL)) {
        for (int x = 0; x < res->width; ++x) {
            setTpmcMatrixIndex(res, x, resy,
                               getTpmcMatrixIndex(matrix, x, iy));
        }
        resy++;
    }
    UNPROTECT(save);
    return res;
}

static TpmcMatrix *appendMatrices(TpmcMatrix *prefix, TpmcMatrix *suffix) {
    if (prefix->height != suffix->height) {
        cant_happen
            ("appendMatrices given matrices with different heights, %d vs %d",
             prefix->height, suffix->height);
    }
    TpmcMatrix *res =
        newTpmcMatrix(prefix->width + suffix->width, prefix->height);
    int save = PROTECT(res);
    for (int x = 0; x < res->width; ++x) {
        for (int y = 0; y < res->height; ++y) {
            if (x >= prefix->width) {
                setTpmcMatrixIndex(res, x, y,
                                   getTpmcMatrixIndex(suffix,
                                                      x - prefix->width, y));
            } else {
                setTpmcMatrixIndex(res, x, y,
                                   getTpmcMatrixIndex(prefix, x, y));
            }
        }
    }
    UNPROTECT(save);
    return res;
}

static TpmcStateArray *extractStateArraySubset(TpmcStateArray *all,
                                               TpmcIntArray *indices) {
    TpmcStateArray *res = newTpmcStateArray("extractStateArraySubset");
    int save = PROTECT(res);
    for (int i = 0; i < indices->size; ++i) {
        int j = indices->entries[i];
        pushTpmcStateArray(res, all->entries[j]);
    }
    UNPROTECT(save);
    return res;
}

static int determineArity(TpmcPattern *pattern) {
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        LamTypeConstructorInfo *info =
            pattern->pattern->val.constructor->info;
        return info->arity;
    } else {
        return 0;
    }
}

static void populateSubPatternMatrixRowWithWildcards(TpmcMatrix *matrix,
                                                     int y, int arity,
                                                     TpmcPattern *pattern) {
    // FIXME safeMalloc this from strlen + some n
    char buf[512];
    for (int i = 0; i < arity; i++) {
        if (snprintf(buf, 512, "%s$%d", pattern->path->name, i) > 510) {
            cant_happen
                ("internal structure limit exceeded in arg processing");
        }
        HashSymbol *path = newSymbol(buf);
        TpmcPatternValue *wc =
            newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD,
                                TPMCPATTERNVALUE_VAL_WILDCARD());
        int save = PROTECT(wc);
        setTpmcMatrixIndex(matrix, i, y, newTpmcPattern(wc));
        getTpmcMatrixIndex(matrix, i, y)->path = path;
        UNPROTECT(save);
    }
}

static void populateSubPatternMatrixRowWithComponents(TpmcMatrix *matrix,
                                                      int y, int arity,
                                                      TpmcPattern *pattern) {
    if (arity != pattern->pattern->val.constructor->components->size) {
        ppTpmcPattern(pattern);
        cant_happen
            ("arity %d does not match constructor arity %d in populateSubPatternMatrixRowWithComponents",
             arity, pattern->pattern->val.constructor->components->size);
    }
    for (int i = 0; i < arity; i++) {
        TpmcPattern *entry =
            pattern->pattern->val.constructor->components->entries[i];
        setTpmcMatrixIndex(matrix, i, y, entry);
    }
}

static TpmcMatrix *makeSubPatternMatrix(TpmcPatternArray *patterns, int arity) {
    TpmcMatrix *matrix = newTpmcMatrix(arity, patterns->size);
    if (arity == 0) {
        return matrix;
    }
    int save = PROTECT(matrix);
    for (int i = 0; i < patterns->size; ++i) {
        TpmcPattern *pattern = patterns->entries[i];
        switch (pattern->pattern->type) {
            case TPMCPATTERNVALUE_TYPE_VAR:
                cant_happen
                    ("encountered pattern type var during makeSubPatternMatrix");
            case TPMCPATTERNVALUE_TYPE_COMPARISON:
            case TPMCPATTERNVALUE_TYPE_WILDCARD:
                populateSubPatternMatrixRowWithWildcards(matrix, i, arity,
                                                         pattern);
                break;
            case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
                populateSubPatternMatrixRowWithComponents(matrix, i, arity,
                                                          pattern);
                break;
            case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
                cant_happen
                    ("encountered pattern type assignment during makeSubPatternMatrix");
            case TPMCPATTERNVALUE_TYPE_CHARACTER:
                cant_happen
                    ("encountered pattern type char during makeSubPatternMatrix");
            case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
                cant_happen
                    ("encountered pattern type int during makeSubPatternMatrix");
            default:
                cant_happen
                    ("unrecognised pattern type %d during makeSubPatternMatrix",
                     pattern->pattern->type);
        }
    }
    UNPROTECT(save);
    return matrix;
}

static TpmcPattern *replaceComponentsWithWildcards(TpmcPattern *pattern) {
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        TpmcConstructorPattern *constructor =
            pattern->pattern->val.constructor;
        if (constructor->components->size > 0) {
            TpmcPatternArray *components =
                newTpmcPatternArray("replaceComponentsWithWildcards");
            int save = PROTECT(components);
            for (int i = 0; i < constructor->components->size; i++) {
                TpmcPatternValue *wc =
                    newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD,
                                        TPMCPATTERNVALUE_VAL_WILDCARD());
                int save2 = PROTECT(wc);
                TpmcPattern *replacement = newTpmcPattern(wc);
                PROTECT(replacement);
                replacement->path = constructor->components->entries[i]->path;
                pushTpmcPatternArray(components, replacement);
                UNPROTECT(save2);
            }
            TpmcConstructorPattern *newCons =
                newTpmcConstructorPattern(constructor->tag, constructor->info,
                                          components);
            PROTECT(newCons);
            TpmcPatternValue *patternValue =
                newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CONSTRUCTOR,
                                    TPMCPATTERNVALUE_VAL_CONSTRUCTOR
                                    (newCons));
            PROTECT(patternValue);
            TpmcPattern *replacement = newTpmcPattern(patternValue);
            replacement->path = pattern->path;
            UNPROTECT(save);
            return replacement;
        }
    }
    return pattern;
}

static TpmcIntArray *makeTpmcIntArray(int size, int initialValue) {
    TpmcIntArray *res = newTpmcIntArray();
    int save = PROTECT(res);
    for (int i = 0; i < size; ++i) {
        pushTpmcIntArray(res, initialValue);
    }
    UNPROTECT(save);
    return res;
}

static bool arcsAreExhaustive(int size, TpmcArcArray *arcs) {
    TpmcIntArray *flags = makeTpmcIntArray(size, 0);
    int save = PROTECT(flags);
    for (int i = 0; i < arcs->size; ++i) {
        TpmcArc *arc = arcs->entries[i];
        TpmcPattern *pattern = arc->test;
        if (pattern->pattern->type != TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
            cant_happen("arcsAreExhaustive given non-constructor arc");
        }
        LamTypeConstructorInfo *info =
            pattern->pattern->val.constructor->info;
        if (info->index >= size) {
            cant_happen
                ("arcsAreExhaustive given constructor with out-of-range index");
        }
        flags->entries[info->index] = 1;
    }
    bool res = true;
    for (int i = 0; i < size; i++) {
        if (flags->entries[i] == 0) {
            res = false;
            break;
        }
    }
    validateLastAlloc();
    UNPROTECT(save);
    return res;
}

static bool constructorsAreExhaustive(TpmcState *state) {
    TpmcTestState *testState = state->state->val.test;
    if (testState->arcs->size == 0) {
        cant_happen
            ("constructorsAreExhaustive() passed a test state with zero arcs");
    }
    TpmcPattern *pattern = testState->arcs->entries[0]->test;
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_WILDCARD) {
        cant_happen
            ("constructorsAreExhaustive() passed a test state with wildcards");
    } else if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        int size = pattern->pattern->val.constructor->info->size;
        return arcsAreExhaustive(size, testState->arcs);
    } else {
        return false;
    }
}

static TpmcPattern *makeNamedWildcardPattern(HashSymbol *path) {
    TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD,
                                               TPMCPATTERNVALUE_VAL_WILDCARD
                                               ());
    int save = PROTECT(wc);
    TpmcPattern *pattern = newTpmcPattern(wc);
    pattern->path = path;
    UNPROTECT(save);
    return pattern;
}

static TpmcState *deduplicateState(TpmcState *state,
                                   TpmcStateArray *knownStates) {
    for (int i = 0; i < knownStates->size; i++) {
        if (tpmcStateEq(state, knownStates->entries[i])) {
            validateLastAlloc();
            return knownStates->entries[i];
        }
    }
    pushTpmcStateArray(knownStates, state);
    return state;
}

static void collectPathsBoundByConstructor(TpmcPatternArray *components,
                                           TpmcVariableTable *boundVariables) 
{
    for (int i = 0; i < components->size; ++i) {
        TpmcPattern *pattern = components->entries[i];
        setTpmcVariableTable(boundVariables, pattern->path);
    }
}

static void collectPathsBoundByPattern(TpmcPattern *pattern,
                                       TpmcVariableTable *boundVariables) {
    // FIXME is this correct?
    setTpmcVariableTable(boundVariables, pattern->path);
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            cant_happen("collectPathsBoundByPattern encountered VAR");
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            cant_happen("collectPathsBoundByPattern encountered ASSIGNMENT");
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            break;
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:{
                TpmcPatternArray *components =
                    pattern->pattern->val.constructor->components;
                collectPathsBoundByConstructor(components, boundVariables);
            }
            break;
        default:
            cant_happen("unrecognised type %d in collectPathsBoundByPattern",
                        pattern->pattern->type);
    }
}

static TpmcVariableTable *variablesBoundByPattern(TpmcPattern *pattern) {
    TpmcVariableTable *boundVariables = newTpmcVariableTable();
    int save = PROTECT(boundVariables);
    collectPathsBoundByPattern(pattern, boundVariables);
    UNPROTECT(save);
    return boundVariables;
}

static TpmcVariableTable *getTestStatesFreeVariables(TpmcTestState *testState) {
    // The free variables of a test state is the union of the free variables
    // of the outgoing arcs, plus the test variable.
    TpmcVariableTable *freeVariables = newTpmcVariableTable();
    int save = PROTECT(freeVariables);
    setTpmcVariableTable(freeVariables, testState->path);
    for (int i = 0; i < testState->arcs->size; ++i) {
        TpmcArc *arc = testState->arcs->entries[i];
        if (arc->freeVariables == NULL) {
            cant_happen
                ("getTestStatesFreeVariables encountered arc wil null free variables");
        }
        int i = 0;
        HashSymbol *key;
        while ((key =
                iterateTpmcVariableTable(arc->freeVariables, &i)) != NULL) {
            setTpmcVariableTable(freeVariables, key);
        }
    }
    UNPROTECT(save);
    return freeVariables;
}

static TpmcVariableTable *getStatesFreeVariables(TpmcState *state) {
    if (state->freeVariables == NULL) {
        switch (state->state->type) {
            case TPMCSTATEVALUE_TYPE_TEST:
                state->freeVariables =
                    getTestStatesFreeVariables(state->state->val.test);
                break;
            case TPMCSTATEVALUE_TYPE_FINAL:
                cant_happen
                    ("getStatesFreeVariables encountered final state with null free variables");
            case TPMCSTATEVALUE_TYPE_ERROR:
                cant_happen
                    ("getStatesFreeVariables encountered error state with null free variables");
            default:
                cant_happen
                    ("unrecognised state type %d in getStateFreeVariables",
                     state->state->type);
        }
    }
    return state->freeVariables;
}

static TpmcArc *makeTpmcArc(TpmcState *state, TpmcPattern *pattern) {
    TpmcArc *arc = newTpmcArc(state, pattern);
    int save = PROTECT(arc);
    // the free variables of an arc are the free variables of its state minus the variables bound in the pattern
    TpmcVariableTable *boundVariables = variablesBoundByPattern(pattern);
    PROTECT(boundVariables);
    TpmcVariableTable *statesFreeVariables = getStatesFreeVariables(state);
    PROTECT(statesFreeVariables);
    int i = 0;
    HashSymbol *key;
    while ((key = iterateTpmcVariableTable(statesFreeVariables, &i)) != NULL) {
        if (!getTpmcVariableTable(boundVariables, key)) {
            setTpmcVariableTable(arc->freeVariables, key);
        }
    }
    state->refcount++;
    UNPROTECT(save);
    return arc;
}

static TpmcState *mixture(TpmcMatrix *M, TpmcStateArray *finalStates,
                          TpmcState *errorState,
                          TpmcStateArray *knownStates) {
    ENTER(mixture);
    // there is some column N whose topmost pattern is a constructor
    int index = findFirstConstructorColumn(M);
    DEBUG("mixture found constructor in column %d of %dx%d", index, M->width, M->height);
    TpmcPatternArray *N = extractMatrixColumn(M, index);
    int save = PROTECT(N);
    DEBUGN("mixture extracted column %d: ", index);
    IFDEBUGN(ppTpmcPatternArray(N));
    TpmcMatrix *MN = discardMatrixColumn(M, index);
    PROTECT(MN);
    // The goal is to build a test state with the variable v and some outgoing arcs
    // (one for each constructor and possibly a default arc).
    TpmcState *state = makeEmptyTestState(N->entries[0]->path);
    PROTECT(state);
    // For each constructor c in the selected column, its arc is defined as follows:
    for (int row = 0; row < N->size; row++) {
        TpmcPattern *c = N->entries[row];
        if (!patternIsWildcard(c)) {
            DEBUGN("mixture considering pattern on row %d ", row);
            IFDEBUGN(ppTpmcPattern(c));
            // Let {i1 , ... , ij} be the row-indices of the patterns in N that match c.
            TpmcIntArray *matchingIndices = findPatternsMatching(c, N);
            int save2 = PROTECT(matchingIndices);
            // Let {pat1 , ... , patj} be the patterns in the column corresponding to the indices computed above,
            TpmcPatternArray *matchingPatterns =
                extractColumnSubset(N, matchingIndices);
            PROTECT(matchingPatterns);
            // let n be the arity of the constructor c
            int arity = determineArity(c);
            DEBUG("arity %d\n", arity);
            // For each pati, its n sub-patterns are extracted;
            // if pati is a wildcard, n wildcards are produced instead, each tagged with the right path variable.
            TpmcMatrix *subPatternMatrix =
                makeSubPatternMatrix(matchingPatterns, arity);
            PROTECT(subPatternMatrix);
            // This matrix is then appended to the result of selecting, from each column in MN,
            // those rows whose indices are in {i1 , ... , ij}. 
            TpmcMatrix *prefixMatrix = extractMatrixRows(MN, matchingIndices);
            PROTECT(prefixMatrix);
            TpmcMatrix *newMatrix =
                appendMatrices(prefixMatrix, subPatternMatrix);
            PROTECT(newMatrix);
            // Finally the indices are used to select the corresponding final states that go with these rows.
            TpmcStateArray *newFinalStates =
                extractStateArraySubset(finalStates, matchingIndices);
            PROTECT(newFinalStates);
            // The arc for the constructor c is now defined as (c’,state), where c’ is c with any immediate
            // sub-patterns replaced by their path variables (thus c’ is a simple pattern)
            TpmcPattern *cPrime = replaceComponentsWithWildcards(c);
            PROTECT(cPrime);
            // and state is the result of recursively applying match to the new matrix and the new sequence of final states
            DEBUG("mixture calling match for normal case, column %d row %d of %dx%d", index, row, M->width, M->height);
            TpmcState *newState =
                tpmcMatch(newMatrix, newFinalStates, errorState, knownStates);
            PROTECT(newState);
            TpmcArc *arc = makeTpmcArc(newState, cPrime);
            PROTECT(arc);
            if (tpmcArcInArray(arc, state->state->val.test->arcs)) {
                arc->state->refcount--;
                validateLastAlloc();
            } else {
                DEBUGN("mixture constructed new arc: ");
                IFDEBUGN(ppTpmcArc(arc));
                pushTpmcArcArray(state->state->val.test->arcs, arc);
            }
            UNPROTECT(save2);
        }
    }
    // Finally, the possibility for matching failure is considered.
    // If the set of constructors is exhaustive, then no more arcs are computed
    if (constructorsAreExhaustive(state)) {
        TpmcState *res = deduplicateState(state, knownStates);
        UNPROTECT(save);
        LEAVE(mixture);
        return res;
    }
    // Otherwise, a default arc (_,state) is the last arc.
    // If there are any wildcard patterns in the selected column
    TpmcIntArray *wcIndices = newTpmcIntArray();
    PROTECT(wcIndices);
    int row = 0;
    TpmcPattern *candidate;
    while (iterateTpmcPatternArray(N, &row, &candidate, NULL)) {
        if (patternIsWildcard(candidate)) {
            pushTpmcIntArray(wcIndices, row - 1);
        }
    }
    if (countTpmcIntArray(wcIndices) > 0) {
        // then their rows are selected from the rest of the matrix and the final states
        TpmcMatrix *wcMatrix = extractMatrixRows(MN, wcIndices);
        PROTECT(wcMatrix);
        TpmcStateArray *wcFinalStates =
            extractStateArraySubset(finalStates, wcIndices);
        PROTECT(wcFinalStates);
        // and the state is the result of applying match to the new matrix and states
        DEBUG("mixture calling match for error case");
        TpmcState *wcState =
            tpmcMatch(wcMatrix, wcFinalStates, errorState, knownStates);
        PROTECT(wcState);
        TpmcPattern *wcPattern =
            makeNamedWildcardPattern(N->entries[0]->path);
        PROTECT(wcPattern);
        TpmcArc *wcArc = makeTpmcArc(wcState, wcPattern);
        PROTECT(wcArc);
        pushTpmcArcArray(state->state->val.test->arcs, wcArc);
        TpmcState *res = deduplicateState(state, knownStates);
        UNPROTECT(save);
        LEAVE(mixture);
        return res;
    } else {
        validateLastAlloc();
        // Otherwise, the error state is used after its reference count has been incremented
        TpmcPattern *errorPattern =
            makeNamedWildcardPattern(N->entries[0]->path);
        PROTECT(errorPattern);
        TpmcArc *errorArc = makeTpmcArc(errorState, errorPattern);
        PROTECT(errorArc);
        pushTpmcArcArray(state->state->val.test->arcs, errorArc);
        TpmcState *res = deduplicateState(state, knownStates);
        UNPROTECT(save);
        LEAVE(mixture);
        return res;
    }
}

TpmcState *tpmcMatch(TpmcMatrix *matrix, TpmcStateArray *finalStates,
                     TpmcState *errorState, TpmcStateArray *knownStates) {
    ENTER(match);
    IFDEBUG(ppTpmcMatrix(matrix));
    IFDEBUG(ppTpmcStateArray(finalStates));
    if (matrix->height == 0) {
        cant_happen("zero-height matrix passed to match");
    }
    TpmcState *res = NULL;
    if (noRemainingTests(matrix)) {
        DEBUG("match: variable rule applies");
        res = finalStates->entries[0];
    } else {
        DEBUG("match: mixture rule applies");
        res = mixture(matrix, finalStates, errorState, knownStates);
    }
    IFDEBUG(ppTpmcState(res));
    LEAVE(match);
    return res;
}
