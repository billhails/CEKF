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
#include "common.h"
#include "tpmc_match.h"
#include "tpmc_compare.h"
#include "debug_tpmc.h"
#include "debug_lambda.h"
#include "lambda_helper.h"
#include "symbol.h"

static int stateCounter = 0;

TpmcState *tpmcMakeState(TpmcStateValue *val) {
    return newTpmcState(stateCounter++, val);
}

static bool patternIsWildcard(TpmcMatrix *m, int x, int y) {
    TpmcPatternValueType type = TpmcMatrixIndex(m, x, y)->pattern->type;
    // comparisons are just pairs of wildcards
    return type == TPMCPATTERNVALUE_TYPE_WILDCARD || type == TPMCPATTERNVALUE_TYPE_COMPARISON;
}

static bool allRowZeroAreWildcards(TpmcMatrix *matrix) {
    for (int x = 0; x < matrix->width; x++) {
        if (!patternIsWildcard(matrix, x, 0)) {
            return false;
        }
    }
    return true;
}

static int findFirstConstructorColumn(TpmcMatrix *matrix) {
    for (int x = 0; x < matrix->width; x++) {
        if (!patternIsWildcard(matrix, x, 0)) {
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
    TpmcStateValue *val = newTpmcStateValue(TPMCSTATEVALUE_TYPE_TEST, TPMCSTATEVALUE_VAL_TEST(test));
    PROTECT(val);
    TpmcState *state = tpmcMakeState(val);
    UNPROTECT(save);
    return state;
}

static bool patternMatches(TpmcPattern *constructor, TpmcPattern *pattern) {
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            cant_happen("patternMatches ennncountered var");
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            return patternMatches(constructor, pattern->pattern->val.comparison->previous) &&
                patternMatches(constructor, pattern->pattern->val.comparison->current);
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            cant_happen("patternMatches encountered assignment");
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            return true;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            return constructor->pattern->type == TPMCPATTERNVALUE_TYPE_CHARACTER &&
                constructor->pattern->val.character == pattern->pattern->val.character;
        case TPMCPATTERNVALUE_TYPE_INTEGER:
            return constructor->pattern->type == TPMCPATTERNVALUE_TYPE_INTEGER &&
                constructor->pattern->val.integer == pattern->pattern->val.integer;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            // remember the "constructor" is really just "not a wildcard"
            return constructor->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR &&
                // pointer equivalence works for hash symbols
                constructor->pattern->val.constructor->tag == pattern->pattern->val.constructor->tag;
        default:
            cant_happen("unrecognized pattern type %d in patternMatches", pattern->pattern->type);
    }
}

static bool patternIndexMatches(TpmcMatrix *matrix, int x, int y, int yy) {
    if (y == yy) {
        return true;
    }
    TpmcPattern *constructor = TpmcMatrixIndex(matrix, x, y);
    TpmcPattern *pattern = TpmcMatrixIndex(matrix, x, yy);
    return patternMatches(constructor, pattern);
}

TpmcIntArray *findPatternsMatching(TpmcMatrix *matrix, int x, int y) {
    TpmcIntArray *res = newTpmcIntArray();
    int save = PROTECT(res);
    for (int yy = 0; yy < matrix->height; yy++) {
        if (patternIndexMatches(matrix, x, y, yy)) {
            res = pushTpmcIntArray(res, yy);
            REPLACE_PROTECT(save, res);
        }
    }
    UNPROTECT(save);
    return res;
}

static TpmcPatternArray *extractMatrixColumnSubset(TpmcMatrix *matrix, int x, TpmcIntArray *ys) {
    TpmcPatternArray *res = newTpmcPatternArray();
    int save = PROTECT(res);
    for (int i = 0; i < ys->size; ++i) {
        int y = ys->entries[i];
        res = pushTpmcPatternArray(res, TpmcMatrixIndex(matrix, x, y));
        REPLACE_PROTECT(save, res);
    }
    UNPROTECT(save);
    return res;
}

static TpmcStateArray *extractStateArraySubset(TpmcStateArray *all, TpmcIntArray *indices) {
    TpmcStateArray *res = newTpmcStateArray();
    int save = PROTECT(res);
    for (int i = 0; i < indices->size; ++i) {
        int j = indices->entries[i];
        res = pushTpmcStateArray(res, all->entries[j]);
        REPLACE_PROTECT(save, res);
    }
    UNPROTECT(save);
    return res;
}

static int determineArity(TpmcPattern *pattern) {
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        LamTypeConstructorInfo *info = pattern->pattern->val.constructor->info;
        // printf("%s has arity %d\n", pattern->pattern->val.constructor->tag->name, info->arity);
        return info->arity;
    } else {
        return 0;
    }
}

static void populateSubPatternMatrixRowWithWildcards(TpmcMatrix *matrix, int y, int arity, TpmcPattern *pattern) {
    char buf[512];
    for (int i = 0; i < arity; i++) {
        if (snprintf(buf, 512, "%s$%d", pattern->path->name, i) > 510) {
            cant_happen("internal structure limit exceeded in arg processing");
        }
        HashSymbol *path = newSymbol(buf);
        TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD, TPMCPATTERNVALUE_VAL_WILDCARD());
        int save = PROTECT(wc);
        TpmcMatrixIndex(matrix, i, y) = newTpmcPattern(wc);
        TpmcMatrixIndex(matrix, i, y)->path = path;
        UNPROTECT(save);
    }
}

static void populateSubPatternMatrixRowWithComponents(TpmcMatrix *matrix, int y, int arity, TpmcPattern *pattern) {
    if (arity != pattern->pattern->val.constructor->components->size) {
        cant_happen("arity %d does not match constructor arity %d in populateSubPatternMatrixRowWithComponents",
            arity, pattern->pattern->val.constructor->components->size);
    }
    for (int i = 0; i < arity; i++) {
        TpmcMatrixIndex(matrix, i, y) = pattern->pattern->val.constructor->components->entries[i];
    }
}

static void populateSubPatternMatrix(TpmcMatrix *matrix, TpmcPatternArray *patterns, int arity) {
    for (int i = 0; i < patterns->size; ++i) {
        TpmcPattern *pattern = patterns->entries[i];
        switch (pattern->pattern->type) {
            case TPMCPATTERNVALUE_TYPE_VAR:
                cant_happen("encountered pattern type var during populateSubPatternMatrix");
            case TPMCPATTERNVALUE_TYPE_COMPARISON:
            case TPMCPATTERNVALUE_TYPE_WILDCARD:
                populateSubPatternMatrixRowWithWildcards(matrix, i, arity, pattern);
                break;
            case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
                populateSubPatternMatrixRowWithComponents(matrix, i, arity, pattern);
                break;
            case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
                cant_happen("encountered pattern type assignment during populateSubPatternMatrix");
            case TPMCPATTERNVALUE_TYPE_CHARACTER:
                cant_happen("encountered pattern type char during populateSubPatternMatrix");
            case TPMCPATTERNVALUE_TYPE_INTEGER:
                cant_happen("encountered pattern type char during populateSubPatternMatrix");
            default:
                cant_happen("unrecognised pattern type %d during populateSubPatternMatrix", pattern->pattern->type);
        }
    }
}

static void copyMatrixExceptColAndOnlyRows(int col, TpmcIntArray *ys, TpmcMatrix *from, TpmcMatrix *to) {
    int tx = 0;
    for (int x = 0; x < from->width; x++) {
        if (x != col) {
            for (int iy = 0; iy < ys->size; ++iy) {
                int y = ys->entries[iy];
                // printf("copyMatrixExceptCol(%d), to[%d][%d] <= from[%d][%d]\n", col, tx, iy, x, y);
                TpmcMatrixIndex(to, tx, iy) = TpmcMatrixIndex(from, x, y);
            }
            tx++;
        }
    }
}

static void copyMatrixWithOffset(int offset, TpmcMatrix *from, TpmcMatrix *to) {
    for (int x = 0; x < from->width; x++) {
        for (int y = 0; y < from->height; ++y) {
            // printf("copyMatrixWithOffset(%d), to[%d][%d] <= from[%d][%d]\n", offset, x+offset, y, x, y);
            TpmcMatrixIndex(to, x + offset, y) = TpmcMatrixIndex(from, x, y);
        }
    }
}

static TpmcPattern *replaceComponentsWithWildcards(TpmcPattern *pattern) {
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        TpmcConstructorPattern *constructor = pattern->pattern->val.constructor;
        if (constructor->components->size > 0) {
            TpmcPatternArray *components = newTpmcPatternArray();
            int save = PROTECT(components);
            for (int i = 0; i < constructor->components->size; i++) {
                TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD, TPMCPATTERNVALUE_VAL_WILDCARD());
                int save2 = PROTECT(wc);
                TpmcPattern *replacement = newTpmcPattern(wc);
                PROTECT(replacement);
                replacement->path = constructor->components->entries[i]->path;
                components = pushTpmcPatternArray(components, replacement);
                REPLACE_PROTECT(save, components);
                UNPROTECT(save2);
            }
            TpmcConstructorPattern *newCons = newTpmcConstructorPattern(constructor->tag, constructor->info, components);
            PROTECT(newCons);
            TpmcPatternValue *patternValue = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CONSTRUCTOR, TPMCPATTERNVALUE_VAL_CONSTRUCTOR(newCons));
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
        res = pushTpmcIntArray(res, initialValue);
        REPLACE_PROTECT(save, res);
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
        LamTypeConstructorInfo *info = pattern->pattern->val.constructor->info;
        if (info->index >= size) {
            cant_happen("arcsAreExhaustive given constructor with out-of-range index");
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
    return res;
}

static bool constructorsAreExhaustive(TpmcState *state) {
    TpmcTestState *testState = state->state->val.test;
    if (testState->arcs->size == 0) {
        cant_happen("constructorsAreExhaustive() passed a test state with zero arcs");
    }
    TpmcPattern *pattern = testState->arcs->entries[0]->test;
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_WILDCARD) {
        cant_happen("constructorsAreExhaustive() passed a test state with wildcards");
    } else if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        int size = pattern->pattern->val.constructor->info->size;
        return arcsAreExhaustive(size, testState->arcs);
    } else {
        return false;
    }
}

static TpmcPattern *makeNamedWildcardPattern(HashSymbol *path) {
    TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD, TPMCPATTERNVALUE_VAL_WILDCARD());
    int save = PROTECT(wc);
    TpmcPattern *pattern = newTpmcPattern(wc);
    pattern->path = path;
    UNPROTECT(save);
    return pattern;
}

static TpmcState *deduplicateState(TpmcState *state, TpmcStateArrayContainer *knownStates) {
    for (int i = 0; i < knownStates->array->size; i++) {
        if (tpmcStateEq(state, knownStates->array->entries[i])) {
            knownStates->array->entries[i]->refcount++;
            return knownStates->array->entries[i];
        }
    }
    int save = PROTECT(state);
    knownStates->array = pushTpmcStateArray(knownStates->array, state);
    state->refcount++;
    UNPROTECT(save);
    return state;
}

static TpmcState *mixture(TpmcMatrix *matrix, TpmcStateArray *finalStates, TpmcState *errorState, TpmcStateArrayContainer *knownStates) {
    printf("enter mixture\n");
    // there is some column whose topmost pattern is a constructor
    int x = findFirstConstructorColumn(matrix);
    // The goal is to build a test state with the variable v and some outgoing arcs (one for each constructor and possibly a default arc).
    TpmcState *state = makeEmptyTestState(TpmcMatrixIndex(matrix, x, 0)->path);
    int save = PROTECT(state);
    for (int y = 0; y < matrix->height; y++) {
        if (!patternIsWildcard(matrix, x, y)) {
            // For each constructor c in the selected column, its arc is defined as follows:
            TpmcPattern *c = TpmcMatrixIndex(matrix, x, y);
            // Let {i1 , ... , ij} be the row-indices of the patterns in the column that match c.
            TpmcIntArray *matchingIndices = findPatternsMatching(matrix, x, y);
            int save2 = PROTECT(matchingIndices);
            // Let {pat1 , ... , patj} be the patterns in the column corresponding to the indices computed above,
            TpmcPatternArray *matchingPatterns = extractMatrixColumnSubset(matrix, x, matchingIndices);
            PROTECT(matchingPatterns);
            // let n be the arity of the constructor c
            int arity = determineArity(c);
            //  ... a pattern matrix with n columns and j rows (create ahead of time)
            TpmcMatrix *subPatternMatrix = newTpmcMatrix(arity, matchingPatterns->size); // could be zero-width
            PROTECT(subPatternMatrix);
            // For each pati, its n sub-patterns are extracted;
            // if pati is a wildcard, n wildcards are produced instead, each tagged with the right path variable.
            populateSubPatternMatrix(subPatternMatrix, matchingPatterns, arity);
            // This matrix is then appended to the result of selecting, from each column in the rest of the
            // original matrix, those rows whose indices are in {i1 , ... , ij}. 
            TpmcMatrix *newMatrix = newTpmcMatrix(matrix->width + arity - 1, matchingPatterns->size);
            PROTECT(newMatrix);
            copyMatrixExceptColAndOnlyRows(x, matchingIndices, matrix, newMatrix);
            copyMatrixWithOffset(matrix->width - 1, subPatternMatrix, newMatrix);
            // Finally the indices are used to select the corresponding final states that go with these rows.
            TpmcStateArray *newFinalStates = extractStateArraySubset(finalStates, matchingIndices);
            PROTECT(newFinalStates);
            // The arc for the constructor c is now defined as (c’,state), where c’ is c with any immediate
            // sub-patterns replaced by their path variables (thus c’ is a simple pattern)
            TpmcPattern *cPrime = replaceComponentsWithWildcards(c);
            UNPROTECT(save2);
            save2 = PROTECT(newMatrix);
            PROTECT(newFinalStates);
            PROTECT(cPrime);
            // and state is the result of recursively applying match to the new matrix and the new sequence of final states
            TpmcState *newState = tpmcMatch(newMatrix, newFinalStates, errorState, knownStates);
            PROTECT(newState);
            TpmcArc *arc = newTpmcArc(newState, cPrime);
            PROTECT(arc);
            state->state->val.test->arcs = pushTpmcArcArray(state->state->val.test->arcs, arc);
            UNPROTECT(save2);
        }
    }
    // Finally, the possibility for matching failure is considered.
    // If the set of constructors is exhaustive, then no more arcs are computed
    if (constructorsAreExhaustive(state)) {
        UNPROTECT(save);
        // printf("leave mixture\n");
        return deduplicateState(state, knownStates);
    }
    // Otherwise, a default arc (_,state) is the last arc.
    // If there are any wildcard patterns in the selected column
    TpmcIntArray *wcIndices = newTpmcIntArray();
    int save2 = PROTECT(wcIndices);
    for (int y = 0; y < matrix->height; y++) {
        if (patternIsWildcard(matrix, x, y)) {
            wcIndices = pushTpmcIntArray(wcIndices, y);
            REPLACE_PROTECT(save2, wcIndices);
        }
    }
    if (wcIndices->size > 0) {
        // then their rows are selected from the rest of the matrix and the final states
        TpmcMatrix *wcMatrix = newTpmcMatrix(matrix->width - 1, wcIndices->size);
        PROTECT(wcMatrix);
        copyMatrixExceptColAndOnlyRows(x, wcIndices, matrix, wcMatrix);
        TpmcStateArray *wcFinalStates = extractStateArraySubset(finalStates, wcIndices);
        PROTECT(wcFinalStates);
        // and the state is the result of applying match to the new matrix and states
        TpmcState *wcState = tpmcMatch(wcMatrix, wcFinalStates, errorState, knownStates);
        PROTECT(wcState);
        TpmcPattern *wcPattern = makeNamedWildcardPattern(TpmcMatrixIndex(matrix, x, 0)->path);
        PROTECT(wcPattern);
        TpmcArc *wcArc = newTpmcArc(wcState, wcPattern);
        PROTECT(wcArc);
        state->state->val.test->arcs = pushTpmcArcArray(state->state->val.test->arcs, wcArc);
        UNPROTECT(save);
        // printf("leave mixture\n");
        return deduplicateState(state, knownStates);
    } else {
        // Otherwise, the error state is used after its reference count has been incremented
        TpmcPattern *errorPattern = makeNamedWildcardPattern(TpmcMatrixIndex(matrix, x, 0)->path);
        PROTECT(errorPattern);
        errorState->refcount++;
        TpmcArc *errorArc = newTpmcArc(errorState, errorPattern);
        PROTECT(errorArc);
        state->state->val.test->arcs = pushTpmcArcArray(state->state->val.test->arcs, errorArc);
        UNPROTECT(save);
        // printf("leave mixture\n");
        return deduplicateState(state, knownStates);
    }
}

TpmcState *tpmcMatch(TpmcMatrix *matrix, TpmcStateArray *finalStates, TpmcState *errorState, TpmcStateArrayContainer *knownStates) {
    if (matrix->height == 0) {
        cant_happen("zero-height matrix passed to match");
    }
    if (allRowZeroAreWildcards(matrix)) {
        // printf("variable rule applies\n");
        finalStates->entries[0]->refcount++;
        return finalStates->entries[0];
    }
    fprintf(stderr, "mixture rule applies\n");
    return mixture(matrix, finalStates, errorState, knownStates);
}

