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
#include <stdlib.h>
#include "common.h"
#include "tpmc_match.h"
#include "tpmc_compare.h"
#include "tpmc_debug.h"
#include "tpmc_pp.h"
#include "lambda_debug.h"
#include "lambda_helper.h"
#include "symbol.h"
#include "types.h"
#include "parser_info.h"

#ifdef DEBUG_TPMC_MATCH
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static TpmcState *match(TpmcMatrix *matrix, TpmcStateArray *finalStates,
                        TpmcState *errorState, TpmcStateArray *knownStates,
                        TpmcStateTable *stateTable, bool *unsafe, ParserInfo I);

static HashSymbol *stampToSymbol(int stamp) {
    char buf[32];
    snprintf(buf, sizeof(buf), "#state%d", stamp);
    return newSymbol(buf);
}

TpmcState *tpmcMatch(TpmcMatrix *matrix, TpmcStateArray *finalStates,
                     TpmcState *errorState, TpmcStateArray *knownStates,
                     bool allow_unsafe, ParserInfo I) {
#ifdef DEBUG_TPMC_MATCH
    // system("clear");
#endif
    TpmcStateTable *stateTable = newTpmcStateTable();
    int save = PROTECT(stateTable);
    bool unsafe = false;
    TpmcState *result = match(matrix, finalStates, errorState, knownStates, stateTable, &unsafe, I);
    UNPROTECT(save);
    if (unsafe) {
        if (!allow_unsafe) {
            can_happen("unsafe function must be declared unsafe at %s line %d", I.filename, I.lineno);
        }
    } else {
        if (allow_unsafe) {
            can_happen("safe function declared unsafe at %s line %d", I.filename, I.lineno);
        }
    }
    return result;
}

TpmcState *tpmcMakeState(TpmcStateValue *val) {
    static int counter = 0;
    return newTpmcState(counter++, val);
}

static bool patternIsWildcard(TpmcPattern *pattern) {
    return pattern->pattern->type == TPMCPATTERNVALUE_TYPE_WILDCARD;
}

static bool patternIsComparison(TpmcPattern *pattern) {
    return pattern->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON;
}

static bool topRowOnlyVariables(TpmcMatrix *matrix) {
    for (Index x = 0; x < matrix->width; x++) {
        if (!patternIsWildcard(getTpmcMatrixIndex(matrix, x, 0))) {
            return false;
        }
    }
    return true;
}

static bool columnHasComparisons(int x, TpmcMatrix *matrix) {
    for (Index y = 0; y < matrix->height; y++) {
        if (patternIsComparison(getTpmcMatrixIndex(matrix, x, y))) {
            return true;
        }
    }
    return false;
}

static int findFirstConstructorColumn(TpmcMatrix *matrix) {
    for (Index x = 0; x < matrix->width; x++) {
        if (!patternIsWildcard(getTpmcMatrixIndex(matrix, x, 0))) {
            DEBUG("findFirstConstructorColumn(%d x %d) => %d", matrix->width, matrix->height, x);
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
    TpmcStateValue *val = newTpmcStateValue_Test(test);
    PROTECT(val);
    TpmcState *testState = tpmcMakeState(val);
    UNPROTECT(save);
    return testState;
}

static bool patternMatches(TpmcPattern *constructor, TpmcPattern *pattern) {
    bool isComparison =
        (constructor->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON);
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            cant_happen("patternMatches encountered var");
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
                        && cmpMaybeBigInt(constructor->pattern->val.bigInteger,
                                     pattern->pattern->val.bigInteger) == 0);
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
        case TPMCPATTERNVALUE_TYPE_TUPLE:{
                bool res =
                    (constructor->pattern->type == TPMCPATTERNVALUE_TYPE_TUPLE) || isComparison;
                if (countTpmcPatternArray(constructor->pattern->val.tuple) !=
                    countTpmcPatternArray(pattern->pattern->val.tuple)) {
                       can_happen("tuple arity mismatch");
                       return false;
                }
                return res;
            }
        default:
            cant_happen("unrecognized pattern type %s",
                        tpmcPatternValueTypeName(pattern->pattern->type));
    }
}

TpmcIntArray *findPatternsMatching(TpmcPattern *c, TpmcPatternArray *N) {
    TpmcIntArray *res = newTpmcIntArray();
    int save = PROTECT(res);
    Index i = 0;
    TpmcPattern *candidate;
    while (iterateTpmcPatternArray(N, &i, &candidate, NULL)) {
        if (patternMatches(c, candidate)) {
            pushTpmcIntArray(res, i - 1);
        }
    }
    UNPROTECT(save);
    return res;
}

static TpmcPatternArray *extractColumnSubset(TpmcIntArray *ys,
                                             TpmcPatternArray *N) {
    TpmcPatternArray *res = newTpmcPatternArray("extractColumnSubset");
    int save = PROTECT(res);
    Index i = 0;
    int y;
    while (iterateTpmcIntArray(ys, &i, &y, NULL)) {
        pushTpmcPatternArray(res, N->entries[y]);
    }
    UNPROTECT(save);
    return res;
}

static TpmcPatternArray *extractMatrixColumn(int x, TpmcMatrix *matrix) {
    TpmcPatternArray *res = newTpmcPatternArray("extractMatrixColumn");
    int save = PROTECT(res);
    for (Index y = 0; y < matrix->height; y++) {
        pushTpmcPatternArray(res, getTpmcMatrixIndex(matrix, x, y));
    }
    UNPROTECT(save);
    return res;
}

static TpmcMatrix *discardMatrixColumn(Index column, TpmcMatrix *matrix) {
    TpmcMatrix *res = newTpmcMatrix(matrix->width - 1, matrix->height);
    int save = PROTECT(res);
    for (Index x = 0; x < matrix->width; x++) {
        for (Index y = 0; y < matrix->height; y++) {
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

static TpmcMatrix *extractMatrixRows(TpmcIntArray *indices,
                                     TpmcMatrix *matrix) {
    TpmcMatrix *res = newTpmcMatrix(matrix->width, indices->size);
    int save = PROTECT(res);
    int resy = 0;
    int iy = 0;
    Index i = 0;
    while (iterateTpmcIntArray(indices, &i, &iy, NULL)) {
        for (Index x = 0; x < res->width; ++x) {
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
    for (Index x = 0; x < res->width; ++x) {
        for (Index y = 0; y < res->height; ++y) {
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

static TpmcStateArray *extractStateArraySubset(TpmcIntArray *indices, TpmcStateArray *all) {
    TpmcStateArray *res = newTpmcStateArray("extractStateArraySubset");
    int save = PROTECT(res);
    for (Index i = 0; i < indices->size; ++i) {
        int j = indices->entries[i];
        pushTpmcStateArray(res, all->entries[j]);
    }
    UNPROTECT(save);
    return res;
}

static int arityOf(TpmcPattern *pattern) {
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:{
            LamTypeConstructorInfo *info =
                pattern->pattern->val.constructor->info;
            return info->arity;
        }
        case TPMCPATTERNVALUE_TYPE_TUPLE:
            return countTpmcPatternArray(pattern->pattern->val.tuple);
        default:
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
            newTpmcPatternValue_Wildcard();
        int save = PROTECT(wc);
        setTpmcMatrixIndex(matrix, i, y, newTpmcPattern(wc));
        getTpmcMatrixIndex(matrix, i, y)->path = path;
        UNPROTECT(save);
    }
}

static void populateSubPatternMatrixRowWithConstructor(TpmcMatrix *matrix,
                                                      int y, Index arity,
                                                      TpmcPattern *pattern,
                                                      ParserInfo I) {
    if (arity != pattern->pattern->val.constructor->components->size) {
        ppTpmcPattern(pattern);
        can_happen
            ("\narity %d does not match constructor arity %d at %s line %d",
             arity, pattern->pattern->val.constructor->components->size, I.filename, I.lineno);
        exit(1);
    }
    for (Index i = 0; i < arity; i++) {
        TpmcPattern *entry =
            pattern->pattern->val.constructor->components->entries[i];
        setTpmcMatrixIndex(matrix, i, y, entry);
    }
}

static void populateSubPatternMatrixRowWithTuple(TpmcMatrix *matrix,
                                                 int y, Index arity,
                                                 TpmcPattern *pattern,
                                                 ParserInfo I) {
    if (arity != countTpmcPatternArray(pattern->pattern->val.tuple)) {
        ppTpmcPattern(pattern);
        can_happen
            ("arity %d does not match tuple arity %d at %s line %d",
             arity, countTpmcPatternArray(pattern->pattern->val.tuple), I.filename, I.lineno);
             exit(1);
    }
    for (Index i = 0; i < arity; i++) {
        TpmcPattern *entry = pattern->pattern->val.tuple->entries[i];
        setTpmcMatrixIndex(matrix, i, y, entry);
    }
}

static TpmcMatrix *makeSubPatternMatrix(TpmcPatternArray *patterns, int arity, ParserInfo I) {
    TpmcMatrix *matrix = newTpmcMatrix(arity, patterns->size);
    if (arity == 0) {
        return matrix;
    }
    int save = PROTECT(matrix);
    for (Index i = 0; i < patterns->size; ++i) {
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
                populateSubPatternMatrixRowWithConstructor(matrix, i, arity,
                                                          pattern, I);
                break;
            case TPMCPATTERNVALUE_TYPE_TUPLE:
                populateSubPatternMatrixRowWithTuple(matrix, i, arity, pattern, I);
                break;
            case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
                cant_happen("encountered pattern type assignment");
            case TPMCPATTERNVALUE_TYPE_CHARACTER:
                cant_happen("encountered pattern type char");
            case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
                cant_happen("encountered pattern type int");
            default:
                cant_happen("unrecognised pattern type %s",
                     tpmcPatternValueTypeName(pattern->pattern->type));
        }
    }
    UNPROTECT(save);
    return matrix;
}

static TpmcPatternArray *replaceComponentsWithWildcards(TpmcPatternArray *components) {
    ENTER(replaceComponentsWithWildcards);
    TpmcPatternArray *result =
        newTpmcPatternArray("replaceComponentsWithWildcards");
    int save = PROTECT(result);
    for (Index i = 0; i < components->size; i++) {
        DEBUG("i = %d, size = %d", i, components->size);
        TpmcPatternValue *wc = newTpmcPatternValue_Wildcard();
        int save2 = PROTECT(wc);
        TpmcPattern *replacement = newTpmcPattern(wc);
        PROTECT(replacement);
        replacement->path = components->entries[i]->path;
        pushTpmcPatternArray(result, replacement);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    LEAVE(replaceComponentsWithWildcards);
    return result;
}

static TpmcPattern *replacePatternComponentsWithWildcards(TpmcPattern *pattern) {
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR: {
            TpmcConstructorPattern *constructor =
                pattern->pattern->val.constructor;
            if (constructor->components->size > 0) {
                TpmcPatternArray *components = replaceComponentsWithWildcards(constructor->components);
                int save = PROTECT(components);
                TpmcConstructorPattern *newCons =
                    newTpmcConstructorPattern(constructor->tag, constructor->nameSpace, constructor->info,
                                              components);
                PROTECT(newCons);
                TpmcPatternValue *patternValue =
                    newTpmcPatternValue_Constructor(newCons);
                PROTECT(patternValue);
                TpmcPattern *replacement = newTpmcPattern(patternValue);
                replacement->path = pattern->path;
                UNPROTECT(save);
                return replacement;
            } else {
                return pattern;
            }
        }
        case TPMCPATTERNVALUE_TYPE_TUPLE: {
            TpmcPatternArray *tuple = pattern->pattern->val.tuple;
            if (tuple->size > 0) {
                TpmcPatternArray *components = replaceComponentsWithWildcards(tuple);
                int save = PROTECT(components);
                TpmcPatternValue *patternValue = newTpmcPatternValue_Tuple(components);
                    PROTECT(patternValue);
                TpmcPattern *replacement = newTpmcPattern(patternValue);
                replacement->path = pattern->path;
                UNPROTECT(save);
                return replacement;
            } else {
                return pattern;
            }
        }
        default:
            return pattern;
    }
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

static bool arcsAreExhaustive(int size, TpmcArcArray *arcs, ParserInfo I) {
    TpmcIntArray *flags = makeTpmcIntArray(size, 0);
    int save = PROTECT(flags);
    for (Index i = 0; i < arcs->size; ++i) {
        TpmcArc *arc = arcs->entries[i];
        TpmcPattern *pattern = arc->test;
        switch (pattern->pattern->type) {
            case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR: {
                LamTypeConstructorInfo *info =
                    pattern->pattern->val.constructor->info;
                if (info->index >= size) {
                    cant_happen("arcsAreExhaustive given constructor %s with out-of-range index (%d >= %d) while parsing %s, line %d", info->name->name, info->index, size, I.filename, I.lineno);
                }
                flags->entries[info->index] = 1;
            }
            break;
            case TPMCPATTERNVALUE_TYPE_TUPLE: {
                // tuples are exhaustive
                UNPROTECT(save);
                return true;
            }
            break;
            default:
                cant_happen("arcsAreExhaustive given non-constructor arc while parsing %s, line %d", I.filename, I.lineno);
        }
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

static bool constructorsAreExhaustive(TpmcState *state, ParserInfo I) {
    TpmcTestState *testState = state->state->val.test;
    if (testState->arcs->size == 0) {
        return false;
    }
    TpmcPattern *pattern = testState->arcs->entries[0]->test;
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_WILDCARD) {
        cant_happen
            ("constructorsAreExhaustive() passed a test state with wildcards while parsing %s, line %d", I.filename, I.lineno);
    } else if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        int size = pattern->pattern->val.constructor->info->size;
        return arcsAreExhaustive(size, testState->arcs, I);
    } else if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_TUPLE) {
        int size = pattern->pattern->val.tuple->size;
        return arcsAreExhaustive(size, testState->arcs, I);
    } else {
        return false;
    }
}

static TpmcPattern *makeNamedWildcardPattern(HashSymbol *path) {
    TpmcPatternValue *wc = newTpmcPatternValue_Wildcard();
    int save = PROTECT(wc);
    TpmcPattern *pattern = newTpmcPattern(wc);
    pattern->path = path;
    UNPROTECT(save);
    return pattern;
}

static TpmcState *deduplicateState(TpmcState *state,
                                   TpmcStateArray *knownStates,
                                   TpmcStateTable *stateTable) {
    // First check the hash table for fast lookup
    HashSymbol *key = stampToSymbol(state->stamp);
    TpmcState *existing = NULL;
    if (getTpmcStateTable(stateTable, key, &existing)) {
        // Found in hash table, verify with deep equality
        if (tpmcStateEq(state, existing)) {
            validateLastAlloc();
            return existing;
        }
    }
    
    // Not in hash table, do full linear search (for states with different stamps but same structure)
    for (Index i = 0; i < knownStates->size; i++) {
        if (tpmcStateEq(state, knownStates->entries[i])) {
            validateLastAlloc();
            // Also add to hash table for future lookups
            setTpmcStateTable(stateTable, key, knownStates->entries[i]);
            return knownStates->entries[i];
        }
    }
    
    // New state, add to both array and hash table
    pushTpmcStateArray(knownStates, state);
    setTpmcStateTable(stateTable, key, state);
    return state;
}

static void collectPathsBoundByConstructor(TpmcPatternArray *components,
                                           TpmcVariableTable *boundVariables) 
{
    for (Index i = 0; i < components->size; ++i) {
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
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:{
                TpmcPatternArray *components =
                    pattern->pattern->val.constructor->components;
                collectPathsBoundByConstructor(components, boundVariables);
            }
            break;
        case TPMCPATTERNVALUE_TYPE_TUPLE:{
                TpmcPatternArray *components = pattern->pattern->val.tuple;
                collectPathsBoundByConstructor(components, boundVariables);
            }
            break;
        default:
            cant_happen("unrecognised type %s",
                        tpmcPatternValueTypeName(pattern->pattern->type));
    }
}

static TpmcVariableTable *variablesBoundByPattern(TpmcPattern *pattern) {
    ENTER(variablesBoundByPattern);
    TpmcVariableTable *boundVariables = newTpmcVariableTable();
    int save = PROTECT(boundVariables);
    collectPathsBoundByPattern(pattern, boundVariables);
    LEAVE(variablesBoundByPattern);
    UNPROTECT(save);
    return boundVariables;
}

static TpmcVariableTable *getTestStatesFreeVariables(TpmcTestState *testState) {
    // The free variables of a test state is the union of the free variables
    // of the outgoing arcs, plus the test variable.
    TpmcVariableTable *freeVariables = newTpmcVariableTable();
    int save = PROTECT(freeVariables);
    setTpmcVariableTable(freeVariables, testState->path);
    for (Index i = 0; i < testState->arcs->size; ++i) {
        TpmcArc *arc = testState->arcs->entries[i];
        if (arc->freeVariables == NULL) {
            cant_happen
                ("getTestStatesFreeVariables encountered arc wil null free variables");
        }
        Index i = 0;
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
                    ("unrecognised state type %s in getStateFreeVariables",
                     tpmcStateValueTypeName(state->state->type));
        }
    }
    return state->freeVariables;
}

static void addFreeVariablesRequiredByPattern(TpmcPattern *pattern, TpmcVariableTable *freeVariables) {
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON) {
        TpmcPattern *previous = pattern->pattern->val.comparison->previous;
        HashSymbol *name = previous->path;
        setTpmcVariableTable(freeVariables, name);
    }
}

static TpmcArc *makeTpmcArc(TpmcPattern *pattern, TpmcState *state) {
    ENTER(makeTpmcArc);
    DEBUG("makeTpmcArc pattern=%p state=%p", pattern, state);
    int save = PROTECT(pattern);
    PROTECT(state);
    TpmcArc *arc = newTpmcArc(state, pattern);
    PROTECT(arc);
    // the free variables of an arc are the free variables of its state minus the variables bound in the pattern
    TpmcVariableTable *boundVariables = variablesBoundByPattern(pattern);
    PROTECT(boundVariables);
    validateLastAlloc();
    TpmcVariableTable *statesFreeVariables = getStatesFreeVariables(state);
    PROTECT(statesFreeVariables);
    Index i = 0;
    HashSymbol *key;
    while ((key = iterateTpmcVariableTable(statesFreeVariables, &i)) != NULL) {
        if (!getTpmcVariableTable(boundVariables, key)) {
            setTpmcVariableTable(arc->freeVariables, key);
        }
    }
    addFreeVariablesRequiredByPattern(pattern, arc->freeVariables);
    state->refCount++;
    LEAVE(makeTpmcArc);
    UNPROTECT(save);
    return arc;
}

static TpmcIntArray *findWcIndices(TpmcPatternArray *N) {
    TpmcIntArray *wcIndices = newTpmcIntArray();
    int save = PROTECT(wcIndices);
    Index row = 0;
    TpmcPattern *candidate;
    while (iterateTpmcPatternArray(N, &row, &candidate, NULL)) {
        if (patternIsWildcard(candidate)) {
            pushTpmcIntArray(wcIndices, row - 1);
        }
    }
    UNPROTECT(save);
    return wcIndices;
}

static TpmcState *mixture(TpmcMatrix *M, TpmcStateArray *finalStates,
                          TpmcState *errorState, TpmcStateArray *knownStates,
                          TpmcStateTable *stateTable, bool *unsafe, ParserInfo I) {
    ENTER(mixture);
    // there is some column N whose topmost pattern is a constructor
    int firstConstructorColumn = findFirstConstructorColumn(M);
    // this heuristic allows for comparisons to work:
    if (firstConstructorColumn > 0 && columnHasComparisons(firstConstructorColumn, M)) {
        firstConstructorColumn = 0;
    }
    TpmcPatternArray *N = extractMatrixColumn(firstConstructorColumn, M);
    int save = PROTECT(N);
    // let M-N be all the columns in M except N
    TpmcMatrix *MN = discardMatrixColumn(firstConstructorColumn, M);
    PROTECT(MN);
    // The goal is to build a test state with the variable v and some outgoing arcs
    // (one for each constructor and possibly a default arc).
    TpmcState *testState = makeEmptyTestState(N->entries[0]->path);
    PROTECT(testState);
    for (Index row = 0; row < N->size; row++) {
        TpmcPattern *c = N->entries[row];
        // For each constructor c in the selected column, its arc is defined as follows:
        if (!patternIsWildcard(c)) {
            // Skip if we've already added an arc for this exact constructor pattern
            // This prevents redundant recursive match() calls for duplicate constructors
            bool alreadyProcessed = false;
            for (Index i = 0; i < testState->state->val.test->arcs->size; i++) {
                TpmcArc *existingArc = testState->state->val.test->arcs->entries[i];
                // Check if the constructor pattern matches (not the full arc, just the pattern)
                if (c->pattern->type == existingArc->test->pattern->type) {
                    if (c->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
                        if (c->pattern->val.constructor->tag == existingArc->test->pattern->val.constructor->tag) {
                            alreadyProcessed = true;
                            break;
                        }
                    } else if (c->pattern->type == TPMCPATTERNVALUE_TYPE_CHARACTER) {
                        if (c->pattern->val.character == existingArc->test->pattern->val.character) {
                            alreadyProcessed = true;
                            break;
                        }
                    } else if (c->pattern->type == TPMCPATTERNVALUE_TYPE_BIGINTEGER) {
                        if (cmpMaybeBigInt(c->pattern->val.bigInteger, existingArc->test->pattern->val.bigInteger) == 0) {
                            alreadyProcessed = true;
                            break;
                        }
                    } else if (c->pattern->type == TPMCPATTERNVALUE_TYPE_TUPLE) {
                        // All tuples of same arity are considered the same constructor
                        alreadyProcessed = true;
                        break;
                    } else if (c->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON) {
                        // Comparisons might be unique, check deeper
                        if (tpmcPatternEq(c, existingArc->test)) {
                            alreadyProcessed = true;
                            break;
                        }
                    }
                }
            }
            if (alreadyProcessed) {
                continue;
            }
            // Let {i1 , ... , ij} be the row-indices of the patterns in N that match c.
            TpmcIntArray *indicesMatchingC = findPatternsMatching(c, N);
            int save2 = PROTECT(indicesMatchingC);
            // Let {pat1 , ... , patj} be the patterns in the column corresponding to the indices computed above,
            TpmcPatternArray *patternsMatchingC = extractColumnSubset(indicesMatchingC, N);
            PROTECT(patternsMatchingC);
            // let n be the arity of the constructor c
            int n = arityOf(c);
            // For each pati, its n sub-patterns are extracted;
            // if pati is a wildcard, n wildcards are produced instead, each tagged with the right path variable.
            TpmcMatrix *subPatternsMatchingC = makeSubPatternMatrix(patternsMatchingC, n, I);
            PROTECT(subPatternsMatchingC);
            // This matrix is then appended to the result of selecting, from each column in MN,
            // those rows whose indices are in {i1 , ... , ij}. 
            TpmcMatrix *prefixMatrix = extractMatrixRows(indicesMatchingC, MN);
            PROTECT(prefixMatrix);
            TpmcMatrix *newMatrix = appendMatrices(prefixMatrix, subPatternsMatchingC);
            PROTECT(newMatrix);
            // Finally the indices are used to select the corresponding final states that go with these rows.
            TpmcStateArray *newFinalStates = extractStateArraySubset(indicesMatchingC, finalStates);
            PROTECT(newFinalStates);
            // The arc for the constructor c is now defined as (c’,state), where c’ is c with any immediate
            // sub-patterns replaced by their path variables (thus c’ is a simple pattern)
            TpmcPattern *cPrime = replacePatternComponentsWithWildcards(c);
            PROTECT(cPrime);
            // and state is the result of recursively applying match to the new matrix and the new sequence of final states
            TpmcState *newState = match(newMatrix, newFinalStates, errorState, knownStates, stateTable, unsafe, I);
            PROTECT(newState);
            TpmcArc *arc = makeTpmcArc(cPrime, newState);
            PROTECT(arc);
            // Add the arc (duplicate constructors are now filtered earlier in the loop)
            pushTpmcArcArray(testState->state->val.test->arcs, arc);
            UNPROTECT(save2);
        }
    }
    // Finally, the possibility for matching failure is considered.
    // If the set of constructors is exhaustive, then no more arcs are computed
    if (!constructorsAreExhaustive(testState, I)) {
        // Otherwise, a default arc (_,state) is the last arc.
        // If there are any wildcard patterns in the selected column
        TpmcIntArray *wcIndices = findWcIndices(N);
        PROTECT(wcIndices);
        if (countTpmcIntArray(wcIndices) > 0) {
            // then their rows are selected from the rest of the matrix and the final states
            TpmcMatrix *wcMatrix = extractMatrixRows(wcIndices, MN);
            PROTECT(wcMatrix);
            TpmcStateArray *wcFinalStates = extractStateArraySubset(wcIndices, finalStates);
            PROTECT(wcFinalStates);
            // and the state is the result of applying match to the new matrix and states
            TpmcState *wcState = match(wcMatrix, wcFinalStates, errorState, knownStates, stateTable, unsafe, I);
            PROTECT(wcState);
            TpmcPattern *wcPattern = makeNamedWildcardPattern(N->entries[0]->path);
            PROTECT(wcPattern);
            TpmcArc *wcArc = makeTpmcArc(wcPattern, wcState);
            PROTECT(wcArc);
            pushTpmcArcArray(testState->state->val.test->arcs, wcArc);
        } else {
            validateLastAlloc();
            *unsafe = true;
            // Otherwise, the error state is used after its reference count has been incremented
            TpmcPattern *errorPattern = makeNamedWildcardPattern(N->entries[0]->path);
            PROTECT(errorPattern);
            TpmcArc *errorArc = makeTpmcArc(errorPattern, errorState);
            PROTECT(errorArc);
            pushTpmcArcArray(testState->state->val.test->arcs, errorArc);
        }
    }
    TpmcState *res = deduplicateState(testState, knownStates, stateTable);
    UNPROTECT(save);
    LEAVE(mixture);
    return res;
}

static TpmcState *match(TpmcMatrix *matrix, TpmcStateArray *finalStates,
                        TpmcState *errorState, TpmcStateArray *knownStates,
                        TpmcStateTable *stateTable, bool *unsafe, ParserInfo I) {
    ENTER(match);
    // IFDEBUG(ppTpmcMatrix(matrix));
    // IFDEBUG(ppTpmcStateArray(finalStates));
    if (matrix->height == 0) {
        cant_happen("zero-height matrix passed to match");
    }
    TpmcState *res = NULL;
    if (topRowOnlyVariables(matrix)) {
        res = finalStates->entries[0];
    } else {
        res = mixture(matrix, finalStates, errorState, knownStates, stateTable, unsafe, I);
    }
    IFDEBUG(ppTpmcState(res));
    LEAVE(match);
    return res;
}
