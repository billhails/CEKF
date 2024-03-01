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
#include "lambda_debug.h"
#include "lambda_helper.h"
#include "symbol.h"

#ifdef DEBUG_TPMC_MATCH
#    include "debugging_on.h"
#else
#    include "debugging_off.h"
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

static bool patternIsWildcard(TpmcMatrix *m, int x, int y) {
    TpmcPatternValueType type = getTpmcMatrixIndex(m, x, y)->pattern->type;
    DEBUG("patternIsWildcard x = %d, y = %d, type = %d", x, y, type);
    return type == TPMCPATTERNVALUE_TYPE_WILDCARD;
}

static bool noRemainingTests(TpmcMatrix *matrix) {
    DEBUG("noRemainingTests %d", matrix->width);
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
    ENTER(makeEmptyTestState);
    TpmcArcArray *arcs = newTpmcArcArray();
    int save = PROTECT(arcs);
    TpmcTestState *test = newTpmcTestState(path, arcs);
    PROTECT(test);
    TpmcStateValue *val = newTpmcStateValue(TPMCSTATEVALUE_TYPE_TEST,
                                            TPMCSTATEVALUE_VAL_TEST(test));
    PROTECT(val);
    TpmcState *state = tpmcMakeState(val);
#ifdef DEBUG_TPMC_MATCH2
    eprintf("makeEmptyTestState returning: ");
    printTpmcState(state, 0);
    eprintf("\n");
#endif
    UNPROTECT(save);
    LEAVE(makeEmptyTestState);
    return state;
}

static bool patternMatches(TpmcPattern *constructor, TpmcPattern *pattern) {
    ENTER(patternMatches);
#ifdef DEBUG_TPMC_MATCH2
    eprintf("patternMatches constructor: ");
    printTpmcPattern(constructor, 0);
    eprintf("\npatternMatches pattern: ");
    printTpmcPattern(pattern, 0);
    eprintf("\n");
#endif
    bool isComparison =
        (constructor->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON);
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            cant_happen("patternMatches ennncountered var");
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            LEAVE(patternMatches);
            return true;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            cant_happen("patternMatches encountered assignment");
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            LEAVE(patternMatches);
            return true;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:{
                bool res = isComparison
                    || (constructor->pattern->type ==
                        TPMCPATTERNVALUE_TYPE_CHARACTER
                        && constructor->pattern->val.character ==
                        pattern->pattern->val.character);
                LEAVE(patternMatches);
                return res;
            }
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:{
                bool res = isComparison
                    || (constructor->pattern->type ==
                        TPMCPATTERNVALUE_TYPE_BIGINTEGER
                        && cmpBigInt(constructor->pattern->val.biginteger,
                                     pattern->pattern->val.biginteger) == 0);
                LEAVE(patternMatches);
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
                LEAVE(patternMatches);
                return res;
            }
        default:
            cant_happen("unrecognized pattern type %d in patternMatches",
                        pattern->pattern->type);
    }
}

static bool patternIndexMatches(TpmcMatrix *matrix, int x, int y, int yy) {
    if (y == yy) {
#ifdef DEBUG_TPMC_MATCH2
        eprintf("patternIndexMatches is true trivially\n");
#endif
        return true;
    }
    TpmcPattern *constructor = getTpmcMatrixIndex(matrix, x, y);
    TpmcPattern *pattern = getTpmcMatrixIndex(matrix, x, yy);
    return patternMatches(constructor, pattern);
}

TpmcIntArray *findPatternsMatching(TpmcMatrix *matrix, int x, int y) {
    ENTER(findPatternsMatching);
    TpmcIntArray *res = newTpmcIntArray();
    int save = PROTECT(res);
    for (int yy = 0; yy < matrix->height; yy++) {
        if (patternIndexMatches(matrix, x, y, yy)) {
            pushTpmcIntArray(res, yy);
        }
    }
#ifdef DEBUG_TPMC_MATCH2
    eprintf("findPatternsMatching %d %d returning: ", x, y);
    printTpmcIntArray(res, 0);
    eprintf("\n");
#endif
    UNPROTECT(save);
    LEAVE(findPatternsMatching);
    return res;
}

static TpmcPatternArray *extractMatrixColumnSubset(TpmcMatrix *matrix, int x,
                                                   TpmcIntArray *ys) {
    ENTER(extractMatrixColumnSubset);
    TpmcPatternArray *res = newTpmcPatternArray("extractMatrixColumnSubset");
    int save = PROTECT(res);
    for (int i = 0; i < ys->size; ++i) {
        int y = ys->entries[i];
        pushTpmcPatternArray(res, getTpmcMatrixIndex(matrix, x, y));
    }
#ifdef DEBUG_TPMC_MATCH2
    eprintf("extractMatrixColumnSubset returning: ");
    printTpmcPatternArray(res, 0);
    eprintf("\n");
#endif
    UNPROTECT(save);
    LEAVE(extractMatrixColumnSubset);
    return res;
}

static TpmcStateArray *extractStateArraySubset(TpmcStateArray *all,
                                               TpmcIntArray *indices) {
    ENTER(extractStateArraySubset);
#ifdef DEBUG_TPMC_MATCH2
    eprintf("extractStateArraySubset all: ");
    printTpmcStateArray(all, 0);
    eprintf("\nextractStateArraySubset indices: ");
    printTpmcIntArray(indices, 0);
    eprintf("\n");
#endif
    TpmcStateArray *res = newTpmcStateArray("extractStateArraySubset");
    int save = PROTECT(res);
    for (int i = 0; i < indices->size; ++i) {
        int j = indices->entries[i];
        pushTpmcStateArray(res, all->entries[j]);
    }
    UNPROTECT(save);
    LEAVE(extractStateArraySubset);
    return res;
}

static int determineArity(TpmcPattern *pattern) {
    if (pattern->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
        LamTypeConstructorInfo *info =
            pattern->pattern->val.constructor->info;
        DEBUG("'%s' has arity %d",
              pattern->pattern->val.constructor->tag->name, info->arity);
        return info->arity;
    } else {
        return 0;
    }
}

static void populateSubPatternMatrixRowWithWildcards(TpmcMatrix *matrix,
                                                     int y, int arity,
                                                     TpmcPattern *pattern) {
    ENTER(populateSubPatternMatrixRowWithWildcards);
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
    LEAVE(populateSubPatternMatrixRowWithWildcards);
}

static void populateSubPatternMatrixRowWithComponents(TpmcMatrix *matrix,
                                                      int y, int arity,
                                                      TpmcPattern *pattern) {
    ENTER(populateSubPatternMatrixRowWithComponents);
    if (arity != pattern->pattern->val.constructor->components->size) {
        cant_happen
            ("arity %d does not match constructor arity %d in populateSubPatternMatrixRowWithComponents",
             arity, pattern->pattern->val.constructor->components->size);
    }
    for (int i = 0; i < arity; i++) {
        setTpmcMatrixIndex(matrix, i, y,
                           pattern->pattern->val.constructor->
                           components->entries[i]);
    }
    LEAVE(populateSubPatternMatrixRowWithComponents);
}

static void populateSubPatternMatrix(TpmcMatrix *matrix,
                                     TpmcPatternArray *patterns, int arity) {
    ENTER(populateSubPatternMatrix);
    if (arity == 0) {
        LEAVE(populateSubPatternMatrix);
        return;
    }
    for (int i = 0; i < patterns->size; ++i) {
        TpmcPattern *pattern = patterns->entries[i];
        switch (pattern->pattern->type) {
            case TPMCPATTERNVALUE_TYPE_VAR:
                cant_happen
                    ("encountered pattern type var during populateSubPatternMatrix");
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
                    ("encountered pattern type assignment during populateSubPatternMatrix");
            case TPMCPATTERNVALUE_TYPE_CHARACTER:
                cant_happen
                    ("encountered pattern type char during populateSubPatternMatrix");
            case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
                cant_happen
                    ("encountered pattern type int during populateSubPatternMatrix");
            default:
                cant_happen
                    ("unrecognised pattern type %d during populateSubPatternMatrix",
                     pattern->pattern->type);
        }
    }
    LEAVE(populateSubPatternMatrix);
}

static void copyMatrixExceptColAndOnlyRows(int col, TpmcIntArray *ys,
                                           TpmcMatrix *from, TpmcMatrix *to) {
    ENTER(copyMatrixExceptColAndOnlyRows);
    DEBUG("copyMatrixExceptColAndOnlyRows col : %d", col);
#ifdef DEBUG_TPMC_MATCH2
    eprintf("copyMatrixExceptColAndOnlyRows rows: ");
    printTpmcIntArray(ys, 0);
    eprintf("\n");
    DEBUG("copyMatrixExceptColAndOnlyRows from: %d * %d to: %d * %d",
          from->width, from->height, to->width, to->height);
#endif
    int tx = 0;
    for (int x = 0; x < from->width; x++) {
        if (x != col) {
            for (int iy = 0; iy < ys->size; ++iy) {
                int y = ys->entries[iy];
                DEBUG("copyMatrixExceptCol(%d), to[%d][%d] <= from[%d][%d]",
                      col, tx, iy, x, y);
                setTpmcMatrixIndex(to, tx, iy,
                                   getTpmcMatrixIndex(from, x, y));
            }
            tx++;
        }
    }
    LEAVE(copyMatrixExceptColAndOnlyRows);
}

static void copyMatrixWithOffset(int offset, TpmcMatrix *from, TpmcMatrix *to) {
    ENTER(copyMatrixWithOffset);
    for (int x = 0; x < from->width; x++) {
        for (int y = 0; y < from->height; ++y) {
            DEBUG("copyMatrixWithOffset(%d), to[%d][%d] <= from[%d][%d]",
                  offset, x + offset, y, x, y);
            setTpmcMatrixIndex(to, x + offset, y,
                               getTpmcMatrixIndex(from, x, y));
        }
    }
    LEAVE(copyMatrixWithOffset);
}

static TpmcPattern *replaceComponentsWithWildcards(TpmcPattern *pattern) {
    ENTER(replaceComponentsWithWildcards);
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
            LEAVE(replaceComponentsWithWildcards);
            return replacement;
        }
    }
    LEAVE(replaceComponentsWithWildcards);
    return pattern;
}

static TpmcIntArray *makeTpmcIntArray(int size, int initialValue) {
    ENTER(makeTpmcIntArray);
    TpmcIntArray *res = newTpmcIntArray();
    int save = PROTECT(res);
    for (int i = 0; i < size; ++i) {
        pushTpmcIntArray(res, initialValue);
    }
    UNPROTECT(save);
    LEAVE(makeTpmcIntArray);
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
    ENTER(makeNamedWildcardPattern);
    TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD,
                                               TPMCPATTERNVALUE_VAL_WILDCARD
                                               ());
    int save = PROTECT(wc);
    TpmcPattern *pattern = newTpmcPattern(wc);
    pattern->path = path;
    UNPROTECT(save);
    LEAVE(makeNamedWildcardPattern);
    return pattern;
}

static TpmcState *deduplicateState(TpmcState *state,
                                   TpmcStateArray *knownStates) {
    for (int i = 0; i < knownStates->size; i++) {
        if (tpmcStateEq(state, knownStates->entries[i])) {
            DEBUG("deduplicateState found dup");
            validateLastAlloc();
            return knownStates->entries[i];
        }
    }
    DEBUG("deduplicateState adding new");
    pushTpmcStateArray(knownStates, state);
    DEBUG("deduplicateState added new");
    return state;
}

static void collectPathsBoundByConstructor(TpmcPatternArray *components,
                                           TpmcVariableTable *boundVariables) 
{
    ENTER(collectPathsBoundByConstructor);
    for (int i = 0; i < components->size; ++i) {
        TpmcPattern *pattern = components->entries[i];
        setTpmcVariableTable(boundVariables, pattern->path);
    }
    LEAVE(collectPathsBoundByConstructor);
}

static void collectPathsBoundByPattern(TpmcPattern *pattern,
                                       TpmcVariableTable *boundVariables) {
    ENTER(collecPathsBoundByPattern);
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
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            collectPathsBoundByConstructor(pattern->pattern->val.
                                           constructor->components,
                                           boundVariables);
            break;
        default:
            cant_happen("unrecognised type %d in collectPathsBoundByPattern",
                        pattern->pattern->type);
    }
    LEAVE("collecPathsBoundByPattern");
}

static TpmcVariableTable *variablesBoundByPattern(TpmcPattern *pattern) {
    ENTER(variablesBoundByPattern);
    TpmcVariableTable *boundVariables = newTpmcVariableTable();
    int save = PROTECT(boundVariables);
    collectPathsBoundByPattern(pattern, boundVariables);
    UNPROTECT(save);
    LEAVE(variablesBoundByPattern);
    return boundVariables;
}

static TpmcVariableTable *getTestStatesFreeVariables(TpmcTestState *testState) {
    // The free variables of a test state is the union of the free variables of the outgoing arcs, plus the test variable.
    ENTER(getTestStatesFreeVariables);
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
    LEAVE(getTestStatesFreeVariables);
    return freeVariables;
}

static TpmcVariableTable *getStatesFreeVariables(TpmcState *state) {
    ENTER(getStatesFreeVariables);
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
    LEAVE(getStatesFreeVariables);
    return state->freeVariables;
}

static TpmcArc *makeTpmcArc(TpmcState *state, TpmcPattern *pattern) {
    ENTER(makeTpmcArc);
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
            DEBUG("makeTpmcArc adding free variable %s", key->name);
            setTpmcVariableTable(arc->freeVariables, key);
        }
    }
    state->refcount++;
    DEBUG("makeTpmcArc creating arc to state with refcount %d",
          state->refcount);
    IFDEBUG(printTpmcState(state, 0));
    UNPROTECT(save);
    LEAVE(makeTpmcArc);
    return arc;
}

#ifdef DEBUG_TPMC_MATCH
void ppPattern(TpmcPattern *pattern) {
    eprintf("%s == ", pattern->path->name);
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_COMPARISON:{
                TpmcComparisonPattern *c = pattern->pattern->val.comparison;
                eprintf("(%s == %s)", c->previous->path->name,
                        c->current->path->name);
                break;
            }
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            eprintf("_");
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            eprintf("'%c'", pattern->pattern->val.character);
            break;
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            fprintBigInt(stderr, pattern->pattern->val.biginteger);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:{
                TpmcConstructorPattern *c = pattern->pattern->val.constructor;
                eprintf("%s(", c->tag->name);
                for (int i = 0; i < c->components->size; ++i) {
                    ppPattern(c->components->entries[i]);
                    if (i + 1 < c->components->size) {
                        eprintf(", ");
                    }
                }
                eprintf(")");
                break;
            }
        default:
            cant_happen("ppPattern encountered unexpected type");
    }
}

#    define PPPATTERN(p) ppPattern(p); eprintf("\n")
#else
#    define PPPATTERN(p)
#endif

static TpmcState *mixture(TpmcMatrix *matrix, TpmcStateArray *finalStates,
                          TpmcState *errorState,
                          TpmcStateArray *knownStates) {
    ENTER(mixture);
    // there is some column whose topmost pattern is a constructor
    int x = findFirstConstructorColumn(matrix);
    // The goal is to build a test state with the variable v and some outgoing arcs (one for each constructor and possibly a default arc).
    TpmcState *state =
        makeEmptyTestState(getTpmcMatrixIndex(matrix, x, 0)->path);
    int save = PROTECT(state);
    // For each constructor c in the selected column, its arc is defined as follows:
    for (int y = 0; y < matrix->height; y++) {
        DEBUG("mixture examining[%d][%d]", x, y);
        PPPATTERN(getTpmcMatrixIndex(matrix, x, y));
        if (!patternIsWildcard(matrix, x, y)) {
            DEBUG("mixture pattern is not wildcard");
            TpmcPattern *c = getTpmcMatrixIndex(matrix, x, y);
            // Let {i1 , ... , ij} be the row-indices of the patterns in the column that match c.
            TpmcIntArray *matchingIndices =
                findPatternsMatching(matrix, x, y);
            validateLastAlloc();
            int save2 = PROTECT(matchingIndices);
            // Let {pat1 , ... , patj} be the patterns in the column corresponding to the indices computed above,
            TpmcPatternArray *matchingPatterns =
                extractMatrixColumnSubset(matrix, x, matchingIndices);
            PROTECT(matchingPatterns);
            // let n be the arity of the constructor c
            int arity = determineArity(c);
            //  ... a pattern matrix with n columns and j rows (create ahead of time)
            DEBUG("mixture - creating sub-pattern matrix %d * %d", arity,
                  matchingPatterns->size);
            TpmcMatrix *subPatternMatrix = newTpmcMatrix(arity, matchingPatterns->size);        // could be zero-width
            PROTECT(subPatternMatrix);
            // For each pati, its n sub-patterns are extracted;
            // if pati is a wildcard, n wildcards are produced instead, each tagged with the right path variable.
            populateSubPatternMatrix(subPatternMatrix, matchingPatterns,
                                     arity);
            // This matrix is then appended to the result of selecting, from each column in the rest of the
            // original matrix, those rows whose indices are in {i1 , ... , ij}. 
            TpmcMatrix *newMatrix = newTpmcMatrix(matrix->width + arity - 1,
                                                  matchingPatterns->size);
            DEBUG("mixture - created newMatrix %d * %d", newMatrix->width,
                  newMatrix->height);
            PROTECT(newMatrix);
            copyMatrixExceptColAndOnlyRows(x, matchingIndices, matrix,
                                           newMatrix);
            copyMatrixWithOffset(matrix->width - 1, subPatternMatrix,
                                 newMatrix);
            // Finally the indices are used to select the corresponding final states that go with these rows.
            TpmcStateArray *newFinalStates =
                extractStateArraySubset(finalStates, matchingIndices);
            PROTECT(newFinalStates);
            // The arc for the constructor c is now defined as (c’,state), where c’ is c with any immediate
            // sub-patterns replaced by their path variables (thus c’ is a simple pattern)
            TpmcPattern *cPrime = replaceComponentsWithWildcards(c);
            PROTECT(cPrime);
            // and state is the result of recursively applying match to the new matrix and the new sequence of final states
            TpmcState *newState =
                tpmcMatch(newMatrix, newFinalStates, errorState, knownStates);
            PROTECT(newState);
            TpmcArc *arc = makeTpmcArc(newState, cPrime);
            PROTECT(arc);
            if (tpmcArcInArray(arc, state->state->val.test->arcs)) {
                arc->state->refcount--;
                validateLastAlloc();
            } else {
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
        DEBUG("mixture - constructors are exhaustive");
        LEAVE(mixture);
        return res;
    }
    // Otherwise, a default arc (_,state) is the last arc.
    // If there are any wildcard patterns in the selected column
    TpmcIntArray *wcIndices = newTpmcIntArray();
    PROTECT(wcIndices);
    for (int y = 0; y < matrix->height; y++) {
        if (patternIsWildcard(matrix, x, y)) {
            pushTpmcIntArray(wcIndices, y);
        }
    }
    if (wcIndices->size > 0) {
        // then their rows are selected from the rest of the matrix and the final states
        TpmcMatrix *wcMatrix =
            newTpmcMatrix(matrix->width - 1, wcIndices->size);
        PROTECT(wcMatrix);
        copyMatrixExceptColAndOnlyRows(x, wcIndices, matrix, wcMatrix);
        TpmcStateArray *wcFinalStates =
            extractStateArraySubset(finalStates, wcIndices);
        PROTECT(wcFinalStates);
        // and the state is the result of applying match to the new matrix and states
        TpmcState *wcState =
            tpmcMatch(wcMatrix, wcFinalStates, errorState, knownStates);
        PROTECT(wcState);
        TpmcPattern *wcPattern =
            makeNamedWildcardPattern(getTpmcMatrixIndex(matrix, x, 0)->path);
        PROTECT(wcPattern);
        TpmcArc *wcArc = makeTpmcArc(wcState, wcPattern);
        PROTECT(wcArc);
        pushTpmcArcArray(state->state->val.test->arcs, wcArc);
        TpmcState *res = deduplicateState(state, knownStates);
        UNPROTECT(save);
        DEBUG("mixture - wildcards supply default");
        LEAVE(mixture);
        return res;
    } else {
        validateLastAlloc();
        // Otherwise, the error state is used after its reference count has been incremented
        TpmcPattern *errorPattern =
            makeNamedWildcardPattern(getTpmcMatrixIndex(matrix, x, 0)->path);
        PROTECT(errorPattern);
        TpmcArc *errorArc = makeTpmcArc(errorState, errorPattern);
        PROTECT(errorArc);
        pushTpmcArcArray(state->state->val.test->arcs, errorArc);
        TpmcState *res = deduplicateState(state, knownStates);
        UNPROTECT(save);
        DEBUG("mixture - error state supplies default");
        LEAVE(mixture);
        return res;
    }
}

TpmcState *tpmcMatch(TpmcMatrix *matrix, TpmcStateArray *finalStates,
                     TpmcState *errorState, TpmcStateArray *knownStates) {
    ENTER(tpmcMatch);
    if (matrix->height == 0) {
        cant_happen("zero-height matrix passed to match");
    }
    TpmcState *res = NULL;
#ifdef DEBUG_TPMC_MATCH2
    eprintf("tpmcMatch: matrix: ");
    printTpmcMatrix(matrix, 0);
    eprintf("\ntpmcMatch: finalStates: ");
    printTpmcStateArray(finalStates, 0);
    eprintf("\n");
#endif
    if (noRemainingTests(matrix)) {
        DEBUG("variable rule applies");
        res = finalStates->entries[0];
    } else {
        DEBUG("mixture rule applies");
        res = mixture(matrix, finalStates, errorState, knownStates);
    }
    LEAVE(tpmcMatch);
#ifdef DEBUG_TPMC_MATCH2
    eprintf("tpmcMatch returning: ");
    printTpmcState(res, 0);
    eprintf("\n");
#endif
    return res;
}
