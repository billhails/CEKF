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
 * Term Pattern Matching Compiler types
 *
 * generated from src/tpmc.yaml by makeAST.py
 */

#include "tpmc.h"
#include <stdio.h>
#include <strings.h>
#include "common.h"

struct TpmcMatchRules * newTpmcMatchRules(struct TpmcMatchRuleArray * rules, struct TpmcVariableArray * rootVariables) {
    struct TpmcMatchRules * x = NEW(TpmcMatchRules, OBJTYPE_TPMCMATCHRULES);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcMatchRules %p\n", x);
#endif
    x->rules = rules;
    x->rootVariables = rootVariables;
    return x;
}

struct TpmcMatchRule * newTpmcMatchRule(struct TpmcState * action, struct TpmcPatternArray * patterns) {
    struct TpmcMatchRule * x = NEW(TpmcMatchRule, OBJTYPE_TPMCMATCHRULE);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcMatchRule %p\n", x);
#endif
    x->action = action;
    x->patterns = patterns;
    return x;
}

struct TpmcComparisonPattern * newTpmcComparisonPattern(struct TpmcPattern * previous, struct TpmcPattern * current) {
    struct TpmcComparisonPattern * x = NEW(TpmcComparisonPattern, OBJTYPE_TPMCCOMPARISONPATTERN);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcComparisonPattern %p\n", x);
#endif
    x->previous = previous;
    x->current = current;
    return x;
}

struct TpmcAssignmentPattern * newTpmcAssignmentPattern(HashSymbol * name, struct TpmcPattern * value) {
    struct TpmcAssignmentPattern * x = NEW(TpmcAssignmentPattern, OBJTYPE_TPMCASSIGNMENTPATTERN);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcAssignmentPattern %p\n", x);
#endif
    x->name = name;
    x->value = value;
    return x;
}

struct TpmcConstructorPattern * newTpmcConstructorPattern(HashSymbol * tag, LamTypeConstructorInfo * info, struct TpmcPatternArray * components) {
    struct TpmcConstructorPattern * x = NEW(TpmcConstructorPattern, OBJTYPE_TPMCCONSTRUCTORPATTERN);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcConstructorPattern %p\n", x);
#endif
    x->tag = tag;
    x->info = info;
    x->components = components;
    return x;
}

struct TpmcPattern * newTpmcPattern(struct TpmcPatternValue * pattern) {
    struct TpmcPattern * x = NEW(TpmcPattern, OBJTYPE_TPMCPATTERN);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcPattern %p\n", x);
#endif
    x->pattern = pattern;
    x->path = NULL;
    return x;
}

struct TpmcTestState * newTpmcTestState(HashSymbol * path, struct TpmcArcArray * arcs) {
    struct TpmcTestState * x = NEW(TpmcTestState, OBJTYPE_TPMCTESTSTATE);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcTestState %p\n", x);
#endif
    x->path = path;
    x->arcs = arcs;
    return x;
}

struct TpmcFinalState * newTpmcFinalState(LamExp * action) {
    struct TpmcFinalState * x = NEW(TpmcFinalState, OBJTYPE_TPMCFINALSTATE);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcFinalState %p\n", x);
#endif
    x->action = action;
    return x;
}

struct TpmcState * newTpmcState(int stamp, struct TpmcStateValue * state) {
    struct TpmcState * x = NEW(TpmcState, OBJTYPE_TPMCSTATE);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcState %p\n", x);
#endif
    x->stamp = stamp;
    x->state = state;
    x->refcount = 0;
    x->freeVariables = NULL;
    return x;
}

struct TpmcArc * newTpmcArc(struct TpmcState * state, struct TpmcPattern * test, HashTable * freeVariables) {
    struct TpmcArc * x = NEW(TpmcArc, OBJTYPE_TPMCARC);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcArc %p\n", x);
#endif
    x->state = state;
    x->test = test;
    x->freeVariables = freeVariables;
    return x;
}

struct TpmcArcList * newTpmcArcList(struct TpmcArc * arc, struct TpmcArcList * next) {
    struct TpmcArcList * x = NEW(TpmcArcList, OBJTYPE_TPMCARCLIST);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcArcList %p\n", x);
#endif
    x->arc = arc;
    x->next = next;
    return x;
}

struct TpmcIntList * newTpmcIntList(int integer, struct TpmcIntList * next) {
    struct TpmcIntList * x = NEW(TpmcIntList, OBJTYPE_TPMCINTLIST);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcIntList %p\n", x);
#endif
    x->integer = integer;
    x->next = next;
    return x;
}

struct TpmcPatternValue * newTpmcPatternValue(enum TpmcPatternValueType  type, union TpmcPatternValueVal  val) {
    struct TpmcPatternValue * x = NEW(TpmcPatternValue, OBJTYPE_TPMCPATTERNVALUE);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcPatternValue %p\n", x);
#endif
    x->type = type;
    x->val = val;
    return x;
}

struct TpmcStateValue * newTpmcStateValue(enum TpmcStateValueType  type, union TpmcStateValueVal  val) {
    struct TpmcStateValue * x = NEW(TpmcStateValue, OBJTYPE_TPMCSTATEVALUE);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcStateValue %p\n", x);
#endif
    x->type = type;
    x->val = val;
    return x;
}

struct TpmcMatchRuleArray * newTpmcMatchRuleArray(void) {
    struct TpmcMatchRuleArray * x = NEW(TpmcMatchRuleArray, OBJTYPE_TPMCMATCHRULEARRAY);
    int save = PROTECT(x);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcMatchRuleArray %p\n", x);
#endif
    x->entries = NULL;
    x->size = 0;
    x->capacity = 0;
    x->entries = NEW_ARRAY(struct TpmcMatchRule *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcVariableArray * newTpmcVariableArray(void) {
    struct TpmcVariableArray * x = NEW(TpmcVariableArray, OBJTYPE_TPMCVARIABLEARRAY);
    int save = PROTECT(x);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcVariableArray %p\n", x);
#endif
    x->entries = NULL;
    x->size = 0;
    x->capacity = 0;
    x->entries = NEW_ARRAY(HashSymbol *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcPatternArray * newTpmcPatternArray(char * _tag) {
    struct TpmcPatternArray * x = NEW(TpmcPatternArray, OBJTYPE_TPMCPATTERNARRAY);
    int save = PROTECT(x);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcPatternArray %p\n", x);
#endif
    x->entries = NULL;
    x->_tag = _tag;
    x->size = 0;
    x->capacity = 0;
    x->entries = NEW_ARRAY(struct TpmcPattern *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcStateArray * newTpmcStateArray(char * _tag) {
    struct TpmcStateArray * x = NEW(TpmcStateArray, OBJTYPE_TPMCSTATEARRAY);
    int save = PROTECT(x);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcStateArray %p\n", x);
#endif
    x->entries = NULL;
    x->_tag = _tag;
    x->size = 0;
    x->capacity = 0;
    x->entries = NEW_ARRAY(struct TpmcState *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcArcArray * newTpmcArcArray(void) {
    struct TpmcArcArray * x = NEW(TpmcArcArray, OBJTYPE_TPMCARCARRAY);
    int save = PROTECT(x);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcArcArray %p\n", x);
#endif
    x->entries = NULL;
    x->size = 0;
    x->capacity = 0;
    x->entries = NEW_ARRAY(struct TpmcArc *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcIntArray * newTpmcIntArray(void) {
    struct TpmcIntArray * x = NEW(TpmcIntArray, OBJTYPE_TPMCINTARRAY);
    int save = PROTECT(x);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcIntArray %p\n", x);
#endif
    x->entries = NULL;
    x->size = 0;
    x->capacity = 0;
    x->entries = NEW_ARRAY(int, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcMatrix * newTpmcMatrix(int width, int height) {
    struct TpmcMatrix * x = NEW(TpmcMatrix, OBJTYPE_TPMCMATRIX);
    int save = PROTECT(x);
#ifdef DEBUG_ALLOC
    eprintf("new TpmcMatrix %p\n", x);
#endif
    x->entries = NULL;
    x->width = 0;
    x->height = 0;
    if (width * height > 0) {
        x->entries = NEW_ARRAY(struct TpmcPattern *, width * height);
        bzero(x->entries, sizeof(struct TpmcPattern *) * width * height);
    }
    x->width = width;
    x->height = height;
    UNPROTECT(save);
    return x;
}


void pushTpmcMatchRuleArray(struct TpmcMatchRuleArray * x, struct TpmcMatchRule * entry) {
    if (x->size == x->capacity) {
        x->entries = GROW_ARRAY(struct TpmcMatchRule *, x->entries, x->capacity, x->capacity *2);
        x->capacity *= 2;
    }
    x->entries[x->size++] = entry;
}

void pushTpmcVariableArray(struct TpmcVariableArray * x, HashSymbol * entry) {
    if (x->size == x->capacity) {
        x->entries = GROW_ARRAY(HashSymbol *, x->entries, x->capacity, x->capacity *2);
        x->capacity *= 2;
    }
    x->entries[x->size++] = entry;
}

void pushTpmcPatternArray(struct TpmcPatternArray * x, struct TpmcPattern * entry) {
    if (x->size == x->capacity) {
        x->entries = GROW_ARRAY(struct TpmcPattern *, x->entries, x->capacity, x->capacity *2);
        x->capacity *= 2;
    }
    x->entries[x->size++] = entry;
}

void pushTpmcStateArray(struct TpmcStateArray * x, struct TpmcState * entry) {
    if (x->size == x->capacity) {
        x->entries = GROW_ARRAY(struct TpmcState *, x->entries, x->capacity, x->capacity *2);
        x->capacity *= 2;
    }
    x->entries[x->size++] = entry;
}

void pushTpmcArcArray(struct TpmcArcArray * x, struct TpmcArc * entry) {
    if (x->size == x->capacity) {
        x->entries = GROW_ARRAY(struct TpmcArc *, x->entries, x->capacity, x->capacity *2);
        x->capacity *= 2;
    }
    x->entries[x->size++] = entry;
}

void pushTpmcIntArray(struct TpmcIntArray * x, int entry) {
    if (x->size == x->capacity) {
        x->entries = GROW_ARRAY(int, x->entries, x->capacity, x->capacity *2);
        x->capacity *= 2;
    }
    x->entries[x->size++] = entry;
}


/************************************/

void markTpmcMatchRules(struct TpmcMatchRules * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcMatchRuleArray(x->rules);
    markTpmcVariableArray(x->rootVariables);
}

void markTpmcMatchRule(struct TpmcMatchRule * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcState(x->action);
    markTpmcPatternArray(x->patterns);
}

void markTpmcComparisonPattern(struct TpmcComparisonPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcPattern(x->previous);
    markTpmcPattern(x->current);
}

void markTpmcAssignmentPattern(struct TpmcAssignmentPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->name);
    markTpmcPattern(x->value);
}

void markTpmcConstructorPattern(struct TpmcConstructorPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->tag);
    markLamTypeConstructorInfo(x->info);
    markTpmcPatternArray(x->components);
}

void markTpmcPattern(struct TpmcPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
    markTpmcPatternValue(x->pattern);
}

void markTpmcTestState(struct TpmcTestState * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
    markTpmcArcArray(x->arcs);
}

void markTpmcFinalState(struct TpmcFinalState * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->action);
}

void markTpmcState(struct TpmcState * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashTable(x->freeVariables);
    markTpmcStateValue(x->state);
}

void markTpmcArc(struct TpmcArc * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcState(x->state);
    markTpmcPattern(x->test);
    markHashTable(x->freeVariables);
}

void markTpmcArcList(struct TpmcArcList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcArc(x->arc);
    markTpmcArcList(x->next);
}

void markTpmcIntList(struct TpmcIntList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcIntList(x->next);
}

void markTpmcPatternValue(struct TpmcPatternValue * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            markHashSymbol(x->val.var);
            break;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            markTpmcComparisonPattern(x->val.comparison);
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            markTpmcAssignmentPattern(x->val.assignment);
            break;
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            break;
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            markBigInt(x->val.biginteger);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            markTpmcConstructorPattern(x->val.constructor);
            break;
        default:
            cant_happen("unrecognised type %d in markTpmcPatternValue", x->type);
    }
}

void markTpmcStateValue(struct TpmcStateValue * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            markTpmcTestState(x->val.test);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            markTpmcFinalState(x->val.final);
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            break;
        default:
            cant_happen("unrecognised type %d in markTpmcStateValue", x->type);
    }
}

void markTpmcMatchRuleArray(struct TpmcMatchRuleArray * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
        markTpmcMatchRule(x->entries[i]);
    }
}

void markTpmcVariableArray(struct TpmcVariableArray * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
        markHashSymbol(x->entries[i]);
    }
}

void markTpmcPatternArray(struct TpmcPatternArray * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
        markTpmcPattern(x->entries[i]);
    }
}

void markTpmcStateArray(struct TpmcStateArray * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
        markTpmcState(x->entries[i]);
    }
}

void markTpmcArcArray(struct TpmcArcArray * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
        markTpmcArc(x->entries[i]);
    }
}

void markTpmcIntArray(struct TpmcIntArray * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
    }
}

void markTpmcMatrix(struct TpmcMatrix * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    int size = x->width * x->height;
    for (int i = 0; i < size; i++) {
        markTpmcPattern(x->entries[i]);
    }
}


void markTpmcObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_TPMCMATCHRULES:
            markTpmcMatchRules((TpmcMatchRules *)h);
            break;
        case OBJTYPE_TPMCMATCHRULE:
            markTpmcMatchRule((TpmcMatchRule *)h);
            break;
        case OBJTYPE_TPMCCOMPARISONPATTERN:
            markTpmcComparisonPattern((TpmcComparisonPattern *)h);
            break;
        case OBJTYPE_TPMCASSIGNMENTPATTERN:
            markTpmcAssignmentPattern((TpmcAssignmentPattern *)h);
            break;
        case OBJTYPE_TPMCCONSTRUCTORPATTERN:
            markTpmcConstructorPattern((TpmcConstructorPattern *)h);
            break;
        case OBJTYPE_TPMCPATTERN:
            markTpmcPattern((TpmcPattern *)h);
            break;
        case OBJTYPE_TPMCTESTSTATE:
            markTpmcTestState((TpmcTestState *)h);
            break;
        case OBJTYPE_TPMCFINALSTATE:
            markTpmcFinalState((TpmcFinalState *)h);
            break;
        case OBJTYPE_TPMCSTATE:
            markTpmcState((TpmcState *)h);
            break;
        case OBJTYPE_TPMCARC:
            markTpmcArc((TpmcArc *)h);
            break;
        case OBJTYPE_TPMCARCLIST:
            markTpmcArcList((TpmcArcList *)h);
            break;
        case OBJTYPE_TPMCINTLIST:
            markTpmcIntList((TpmcIntList *)h);
            break;
        case OBJTYPE_TPMCPATTERNVALUE:
            markTpmcPatternValue((TpmcPatternValue *)h);
            break;
        case OBJTYPE_TPMCSTATEVALUE:
            markTpmcStateValue((TpmcStateValue *)h);
            break;
        case OBJTYPE_TPMCMATCHRULEARRAY:
            markTpmcMatchRuleArray((TpmcMatchRuleArray *)h);
            break;
        case OBJTYPE_TPMCVARIABLEARRAY:
            markTpmcVariableArray((TpmcVariableArray *)h);
            break;
        case OBJTYPE_TPMCPATTERNARRAY:
            markTpmcPatternArray((TpmcPatternArray *)h);
            break;
        case OBJTYPE_TPMCSTATEARRAY:
            markTpmcStateArray((TpmcStateArray *)h);
            break;
        case OBJTYPE_TPMCARCARRAY:
            markTpmcArcArray((TpmcArcArray *)h);
            break;
        case OBJTYPE_TPMCINTARRAY:
            markTpmcIntArray((TpmcIntArray *)h);
            break;
        case OBJTYPE_TPMCMATRIX:
            markTpmcMatrix((TpmcMatrix *)h);
            break;
        default:
            cant_happen("unrecognised type %d in markTpmcObj\n", h->type);
    }
}

/************************************/

void freeTpmcMatchRules(struct TpmcMatchRules * x) {
    FREE(x, TpmcMatchRules);
}

void freeTpmcMatchRule(struct TpmcMatchRule * x) {
    FREE(x, TpmcMatchRule);
}

void freeTpmcComparisonPattern(struct TpmcComparisonPattern * x) {
    FREE(x, TpmcComparisonPattern);
}

void freeTpmcAssignmentPattern(struct TpmcAssignmentPattern * x) {
    FREE(x, TpmcAssignmentPattern);
}

void freeTpmcConstructorPattern(struct TpmcConstructorPattern * x) {
    FREE(x, TpmcConstructorPattern);
}

void freeTpmcPattern(struct TpmcPattern * x) {
    FREE(x, TpmcPattern);
}

void freeTpmcTestState(struct TpmcTestState * x) {
    FREE(x, TpmcTestState);
}

void freeTpmcFinalState(struct TpmcFinalState * x) {
    FREE(x, TpmcFinalState);
}

void freeTpmcState(struct TpmcState * x) {
    FREE(x, TpmcState);
}

void freeTpmcArc(struct TpmcArc * x) {
    FREE(x, TpmcArc);
}

void freeTpmcArcList(struct TpmcArcList * x) {
    FREE(x, TpmcArcList);
}

void freeTpmcIntList(struct TpmcIntList * x) {
    FREE(x, TpmcIntList);
}

void freeTpmcPatternValue(struct TpmcPatternValue * x) {
    FREE(x, TpmcPatternValue);
}

void freeTpmcStateValue(struct TpmcStateValue * x) {
    FREE(x, TpmcStateValue);
}

void freeTpmcMatchRuleArray(struct TpmcMatchRuleArray * x) {
    FREE_ARRAY(struct TpmcMatchRule *, x->entries, x->capacity);
    FREE(x, TpmcMatchRuleArray);
}

void freeTpmcVariableArray(struct TpmcVariableArray * x) {
    FREE_ARRAY(HashSymbol *, x->entries, x->capacity);
    FREE(x, TpmcVariableArray);
}

void freeTpmcPatternArray(struct TpmcPatternArray * x) {
    FREE_ARRAY(struct TpmcPattern *, x->entries, x->capacity);
    FREE(x, TpmcPatternArray);
}

void freeTpmcStateArray(struct TpmcStateArray * x) {
    FREE_ARRAY(struct TpmcState *, x->entries, x->capacity);
    FREE(x, TpmcStateArray);
}

void freeTpmcArcArray(struct TpmcArcArray * x) {
    FREE_ARRAY(struct TpmcArc *, x->entries, x->capacity);
    FREE(x, TpmcArcArray);
}

void freeTpmcIntArray(struct TpmcIntArray * x) {
    FREE_ARRAY(int, x->entries, x->capacity);
    FREE(x, TpmcIntArray);
}

void freeTpmcMatrix(struct TpmcMatrix * x) {
    FREE_ARRAY(struct TpmcPattern *, x->entries, x->width * x->height);
    FREE(x, TpmcMatrix);
}


void freeTpmcObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_TPMCMATCHRULES:
            freeTpmcMatchRules((TpmcMatchRules *)h);
            break;
        case OBJTYPE_TPMCMATCHRULE:
            freeTpmcMatchRule((TpmcMatchRule *)h);
            break;
        case OBJTYPE_TPMCCOMPARISONPATTERN:
            freeTpmcComparisonPattern((TpmcComparisonPattern *)h);
            break;
        case OBJTYPE_TPMCASSIGNMENTPATTERN:
            freeTpmcAssignmentPattern((TpmcAssignmentPattern *)h);
            break;
        case OBJTYPE_TPMCCONSTRUCTORPATTERN:
            freeTpmcConstructorPattern((TpmcConstructorPattern *)h);
            break;
        case OBJTYPE_TPMCPATTERN:
            freeTpmcPattern((TpmcPattern *)h);
            break;
        case OBJTYPE_TPMCTESTSTATE:
            freeTpmcTestState((TpmcTestState *)h);
            break;
        case OBJTYPE_TPMCFINALSTATE:
            freeTpmcFinalState((TpmcFinalState *)h);
            break;
        case OBJTYPE_TPMCSTATE:
            freeTpmcState((TpmcState *)h);
            break;
        case OBJTYPE_TPMCARC:
            freeTpmcArc((TpmcArc *)h);
            break;
        case OBJTYPE_TPMCARCLIST:
            freeTpmcArcList((TpmcArcList *)h);
            break;
        case OBJTYPE_TPMCINTLIST:
            freeTpmcIntList((TpmcIntList *)h);
            break;
        case OBJTYPE_TPMCPATTERNVALUE:
            freeTpmcPatternValue((TpmcPatternValue *)h);
            break;
        case OBJTYPE_TPMCSTATEVALUE:
            freeTpmcStateValue((TpmcStateValue *)h);
            break;
        case OBJTYPE_TPMCMATCHRULEARRAY:
            freeTpmcMatchRuleArray((TpmcMatchRuleArray *)h);
            break;
        case OBJTYPE_TPMCVARIABLEARRAY:
            freeTpmcVariableArray((TpmcVariableArray *)h);
            break;
        case OBJTYPE_TPMCPATTERNARRAY:
            freeTpmcPatternArray((TpmcPatternArray *)h);
            break;
        case OBJTYPE_TPMCSTATEARRAY:
            freeTpmcStateArray((TpmcStateArray *)h);
            break;
        case OBJTYPE_TPMCARCARRAY:
            freeTpmcArcArray((TpmcArcArray *)h);
            break;
        case OBJTYPE_TPMCINTARRAY:
            freeTpmcIntArray((TpmcIntArray *)h);
            break;
        case OBJTYPE_TPMCMATRIX:
            freeTpmcMatrix((TpmcMatrix *)h);
            break;
        default:
            cant_happen("unrecognised type %d in freeTpmcObj\n", h->type);
    }
}

char *typenameTpmcObj(int type) {
    switch(type) {
        case OBJTYPE_TPMCMATCHRULES:
            return "TpmcMatchRules";
        case OBJTYPE_TPMCMATCHRULE:
            return "TpmcMatchRule";
        case OBJTYPE_TPMCCOMPARISONPATTERN:
            return "TpmcComparisonPattern";
        case OBJTYPE_TPMCASSIGNMENTPATTERN:
            return "TpmcAssignmentPattern";
        case OBJTYPE_TPMCCONSTRUCTORPATTERN:
            return "TpmcConstructorPattern";
        case OBJTYPE_TPMCPATTERN:
            return "TpmcPattern";
        case OBJTYPE_TPMCTESTSTATE:
            return "TpmcTestState";
        case OBJTYPE_TPMCFINALSTATE:
            return "TpmcFinalState";
        case OBJTYPE_TPMCSTATE:
            return "TpmcState";
        case OBJTYPE_TPMCARC:
            return "TpmcArc";
        case OBJTYPE_TPMCARCLIST:
            return "TpmcArcList";
        case OBJTYPE_TPMCINTLIST:
            return "TpmcIntList";
        case OBJTYPE_TPMCPATTERNVALUE:
            return "TpmcPatternValue";
        case OBJTYPE_TPMCSTATEVALUE:
            return "TpmcStateValue";
        case OBJTYPE_TPMCMATCHRULEARRAY:
            return "TpmcMatchRuleArray";
        case OBJTYPE_TPMCVARIABLEARRAY:
            return "TpmcVariableArray";
        case OBJTYPE_TPMCPATTERNARRAY:
            return "TpmcPatternArray";
        case OBJTYPE_TPMCSTATEARRAY:
            return "TpmcStateArray";
        case OBJTYPE_TPMCARCARRAY:
            return "TpmcArcArray";
        case OBJTYPE_TPMCINTARRAY:
            return "TpmcIntArray";
        case OBJTYPE_TPMCMATRIX:
            return "TpmcMatrix";
        default:
            cant_happen("unrecognised type %d in typenameTpmcObj\n", type);
    }
}

