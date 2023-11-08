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

struct TpmcMatchRules * newTpmcMatchRules(struct TpmcMatchRuleArray * rules, struct TpmcVariableArray * rootVariables) {
    struct TpmcMatchRules * x = NEW(TpmcMatchRules, OBJTYPE_TPMCMATCHRULES);
    x->rules = rules;
    x->rootVariables = rootVariables;
    return x;
}

struct TpmcMatchRule * newTpmcMatchRule(struct TpmcState * action, struct TpmcPatternArray * patterns) {
    struct TpmcMatchRule * x = NEW(TpmcMatchRule, OBJTYPE_TPMCMATCHRULE);
    x->action = action;
    x->patterns = patterns;
    return x;
}

struct TpmcVarPattern * newTpmcVarPattern(HashSymbol * name) {
    struct TpmcVarPattern * x = NEW(TpmcVarPattern, OBJTYPE_TPMCVARPATTERN);
    x->name = name;
    return x;
}

struct TpmcComparisonPattern * newTpmcComparisonPattern(struct TpmcPattern * previous, struct TpmcPattern * current) {
    struct TpmcComparisonPattern * x = NEW(TpmcComparisonPattern, OBJTYPE_TPMCCOMPARISONPATTERN);
    x->previous = previous;
    x->current = current;
    return x;
}

struct TpmcAssignmentPattern * newTpmcAssignmentPattern(HashSymbol * name, struct TpmcPattern * value) {
    struct TpmcAssignmentPattern * x = NEW(TpmcAssignmentPattern, OBJTYPE_TPMCASSIGNMENTPATTERN);
    x->name = name;
    x->value = value;
    return x;
}

struct TpmcConstructorPattern * newTpmcConstructorPattern(HashSymbol * tag, LamTypeConstructorInfo * info, struct TpmcPatternArray * components) {
    struct TpmcConstructorPattern * x = NEW(TpmcConstructorPattern, OBJTYPE_TPMCCONSTRUCTORPATTERN);
    x->tag = tag;
    x->info = info;
    x->components = components;
    return x;
}

struct TpmcPattern * newTpmcPattern(struct TpmcPatternValue * pattern) {
    struct TpmcPattern * x = NEW(TpmcPattern, OBJTYPE_TPMCPATTERN);
    x->pattern = pattern;
    x->path = NULL;
    return x;
}

struct TpmcTestState * newTpmcTestState(HashSymbol * path, struct TpmcArcArray * arcs) {
    struct TpmcTestState * x = NEW(TpmcTestState, OBJTYPE_TPMCTESTSTATE);
    x->path = path;
    x->arcs = arcs;
    return x;
}

struct TpmcFinalState * newTpmcFinalState(LamExp * action) {
    struct TpmcFinalState * x = NEW(TpmcFinalState, OBJTYPE_TPMCFINALSTATE);
    x->action = action;
    return x;
}

struct TpmcState * newTpmcState(int stamp, struct TpmcStateValue * state) {
    struct TpmcState * x = NEW(TpmcState, OBJTYPE_TPMCSTATE);
    x->stamp = stamp;
    x->state = state;
    x->refcount = 0;
    x->freeVariables = NULL;
    return x;
}

struct TpmcArc * newTpmcArc(struct TpmcState * state, struct TpmcPattern * test) {
    struct TpmcArc * x = NEW(TpmcArc, OBJTYPE_TPMCARC);
    x->state = state;
    x->test = test;
    return x;
}

struct TpmcStateArrayContainer * newTpmcStateArrayContainer(struct TpmcStateArray * array) {
    struct TpmcStateArrayContainer * x = NEW(TpmcStateArrayContainer, OBJTYPE_TPMCSTATEARRAYCONTAINER);
    x->array = array;
    return x;
}

struct TpmcPatternValue * newTpmcPatternValue(enum TpmcPatternValueType  type, union TpmcPatternValueVal  val) {
    struct TpmcPatternValue * x = NEW(TpmcPatternValue, OBJTYPE_TPMCPATTERNVALUE);
    x->type = type;
    x->val = val;
    return x;
}

struct TpmcStateValue * newTpmcStateValue(enum TpmcStateValueType  type, union TpmcStateValueVal  val) {
    struct TpmcStateValue * x = NEW(TpmcStateValue, OBJTYPE_TPMCSTATEVALUE);
    x->type = type;
    x->val = val;
    return x;
}

struct TpmcMatchRuleArray * newTpmcMatchRuleArray() {
    struct TpmcMatchRuleArray * x = NEW_MATRIX(TpmcMatchRuleArray, 4, struct TpmcMatchRule *, OBJTYPE_TPMCMATCHRULEARRAY);
    x->size = 0;
    x->capacity = 4;
    return x;
}

struct TpmcVariableArray * newTpmcVariableArray() {
    struct TpmcVariableArray * x = NEW_MATRIX(TpmcVariableArray, 4, HashSymbol *, OBJTYPE_TPMCVARIABLEARRAY);
    x->size = 0;
    x->capacity = 4;
    return x;
}

struct TpmcPatternArray * newTpmcPatternArray() {
    struct TpmcPatternArray * x = NEW_MATRIX(TpmcPatternArray, 4, struct TpmcPattern *, OBJTYPE_TPMCPATTERNARRAY);
    x->size = 0;
    x->capacity = 4;
    return x;
}

struct TpmcStateArray * newTpmcStateArray() {
    struct TpmcStateArray * x = NEW_MATRIX(TpmcStateArray, 4, struct TpmcState *, OBJTYPE_TPMCSTATEARRAY);
    x->size = 0;
    x->capacity = 4;
    return x;
}

struct TpmcArcArray * newTpmcArcArray() {
    struct TpmcArcArray * x = NEW_MATRIX(TpmcArcArray, 4, struct TpmcArc *, OBJTYPE_TPMCARCARRAY);
    x->size = 0;
    x->capacity = 4;
    return x;
}

struct TpmcIntArray * newTpmcIntArray() {
    struct TpmcIntArray * x = NEW_MATRIX(TpmcIntArray, 4, int, OBJTYPE_TPMCINTARRAY);
    x->size = 0;
    x->capacity = 4;
    return x;
}

struct TpmcMatrix * newTpmcMatrix(int width, int height) {
    struct TpmcMatrix * x = NEW_MATRIX(TpmcMatrix, width * height, struct TpmcPattern *, OBJTYPE_TPMCMATRIX);
    x->width = width;
    x->height = height;
    return x;
}


struct TpmcMatchRuleArray * pushTpmcMatchRuleArray(struct TpmcMatchRuleArray * old, struct TpmcMatchRule * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcMatchRuleArray, old, old->capacity, old->capacity * 2, struct TpmcMatchRule *);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcVariableArray * pushTpmcVariableArray(struct TpmcVariableArray * old, HashSymbol * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcVariableArray, old, old->capacity, old->capacity * 2, HashSymbol *);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcPatternArray * pushTpmcPatternArray(struct TpmcPatternArray * old, struct TpmcPattern * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcPatternArray, old, old->capacity, old->capacity * 2, struct TpmcPattern *);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcStateArray * pushTpmcStateArray(struct TpmcStateArray * old, struct TpmcState * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcStateArray, old, old->capacity, old->capacity * 2, struct TpmcState *);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcArcArray * pushTpmcArcArray(struct TpmcArcArray * old, struct TpmcArc * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcArcArray, old, old->capacity, old->capacity * 2, struct TpmcArc *);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcIntArray * pushTpmcIntArray(struct TpmcIntArray * old, int entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcIntArray, old, old->capacity, old->capacity * 2, int);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
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

void markTpmcVarPattern(struct TpmcVarPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->name);
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
}

void markTpmcStateArrayContainer(struct TpmcStateArrayContainer * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcStateArray(x->array);
}

void markTpmcPatternValue(struct TpmcPatternValue * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            markTpmcVarPattern(x->val.var);
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
        case TPMCPATTERNVALUE_TYPE_INTEGER:
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
    for (int i = 0; i < x->width * x->height; i++) {
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
        case OBJTYPE_TPMCVARPATTERN:
            markTpmcVarPattern((TpmcVarPattern *)h);
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
        case OBJTYPE_TPMCSTATEARRAYCONTAINER:
            markTpmcStateArrayContainer((TpmcStateArrayContainer *)h);
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
            cant_happen("unrecognized type in markTpmcObj\n");
    }
}

/************************************/

void freeTpmcMatchRules(struct TpmcMatchRules * x) {
    FREE(x, TpmcMatchRules);
}

void freeTpmcMatchRule(struct TpmcMatchRule * x) {
    FREE(x, TpmcMatchRule);
}

void freeTpmcVarPattern(struct TpmcVarPattern * x) {
    FREE(x, TpmcVarPattern);
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

void freeTpmcStateArrayContainer(struct TpmcStateArrayContainer * x) {
    FREE(x, TpmcStateArrayContainer);
}

void freeTpmcPatternValue(struct TpmcPatternValue * x) {
    FREE(x, TpmcPatternValue);
}

void freeTpmcStateValue(struct TpmcStateValue * x) {
    FREE(x, TpmcStateValue);
}

void freeTpmcMatchRuleArray(struct TpmcMatchRuleArray * x) {
    FREE_MATRIX(TpmcMatchRuleArray, x, x->capacity, struct TpmcMatchRule *);
}

void freeTpmcVariableArray(struct TpmcVariableArray * x) {
    FREE_MATRIX(TpmcVariableArray, x, x->capacity, HashSymbol *);
}

void freeTpmcPatternArray(struct TpmcPatternArray * x) {
    FREE_MATRIX(TpmcPatternArray, x, x->capacity, struct TpmcPattern *);
}

void freeTpmcStateArray(struct TpmcStateArray * x) {
    FREE_MATRIX(TpmcStateArray, x, x->capacity, struct TpmcState *);
}

void freeTpmcArcArray(struct TpmcArcArray * x) {
    FREE_MATRIX(TpmcArcArray, x, x->capacity, struct TpmcArc *);
}

void freeTpmcIntArray(struct TpmcIntArray * x) {
    FREE_MATRIX(TpmcIntArray, x, x->capacity, int);
}

void freeTpmcMatrix(struct TpmcMatrix * x) {
    FREE_MATRIX(TpmcMatrix, x, x->width * x->height, struct TpmcPattern *);
}


void freeTpmcObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_TPMCMATCHRULES:
            freeTpmcMatchRules((TpmcMatchRules *)h);
            break;
        case OBJTYPE_TPMCMATCHRULE:
            freeTpmcMatchRule((TpmcMatchRule *)h);
            break;
        case OBJTYPE_TPMCVARPATTERN:
            freeTpmcVarPattern((TpmcVarPattern *)h);
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
        case OBJTYPE_TPMCSTATEARRAYCONTAINER:
            freeTpmcStateArrayContainer((TpmcStateArrayContainer *)h);
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
            cant_happen("unrecognized type in freeTpmcObj\n");
    }
}

char *typenameTpmcObj(int type) {
    switch(type) {
        case OBJTYPE_TPMCMATCHRULES:
            return "TpmcMatchRules";
        case OBJTYPE_TPMCMATCHRULE:
            return "TpmcMatchRule";
        case OBJTYPE_TPMCVARPATTERN:
            return "TpmcVarPattern";
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
        case OBJTYPE_TPMCSTATEARRAYCONTAINER:
            return "TpmcStateArrayContainer";
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
            cant_happen("unrecognized type in typenameTpmcObj\n");
    }
}

