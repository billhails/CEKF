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

struct TpmcMatchRules * newTpmcMatchRules(int id, struct TpmcMatchRuleArray * rules, struct TpmcVariableArray * rootVariables, struct TpmcState * errorState, struct TpmcStateArray * knownStates) {
    struct TpmcMatchRules * x = NEW(TpmcMatchRules, OBJTYPE_TPMCMATCHRULES);
    x->id = id;
    x->rules = rules;
    x->rootVariables = rootVariables;
    x->errorState = errorState;
    x->knownStates = knownStates;
    return x;
}

struct TpmcMatchRule * newTpmcMatchRule(struct TpmcState * action, struct TpmcPatternArray * patterns) {
    struct TpmcMatchRule * x = NEW(TpmcMatchRule, OBJTYPE_TPMCMATCHRULE);
    x->action = action;
    x->patterns = patterns;
    return x;
}

struct TpmcVarPattern * newTpmcVarPattern(HashSymbol * path, HashSymbol * name) {
    struct TpmcVarPattern * x = NEW(TpmcVarPattern, OBJTYPE_TPMCVARPATTERN);
    x->path = path;
    x->name = name;
    return x;
}

struct TpmcComparisonPattern * newTpmcComparisonPattern(HashSymbol * path, struct TpmcPattern * previous, struct TpmcPattern * current) {
    struct TpmcComparisonPattern * x = NEW(TpmcComparisonPattern, OBJTYPE_TPMCCOMPARISONPATTERN);
    x->path = path;
    x->previous = previous;
    x->current = current;
    return x;
}

struct TpmcAssignmentPattern * newTpmcAssignmentPattern(HashSymbol * path, HashSymbol * name, struct TpmcPattern * value) {
    struct TpmcAssignmentPattern * x = NEW(TpmcAssignmentPattern, OBJTYPE_TPMCASSIGNMENTPATTERN);
    x->path = path;
    x->name = name;
    x->value = value;
    return x;
}

struct TpmcWildcardPattern * newTpmcWildcardPattern(HashSymbol * path) {
    struct TpmcWildcardPattern * x = NEW(TpmcWildcardPattern, OBJTYPE_TPMCWILDCARDPATTERN);
    x->path = path;
    return x;
}

struct TpmcConstantPattern * newTpmcConstantPattern(HashSymbol * path, LamExp * value) {
    struct TpmcConstantPattern * x = NEW(TpmcConstantPattern, OBJTYPE_TPMCCONSTANTPATTERN);
    x->path = path;
    x->value = value;
    return x;
}

struct TpmcConstructorPattern * newTpmcConstructorPattern(HashSymbol * path, HashSymbol * tag, struct TpmcPatternArray * components) {
    struct TpmcConstructorPattern * x = NEW(TpmcConstructorPattern, OBJTYPE_TPMCCONSTRUCTORPATTERN);
    x->path = path;
    x->tag = tag;
    x->components = components;
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

struct TpmcArc * newTpmcArc(struct TpmcState * state, struct TpmcPattern * test) {
    struct TpmcArc * x = NEW(TpmcArc, OBJTYPE_TPMCARC);
    x->state = state;
    x->test = test;
    return x;
}

struct TpmcPattern * newTpmcPattern(enum TpmcPatternType  type, union TpmcPatternVal  val) {
    struct TpmcPattern * x = NEW(TpmcPattern, OBJTYPE_TPMCPATTERN);
    x->type = type;
    x->val = val;
    return x;
}

struct TpmcState * newTpmcState(enum TpmcStateType  type, union TpmcStateVal  val) {
    struct TpmcState * x = NEW(TpmcState, OBJTYPE_TPMCSTATE);
    x->type = type;
    x->val = val;
    return x;
}

struct TpmcMatchRuleArray * newTpmcMatchRuleArray() {
    struct TpmcMatchRuleArray * res = NEW_MATRIX(TpmcMatchRuleArray, 4, TpmcMatchRule, OBJTYPE_TPMCMATCHRULEARRAY);
    res->size = 0;
    res->capacity = 4;
    return res;
}

struct TpmcVariableArray * newTpmcVariableArray() {
    struct TpmcVariableArray * res = NEW_MATRIX(TpmcVariableArray, 4, HashSymbol, OBJTYPE_TPMCVARIABLEARRAY);
    res->size = 0;
    res->capacity = 4;
    return res;
}

struct TpmcStateArray * newTpmcStateArray() {
    struct TpmcStateArray * res = NEW_MATRIX(TpmcStateArray, 4, TpmcState, OBJTYPE_TPMCSTATEARRAY);
    res->size = 0;
    res->capacity = 4;
    return res;
}

struct TpmcPatternArray * newTpmcPatternArray() {
    struct TpmcPatternArray * res = NEW_MATRIX(TpmcPatternArray, 4, TpmcPattern, OBJTYPE_TPMCPATTERNARRAY);
    res->size = 0;
    res->capacity = 4;
    return res;
}

struct TpmcArcArray * newTpmcArcArray() {
    struct TpmcArcArray * res = NEW_MATRIX(TpmcArcArray, 4, TpmcArc, OBJTYPE_TPMCARCARRAY);
    res->size = 0;
    res->capacity = 4;
    return res;
}

struct TpmcMatrix * newTpmcMatrix(int x, int y) {
    struct TpmcMatrix * res = NEW_MATRIX(TpmcMatrix, x * y, TpmcPattern, OBJTYPE_TPMCMATRIX);
    res->x = x;
    res->y = y;
    return res;
}


struct TpmcMatchRuleArray * pushTpmcMatchRuleArray(struct TpmcMatchRuleArray * old, struct TpmcMatchRule * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcMatchRuleArray, old, old->capacity, old->capacity * 2, TpmcMatchRule);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcVariableArray * pushTpmcVariableArray(struct TpmcVariableArray * old, HashSymbol * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcVariableArray, old, old->capacity, old->capacity * 2, HashSymbol);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcStateArray * pushTpmcStateArray(struct TpmcStateArray * old, struct TpmcState * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcStateArray, old, old->capacity, old->capacity * 2, TpmcState);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcPatternArray * pushTpmcPatternArray(struct TpmcPatternArray * old, struct TpmcPattern * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcPatternArray, old, old->capacity, old->capacity * 2, TpmcPattern);
        old->capacity *= 2;
    }
    old->entries[old->size++] = entry;
    return old;
}

struct TpmcArcArray * pushTpmcArcArray(struct TpmcArcArray * old, struct TpmcArc * entry) {
    if (old->size == old->capacity) {
        old = GROW_MATRIX(TpmcArcArray, old, old->capacity, old->capacity * 2, TpmcArc);
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
    markTpmcState(x->errorState);
}

void markTpmcMatchRule(struct TpmcMatchRule * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcState(x->action);
}

void markTpmcVarPattern(struct TpmcVarPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
    markHashSymbol(x->name);
}

void markTpmcComparisonPattern(struct TpmcComparisonPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
    markTpmcPattern(x->previous);
    markTpmcPattern(x->current);
}

void markTpmcAssignmentPattern(struct TpmcAssignmentPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
    markHashSymbol(x->name);
    markTpmcPattern(x->value);
}

void markTpmcWildcardPattern(struct TpmcWildcardPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
}

void markTpmcConstantPattern(struct TpmcConstantPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
    markLamExp(x->value);
}

void markTpmcConstructorPattern(struct TpmcConstructorPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
    markHashSymbol(x->tag);
}

void markTpmcTestState(struct TpmcTestState * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->path);
}

void markTpmcFinalState(struct TpmcFinalState * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->action);
}

void markTpmcArc(struct TpmcArc * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTpmcState(x->state);
    markTpmcPattern(x->test);
}

void markTpmcPattern(struct TpmcPattern * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case TPMCPATTERN_TYPE_VAR:
            markTpmcVarPattern(x->val.var);
            break;
        case TPMCPATTERN_TYPE_COMPARISON:
            markTpmcComparisonPattern(x->val.comparison);
            break;
        case TPMCPATTERN_TYPE_ASSIGNMENT:
            markTpmcAssignmentPattern(x->val.assignment);
            break;
        case TPMCPATTERN_TYPE_WILDCARD:
            markTpmcWildcardPattern(x->val.wildcard);
            break;
        case TPMCPATTERN_TYPE_CONSTANT:
            markTpmcConstantPattern(x->val.constant);
            break;
        case TPMCPATTERN_TYPE_CONSTRUCTOR:
            markTpmcConstructorPattern(x->val.constructor);
            break;
        default:
            cant_happen("unrecognised type %d in markTpmcPattern", x->type);
    }
}

void markTpmcState(struct TpmcState * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case TPMCSTATE_TYPE_TEST:
            markTpmcTestState(x->val.test);
            break;
        case TPMCSTATE_TYPE_FINAL:
            markTpmcFinalState(x->val.final);
            break;
        case TPMCSTATE_TYPE_ERROR:
            break;
        default:
            cant_happen("unrecognised type %d in markTpmcState", x->type);
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

void markTpmcStateArray(struct TpmcStateArray * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
        markTpmcState(x->entries[i]);
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

void markTpmcArcArray(struct TpmcArcArray * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
        markTpmcArc(x->entries[i]);
    }
}

void markTpmcMatrix(struct TpmcMatrix * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->x * x->y; i++) {
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
        case OBJTYPE_TPMCWILDCARDPATTERN:
            markTpmcWildcardPattern((TpmcWildcardPattern *)h);
            break;
        case OBJTYPE_TPMCCONSTANTPATTERN:
            markTpmcConstantPattern((TpmcConstantPattern *)h);
            break;
        case OBJTYPE_TPMCCONSTRUCTORPATTERN:
            markTpmcConstructorPattern((TpmcConstructorPattern *)h);
            break;
        case OBJTYPE_TPMCTESTSTATE:
            markTpmcTestState((TpmcTestState *)h);
            break;
        case OBJTYPE_TPMCFINALSTATE:
            markTpmcFinalState((TpmcFinalState *)h);
            break;
        case OBJTYPE_TPMCARC:
            markTpmcArc((TpmcArc *)h);
            break;
        case OBJTYPE_TPMCPATTERN:
            markTpmcPattern((TpmcPattern *)h);
            break;
        case OBJTYPE_TPMCSTATE:
            markTpmcState((TpmcState *)h);
            break;
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

void freeTpmcWildcardPattern(struct TpmcWildcardPattern * x) {
    FREE(x, TpmcWildcardPattern);
}

void freeTpmcConstantPattern(struct TpmcConstantPattern * x) {
    FREE(x, TpmcConstantPattern);
}

void freeTpmcConstructorPattern(struct TpmcConstructorPattern * x) {
    FREE(x, TpmcConstructorPattern);
}

void freeTpmcTestState(struct TpmcTestState * x) {
    FREE(x, TpmcTestState);
}

void freeTpmcFinalState(struct TpmcFinalState * x) {
    FREE(x, TpmcFinalState);
}

void freeTpmcArc(struct TpmcArc * x) {
    FREE(x, TpmcArc);
}

void freeTpmcPattern(struct TpmcPattern * x) {
    FREE(x, TpmcPattern);
}

void freeTpmcState(struct TpmcState * x) {
    FREE(x, TpmcState);
}

void freeTpmcMatchRuleArray(struct TpmcMatchRuleArray * x) {
    FREE_MATRIX(TpmcMatchRuleArray, x, x->capacity, struct TpmcMatchRule *);
}

void freeTpmcVariableArray(struct TpmcVariableArray * x) {
    FREE_MATRIX(TpmcVariableArray, x, x->capacity, HashSymbol *);
}

void freeTpmcStateArray(struct TpmcStateArray * x) {
    FREE_MATRIX(TpmcStateArray, x, x->capacity, struct TpmcState *);
}

void freeTpmcPatternArray(struct TpmcPatternArray * x) {
    FREE_MATRIX(TpmcPatternArray, x, x->capacity, struct TpmcPattern *);
}

void freeTpmcArcArray(struct TpmcArcArray * x) {
    FREE_MATRIX(TpmcArcArray, x, x->capacity, struct TpmcArc *);
}

void freeTpmcMatrix(struct TpmcMatrix * x) {
    FREE_MATRIX(TpmcMatrix, x, x->x * x->y, struct TpmcPattern *);
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
        case OBJTYPE_TPMCWILDCARDPATTERN:
            freeTpmcWildcardPattern((TpmcWildcardPattern *)h);
            break;
        case OBJTYPE_TPMCCONSTANTPATTERN:
            freeTpmcConstantPattern((TpmcConstantPattern *)h);
            break;
        case OBJTYPE_TPMCCONSTRUCTORPATTERN:
            freeTpmcConstructorPattern((TpmcConstructorPattern *)h);
            break;
        case OBJTYPE_TPMCTESTSTATE:
            freeTpmcTestState((TpmcTestState *)h);
            break;
        case OBJTYPE_TPMCFINALSTATE:
            freeTpmcFinalState((TpmcFinalState *)h);
            break;
        case OBJTYPE_TPMCARC:
            freeTpmcArc((TpmcArc *)h);
            break;
        case OBJTYPE_TPMCPATTERN:
            freeTpmcPattern((TpmcPattern *)h);
            break;
        case OBJTYPE_TPMCSTATE:
            freeTpmcState((TpmcState *)h);
            break;
        case OBJTYPE_TPMCMATCHRULEARRAY:
            freeTpmcMatchRuleArray((TpmcMatchRuleArray *)h);
            break;
        case OBJTYPE_TPMCVARIABLEARRAY:
            freeTpmcVariableArray((TpmcVariableArray *)h);
            break;
        case OBJTYPE_TPMCSTATEARRAY:
            freeTpmcStateArray((TpmcStateArray *)h);
            break;
        case OBJTYPE_TPMCPATTERNARRAY:
            freeTpmcPatternArray((TpmcPatternArray *)h);
            break;
        case OBJTYPE_TPMCARCARRAY:
            freeTpmcArcArray((TpmcArcArray *)h);
            break;
        case OBJTYPE_TPMCMATRIX:
            freeTpmcMatrix((TpmcMatrix *)h);
            break;
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
        case OBJTYPE_TPMCWILDCARDPATTERN:
            return "TpmcWildcardPattern";
        case OBJTYPE_TPMCCONSTANTPATTERN:
            return "TpmcConstantPattern";
        case OBJTYPE_TPMCCONSTRUCTORPATTERN:
            return "TpmcConstructorPattern";
        case OBJTYPE_TPMCTESTSTATE:
            return "TpmcTestState";
        case OBJTYPE_TPMCFINALSTATE:
            return "TpmcFinalState";
        case OBJTYPE_TPMCARC:
            return "TpmcArc";
        case OBJTYPE_TPMCPATTERN:
            return "TpmcPattern";
        case OBJTYPE_TPMCSTATE:
            return "TpmcState";
        case OBJTYPE_TPMCMATCHRULEARRAY:
            return "TpmcMatchRuleArray";
        case OBJTYPE_TPMCVARIABLEARRAY:
            return "TpmcVariableArray";
        case OBJTYPE_TPMCSTATEARRAY:
            return "TpmcStateArray";
        case OBJTYPE_TPMCPATTERNARRAY:
            return "TpmcPatternArray";
        case OBJTYPE_TPMCARCARRAY:
            return "TpmcArcArray";
        case OBJTYPE_TPMCMATRIX:
            return "TpmcMatrix";
    }
}

