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
 * Generated from src/tpmc.yaml by tools/makeAST.py
 */

#include "tpmc.h"
#include <stdio.h>
#include <strings.h>
#include "common.h"
#ifdef DEBUG_ALLOC
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

/*
 * constructor functions
 */

struct TpmcMatchRules * newTpmcMatchRules(struct TpmcMatchRuleArray * rules, struct TpmcVariableArray * rootVariables) {
    struct TpmcMatchRules * x = NEW(TpmcMatchRules, OBJTYPE_TPMCMATCHRULES);
    DEBUG("new TpmcMatchRules %pn", x);
    x->rules = rules;
    x->rootVariables = rootVariables;
    return x;
}

struct TpmcMatchRule * newTpmcMatchRule(struct TpmcState * action, struct TpmcPatternArray * patterns) {
    struct TpmcMatchRule * x = NEW(TpmcMatchRule, OBJTYPE_TPMCMATCHRULE);
    DEBUG("new TpmcMatchRule %pn", x);
    x->action = action;
    x->patterns = patterns;
    return x;
}

struct TpmcComparisonPattern * newTpmcComparisonPattern(struct TpmcPattern * previous, struct TpmcPattern * current) {
    struct TpmcComparisonPattern * x = NEW(TpmcComparisonPattern, OBJTYPE_TPMCCOMPARISONPATTERN);
    DEBUG("new TpmcComparisonPattern %pn", x);
    x->previous = previous;
    x->current = current;
    return x;
}

struct TpmcAssignmentPattern * newTpmcAssignmentPattern(HashSymbol * name, struct TpmcPattern * value) {
    struct TpmcAssignmentPattern * x = NEW(TpmcAssignmentPattern, OBJTYPE_TPMCASSIGNMENTPATTERN);
    DEBUG("new TpmcAssignmentPattern %pn", x);
    x->name = name;
    x->value = value;
    return x;
}

struct TpmcConstructorPattern * newTpmcConstructorPattern(HashSymbol * tag, LamTypeConstructorInfo * info, struct TpmcPatternArray * components) {
    struct TpmcConstructorPattern * x = NEW(TpmcConstructorPattern, OBJTYPE_TPMCCONSTRUCTORPATTERN);
    DEBUG("new TpmcConstructorPattern %pn", x);
    x->tag = tag;
    x->info = info;
    x->components = components;
    return x;
}

struct TpmcPattern * newTpmcPattern(struct TpmcPatternValue * pattern) {
    struct TpmcPattern * x = NEW(TpmcPattern, OBJTYPE_TPMCPATTERN);
    DEBUG("new TpmcPattern %pn", x);
    x->pattern = pattern;
    x->path = NULL;
    return x;
}

struct TpmcTestState * newTpmcTestState(HashSymbol * path, struct TpmcArcArray * arcs) {
    struct TpmcTestState * x = NEW(TpmcTestState, OBJTYPE_TPMCTESTSTATE);
    DEBUG("new TpmcTestState %pn", x);
    x->path = path;
    x->arcs = arcs;
    return x;
}

struct TpmcFinalState * newTpmcFinalState(LamExp * action) {
    struct TpmcFinalState * x = NEW(TpmcFinalState, OBJTYPE_TPMCFINALSTATE);
    DEBUG("new TpmcFinalState %pn", x);
    x->action = action;
    return x;
}

struct TpmcState * newTpmcState(int stamp, struct TpmcStateValue * state) {
    struct TpmcState * x = NEW(TpmcState, OBJTYPE_TPMCSTATE);
    DEBUG("new TpmcState %pn", x);
    x->stamp = stamp;
    x->state = state;
    x->refcount = 0;
    x->freeVariables = NULL;
    return x;
}

struct TpmcArc * newTpmcArc(struct TpmcState * state, struct TpmcPattern * test, HashTable * freeVariables) {
    struct TpmcArc * x = NEW(TpmcArc, OBJTYPE_TPMCARC);
    DEBUG("new TpmcArc %pn", x);
    x->state = state;
    x->test = test;
    x->freeVariables = freeVariables;
    return x;
}

struct TpmcArcList * newTpmcArcList(struct TpmcArc * arc, struct TpmcArcList * next) {
    struct TpmcArcList * x = NEW(TpmcArcList, OBJTYPE_TPMCARCLIST);
    DEBUG("new TpmcArcList %pn", x);
    x->arc = arc;
    x->next = next;
    return x;
}

struct TpmcIntList * newTpmcIntList(int integer, struct TpmcIntList * next) {
    struct TpmcIntList * x = NEW(TpmcIntList, OBJTYPE_TPMCINTLIST);
    DEBUG("new TpmcIntList %pn", x);
    x->integer = integer;
    x->next = next;
    return x;
}

struct TpmcPatternValue * newTpmcPatternValue(enum TpmcPatternValueType  type, union TpmcPatternValueVal  val) {
    struct TpmcPatternValue * x = NEW(TpmcPatternValue, OBJTYPE_TPMCPATTERNVALUE);
    DEBUG("new TpmcPatternValue %pn", x);
    x->type = type;
    x->val = val;
    return x;
}

struct TpmcStateValue * newTpmcStateValue(enum TpmcStateValueType  type, union TpmcStateValueVal  val) {
    struct TpmcStateValue * x = NEW(TpmcStateValue, OBJTYPE_TPMCSTATEVALUE);
    DEBUG("new TpmcStateValue %pn", x);
    x->type = type;
    x->val = val;
    return x;
}

struct TpmcMatchRuleArray * newTpmcMatchRuleArray(void) {
    struct TpmcMatchRuleArray * x = NEW(TpmcMatchRuleArray, OBJTYPE_TPMCMATCHRULEARRAY);
    DEBUG("new TpmcMatchRuleArray %p", x);
    x->entries = NULL;
    x->size = 0;
    x->capacity = 0;
    int save = PROTECT(x);
    x->entries = NEW_ARRAY(struct TpmcMatchRule *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcVariableArray * newTpmcVariableArray(void) {
    struct TpmcVariableArray * x = NEW(TpmcVariableArray, OBJTYPE_TPMCVARIABLEARRAY);
    DEBUG("new TpmcVariableArray %p", x);
    x->entries = NULL;
    x->size = 0;
    x->capacity = 0;
    int save = PROTECT(x);
    x->entries = NEW_ARRAY(HashSymbol *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcPatternArray * newTpmcPatternArray(char * _tag) {
    struct TpmcPatternArray * x = NEW(TpmcPatternArray, OBJTYPE_TPMCPATTERNARRAY);
    DEBUG("new TpmcPatternArray %p", x);
    x->entries = NULL;
    x->_tag = _tag;
    x->size = 0;
    x->capacity = 0;
    int save = PROTECT(x);
    x->entries = NEW_ARRAY(struct TpmcPattern *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcStateArray * newTpmcStateArray(char * _tag) {
    struct TpmcStateArray * x = NEW(TpmcStateArray, OBJTYPE_TPMCSTATEARRAY);
    DEBUG("new TpmcStateArray %p", x);
    x->entries = NULL;
    x->_tag = _tag;
    x->size = 0;
    x->capacity = 0;
    int save = PROTECT(x);
    x->entries = NEW_ARRAY(struct TpmcState *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcArcArray * newTpmcArcArray(void) {
    struct TpmcArcArray * x = NEW(TpmcArcArray, OBJTYPE_TPMCARCARRAY);
    DEBUG("new TpmcArcArray %p", x);
    x->entries = NULL;
    x->size = 0;
    x->capacity = 0;
    int save = PROTECT(x);
    x->entries = NEW_ARRAY(struct TpmcArc *, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcIntArray * newTpmcIntArray(void) {
    struct TpmcIntArray * x = NEW(TpmcIntArray, OBJTYPE_TPMCINTARRAY);
    DEBUG("new TpmcIntArray %p", x);
    x->entries = NULL;
    x->size = 0;
    x->capacity = 0;
    int save = PROTECT(x);
    x->entries = NEW_ARRAY(int, 4);
    x->capacity = 4;
    UNPROTECT(save);
    return x;
}

struct TpmcMatrix * newTpmcMatrix(int width, int height) {
    struct TpmcMatrix * x = NEW(TpmcMatrix, OBJTYPE_TPMCMATRIX);
    DEBUG("new TpmcMatrix %p", x);
    x->entries = NULL;
    x->width = 0;
    x->height = 0;
    int save = PROTECT(x);
    if (width * height > 0) {
        x->entries = NEW_ARRAY(struct TpmcPattern *, width * height);
        bzero(x->entries, sizeof(struct TpmcPattern *) * width * height);
    }
    x->width = width;
    x->height = height;
    UNPROTECT(save);
    return x;
}


/*
 * copy functions
 */

struct TpmcMatchRules * copyTpmcMatchRules(struct TpmcMatchRules * o) {
    if (o == NULL) return NULL;
    struct TpmcMatchRules * x = NEW(TpmcMatchRules, OBJTYPE_TPMCMATCHRULES);
    DEBUG("copy TpmcMatchRules %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcMatchRules));
    x->header = _h;
    int save = PROTECT(x);
    x->rules = copyTpmcMatchRuleArray(o->rules);
    x->rootVariables = copyTpmcVariableArray(o->rootVariables);
    UNPROTECT(save);
    return x;
}

struct TpmcMatchRule * copyTpmcMatchRule(struct TpmcMatchRule * o) {
    if (o == NULL) return NULL;
    struct TpmcMatchRule * x = NEW(TpmcMatchRule, OBJTYPE_TPMCMATCHRULE);
    DEBUG("copy TpmcMatchRule %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcMatchRule));
    x->header = _h;
    int save = PROTECT(x);
    x->action = copyTpmcState(o->action);
    x->patterns = copyTpmcPatternArray(o->patterns);
    UNPROTECT(save);
    return x;
}

struct TpmcComparisonPattern * copyTpmcComparisonPattern(struct TpmcComparisonPattern * o) {
    if (o == NULL) return NULL;
    struct TpmcComparisonPattern * x = NEW(TpmcComparisonPattern, OBJTYPE_TPMCCOMPARISONPATTERN);
    DEBUG("copy TpmcComparisonPattern %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcComparisonPattern));
    x->header = _h;
    int save = PROTECT(x);
    x->previous = copyTpmcPattern(o->previous);
    x->current = copyTpmcPattern(o->current);
    UNPROTECT(save);
    return x;
}

struct TpmcAssignmentPattern * copyTpmcAssignmentPattern(struct TpmcAssignmentPattern * o) {
    if (o == NULL) return NULL;
    struct TpmcAssignmentPattern * x = NEW(TpmcAssignmentPattern, OBJTYPE_TPMCASSIGNMENTPATTERN);
    DEBUG("copy TpmcAssignmentPattern %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcAssignmentPattern));
    x->header = _h;
    int save = PROTECT(x);
    x->name = o->name;
    x->value = copyTpmcPattern(o->value);
    UNPROTECT(save);
    return x;
}

struct TpmcConstructorPattern * copyTpmcConstructorPattern(struct TpmcConstructorPattern * o) {
    if (o == NULL) return NULL;
    struct TpmcConstructorPattern * x = NEW(TpmcConstructorPattern, OBJTYPE_TPMCCONSTRUCTORPATTERN);
    DEBUG("copy TpmcConstructorPattern %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcConstructorPattern));
    x->header = _h;
    int save = PROTECT(x);
    x->tag = o->tag;
    x->info = copyLamTypeConstructorInfo(o->info);
    x->components = copyTpmcPatternArray(o->components);
    UNPROTECT(save);
    return x;
}

struct TpmcPattern * copyTpmcPattern(struct TpmcPattern * o) {
    if (o == NULL) return NULL;
    struct TpmcPattern * x = NEW(TpmcPattern, OBJTYPE_TPMCPATTERN);
    DEBUG("copy TpmcPattern %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcPattern));
    x->header = _h;
    int save = PROTECT(x);
    x->path = o->path;
    x->pattern = copyTpmcPatternValue(o->pattern);
    UNPROTECT(save);
    return x;
}

struct TpmcTestState * copyTpmcTestState(struct TpmcTestState * o) {
    if (o == NULL) return NULL;
    struct TpmcTestState * x = NEW(TpmcTestState, OBJTYPE_TPMCTESTSTATE);
    DEBUG("copy TpmcTestState %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcTestState));
    x->header = _h;
    int save = PROTECT(x);
    x->path = o->path;
    x->arcs = copyTpmcArcArray(o->arcs);
    UNPROTECT(save);
    return x;
}

struct TpmcFinalState * copyTpmcFinalState(struct TpmcFinalState * o) {
    if (o == NULL) return NULL;
    struct TpmcFinalState * x = NEW(TpmcFinalState, OBJTYPE_TPMCFINALSTATE);
    DEBUG("copy TpmcFinalState %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcFinalState));
    x->header = _h;
    int save = PROTECT(x);
    x->action = copyLamExp(o->action);
    UNPROTECT(save);
    return x;
}

struct TpmcState * copyTpmcState(struct TpmcState * o) {
    if (o == NULL) return NULL;
    struct TpmcState * x = NEW(TpmcState, OBJTYPE_TPMCSTATE);
    DEBUG("copy TpmcState %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcState));
    x->header = _h;
    int save = PROTECT(x);
    x->refcount = o->refcount;
    x->stamp = o->stamp;
    x->freeVariables = o->freeVariables;
    x->state = copyTpmcStateValue(o->state);
    UNPROTECT(save);
    return x;
}

struct TpmcArc * copyTpmcArc(struct TpmcArc * o) {
    if (o == NULL) return NULL;
    struct TpmcArc * x = NEW(TpmcArc, OBJTYPE_TPMCARC);
    DEBUG("copy TpmcArc %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcArc));
    x->header = _h;
    int save = PROTECT(x);
    x->state = copyTpmcState(o->state);
    x->test = copyTpmcPattern(o->test);
    x->freeVariables = o->freeVariables;
    UNPROTECT(save);
    return x;
}

struct TpmcArcList * copyTpmcArcList(struct TpmcArcList * o) {
    if (o == NULL) return NULL;
    struct TpmcArcList * x = NEW(TpmcArcList, OBJTYPE_TPMCARCLIST);
    DEBUG("copy TpmcArcList %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcArcList));
    x->header = _h;
    int save = PROTECT(x);
    x->arc = copyTpmcArc(o->arc);
    x->next = copyTpmcArcList(o->next);
    UNPROTECT(save);
    return x;
}

struct TpmcIntList * copyTpmcIntList(struct TpmcIntList * o) {
    if (o == NULL) return NULL;
    struct TpmcIntList * x = NEW(TpmcIntList, OBJTYPE_TPMCINTLIST);
    DEBUG("copy TpmcIntList %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcIntList));
    x->header = _h;
    int save = PROTECT(x);
    x->integer = o->integer;
    x->next = copyTpmcIntList(o->next);
    UNPROTECT(save);
    return x;
}

struct TpmcPatternValue * copyTpmcPatternValue(struct TpmcPatternValue * o) {
    if (o == NULL) return NULL;
    struct TpmcPatternValue * x = NEW(TpmcPatternValue, OBJTYPE_TPMCPATTERNVALUE);
    DEBUG("copy TpmcPatternValue %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcPatternValue));
    x->header = _h;
    int save = PROTECT(x);
    switch(o->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            x->val.var = o->val.var;
            break;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            x->val.comparison = copyTpmcComparisonPattern(o->val.comparison);
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            x->val.assignment = copyTpmcAssignmentPattern(o->val.assignment);
            break;
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            x->val.wildcard = o->val.wildcard;
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            x->val.character = o->val.character;
            break;
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            x->val.biginteger = o->val.biginteger;
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            x->val.constructor = copyTpmcConstructorPattern(o->val.constructor);
            break;
        default:
            cant_happen("unrecognised type %d in copyTpmcPatternValue", o->type);
    }
    x->type = o->type;
    UNPROTECT(save);
    return x;
}

struct TpmcStateValue * copyTpmcStateValue(struct TpmcStateValue * o) {
    if (o == NULL) return NULL;
    struct TpmcStateValue * x = NEW(TpmcStateValue, OBJTYPE_TPMCSTATEVALUE);
    DEBUG("copy TpmcStateValue %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcStateValue));
    x->header = _h;
    int save = PROTECT(x);
    switch(o->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            x->val.test = copyTpmcTestState(o->val.test);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            x->val.final = copyTpmcFinalState(o->val.final);
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            x->val.error = o->val.error;
            break;
        default:
            cant_happen("unrecognised type %d in copyTpmcStateValue", o->type);
    }
    x->type = o->type;
    UNPROTECT(save);
    return x;
}

struct TpmcMatchRuleArray * copyTpmcMatchRuleArray(struct TpmcMatchRuleArray * o) {
    if (o == NULL) return NULL;
    struct TpmcMatchRuleArray * x = NEW(TpmcMatchRuleArray, OBJTYPE_TPMCMATCHRULEARRAY);
    DEBUG("copy TpmcMatchRuleArray %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcMatchRuleArray));
    x->header = _h;
    int save = PROTECT(x);
    if (o->entries != NULL) {
        x->entries = NEW_ARRAY(struct TpmcMatchRule *, x->capacity);
        x->size = 0;
        x->capacity = o->capacity;
        for (int i = 0; i < o->size; i++) {
            x->entries[i] = copyTpmcMatchRule(o->entries[i]);
            x->size++;
        }
    }
    UNPROTECT(save);
    return x;
}

struct TpmcVariableArray * copyTpmcVariableArray(struct TpmcVariableArray * o) {
    if (o == NULL) return NULL;
    struct TpmcVariableArray * x = NEW(TpmcVariableArray, OBJTYPE_TPMCVARIABLEARRAY);
    DEBUG("copy TpmcVariableArray %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcVariableArray));
    x->header = _h;
    int save = PROTECT(x);
    if (o->entries != NULL) {
        x->entries = NEW_ARRAY(HashSymbol *, x->capacity);
        x->size = 0;
        x->capacity = o->capacity;
        for (int i = 0; i < o->size; i++) {
            x->entries[i] = o->entries[i];
            x->size++;
        }
    }
    UNPROTECT(save);
    return x;
}

struct TpmcPatternArray * copyTpmcPatternArray(struct TpmcPatternArray * o) {
    if (o == NULL) return NULL;
    struct TpmcPatternArray * x = NEW(TpmcPatternArray, OBJTYPE_TPMCPATTERNARRAY);
    DEBUG("copy TpmcPatternArray %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcPatternArray));
    x->header = _h;
    int save = PROTECT(x);
    if (o->entries != NULL) {
        x->entries = NEW_ARRAY(struct TpmcPattern *, x->capacity);
        x->size = 0;
        x->capacity = o->capacity;
        for (int i = 0; i < o->size; i++) {
            x->entries[i] = copyTpmcPattern(o->entries[i]);
            x->size++;
        }
    }
    UNPROTECT(save);
    return x;
}

struct TpmcStateArray * copyTpmcStateArray(struct TpmcStateArray * o) {
    if (o == NULL) return NULL;
    struct TpmcStateArray * x = NEW(TpmcStateArray, OBJTYPE_TPMCSTATEARRAY);
    DEBUG("copy TpmcStateArray %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcStateArray));
    x->header = _h;
    int save = PROTECT(x);
    if (o->entries != NULL) {
        x->entries = NEW_ARRAY(struct TpmcState *, x->capacity);
        x->size = 0;
        x->capacity = o->capacity;
        for (int i = 0; i < o->size; i++) {
            x->entries[i] = copyTpmcState(o->entries[i]);
            x->size++;
        }
    }
    UNPROTECT(save);
    return x;
}

struct TpmcArcArray * copyTpmcArcArray(struct TpmcArcArray * o) {
    if (o == NULL) return NULL;
    struct TpmcArcArray * x = NEW(TpmcArcArray, OBJTYPE_TPMCARCARRAY);
    DEBUG("copy TpmcArcArray %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcArcArray));
    x->header = _h;
    int save = PROTECT(x);
    if (o->entries != NULL) {
        x->entries = NEW_ARRAY(struct TpmcArc *, x->capacity);
        x->size = 0;
        x->capacity = o->capacity;
        for (int i = 0; i < o->size; i++) {
            x->entries[i] = copyTpmcArc(o->entries[i]);
            x->size++;
        }
    }
    UNPROTECT(save);
    return x;
}

struct TpmcIntArray * copyTpmcIntArray(struct TpmcIntArray * o) {
    if (o == NULL) return NULL;
    struct TpmcIntArray * x = NEW(TpmcIntArray, OBJTYPE_TPMCINTARRAY);
    DEBUG("copy TpmcIntArray %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcIntArray));
    x->header = _h;
    int save = PROTECT(x);
    if (o->entries != NULL) {
        x->entries = NEW_ARRAY(int, x->capacity);
        x->size = 0;
        x->capacity = o->capacity;
        for (int i = 0; i < o->size; i++) {
            x->entries[i] = o->entries[i];
            x->size++;
        }
    }
    UNPROTECT(save);
    return x;
}

struct TpmcMatrix * copyTpmcMatrix(struct TpmcMatrix * o) {
    if (o == NULL) return NULL;
    struct TpmcMatrix * x = NEW(TpmcMatrix, OBJTYPE_TPMCMATRIX);
    DEBUG("copy TpmcMatrix %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TpmcMatrix));
    x->header = _h;
    int save = PROTECT(x);
    if (o->entries != NULL) {
        x->entries = NEW_ARRAY(struct TpmcPattern *, x->width * x->height);
        x->width = 0;
        x->height = 0;
        for (int i = 0; i < (o->width * o->height); i++) {
            x->entries[i] = copyTpmcPattern(o->entries[i]);
        }
        x->height = o->height;
        x->width = o->width;
    }
    UNPROTECT(save);
    return x;
}


/*
 * push functions
 */

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


/*
 * mark functions
 */

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


/*
 * generic mark function
 */

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

/*
 * free functions
 */

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


/*
 * generic free function
 */

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

/*
 * type identifier function
 */

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
