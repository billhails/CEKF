#ifndef cekf_tpmc_h
#define cekf_tpmc_h
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

#include "hash.h"
#include "memory.h"
#include "common.h"
#ifdef DEBUG_LOG_GC
#include <stdio.h>
#endif
#include "lambda.h"
#include "lambda_pp.h"

typedef enum TpmcPatternValueType {
    TPMCPATTERNVALUE_TYPE_VAR,
    TPMCPATTERNVALUE_TYPE_COMPARISON,
    TPMCPATTERNVALUE_TYPE_ASSIGNMENT,
    TPMCPATTERNVALUE_TYPE_WILDCARD,
    TPMCPATTERNVALUE_TYPE_CHARACTER,
    TPMCPATTERNVALUE_TYPE_INTEGER,
    TPMCPATTERNVALUE_TYPE_CONSTRUCTOR,
} TpmcPatternValueType;

typedef enum TpmcStateValueType {
    TPMCSTATEVALUE_TYPE_TEST,
    TPMCSTATEVALUE_TYPE_FINAL,
    TPMCSTATEVALUE_TYPE_ERROR,
} TpmcStateValueType;



typedef union TpmcPatternValueVal {
    HashSymbol * var;
    struct TpmcComparisonPattern * comparison;
    struct TpmcAssignmentPattern * assignment;
    void * wildcard;
    char character;
    int integer;
    struct TpmcConstructorPattern * constructor;
} TpmcPatternValueVal;

typedef union TpmcStateValueVal {
    struct TpmcTestState * test;
    struct TpmcFinalState * final;
    void * error;
} TpmcStateValueVal;



typedef struct TpmcMatchRules {
    Header header;
    struct TpmcMatchRuleArray * rules;
    struct TpmcVariableArray * rootVariables;
} TpmcMatchRules;

typedef struct TpmcMatchRule {
    Header header;
    struct TpmcState * action;
    struct TpmcPatternArray * patterns;
} TpmcMatchRule;

typedef struct TpmcComparisonPattern {
    Header header;
    struct TpmcPattern * previous;
    struct TpmcPattern * current;
} TpmcComparisonPattern;

typedef struct TpmcAssignmentPattern {
    Header header;
    HashSymbol * name;
    struct TpmcPattern * value;
} TpmcAssignmentPattern;

typedef struct TpmcConstructorPattern {
    Header header;
    HashSymbol * tag;
    LamTypeConstructorInfo * info;
    struct TpmcPatternArray * components;
} TpmcConstructorPattern;

typedef struct TpmcPattern {
    Header header;
    HashSymbol * path;
    struct TpmcPatternValue * pattern;
} TpmcPattern;

typedef struct TpmcTestState {
    Header header;
    HashSymbol * path;
    struct TpmcArcArray * arcs;
} TpmcTestState;

typedef struct TpmcFinalState {
    Header header;
    LamExp * action;
} TpmcFinalState;

typedef struct TpmcState {
    Header header;
    int refcount;
    int stamp;
    HashTable * freeVariables;
    struct TpmcStateValue * state;
} TpmcState;

typedef struct TpmcArc {
    Header header;
    struct TpmcState * state;
    struct TpmcPattern * test;
    HashTable * freeVariables;
} TpmcArc;

typedef struct TpmcArcList {
    Header header;
    struct TpmcArc * arc;
    struct TpmcArcList * next;
} TpmcArcList;

typedef struct TpmcIntList {
    Header header;
    int integer;
    struct TpmcIntList * next;
} TpmcIntList;

typedef struct TpmcPatternValue {
    Header header;
    enum TpmcPatternValueType  type;
    union TpmcPatternValueVal  val;
} TpmcPatternValue;

typedef struct TpmcStateValue {
    Header header;
    enum TpmcStateValueType  type;
    union TpmcStateValueVal  val;
} TpmcStateValue;



typedef struct TpmcMatchRuleArray {
    Header header;
    int size;
    int capacity;
    struct TpmcMatchRule * *entries;
} TpmcMatchRuleArray;

typedef struct TpmcVariableArray {
    Header header;
    int size;
    int capacity;
    HashSymbol * *entries;
} TpmcVariableArray;

typedef struct TpmcPatternArray {
    Header header;
    char *_tag;
    int size;
    int capacity;
    struct TpmcPattern * *entries;
} TpmcPatternArray;

typedef struct TpmcStateArray {
    Header header;
    char *_tag;
    int size;
    int capacity;
    struct TpmcState * *entries;
} TpmcStateArray;

typedef struct TpmcArcArray {
    Header header;
    int size;
    int capacity;
    struct TpmcArc * *entries;
} TpmcArcArray;

typedef struct TpmcIntArray {
    Header header;
    int size;
    int capacity;
    int *entries;
} TpmcIntArray;

typedef struct TpmcMatrix {
    Header header;
    int width;
    int height;
    struct TpmcPattern * *entries;
} TpmcMatrix;

struct TpmcMatchRules * newTpmcMatchRules(struct TpmcMatchRuleArray * rules, struct TpmcVariableArray * rootVariables);
struct TpmcMatchRule * newTpmcMatchRule(struct TpmcState * action, struct TpmcPatternArray * patterns);
struct TpmcComparisonPattern * newTpmcComparisonPattern(struct TpmcPattern * previous, struct TpmcPattern * current);
struct TpmcAssignmentPattern * newTpmcAssignmentPattern(HashSymbol * name, struct TpmcPattern * value);
struct TpmcConstructorPattern * newTpmcConstructorPattern(HashSymbol * tag, LamTypeConstructorInfo * info, struct TpmcPatternArray * components);
struct TpmcPattern * newTpmcPattern(struct TpmcPatternValue * pattern);
struct TpmcTestState * newTpmcTestState(HashSymbol * path, struct TpmcArcArray * arcs);
struct TpmcFinalState * newTpmcFinalState(LamExp * action);
struct TpmcState * newTpmcState(int stamp, struct TpmcStateValue * state);
struct TpmcArc * newTpmcArc(struct TpmcState * state, struct TpmcPattern * test, HashTable * freeVariables);
struct TpmcArcList * newTpmcArcList(struct TpmcArc * arc, struct TpmcArcList * next);
struct TpmcIntList * newTpmcIntList(int integer, struct TpmcIntList * next);
struct TpmcPatternValue * newTpmcPatternValue(enum TpmcPatternValueType  type, union TpmcPatternValueVal  val);
struct TpmcStateValue * newTpmcStateValue(enum TpmcStateValueType  type, union TpmcStateValueVal  val);
struct TpmcMatchRuleArray * newTpmcMatchRuleArray();
struct TpmcVariableArray * newTpmcVariableArray();
struct TpmcPatternArray * newTpmcPatternArray(char * _tag);
struct TpmcStateArray * newTpmcStateArray(char * _tag);
struct TpmcArcArray * newTpmcArcArray();
struct TpmcIntArray * newTpmcIntArray();
struct TpmcMatrix * newTpmcMatrix(int width, int height);

void markTpmcMatchRules(struct TpmcMatchRules * x);
void markTpmcMatchRule(struct TpmcMatchRule * x);
void markTpmcComparisonPattern(struct TpmcComparisonPattern * x);
void markTpmcAssignmentPattern(struct TpmcAssignmentPattern * x);
void markTpmcConstructorPattern(struct TpmcConstructorPattern * x);
void markTpmcPattern(struct TpmcPattern * x);
void markTpmcTestState(struct TpmcTestState * x);
void markTpmcFinalState(struct TpmcFinalState * x);
void markTpmcState(struct TpmcState * x);
void markTpmcArc(struct TpmcArc * x);
void markTpmcArcList(struct TpmcArcList * x);
void markTpmcIntList(struct TpmcIntList * x);
void markTpmcPatternValue(struct TpmcPatternValue * x);
void markTpmcStateValue(struct TpmcStateValue * x);
void markTpmcMatchRuleArray(struct TpmcMatchRuleArray * x);
void markTpmcVariableArray(struct TpmcVariableArray * x);
void markTpmcPatternArray(struct TpmcPatternArray * x);
void markTpmcStateArray(struct TpmcStateArray * x);
void markTpmcArcArray(struct TpmcArcArray * x);
void markTpmcIntArray(struct TpmcIntArray * x);
void markTpmcMatrix(struct TpmcMatrix * x);

void freeTpmcMatchRules(struct TpmcMatchRules * x);
void freeTpmcMatchRule(struct TpmcMatchRule * x);
void freeTpmcComparisonPattern(struct TpmcComparisonPattern * x);
void freeTpmcAssignmentPattern(struct TpmcAssignmentPattern * x);
void freeTpmcConstructorPattern(struct TpmcConstructorPattern * x);
void freeTpmcPattern(struct TpmcPattern * x);
void freeTpmcTestState(struct TpmcTestState * x);
void freeTpmcFinalState(struct TpmcFinalState * x);
void freeTpmcState(struct TpmcState * x);
void freeTpmcArc(struct TpmcArc * x);
void freeTpmcArcList(struct TpmcArcList * x);
void freeTpmcIntList(struct TpmcIntList * x);
void freeTpmcPatternValue(struct TpmcPatternValue * x);
void freeTpmcStateValue(struct TpmcStateValue * x);
void freeTpmcMatchRuleArray(struct TpmcMatchRuleArray * x);
void freeTpmcVariableArray(struct TpmcVariableArray * x);
void freeTpmcPatternArray(struct TpmcPatternArray * x);
void freeTpmcStateArray(struct TpmcStateArray * x);
void freeTpmcArcArray(struct TpmcArcArray * x);
void freeTpmcIntArray(struct TpmcIntArray * x);
void freeTpmcMatrix(struct TpmcMatrix * x);

void pushTpmcMatchRuleArray(struct TpmcMatchRuleArray * obj, struct TpmcMatchRule * entry);
void pushTpmcVariableArray(struct TpmcVariableArray * obj, HashSymbol * entry);
void pushTpmcPatternArray(struct TpmcPatternArray * obj, struct TpmcPattern * entry);
void pushTpmcStateArray(struct TpmcStateArray * obj, struct TpmcState * entry);
void pushTpmcArcArray(struct TpmcArcArray * obj, struct TpmcArc * entry);
void pushTpmcIntArray(struct TpmcIntArray * obj, int entry);

#define TPMCPATTERNVALUE_VAL_VAR(x) ((union TpmcPatternValueVal ){.var = (x)})
#define TPMCPATTERNVALUE_VAL_COMPARISON(x) ((union TpmcPatternValueVal ){.comparison = (x)})
#define TPMCPATTERNVALUE_VAL_ASSIGNMENT(x) ((union TpmcPatternValueVal ){.assignment = (x)})
#define TPMCPATTERNVALUE_VAL_WILDCARD() ((union TpmcPatternValueVal ){.wildcard = (NULL)})
#define TPMCPATTERNVALUE_VAL_CHARACTER(x) ((union TpmcPatternValueVal ){.character = (x)})
#define TPMCPATTERNVALUE_VAL_INTEGER(x) ((union TpmcPatternValueVal ){.integer = (x)})
#define TPMCPATTERNVALUE_VAL_CONSTRUCTOR(x) ((union TpmcPatternValueVal ){.constructor = (x)})
#define TPMCSTATEVALUE_VAL_TEST(x) ((union TpmcStateValueVal ){.test = (x)})
#define TPMCSTATEVALUE_VAL_FINAL(x) ((union TpmcStateValueVal ){.final = (x)})
#define TPMCSTATEVALUE_VAL_ERROR() ((union TpmcStateValueVal ){.error = (NULL)})

static inline struct TpmcPattern * getTpmcMatrixIndex(struct TpmcMatrix * obj, int x, int y) {
    if (x >= obj->width || y >= obj->height || x < 0 || y < 0) {
        cant_happen("2d matrix bounds exceeded");
    }
    return obj->entries[x + y * obj->width];
}

static inline void setTpmcMatrixIndex(struct TpmcMatrix * obj, int x, int y, struct TpmcPattern * val) {
    if (x >= obj->width || y >= obj->height || x < 0 || y < 0) {
        cant_happen("2d matrix bounds exceeded");
    }
    obj->entries[x + y * obj->width] = val;
}

#endif
