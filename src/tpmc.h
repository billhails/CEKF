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
#include "lambda.h"

typedef enum TpmcPatternType {
    TPMCPATTERN_TYPE_VAR,
    TPMCPATTERN_TYPE_COMPARISON,
    TPMCPATTERN_TYPE_ASSIGNMENT,
    TPMCPATTERN_TYPE_WILDCARD,
    TPMCPATTERN_TYPE_CONSTANT,
    TPMCPATTERN_TYPE_CONSTRUCTOR,
} TpmcPatternType;

typedef enum TpmcStateType {
    TPMCSTATE_TYPE_TEST,
    TPMCSTATE_TYPE_FINAL,
    TPMCSTATE_TYPE_ERROR,
} TpmcStateType;



typedef union TpmcPatternVal {
    struct TpmcVarPattern * var;
    struct TpmcComparisonPattern * comparison;
    struct TpmcAssignmentPattern * assignment;
    struct TpmcWildcardPattern * wildcard;
    struct TpmcConstantPattern * constant;
    struct TpmcConstructorPattern * constructor;
} TpmcPatternVal;

typedef union TpmcStateVal {
    struct TpmcTestState * test;
    struct TpmcFinalState * final;
    void * error;
} TpmcStateVal;



typedef struct TpmcMatchRules {
    Header header;
    int id;
    struct TpmcMatchRuleArray * rules;
    struct TpmcVariableArray * rootVariables;
    struct TpmcState * errorState;
    struct TpmcStateArray * knownStates;
} TpmcMatchRules;

typedef struct TpmcMatchRule {
    Header header;
    struct TpmcState * action;
    struct TpmcPatternArray * patterns;
} TpmcMatchRule;

typedef struct TpmcVarPattern {
    Header header;
    HashSymbol * path;
    HashSymbol * name;
} TpmcVarPattern;

typedef struct TpmcComparisonPattern {
    Header header;
    HashSymbol * path;
    struct TpmcPattern * previous;
    struct TpmcPattern * current;
} TpmcComparisonPattern;

typedef struct TpmcAssignmentPattern {
    Header header;
    HashSymbol * path;
    HashSymbol * name;
    struct TpmcPattern * value;
} TpmcAssignmentPattern;

typedef struct TpmcWildcardPattern {
    Header header;
    HashSymbol * path;
} TpmcWildcardPattern;

typedef struct TpmcConstantPattern {
    Header header;
    HashSymbol * path;
    LamExp * value;
} TpmcConstantPattern;

typedef struct TpmcConstructorPattern {
    Header header;
    HashSymbol * path;
    HashSymbol * tag;
    struct TpmcPatternArray * components;
} TpmcConstructorPattern;

typedef struct TpmcTestState {
    Header header;
    HashSymbol * path;
    struct TpmcArcArray * arcs;
} TpmcTestState;

typedef struct TpmcFinalState {
    Header header;
    LamExp * action;
} TpmcFinalState;

typedef struct TpmcArc {
    Header header;
    struct TpmcState * state;
    struct TpmcPattern * test;
} TpmcArc;

typedef struct TpmcPattern {
    Header header;
    enum TpmcPatternType  type;
    union TpmcPatternVal  val;
} TpmcPattern;

typedef struct TpmcState {
    Header header;
    enum TpmcStateType  type;
    union TpmcStateVal  val;
} TpmcState;



typedef struct TpmcMatchRuleArray {
    Header header;
    int size;
    int capacity;
    struct TpmcMatchRule * entries[0];
} TpmcMatchRuleArray;

typedef struct TpmcVariableArray {
    Header header;
    int size;
    int capacity;
    HashSymbol * entries[0];
} TpmcVariableArray;

typedef struct TpmcStateArray {
    Header header;
    int size;
    int capacity;
    struct TpmcState * entries[0];
} TpmcStateArray;

typedef struct TpmcPatternArray {
    Header header;
    int size;
    int capacity;
    struct TpmcPattern * entries[0];
} TpmcPatternArray;

typedef struct TpmcArcArray {
    Header header;
    int size;
    int capacity;
    struct TpmcArc * entries[0];
} TpmcArcArray;

typedef struct TpmcMatrix {
    Header header;
    int x;
    int y;
    struct TpmcPattern * entries[0];
} TpmcMatrix;

struct TpmcMatchRules * newTpmcMatchRules(int id, struct TpmcMatchRuleArray * rules, struct TpmcVariableArray * rootVariables, struct TpmcState * errorState, struct TpmcStateArray * knownStates);
struct TpmcMatchRule * newTpmcMatchRule(struct TpmcState * action, struct TpmcPatternArray * patterns);
struct TpmcVarPattern * newTpmcVarPattern(HashSymbol * path, HashSymbol * name);
struct TpmcComparisonPattern * newTpmcComparisonPattern(HashSymbol * path, struct TpmcPattern * previous, struct TpmcPattern * current);
struct TpmcAssignmentPattern * newTpmcAssignmentPattern(HashSymbol * path, HashSymbol * name, struct TpmcPattern * value);
struct TpmcWildcardPattern * newTpmcWildcardPattern(HashSymbol * path);
struct TpmcConstantPattern * newTpmcConstantPattern(HashSymbol * path, LamExp * value);
struct TpmcConstructorPattern * newTpmcConstructorPattern(HashSymbol * path, HashSymbol * tag, struct TpmcPatternArray * components);
struct TpmcTestState * newTpmcTestState(HashSymbol * path, struct TpmcArcArray * arcs);
struct TpmcFinalState * newTpmcFinalState(LamExp * action);
struct TpmcArc * newTpmcArc(struct TpmcState * state, struct TpmcPattern * test);
struct TpmcPattern * newTpmcPattern(enum TpmcPatternType  type, union TpmcPatternVal  val);
struct TpmcState * newTpmcState(enum TpmcStateType  type, union TpmcStateVal  val);
struct TpmcMatchRuleArray * newTpmcMatchRuleArray();
struct TpmcVariableArray * newTpmcVariableArray();
struct TpmcStateArray * newTpmcStateArray();
struct TpmcPatternArray * newTpmcPatternArray();
struct TpmcArcArray * newTpmcArcArray();
struct TpmcMatrix * newTpmcMatrix(int x, int y);

void markTpmcMatchRules(struct TpmcMatchRules * x);
void markTpmcMatchRule(struct TpmcMatchRule * x);
void markTpmcVarPattern(struct TpmcVarPattern * x);
void markTpmcComparisonPattern(struct TpmcComparisonPattern * x);
void markTpmcAssignmentPattern(struct TpmcAssignmentPattern * x);
void markTpmcWildcardPattern(struct TpmcWildcardPattern * x);
void markTpmcConstantPattern(struct TpmcConstantPattern * x);
void markTpmcConstructorPattern(struct TpmcConstructorPattern * x);
void markTpmcTestState(struct TpmcTestState * x);
void markTpmcFinalState(struct TpmcFinalState * x);
void markTpmcArc(struct TpmcArc * x);
void markTpmcPattern(struct TpmcPattern * x);
void markTpmcState(struct TpmcState * x);
void markTpmcMatchRuleArray(struct TpmcMatchRuleArray * x);
void markTpmcVariableArray(struct TpmcVariableArray * x);
void markTpmcStateArray(struct TpmcStateArray * x);
void markTpmcPatternArray(struct TpmcPatternArray * x);
void markTpmcArcArray(struct TpmcArcArray * x);
void markTpmcMatrix(struct TpmcMatrix * x);

void freeTpmcMatchRules(struct TpmcMatchRules * x);
void freeTpmcMatchRule(struct TpmcMatchRule * x);
void freeTpmcVarPattern(struct TpmcVarPattern * x);
void freeTpmcComparisonPattern(struct TpmcComparisonPattern * x);
void freeTpmcAssignmentPattern(struct TpmcAssignmentPattern * x);
void freeTpmcWildcardPattern(struct TpmcWildcardPattern * x);
void freeTpmcConstantPattern(struct TpmcConstantPattern * x);
void freeTpmcConstructorPattern(struct TpmcConstructorPattern * x);
void freeTpmcTestState(struct TpmcTestState * x);
void freeTpmcFinalState(struct TpmcFinalState * x);
void freeTpmcArc(struct TpmcArc * x);
void freeTpmcPattern(struct TpmcPattern * x);
void freeTpmcState(struct TpmcState * x);
void freeTpmcMatchRuleArray(struct TpmcMatchRuleArray * x);
void freeTpmcVariableArray(struct TpmcVariableArray * x);
void freeTpmcStateArray(struct TpmcStateArray * x);
void freeTpmcPatternArray(struct TpmcPatternArray * x);
void freeTpmcArcArray(struct TpmcArcArray * x);
void freeTpmcMatrix(struct TpmcMatrix * x);

struct TpmcMatchRuleArray * pushTpmcMatchRuleArray(struct TpmcMatchRuleArray * old, struct TpmcMatchRule * entry);
struct TpmcVariableArray * pushTpmcVariableArray(struct TpmcVariableArray * old, HashSymbol * entry);
struct TpmcStateArray * pushTpmcStateArray(struct TpmcStateArray * old, struct TpmcState * entry);
struct TpmcPatternArray * pushTpmcPatternArray(struct TpmcPatternArray * old, struct TpmcPattern * entry);
struct TpmcArcArray * pushTpmcArcArray(struct TpmcArcArray * old, struct TpmcArc * entry);

#define TPMCPATTERN_VAL_VAR(x) ((union TpmcPatternVal ){.var = (x)})
#define TPMCPATTERN_VAL_COMPARISON(x) ((union TpmcPatternVal ){.comparison = (x)})
#define TPMCPATTERN_VAL_ASSIGNMENT(x) ((union TpmcPatternVal ){.assignment = (x)})
#define TPMCPATTERN_VAL_WILDCARD(x) ((union TpmcPatternVal ){.wildcard = (x)})
#define TPMCPATTERN_VAL_CONSTANT(x) ((union TpmcPatternVal ){.constant = (x)})
#define TPMCPATTERN_VAL_CONSTRUCTOR(x) ((union TpmcPatternVal ){.constructor = (x)})
#define TPMCSTATE_VAL_TEST(x) ((union TpmcStateVal ){.test = (x)})
#define TPMCSTATE_VAL_FINAL(x) ((union TpmcStateVal ){.final = (x)})
#define TPMCSTATE_VAL_ERROR() ((union TpmcStateVal ){.error = (NULL)})

#endif
