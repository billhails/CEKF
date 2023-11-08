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
 * Term Pattern Matching Compiler logic
 */

#include <stdio.h>
#include "common.h"
#include "tpmc_logic.h"
#include "tpmc.h"
#include "debug_tpmc.h"
#include "tpmc_match.h"
#include "ast_helper.h"
#include "symbol.h"
#include "memory.h"
#include "lambda_conversion.h"

static TpmcPattern *convertPattern(AstArg *arg, LamContext *env);

static TpmcPattern *makePattern(TpmcPatternValue *val) {
    return newTpmcPattern(val);
}

static TpmcVariableArray *createRootVariables(int nargs) {
    TpmcVariableArray *rootVariables = newTpmcVariableArray();
    int p = PROTECT(rootVariables);
    for (int i = 0; i < nargs; i++) {
        HashSymbol *s = genSym("p$");
        rootVariables = pushTpmcVariableArray(rootVariables, s);
        REPLACE_PROTECT(p, rootVariables);
    }
    UNPROTECT(p);
    return rootVariables;
}

static TpmcPatternArray *convertArgList(AstArgList *argList, LamContext *env) {
    TpmcPatternArray *patterns = newTpmcPatternArray();
    int save = PROTECT(patterns);
    while (argList != NULL) {
        TpmcPattern *pattern = convertPattern(argList->arg, env);
        int save2 = PROTECT(pattern);
        patterns = pushTpmcPatternArray(patterns, pattern);
        REPLACE_PROTECT(save, patterns);
        UNPROTECT(save2);
        argList = argList->next;
    }
    UNPROTECT(save);
    return patterns;
}

static TpmcPattern *makeWildcardPattern() {
    TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD, TPMCPATTERNVALUE_VAL_WILDCARD());
    int save = PROTECT(wc);
    TpmcPattern *pattern = makePattern(wc);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeVarPattern(HashSymbol *symbol, LamContext *env) {
    LamTypeConstructorInfo *info = lookupInLamContext(env, symbol);
    if (info == NULL) {
        printf("makeVarPattern %s is var\n", symbol->name);
        TpmcVarPattern *var = newTpmcVarPattern(symbol);
        int save = PROTECT(var);
        TpmcPatternValue *val = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_VAR, TPMCPATTERNVALUE_VAL_VAR(var));
        PROTECT(val);
        TpmcPattern *pattern = makePattern(val);
        UNPROTECT(save);
        return pattern;
    } else {
        printf("makeVarPattern %s is constructor\n", symbol->name);
        TpmcPatternArray *args = newTpmcPatternArray();
        int save = PROTECT(args);
        TpmcConstructorPattern *constructor = newTpmcConstructorPattern(symbol, info, args);
        PROTECT(constructor);
        TpmcPatternValue *val = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CONSTRUCTOR, TPMCPATTERNVALUE_VAL_CONSTRUCTOR(constructor));
        PROTECT(val);
        TpmcPattern *pattern = makePattern(val);
        UNPROTECT(save);
        return pattern;
    }
}

static TpmcPattern *makeAssignmentPattern(AstNamedArg *named, LamContext *env) {
    TpmcPattern *value = convertPattern(named->arg, env);
    int save = PROTECT(value);
    TpmcAssignmentPattern *assignment = newTpmcAssignmentPattern(named->name, value);
    PROTECT(assignment);
    TpmcPatternValue *val = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_ASSIGNMENT, TPMCPATTERNVALUE_VAL_ASSIGNMENT(assignment));
    PROTECT(val);
    TpmcPattern *pattern = makePattern(val);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeConstructorPattern(AstUnpack *unpack, LamContext *env) {
    LamTypeConstructorInfo *info = lookupInLamContext(env, unpack->symbol);
    if (info == NULL) {
        cant_happen("makeConstructorPattern() passed invalid constructor: %s", unpack->symbol->name);
    }
    TpmcPatternArray *patterns = convertArgList(unpack->argList, env);
    int save = PROTECT(patterns);
    TpmcConstructorPattern *constructor = newTpmcConstructorPattern(unpack->symbol, info,  patterns);
    PROTECT(constructor);
    TpmcPatternValue *val = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CONSTRUCTOR, TPMCPATTERNVALUE_VAL_CONSTRUCTOR(constructor));
    PROTECT(val);
    TpmcPattern *pattern = makePattern(val);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeIntegerPattern(int number) {
    TpmcPatternValue *val = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_INTEGER, TPMCPATTERNVALUE_VAL_INTEGER(number));
    int save = PROTECT(val);
    TpmcPattern *pattern = makePattern(val);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeCharacterPattern(char character) {
    TpmcPatternValue *val = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CHARACTER, TPMCPATTERNVALUE_VAL_CHARACTER(character));
    int save = PROTECT(val);
    TpmcPattern *pattern = makePattern(val);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *convertPattern(AstArg *arg, LamContext *env) {
    switch (arg->type) {
        case AST_ARG_TYPE_WILDCARD:
            return makeWildcardPattern();
        case AST_ARG_TYPE_SYMBOL:
            return makeVarPattern(arg->val.symbol, env);
        case AST_ARG_TYPE_NAMED:
            return makeAssignmentPattern(arg->val.named, env);
        case AST_ARG_TYPE_ENV:
            cant_happen("env arg type not supported yet in convertPattern");
        case AST_ARG_TYPE_UNPACK:
            return makeConstructorPattern(arg->val.unpack, env);
        case AST_ARG_TYPE_NUMBER:
            return makeIntegerPattern(arg->val.number);
        case AST_ARG_TYPE_CHARACTER:
            return makeCharacterPattern(arg->val.character);
        default:
            cant_happen("unrecognized arg type %d in convertPattern", arg->type);
    }
}

static TpmcMatchRule *convertSingle(AstArgList *argList, LamExp *action, LamContext *env) {
    TpmcPatternArray *patterns = convertArgList(argList, env);
    int save = PROTECT(patterns);
    TpmcFinalState *finalState = newTpmcFinalState(action);
    PROTECT(finalState);
    TpmcStateValue *stateVal = newTpmcStateValue(TPMCSTATEVALUE_TYPE_FINAL, TPMCSTATEVALUE_VAL_FINAL(finalState));
    PROTECT(stateVal);
    TpmcState *state = tpmcMakeState(stateVal);
    PROTECT(state);
    TpmcMatchRule *result = newTpmcMatchRule(state, patterns);
    UNPROTECT(save);
    return result;
}

static TpmcMatchRuleArray *convertComposite(int nbodies, AstArgList **argLists, LamExp **actions, LamContext *env) {
    TpmcMatchRuleArray *result = newTpmcMatchRuleArray();
    int save = PROTECT(result);
    for (int i = 0; i < nbodies; i++) {
        TpmcMatchRule *rule = convertSingle(argLists[i], actions[i], env);
        int save2 = PROTECT(rule);
        result = pushTpmcMatchRuleArray(result, rule);
        REPLACE_PROTECT(save, result);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return result;
}

static TpmcState *makeErrorState() {
    TpmcStateValue *stateVal = newTpmcStateValue(TPMCSTATEVALUE_TYPE_ERROR, TPMCSTATEVALUE_VAL_ERROR());
    int save = PROTECT(stateVal);
    TpmcState *state = tpmcMakeState(stateVal);
    UNPROTECT(save);
    return state;
}

static void renamePattern(TpmcPattern *pattern, HashSymbol *variable);

static void renameComparisonPattern(TpmcComparisonPattern *pattern, HashSymbol *path) {
    renamePattern(pattern->current, path); // previous will already have been named
}

static void renameAssignmentPattern(TpmcAssignmentPattern *pattern, HashSymbol *path) {
    renamePattern(pattern->value, path);
}

static void renameConstructorPattern(TpmcConstructorPattern *pattern, HashSymbol *path) {
    TpmcPatternArray *components = pattern->components;
    char buf[512];
    for (int i = 0; i < components->size; i++) {
        if (snprintf(buf, 512, "%s$%d", path->name, i) >= 511) {
            can_happen("maximum path depth exceeded");
        }
        HashSymbol *newPath = newSymbol(buf);
        renamePattern(components->entries[i], newPath);
    }
}

static void renamePattern(TpmcPattern *pattern, HashSymbol *variable) {
    pattern->path = variable;
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
        case TPMCPATTERNVALUE_TYPE_INTEGER:
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            break;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            renameComparisonPattern(pattern->pattern->val.comparison, variable);
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            renameAssignmentPattern(pattern->pattern->val.assignment, variable);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            renameConstructorPattern(pattern->pattern->val.constructor, variable);
            break;
        default:
            cant_happen("unrecognised pattern type in renamePattern");
    }
}

static void renameRule(TpmcMatchRule *rule, TpmcVariableArray *rootVariables) {
    if (rule->patterns->size != rootVariables->size) {
        cant_happen("size mismatch in renameRule");
    }
    for (int i = 0; i < rootVariables->size; i++) {
        renamePattern(rule->patterns->entries[i], rootVariables->entries[i]);
    }
}

static void renameRules(TpmcMatchRules *input) {
    for (int i = 0; i < input->rules->size; i++) {
        renameRule(input->rules->entries[i], input->rootVariables);
    }
}

static TpmcPattern *replaceComparisonPattern(TpmcPattern *pattern, HashTable *seen);

static TpmcPattern *replaceVarPattern(TpmcPattern *pattern, HashTable *seen) {
    TpmcPattern *other = NULL;
    if (hashGet(seen, pattern->pattern->val.var->name, &other)) {
        if (other->pattern->type == TPMCPATTERNVALUE_TYPE_ASSIGNMENT) {
            can_happen("cannot compare assignment (var %s)", pattern->pattern->val.var->name->name);
        }
        TpmcComparisonPattern *comp = newTpmcComparisonPattern(other, pattern);
        int save = PROTECT(comp);
        TpmcPatternValue *val = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_COMPARISON, TPMCPATTERNVALUE_VAL_COMPARISON(comp));
        PROTECT(val);
        TpmcPattern *result = makePattern(val);
        UNPROTECT(save);
        return result;
    } else {
        hashSet(seen, pattern->pattern->val.var->name, &pattern);
        return pattern;
    }
}

static TpmcPattern *replaceAssignmentPattern(TpmcPattern *pattern, HashTable *seen) {
    TpmcPattern *other = NULL;
    if (hashGet(seen, pattern->pattern->val.assignment->name, &other)) {
        can_happen("cannot compare assignment (var %s)", pattern->pattern->val.assignment->name->name);
    } else {
        hashSet(seen, pattern->pattern->val.assignment->name, &pattern);
    }
    pattern->pattern->val.assignment->value = replaceComparisonPattern(pattern->pattern->val.assignment->value, seen);
    return pattern;
}

static TpmcPattern *replaceConstructorPattern(TpmcPattern *pattern, HashTable *seen) {
    TpmcPatternArray *components = pattern->pattern->val.constructor->components;
    for (int i = 0; i < components->size; ++i) {
        components->entries[i] = replaceComparisonPattern(components->entries[i], seen);
    }
    return pattern;
}

static void printHashTablePattern(void *p, int d) {
    printTpmcPattern((TpmcPattern *)p, d);
}

static void printHashTableSymbol(void *p, int d) {
    printAstSymbol((HashSymbol *)p, d);
}

static TpmcPattern *replaceComparisonPattern(TpmcPattern *pattern, HashTable *seen) {
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_INTEGER:
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            return pattern;
        case TPMCPATTERNVALUE_TYPE_VAR:
            return replaceVarPattern(pattern, seen);
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            return replaceAssignmentPattern(pattern, seen);
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            return replaceConstructorPattern(pattern, seen);
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            cant_happen("encounterted comparison pattern during replaceComparisonPattern");
        default:
            cant_happen("unrecognised pattern type in renamePattern");
    }
}

static void replaceComparisonRule(TpmcMatchRule *rule) {
    HashTable *seen = newHashTable(sizeof(TpmcPattern *), NULL, printHashTablePattern);
    int save = PROTECT(seen);
    for (int i = 0; i < rule->patterns->size; i++) {
        rule->patterns->entries[i] = replaceComparisonPattern(rule->patterns->entries[i], seen);
    }
    UNPROTECT(save);
}

static void replaceComparisonRules(TpmcMatchRules *input) {
    for (int i = 0; i < input->rules->size; i++) {
        replaceComparisonRule(input->rules->entries[i]);
    }
}

static TpmcPattern *collectPatternSubstitutions(TpmcPattern *pattern, HashTable *substitutions);

static TpmcPattern *collectVarSubstitutions(TpmcPattern *pattern, HashTable *substitutions) {
    hashSet(substitutions, pattern->pattern->val.var->name, &(pattern->path));
    TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD, TPMCPATTERNVALUE_VAL_WILDCARD());
    pattern->pattern = wc;
    return pattern;
}

static TpmcPattern *collectAssignmentSubstitutions(TpmcPattern *pattern, HashTable *substitutions) {
    hashSet(substitutions, pattern->pattern->val.assignment->name, &(pattern->path));
    // we no longer need to remember this is an assignment now we have the substitution
    return collectPatternSubstitutions(pattern->pattern->val.assignment->value, substitutions);
}

static TpmcPattern *collectConstructorSubstitutions(TpmcPattern *pattern, HashTable *substitutions) {
    TpmcPatternArray *components = pattern->pattern->val.constructor->components;
    for (int i = 0; i < components->size; ++i) {
        components->entries[i] = collectPatternSubstitutions(components->entries[i], substitutions);
    }
    return pattern;
}

static TpmcPattern *collectComparisonSubstitutions(TpmcPattern *pattern, HashTable *substitutions) {
    pattern->pattern->val.comparison->previous = collectPatternSubstitutions(pattern->pattern->val.comparison->previous, substitutions);
    pattern->pattern->val.comparison->current = collectPatternSubstitutions(pattern->pattern->val.comparison->current, substitutions);
    return pattern;
}

static void performActionSubstitution(TpmcState *action, HashTable *substitutions) {
    if (action->state->type != TPMCSTATEVALUE_TYPE_FINAL) {
        cant_happen("attempt to call performActionSubstitution on non-final state");
    }
    action->state->val.final->action = lamPerformSubstitutions(action->state->val.final->action, substitutions);
}

static TpmcPattern *collectPatternSubstitutions(TpmcPattern *pattern, HashTable *substitutions) {
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_INTEGER:
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            return pattern;
        case TPMCPATTERNVALUE_TYPE_VAR:
            return collectVarSubstitutions(pattern, substitutions);
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            return collectAssignmentSubstitutions(pattern, substitutions);
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            return collectConstructorSubstitutions(pattern, substitutions);
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            return collectComparisonSubstitutions(pattern, substitutions);
        default:
            cant_happen("unrecognised pattern type in renamePattern");
    }
}

static void performRuleSubstitutions(TpmcMatchRule *rule) {
    HashTable *substitutions = newHashTable(sizeof(HashSymbol *), NULL, printHashTableSymbol);
    int save = PROTECT(substitutions);
    for (int i = 0; i < rule->patterns->size; i++) {
        rule->patterns->entries[i] = collectPatternSubstitutions(rule->patterns->entries[i], substitutions);
    }
    performActionSubstitution(rule->action, substitutions);
    UNPROTECT(save);
}

static void performRulesSubstitutions(TpmcMatchRules *input) {
    for (int i = 0; i < input->rules->size; i++) {
        performRuleSubstitutions(input->rules->entries[i]);
    }
}

static void populateMatrixRow(TpmcMatchRule *rule, TpmcMatrix *matrix, int row) {
    for (int col = 0; col < rule->patterns->size; col++) {
        TpmcMatrixIndex(matrix, col, row) = rule->patterns->entries[col];
    }
}

static TpmcStateArray *extractFinalStates(TpmcMatchRules *input) {
    TpmcStateArray *res = newTpmcStateArray();
    int save = PROTECT(res);
    for (int i = 0; i < input->rules->size; i++) {
        res = pushTpmcStateArray(res, input->rules->entries[i]->action);
        REPLACE_PROTECT(save, res);
    }
    UNPROTECT(save);
    return res;
}

static TpmcMatrix *convertToMatrix(TpmcMatchRules *input) {
    int height = input->rules->size;
    if (height == 0) {
        cant_happen("zero height matrix");
    }
    int width = input->rules->entries[0]->patterns->size;
    if (width == 0) {
        cant_happen("zero width matrix");
    }
    TpmcMatrix *matrix = newTpmcMatrix(width, height);
    int save = PROTECT(matrix);
    for (int row = 0; row < height; ++row) {
        populateMatrixRow(input->rules->entries[row], matrix, row);
    }
    UNPROTECT(save);
    return matrix;
}

LamLam * tpmcConvert(int nargs, int nbodies, AstArgList ** argLists, LamExp ** actions, LamContext * env) {
    TpmcVariableArray *rootVariables = createRootVariables(nargs);
    int save = PROTECT(rootVariables);
    TpmcMatchRuleArray *rules = convertComposite(nbodies, argLists, actions, env);
    PROTECT(rules);
    TpmcMatchRules *input = newTpmcMatchRules(rules, rootVariables);
    UNPROTECT(save);
    save = PROTECT(input);
    replaceComparisonRules(input);
    renameRules(input);
    performRulesSubstitutions(input);
    TpmcMatrix *initialMatrix = convertToMatrix(input);
    PROTECT(initialMatrix);
    TpmcStateArray *finalStates = extractFinalStates(input);
    PROTECT(finalStates);
    TpmcStateArray *knownStates = newTpmcStateArray();
    int save2 = PROTECT(knownStates);
    TpmcStateArrayContainer *knownStatesContainer = newTpmcStateArrayContainer(knownStates);
    REPLACE_PROTECT(save2, knownStatesContainer);
    for (int i = 0; i < finalStates->size; ++i) {
        knownStatesContainer->array = pushTpmcStateArray(knownStatesContainer->array, finalStates->entries[i]);
    }
    // printTpmcMatchRules(input, 0);
    TpmcState *errorState = makeErrorState();
    PROTECT(errorState);
    TpmcState *dfa = tpmcMatch(initialMatrix, finalStates, errorState, knownStatesContainer);
    PROTECT(dfa);
    printTpmcState(dfa, 0);
    return NULL;
}
