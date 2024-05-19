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
#include <stdlib.h>
#include "common.h"
#include "tpmc_logic.h"
#include "tpmc_translate.h"
#include "tpmc.h"
#include "tpmc_debug.h"
#include "tpmc_match.h"
#include "ast_helper.h"
#include "symbol.h"
#include "memory.h"
#include "lambda_substitution.h"
#include "lambda_pp.h"
#include "tpmc_mermaid.h"
#include "tpmc_pp.h"
#include "types.h"
#ifdef DEBUG_TPMC_LOGIC
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static TpmcPattern *convertPattern(AstArg *arg, LamContext *env);

static TpmcVariableArray *createRootVariables(int nargs) {
    ENTER(createRootVariables);
    TpmcVariableArray *rootVariables = newTpmcVariableArray();
    int p = PROTECT(rootVariables);
    for (int i = 0; i < nargs; i++) {
        HashSymbol *s = genSym("p$");
        IFDEBUG(eprintf("%s", s->name));
        pushTpmcVariableArray(rootVariables, s);
    }
    UNPROTECT(p);
    LEAVE(createRootVariables);
    return rootVariables;
}

static TpmcPatternArray *convertArgList(AstArgList *argList, LamContext *env) {
    TpmcPatternArray *patterns = newTpmcPatternArray("convertArgList");
    int save = PROTECT(patterns);
    while (argList != NULL) {
        TpmcPattern *pattern = convertPattern(argList->arg, env);
        int save2 = PROTECT(pattern);
        pushTpmcPatternArray(patterns, pattern);
        UNPROTECT(save2);
        argList = argList->next;
    }
    UNPROTECT(save);
    return patterns;
}

static TpmcPattern *makeWildcardPattern() {
    TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD,
                                               TPMCPATTERNVALUE_VAL_WILDCARD
                                               ());
    int save = PROTECT(wc);
    TpmcPattern *pattern = newTpmcPattern(wc);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeLookupPattern(AstLookupSymbol *lookup, LamContext *env) {
    LamTypeConstructorInfo *info = lookupScopedAstSymbolInLamContext(env, lookup);
    if (info == NULL) {
        cant_happen("makeLookupPattern() passed invalid constructor");
    }
    TpmcPatternArray *args = newTpmcPatternArray("makeLookupPattern");
    int save = PROTECT(args);
    TpmcConstructorPattern *constructor =
        newTpmcConstructorPattern(lookup->symbol, lookup->namespace, info, args);
    PROTECT(constructor);
    TpmcPatternValue *val =
        newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CONSTRUCTOR,
                            TPMCPATTERNVALUE_VAL_CONSTRUCTOR
                            (constructor));
    PROTECT(val);
    TpmcPattern *pattern = newTpmcPattern(val);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeVarPattern(HashSymbol *symbol, LamContext *env) {
    LamTypeConstructorInfo *info = lookupConstructorInLamContext(env, symbol);
    if (info == NULL) {
        TpmcPatternValue *val = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_VAR,
                                                    TPMCPATTERNVALUE_VAL_VAR
                                                    (symbol));
        int save = PROTECT(val);
        TpmcPattern *pattern = newTpmcPattern(val);
        UNPROTECT(save);
        return pattern;
    } else {
        TpmcPatternArray *args = newTpmcPatternArray("makeVarPattern");
        int save = PROTECT(args);
        int namespace = lookupCurrentNamespaceInLamContext(env);
        TpmcConstructorPattern *constructor =
            newTpmcConstructorPattern(symbol, namespace, info, args);
        PROTECT(constructor);
        TpmcPatternValue *val =
            newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CONSTRUCTOR,
                                TPMCPATTERNVALUE_VAL_CONSTRUCTOR
                                (constructor));
        PROTECT(val);
        TpmcPattern *pattern = newTpmcPattern(val);
        UNPROTECT(save);
        return pattern;
    }
}

static TpmcPattern *makeAssignmentPattern(AstNamedArg *named, LamContext *env) {
    TpmcPattern *value = convertPattern(named->arg, env);
    int save = PROTECT(value);
    TpmcAssignmentPattern *assignment =
        newTpmcAssignmentPattern(named->name, value);
    PROTECT(assignment);
    TpmcPatternValue *val =
        newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_ASSIGNMENT,
                            TPMCPATTERNVALUE_VAL_ASSIGNMENT(assignment));
    PROTECT(val);
    TpmcPattern *pattern = newTpmcPattern(val);
    UNPROTECT(save);
    return pattern;
}

static void getSymbolAndNamespace(AstLookupOrSymbol *los, LamContext *env, HashSymbol **name, int *namespace) {
    switch (los->type) {
        case AST_LOOKUPORSYMBOL_TYPE_LOOKUP:
            *name = los->val.lookup->symbol;
            *namespace = los->val.lookup->namespace;
            break;
        case AST_LOOKUPORSYMBOL_TYPE_SYMBOL:{
                *namespace = lookupCurrentNamespaceInLamContext(env);
                *name = los->val.symbol;
            }
            break;
        default:
            cant_happen("unrecognized %s", astLookupOrSymbolTypeName(los->type));
    }
}

static TpmcPattern *makeConstructorPattern(AstUnpack *unpack, LamContext *env) {
    LamTypeConstructorInfo *info = lookupScopedAstConstructorInLamContext(env, unpack->symbol);
    if (info == NULL) {
        cant_happen("makeConstructorPattern() passed invalid constructor");
    }
    TpmcPatternArray *patterns = convertArgList(unpack->argList, env);
    int save = PROTECT(patterns);
    HashSymbol *symbol = NULL;
    int namespace = 0;
    getSymbolAndNamespace(unpack->symbol, env, &symbol, &namespace);
    TpmcConstructorPattern *constructor =
        newTpmcConstructorPattern(symbol, namespace, info, patterns);
    PROTECT(constructor);
    TpmcPatternValue *val =
        newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CONSTRUCTOR,
                            TPMCPATTERNVALUE_VAL_CONSTRUCTOR(constructor));
    PROTECT(val);
    TpmcPattern *pattern = newTpmcPattern(val);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeTuplePattern(AstArgList *args, LamContext *env) {
    TpmcPatternArray *tuple = convertArgList(args, env);
    int save = PROTECT(tuple);
    TpmcPatternValue *val =
        newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_TUPLE,
                            TPMCPATTERNVALUE_VAL_TUPLE(tuple));
    PROTECT(val);
    TpmcPattern *pattern = newTpmcPattern(val);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeMaybeBigIntegerPattern(MaybeBigInt *number) {
    TpmcPatternValue *val =
        newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_BIGINTEGER,
                            TPMCPATTERNVALUE_VAL_BIGINTEGER(number));
    int save = PROTECT(val);
    TpmcPattern *pattern = newTpmcPattern(val);
    UNPROTECT(save);
    return pattern;
}

static TpmcPattern *makeCharacterPattern(char character) {
    TpmcPatternValue *val =
        newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_CHARACTER,
                            TPMCPATTERNVALUE_VAL_CHARACTER(character));
    int save = PROTECT(val);
    TpmcPattern *pattern = newTpmcPattern(val);
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
        case AST_ARG_TYPE_UNPACK:
            return makeConstructorPattern(arg->val.unpack, env);
        case AST_ARG_TYPE_TUPLE:
            return makeTuplePattern(arg->val.tuple, env);
        case AST_ARG_TYPE_NUMBER:
            return makeMaybeBigIntegerPattern(arg->val.number);
        case AST_ARG_TYPE_CHARACTER:
            return makeCharacterPattern(arg->val.character);
        case AST_ARG_TYPE_LOOKUP:
            return makeLookupPattern(arg->val.lookup, env);
        default:
            cant_happen("unrecognized arg type %s in convertPattern",
                        astArgTypeName(arg->type));
    }
}

static TpmcMatchRule *convertSingle(AstArgList *argList, LamExp *action,
                                    LamContext *env) {
    TpmcPatternArray *patterns = convertArgList(argList, env);
    int save = PROTECT(patterns);
    TpmcFinalState *finalState = newTpmcFinalState(action);
    PROTECT(finalState);
    TpmcStateValue *stateVal = newTpmcStateValue(TPMCSTATEVALUE_TYPE_FINAL,
                                                 TPMCSTATEVALUE_VAL_FINAL
                                                 (finalState));
    PROTECT(stateVal);
    TpmcState *state = tpmcMakeState(stateVal);
    PROTECT(state);
    TpmcMatchRule *result = newTpmcMatchRule(state, patterns);
    UNPROTECT(save);
    return result;
}

static TpmcMatchRuleArray *convertComposite(int nbodies,
                                            AstArgList **argLists,
                                            LamExp **actions,
                                            LamContext *env) {
    TpmcMatchRuleArray *result = newTpmcMatchRuleArray();
    int save = PROTECT(result);
    for (int i = 0; i < nbodies; i++) {
        TpmcMatchRule *rule = convertSingle(argLists[i], actions[i], env);
        int save2 = PROTECT(rule);
        pushTpmcMatchRuleArray(result, rule);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return result;
}

static TpmcState *makeErrorState() {
    TpmcStateValue *stateVal = newTpmcStateValue(TPMCSTATEVALUE_TYPE_ERROR,
                                                 TPMCSTATEVALUE_VAL_ERROR());
    int save = PROTECT(stateVal);
    TpmcState *state = tpmcMakeState(stateVal);
    PROTECT(state);
    state->freeVariables = newTpmcVariableTable();
    UNPROTECT(save);
    return state;
}

static void renamePattern(TpmcPattern *pattern, HashSymbol *variable);

static void renameComparisonPattern(TpmcComparisonPattern *pattern,
                                    HashSymbol *path) {
    renamePattern(pattern->current, path);      // previous will already have been named
}

static void renameAssignmentPattern(TpmcAssignmentPattern *pattern,
                                    HashSymbol *path) {
    renamePattern(pattern->value, path);
}

static void renameConstructorPattern(TpmcConstructorPattern *pattern,
                                     HashSymbol *path) {
    TpmcPatternArray *components = pattern->components;
    char buf[512];
    for (Index i = 0; i < components->size; i++) {
        if (snprintf(buf, 512, "%s$%d", path->name, i) >= 511) {
            can_happen("maximum path depth exceeded");
        }
        DEBUG("renameConstructorPattern: %s", buf);
        HashSymbol *newPath = newSymbol(buf);
        renamePattern(components->entries[i], newPath);
    }
}

static void renameTuplePattern(TpmcPatternArray *components,
                                     HashSymbol *path) {
    char buf[512];
    for (Index i = 0; i < components->size; i++) {
        if (snprintf(buf, 512, "%s$%d", path->name, i) >= 511) {
            can_happen("maximum path depth exceeded");
        }
        DEBUG("renameTuplePattern: %s", buf);
        HashSymbol *newPath = newSymbol(buf);
        renamePattern(components->entries[i], newPath);
    }
}

static void renamePattern(TpmcPattern *pattern, HashSymbol *variable) {
    pattern->path = variable;
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            break;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            renameComparisonPattern(pattern->pattern->val.comparison,
                                    variable);
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            renameAssignmentPattern(pattern->pattern->val.assignment,
                                    variable);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            renameConstructorPattern(pattern->pattern->val.constructor,
                                     variable);
            break;
        case TPMCPATTERNVALUE_TYPE_TUPLE:
            renameTuplePattern(pattern->pattern->val.tuple, variable);
            break;
        default:
            cant_happen("unrecognised pattern type %s", tpmcPatternValueTypeName(pattern->pattern->type));
    }
}

static void renameRule(TpmcMatchRule *rule, TpmcVariableArray *rootVariables) {
    if (rule->patterns->size != rootVariables->size) {
        printTpmcMatchRule(rule, 0);
        eprintf("\n");
        printTpmcVariableArray(rootVariables, 0);
        eprintf("\n");
        cant_happen("size mismatch in renameRule");
    }
    for (Index i = 0; i < rootVariables->size; i++) {
        renamePattern(rule->patterns->entries[i], rootVariables->entries[i]);
    }
}

static void renameRules(TpmcMatchRules *input) {
    for (Index i = 0; i < input->rules->size; i++) {
        renameRule(input->rules->entries[i], input->rootVariables);
    }
}

static TpmcPattern *replaceComparisonPattern(TpmcPattern *pattern,
                                             TpmcPatternTable *seen);

static TpmcPattern *replaceVarPattern(TpmcPattern *pattern,
                                      TpmcPatternTable *seen) {
    TpmcPattern *other = NULL;
    if (getTpmcPatternTable(seen, pattern->pattern->val.var, &other)) {
        if (other->pattern->type == TPMCPATTERNVALUE_TYPE_ASSIGNMENT) {
            // FIXME should be possible to allow this? assignments are just variable bindings
            // would be necessary to refine the patternsMatchingPattern algorithm in tpmc_match.c:mixture()
            can_happen("cannot compare assignment (var %s)",
                       pattern->pattern->val.var->name);
        }
        TpmcComparisonPattern *comp =
            newTpmcComparisonPattern(other, pattern);
        int save = PROTECT(comp);
        TpmcPatternValue *val =
            newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_COMPARISON,
                                TPMCPATTERNVALUE_VAL_COMPARISON(comp));
        PROTECT(val);
        TpmcPattern *result = newTpmcPattern(val);
        UNPROTECT(save);
        return result;
    } else {
        setTpmcPatternTable(seen, pattern->pattern->val.var, pattern);
        return pattern;
    }
}

static TpmcPattern *replaceAssignmentPattern(TpmcPattern *pattern,
                                             TpmcPatternTable *seen) {
    TpmcPattern *other = NULL;
    if (getTpmcPatternTable
        (seen, pattern->pattern->val.assignment->name, &other)) {
        can_happen("cannot compare assignment (var %s)",
                   pattern->pattern->val.assignment->name->name);
    } else {
        setTpmcPatternTable(seen, pattern->pattern->val.assignment->name,
                            pattern);
    }
    pattern->pattern->val.assignment->value =
        replaceComparisonPattern(pattern->pattern->val.assignment->value,
                                 seen);
    return pattern;
}

static TpmcPattern *replaceConstructorPattern(TpmcPattern *pattern,
                                              TpmcPatternTable *seen) {
    TpmcPatternArray *components =
        pattern->pattern->val.constructor->components;
    for (Index i = 0; i < components->size; ++i) {
        components->entries[i] =
            replaceComparisonPattern(components->entries[i], seen);
    }
    return pattern;
}

static TpmcPattern *replaceTuplePattern(TpmcPattern *pattern,
                                        TpmcPatternTable *seen) {
    TpmcPatternArray *components = pattern->pattern->val.tuple;
    for (Index i = 0; i < components->size; ++i) {
        components->entries[i] =
            replaceComparisonPattern(components->entries[i], seen);
    }
    return pattern;
}

static TpmcPattern *replaceComparisonPattern(TpmcPattern *pattern,
                                             TpmcPatternTable *seen) {
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            return pattern;
        case TPMCPATTERNVALUE_TYPE_VAR:
            return replaceVarPattern(pattern, seen);
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            return replaceAssignmentPattern(pattern, seen);
        case TPMCPATTERNVALUE_TYPE_TUPLE:
            return replaceTuplePattern(pattern, seen);
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            return replaceConstructorPattern(pattern, seen);
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            cant_happen
                ("encounterted nested comparison pattern during replaceComparisonPattern");
        default:
            cant_happen("unrecognised pattern type %s", tpmcPatternValueTypeName(pattern->pattern->type));
    }
}

static void replaceComparisonRule(TpmcMatchRule *rule) {
    TpmcPatternTable *seen = newTpmcPatternTable();
    int save = PROTECT(seen);
    for (Index i = 0; i < rule->patterns->size; i++) {
        rule->patterns->entries[i] =
            replaceComparisonPattern(rule->patterns->entries[i], seen);
    }
    UNPROTECT(save);
    validateLastAlloc();
}

static void replaceComparisonRules(TpmcMatchRules *input) {
    for (Index i = 0; i < input->rules->size; i++) {
        replaceComparisonRule(input->rules->entries[i]);
    }
}

static TpmcPattern *collectPatternSubstitutions(TpmcPattern *pattern, TpmcSubstitutionTable
                                                *substitutions);

static TpmcPattern *collectVarSubstitutions(TpmcPattern *pattern, TpmcSubstitutionTable
                                            *substitutions) {
    setTpmcSubstitutionTable(substitutions, pattern->pattern->val.var,
                             pattern->path);
    TpmcPatternValue *wc = newTpmcPatternValue(TPMCPATTERNVALUE_TYPE_WILDCARD,
                                               TPMCPATTERNVALUE_VAL_WILDCARD
                                               ());
    pattern->pattern = wc;
    return pattern;
}

static TpmcPattern *collectAssignmentSubstitutions(TpmcPattern *pattern, TpmcSubstitutionTable
                                                   *substitutions) {
    setTpmcSubstitutionTable(substitutions,
                             pattern->pattern->val.assignment->name,
                             pattern->path);
    // we no longer need to remember this is an assignment now we have the substitution
    TpmcPattern *value = pattern->pattern->val.assignment->value;
    return collectPatternSubstitutions(value, substitutions);
}

static TpmcPattern *collectConstructorSubstitutions(TpmcPattern *pattern, TpmcSubstitutionTable
                                                    *substitutions) {
    TpmcPatternArray *components =
        pattern->pattern->val.constructor->components;
    for (Index i = 0; i < components->size; ++i) {
        components->entries[i] =
            collectPatternSubstitutions(components->entries[i],
                                        substitutions);
    }
    return pattern;
}

static TpmcPattern *collectTupleSubstitutions(TpmcPattern *pattern, TpmcSubstitutionTable *substitutions) {
    TpmcPatternArray *components =
        pattern->pattern->val.tuple;
    for (Index i = 0; i < components->size; ++i) {
        components->entries[i] =
            collectPatternSubstitutions(components->entries[i],
                                        substitutions);
    }
    return pattern;
}

static TpmcPattern *collectComparisonSubstitutions(TpmcPattern *pattern, TpmcSubstitutionTable
                                                   *substitutions) {
    TpmcPattern *previous = pattern->pattern->val.comparison->previous;
    pattern->pattern->val.comparison->previous =
        collectPatternSubstitutions(previous, substitutions);
    pattern->pattern->val.comparison->current =
        collectPatternSubstitutions(pattern->pattern->val.comparison->current,
                                    substitutions);
    return pattern;
}

static void performActionSubstitution(TpmcState *state,
                                      TpmcSubstitutionTable *substitutions) {
    if (state->state->type != TPMCSTATEVALUE_TYPE_FINAL) {
        cant_happen
            ("attempt to call performActionSubstitution on non-final state");
    }
    state->state->val.final->action =
        lamPerformSubstitutions(state->state->val.final->action,
                                substitutions);
}

static void populateFreeVariables(TpmcState *state,
                                  TpmcSubstitutionTable *substitutions) {
    if (state->state->type != TPMCSTATEVALUE_TYPE_FINAL) {
        cant_happen
            ("attempt to call populateFreeCariables on non-final state");
    }
    state->freeVariables = newTpmcVariableTable();
    Index i = 0;
    HashSymbol *path = NULL;
    HashSymbol *key;
    while ((key =
            iterateTpmcSubstitutionTable(substitutions, &i, &path)) != NULL) {
        setTpmcVariableTable(state->freeVariables, path);
    }
}

static TpmcPattern *collectPatternSubstitutions(TpmcPattern *pattern, TpmcSubstitutionTable
                                                *substitutions) {
    switch (pattern->pattern->type) {
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            return pattern;
        case TPMCPATTERNVALUE_TYPE_VAR:
            return collectVarSubstitutions(pattern, substitutions);
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            return collectAssignmentSubstitutions(pattern, substitutions);
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            return collectConstructorSubstitutions(pattern, substitutions);
        case TPMCPATTERNVALUE_TYPE_TUPLE:
            return collectTupleSubstitutions(pattern, substitutions);
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            return collectComparisonSubstitutions(pattern, substitutions);
        default:
            cant_happen("unrecognised pattern type %s", tpmcPatternValueTypeName(pattern->pattern->type));
    }
}

static void performRuleSubstitutions(TpmcMatchRule *rule) {
    TpmcSubstitutionTable *substitutions = newTpmcSubstitutionTable();
    int save = PROTECT(substitutions);
    for (Index i = 0; i < rule->patterns->size; i++) {
        rule->patterns->entries[i] =
            collectPatternSubstitutions(rule->patterns->entries[i],
                                        substitutions);
    }
    performActionSubstitution(rule->action, substitutions);
    populateFreeVariables(rule->action, substitutions);
    UNPROTECT(save);
}

static void performRulesSubstitutions(TpmcMatchRules *input) {
    for (Index i = 0; i < input->rules->size; i++) {
        performRuleSubstitutions(input->rules->entries[i]);
    }
}

static void populateMatrixRow(TpmcMatchRule *rule, TpmcMatrix *matrix,
                              int row) {
    for (Index col = 0; col < rule->patterns->size; col++) {
        setTpmcMatrixIndex(matrix, col, row, rule->patterns->entries[col]);
    }
}

static TpmcStateArray *extractFinalStates(TpmcMatchRules *input) {
    TpmcStateArray *res = newTpmcStateArray("extractFinalStates");
    int save = PROTECT(res);
    for (Index i = 0; i < input->rules->size; i++) {
        pushTpmcStateArray(res, input->rules->entries[i]->action);
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
    TpmcMatrix *matrix = newTpmcMatrix(width, height);
    int save = PROTECT(matrix);
    for (int row = 0; row < height; ++row) {
        populateMatrixRow(input->rules->entries[row], matrix, row);
    }
    UNPROTECT(save);
    return matrix;
}

static LamVarList *_arrayToVarList(TpmcVariableArray *array, Index count) {
    if (count == array->size) {
        return NULL;
    }
    LamVarList *next = _arrayToVarList(array, count + 1);
    int save = PROTECT(next);
    LamVarList *this = newLamVarList(array->entries[count], next);
    UNPROTECT(save);
    return this;
}

static LamVarList *arrayToVarList(TpmcVariableArray *array) {
    return _arrayToVarList(array, 0);
}

LamLam *tpmcConvert(int nargs, int nbodies, AstArgList **argLists,
                    LamExp **actions, LamContext *env) {
    TpmcVariableArray *rootVariables = createRootVariables(nargs);
    int save = PROTECT(rootVariables);
    TpmcMatchRuleArray *rules =
        convertComposite(nbodies, argLists, actions, env);
    PROTECT(rules);
    TpmcMatchRules *input = newTpmcMatchRules(rules, rootVariables);
    REPLACE_PROTECT(save, input);
    replaceComparisonRules(input);
    renameRules(input);
    performRulesSubstitutions(input);
    // DEBUG("*** RULES ***");
    // IFDEBUG(printTpmcMatchRules(input, 0));
    TpmcMatrix *matrix = convertToMatrix(input);
    PROTECT(matrix);
    DEBUG("*** MATRIX ***");
    IFDEBUG(ppTpmcMatrix(matrix));
    TpmcStateArray *finalStates = extractFinalStates(input);
    PROTECT(finalStates);
    TpmcStateArray *knownStates = newTpmcStateArray("tpmcConvert");
    PROTECT(knownStates);
    for (Index i = 0; i < finalStates->size; ++i) {
        pushTpmcStateArray(knownStates, finalStates->entries[i]);
    }
    TpmcState *errorState = makeErrorState();
    PROTECT(errorState);
    TpmcState *dfa = tpmcMatch(matrix, finalStates, errorState, knownStates);
    PROTECT(dfa);
    // DEBUG("*** DFA ***");
    // IFDEBUG(printTpmcState(dfa, 0));
    tpmcMermaid(dfa);
    LamExp *body = tpmcTranslate(dfa);
    PROTECT(body);
    // DEBUG("tpmcTranslate returned %p", body);
    LamVarList *args = arrayToVarList(rootVariables);
    PROTECT(args);
    LamLam *res = newLamLam(args, body);
    PROTECT(res);
#ifdef DEBUG_TPMC_LOGIC
    LamExp *tmp = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(res));
    PROTECT(tmp);
#endif
    DEBUG("*** BODY ***");
    IFDEBUG(ppLamExp(tmp));
    IFDEBUG(validateLastAlloc());
    UNPROTECT(save);
    return res;
}
