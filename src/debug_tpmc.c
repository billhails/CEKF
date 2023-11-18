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

#include <stdio.h>

#include "debug_tpmc.h"
#include "lambda_pp.h"

static void pad(int depth) { fprintf(stderr, "%*s", depth * 4, ""); }

void printTpmcMatchRules(struct TpmcMatchRules * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcMatchRules (NULL)"); return; }
    fprintf(stderr, "TpmcMatchRules[\n");
    printTpmcMatchRuleArray(x->rules, depth+1);
    fprintf(stderr, "\n");
    printTpmcVariableArray(x->rootVariables, depth+1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcMatchRule(struct TpmcMatchRule * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcMatchRule (NULL)"); return; }
    fprintf(stderr, "TpmcMatchRule[\n");
    printTpmcState(x->action, depth + 1);
    fprintf(stderr, "\n");
    printTpmcPatternArray(x->patterns, depth+1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcComparisonPattern(struct TpmcComparisonPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcComparisonPattern (NULL)"); return; }
    fprintf(stderr, "TpmcComparisonPattern[\n");
    printTpmcPattern(x->previous, depth + 1);
    fprintf(stderr, "\n");
    printTpmcPattern(x->current, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcAssignmentPattern(struct TpmcAssignmentPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcAssignmentPattern (NULL)"); return; }
    fprintf(stderr, "TpmcAssignmentPattern[\n");
        printAstSymbol(x->name, depth + 1);
    fprintf(stderr, "\n");
    printTpmcPattern(x->value, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcConstructorPattern(struct TpmcConstructorPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcConstructorPattern (NULL)"); return; }
    fprintf(stderr, "TpmcConstructorPattern[\n");
        printAstSymbol(x->tag, depth + 1);
    fprintf(stderr, "\n");
        printLamTypeConstructorInfo(x->info, depth + 1);
    fprintf(stderr, "\n");
    printTpmcPatternArray(x->components, depth+1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcPattern(struct TpmcPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcPattern (NULL)"); return; }
    fprintf(stderr, "TpmcPattern[\n");
        printAstSymbol(x->path, depth + 1);
    fprintf(stderr, "\n");
    printTpmcPatternValue(x->pattern, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcTestState(struct TpmcTestState * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcTestState (NULL)"); return; }
    fprintf(stderr, "TpmcTestState[\n");
        printAstSymbol(x->path, depth + 1);
    fprintf(stderr, "\n");
    printTpmcArcArray(x->arcs, depth+1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcFinalState(struct TpmcFinalState * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcFinalState (NULL)"); return; }
    fprintf(stderr, "TpmcFinalState[\n");
        ppLamExpD(x->action, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcState(struct TpmcState * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcState (NULL)"); return; }
    fprintf(stderr, "TpmcState[\n");
        pad(depth + 1);
fprintf(stderr, "int %d", x->refcount);
    fprintf(stderr, "\n");
        pad(depth + 1);
fprintf(stderr, "int %d", x->stamp);
    fprintf(stderr, "\n");
        printHashTable(x->freeVariables, depth + 1);
    fprintf(stderr, "\n");
    printTpmcStateValue(x->state, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcArc(struct TpmcArc * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcArc (NULL)"); return; }
    fprintf(stderr, "TpmcArc[\n");
    printTpmcState(x->state, depth + 1);
    fprintf(stderr, "\n");
    printTpmcPattern(x->test, depth + 1);
    fprintf(stderr, "\n");
        printHashTable(x->freeVariables, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcArcList(struct TpmcArcList * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcArcList (NULL)"); return; }
    fprintf(stderr, "TpmcArcList[\n");
    printTpmcArc(x->arc, depth + 1);
    fprintf(stderr, "\n");
    printTpmcArcList(x->next, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcIntList(struct TpmcIntList * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcIntList (NULL)"); return; }
    fprintf(stderr, "TpmcIntList[\n");
        pad(depth + 1);
fprintf(stderr, "int %d", x->integer);
    fprintf(stderr, "\n");
    printTpmcIntList(x->next, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcPatternValue(struct TpmcPatternValue * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcPatternValue (NULL)"); return; }
    fprintf(stderr, "TpmcPatternValue[\n");
    switch(x->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            pad(depth + 1);
            fprintf(stderr, "TPMCPATTERNVALUE_TYPE_VAR\n");
                        printAstSymbol(x->val.var, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            pad(depth + 1);
            fprintf(stderr, "TPMCPATTERNVALUE_TYPE_COMPARISON\n");
            printTpmcComparisonPattern(x->val.comparison, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            pad(depth + 1);
            fprintf(stderr, "TPMCPATTERNVALUE_TYPE_ASSIGNMENT\n");
            printTpmcAssignmentPattern(x->val.assignment, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            pad(depth + 1);
            fprintf(stderr, "TPMCPATTERNVALUE_TYPE_WILDCARD\n");
                        pad(depth + 1);
fprintf(stderr, "void * %p", x->val.wildcard);
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            pad(depth + 1);
            fprintf(stderr, "TPMCPATTERNVALUE_TYPE_CHARACTER\n");
                        pad(depth + 1);
fprintf(stderr, "char '%c'", x->val.character);
            break;
        case TPMCPATTERNVALUE_TYPE_INTEGER:
            pad(depth + 1);
            fprintf(stderr, "TPMCPATTERNVALUE_TYPE_INTEGER\n");
                        pad(depth + 1);
fprintf(stderr, "int %d", x->val.integer);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            pad(depth + 1);
            fprintf(stderr, "TPMCPATTERNVALUE_TYPE_CONSTRUCTOR\n");
            printTpmcConstructorPattern(x->val.constructor, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTpmcPatternValue", x->type);
    }
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcStateValue(struct TpmcStateValue * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcStateValue (NULL)"); return; }
    fprintf(stderr, "TpmcStateValue[\n");
    switch(x->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            pad(depth + 1);
            fprintf(stderr, "TPMCSTATEVALUE_TYPE_TEST\n");
            printTpmcTestState(x->val.test, depth + 1);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            pad(depth + 1);
            fprintf(stderr, "TPMCSTATEVALUE_TYPE_FINAL\n");
            printTpmcFinalState(x->val.final, depth + 1);
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            pad(depth + 1);
            fprintf(stderr, "TPMCSTATEVALUE_TYPE_ERROR\n");
                        pad(depth + 1);
fprintf(stderr, "void * %p", x->val.error);
            break;
        default:
            cant_happen("unrecognised type %d in printTpmcStateValue", x->type);
    }
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcMatchRuleArray(struct TpmcMatchRuleArray * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcMatchRuleArray (NULL)"); return; }
    fprintf(stderr, "TpmcMatchRuleArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcMatchRule(x->entries[i], depth + 1);
        fprintf(stderr, "\n");
    }
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcVariableArray(struct TpmcVariableArray * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcVariableArray (NULL)"); return; }
    fprintf(stderr, "TpmcVariableArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
                printAstSymbol(x->entries[i], depth + 1);
        fprintf(stderr, "\n");
    }
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcPatternArray(struct TpmcPatternArray * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcPatternArray (NULL)"); return; }
    fprintf(stderr, "<<%s>>", x->_tag);
    fprintf(stderr, "TpmcPatternArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcPattern(x->entries[i], depth + 1);
        fprintf(stderr, "\n");
    }
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcStateArray(struct TpmcStateArray * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcStateArray (NULL)"); return; }
    fprintf(stderr, "<<%s>>", x->_tag);
    fprintf(stderr, "TpmcStateArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcState(x->entries[i], depth + 1);
        fprintf(stderr, "\n");
    }
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcArcArray(struct TpmcArcArray * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcArcArray (NULL)"); return; }
    fprintf(stderr, "TpmcArcArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcArc(x->entries[i], depth + 1);
        fprintf(stderr, "\n");
    }
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcIntArray(struct TpmcIntArray * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcIntArray (NULL)"); return; }
    fprintf(stderr, "TpmcIntArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
                pad(depth + 1);
fprintf(stderr, "int %d", x->entries[i]);
        fprintf(stderr, "\n");
    }
    pad(depth);
    fprintf(stderr, "]");
}

void printTpmcMatrix(struct TpmcMatrix * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TpmcMatrix (NULL)"); return; }
    fprintf(stderr, "TpmcMatrix(%d * %d)[\n", x->width, x->height);
    for (int i = 0; i < x->height; i++) {
        pad(depth);
        fprintf(stderr, "[\n");
        for (int j = 0; j < x->width; j++) {
            printTpmcPattern(x->entries[i * x->width + j], depth + 1);
            fprintf(stderr, "\n");
        }
        pad(depth);
        fprintf(stderr, "]\n");
    }
    pad(depth);
    fprintf(stderr, "]");
}

