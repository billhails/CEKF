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

static void pad(int depth) { printf("%*s", depth * 4, ""); }

void printTpmcMatchRules(struct TpmcMatchRules * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcMatchRules (NULL)"); return; }
    printf("TpmcMatchRules[\n");
    printTpmcMatchRuleArray(x->rules, depth+1);
    printf("\n");
    printTpmcVariableArray(x->rootVariables, depth+1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcMatchRule(struct TpmcMatchRule * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcMatchRule (NULL)"); return; }
    printf("TpmcMatchRule[\n");
    printTpmcState(x->action, depth + 1);
    printf("\n");
    printTpmcPatternArray(x->patterns, depth+1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcComparisonPattern(struct TpmcComparisonPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcComparisonPattern (NULL)"); return; }
    printf("TpmcComparisonPattern[\n");
    printTpmcPattern(x->previous, depth + 1);
    printf("\n");
    printTpmcPattern(x->current, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcAssignmentPattern(struct TpmcAssignmentPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcAssignmentPattern (NULL)"); return; }
    printf("TpmcAssignmentPattern[\n");
        printAstSymbol(x->name, depth + 1);
    printf("\n");
    printTpmcPattern(x->value, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcConstructorPattern(struct TpmcConstructorPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcConstructorPattern (NULL)"); return; }
    printf("TpmcConstructorPattern[\n");
        printAstSymbol(x->tag, depth + 1);
    printf("\n");
        printLamTypeConstructorInfo(x->info, depth + 1);
    printf("\n");
    printTpmcPatternArray(x->components, depth+1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcPattern(struct TpmcPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcPattern (NULL)"); return; }
    printf("TpmcPattern[\n");
        printAstSymbol(x->path, depth + 1);
    printf("\n");
    printTpmcPatternValue(x->pattern, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcTestState(struct TpmcTestState * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcTestState (NULL)"); return; }
    printf("TpmcTestState[\n");
        printAstSymbol(x->path, depth + 1);
    printf("\n");
    printTpmcArcArray(x->arcs, depth+1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcFinalState(struct TpmcFinalState * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcFinalState (NULL)"); return; }
    printf("TpmcFinalState[\n");
        ppLamExpD(x->action, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcState(struct TpmcState * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcState (NULL)"); return; }
    printf("TpmcState[\n");
        pad(depth + 1);
printf("int %d", x->refcount);
    printf("\n");
        pad(depth + 1);
printf("int %d", x->stamp);
    printf("\n");
        printHashTable(x->freeVariables, depth + 1);
    printf("\n");
    printTpmcStateValue(x->state, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcArc(struct TpmcArc * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcArc (NULL)"); return; }
    printf("TpmcArc[\n");
    printTpmcState(x->state, depth + 1);
    printf("\n");
    printTpmcPattern(x->test, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcStateArrayContainer(struct TpmcStateArrayContainer * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcStateArrayContainer (NULL)"); return; }
    printf("TpmcStateArrayContainer[\n");
    printTpmcStateArray(x->array, depth+1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcPatternValue(struct TpmcPatternValue * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcPatternValue (NULL)"); return; }
    printf("TpmcPatternValue[\n");
    switch(x->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            pad(depth + 1);
            printf("TPMCPATTERNVALUE_TYPE_VAR\n");
                        printAstSymbol(x->val.var, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            pad(depth + 1);
            printf("TPMCPATTERNVALUE_TYPE_COMPARISON\n");
            printTpmcComparisonPattern(x->val.comparison, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            pad(depth + 1);
            printf("TPMCPATTERNVALUE_TYPE_ASSIGNMENT\n");
            printTpmcAssignmentPattern(x->val.assignment, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            pad(depth + 1);
            printf("TPMCPATTERNVALUE_TYPE_WILDCARD\n");
                        pad(depth + 1);
printf("void * %p", x->val.wildcard);
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            pad(depth + 1);
            printf("TPMCPATTERNVALUE_TYPE_CHARACTER\n");
                        pad(depth + 1);
printf("char '%c'", x->val.character);
            break;
        case TPMCPATTERNVALUE_TYPE_INTEGER:
            pad(depth + 1);
            printf("TPMCPATTERNVALUE_TYPE_INTEGER\n");
                        pad(depth + 1);
printf("int %d", x->val.integer);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            pad(depth + 1);
            printf("TPMCPATTERNVALUE_TYPE_CONSTRUCTOR\n");
            printTpmcConstructorPattern(x->val.constructor, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTpmcPatternValue", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcStateValue(struct TpmcStateValue * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcStateValue (NULL)"); return; }
    printf("TpmcStateValue[\n");
    switch(x->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            pad(depth + 1);
            printf("TPMCSTATEVALUE_TYPE_TEST\n");
            printTpmcTestState(x->val.test, depth + 1);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            pad(depth + 1);
            printf("TPMCSTATEVALUE_TYPE_FINAL\n");
            printTpmcFinalState(x->val.final, depth + 1);
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            pad(depth + 1);
            printf("TPMCSTATEVALUE_TYPE_ERROR\n");
                        pad(depth + 1);
printf("void * %p", x->val.error);
            break;
        default:
            cant_happen("unrecognised type %d in printTpmcStateValue", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

void printTpmcMatchRuleArray(struct TpmcMatchRuleArray * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcMatchRuleArray (NULL)"); return; }
    printf("TpmcMatchRuleArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcMatchRule(x->entries[i], depth + 1);
        printf("\n");
    }
    pad(depth);
    printf("]");
}

void printTpmcVariableArray(struct TpmcVariableArray * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcVariableArray (NULL)"); return; }
    printf("TpmcVariableArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
                printAstSymbol(x->entries[i], depth + 1);
        printf("\n");
    }
    pad(depth);
    printf("]");
}

void printTpmcPatternArray(struct TpmcPatternArray * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcPatternArray (NULL)"); return; }
    printf("TpmcPatternArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcPattern(x->entries[i], depth + 1);
        printf("\n");
    }
    pad(depth);
    printf("]");
}

void printTpmcStateArray(struct TpmcStateArray * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcStateArray (NULL)"); return; }
    printf("TpmcStateArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcState(x->entries[i], depth + 1);
        printf("\n");
    }
    pad(depth);
    printf("]");
}

void printTpmcArcArray(struct TpmcArcArray * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcArcArray (NULL)"); return; }
    printf("TpmcArcArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcArc(x->entries[i], depth + 1);
        printf("\n");
    }
    pad(depth);
    printf("]");
}

void printTpmcIntArray(struct TpmcIntArray * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcIntArray (NULL)"); return; }
    printf("TpmcIntArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
                pad(depth + 1);
printf("int %d", x->entries[i]);
        printf("\n");
    }
    pad(depth);
    printf("]");
}

void printTpmcMatrix(struct TpmcMatrix * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TpmcMatrix (NULL)"); return; }
    printf("TpmcMatrix(%d * %d)[\n", x->width, x->height);
    for (int i = 0; i < x->height; i++) {
        pad(depth);
        printf("[\n");
        for (int j = 0; j < x->width; j++) {
            printTpmcPattern(x->entries[i * x->width + j], depth + 1);
            printf("\n");
        }
        pad(depth);
        printf("]\n");
    }
    pad(depth);
    printf("]");
}

