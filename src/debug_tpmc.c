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
#include "bigint.h"

static void pad(int depth) { eprintf("%*s", depth * 4, ""); }

void printTpmcMatchRules(struct TpmcMatchRules * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcMatchRules (NULL)"); return; }
    eprintf("TpmcMatchRules[\n");
    printTpmcMatchRuleArray(x->rules, depth+1);
    eprintf("\n");
    printTpmcVariableArray(x->rootVariables, depth+1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcMatchRule(struct TpmcMatchRule * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcMatchRule (NULL)"); return; }
    eprintf("TpmcMatchRule[\n");
    printTpmcState(x->action, depth + 1);
    eprintf("\n");
    printTpmcPatternArray(x->patterns, depth+1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcComparisonPattern(struct TpmcComparisonPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcComparisonPattern (NULL)"); return; }
    eprintf("TpmcComparisonPattern[\n");
    printTpmcPattern(x->previous, depth + 1);
    eprintf("\n");
    printTpmcPattern(x->current, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcAssignmentPattern(struct TpmcAssignmentPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcAssignmentPattern (NULL)"); return; }
    eprintf("TpmcAssignmentPattern[\n");
        printAstSymbol(x->name, depth + 1);
    eprintf("\n");
    printTpmcPattern(x->value, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcConstructorPattern(struct TpmcConstructorPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcConstructorPattern (NULL)"); return; }
    eprintf("TpmcConstructorPattern[\n");
        printAstSymbol(x->tag, depth + 1);
    eprintf("\n");
        printLamTypeConstructorInfo(x->info, depth + 1);
    eprintf("\n");
    printTpmcPatternArray(x->components, depth+1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcPattern(struct TpmcPattern * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcPattern (NULL)"); return; }
    eprintf("TpmcPattern[\n");
        printAstSymbol(x->path, depth + 1);
    eprintf("\n");
    printTpmcPatternValue(x->pattern, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcTestState(struct TpmcTestState * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcTestState (NULL)"); return; }
    eprintf("TpmcTestState[\n");
        printAstSymbol(x->path, depth + 1);
    eprintf("\n");
    printTpmcArcArray(x->arcs, depth+1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcFinalState(struct TpmcFinalState * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcFinalState (NULL)"); return; }
    eprintf("TpmcFinalState[\n");
        ppLamExpD(x->action, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcState(struct TpmcState * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcState (NULL)"); return; }
    eprintf("TpmcState[\n");
        pad(depth + 1);
eprintf("int %d", x->refcount);
    eprintf("\n");
        pad(depth + 1);
eprintf("int %d", x->stamp);
    eprintf("\n");
        printHashTable(x->freeVariables, depth + 1);
    eprintf("\n");
    printTpmcStateValue(x->state, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcArc(struct TpmcArc * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcArc (NULL)"); return; }
    eprintf("TpmcArc[\n");
    printTpmcState(x->state, depth + 1);
    eprintf("\n");
    printTpmcPattern(x->test, depth + 1);
    eprintf("\n");
        printHashTable(x->freeVariables, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcArcList(struct TpmcArcList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcArcList (NULL)"); return; }
    eprintf("TpmcArcList[\n");
    printTpmcArc(x->arc, depth + 1);
    eprintf("\n");
    printTpmcArcList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcIntList(struct TpmcIntList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcIntList (NULL)"); return; }
    eprintf("TpmcIntList[\n");
        pad(depth + 1);
eprintf("int %d", x->integer);
    eprintf("\n");
    printTpmcIntList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcPatternValue(struct TpmcPatternValue * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcPatternValue (NULL)"); return; }
    eprintf("TpmcPatternValue[\n");
    switch(x->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            pad(depth + 1);
            eprintf("TPMCPATTERNVALUE_TYPE_VAR\n");
                        printAstSymbol(x->val.var, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            pad(depth + 1);
            eprintf("TPMCPATTERNVALUE_TYPE_COMPARISON\n");
            printTpmcComparisonPattern(x->val.comparison, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            pad(depth + 1);
            eprintf("TPMCPATTERNVALUE_TYPE_ASSIGNMENT\n");
            printTpmcAssignmentPattern(x->val.assignment, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            pad(depth + 1);
            eprintf("TPMCPATTERNVALUE_TYPE_WILDCARD\n");
                        pad(depth + 1);
eprintf("void * %p", x->val.wildcard);
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            pad(depth + 1);
            eprintf("TPMCPATTERNVALUE_TYPE_CHARACTER\n");
                        pad(depth + 1);
eprintf("char '%c'", x->val.character);
            break;
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            pad(depth + 1);
            eprintf("TPMCPATTERNVALUE_TYPE_BIGINTEGER\n");
                        printBigInt(x->val.biginteger, depth + 1);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            pad(depth + 1);
            eprintf("TPMCPATTERNVALUE_TYPE_CONSTRUCTOR\n");
            printTpmcConstructorPattern(x->val.constructor, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTpmcPatternValue", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcStateValue(struct TpmcStateValue * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcStateValue (NULL)"); return; }
    eprintf("TpmcStateValue[\n");
    switch(x->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            pad(depth + 1);
            eprintf("TPMCSTATEVALUE_TYPE_TEST\n");
            printTpmcTestState(x->val.test, depth + 1);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            pad(depth + 1);
            eprintf("TPMCSTATEVALUE_TYPE_FINAL\n");
            printTpmcFinalState(x->val.final, depth + 1);
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            pad(depth + 1);
            eprintf("TPMCSTATEVALUE_TYPE_ERROR\n");
                        pad(depth + 1);
eprintf("void * %p", x->val.error);
            break;
        default:
            cant_happen("unrecognised type %d in printTpmcStateValue", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTpmcMatchRuleArray(struct TpmcMatchRuleArray * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcMatchRuleArray (NULL)"); return; }
    eprintf("TpmcMatchRuleArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcMatchRule(x->entries[i], depth + 1);
        eprintf("\n");
    }
    pad(depth);
    eprintf("]");
}

void printTpmcVariableArray(struct TpmcVariableArray * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcVariableArray (NULL)"); return; }
    eprintf("TpmcVariableArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
                printAstSymbol(x->entries[i], depth + 1);
        eprintf("\n");
    }
    pad(depth);
    eprintf("]");
}

void printTpmcPatternArray(struct TpmcPatternArray * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcPatternArray (NULL)"); return; }
    eprintf("<<%s>>", x->_tag);
    eprintf("TpmcPatternArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcPattern(x->entries[i], depth + 1);
        eprintf("\n");
    }
    pad(depth);
    eprintf("]");
}

void printTpmcStateArray(struct TpmcStateArray * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcStateArray (NULL)"); return; }
    eprintf("<<%s>>", x->_tag);
    eprintf("TpmcStateArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcState(x->entries[i], depth + 1);
        eprintf("\n");
    }
    pad(depth);
    eprintf("]");
}

void printTpmcArcArray(struct TpmcArcArray * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcArcArray (NULL)"); return; }
    eprintf("TpmcArcArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        printTpmcArc(x->entries[i], depth + 1);
        eprintf("\n");
    }
    pad(depth);
    eprintf("]");
}

void printTpmcIntArray(struct TpmcIntArray * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcIntArray (NULL)"); return; }
    eprintf("TpmcIntArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
                pad(depth + 1);
eprintf("int %d", x->entries[i]);
        eprintf("\n");
    }
    pad(depth);
    eprintf("]");
}

void printTpmcMatrix(struct TpmcMatrix * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TpmcMatrix (NULL)"); return; }
    eprintf("TpmcMatrix(%d * %d)[\n", x->width, x->height);
    for (int i = 0; i < x->height; i++) {
        pad(depth);
        eprintf("[\n");
        for (int j = 0; j < x->width; j++) {
            printTpmcPattern(x->entries[i * x->width + j], depth + 1);
            eprintf("\n");
        }
        pad(depth);
        eprintf("]\n");
    }
    pad(depth);
    eprintf("]");
}

