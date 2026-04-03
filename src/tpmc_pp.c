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
 */

#include "tpmc_pp.h"
#include "common.h"

void ppTpmcComparisonPattern(TpmcComparisonPattern *comparisonPattern) {
    ppTpmcPattern(comparisonPattern->previous);
    printf("==");
    ppTpmcPattern(comparisonPattern->current);
}

void ppTpmcAssignmentPattern(TpmcAssignmentPattern *assignmentPattern) {
    printf("%s<-", assignmentPattern->name->name);
    ppTpmcPattern(assignmentPattern->value);
}

void ppTpmcConstructorPattern(TpmcConstructorPattern *constructorPattern) {
    ppTpmcSymbol(constructorPattern->tag);
    ppTpmcPatternArray(constructorPattern->components);
}

void ppTpmcTuplePattern(TpmcPatternArray *tuple) {
    printf("#");
    ppTpmcPatternArray(tuple);
}

void ppTpmcPatternArray(TpmcPatternArray *patternArray) {
    printf("(");
    Index i = 0;
    TpmcPattern *pattern = NULL;
    bool more = false;
    while (iterateTpmcPatternArray(patternArray, &i, &pattern, &more)) {
        ppTpmcPattern(pattern);
        if (more) {
            printf(", ");
        }
    }
    printf(")");
}

void ppTpmcPatternValue(TpmcPatternValue *patternValue) {
    if (patternValue == NULL) {
        printf("<NULL pattern value>");
        return;
    }
    switch (patternValue->type) {
    case TPMCPATTERNVALUE_TYPE_VAR:
        ppTpmcSymbol(patternValue->val.var);
        break;
    case TPMCPATTERNVALUE_TYPE_COMPARISON:
        ppTpmcComparisonPattern(patternValue->val.comparison);
        break;
    case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
        ppTpmcAssignmentPattern(patternValue->val.assignment);
        break;
    case TPMCPATTERNVALUE_TYPE_WILDCARD:
        printf("_");
        break;
    case TPMCPATTERNVALUE_TYPE_CHARACTER:
        printf("'%c'", patternValue->val.character);
        break;
    case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
        fprintMaybeBigInt(errout, patternValue->val.bigInteger);
        break;
    case TPMCPATTERNVALUE_TYPE_TUPLE:
        ppTpmcTuplePattern(patternValue->val.tuple);
        break;
    case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
        ppTpmcConstructorPattern(patternValue->val.constructor);
        break;
    }
}

void ppTpmcPattern(TpmcPattern *pattern) {
    if (pattern == NULL) {
        printf("<NULL pattern>");
        return;
    }
    if (pattern->path == NULL) {
        printf("<NULL path>");
    } else {
        printf("%s", pattern->path->name);
    }
    if (pattern->pattern->type != TPMCPATTERNVALUE_TYPE_WILDCARD) {
        printf("=(");
        ppTpmcPatternValue(pattern->pattern);
        printf(")");
    }
}

void ppTpmcMatrix(TpmcMatrix *matrix) {
    if (matrix == NULL) {
        printf("<NULL matrix>\n");
        return;
    }
    printf("TpmcMatrix[\n");
    for (Index height = 0; height < matrix->height; height++) {
        printf("  (");
        for (Index width = 0; width < matrix->width; width++) {
            ppTpmcPattern(getTpmcMatrixIndex(matrix, width, height));
            if (width + 1 < matrix->width)
                printf(", ");
        }
        printf(")\n");
    }
    printf("]\n");
}

static char getTpmcStateType(TpmcState *state) {
    switch (state->state->type) {
    case TPMCSTATEVALUE_TYPE_TEST:
        return 'T';
    case TPMCSTATEVALUE_TYPE_FINAL:
        return 'F';
    case TPMCSTATEVALUE_TYPE_ERROR:
        return 'E';
    default:
        return '?';
    }
}

void ppTpmcState(TpmcState *state) {
    printf("%c%d(%d) ", getTpmcStateType(state), state->stamp, state->refCount);
    ppTpmcVariableTable(state->freeVariables);
    printf(" ");
    ppTpmcStateValue(state->state);
}

void ppTpmcVariableTable(SymbolSet *table) {
    printf("[");
    if (table != NULL) {
        Index i = 0;
        Index count = 0;
        HashSymbol *symbol;
        while ((symbol = iterateSymbolSet(table, &i)) != NULL) {
            ppTpmcSymbol(symbol);
            count++;
            if (count < countSymbolSet(table)) {
                printf(", ");
            }
        }
    }
    printf("]");
}

void ppTpmcSymbol(HashSymbol *symbol) { printf("%s", symbol->name); }

void ppTpmcStateValue(TpmcStateValue *value) {
    switch (value->type) {
    case TPMCSTATEVALUE_TYPE_TEST:
        ppTpmcTestState(value->val.test);
        break;
    case TPMCSTATEVALUE_TYPE_FINAL:
        ppTpmcFinalState(value->val.final);
        break;
    case TPMCSTATEVALUE_TYPE_ERROR:
        printf("ERROR");
        break;
    }
}

void ppTpmcTestState(TpmcTestState *test) {
    ppTpmcSymbol(test->path);
    printf(":");
    ppTpmcArcArray(test->arcs);
}

void ppTpmcArcArray(TpmcArcArray *arcs) {
    printf("{");
    Index i = 0;
    TpmcArc *arc;
    bool more;
    while (iterateTpmcArcArray(arcs, &i, &arc, &more)) {
        ppTpmcArc(arc);
        if (more)
            printf(", ");
    }
    printf("}");
}

void ppTpmcArc(TpmcArc *arc) {
    printf("ARC(");
    ppTpmcVariableTable(arc->freeVariables);
    printf("::");
    ppTpmcPattern(arc->test);
    printf("=>");
    ppTpmcState(arc->state);
    printf(")");
}

void ppTpmcFinalState(TpmcFinalState *final) {
    ppLamExp(stdout, final->action);
}

void ppTpmcIntArray(IntArray *array) {
    printf("[");
    Index i = 0;
    int entry;
    bool more;
    while (iterateIntArray(array, &i, &entry, &more)) {
        printf("%d%s", entry, more ? ", " : "");
    }
    printf("]");
}

void ppTpmcStateArray(TpmcStateArray *array) {
    printf("[\n");
    Index i = 0;
    TpmcState *state;
    bool more;
    while (iterateTpmcStateArray(array, &i, &state, &more)) {
        printf("  ");
        ppTpmcState(state);
        if (more)
            printf(",\n");
    }
    printf("\n]");
}
