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
#include <stdio.h>

static void ppTpmcHashSymbol(FILE *out, HashSymbol *symbol) {
    fprintf(out, "%s", symbol->name);
}

void ppTpmcComparisonPattern(FILE *out,
                             TpmcComparisonPattern *comparisonPattern) {
    ppTpmcPattern(out, comparisonPattern->previous);
    fprintf(out, "==");
    ppTpmcPattern(out, comparisonPattern->current);
}

void ppTpmcAssignmentPattern(FILE *out,
                             TpmcAssignmentPattern *assignmentPattern) {
    fprintf(out, "%s<-", assignmentPattern->name->name);
    ppTpmcPattern(out, assignmentPattern->value);
}

void ppTpmcConstructorPattern(FILE *out,
                              TpmcConstructorPattern *constructorPattern) {
    ppTpmcHashSymbol(out, constructorPattern->tag);
    ppTpmcPatternArray(out, constructorPattern->components);
}

void ppTpmcTuplePattern(FILE *out, TpmcPatternArray *tuple) {
    fprintf(out, "#");
    ppTpmcPatternArray(out, tuple);
}

void ppTpmcPatternArray(FILE *out, TpmcPatternArray *patternArray) {
    fprintf(out, "(");
    Index i = 0;
    TpmcPattern *pattern = NULL;
    bool more = false;
    while (iterateTpmcPatternArray(patternArray, &i, &pattern, &more)) {
        ppTpmcPattern(out, pattern);
        if (more) {
            fprintf(out, ", ");
        }
    }
    fprintf(out, ")");
}

void ppTpmcPatternValue(FILE *out, TpmcPatternValue *patternValue) {
    if (patternValue == NULL) {
        fprintf(out, "<NULL pattern value>");
        return;
    }
    switch (patternValue->type) {
    case TPMCPATTERNVALUE_TYPE_VAR:
        ppTpmcHashSymbol(out, patternValue->val.var);
        break;
    case TPMCPATTERNVALUE_TYPE_COMPARISON:
        ppTpmcComparisonPattern(out, patternValue->val.comparison);
        break;
    case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
        ppTpmcAssignmentPattern(out, patternValue->val.assignment);
        break;
    case TPMCPATTERNVALUE_TYPE_WILDCARD:
        fprintf(out, "_");
        break;
    case TPMCPATTERNVALUE_TYPE_CHARACTER:
        fprintf(out, "'%c'", patternValue->val.character);
        break;
    case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
        fprintMaybeBigInt(out, patternValue->val.bigInteger);
        break;
    case TPMCPATTERNVALUE_TYPE_TUPLE:
        ppTpmcTuplePattern(out, patternValue->val.tuple);
        break;
    case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
        ppTpmcConstructorPattern(out, patternValue->val.constructor);
        break;
    }
}

void ppTpmcPattern(FILE *out, TpmcPattern *pattern) {
    if (pattern == NULL) {
        fprintf(out, "<NULL pattern>");
        return;
    }
    if (pattern->path == NULL) {
        fprintf(out, "<NULL path>");
    } else {
        fprintf(out, "%s", pattern->path->name);
    }
    if (pattern->pattern->type != TPMCPATTERNVALUE_TYPE_WILDCARD) {
        fprintf(out, "=(");
        ppTpmcPatternValue(out, pattern->pattern);
        fprintf(out, ")");
    }
}

void ppTpmcMatrix(FILE *out, TpmcMatrix *matrix) {
    if (matrix == NULL) {
        fprintf(out, "<NULL matrix>\n");
        return;
    }
    fprintf(out, "TpmcMatrix[\n");
    for (Index height = 0; height < matrix->height; height++) {
        fprintf(out, "  (");
        for (Index width = 0; width < matrix->width; width++) {
            ppTpmcPattern(out, getTpmcMatrixIndex(matrix, width, height));
            if (width + 1 < matrix->width)
                fprintf(out, ", ");
        }
        fprintf(out, ")\n");
    }
    fprintf(out, "]\n");
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

void ppTpmcState(FILE *out, TpmcState *state) {
    fprintf(out, "%c%d(%d) ", getTpmcStateType(state), state->stamp,
            state->refCount);
    ppTpmcVariableTable(out, state->freeVariables);
    fprintf(out, " ");
    ppTpmcStateValue(out, state->state);
}

void ppTpmcVariableTable(FILE *out, SymbolSet *table) {
    fprintf(out, "[");
    if (table != NULL) {
        Index i = 0;
        Index count = 0;
        HashSymbol *symbol;
        while ((symbol = iterateSymbolSet(table, &i)) != NULL) {
            ppTpmcHashSymbol(out, symbol);
            count++;
            if (count < countSymbolSet(table)) {
                fprintf(out, ", ");
            }
        }
    }
    fprintf(out, "]");
}

void ppTpmcSymbol(FILE *out, HashSymbol *symbol) {
    ppTpmcHashSymbol(out, symbol);
}

void ppTpmcStateValue(FILE *out, TpmcStateValue *value) {
    switch (value->type) {
    case TPMCSTATEVALUE_TYPE_TEST:
        ppTpmcTestState(out, value->val.test);
        break;
    case TPMCSTATEVALUE_TYPE_FINAL:
        ppTpmcFinalState(out, value->val.final);
        break;
    case TPMCSTATEVALUE_TYPE_ERROR:
        fprintf(out, "ERROR");
        break;
    }
}

void ppTpmcTestState(FILE *out, TpmcTestState *test) {
    ppTpmcHashSymbol(out, test->path);
    fprintf(out, ":");
    ppTpmcArcArray(out, test->arcs);
}

void ppTpmcArcArray(FILE *out, TpmcArcArray *arcs) {
    fprintf(out, "{");
    Index i = 0;
    TpmcArc *arc;
    bool more;
    while (iterateTpmcArcArray(arcs, &i, &arc, &more)) {
        ppTpmcArc(out, arc);
        if (more)
            fprintf(out, ", ");
    }
    fprintf(out, "}");
}

void ppTpmcArc(FILE *out, TpmcArc *arc) {
    fprintf(out, "ARC(");
    ppTpmcVariableTable(out, arc->freeVariables);
    fprintf(out, "::");
    ppTpmcPattern(out, arc->test);
    fprintf(out, "=>");
    ppTpmcState(out, arc->state);
    fprintf(out, ")");
}

void ppTpmcFinalState(FILE *out, TpmcFinalState *final) {
    ppLamExp(out, final->action);
}

void ppTpmcIntArray(FILE *out, IntArray *array) {
    fprintf(out, "[");
    Index i = 0;
    int entry;
    bool more;
    while (iterateIntArray(array, &i, &entry, &more)) {
        fprintf(out, "%d%s", entry, more ? ", " : "");
    }
    fprintf(out, "]");
}

void ppTpmcStateArray(FILE *out, TpmcStateArray *array) {
    fprintf(out, "[\n");
    Index i = 0;
    TpmcState *state;
    bool more;
    while (iterateTpmcStateArray(array, &i, &state, &more)) {
        fprintf(out, "  ");
        ppTpmcState(out, state);
        if (more)
            fprintf(out, ",\n");
    }
    fprintf(out, "\n]");
}
