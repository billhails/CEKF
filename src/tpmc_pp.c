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
    eprintf("==");
    ppTpmcPattern(comparisonPattern->current);
}

void ppTpmcAssignmentPattern(TpmcAssignmentPattern *assignmentPattern) {
    eprintf("%s<-", assignmentPattern->name->name);
    ppTpmcPattern(assignmentPattern->value);
}

void ppTpmcConstructorPattern(TpmcConstructorPattern *constructorPattern) {
    ppTpmcSymbol(constructorPattern->tag);
    eprintf(":%d", constructorPattern->namespace);
    ppTpmcPatternArray(constructorPattern->components);
}

void ppTpmcTuplePattern(TpmcPatternArray *tuple) {
    eprintf("#");
    ppTpmcPatternArray(tuple);
}

void ppTpmcPatternArray(TpmcPatternArray *patternArray) {
    eprintf("(");
    Index i = 0;
    TpmcPattern *pattern = NULL;
    bool more = false;
    while (iterateTpmcPatternArray(patternArray, &i, &pattern, &more)) {
        ppTpmcPattern(pattern);
        if (more) {
            eprintf(", ");
        }
    }
    eprintf(")");
}

void ppTpmcPatternValue(TpmcPatternValue *patternValue) {
    if (patternValue == NULL) {
        eprintf("<NULL pattern value>");
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
            eprintf("_");
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            eprintf("'%c'", patternValue->val.character);
            break;
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            fprintMaybeBigInt(errout, patternValue->val.biginteger);
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
        eprintf("<NULL pattern>");
        return;
    }
    if (pattern->path == NULL) {
        eprintf("<NULL path>");
    } else {
        eprintf("%s", pattern->path->name);
    }
    if (pattern->pattern->type != TPMCPATTERNVALUE_TYPE_WILDCARD) {
        eprintf("=(");
        ppTpmcPatternValue(pattern->pattern);
        eprintf(")");
    }
}

void ppTpmcMatrix(TpmcMatrix *matrix) {
    if (matrix == NULL) {
        eprintf("<NULL matrix>\n");
        return;
    }
    eprintf("TpmcMatrix[\n");
    for (Index height = 0; height < matrix->height; height++) {
        eprintf("  (");
        for (Index width = 0; width < matrix->width; width++) {
            ppTpmcPattern(getTpmcMatrixIndex(matrix, width, height));
            if (width + 1 < matrix->width)
                eprintf(", ");
        }
        eprintf(")\n");
    }
    eprintf("]\n");
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
    eprintf("%c%d(%d) ", getTpmcStateType(state), state->stamp,
            state->refcount);
    ppTpmcVariableTable(state->freeVariables);
    eprintf(" ");
    ppTpmcStateValue(state->state);
}

void ppTpmcVariableTable(TpmcVariableTable *table) {
    eprintf("[");
    if (table != NULL) {
        Index i = 0;
        Index count = 0;
        HashSymbol *symbol;
        while ((symbol = iterateTpmcVariableTable(table, &i)) != NULL) {
            ppTpmcSymbol(symbol);
            count++;
            if (count < countTpmcVariableTable(table)) {
                eprintf(", ");
            }
        }
    }
    eprintf("]");
}

void ppTpmcSymbol(HashSymbol *symbol) {
    eprintf("%s", symbol->name);
}

void ppTpmcStateValue(TpmcStateValue *value) {
    switch (value->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            ppTpmcTestState(value->val.test);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            ppTpmcFinalState(value->val.final);
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            eprintf("ERROR");
            break;
    }
}

void ppTpmcTestState(TpmcTestState *test) {
    ppTpmcSymbol(test->path);
    eprintf(":");
    ppTpmcArcArray(test->arcs);
}

void ppTpmcArcArray(TpmcArcArray *arcs) {
    eprintf("{");
    Index i = 0;
    TpmcArc *arc;
    bool more;
    while (iterateTpmcArcArray(arcs, &i, &arc, &more)) {
        ppTpmcArc(arc);
        if (more)
            eprintf(", ");
    }
    eprintf("}");
}

void ppTpmcArc(TpmcArc *arc) {
    eprintf("ARC(");
    ppTpmcVariableTable(arc->freeVariables);
    eprintf("::");
    ppTpmcPattern(arc->test);
    eprintf("=>");
    ppTpmcState(arc->state);
    eprintf(")");
}

void ppTpmcFinalState(TpmcFinalState *final) {
    ppLamExp(final->action);
}

void ppTpmcIntArray(TpmcIntArray *array) {
    eprintf("[");
    Index i = 0;
    int entry;
    bool more;
    while (iterateTpmcIntArray(array, &i, &entry, &more)) {
        eprintf("%d%s", entry, more ? ", " : "");
    }
    eprintf("]");
}

void ppTpmcStateArray(TpmcStateArray *array) {
    eprintf("[\n");
    Index i = 0;
    TpmcState *state;
    bool more;
    while (iterateTpmcStateArray(array, &i, &state, &more)) {
        eprintf("  ");
        ppTpmcState(state);
        if (more)
            eprintf(",\n");
    }
    eprintf("\n]");
}
