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
#include <string.h>
#include <stdlib.h>
#include "common.h"
#include "symbol.h"
#include "tpmc_mermaid.h"

int tpmc_mermaid_flag = 0;
char *tpmc_mermaid_function = NULL;

static char *mermaidState(TpmcState *state);
static void mermaidPattern(TpmcPattern *pattern);

static TpmcVariableTable *seen = NULL;

static int initSeenTable() {
    seen = newTpmcVariableTable();
    return PROTECT(seen);
}

static void terminateSeenTable(int save) {
    UNPROTECT(save);
    seen = NULL;
}

static bool seenName(char *name) {
    HashSymbol *symbol = newSymbol(name);
    if (getTpmcVariableTable(seen, symbol))
        return true;
    setTpmcVariableTable(seen, symbol);
    return false;
}

static void mermaidFreeVariables(TpmcVariableTable *freeVariables) {
    printf("[");
    if (freeVariables != NULL) {
        int i = 0;
        int count = 0;
        HashSymbol *key;
        while ((key = iterateTpmcVariableTable(freeVariables, &i)) != NULL) {
            printf("%s", key->name);
            count++;
            if (count < countTpmcVariableTable(freeVariables)) {
                printf(" ");
            }
        }
    }
    printf("]");
}

static char *mermaidStateName(TpmcState *state) {
    static char buf[512];
    switch (state->state->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            sprintf(buf, "T%d", state->stamp);
            if (!seenName(buf)) {
                printf("%s(\"%s\\n", buf,
                       state->state->val.test->path->name);
                mermaidFreeVariables(state->freeVariables);
                printf("\\n(arcs %d)\")\n", countTpmcArcArray(state->state->val.test->arcs));
            }
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
            sprintf(buf, "F%d", state->stamp);
            if (!seenName(buf)) {
                printf("%s(\"", buf);
                ppLamExp(state->state->val.final->action);
                printf("\\n");
                mermaidFreeVariables(state->freeVariables);
                printf("\")\n");
            }
            break;
        case TPMCSTATEVALUE_TYPE_ERROR:
            sprintf(buf, "ERROR");
            printf("%s\n", buf);
            break;
        default:
            cant_happen("unrecognised statevalue type %d in mermaidStateName",
                        state->state->type);
    }
    return strdup(buf);
}

static void mermaidConstructorComponents(TpmcPatternArray *patterns) {
    for (int i = 0; i < countTpmcPatternArray(patterns); ++i) {
        mermaidPattern(patterns->entries[i]);
        if (i < (countTpmcPatternArray(patterns) - 1)) {
            printf(", ");
        }
    }
}

static void mermaidPattern(TpmcPattern *pattern) {
    printf("%s:", pattern->path->name);
    TpmcPatternValue *value = pattern->pattern;
    switch (value->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            printf("var %s", value->val.var->name);
            break;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            mermaidPattern(value->val.comparison->previous);
            printf("==");
            mermaidPattern(value->val.comparison->current);
            break;
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            printf("assignment");
            break;
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            printf("_");
            break;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            printf("'%c'", value->val.character);
            break;
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            fprintBigInt(stdout, value->val.biginteger);
            break;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            printf("%s(", value->val.constructor->tag->name);
            mermaidConstructorComponents(value->val.constructor->components);
            printf(")");
            break;
        case TPMCPATTERNVALUE_TYPE_TUPLE:
            printf("#(");
            mermaidConstructorComponents(value->val.tuple);
            printf(")");
            break;
        default:
            cant_happen("unrecognised type %s", tpmcPatternValueTypeName(value->type));
    }
}

static void mermaidArcLabel(TpmcArc *arc) {
    printf("\"");
    mermaidPattern(arc->test);
    printf("\\n");
    mermaidFreeVariables(arc->freeVariables);
    printf("\"");
}

static void mermaidArc(char *stateName, TpmcArc *arc) {
    char *targetState = mermaidState(arc->state);
    printf("%s --", stateName);
    mermaidArcLabel(arc);
    printf("--> %s\n", targetState);
    free(targetState);
}

static void mermaidTestState(char *name, TpmcTestState *testState) {
    for (int i = 0; i < countTpmcArcArray(testState->arcs); i++) {
        mermaidArc(name, testState->arcs->entries[i]);
    }
}

static void mermaidStateValue(char *name, TpmcStateValue *value) {
    if (value == NULL) {
        return;
    }
    switch (value->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            mermaidTestState(name, value->val.test);
            break;
        case TPMCSTATEVALUE_TYPE_FINAL:
        case TPMCSTATEVALUE_TYPE_ERROR:
            break;
        default:
            cant_happen
                ("unrecognised statevalue type %d in mermaidStateValue",
                 value->type);
    }
}

static char *mermaidState(TpmcState *state) {
    if (state == NULL) {
        return "";
    }
    char *name = mermaidStateName(state);
    mermaidStateValue(name, state->state);
    return name;
}

void tpmcMermaid(TpmcState *state) {
    if (tpmc_mermaid_flag) {
        int save = initSeenTable();
        printf("## %s\n", tpmc_mermaid_function);
        printf("```mermaid\n");
        printf("flowchart TD\n");
        free(mermaidState(state));
        printf("```\n");
        terminateSeenTable(save);
    }
}
