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
#include "tpmc_mermaid.h"
#include "common.h"
#include "symbol.h"
#include "utils.h"
#include <stdlib.h>
#include <string.h>

int tpmc_mermaid_flag = 0;
char *tpmc_mermaid_function = NULL;

static SCharVec *mermaidState(TpmcState *state, SymbolSet *seen);
static void mermaidPattern(TpmcPattern *pattern);

static bool seenName(char *name, SymbolSet *seen) {
    HashSymbol *symbol = newSymbol(name);
    if (getSymbolSet(seen, symbol))
        return true;
    setSymbolSet(seen, symbol);
    return false;
}

static void mermaidFreeVariables(SymbolSet *freeVariables) {
    printf("[");
    if (freeVariables != NULL) {
        Index i = 0;
        Index count = 0;
        HashSymbol *key;
        while ((key = iterateSymbolSet(freeVariables, &i)) != NULL) {
            printf("%s", key->name);
            count++;
            if (count < countSymbolSet(freeVariables)) {
                printf(" ");
            }
        }
    }
    printf("]");
}

static SCharVec *mermaidStateName(TpmcState *state, SymbolSet *seen) {
    static char buf[512];
    switch (state->state->type) {
    case TPMCSTATEVALUE_TYPE_TEST:
        sprintf(buf, "T%d", state->stamp);
        if (!seenName(buf, seen)) {
            printf("%s(\"%s<br/>", buf, state->state->val.test->path->name);
            mermaidFreeVariables(state->freeVariables);
            printf("<br/>(arcs %d)\")\n",
                   countTpmcArcArray(state->state->val.test->arcs));
        }
        break;
    case TPMCSTATEVALUE_TYPE_FINAL:
        sprintf(buf, "F%d", state->stamp);
        if (!seenName(buf, seen)) {
            printf("%s(\"", buf);
            ppLamExp(state->state->val.final->action);
            printf("<br/>");
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
    return stringToSCharVec(buf);
}

static void mermaidConstructorComponents(TpmcPatternArray *patterns) {
    for (Index i = 0; i < countTpmcPatternArray(patterns); ++i) {
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
        fprintMaybeBigInt(stdout, value->val.bigInteger);
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
        cant_happen("unrecognised type %s",
                    tpmcPatternValueTypeName(value->type));
    }
}

static void mermaidArcLabel(TpmcArc *arc) {
    printf("\"");
    mermaidPattern(arc->test);
    printf("<br/>");
    mermaidFreeVariables(arc->freeVariables);
    printf("\"");
}

static void mermaidArc(SCharVec *stateName, TpmcArc *arc, SymbolSet *seen) {
    SCharVec *targetState = mermaidState(arc->state, seen);
    int save = PROTECT(targetState);
    printf("%s --", stateName->entries);
    mermaidArcLabel(arc);
    printf("--> %s\n", targetState->entries);
    UNPROTECT(save);
}

static void mermaidTestState(SCharVec *name, TpmcTestState *testState,
                             SymbolSet *seen) {
    for (Index i = 0; i < countTpmcArcArray(testState->arcs); i++) {
        mermaidArc(name, testState->arcs->entries[i], seen);
    }
}

static void mermaidStateValue(SCharVec *name, TpmcStateValue *value,
                              SymbolSet *seen) {
    if (value == NULL) {
        return;
    }
    switch (value->type) {
    case TPMCSTATEVALUE_TYPE_TEST:
        mermaidTestState(name, value->val.test, seen);
        break;
    case TPMCSTATEVALUE_TYPE_FINAL:
    case TPMCSTATEVALUE_TYPE_ERROR:
        break;
    default:
        cant_happen("unrecognised statevalue type %d in mermaidStateValue",
                    value->type);
    }
}

static SCharVec *mermaidState(TpmcState *state, SymbolSet *seen) {
    if (state == NULL) {
        return stringToSCharVec("");
    }
    SCharVec *name = mermaidStateName(state, seen);
    int save = PROTECT(name);
    mermaidStateValue(name, state->state, seen);
    UNPROTECT(save);
    return name;
}

void tpmcMermaid(TpmcState *state) {
    if (tpmc_mermaid_flag) {
        SymbolSet *seen = newSymbolSet();
        int save = PROTECT(seen);
        printf("```mermaid\n");
        printf("---\n");
        printf("title: %s\n", tpmc_mermaid_function);
        printf("---\n");
        printf("flowchart LR\n");
        mermaidState(state, seen);
        printf("```\n");
        UNPROTECT(save);
    }
}
