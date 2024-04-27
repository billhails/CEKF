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

// TODO should be able to get rid of this now we auto-generate comparison functions

#include "tpmc_compare.h"
#include "tpmc_pp.h"
#include "common.h"
#include "types.h"

#ifdef DEBUG_TPMC_COMPARE
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

#define PREAMBLE() do {\
    if (a == b) { \
        return true; \
    } \
    if (a == NULL || b == NULL) { \
        return false; \
    } \
} while (0)

bool tpmcStateEq(TpmcState *a, TpmcState *b) {
    PREAMBLE();
    return a->stamp == b->stamp || tpmcStateValueEq(a->state, b->state);
}

bool tpmcStateValueEq(TpmcStateValue *a, TpmcStateValue *b) {
    PREAMBLE();
    if (a->type != b->type) {
        return false;
    }
    switch (a->type) {
        case TPMCSTATEVALUE_TYPE_TEST:
            return tpmcTestStateEq(a->val.test, b->val.test);
        case TPMCSTATEVALUE_TYPE_FINAL:
            return false;
        case TPMCSTATEVALUE_TYPE_ERROR:
            return true;
        default:
            cant_happen("unrecognised type %d passed to tpmcStateValueEq",
                        a->type);
    }
}

bool tpmcTestStateEq(TpmcTestState *a, TpmcTestState *b) {
    PREAMBLE();
    return (a->path == b->path && tpmcArcArrayEq(a->arcs, b->arcs));
}

bool tpmcArcArrayEq(TpmcArcArray *a, TpmcArcArray *b) {
    PREAMBLE();
    if (a->size != b->size) {
        return false;
    }
    for (Index i = 0; i < a->size; ++i) {
        if (!tpmcArcEq(a->entries[i], b->entries[i])) {
            return false;
        }
    }
    return true;
}

bool tpmcArcEq(TpmcArc *a, TpmcArc *b) {
    PREAMBLE();
    return (tpmcStateEq(a->state, b->state)
            && tpmcPatternEq(a->test, b->test));
}

bool tpmcArcInArray(TpmcArc *arc, TpmcArcArray *arcArray) {
    DEBUGN("tpmcArcInArray: ");
    IFDEBUGN(ppTpmcPattern(arc->test));
    for (Index i = 0; i < arcArray->size; i++) {
        if (tpmcArcEq(arcArray->entries[i], arc)) {
            DEBUG("tpmcArcInArray returning true");
            return true;
        }
    }
    DEBUG("tpmcArcInArray returning false");
    return false;
}

bool tpmcPatternEq(TpmcPattern *a, TpmcPattern *b) {
    PREAMBLE();
    return (a->path == b->path && tpmcPatternValueEq(a->pattern, b->pattern));
}

bool tpmcPatternValueEq(TpmcPatternValue *a, TpmcPatternValue *b) {
    PREAMBLE();
    if (a->type != b->type) {
        return false;
    }
    switch (a->type) {
        case TPMCPATTERNVALUE_TYPE_VAR:
            return a->val.var == b->val.var;
        case TPMCPATTERNVALUE_TYPE_COMPARISON:
            return tpmcComparisonPatternEq(a->val.comparison,
                                           b->val.comparison);
        case TPMCPATTERNVALUE_TYPE_ASSIGNMENT:
            return tpmcAssignmentPatternEq(a->val.assignment,
                                           b->val.assignment);
        case TPMCPATTERNVALUE_TYPE_WILDCARD:
            return true;
        case TPMCPATTERNVALUE_TYPE_CHARACTER:
            return a->val.character == b->val.character;
        case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
            return cmpMaybeBigInt(a->val.biginteger, b->val.biginteger) == 0;
        case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
            return tpmcConstructorPatternEq(a->val.constructor,
                                            b->val.constructor);
        case TPMCPATTERNVALUE_TYPE_TUPLE:
            return tpmcPatternArrayEq(a->val.tuple, b->val.tuple);
        default:
            cant_happen("unrecognised type %s", tpmcPatternValueTypeName(a->type));
    }
}

bool tpmcComparisonPatternEq(TpmcComparisonPattern *a,
                             TpmcComparisonPattern *b) {
    PREAMBLE();
    return tpmcPatternEq(a->previous, b->previous)
        && tpmcPatternEq(a->current, b->current);
}

bool tpmcAssignmentPatternEq(TpmcAssignmentPattern *a,
                             TpmcAssignmentPattern *b) {
    PREAMBLE();
    return a->name == b->name && tpmcPatternEq(a->value, b->value);
}

bool tpmcConstructorPatternEq(TpmcConstructorPattern *a,
                              TpmcConstructorPattern *b) {
    PREAMBLE();
    return a->tag == b->tag
        && tpmcPatternArrayEq(a->components, b->components);
}

bool tpmcPatternArrayEq(TpmcPatternArray *a, TpmcPatternArray *b) {
    PREAMBLE();
    if (a->size != b->size) {
        return false;
    }
    for (Index i = 0; i < a->size; ++i) {
        if (!tpmcPatternEq(a->entries[i], b->entries[i])) {
            return false;
        }
    }
    return true;
}

#undef PREAMBLE
