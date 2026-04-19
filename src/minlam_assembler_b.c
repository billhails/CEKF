/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "minlam_assembler_b.h"

#include "emit_b_helper.h"

/** Returns the buffer assigned to a plan label. */
static BBuffer *getPlanBuffer(BAssemblyPlan *plan, HashSymbol *label) {
    BBuffer *buffer = NULL;
    if (!getBBufferBag(plan->buffers, label, &buffer)) {
        cant_happen("missing BBuffer for %s in BAssemblyPlan", label->name);
    }
    return buffer;
}

/** Records a rebased label in the global label map. */
static void addGlobalLabel(BLayout *layout, HashSymbol *label, Index location) {
    if (getIndexMap(layout->globalLabels, label, NULL)) {
        cant_happen("duplicate global label %s in BLayout", label->name);
    }
    setIndexMap(layout->globalLabels, label, location);
}

/** Computes the starting code offset for each buffer in plan order. */
static void layoutBufferBases(BAssemblyPlan *plan, BLayout *layout) {
    Index offset = 0;

    for (Index i = 0; i < countSymbolArray(plan->order); i++) {
        HashSymbol *label = getSymbolArray(plan->order, i);
        BBuffer *buffer = getPlanBuffer(plan, label);
        if (getIndexMap(layout->bufferBases, label, NULL)) {
            cant_happen("duplicate buffer %s in BAssemblyPlan order",
                        label->name);
        }
        setIndexMap(layout->bufferBases, label, offset);
        offset += countUIntArray(buffer->codes);
    }
}

/** Rewrites buffer-local labels into final code coordinates. */
static void layoutGlobalLabels(BAssemblyPlan *plan, BLayout *layout) {
    for (Index i = 0; i < countSymbolArray(plan->order); i++) {
        HashSymbol *bufferLabel = getSymbolArray(plan->order, i);
        BBuffer *buffer = getPlanBuffer(plan, bufferLabel);
        Index base = 0;
        Index localOffset = 0;
        HashSymbol *label = NULL;

        if (!getIndexMap(layout->bufferBases, bufferLabel, &base)) {
            cant_happen("missing buffer base for %s in BLayout",
                        bufferLabel->name);
        }

        addGlobalLabel(layout, bufferLabel, base);

        Index iter = 0;
        while ((label = iterateIndexMap(buffer->labels, &iter, &localOffset)) !=
               NULL) {
            addGlobalLabel(layout, label, base + localOffset);
        }
    }
}

/** Resolves a label to its final code address. */
static Index resolveGlobalLabel(BLayout *layout, HashSymbol *label) {
    Index target = 0;

    if (!getIndexMap(layout->globalLabels, label, &target)) {
        cant_happen("missing global label %s in BLayout", label->name);
    }
    return target;
}

/** Patches one code operand with its resolved label address. */
static void patchCodeFixup(BBuffer *buffer, BLayout *layout, CodeFixup *fixup) {
    pokeUIntArray(buffer->codes, fixup->location,
                  (UInteger)resolveGlobalLabel(layout, fixup->label));
}

/** Finds which source buffer owns a flattened code location. */
static HashSymbol *findBufferLabelForLocation(BAssemblyPlan *plan,
                                              BLayout *layout, Index location) {
    for (Index i = 0; i < countSymbolArray(plan->order); i++) {
        HashSymbol *label = getSymbolArray(plan->order, i);
        BBuffer *buffer = getPlanBuffer(plan, label);
        Index base = 0;

        if (!getIndexMap(layout->bufferBases, label, &base)) {
            cant_happen("missing buffer base for %s in BLayout", label->name);
        }
        if (location >= base &&
            location < base + countUIntArray(buffer->codes)) {
            return label;
        }
    }

    cant_happen("missing owner buffer for code location %u", location);
}

/** Returns the number of tables of one family in a buffer. */
static Index countTableFamily(BBuffer *buffer, BFixupType type) {
    switch (type) {
    case BFIXUP_TYPE_INTTABLE:
    case BFIXUP_TYPE_INTCOND:
        return countIntCondTable(buffer->intConds);
    case BFIXUP_TYPE_CHARTABLE:
    case BFIXUP_TYPE_CHARCOND:
        return countCharCondTable(buffer->charConds);
    case BFIXUP_TYPE_MATCHTABLE:
    case BFIXUP_TYPE_MATCHCOND:
        return countMatchTable(buffer->matches);
    default:
        cant_happen("unexpected fixup type %s in countTableFamily",
                    bFixupTypeName(type));
    }
}

/** Computes the global base for one table family in one buffer. */
static Index getTableBase(BAssemblyPlan *plan, HashSymbol *bufferLabel,
                          BFixupType type) {
    Index base = 0;

    for (Index i = 0; i < countSymbolArray(plan->order); i++) {
        HashSymbol *label = getSymbolArray(plan->order, i);
        BBuffer *buffer = getPlanBuffer(plan, label);

        if (label == bufferLabel) {
            return base;
        }
        base += countTableFamily(buffer, type);
    }

    cant_happen("missing buffer %s in BAssemblyPlan order", bufferLabel->name);
}

/** Resolves a local table id to its final global index. */
static Index resolveGlobalTableId(BAssemblyPlan *plan, BLayout *layout,
                                  Index location, Index localTableId,
                                  BFixupType type) {
    HashSymbol *bufferLabel =
        findBufferLabelForLocation(plan, layout, location);

    return getTableBase(plan, bufferLabel, type) + localTableId;
}

/** Patches one INTCOND table operand to its global table index. */
static void patchIntTableFixup(BAssemblyPlan *plan, BBuffer *buffer,
                               BLayout *layout, TableFixup *fixup) {
    pokeUIntArray(buffer->codes, fixup->location,
                  (UInteger)resolveGlobalTableId(plan, layout, fixup->location,
                                                 fixup->tableId,
                                                 BFIXUP_TYPE_INTTABLE));
}

/** Patches one CHARCOND table operand to its global table index. */
static void patchCharTableFixup(BAssemblyPlan *plan, BBuffer *buffer,
                                BLayout *layout, TableFixup *fixup) {
    pokeUIntArray(buffer->codes, fixup->location,
                  (UInteger)resolveGlobalTableId(plan, layout, fixup->location,
                                                 fixup->tableId,
                                                 BFIXUP_TYPE_CHARTABLE));
}

/** Patches one MATCH table operand to its global table index. */
static void patchMatchTableFixup(BAssemblyPlan *plan, BBuffer *buffer,
                                 BLayout *layout, TableFixup *fixup) {
    pokeUIntArray(buffer->codes, fixup->location,
                  (UInteger)resolveGlobalTableId(plan, layout, fixup->location,
                                                 fixup->tableId,
                                                 BFIXUP_TYPE_MATCHTABLE));
}

/** Patches one entry in an integer cond table. */
static void patchIntCondFixup(BBuffer *buffer, BLayout *layout, Index tableBase,
                              CondFixup *fixup) {
    IntCondSwitch *table =
        getIntCondTable(buffer->intConds, tableBase + fixup->tableId);
    Index caseCount = countIntCondCaseArray(table->cases);
    Index target = resolveGlobalLabel(layout, fixup->label);

    if (fixup->tableIndex < caseCount) {
        getIntCondCaseArray(table->cases, fixup->tableIndex)->target = target;
        return;
    }
    if (fixup->tableIndex == caseCount) {
        table->default_target = target;
        return;
    }

    cant_happen("int cond index %u out of range for table %u",
                fixup->tableIndex, tableBase + fixup->tableId);
}

/** Patches one entry in a character cond table. */
static void patchCharCondFixup(BBuffer *buffer, BLayout *layout,
                               Index tableBase, CondFixup *fixup) {
    CharCondSwitch *table =
        getCharCondTable(buffer->charConds, tableBase + fixup->tableId);
    Index caseCount = countCharCondCaseArray(table->cases);
    Index target = resolveGlobalLabel(layout, fixup->label);

    if (fixup->tableIndex < caseCount) {
        getCharCondCaseArray(table->cases, fixup->tableIndex)->target = target;
        return;
    }
    if (fixup->tableIndex == caseCount) {
        table->default_target = target;
        return;
    }

    cant_happen("char cond index %u out of range for table %u",
                fixup->tableIndex, tableBase + fixup->tableId);
}

/** Patches MATCHCOND fixups from one appended buffer. */
static void patchCondFixups(BBuffer *buffer, BLayout *layout, Index fixupBase,
                            Index intBase, Index charBase, Index matchBase) {
    for (Index i = fixupBase; i < countBFixupArray(buffer->fixups); i++) {
        BFixup *fixup = getBFixupArray(buffer->fixups, i);

        if (isBFixup_IntCond(fixup)) {
            patchIntCondFixup(buffer, layout, intBase,
                              getBFixup_IntCond(fixup));
        } else if (isBFixup_CharCond(fixup)) {
            patchCharCondFixup(buffer, layout, charBase,
                               getBFixup_CharCond(fixup));
        } else if (isBFixup_MatchCond(fixup)) {
            CondFixup *condFixup = getBFixup_MatchCond(fixup);
            Index tableId = matchBase + condFixup->tableId;
            IndexArray *table = getMatchTable(buffer->matches, tableId);

            if (condFixup->tableIndex >= countIndexArray(table)) {
                cant_happen("match table index %u out of range for table %u",
                            condFixup->tableIndex, tableId);
            }
            pokeIndexArray(table, condFixup->tableIndex,
                           resolveGlobalLabel(layout, condFixup->label));
        }
    }
}

/** Applies the currently supported fixups to the flattened buffer. */
static void patchFixups(BAssemblyPlan *plan, BBuffer *buffer, BLayout *layout) {
    for (Index i = 0; i < countBFixupArray(buffer->fixups); i++) {
        BFixup *fixup = getBFixupArray(buffer->fixups, i);

        if (isBFixup_Code(fixup)) {
            patchCodeFixup(buffer, layout, getBFixup_Code(fixup));
        } else if (isBFixup_IntTable(fixup)) {
            patchIntTableFixup(plan, buffer, layout, getBFixup_IntTable(fixup));
        } else if (isBFixup_CharTable(fixup)) {
            patchCharTableFixup(plan, buffer, layout,
                                getBFixup_CharTable(fixup));
        } else if (isBFixup_MatchTable(fixup)) {
            patchMatchTableFixup(plan, buffer, layout,
                                 getBFixup_MatchTable(fixup));
        }
    }
}

/** Builds the code layout used for later patching. */
BLayout *layoutBAssemblyPlan(BAssemblyPlan *plan) {
    if (plan == NULL) {
        return NULL;
    }

    BLayout *layout = newBLayout(0);
    int save = PROTECT(layout);
    layoutBufferBases(plan, layout);
    layoutGlobalLabels(plan, layout);
    if (!getIndexMap(layout->globalLabels, plan->entryLabel,
                     &layout->entryPoint)) {
        cant_happen("missing entry label %s in BLayout",
                    plan->entryLabel->name);
    }
    UNPROTECT(save);
    return layout;
}

/** Flattens the plan buffers and patches resolved operands. */
BBuffer *assembleBAssemblyPlan(BAssemblyPlan *plan) {
    if (plan == NULL) {
        return NULL;
    }

    int save = STARTPROTECT();
    BLayout *layout = layoutBAssemblyPlan(plan);
    PROTECT(layout);
    BBuffer *final = bemitter_newBuffer();
    PROTECT(final);
    final->constants = plan->constants;

    for (Index i = 0; i < countSymbolArray(plan->order); i++) {
        HashSymbol *label = getSymbolArray(plan->order, i);
        BBuffer *buffer = getPlanBuffer(plan, label);
        Index fixupBase = countBFixupArray(final->fixups);
        Index intBase = countIntCondTable(final->intConds);
        Index charBase = countCharCondTable(final->charConds);
        Index matchBase = countMatchTable(final->matches);

        bemit_buffer_label(final, label, bemitter_buffer_pos(final));
        bemitter_append(final, buffer, 0);
        patchCondFixups(final, layout, fixupBase, intBase, charBase, matchBase);
    }

    patchFixups(plan, final, layout);

    UNPROTECT(save);
    return final;
}
