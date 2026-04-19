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

static BBuffer *getPlanBuffer(BAssemblyPlan *plan, HashSymbol *label) {
    BBuffer *buffer = NULL;
    if (!getBBufferBag(plan->buffers, label, &buffer)) {
        cant_happen("missing BBuffer for %s in BAssemblyPlan", label->name);
    }
    return buffer;
}

static void addGlobalLabel(BLayout *layout, HashSymbol *label, Index location) {
    if (getIndexMap(layout->globalLabels, label, NULL)) {
        cant_happen("duplicate global label %s in BLayout", label->name);
    }
    setIndexMap(layout->globalLabels, label, location);
}

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

static void patchCodeFixup(BBuffer *buffer, BLayout *layout, CodeFixup *fixup) {
    Index target = 0;

    if (!getIndexMap(layout->globalLabels, fixup->label, &target)) {
        cant_happen("missing global label %s in BLayout", fixup->label->name);
    }
    pokeUIntArray(buffer->codes, fixup->location, (UInteger)target);
}

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

static Index getIntCondBase(BAssemblyPlan *plan, HashSymbol *bufferLabel) {
    Index base = 0;

    for (Index i = 0; i < countSymbolArray(plan->order); i++) {
        HashSymbol *label = getSymbolArray(plan->order, i);
        BBuffer *buffer = getPlanBuffer(plan, label);

        if (label == bufferLabel) {
            return base;
        }
        base += countIntCondTable(buffer->intConds);
    }

    cant_happen("missing buffer %s in BAssemblyPlan order", bufferLabel->name);
}

static void patchIntTableFixup(BAssemblyPlan *plan, BBuffer *buffer,
                               BLayout *layout, TableFixup *fixup) {
    HashSymbol *bufferLabel =
        findBufferLabelForLocation(plan, layout, fixup->location);
    Index tableId = getIntCondBase(plan, bufferLabel) + fixup->tableId;

    pokeUIntArray(buffer->codes, fixup->location, (UInteger)tableId);
}

static void patchFixups(BAssemblyPlan *plan, BBuffer *buffer, BLayout *layout) {
    for (Index i = 0; i < countBFixupArray(buffer->fixups); i++) {
        BFixup *fixup = getBFixupArray(buffer->fixups, i);

        if (isBFixup_Code(fixup)) {
            patchCodeFixup(buffer, layout, getBFixup_Code(fixup));
        } else if (isBFixup_IntTable(fixup)) {
            patchIntTableFixup(plan, buffer, layout, getBFixup_IntTable(fixup));
        }
    }
}

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
        bemit_buffer_label(final, label, bemitter_buffer_pos(final));
        bemitter_append(final, buffer, 0);
    }

    patchFixups(plan, final, layout);

    UNPROTECT(save);
    return final;
}
