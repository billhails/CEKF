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

#include "emit_b_assembler.h"

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

/** Returns the number of tables of one family in a buffer. */
static Index countTableFamily(BBuffer *buffer, BFixupType type) {
    switch (type) {
    case BFIXUP_TYPE_INTTABLE:
        return countIntCondTable(buffer->intConds);
    case BFIXUP_TYPE_CHARTABLE:
        return countCharCondTable(buffer->charConds);
    case BFIXUP_TYPE_MATCHTABLE:
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

/** Appends a buffer's code words to the linked image. */
static void appendBufferCodes(BLinkedImage *image, BBuffer *buffer) {
    for (Index i = 0; i < countUIntArray(buffer->codes); i++) {
        pushUIntArray(image->codes, getUIntArray(buffer->codes, i));
    }
}

/** Appends rebased source locations to the linked image. */
static void appendBufferLocations(BLinkedImage *image, BBuffer *buffer,
                                  Index codeBase) {
    for (Index i = 0; i < countBLocationArray(buffer->locations); i++) {
        BLocation *location =
            copyBLocation(getBLocationArray(buffer->locations, i));
        int save = PROTECT(location);
        location->index += codeBase;
        pushBLocationArray(image->locations, location);
        UNPROTECT(save);
    }
}

/** Appends rebased comments to the linked image. */
static void appendBufferComments(BLinkedImage *image, BBuffer *buffer,
                                 Index codeBase) {
    for (Index i = 0; i < countBCommentArray(buffer->comments); i++) {
        BComment *comment = copyBComment(getBCommentArray(buffer->comments, i));
        int save = PROTECT(comment);
        comment->index += codeBase;
        pushBCommentArray(image->comments, comment);
        UNPROTECT(save);
    }
}

/** Appends rebased integer cond tables to the linked image. */
static void appendBufferIntConds(BLinkedImage *image, BBuffer *buffer,
                                 Index codeBase) {
    for (Index i = 0; i < countIntCondTable(buffer->intConds); i++) {
        IntCondSwitch *table =
            copyIntCondSwitch(getIntCondTable(buffer->intConds, i));
        int save = PROTECT(table);
        table->default_target += codeBase;
        for (Index j = 0; j < countIntCondCaseArray(table->cases); j++) {
            getIntCondCaseArray(table->cases, j)->target += codeBase;
        }
        pushIntCondTable(image->intConds, table);
        UNPROTECT(save);
    }
}

/** Appends rebased character cond tables to the linked image. */
static void appendBufferCharConds(BLinkedImage *image, BBuffer *buffer,
                                  Index codeBase) {
    for (Index i = 0; i < countCharCondTable(buffer->charConds); i++) {
        CharCondSwitch *table =
            copyCharCondSwitch(getCharCondTable(buffer->charConds, i));
        int save = PROTECT(table);
        table->default_target += codeBase;
        for (Index j = 0; j < countCharCondCaseArray(table->cases); j++) {
            getCharCondCaseArray(table->cases, j)->target += codeBase;
        }
        pushCharCondTable(image->charConds, table);
        UNPROTECT(save);
    }
}

/** Appends rebased match tables to the linked image. */
static void appendBufferMatches(BLinkedImage *image, BBuffer *buffer,
                                Index codeBase) {
    for (Index i = 0; i < countMatchTable(buffer->matches); i++) {
        IndexArray *table = copyIndexArray(getMatchTable(buffer->matches, i));
        int save = PROTECT(table);
        for (Index j = 0; j < countIndexArray(table); j++) {
            pokeIndexArray(table, j, getIndexArray(table, j) + codeBase);
        }
        pushMatchTable(image->matches, table);
        UNPROTECT(save);
    }
}

/** Patches one code operand with its resolved label address. */
static void patchCodeFixup(BLinkedImage *image, BLayout *layout, Index codeBase,
                           CodeFixup *fixup) {
    pokeUIntArray(image->codes, codeBase + fixup->location,
                  (UInteger)resolveGlobalLabel(layout, fixup->label));
}

/** Patches one table operand in the linked image. */
static void patchTableFixup(BAssemblyPlan *plan, BLinkedImage *image,
                            HashSymbol *bufferLabel, Index codeBase,
                            TableFixup *fixup, BFixupType type) {
    Index tableId = getTableBase(plan, bufferLabel, type) + fixup->tableId;
    pokeUIntArray(image->codes, codeBase + fixup->location, (UInteger)tableId);
}

/** Patches fixups for one source buffer in the linked image. */
static void patchBufferFixups(BAssemblyPlan *plan, BLayout *layout,
                              BLinkedImage *image, HashSymbol *bufferLabel,
                              BBuffer *buffer, Index codeBase) {
    for (Index i = 0; i < countBFixupArray(buffer->fixups); i++) {
        BFixup *fixup = getBFixupArray(buffer->fixups, i);

        if (isBFixup_Code(fixup)) {
            patchCodeFixup(image, layout, codeBase, getBFixup_Code(fixup));
        } else if (isBFixup_IntTable(fixup)) {
            patchTableFixup(plan, image, bufferLabel, codeBase,
                            getBFixup_IntTable(fixup), BFIXUP_TYPE_INTTABLE);
        } else if (isBFixup_CharTable(fixup)) {
            patchTableFixup(plan, image, bufferLabel, codeBase,
                            getBFixup_CharTable(fixup), BFIXUP_TYPE_CHARTABLE);
        } else if (isBFixup_MatchTable(fixup)) {
            patchTableFixup(plan, image, bufferLabel, codeBase,
                            getBFixup_MatchTable(fixup),
                            BFIXUP_TYPE_MATCHTABLE);
        }
    }
}

/** Appends one source buffer into the final linked image. */
static void appendBufferToImage(BAssemblyPlan *plan, BLayout *layout,
                                BLinkedImage *image, HashSymbol *bufferLabel) {
    BBuffer *buffer = getPlanBuffer(plan, bufferLabel);
    Index codeBase = 0;

    if (!getIndexMap(layout->bufferBases, bufferLabel, &codeBase)) {
        cant_happen("missing buffer base for %s in BLayout", bufferLabel->name);
    }
    if (countUIntArray(image->codes) != codeBase) {
        cant_happen("unexpected code base for %s during assembly",
                    bufferLabel->name);
    }

    appendBufferCodes(image, buffer);
    appendBufferLocations(image, buffer, codeBase);
    appendBufferComments(image, buffer, codeBase);
    appendBufferIntConds(image, buffer, codeBase);
    appendBufferCharConds(image, buffer, codeBase);
    appendBufferMatches(image, buffer, codeBase);
    patchBufferFixups(plan, layout, image, bufferLabel, buffer, codeBase);
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

/** Flattens the plan buffers into a linked bytecode image. */
BLinkedImage *assembleBAssemblyPlan(BAssemblyPlan *plan) {
    if (plan == NULL) {
        return NULL;
    }

    int save = STARTPROTECT();
    BLayout *layout = layoutBAssemblyPlan(plan);
    PROTECT(layout);
    UIntArray *codes = newUIntArray();
    PROTECT(codes);
    BLocationArray *locations = newBLocationArray();
    PROTECT(locations);
    BCommentArray *comments = newBCommentArray();
    PROTECT(comments);
    IntCondTable *intConds = newIntCondTable();
    PROTECT(intConds);
    CharCondTable *charConds = newCharCondTable();
    PROTECT(charConds);
    MatchTable *matches = newMatchTable();
    PROTECT(matches);
    BLinkedImage *image =
        newBLinkedImage(codes, plan->constants, locations, comments, intConds,
                        charConds, matches, layout->entryPoint, plan->maxReg);
    PROTECT(image);

    for (Index i = 0; i < countSymbolArray(plan->order); i++) {
        appendBufferToImage(plan, layout, image,
                            getSymbolArray(plan->order, i));
    }

    UNPROTECT(save);
    return image;
}
