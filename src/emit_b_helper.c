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
 *
 * Minimal AST after desugaring
 * Generated from src/minlam.yaml by tools/generate.py
 */

#include "emit_b_helper.h"
#include "types.h"
#include <stdarg.h>

Index bemitter_buffer_pos(BBuffer *body) { return body->codes->size; }

Index bemitter_pos(BEmitterContext *context) {
    return bemitter_buffer_pos(context->body);
}

void bemit_comment(BEmitterContext *context, SCharArray *msg) {
    BComment *comment = newBComment(bemitter_pos(context), msg);
    int save = PROTECT(comment);
    pushBCommentArray(context->body->comments, comment);
    UNPROTECT(save);
}

void bemit_word(BEmitterContext *context, UInteger word) {
    pushUIntArray(context->body->codes, word);
}

void bemit_code(BEmitterContext *context, BBC code, int a1, int a2, int a3) {
#ifdef SAFETY_CHECKS
    if ((code & 0xFF) != code)
        cant_happen("bytecode instruction out of range: %s", bBCName(code));
#endif
    bool unpack = (a1 & 0xFF) != a1 || (a2 & 0xFF) != a2 || (a3 & 0xFF) != a3;
    if (unpack) {
        bemit_word(context, BBC_TYPE_EXT);
        bemit_word(context, code);
        bemit_word(context, a1);
        bemit_word(context, a2);
        bemit_word(context, a3);
    } else {
        UInteger word = (code & 0xFF) | ((a1 & 0xFF) << 8) |
                        ((a2 & 0xFF) << 16) | ((a3 & 0xFF) << 24);
        bemit_word(context, word);
    }
}

void bemit_location(BEmitterContext *context, ParserInfo PI) {
    BLocation *loc =
        newBLocation(bemitter_pos(context), PI.fileName, PI.lineNo);
    int save = PROTECT(loc);
    pushBLocationArray(context->body->locations, loc);
    UNPROTECT(save);
}

void bemit_buffer_label(BBuffer *body, HashSymbol *label, Index loc) {
#ifdef SAFETY_CHECKS
    if (getIndexMap(body->labels, label, NULL))
        cant_happen("redefinition of label %s", label->name);
#endif
    setIndexMap(body->labels, label, loc);
}

void bemit_label(BEmitterContext *context, HashSymbol *label, Index loc) {
    bemit_buffer_label(context->body, label, loc);
}

void bemit_fixup_code(BEmitterContext *context, HashSymbol *label, Index loc) {
    BFixup *fixup = makeBFixup_Code(label, loc);
    int save = PROTECT(fixup);
    pushBFixupArray(context->body->fixups, fixup);
    UNPROTECT(save);
}

void bemit_fixup_inttable(BEmitterContext *context, Index tableId, Index loc) {
    BFixup *fixup = makeBFixup_IntTable(tableId, loc);
    int save = PROTECT(fixup);
    pushBFixupArray(context->body->fixups, fixup);
    UNPROTECT(save);
}

void bemit_fixup_chartable(BEmitterContext *context, Index tableId, Index loc) {
    BFixup *fixup = makeBFixup_CharTable(tableId, loc);
    int save = PROTECT(fixup);
    pushBFixupArray(context->body->fixups, fixup);
    UNPROTECT(save);
}

void bemit_fixup_matchtable(BEmitterContext *context, Index tableId,
                            Index loc) {
    BFixup *fixup = makeBFixup_MatchTable(tableId, loc);
    int save = PROTECT(fixup);
    pushBFixupArray(context->body->fixups, fixup);
    UNPROTECT(save);
}

BBuffer *bemitter_newBuffer() {
    UIntArray *codes = newUIntArray();
    int save = PROTECT(codes);
    BFixupArray *fixups = newBFixupArray();
    PROTECT(fixups);
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
    BBuffer *body = newBBuffer(codes, fixups, locations, comments, intConds,
                               charConds, matches);
    UNPROTECT(save);
    return body;
}

BEmitterContext *bemitter_newContext(EmitterContext ec) {
    BBuffer *body = bemitter_newBuffer();
    int save = PROTECT(body);
    BConstantArray *constants = newBConstantArray();
    PROTECT(constants);
    BEmitterContext *context = newBEmitterContext(body, ec, constants);
    UNPROTECT(save);
    return context;
}

// this just makes the fixup global, it doesn't apply it
// adding bufferId won't work - FIXME LATER
static void fixUpFix(BFixup *fixup, Index start, Index bufferId) {
    switch (fixup->type) {
    case BFIXUP_TYPE_CODE:
        getBFixup_Code(fixup)->location += start;
        break;
    case BFIXUP_TYPE_CHARCOND:
        getBFixup_CharCond(fixup)->tableId += bufferId;
        break;
    case BFIXUP_TYPE_INTCOND:
        getBFixup_IntCond(fixup)->tableId += bufferId;
        break;
    case BFIXUP_TYPE_MATCHCOND:
        getBFixup_MatchCond(fixup)->tableId += bufferId;
        break;
    case BFIXUP_TYPE_CHARTABLE:
        getBFixup_CharTable(fixup)->location += start;
        getBFixup_CharTable(fixup)->tableId += bufferId;
        break;
    case BFIXUP_TYPE_INTTABLE:
        getBFixup_IntTable(fixup)->location += start;
        getBFixup_IntTable(fixup)->tableId += bufferId;
        break;
    case BFIXUP_TYPE_MATCHTABLE:
        getBFixup_MatchTable(fixup)->location += start;
        getBFixup_MatchTable(fixup)->tableId += bufferId;
        break;
    }
}

void bemitter_append(BBuffer *to, BBuffer *from, Index bufferId) {
    Index start = to->codes->size;
    Index i = 0;
    Index loc = 0;
    HashSymbol *label = NULL;
    // might consider extend + memcpy later
    for (i = 0; i < from->codes->size; ++i) {
        pushUIntArray(to->codes, from->codes->entries[i]);
    }
    i = 0;
    while ((label = iterateIndexMap(from->labels, &i, &loc))) {
#ifdef SAFETY_CHECKS
        if (getIndexMap(to->labels, label, NULL))
            cant_happen("duplicate label %s during append", label->name);
#endif
        setIndexMap(to->labels, label, loc + start);
    }
    for (i = 0; i < from->fixups->size; i++) {
        BFixup *fixup = copyBFixup(from->fixups->entries[i]);
        int save = PROTECT(fixup);
        fixUpFix(fixup, start, bufferId);
        pushBFixupArray(to->fixups, fixup);
        UNPROTECT(save);
    }
    for (i = 0; i < from->locations->size; i++) {
        BLocation *location = copyBLocation(from->locations->entries[i]);
        int save = PROTECT(location);
        location->index += start;
        pushBLocationArray(to->locations, location);
        UNPROTECT(save);
    }
    for (i = 0; i < from->comments->size; i++) {
        BComment *comment = copyBComment(from->comments->entries[i]);
        int save = PROTECT(comment);
        comment->index += start;
        pushBCommentArray(to->comments, comment);
        UNPROTECT(save);
    }
    for (i = 0; i < from->charConds->size; i++) {
        CharCondSwitch *charCondSwitch =
            copyCharCondSwitch(from->charConds->entries[i]);
        int save = PROTECT(charCondSwitch);
        charCondSwitch->default_target += start;
        for (Index j = 0; j < charCondSwitch->cases->size; j++) {
            charCondSwitch->cases->entries[j]->target += start;
        }
        pushCharCondTable(to->charConds, charCondSwitch);
        UNPROTECT(save);
    }
    for (i = 0; i < from->intConds->size; i++) {
        IntCondSwitch *intCondSwitch =
            copyIntCondSwitch(from->intConds->entries[i]);
        int save = PROTECT(intCondSwitch);
        intCondSwitch->default_target += start;
        for (Index j = 0; j < intCondSwitch->cases->size; j++) {
            intCondSwitch->cases->entries[j]->target += start;
        }
        pushIntCondTable(to->intConds, intCondSwitch);
        UNPROTECT(save);
    }
    for (i = 0; i < from->matches->size; i++) {
        IndexArray *indexArray = copyIndexArray(from->matches->entries[i]);
        int save = PROTECT(indexArray);
        for (Index j = 0; j < indexArray->size; j++) {
            indexArray->entries[j] += start;
        }
        pushMatchTable(to->matches, indexArray);
        UNPROTECT(save);
    }
}