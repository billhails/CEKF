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

#include "minlam_dump_b.h"

#include "hash.h"

static void printCommentText(FILE *out, SCharArray *text) {
    if (text == NULL) {
        return;
    }
    for (Index i = 0; i < text->size; i++) {
        char ch = text->entries[i];
        if (ch == '\0') {
            break;
        }
        fputc(ch, out);
    }
}

static void dumpLabelsAt(FILE *out, IndexMap *labels, Index index) {
    if (labels == NULL) {
        return;
    }

    Index iter = 0;
    Index labelIndex = 0;
    HashSymbol *label = NULL;
    while ((label = iterateIndexMap(labels, &iter, &labelIndex)) != NULL) {
        if (labelIndex == index) {
            fprintf(out, "%s:\n", label->name);
        }
    }
}

static void dumpFixupsAt(FILE *out, BFixupArray *fixups, Index index) {
    if (fixups == NULL) {
        return;
    }

    for (Index i = 0; i < countBFixupArray(fixups); i++) {
        BFixup *fixup = fixups->entries[i];
        if (fixup->index == index) {
            fprintf(out, "        ; fixup -> %s\n", fixup->label->name);
        }
    }
}

static void dumpLocationsAt(FILE *out, BLocationArray *locations, Index index) {
    if (locations == NULL) {
        return;
    }

    for (Index i = 0; i < countBLocationArray(locations); i++) {
        BLocation *location = locations->entries[i];
        if (location->index == index) {
            fprintf(out, "        ; loc %s:%d\n", location->fileName,
                    location->lineNo);
        }
    }
}

static void dumpCommentsAt(FILE *out, BCommentArray *comments, Index index) {
    if (comments == NULL) {
        return;
    }

    for (Index i = 0; i < countBCommentArray(comments); i++) {
        BComment *comment = comments->entries[i];
        if (comment->index == index) {
            fprintf(out, "        ; ");
            printCommentText(out, comment->text);
            fprintf(out, "\n");
        }
    }
}

static void dumpPackedOpcodeHint(FILE *out, UInteger word) {
    UInteger opcode = word & 0xffU;
    if (opcode >= BBC_TYPE_CALL_BUILTIN && opcode <= BBC_TYPE_VEC_SET) {
        fprintf(out, "    ; %s dst=%u a=%u b=%u", bBCName((BBC)opcode),
                (word >> 8) & 0xffU, (word >> 16) & 0xffU,
                (word >> 24) & 0xffU);
    }
}

static void dumpCodeWords(FILE *out, UIntArray *codes, IndexMap *labels,
                          BFixupArray *fixups, BLocationArray *locations,
                          BCommentArray *comments) {
    if (codes == NULL) {
        fprintf(out, "; <no code>\n");
        return;
    }

    for (Index i = 0; i < countUIntArray(codes); i++) {
        dumpLabelsAt(out, labels, i);
        fprintf(out, "%04u: .word 0x%08x", i, (unsigned int)codes->entries[i]);
        dumpPackedOpcodeHint(out, codes->entries[i]);
        fprintf(out, "\n");
        dumpFixupsAt(out, fixups, i);
        dumpLocationsAt(out, locations, i);
        dumpCommentsAt(out, comments, i);
    }
}

void dumpBBuffer(FILE *out, BBuffer *buffer) {
    if (buffer == NULL) {
        fprintf(out, "; BBuffer (NULL)\n");
        return;
    }

    fprintf(out, "; BBuffer\n");
    dumpCodeWords(out, buffer->codes, buffer->labels, buffer->fixups,
                  buffer->locations, buffer->comments);
}

void dumpBLinkedImage(FILE *out, BLinkedImage *image) {
    if (image == NULL) {
        fprintf(out, "; BLinkedImage (NULL)\n");
        return;
    }

    fprintf(out, "; BLinkedImage entry=%u\n", image->entryPoint);
    dumpCodeWords(out, image->codes, NULL, NULL, image->locations,
                  image->comments);
}
