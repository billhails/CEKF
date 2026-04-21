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

#include "emit_b_dump.h"
#include "cekfs_debug.h"
#include "hash.h"
#include <ctype.h>
#include <string.h>

static void ppad(FILE *out) { fprintf(out, "%*s; ", 23, ""); }

static const char *opcodeDisplayName(BBC opcode) {
    const char *name = bBCName(opcode);

    if (name != NULL && strncmp(name, "BBC_TYPE_", 9) == 0) {
        return name + 9;
    }
    return name;
}

static void printReg(FILE *out, UInteger reg) { fprintf(out, "r%u", reg); }

static void printCodepoint(FILE *out, UInteger codepoint) {
    fprintf(out, "U+%04X", (unsigned int)codepoint);
    if (codepoint <= 0x7f && isprint((int)codepoint)) {
        fprintf(out, " ('%c')", (int)codepoint);
    }
}

static void dumpInlineConstantSummary(FILE *out, BConstantArray *constants,
                                      UInteger constIndex) {
    if (constants == NULL || constIndex >= (UInteger)constants->size) {
        return;
    }

    Value value = constants->entries[constIndex];
    fprintf(out, " -> ");
    switch (value.type) {
    case VALUE_TYPE_NONE:
        fprintf(out, "none");
        break;
    case VALUE_TYPE_STDINT:
        fprintf(out, "%d", getValue_Stdint(value));
        break;
    case VALUE_TYPE_STDINT_IMAG:
        fprintf(out, "%di", getValue_Stdint_imag(value));
        break;
    case VALUE_TYPE_CHARACTER:
        printCodepoint(out, getValue_Character(value));
        break;
    case VALUE_TYPE_IRRATIONAL:
        fprintf(out, "%g", getValue_Irrational(value));
        break;
    case VALUE_TYPE_IRRATIONAL_IMAG:
        fprintf(out, "%gi", getValue_Irrational_imag(value));
        break;
    default: {
        const char *typeName = valueTypeName(value.type);
        if (typeName != NULL && strncmp(typeName, "VALUE_TYPE_", 11) == 0) {
            typeName += 11;
        }
        fprintf(out, "%s", typeName);
        break;
    }
    }
}

static bool isKnownOpcode(UInteger raw) {
    switch ((BBC)raw) {
    case BBC_TYPE_CALL_BUILTIN:
    case BBC_TYPE_CHARCOND:
    case BBC_TYPE_CLOSURE_NEW:
    case BBC_TYPE_CLOSURE_SET_ENV:
    case BBC_TYPE_DONE:
    case BBC_TYPE_EXT:
    case BBC_TYPE_INTCOND:
    case BBC_TYPE_JMP_FALSE:
    case BBC_TYPE_JMP_REG:
    case BBC_TYPE_JMP:
    case BBC_TYPE_LOAD_ADDR:
    case BBC_TYPE_LOAD_CHAR:
    case BBC_TYPE_LOAD_CONST:
    case BBC_TYPE_LOAD_I32:
    case BBC_TYPE_LOAD_NONE:
    case BBC_TYPE_MAKE_VEC:
    case BBC_TYPE_MATCH:
    case BBC_TYPE_MOVE:
    case BBC_TYPE_PRIM_ADD:
    case BBC_TYPE_PRIM_CANON:
    case BBC_TYPE_PRIM_CMP:
    case BBC_TYPE_PRIM_DIV:
    case BBC_TYPE_PRIM_EQ:
    case BBC_TYPE_PRIM_GCD:
    case BBC_TYPE_PRIM_GE:
    case BBC_TYPE_PRIM_GT:
    case BBC_TYPE_PRIM_LCM:
    case BBC_TYPE_PRIM_LE:
    case BBC_TYPE_PRIM_LT:
    case BBC_TYPE_PRIM_MOD:
    case BBC_TYPE_PRIM_MUL:
    case BBC_TYPE_PRIM_NE:
    case BBC_TYPE_PRIM_POW:
    case BBC_TYPE_PRIM_SUB:
    case BBC_TYPE_PRIM_VEC:
    case BBC_TYPE_UNPROTECT:
    case BBC_TYPE_TAILCALL:
    case BBC_TYPE_VEC_GET_IMM:
    case BBC_TYPE_VEC_SET:
        return true;
    }
    return false;
}

static Index opcodeTrailingWords(BBC opcode) {
    switch (opcode) {
    case BBC_TYPE_CHARCOND:
    case BBC_TYPE_CLOSURE_NEW:
    case BBC_TYPE_INTCOND:
    case BBC_TYPE_JMP_FALSE:
    case BBC_TYPE_JMP:
    case BBC_TYPE_LOAD_ADDR:
    case BBC_TYPE_LOAD_CHAR:
    case BBC_TYPE_LOAD_I32:
    case BBC_TYPE_MATCH:
        return 1;
    default:
        return 0;
    }
}

static const char *opcodeTrailingWordName(BBC opcode) {
    switch (opcode) {
    case BBC_TYPE_CHARCOND:
    case BBC_TYPE_INTCOND:
    case BBC_TYPE_MATCH:
        return "table";
    case BBC_TYPE_CLOSURE_NEW:
    case BBC_TYPE_LOAD_ADDR:
        return "addr";
    case BBC_TYPE_JMP_FALSE:
    case BBC_TYPE_JMP:
        return "target";
    case BBC_TYPE_LOAD_CHAR:
        return "codepoint";
    case BBC_TYPE_LOAD_I32:
        return "imm32";
    default:
        return "arg";
    }
}

static void dumpLocationsAt(FILE *out, BLocationArray *locations, Index index);
static void dumpCommentsAt(FILE *out, BCommentArray *comments, Index index);
static void dumpMetadataAt(FILE *out, BFixupArray *fixups,
                           BLocationArray *locations, BCommentArray *comments,
                           Index index);

static void dumpRawWordLine(FILE *out, Index index, UInteger word,
                            const char *note) {
    fprintf(out, "%04x: .word 0x%08x", index, (unsigned int)word);
    if (note != NULL) {
        ppad(out);
        fprintf(out, "%s", note);
    }
    fprintf(out, "\n");
}

static void dumpInstructionOperands(FILE *out, BBC opcode, UInteger a1,
                                    UInteger a2, UInteger a3,
                                    BConstantArray *constants) {
    switch (opcode) {
    case BBC_TYPE_CALL_BUILTIN:
        fprintf(out, " builtin=%u dst=", (unsigned int)a1);
        printReg(out, a2);
        fprintf(out, " argv=");
        printReg(out, a3);
        break;
    case BBC_TYPE_CHARCOND:
    case BBC_TYPE_INTCOND:
    case BBC_TYPE_MATCH:
        fprintf(out, " test=");
        printReg(out, a1);
        fprintf(out, " table=<next>");
        break;
    case BBC_TYPE_CLOSURE_NEW:
    case BBC_TYPE_LOAD_ADDR:
        fprintf(out, " dst=");
        printReg(out, a1);
        fprintf(out, " addr=<next>");
        break;
    case BBC_TYPE_CLOSURE_SET_ENV:
        fprintf(out, " clo=");
        printReg(out, a1);
        fprintf(out, " env=");
        printReg(out, a2);
        break;
    case BBC_TYPE_DONE:
        fprintf(out, " status=%u", (unsigned int)a1);
        break;
    case BBC_TYPE_EXT:
        fprintf(out, " <prefix>");
        break;
    case BBC_TYPE_JMP_FALSE:
        fprintf(out, " test=");
        printReg(out, a1);
        fprintf(out, " target=<next>");
        break;
    case BBC_TYPE_JMP_REG:
        fprintf(out, " target=");
        printReg(out, a1);
        break;
    case BBC_TYPE_JMP:
        fprintf(out, " target=<next>");
        break;
    case BBC_TYPE_LOAD_CHAR:
        fprintf(out, " dst=");
        printReg(out, a1);
        fprintf(out, " codepoint=<next>");
        break;
    case BBC_TYPE_LOAD_CONST:
        fprintf(out, " dst=");
        printReg(out, a1);
        fprintf(out, " const=%u", (unsigned int)a2);
        dumpInlineConstantSummary(out, constants, a2);
        break;
    case BBC_TYPE_LOAD_I32:
        fprintf(out, " dst=");
        printReg(out, a1);
        fprintf(out, " imm32=<next>");
        break;
    case BBC_TYPE_LOAD_NONE:
        fprintf(out, " dst=");
        printReg(out, a1);
        break;
    case BBC_TYPE_MAKE_VEC:
        fprintf(out, " dst=");
        printReg(out, a1);
        fprintf(out, " count=%u", (unsigned int)a2);
        break;
    case BBC_TYPE_MOVE:
        fprintf(out, " dst=");
        printReg(out, a1);
        fprintf(out, " src=");
        printReg(out, a2);
        break;
    case BBC_TYPE_PRIM_ADD:
    case BBC_TYPE_PRIM_CANON:
    case BBC_TYPE_PRIM_CMP:
    case BBC_TYPE_PRIM_DIV:
    case BBC_TYPE_PRIM_EQ:
    case BBC_TYPE_PRIM_GCD:
    case BBC_TYPE_PRIM_GE:
    case BBC_TYPE_PRIM_GT:
    case BBC_TYPE_PRIM_LCM:
    case BBC_TYPE_PRIM_LE:
    case BBC_TYPE_PRIM_LT:
    case BBC_TYPE_PRIM_MOD:
    case BBC_TYPE_PRIM_MUL:
    case BBC_TYPE_PRIM_NE:
    case BBC_TYPE_PRIM_POW:
    case BBC_TYPE_PRIM_SUB:
    case BBC_TYPE_PRIM_VEC:
        fprintf(out, " dst=");
        printReg(out, a1);
        fprintf(out, " left=");
        printReg(out, a2);
        fprintf(out, " right=");
        printReg(out, a3);
        break;
    case BBC_TYPE_TAILCALL:
        fprintf(out, " clo=");
        printReg(out, a1);
        break;
    case BBC_TYPE_UNPROTECT:
        break;
    case BBC_TYPE_VEC_GET_IMM:
        fprintf(out, " dst=");
        printReg(out, a1);
        fprintf(out, " index=%u vec=", (unsigned int)a2);
        printReg(out, a3);
        break;
    case BBC_TYPE_VEC_SET:
        fprintf(out, " vec=");
        printReg(out, a1);
        fprintf(out, " index=%u src=", (unsigned int)a2);
        printReg(out, a3);
        break;
    }
}

static void dumpInstructionLine(FILE *out, Index index, UInteger word,
                                BBC opcode, UInteger a1, UInteger a2,
                                UInteger a3, bool unpacked,
                                BConstantArray *constants) {
    fprintf(out, "%04x: .inst 0x%08x %s", index, (unsigned int)word,
            opcodeDisplayName(opcode));
    if (unpacked) {
        fprintf(out, " [EXT]");
    }
    dumpInstructionOperands(out, opcode, a1, a2, a3, constants);
    fprintf(out, "\n");
}

static void dumpTrailingWordLine(FILE *out, Index index, BBC opcode,
                                 UInteger word) {
    switch (opcode) {
    case BBC_TYPE_CHARCOND:
    case BBC_TYPE_INTCOND:
    case BBC_TYPE_MATCH:
        fprintf(out, "%04x: .word 0x%08x ; table=%u\n", index,
                (unsigned int)word, (unsigned int)word);
        break;
    case BBC_TYPE_CLOSURE_NEW:
    case BBC_TYPE_LOAD_ADDR:
        fprintf(out, "%04x: .word 0x%08x ; addr\n", index, (unsigned int)word);
        break;
    case BBC_TYPE_JMP_FALSE:
    case BBC_TYPE_JMP:
        fprintf(out, "%04x: .word 0x%08x ; target=%u\n", index,
                (unsigned int)word, (unsigned int)word);
        break;
    case BBC_TYPE_LOAD_CHAR:
        fprintf(out, "%04x: .word 0x%08x ; codepoint=", index,
                (unsigned int)word);
        printCodepoint(out, word);
        fprintf(out, "\n");
        break;
    case BBC_TYPE_LOAD_I32:
        fprintf(out, "%04x: .word 0x%08x ; imm32=%d\n", index,
                (unsigned int)word, (int)word);
        break;
    default:
        dumpRawWordLine(out, index, word, opcodeTrailingWordName(opcode));
        break;
    }
}

static void dumpConstantValue(FILE *out, Value value) {
    printValue(out, value, 0);
}

static void dumpTableFixupAt(FILE *out, const char *name, TableFixup *fixup,
                             Index index) {
    if (fixup->location == index) {
        ppad(out);
        fprintf(out, "table fixup -> %s[%u]\n", name, fixup->tableId);
    }
}

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

    for (Index i = 0; i < fixups->size; i++) {
        BFixup *fixup = fixups->entries[i];
        switch (fixup->type) {
        case BFIXUP_TYPE_CODE:
            if (fixup->val.code->location == index) {
                ppad(out);
                fprintf(out, "fixup -> %s\n", fixup->val.code->label->name);
            }
            break;
        case BFIXUP_TYPE_CHARTABLE:
            dumpTableFixupAt(out, "charConds", fixup->val.charTable, index);
            break;
        case BFIXUP_TYPE_INTTABLE:
            dumpTableFixupAt(out, "intConds", fixup->val.intTable, index);
            break;
        case BFIXUP_TYPE_MATCHTABLE:
            dumpTableFixupAt(out, "matches", fixup->val.matchTable, index);
            break;
        default:
            cant_happen("unrecognised BFixup type %d", fixup->type);
        }
    }
}

static void dumpMetadataAt(FILE *out, BFixupArray *fixups,
                           BLocationArray *locations, BCommentArray *comments,
                           Index index) {
    dumpFixupsAt(out, fixups, index);
    dumpLocationsAt(out, locations, index);
    dumpCommentsAt(out, comments, index);
}

static void dumpLocationsAt(FILE *out, BLocationArray *locations, Index index) {
    if (locations == NULL) {
        return;
    }

    for (Index i = 0; i < locations->size; i++) {
        BLocation *location = locations->entries[i];
        if (location->index == index) {
            ppad(out);
            fprintf(out, "loc %s:%d\n", location->fileName, location->lineNo);
        }
    }
}

static void dumpCommentsAt(FILE *out, BCommentArray *comments, Index index) {
    if (comments == NULL) {
        return;
    }

    for (Index i = 0; i < comments->size; i++) {
        BComment *comment = comments->entries[i];
        if (comment->index == index) {
            ppad(out);
            printCommentText(out, comment->text);
            fprintf(out, "\n");
        }
    }
}

static void dumpCodeWords(FILE *out, UIntArray *codes,
                          BConstantArray *constants, IndexMap *labels,
                          BFixupArray *fixups, BLocationArray *locations,
                          BCommentArray *comments) {
    if (codes == NULL) {
        fprintf(out, "; <no code>\n");
        return;
    }

    for (Index i = 0; i < codes->size;) {
        UInteger word = codes->entries[i];
        UInteger rawOpcode = word & 0xffU;
        bool unpacked = false;
        BBC opcode = 0;
        UInteger a1 = 0;
        UInteger a2 = 0;
        UInteger a3 = 0;
        Index payloadWords = 0;
        Index extraWords = 0;

        dumpLabelsAt(out, labels, i);
        if (rawOpcode == BBC_TYPE_EXT) {
            unpacked = true;
            if (i + 4 >= codes->size) {
                dumpRawWordLine(out, i, word, "BBC_TYPE_EXT (truncated)");
                dumpMetadataAt(out, fixups, locations, comments, i);
                for (Index j = i + 1; j < codes->size; j++) {
                    dumpLabelsAt(out, labels, j);
                    dumpRawWordLine(out, j, codes->entries[j], "ext payload");
                    dumpMetadataAt(out, fixups, locations, comments, j);
                }
                break;
            }

            if (!isKnownOpcode(codes->entries[i + 1])) {
                dumpRawWordLine(out, i, word, "BBC_TYPE_EXT (unknown opcode)");
                dumpMetadataAt(out, fixups, locations, comments, i);
                for (Index j = 1; j <= 4; j++) {
                    static const char *notes[] = {"ext opcode", "ext a1",
                                                  "ext a2", "ext a3"};
                    dumpLabelsAt(out, labels, i + j);
                    dumpRawWordLine(out, i + j, codes->entries[i + j],
                                    notes[j - 1]);
                    dumpMetadataAt(out, fixups, locations, comments, i + j);
                }
                i += 5;
                continue;
            }

            opcode = (BBC)codes->entries[i + 1];
            a1 = codes->entries[i + 2];
            a2 = codes->entries[i + 3];
            a3 = codes->entries[i + 4];
            payloadWords = 4;
        } else if (isKnownOpcode(rawOpcode)) {
            opcode = (BBC)rawOpcode;
            a1 = (word >> 8) & 0xffU;
            a2 = (word >> 16) & 0xffU;
            a3 = (word >> 24) & 0xffU;
        } else {
            dumpRawWordLine(out, i, word, NULL);
            dumpMetadataAt(out, fixups, locations, comments, i);
            i++;
            continue;
        }

        extraWords = opcodeTrailingWords(opcode);
        dumpInstructionLine(out, i, word, opcode, a1, a2, a3, unpacked,
                            constants);
        dumpMetadataAt(out, fixups, locations, comments, i);

        if (unpacked) {
            dumpLabelsAt(out, labels, i + 1);
            dumpRawWordLine(out, i + 1, codes->entries[i + 1], "ext opcode");
            dumpMetadataAt(out, fixups, locations, comments, i + 1);
            dumpLabelsAt(out, labels, i + 2);
            dumpRawWordLine(out, i + 2, codes->entries[i + 2], "ext a1");
            dumpMetadataAt(out, fixups, locations, comments, i + 2);
            dumpLabelsAt(out, labels, i + 3);
            dumpRawWordLine(out, i + 3, codes->entries[i + 3], "ext a2");
            dumpMetadataAt(out, fixups, locations, comments, i + 3);
            dumpLabelsAt(out, labels, i + 4);
            dumpRawWordLine(out, i + 4, codes->entries[i + 4], "ext a3");
            dumpMetadataAt(out, fixups, locations, comments, i + 4);
        }

        for (Index j = 0; j < extraWords; j++) {
            Index operandIndex = i + 1 + payloadWords + j;
            if (operandIndex >= codes->size) {
                ppad(out);
                fprintf(out, "truncated %s word\n",
                        opcodeTrailingWordName(opcode));
                break;
            }
            dumpLabelsAt(out, labels, operandIndex);
            dumpTrailingWordLine(out, operandIndex, opcode,
                                 codes->entries[operandIndex]);
            dumpMetadataAt(out, fixups, locations, comments, operandIndex);
        }

        i += 1 + payloadWords + extraWords;
    }
}

static void dumpConstants(FILE *out, BConstantArray *constants) {
    if (constants == NULL || constants->size == 0) {
        return;
    }

    fprintf(out, "\n; constants\n");
    for (Index i = 0; i < constants->size; i++) {
        fprintf(out, ".const %u = ", i);
        dumpConstantValue(out, constants->entries[i]);
        fprintf(out, "\n");
    }
}

static void dumpIntCondTables(FILE *out, BConstantArray *constants,
                              IntCondTable *tables) {
    if (tables == NULL || tables->size == 0) {
        return;
    }

    fprintf(out, "\n; intConds\n");
    for (Index i = 0; i < tables->size; i++) {
        IntCondSwitch *table = tables->entries[i];
        fprintf(out, ".intcond %u default=%u\n", i, table->default_target);
        for (Index j = 0; j < table->cases->size; j++) {
            IntCondCase *condCase = table->cases->entries[j];
            fprintf(out, "    case[%u] const=%u", j, condCase->const_index);
            if (constants != NULL && condCase->const_index < constants->size) {
                fprintf(out, " (");
                dumpConstantValue(out,
                                  constants->entries[condCase->const_index]);
                fprintf(out, ")");
            }
            fprintf(out, " -> %u\n", condCase->target);
        }
    }
}

static void dumpCharCondTables(FILE *out, CharCondTable *tables) {
    if (tables == NULL || tables->size == 0) {
        return;
    }

    fprintf(out, "\n; charConds\n");
    for (Index i = 0; i < tables->size; i++) {
        CharCondSwitch *table = tables->entries[i];
        fprintf(out, ".charcond %u default=%u\n", i, table->default_target);
        for (Index j = 0; j < table->cases->size; j++) {
            CharCondCase *condCase = table->cases->entries[j];
            fprintf(out, "    case[%u] char=%d -> %u\n", j,
                    (int)condCase->codepoint, condCase->target);
        }
    }
}

static void dumpMatchTables(FILE *out, MatchTable *tables) {
    if (tables == NULL || tables->size == 0) {
        return;
    }

    fprintf(out, "\n; matches\n");
    for (Index i = 0; i < tables->size; i++) {
        IndexArray *table = tables->entries[i];
        fprintf(out, ".match %u\n", i);
        for (Index j = 0; j < table->size; j++) {
            fprintf(out, "    tag[%u] -> %u\n", j, table->entries[j]);
        }
    }
}

void dumpBBuffer(FILE *out, BBuffer *buffer) {
    if (buffer == NULL) {
        fprintf(out, "; BBuffer (NULL)\n");
        return;
    }

    fprintf(out, "; BBuffer\n");
    dumpCodeWords(out, buffer->codes, buffer->constants, buffer->labels,
                  buffer->fixups, buffer->locations, buffer->comments);
    dumpConstants(out, buffer->constants);
    dumpIntCondTables(out, buffer->constants, buffer->intConds);
    dumpCharCondTables(out, buffer->charConds);
    dumpMatchTables(out, buffer->matches);
}

void dumpBLinkedImage(FILE *out, BLinkedImage *image) {
    if (image == NULL) {
        fprintf(out, "; BLinkedImage (NULL)\n");
        return;
    }

    fprintf(out, "; BLinkedImage entry=%u, MAX_REG=%d\n", image->entryPoint,
            image->maxReg);
    dumpCodeWords(out, image->codes, image->constants, NULL, NULL,
                  image->locations, image->comments);
    dumpConstants(out, image->constants);
    dumpIntCondTables(out, image->constants, image->intConds);
    dumpCharCondTables(out, image->charConds);
    dumpMatchTables(out, image->matches);
}
