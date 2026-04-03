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

// Debugging support for printing various structures.

#include <stdio.h>
#include <stdlib.h>

#include "builtins_debug.h"
#include "common.h"
#include "debug.h"
#include "hash.h"

static void loc(FILE *out, size_t ii, size_t *li, LocationArray *l) {
    static Location prev;
    if (l != NULL) {
        while (*li < l->size && ii > l->entries[*li]->loc) {
            (*li)++;
        }
        if (*li < l->size && ii == l->entries[*li]->loc) {
            Location *found = l->entries[*li];
            if (prev.lineNo != found->lineNo ||
                prev.fileName != found->fileName) {
                fprintf(out, "    # %s %d", found->fileName, found->lineNo);
                prev = *found;
            }
        }
    }
    fprintf(out, "\n");
}

void dumpByteCode(FILE *out, ByteCodeArray *b, LocationArray *l) {
    size_t i = 0;
    size_t li = 0;
    while (i < b->size) {
        size_t ii = i;
        fprintf(out, "%04lx ", ii);
        int thisByte;
        switch (thisByte = readByte(b, &i)) {
        case BYTECODES_TYPE_NONE: {
            fprintf(out, "NONE");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_LAM: {
            int nArgs = readByte(b, &i);
            int letRecOffset = readWord(b, &i);
            int offset = readOffset(b, &i);
            fprintf(out, "LAM [%d][%d][%04x]", nArgs, letRecOffset, offset);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_VAR: {

#ifdef SIXTEEN_BIT_ENVIRONMENT
            int frame = readShort(b, &i);
            int offset = readShort(b, &i);
#else
            int frame = readByte(b, &i);
            int offset = readByte(b, &i);
#endif
            fprintf(out, "VAR [%d:%d]", frame, offset);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_LVAR: {
#ifdef SIXTEEN_BIT_ENVIRONMENT
            int offset = readShort(b, &i);
#else
            int offset = readByte(b, &i);
#endif
            fprintf(out, "LVAR [%d]", offset);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_ADD: {
            fprintf(out, "ADD");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_SUB: {
            fprintf(out, "SUB");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_MUL: {
            fprintf(out, "MUL");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_DIV: {
            fprintf(out, "DIV");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_GCD: {
            fprintf(out, "GCD");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_LCM: {
            fprintf(out, "LCM");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_CANON: {
            fprintf(out, "CANON");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_POW: {
            fprintf(out, "POW");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_MOD: {
            fprintf(out, "MOD");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_EQ: {
            fprintf(out, "EQ");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_NE: {
            fprintf(out, "NE");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_GT: {
            fprintf(out, "GT");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_LT: {
            fprintf(out, "LT");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_GE: {
            fprintf(out, "GE");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_LE: {
            fprintf(out, "LE");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_CMP: {
            fprintf(out, "CMP");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_MAKEVEC: {
            int size = readByte(b, &i);
            fprintf(out, "MAKEVEC [%d]", size);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_VEC: {
            fprintf(out, "VEC");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_APPLY: {
            int nArgs = readByte(b, &i);
            fprintf(out, "APPLY [%d]", nArgs);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_IF: {
            int offset = readOffset(b, &i);
            fprintf(out, "IF [%04x]", offset);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_MATCH: {
            int count = readByte(b, &i);
            fprintf(out, "MATCH [%d]", count);
            while (count > 0) {
                int offset = readOffset(b, &i);
                fprintf(out, "[%04x]", offset);
                count--;
            }
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_CHARCOND: {
            int count = readWord(b, &i);
            fprintf(out, "CHARCOND [%d]", count);
            while (count > 0) {
                int val = readInteger(b, &i);
                int offset = readOffset(b, &i);
                fprintf(out, " %d:[%04x]", val, offset);
                count--;
            }
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_INTCOND: {
            int count = readWord(b, &i);
            fprintf(out, "INTCOND [%d]", count);
            while (count > 0) {
                int type = readByte(b, &i);
                switch (type) {
                case BYTECODES_TYPE_BIGINT: {
                    bigint bi = readBigint(b, &i);
                    fprintf(out, " [bigint]");
                    bigint_fprint(out, &bi);
                    bigint_free(&bi);
                } break;
                case BYTECODES_TYPE_STDINT: {
                    int li = readInteger(b, &i);
                    fprintf(out, " [int]%d", li);
                } break;
                default:
                    cant_happen("expected INT or BIGINT in "
                                "BYTECODES_TYPE_INTCOND cases");
                }
                int offset = readOffset(b, &i);
                fprintf(out, ":[%04x]", offset);
                count--;
            }
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_LETREC: {
            int size = readWord(b, &i);
            fprintf(out, "LETREC [%d]", size);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_AMB: {
            int offset = readOffset(b, &i);
            fprintf(out, "AMB [%04x]", offset);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_CUT: {
            fprintf(out, "CUT");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_BACK: {
            fprintf(out, "BACK");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_LET: {
            int offset = readOffset(b, &i);
            fprintf(out, "LET [%04x]", offset);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_JMP: {
            int offset = readOffset(b, &i);
            fprintf(out, "JMP [%04x]", offset);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_PUSHN: {
            int size = readByte(b, &i);
            fprintf(out, "PUSHN [%d]", size);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_CALLCC: {
            fprintf(out, "CALLCC");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_STDINT_IMAG: {
            int val = readInteger(b, &i);
            fprintf(out, "STDINT_IMAG [%d]", val);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_STDINT: {
            int val = readInteger(b, &i);
            fprintf(out, "STDINT [%d]", val);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_BIGINT: {
            fprintf(out, "BIGINT [");
            bigint bi = readBigint(b, &i);
            bigint_fprint(out, &bi);
            fprintf(out, "]");
            bigint_free(&bi);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_BIGINT_IMAG: {
            fprintf(out, "BIGINT_IMAG [");
            bigint bi = readBigint(b, &i);
            bigint_fprint(out, &bi);
            fprintf(out, "]");
            loc(out, ii, &li, l);
            bigint_free(&bi);
        } break;
        case BYTECODES_TYPE_IRRATIONAL: {
            Double f = readDouble(b, &i);
            fprintf(out, "IRRATIONAL [%f]", f);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_IRRATIONAL_IMAG: {
            Double f = readDouble(b, &i);
            fprintf(out, "IRRATIONAL_IMAG [%f]", f);
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_CHAR: {
            Character c = readCharacter(b, &i);
            fprintf(out, "CHAR [%s]", charRep(c));
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_RETURN: {
            fprintf(out, "RETURN");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_DONE: {
            fprintf(out, "DONE");
            loc(out, ii, &li, l);
        } break;
        case BYTECODES_TYPE_ERROR: {
            fprintf(out, "ERROR");
            loc(out, ii, &li, l);
        } break;
        default:
            cant_happen("unrecognised bytecode %s in dumpByteCode",
                        byteCodesName(thisByte));
        }
    }
}
