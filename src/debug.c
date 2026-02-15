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

static void loc(size_t ii, size_t *li, LocationArray *l) {
    static Location prev;
    if (l != NULL) {
        while (*li < l->size && ii > l->entries[*li]->loc) {
            (*li)++;
        }
        if (*li < l->size && ii == l->entries[*li]->loc) {
            Location *found = l->entries[*li];
            if (prev.lineNo != found->lineNo ||
                prev.fileName != found->fileName) {
                eprintf("    # %s %d", found->fileName, found->lineNo);
                prev = *found;
            }
        }
    }
    eprintf("\n");
}

void dumpByteCode(ByteCodeArray *b, LocationArray *l) {
    size_t i = 0;
    size_t li = 0;
    while (i < b->size) {
        size_t ii = i;
        eprintf("%04lx ", ii);
        int thisByte;
        switch (thisByte = readByte(b, &i)) {
        case BYTECODES_TYPE_NONE: {
            eprintf("NONE");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_LAM: {
            int nArgs = readByte(b, &i);
            int letRecOffset = readByte(b, &i);
            int offset = readOffset(b, &i);
            eprintf("LAM [%d][%d][%04x]", nArgs, letRecOffset, offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_VAR: {

#ifdef SIXTEEN_BIT_ENVIRONMENT
            int frame = readShort(b, &i);
            int offset = readShort(b, &i);
#else
            int frame = readByte(b, &i);
            int offset = readByte(b, &i);
#endif
            eprintf("VAR [%d:%d]", frame, offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_LVAR: {
#ifdef SIXTEEN_BIT_ENVIRONMENT
            int offset = readShort(b, &i);
#else
            int offset = readByte(b, &i);
#endif
            eprintf("LVAR [%d]", offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_ADD: {
            eprintf("ADD");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_SUB: {
            eprintf("SUB");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_MUL: {
            eprintf("MUL");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_DIV: {
            eprintf("DIV");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_POW: {
            eprintf("POW");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_MOD: {
            eprintf("MOD");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_EQ: {
            eprintf("EQ");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_NE: {
            eprintf("NE");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_GT: {
            eprintf("GT");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_LT: {
            eprintf("LT");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_GE: {
            eprintf("GE");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_LE: {
            eprintf("LE");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_CMP: {
            eprintf("CMP");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_MAKEVEC: {
            int size = readByte(b, &i);
            eprintf("MAKEVEC [%d]", size);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PRIM_VEC: {
            eprintf("VEC");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_APPLY: {
            int nArgs = readByte(b, &i);
            eprintf("APPLY [%d]", nArgs);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_IF: {
            int offset = readOffset(b, &i);
            eprintf("IF [%04x]", offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_MATCH: {
            int count = readByte(b, &i);
            eprintf("MATCH [%d]", count);
            while (count > 0) {
                int offset = readOffset(b, &i);
                eprintf("[%04x]", offset);
                count--;
            }
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_CHARCOND: {
            int count = readWord(b, &i);
            eprintf("CHARCOND [%d]", count);
            while (count > 0) {
                int val = readInteger(b, &i);
                int offset = readOffset(b, &i);
                eprintf(" %d:[%04x]", val, offset);
                count--;
            }
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_INTCOND: {
            int count = readWord(b, &i);
            eprintf("INTCOND [%d]", count);
            while (count > 0) {
                int type = readByte(b, &i);
                switch (type) {
                case BYTECODES_TYPE_BIGINT: {
                    bigint bi = readBigint(b, &i);
                    eprintf(" [bigint]");
                    bigint_fprint(errout, &bi);
                    bigint_free(&bi);
                } break;
                case BYTECODES_TYPE_STDINT: {
                    int li = readInteger(b, &i);
                    eprintf(" [int]%d", li);
                } break;
                default:
                    cant_happen("expected INT or BIGINT in "
                                "BYTECODES_TYPE_INTCOND cases");
                }
                int offset = readOffset(b, &i);
                eprintf(":[%04x]", offset);
                count--;
            }
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_LETREC: {
            int size = readByte(b, &i);
            eprintf("LETREC [%d]", size);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_AMB: {
            int offset = readOffset(b, &i);
            eprintf("AMB [%04x]", offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_CUT: {
            eprintf("CUT");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_BACK: {
            eprintf("BACK");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_LET: {
            int offset = readOffset(b, &i);
            eprintf("LET [%04x]", offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_JMP: {
            int offset = readOffset(b, &i);
            eprintf("JMP [%04x]", offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_PUSHN: {
            int size = readByte(b, &i);
            eprintf("PUSHN [%d]", size);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_CALLCC: {
            eprintf("CALLCC");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_STDINT_IMAG: {
            int val = readInteger(b, &i);
            eprintf("STDINT_IMAG [%d]", val);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_STDINT: {
            int val = readInteger(b, &i);
            eprintf("STDINT [%d]", val);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_BIGINT: {
            eprintf("BIGINT [");
            bigint bi = readBigint(b, &i);
            bigint_fprint(errout, &bi);
            eprintf("]");
            bigint_free(&bi);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_BIGINT_IMAG: {
            eprintf("BIGINT_IMAG [");
            bigint bi = readBigint(b, &i);
            bigint_fprint(errout, &bi);
            eprintf("]");
            loc(ii, &li, l);
            bigint_free(&bi);
        } break;
        case BYTECODES_TYPE_IRRATIONAL: {
            Double f = readDouble(b, &i);
            eprintf("IRRATIONAL [%f]", f);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_IRRATIONAL_IMAG: {
            Double f = readDouble(b, &i);
            eprintf("IRRATIONAL_IMAG [%f]", f);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_CHAR: {
            Character c = readCharacter(b, &i);
            eprintf("CHAR [%s]", charRep(c));
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_RETURN: {
            eprintf("RETURN");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_DONE: {
            eprintf("DONE");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_ERROR: {
            eprintf("ERROR");
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_NS_START: {
            int count = readWord(b, &i);
            eprintf("NS_START [%d]", count);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_NS_END: {
            int numLambdas = readWord(b, &i);
            int stackOffset = readWord(b, &i);
            eprintf("NS_END [%d][%d]", numLambdas, stackOffset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_NS_FINISH: {
            int count = readWord(b, &i);
            eprintf("NS_FINISH [%d]", count);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_NS_PUSHSTACK: {
            int offset = readWord(b, &i);
            eprintf("NS_PUSHSTACK [%d]", offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_NS_PUSHENV: {
            int frame = readWord(b, &i);
            int offset = readWord(b, &i);
            eprintf("NS_PUSHENV [%d][%d]", frame, offset);
            loc(ii, &li, l);
        } break;
        case BYTECODES_TYPE_NS_POP: {
            eprintf("NS_POP");
            loc(ii, &li, l);
        } break;
        default:
            cant_happen("unrecognised bytecode %s in dumpByteCode",
                        byteCodesName(thisByte));
        }
    }
}
