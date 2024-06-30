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

#include "common.h"
#include "debug.h"
#include "hash.h"
#include "builtins_debug.h"

void dumpByteCode(ByteCodeArray *bca) {
    size_t i = 0;
    while (i < bca->size) {
        eprintf("%04lx ### ", i);
        int thisByte;
        switch (thisByte = readByte(bca, &i)) {
            case BYTECODES_TYPE_NONE:{
                    eprintf("NONE\n");
                }
                break;
            case BYTECODES_TYPE_LAM:{
                    int nargs = readByte(bca, &i);
                    int letRecOffset = readByte(bca, &i);
                    int offset = readOffset(bca, &i);
                    eprintf("LAM [%d][%d][%04x]\n", nargs, letRecOffset,
                            offset);
                }
                break;
            case BYTECODES_TYPE_VAR:{
                    int frame = readByte(bca, &i);
                    int offset = readByte(bca, &i);
                    eprintf("VAR [%d:%d]\n", frame, offset);
                }
                break;
            case BYTECODES_TYPE_LVAR:{
                    int offset = readByte(bca, &i);
                    eprintf("LVAR [%d]\n", offset);
                }
                break;
            case BYTECODES_TYPE_PRIM_ADD:{
                    eprintf("ADD\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_SUB:{
                    eprintf("SUB\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_MUL:{
                    eprintf("MUL\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_DIV:{
                    eprintf("DIV\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_POW:{
                    eprintf("POW\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_MOD:{
                    eprintf("MOD\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_EQ:{
                    eprintf("EQ\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_NE:{
                    eprintf("NE\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_GT:{
                    eprintf("GT\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_LT:{
                    eprintf("LT\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_GE:{
                    eprintf("GE\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_LE:{
                    eprintf("LE\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_CMP:{
                    eprintf("CMP\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_XOR:{
                    eprintf("XOR\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_NOT:{
                    eprintf("NOT\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_MAKEVEC:{
                    int size = readByte(bca, &i);
                    eprintf("MAKEVEC [%d]\n", size);
                }
                break;
            case BYTECODES_TYPE_PRIM_VEC:{
                    eprintf("VEC\n");
                }
                break;
            case BYTECODES_TYPE_APPLY:{
                    int nargs = readByte(bca, &i);
                    eprintf("APPLY [%d]\n", nargs);
                }
                break;
            case BYTECODES_TYPE_IF:{
                    int offset = readOffset(bca, &i);
                    eprintf("IF [%04x]\n", offset);
                }
                break;
            case BYTECODES_TYPE_MATCH:{
                    int count = readByte(bca, &i);
                    eprintf("MATCH [%d]", count);
                    while (count > 0) {
                        int offset = readOffset(bca, &i);
                        eprintf("[%04x]", offset);
                        count--;
                    }
                    eprintf("\n");
                }
                break;
            case BYTECODES_TYPE_CHARCOND:{
                    int count = readWord(bca, &i);
                    eprintf("CHARCOND [%d]", count);
                    while (count > 0) {
                        int val = readInteger(bca, &i);
                        int offset = readOffset(bca, &i);
                        eprintf(" %d:[%04x]", val, offset);
                        count--;
                    }
                    eprintf("\n");
                }
                break;
            case BYTECODES_TYPE_INTCOND:{
                    int count = readWord(bca, &i);
                    eprintf("INTCOND [%d]", count);
                    while (count > 0) {
                        int type = readByte(bca, &i);
                        switch (type) {
                            case BYTECODES_TYPE_BIGINT: {
                                bigint bi = readBigint(bca, &i);
                                eprintf(" [bigint]");
                                bigint_fprint(errout, &bi);
                                bigint_free(&bi);
                            }
                            break;
                            case BYTECODES_TYPE_STDINT: {
                                int li = readInteger(bca, &i);
                                eprintf(" [int]%d", li);
                            }
                            break;
                            default:
                                cant_happen("expected INT or BIGINT in BYTECODES_TYPE_INTCOND cases");
                        }
                        int offset = readOffset(bca, &i);
                        eprintf(":[%04x]", offset);
                        count--;
                    }
                    eprintf("\n");
                }
                break;
            case BYTECODES_TYPE_LETREC:{
                    int size = readByte(bca, &i);
                    eprintf("LETREC [%d]\n", size);
                }
                break;
            case BYTECODES_TYPE_AMB:{
                    int offset = readOffset(bca, &i);
                    eprintf("AMB [%04x]\n", offset);
                }
                break;
            case BYTECODES_TYPE_CUT:{
                    eprintf("CUT\n");
                }
                break;
            case BYTECODES_TYPE_BACK:{
                    eprintf("BACK\n");
                }
                break;
            case BYTECODES_TYPE_LET:{
                    int offset = readOffset(bca, &i);
                    eprintf("LET [%04x]\n", offset);
                }
                break;
            case BYTECODES_TYPE_JMP:{
                    int offset = readOffset(bca, &i);
                    eprintf("JMP [%04x]\n", offset);
                }
                break;
            case BYTECODES_TYPE_PUSHN:{
                    int size = readByte(bca, &i);
                    eprintf("PUSHN [%d]\n", size);
                }
                break;
            case BYTECODES_TYPE_CALLCC:{
                    eprintf("CALLCC\n");
                }
                break;
            case BYTECODES_TYPE_TRUE:{
                    eprintf("TRUE\n");
                }
                break;
            case BYTECODES_TYPE_FALSE:{
                    eprintf("FALSE\n");
                }
                break;
            case BYTECODES_TYPE_VOID:{
                    eprintf("VOID\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_PUTC:{
                    eprintf("PUTC\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_PUTN:{
                    eprintf("PUTN\n");
                }
                break;
            case BYTECODES_TYPE_PRIM_PUTV:{
                    eprintf("PUTV\n");
                }
                break;
            case BYTECODES_TYPE_STDINT:{
                    int val = readInteger(bca, &i);
                    eprintf("STDINT [%d]\n", val);
                }
                break;
            case BYTECODES_TYPE_BIGINT:{
                    eprintf("BIGINT [");
                    bigint bi = readBigint(bca, &i);
                    bigint_fprint(errout, &bi);
                    eprintf("]\n");
                    bigint_free(&bi);
                }
                break;
            case BYTECODES_TYPE_IRRATIONAL:{
                Double f = readDouble(bca, &i);
                eprintf("IRRATIONAL [%f]\n", f);
            }
            break;
            case BYTECODES_TYPE_CHAR:{
                    Character c = readCharacter(bca, &i);
                    eprintf("CHAR [%s]\n", charRep(c));
                }
                break;
            case BYTECODES_TYPE_RETURN:{
                    eprintf("RETURN\n");
                }
                break;
            case BYTECODES_TYPE_DONE:{
                    eprintf("DONE\n");
                }
                break;
            case BYTECODES_TYPE_ERROR:{
                    eprintf("ERROR\n");
                }
                break;
            case BYTECODES_TYPE_NS_START:{
                    int count = readWord(bca, &i);
                    eprintf("NS_START [%d]\n", count);
                }
                break;
            case BYTECODES_TYPE_NS_END:{
                    int numLambdas = readWord(bca, &i);
                    int stackOffset = readWord(bca, &i);
                    eprintf("NS_END [%d][%d]\n", numLambdas, stackOffset);
                }
                break;
            case BYTECODES_TYPE_NS_FINISH:{
                    int count = readWord(bca, &i);
                    eprintf("NS_FINISH [%d]\n", count);
                }
                break;
            case BYTECODES_TYPE_NS_PUSHS:{
                    int offset = readWord(bca, &i);
                    eprintf("NS_PUSHS [%d]\n", offset);
                }
                break;
            case BYTECODES_TYPE_NS_PUSHE:{
                    int frame = readWord(bca, &i);
                    int offset = readWord(bca, &i);
                    eprintf("NS_PUSHE [%d][%d]\n", frame, offset);
                }
                break;
            case BYTECODES_TYPE_NS_POP:{
                    eprintf("NS_POP\n");
                }
                break;
            default:
                cant_happen("unrecognised bytecode %d in dumpByteCode",
                            thisByte);
        }
    }
}
