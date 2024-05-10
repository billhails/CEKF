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

static void printClo(Clo *x, char *type, int depth);
static void printElidedEnv(Env *x) __attribute__((unused));
static void printEnv(Env *x, int depth);
static void printFail(Fail *x, int depth) __attribute__((unused));
static void printKont(Kont *x, int depth);
static void printStack(Stack *x, int depth) __attribute__((unused));
static void printVec(Vec *x);

static void printPad(int depth) {
    eprintf("%*s", depth * PAD_WIDTH, "");
}

void printContainedValue(Value x, int depth) {
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printPad(depth);
            eprintf("#V");
            break;
        case VALUE_TYPE_STDINT:
            printPad(depth);
            eprintf("%d", x.val.stdint);
            break;
        case VALUE_TYPE_BIGINT:
            printPad(depth);
            fprintBigInt(errout, x.val.bigint);
            break;
        case VALUE_TYPE_CHARACTER:
            printPad(depth);
            switch (x.val.character) {
                case '\n':
                    eprintf("'\\n'");
                    break;
                case '\t':
                    eprintf("'\\t'");
                    break;
                default:
                    eprintf("'%c'", x.val.character);
                    break;
            }
            break;
        case VALUE_TYPE_CLO:
            printClo(x.val.clo, "C", depth);
            break;
        case VALUE_TYPE_PCLO:
            printClo(x.val.clo, "PC", depth);
            break;
        case VALUE_TYPE_CONT:
            printKont(x.val.kont, depth);
            break;
        case VALUE_TYPE_RATIONAL:
        case VALUE_TYPE_VEC:
            printPad(depth);
            printVec(x.val.vec);
            break;
        case VALUE_TYPE_BUILTIN:
            printBuiltInImplementation(x.val.builtIn, depth);
            break;
        default:
            cant_happen("unrecognised value type %d", x.type);
    }
}

static void printSnapshot(Snapshot s, int depth) {
    printPad(depth);
    if (s.frameSize == 0) {
        eprintf("S/");
        return;
    }
    eprintf("SS[\n");
    for (int i = 0; i < s.frameSize; i++) {
        printContainedValue(s.frame[i], depth + 1);
        if (i < s.frameSize - 1) {
            eprintf(",");
        }
        eprintf("\n");
    }
    printPad(depth);
    eprintf("]");
}

static void printElidedSnapshot(Snapshot s) {
    if (s.frameSize == 0) {
        eprintf("S/");
        return;
    }
    eprintf("S[<...>]");
}

void printValue(Value x, int depth) {
    printPad(depth);
    if (x.type == VALUE_TYPE_VOID) {
        eprintf("V/");
        return;
    }
    eprintf("V[\n");
    printContainedValue(x, depth + 1);
    eprintf("\n");
    printPad(depth);
    eprintf("]");
}

void printElidedClo(Clo *x, char *type) {
    eprintf("%s[%d, %04lx, E[<...>], ", type, x->pending, x->ip);
    eprintf("]");
}

void printElidedKont(Kont *x) {
    if (x == NULL) {
        eprintf("K/");
        return;
    }
    eprintf("K[");
    eprintf("%04lx, E[<...>], ", x->body);
    printElidedSnapshot(x->snapshot);
    printElidedKont(x->next);
    eprintf("]");
}

void printVec(Vec *x) {
    eprintf("#[");
    for (int i = 0; i < x->size; i++) {
        printContainedValue(x->values[i], 0);
        if (i + 1 < x->size) {
            eprintf(" ");
        }
    }
    eprintf("]");
}

void printElidedValue(Value x) {
    eprintf("V[");
    switch (x.type) {
        case VALUE_TYPE_VOID:
            eprintf("#V");
            break;
        case VALUE_TYPE_STDINT:
            eprintf("%d", x.val.stdint);
            break;
        case VALUE_TYPE_CHARACTER:
            switch (x.val.character) {
                case '\n':
                    eprintf("'\\n'");
                    break;
                case '\t':
                    eprintf("'\\t'");
                    break;
                default:
                    eprintf("'%c'", x.val.character);
                    break;
            }
            break;
        case VALUE_TYPE_VEC:
            printVec(x.val.vec);
            break;
        case VALUE_TYPE_CLO:
            printElidedClo(x.val.clo, "C");
            break;
        case VALUE_TYPE_PCLO:
            printElidedClo(x.val.clo, "PC");
            break;
        case VALUE_TYPE_CONT:
            printElidedKont(x.val.kont);
            break;
        default:
            cant_happen("unrecognised value type in printElidedValue");
    }
    eprintf("]");
}

static void printClo(Clo *x, char *type, int depth) {
    printPad(depth);
    eprintf("%s[%d, %04lx, ", type, x->pending, x->ip);
    // printElidedEnv(x->env);
    eprintf("]");
}

static void ppValue(Value v) {
    switch (v.type) {
        case VALUE_TYPE_VOID:
            eprintf("VOI");
            break;
        case VALUE_TYPE_STDINT:
            eprintf("INT");
            break;
        case VALUE_TYPE_BIGINT:
            eprintf("BIG");
            break;
        case VALUE_TYPE_RATIONAL:
            eprintf("RAT");
            break;
        case VALUE_TYPE_IRRATIONAL:
            eprintf("IRR");
            break;
        case VALUE_TYPE_STDINT_IMAG:
            eprintf("IMA");
            break;
        case VALUE_TYPE_BIGINT_IMAG:
            eprintf("IMA");
            break;
        case VALUE_TYPE_RATIONAL_IMAG:
            eprintf("IMA");
            break;
        case VALUE_TYPE_IRRATIONAL_IMAG:
            eprintf("IMA");
            break;
        case VALUE_TYPE_COMPLEX:
            eprintf("COM");
            break;
        case VALUE_TYPE_CHARACTER:
            eprintf("CHA");
            break;
        case VALUE_TYPE_CLO:
            eprintf("CLO");
            break;
        case VALUE_TYPE_PCLO:
            eprintf("PCL");
            break;
        case VALUE_TYPE_CONT:
            eprintf("KON");
            break;
        case VALUE_TYPE_VEC:
            eprintf("VEC");
            break;
        case VALUE_TYPE_BUILTIN:
            eprintf("BIN");
            break;
        case VALUE_TYPE_NAMESPACE:
            eprintf("NAS");
            break;
        default:
            cant_happen("unrecognised value type %d", v.type);
    }
}

static void ppStack(Stack *s) {
    eprintf("S:|");
    for (int i = 0; i < s->sp; ++i) {
        ppValue(s->stack[i]);
        eprintf("|");
    }
    eprintf("\n");
}

static void ppEnv(struct Env *e) {
    while (e != NULL) {
        eprintf("E:|");
        for (int i = 0; i < e->count; ++i) {
            ppValue(e->values[i]);
            eprintf("|");
        }
        eprintf("\n");
        e = e->next;
    }
}

void printCEKF(CEKF * x) {
    // int depth = 1;
    // eprintf("\nCEKF (\n");
    // printPad(depth);
    // eprintf("%04lx", x->C);
    // eprintf(",\n");
    // printEnv(x->E, depth);
    // eprintf(",\n");
    ppStack(&x->S);
    ppEnv(x->E);
    // printKont(x->K, depth);
    // eprintf(",\n");
    // printFail(x->F, depth);
    // eprintf(",\n");
    // printValue(x->V, depth);
    // eprintf(",\n");
    // printStack(&x->S, depth);
    // eprintf("\n)\n\n");
}

static void printStack(Stack *x, int depth) {
    printPad(depth);
    if (x == NULL || x->sp == 0) {
        eprintf("S/");
        return;
    }
    eprintf("S[ ");
    for (int i = 0; i < x->sp; ++i) {
        printContainedValue(peekValue(x, i), depth + 1);
        if (i < x->sp - 1) {
            eprintf(",");
        }
        // eprintf("\n");
    }
    // printPad(depth);
    eprintf("]\n");
}

static void printValues(Value *values, int count, int depth) {
    printPad(depth);
    eprintf("{\n");
    for (int i = 0; i < count; ++i) {
        printValue(values[i], depth + 1);
        if (i + 1 < count) {
            eprintf(",");
        }
        eprintf("\n");
    }
    printPad(depth);
    eprintf("}");
}

void printElidedValues(Value *values, int count) {
    eprintf("{");
    for (int i = 0; i < count; ++i) {
        printElidedValue(values[i]);
        if (i + 1 < count) {
            eprintf(", ");
        }
    }
    eprintf("}");
}

void printEnv(Env *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        eprintf("E/");
        return;
    }
    eprintf("E[\n");
    while (x != NULL) {
        printValues(x->values, x->count, depth + 1);
        if (x->next != NULL)
            eprintf(",");
        eprintf("\n");
        x = x->next;
    }
    printPad(depth);
    eprintf("]");
}

void printElidedEnv(Env *x) {
    if (x == NULL) {
        eprintf("E/");
        return;
    }
    eprintf("E[");
    while (x != NULL) {
        printElidedValues(x->values, x->count);
        if (x->next != NULL)
            eprintf(", ");
        x = x->next;
    }
    eprintf("]");
}

static void printKont(Kont *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        eprintf("K/");
        return;
    }
    eprintf("K[\n");
    if (x != NULL) {
        printPad(depth + 1);
        eprintf("%04lx,\n", x->body);
        printEnv(x->env, depth + 1);
        eprintf(",\n");
        printSnapshot(x->snapshot, depth + 1);
        eprintf(",\n");
        printKont(x->next, depth + 1);
        eprintf("\n");
    }
    printPad(depth);
    eprintf("]");
}

static void printFail(Fail *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        eprintf("F/");
        return;
    }
    eprintf("F[\n");
    if (x != NULL) {
        printPad(depth + 1);
        eprintf("%04lx", x->exp);
        eprintf(",\n");
        printEnv(x->env, depth + 1);
        eprintf(",\n");
        printSnapshot(x->snapshot, depth + 1);
        eprintf(",\n");
        printKont(x->kont, depth + 1);
        eprintf(",\n");
        printFail(x->next, depth + 1);
        eprintf("\n");
    }
    printPad(depth);
    eprintf("]");
}

void dumpByteCode(ByteCodeArray *bca) {
    size_t i = 0;
    while (i < bca->count) {
        eprintf("%04lx ### ", i);
        int thisByte;
        switch (thisByte = readByte(bca, &i)) {
            case BYTECODE_NONE:{
                    eprintf("NONE\n");
                }
                break;
            case BYTECODE_LAM:{
                    int nargs = readByte(bca, &i);
                    int letRecOffset = readByte(bca, &i);
                    int offset = readOffset(bca, &i);
                    eprintf("LAM [%d][%d][%04x]\n", nargs, letRecOffset,
                            offset);
                }
                break;
            case BYTECODE_VAR:{
                    int frame = readByte(bca, &i);
                    int offset = readByte(bca, &i);
                    eprintf("VAR [%d:%d]\n", frame, offset);
                }
                break;
            case BYTECODE_LVAR:{
                    int offset = readByte(bca, &i);
                    eprintf("LVAR [%d]\n", offset);
                }
                break;
            case BYTECODE_PRIM_ADD:{
                    eprintf("ADD\n");
                }
                break;
            case BYTECODE_PRIM_SUB:{
                    eprintf("SUB\n");
                }
                break;
            case BYTECODE_PRIM_MUL:{
                    eprintf("MUL\n");
                }
                break;
            case BYTECODE_PRIM_DIV:{
                    eprintf("DIV\n");
                }
                break;
            case BYTECODE_PRIM_POW:{
                    eprintf("POW\n");
                }
                break;
            case BYTECODE_PRIM_MOD:{
                    eprintf("MOD\n");
                }
                break;
            case BYTECODE_PRIM_EQ:{
                    eprintf("EQ\n");
                }
                break;
            case BYTECODE_PRIM_NE:{
                    eprintf("NE\n");
                }
                break;
            case BYTECODE_PRIM_GT:{
                    eprintf("GT\n");
                }
                break;
            case BYTECODE_PRIM_LT:{
                    eprintf("LT\n");
                }
                break;
            case BYTECODE_PRIM_GE:{
                    eprintf("GE\n");
                }
                break;
            case BYTECODE_PRIM_LE:{
                    eprintf("LE\n");
                }
                break;
            case BYTECODE_PRIM_CMP:{
                    eprintf("CMP\n");
                }
                break;
            case BYTECODE_PRIM_XOR:{
                    eprintf("XOR\n");
                }
                break;
            case BYTECODE_PRIM_NOT:{
                    eprintf("NOT\n");
                }
                break;
            case BYTECODE_PRIM_MAKEVEC:{
                    int size = readByte(bca, &i);
                    eprintf("MAKEVEC [%d]\n", size);
                }
                break;
            case BYTECODE_PRIM_VEC:{
                    eprintf("VEC\n");
                }
                break;
            case BYTECODE_APPLY:{
                    int nargs = readByte(bca, &i);
                    eprintf("APPLY [%d]\n", nargs);
                }
                break;
            case BYTECODE_IF:{
                    int offset = readOffset(bca, &i);
                    eprintf("IF [%04x]\n", offset);
                }
                break;
            case BYTECODE_MATCH:{
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
            case BYTECODE_CHARCOND:{
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
            case BYTECODE_INTCOND:{
                    int count = readWord(bca, &i);
                    eprintf("INTCOND [%d]", count);
                    while (count > 0) {
                        int type = readByte(bca, &i);
                        switch (type) {
                            case BYTECODE_BIGINT: {
                                bigint bi = readBigint(bca, &i);
                                eprintf(" [bigint]");
                                bigint_fprint(errout, &bi);
                                bigint_free(&bi);
                            }
                            break;
                            case BYTECODE_STDINT: {
                                int li = readInteger(bca, &i);
                                eprintf(" [int]%d", li);
                            }
                            break;
                            default:
                                cant_happen("expected INT or BIGINT in BYTECODE_INTCOND cases");
                        }
                        int offset = readOffset(bca, &i);
                        eprintf(":[%04x]", offset);
                        count--;
                    }
                    eprintf("\n");
                }
                break;
            case BYTECODE_LETREC:{
                    int size = readByte(bca, &i);
                    eprintf("LETREC [%d]\n", size);
                }
                break;
            case BYTECODE_AMB:{
                    int offset = readOffset(bca, &i);
                    eprintf("AMB [%04x]\n", offset);
                }
                break;
            case BYTECODE_CUT:{
                    eprintf("CUT\n");
                }
                break;
            case BYTECODE_BACK:{
                    eprintf("BACK\n");
                }
                break;
            case BYTECODE_LET:{
                    int offset = readOffset(bca, &i);
                    eprintf("LET [%04x]\n", offset);
                }
                break;
            case BYTECODE_JMP:{
                    int offset = readOffset(bca, &i);
                    eprintf("JMP [%04x]\n", offset);
                }
                break;
            case BYTECODE_PUSHN:{
                    int size = readByte(bca, &i);
                    eprintf("PUSHN [%d]\n", size);
                }
                break;
            case BYTECODE_CALLCC:{
                    eprintf("CALLCC\n");
                }
                break;
            case BYTECODE_TRUE:{
                    eprintf("TRUE\n");
                }
                break;
            case BYTECODE_FALSE:{
                    eprintf("FALSE\n");
                }
                break;
            case BYTECODE_VOID:{
                    eprintf("VOID\n");
                }
                break;
            case BYTECODE_PRIM_PUTC:{
                    eprintf("PUTC\n");
                }
                break;
            case BYTECODE_PRIM_PUTN:{
                    eprintf("PUTN\n");
                }
                break;
            case BYTECODE_PRIM_PUTV:{
                    eprintf("PUTV\n");
                }
                break;
            case BYTECODE_STDINT:{
                    int val = readInteger(bca, &i);
                    eprintf("STDINT [%d]\n", val);
                }
                break;
            case BYTECODE_BIGINT:{
                    eprintf("BIGINT [");
                    bigint bi = readBigint(bca, &i);
                    bigint_fprint(errout, &bi);
                    eprintf("]\n");
                    bigint_free(&bi);
                }
                break;
            case BYTECODE_IRRATIONAL:{
                Double f = readDouble(bca, &i);
                eprintf("IRRATIONAL [%f]\n", f);
            }
            break;
            case BYTECODE_CHAR:{
                    char c = readByte(bca, &i);
                    eprintf("CHAR [%s]\n", charRep(c));
                }
                break;
            case BYTECODE_RETURN:{
                    eprintf("RETURN\n");
                }
                break;
            case BYTECODE_DONE:{
                    eprintf("DONE\n");
                }
                break;
            case BYTECODE_ERROR:{
                    eprintf("ERROR\n");
                }
                break;
            case BYTECODE_NS_START:{
                    int count = readWord(bca, &i);
                    eprintf("NS_START [%d]\n", count);
                }
                break;
            case BYTECODE_NS_END:{
                    int numLambdas = readWord(bca, &i);
                    int stackOffset = readWord(bca, &i);
                    eprintf("NS_END [%d][%d]\n", numLambdas, stackOffset);
                }
                break;
            case BYTECODE_NS_FINISH:{
                    int count = readWord(bca, &i);
                    eprintf("NS_FINISH [%d]\n", count);
                }
                break;
            case BYTECODE_NS_PUSHS:{
                    int offset = readWord(bca, &i);
                    eprintf("NS_PUSHS [%d]\n", offset);
                }
                break;
            case BYTECODE_NS_PUSHE:{
                    int frame = readWord(bca, &i);
                    int offset = readWord(bca, &i);
                    eprintf("NS_PUSHE [%d][%d]\n", frame, offset);
                }
                break;
            case BYTECODE_NS_POP:{
                    eprintf("NS_POP\n");
                }
                break;
            default:
                cant_happen("unrecognised bytecode %d in dumpByteCode",
                            thisByte);
        }
    }
}
