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

#include <stdio.h>

#include "bytecode.h"

void initByteCodeArray(ByteCodeArray *b) {
    b->count = 0;
    b->capacity = 0;
    b->entries = NULL;
}


static void growCapacity(ByteCodeArray *byteCodes, int newCapacity) {
    int oldCapacity = byteCodes->capacity;
    byte *entries = GROW_ARRAY(byte, byteCodes->entries, oldCapacity, newCapacity);

    for (int i = oldCapacity; i < newCapacity; i++) {
        entries[i] = BYTECODE_NONE;
    }

    byteCodes->entries = entries;
    byteCodes->capacity = newCapacity;
}

static void addByte(ByteCodeArray *b, int code) {
    if (code > 255) {
        cant_happen("maximim byte size exceeded");
    }
    if (b->count == b->capacity) {
        growCapacity(b, b->capacity < 8 ? 8 : b->capacity * 2);
    }
    b->entries[b->count++] = code;
}

static void writeWordAt(int loc, ByteCodeArray *b, int word) {
    if (word > 65535) {
        cant_happen("maximum word size exceeded");
    }
    b->entries[loc] = word >> 8;
    b->entries[loc + 1] = word & 255;
}

static void writeWord(ByteCodeArray *b, int word) {
    if (word > 65535) {
        cant_happen("maximum word size exceeded");
    }
    addByte(b, word >> 8);
    addByte(b, word & 255);
}

static void writeInt(ByteCodeArray *b, int word) {
    if (word > 4294967295) {
        cant_happen("maximum int size exceeded");
    }
    addByte(b, (word >> 24) & 255);
    addByte(b, (word >> 16) & 255);
    addByte(b, (word >> 8) & 255);
    addByte(b, word & 255);
}

void writeAexpLam(AexpLam *x, ByteCodeArray *b) {
    if (x == NULL) return;
    addByte(b, BYTECODE_LAM);
    addByte(b, x->nargs);
    int patch = b->count;
    writeWord(b, 0);
    writeExp(x->exp, b);
    addByte(b, BYTECODE_RETURN);
    int offset = b->count - patch;
    writeWordAt(patch, b, offset);
}

void writeAexpVarList(AexpVarList *x, ByteCodeArray *b) {
    cant_happen("writeAexpVarList called");
}

void writeAexpVar(AexpVar *x, ByteCodeArray *b) {
    cant_happen("writeAexpVar called");
}

void writeAexpAnnotatedVar(AexpAnnotatedVar *x, ByteCodeArray *b) {
    if (x == NULL) return;
    addByte(b, BYTECODE_VAR);
    addByte(b, x->frame);
    addByte(b, x->offset);
}

void writeAexpPrimApp(AexpPrimApp *x, ByteCodeArray *b) {
    if (x == NULL) return;
    writeAexp(x->exp1, b);
    writeAexp(x->exp2, b);
    byte prim;
    switch (x->op) {
        case AEXP_PRIM_ADD:
            prim = BYTECODE_PRIM_ADD;
            break;
        case AEXP_PRIM_SUB:
            prim = BYTECODE_PRIM_SUB;
            break;
        case AEXP_PRIM_MUL:
            prim = BYTECODE_PRIM_MUL;
            break;
        case AEXP_PRIM_DIV:
            prim = BYTECODE_PRIM_DIV;
            break;
        case AEXP_PRIM_EQ:
            prim = BYTECODE_PRIM_EQ;
            break;
        case AEXP_PRIM_NE:
            prim = BYTECODE_PRIM_NE;
            break;
        case AEXP_PRIM_GT:
            prim = BYTECODE_PRIM_GT;
            break;
        case AEXP_PRIM_LT:
            prim = BYTECODE_PRIM_LT;
            break;
        case AEXP_PRIM_GE:
            prim = BYTECODE_PRIM_GE;
            break;
        case AEXP_PRIM_LE:
            prim = BYTECODE_PRIM_LE;
            break;
        default:
            cant_happen("unrecognised AexpPrimOp in writeAexpPrimApp");
    }
    addByte(b, prim);
}

void writeAexpList(AexpList *x, ByteCodeArray *b) {
    while (x != NULL) {
        writeAexp(x->exp, b);
        x = x->next;
    }
}

void writeCexpApply(CexpApply *x, ByteCodeArray *b) {
    writeAexpList(x->args, b);
    writeAexp(x->function, b);
    addByte(b, BYTECODE_APPLY);
}

void writeCexpCond(CexpCond *x, ByteCodeArray *b) {
    writeAexp(x->condition, b);
    addByte(b, BYTECODE_IF);
    int patch = b->count;
    writeWord(b, 0);
    writeExp(x->consequent, b);
    addByte(b, BYTECODE_JMP);
    int patch2 = b->count;
    writeWord(b, 0);
    int offset = b->count - patch;
    writeWordAt(patch, b, offset);
    writeExp(x->alternative, b);
    int offset2 = b->count - patch2;
    writeWordAt(patch2, b, offset2);
}

void writeCexpLetRec(CexpLetRec *x, ByteCodeArray *b) {
    addByte(b, BYTECODE_ENV);
    addByte(b, x->nbindings);
    writeLetRecBindings(x->bindings, b);
    addByte(b, BYTECODE_LETREC);
    addByte(b, x->nbindings);
    writeExp(x->body, b);
}

void writeLetRecBindings(LetRecBindings *x, ByteCodeArray *b) {
    while (x != NULL) {
        writeAexp(x->val, b);
        x = x->next;
    }
}

void writeCexpAmb(CexpAmb *x, ByteCodeArray *b) {
    addByte(b, BYTECODE_AMB);
    int patch = b->count;
    writeWord(b, 0);
    writeExp(x->exp1, b);
    addByte(b, BYTECODE_JMP);
    int patch2 = b->count;
    writeWord(b, 0);
    int offset = b->count - patch;
    writeWordAt(patch, b, offset);
    writeExp(x->exp2, b);
    int offset2 = b->count - patch2;
    writeWordAt(patch2, b, offset2);
}

void writeExpLet(ExpLet *x, ByteCodeArray *b) {
    addByte(b, BYTECODE_LET);
    int patch = b->count;
    writeWord(b, 0);
    writeExp(x->val, b);
    addByte(b, BYTECODE_RETURN);
    writeWordAt(patch, b, b->count - patch);
    writeExp(x->body, b);
}

void writeAexp(Aexp *x, ByteCodeArray *b) {
    switch (x->type) {
        case AEXP_TYPE_LAM: {
            writeAexpLam(x->val.lam, b);
        }
        break;
        case AEXP_TYPE_VAR: {
            cant_happen("un-annotated var in writeAexp");
        }
        break;
        case AEXP_TYPE_ANNOTATEDVAR: {
            writeAexpAnnotatedVar(x->val.annotatedVar, b);
        }
        break;
        case AEXP_TYPE_TRUE: {
            addByte(b, BYTECODE_TRUE);
        }
        break;
        case AEXP_TYPE_FALSE: {
            addByte(b, BYTECODE_FALSE);
        }
        break;
        case AEXP_TYPE_INT: {
            addByte(b, BYTECODE_INT);
            writeInt(b, x->val.integer);
        }
        break;
        case AEXP_TYPE_PRIM: {
            writeAexpPrimApp(x->val.prim, b);
        }
        break;
        default:
            cant_happen("unrecognized Aexp type in writeAexp");
    }
}

void writeCexp(Cexp *x, ByteCodeArray *b) {
    switch (x->type) {
        case CEXP_TYPE_APPLY: {
            writeCexpApply(x->val.apply, b);
        }
        break;
        case CEXP_TYPE_COND: {
            writeCexpCond(x->val.cond, b);
        }
        break;
        case CEXP_TYPE_CALLCC: {
            writeAexp(x->val.callCC, b);
            addByte(b, BYTECODE_CALLCC);
        }
        break;
        case CEXP_TYPE_LETREC: {
            writeCexpLetRec(x->val.letRec, b);
        }
        break;
        case CEXP_TYPE_AMB: {
            writeCexpAmb(x->val.amb, b);
        }
        break;
        case CEXP_TYPE_BACK: {
            addByte(b, BYTECODE_BACK);
        }
        break;
        default:
            cant_happen("unrecognized Cexp type in writeCexp");
    }
}

void writeExp(Exp *x, ByteCodeArray *b) {
    switch (x->type) {
        case EXP_TYPE_AEXP: {
            writeAexp(x->val.aexp, b);
        }
        break;
        case EXP_TYPE_CEXP: {
            writeCexp(x->val.cexp, b);
        }
        break;
        case EXP_TYPE_LET: {
            writeExpLet(x->val.let, b);
        }
        break;
        case EXP_TYPE_DONE: {
            addByte(b, BYTECODE_DONE);
        }
        break;
        default:
            cant_happen("unrecognized Exp type in writeExp");
    }
}

void writeEnd(ByteCodeArray *b) {
    addByte(b, BYTECODE_RETURN);
}
