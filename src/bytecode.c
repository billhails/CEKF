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

#include "bytecode.h"

static void growCapacity(ByteCodeArray *byteCodes, int capacity) {
    byte *entries = NEW_ARRAY(byte, capacity);


    for (int i = 0; i < byteCodes->capacity; i++) {
        entries[i] = byteCodes->entries[i];
    }

    for (; i < capacity; i++) {
        entries[i] = BYTECODE_NONE;
    }

    FREE_ARRAY(byte, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
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

void writeAexpLam(AexpLam *x, ByteCodeArray *b) {
    if (x == NULL) return;
    addByte(b, BYTECODE_LAM);
    addByte(b, x->nargs);
    writeExp(b, x->exp);
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
    writeExp(x->exp1, b);
    writeExp(x->exp2, b);
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
        writeExp(x->exp, b);
        x = x->next;
    }
}

void writeCexpApply(CexpApply *x, ByteCodeArray *b) {
    writeAexpList(x->args, b);
    writeExp(x->function, b);
    addByte(b, BYTECODE_APPLY);
}

void writeCexpCond(CexpCond *x, ByteCodeArray *b) {
    writeExp(x->condition, b);
    addByte(b, BYTECODE_IF);
    int patch = b->count;
    writeWord(b, 0);
    writeExp(x->consequent, b);
    int offset = b->count - patch;
    writeWordAt(patch, b, offset);
    writeExp(x->alternative, b);
}

void writeCexpLetRec(CexpLetRec *x, ByteCodeArray *b) {
    addByte(b, BYTECODE_ENV);
    addByte(b, x->nbindings);
    writeLetRecBindings(x->bindings, b);
    addByte(b, BYTECODE_LETREC);
    addByte(x->nbindings);
    writeExp(x->body);
}

void writeLetRecBindings(LetRecBindings *x, ByteCodeArray *b) {
    while (x != NULL) {
        writeExp(x->val, b);
        x = x->next;
    }
}

void writeCexpAmb(CexpAmb *x, ByteCodeArray *b) {
    addByte(b, BYTECODE_AMB);
    int patch = b->count;
    addWord(b, 0);
    writeExp(x->exp1, b);
    writeWordAt(patch, b, b->count - patch);
    writeExp(x->exp2, b);
}

void writeExpLet(ExpLet *x, ByteCodeArray *b) {
    addByte(b, BYTECODE_LET);
    int patch = b->count;
    addWord(b, 0);
    writeExp(x->val, b);
    writeWordAt(patch, b, b->count - patch);
    writeExp(x->body, b);
}

void writeExp(Exp *x, ByteCodeArray *b) {
}

