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

// This file contains code for maintaining the bytecode array as well
// as generating bytecode from ANF lambda expressions.

#include <stdio.h>
#include <stdbool.h>

#include "bytecode.h"

void initByteCodeArray(ByteCodeArray *b) {
    b->count = 0;
    b->capacity = 0;
    b->entries = NULL;
}

void resetByteCodeArray(ByteCodeArray *b) {
    b->count = 0;
    for (int i = 0; i < b->capacity; i++) {
        b->entries[i] = BYTECODE_NONE;
    }
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

static void writeCurrentAddressAt(int patch, ByteCodeArray *b) {
    int offset = b->count - patch;
    writeWordAt(patch, b, offset);
}

static void writeWord(ByteCodeArray *b, int word) {
    if (word > 65535) {
        cant_happen("maximum word size exceeded");
    }
    addByte(b, word >> 8);
    addByte(b, word & 255);
}

static int reserveWord(ByteCodeArray *b) {
    int address = b->count;
    addByte(b, 0);
    addByte(b, 0);
    return address;
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
    addByte(b, x->letRecOffset);
    int patch = reserveWord(b);
    writeExp(x->exp, b);
    addByte(b, BYTECODE_RETURN);
    writeCurrentAddressAt(patch, b);
}

void writeAexpVarList(AexpVarList *x, ByteCodeArray *b) {
    cant_happen("writeAexpVarList called");
}

void writeAexpVar(HashSymbol *x, ByteCodeArray *b) {
    cant_happen("writeAexpVar called");
}

void writeAexpAnnotatedVar(AexpAnnotatedVar *x, ByteCodeArray *b) {
    if (x == NULL) return;
    switch (x->type) {
        case VAR_TYPE_ENV:
            addByte(b, BYTECODE_VAR);
            addByte(b, x->frame);
            addByte(b, x->offset);
            break;
        case VAR_TYPE_STACK:
            addByte(b, BYTECODE_LVAR);
            addByte(b, x->offset);
            break;
        default:
            cant_happen("unrecognised annotated var type");
    }
            
}

void writeAexpUnaryApp(AexpUnaryApp *x, ByteCodeArray *b) {
    if (x == NULL) return;
    writeAexp(x->exp, b);
    byte prim;
    switch (x->op) {
        case AEXP_UNARY_CAR:
            prim = BYTECODE_PRIM_CAR;
            break;
        case AEXP_UNARY_CDR:
            prim = BYTECODE_PRIM_CDR;
            break;
        case AEXP_UNARY_NOT:
            prim = BYTECODE_PRIM_NOT;
            break;
        case AEXP_UNARY_PRINT:
            prim = BYTECODE_PRIM_PRINT;
            break;
        default:
            cant_happen("unrecognised AexpUnaryOp in writeAexpUnaryApp");
    }
    addByte(b, prim);
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
        case AEXP_PRIM_XOR:
            prim = BYTECODE_PRIM_XOR;
            break;
        case AEXP_PRIM_CONS:
            prim = BYTECODE_PRIM_CONS;
            break;
        case AEXP_PRIM_VEC:
            prim = BYTECODE_PRIM_VEC;
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

void writeAexpMakeVec(AexpMakeVec *x, ByteCodeArray *b) {
    writeAexpList(x->args, b);
    addByte(b, BYTECODE_PRIM_MAKEVEC);
    addByte(b, x->nargs);
}

void writeCexpApply(CexpApply *x, ByteCodeArray *b) {
    writeAexpList(x->args, b);
    writeAexp(x->function, b);
    addByte(b, BYTECODE_APPLY);
}

void writeCexpCond(CexpCond *x, ByteCodeArray *b) {
    writeAexp(x->condition, b);
    addByte(b, BYTECODE_IF);
    int patch = reserveWord(b);
    writeExp(x->consequent, b);
    addByte(b, BYTECODE_JMP);
    int patch2 = reserveWord(b);
    writeCurrentAddressAt(patch, b);
    writeExp(x->alternative, b);
    writeCurrentAddressAt(patch2, b);
}

void writeCexpLetRec(CexpLetRec *x, ByteCodeArray *b) {
    writeLetRecBindings(x->bindings, b);
    addByte(b, BYTECODE_LETREC);
    addByte(b, x->nbindings);
    writeExp(x->body, b);
}

static AexpInteger validateMatchIndex(Aexp *index) {
    if (index->type != AEXP_TYPE_INT) {
        cant_happen("match index must be literal integer");
    }
    AexpInteger i = index->val.integer;
    if (i < 0 || i > 255) {
        cant_happen("match index must be in the range 0-255");
    }
    return i;
}

static int validateCexpMatch(CexpMatch *x) {
    bool seen[256];
    for (int i = 0; i < 256; ++i) {
        seen[i] = false;
    }
    for (MatchList *m = x->clauses; m != NULL; m = m->next) {
        for (AexpList *matches = m->matches; matches != NULL; matches = matches->next) {
            AexpInteger index = validateMatchIndex(matches->exp);
            if (seen[index]) {
                cant_happen("duplicate index %d in validateCexpMatch", index);
            }
            seen[index] = true;
        }
    }
    bool end = false;
    int count = 0;
    for (int i = 0; i < 256; ++i) {
        if (seen[i]) {
            if (end)
                cant_happen("non-contiguous match indices in validateCexpMatch");
            else
                count = i + 1;
        } else {
            end = true;
        }
    }
    if (count == 0)
        cant_happen("empty match indices in validateCexpMatch");
    return count;
}

void writeCexpMatch(CexpMatch *x, ByteCodeArray *b) {
    int count = validateCexpMatch(x);
    writeAexp(x->condition, b);
    addByte(b, BYTECODE_MATCH);
    // create a dispatch table
    addByte(b, count);
    int patches[256];
    int jumps[256];
    for (int i = 0; i < count; ++i) {
        patches[i] = reserveWord(b);
    }
    int jumpCounter = 0;
    for (MatchList *m = x->clauses; m != NULL; m = m->next) {
        for (AexpList *l = m->matches; l != NULL; l = l->next) {
            AexpInteger i = l->exp->val.integer;
            writeCurrentAddressAt(patches[i], b);
        }
        writeExp(m->body, b);
        addByte(b, BYTECODE_JMP);
        jumps[jumpCounter++] = reserveWord(b);
    }
    for (int i = 0; i < jumpCounter; i++)
        writeCurrentAddressAt(jumps[i], b);
}

void writeLetRecBindings(LetRecBindings *x, ByteCodeArray *b) {
    while (x != NULL) {
        writeAexp(x->val, b);
        x = x->next;
    }
}

void writeCexpAmb(CexpAmb *x, ByteCodeArray *b) {
    addByte(b, BYTECODE_AMB);
    int patch = reserveWord(b);
    writeExp(x->exp1, b);
    addByte(b, BYTECODE_JMP);
    int patch2 = reserveWord(b);
    writeCurrentAddressAt(patch, b);
    writeExp(x->exp2, b);
    writeCurrentAddressAt(patch2, b);
}

void writeExpLet(ExpLet *x, ByteCodeArray *b) {
    addByte(b, BYTECODE_LET);
    int patch = reserveWord(b);
    writeExp(x->val, b);
    addByte(b, BYTECODE_RETURN);
    writeCurrentAddressAt(patch, b);
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
        case AEXP_TYPE_VOID: {
            addByte(b, BYTECODE_VOID);
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
        case AEXP_TYPE_UNARY: {
            writeAexpUnaryApp(x->val.unary, b);
        }
        break;
        case AEXP_TYPE_MAKEVEC: {
            writeAexpMakeVec(x->val.makeVec, b);
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
        case CEXP_TYPE_MATCH: {
            writeCexpMatch(x->val.match, b);
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
