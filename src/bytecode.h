#ifndef cekf_bytecode_h
#define cekf_bytecode_h
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

#include "anf.h"
#include "memory.h"

typedef uint8_t byte;
typedef uint16_t word;

typedef enum ByteCodes {
    BYTECODE_NONE,
    BYTECODE_LAM,
    BYTECODE_VAR,
    BYTECODE_LVAR,
    BYTECODE_PRIM_ADD,

    BYTECODE_PRIM_SUB,
    BYTECODE_PRIM_MUL,
    BYTECODE_PRIM_DIV,
    BYTECODE_PRIM_POW,
    BYTECODE_PRIM_MOD,

    BYTECODE_PRIM_EQ,
    BYTECODE_PRIM_NE,
    BYTECODE_PRIM_GT,
    BYTECODE_PRIM_LT,
    BYTECODE_PRIM_GE,

    BYTECODE_PRIM_LE,
    BYTECODE_PRIM_XOR,
    BYTECODE_PRIM_MAKEVEC,
    BYTECODE_PRIM_VEC,
    BYTECODE_PRIM_NOT,
    BYTECODE_MATCH,
    BYTECODE_APPLY,

    BYTECODE_IF,
    BYTECODE_CHARCOND,
    BYTECODE_INTCOND,
    BYTECODE_LETREC,
    BYTECODE_AMB,

    BYTECODE_CUT,
    BYTECODE_BACK,
    BYTECODE_LET,
    BYTECODE_CALLCC,
    BYTECODE_TRUE,

    BYTECODE_FALSE,
    BYTECODE_VOID,
    BYTECODE_STDINT,
    BYTECODE_BIGINT,
    BYTECODE_CHAR,

    BYTECODE_RETURN,
    BYTECODE_JMP,
    BYTECODE_PUSHN,
    BYTECODE_DONE,
    BYTECODE_ERROR,

    BYTECODE_PRIM_CMP,
    BYTECODE_PRIM_PUTC,
    BYTECODE_PRIM_PUTN,
    BYTECODE_PRIM_PUTV,
} ByteCodes;

typedef struct ByteCodeArray {
    struct Header header;
    size_t capacity;
    size_t count;
    byte *entries;
} ByteCodeArray;

void initByteCodeArray(ByteCodeArray *b);
void resetByteCodeArray(ByteCodeArray *b);

void writeAexpLam(AexpLam *x, ByteCodeArray *b);
void writeAexpAnnotatedVar(AexpAnnotatedVar *x, ByteCodeArray *b);
void writeAexpPrimApp(AexpPrimApp *x, ByteCodeArray *b);
void writeAexpUnaryApp(AexpUnaryApp *x, ByteCodeArray *b);
void writeAexpList(AexpList *x, ByteCodeArray *b);
void writeCexpApply(CexpApply *x, ByteCodeArray *b);
void writeCexpIf(CexpIf *x, ByteCodeArray *b);
void writeCexpMatch(CexpMatch *x, ByteCodeArray *b);
void writeCexpLetRec(CexpLetRec *x, ByteCodeArray *b);
void writeLetRecBindings(LetRecBindings *x, ByteCodeArray *b);
void writeCexpAmb(CexpAmb *x, ByteCodeArray *b);
void writeCexpCut(CexpCut *x, ByteCodeArray *b);
void writeCexpAnd(CexpBool *x, ByteCodeArray *b);
void writeCexpOr(CexpBool *x, ByteCodeArray *b);
void writeExpLet(ExpLet *x, ByteCodeArray *b);
void writeAexp(Aexp *x, ByteCodeArray *b);
void writeCexp(Cexp *x, ByteCodeArray *b);
void writeExp(Exp *x, ByteCodeArray *b);

void writeEnd(ByteCodeArray *b);

static inline byte readByte(ByteCodeArray *b, size_t *i) {
    return b->entries[(*i)++];
}

static inline void _readWord(ByteCodeArray *b, size_t *i, word *a) {
    memcpy(a, &b->entries[*i], sizeof(word));
    (*i) += sizeof(word);
}

static inline word readWord(ByteCodeArray *b, size_t *i) {
    word a;
    _readWord(b, i, &a);
    return a;
}

static inline void _readInt(ByteCodeArray *b, size_t *i, int *a) {
    memcpy(a, &b->entries[*i], sizeof(int));
    (*i) += sizeof(int);
}

static inline int readInt(ByteCodeArray *b, size_t *i) {
    int a;
    _readInt(b, i, &a);
    return a;
}

static inline int readOffset(ByteCodeArray *b, size_t *i) {
    int ii = *i;
    int offset = readWord(b, i);
    return ii + offset;
}

static inline int readOffsetAt(ByteCodeArray *b, int i, int step) {
    size_t ii = i + step * sizeof(word);
    int offset = readWord(b, &ii);
    return i + offset + step * sizeof(word);
}

static inline bigint readBigint(ByteCodeArray *b, size_t *i) {
    bigint a;
    bigint_init(&a);
    int size;
    int capacity;
    _readInt(b, i, &size);
    _readInt(b, i, &capacity);
    int neg = readByte(b, i);
    bigint_reserve(&a, capacity);
    int nbytes = capacity * sizeof(bigint_word);
    memcpy(a.words, &b->entries[*i], nbytes);
    (*i) += nbytes;
    a.size = size;
    a.neg = neg;
    return a;
}


#endif
