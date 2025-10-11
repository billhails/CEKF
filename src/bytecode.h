#ifndef cekf_bytecode_h
#  define cekf_bytecode_h
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

#  include "anf.h"
#  include "memory.h"
#  include "types.h"
#  include "cekfs.h"

// MUST remember to increment this if bytecodes change
#  define CEKF_BYTECODE_VERSION 5

enum ReadByteCodeStatus {
    BYTECODES_OK,
    BYTECODES_BADFILE,
    BYTECODES_BADHEADER,
    BYTECODES_BADVERSION
};

char *charRep(Character c);

void resetByteCodeArray(ByteCodeArray *b);

void writeAexpLam(AexpLam *x, ByteCodeArray *b, LocationArray *L);
void writeAexpAnnotatedVar(AexpAnnotatedVar *x, ByteCodeArray *b, LocationArray *L);
void writeAexpPrimApp(AexpPrimApp *x, ByteCodeArray *b, LocationArray *L);
void writeAexpList(AexpList *x, ByteCodeArray *b, LocationArray *L);
void writeCexpApply(CexpApply *x, ByteCodeArray *b, LocationArray *L);
void writeCexpIf(CexpIf *x, ByteCodeArray *b, LocationArray *L);
void writeCexpMatch(CexpMatch *x, ByteCodeArray *b, LocationArray *L);
void writeCexpLetRec(CexpLetRec *x, ByteCodeArray *b, LocationArray *L);
void writeLetRecBindings(LetRecBindings *x, ByteCodeArray *b, LocationArray *L);
void writeCexpAmb(CexpAmb *x, ByteCodeArray *b, LocationArray *L);
void writeCexpCut(CexpCut *x, ByteCodeArray *b, LocationArray *L);
void writeExpLet(ExpLet *x, ByteCodeArray *b, LocationArray *L);
void writeAexp(Aexp *x, ByteCodeArray *b, LocationArray *L);
void writeCexp(Cexp *x, ByteCodeArray *b, LocationArray *L);
void writeExp(Exp *x, ByteCodeArray *b, LocationArray *L);

bool writeBinaryOutputFile(ByteCodeArray *b, char *filename);
enum ReadByteCodeStatus readBinaryInputFile(ByteCodeArray *b, char *filename);

void writeEnd(ByteCodeArray *b);

static inline Byte readByte(ByteCodeArray *b, Control *i) {
    return b->entries[(*i)++];
}

static inline Character readCharacter(ByteCodeArray *b, Control *i) {
    Character c;
    memcpy(&c, &b->entries[*i], sizeof(Character));
    (*i) += sizeof(Character);
    return c;
}

static inline void _readWord(ByteCodeArray *b, Control *i, Word *a) {
    memcpy(a, &b->entries[*i], sizeof(Word));
    (*i) += sizeof(Word);
}

static inline Word readWord(ByteCodeArray *b, Control *i) {
    Word a;
    _readWord(b, i, &a);
    return a;
}

static inline void _readInteger(ByteCodeArray *b, Control *i, Integer *a) {
    memcpy(a, &b->entries[*i], sizeof(Integer));
    (*i) += sizeof(Integer);
}

static inline void _readDouble(ByteCodeArray *b, Control *i, Double *a) {
    memcpy(a, &b->entries[*i], sizeof(Double));
    (*i) += sizeof(Double);
}

static inline Integer readInteger(ByteCodeArray *b, Control *i) {
    Integer a;
    _readInteger(b, i, &a);
    return a;
}

static inline Double readDouble(ByteCodeArray *b, Control *i) {
    Double a;
    _readDouble(b, i, &a);
    return a;
}

static inline Integer readOffset(ByteCodeArray *b, Control *i) {
    Integer ii = *i;
    Word offset = readWord(b, i);
    return ii + offset;
}

static inline Integer readOffsetAt(ByteCodeArray *b, Control i, int step) {
    size_t ii = i + step * sizeof(Word);
    Word offset = readWord(b, &ii);
    return (Integer)(i + offset + step * sizeof(Word));
}

static inline bigint readBigint(ByteCodeArray *b, Control *i) {
    bigint a;
    bigint_init(&a);
    int size;
    int capacity;
    _readInteger(b, i, &size);
    _readInteger(b, i, &capacity);
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
