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

#include "exp.h"
#include "memory.h"

typedef uint8_t byte;

typedef enum ByteCodes {
    BYTECODE_NONE,
    BYTECODE_LAM,
    BYTECODE_VAR,
    BYTECODE_PRIM_ADD,
    BYTECODE_PRIM_SUB,
    BYTECODE_PRIM_MUL,
    BYTECODE_PRIM_DIV,
    BYTECODE_PRIM_EQ,
    BYTECODE_PRIM_NE,
    BYTECODE_PRIM_GT,
    BYTECODE_PRIM_LT,
    BYTECODE_PRIM_GE,
    BYTECODE_PRIM_LE,
    BYTECODE_APPLY,
    BYTECODE_IF,
    BYTECODE_ENV,
    BYTECODE_LETREC,
    BYTECODE_AMB,
    BYTECODE_BACK,
    BYTECODE_LET,
    BYTECODE_CALLCC,
    BYTECODE_TRUE,
    BYTECODE_FALSE,
    BYTECODE_INT,
    BYTECODE_RETURN,
    BYTECODE_JMP,
    BYTECODE_DONE,

} ByteCodes;

typedef struct ByteCodeArray {
    struct Header header;
    int capacity;
    int count;
    byte *entries;
} ByteCodeArray;

void initByteCodeArray(ByteCodeArray *b);

void writeAexpLam(AexpLam *x, ByteCodeArray *b);
void writeAexpVarList(AexpVarList *x, ByteCodeArray *b);
void writeAexpVar(AexpVar *x, ByteCodeArray *b);
void writeAexpAnnotatedVar(AexpAnnotatedVar *x, ByteCodeArray *b);
void writeAexpPrimApp(AexpPrimApp *x, ByteCodeArray *b);
void writeAexpList(AexpList *x, ByteCodeArray *b);
void writeCexpApply(CexpApply *x, ByteCodeArray *b);
void writeCexpCond(CexpCond *x, ByteCodeArray *b);
void writeCexpLetRec(CexpLetRec *x, ByteCodeArray *b);
void writeLetRecBindings(LetRecBindings *x, ByteCodeArray *b);
void writeCexpAmb(CexpAmb *x, ByteCodeArray *b);
void writeExpLet(ExpLet *x, ByteCodeArray *b);
void writeAexp(Aexp *x, ByteCodeArray *b);
void writeCexp(Cexp *x, ByteCodeArray *b);
void writeExp(Exp *x, ByteCodeArray *b);

void writeEnd(ByteCodeArray *b);

#endif
