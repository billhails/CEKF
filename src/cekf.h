#ifndef cekf_cekf_h
#define cekf_cekf_h
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

/**
 * The structures of the CEKF machine.
 */

#include <stdbool.h>
#include <stddef.h>

#include "bytecode.h"
#include "common.h"
#include "exp.h"
#include "memory.h"
#include "value.h"

typedef size_t Control;

typedef struct Stack {
    int capacity;
    int sp;
    struct Value *stack;
} Stack;

typedef struct {
    Control C;
    struct Env *E;
    struct Kont *K;
    struct Fail *F;
    struct Value V;
    struct Stack S;
    struct ByteCodeArray B;
} CEKF;

typedef struct Env {
    struct Header header;
    struct Env *next;
    int count;
    struct Value *values;
} Env;

typedef struct Snapshot {
    int frameSize;
    struct Value *frame;
} Snapshot;

typedef struct Kont {
    struct Header header;
    Control body;
    struct Env *rho;
    Snapshot snapshot;
    struct Kont *next;
} Kont;

typedef struct ValueList {
    struct Header header;
    int count;
    struct Value *values;
} ValueList;

typedef struct Clo {
    struct Header header;
    int nvar;
    Control c;
    struct Env *rho;
} Clo;

typedef struct Fail {
    struct Header header;
    Control exp;
    struct Env *rho;
    struct Kont *k;
    Snapshot snapshot;
    struct Fail *next;
} Fail;

typedef struct Cons {
    struct Header header;
    struct Value car;
    struct Value cdr;
} Cons;

typedef struct Vec {
    struct Header header;
    int size;
    struct Value values[0];
} Vec;

void snapshotClo(Stack *stack, struct Clo *target, int letRecOffset);
void patchClo(Stack *stack, struct Clo *target);
void snapshotKont(Stack *stack, struct Kont *target);
void snapshotFail(Stack *stack, struct Fail *target);
void restoreKont(Stack *stack, struct Kont *source);
void restoreFail(Stack *stack, struct Fail *source);
void setFrame(Stack *stack, int nargs);
void clearFrame(Stack *stack);
void copyTosToEnv(Stack *s, Env *e, int n);
void copyValues(Value *to, Value *from, int size);

extern Snapshot noSnapshot;

void pushValue(Stack *stack, Value v);
struct Value popValue(Stack *stack);
struct Value peekValue(Stack *stack, int offset);
void copyTopToValues(Stack *s, Value *values, int size);
void markStack(Stack *stack);
void initStack(Stack *stack);
int frameSize(Stack *stack);
void pushN(Stack *stack, int n);
void popN(Stack *s, int n);


ValueList *newValueList(int count);
Clo *newClo(int nvar, Control c, Env *rho);
Env *newEnv(Env *next, int count);
Kont *newKont(Control body, Env *rho, Kont *next);
Fail *newFail(Control exp, Env *rho, Kont *k, Fail *next);
Cons *newCons(Value car, Value cdr);
Vec *newVec(int size);

void markValue(Value x);
void markValueList(ValueList *x);
void markClo(Clo *x);
void markEnv(Env *x);
void markKont(Kont *x);
void markFail(Fail *x);
void markCons(Cons *x);
void markVec(Vec *x);

#endif
