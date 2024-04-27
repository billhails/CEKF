#ifndef cekf_cekf_h
#  define cekf_cekf_h
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

#  include <stdbool.h>
#  include <stddef.h>

#  include "bytecode.h"
#  include "common.h"
#  include "anf.h"
#  include "memory.h"
#  include "value.h"
#  include "types.h"


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
    struct Env *env;
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
    int pending;
    Control ip;
    struct Env *env;
} Clo;

typedef struct Fail {
    struct Header header;
    Control exp;
    struct Env *env;
    struct Kont *kont;
    Snapshot snapshot;
    struct Fail *next;
} Fail;

typedef struct Vec {
    struct Header header;
    int size;
    struct Value values[0];
} Vec;

int protectValue(Value v);

void snapshotClo(Stack *stack, struct Clo *target, int letRecOffset);
void patchClo(Stack *stack, struct Clo *target);
void snapshotKont(Stack *stack, struct Kont *target);
void snapshotFail(Stack *stack, struct Fail *target);
void restoreKont(Stack *stack, struct Kont *source);
void restoreFail(Stack *stack, struct Fail *source);
void setFrame(Stack *stack, int base, int nargs);
void clearFrame(Stack *stack);
void copyTosToEnv(Stack *s, Env *e, int n);
void copyValues(Value *to, Value *from, int size);
// safe version of copyValues:
void moveValues(Value *to, Value *from, int size);

extern Snapshot noSnapshot;

void pushValue(Stack *stack, Value v);
struct Value popValue(Stack *stack);
struct Value peekValue(Stack *stack, int offset);
struct Value peekTop(Stack *s);
void copyTopToValues(Stack *s, Value *values, int size);
void markStack(Stack *stack);
void initStack(Stack *stack);
int frameSize(Stack *stack);
void pushN(Stack *stack, int n);
void popN(Stack *s, int n);
void ensureCapacity(Stack *s, int n);

ValueList *newValueList(int count);
Clo *newClo(int nvar, Control ip, Env *env);
Env *newEnv(Env *next, int count);
Kont *newKont(Control body, Env *env, Kont *next);
Fail *newFail(Control exp, Env *env, Kont *kont, Fail *next);
Vec *newVec(int size);

void markValue(Value x);
void markValueList(ValueList *x);
void markClo(Clo *x);
void markEnv(Env *x);
void markKont(Kont *x);
void markFail(Fail *x);
void markVec(Vec *x);
void dumpStack(Stack *stack);

#endif
