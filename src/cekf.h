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
#  include "cekfs.h"

typedef struct {
    Control C;
    struct Env *E;
    struct Kont *K;
    struct Fail *F;
    struct Value V;
    struct Stack S;
    struct ByteCodeArray B;
    Index nsPosition;
} CEKF;

Vec *snapshotNamespace(Stack *s);
void restoreNamespace(Stack *s, Vec *vl);
void snapshotClo(struct Clo *target, Stack *stack, int letRecOffset);
void patchClo(struct Clo *target, Stack *stack);
void snapshotKont(struct Kont *target, Stack *stack);
void snapshotFail(struct Fail *target, Stack *stack);
void restoreKont(Stack *stack, struct Kont *source);
void restoreFail(Stack *stack, struct Fail *source);
void copyTosToEnv(Env *e, Stack *s, int n);
void copyValues(Value *to, Value *from, int size);

void copyTosToVec(Vec *vec, Stack *s);
void pushN(Stack *stack, int n);
void popN(Stack *s, int n);
void patchVec(Vec *v, Stack *s, int num);
Env *makeEnv(Env *parent, Index size);
Kont *makeKont(Control offset, Env *env, Kont *next);
Fail *makeFail(Control offset, Env *env, Kont *k, Fail *next);

Vec *newVec(int size);

void markValue(Value x);
void markEnv(Env *x);
void markVec(Vec *x);
void dumpStack(Stack *stack);

#endif
