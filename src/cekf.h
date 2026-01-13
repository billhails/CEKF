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

#include "anf.h"
#include "bytecode.h"
#include "cekfs.h"
#include "common.h"
#include "memory.h"
#include "types.h"
#include "value.h"

Vec *snapshotNameSpace(Stack *s);
void restoreNameSpace(Stack *s, Vec *vl);
void snapshotClo(struct Clo *target, Stack *stack, int letRecOffset);
void patchClo(struct Clo *target, Stack *stack);
void snapshotKont(struct Kont *target, Stack *stack);
void snapshotFail(struct Fail *target, Stack *stack);
void restoreKont(Stack *stack, struct Kont *source);
void restoreFail(Stack *stack, struct Fail *source);
void copyTosToEnv(Env *e, Stack *s, int n);
void copyValues(Value *to, Value *from, int size);
#ifdef SAFETY_CHECKS
void reportKonts(void);
#endif

void copyTosToVec(Vec *vec, Stack *s);
void pushN(Stack *stack, int n);
void popN(Stack *s, int n);
void patchVec(Vec *v, Stack *s, int num);
Env *makeEnv(Env *parent);
Kont *makeKont(Control offset, Env *env, bool makeStack, Kont *next);
Fail *makeFail(Control offset, Env *env, Kont *k, Fail *next);

Vec *newVec(int size);

void markValue(Value x);
void markEnv(Env *x);
void markVec(Vec *x);
#ifdef DEBUG_STEP
void dumpStack(Stack *stack);
void dumpFrame(Frame *frame);
#endif

CharVec *listToUtf8(Value v);
Value utf8ToList(const char *utf8);
Value makeNull(void);
Value makePair(Value car, Value cdr);

#endif
