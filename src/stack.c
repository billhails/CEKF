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

#include "common.h"
#include "cekf.h"
#include "memory.h"

#include <stddef.h>
#include <string.h>

Snapshot noSnapshot = {
    .frameSize = 0,
    .frame = NULL
};

void initStack(Stack *stack) {
    stack->sp = 0;
    stack->fp = 0;
    stack->capacity = 0;
    stack->stack = NULL;
}

static int frameSize(Stack *stack) {
    return stack->sp - stack->fp;
}

static void growCapacity(Stack *stack, int newCapacity) {
    int oldCapacity = stack->capacity;
    Value *newStack = GROW_ARRAY(Value, stack->stack, oldCapacity, newCapacity);

    stack->stack = newStack;
    stack->capacity = newCapacity;
}

void pushValue(Stack *stack, Value v) {
    if (stack->sp == stack->capacity) {
        growCapacity(stack, stack->capacity < 8 ? 8 : stack->capacity * 2);
    }
    stack->stack[stack->sp++] = v;
}

Value popValue(Stack *stack) {
    if (stack->sp == 0) {
        cant_happen("stack underflow");
    }
    return stack->stack[--stack->sp];
}

void markStack(Stack *stack) {
    for (int i = 0; i < stack->sp; i++) {
        markValue(stack->stack[i]);
    }
}

Value peekValue(Stack *stack, int offset) {
    if (stack->fp + offset >= stack->sp) {
        cant_happen("peek beyond top of stack not allowed");
    }
    return stack->stack[stack->fp + offset];
}

static void copyToFrame(Stack *stack, Value *frame) {
    for (int i = 0; i < frameSize(stack); ++i) {
        frame[i] = peekValue(stack, i);
    }
}

static void copyFromSnapshot(Stack *stack, Snapshot snapshot) {
    stack->fp = stack->sp;
    for (int i = 0; i < snapshot.frameSize; ++i) {
        pushValue(stack, snapshot.frame[i]);
    }
}

static void copyToSnapshot(Stack *stack, Snapshot *snapshot) {
    int size = frameSize(stack);
    if (size > 0) {
        snapshot->frame = NEW_ARRAY(Value, size);
        for (int i = 0; i < size; ++i) {
            snapshot->frame[i] = peekValue(stack, i);
        }
        snapshot->frameSize = size;
    }
}

void snapshotClo(Stack *stack, Clo *target) {
    Env *env = newEnv(target->rho, frameSize(stack));
    target->rho = env;
    copyToFrame(stack, env->values);
}

void patchClo(Stack *stack, Clo *target) {
    Env *rho = target->rho ? target->rho->next : NULL;
    Env *env = newEnv(rho, frameSize(stack));
    target->rho = env;
    copyToFrame(stack, env->values);
}

void snapshotKont(Stack *stack, Kont *target) {
    copyToSnapshot(stack, &target->snapshot);
}

void snapshotFail(Stack *stack, Fail *target) {
    copyToSnapshot(stack, &target->snapshot);
}

void restoreKont(Stack *stack, Kont *source) {
    copyFromSnapshot(stack, source->snapshot);
}

void restoreFail(Stack *stack, Fail *source) {
    copyFromSnapshot(stack, source->snapshot);
}

