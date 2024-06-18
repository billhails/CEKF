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

// run-time stack and closure/continuation management

#include "common.h"
#include "cekf.h"
#include "memory.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include "debug.h"
#ifdef DEBUG_STACK
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

Snapshot noSnapshot = {
    .frameSize = 0,
    .frame = NULL
};

void initStack(Stack *stack) {
    stack->sp = 0;
    stack->capacity = 0;
    stack->stack = NULL;
}

int frameSize(Stack *s) {
    return s->sp;
}

// safe move n bytes from sp - n to base
// set sp to base + n
void setFrame(Stack *s, int base, int n) {
    DEBUG("setFrame(%d, %d)", base, n);
    ensureCapacity(s, base + n);
    if (n) {
        //         type,  dest,            src,                  amount
        MOVE_ARRAY(Value, &s->stack[base], &s->stack[s->sp - n], n);
    }
    s->sp = base + n;
}

void clearFrame(Stack *s) {
    DEBUG("clearFrame()");
    s->sp = 0;
}

void discardStackTop(Stack *s, int num) {
#ifdef SAFETY_CHECKS
    if (num > s->sp || num < 0) {
        cant_happen("discard out of bounds");
    }
#endif
    s->sp -= num;
}

static void growCapacity(Stack *s) {
    int newCapacity = s->capacity < 8 ? 8 : s->capacity * 2;
    s->stack = GROW_ARRAY(Value, s->stack, s->capacity, newCapacity);
    s->capacity = newCapacity;
}

void ensureCapacity(Stack *s, int size) {
    while (s->capacity <= size) {
        growCapacity(s);
    }
}

void pushValue(Stack *s, Value v) {
#ifdef DEBUG_STACK
    eprintf("pushValue(");
    printValue(v, 0);
    eprintf(") sp = %d, capacity = %d\n", s->sp, s->capacity);
#endif
    if (s->sp == s->capacity) {
        growCapacity(s);
    }
    s->stack[s->sp++] = v;
}

void extendStack(Stack *s, int extra) {
    DEBUG("extendStack(%d)", extra);
    while (s->sp + extra >= s->capacity) {
        growCapacity(s);
    }
    while (extra-- > 0) {
        s->stack[s->sp++] = vVoid;
    }
}

void dumpStack(Stack *s) {
    eprintf("STACK DUMP sp = %d, capacity = %d\n", s->sp, s->capacity);
    eprintf("=================================\n");
    for (int i = 0; i < s->sp; i++) {
        eprintf("[%d] *** ", i);
        printValue(s->stack[i], 0);
        eprintf("\n");
    }
    eprintf("=================================\n");
}

Value popValue(Stack *s) {
    DEBUG("popValue()");
#ifdef SAFETY_CHECKS
    if (s->sp == 0) {
        cant_happen("stack underflow");
    }
#endif
    return s->stack[--s->sp];
}

void markStack(Stack *s) {
    DEBUG("markStack()");
    for (int i = 0; i < s->sp; ++i) {
        markValue(s->stack[i]);
    }
}

Value peekValue(Stack *s, int offset) {
    if (offset < 0) {
        offset = s->sp + offset;
    }
#ifdef SAFETY_CHECKS
    if (offset >= s->sp || offset < 0) {
        cant_happen("peek out of bounds %d/%d", offset, s->sp);
    }
#endif
    return s->stack[offset];
}

void pokeValue(Stack *s, int offset, Value v) {
    if (offset < 0) {
        offset = s->sp + offset;
    }
#ifdef SAFETY_CHECKS
    if (offset >= s->sp || offset < 0) {
        cant_happen("poke out of bounds %d/%d", offset, s->sp);
    }
#endif
    s->stack[offset] = v;
}

Value peekTop(Stack *s) {
#ifdef SAFETY_CHECKS
    if (s->sp == 0) {
        cant_happen("peek top of empty stack not allowed");
    }
#endif
    return s->stack[s->sp - 1];
}

void copyTopToValues(Stack *s, Value *values, int size) {
    COPY_ARRAY(Value, values, &(s->stack[s->sp - size]), size);
}

static void copyToValues(Stack *s, Value *values, int except) {
    COPY_ARRAY(Value, values, s->stack, s->sp - except);
}

static void copyFromValues(Stack *s, Value *values, int size) {
    COPY_ARRAY(Value, s->stack, values, size);
    s->sp = size;
}

static void copyFromSnapshot(Stack *s, Snapshot ss) {
    COPY_ARRAY(Value, s->stack, ss.frame, ss.frameSize);
    s->sp = ss.frameSize;
}

static void copyToSnapshot(Stack *s, Snapshot *ss) {
    if (s->sp > 0) {
        ss->frame = NEW_ARRAY(Value, s->sp);
        copyToValues(s, ss->frame, 0);
    }
    ss->frameSize = s->sp;
}

void copyValues(Value *to, Value *from, int size) {
    COPY_ARRAY(Value, to, from, size);
}

void moveValues(Value *to, Value *from, int size) {
    MOVE_ARRAY(Value, to, from, size);
}

void copyTosToEnv(Stack *s, Env *e, int n) {
    DEBUG("copyTosToEnv, sp = %d, capacity = %d", s->sp, s->capacity);
    copyValues(e->values, &(s->stack[s->sp - n]), n);
}

void snapshotClo(Stack *s, Clo *target, int letRecOffset) {
    DEBUG("snapshotClo, sp = %d, capacity = %d", s->sp, s->capacity);
    Env *env = newEnv(target->env, s->sp - letRecOffset);
    target->env = env;
    copyToValues(s, env->values, letRecOffset);
}

void patchValueList(Stack *s, ValueList *v, int num) {
#ifdef SAFETY_CHECKS
    if (num < 0) {
        cant_happen("negative count");
    }
    if (num > s->sp) {
        cant_happen("not enough values on stack");
    }
    if (s->sp > v->count) {
        cant_happen("not enough space in target");
    }
#endif
    copyValues(&v->values[s->sp - num], &s->stack[s->sp - num], num);
}

void restoreNamespace(Stack *s, ValueList *vl) {
    copyFromValues(s, vl->values, vl->count);
}

ValueList *snapshotNamespace(Stack *s) {
    ValueList *vl = newValueList(s->sp);
    copyToValues(s, vl->values, 0);
    return vl;
}

void patchClo(Stack *s, Clo *target) {
    DEBUG("patchClo, sp = %d, capacity = %d", s->sp, s->capacity);
    target->env->values =
        GROW_ARRAY(Value, target->env->values, target->env->count, s->sp);
    copyToValues(s, target->env->values, 0);
    target->env->count = s->sp;
}

void snapshotKont(Stack *s, Kont *target) {
    DEBUG("snapshotKont, sp = %d, capacity = %d", s->sp, s->capacity);
    copyToSnapshot(s, &target->snapshot);
}

void snapshotFail(Stack *s, Fail *target) {
    DEBUG("snapshotFail, sp = %d, capacity = %d", s->sp, s->capacity);
    copyToSnapshot(s, &target->snapshot);
}

void restoreKont(Stack *s, Kont *source) {
    DEBUG("restoreKont, size = %d, capacity = %d", source->snapshot.frameSize,
          s->capacity);
    copyFromSnapshot(s, source->snapshot);
}

void restoreFail(Stack *s, Fail *source) {
    DEBUG("restoreFail, size = %d, capacity = %d", source->snapshot.frameSize,
          s->capacity);
    copyFromSnapshot(s, source->snapshot);
}

void pushN(Stack *s, int n) {
    for (; n > 0; n--) {
        pushValue(s, vVoid);
    }
}

void popN(Stack *s, int n) {
    s->sp -= n;
#ifdef SAFETY_CHECKS
    if (s->sp < 0) {
        cant_happen("stack underflow in popN");
    }
#endif
}
