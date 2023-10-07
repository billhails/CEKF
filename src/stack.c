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
#ifdef DEBUG_STACK
#include "debug.h"
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

static int succ(int i, int modulus) {
    return i + 1;
}

int frameSize(Stack *s) {
    return s->sp;
}

void setFrame(Stack *s, int n) {
#ifdef DEBUG_STACK
    printf("setFrame(%d)\n", n);
#endif
    if (n) {
        MOVE_ARRAY(Value, s->stack, &s->stack[s->sp - n], n);
    }
    s->sp = n;
}

void clearFrame(Stack *s) {
#ifdef DEBUG_STACK
    printf("clearFrame()\n");
#endif
    s->sp = 0;
}

static void growCapacity(Stack *s, int newCapacity) {
#ifdef DEBUG_STACK
    printf("growCapacity(%d)\n", newCapacity);
#endif
    s->stack = GROW_ARRAY(Value, s->stack, s->capacity, newCapacity);
    s->capacity = newCapacity;
}

void pushValue(Stack *s, Value v) {
#ifdef DEBUG_STACK
    printf("pushValue(");
    printValue(v, 0);
    printf(") sp = %d, capacity = %d\n", s->sp, s->capacity);
#endif
    if (s->sp == s->capacity) {
        growCapacity(s, s->capacity < 8 ? 8 : s->capacity * 2);
    }
    s->stack[s->sp++] = v;
}

Value popValue(Stack *s) {
#ifdef DEBUG_STACK
    printf("popValue()\n");
#endif
    if (s->sp == 0) {
        cant_happen("stack underflow");
    }
    return s->stack[--s->sp];
}

void markStack(Stack *s) {
#ifdef DEBUG_STACK
    printf("markStack()\n");
#endif
    for (int i = 0; i < s->sp; ++i) {
        markValue(s->stack[i]);
    }
}

Value peekValue(Stack *s, int offset) {
    if (offset >= s->sp) {
        cant_happen("peek beyond top of stack not allowed");
    }
    return s->stack[offset];
}

void copyTopToValues(Stack *s, Value *values, int size) {
    COPY_ARRAY(Value, values, &(s->stack[s->sp - size]), size);
}

static void copyToValues(Stack *s, Value *values, int size) {
    COPY_ARRAY(Value, values, s->stack, s->sp - size);
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

void snapshotClo(Stack *s, Clo *target, int letRecOffset) {
#ifdef DEBUG_STACK
    printf("snapshotClo, sp = %d, capacity = %d\n", s->sp, s->capacity);
#endif
    Env *env = newEnv(target->rho, s->sp - letRecOffset);
    target->rho = env;
    copyToValues(s, env->values, letRecOffset);
}

void patchClo(Stack *s, Clo *target) {
#ifdef DEBUG_STACK
    printf("patchClo, sp = %d, capacity = %d\n", s->sp, s->capacity);
#endif
    target->rho->values = GROW_ARRAY(Value, target->rho->values, target->rho->count, s->sp);
    copyToValues(s, target->rho->values, 0);
    target->rho->count = s->sp;
}

void snapshotKont(Stack *s, Kont *target) {
#ifdef DEBUG_STACK
    printf("snapshotKont, sp = %d, capacity = %d\n", s->sp, s->capacity);
#endif
    copyToSnapshot(s, &target->snapshot);
}

void snapshotFail(Stack *s, Fail *target) {
#ifdef DEBUG_STACK
    printf("snapshotFail, sp = %d, capacity = %d\n", s->sp, s->capacity);
#endif
    copyToSnapshot(s, &target->snapshot);
}

void restoreKont(Stack *s, Kont *source) {
#ifdef DEBUG_STACK
    printf("restoreKont, size = %d, capacity = %d\n", source->snapshot.frameSize, s->capacity);
#endif
    copyFromSnapshot(s, source->snapshot);
}

void restoreFail(Stack *s, Fail *source) {
#ifdef DEBUG_STACK
    printf("restoreFail, size = %d, capacity = %d\n", source->snapshot.frameSize, s->capacity);
#endif
    copyFromSnapshot(s, source->snapshot);
}

void pushN(Stack *s, int n) {
    for (; n > 0; n--) {
        pushValue(s, vVoid);
    }
}

void popN(Stack *s, int n) {
    s->sp -= n;
    if (s->sp < 0) {
        cant_happen("stack underflow in popN");
    }
}

#ifdef TEST_STACK

#define EXPECT(EXP) do { if (EXP) printf("assertion " #EXP " passed\n"); else printf("ASSERTION " #EXP " FAILED\n"); } while(0)

static Stack S;

void markTestStack() {
    printf("markTestStack()\n");
    markStack(&S);
}

static Value integer(int i) {
    Value v;
    v.type = VALUE_TYPE_INTEGER;
    v.val = VALUE_VAL_INTEGER(i);
    return v;
}

bool isInteger(Value v, int i) {
    return v.type == VALUE_TYPE_INTEGER && v.val.z == i;
}

void testStack() {
    Value v;
    initStack(&S);

    EXPECT(S.pushable == false);
    EXPECT(S.popable == false);
    EXPECT(S.capacity == 0);
    EXPECT(S.sp == 0);
    EXPECT(S.fp == 0);
    EXPECT(S.stack == NULL);
    EXPECT(frameSize(&S) == 0);

    pushValue(&S, integer(100));
    EXPECT(S.capacity == 1);
    EXPECT(S.fp == 0);
    EXPECT(S.sp == 0);
    EXPECT(frameSize(&S) == 1);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == false);
    EXPECT(S.stack != NULL);
    EXPECT(frameSize(&S) == 1);
    v = peekValue(&S, 0);
    EXPECT(isInteger(v, 100));

    pushValue(&S, integer(101));
    EXPECT(S.capacity == 2);
    EXPECT(S.fp == 0);
    EXPECT(S.sp == 0);
    EXPECT(frameSize(&S) == 2);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == false);
    v = peekValue(&S, 0);
    EXPECT(isInteger(v, 100));
    v = peekValue(&S, 1);
    EXPECT(isInteger(v, 101);

    v = popValue(&S);
    EXPECT(isInteger(v, 101));
    EXPECT(S.capacity == 2);
    EXPECT(S.fp == 0);
    EXPECT(S.sp == 1);
    EXPECT(frameSize(&S) == 1);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == true);

    v = popValue(&S);
    EXPECT(isInteger(v, 100));
    EXPECT(S.capacity == 2);
    EXPECT(S.fp == 0);
    EXPECT(S.sp == 0);
    EXPECT(frameSize(&S) == 0);
    EXPECT(S.popable == false);
    EXPECT(S.pushable == true);

    pushValue(&S, vTrue);
    pushValue(&S, vFalse);
    pushValue(&S, vTrue);
    EXPECT(S.capacity == 4);
    EXPECT(S.fp == 0);
    EXPECT(S.sp == 3);
    EXPECT(frameSize(&S) == 3);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == true);

    setFrame(&S, 1);
    EXPECT(S.capacity == 4);
    EXPECT(S.fp == 2);
    EXPECT(S.sp == 3);
    EXPECT(frameSize(&S) == 1);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == true);

    pushValue(&S, vFalse);
    EXPECT(S.capacity == 4);
    EXPECT(S.fp == 2);
    EXPECT(S.sp == 0);
    EXPECT(frameSize(&S) == 2);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == true);

    v.type = VALUE_TYPE_INTEGER;
    v.val = VALUE_VAL_INTEGER(100);
    pushValue(&S, v);
    EXPECT(S.capacity == 4);
    EXPECT(S.fp == 2);
    EXPECT(S.sp == 1);
    EXPECT(frameSize(&S) == 3);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == true);

    v.val = VALUE_VAL_INTEGER(101);
    pushValue(&S, v);
    EXPECT(S.capacity == 4);
    EXPECT(S.fp == 2);
    EXPECT(S.sp == 2);
    EXPECT(frameSize(&S) == 4);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == false);

    v.val = VALUE_VAL_INTEGER(102);
    pushValue(&S, v);
    EXPECT(S.capacity == 8);
    EXPECT(S.fp == 0);
    EXPECT(S.sp == 5);
    EXPECT(frameSize(&S) == 5);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == true);

    v = peekValue(&S, 0);
    EXPECT(v.type == VALUE_TYPE_TRUE);

    v = peekValue(&S, 1);
    EXPECT(v.type == VALUE_TYPE_FALSE);

    v = peekValue(&S, 2);
    EXPECT(v.type == VALUE_TYPE_INTEGER);
    EXPECT(v.val.z == 100);

    v = peekValue(&S, 3);
    EXPECT(v.type == VALUE_TYPE_INTEGER);
    EXPECT(v.val.z == 101);

    v = peekValue(&S, 4);
    EXPECT(v.type == VALUE_TYPE_INTEGER);
    EXPECT(v.val.z == 102);

    setFrame(&S, 1);
    EXPECT(S.capacity == 8);
    EXPECT(S.fp == 4);
    EXPECT(S.sp == 5);
    EXPECT(frameSize(&S) == 1);
    EXPECT(S.popable == true);
    EXPECT(S.pushable == true);

}
#endif
