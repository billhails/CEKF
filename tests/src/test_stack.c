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
#include "test.h"
#include "memory.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

static Stack S;

void markTestStack() {
    printf("markTestStack()\n");
    markStack(&S);
}

static Value integer(int i) {
    Value v;
    v.type = VALUE_TYPE_STDINT;
    v.val = VALUE_VAL_STDINT(i);
    return v;
}

bool isInteger(Value v, int i) {
    return v.type == VALUE_TYPE_STDINT && v.val.z == i;
}

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    Value v;
    initStack(&S);

    assert(S.capacity == 0);
    assert(S.sp == 0);
    assert(S.stack == NULL);
    assert(frameSize(&S) == 0);

    pushValue(&S, integer(100));
    assert(S.capacity == 8);
    assert(S.sp == 1);
    assert(frameSize(&S) == 1);
    assert(S.stack != NULL);
    assert(frameSize(&S) == 1);
    v = peekValue(&S, 0);
    assert(isInteger(v, 100));

    pushValue(&S, integer(101));
    assert(S.capacity == 8);
    assert(S.sp == 2);
    assert(frameSize(&S) == 2);
    v = peekValue(&S, 0);
    assert(isInteger(v, 100));
    v = peekValue(&S, 1);
    assert(isInteger(v, 101));

    v = popValue(&S);
    assert(isInteger(v, 101));
    assert(S.capacity == 8);
    assert(S.sp == 1);
    assert(frameSize(&S) == 1);

    v = popValue(&S);
    assert(isInteger(v, 100));
    assert(S.capacity == 8);
    assert(S.sp == 0);
    assert(frameSize(&S) == 0);

    pushValue(&S, vTrue);
    pushValue(&S, vFalse);
    pushValue(&S, vTrue);
    assert(S.capacity == 8);
    assert(S.sp == 3);
    assert(frameSize(&S) == 3);

    setFrame(&S, 1);
    assert(S.capacity == 8);
    assert(S.sp == 1);
    assert(frameSize(&S) == 1);

    pushValue(&S, vFalse);
    assert(S.capacity == 8);
    assert(S.sp == 2);
    assert(frameSize(&S) == 2);

    v.type = VALUE_TYPE_STDINT;
    v.val = VALUE_VAL_STDINT(100);
    pushValue(&S, v);
    assert(S.capacity == 8);
    assert(S.sp == 3);
    assert(frameSize(&S) == 3);

    v.val = VALUE_VAL_STDINT(101);
    pushValue(&S, v);
    assert(S.capacity == 8);
    assert(S.sp == 4);
    assert(frameSize(&S) == 4);

    v.val = VALUE_VAL_STDINT(102);
    pushValue(&S, v);
    assert(S.capacity == 8);
    assert(S.sp == 5);
    assert(frameSize(&S) == 5);

    v = peekValue(&S, 0);
    assert(v.type == VALUE_TYPE_STDINT);

    v = peekValue(&S, 1);
    assert(v.type == VALUE_TYPE_STDINT);

    v = peekValue(&S, 2);
    assert(v.type == VALUE_TYPE_STDINT);
    assert(v.val.z == 100);

    v = peekValue(&S, 3);
    assert(v.type == VALUE_TYPE_STDINT);
    assert(v.val.z == 101);

    v = peekValue(&S, 4);
    assert(v.type == VALUE_TYPE_STDINT);
    assert(v.val.z == 102);

    setFrame(&S, 1);
    assert(S.capacity == 8);
    assert(S.sp == 1);
    assert(frameSize(&S) == 1);

}
