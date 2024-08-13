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

static Stack *S;

void markTestStack() {
    printf("markTestStack()\n");
    markStack(S);
}

static Value integer(int i) {
    Value v;
    v.type = VALUE_TYPE_STDINT;
    v.val = VALUE_VAL_STDINT(i);
    return v;
}

bool isInteger(Value v, int i) {
    return v.type == VALUE_TYPE_STDINT && v.val.stdint == i;
}

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    Value v;
    initProtection();
    S = newStack();

    assert(S->entries_capacity == 8);
    assert(S->entries != NULL);
    assert(totalSizeStack(S) == 0);

    pushStackEntry(S, integer(100));
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 1);
    assert(S->entries != NULL);
    assert(totalSizeStack(S) == 1);
    v = peeknStack(S, 0);
    assert(isInteger(v, 100));

    pushStackEntry(S, integer(101));
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 2);
    v = peeknStack(S, 0);
    assert(isInteger(v, 100));
    v = peeknStack(S, 1);
    assert(isInteger(v, 101));

    v = popStackEntry(S);
    assert(isInteger(v, 101));
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 1);

    v = popStackEntry(S);
    assert(isInteger(v, 100));
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 0);

    pushStackEntry(S, vTrue);
    pushStackEntry(S, vFalse);
    pushStackEntry(S, vTrue);
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 3);

    moveStack(S, 0, 1);
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 1);

    pushStackEntry(S, vFalse);
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 2);

    v.type = VALUE_TYPE_STDINT;
    v.val = VALUE_VAL_STDINT(100);
    pushStackEntry(S, v);
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 3);

    v.val = VALUE_VAL_STDINT(101);
    pushStackEntry(S, v);
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 4);

    v.val = VALUE_VAL_STDINT(102);
    pushStackEntry(S, v);
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 5);

    v = peeknStack(S, 0);
    assert(v.type == VALUE_TYPE_STDINT);

    v = peeknStack(S, 1);
    assert(v.type == VALUE_TYPE_STDINT);

    v = peeknStack(S, 2);
    assert(v.type == VALUE_TYPE_STDINT);
    assert(v.val.stdint == 100);

    v = peeknStack(S, 3);
    assert(v.type == VALUE_TYPE_STDINT);
    assert(v.val.stdint == 101);

    v = peeknStack(S, 4);
    assert(v.type == VALUE_TYPE_STDINT);
    assert(v.val.stdint == 102);

    moveStack(S, 0, 1);
    assert(S->entries_capacity == 8);
    assert(totalSizeStack(S) == 1);

}
