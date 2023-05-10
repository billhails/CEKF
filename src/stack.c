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
#include "stack.h"
#include "memory.h"

#include <stddef.h>

void initStack(Stack *stack) {
    stack->sp = 0;
    stack->capacity = 0;
    stack->stack = NULL;
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
