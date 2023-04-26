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
#include <stdio.h>
#include <stdlib.h>

#include "memory.h"
#include "step.h"

static int bytesAllocated = 0;
static int nextGC = 0;

static void collectGarbage();

#define MAX_PROTECTION 256

static Header *protected[MAX_PROTECTION];
static int protectedIndex = 0;
static Header *allocated = NULL;

int protect(Header *obj) {
    if (obj == NULL) return protectedIndex;

    if (protectedIndex == MAX_PROTECTION) {
        fprintf(stderr, "maximum GC protected limit exceeded\n");
        exit(1);
    }

    protected[protectedIndex++] = obj;
    return protectedIndex - 1;
}

void unProtect(int index) {
    protectedIndex = index;
}

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
    bytesAllocated += newSize - oldSize;

    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif
        if (bytesAllocated > nextGC) {
            collectGarbage();
        }
    }

    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void *result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
}

void *allocate(size_t size, ObjType type) {
    Header *newObj = (Header *)reallocate(NULL, (size_t)0, size);
    newObj->type = type;
    newObj->keep = false;
    newObj->next = allocated;
    allocated = newObj;
    return (void *)newObj;
}

static void markProtected() {
    for (int i = 0; i < protectedIndex; ++i) {
        switch (protected[i]->type) {
            case OBJTYPE_AMB:
            case OBJTYPE_APPLY:
            case OBJTYPE_BINDINGS:
            case OBJTYPE_COND:
            case OBJTYPE_EXP:
            case OBJTYPE_EXPLIST:
            case OBJTYPE_LAM:
            case OBJTYPE_LET:
            case OBJTYPE_LETREC:
            case OBJTYPE_PRIMAPP:
            case OBJTYPE_VAR:
            case OBJTYPE_VARLIST:
                markExpObj(protected[i]);
                break;
            case OBJTYPE_CLO:
            case OBJTYPE_ENV:
            case OBJTYPE_FAIL:
            case OBJTYPE_KONT:
            case OBJTYPE_VALUE:
            case OBJTYPE_VALUELIST:
                markCekfObj(protected[i]);
                break;
        }
    }
}

static void mark() {
    markCEKF();
    markProtected();
}

static void sweep() {
    Header *current = allocated;
    Header **previous = &allocated;
    while (current != NULL) {
        if (current->keep) {
            previous = &current->next;
            current->keep = false;
        } else {
            *previous = current->next;
            reallocate(current, (size_t)0, (size_t)0);
        }
        current = current->next;
    }
}

static void collectGarbage() {
    mark();
    sweep();
    nextGC = bytesAllocated * 2;
}
