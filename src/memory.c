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

#include "common.h"
#include "memory.h"
#include "step.h"

static int bytesAllocated = 0;
static int nextGC = 0;

static void collectGarbage();

#define MAX_PROTECTION 256

static Header *protected[MAX_PROTECTION];
static int protectedIndex = 0;
static Header *allocated = NULL;

#ifdef DEBUG_LOG_GC
const char *typeName(ObjType type) {
    switch (type) {
        case OBJTYPE_AMB:
            return "amb";
        case OBJTYPE_APPLY:
            return "apply";
        case OBJTYPE_BINDINGS:
            return "bindings";
        case OBJTYPE_COND:
            return "cond";
        case OBJTYPE_EXP:
            return "exp";
        case OBJTYPE_EXPLIST:
            return "explist";
        case OBJTYPE_LAM:
            return "lam";
        case OBJTYPE_LET:
            return "let";
        case OBJTYPE_LETREC:
            return "letrec";
        case OBJTYPE_PRIMAPP:
            return "primapp";
        case OBJTYPE_VAR:
            return "var";
        case OBJTYPE_VARLIST:
            return "varlist";
        case OBJTYPE_CLO:
            return "clo";
        case OBJTYPE_ENV:
            return "env";
        case OBJTYPE_FAIL:
            return "fail";
        case OBJTYPE_KONT:
            return "kont";
        case OBJTYPE_VALUELIST:
            return "valuelist";
        case OBJTYPE_HASHTABLE:
            return "hashtable";
        default:
            return "???";
    }
}
#endif

int protect(Header *obj) {
#ifdef DEBUG_LOG_GC
    fprintf(
        stderr,
        "PROTECT(%s) -> %d\n",
        (obj == NULL ? "NULL" : typeName(obj->type)),
        protectedIndex
    );
#endif
    if (obj == NULL) return protectedIndex;

    if (protectedIndex == MAX_PROTECTION) {
        fprintf(stderr, "maximum GC protected limit exceeded\n");
        exit(1);
    }

    protected[protectedIndex++] = obj;
    return protectedIndex - 1;
}

void unProtect(int index) {
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "UNPROTECT(%d)\n", index);
#endif
    protectedIndex = index;
}

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
    bytesAllocated += newSize - oldSize;

    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#else
        if (bytesAllocated > nextGC) {
            collectGarbage();
        }
#endif
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
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "allocate type %s\n", typeName(type));
#endif
    Header *newObj = (Header *)reallocate(NULL, (size_t)0, size);
    newObj->type = type;
    newObj->keep = false;
    newObj->next = allocated;
    allocated = newObj;
    return (void *)newObj;
}

void markObj(Header *h) {
    switch (h->type) {
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
            markExpObj(h);
            break;
        case OBJTYPE_CLO:
        case OBJTYPE_ENV:
        case OBJTYPE_FAIL:
        case OBJTYPE_KONT:
        case OBJTYPE_VALUELIST:
            markCekfObj(h);
            break;
        case OBJTYPE_HASHTABLE:
            markHashTableObj(h);
            break;
    }
}

void freeObj(Header *h) {
    switch (h->type) {
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
            freeExpObj(h);
            break;
        case OBJTYPE_CLO:
        case OBJTYPE_ENV:
        case OBJTYPE_FAIL:
        case OBJTYPE_KONT:
        case OBJTYPE_VALUELIST:
            freeCekfObj(h);
            break;
        case OBJTYPE_HASHTABLE:
            freeHashTableObj(h);
            break;
    }
}

static void markProtected() {
    for (int i = 0; i < protectedIndex; ++i) {
        markObj(protected[i]);
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
#ifdef DEBUG_LOG_GC
            fprintf(stderr, "sweep keep type %s\n", typeName(current->type));
#endif
            previous = &current->next;
            current->keep = false;
        } else {
#ifdef DEBUG_LOG_GC
            fprintf(stderr, "sweep discard type %s\n", typeName(current->type));
#endif
            *previous = current->next;
            freeObj(current);
        }
        current = *previous;
    }
}

static void collectGarbage() {
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "GC started\n");
#endif
    mark();
    sweep();
    nextGC = bytesAllocated * 2;
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "GC finished, bytesAllocated %d, nextGC %d\n", bytesAllocated, nextGC);
#endif
}
