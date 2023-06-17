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
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

#include "common.h"
#include "analysis.h"
#include "memory.h"
#include "step.h"
#include "exp.h"
#include "cekf.h"

static int bytesAllocated = 0;
static int nextGC = 0;
static bool gcEnabled = true;
static int numAlloc = 0;

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
        case OBJTYPE_BOOL:
            return "bool";
        case OBJTYPE_APPLY:
            return "apply";
        case OBJTYPE_BINDINGS:
            return "bindings";
        case OBJTYPE_COND:
            return "cond";
        case OBJTYPE_AEXP:
            return "aexp";
        case OBJTYPE_CEXP:
            return "cexp";
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
        case OBJTYPE_UNARYAPP:
            return "unaryapp";
        case OBJTYPE_HASHSYMBOL:
            return "var";
        case OBJTYPE_ANNOTATEDVAR:
            return "annotatedvar";
        case OBJTYPE_VARLIST:
            return "varlist";
        case OBJTYPE_CLO:
            return "clo";
        case OBJTYPE_ENV:
            return "env";
        case OBJTYPE_CTENV:
            return "ctenv";
        case OBJTYPE_FAIL:
            return "fail";
        case OBJTYPE_KONT:
            return "kont";
        case OBJTYPE_CONS:
            return "cons";
        case OBJTYPE_VEC:
            return "vec";
        case OBJTYPE_VALUELIST:
            return "valuelist";
        case OBJTYPE_HASHTABLE:
            return "hashtable";
        case OBJTYPE_WRESULT:
            return "wresult";
        TIN_OBJTYPE_CASES()
            typenameTinObj(type);
            break;
        AST_OBJTYPE_CASES()
            typenameAstObj(type);
            break;
        default:
            cant_happen("unrecognised ObjType %d in typeName", type);
    }
}
#endif

char *safeStrdup(char *s) {
    char *t = strdup(s);
    if (t == NULL) exit(1);
    return t;
}

bool enableGC() {
    bool previous = gcEnabled;
    gcEnabled = true;
    return previous;
}

bool disableGC() {
    bool previous = gcEnabled;
    gcEnabled = false;
    return previous;
}

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
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "reallocate %d + %lu - %lu [%d]\n", bytesAllocated, newSize, oldSize, numAlloc);
    if (newSize > oldSize)
        numAlloc++;
    if (newSize < oldSize)
        numAlloc--;
    if (numAlloc < 0)
        cant_happen("more frees than mallocs!");

#endif
    bytesAllocated += newSize - oldSize;
    if (bytesAllocated < 0)
        cant_happen("more bytes freed than allocated! %d += %lu - %lu [%d]", bytesAllocated, newSize, oldSize, numAlloc);

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
#ifdef DEBUG_STRESS_GC
        char *zerop = (char *)pointer;
        for (int i = 0; i < oldSize; i++) {
            zerop[i] = '\0';
        }
#endif
        free(pointer);
        return NULL;
    }

    void *result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
}

void *allocate(size_t size, ObjType type) {
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "allocate type %s %d %lu [%d]\n", typeName(type), bytesAllocated, size, numAlloc);
#endif
    Header *newObj = (Header *)reallocate(NULL, (size_t)0, size);
    newObj->type = type;
    newObj->keep = false;
    newObj->next = allocated;
    allocated = newObj;
    return (void *)newObj;
}

void markObj(Header *h) {
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "markObj %s\n", typeName(h->type));
#endif
    switch (h->type) {
        case OBJTYPE_AMB:
        case OBJTYPE_APPLY:
        case OBJTYPE_BINDINGS:
        case OBJTYPE_BOOL:
        case OBJTYPE_COND:
        case OBJTYPE_AEXP:
        case OBJTYPE_CEXP:
        case OBJTYPE_EXP:
        case OBJTYPE_EXPLIST:
        case OBJTYPE_LAM:
        case OBJTYPE_LET:
        case OBJTYPE_LETREC:
        case OBJTYPE_PRIMAPP:
        case OBJTYPE_ANNOTATEDVAR:
        case OBJTYPE_VARLIST:
        case OBJTYPE_MAKEVEC:
            markExpObj(h);
            break;
        case OBJTYPE_CLO:
        case OBJTYPE_ENV:
        case OBJTYPE_FAIL:
        case OBJTYPE_KONT:
        case OBJTYPE_CONS:
        case OBJTYPE_VEC:
        case OBJTYPE_VALUELIST:
            markCekfObj(h);
            break;
        case OBJTYPE_CTENV:
            markCTEnv(h);
            break;
        case OBJTYPE_HASHTABLE:
            markHashTableObj(h);
            break;
        case OBJTYPE_HASHSYMBOL:
            markHashSymbolObj(h);
            break;
        case OBJTYPE_WRESULT:
            markWResultObj(h);
            break;
        TIN_OBJTYPE_CASES()
            markTinObj(h);
            break;
        AST_OBJTYPE_CASES()
            markAstObj(h);
            break;
        default:
            cant_happen("unrecognised ObjType %d in markObj", h->type);
    }
}

void freeObj(Header *h) {
    switch (h->type) {
        case OBJTYPE_AMB:
        case OBJTYPE_APPLY:
        case OBJTYPE_BINDINGS:
        case OBJTYPE_BOOL:
        case OBJTYPE_COND:
        case OBJTYPE_AEXP:
        case OBJTYPE_CEXP:
        case OBJTYPE_EXP:
        case OBJTYPE_EXPLIST:
        case OBJTYPE_LAM:
        case OBJTYPE_LET:
        case OBJTYPE_LETREC:
        case OBJTYPE_PRIMAPP:
        case OBJTYPE_UNARYAPP:
        case OBJTYPE_ANNOTATEDVAR:
        case OBJTYPE_VARLIST:
        case OBJTYPE_MAKEVEC:
            freeExpObj(h);
            break;
        case OBJTYPE_CLO:
        case OBJTYPE_ENV:
        case OBJTYPE_FAIL:
        case OBJTYPE_KONT:
        case OBJTYPE_CONS:
        case OBJTYPE_VEC:
        case OBJTYPE_VALUELIST:
            freeCekfObj(h);
            break;
        case OBJTYPE_CTENV:
            freeCTEnv(h);
            break;
        case OBJTYPE_HASHTABLE:
            freeHashTableObj(h);
            break;
        case OBJTYPE_HASHSYMBOL:
            freeHashSymbolObj(h);
            break;
        case OBJTYPE_WRESULT:
            freeWResultObj(h);
            break;
        TIN_OBJTYPE_CASES()
            freeTinObj(h);
            break;
        AST_OBJTYPE_CASES()
            freeAstObj(h);
            break;
        default:
            cant_happen("unrecognised ObjType %d in freeObj", h->type);
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
    markVarTable();
#ifdef TEST_STACK
    markTestStack();
#endif
}

static void sweep() {
    Header *current = allocated;
    Header **previous = &allocated;
    while (current != NULL) {
        if (current->keep) {
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
    if (!gcEnabled) return;
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
