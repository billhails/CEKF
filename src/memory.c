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

// memory management and garbage collection

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "annotate.h"
#include "memory.h"
#include "step.h"
#include "anf.h"
#include "cekf.h"
#include "module.h"
#include "symbol.h"

static int bytesAllocated = 0;
static int nextGC = 0;
static bool gcEnabled = true;
static int numAlloc = 0;
static int maxMem = 0;
static int numGc = 0;

static void collectGarbage();

static Header *lastAlloc = NULL;

/*
#define MAX_PROTECTION 256
*/

typedef struct ProtectionStack {
    Header header;
    int capacity;
    int sp;
    Header *stack[0];
} ProtectionStack;

void reportMemory() {
    printf("gc runs: %d, current memory: %d, max memory: %d\n", numGc,
           bytesAllocated, maxMem);
}

static ProtectionStack *protected = NULL;

static Header *allocated = NULL;

void validateLastAlloc() {
    lastAlloc = NULL;
}

const char *typeName(ObjType type, void *p) {
    switch (type) {
        case OBJTYPE_HASHSYMBOL:
            return "var";
        case OBJTYPE_CLO:
            return "clo";
        case OBJTYPE_ENV:
            return "env";
        case OBJTYPE_FAIL:
            return "fail";
        case OBJTYPE_KONT:
            return "kont";
        case OBJTYPE_VEC:
            return "vec";
        case OBJTYPE_VALUELIST:
            return "valuelist";
        case OBJTYPE_HASHTABLE:
            return "hashtable";
        case OBJTYPE_PROTECTION:
            return "protection";
        case OBJTYPE_BIGINT:
            return "bigint";
        case OBJTYPE_PMMODULE:
            return "pmmodule";
            ANF_OBJTYPE_CASES()
                return typenameAnfObj(type);
            AST_OBJTYPE_CASES()
                return typenameAstObj(type);
            LAMBDA_OBJTYPE_CASES()
                return typenameLambdaObj(type);
            TPMC_OBJTYPE_CASES()
                return typenameTpmcObj(type);
            TC_OBJTYPE_CASES()
                return typenameTcObj(type);
        default:
            cant_happen("unrecognised ObjType %d in typeName at %p", type, p);
    }
}

char *safeStrdup(char *s) {
    char *t = strdup(s);
    if (t == NULL)
        exit(1);
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

#define INITIAL_PROTECTION 8
#define NEW_PROTECT(size) ((ProtectionStack *)allocate(sizeof(ProtectionStack) + size * sizeof(Header *), OBJTYPE_PROTECTION))
#define FREE_PROTECT(p) ((void)reallocate(p, sizeof(ProtectionStack) + ((ProtectionStack *)p)->capacity * sizeof(Header *), 0))

void initProtection(void) {
#ifdef DEBUG_LOG_GC
    eprintf("initProtection\n");
#endif
    protected = NEW_PROTECT(INITIAL_PROTECTION);
    protected->capacity = INITIAL_PROTECTION;
    protected->sp = 0;
}

void replaceProtect(int i, Header *obj) {
    protected->stack[i] = obj;
}

int protect(Header *obj) {
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "PROTECT(%p:%s) -> %d (%d)\n", obj,
            (obj == NULL ? "NULL" : typeName(obj->type, obj)), protected->sp,
            protected->capacity);
#endif
    if (obj == NULL)
        return protected->sp;

    protected->stack[protected->sp++] = obj;
    if (protected->sp == protected->capacity) {
#ifdef DEBUG_LOG_GC
        eprintf("protect old stack: %p\n", (void *) protected);
#endif
        ProtectionStack *tmp = NEW_PROTECT(protected->capacity * 2);
        tmp->capacity = protected->capacity * 2;
        for (tmp->sp = 0; tmp->sp < protected->sp; tmp->sp++) {
            tmp->stack[tmp->sp] = protected->stack[tmp->sp];
        }
        protected = tmp;
#ifdef DEBUG_LOG_GC
        eprintf("protect new stack: %p\n", (void *) protected);
#endif
    }
#ifdef DEBUG_LOG_GC
    eprintf("PROTECT(%s) done -> %d (%d)\n", typeName(obj->type, obj),
            protected->sp, protected->capacity);
#endif
    return protected->sp - 1;
}

void unProtect(int index) {
#ifdef DEBUG_LOG_GC
    eprintf("UNPROTECT(%d)\n", index);
#endif
    protected->sp = index;
}

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
#ifdef DEBUG_LOG_GC
    eprintf
        ("reallocate bytesAllocated %d + newsize %lu - oldsize %lu [%d] pointer %p\n",
         bytesAllocated, newSize, oldSize, numAlloc, pointer);
    if (newSize > oldSize)
        numAlloc++;
    if (newSize < oldSize)
        numAlloc--;
    if (numAlloc < 0)
        cant_happen("more frees than mallocs!");

#endif
    bytesAllocated += newSize - oldSize;
    if (bytesAllocated < 0)
        cant_happen("more bytes freed than allocated! %d += %lu - %lu [%d]",
                    bytesAllocated, newSize, oldSize, numAlloc);

    if (bytesAllocated > maxMem) {
        maxMem = bytesAllocated;
    }

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
        char *zerop = (char *) pointer;
        for (size_t i = 0; i < oldSize; i++) {
            zerop[i] = '\0';
        }
#endif
        free(pointer);
        return NULL;
    }

    void *result = realloc(pointer, newSize);
    if (result == NULL)
        exit(1);
#ifdef DEBUG_LOG_GC
    eprintf("reallocate ptr %p => %p\n", pointer, result);
#endif
    return result;
}

void *allocate(size_t size, ObjType type) {
#ifdef DEBUG_LOG_GC
    eprintf("allocate type %s %d %lu [%d]\n", typeName(type, 0),
            bytesAllocated, size, numAlloc);
#endif
    Header *newObj = (Header *) reallocate(NULL, (size_t) 0, size);
    newObj->type = type;
    newObj->keep = false;
    newObj->next = allocated;
    allocated = newObj;
    if (gcEnabled) {
        lastAlloc = newObj;
    } else {
        lastAlloc = NULL;
    }
    return (void *) newObj;
}

static void markProtectionObj(Header *h) {
#ifdef DEBUG_LOG_GC
    eprintf("markProtectionObj\n");
#endif
    MARK(h);
    ProtectionStack *protected = (ProtectionStack *) h;
    for (int i = 0; i < protected->sp; ++i) {
        markObj(protected->stack[i], i);
    }
#ifdef DEBUG_LOG_GC
    eprintf("markProtectionObj done\n");
#endif
}

void markObj(Header *h, int i) {
#ifdef DEBUG_LOG_GC
    eprintf("markObj [%d]%s %p\n", i, typeName(h->type, h), h);
#endif
    switch (h->type) {
        case OBJTYPE_CLO:
        case OBJTYPE_ENV:
        case OBJTYPE_FAIL:
        case OBJTYPE_KONT:
        case OBJTYPE_VEC:
        case OBJTYPE_VALUELIST:
            markCekfObj(h);
            break;
        case OBJTYPE_HASHTABLE:
            markHashTableObj(h);
            break;
        case OBJTYPE_HASHSYMBOL:
            markHashSymbolObj(h);
            break;
        case OBJTYPE_PROTECTION:
            markProtectionObj(h);
            break;
        case OBJTYPE_PMMODULE:
            markPmModule(h);
            break;
            ANF_OBJTYPE_CASES()
                markAnfObj(h);
            break;
            AST_OBJTYPE_CASES()
                markAstObj(h);
            break;
            LAMBDA_OBJTYPE_CASES()
                markLambdaObj(h);
            break;
            TPMC_OBJTYPE_CASES()
                markTpmcObj(h);
            break;
            TC_OBJTYPE_CASES()
                markTcObj(h);
            break;
        case OBJTYPE_BIGINT:
            markBigInt((BigInt *) h);
            break;
        default:
            cant_happen("unrecognised ObjType %d in markObj at [%d]", h->type,
                        i);
    }
}

static void freeProtectionObj(Header *h) {
    FREE_PROTECT(h);
}

void freeObj(Header *h) {
    switch (h->type) {
        case OBJTYPE_CLO:
        case OBJTYPE_ENV:
        case OBJTYPE_FAIL:
        case OBJTYPE_KONT:
        case OBJTYPE_VEC:
        case OBJTYPE_VALUELIST:
            freeCekfObj(h);
            break;
        case OBJTYPE_BIGINT:
            freeBigInt((BigInt *) h);
            break;
        case OBJTYPE_HASHTABLE:
            freeHashTableObj(h);
            break;
        case OBJTYPE_HASHSYMBOL:
            freeHashSymbolObj(h);
            break;
        case OBJTYPE_PROTECTION:
            freeProtectionObj(h);
            break;
        case OBJTYPE_PMMODULE:
            freePmModule(h);
            break;
            ANF_OBJTYPE_CASES()
                freeAnfObj(h);
            break;
            AST_OBJTYPE_CASES()
                freeAstObj(h);
            break;
            LAMBDA_OBJTYPE_CASES()
                freeLambdaObj(h);
            break;
            TPMC_OBJTYPE_CASES()
                freeTpmcObj(h);
            break;
            TC_OBJTYPE_CASES()
                freeTcObj(h);
            break;
        default:
            cant_happen("unrecognised ObjType %d in freeObj at %p", h->type,
                        (void *) h);
    }
}

static void markProtected() {
    if (protected != NULL)
        markProtectionObj((Header *) protected);
}

static void mark() {
    markCEKF();
    markProtected();
#ifdef DEBUG_LOG_GC
    eprintf("starting markVarTable\n");
#endif
    markVarTable();
#ifdef DEBUG_LOG_GC
    eprintf("markVarTable done\n");
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
            eprintf("sweep discard %p\n", (void *) current);
            eprintf("              type %s\n",
                    typeName(current->type, current));
#endif
            *previous = current->next;
            freeObj(current);
        }
        current = *previous;
    }
}

static void collectGarbage() {
    if (!gcEnabled)
        return;
    numGc++;
#ifdef DEBUG_LOG_GC
    eprintf("GC started\n");
#endif
#ifdef DEBUG_GC
    eprintf("GC\n");
#endif
    mark();
#ifdef DEBUG_ALLOC
    if (lastAlloc && !MARKED(lastAlloc)) {
        cant_happen("alloc of %s (%p) immediately dropped",
                    typeName(lastAlloc->type, lastAlloc), lastAlloc);
    }
#endif
    sweep();
    nextGC = bytesAllocated * 2;
#ifdef DEBUG_LOG_GC
    eprintf("GC finished, bytesAllocated %d, nextGC %d\n", bytesAllocated,
            nextGC);
#endif
}
