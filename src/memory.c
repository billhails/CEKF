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

/*
#define MAX_PROTECTION 256
*/

typedef struct ProtectionStack {
    Header header;
    int capacity;
    int sp;
    Header *stack[0];
} ProtectionStack;

/*
static Header *protected[MAX_PROTECTION];
static int protectedIndex = 0;
*/

static ProtectionStack *protected = NULL;

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
        case OBJTYPE_PROTECTION:
            return "protection";
        TIN_OBJTYPE_CASES()
            return typenameTinObj(type);
        AST_OBJTYPE_CASES()
            return typenameAstObj(type);
        LAMBDA_OBJTYPE_CASES()
            return typenameLambdaObj(type);
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

#define INITIAL_PROTECTION 8
#define NEW_PROTECT(size) ((ProtectionStack *)allocate(sizeof(ProtectionStack) + size * sizeof(Header *), OBJTYPE_PROTECTION))
#define FREE_PROTECT(p) ((void)reallocate(p, sizeof(ProtectionStack) + ((ProtectionStack *)p)->capacity * sizeof(Header *), 0))

void initProtection(void) {
    protected = NEW_PROTECT(INITIAL_PROTECTION);
    protected->capacity = INITIAL_PROTECTION;
    protected->sp = 0;
}

int protect(Header *obj) {
#ifdef DEBUG_LOG_GC
    fprintf(
        stderr,
        "PROTECT(%s) -> %d (%d)\n",
        (obj == NULL ? "NULL" : typeName(obj->type)),
        protected->sp,
        protected->capacity
    );
#endif
    if (obj == NULL) return protected->sp;

    protected->stack[protected->sp++] = obj;
    if (protected->sp == protected->capacity) {
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "protect old stack: %p\n", (void *)protected);
#endif
        ProtectionStack *tmp = NEW_PROTECT(protected->capacity * 2);
        tmp->capacity = protected->capacity * 2;
        for (tmp->sp = 0; tmp->sp < protected->sp; tmp->sp++) {
            tmp->stack[tmp->sp] = protected->stack[tmp->sp];
        }
        protected = tmp;
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "protect new stack: %p\n", (void *)protected);
#endif
    }

#ifdef DEBUG_LOG_GC
    fprintf(stderr, "PROTECT(%s) done -> %d (%d)\n", typeName(obj->type), protected->sp, protected->capacity);
#endif
    return protected->sp - 1;
}

void unProtect(int index) {
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "UNPROTECT(%d)\n", index);
#endif
    protected->sp = index;
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

static void markProtectionObj(Header *h) {
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "markProtectionObj\n");
#endif
    MARK(h);
    ProtectionStack *protected = (ProtectionStack *)h;
    for (int i = 0; i < protected->sp; ++i) {
        markObj(protected->stack[i]);
    }
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "markProtectionObj done\n");
#endif
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
        case OBJTYPE_MATCH:
        case OBJTYPE_MATCHLIST:
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
        case OBJTYPE_PROTECTION:
            markProtectionObj(h);
            break;
        TIN_OBJTYPE_CASES()
            markTinObj(h);
            break;
        AST_OBJTYPE_CASES()
            markAstObj(h);
            break;
        LAMBDA_OBJTYPE_CASES()
            markLambdaObj(h);
            break;
        default:
            cant_happen("unrecognised ObjType %d in markObj", h->type);
    }
}

static void freeProtectionObj(Header *h) {
    FREE_PROTECT(h);
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
        case OBJTYPE_MATCH:
        case OBJTYPE_MATCHLIST:
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
        case OBJTYPE_PROTECTION:
            freeProtectionObj(h);
            break;
        TIN_OBJTYPE_CASES()
            freeTinObj(h);
            break;
        AST_OBJTYPE_CASES()
            freeAstObj(h);
            break;
        LAMBDA_OBJTYPE_CASES()
            freeLambdaObj(h);
        default:
            cant_happen("unrecognised ObjType %d in freeObj", h->type);
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
    fprintf(stderr, "starting markVarTable\n");
#endif
    markVarTable();
#ifdef DEBUG_LOG_GC
    fprintf(stderr, "markVarTable done\n");
#endif
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
            fprintf(stderr, "sweep discard %p\n", (void *)current);
            fprintf(stderr, "              type %s\n", typeName(current->type));
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
