/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

// shared slot heap and assignment support for emit_b and emit_c branches

#include "emit_helper.h"
#include <math.h>

static inline Index parent(Index i) { return i / 2; } // rounds towards zero
static inline Index left_child(Index i) { return i * 2; }
static inline Index right_child(Index i) { return i * 2 + 1; }
static inline void swap(Index a, Index b, EmitterContext *ctx) {
    HashSymbol *temp = ctx->heap->entries[a];
    ctx->heap->entries[a] = ctx->heap->entries[b];
    ctx->heap->entries[b] = temp;
}

Integer slotIndex(HashSymbol *key, EmitterContext *ctx) {
    Slot *slot = NULL;

    if (getSlotPool(ctx->slots, key, &slot)) {
        return slot->index;
    } else {
        cant_happen("unrecognised %s", key->name);
    }
}

// returns the register number of the slot
static Integer reg(Index i, EmitterContext *ctx) {
#ifdef SAFETY_CHECKS
    if (i < 1 || i >= ctx->heap->size)
        cant_happen("heap index %u out of range", i);
#endif

    HashSymbol *key = ctx->heap->entries[i];
    return slotIndex(key, ctx);
}

Integer emitter_peekHeap(EmitterContext *ctx) {
    SymbolArray *heap = ctx->heap;

    if (heap->size < 2) {
        return 1 << (sizeof(Integer) * 8 - 2);
    }

    return reg(1, ctx);
}

// hide implementation details
SymbolArray *emitter_createHeap() {
    SymbolArray *result = newSymbolArray();
    int save = PROTECT(result);
    pushSymbolArray(result, newSymbol("empty")); // we ignore index 0
    UNPROTECT(save);
    return result;
}

void emitter_addToHeap(EmitterContext *ctx, HashSymbol *key) {
    SymbolArray *heap = ctx->heap;

#ifdef SAFETY_CHECKS
    if (heap->size == 0)
        cant_happen("malformed heap");
#endif

    pushSymbolArray(heap, key);

    if (heap->size > 2) {
        Index i = heap->size - 1;

        // keep swapping while it's less than its parent
        while (reg(i, ctx) < reg(parent(i), ctx)) {
            swap(i, parent(i), ctx);

            if (parent(i) > 1) {
                i = parent(i);
            } else {
                break;
            }
        }
    }
}

// can return NULL
HashSymbol *emitter_removeFromHeap(EmitterContext *ctx) {
    SymbolArray *heap = ctx->heap;

#ifdef SAFETY_CHECKS
    if (heap->size == 0)
        cant_happen("malformed heap");
#endif

    if (heap->size == 1) // empty
        return NULL;

    if (heap->size == 2)             // only one left
        return popSymbolArray(heap); // return it

    HashSymbol *smallest = heap->entries[1];
    heap->entries[1] = popSymbolArray(heap);

    if (heap->size == 2) // only one left after removel
        return smallest;

    if (heap->size == 3) { // only two left after removal
        if (reg(2, ctx) < reg(1, ctx))
            swap(1, 2, ctx);

        return smallest;
    }

    Index p = 1;
    Index lc = left_child(p);
    Index rc = right_child(p);

    // there are at least 3 nodes in the heap
    while (reg(p, ctx) >= reg(lc, ctx) || reg(p, ctx) >= reg(rc, ctx)) {
        if (reg(lc, ctx) < reg(rc, ctx)) {
            swap(p, lc, ctx);
            p = lc;
        } else {
            swap(p, rc, ctx);
            p = rc;
        }

        lc = left_child(p);
        rc = right_child(p);

        if (lc >= heap->size || rc >= heap->size)
            break;
    }

    return smallest;
}

#ifdef SAFETY_CHECKS
void emitter_assertNoLeakedSlots(EmitterContext *ctx, int line) {
    if (ctx->activeSlots != ctx->currentReg) {
        cant_happen("slot leak: active %d, expected %d line %d",
                    ctx->activeSlots, ctx->currentReg, line);
    }
}
#endif

static inline void setMaxReg(EmitterContext *ctx) {
    ctx->maxReg = MAX(ctx->maxReg, ctx->totalSlots);
}

BuiltIn *emitter_findBuiltIn(MinApply *node, EmitterContext *ctx) {
    BuiltIn *bi = NULL;
    for (Index i = 0; i < ctx->builtIns->size; i++) {
        if (ctx->builtIns->entries[i]->internalName ==
            getMinExp_Var(node->function)) {
            bi = ctx->builtIns->entries[i];
            break;
        }
    }
    if (bi == NULL) {
        cant_happen("could not find builtin %s",
                    getMinExp_Var(node->function)->name);
    }
    return bi;
}

static Slot *createNewSlot(EmitterContext *ctx) {
    SCharArray *text = newSCharArray();
    int save = PROTECT(text);
    char buf[64];
    sprintf(buf, "reg[%d]", ctx->totalSlots);
    for (char *c = buf; *c; c++) {
        pushSCharArray(text, *c);
    }
    pushSCharArray(text, '\0');
    Slot *result = newSlot(false, text, ctx->totalSlots);
    UNPROTECT(save);
    return result;
}

HashSymbol *emitter_claimSlotSymbol(EmitterContext *ctx) {
    Slot *resultSlot = NULL;
    HashSymbol *result = emitter_removeFromHeap(ctx);
    if (result != NULL) {
        if (getSlotPool(ctx->slots, result, &resultSlot)) {
            resultSlot->isAvailable = false;
            ctx->activeSlots++;
            return result;
        } else {
            cant_happen("key \"%s\" from heap not in pool", result->name);
        }
    }
    // no available slots, create a new one
    result = genSym("slot");
    resultSlot = createNewSlot(ctx);
    int save = PROTECT(resultSlot);
    setSlotPool(ctx->slots, result, resultSlot);
    pushSymbolArray(ctx->slotSymbols, result);
    ctx->activeSlots++;
    ctx->totalSlots++;
#ifdef SAFETY_CHECKS
    if (ctx->totalSlots != countSymbolArray(ctx->slotSymbols)) {
        cant_happen("slot index misalignment");
    }
#endif
    setMaxReg(ctx);
    UNPROTECT(save);
    return result;
}

void emitter_releaseSlotSymbol(HashSymbol *temp, EmitterContext *ctx) {
    Slot *slot = NULL;
    if (getSlotPool(ctx->slots, temp, &slot)) {
        if (!slot->isAvailable) {
            ctx->activeSlots--;
            slot->isAvailable = true;
            emitter_addToHeap(ctx, temp);
        }
    } else {
        cant_happen("slot not found");
    }
}

HashSymbol *symbolForSlot(Index i, EmitterContext *ctx) {
    return getSymbolArray(ctx->slotSymbols, i);
}

Slot *emitter_getSlot(HashSymbol *temp, EmitterContext *ctx) {
    Slot *slot = NULL;
    if (getSlotPool(ctx->slots, temp, &slot)) {
        return slot;
    } else {
        cant_happen("slot not found");
    }
}

bool emitter_slotsAvailableBelow(int N, EmitterContext *ctx) {
    if (N == 0)
        return false;
    if (ctx->totalSlots < (Index)N) // we've never allocated them
        return true;
    if (emitter_peekHeap(ctx) < N)
        return true;
    return false;
}

static SymbolMap *cleanIdentities(SymbolMap *M) {
    HashSymbol *source = NULL;
    HashSymbol *dest = NULL;
    Index i = 0;
    SymbolMap *cleaned = newSymbolMap();
    int save = PROTECT(cleaned);
    while ((dest = iterateSymbolMap(M, &i, &source)) != NULL) {
        if (source != dest) {
            setSymbolMap(cleaned, dest, source);
        }
    }
    UNPROTECT(save);
    return cleaned;
}

static SymbolSet *getTargets(SymbolMap *M) {
    SymbolSet *T = newSymbolSet();
    int save = PROTECT(T);
    HashSymbol *target = NULL;
    Index i = 0;
    while ((target = iterateSymbolMap(M, &i, NULL)) != NULL) {
        setSymbolSet(T, target);
    }
    UNPROTECT(save);
    return T;
}

static IntMap *initUses(SymbolMap *M) {
    IntMap *uses = newIntMap();
    int save = PROTECT(uses);
    Index i = 0;
    Integer count = 0;
    HashSymbol *source = NULL;
    HashSymbol *dest = NULL;
    while ((dest = iterateSymbolMap(M, &i, &source)) != NULL) {
        if (getIntMap(uses, source, &count)) {
            count += 1;
        } else {
            count = 1;
        }
        setIntMap(uses, source, count);
        if (!getIntMap(uses, dest, NULL)) {
            setIntMap(uses, dest, 0);
        }
    }
    UNPROTECT(save);
    return uses;
}

static void emitMove(SlotAssignArray *assignments, HashSymbol *source,
                     HashSymbol *target) {
    SlotAssign *assign = newSlotAssign(source, target);
    int save = PROTECT(assign);
    pushSlotAssignArray(assignments, assign);
    UNPROTECT(save);
}

static SymbolMap *deleteConnection(SymbolMap *M, HashSymbol *target) {
    SymbolMap *replacement = newSymbolMap();
    int save = PROTECT(replacement);
    HashSymbol *t = NULL;
    HashSymbol *s = NULL;
    Index i = 0;
    while ((t = iterateSymbolMap(M, &i, &s)) != NULL) {
        if (t != target) {
            setSymbolMap(replacement, t, s);
        }
    }
    UNPROTECT(save);
    return replacement;
}

static SymbolSet *deleteTarget(SymbolSet *T, HashSymbol *target) {
    SymbolSet *replacement = newSymbolSet();
    int save = PROTECT(replacement);
    HashSymbol *t = NULL;
    Index i = 0;
    while ((t = iterateSymbolSet(T, &i)) != NULL) {
        if (t != target) {
            setSymbolSet(replacement, t);
        }
    }
    UNPROTECT(save);
    return replacement;
}

static void decrementUses(IntMap *uses, HashSymbol *source) {
    Integer use = 0;
    if (getIntMap(uses, source, &use)) {
#ifdef SAFETY_CHECKS
        if (use == 0)
            cant_happen("decrement use below zero");
#endif
        setIntMap(uses, source, use - 1);
    } else {
        cant_happen("cannot find use count");
    }
}

static void incrementUses(IntMap *uses, HashSymbol *source) {
    Integer use = 0;
    if (getIntMap(uses, source, &use)) {
        setIntMap(uses, source, use + 1);
    } else {
        setIntMap(uses, source, 1);
    }
}

static SymbolMap *replaceAllTmp(SymbolMap *M, HashSymbol *val,
                                HashSymbol *replacement) {
    SymbolMap *N = newSymbolMap();
    int save = PROTECT(N);
    HashSymbol *k = NULL, *v = NULL;
    Index i = 0;
    while ((k = iterateSymbolMap(M, &i, &v)) != NULL) {
        if (v == val) {
            setSymbolMap(N, k, replacement);
        } else {
            setSymbolMap(N, k, v);
        }
    }
    UNPROTECT(save);
    return N;
}

// resolves a map of copy instructions to an ordered,
// minimal sequence of assignments.
SlotAssignArray *resolveCopy(SymbolMap *M, HashSymbol *tmp, HashSymbol **J) {
    M = cleanIdentities(M); // discard x <-- x
    int save_M = PROTECT(M);
    IntMap *uses = initUses(M); // count uses (including 0)
    PROTECT(uses);
    SymbolSet *T = getTargets(M); // keys of M
    int save_T = PROTECT(T);
    SlotAssignArray *assignments = newSlotAssignArray(); // results
    PROTECT(assignments);
    if (J != NULL && *J != NULL) {
        incrementUses(uses, *J);
    }
    setIntMap(uses, tmp, 0);

RESOLVER_RESTART:
    while (countSymbolMap(M) > 0) {
        HashSymbol *target = NULL;
        Index i = 0;
        while ((target = iterateSymbolSet(T, &i)) != NULL) {
            int count = 0;
            if (getIntMap(uses, target, &count) && count == 0) {
                HashSymbol *source = NULL;
                if (getSymbolMap(M, target, &source)) {
                    emitMove(assignments, source, target);
                    M = deleteConnection(M, target);
                    REPLACE_PROTECT(save_M, M);
                    T = deleteTarget(T, target);
                    REPLACE_PROTECT(save_T, T);
                    decrementUses(uses, source);
                    // not safe to continue to iterate over T after changing it
                    goto RESOLVER_RESTART;
                } else {
                    cant_happen("failed to find source");
                }
            }
        }
        i = 0;
        if ((target = iterateSymbolSet(T, &i)) != NULL) {
            Integer count = 0;
            if (!getIntMap(uses, target, &count)) {
                cant_happen("failed to find use count");
            }
            emitMove(assignments, target, tmp);
            if (J != NULL && *J == target) {
                *J = tmp;
            }
            M = replaceAllTmp(M, target, tmp);
            REPLACE_PROTECT(save_M, M);
            setIntMap(uses, tmp, count);
            setIntMap(uses, target, 0);
        } else {
            cant_happen("expected at least one occupied slot");
        }
    }
    UNPROTECT(save_M);
    return assignments;
}