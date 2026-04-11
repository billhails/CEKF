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

// shared slot heap support for emit_b and emit_c branches

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

// returns the register number of the slot
static Integer reg(Index i, EmitterContext *ctx) {
#ifdef SAFETY_CHECKS
    if (i < 1 || i >= ctx->heap->size)
        cant_happen("heap index %u out of range", i);
#endif

    HashSymbol *key = ctx->heap->entries[i];
    Slot *slot = NULL;

    if (getSlotPool(ctx->slots, key, &slot)) {
        return slot->index;
    } else {
        cant_happen("unrecognised %s at index %u", key->name, i);
    }
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
    result = genSym("tmp_");
    resultSlot = createNewSlot(ctx);
    int save = PROTECT(resultSlot);
    setSlotPool(ctx->slots, result, resultSlot);
    ctx->activeSlots++;
    ctx->totalSlots++;
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
    if (ctx->totalSlots < N) // we've never allocated them
        return true;
    if (emitter_peekHeap(ctx) < N)
        return true;
    return false;
}