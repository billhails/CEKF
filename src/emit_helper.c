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

#include "emit_helper.h"

static inline Index parent(Index i) { return i / 2; } // rounds towards zero
static inline Index left_child(Index i) { return i * 2; }
static inline Index right_child(Index i) { return i * 2 + 1; }
static inline void swap(Index a, Index b, EmitterContext *context) {
    HashSymbol *temp = context->heap->entries[a];
    context->heap->entries[a] = context->heap->entries[b];
    context->heap->entries[b] = temp;
}

static Integer value(Index i, EmitterContext *cx) {
#ifdef SAFETY_CHECKS
    if (i < 1 || i >= cx->heap->size)
        cant_happen("heap index %u out of range", i);
#endif

    HashSymbol *key = cx->heap->entries[i];
    Slot *slot = NULL;

    if (getSlotPool(cx->slots, key, &slot)) {
        return slot->index;
    } else {
        cant_happen("slotIndex given unrecognised slot key %s", key->name);
    }
}

// hide implementation details
SymbolArray *emit_createHeap() {
    SymbolArray *result = newSymbolArray();
    int save = PROTECT(result);
    pushSymbolArray(result, newSymbol("empty")); // we ignore index 0
    UNPROTECT(save);
    return result;
}

void emit_addToHeap(EmitterContext *cx, HashSymbol *key) {
    SymbolArray *heap = cx->heap;

#ifdef SAFETY_CHECKS
    if (heap->size == 0)
        cant_happen("malformed heap");
#endif

    pushSymbolArray(heap, key);

    if (heap->size > 2) {
        Index i = heap->size - 1;

        // keep swapping while it's less than its parent
        while (value(i, cx) < value(parent(i), cx)) {
            swap(i, parent(i), cx);

            if (parent(i) > 1) {
                i = parent(i);
            } else {
                break;
            }
        }
    }
}

// can return NULL
HashSymbol *emit_removeFromHeap(EmitterContext *cx) {
    SymbolArray *heap = cx->heap;

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
        swap(1, 2, cx);    // first must be bigger because it was last
        return smallest;
    }

    Index p = 1;
    Index lc = left_child(p);
    Index rc = right_child(p);

    // there are at least 3 nodes jn the heap
    while (value(p, cx) >= value(lc, cx) || value(p, cx) >= value(rc, cx)) {
        if (value(lc, cx) < value(rc, cx)) {
            swap(p, lc, cx);
            p = lc;
        } else {
            swap(p, rc, cx);
            p = rc;
        }
        lc = left_child(p);
        rc = right_child(p);
        if (lc >= heap->size || rc >= heap->size)
            break;
    }

    return smallest;
}