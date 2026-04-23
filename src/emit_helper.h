#ifndef cekf_emit_helper_h
#define cekf_emit_helper_h
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

#include "emit.h"
#include "minlam.h"
#include "symbol.h"
#include <sys/param.h>

void emitter_addToHeap(EmitterContext *, HashSymbol *);
HashSymbol *emitter_removeFromHeap(EmitterContext *); // can return NULL
SymbolArray *emitter_createHeap();
Integer emitter_peekHeap(EmitterContext *);

#ifdef SAFETY_CHECKS
void emitter_assertNoLeakedSlots(EmitterContext *, int);
#define ASSERT_SLOTS(ctx) emitter_assertNoLeakedSlots(ctx, __LINE__)
#else
#define ASSERT_SLOTS(ctx)
#endif

static inline void setProtectionStatus(EmitterContext *to,
                                       EmitterContext *from) {
    to->needsUnprotect = to->needsUnprotect || from->needsUnprotect;
}

static inline void retrieveMaxReg(EmitterContext *to, EmitterContext *from) {
    to->maxReg = MAX(to->maxReg, from->maxReg);
}

Integer slotIndex(HashSymbol *key, EmitterContext *ctx);
BuiltIn *emitter_findBuiltIn(MinApply *node, EmitterContext *ctx);
HashSymbol *emitter_claimSlotSymbol(EmitterContext *ctx);
void emitter_releaseSlotSymbol(HashSymbol *temp, EmitterContext *ctx);
Slot *emitter_getSlot(HashSymbol *temp, EmitterContext *ctx);
bool emitter_slotsAvailableBelow(int N, EmitterContext *ctx);
SlotAssignArray *resolveCopy(SymbolMap *M, HashSymbol *tmp, HashSymbol **J);
HashSymbol *symbolForSlot(Index i, EmitterContext *ctx);

#endif