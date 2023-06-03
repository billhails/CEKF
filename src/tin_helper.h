#ifndef cekf_tin_helper_h
#define cekf_tin_helper_h
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

#include "tin.h"
#include "hash.h"
#include "memory.h"

void markTinSymbolTable();

void printTinSymbol(HashSymbol *x, int depth);
TinContext *freshTinContext();

void addToSubstitution(TinSubstitution *substitution, HashSymbol *symbol, TinMonoType *monotype);
TinContext *extendTinContext(TinContext *parent);
HashSymbol *freshTypeVariable();
void addToContext(TinContext *context, HashSymbol *symbol, TinPolyType *polyType);
TinSubstitution *unify(TinMonoType *t1, TinMonoType *t2);
TinMonoType *applyMonoTypeSubstitution(TinSubstitution *s, TinMonoType *mtype);

TinPolyType *applyPolyTypeSubstitution(TinSubstitution *s, TinPolyType *ptype);
TinPolyType *lookupInContext(TinContext *context, HashSymbol *var);
TinSubstitution *makeEmptySubstitution();
TinContext *applyContextSubstitution(TinSubstitution *s, TinContext *context);
TinMonoType *instantiate(TinPolyType *tpt);
TinSubstitution *applySubstitutionSubstitution(TinSubstitution *s1, TinSubstitution *s2);
TinContext *applyContextSubstitution(TinSubstitution *s, TinContext *context);
TinPolyType *generalize(TinContext *context, TinMonoType *monoType);

#endif
