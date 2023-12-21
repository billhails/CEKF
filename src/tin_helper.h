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

void markTinSymbolTable(void);

void showTinMonoType(TinMonoType *monoType);
void showTinPolyType(TinPolyType *polyType);

void printTinSymbol(HashSymbol *x, int depth);
TinContext *freshTinContext(void);

void addToSubstitution(TinSubstitution *substitution, HashSymbol *symbol, TinMonoType *monotype);
TinContext *extendTinContext(TinContext *parent);
HashSymbol *freshTypeVariable(const char *suffix);
void addVarToContext(TinContext *context, HashSymbol *symbol, TinPolyType *polyType);
void addConstructorToContext(TinContext *context, HashSymbol *symbol, TinPolyType *polyType);
TinSubstitution *unify(TinMonoType *t1, TinMonoType *t2, const char *caller);
TinMonoType *applyMonoTypeSubstitution(TinSubstitution *s, TinMonoType *mtype);

TinPolyType *applyPolyTypeSubstitution(TinSubstitution *s, TinPolyType *ptype);
TinPolyType *lookupInContext(TinContext *context, HashSymbol *var);
bool isTypeConstructor(TinContext *context, HashSymbol *var);
TinSubstitution *makeEmptySubstitution(void);
TinContext *applyContextSubstitution(TinSubstitution *s, TinContext *context);
void applyContextSubstitutionInPlace(TinSubstitution *s, TinContext *context);
TinMonoType *instantiate(TinPolyType *tpt);
TinSubstitution *applySubstitutionSubstitution(TinSubstitution *s1, TinSubstitution *s2);
TinPolyType *generalize(TinContext *context, TinMonoType *monoType);

#endif
