#ifndef cekf_minlam_pp_h
#define cekf_minlam_pp_h
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
 *
 * bespoke pretty-printer for minlam.yaml structs
 *
 */

#include "minlam.h"

void ppHashSymbol(HashSymbol *);
void ppMinApply(MinApply *);
void ppMinAnnotatedVar(MinAnnotatedVar *);
void ppMinBindings(MinBindings *);
void ppMinCallCC(MinExp *);
void ppMinCond(MinCond *);
void ppMinExpD(MinExp *, int);
void ppMinExp(MinExp *);
void ppMinIff(MinIff *);
void ppMinIntList(MinIntList *);
void ppMinLam(MinLam *);
void ppMinLetRec(MinLetRec *);
void ppMinMakeTuple(MinExprList *);
void ppMinMakeVec(MinExprList *);
void ppMinMatch(MinMatch *);
void ppMinPrimApp(MinPrimApp *);
void ppMinPrimOp(MinPrimOp);
void ppMinSequence(MinExprList *);
void ppMinVarList(SymbolList *);

#endif
