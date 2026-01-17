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

void ppMinExpD(MinExp *exp, int depth);
void ppMinLam(MinLam *lam);
void ppMinVarList(MinVarList *varList);
void ppMinExp(MinExp *exp);
void ppMinPrimApp(MinPrimApp *primApp);
void ppMinPrimOp(MinPrimOp type);
void ppMinSequence(MinExprList *sequence);
void ppMinMakeVec(MinExprList *args);
void ppMinApply(MinApply *apply);
void ppMinIff(MinIff *iff);
void ppMinCond(MinCond *cond);
void ppMinCallCC(MinExp *exp);
void ppMinLetRec(MinLetRec *letRec);
void ppMinMatch(MinMatch *match);
void ppMinBindings(MinBindings *bindings);
void ppMinIntList(MinIntList *list);
void ppMinMakeTuple(MinExprList *args);
void ppMinNameSpaces(MinNameSpaceArray *arr);
void ppMinLookUp(MinLookUp *lookUp);
void ppHashSymbol(HashSymbol *);

#endif
