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

void ppMinApply(FILE *, MinApply *);
void ppMinAnnotatedVar(FILE *, MinAnnotatedVar *);
void ppMinBindings(FILE *, MinBindings *);
void ppMinCallCC(FILE *, MinExp *);
void ppMinCond(FILE *, MinCond *);
void ppMinExpD(FILE *, MinExp *, int);
void ppMinExp(FILE *, MinExp *);
void ppMinIff(FILE *, MinIff *);
void ppMinIntList(FILE *, MinIntList *);
void ppMinLam(FILE *, MinLam *);
void ppMinLetRec(FILE *, MinLetRec *);
void ppMinMakeTuple(FILE *, MinExprList *);
void ppMinMakeVec(FILE *, MinExprList *);
void ppMinMatch(FILE *, MinMatch *);
void ppMinPrimApp(FILE *, MinPrimApp *);
void ppMinPrimOp(FILE *, MinPrimOp);
void ppMinSequence(FILE *, MinExprList *);
void ppMinVarList(FILE *, SymbolList *);

#endif
