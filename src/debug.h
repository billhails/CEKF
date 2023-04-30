#ifndef cekf_debug_h
#define cekf_debug_h
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

#include "cekf.h"
#include "exp.h"

void printValue(Value x);
void printValueList(ValueList *x);
void printClo(Clo *x);
void printCEKF(CEKF *x);
void printEnv(Env *x);
void printElidedEnv(Env *x);
void printKont(Kont *x);
void printFail(Fail *x);
void printAexpLam(AexpLam *x);
void printAexpVarList(AexpVarList *x);
void printAexpVar(AexpVar *x);
void printAexpPrimApp(AexpPrimApp *x);
void printAexpList(AexpList *x);
void printBareAexpList(AexpList *x);
void printCexpApply(CexpApply *x);
void printCexpCond(CexpCond *x);
void printCexpLetRec(CexpLetRec *x);
void printLetRecBindings(LetRecBindings *x);
void printCexpAmb(CexpAmb *x);
void printExp(Exp *x);
void printExpLet(ExpLet *x);

#endif
