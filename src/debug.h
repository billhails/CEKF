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
#include "analysis.h"
#include "bytecode.h"

void printAexp(Aexp *x);
void printAexpAnnotatedVar(AexpAnnotatedVar *x);
void printAexpLam(AexpLam *x);
void printAexpList(AexpList *x);
void printAexpPrimApp(AexpPrimApp *x);
void printAexpVar(AexpVar *x);
void printAexpVarList(AexpVarList *x);
void printBareAexpList(AexpList *x);
void printCEKF(CEKF *x);
void printCexpAmb(CexpAmb *x);
void printCexpApply(CexpApply *x);
void printCexp(Cexp *x);
void printCexpCond(CexpCond *x);
void printCexpLetRec(CexpLetRec *x);
void printClo(Clo *x);
void printCTEnv(CTEnv *x);
void printElidedEnv(Env *x);
void printEnv(Env *x);
void printExp(Exp *x);
void printExpLet(ExpLet *x);
void printFail(Fail *x);
void printKont(Kont *x);
void printCons(Cons *x);
void printLetRecBindings(LetRecBindings *x);
void printStack(Stack *x);
void printValueList(ValueList *x);
void printValue(Value x);

void dumpByteCode(ByteCodeArray *b);

#endif
