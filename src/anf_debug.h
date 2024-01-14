#ifndef cekf_anf_debug_h
#define cekf_anf_debug_h
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
 * ANF structures to be converted to bytecode.
 *
 * Generated from src/anf.yaml by tools/makeAST.py
 */

#include "anf_helper.h"
#include "bigint.h"
#include "ast_helper.h"
#include "tc.h"
#include "tc_debug.h"

/*
 * print declarations
 */

void printAexpLam(struct AexpLam * x, int depth);
void printAexpVarList(struct AexpVarList * x, int depth);
void printAexpAnnotatedVar(struct AexpAnnotatedVar * x, int depth);
void printAexpPrimApp(struct AexpPrimApp * x, int depth);
void printAexpUnaryApp(struct AexpUnaryApp * x, int depth);
void printAexpList(struct AexpList * x, int depth);
void printAexpIntList(struct AexpIntList * x, int depth);
void printCexpApply(struct CexpApply * x, int depth);
void printAexpMakeVec(struct AexpMakeVec * x, int depth);
void printCexpIf(struct CexpIf * x, int depth);
void printCexpCond(struct CexpCond * x, int depth);
void printCexpIntCondCases(struct CexpIntCondCases * x, int depth);
void printCexpCharCondCases(struct CexpCharCondCases * x, int depth);
void printCexpMatch(struct CexpMatch * x, int depth);
void printMatchList(struct MatchList * x, int depth);
void printCexpLetRec(struct CexpLetRec * x, int depth);
void printLetRecBindings(struct LetRecBindings * x, int depth);
void printCexpAmb(struct CexpAmb * x, int depth);
void printCexpCut(struct CexpCut * x, int depth);
void printCexpBool(struct CexpBool * x, int depth);
void printExpLet(struct ExpLet * x, int depth);
void printTypedAexp(struct TypedAexp * x, int depth);
void printCexpCondCases(struct CexpCondCases * x, int depth);
void printAexp(struct Aexp * x, int depth);
void printCexp(struct Cexp * x, int depth);
void printExp(struct Exp * x, int depth);

/*
 * compare declarations
 */

bool eqAexpLam(struct AexpLam * a, struct AexpLam * b);
bool eqAexpVarList(struct AexpVarList * a, struct AexpVarList * b);
bool eqAexpAnnotatedVar(struct AexpAnnotatedVar * a, struct AexpAnnotatedVar * b);
bool eqAexpPrimApp(struct AexpPrimApp * a, struct AexpPrimApp * b);
bool eqAexpUnaryApp(struct AexpUnaryApp * a, struct AexpUnaryApp * b);
bool eqAexpList(struct AexpList * a, struct AexpList * b);
bool eqAexpIntList(struct AexpIntList * a, struct AexpIntList * b);
bool eqCexpApply(struct CexpApply * a, struct CexpApply * b);
bool eqAexpMakeVec(struct AexpMakeVec * a, struct AexpMakeVec * b);
bool eqCexpIf(struct CexpIf * a, struct CexpIf * b);
bool eqCexpCond(struct CexpCond * a, struct CexpCond * b);
bool eqCexpIntCondCases(struct CexpIntCondCases * a, struct CexpIntCondCases * b);
bool eqCexpCharCondCases(struct CexpCharCondCases * a, struct CexpCharCondCases * b);
bool eqCexpMatch(struct CexpMatch * a, struct CexpMatch * b);
bool eqMatchList(struct MatchList * a, struct MatchList * b);
bool eqCexpLetRec(struct CexpLetRec * a, struct CexpLetRec * b);
bool eqLetRecBindings(struct LetRecBindings * a, struct LetRecBindings * b);
bool eqCexpAmb(struct CexpAmb * a, struct CexpAmb * b);
bool eqCexpCut(struct CexpCut * a, struct CexpCut * b);
bool eqCexpBool(struct CexpBool * a, struct CexpBool * b);
bool eqExpLet(struct ExpLet * a, struct ExpLet * b);
bool eqTypedAexp(struct TypedAexp * a, struct TypedAexp * b);
bool eqCexpCondCases(struct CexpCondCases * a, struct CexpCondCases * b);
bool eqAexp(struct Aexp * a, struct Aexp * b);
bool eqCexp(struct Cexp * a, struct Cexp * b);
bool eqExp(struct Exp * a, struct Exp * b);

#endif
