#ifndef cekf_debug_lambda_h
#define cekf_debug_lambda_h
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
 * Plain lambda structures generated by lambda conversion.
 *
 * generated from src/lambda.yaml by makeAST.py
 */

#include "lambda_helper.h"
#include "bigint.h"

void printLamLam(struct LamLam * x, int depth);
void printLamVarList(struct LamVarList * x, int depth);
void printLamPrimApp(struct LamPrimApp * x, int depth);
void printLamUnaryApp(struct LamUnaryApp * x, int depth);
void printLamSequence(struct LamSequence * x, int depth);
void printLamList(struct LamList * x, int depth);
void printLamApply(struct LamApply * x, int depth);
void printLamConstant(struct LamConstant * x, int depth);
void printLamConstruct(struct LamConstruct * x, int depth);
void printLamMakeVec(struct LamMakeVec * x, int depth);
void printLamIff(struct LamIff * x, int depth);
void printLamCond(struct LamCond * x, int depth);
void printLamIntCondCases(struct LamIntCondCases * x, int depth);
void printLamCharCondCases(struct LamCharCondCases * x, int depth);
void printLamMatch(struct LamMatch * x, int depth);
void printLamMatchList(struct LamMatchList * x, int depth);
void printLamIntList(struct LamIntList * x, int depth);
void printLamLet(struct LamLet * x, int depth);
void printLamLetRec(struct LamLetRec * x, int depth);
void printLamLetRecBindings(struct LamLetRecBindings * x, int depth);
void printLamContext(struct LamContext * x, int depth);
void printLamAnd(struct LamAnd * x, int depth);
void printLamOr(struct LamOr * x, int depth);
void printLamAmb(struct LamAmb * x, int depth);
void printLamTypeDefs(struct LamTypeDefs * x, int depth);
void printLamTypeDefList(struct LamTypeDefList * x, int depth);
void printLamTypeDef(struct LamTypeDef * x, int depth);
void printLamTypeConstructorList(struct LamTypeConstructorList * x, int depth);
void printLamType(struct LamType * x, int depth);
void printLamTypeArgs(struct LamTypeArgs * x, int depth);
void printLamTypeConstructor(struct LamTypeConstructor * x, int depth);
void printLamTypeConstructorArgs(struct LamTypeConstructorArgs * x, int depth);
void printLamTypeFunction(struct LamTypeFunction * x, int depth);
void printLamTypeConstructorInfo(struct LamTypeConstructorInfo * x, int depth);
void printLamExp(struct LamExp * x, int depth);
void printLamCondCases(struct LamCondCases * x, int depth);
void printLamTypeConstructorType(struct LamTypeConstructorType * x, int depth);

#endif
