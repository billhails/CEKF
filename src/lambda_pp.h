#ifndef cekf_lambda_pp_h
#define cekf_lambda_pp_h
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
 * bespoke pretty-printer for lambda.yaml structs
 *
 */

#include "lambda.h"

void ppLamExpD(LamExp *exp, int depth);
void ppLamLam(LamLam *lam);
void ppLamVarList(LamVarList *varList);
void ppLamExp(LamExp *exp);
void ppHashSymbol(HashSymbol *symbol);
void ppLamPrimApp(LamPrimApp *primApp);
void ppLamPrimOp(LamPrimOp type);
void ppLamUnary(LamUnaryApp *unaryApp);
void ppLamUnaryOp(LamUnaryOp type);
void ppLamSequence(LamSequence *sequence);
void ppLamMakeVec(LamMakeVec *makeVec);
void ppLamApply(LamApply *apply);
void ppLamIff(LamIff *iff);
void ppLamCond(LamCond *cond);
void ppLamCallCC(LamExp *exp);
void ppLamLetRec(LamLetRec *letRec);
void ppLamConstruct(LamConstruct *construct);
void ppLamConstant(LamConstant *constant);
void ppLamTypeDefs(LamTypeDefs *typeDefs);
void ppLamLet(LamLet *let);
void ppLamMatch(LamMatch *match);
void ppLamLetRecBindings(LamLetRecBindings *bindings);
void ppLamIntList(LamIntList *list);

#endif
