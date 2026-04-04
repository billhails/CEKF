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
#include <stdio.h>

void ppLamExpD(FILE *out, LamExp *exp, int depth);
void ppLamLam(FILE *out, LamLam *lam);
void ppLamVarList(FILE *out, SymbolList *varList);
void ppLamExp(FILE *out, LamExp *exp);
void ppHashSymbol(FILE *out, HashSymbol *symbol);
void ppLamPrimApp(FILE *out, LamPrimApp *primApp);
void ppLamPrimOp(FILE *out, LamPrimOp type);
void ppLamSequence(FILE *out, LamSequence *sequence);
void ppLamMakeVec(FILE *out, LamMakeVec *makeVec);
void ppLamApply(FILE *out, LamApply *apply);
void ppLamIff(FILE *out, LamIff *iff);
void ppLamCond(FILE *out, LamCond *cond);
void ppLamCallCC(FILE *out, LamExp *exp);
void ppLamPrint(FILE *out, LamPrint *print);
void ppLamLetRec(FILE *out, LamLetRec *letRec);
void ppLamLetStar(FILE *out, LamLetStar *letStar);
void ppLamDeconstruct(FILE *out, LamDeconstruct *deconstruct);
void ppLamConstruct(FILE *out, LamConstruct *construct);
void ppLamConstant(FILE *out, LamConstant *constant);
void ppLamTypeDefs(FILE *out, LamTypeDefs *typeDefs);
void ppLamLet(FILE *out, LamLet *let);
void ppLamMatch(FILE *out, LamMatch *match);
void ppLamTupleIndex(FILE *out, LamTupleIndex *index);
void ppLamBindings(FILE *out, LamBindings *bindings);
void ppLamIntList(FILE *out, LamIntList *list);
void ppLamMakeTuple(FILE *out, LamArgs *args);
void ppLamContext(FILE *out, LamContext *env);

#endif
