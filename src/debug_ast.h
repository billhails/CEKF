#ifndef cekf_debug_ast_h
#define cekf_debug_ast_h
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

 #include "ast.h"

void printAstArgList(AstArgList *x);
void printAstArg(AstArg *x);
void printAstArgPair(AstArgPair *x);
void printAstBinOp(AstBinOp *x);
void printAstCompositeFunction(AstCompositeFunction *x);
void printAstConditional(AstConditional *x);
void printAstDefine(AstDefine *x);
void printAstDefinition(AstDefinition *x);
void printAstDefinitions(AstDefinitions *x);
void printAstEnv(AstEnv *x);
void printAstEnvType(AstEnvType *x);
void printAstExpression(AstExpression *x);
void printAstExpressions(AstExpressions *x);
void printAstFlatType(AstFlatType *x);
void printAstFunCall(AstFunCall *x);
void printAstFunction(AstFunction *x);
void printAstFun(AstFun *x);
void printAstLoad(AstLoad *x);
void printAstNamedArg(AstNamedArg *x);
void printAstNest(AstNest *x);
void printAstPackage(AstPackage *x);
void printAstPrototypeBody(AstPrototypeBody *x);
void printAstPrototype(AstPrototype *x);
void printAstPrototypeSymbolType(AstPrototypeSymbolType *x);
void printAstSinglePrototype(AstSinglePrototype *x);
void printAstString(AstString *x);
void printAstSwitch(AstSwitch *x);
void printAstSymbol(AstSymbol *x);
void printAstTypeBody(AstTypeBody *x);
void printAstTypeClause(AstTypeClause *x);
void printAstTypeConstructor(AstTypeConstructor *x);
void printAstTypeDef(AstTypeDef *x);
void printAstTypeList(AstTypeList *x);
void printAstType(AstType *x);
void printAstTypeSymbols(AstTypeSymbols *x);
void printAstUnpack(AstUnpack *x);

 #endif
