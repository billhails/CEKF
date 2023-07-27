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

// generated from src/ast.yaml by makeAST.py



#include "ast_helper.h"

void printAstNest(struct AstNest * x, int depth);
void printAstDefinitions(struct AstDefinitions * x, int depth);
void printAstDefine(struct AstDefine * x, int depth);
void printAstPrototype(struct AstPrototype * x, int depth);
void printAstPrototypeBody(struct AstPrototypeBody * x, int depth);
void printAstPrototypeSymbolType(struct AstPrototypeSymbolType * x, int depth);
void printAstLoad(struct AstLoad * x, int depth);
void printAstTypeDef(struct AstTypeDef * x, int depth);
void printAstFlatType(struct AstFlatType * x, int depth);
void printAstTypeSymbols(struct AstTypeSymbols * x, int depth);
void printAstTypeBody(struct AstTypeBody * x, int depth);
void printAstTypeConstructor(struct AstTypeConstructor * x, int depth);
void printAstTypeFunction(struct AstTypeFunction * x, int depth);
void printAstTypeList(struct AstTypeList * x, int depth);
void printAstType(struct AstType * x, int depth);
void printAstCompositeFunction(struct AstCompositeFunction * x, int depth);
void printAstFunction(struct AstFunction * x, int depth);
void printAstArgList(struct AstArgList * x, int depth);
void printAstUnpack(struct AstUnpack * x, int depth);
void printAstNamedArg(struct AstNamedArg * x, int depth);
void printAstEnvType(struct AstEnvType * x, int depth);
void printAstFunCall(struct AstFunCall * x, int depth);
void printAstPackage(struct AstPackage * x, int depth);
void printAstExpressions(struct AstExpressions * x, int depth);
void printAstEnv(struct AstEnv * x, int depth);
void printAstDefinition(struct AstDefinition * x, int depth);
void printAstSinglePrototype(struct AstSinglePrototype * x, int depth);
void printAstTypeClause(struct AstTypeClause * x, int depth);
void printAstArg(struct AstArg * x, int depth);
void printAstExpression(struct AstExpression * x, int depth);

#endif
