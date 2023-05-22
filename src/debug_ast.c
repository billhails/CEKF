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

// generated by makeAST.py



#include <stdio.h>

#include "debug_ast.h"

static void pad(int depth) { printf("%*s", depth * 4, ""); }

void printAstNest(struct AstNest * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstNest (NULL)"); return; }
    printf("AstNest[\n");
    printAstDefinitions(x->definitions, depth + 1);
    printf("\n");
    printAstExpressions(x->expressions, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstDefinitions(struct AstDefinitions * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstDefinitions (NULL)"); return; }
    printf("AstDefinitions[\n");
    printAstDefinitions(x->next, depth + 1);
    printf("\n");
    printAstDefinition(x->definition, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstDefine(struct AstDefine * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstDefine (NULL)"); return; }
    printf("AstDefine[\n");
    pad(depth + 1);
    printAstSymbol(x->symbol, depth);
    printf("\n");
    printAstExpression(x->expression, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstPrototype(struct AstPrototype * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstPrototype (NULL)"); return; }
    printf("AstPrototype[\n");
    pad(depth + 1);
    printAstSymbol(x->symbol, depth);
    printf("\n");
    printAstPrototypeBody(x->body, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstPrototypeBody(struct AstPrototypeBody * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstPrototypeBody (NULL)"); return; }
    printf("AstPrototypeBody[\n");
    printAstPrototypeBody(x->next, depth + 1);
    printf("\n");
    printAstSinglePrototype(x->single, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstPrototypeSymbolType(struct AstPrototypeSymbolType * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstPrototypeSymbolType (NULL)"); return; }
    printf("AstPrototypeSymbolType[\n");
    pad(depth + 1);
    printAstSymbol(x->symbol, depth);
    printf("\n");
    printAstType(x->type, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstLoad(struct AstLoad * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstLoad (NULL)"); return; }
    printf("AstLoad[\n");
    printAstPackage(x->package, depth + 1);
    printf("\n");
    pad(depth + 1);
    printAstSymbol(x->symbol, depth);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstTypeDef(struct AstTypeDef * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstTypeDef (NULL)"); return; }
    printf("AstTypeDef[\n");
    printAstFlatType(x->flatType, depth + 1);
    printf("\n");
    printAstTypeBody(x->typeBody, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstFlatType(struct AstFlatType * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstFlatType (NULL)"); return; }
    printf("AstFlatType[\n");
    pad(depth + 1);
    printAstSymbol(x->symbol, depth);
    printf("\n");
    printAstTypeSymbols(x->typeSymbols, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstTypeSymbols(struct AstTypeSymbols * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstTypeSymbols (NULL)"); return; }
    printf("AstTypeSymbols[\n");
    printAstTypeSymbols(x->next, depth + 1);
    printf("\n");
    pad(depth + 1);
    printAstSymbol(x->typeSymbol, depth);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstTypeBody(struct AstTypeBody * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstTypeBody (NULL)"); return; }
    printf("AstTypeBody[\n");
    printAstTypeBody(x->next, depth + 1);
    printf("\n");
    printAstTypeConstructor(x->typeConstructor, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstTypeConstructor(struct AstTypeConstructor * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstTypeConstructor (NULL)"); return; }
    printf("AstTypeConstructor[\n");
    pad(depth + 1);
    printAstSymbol(x->symbol, depth);
    printf("\n");
    printAstTypeList(x->typeList, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstTypeList(struct AstTypeList * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstTypeList (NULL)"); return; }
    printf("AstTypeList[\n");
    printAstTypeList(x->next, depth + 1);
    printf("\n");
    printAstType(x->type, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstType(struct AstType * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstType (NULL)"); return; }
    printf("AstType[\n");
    printAstType(x->next, depth + 1);
    printf("\n");
    printAstTypeClause(x->typeClause, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstConditional(struct AstConditional * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstConditional (NULL)"); return; }
    printf("AstConditional[\n");
    printAstExpression(x->expression, depth + 1);
    printf("\n");
    printAstNest(x->consequent, depth + 1);
    printf("\n");
    printAstNest(x->alternative, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstSwitch(struct AstSwitch * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstSwitch (NULL)"); return; }
    printf("AstSwitch[\n");
    printAstExpressions(x->expressions, depth + 1);
    printf("\n");
    printAstCompositeFunction(x->compositeFunction, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstCompositeFunction(struct AstCompositeFunction * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstCompositeFunction (NULL)"); return; }
    printf("AstCompositeFunction[\n");
    printAstCompositeFunction(x->next, depth + 1);
    printf("\n");
    printAstFunction(x->function, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstFunction(struct AstFunction * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstFunction (NULL)"); return; }
    printf("AstFunction[\n");
    printAstArgList(x->argList, depth + 1);
    printf("\n");
    printAstNest(x->nest, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstArgList(struct AstArgList * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstArgList (NULL)"); return; }
    printf("AstArgList[\n");
    printAstArgList(x->next, depth + 1);
    printf("\n");
    printAstArg(x->arg, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstUnpack(struct AstUnpack * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstUnpack (NULL)"); return; }
    printf("AstUnpack[\n");
    pad(depth + 1);
    printAstSymbol(x->symbol, depth);
    printf("\n");
    printAstArgList(x->argList, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstArgPair(struct AstArgPair * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstArgPair (NULL)"); return; }
    printf("AstArgPair[\n");
    printAstArg(x->car, depth + 1);
    printf("\n");
    printAstArg(x->cdr, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstNamedArg(struct AstNamedArg * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstNamedArg (NULL)"); return; }
    printf("AstNamedArg[\n");
    pad(depth + 1);
    printAstSymbol(x->name, depth);
    printf("\n");
    printAstArg(x->arg, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstEnvType(struct AstEnvType * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstEnvType (NULL)"); return; }
    printf("AstEnvType[\n");
    pad(depth + 1);
    printAstSymbol(x->name, depth);
    printf("\n");
    pad(depth + 1);
    printAstSymbol(x->prototype, depth);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstBinOp(struct AstBinOp * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstBinOp (NULL)"); return; }
    printf("AstBinOp[\n");
    switch (x->type) {
        case AST_BINOPTYPE_TYPE_THEN:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_THEN");
            break;
        case AST_BINOPTYPE_TYPE_AND:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_AND");
            break;
        case AST_BINOPTYPE_TYPE_OR:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_OR");
            break;
        case AST_BINOPTYPE_TYPE_XOR:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_XOR");
            break;
        case AST_BINOPTYPE_TYPE_EQ:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_EQ");
            break;
        case AST_BINOPTYPE_TYPE_NE:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_NE");
            break;
        case AST_BINOPTYPE_TYPE_GT:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_GT");
            break;
        case AST_BINOPTYPE_TYPE_LT:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_LT");
            break;
        case AST_BINOPTYPE_TYPE_GE:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_GE");
            break;
        case AST_BINOPTYPE_TYPE_LE:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_LE");
            break;
        case AST_BINOPTYPE_TYPE_CONS:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_CONS");
            break;
        case AST_BINOPTYPE_TYPE_APPEND:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_APPEND");
            break;
        case AST_BINOPTYPE_TYPE_ADD:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_ADD");
            break;
        case AST_BINOPTYPE_TYPE_SUB:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_SUB");
            break;
        case AST_BINOPTYPE_TYPE_MUL:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_MUL");
            break;
        case AST_BINOPTYPE_TYPE_DIV:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_DIV");
            break;
        case AST_BINOPTYPE_TYPE_MOD:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_MOD");
            break;
        case AST_BINOPTYPE_TYPE_POW:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_POW");
            break;
        case AST_BINOPTYPE_TYPE_DOT:
            pad(depth + 1);
            printf("AST_BINOPTYPE_TYPE_DOT");
            break;
    }
    printf("\n");
    printAstExpression(x->lhs, depth + 1);
    printf("\n");
    printAstExpression(x->rhs, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstFunCall(struct AstFunCall * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstFunCall (NULL)"); return; }
    printf("AstFunCall[\n");
    printAstExpression(x->function, depth + 1);
    printf("\n");
    printAstExpressions(x->arguments, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstPackage(struct AstPackage * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstPackage (NULL)"); return; }
    printf("AstPackage[\n");
    printAstPackage(x->next, depth + 1);
    printf("\n");
    pad(depth + 1);
    printAstSymbol(x->symbol, depth);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstExpressions(struct AstExpressions * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstExpressions (NULL)"); return; }
    printf("AstExpressions[\n");
    printAstExpressions(x->next, depth + 1);
    printf("\n");
    printAstExpression(x->expression, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstEnv(struct AstEnv * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstEnv (NULL)"); return; }
    printf("AstEnv[\n");
    printAstPackage(x->package, depth + 1);
    printf("\n");
    printAstDefinitions(x->definitions, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstString(struct AstString * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstString (NULL)"); return; }
    printf("AstString[\n");
    pad(depth + 1);
    printf("char * %s", x->string);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstDefinition(struct AstDefinition * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstDefinition (NULL)"); return; }
    printf("AstDefinition[\n");
    switch(x->type) {
        case AST_DEFINITION_TYPE_DEFINE:
            pad(depth + 1);
            printf("AST_DEFINITION_TYPE_DEFINE\n");
            printAstDefine(x->val.define, depth + 1);
            break;
        case AST_DEFINITION_TYPE_PROTOTYPE:
            pad(depth + 1);
            printf("AST_DEFINITION_TYPE_PROTOTYPE\n");
            printAstPrototype(x->val.prototype, depth + 1);
            break;
        case AST_DEFINITION_TYPE_LOAD:
            pad(depth + 1);
            printf("AST_DEFINITION_TYPE_LOAD\n");
            printAstLoad(x->val.load, depth + 1);
            break;
        case AST_DEFINITION_TYPE_TYPEDEF:
            pad(depth + 1);
            printf("AST_DEFINITION_TYPE_TYPEDEF\n");
            printAstTypeDef(x->val.typeDef, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstDefinition", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstSinglePrototype(struct AstSinglePrototype * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstSinglePrototype (NULL)"); return; }
    printf("AstSinglePrototype[\n");
    switch(x->type) {
        case AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE:
            pad(depth + 1);
            printf("AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE\n");
            printAstPrototypeSymbolType(x->val.symbolType, depth + 1);
            break;
        case AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE:
            pad(depth + 1);
            printf("AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE\n");
            printAstPrototype(x->val.prototype, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstSinglePrototype", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstTypeClause(struct AstTypeClause * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstTypeClause (NULL)"); return; }
    printf("AstTypeClause[\n");
    switch(x->type) {
        case AST_TYPECLAUSE_TYPE_INTEGER:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_INTEGER\n");
            pad(depth + 1);
            printf("void * %p", x->val.integer);
            break;
        case AST_TYPECLAUSE_TYPE_CHARACTER:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_CHARACTER\n");
            pad(depth + 1);
            printf("void * %p", x->val.character);
            break;
        case AST_TYPECLAUSE_TYPE_BOOLEAN:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_BOOLEAN\n");
            pad(depth + 1);
            printf("void * %p", x->val.boolean);
            break;
        case AST_TYPECLAUSE_TYPE_STRING:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_STRING\n");
            pad(depth + 1);
            printf("void * %p", x->val.string);
            break;
        case AST_TYPECLAUSE_TYPE_LIST:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_LIST\n");
            printAstType(x->val.list, depth + 1);
            break;
        case AST_TYPECLAUSE_TYPE_TYPE:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_TYPE\n");
            printAstType(x->val.type, depth + 1);
            break;
        case AST_TYPECLAUSE_TYPE_TYPESYMBOL:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_TYPESYMBOL\n");
            pad(depth + 1);
            printAstSymbol(x->val.typeSymbol, depth);
            break;
        case AST_TYPECLAUSE_TYPE_VAR:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_VAR\n");
            pad(depth + 1);
            printAstSymbol(x->val.var, depth);
            break;
        case AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR\n");
            printAstTypeConstructor(x->val.typeconstructor, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstTypeClause", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstFun(struct AstFun * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstFun (NULL)"); return; }
    printf("AstFun[\n");
    switch(x->type) {
        case AST_FUN_TYPE_FUNCTION:
            pad(depth + 1);
            printf("AST_FUN_TYPE_FUNCTION\n");
            printAstFunction(x->val.function, depth + 1);
            break;
        case AST_FUN_TYPE_COMPOSITEFUNCTION:
            pad(depth + 1);
            printf("AST_FUN_TYPE_COMPOSITEFUNCTION\n");
            printAstCompositeFunction(x->val.compositeFunction, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstFun", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstArg(struct AstArg * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstArg (NULL)"); return; }
    printf("AstArg[\n");
    switch(x->type) {
        case AST_ARG_TYPE_WILDCARD:
            pad(depth + 1);
            printf("AST_ARG_TYPE_WILDCARD\n");
            pad(depth + 1);
            printf("void * %p", x->val.wildcard);
            break;
        case AST_ARG_TYPE_SYMBOL:
            pad(depth + 1);
            printf("AST_ARG_TYPE_SYMBOL\n");
            pad(depth + 1);
            printAstSymbol(x->val.symbol, depth);
            break;
        case AST_ARG_TYPE_CONS:
            pad(depth + 1);
            printf("AST_ARG_TYPE_CONS\n");
            printAstArgPair(x->val.cons, depth + 1);
            break;
        case AST_ARG_TYPE_NAMED:
            pad(depth + 1);
            printf("AST_ARG_TYPE_NAMED\n");
            printAstNamedArg(x->val.named, depth + 1);
            break;
        case AST_ARG_TYPE_LIST:
            pad(depth + 1);
            printf("AST_ARG_TYPE_LIST\n");
            printAstArgList(x->val.list, depth + 1);
            break;
        case AST_ARG_TYPE_ENV:
            pad(depth + 1);
            printf("AST_ARG_TYPE_ENV\n");
            printAstEnvType(x->val.env, depth + 1);
            break;
        case AST_ARG_TYPE_UNPACK:
            pad(depth + 1);
            printf("AST_ARG_TYPE_UNPACK\n");
            printAstUnpack(x->val.unpack, depth + 1);
            break;
        case AST_ARG_TYPE_NUMBER:
            pad(depth + 1);
            printf("AST_ARG_TYPE_NUMBER\n");
            pad(depth + 1);
            printf("int %d", x->val.number);
            break;
        case AST_ARG_TYPE_STRING:
            pad(depth + 1);
            printf("AST_ARG_TYPE_STRING\n");
            printAstString(x->val.string, depth + 1);
            break;
        case AST_ARG_TYPE_CHARACTER:
            pad(depth + 1);
            printf("AST_ARG_TYPE_CHARACTER\n");
            pad(depth + 1);
            printf("char %c", x->val.character);
            break;
        case AST_ARG_TYPE_YES:
            pad(depth + 1);
            printf("AST_ARG_TYPE_YES\n");
            pad(depth + 1);
            printf("void * %p", x->val.yes);
            break;
        case AST_ARG_TYPE_NO:
            pad(depth + 1);
            printf("AST_ARG_TYPE_NO\n");
            pad(depth + 1);
            printf("void * %p", x->val.no);
            break;
        default:
            cant_happen("unrecognised type %d in printAstArg", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstExpression(struct AstExpression * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstExpression (NULL)"); return; }
    printf("AstExpression[\n");
    switch(x->type) {
        case AST_EXPRESSION_TYPE_BACK:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_BACK\n");
            pad(depth + 1);
            printf("void * %p", x->val.back);
            break;
        case AST_EXPRESSION_TYPE_BINOP:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_BINOP\n");
            printAstBinOp(x->val.binOp, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_NOT:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_NOT\n");
            printAstExpression(x->val.not, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_NEGATE:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_NEGATE\n");
            printAstExpression(x->val.negate, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_HERE:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_HERE\n");
            printAstExpression(x->val.here, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_FUNCALL\n");
            printAstFunCall(x->val.funCall, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_SYMBOL\n");
            pad(depth + 1);
            printAstSymbol(x->val.symbol, depth);
            break;
        case AST_EXPRESSION_TYPE_NUMBER:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_NUMBER\n");
            pad(depth + 1);
            printf("int %d", x->val.number);
            break;
        case AST_EXPRESSION_TYPE_STRING:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_STRING\n");
            printAstString(x->val.string, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_CHARACTER:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_CHARACTER\n");
            pad(depth + 1);
            printf("char %c", x->val.character);
            break;
        case AST_EXPRESSION_TYPE_YES:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_YES\n");
            pad(depth + 1);
            printf("void * %p", x->val.yes);
            break;
        case AST_EXPRESSION_TYPE_NO:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_NO\n");
            pad(depth + 1);
            printf("void * %p", x->val.no);
            break;
        case AST_EXPRESSION_TYPE_LIST:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_LIST\n");
            printAstExpressions(x->val.list, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_FUN:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_FUN\n");
            printAstFun(x->val.fun, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_ENV:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_ENV\n");
            printAstEnv(x->val.env, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_CONDITIONAL:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_CONDITIONAL\n");
            printAstConditional(x->val.conditional, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_SWITCHSTATEMENT:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_SWITCHSTATEMENT\n");
            printAstSwitch(x->val.switchStatement, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstExpression", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

