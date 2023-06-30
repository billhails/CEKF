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
    printAstDefinition(x->definition, depth + 1);
    printf("\n");
    printAstDefinitions(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstDefine(struct AstDefine * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstDefine (NULL)"); return; }
    printf("AstDefine[\n");
        printAstSymbol(x->symbol, depth + 1);
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
        printAstSymbol(x->symbol, depth + 1);
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
    printAstSinglePrototype(x->single, depth + 1);
    printf("\n");
    printAstPrototypeBody(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstPrototypeSymbolType(struct AstPrototypeSymbolType * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstPrototypeSymbolType (NULL)"); return; }
    printf("AstPrototypeSymbolType[\n");
        printAstSymbol(x->symbol, depth + 1);
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
        printAstSymbol(x->symbol, depth + 1);
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
        printAstSymbol(x->symbol, depth + 1);
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
        printAstSymbol(x->typeSymbol, depth + 1);
    printf("\n");
    printAstTypeSymbols(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstTypeBody(struct AstTypeBody * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstTypeBody (NULL)"); return; }
    printf("AstTypeBody[\n");
    printAstTypeConstructor(x->typeConstructor, depth + 1);
    printf("\n");
    printAstTypeBody(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstTypeConstructor(struct AstTypeConstructor * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstTypeConstructor (NULL)"); return; }
    printf("AstTypeConstructor[\n");
        printAstSymbol(x->symbol, depth + 1);
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
    printAstType(x->type, depth + 1);
    printf("\n");
    printAstTypeList(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstType(struct AstType * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstType (NULL)"); return; }
    printf("AstType[\n");
    printAstTypeClause(x->typeClause, depth + 1);
    printf("\n");
    printAstType(x->next, depth + 1);
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

void printAstCompositeFunction(struct AstCompositeFunction * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstCompositeFunction (NULL)"); return; }
    printf("AstCompositeFunction[\n");
    printAstFunction(x->function, depth + 1);
    printf("\n");
    printAstCompositeFunction(x->next, depth + 1);
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
    printAstArg(x->arg, depth + 1);
    printf("\n");
    printAstArgList(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstUnpack(struct AstUnpack * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstUnpack (NULL)"); return; }
    printf("AstUnpack[\n");
        printAstSymbol(x->symbol, depth + 1);
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
        printAstSymbol(x->name, depth + 1);
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
        printAstSymbol(x->name, depth + 1);
    printf("\n");
        printAstSymbol(x->prototype, depth + 1);
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
        printAstSymbol(x->symbol, depth + 1);
    printf("\n");
    printAstPackage(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printAstExpressions(struct AstExpressions * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("AstExpressions (NULL)"); return; }
    printf("AstExpressions[\n");
    printAstExpression(x->expression, depth + 1);
    printf("\n");
    printAstExpressions(x->next, depth + 1);
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
        case AST_TYPECLAUSE_TYPE_VAR:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_VAR\n");
                        printAstSymbol(x->val.var, depth + 1);
            break;
        case AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR:
            pad(depth + 1);
            printf("AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR\n");
            printAstTypeConstructor(x->val.typeConstructor, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstTypeClause", x->type);
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
                        printAstSymbol(x->val.symbol, depth + 1);
            break;
        case AST_ARG_TYPE_NAMED:
            pad(depth + 1);
            printf("AST_ARG_TYPE_NAMED\n");
            printAstNamedArg(x->val.named, depth + 1);
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
        case AST_EXPRESSION_TYPE_NIL:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_NIL\n");
                        pad(depth + 1);
printf("void * %p", x->val.nil);
            break;
        case AST_EXPRESSION_TYPE_BACK:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_BACK\n");
                        pad(depth + 1);
printf("void * %p", x->val.back);
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_FUNCALL\n");
            printAstFunCall(x->val.funCall, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_SYMBOL\n");
                        printAstSymbol(x->val.symbol, depth + 1);
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
        case AST_EXPRESSION_TYPE_FUN:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_FUN\n");
            printAstCompositeFunction(x->val.fun, depth + 1);
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
        case AST_EXPRESSION_TYPE_NEST:
            pad(depth + 1);
            printf("AST_EXPRESSION_TYPE_NEST\n");
            printAstNest(x->val.nest, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstExpression", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

