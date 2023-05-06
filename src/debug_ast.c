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

#include <stdio.h>
#include "debug_ast.h"


void printAstArgList(AstArgList *x) {
    if (x == NULL) return;
    printAstArg(x->arg);
    if (x->next) {
        printf(", ");
        printAstArgList(x->next);
    }
}

void printAstArg(AstArg *x) {
    if (x == NULL) return;
    switch (x->type) {
        case AST_ARG_TYPE_SYMBOL: {
            printAstSymbol(x->val.symbol);
        }
        break;
        case AST_ARG_TYPE_CONS: {
            printAstArgPair(x->val.cons);
        }
        break;
        case AST_ARG_TYPE_NAMED: {
            printAstNamedArg(x->val.named);
        }
        break;
        case AST_ARG_TYPE_LIST: {
            printAstArgList(x->val.list);
        }
        break;
        case AST_ARG_TYPE_ENV: {
            printAstEnvType(x->val.env);
        }
        break;
        case AST_ARG_TYPE_STRING: {
            printAstString(x->val.string);
        }
        break;
        case AST_ARG_TYPE_NUMBER:
            printf("%d", x->val.number);
            break;
        case AST_ARG_TYPE_CHAR:
            printf("'%c'", x->val.character);
            break;
        case AST_ARG_TYPE_TRUE:
            printf("true");
            break;
        case AST_ARG_TYPE_FALSE:
            printf("false");
            break;
        case AST_ARG_TYPE_WILDCARD:
            printf("_");
        break;
    }
}

void printAstArgPair(AstArgPair *x) {
    if (x == NULL) return;
    printf("(");
    printAstArg(x->car);
    printf(" @ ");
    printAstArg(x->cdr);
    printf(")");
}

void printAstBinOp(AstBinOp *x) {
    if (x == NULL) return;
    printf("(");
    printAstExpression(x->lhs);
    switch(x->type) {
        case AST_BINOP_TYPE_THEN: printf(" then "); break;
        case AST_BINOP_TYPE_AND: printf(" and "); break;
        case AST_BINOP_TYPE_OR: printf(" or "); break;
        case AST_BINOP_TYPE_XOR: printf(" xor "); break;
        case AST_BINOP_TYPE_EQ: printf(" == "); break;
        case AST_BINOP_TYPE_NE: printf(" != "); break;
        case AST_BINOP_TYPE_LT: printf(" < "); break;
        case AST_BINOP_TYPE_GT: printf(" > "); break;
        case AST_BINOP_TYPE_LE: printf(" <= "); break;
        case AST_BINOP_TYPE_GE: printf(" >= "); break;
        case AST_BINOP_TYPE_CONS: printf(" @ "); break;
        case AST_BINOP_TYPE_APPEND: printf(" @@ "); break;
        case AST_BINOP_TYPE_ADD: printf(" + "); break;
        case AST_BINOP_TYPE_SUB: printf(" - "); break;
        case AST_BINOP_TYPE_MUL: printf(" * "); break;
        case AST_BINOP_TYPE_DIV: printf(" / "); break;
        case AST_BINOP_TYPE_MOD: printf(" %% "); break;
        case AST_BINOP_TYPE_POW: printf(" ^ "); break;
        case AST_BINOP_TYPE_DOT: printf(" . "); break;
    }
    printAstExpression(x->rhs);
    printf(")");
}

static void printAstCompositeFunctionInternal(AstCompositeFunction *x) {
    if (x == NULL) return;
    printAstFunction(x->function);
    printAstCompositeFunctionInternal(x->next);
}

void printAstCompositeFunction(AstCompositeFunction *x) {
    if (x == NULL) return;
    printf("{\n");
    printAstCompositeFunctionInternal(x);
    printf("}\n");
}

void printAstConditional(AstConditional *x) {
    if (x == NULL) return;
    printf("if (");
    printAstExpression(x->expression);
    printf(") ");
    printAstNest(x->consequent);
    printf(" else ");
    printAstNest(x->alternative);
}

void printAstDefine(AstDefine *x) {
    if (x == NULL) return;
    printAstSymbol(x->symbol);
    printf(" = ");
    printAstExpression(x->expression);
    printf(";\n");
}

void printAstDefinition(AstDefinition *x) {
    if (x == NULL) return;
    switch (x->type) {
        case AST_DEFINITION_TYPE_DEFINE: {
            printAstDefine(x->val.define);
        }
        break;
        case AST_DEFINITION_TYPE_PROTOTYPE: {
            printAstPrototype(x->val.prototype);
        }
        break;
        case AST_DEFINITION_TYPE_LOAD: {
            printAstLoad(x->val.load);
        }
        break;
        case AST_DEFINITION_TYPE_TYPEDEF: {
            printAstTypeDef(x->val.typeDef);
        }
        break;
    }
}

void printAstDefinitions(AstDefinitions *x) {
    if (x == NULL) return;
    printAstDefinition(x->definition);
    printAstDefinitions(x->next);
}

void printAstEnv(AstEnv *x) {
    if (x == NULL) return;
    printf("env ");
    printAstPackage(x->package);
    printf(" {\n");
    printAstDefinitions(x->definitions);
    printf("}\n");
}

void printAstEnvType(AstEnvType *x) {
    if (x == NULL) return;
    printAstSymbol(x->name);
    printf(" : ");
    printAstSymbol(x->prototype);
}

void printAstExpression(AstExpression *x) {
    if (x == NULL) return;
    switch (x->type) {
        case AST_EXPRESSION_TYPE_BINOP: {
            printAstBinOp(x->val.binOp);
        }
        break;
        case AST_EXPRESSION_TYPE_NOT: {
            printf("(not ");
            printAstExpression(x->val.expression);
            printf(")");
        }
        break;
        case AST_EXPRESSION_TYPE_NEGATE: {
            printf("(- ");
            printAstExpression(x->val.expression);
            printf(")");
        }
        break;
        case AST_EXPRESSION_TYPE_FUNCALL: {
            printAstFunCall(x->val.funCall);
        }
        break;
        case AST_EXPRESSION_TYPE_SYMBOL: {
            printAstSymbol(x->val.symbol);
        }
        break;
        case AST_EXPRESSION_TYPE_STRING: {
            printAstString(x->val.string);
        }
        break;
        case AST_EXPRESSION_TYPE_LIST: {
            printf("[");
            printAstExpressions(x->val.list);
            printf("]");
        }
        break;
        case AST_EXPRESSION_TYPE_FUN: {
            printAstFun(x->val.fun);
        }
        break;
        case AST_EXPRESSION_TYPE_ENV: {
            printAstEnv(x->val.env);
        }
        break;
        case AST_EXPRESSION_TYPE_CONDITIONAL: {
            printAstConditional(x->val.conditional);
        }
        break;
        case AST_EXPRESSION_TYPE_SWITCH: {
            printAstSwitch(x->val.switchStatement);
        }
        break;
        case AST_EXPRESSION_TYPE_NUMBER:
            printf("%d", x->val.number);
            break;
        case AST_EXPRESSION_TYPE_CHAR:
            printf("'%d'", x->val.character);
            break;
        case AST_EXPRESSION_TYPE_TRUE:
            printf("true");
            break;
        case AST_EXPRESSION_TYPE_FALSE:
            printf("false");
            break;
        case AST_EXPRESSION_TYPE_BACK:
            printf("back");
            break;
        break;
    }
}

void printAstExpressions(AstExpressions *x) {
    if (x == NULL) return;
    printAstExpression(x->expression);
    if (x->next != NULL) {
        printf(", ");
        printAstExpressions(x->next);
    }
}

void printAstFlatType(AstFlatType *x) {
    if (x == NULL) return;
    printAstSymbol(x->symbol);
    if (x->typeSymbols != NULL) {
        printf("(");
        printAstTypeSymbols(x->typeSymbols);
        printf(")");
    }
}

void printAstFunCall(AstFunCall *x) {
    if (x == NULL) return;
    printAstSymbol(x->symbol);
    printf("(");
    printAstExpressions(x->expressions);
    printf(")");
}

void printAstFunction(AstFunction *x) {
    if (x == NULL) return;
    printAstArgList(x->argList);
    printAstNest(x->nest);
}

void printAstFun(AstFun *x) {
    if (x == NULL) return;
    switch (x->type) {
        case AST_FUN_TYPE_FUNCTION: {
            printAstFunction(x->val.function);
        }
        break;
        case AST_FUN_TYPE_COMPOSITEFUNCTION: {
            printAstCompositeFunction(x->val.compositeFunction);
        }
        break;
    }
}

void printAstLoad(AstLoad *x) {
    if (x == NULL) return;
    printf("load ");
    printAstPackage(x->package);
    if (x->symbol != NULL) {
        printf(" as ");
        printAstSymbol(x->symbol);
    }
    printf(";\n");
}

void printAstNamedArg(AstNamedArg *x) {
    if (x == NULL) return;
    printAstSymbol(x->name);
    printf(" = ");
    printAstArg(x->arg);
}

void printAstNest(AstNest *x) {
    if (x == NULL) return;
    if (x->definitions != NULL) {
        printf("let\n");
        printAstDefinitions(x->definitions);
        printf("in\n");
    }
    printAstExpression(x->expression);
}

void printAstPackage(AstPackage *x) {
    if (x == NULL) return;
    printAstSymbol(x->symbol);
    if (x->next) {
        printf(".");
        printAstPackage(x->next);
    }
}

void printAstPrototypeBody(AstPrototypeBody *x) {
    if (x == NULL) return;
    printAstSinglePrototype(x->single);
    printAstPrototypeBody(x->next);
}

void printAstPrototype(AstPrototype *x) {
    if (x == NULL) return;
    printAstSymbol(x->symbol);
    printAstPrototypeBody(x->body);
}

void printAstPrototypeSymbolType(AstPrototypeSymbolType *x) {
    if (x == NULL) return;
    printAstSymbol(x->symbol);
    printf(" ??? ");
    printAstType(x->type);
}

void printAstSinglePrototype(AstSinglePrototype *x) {
    if (x == NULL) return;
    switch (x->type) {
        case AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE: {
            printAstPrototypeSymbolType(x->val.symbolType);
        }
        break;
        case AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE: {
            printAstPrototype(x->val.prototype);
        }
        break;
    }
}

void printAstString(AstString *x) {
    if (x == NULL) return;
    printf("\"%s\"", x->string);
}

void printAstSwitch(AstSwitch *x) {
    if (x == NULL) return;
    printf("switch (");
    printAstExpressions(x->expressions);
    printf(") ");
    printAstCompositeFunction(x->compositeFunction);
}

void printAstSymbol(AstSymbol *x) {
    if (x == NULL) return;
    printf("%s", x->name);
}

void printAstTypeBody(AstTypeBody *x) {
    if (x == NULL) return;
    printAstTypeConstructor(x->typeConstructor);
    if (x->next != NULL) {
        printf(" | ");
        printAstTypeBody(x->next);
    }
}

void printAstTypeClause(AstTypeClause *x) {
    if (x == NULL) return;
    switch (x->type) {
        case AST_TYPECLAUSE_TYPE_LIST: {
            printAstType(x->val.type);
        }
        break;
        case AST_TYPECLAUSE_TYPE_INT:
            printf("int");
            break;
        case AST_TYPECLAUSE_TYPE_CHAR:
            printf("char");
            break;
        case AST_TYPECLAUSE_TYPE_BOOL:
            printf("bool");
            break;
        case AST_TYPECLAUSE_TYPE_STRING:
            printf("string");
            break;
        case AST_TYPECLAUSE_TYPE_VAR: {
            printAstSymbol(x->val.typeSymbol);
        }
        break;
        case AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR: {
            printAstTypeConstructor(x->val.constructor);
        }
        break;
        case AST_TYPECLAUSE_TYPE_TYPE: {
            printAstType(x->val.type);
        }
        break;
    }
}

void printAstTypeConstructor(AstTypeConstructor *x) {
    if (x == NULL) return;
    printAstSymbol(x->symbol);
    printf("(");
    printAstTypeList(x->typeList);
    printf(")");
}

void printAstTypeDef(AstTypeDef *x) {
    if (x == NULL) return;
    printAstFlatType(x->flatType);
    printf("{ ");
    printAstTypeBody(x->typeBody);
    printf(" }");
}

void printAstTypeList(AstTypeList *x) {
    if (x == NULL) return;
    printAstType(x->type);
    if (x->next != NULL) {
        printf(", ");
        printAstTypeList(x->next);
    }
}

void printAstType(AstType *x) {
    if (x == NULL) return;
    printAstTypeClause(x->typeClause);
    if (x->next != NULL) {
        printf(", ");
        printAstType(x->next);
    }
}

void printAstTypeSymbols(AstTypeSymbols *x) {
    if (x == NULL) return;
    printf("#");
    printAstSymbol(x->typeSymbol);
    if (x->next) {
        printf(", ");
        printAstTypeSymbols(x->next);
    }
}
