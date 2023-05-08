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

/* memory allocation routines for the AST generated by the parser */

#include "ast.h"
#include "common.h"
#include "hash.h"
#include "memory.h"

AstNest *newAstNest(AstDefinitions *definitions, AstExpressions *expressions) {
    AstNest *x = NEW(AstNest, OBJTYPE_AST);
    x->definitions = definitions;
    x->expressions = expressions;
    return x;
}

AstDefinitions *newAstDefinitions(AstDefinitions *next, AstDefinition *definition) {
    AstDefinitions *x = NEW(AstDefinitions, OBJTYPE_AST);
    x->next = next;
    x->definition = definition;
    return x;
}

AstDefinition *newAstDefinition(AstDefinitionType type, AstDefinitionValue val) {
    AstDefinition *x = NEW(AstDefinition, OBJTYPE_AST);
    x->type = type;
    x->val = val;
    return x;
}

AstDefine *newAstDefine(AstSymbol *symbol, AstExpression *expression) {
    AstDefine *x = NEW(AstDefine, OBJTYPE_AST);
    x->symbol = symbol;
    x->expression = expression;
    return x;
}

AstPrototype *newAstPrototype(AstSymbol *symbol, AstPrototypeBody *body) {
    AstPrototype *x = NEW(AstPrototype, OBJTYPE_AST);
    x->symbol = symbol;
    x->body = body;
    return x;
}

AstPrototypeBody *newAstPrototypeBody(AstPrototypeBody *next, AstSinglePrototype *single) {
    AstPrototypeBody *x = NEW(AstPrototypeBody, OBJTYPE_AST);
    x->next = next;
    x->single = single;
    return x;
}

AstSinglePrototype *newAstSinglePrototype(AstSinglePrototypeType type, AstSinglePrototypeValue val) {
    AstSinglePrototype *x = NEW(AstSinglePrototype, OBJTYPE_AST);
    x->type = type;
    x->val = val;
    return x;
}

AstPrototypeSymbolType *newAstPrototypeSymbolType(AstSymbol *symbol, AstType *type) {
    AstPrototypeSymbolType *x = NEW(AstPrototypeSymbolType, OBJTYPE_AST);
    x->symbol = symbol;
    x->type = type;
    return x;
}

AstLoad *newAstLoad(AstPackage *package, AstSymbol *symbol) {
    AstLoad *x = NEW(AstLoad, OBJTYPE_AST);
    x->package = package;
    x->symbol = symbol;
    return x;
}

AstTypeDef *newAstTypeDef(AstFlatType *flatType, AstTypeBody *typeBody) {
    AstTypeDef *x = NEW(AstTypeDef, OBJTYPE_AST);
    x->flatType = flatType;
    x->typeBody = typeBody;
    return x;
}

AstFlatType *newAstFlatType(AstSymbol *symbol, AstTypeSymbols *typeSymbols) {
    AstFlatType *x = NEW(AstFlatType, OBJTYPE_AST);
    x->symbol = symbol;
    x->typeSymbols = typeSymbols;
    return x;
}

AstTypeSymbols *newAstTypeSymbols(AstTypeSymbols *next, AstSymbol *typeSymbol) {
    AstTypeSymbols *x = NEW(AstTypeSymbols, OBJTYPE_AST);
    x->next = next;
    x->typeSymbol = typeSymbol;
    return x;
}

AstTypeBody *newAstTypeBody(AstTypeBody *next, AstTypeConstructor *typeConstructor) {
    AstTypeBody *x = NEW(AstTypeBody, OBJTYPE_AST);
    x->next = next;
    x->typeConstructor = typeConstructor;
    return x;
}

AstTypeConstructor *newAstTypeConstructor(AstSymbol *symbol, AstTypeList *typeList) {
    AstTypeConstructor *x = NEW(AstTypeConstructor, OBJTYPE_AST);
    x->symbol = symbol;
    x->typeList = typeList;
    return x;
}

AstTypeList *newAstTypeList(AstTypeList *next, AstType *type) {
    AstTypeList *x = NEW(AstTypeList, OBJTYPE_AST);
    x->next = next;
    x->type = type;
    return x;
}

AstType *newAstType(AstType *next, AstTypeClause *typeClause) {
    AstType *x = NEW(AstType, OBJTYPE_AST);
    x->next = next;
    x->typeClause = typeClause;
    return x;
}

AstTypeClause *newAstTypeClause(AstTypeClauseType type, AstTypeClauseValue val) {
    AstTypeClause *x = NEW(AstTypeClause, OBJTYPE_AST);
    x->type = type;
    x->val = val;
    return x;
}

AstConditional *newAstConditional(AstExpression *expression, AstNest *consequent, AstNest *alternative) {
    AstConditional *x = NEW(AstConditional, OBJTYPE_AST);
    x->expression = expression;
    x->consequent = consequent;
    x->alternative = alternative;
    return x;
}

AstSwitch *newAstSwitch(AstExpressions *expressions, AstCompositeFunction *compositeFunction) {
    AstSwitch *x = NEW(AstSwitch, OBJTYPE_AST);
    x->expressions = expressions;
    x->compositeFunction = compositeFunction;
    return x;
}

AstFun *newAstFun(AstFunType type, AstFunValue val) {
    AstFun *x = NEW(AstFun, OBJTYPE_AST);
    x->type = type;
    x->val = val;
    return x;
}

AstCompositeFunction *newAstCompositeFunction(AstCompositeFunction *next, AstFunction *function) {
    AstCompositeFunction *x = NEW(AstCompositeFunction, OBJTYPE_AST);
    x->next = next;
    x->function = function;
    return x;
}

AstFunction *newAstFunction(AstArgList *argList, AstNest *nest) {
    AstFunction *x = NEW(AstFunction, OBJTYPE_AST);
    x->argList = argList;
    x->nest = nest;
    return x;
}

AstArg *newAstArg(AstArgType type, AstArgValue val) {
    AstArg *x = NEW(AstArg, OBJTYPE_AST);
    x->type = type;
    x->val = val;
    return x;
}

AstArgList *newAstArgList(AstArgList *next, AstArg *arg) {
    AstArgList *x = NEW(AstArgList, OBJTYPE_AST);
    x->next = next;
    x->arg = arg;
    return x;
}

AstArgPair *newAstArgPair(AstArg *car, AstArg *cdr) {
    AstArgPair *x = NEW(AstArgPair, OBJTYPE_AST);
    x->car = car;
    x->cdr = cdr;
    return x;
}

AstNamedArg *newAstNamedArg(AstSymbol *name, AstArg *arg) {
    AstNamedArg *x = NEW(AstNamedArg, OBJTYPE_AST);
    x->name = name;
    x->arg = arg;
    return x;
}

AstEnvType *newAstEnvType(AstSymbol *name, AstSymbol *prototype) {
    AstEnvType *x = NEW(AstEnvType, OBJTYPE_AST);
    x->name = name;
    x->prototype = prototype;
    return x;
}

AstExpression *newAstExpression(AstExpressionType type, AstExpressionValue val) {
    AstExpression *x = NEW(AstExpression, OBJTYPE_AST);
    x->type = type;
    x->val = val;
    return x;
}

AstBinOp *newAstBinOp(AstBinOpType type, AstExpression *lhs, AstExpression *rhs) {
    AstBinOp *x = NEW(AstBinOp, OBJTYPE_AST);
    x->type = type;
    x->lhs = lhs;
    x->rhs = rhs;
    return x;
}

AstFunCall *newAstFunCall(AstExpression *function, AstExpressions *arguments) {
    AstFunCall *x = NEW(AstFunCall, OBJTYPE_AST);
    x->function = function;
    x->arguments = arguments;
    return x;
}

AstPackage *newAstPackage(AstPackage *next, AstSymbol *symbol) {
    AstPackage *x = NEW(AstPackage, OBJTYPE_AST);
    x->next = next;
    x->symbol = symbol;
    return x;
}

AstExpressions *newAstExpressions(AstExpressions *next, AstExpression *expression) {
    AstExpressions *x = NEW(AstExpressions, OBJTYPE_AST);
    x->next = next;
    x->expression = expression;
    return x;
}

AstEnv *newAstEnv(AstPackage *package, AstDefinitions *definitions) {
    AstEnv *x = NEW(AstEnv, OBJTYPE_AST);
    x->package = package;
    x->definitions = definitions;
    return x;
}

AstSymbol *newAstSymbol(AstSymbolType type, char *name) {
    AstSymbol *x = NEW(AstSymbol, OBJTYPE_AST);
    x->type = type;
    x->name = safe_strdup(name);
    x->hash = hashString(name);
    return x;
}

AstString *newAstString(char *string) {
    AstString *x = NEW(AstString, OBJTYPE_AST);
    x->string = safe_strdup(string);
    return x;
}

AstUnpack *newAstUnpack(AstSymbol *symbol, AstArgList *argList) {
    AstUnpack *x = NEW(AstUnpack, OBJTYPE_AST);
    x->symbol = symbol;
    x->argList = argList;
    return x;
}

/************************/

void markAstArgList(AstArgList *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstArgList(x->next);
    markAstArg(x->arg);
}

void markAstArg(AstArg *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case AST_ARG_TYPE_SYMBOL: {
            markAstSymbol(x->val.symbol);
        }
        break;
        case AST_ARG_TYPE_CONS: {
            markAstArgPair(x->val.cons);
        }
        break;
        case AST_ARG_TYPE_NAMED: {
            markAstNamedArg(x->val.named);
        }
        break;
        case AST_ARG_TYPE_LIST: {
            markAstArgList(x->val.list);
        }
        break;
        case AST_ARG_TYPE_ENV: {
            markAstEnvType(x->val.env);
        }
        break;
        case AST_ARG_TYPE_STRING: {
            markAstString(x->val.string);
        }
        break;
        case AST_ARG_TYPE_UNPACK: {
            markAstUnpack(x->val.unpack);
        }
        break;
        case AST_ARG_TYPE_NUMBER:
        case AST_ARG_TYPE_CHAR:
        case AST_ARG_TYPE_TRUE:
        case AST_ARG_TYPE_FALSE:
        case AST_ARG_TYPE_WILDCARD:
        break;
    }
}

void markAstArgPair(AstArgPair *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstArg(x->car);
    markAstArg(x->cdr);
}

void markAstBinOp(AstBinOp *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpression(x->lhs);
    markAstExpression(x->rhs);
}

void markAstCompositeFunction(AstCompositeFunction *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstCompositeFunction(x->next);
    markAstFunction(x->function);
}

void markAstConditional(AstConditional *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpression(x->expression);
    markAstNest(x->consequent);
    markAstNest(x->alternative);
}

void markAstDefine(AstDefine *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSymbol(x->symbol);
    markAstExpression(x->expression);
}

void markAstDefinition(AstDefinition *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case AST_DEFINITION_TYPE_DEFINE: {
            markAstDefine(x->val.define);
        }
        break;
        case AST_DEFINITION_TYPE_PROTOTYPE: {
            markAstPrototype(x->val.prototype);
        }
        break;
        case AST_DEFINITION_TYPE_LOAD: {
            markAstLoad(x->val.load);
        }
        break;
        case AST_DEFINITION_TYPE_TYPEDEF: {
            markAstTypeDef(x->val.typeDef);
        }
        break;
    }
}

void markAstDefinitions(AstDefinitions *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstDefinitions(x->next);
    markAstDefinition(x->definition);
}

void markAstEnv(AstEnv *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstPackage(x->package);
    markAstDefinitions(x->definitions);
}

void markAstEnvType(AstEnvType *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSymbol(x->name);
    markAstSymbol(x->prototype);
}

void markAstExpression(AstExpression *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case AST_EXPRESSION_TYPE_BINOP: {
            markAstBinOp(x->val.binOp);
        }
        break;
        case AST_EXPRESSION_TYPE_HERE:
        case AST_EXPRESSION_TYPE_NOT:
        case AST_EXPRESSION_TYPE_NEGATE: {
            markAstExpression(x->val.expression);
        }
        break;
        case AST_EXPRESSION_TYPE_FUNCALL: {
            markAstFunCall(x->val.funCall);
        }
        break;
        case AST_EXPRESSION_TYPE_SYMBOL: {
            markAstSymbol(x->val.symbol);
        }
        break;
        case AST_EXPRESSION_TYPE_STRING: {
            markAstString(x->val.string);
        }
        break;
        case AST_EXPRESSION_TYPE_LIST: {
            markAstExpressions(x->val.list);
        }
        break;
        case AST_EXPRESSION_TYPE_FUN: {
            markAstFun(x->val.fun);
        }
        break;
        case AST_EXPRESSION_TYPE_ENV: {
            markAstEnv(x->val.env);
        }
        break;
        case AST_EXPRESSION_TYPE_CONDITIONAL: {
            markAstConditional(x->val.conditional);
        }
        break;
        case AST_EXPRESSION_TYPE_SWITCH: {
            markAstSwitch(x->val.switchStatement);
        }
        break;
        case AST_EXPRESSION_TYPE_NUMBER:
        case AST_EXPRESSION_TYPE_CHAR:
        case AST_EXPRESSION_TYPE_TRUE:
        case AST_EXPRESSION_TYPE_FALSE:
        case AST_EXPRESSION_TYPE_BACK:
        break;
    }
}

void markAstExpressions(AstExpressions *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpressions(x->next);
    markAstExpression(x->expression);
}

void markAstFlatType(AstFlatType *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSymbol(x->symbol);
    markAstTypeSymbols(x->typeSymbols);
}

void markAstFunCall(AstFunCall *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpression(x->function);
    markAstExpressions(x->arguments);
}

void markAstFunction(AstFunction *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstArgList(x->argList);
    markAstNest(x->nest);
}

void markAstFun(AstFun *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case AST_FUN_TYPE_FUNCTION: {
            markAstFunction(x->val.function);
        }
        break;
        case AST_FUN_TYPE_COMPOSITEFUNCTION: {
            markAstCompositeFunction(x->val.compositeFunction);
        }
        break;
    }
}

void markAstLoad(AstLoad *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstPackage(x->package);
    markAstSymbol(x->symbol);
}

void markAstNamedArg(AstNamedArg *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSymbol(x->name);
    markAstArg(x->arg);
}

void markAstNest(AstNest *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstDefinitions(x->definitions);
    markAstExpressions(x->expressions);
}

void markAstPackage(AstPackage *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstPackage(x->next);
    markAstSymbol(x->symbol);
}

void markAstPrototypeBody(AstPrototypeBody *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstPrototypeBody(x->next);
    markAstSinglePrototype(x->single);
}

void markAstPrototype(AstPrototype *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSymbol(x->symbol);
    markAstPrototypeBody(x->body);
}

void markAstPrototypeSymbolType(AstPrototypeSymbolType *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSymbol(x->symbol);
    markAstType(x->type);
}

void markAstSinglePrototype(AstSinglePrototype *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE: {
            markAstPrototypeSymbolType(x->val.symbolType);
        }
        break;
        case AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE: {
            markAstPrototype(x->val.prototype);
        }
        break;
    }
}

void markAstString(AstString *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void markAstSwitch(AstSwitch *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpressions(x->expressions);
    markAstCompositeFunction(x->compositeFunction);
}

void markAstSymbol(AstSymbol *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void markAstTypeBody(AstTypeBody *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstTypeBody(x->next);
    markAstTypeConstructor(x->typeConstructor);
}

void markAstTypeClause(AstTypeClause *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case AST_TYPECLAUSE_TYPE_LIST: {
            markAstType(x->val.type);
        }
        break;
        case AST_TYPECLAUSE_TYPE_INT:
        case AST_TYPECLAUSE_TYPE_CHAR:
        case AST_TYPECLAUSE_TYPE_BOOL:
        case AST_TYPECLAUSE_TYPE_STRING:
        break;
        case AST_TYPECLAUSE_TYPE_VAR: {
            markAstSymbol(x->val.typeSymbol);
        }
        break;
        case AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR: {
            markAstTypeConstructor(x->val.constructor);
        }
        break;
        case AST_TYPECLAUSE_TYPE_TYPE: {
            markAstType(x->val.type);
        }
        break;
    }
}

void markAstTypeConstructor(AstTypeConstructor *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSymbol(x->symbol);
    markAstTypeList(x->typeList);
}

void markAstTypeDef(AstTypeDef *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstFlatType(x->flatType);
    markAstTypeBody(x->typeBody);
}

void markAstTypeList(AstTypeList *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstTypeList(x->next);
    markAstType(x->type);
}

void markAstType(AstType *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstType(x->next);
    markAstTypeClause(x->typeClause);
}

void markAstTypeSymbols(AstTypeSymbols *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstTypeSymbols(x->next);
    markAstSymbol(x->typeSymbol);
}

void markAstUnpack(AstUnpack *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSymbol(x->symbol);
    markAstArgList(x->argList);
}
