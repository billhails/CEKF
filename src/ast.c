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



#include "ast.h"

struct AstNest * newAstNest(struct AstDefinitions * definitions, struct AstExpressions * expressions) {
    struct AstNest * x = NEW(AstNest, OBJTYPE_ASTNEST);
    x->definitions = definitions;
    x->expressions = expressions;
    return x;
}

struct AstDefinitions * newAstDefinitions(struct AstDefinition * definition, struct AstDefinitions * next) {
    struct AstDefinitions * x = NEW(AstDefinitions, OBJTYPE_ASTDEFINITIONS);
    x->definition = definition;
    x->next = next;
    return x;
}

struct AstDefine * newAstDefine(HashSymbol * symbol, struct AstExpression * expression) {
    struct AstDefine * x = NEW(AstDefine, OBJTYPE_ASTDEFINE);
    x->symbol = symbol;
    x->expression = expression;
    return x;
}

struct AstPrototype * newAstPrototype(HashSymbol * symbol, struct AstPrototypeBody * body) {
    struct AstPrototype * x = NEW(AstPrototype, OBJTYPE_ASTPROTOTYPE);
    x->symbol = symbol;
    x->body = body;
    return x;
}

struct AstPrototypeBody * newAstPrototypeBody(struct AstSinglePrototype * single, struct AstPrototypeBody * next) {
    struct AstPrototypeBody * x = NEW(AstPrototypeBody, OBJTYPE_ASTPROTOTYPEBODY);
    x->single = single;
    x->next = next;
    return x;
}

struct AstPrototypeSymbolType * newAstPrototypeSymbolType(HashSymbol * symbol, struct AstType * type) {
    struct AstPrototypeSymbolType * x = NEW(AstPrototypeSymbolType, OBJTYPE_ASTPROTOTYPESYMBOLTYPE);
    x->symbol = symbol;
    x->type = type;
    return x;
}

struct AstLoad * newAstLoad(struct AstPackage * package, HashSymbol * symbol) {
    struct AstLoad * x = NEW(AstLoad, OBJTYPE_ASTLOAD);
    x->package = package;
    x->symbol = symbol;
    return x;
}

struct AstTypeDef * newAstTypeDef(struct AstFlatType * flatType, struct AstTypeBody * typeBody) {
    struct AstTypeDef * x = NEW(AstTypeDef, OBJTYPE_ASTTYPEDEF);
    x->flatType = flatType;
    x->typeBody = typeBody;
    return x;
}

struct AstFlatType * newAstFlatType(HashSymbol * symbol, struct AstTypeSymbols * typeSymbols) {
    struct AstFlatType * x = NEW(AstFlatType, OBJTYPE_ASTFLATTYPE);
    x->symbol = symbol;
    x->typeSymbols = typeSymbols;
    return x;
}

struct AstTypeSymbols * newAstTypeSymbols(HashSymbol * typeSymbol, struct AstTypeSymbols * next) {
    struct AstTypeSymbols * x = NEW(AstTypeSymbols, OBJTYPE_ASTTYPESYMBOLS);
    x->typeSymbol = typeSymbol;
    x->next = next;
    return x;
}

struct AstTypeBody * newAstTypeBody(struct AstTypeConstructor * typeConstructor, struct AstTypeBody * next) {
    struct AstTypeBody * x = NEW(AstTypeBody, OBJTYPE_ASTTYPEBODY);
    x->typeConstructor = typeConstructor;
    x->next = next;
    return x;
}

struct AstTypeConstructor * newAstTypeConstructor(HashSymbol * symbol, struct AstTypeList * typeList) {
    struct AstTypeConstructor * x = NEW(AstTypeConstructor, OBJTYPE_ASTTYPECONSTRUCTOR);
    x->symbol = symbol;
    x->typeList = typeList;
    return x;
}

struct AstTypeList * newAstTypeList(struct AstType * type, struct AstTypeList * next) {
    struct AstTypeList * x = NEW(AstTypeList, OBJTYPE_ASTTYPELIST);
    x->type = type;
    x->next = next;
    return x;
}

struct AstType * newAstType(struct AstTypeClause * typeClause, struct AstType * next) {
    struct AstType * x = NEW(AstType, OBJTYPE_ASTTYPE);
    x->typeClause = typeClause;
    x->next = next;
    return x;
}

struct AstConditional * newAstConditional(struct AstExpression * expression, struct AstNest * consequent, struct AstNest * alternative) {
    struct AstConditional * x = NEW(AstConditional, OBJTYPE_ASTCONDITIONAL);
    x->expression = expression;
    x->consequent = consequent;
    x->alternative = alternative;
    return x;
}

struct AstCompositeFunction * newAstCompositeFunction(struct AstFunction * function, struct AstCompositeFunction * next) {
    struct AstCompositeFunction * x = NEW(AstCompositeFunction, OBJTYPE_ASTCOMPOSITEFUNCTION);
    x->function = function;
    x->next = next;
    return x;
}

struct AstFunction * newAstFunction(struct AstArgList * argList, struct AstNest * nest) {
    struct AstFunction * x = NEW(AstFunction, OBJTYPE_ASTFUNCTION);
    x->argList = argList;
    x->nest = nest;
    return x;
}

struct AstArgList * newAstArgList(struct AstArg * arg, struct AstArgList * next) {
    struct AstArgList * x = NEW(AstArgList, OBJTYPE_ASTARGLIST);
    x->arg = arg;
    x->next = next;
    return x;
}

struct AstUnpack * newAstUnpack(HashSymbol * symbol, struct AstArgList * argList) {
    struct AstUnpack * x = NEW(AstUnpack, OBJTYPE_ASTUNPACK);
    x->symbol = symbol;
    x->argList = argList;
    return x;
}

struct AstArgPair * newAstArgPair(struct AstArg * car, struct AstArg * cdr) {
    struct AstArgPair * x = NEW(AstArgPair, OBJTYPE_ASTARGPAIR);
    x->car = car;
    x->cdr = cdr;
    return x;
}

struct AstNamedArg * newAstNamedArg(HashSymbol * name, struct AstArg * arg) {
    struct AstNamedArg * x = NEW(AstNamedArg, OBJTYPE_ASTNAMEDARG);
    x->name = name;
    x->arg = arg;
    return x;
}

struct AstEnvType * newAstEnvType(HashSymbol * name, HashSymbol * prototype) {
    struct AstEnvType * x = NEW(AstEnvType, OBJTYPE_ASTENVTYPE);
    x->name = name;
    x->prototype = prototype;
    return x;
}

struct AstFunCall * newAstFunCall(struct AstExpression * function, struct AstExpressions * arguments) {
    struct AstFunCall * x = NEW(AstFunCall, OBJTYPE_ASTFUNCALL);
    x->function = function;
    x->arguments = arguments;
    return x;
}

struct AstPackage * newAstPackage(HashSymbol * symbol, struct AstPackage * next) {
    struct AstPackage * x = NEW(AstPackage, OBJTYPE_ASTPACKAGE);
    x->symbol = symbol;
    x->next = next;
    return x;
}

struct AstExpressions * newAstExpressions(struct AstExpression * expression, struct AstExpressions * next) {
    struct AstExpressions * x = NEW(AstExpressions, OBJTYPE_ASTEXPRESSIONS);
    x->expression = expression;
    x->next = next;
    return x;
}

struct AstEnv * newAstEnv(struct AstPackage * package, struct AstDefinitions * definitions) {
    struct AstEnv * x = NEW(AstEnv, OBJTYPE_ASTENV);
    x->package = package;
    x->definitions = definitions;
    return x;
}

struct AstString * newAstString(char * string) {
    struct AstString * x = NEW(AstString, OBJTYPE_ASTSTRING);
    x->string = string;
    return x;
}

struct AstDefinition * newAstDefinition(enum AstDefinitionType  type, union AstDefinitionVal  val) {
    struct AstDefinition * x = NEW(AstDefinition, OBJTYPE_ASTDEFINITION);
    x->type = type;
    x->val = val;
    return x;
}

struct AstSinglePrototype * newAstSinglePrototype(enum AstSinglePrototypeType  type, union AstSinglePrototypeVal  val) {
    struct AstSinglePrototype * x = NEW(AstSinglePrototype, OBJTYPE_ASTSINGLEPROTOTYPE);
    x->type = type;
    x->val = val;
    return x;
}

struct AstTypeClause * newAstTypeClause(enum AstTypeClauseType  type, union AstTypeClauseVal  val) {
    struct AstTypeClause * x = NEW(AstTypeClause, OBJTYPE_ASTTYPECLAUSE);
    x->type = type;
    x->val = val;
    return x;
}

struct AstArg * newAstArg(enum AstArgType  type, union AstArgVal  val) {
    struct AstArg * x = NEW(AstArg, OBJTYPE_ASTARG);
    x->type = type;
    x->val = val;
    return x;
}

struct AstExpression * newAstExpression(enum AstExpressionType  type, union AstExpressionVal  val) {
    struct AstExpression * x = NEW(AstExpression, OBJTYPE_ASTEXPRESSION);
    x->type = type;
    x->val = val;
    return x;
}


/************************************/

void markAstNest(struct AstNest * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstDefinitions(x->definitions);
    markAstExpressions(x->expressions);
}

void markAstDefinitions(struct AstDefinitions * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstDefinition(x->definition);
    markAstDefinitions(x->next);
}

void markAstDefine(struct AstDefine * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpression(x->expression);
}

void markAstPrototype(struct AstPrototype * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstPrototypeBody(x->body);
}

void markAstPrototypeBody(struct AstPrototypeBody * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstSinglePrototype(x->single);
    markAstPrototypeBody(x->next);
}

void markAstPrototypeSymbolType(struct AstPrototypeSymbolType * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstType(x->type);
}

void markAstLoad(struct AstLoad * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstPackage(x->package);
}

void markAstTypeDef(struct AstTypeDef * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstFlatType(x->flatType);
    markAstTypeBody(x->typeBody);
}

void markAstFlatType(struct AstFlatType * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstTypeSymbols(x->typeSymbols);
}

void markAstTypeSymbols(struct AstTypeSymbols * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstTypeSymbols(x->next);
}

void markAstTypeBody(struct AstTypeBody * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstTypeConstructor(x->typeConstructor);
    markAstTypeBody(x->next);
}

void markAstTypeConstructor(struct AstTypeConstructor * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstTypeList(x->typeList);
}

void markAstTypeList(struct AstTypeList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstType(x->type);
    markAstTypeList(x->next);
}

void markAstType(struct AstType * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstTypeClause(x->typeClause);
    markAstType(x->next);
}

void markAstConditional(struct AstConditional * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpression(x->expression);
    markAstNest(x->consequent);
    markAstNest(x->alternative);
}

void markAstCompositeFunction(struct AstCompositeFunction * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstFunction(x->function);
    markAstCompositeFunction(x->next);
}

void markAstFunction(struct AstFunction * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstArgList(x->argList);
    markAstNest(x->nest);
}

void markAstArgList(struct AstArgList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstArg(x->arg);
    markAstArgList(x->next);
}

void markAstUnpack(struct AstUnpack * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstArgList(x->argList);
}

void markAstArgPair(struct AstArgPair * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstArg(x->car);
    markAstArg(x->cdr);
}

void markAstNamedArg(struct AstNamedArg * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstArg(x->arg);
}

void markAstEnvType(struct AstEnvType * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void markAstFunCall(struct AstFunCall * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpression(x->function);
    markAstExpressions(x->arguments);
}

void markAstPackage(struct AstPackage * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstPackage(x->next);
}

void markAstExpressions(struct AstExpressions * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstExpression(x->expression);
    markAstExpressions(x->next);
}

void markAstEnv(struct AstEnv * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAstPackage(x->package);
    markAstDefinitions(x->definitions);
}

void markAstString(struct AstString * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void markAstDefinition(struct AstDefinition * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case AST_DEFINITION_TYPE_DEFINE:
            markAstDefine(x->val.define);
            break;
        case AST_DEFINITION_TYPE_PROTOTYPE:
            markAstPrototype(x->val.prototype);
            break;
        case AST_DEFINITION_TYPE_LOAD:
            markAstLoad(x->val.load);
            break;
        case AST_DEFINITION_TYPE_TYPEDEF:
            markAstTypeDef(x->val.typeDef);
            break;
        default:
            cant_happen("unrecognised type %d in markAstDefinition", x->type);
    }
}

void markAstSinglePrototype(struct AstSinglePrototype * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE:
            markAstPrototypeSymbolType(x->val.symbolType);
            break;
        case AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE:
            markAstPrototype(x->val.prototype);
            break;
        default:
            cant_happen("unrecognised type %d in markAstSinglePrototype", x->type);
    }
}

void markAstTypeClause(struct AstTypeClause * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case AST_TYPECLAUSE_TYPE_INTEGER:
            break;
        case AST_TYPECLAUSE_TYPE_CHARACTER:
            break;
        case AST_TYPECLAUSE_TYPE_BOOLEAN:
            break;
        case AST_TYPECLAUSE_TYPE_STRING:
            break;
        case AST_TYPECLAUSE_TYPE_LIST:
            markAstType(x->val.list);
            break;
        case AST_TYPECLAUSE_TYPE_TYPE:
            markAstType(x->val.type);
            break;
        case AST_TYPECLAUSE_TYPE_VAR:
            break;
        case AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR:
            markAstTypeConstructor(x->val.typeConstructor);
            break;
        default:
            cant_happen("unrecognised type %d in markAstTypeClause", x->type);
    }
}

void markAstArg(struct AstArg * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case AST_ARG_TYPE_WILDCARD:
            break;
        case AST_ARG_TYPE_SYMBOL:
            break;
        case AST_ARG_TYPE_NAMED:
            markAstNamedArg(x->val.named);
            break;
        case AST_ARG_TYPE_ENV:
            markAstEnvType(x->val.env);
            break;
        case AST_ARG_TYPE_UNPACK:
            markAstUnpack(x->val.unpack);
            break;
        case AST_ARG_TYPE_NUMBER:
            break;
        case AST_ARG_TYPE_STRING:
            markAstString(x->val.string);
            break;
        case AST_ARG_TYPE_CHARACTER:
            break;
        case AST_ARG_TYPE_YES:
            break;
        case AST_ARG_TYPE_NO:
            break;
        default:
            cant_happen("unrecognised type %d in markAstArg", x->type);
    }
}

void markAstExpression(struct AstExpression * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case AST_EXPRESSION_TYPE_NIL:
            break;
        case AST_EXPRESSION_TYPE_BACK:
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            markAstFunCall(x->val.funCall);
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            break;
        case AST_EXPRESSION_TYPE_NUMBER:
            break;
        case AST_EXPRESSION_TYPE_STRING:
            markAstString(x->val.string);
            break;
        case AST_EXPRESSION_TYPE_CHARACTER:
            break;
        case AST_EXPRESSION_TYPE_YES:
            break;
        case AST_EXPRESSION_TYPE_NO:
            break;
        case AST_EXPRESSION_TYPE_FUN:
            markAstCompositeFunction(x->val.fun);
            break;
        case AST_EXPRESSION_TYPE_ENV:
            markAstEnv(x->val.env);
            break;
        case AST_EXPRESSION_TYPE_CONDITIONAL:
            markAstConditional(x->val.conditional);
            break;
        case AST_EXPRESSION_TYPE_NEST:
            markAstNest(x->val.nest);
            break;
        default:
            cant_happen("unrecognised type %d in markAstExpression", x->type);
    }
}


void markAstObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_ASTNEST:
            markAstNest((AstNest *)h);
            break;
        case OBJTYPE_ASTDEFINITIONS:
            markAstDefinitions((AstDefinitions *)h);
            break;
        case OBJTYPE_ASTDEFINE:
            markAstDefine((AstDefine *)h);
            break;
        case OBJTYPE_ASTPROTOTYPE:
            markAstPrototype((AstPrototype *)h);
            break;
        case OBJTYPE_ASTPROTOTYPEBODY:
            markAstPrototypeBody((AstPrototypeBody *)h);
            break;
        case OBJTYPE_ASTPROTOTYPESYMBOLTYPE:
            markAstPrototypeSymbolType((AstPrototypeSymbolType *)h);
            break;
        case OBJTYPE_ASTLOAD:
            markAstLoad((AstLoad *)h);
            break;
        case OBJTYPE_ASTTYPEDEF:
            markAstTypeDef((AstTypeDef *)h);
            break;
        case OBJTYPE_ASTFLATTYPE:
            markAstFlatType((AstFlatType *)h);
            break;
        case OBJTYPE_ASTTYPESYMBOLS:
            markAstTypeSymbols((AstTypeSymbols *)h);
            break;
        case OBJTYPE_ASTTYPEBODY:
            markAstTypeBody((AstTypeBody *)h);
            break;
        case OBJTYPE_ASTTYPECONSTRUCTOR:
            markAstTypeConstructor((AstTypeConstructor *)h);
            break;
        case OBJTYPE_ASTTYPELIST:
            markAstTypeList((AstTypeList *)h);
            break;
        case OBJTYPE_ASTTYPE:
            markAstType((AstType *)h);
            break;
        case OBJTYPE_ASTCONDITIONAL:
            markAstConditional((AstConditional *)h);
            break;
        case OBJTYPE_ASTCOMPOSITEFUNCTION:
            markAstCompositeFunction((AstCompositeFunction *)h);
            break;
        case OBJTYPE_ASTFUNCTION:
            markAstFunction((AstFunction *)h);
            break;
        case OBJTYPE_ASTARGLIST:
            markAstArgList((AstArgList *)h);
            break;
        case OBJTYPE_ASTUNPACK:
            markAstUnpack((AstUnpack *)h);
            break;
        case OBJTYPE_ASTARGPAIR:
            markAstArgPair((AstArgPair *)h);
            break;
        case OBJTYPE_ASTNAMEDARG:
            markAstNamedArg((AstNamedArg *)h);
            break;
        case OBJTYPE_ASTENVTYPE:
            markAstEnvType((AstEnvType *)h);
            break;
        case OBJTYPE_ASTFUNCALL:
            markAstFunCall((AstFunCall *)h);
            break;
        case OBJTYPE_ASTPACKAGE:
            markAstPackage((AstPackage *)h);
            break;
        case OBJTYPE_ASTEXPRESSIONS:
            markAstExpressions((AstExpressions *)h);
            break;
        case OBJTYPE_ASTENV:
            markAstEnv((AstEnv *)h);
            break;
        case OBJTYPE_ASTSTRING:
            markAstString((AstString *)h);
            break;
        case OBJTYPE_ASTDEFINITION:
            markAstDefinition((AstDefinition *)h);
            break;
        case OBJTYPE_ASTSINGLEPROTOTYPE:
            markAstSinglePrototype((AstSinglePrototype *)h);
            break;
        case OBJTYPE_ASTTYPECLAUSE:
            markAstTypeClause((AstTypeClause *)h);
            break;
        case OBJTYPE_ASTARG:
            markAstArg((AstArg *)h);
            break;
        case OBJTYPE_ASTEXPRESSION:
            markAstExpression((AstExpression *)h);
            break;
    }
}

/************************************/

void freeAstNest(struct AstNest * x) {
    FREE(x, AstNest);
}

void freeAstDefinitions(struct AstDefinitions * x) {
    FREE(x, AstDefinitions);
}

void freeAstDefine(struct AstDefine * x) {
    FREE(x, AstDefine);
}

void freeAstPrototype(struct AstPrototype * x) {
    FREE(x, AstPrototype);
}

void freeAstPrototypeBody(struct AstPrototypeBody * x) {
    FREE(x, AstPrototypeBody);
}

void freeAstPrototypeSymbolType(struct AstPrototypeSymbolType * x) {
    FREE(x, AstPrototypeSymbolType);
}

void freeAstLoad(struct AstLoad * x) {
    FREE(x, AstLoad);
}

void freeAstTypeDef(struct AstTypeDef * x) {
    FREE(x, AstTypeDef);
}

void freeAstFlatType(struct AstFlatType * x) {
    FREE(x, AstFlatType);
}

void freeAstTypeSymbols(struct AstTypeSymbols * x) {
    FREE(x, AstTypeSymbols);
}

void freeAstTypeBody(struct AstTypeBody * x) {
    FREE(x, AstTypeBody);
}

void freeAstTypeConstructor(struct AstTypeConstructor * x) {
    FREE(x, AstTypeConstructor);
}

void freeAstTypeList(struct AstTypeList * x) {
    FREE(x, AstTypeList);
}

void freeAstType(struct AstType * x) {
    FREE(x, AstType);
}

void freeAstConditional(struct AstConditional * x) {
    FREE(x, AstConditional);
}

void freeAstCompositeFunction(struct AstCompositeFunction * x) {
    FREE(x, AstCompositeFunction);
}

void freeAstFunction(struct AstFunction * x) {
    FREE(x, AstFunction);
}

void freeAstArgList(struct AstArgList * x) {
    FREE(x, AstArgList);
}

void freeAstUnpack(struct AstUnpack * x) {
    FREE(x, AstUnpack);
}

void freeAstArgPair(struct AstArgPair * x) {
    FREE(x, AstArgPair);
}

void freeAstNamedArg(struct AstNamedArg * x) {
    FREE(x, AstNamedArg);
}

void freeAstEnvType(struct AstEnvType * x) {
    FREE(x, AstEnvType);
}

void freeAstFunCall(struct AstFunCall * x) {
    FREE(x, AstFunCall);
}

void freeAstPackage(struct AstPackage * x) {
    FREE(x, AstPackage);
}

void freeAstExpressions(struct AstExpressions * x) {
    FREE(x, AstExpressions);
}

void freeAstEnv(struct AstEnv * x) {
    FREE(x, AstEnv);
}

void freeAstString(struct AstString * x) {
    FREE(x, AstString);
}

void freeAstDefinition(struct AstDefinition * x) {
    FREE(x, AstDefinition);
}

void freeAstSinglePrototype(struct AstSinglePrototype * x) {
    FREE(x, AstSinglePrototype);
}

void freeAstTypeClause(struct AstTypeClause * x) {
    FREE(x, AstTypeClause);
}

void freeAstArg(struct AstArg * x) {
    FREE(x, AstArg);
}

void freeAstExpression(struct AstExpression * x) {
    FREE(x, AstExpression);
}


void freeAstObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_ASTNEST:
            freeAstNest((AstNest *)h);
            break;
        case OBJTYPE_ASTDEFINITIONS:
            freeAstDefinitions((AstDefinitions *)h);
            break;
        case OBJTYPE_ASTDEFINE:
            freeAstDefine((AstDefine *)h);
            break;
        case OBJTYPE_ASTPROTOTYPE:
            freeAstPrototype((AstPrototype *)h);
            break;
        case OBJTYPE_ASTPROTOTYPEBODY:
            freeAstPrototypeBody((AstPrototypeBody *)h);
            break;
        case OBJTYPE_ASTPROTOTYPESYMBOLTYPE:
            freeAstPrototypeSymbolType((AstPrototypeSymbolType *)h);
            break;
        case OBJTYPE_ASTLOAD:
            freeAstLoad((AstLoad *)h);
            break;
        case OBJTYPE_ASTTYPEDEF:
            freeAstTypeDef((AstTypeDef *)h);
            break;
        case OBJTYPE_ASTFLATTYPE:
            freeAstFlatType((AstFlatType *)h);
            break;
        case OBJTYPE_ASTTYPESYMBOLS:
            freeAstTypeSymbols((AstTypeSymbols *)h);
            break;
        case OBJTYPE_ASTTYPEBODY:
            freeAstTypeBody((AstTypeBody *)h);
            break;
        case OBJTYPE_ASTTYPECONSTRUCTOR:
            freeAstTypeConstructor((AstTypeConstructor *)h);
            break;
        case OBJTYPE_ASTTYPELIST:
            freeAstTypeList((AstTypeList *)h);
            break;
        case OBJTYPE_ASTTYPE:
            freeAstType((AstType *)h);
            break;
        case OBJTYPE_ASTCONDITIONAL:
            freeAstConditional((AstConditional *)h);
            break;
        case OBJTYPE_ASTCOMPOSITEFUNCTION:
            freeAstCompositeFunction((AstCompositeFunction *)h);
            break;
        case OBJTYPE_ASTFUNCTION:
            freeAstFunction((AstFunction *)h);
            break;
        case OBJTYPE_ASTARGLIST:
            freeAstArgList((AstArgList *)h);
            break;
        case OBJTYPE_ASTUNPACK:
            freeAstUnpack((AstUnpack *)h);
            break;
        case OBJTYPE_ASTARGPAIR:
            freeAstArgPair((AstArgPair *)h);
            break;
        case OBJTYPE_ASTNAMEDARG:
            freeAstNamedArg((AstNamedArg *)h);
            break;
        case OBJTYPE_ASTENVTYPE:
            freeAstEnvType((AstEnvType *)h);
            break;
        case OBJTYPE_ASTFUNCALL:
            freeAstFunCall((AstFunCall *)h);
            break;
        case OBJTYPE_ASTPACKAGE:
            freeAstPackage((AstPackage *)h);
            break;
        case OBJTYPE_ASTEXPRESSIONS:
            freeAstExpressions((AstExpressions *)h);
            break;
        case OBJTYPE_ASTENV:
            freeAstEnv((AstEnv *)h);
            break;
        case OBJTYPE_ASTSTRING:
            freeAstString((AstString *)h);
            break;
        case OBJTYPE_ASTDEFINITION:
            freeAstDefinition((AstDefinition *)h);
            break;
        case OBJTYPE_ASTSINGLEPROTOTYPE:
            freeAstSinglePrototype((AstSinglePrototype *)h);
            break;
        case OBJTYPE_ASTTYPECLAUSE:
            freeAstTypeClause((AstTypeClause *)h);
            break;
        case OBJTYPE_ASTARG:
            freeAstArg((AstArg *)h);
            break;
        case OBJTYPE_ASTEXPRESSION:
            freeAstExpression((AstExpression *)h);
            break;
    }
}

char *typenameAstObj(int type) {
    switch(type) {
        case OBJTYPE_ASTNEST:
            return "AstNest";
        case OBJTYPE_ASTDEFINITIONS:
            return "AstDefinitions";
        case OBJTYPE_ASTDEFINE:
            return "AstDefine";
        case OBJTYPE_ASTPROTOTYPE:
            return "AstPrototype";
        case OBJTYPE_ASTPROTOTYPEBODY:
            return "AstPrototypeBody";
        case OBJTYPE_ASTPROTOTYPESYMBOLTYPE:
            return "AstPrototypeSymbolType";
        case OBJTYPE_ASTLOAD:
            return "AstLoad";
        case OBJTYPE_ASTTYPEDEF:
            return "AstTypeDef";
        case OBJTYPE_ASTFLATTYPE:
            return "AstFlatType";
        case OBJTYPE_ASTTYPESYMBOLS:
            return "AstTypeSymbols";
        case OBJTYPE_ASTTYPEBODY:
            return "AstTypeBody";
        case OBJTYPE_ASTTYPECONSTRUCTOR:
            return "AstTypeConstructor";
        case OBJTYPE_ASTTYPELIST:
            return "AstTypeList";
        case OBJTYPE_ASTTYPE:
            return "AstType";
        case OBJTYPE_ASTCONDITIONAL:
            return "AstConditional";
        case OBJTYPE_ASTCOMPOSITEFUNCTION:
            return "AstCompositeFunction";
        case OBJTYPE_ASTFUNCTION:
            return "AstFunction";
        case OBJTYPE_ASTARGLIST:
            return "AstArgList";
        case OBJTYPE_ASTUNPACK:
            return "AstUnpack";
        case OBJTYPE_ASTARGPAIR:
            return "AstArgPair";
        case OBJTYPE_ASTNAMEDARG:
            return "AstNamedArg";
        case OBJTYPE_ASTENVTYPE:
            return "AstEnvType";
        case OBJTYPE_ASTFUNCALL:
            return "AstFunCall";
        case OBJTYPE_ASTPACKAGE:
            return "AstPackage";
        case OBJTYPE_ASTEXPRESSIONS:
            return "AstExpressions";
        case OBJTYPE_ASTENV:
            return "AstEnv";
        case OBJTYPE_ASTSTRING:
            return "AstString";
        case OBJTYPE_ASTDEFINITION:
            return "AstDefinition";
        case OBJTYPE_ASTSINGLEPROTOTYPE:
            return "AstSinglePrototype";
        case OBJTYPE_ASTTYPECLAUSE:
            return "AstTypeClause";
        case OBJTYPE_ASTARG:
            return "AstArg";
        case OBJTYPE_ASTEXPRESSION:
            return "AstExpression";
    }
}

