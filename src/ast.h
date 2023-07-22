#ifndef cekf_ast_h
#define cekf_ast_h
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



#include "hash.h"
#include "memory.h"

typedef enum AstDefinitionType {
    AST_DEFINITION_TYPE_DEFINE,
    AST_DEFINITION_TYPE_PROTOTYPE,
    AST_DEFINITION_TYPE_LOAD,
    AST_DEFINITION_TYPE_TYPEDEF,
} AstDefinitionType;

typedef enum AstSinglePrototypeType {
    AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE,
    AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE,
} AstSinglePrototypeType;

typedef enum AstTypeClauseType {
    AST_TYPECLAUSE_TYPE_INTEGER,
    AST_TYPECLAUSE_TYPE_CHARACTER,
    AST_TYPECLAUSE_TYPE_TYPE,
    AST_TYPECLAUSE_TYPE_VAR,
    AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR,
} AstTypeClauseType;

typedef enum AstArgType {
    AST_ARG_TYPE_WILDCARD,
    AST_ARG_TYPE_SYMBOL,
    AST_ARG_TYPE_NAMED,
    AST_ARG_TYPE_ENV,
    AST_ARG_TYPE_UNPACK,
    AST_ARG_TYPE_NUMBER,
    AST_ARG_TYPE_CHARACTER,
} AstArgType;

typedef enum AstExpressionType {
    AST_EXPRESSION_TYPE_NIL,
    AST_EXPRESSION_TYPE_BACK,
    AST_EXPRESSION_TYPE_FUNCALL,
    AST_EXPRESSION_TYPE_SYMBOL,
    AST_EXPRESSION_TYPE_NUMBER,
    AST_EXPRESSION_TYPE_CHARACTER,
    AST_EXPRESSION_TYPE_FUN,
    AST_EXPRESSION_TYPE_ENV,
    AST_EXPRESSION_TYPE_NEST,
} AstExpressionType;



typedef union AstDefinitionVal {
    struct AstDefine * define;
    struct AstPrototype * prototype;
    struct AstLoad * load;
    struct AstTypeDef * typeDef;
} AstDefinitionVal;

typedef union AstSinglePrototypeVal {
    struct AstPrototypeSymbolType * symbolType;
    struct AstPrototype * prototype;
} AstSinglePrototypeVal;

typedef union AstTypeClauseVal {
    void * integer;
    void * character;
    struct AstType * type;
    HashSymbol * var;
    struct AstTypeConstructor * typeConstructor;
} AstTypeClauseVal;

typedef union AstArgVal {
    void * wildcard;
    HashSymbol * symbol;
    struct AstNamedArg * named;
    struct AstEnvType * env;
    struct AstUnpack * unpack;
    int number;
    char character;
} AstArgVal;

typedef union AstExpressionVal {
    void * nil;
    void * back;
    struct AstFunCall * funCall;
    HashSymbol * symbol;
    int number;
    char character;
    struct AstCompositeFunction * fun;
    struct AstEnv * env;
    struct AstNest * nest;
} AstExpressionVal;



typedef struct AstNest {
    Header header;
    struct AstDefinitions * definitions;
    struct AstExpressions * expressions;
} AstNest;

typedef struct AstDefinitions {
    Header header;
    struct AstDefinition * definition;
    struct AstDefinitions * next;
} AstDefinitions;

typedef struct AstDefine {
    Header header;
    HashSymbol * symbol;
    struct AstExpression * expression;
} AstDefine;

typedef struct AstPrototype {
    Header header;
    HashSymbol * symbol;
    struct AstPrototypeBody * body;
} AstPrototype;

typedef struct AstPrototypeBody {
    Header header;
    struct AstSinglePrototype * single;
    struct AstPrototypeBody * next;
} AstPrototypeBody;

typedef struct AstPrototypeSymbolType {
    Header header;
    HashSymbol * symbol;
    struct AstType * type;
} AstPrototypeSymbolType;

typedef struct AstLoad {
    Header header;
    struct AstPackage * package;
    HashSymbol * symbol;
} AstLoad;

typedef struct AstTypeDef {
    Header header;
    struct AstFlatType * flatType;
    struct AstTypeBody * typeBody;
} AstTypeDef;

typedef struct AstFlatType {
    Header header;
    HashSymbol * symbol;
    struct AstTypeSymbols * typeSymbols;
} AstFlatType;

typedef struct AstTypeSymbols {
    Header header;
    HashSymbol * typeSymbol;
    struct AstTypeSymbols * next;
} AstTypeSymbols;

typedef struct AstTypeBody {
    Header header;
    struct AstTypeConstructor * typeConstructor;
    struct AstTypeBody * next;
} AstTypeBody;

typedef struct AstTypeConstructor {
    Header header;
    HashSymbol * symbol;
    struct AstTypeList * typeList;
} AstTypeConstructor;

typedef struct AstTypeList {
    Header header;
    struct AstType * type;
    struct AstTypeList * next;
} AstTypeList;

typedef struct AstType {
    Header header;
    struct AstTypeClause * typeClause;
    struct AstType * next;
} AstType;

typedef struct AstCompositeFunction {
    Header header;
    struct AstFunction * function;
    struct AstCompositeFunction * next;
} AstCompositeFunction;

typedef struct AstFunction {
    Header header;
    struct AstArgList * argList;
    struct AstNest * nest;
} AstFunction;

typedef struct AstArgList {
    Header header;
    struct AstArg * arg;
    struct AstArgList * next;
} AstArgList;

typedef struct AstUnpack {
    Header header;
    HashSymbol * symbol;
    struct AstArgList * argList;
} AstUnpack;

typedef struct AstNamedArg {
    Header header;
    HashSymbol * name;
    struct AstArg * arg;
} AstNamedArg;

typedef struct AstEnvType {
    Header header;
    HashSymbol * name;
    HashSymbol * prototype;
} AstEnvType;

typedef struct AstFunCall {
    Header header;
    struct AstExpression * function;
    struct AstExpressions * arguments;
} AstFunCall;

typedef struct AstPackage {
    Header header;
    HashSymbol * symbol;
    struct AstPackage * next;
} AstPackage;

typedef struct AstExpressions {
    Header header;
    struct AstExpression * expression;
    struct AstExpressions * next;
} AstExpressions;

typedef struct AstEnv {
    Header header;
    struct AstPackage * package;
    struct AstDefinitions * definitions;
} AstEnv;

typedef struct AstDefinition {
    Header header;
    enum AstDefinitionType  type;
    union AstDefinitionVal  val;
} AstDefinition;

typedef struct AstSinglePrototype {
    Header header;
    enum AstSinglePrototypeType  type;
    union AstSinglePrototypeVal  val;
} AstSinglePrototype;

typedef struct AstTypeClause {
    Header header;
    enum AstTypeClauseType  type;
    union AstTypeClauseVal  val;
} AstTypeClause;

typedef struct AstArg {
    Header header;
    enum AstArgType  type;
    union AstArgVal  val;
} AstArg;

typedef struct AstExpression {
    Header header;
    enum AstExpressionType  type;
    union AstExpressionVal  val;
} AstExpression;

struct AstNest * newAstNest(struct AstDefinitions * definitions, struct AstExpressions * expressions);
struct AstDefinitions * newAstDefinitions(struct AstDefinition * definition, struct AstDefinitions * next);
struct AstDefine * newAstDefine(HashSymbol * symbol, struct AstExpression * expression);
struct AstPrototype * newAstPrototype(HashSymbol * symbol, struct AstPrototypeBody * body);
struct AstPrototypeBody * newAstPrototypeBody(struct AstSinglePrototype * single, struct AstPrototypeBody * next);
struct AstPrototypeSymbolType * newAstPrototypeSymbolType(HashSymbol * symbol, struct AstType * type);
struct AstLoad * newAstLoad(struct AstPackage * package, HashSymbol * symbol);
struct AstTypeDef * newAstTypeDef(struct AstFlatType * flatType, struct AstTypeBody * typeBody);
struct AstFlatType * newAstFlatType(HashSymbol * symbol, struct AstTypeSymbols * typeSymbols);
struct AstTypeSymbols * newAstTypeSymbols(HashSymbol * typeSymbol, struct AstTypeSymbols * next);
struct AstTypeBody * newAstTypeBody(struct AstTypeConstructor * typeConstructor, struct AstTypeBody * next);
struct AstTypeConstructor * newAstTypeConstructor(HashSymbol * symbol, struct AstTypeList * typeList);
struct AstTypeList * newAstTypeList(struct AstType * type, struct AstTypeList * next);
struct AstType * newAstType(struct AstTypeClause * typeClause, struct AstType * next);
struct AstCompositeFunction * newAstCompositeFunction(struct AstFunction * function, struct AstCompositeFunction * next);
struct AstFunction * newAstFunction(struct AstArgList * argList, struct AstNest * nest);
struct AstArgList * newAstArgList(struct AstArg * arg, struct AstArgList * next);
struct AstUnpack * newAstUnpack(HashSymbol * symbol, struct AstArgList * argList);
struct AstNamedArg * newAstNamedArg(HashSymbol * name, struct AstArg * arg);
struct AstEnvType * newAstEnvType(HashSymbol * name, HashSymbol * prototype);
struct AstFunCall * newAstFunCall(struct AstExpression * function, struct AstExpressions * arguments);
struct AstPackage * newAstPackage(HashSymbol * symbol, struct AstPackage * next);
struct AstExpressions * newAstExpressions(struct AstExpression * expression, struct AstExpressions * next);
struct AstEnv * newAstEnv(struct AstPackage * package, struct AstDefinitions * definitions);
struct AstDefinition * newAstDefinition(enum AstDefinitionType  type, union AstDefinitionVal  val);
struct AstSinglePrototype * newAstSinglePrototype(enum AstSinglePrototypeType  type, union AstSinglePrototypeVal  val);
struct AstTypeClause * newAstTypeClause(enum AstTypeClauseType  type, union AstTypeClauseVal  val);
struct AstArg * newAstArg(enum AstArgType  type, union AstArgVal  val);
struct AstExpression * newAstExpression(enum AstExpressionType  type, union AstExpressionVal  val);

void markAstNest(struct AstNest * x);
void markAstDefinitions(struct AstDefinitions * x);
void markAstDefine(struct AstDefine * x);
void markAstPrototype(struct AstPrototype * x);
void markAstPrototypeBody(struct AstPrototypeBody * x);
void markAstPrototypeSymbolType(struct AstPrototypeSymbolType * x);
void markAstLoad(struct AstLoad * x);
void markAstTypeDef(struct AstTypeDef * x);
void markAstFlatType(struct AstFlatType * x);
void markAstTypeSymbols(struct AstTypeSymbols * x);
void markAstTypeBody(struct AstTypeBody * x);
void markAstTypeConstructor(struct AstTypeConstructor * x);
void markAstTypeList(struct AstTypeList * x);
void markAstType(struct AstType * x);
void markAstCompositeFunction(struct AstCompositeFunction * x);
void markAstFunction(struct AstFunction * x);
void markAstArgList(struct AstArgList * x);
void markAstUnpack(struct AstUnpack * x);
void markAstNamedArg(struct AstNamedArg * x);
void markAstEnvType(struct AstEnvType * x);
void markAstFunCall(struct AstFunCall * x);
void markAstPackage(struct AstPackage * x);
void markAstExpressions(struct AstExpressions * x);
void markAstEnv(struct AstEnv * x);
void markAstDefinition(struct AstDefinition * x);
void markAstSinglePrototype(struct AstSinglePrototype * x);
void markAstTypeClause(struct AstTypeClause * x);
void markAstArg(struct AstArg * x);
void markAstExpression(struct AstExpression * x);

void freeAstNest(struct AstNest * x);
void freeAstDefinitions(struct AstDefinitions * x);
void freeAstDefine(struct AstDefine * x);
void freeAstPrototype(struct AstPrototype * x);
void freeAstPrototypeBody(struct AstPrototypeBody * x);
void freeAstPrototypeSymbolType(struct AstPrototypeSymbolType * x);
void freeAstLoad(struct AstLoad * x);
void freeAstTypeDef(struct AstTypeDef * x);
void freeAstFlatType(struct AstFlatType * x);
void freeAstTypeSymbols(struct AstTypeSymbols * x);
void freeAstTypeBody(struct AstTypeBody * x);
void freeAstTypeConstructor(struct AstTypeConstructor * x);
void freeAstTypeList(struct AstTypeList * x);
void freeAstType(struct AstType * x);
void freeAstCompositeFunction(struct AstCompositeFunction * x);
void freeAstFunction(struct AstFunction * x);
void freeAstArgList(struct AstArgList * x);
void freeAstUnpack(struct AstUnpack * x);
void freeAstNamedArg(struct AstNamedArg * x);
void freeAstEnvType(struct AstEnvType * x);
void freeAstFunCall(struct AstFunCall * x);
void freeAstPackage(struct AstPackage * x);
void freeAstExpressions(struct AstExpressions * x);
void freeAstEnv(struct AstEnv * x);
void freeAstDefinition(struct AstDefinition * x);
void freeAstSinglePrototype(struct AstSinglePrototype * x);
void freeAstTypeClause(struct AstTypeClause * x);
void freeAstArg(struct AstArg * x);
void freeAstExpression(struct AstExpression * x);

#define AST_DEFINITION_VAL_DEFINE(x) ((union AstDefinitionVal ){.define = (x)})
#define AST_DEFINITION_VAL_PROTOTYPE(x) ((union AstDefinitionVal ){.prototype = (x)})
#define AST_DEFINITION_VAL_LOAD(x) ((union AstDefinitionVal ){.load = (x)})
#define AST_DEFINITION_VAL_TYPEDEF(x) ((union AstDefinitionVal ){.typeDef = (x)})
#define AST_SINGLEPROTOTYPE_VAL_SYMBOLTYPE(x) ((union AstSinglePrototypeVal ){.symbolType = (x)})
#define AST_SINGLEPROTOTYPE_VAL_PROTOTYPE(x) ((union AstSinglePrototypeVal ){.prototype = (x)})
#define AST_TYPECLAUSE_VAL_INTEGER() ((union AstTypeClauseVal ){.integer = (NULL)})
#define AST_TYPECLAUSE_VAL_CHARACTER() ((union AstTypeClauseVal ){.character = (NULL)})
#define AST_TYPECLAUSE_VAL_TYPE(x) ((union AstTypeClauseVal ){.type = (x)})
#define AST_TYPECLAUSE_VAL_VAR(x) ((union AstTypeClauseVal ){.var = (x)})
#define AST_TYPECLAUSE_VAL_TYPECONSTRUCTOR(x) ((union AstTypeClauseVal ){.typeConstructor = (x)})
#define AST_ARG_VAL_WILDCARD() ((union AstArgVal ){.wildcard = (NULL)})
#define AST_ARG_VAL_SYMBOL(x) ((union AstArgVal ){.symbol = (x)})
#define AST_ARG_VAL_NAMED(x) ((union AstArgVal ){.named = (x)})
#define AST_ARG_VAL_ENV(x) ((union AstArgVal ){.env = (x)})
#define AST_ARG_VAL_UNPACK(x) ((union AstArgVal ){.unpack = (x)})
#define AST_ARG_VAL_NUMBER(x) ((union AstArgVal ){.number = (x)})
#define AST_ARG_VAL_CHARACTER(x) ((union AstArgVal ){.character = (x)})
#define AST_EXPRESSION_VAL_NIL() ((union AstExpressionVal ){.nil = (NULL)})
#define AST_EXPRESSION_VAL_BACK() ((union AstExpressionVal ){.back = (NULL)})
#define AST_EXPRESSION_VAL_FUNCALL(x) ((union AstExpressionVal ){.funCall = (x)})
#define AST_EXPRESSION_VAL_SYMBOL(x) ((union AstExpressionVal ){.symbol = (x)})
#define AST_EXPRESSION_VAL_NUMBER(x) ((union AstExpressionVal ){.number = (x)})
#define AST_EXPRESSION_VAL_CHARACTER(x) ((union AstExpressionVal ){.character = (x)})
#define AST_EXPRESSION_VAL_FUN(x) ((union AstExpressionVal ){.fun = (x)})
#define AST_EXPRESSION_VAL_ENV(x) ((union AstExpressionVal ){.env = (x)})
#define AST_EXPRESSION_VAL_NEST(x) ((union AstExpressionVal ){.nest = (x)})

#endif
