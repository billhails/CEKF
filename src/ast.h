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

#include <stdbool.h>

#include "hash.h"
#include "memory.h"

/* structures generated directly by the parser
 */

typedef struct AstNest {
    struct Header header;
    struct AstDefinitions *definitions;
    struct AstExpressions *expressions;
} AstNest;


/** definitions */

typedef struct AstDefinitions {
    struct Header header;
    struct AstDefinitions *next;
    struct AstDefinition *definition;
} AstDefinitions;

typedef enum AstDefinitionType {
    AST_DEFINITION_TYPE_DEFINE,
    AST_DEFINITION_TYPE_PROTOTYPE,
    AST_DEFINITION_TYPE_LOAD,
    AST_DEFINITION_TYPE_TYPEDEF
} AstDefinitionType;

typedef union AstDefinitionValue {
    struct AstDefine *define;
    struct AstPrototype *prototype;
    struct AstLoad *load;
    struct AstTypeDef *typeDef;
} AstDefinitionValue;

typedef struct AstDefinition {
    struct Header header;
    AstDefinitionType type;
    AstDefinitionValue val;
} AstDefinition;

#define AST_DEFINITION_VAL_DEFINE(x) ((AstDefinitionValue){.define = (x)})
#define AST_DEFINITION_VAL_PROTOTYPE(x) ((AstDefinitionValue){.prototype = (x)})
#define AST_DEFINITION_VAL_LOAD(x) ((AstDefinitionValue){.load = (x)})
#define AST_DEFINITION_VAL_TYPEDEF(x) ((AstDefinitionValue){.typeDef = (x)})

typedef struct AstDefine {
    struct Header header;
    struct AstSymbol *symbol;
    struct AstExpression *expression;
} AstDefine;

typedef struct AstPrototype {
    struct Header header;
    struct AstSymbol *symbol;
    struct AstPrototypeBody *body;
} AstPrototype;

typedef struct AstPrototypeBody {
    struct Header header;
    struct AstPrototypeBody *next;
    struct AstSinglePrototype *single;
} AstPrototypeBody;

typedef enum AstSinglePrototypeType {
    AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE,
    AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE,
} AstSinglePrototypeType;

typedef union AstSinglePrototypeValue {
    struct AstPrototypeSymbolType *symbolType;
    struct AstPrototype *prototype;
} AstSinglePrototypeValue;

typedef struct AstSinglePrototype {
    struct Header header;
    AstSinglePrototypeType type;
    AstSinglePrototypeValue val;
} AstSinglePrototype;

#define AST_SINGLEPROTOTYPE_VAL_SYMBOLTYPE(x) ((AstSinglePrototypeValue){.symbolType = (x)})
#define AST_SINGLEPROTOTYPE_VAL_PROTOTYPE(x) ((AstSinglePrototypeValue){.prototype = (x)})

typedef struct AstPrototypeSymbolType {
    struct AstSymbol *symbol;
    struct AstType *type;
} AstPrototypeSymbolType;

typedef struct AstLoad {
    struct Header header;
    struct AstPackage *package;
    struct AstSymbol *symbol;
} AstLoad;

typedef struct AstTypeDef {
    struct Header header;
    struct AstFlatType *flatType;
    struct AstTypeBody *typeBody;
} AstTypeDef;

typedef struct AstFlatType {
    struct Header header;
    struct AstSymbol *symbol;
    struct AstTypeSymbols *typeSymbols;
} AstFlatType;

typedef struct AstTypeSymbols {
    struct Header header;
    struct AstTypeSymbols *next;
    struct AstSymbol *typeSymbol;
} AstTypeSymbols;

typedef struct AstTypeBody {
    struct Header header;
    struct AstTypeBody *next;
    struct AstTypeConstructor *typeConstructor;
} AstTypeBody;

typedef struct AstTypeConstructor {
    struct Header header;
    struct AstSymbol *symbol;
    struct AstTypeList *typeList;
} AstTypeConstructor;

typedef struct AstTypeList {
    struct Header header;
    struct AstTypeList *next;
    struct AstType *type;
} AstTypeList;

typedef struct AstType {
    struct Header header;
    struct AstType *next; // for functions
    struct AstTypeClause *typeClause;
} AstType;

typedef enum AstTypeClauseType {
    AST_TYPECLAUSE_TYPE_LIST,
    AST_TYPECLAUSE_TYPE_INT,
    AST_TYPECLAUSE_TYPE_CHAR,
    AST_TYPECLAUSE_TYPE_BOOL,
    AST_TYPECLAUSE_TYPE_STRING,
    AST_TYPECLAUSE_TYPE_VAR,
    AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR,
    AST_TYPECLAUSE_TYPE_TYPE,
} AstTypeClauseType;

typedef union AstTypeClauseValue {
    void *none;
    struct AstType *type;
    struct AstSymbol *typeSymbol;
    struct AstTypeConstructor *constructor;
} AstTypeClauseValue;

typedef struct AstTypeClause {
    struct Header header;
    AstTypeClauseType type;
    AstTypeClauseValue val;
} AstTypeClause;

#define AST_TYPECLAUSE_VAL_LIST(x) ((AstTypeClauseValue){.type = (x)})
#define AST_TYPECLAUSE_VAL_INT() ((AstTypeClauseValue){.none = NULL})
#define AST_TYPECLAUSE_VAL_CHAR() ((AstTypeClauseValue){.none = NULL})
#define AST_TYPECLAUSE_VAL_BOOL() ((AstTypeClauseValue){.none = NULL})
#define AST_TYPECLAUSE_VAL_STRING() ((AstTypeClauseValue){.none = NULL})
#define AST_TYPECLAUSE_VAL_VAR(x) ((AstTypeClauseValue){.typeSymbol = (x)})
#define AST_TYPECLAUSE_VAL_TYPECONSTRUCTOR(x) ((AstTypeClauseValue){.constructor = (x)})
#define AST_TYPECLAUSE_VAL_TYPE(x) ((AstTypeClauseValue){.type = (x)})

/** expressions */

typedef struct AstConditional {
    struct Header header;
    struct AstExpression *expression;
    struct AstNest *consequent;
    struct AstNest *alternative;
} AstConditional;

typedef struct AstSwitch {
    struct Header header;
    struct AstExpressions *expressions;
    struct AstCompositeFunction *compositeFunction;
} AstSwitch;

typedef enum AstFunType {
    AST_FUN_TYPE_FUNCTION,
    AST_FUN_TYPE_COMPOSITEFUNCTION,
} AstFunType;

typedef union AstFunValue {
    struct AstFunction *function;
    struct AstCompositeFunction *compositeFunction;
} AstFunValue;

typedef struct AstFun {
    struct Header header;
    AstFunType type;
    AstFunValue val;
} AstFun;

#define AST_FUN_VAL_FUNCTION(x) ((AstFunValue){.function = (x)})
#define AST_FUN_VAL_COMPOSITEFUNCTION(x) ((AstFunValue){.compositeFunction = (x)})

typedef struct AstCompositeFunction {
    struct Header header;
    struct AstCompositeFunction *next;
    struct AstFunction *function;
} AstCompositeFunction;

typedef struct AstFunction {
    struct Header header;
    struct AstArgList *argList;
    struct AstNest *nest;
} AstFunction;

typedef struct AstArgList {
    struct Header header;
    struct AstArgList *next;
    struct AstArg *arg;
} AstArgList;

typedef enum AstArgType {
    AST_ARG_TYPE_SYMBOL,
    AST_ARG_TYPE_CONS,
    AST_ARG_TYPE_NAMED,
    AST_ARG_TYPE_LIST,
    AST_ARG_TYPE_ENV,
    AST_ARG_TYPE_NUMBER,
    AST_ARG_TYPE_STRING,
    AST_ARG_TYPE_CHAR,
    AST_ARG_TYPE_TRUE,
    AST_ARG_TYPE_FALSE,
    AST_ARG_TYPE_WILDCARD,
    AST_ARG_TYPE_UNPACK,
} AstArgType;

typedef union AstArgValue {
    void *none;
    struct AstSymbol *symbol;
    struct AstArgPair *cons;
    struct AstNamedArg *named;
    struct AstArgList *list;
    struct AstEnvType *env;
    struct AstUnpack *unpack;
    int number;
    struct AstString *string;
    char character;
    bool boolean;

} AstArgValue;

typedef struct AstArg {
    struct Header header;
    AstArgType type;
    AstArgValue val;
} AstArg;

#define AST_ARG_VAL_SYMBOL(x) ((AstArgValue){.symbol = (x)})
#define AST_ARG_VAL_CONS(x) ((AstArgValue){.cons = (x)})
#define AST_ARG_VAL_NAMED(x) ((AstArgValue){.named = (x)})
#define AST_ARG_VAL_LIST(x) ((AstArgValue){.list = (x)})
#define AST_ARG_VAL_ENV(x) ((AstArgValue){.env = (x)})
#define AST_ARG_VAL_NUMBER(x) ((AstArgValue){.number = (x)})
#define AST_ARG_VAL_STRING(x) ((AstArgValue){.string = (x)})
#define AST_ARG_VAL_CHAR(x) ((AstArgValue){.character = (x)})
#define AST_ARG_VAL_UNPACK(x) ((AstArgValue){.unpack = (x)})
#define AST_ARG_VAL_TRUE() ((AstArgValue){.boolean = true})
#define AST_ARG_VAL_FALSE() ((AstArgValue){.boolean = false})
#define AST_ARG_VAL_WILDCARD() ((AstArgValue){.none = NULL})

typedef struct AstUnpack {
    struct Header header;
    struct AstSymbol *symbol;
    struct AstArgList *argList;
} AstUnpack;

typedef struct AstArgPair {
    struct Header header;
    struct AstArg *car;
    struct AstArg *cdr;
} AstArgPair;

typedef struct AstNamedArg {
    struct Header header;
    struct AstSymbol *name;
    struct AstArg *arg;
} AstNamedArg;

typedef struct AstEnvType {
    struct Header header;
    struct AstSymbol *name;
    struct AstSymbol *prototype;
} AstEnvType;

typedef enum AstExpressionType {
    AST_EXPRESSION_TYPE_BINOP,
    AST_EXPRESSION_TYPE_NOT,
    AST_EXPRESSION_TYPE_NEGATE,
    AST_EXPRESSION_TYPE_FUNCALL,
    AST_EXPRESSION_TYPE_SYMBOL,
    AST_EXPRESSION_TYPE_NUMBER,
    AST_EXPRESSION_TYPE_STRING,
    AST_EXPRESSION_TYPE_CHAR,
    AST_EXPRESSION_TYPE_TRUE,
    AST_EXPRESSION_TYPE_FALSE,
    AST_EXPRESSION_TYPE_LIST,
    AST_EXPRESSION_TYPE_FUN,
    AST_EXPRESSION_TYPE_ENV,
    AST_EXPRESSION_TYPE_BACK,
    AST_EXPRESSION_TYPE_CONDITIONAL,
    AST_EXPRESSION_TYPE_SWITCH,
    AST_EXPRESSION_TYPE_HERE,
} AstExpressionType;

typedef union AstExpressionValue {
    void *none;
    struct AstBinOp *binOp;
    struct AstExpression *expression;
    struct AstFunCall *funCall;
    struct AstSymbol *symbol;
    int number;
    struct AstString *string;
    char character;
    bool boolean;
    struct AstExpressions *list;
    struct AstFun *fun;
    struct AstEnv *env;
    struct AstConditional *conditional;
    struct AstSwitch *switchStatement;
} AstExpressionValue;

typedef struct AstExpression {
    struct Header header;
    AstExpressionType type;
    AstExpressionValue val;
} AstExpression;

#define AST_EXPRESSION_VAL_BINOP(x) ((AstExpressionValue){.binOp = (x)})
#define AST_EXPRESSION_VAL_NOT(x) ((AstExpressionValue){.expression = (x)})
#define AST_EXPRESSION_VAL_NEGATE(x) ((AstExpressionValue){.expression = (x)})
#define AST_EXPRESSION_VAL_FUNCALL(x) ((AstExpressionValue){.funCall = (x)})
#define AST_EXPRESSION_VAL_SYMBOL(x) ((AstExpressionValue){.symbol = (x)})
#define AST_EXPRESSION_VAL_NUMBER(x) ((AstExpressionValue){.number = (x)})
#define AST_EXPRESSION_VAL_STRING(x) ((AstExpressionValue){.string = (x)})
#define AST_EXPRESSION_VAL_CHAR(x) ((AstExpressionValue){.character = (x)})
#define AST_EXPRESSION_VAL_TRUE() ((AstExpressionValue){.boolean = true})
#define AST_EXPRESSION_VAL_FALSE() ((AstExpressionValue){.binOp = false})
#define AST_EXPRESSION_VAL_LIST(x) ((AstExpressionValue){.list = (x)})
#define AST_EXPRESSION_VAL_FUN(x) ((AstExpressionValue){.fun = (x)})
#define AST_EXPRESSION_VAL_ENV(x) ((AstExpressionValue){.env = (x)})
#define AST_EXPRESSION_VAL_BACK() ((AstExpressionValue){.none = NULL})
#define AST_EXPRESSION_VAL_CONDITIONAL(x) ((AstExpressionValue){.conditional = (x)})
#define AST_EXPRESSION_VAL_SWITCH(x) ((AstExpressionValue){.switchStatement = (x)})
#define AST_EXPRESSION_VAL_HERE(x) ((AstExpressionValue){.expression = (x)})

typedef enum AstBinOpType {
    AST_BINOP_TYPE_THEN,
    AST_BINOP_TYPE_AND,
    AST_BINOP_TYPE_OR,
    AST_BINOP_TYPE_XOR,
    AST_BINOP_TYPE_EQ,
    AST_BINOP_TYPE_NE,
    AST_BINOP_TYPE_LT,
    AST_BINOP_TYPE_GT,
    AST_BINOP_TYPE_LE,
    AST_BINOP_TYPE_GE,
    AST_BINOP_TYPE_CONS,
    AST_BINOP_TYPE_APPEND,
    AST_BINOP_TYPE_ADD,
    AST_BINOP_TYPE_SUB,
    AST_BINOP_TYPE_MUL,
    AST_BINOP_TYPE_DIV,
    AST_BINOP_TYPE_MOD,
    AST_BINOP_TYPE_POW,
    AST_BINOP_TYPE_DOT,
} AstBinOpType;

typedef struct AstBinOp {
    AstBinOpType type;
    struct AstExpression *lhs;
    struct AstExpression *rhs;
} AstBinOp;

typedef struct AstFunCall {
    struct Header header;
    struct AstExpression *function;
    struct AstExpressions *arguments;
} AstFunCall;

typedef struct AstPackage {
    struct Header header;
    struct AstPackage *next;
    struct AstSymbol *symbol;
} AstPackage;

typedef struct AstExpressions {
    struct Header header;
    struct AstExpressions *next;
    struct AstExpression *expression;
} AstExpressions;

typedef struct AstEnv {
    struct Header header;
    struct AstPackage *package;
    struct AstDefinitions *definitions;
} AstEnv;

typedef enum AstSymbolType {
    AST_SYMBOL_TYPE_SYMBOL,
    AST_SYMBOL_TYPE_TYPESYMBOL,
} AstSymbolType;

typedef struct AstSymbol {
    struct Header header;
    AstSymbolType type;
    hash_t hash;
    char *name;
} AstSymbol;

typedef struct AstString {
    char *string;
} AstString;

/********************************************/

AstArgList *newAstArgList(AstArgList *next, AstArg *arg);
AstArg *newAstArg(AstArgType type, AstArgValue val);
AstArgPair *newAstArgPair(AstArg *car, AstArg *cdr);
AstBinOp *newAstBinOp(AstBinOpType type, AstExpression *lhs, AstExpression *rhs);
AstCompositeFunction *newAstCompositeFunction(AstCompositeFunction *next, AstFunction *function);
AstConditional *newAstConditional(AstExpression *expression, AstNest *consequent, AstNest *alternative);
AstDefine *newAstDefine(AstSymbol *symbol, AstExpression *expression);
AstDefinition *newAstDefinition(AstDefinitionType type, AstDefinitionValue val);
AstDefinitions *newAstDefinitions(AstDefinitions *next, AstDefinition *definition);
AstEnv *newAstEnv(AstPackage *package, AstDefinitions *definitions);
AstEnvType *newAstEnvType(AstSymbol *name, AstSymbol *prototype);
AstExpression *newAstExpression(AstExpressionType type, AstExpressionValue val);
AstExpressions *newAstExpressions(AstExpressions *next, AstExpression *expression);
AstFlatType *newAstFlatType(AstSymbol *symbol, AstTypeSymbols *typeSymbols);
AstFunCall *newAstFunCall(AstExpression *function, AstExpressions *arguments);
AstFunction *newAstFunction(AstArgList *argList, AstNest *nest);
AstFun *newAstFun(AstFunType type, AstFunValue val);
AstLoad *newAstLoad(AstPackage *package, AstSymbol *symbol);
AstNamedArg *newAstNamedArg(AstSymbol *name, AstArg *arg);
AstNest *newAstNest(AstDefinitions *definitions, AstExpressions *expressions);
AstPackage *newAstPackage(AstPackage *next, AstSymbol *symbol);
AstPrototypeBody *newAstPrototypeBody(AstPrototypeBody *next, AstSinglePrototype *single);
AstPrototype *newAstPrototype(AstSymbol *symbol, AstPrototypeBody *body);
AstPrototypeSymbolType *newAstPrototypeSymbolType(AstSymbol *symbol, AstType *type);
AstSinglePrototype *newAstSinglePrototype(AstSinglePrototypeType type, AstSinglePrototypeValue val);
AstString *newAstString(char *string);
AstSwitch *newAstSwitch(AstExpressions *expressions, AstCompositeFunction *compositeFunction);
AstSymbol *newAstSymbol(AstSymbolType type, char *name);
AstTypeBody *newAstTypeBody(AstTypeBody *next, AstTypeConstructor *typeConstructor);
AstTypeClause *newAstTypeClause(AstTypeClauseType type, AstTypeClauseValue val);
AstTypeConstructor *newAstTypeConstructor(AstSymbol *symbol, AstTypeList *typeList);
AstTypeDef *newAstTypeDef(AstFlatType *flatType, AstTypeBody *typeBody);
AstTypeList *newAstTypeList(AstTypeList *next, AstType *type);
AstType *newAstType(AstType *next, AstTypeClause *typeClause);
AstTypeSymbols *newAstTypeSymbols(AstTypeSymbols *next, AstSymbol *typeSymbol);
AstUnpack *newAstUnpack(AstSymbol *symbol, AstArgList *argList);

void markAstArgList(AstArgList *x);
void markAstArg(AstArg *x);
void markAstArgPair(AstArgPair *x);
void markAstBinOp(AstBinOp *x);
void markAstCompositeFunction(AstCompositeFunction *x);
void markAstConditional(AstConditional *x);
void markAstDefine(AstDefine *x);
void markAstDefinition(AstDefinition *x);
void markAstDefinitions(AstDefinitions *x);
void markAstEnv(AstEnv *x);
void markAstEnvType(AstEnvType *x);
void markAstExpression(AstExpression *x);
void markAstExpressions(AstExpressions *x);
void markAstFlatType(AstFlatType *x);
void markAstFunCall(AstFunCall *x);
void markAstFunction(AstFunction *x);
void markAstFun(AstFun *x);
void markAstLoad(AstLoad *x);
void markAstNamedArg(AstNamedArg *x);
void markAstNest(AstNest *x);
void markAstPackage(AstPackage *x);
void markAstPrototypeBody(AstPrototypeBody *x);
void markAstPrototype(AstPrototype *x);
void markAstPrototypeSymbolType(AstPrototypeSymbolType *x);
void markAstSinglePrototype(AstSinglePrototype *x);
void markAstString(AstString *x);
void markAstSwitch(AstSwitch *x);
void markAstSymbol(AstSymbol *x);
void markAstTypeBody(AstTypeBody *x);
void markAstTypeClause(AstTypeClause *x);
void markAstTypeConstructor(AstTypeConstructor *x);
void markAstTypeDef(AstTypeDef *x);
void markAstTypeList(AstTypeList *x);
void markAstType(AstType *x);
void markAstTypeSymbols(AstTypeSymbols *x);
void markAstUnpack(AstUnpack *x);

#endif
