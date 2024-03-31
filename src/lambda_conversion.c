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

// conversion of the AST generated by the parser
// to an intermediate "plain" lambda calculus which
// will then be fed into the type checker and the
// A-Normal Form converter.

#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "lambda_conversion.h"
#include "lambda_helper.h"
#include "symbols.h"
#include "tpmc_logic.h"
#include "tpmc_mermaid.h"
#include "ast_debug.h"
#include "print_generator.h"

static LamLetRecBindings *convertFuncDefs(AstDefinitions *definitions,
                                          LamContext *env);
static LamList *convertExpressions(AstExpressions *expressions,
                                   LamContext *env);
static LamSequence *convertSequence(AstExpressions *expressions,
                                    LamContext *env);
static LamLetRecBindings *prependDefinition(AstDefinition *definition,
                                            LamContext *env,
                                            LamLetRecBindings *next);
static LamLetRecBindings *prependDefine(AstDefine *define, LamContext *env,
                                        LamLetRecBindings *next);
static LamExp *convertExpression(AstExpression *expression, LamContext *env);
static bool typeHasFields(AstTypeBody *typeBody);
static LamTypeDefList *collectTypeDefs(AstDefinitions *definitions,
                                       LamContext *env);
static LamTypeConstructor *collectTypeConstructor(AstTypeConstructor
                                                  *typeConstructor,
                                                  LamType *type, int size,
                                                  int index, bool needsVec,
                                                  LamContext *env);
static void collectTypeInfo(HashSymbol *symbol, LamTypeConstructor *type,
                            bool needsVec, int enumCount, int index,
                            int arity, LamContext *env);
static LamTypeConstructorArgs *convertAstTypeList(AstTypeList *typeList);
static HashSymbol *dollarSubstitute(HashSymbol *original);

#ifdef DEBUG_LAMBDA_CONVERT
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static bool inPreamble = true;  // preamble is treated specially
static bool preambleLocked = false;

LamExp *lamConvertNest(AstNest *nest, LamContext *env) {
    ENTER(lamConvertNest);
    bool hasLock = inPreamble && !preambleLocked;
    if (hasLock)
        preambleLocked = true;
    env = extendLamContext(env);
    int save = PROTECT(env);
    LamTypeDefList *typeDefList = collectTypeDefs(nest->definitions, env);
    (void) PROTECT(typeDefList);
    LamLetRecBindings *funcDefsList = convertFuncDefs(nest->definitions, env);
    PROTECT(funcDefsList);
    funcDefsList =
        makePrintFunctions(typeDefList, funcDefsList, env, inPreamble);
    PROTECT(funcDefsList);
    if (hasLock)
        inPreamble = false;
    LamSequence *body = convertSequence(nest->expressions, env);
    if (hasLock)
        inPreamble = true;
    (void) PROTECT(body);
    LamExp *letRecBody = newLamExp(LAMEXP_TYPE_LIST, LAMEXP_VAL_LIST(body));
    (void) PROTECT(letRecBody);
    LamExp *result = NULL;
    if (funcDefsList != NULL) {
        LamLetRec *letRec =
            newLamLetRec(countLamLetRecBindings(funcDefsList), funcDefsList,
                         letRecBody);
        (void) PROTECT(letRec);
        result = newLamExp(LAMEXP_TYPE_LETREC, LAMEXP_VAL_LETREC(letRec));
    } else {
        result = newLamExp(LAMEXP_TYPE_LIST, LAMEXP_VAL_LIST(body));
    }
    (void) PROTECT(result);
    if (typeDefList != NULL) {
        LamTypeDefs *typeDefs = newLamTypeDefs(typeDefList, result);
        (void) PROTECT(typeDefs);
        result =
            newLamExp(LAMEXP_TYPE_TYPEDEFS, LAMEXP_VAL_TYPEDEFS(typeDefs));
    }
    if (hasLock)
        preambleLocked = false;
    UNPROTECT(save);
    LEAVE(lamConvertNest);
    return result;
}

static LamExp *lamConvertIff(AstIff *iff, LamContext *context) {
    ENTER(lamConvertIff);
    LamExp *test = convertExpression(iff->test, context);
    int save = PROTECT(test);
    LamExp *consequent = lamConvertNest(iff->consequent, context);
    PROTECT(consequent);
    LamExp *alternative = lamConvertNest(iff->alternative, context);
    PROTECT(alternative);
    LamIff *lamIff = newLamIff(test, consequent, alternative);
    PROTECT(lamIff);
    LamExp *result = newLamExp(LAMEXP_TYPE_IFF, LAMEXP_VAL_IFF(lamIff));
    UNPROTECT(save);
    LEAVE(lamConvertIff);
    return result;
}

static LamExp *lamConvertPrint(AstPrint *print, LamContext *context) {
    ENTER(lamConvertPrint);
    LamExp *exp = convertExpression(print->exp, context);
    int save = PROTECT(exp);
    LamPrint *lamPrint = newLamPrint(exp);
    PROTECT(lamPrint);
    LamExp *result = newLamExp(LAMEXP_TYPE_PRINT, LAMEXP_VAL_PRINT(lamPrint));
    UNPROTECT(save);
    LEAVE(lamConvertPrint);
    return result;
}

static LamLetRecBindings *convertFuncDefs(AstDefinitions *definitions,
                                          LamContext *env) {
    ENTER(convertFuncDefs);
    if (definitions == NULL) {
        return NULL;
    }
    LamLetRecBindings *next = convertFuncDefs(definitions->next, env);
    int save = PROTECT(next);
    LamLetRecBindings *this =
        prependDefinition(definitions->definition, env, next);
    UNPROTECT(save);
    LEAVE(convertFuncDefs);
    return this;
}

static LamTypeArgs *convertTypeSymbols(AstTypeSymbols *symbols) {
    if (symbols == NULL)
        return NULL;
    LamTypeArgs *next = convertTypeSymbols(symbols->next);
    int save = PROTECT(next);
    LamTypeArgs *this = newLamTypeArgs(symbols->typeSymbol, next);
    UNPROTECT(save);
    return this;
}

static LamType *convertUserType(AstUserType *userType) {
    LamTypeArgs *args = convertTypeSymbols(userType->typeSymbols);
    int save = PROTECT(args);
    LamType *res = newLamType(userType->symbol, args);
    UNPROTECT(save);
    return res;
}

static LamTypeFunction *convertAstTypeFunction(AstTypeFunction
                                               *astTypeFunction) {
    LamTypeConstructorArgs *lamTypeConstructorArgs =
        convertAstTypeList(astTypeFunction->typeList);
    int save = PROTECT(lamTypeConstructorArgs);
    LamTypeFunction *this =
        newLamTypeFunction(astTypeFunction->symbol, lamTypeConstructorArgs);
    UNPROTECT(save);
    return this;
}

static LamTypeConstructorType *convertAstTypeClause(AstTypeClause
                                                    *astTypeClause) {
    switch (astTypeClause->type) {
        case AST_TYPECLAUSE_TYPE_INTEGER:
            return
                newLamTypeConstructorType(LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER,
                                          LAMTYPECONSTRUCTORTYPE_VAL_INTEGER
                                          ());
            break;
        case AST_TYPECLAUSE_TYPE_CHARACTER:
            return
                newLamTypeConstructorType
                (LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER,
                 LAMTYPECONSTRUCTORTYPE_VAL_CHARACTER());
            break;
        case AST_TYPECLAUSE_TYPE_VAR:
            return newLamTypeConstructorType(LAMTYPECONSTRUCTORTYPE_TYPE_VAR,
                                             LAMTYPECONSTRUCTORTYPE_VAL_VAR
                                             (astTypeClause->val.var));
            break;
        case AST_TYPECLAUSE_TYPE_TYPEFUNCTION:{
                LamTypeFunction *lamTypeFunction =
                    convertAstTypeFunction(astTypeClause->val.typeFunction);
                int save = PROTECT(lamTypeFunction);
                LamTypeConstructorType *this =
                    newLamTypeConstructorType
                    (LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION,
                     LAMTYPECONSTRUCTORTYPE_VAL_FUNCTION(lamTypeFunction)
                    );
                UNPROTECT(save);
                return this;
            }
            break;
        default:
            cant_happen
                ("unrecognised astTypeClause type %d in convertAstTypeClause",
                 astTypeClause->type);
    }
}

static LamTypeFunction *makeArrow(LamTypeConstructorType *lhs,
                                  LamTypeConstructorType *rhs) {
    LamTypeConstructorArgs *rhsArg = newLamTypeConstructorArgs(rhs, NULL);
    int save = PROTECT(rhsArg);
    LamTypeConstructorArgs *args = newLamTypeConstructorArgs(lhs, rhsArg);
    PROTECT(args);
    LamTypeFunction *res = newLamTypeFunction(arrowSymbol(), args);
    UNPROTECT(save);
    return res;
}

static LamTypeConstructorType *convertAstType(AstType *astType) {
    if (astType->next) {        // it's a function
        LamTypeConstructorType *next = convertAstType(astType->next);
        int save = PROTECT(next);
        LamTypeConstructorType *this =
            convertAstTypeClause(astType->typeClause);
        PROTECT(this);
        LamTypeFunction *arrow = makeArrow(this, next);
        PROTECT(arrow);
        LamTypeConstructorType *res =
            newLamTypeConstructorType(LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION,
                                      LAMTYPECONSTRUCTORTYPE_VAL_FUNCTION
                                      (arrow)
            );
        UNPROTECT(save);
        return res;
    } else {
        return convertAstTypeClause(astType->typeClause);
    }
}

static LamTypeConstructorArgs *convertAstTypeList(AstTypeList *typeList) {
    if (typeList == NULL)
        return NULL;
    LamTypeConstructorArgs *next = convertAstTypeList(typeList->next);
    int save = PROTECT(next);
    LamTypeConstructorType *arg = convertAstType(typeList->type);
    PROTECT(arg);
    LamTypeConstructorArgs *this = newLamTypeConstructorArgs(arg, next);
    UNPROTECT(save);
    return this;
}

static void collectTypeInfo(HashSymbol *symbol, LamTypeConstructor *type,
                            bool needsVec, int enumCount, int index,
                            int arity, LamContext *env) {
    ENTER(collectTypeInfo);
    LamTypeConstructorInfo *info =
        newLamTypeConstructorInfo(type, needsVec, arity, enumCount,
                                  index);
    int save = PROTECT(info);
    addToLamContext(env, symbol, info);
    UNPROTECT(save);
    LEAVE(collectTypeInfo);
}

static LamTypeConstructor *collectTypeConstructor(AstTypeConstructor
                                                  *typeConstructor,
                                                  LamType *type,
                                                  int enumCount, int index,
                                                  bool needsVec,
                                                  LamContext *env) {
    int nargs = countAstTypeList(typeConstructor->typeList);
    LamTypeConstructorArgs *args =
        convertAstTypeList(typeConstructor->typeList);
    int save = PROTECT(args);
    LamTypeConstructor *lamTypeConstructor =
        newLamTypeConstructor(typeConstructor->symbol, type, args);
    PROTECT(lamTypeConstructor);
    collectTypeInfo(typeConstructor->symbol, lamTypeConstructor, needsVec,
                    enumCount, index, nargs, env);
    UNPROTECT(save);
    return lamTypeConstructor;
}

static LamTypeDef *collectTypeDef(AstTypeDef *typeDef, LamContext *env) {
    LamType *type = convertUserType(typeDef->userType);
    int save = PROTECT(type);
    AstTypeBody *typeBody = typeDef->typeBody;
    bool needsVec = typeHasFields(typeBody);
    int enumCount = countAstTypeBody(typeBody);
    int index = 0;
    LamTypeConstructorList *lamTypeConstructorList = NULL;
    int save2 = PROTECT(type);
    while (typeBody != NULL) {
        LamTypeConstructor *lamTypeConstructor =
            collectTypeConstructor(typeBody->typeConstructor,
                                   type,
                                   enumCount,
                                   index,
                                   needsVec,
                                   env);
        int save3 = PROTECT(lamTypeConstructor);
        lamTypeConstructorList =
            newLamTypeConstructorList(lamTypeConstructor,
                                      lamTypeConstructorList);
        REPLACE_PROTECT(save2, lamTypeConstructorList);
        UNPROTECT(save3);
        typeBody = typeBody->next;
        index++;
    }
    LamTypeDef *res = newLamTypeDef(type, lamTypeConstructorList);
    UNPROTECT(save);
    return res;
}

static LamTypeDefList *collectTypeDefs(AstDefinitions *definitions,
                                       LamContext *env) {
    if (definitions == NULL) {
        return NULL;
    }
    if (definitions->definition->type == AST_DEFINITION_TYPE_TYPEDEF) {
        LamTypeDef *lamTypeDef =
            collectTypeDef(definitions->definition->val.typeDef, env);
        int save = PROTECT(lamTypeDef);
        LamTypeDefList *rest = collectTypeDefs(definitions->next, env);
        PROTECT(rest);
        LamTypeDefList *res = newLamTypeDefList(lamTypeDef, rest);
        UNPROTECT(save);
        return res;
    } else {
        return collectTypeDefs(definitions->next, env);
    }
}

static LamLetRecBindings *prependDefinition(AstDefinition *definition,
                                            LamContext *env,
                                            LamLetRecBindings *next) {
    ENTER(prependDefinition);
    LamLetRecBindings *result = NULL;
    switch (definition->type) {
        case AST_DEFINITION_TYPE_DEFINE:
            result = prependDefine(definition->val.define, env, next);
            break;
        case AST_DEFINITION_TYPE_TYPEDEF:
            result = next;
            break;
        default:
            cant_happen
                ("unrecognised definition type %d in prependDefinition",
                 definition->type);
    }
    LEAVE(prependDefinition);
    return result;
}

static bool typeHasFields(AstTypeBody *typeBody) {
    ENTER(typeHasFields);
    while (typeBody != NULL) {
        if (typeBody->typeConstructor->typeList != NULL) {
            return true;
        }
        typeBody = typeBody->next;
    }
    return false;
}

static LamExp *makeConstruct(HashSymbol *name, int tag, LamList *args) {
    LamConstruct *construct = newLamConstruct(name, tag, args);
    int save = PROTECT(construct);
    LamExp *res =
        newLamExp(LAMEXP_TYPE_CONSTRUCT, LAMEXP_VAL_CONSTRUCT(construct));
    UNPROTECT(save);
    return res;
}

static LamExp *makeConstant(HashSymbol *name, int tag) {
    LamConstant *constant = newLamConstant(name, tag);
    int save = PROTECT(constant);
    LamExp *res =
        newLamExp(LAMEXP_TYPE_CONSTANT, LAMEXP_VAL_CONSTANT(constant));
    UNPROTECT(save);
    return res;
}

static LamLetRecBindings *prependDefine(AstDefine * define, LamContext * env,
                                        LamLetRecBindings * next) {
    ENTER(prependDefine);
    bool doMermaid = (tpmc_mermaid_function != NULL
                      && strcmp(tpmc_mermaid_function,
                                define->symbol->name) == 0);
    if (doMermaid)
        tpmc_mermaid_flag = 1;
    LamExp *exp = convertExpression(define->expression, env);
    if (doMermaid)
        tpmc_mermaid_flag = 0;
    int save = PROTECT(exp);
    LamLetRecBindings *this =
        newLamLetRecBindings(dollarSubstitute(define->symbol), exp, next);
    UNPROTECT(save);
    LEAVE(prependDefine);
    return this;
}

static HashSymbol *dollarSubstitute(HashSymbol *symbol) {
    if (!inPreamble)
        return symbol;
    bool needs_substitution = false;
    for (char *s = symbol->name; *s != 0; s++) {
        if (*s == '_') {
            needs_substitution = true;
            break;
        }
    }
    if (needs_substitution) {
        char *buf = NEW_ARRAY(char, strlen(symbol->name) + 1);
        strcpy(buf, symbol->name);
        for (int i = 0; buf[i] != 0; i++) {
            if (buf[i] == '_') {
                buf[i] = '$';
            }
        }
        HashSymbol *replacement = newSymbol(buf);
        FREE_ARRAY(char, buf, strlen(buf) + 1);
        return replacement;
    } else {
        return symbol;
    }
}

#define CHECK_ONE_ARG(name, args) do { \
    int count = countLamList(args); \
    if (count != 1) \
        cant_happen("expected 1 arg in " #name ", got %d", count); \
} while(0)

#define CHECK_TWO_ARGS(name, args) do { \
    int count = countLamList(args); \
    if (count != 2) \
        cant_happen("expected 2 args in " #name ", got %d", count); \
} while(0)

static LamExp *makeUnaryOp(LamUnaryOp opCode, LamList *args) {
    CHECK_ONE_ARG(makeUnaryOp, args);
    LamUnaryApp *app = newLamUnaryApp(opCode, args->exp);
    int save = PROTECT(app);
    LamExp *exp = newLamExp(LAMEXP_TYPE_UNARY, LAMEXP_VAL_UNARY(app));
    UNPROTECT(save);
    return exp;
}

static LamExp *makeCallCC(LamList *args) {
    CHECK_ONE_ARG(makeCallCC, args);
    return newLamExp(LAMEXP_TYPE_CALLCC, LAMEXP_VAL_CALLCC(args->exp));
}

static LamExp *makeBinOp(LamPrimOp opCode, LamList *args) {
    CHECK_TWO_ARGS(makeBinOp, args);
    LamPrimApp *app = newLamPrimApp(opCode, args->exp, args->next->exp);
    int save = PROTECT(app);
    LamExp *exp = newLamExp(LAMEXP_TYPE_PRIM, LAMEXP_VAL_PRIM(app));
    UNPROTECT(save);
    return exp;
}

static LamExp *makeLamAnd(LamList *args) {
    CHECK_TWO_ARGS(makeLamAnd, args);
    LamAnd *lamAnd = newLamAnd(args->exp, args->next->exp);
    int save = PROTECT(lamAnd);
    LamExp *res = newLamExp(LAMEXP_TYPE_AND, LAMEXP_VAL_AND(lamAnd));
    UNPROTECT(save);
    return res;
}

static LamExp *makeLamOr(LamList *args) {
    CHECK_TWO_ARGS(makeLamOr, args);
    LamOr *lamOr = newLamOr(args->exp, args->next->exp);
    int save = PROTECT(lamOr);
    LamExp *res = newLamExp(LAMEXP_TYPE_OR, LAMEXP_VAL_OR(lamOr));
    UNPROTECT(save);
    return res;
}

static LamExp *makeLamAmb(LamList *args) {
    CHECK_TWO_ARGS(makeLamAmb, args);
    LamAmb *lamAmb = newLamAmb(args->exp, args->next->exp);
    int save = PROTECT(lamAmb);
    LamExp *res = newLamExp(LAMEXP_TYPE_AMB, LAMEXP_VAL_AMB(lamAmb));
    UNPROTECT(save);
    return res;
}

static LamExp *makePrimApp(HashSymbol *symbol, LamList *args) {
    if (symbol == putcSymbol())
        return makeUnaryOp(LAMUNARYOP_TYPE_PUTC, args);
    if (symbol == putnSymbol())
        return makeUnaryOp(LAMUNARYOP_TYPE_PUTN, args);
    if (symbol == putvSymbol())
        return makeUnaryOp(LAMUNARYOP_TYPE_PUTV, args);
    if (symbol == negSymbol())
        return makeUnaryOp(LAMUNARYOP_TYPE_NEG, args);
    if (symbol == notSymbol())
        return makeUnaryOp(LAMUNARYOP_TYPE_NOT, args);
    if (symbol == hereSymbol())
        return makeCallCC(args);
    if (symbol == thenSymbol())
        return makeLamAmb(args);
    if (symbol == andSymbol())
        return makeLamAnd(args);
    if (symbol == orSymbol())
        return makeLamOr(args);
    if (symbol == xorSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_XOR, args);
    if (symbol == eqSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_EQ, args);
    if (symbol == neSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_NE, args);
    if (symbol == gtSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_GT, args);
    if (symbol == ltSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_LT, args);
    if (symbol == geSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_GE, args);
    if (symbol == leSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_LE, args);
    if (symbol == addSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_ADD, args);
    if (symbol == subSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_SUB, args);
    if (symbol == mulSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_MUL, args);
    if (symbol == divSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_DIV, args);
    if (symbol == modSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_MOD, args);
    if (symbol == powSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_POW, args);
    if (symbol == cmpSymbol())
        return makeBinOp(LAMPRIMOP_TYPE_CMP, args);
    return NULL;
}

static LamExp *inlineConstructor(HashSymbol *symbol, LamList *args,
                                 LamContext *env) {
    LamTypeConstructorInfo *info = lookupInLamContext(env, symbol);
    if (info != NULL) {
        int actualNargs = countLamList(args);
        if (info->needsVec) {
            if (actualNargs == info->arity) {
                return makeConstruct(symbol, info->index, args);
            } else {
                cant_happen("wrong number of arguments to constructor %s",
                            symbol->name);
            }
        } else {
            if (actualNargs > 0) {
                cant_happen("arguments to constant constructor %s",
                            symbol->name);
            }
            return makeConstant(symbol, info->index);
        }
    }
    return NULL;
}

static LamExp *convertApplication(AstFunCall *funCall, LamList *args,
                                  LamContext *env) {
    int actualNargs = countAstExpressions(funCall->arguments);
    LamExp *fun = convertExpression(funCall->function, env);
    int save = PROTECT(fun);
    LamApply *apply = newLamApply(fun, actualNargs, args);
    PROTECT(apply);
    LamExp *result = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return result;
}

static LamExp *convertFunCall(AstFunCall *funCall, LamContext *env) {
    LamList *args = convertExpressions(funCall->arguments, env);
    int save = PROTECT(args);
    LamExp *result = NULL;
    if (funCall->function->type == AST_EXPRESSION_TYPE_SYMBOL) {
        HashSymbol *symbol = funCall->function->val.symbol;
        result = makePrimApp(symbol, args);
        if (result != NULL) {
            UNPROTECT(save);
            return result;
        }
        // see if it's a type constructor we can inline
        result = inlineConstructor(symbol, args, env);
        if (result != NULL) {
            UNPROTECT(save);
            return result;
        }
    }
    // otherwise we convert as normal
    result = convertApplication(funCall, args, env);
    UNPROTECT(save);
    return result;
}

static LamLam *convertCompositeBodies(int nargs, AstCompositeFunction *fun,
                                      LamContext *env) {
    ENTER(convertCompositeBodies);
    int nBodies = countAstCompositeFunction(fun);
    if (nBodies == 0) {
        can_happen("empty composite function");
        LEAVE(convertCompositeBodies);
        return NULL;
    }
    LamExp **actions = NEW_ARRAY(LamExp *, nBodies);
    AstArgList **argLists = NEW_ARRAY(AstArgList *, nBodies);
    int p = PROTECT(NULL);
    AstCompositeFunction *f = fun;
    for (int i = 0; i < nBodies; i++, f = f->next) {
        AstFunction *func = f->function;
        actions[i] = lamConvertNest(func->nest, env);
        PROTECT(actions[i]);
        argLists[i] = func->argList;
    }
    LamLam *result = tpmcConvert(nargs, nBodies, argLists, actions, env);
    PROTECT(result);
    FREE_ARRAY(LamExp *, actions, nBodies);
    FREE_ARRAY(AstArgList *, argLists, nBodies);
    UNPROTECT(p);
    LEAVE(convertCompositeBodies);
    return result;
}

static LamExp *convertCompositeFun(AstCompositeFunction *fun, LamContext *env) {
    ENTER(convertCompositeFun);
    if (fun == NULL)
        cant_happen("composite function with no components");
    int nargs = countAstArgList(fun->function->argList);
    LamLam *lambda = convertCompositeBodies(nargs, fun, env);
    DEBUG("convertCompositeBodies returned %p", lambda);
    int save = PROTECT(lambda);
    LamExp *result = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(lambda));
    UNPROTECT(save);
    LEAVE(convertCompositeFun);
    return result;
}

static LamExp *convertSymbol(HashSymbol *symbol, LamContext *env) {
    LamExp *result = inlineConstructor(symbol, NULL, env);
    if (result == NULL) {
        symbol = dollarSubstitute(symbol);
        result = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(symbol));
    }
    return result;
}

static LamExp *convertExpression(AstExpression *expression, LamContext *env) {
    ENTER(convertExpression);
    LamExp *result = NULL;
    switch (expression->type) {
        case AST_EXPRESSION_TYPE_BACK:
            result = newLamExp(LAMEXP_TYPE_BACK, LAMEXP_VAL_BACK());
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            result = convertFunCall(expression->val.funCall, env);
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            result = convertSymbol(expression->val.symbol, env);
            break;
        case AST_EXPRESSION_TYPE_NUMBER:
            result =
                newLamExp(LAMEXP_TYPE_BIGINTEGER,
                          LAMEXP_VAL_BIGINTEGER(expression->val.number));
            break;
        case AST_EXPRESSION_TYPE_CHARACTER:
            result =
                newLamExp(LAMEXP_TYPE_CHARACTER,
                          LAMEXP_VAL_CHARACTER(expression->val.character));
            break;
        case AST_EXPRESSION_TYPE_FUN:
            result = convertCompositeFun(expression->val.fun, env);
            break;
        case AST_EXPRESSION_TYPE_NEST:
            result = lamConvertNest(expression->val.nest, env);
            break;
        case AST_EXPRESSION_TYPE_IFF:
            result = lamConvertIff(expression->val.iff, env);
            break;
        case AST_EXPRESSION_TYPE_PRINT:
            result = lamConvertPrint(expression->val.print, env);
            break;
        default:
            cant_happen
                ("unrecognised expression type %d in convertExpression",
                 expression->type);
    }
    LEAVE(convertExpression);
    return result;
}

static LamList *convertExpressions(AstExpressions *expressions,
                                   LamContext *env) {
    if (expressions == NULL)
        return NULL;
    LamList *next = convertExpressions(expressions->next, env);
    int save = PROTECT(next);
    LamExp *exp = convertExpression(expressions->expression, env);
    (void) PROTECT(exp);
    LamList *this = newLamList(exp, next);
    UNPROTECT(save);
    return this;
}

static LamSequence *convertSequence(AstExpressions *expressions,
                                    LamContext *env) {
    if (expressions == NULL)
        return NULL;
    LamSequence *next = convertSequence(expressions->next, env);
    int save = PROTECT(next);
    LamExp *exp = convertExpression(expressions->expression, env);
    (void) PROTECT(exp);
    LamSequence *this = newLamSequence(exp, next);
    UNPROTECT(save);
    return this;
}
