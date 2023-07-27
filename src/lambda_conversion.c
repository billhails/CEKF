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
#include <stdlib.h>
#include "common.h"
#include "lambda_conversion.h"
#include "lambda_helper.h"
#include "symbol.h"

#define ARG_CATEGORY_VAR 0
#define ARG_CATEGORY_CONST 1
#define ARG_CATEGORY_STRUCT 2
#define ARG_CATEGORY_ENV 3

#define SAFE_MALLOC(thing) ((thing *)safe_malloc(sizeof(thing)))

typedef struct DecisionTree {
    AstArg **currentArgs;
    AstArgList **remainingArgs;
    AstNest **bodies;
    int *nBodies;
    int globalCount;
    int localCount;
} DecisionTree;

static LamLetRecBindings *convertDefinitions(AstDefinitions *definitions, LamContext *env);
static LamList *convertExpressions(AstExpressions *expressions, LamContext *env);
static int countLetRecBindings(LamLetRecBindings *b);
static LamLetRecBindings *prependDefinition(AstDefinition *definition, LamContext *env, LamLetRecBindings *next);
static LamLetRecBindings *prependDefine(AstDefine *define, LamContext *env, LamLetRecBindings *next);
static LamLetRecBindings *prependTypeDef(AstTypeDef *typeDef, LamLetRecBindings *next, LamContext *env);
static LamExp *convertExpression(AstExpression *expression, LamContext *env);
static LamLetRecBindings *prependTypeConstructor(AstTypeConstructor *typeConstructor, int index, bool hasFields, LamLetRecBindings *next, LamContext *env);
static bool typeHasFields(AstTypeBody *typeBody);

#ifdef DEBUG_LAMBDA_CONVERT
static int invocationId = 0;
#define ENTER(name) \
int myId = invocationId++; \
printf("enter " #name " %d\n", myId);
#define LEAVE(name) \
printf("leave " #name " %d\n", myId);
#else
#define ENTER(n)
#define LEAVE(n)
#endif

#define MAKE_COUNT_LIST(type)           \
static int count ## type (type *list) { \
    ENTER(count ## type)                \
    int count = 0;                      \
    while (list != NULL) {              \
        count++;                        \
        list = list->next;              \
    }                                   \
    LEAVE(count ## type)                \
    return count;                       \
}

MAKE_COUNT_LIST(LamLetRecBindings)

MAKE_COUNT_LIST(AstTypeList)

MAKE_COUNT_LIST(AstExpressions)

MAKE_COUNT_LIST(AstArgList)

MAKE_COUNT_LIST(AstCompositeFunction)

LamExp *lamConvertNest(AstNest *nest, LamContext *env) {
    ENTER(lamConvertNest)
    LamContext *ext = extendLamContext(env);
    int save = PROTECT(ext);
    LamLetRecBindings *bindings = convertDefinitions(nest->definitions, ext);
    (void) PROTECT(bindings);
    LamList *body = convertExpressions(nest->expressions, ext);
    (void) PROTECT(body);
    LamLetRec *letRec = newLamLetRec(countLamLetRecBindings(bindings), bindings, body);
    (void) PROTECT(letRec);
    LamExp *result = newLamExp(LAMEXP_TYPE_LETREC, LAMEXP_VAL_LETREC(letRec));
    UNPROTECT(save);
    LEAVE(lamConvertNest)
    return result;
}
 
static LamLetRecBindings *convertDefinitions(AstDefinitions *definitions, LamContext *env) {
    ENTER(convertDefinitions)
    if (definitions == NULL) {
        LEAVE(convertDefinitions)
        return NULL;
    }
    LamLetRecBindings *next = convertDefinitions(definitions->next, env);
    int save = PROTECT(next);
    LamLetRecBindings *this = prependDefinition(definitions->definition, env, next);
    UNPROTECT(save);
    LEAVE(convertDefinitions)
    return this;
}

static LamLetRecBindings *prependDefinition(AstDefinition *definition, LamContext *env, LamLetRecBindings *next) {
    ENTER(prependDefinition)
    LamLetRecBindings *result = NULL;
    switch (definition->type) {
        case AST_DEFINITION_TYPE_DEFINE:
            result = prependDefine(definition->val.define, env, next);
            break;
        case AST_DEFINITION_TYPE_TYPEDEF:
            result = prependTypeDef(definition->val.typeDef, next, env);
            break;
        default:
            cant_happen("unrecognised definition type %d in prependDefinition", definition->type);
    }
    LEAVE(prependDefinition)
    return result;
}

static LamLetRecBindings *prependTypeDef(AstTypeDef *typeDef, LamLetRecBindings *next, LamContext *env) {
    ENTER(prependTypeDef)
    AstTypeBody *typeBody = typeDef->typeBody;
    bool hasFields = typeHasFields(typeBody);
    int index = 0;
    int save = PROTECT(next);
    while (typeBody != NULL) {
        next = prependTypeConstructor(typeBody->typeConstructor, index, hasFields, next, env);
        UNPROTECT(save);
        save = PROTECT(next);
        typeBody = typeBody->next;
        index++;
    }
    UNPROTECT(save);
    LEAVE(prependTypeDef)
    return next;
}

static bool typeHasFields(AstTypeBody *typeBody) {
    ENTER(typeHasFields)
    while (typeBody != NULL) {
        if (typeBody->typeConstructor->typeList != NULL) {
            return true;
        }
        typeBody = typeBody->next;
    }
    return false;
}


static void collectTypeInfo(HashSymbol *symbol, bool vec, int index, int nargs, LamContext *env) {
    ENTER(collectTypeInfo)
    LamTypeConstructorInfo *info = newLamTypeConstructorInfo(vec, nargs, index);
    int save = PROTECT(info);
    addToLamContext(env, symbol, info);
    UNPROTECT(save);
    LEAVE(collectTypeInfo)
}

static LamVarList *genLamVarList(int nargs) {
    if (nargs == 0) return NULL;
    LamVarList *next = genLamVarList(nargs - 1);
    int save = PROTECT(next);
    LamVarList *this = newLamVarList(genSym("$var"), next);
    UNPROTECT(save);
    return this;
}

static LamList *varListToLamList(LamVarList *varList) {
    if (varList == NULL) return NULL;
    LamList *next = varListToLamList(varList->next);
    int save = PROTECT(next);
    LamExp *exp = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(varList->var));
    (void) PROTECT(exp);
    LamList *this = newLamList(exp, next);
    UNPROTECT(save);
    return this;
}

static LamExp *makeMakeVec(int nargs, int index, LamList *args) {
    LamExp *indexExp = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(index));
    int save = PROTECT(indexExp);
    args = newLamList(indexExp, args);
    (void) PROTECT(args);
    LamMakeVec *makeVec = newLamMakeVec(nargs + 1, args);
    (void) PROTECT(makeVec);
    LamExp *exp = newLamExp(LAMEXP_TYPE_MAKEVEC, LAMEXP_VAL_MAKEVEC(makeVec));
    UNPROTECT(save);
    return exp;
}

static LamExp *makeTypeConstructor(int index, int nargs) {
    LamVarList *varList = genLamVarList(nargs);
    int save = PROTECT(varList);
    LamList *args = varListToLamList(varList);
    (void) PROTECT(args);
    LamExp *exp = makeMakeVec(nargs, index, args);
    (void) PROTECT(exp);
    LamLam *lambda = newLamLam(nargs, varList, exp);
    (void) PROTECT(lambda);
    LamExp *result = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(lambda));
    UNPROTECT(save);
    return result;
}

static LamExp *analyzeTypeConstructor(AstTypeConstructor *typeConstructor, int index, bool someoneHasFields, LamContext *env) {
    ENTER(analyzeTypeConstructor)
    int nargs = countAstTypeList(typeConstructor->typeList);
    // we collect info about the type constructor regardless
    collectTypeInfo(typeConstructor->symbol, someoneHasFields, index, nargs, env);
    LamExp *exp = NULL;
    if (nargs > 0) {
        // if this type constructor has args, then we have to create a function
        // that can be called (and curried) but in the simple case the type info
        // will allow us to inline the constructor when encountered in an expression
        exp = makeTypeConstructor(index, nargs);
    } else {
        // otherwise we can treat it as a constant enum
        if (someoneHasFields) {
            // if another constructor for this type has args, then we must create a
            // constant vector, to be consistent with other instances of this type
            exp = makeMakeVec(0, index, NULL);
        } else {
            // if none of the constructors for this type take arguments, then we can
            // treat it as a simple integer enumeration along with the others
            exp = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(index));
        }
    }
    LEAVE(analyzeTypeConstructor)
    return exp;
}

static LamLetRecBindings *prependTypeConstructor(AstTypeConstructor *typeConstructor, int index, bool hasFields, LamLetRecBindings *next, LamContext *env) {
    ENTER(prependTypeConstructor)
    LamExp *exp = analyzeTypeConstructor(typeConstructor, index, hasFields, env);
    if (exp != NULL) {
        int save = PROTECT(exp);
        LamLetRecBindings *current = newLamLetRecBindings(typeConstructor->symbol, exp, next);
        UNPROTECT(save);
        LEAVE(prependTypeConstructor)
        return current;
    } else {
        LEAVE(prependTypeConstructor)
        return next;
    }
}

static LamLetRecBindings *prependDefine(AstDefine *define, LamContext *env, LamLetRecBindings *next) {
    ENTER(prependDefine)
    LamExp *exp = convertExpression(define->expression, env);
    int save = PROTECT(exp);
    LamLetRecBindings *this = newLamLetRecBindings(define->symbol, exp, next);
    UNPROTECT(save);
    LEAVE(prependDefine)
    return this;
}

static LamExp * convertFunCall(AstFunCall *funCall, LamContext *env) {
    AstExpression *function = funCall->function;
    LamList *args = convertExpressions(funCall->arguments, env);
    int actualNargs = countAstExpressions(funCall->arguments);
    int save = PROTECT(args);
    // see if it's a type constructor we can inline
    if (function->type == AST_EXPRESSION_TYPE_SYMBOL) {
        HashSymbol *symbol = function->val.symbol;
        LamTypeConstructorInfo *info = lookupInLamContext(env, symbol);
        if (info != NULL) {
            if (info->nargs > 0) {
                if (actualNargs == info->nargs) {
                    LamExp *inLine = makeMakeVec(info->nargs, info->index, args);
                    UNPROTECT(save);
                    return inLine;
                }
            }
        }
    }
    // otherwise we convert as normal
    LamExp *fun = convertExpression(function, env);
    (void) PROTECT(fun);
    LamApply *apply = newLamApply(fun, actualNargs, args);
    (void) PROTECT(apply);
    LamExp *result = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return result;
}

/*******************************************/

static int categorizeArg(AstArg *arg, LamContext *env) {
    switch (arg->type) {
        case AST_ARG_TYPE_WILDCARD: {
            return ARG_CATEGORY_VAR;
        }
        break;
        case AST_ARG_TYPE_SYMBOL: {
            LamTypeConstructorInfo *info = lookupInLamContext(env, arg->val.symbol);
            if (info != NULL) {
                return ARG_CATEGORY_STRUCT;
            }
            return ARG_CATEGORY_VAR;
        }
        break;
        case AST_ARG_TYPE_NAMED: {
            return categorizeArg(arg->val.named->arg, env);
        }
        break;
        case AST_ARG_TYPE_ENV: {
            return ARG_CATEGORY_ENV;
        }
        break;
        case AST_ARG_TYPE_UNPACK: {
            return ARG_CATEGORY_STRUCT;
        }
        break;
        case AST_ARG_TYPE_NUMBER: {
            return ARG_CATEGORY_CONST;
        }
        break;
        case AST_ARG_TYPE_CHARACTER: {
            return ARG_CATEGORY_CONST;
        }
        break;
        default:
            cant_happen("unrecognised arg type %d in categorizeArg", arg->type);
    }
}

static void *safe_malloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        perror("malloc");
        exit(1);
    }
    return ptr;
}

static DecisionTree *newDecisionTree(int globalCount) {
    DecisionTree *dt = SAFE_MALLOC(DecisionTree);
    int square = globalCount * globalCount;
    dt->currentArgs = NEW_ARRAY(AstArg *, square);
    dt->remainingArgs = NEW_ARRAY(AstArgList *, square);
    dt->bodies = NEW_ARRAY(AstNest *, square);
    dt->nBodies = NEW_ARRAY(int, globalCount);
    for (int i = 0; i < globalCount; i++) {
        dt->nBodies[i] = 0;
    }
    dt->globalCount = globalCount;
    dt->localCount = 0;
    return dt;
}

static void freeDecisionTree(DecisionTree *dt) {
    int square = dt->globalCount * dt->globalCount;
    FREE_ARRAY(AstArg *, dt->currentArgs, square);
    FREE_ARRAY(AstArgList *, dt->remainingArgs, square);
    FREE_ARRAY(AstNest *, dt->bodies, square);
    FREE_ARRAY(int, dt->nBodies, dt->globalCount);
    free(dt);
}

static AstArg *getCurrentArg(DecisionTree *dt, int major, int minor) {
    return dt->currentArgs[major * dt->globalCount + minor];
}

static AstArgList *getRemainingArgList(DecisionTree *dt, int major, int minor) {
    return dt->remainingArgs[major * dt->globalCount + minor];
}

static AstNest *getBody(DecisionTree *dt, int major, int minor) {
    return dt->bodies[major * dt->globalCount + minor];
}

static int getNBodies(DecisionTree *dt, int major) {
    return dt->nBodies[major];
}

static LamExp *convertCompositeBodiesInParallel(int nargs, int nBodies, LamVarList *generatedArgs, AstArgList *argLists[], AstNest *nests[], LamContext *env) {
    if (nargs == 0) {
        if (nbodies != 1) {
            can_happen("more than one case matches");
        }
        return lamConvertNest(nests[0], env);
    }
    AstArg **constPool = NEW_ARRAY(AstArg*, nBodies);
    AstArgList **constArgs = NEW_ARRAY(AstArgList*, nBodies);
    int numConsts = 0;
    AstArg **varPool = NEW_ARRAY(AstArg*, nBodies);
    AstArgList **varArgs = NEW_ARRAY(AstArgList*, nBodies);
    int numVars = 0;
    AstArg **structPool = NEW_ARRAY(AstArg*, nBodies);
    AstArgList **structArgs = NEW_ARRAY(AstArgList*, nBodies);
    int numStructs = 0;

    FREE_ARRAY(AstArg**, constPool, nBodies);
    FREE_ARRAY(AstArgList**, constArgs, nBodies);
    FREE_ARRAY(AstArg**, varPool, nBodies);
    FREE_ARRAY(AstArgList**, varArgs, nBodies);
    FREE_ARRAY(AstArg**, structPool, nBodies);
    FREE_ARRAY(AstArgList**, structArgs, nBodies);
    return NULL;
}

static LamExp *convertCompositeBodies(int nargs, LamVarList *generatedArgs, AstCompositeFunction *fun, LamContext *env) {
    int nBodies = countAstCompositeFunction(fun);
    if (nBodies == 0) {
        can_happen("empty composite function");
        return NULL;
    }
    AstNest **nests = NEW_ARRAY(AstNest *, nBodies);
    AstArgList **argLists = NEW_ARRAY(AstArgList *, nBodies);
    for (int i = 0; i < nBodies; i++) {
        AstFunction *func = fun->function;
        nests[i] = func->nest;
        argLists[i] = func->argList;
        fun = fun->next;
    }
    LamExp *result = convertCompositeBodiesInParallel(nargs, nBodies, generatedArgs, argLists, nests, env);
    FREE_ARRAY(AstNest*, nests, nBodies);
    FREE_ARRAY(AstArgList*, argLists, nBodies);
    return result;
}

/*******************************************/

static LamExp * convertCompositeFun(AstCompositeFunction *fun, LamContext *env) {
    if (fun == NULL) cant_happen("composite function with no components");
    int nargs = countAstArgList(fun->function->argList);
    LamVarList *generatedArgs = genLamVarList(nargs);
    int save = PROTECT(generatedArgs);
    LamExp *body = convertCompositeBodies(nargs, generatedArgs, fun, env);
    (void) PROTECT(body);
    LamLam *lambda = newLamLam(nargs, generatedArgs, body);
    (void) PROTECT(lambda);
    LamExp *result = newLamExp(LAMEXP_TYPE_LAM, LAMEXP_VAL_LAM(lambda));
    UNPROTECT(save);
    return result;
}

static LamExp *convertExpression(AstExpression *expression, LamContext *env) {
    ENTER(convertExpression)
    LamExp *result = NULL;
    switch (expression->type) {
        case AST_EXPRESSION_TYPE_NIL: {
            HashSymbol *nil = newSymbol("nil");
            result = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(nil));
        }
        break;
        case AST_EXPRESSION_TYPE_BACK:
            result = newLamExp(LAMEXP_TYPE_BACK, LAMEXP_VAL_BACK());
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            result = convertFunCall(expression->val.funCall, env);
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            result = newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(expression->val.symbol));
            break;
        case AST_EXPRESSION_TYPE_NUMBER:
            result = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(expression->val.number));
            break;
        case AST_EXPRESSION_TYPE_CHARACTER:
            result = newLamExp(LAMEXP_TYPE_CHARACTER, LAMEXP_VAL_CHARACTER(expression->val.character));
            break;
        case AST_EXPRESSION_TYPE_FUN:
            result = convertCompositeFun(expression->val.fun, env);
            break;
        case AST_EXPRESSION_TYPE_NEST:
            result = lamConvertNest(expression->val.nest, env);
            break;
        default:
            cant_happen("unrecognised expression type %d in convertExpression", expression->type);
    }
    LEAVE(convertExpression)
    return result;
}

static LamList *convertExpressions(AstExpressions *expressions, LamContext *env) {
    if (expressions == NULL) return NULL;
    LamList *next = convertExpressions(expressions->next, env);
    int save = PROTECT(next);
    LamExp *exp = convertExpression(expressions->expression, env);
    (void) PROTECT(exp);
    LamList *this = newLamList(exp, next);
    UNPROTECT(save);
    return this;
}

