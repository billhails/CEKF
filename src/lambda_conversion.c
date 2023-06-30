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
#include "common.h"
#include "lambda_conversion.h"
#include "lambda_helper.h"
#include "symbol.h"

static LamLetRecBindings *convertDefinitions(AstDefinitions *definitions, LamContext *env);
static LamList *convertExpressions(AstExpressions *expressions, LamContext *env);
static int countLetRecBindings(LamLetRecBindings *b);
static LamLetRecBindings *prependDefinition(AstDefinition *definition, LamContext *env, LamLetRecBindings *next);
static LamLetRecBindings *prependDefine(AstDefine *define, LamContext *env, LamLetRecBindings *next);
static LamLetRecBindings *prependTypeDef(AstTypeDef *typeDef, LamLetRecBindings *next, LamContext *env);
static LamExp *convertExpression(AstExpression *expression);
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

LamLetRec *lamConvertNest(AstNest *nest, LamContext *env) {
    ENTER(lamConvertNest)
    LamContext *ext = extendLamContext(env);
    int save = PROTECT(ext);
    LamLetRecBindings *bindings = convertDefinitions(nest->definitions, ext);
    PROTECT(bindings);
    LamList *body = convertExpressions(nest->expressions, ext);
    PROTECT(body);
    LamLetRec *result = newLamLetRec(countLetRecBindings(bindings), bindings, body);
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

static int countTypeList(AstTypeList *typeList) {
    ENTER(countTypeList)
    int count = 0;
    while (typeList != NULL) {
        count++;
        typeList = typeList->next;
    }
    LEAVE(countTypeList)
    return count;
}

static void collectTypeInfo(HashSymbol *symbol, int index, int nargs, LamContext *env) {
    ENTER(collectTypeInfo)
    LamTypeConstructorInfo *info = newLamTypeConstructorInfo(nargs, index);
    int save = PROTECT(info);
    addToLamContext(env, symbol, info);
    UNPROTECT(save);
    LEAVE(collectTypeInfo)
}

static LamExp *analyzeTypeConstructor(AstTypeConstructor *typeConstructor, int index, bool someoneHasFields, LamContext *env) {
    ENTER(analyzeTypeConstructor)
    int nargs = countTypeList(typeConstructor->typeList);
    collectTypeInfo(typeConstructor->symbol, index, nargs, env);
    if (nargs > 0) {
        // if this type constructor has args, then collectTypeInfo will have collected enough
        // for us to be able to inline it when we encounter it in functions
        LEAVE(analyzeTypeConstructor)
        return NULL;
    } else {
        // otherwise we can treat it as a constant
        LamExp *indexExp = newLamExp(LAMEXP_TYPE_INTEGER, LAMEXP_VAL_INTEGER(index));
        if (someoneHasFields) {
            // if another constructor for this type has args, then we must create a constant vector
            int save = PROTECT(indexExp);
            LamList *args = newLamList(indexExp, NULL);
            PROTECT(args);
            LamMakeVec * makeVec = newLamMakeVec(1, args);
            PROTECT(makeVec);
            LamExp * makeVecExp = newLamExp(LAMEXP_TYPE_MAKEVEC, LAMEXP_VAL_MAKEVEC(makeVec));
            UNPROTECT(save);
            LEAVE(analyzeTypeConstructor)
            return makeVecExp;
        } else {
            // if none of the constructors for this type take arguments, then we can treat it as a simple enumeration
            LEAVE(analyzeTypeConstructor)
            return indexExp;
        }
    }
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
    LamExp *exp = convertExpression(define->expression);
    int save = PROTECT(exp);
    LamLetRecBindings *this = newLamLetRecBindings(define->symbol, exp, next);
    UNPROTECT(save);
    LEAVE(prependDefine)
    return this;
}

static LamExp *convertExpression(AstExpression *expression) {
    return NULL;
}

static LamList *convertExpressions(AstExpressions *expressions, LamContext *env) {
    return NULL;
}

static int countLetRecBindings(LamLetRecBindings *b) {
    ENTER(countLetRecBindings)
    int count = 0;

    while (b != NULL) {
        b = b->next;
        count++;
    }

    LEAVE(countLetRecBindings)
    return count;
}
