/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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
 *
 * Desugaring plain lambda structures to minimal lambda structures
 */

#include "lambda.h"
#include "memory.h"
#include "minlam.h"

#include "lambda_desugar.h"

#ifdef DEBUG_LAMBDA_DESUGAR
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

// Forward declarations
static MinLam *desugarLamLam(LamLam *node);
static MinVarList *desugarLamVarList(LamVarList *node);
static MinPrimApp *desugarLamPrimApp(LamPrimApp *node);
static MinSequence *desugarLamSequence(LamSequence *node);
static MinArgs *desugarLamArgs(LamArgs *node);
static MinApply *desugarLamApply(LamApply *node);
static MinLookUp *desugarLamLookUp(LamLookUp *node);
static MinLookUpSymbol *desugarLamLookUpSymbol(LamLookUpSymbol *node);
static MinConstant *desugarLamConstant(LamConstant *node);
static MinConstruct *desugarLamConstruct(LamConstruct *node);
static MinDeconstruct *desugarLamDeconstruct(LamDeconstruct *node);
static MinTupleIndex *desugarLamTupleIndex(LamTupleIndex *node);
static MinMakeVec *desugarLamMakeVec(LamMakeVec *node);
static MinIff *desugarLamIff(LamIff *node);
static MinCond *desugarLamCond(LamCond *node);
static MinIntCondCases *desugarLamIntCondCases(LamIntCondCases *node);
static MinCharCondCases *desugarLamCharCondCases(LamCharCondCases *node);
static MinMatch *desugarLamMatch(LamMatch *node);
static MinMatchList *desugarLamMatchList(LamMatchList *node);
static MinIntList *desugarLamIntList(LamIntList *node);
static MinLet *desugarLamLet(LamLet *node);
static MinLetRec *desugarLamLetRec(LamLetRec *node);
static MinLetStar *desugarLamLetStar(LamLetStar *node);
static MinBindings *desugarLamBindings(LamBindings *node);
static MinAmb *desugarLamAmb(LamAmb *node);
static MinPrint *desugarLamPrint(LamPrint *node);
static MinTypeOf *desugarLamTypeOf(LamTypeOf *node);
static MinTypeDefs *desugarLamTypeDefs(LamTypeDefs *node);
static MinTypeDefList *desugarLamTypeDefList(LamTypeDefList *node);
static MinTypeDef *desugarLamTypeDef(LamTypeDef *node);
static MinTypeDef *desugarLamTypeDef(LamTypeDef *node);
static MinTypeConstructorList *
desugarLamTypeConstructorList(LamTypeConstructorList *node);
static MinTypeSig *desugarLamTypeSig(LamTypeSig *node);
static MinTypeTags *desugarLamTypeTags(LamTypeTags *node);
static MinTypeSigArgs *desugarLamTypeSigArgs(LamTypeSigArgs *node);
static MinTypeConstructor *desugarLamTypeConstructor(LamTypeConstructor *node);
static MinTypeConstructorArgs *
desugarLamTypeConstructorArgs(LamTypeConstructorArgs *node);
static MinTypeFunction *desugarLamTypeFunction(LamTypeFunction *node);
static MinTypeFunction *desugarLamTypeFunction(LamTypeFunction *node);
static MinTypeConstructorInfo *
desugarLamTypeConstructorInfo(LamTypeConstructorInfo *node);
static MinExp *desugarLamExp_internal(LamExp *node);
static MinLookUpOrSymbol *desugarLamLookUpOrSymbol(LamLookUpOrSymbol *node);
static MinCondCases *desugarLamCondCases(LamCondCases *node);
static MinTypeConstructorType *
desugarLamTypeConstructorType(LamTypeConstructorType *node);
static MinNameSpaceArray *desugarLamNameSpaceArray(LamNameSpaceArray *node);

// Visitor implementations
static MinVarList *desugarLamVarList(LamVarList *node) {
    ENTER(desugarLamVarList);
    if (node == NULL) {
        LEAVE(desugarLamVarList);
        return NULL;
    }

    // Pass through var (type: HashSymbol, not memory-managed)
    MinVarList *new_next = desugarLamVarList(node->next);
    int save = PROTECT(new_next);
    MinVarList *this = newMinVarList(CPI(node), node->var, new_next);

    UNPROTECT(save);
    LEAVE(desugarLamVarList);
    return this;
}

static MinPrimOp desugarLamPrimOp(LamPrimOp primOp) {
    switch (primOp) {
    case LAMPRIMOP_TYPE_ADD:
        return MINPRIMOP_TYPE_ADD;
    case LAMPRIMOP_TYPE_SUB:
        return MINPRIMOP_TYPE_SUB;
    case LAMPRIMOP_TYPE_MUL:
        return MINPRIMOP_TYPE_MUL;
    case LAMPRIMOP_TYPE_DIV:
        return MINPRIMOP_TYPE_DIV;
    case LAMPRIMOP_TYPE_MOD:
        return MINPRIMOP_TYPE_MOD;
    case LAMPRIMOP_TYPE_POW:
        return MINPRIMOP_TYPE_POW;
    case LAMPRIMOP_TYPE_EQ:
        return MINPRIMOP_TYPE_EQ;
    case LAMPRIMOP_TYPE_NE:
        return MINPRIMOP_TYPE_NE;
    case LAMPRIMOP_TYPE_GT:
        return MINPRIMOP_TYPE_GT;
    case LAMPRIMOP_TYPE_LT:
        return MINPRIMOP_TYPE_LT;
    case LAMPRIMOP_TYPE_GE:
        return MINPRIMOP_TYPE_GE;
    case LAMPRIMOP_TYPE_LE:
        return MINPRIMOP_TYPE_LE;
    case LAMPRIMOP_TYPE_CMP:
        return MINPRIMOP_TYPE_CMP;
    case LAMPRIMOP_TYPE_VEC:
        return MINPRIMOP_TYPE_VEC;
    default:
        cant_happen("unrecognised %s", lamPrimOpName(primOp));
    }
}
static MinPrimApp *desugarLamPrimApp(LamPrimApp *node) {
    ENTER(desugarLamPrimApp);
    if (node == NULL) {
        LEAVE(desugarLamPrimApp);
        return NULL;
    }

    MinExp *new_exp1 = desugarLamExp_internal(node->exp1);
    int save = PROTECT(new_exp1);
    MinExp *new_exp2 = desugarLamExp_internal(node->exp2);
    PROTECT(new_exp2);
    MinPrimApp *result = newMinPrimApp(CPI(node), desugarLamPrimOp(node->type),
                                       new_exp1, new_exp2);
    UNPROTECT(save);
    LEAVE(desugarLamPrimApp);
    return result;
}

static MinLam *desugarLamLam(LamLam *node) {
    ENTER(desugarLamLam);
    if (node == NULL) {
        LEAVE(desugarLamLam);
        return NULL;
    }

    MinVarList *params = desugarLamVarList(node->args);
    int save = PROTECT(params);
    MinExp *body = desugarLamExp_internal(node->exp);
    PROTECT(body);
    MinLam *result = newMinLam(CPI(node), params, body);
    UNPROTECT(save);
    LEAVE(desugarLamLam);
    return result;
}

static MinSequence *desugarLamSequence(LamSequence *node) {
    ENTER(desugarLamSequence);
    if (node == NULL) {
        LEAVE(desugarLamSequence);
        return NULL;
    }

    MinSequence *next = desugarLamSequence(node->next);
    int save = PROTECT(next);
    MinExp *exp = desugarLamExp_internal(node->exp);
    PROTECT(exp);
    MinSequence *result = newMinSequence(CPI(node), exp, next);
    UNPROTECT(save);
    LEAVE(desugarLamSequence);
    return result;
}

static MinArgs *desugarLamArgs(LamArgs *node) {
    ENTER(desugarLamArgs);
    if (node == NULL) {
        LEAVE(desugarLamArgs);
        return NULL;
    }

    MinArgs *next = desugarLamArgs(node->next);
    int save = PROTECT(next);
    MinExp *exp = desugarLamExp_internal(node->exp);
    PROTECT(exp);
    MinArgs *result = newMinArgs(CPI(node), exp, next);
    UNPROTECT(save);
    LEAVE(desugarLamArgs);
    return result;
}

static MinApply *desugarLamApply(LamApply *node) {
    ENTER(desugarLamApply);
    if (node == NULL) {
        LEAVE(desugarLamApply);
        return NULL;
    }

    MinExp *function = desugarLamExp_internal(node->function);
    int save = PROTECT(function);
    MinArgs *args = desugarLamArgs(node->args);
    PROTECT(args);
    MinApply *result = newMinApply(CPI(node), function, args);
    UNPROTECT(save);
    LEAVE(desugarLamApply);
    return result;
}

static MinLookUp *desugarLamLookUp(LamLookUp *node) {
    ENTER(desugarLamLookUp);
    if (node == NULL) {
        LEAVE(desugarLamLookUp);
        return NULL;
    }

    MinExp *exp = desugarLamExp_internal(node->exp);
    int save = PROTECT(exp);
    MinLookUp *result =
        newMinLookUp(CPI(node), node->nsId, node->nsSymbol, exp);
    UNPROTECT(save);
    LEAVE(desugarLamLookUp);
    return result;
}

static MinLookUpSymbol *desugarLamLookUpSymbol(LamLookUpSymbol *node) {
    ENTER(desugarLamLookUpSymbol);
    if (node == NULL) {
        LEAVE(desugarLamLookUpSymbol);
        return NULL;
    }

    MinLookUpSymbol *result =
        newMinLookUpSymbol(CPI(node), node->nsId, node->nsSymbol, node->symbol);
    LEAVE(desugarLamLookUpSymbol);
    return result;
}

static MinConstant *desugarLamConstant(LamConstant *node) {
    ENTER(desugarLamConstant);
    if (node == NULL) {
        LEAVE(desugarLamConstant);
        return NULL;
    }

    MinConstant *result = newMinConstant(CPI(node), node->name, node->tag);
    LEAVE(desugarLamConstant);
    return result;
}

static MinConstruct *desugarLamConstruct(LamConstruct *node) {
    ENTER(desugarLamConstruct);
    if (node == NULL) {
        LEAVE(desugarLamConstruct);
        return NULL;
    }

    MinArgs *args = desugarLamArgs(node->args);
    int save = PROTECT(args);
    MinConstruct *result =
        newMinConstruct(CPI(node), node->name, node->tag, args);

    UNPROTECT(save);
    LEAVE(desugarLamConstruct);
    return result;
}

static MinDeconstruct *desugarLamDeconstruct(LamDeconstruct *node) {
    ENTER(desugarLamDeconstruct);
    if (node == NULL) {
        LEAVE(desugarLamDeconstruct);
        return NULL;
    }

    MinExp *new_exp = desugarLamExp_internal(node->exp);
    int save = PROTECT(new_exp);
    MinDeconstruct *result = newMinDeconstruct(CPI(node), node->name,
                                               node->nsId, node->vec, new_exp);
    UNPROTECT(save);
    LEAVE(desugarLamDeconstruct);
    return result;
}

static MinTupleIndex *desugarLamTupleIndex(LamTupleIndex *node) {
    ENTER(desugarLamTupleIndex);
    if (node == NULL) {
        LEAVE(desugarLamTupleIndex);
        return NULL;
    }

    MinExp *new_exp = desugarLamExp_internal(node->exp);
    int save = PROTECT(new_exp);
    MinTupleIndex *result =
        newMinTupleIndex(CPI(node), node->vec, node->size, new_exp);
    UNPROTECT(save);
    LEAVE(desugarLamTupleIndex);
    return result;
}

static MinMakeVec *desugarLamMakeVec(LamMakeVec *node) {
    ENTER(desugarLamMakeVec);
    if (node == NULL) {
        LEAVE(desugarLamMakeVec);
        return NULL;
    }

    MinArgs *new_args = desugarLamArgs(node->args);
    int save = PROTECT(new_args);
    MinMakeVec *result = newMinMakeVec(CPI(node), node->nArgs, new_args);
    UNPROTECT(save);
    LEAVE(desugarLamMakeVec);
    return result;
}

static MinIff *desugarLamIff(LamIff *node) {
    ENTER(desugarLamIff);
    if (node == NULL) {
        LEAVE(desugarLamIff);
        return NULL;
    }

    MinExp *condition = desugarLamExp_internal(node->condition);
    int save = PROTECT(condition);
    MinExp *consequent = desugarLamExp_internal(node->consequent);
    PROTECT(consequent);
    MinExp *alternative = desugarLamExp_internal(node->alternative);
    PROTECT(alternative);
    MinIff *result = newMinIff(CPI(node), condition, consequent, alternative);
    UNPROTECT(save);
    LEAVE(desugarLamIff);
    return result;
}

static MinCond *desugarLamCond(LamCond *node) {
    ENTER(desugarLamCond);
    if (node == NULL) {
        LEAVE(desugarLamCond);
        return NULL;
    }

    MinExp *value = desugarLamExp_internal(node->value);
    int save = PROTECT(value);
    MinCondCases *cases = desugarLamCondCases(node->cases);
    PROTECT(cases);
    MinCond *result = newMinCond(CPI(node), value, cases);
    UNPROTECT(save);
    LEAVE(desugarLamCond);
    return result;
}

static MinIntCondCases *desugarLamIntCondCases(LamIntCondCases *node) {
    ENTER(desugarLamIntCondCases);
    if (node == NULL) {
        LEAVE(desugarLamIntCondCases);
        return NULL;
    }

    MinExp *body = desugarLamExp_internal(node->body);
    int save = PROTECT(body);
    MinIntCondCases *next = desugarLamIntCondCases(node->next);
    PROTECT(next);
    MinIntCondCases *result =
        newMinIntCondCases(CPI(node), node->constant, body, next);
    UNPROTECT(save);
    LEAVE(desugarLamIntCondCases);
    return result;
}

static MinCharCondCases *desugarLamCharCondCases(LamCharCondCases *node) {
    ENTER(desugarLamCharCondCases);
    if (node == NULL) {
        LEAVE(desugarLamCharCondCases);
        return NULL;
    }

    MinExp *body = desugarLamExp_internal(node->body);
    int save = PROTECT(body);
    MinCharCondCases *next = desugarLamCharCondCases(node->next);
    PROTECT(next);
    MinCharCondCases *result =
        newMinCharCondCases(CPI(node), node->constant, body, next);
    UNPROTECT(save);
    LEAVE(desugarLamCharCondCases);
    return result;
}

static MinMatch *desugarLamMatch(LamMatch *node) {
    ENTER(desugarLamMatch);
    if (node == NULL) {
        LEAVE(desugarLamMatch);
        return NULL;
    }

    MinExp *index = desugarLamExp_internal(node->index);
    int save = PROTECT(index);
    MinMatchList *cases = desugarLamMatchList(node->cases);
    PROTECT(cases);
    MinMatch *result = newMinMatch(CPI(node), index, cases);
    UNPROTECT(save);
    LEAVE(desugarLamMatch);
    return result;
}

static MinMatchList *desugarLamMatchList(LamMatchList *node) {
    ENTER(desugarLamMatchList);
    if (node == NULL) {
        LEAVE(desugarLamMatchList);
        return NULL;
    }

    MinIntList *matches = desugarLamIntList(node->matches);
    int save = PROTECT(matches);
    MinExp *body = desugarLamExp_internal(node->body);
    PROTECT(body);
    MinMatchList *next = desugarLamMatchList(node->next);
    PROTECT(next);
    MinMatchList *result = newMinMatchList(CPI(node), matches, body, next);
    UNPROTECT(save);
    LEAVE(desugarLamMatchList);
    return result;
}

static MinIntList *desugarLamIntList(LamIntList *node) {
    ENTER(desugarLamIntList);
    if (node == NULL) {
        LEAVE(desugarLamIntList);
        return NULL;
    }

    MinIntList *next = desugarLamIntList(node->next);
    int save = PROTECT(next);
    MinIntList *result =
        newMinIntList(CPI(node), node->item, node->name, node->nsId, next);
    UNPROTECT(save);
    LEAVE(desugarLamIntList);
    return result;
}

static MinLet *desugarLamLet(LamLet *node) {
    ENTER(desugarLamLet);
    if (node == NULL) {
        LEAVE(desugarLamLet);
        return NULL;
    }

    MinBindings *bindings = desugarLamBindings(node->bindings);
    int save = PROTECT(bindings);
    MinExp *body = desugarLamExp_internal(node->body);
    PROTECT(body);
    MinLet *result = newMinLet(CPI(node), bindings, body);
    UNPROTECT(save);
    LEAVE(desugarLamLet);
    return result;
}

static MinLetRec *desugarLamLetRec(LamLetRec *node) {
    ENTER(desugarLamLetRec);
    if (node == NULL) {
        LEAVE(desugarLamLetRec);
        return NULL;
    }

    MinBindings *bindings = desugarLamBindings(node->bindings);
    int save = PROTECT(bindings);
    MinExp *body = desugarLamExp_internal(node->body);
    PROTECT(body);
    MinLetRec *result = newMinLetRec(CPI(node), bindings, body);
    UNPROTECT(save);
    LEAVE(desugarLamLetRec);
    return result;
}

static MinLetStar *desugarLamLetStar(LamLetStar *node) {
    ENTER(desugarLamLetStar);
    if (node == NULL) {
        LEAVE(desugarLamLetStar);
        return NULL;
    }

    MinBindings *bindings = desugarLamBindings(node->bindings);
    int save = PROTECT(bindings);
    MinExp *body = desugarLamExp_internal(node->body);
    PROTECT(body);
    MinLetStar *result = newMinLetStar(CPI(node), bindings, body);
    UNPROTECT(save);
    LEAVE(desugarLamLetStar);
    return result;
}

static MinBindings *desugarLamBindings(LamBindings *node) {
    ENTER(desugarLamBindings);
    if (node == NULL) {
        LEAVE(desugarLamBindings);
        return NULL;
    }

    MinExp *val = desugarLamExp_internal(node->val);
    int save = PROTECT(val);
    MinBindings *next = desugarLamBindings(node->next);
    PROTECT(next);
    MinBindings *result = newMinBindings(CPI(node), node->var, val, next);
    UNPROTECT(save);
    LEAVE(desugarLamBindings);
    return result;
}

static MinAmb *desugarLamAmb(LamAmb *node) {
    ENTER(desugarLamAmb);
    if (node == NULL) {
        LEAVE(desugarLamAmb);
        return NULL;
    }

    MinExp *left = desugarLamExp_internal(node->left);
    int save = PROTECT(left);
    MinExp *right = desugarLamExp_internal(node->right);
    PROTECT(right);
    MinAmb *result = newMinAmb(CPI(node), left, right);
    UNPROTECT(save);
    LEAVE(desugarLamAmb);
    return result;
}

static MinPrint *desugarLamPrint(LamPrint *node) {
    ENTER(desugarLamPrint);
    if (node == NULL) {
        LEAVE(desugarLamPrint);
        return NULL;
    }

    MinExp *exp = desugarLamExp_internal(node->exp);
    int save = PROTECT(exp);
    MinExp *printer = desugarLamExp_internal(node->printer);
    PROTECT(printer);
    MinPrint *result = newMinPrint(CPI(node), exp);
    result->printer = printer;
    UNPROTECT(save);
    LEAVE(desugarLamPrint);
    return result;
}

static MinTypeOf *desugarLamTypeOf(LamTypeOf *node) {
    ENTER(desugarLamTypeOf);
    if (node == NULL) {
        LEAVE(desugarLamTypeOf);
        return NULL;
    }

    MinExp *exp = desugarLamExp_internal(node->exp);
    int save = PROTECT(exp);
    MinExp *typeString = desugarLamExp_internal(node->typeString);
    PROTECT(typeString);

    MinTypeOf *result = newMinTypeOf(CPI(node), exp);
    result->typeString = typeString;
    UNPROTECT(save);
    LEAVE(desugarLamTypeOf);
    return result;
}

static MinTypeDefs *desugarLamTypeDefs(LamTypeDefs *node) {
    ENTER(desugarLamTypeDefs);
    if (node == NULL) {
        LEAVE(desugarLamTypeDefs);
        return NULL;
    }

    MinTypeDefList *typeDefs = desugarLamTypeDefList(node->typeDefs);
    int save = PROTECT(typeDefs);
    MinExp *body = desugarLamExp_internal(node->body);
    PROTECT(body);
    MinTypeDefs *result = newMinTypeDefs(CPI(node), typeDefs, body);
    UNPROTECT(save);
    LEAVE(desugarLamTypeDefs);
    return result;
}

static MinTypeDefList *desugarLamTypeDefList(LamTypeDefList *node) {
    ENTER(desugarLamTypeDefList);
    if (node == NULL) {
        LEAVE(desugarLamTypeDefList);
        return NULL;
    }

    MinTypeDef *new_typeDef = desugarLamTypeDef(node->typeDef);
    int save = PROTECT(new_typeDef);
    MinTypeDefList *new_next = desugarLamTypeDefList(node->next);
    PROTECT(new_next);
    MinTypeDefList *result =
        newMinTypeDefList(CPI(node), new_typeDef, new_next);
    UNPROTECT(save);
    LEAVE(desugarLamTypeDefList);
    return result;
}

static MinTypeDef *desugarLamTypeDef(LamTypeDef *node) {
    ENTER(desugarLamTypeDef);
    if (node == NULL) {
        LEAVE(desugarLamTypeDef);
        return NULL;
    }

    MinTypeSig *type = desugarLamTypeSig(node->type);
    int save = PROTECT(type);
    MinTypeConstructorList *constructors =
        desugarLamTypeConstructorList(node->constructors);
    PROTECT(constructors);
    MinTypeDef *result = newMinTypeDef(CPI(node), type, constructors);
    UNPROTECT(save);
    LEAVE(desugarLamTypeDef);
    return result;
}

static MinTypeConstructorList *
desugarLamTypeConstructorList(LamTypeConstructorList *node) {
    ENTER(desugarLamTypeConstructorList);
    if (node == NULL) {
        LEAVE(desugarLamTypeConstructorList);
        return NULL;
    }

    MinTypeConstructor *constructor =
        desugarLamTypeConstructor(node->constructor);
    int save = PROTECT(constructor);
    MinTypeConstructorList *next = desugarLamTypeConstructorList(node->next);
    PROTECT(next);
    MinTypeConstructorList *result =
        newMinTypeConstructorList(CPI(node), constructor, next);
    UNPROTECT(save);
    LEAVE(desugarLamTypeConstructorList);
    return result;
}

static MinTypeSig *desugarLamTypeSig(LamTypeSig *node) {
    ENTER(desugarLamTypeSig);
    if (node == NULL) {
        LEAVE(desugarLamTypeSig);
        return NULL;
    }

    MinTypeSigArgs *args = desugarLamTypeSigArgs(node->args);
    int save = PROTECT(args);
    MinTypeSig *result = newMinTypeSig(CPI(node), node->name, args);
    UNPROTECT(save);
    LEAVE(desugarLamTypeSig);
    return result;
}

static MinTypeTags *desugarLamTypeTags(LamTypeTags *node) {
    ENTER(desugarLamTypeTags);
    if (node == NULL) {
        LEAVE(desugarLamTypeTags);
        return NULL;
    }

    MinTypeTags *next = desugarLamTypeTags(node->next);
    int save = PROTECT(next);
    MinTypeTags *result = newMinTypeTags(CPI(node), node->tag, next);
    UNPROTECT(save);
    LEAVE(desugarLamTypeTags);
    return result;
}

static MinTypeSigArgs *desugarLamTypeSigArgs(LamTypeSigArgs *node) {
    ENTER(desugarLamTypeSigArgs);
    if (node == NULL) {
        LEAVE(desugarLamTypeSigArgs);
        return NULL;
    }

    MinTypeSigArgs *next = desugarLamTypeSigArgs(node->next);
    int save = PROTECT(next);
    MinTypeSigArgs *result = newMinTypeSigArgs(CPI(node), node->name, next);
    UNPROTECT(save);
    LEAVE(desugarLamTypeSigArgs);
    return result;
}

static MinTypeConstructor *desugarLamTypeConstructor(LamTypeConstructor *node) {
    ENTER(desugarLamTypeConstructor);
    if (node == NULL) {
        LEAVE(desugarLamTypeConstructor);
        return NULL;
    }

    MinTypeSig *type = desugarLamTypeSig(node->type);
    int save = PROTECT(type);
    MinTypeConstructorArgs *args = desugarLamTypeConstructorArgs(node->args);
    PROTECT(args);
    MinTypeConstructor *result =
        newMinTypeConstructor(CPI(node), node->name, type, args);
    UNPROTECT(save);
    LEAVE(desugarLamTypeConstructor);
    return result;
}

static MinTypeConstructorArgs *
desugarLamTypeConstructorArgs(LamTypeConstructorArgs *node) {
    ENTER(desugarLamTypeConstructorArgs);
    if (node == NULL) {
        LEAVE(desugarLamTypeConstructorArgs);
        return NULL;
    }

    MinTypeConstructorType *arg = desugarLamTypeConstructorType(node->arg);
    int save = PROTECT(arg);
    MinTypeConstructorArgs *next = desugarLamTypeConstructorArgs(node->next);
    PROTECT(next);
    MinTypeConstructorArgs *result =
        newMinTypeConstructorArgs(CPI(node), arg, next);
    UNPROTECT(save);
    LEAVE(desugarLamTypeConstructorArgs);
    return result;
}

static MinTypeFunction *desugarLamTypeFunction(LamTypeFunction *node) {
    ENTER(desugarLamTypeFunction);
    if (node == NULL) {
        LEAVE(desugarLamTypeFunction);
        return NULL;
    }

    MinLookUpOrSymbol *name = desugarLamLookUpOrSymbol(node->name);
    int save = PROTECT(name);
    MinTypeConstructorArgs *args = desugarLamTypeConstructorArgs(node->args);
    PROTECT(args);
    MinTypeFunction *result = newMinTypeFunction(CPI(node), name, args);
    UNPROTECT(save);
    LEAVE(desugarLamTypeFunction);
    return result;
}

static MinTypeConstructorInfo *
desugarLamTypeConstructorInfo(LamTypeConstructorInfo *node) {
    ENTER(desugarLamTypeConstructorInfo);
    if (node == NULL) {
        LEAVE(desugarLamTypeConstructorInfo);
        return NULL;
    }

    MinTypeConstructor *type = desugarLamTypeConstructor(node->type);
    int save = PROTECT(type);
    MinTypeTags *tags = desugarLamTypeTags(node->tags);
    PROTECT(tags);
    MinTypeConstructorInfo *result = newMinTypeConstructorInfo(
        CPI(node), node->name, node->nsId, type, tags, node->needsVec,
        node->arity, node->size, node->index);
    UNPROTECT(save);
    LEAVE(desugarLamTypeConstructorInfo);
    return result;
}

static MinExp *desugarLamExp_internal(LamExp *node) {
    ENTER(desugarLamExp);
    if (node == NULL) {
        LEAVE(desugarLamExp);
        return NULL;
    }

    MinExp *result = NULL;
    int save = PROTECT(NULL);

    switch (node->type) {
    case LAMEXP_TYPE_AMB: {
        MinAmb *new = desugarLamAmb(getLamExp_Amb(node));
        PROTECT(new);
        result = newMinExp_Amb(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_APPLY: {
        MinApply *new = desugarLamApply(getLamExp_Apply(node));
        PROTECT(new);
        result = newMinExp_Apply(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_ARGS: {
        MinArgs *new = desugarLamArgs(getLamExp_Args(node));
        PROTECT(new);
        result = newMinExp_Args(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_BACK: {
        result = newMinExp_Back(CPI(node));
        break;
    }
    case LAMEXP_TYPE_BIGINTEGER: {
        result = newMinExp_BigInteger(CPI(node), getLamExp_BigInteger(node));
        break;
    }
    case LAMEXP_TYPE_BINDINGS: {
        // LamBindings
        MinBindings *new = desugarLamBindings(getLamExp_Bindings(node));
        PROTECT(new);
        result = newMinExp_Bindings(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_CALLCC: {
        // LamExp
        MinExp *new_callcc = desugarLamExp_internal(getLamExp_CallCC(node));
        PROTECT(new_callcc);
        result = newMinExp_CallCC(CPI(node), new_callcc);
        break;
    }
    case LAMEXP_TYPE_CHARACTER: {
        result = newMinExp_Character(CPI(node), getLamExp_Character(node));
        break;
    }
    case LAMEXP_TYPE_COND: {
        MinCond *new = desugarLamCond(getLamExp_Cond(node));
        PROTECT(new);
        result = newMinExp_Cond(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_CONSTANT: {
        MinConstant *new = desugarLamConstant(getLamExp_Constant(node));
        PROTECT(new);
        result = newMinExp_Constant(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_CONSTRUCT: {
        MinConstruct *new = desugarLamConstruct(getLamExp_Construct(node));
        PROTECT(new);
        result = newMinExp_Construct(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_CONSTRUCTOR: {
        MinTypeConstructorInfo *new =
            desugarLamTypeConstructorInfo(getLamExp_Constructor(node));
        PROTECT(new);
        result = newMinExp_Constructor(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_DECONSTRUCT: {
        MinDeconstruct *new =
            desugarLamDeconstruct(getLamExp_Deconstruct(node));
        PROTECT(new);
        result = newMinExp_Deconstruct(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_ERROR: {
        result = newMinExp_Error(CPI(node));
        break;
    }
    case LAMEXP_TYPE_IFF: {
        MinIff *new = desugarLamIff(getLamExp_Iff(node));
        PROTECT(new);
        result = newMinExp_Iff(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_LAM: {
        MinLam *new = desugarLamLam(getLamExp_Lam(node));
        PROTECT(new);
        result = newMinExp_Lam(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_LET: {
        MinLet *new = desugarLamLet(getLamExp_Let(node));
        PROTECT(new);
        result = newMinExp_Let(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_LETREC: {
        MinLetRec *new = desugarLamLetRec(getLamExp_LetRec(node));
        PROTECT(new);
        result = newMinExp_LetRec(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_LETSTAR: {
        MinLetStar *new = desugarLamLetStar(getLamExp_LetStar(node));
        PROTECT(new);
        result = newMinExp_LetStar(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_LOOKUP: {
        MinLookUp *new = desugarLamLookUp(getLamExp_LookUp(node));
        PROTECT(new);
        result = newMinExp_LookUp(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_MAKETUPLE: {
        MinArgs *new = desugarLamArgs(getLamExp_MakeTuple(node));
        PROTECT(new);
        result = newMinExp_MakeTuple(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_MAKEVEC: {
        MinMakeVec *new = desugarLamMakeVec(getLamExp_MakeVec(node));
        PROTECT(new);
        result = newMinExp_MakeVec(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_MATCH: {
        MinMatch *new = desugarLamMatch(getLamExp_Match(node));
        PROTECT(new);
        result = newMinExp_Match(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_NAMESPACES: {
        MinNameSpaceArray *new =
            desugarLamNameSpaceArray(getLamExp_NameSpaces(node));
        PROTECT(new);
        result = newMinExp_NameSpaces(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_PRIM: {
        MinPrimApp *new = desugarLamPrimApp(getLamExp_Prim(node));
        PROTECT(new);
        result = newMinExp_Prim(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_PRINT: {
        MinPrint *new = desugarLamPrint(getLamExp_Print(node));
        PROTECT(new);
        result = newMinExp_Print(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_SEQUENCE: {
        MinSequence *new = desugarLamSequence(getLamExp_Sequence(node));
        PROTECT(new);
        result = newMinExp_Sequence(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_STDINT: {
        result = newMinExp_Stdint(CPI(node), getLamExp_Stdint(node));
        break;
    }
    case LAMEXP_TYPE_TAG: {
        MinExp *new = desugarLamExp_internal(getLamExp_Tag(node));
        PROTECT(new);
        result = newMinExp_Tag(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_TUPLEINDEX: {
        MinTupleIndex *new = desugarLamTupleIndex(getLamExp_TupleIndex(node));
        PROTECT(new);
        result = newMinExp_TupleIndex(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_TYPEDEFS: {
        MinTypeDefs *new = desugarLamTypeDefs(getLamExp_TypeDefs(node));
        PROTECT(new);
        result = newMinExp_TypeDefs(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_TYPEOF: {
        MinTypeOf *new = desugarLamTypeOf(getLamExp_TypeOf(node));
        PROTECT(new);
        result = newMinExp_TypeOf(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_VAR: {
        result = newMinExp_Var(CPI(node), getLamExp_Var(node));
        break;
    }
    case LAMEXP_TYPE_ENV: {
        result = newMinExp_Env(CPI(node));
        break;
    }
    default:
        cant_happen("unexpected LamExp type %s", lamExpTypeName(node->type));
    }

    UNPROTECT(save);
    LEAVE(desugarLamExp);
    return result;
}

static MinLookUpOrSymbol *desugarLamLookUpOrSymbol(LamLookUpOrSymbol *node) {
    ENTER(desugarLamLookUpOrSymbol);
    if (node == NULL) {
        LEAVE(desugarLamLookUpOrSymbol);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinLookUpOrSymbol *result = NULL;

    switch (node->type) {
    case LAMLOOKUPORSYMBOL_TYPE_SYMBOL: {
        result = newMinLookUpOrSymbol_Symbol(CPI(node),
                                             getLamLookUpOrSymbol_Symbol(node));
        break;
    }
    case LAMLOOKUPORSYMBOL_TYPE_LOOKUP: {
        // LamLookUpSymbol
        MinLookUpSymbol *new =
            desugarLamLookUpSymbol(getLamLookUpOrSymbol_LookUp(node));
        PROTECT(new);
        result = newMinLookUpOrSymbol_LookUp(CPI(node), new);
        break;
    }
    default:
        cant_happen("unrecognized LamLookUpOrSymbol type %d", node->type);
    }

    UNPROTECT(save);
    LEAVE(desugarLamLookUpOrSymbol);
    return result;
}

static MinCondCases *desugarLamCondCases(LamCondCases *node) {
    ENTER(desugarLamCondCases);
    if (node == NULL) {
        LEAVE(desugarLamCondCases);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinCondCases *result = NULL;

    switch (node->type) {
    case LAMCONDCASES_TYPE_INTEGERS: {
        // LamIntCondCases
        MinIntCondCases *new =
            desugarLamIntCondCases(getLamCondCases_Integers(node));
        PROTECT(new);
        result = newMinCondCases_Integers(CPI(node), new);
        break;
    }
    case LAMCONDCASES_TYPE_CHARACTERS: {
        // LamCharCondCases
        MinCharCondCases *new =
            desugarLamCharCondCases(getLamCondCases_Characters(node));
        PROTECT(new);
        result = newMinCondCases_Characters(CPI(node), new);
        break;
    }
    default:
        cant_happen("unrecognized LamCondCases type %d", node->type);
    }

    UNPROTECT(save);
    LEAVE(desugarLamCondCases);
    return result;
}

static MinTypeConstructorType *
desugarLamTypeConstructorType(LamTypeConstructorType *node) {
    ENTER(desugarLamTypeConstructorType);
    if (node == NULL) {
        LEAVE(desugarLamTypeConstructorType);
        return NULL;
    }

    int save = PROTECT(NULL);
    MinTypeConstructorType *result = NULL;

    switch (node->type) {
    case LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER: {
        result = newMinTypeConstructorType_Integer(CPI(node));
        break;
    }
    case LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER: {
        result = newMinTypeConstructorType_Character(CPI(node));
        break;
    }
    case LAMTYPECONSTRUCTORTYPE_TYPE_VAR: {
        result = newMinTypeConstructorType_Var(
            CPI(node), getLamTypeConstructorType_Var(node));
        break;
    }
    case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION: {
        MinTypeFunction *new =
            desugarLamTypeFunction(getLamTypeConstructorType_Function(node));
        PROTECT(new);
        result = newMinTypeConstructorType_Function(CPI(node), new);
        break;
    }
    case LAMTYPECONSTRUCTORTYPE_TYPE_TUPLE: {
        // LamTypeConstructorArgs
        MinTypeConstructorArgs *new = desugarLamTypeConstructorArgs(
            getLamTypeConstructorType_Tuple(node));
        PROTECT(new);
        result = newMinTypeConstructorType_Tuple(CPI(node), new);
        break;
    }
    default:
        cant_happen("unrecognized LamTypeConstructorType type %d", node->type);
    }

    UNPROTECT(save);
    LEAVE(desugarLamTypeConstructorType);
    return result;
}

static MinNameSpaceArray *desugarLamNameSpaceArray(LamNameSpaceArray *node) {
    ENTER(desugarLamNameSpaceArray);
    if (node == NULL) {
        LEAVE(desugarLamNameSpaceArray);
        return NULL;
    }

    MinNameSpaceArray *result = newMinNameSpaceArray();
    int save = PROTECT(result);

    // Iterate over all elements
    for (Index i = 0; i < node->size; i++) {
        LamExp *element = peeknLamNameSpaceArray(node, i);
        struct MinExp *new = desugarLamExp_internal(element);
        int save2 = PROTECT(new);
        pushMinNameSpaceArray(result, new);
        UNPROTECT(save2);
    }

    UNPROTECT(save);
    LEAVE(desugarLamNameSpaceArray);
    return result;
}

// Public entry point
MinExp *desugarLamExp(LamExp *node) { return desugarLamExp_internal(node); }
