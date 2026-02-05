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
#include "minlam_pp.h"

#include "lambda_desugar.h"
#include "lambda_pp.h"

#ifdef DEBUG_LAMBDA_DESUGAR
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

// Forward declarations
static MinExp *desugarLamPrint(LamExp *);
static MinExp *desugarLamLet(LamExp *);
static MinExp *desugarLamLetStar(LamExp *);
static MinExp *desugarLamTypeOf(LamExp *);
static MinExp *desugarLamConstruct(LamExp *);
static MinExp *desugarLamDeconstruct(LamExp *);
static MinExp *desugarLamConstant(LamExp *);
static MinExp *desugarLamMakeTuple(LamExp *);
static MinExp *desugarLamTag(LamExp *);
static MinExp *desugarLamTupleIndex(LamExp *);
static MinExp *desugarLamTypeDefs(LamExp *);

static MinLam *desugarLamLam(LamLam *node);
static MinVarList *desugarLamVarList(LamVarList *node);
static MinPrimApp *desugarLamPrimApp(LamPrimApp *node);
static MinExprList *desugarLamSequence(LamSequence *node);
static MinExprList *desugarLamArgs(LamArgs *node);
static MinApply *desugarLamApply(LamApply *node);
static MinLookUp *desugarLamLookUp(LamLookUp *node);
static MinExprList *desugarLamMakeVec(LamMakeVec *node);
static MinIff *desugarLamIff(LamIff *node);
static MinCond *desugarLamCond(LamCond *node);
static MinIntCondCases *desugarLamIntCondCases(LamIntCondCases *node);
static MinCharCondCases *desugarLamCharCondCases(LamCharCondCases *node);
static MinMatch *desugarLamMatch(LamMatch *node);
static MinMatchList *desugarLamMatchList(LamMatchList *node);
static MinIntList *desugarLamIntList(LamIntList *node);
static MinLetRec *desugarLamLetRec(LamLetRec *node);
static MinBindings *desugarLamBindings(LamBindings *node);
static MinAmb *desugarLamAmb(LamAmb *node);
static MinCondCases *desugarLamCondCases(LamCondCases *node);
static MinNameSpaceArray *desugarLamNameSpaceArray(LamNameSpaceArray *node);

char *desugar_conversion_function = NULL;
int desugar_flag = 0;

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

    MinExp *new_exp1 = desugarLamExp(node->exp1);
    int save = PROTECT(new_exp1);
    MinExp *new_exp2 = desugarLamExp(node->exp2);
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
    MinExp *body = desugarLamExp(node->exp);
    PROTECT(body);
    MinLam *result = newMinLam(CPI(node), params, body);
    UNPROTECT(save);
    LEAVE(desugarLamLam);
    return result;
}

static MinExprList *desugarLamSequence(LamSequence *node) {
    ENTER(desugarLamSequence);
    if (node == NULL) {
        LEAVE(desugarLamSequence);
        return NULL;
    }

    MinExprList *next = desugarLamSequence(node->next);
    int save = PROTECT(next);
    MinExp *exp = desugarLamExp(node->exp);
    PROTECT(exp);
    MinExprList *result = newMinExprList(CPI(node), exp, next);
    UNPROTECT(save);
    LEAVE(desugarLamSequence);
    return result;
}

static MinExprList *desugarLamArgs(LamArgs *node) {
    ENTER(desugarLamArgs);
    if (node == NULL) {
        LEAVE(desugarLamArgs);
        return NULL;
    }

    MinExprList *next = desugarLamArgs(node->next);
    int save = PROTECT(next);
    MinExp *exp = desugarLamExp(node->exp);
    PROTECT(exp);
    MinExprList *result = newMinExprList(CPI(node), exp, next);
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

    MinExp *function = desugarLamExp(node->function);
    int save = PROTECT(function);
    MinExprList *args = desugarLamArgs(node->args);
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

    MinExp *exp = desugarLamExp(node->exp);
    int save = PROTECT(exp);
    MinLookUp *result = newMinLookUp(CPI(node), node->nsId, exp);
    UNPROTECT(save);
    LEAVE(desugarLamLookUp);
    return result;
}

static MinExp *desugarLamConstant(LamExp *exp) {
    ENTER(desugarLamConstant);
    MinExp *result = newMinExp_Stdint(CPI(exp), getLamExp_Constant(exp)->tag);
    LEAVE(desugarLamConstant);
    return result;
}

static LamMakeVec *constructToMakeVec(LamConstruct *construct) {
    int nArgs = countLamArgs(construct->args);
    LamExp *newArg = newLamExp_Stdint(CPI(construct), construct->tag);
    int save = PROTECT(newArg);
    LamArgs *extraItem = newLamArgs(CPI(construct), newArg, construct->args);
    PROTECT(extraItem);
    LamMakeVec *res = newLamMakeVec(CPI(construct), nArgs + 1, extraItem);
    UNPROTECT(save);
    return res;
}

static MinExp *desugarLamConstruct(LamExp *exp) {
    ENTER(desugarLamConstruct);
    LamMakeVec *makeVec = constructToMakeVec(getLamExp_Construct(exp));
    int save = PROTECT(makeVec);
    MinExprList *newMakeVec = desugarLamMakeVec(makeVec);
    PROTECT(newMakeVec);
    MinExp *result = newMinExp_MakeVec(CPI(exp), newMakeVec);
    UNPROTECT(save);
    return result;
}

static LamPrimApp *deconstructToPrimApp(LamDeconstruct *deconstruct) {
    LamExp *index = newLamExp_Stdint(CPI(deconstruct), deconstruct->vec);
    int save = PROTECT(index);
    LamPrimApp *res = newLamPrimApp(CPI(deconstruct), LAMPRIMOP_TYPE_VEC, index,
                                    deconstruct->exp);
    UNPROTECT(save);
    return res;
}

static MinExp *desugarLamDeconstruct(LamExp *exp) {
    ENTER(desugarLamDeconstruct);
    LamPrimApp *primApp = deconstructToPrimApp(getLamExp_Deconstruct(exp));
    int save = PROTECT(primApp);
    MinPrimApp *newApp = desugarLamPrimApp(primApp);
    PROTECT(newApp);
    MinExp *result = newMinExp_Prim(CPI(exp), newApp);
    UNPROTECT(save);
    LEAVE(desugarLamDeconstruct);
    return result;
}

static LamPrimApp *tupleIndexToPrimApp(LamTupleIndex *tupleIndex) {
    LamExp *index = newLamExp_Stdint(CPI(tupleIndex), tupleIndex->vec);
    int save = PROTECT(index);
    LamPrimApp *res = newLamPrimApp(CPI(tupleIndex), LAMPRIMOP_TYPE_VEC, index,
                                    tupleIndex->exp);
    UNPROTECT(save);
    return res;
}

static MinExp *desugarLamTupleIndex(LamExp *exp) {
    ENTER(desugarLamTupleIndex);
    LamPrimApp *app = tupleIndexToPrimApp(getLamExp_TupleIndex(exp));
    int save = PROTECT(app);
    MinPrimApp *newApp = desugarLamPrimApp(app);
    PROTECT(newApp);
    MinExp *result = newMinExp_Prim(CPI(exp), newApp);
    UNPROTECT(save);
    LEAVE(desugarLamTupleIndex);
    return result;
}

static MinExprList *desugarLamMakeVec(LamMakeVec *node) {
    ENTER(desugarLamMakeVec);
    if (node == NULL) {
        LEAVE(desugarLamMakeVec);
        return NULL;
    }

    MinExprList *result = desugarLamArgs(node->args);
    LEAVE(desugarLamMakeVec);
    return result;
}

static MinIff *desugarLamIff(LamIff *node) {
    ENTER(desugarLamIff);
    if (node == NULL) {
        LEAVE(desugarLamIff);
        return NULL;
    }

    MinExp *condition = desugarLamExp(node->condition);
    int save = PROTECT(condition);
    MinExp *consequent = desugarLamExp(node->consequent);
    PROTECT(consequent);
    MinExp *alternative = desugarLamExp(node->alternative);
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

    MinExp *value = desugarLamExp(node->value);
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

    MinExp *body = desugarLamExp(node->body);
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

    MinExp *body = desugarLamExp(node->body);
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

    MinExp *index = desugarLamExp(node->index);
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
    MinExp *body = desugarLamExp(node->body);
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
    MinIntList *result = newMinIntList(CPI(node), node->item, next);
    UNPROTECT(save);
    LEAVE(desugarLamIntList);
    return result;
}

static MinVarList *extractKeysFromBindings(MinBindings *bindings) {
    ENTER(extractKeysFromBindings);
    if (bindings == NULL) {
        LEAVE(extractKeysFromBindings);
        return NULL;
    }
    MinVarList *next = extractKeysFromBindings(bindings->next);
    int save = PROTECT(next);
    MinVarList *result = newMinVarList(CPI(bindings), bindings->var, next);
    UNPROTECT(save);
    LEAVE(extractKeysFromBindings);
    return result;
}

static MinExprList *extractValuesFromBindings(MinBindings *bindings) {
    ENTER(extractValuesFromBindings);
    if (bindings == NULL) {
        LEAVE(extractValuesFromBindings);
        return NULL;
    }
    MinExprList *next = extractValuesFromBindings(bindings->next);
    int save = PROTECT(next);
    MinExprList *result = newMinExprList(CPI(bindings), bindings->val, next);
    UNPROTECT(save);
    LEAVE(extractValuesFromBindings);
    return result;
}

static MinExp *desugarLamLet(LamExp *exp) {
    ENTER(desugarLamLet);
    LamLet *node = getLamExp_Let(exp);
    MinBindings *bindings = desugarLamBindings(node->bindings);
    int save = PROTECT(bindings);
    MinExp *body = desugarLamExp(node->body);
    PROTECT(body);
    MinVarList *fargs = extractKeysFromBindings(bindings);
    PROTECT(fargs);
    MinExp *lambda = makeMinExp_Lam(CPI(node), fargs, body);
    PROTECT(lambda);
    MinExprList *aargs = extractValuesFromBindings(bindings);
    PROTECT(aargs);
    MinExp *result = makeMinExp_Apply(CPI(node), lambda, aargs);
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
    MinExp *body = desugarLamExp(node->body);
    PROTECT(body);
    MinLetRec *result = newMinLetRec(CPI(node), bindings, body);
    UNPROTECT(save);
    LEAVE(desugarLamLetRec);
    return result;
}

static LamExp *nestLets(LamBindings *bindings, LamExp *body) {
    ENTER(nestLets);
    if (bindings == NULL) {
        LEAVE(nestLets);
        return body;
    }
    LamExp *rest = nestLets(bindings->next, body);
    int save = PROTECT(rest);
    LamBindings *single_binding =
        newLamBindings(CPI(bindings), bindings->var, bindings->val, NULL);
    PROTECT(single_binding);
    LamExp *let = makeLamExp_Let(CPI(bindings), single_binding, rest);
    UNPROTECT(save);
    LEAVE(nestLets);
    return let;
}

static MinExp *desugarLamLetStar(LamExp *exp) {
    ENTER(desugarLamLetStar);
    LamLetStar *node = getLamExp_LetStar(exp);
    // build a nest of lets, then desugar that
    LamExp *lets = nestLets(node->bindings, node->body);
    int save = PROTECT(lets);
    MinExp *result = desugarLamExp(lets);
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

    MinExp *val = desugarLamExp(node->val);
    int save = PROTECT(val);
    MinBindings *next = desugarLamBindings(node->next);
    PROTECT(next);
    MinBindings *result = newMinBindings(CPI(node), node->var, val, next);
    if (desugar_conversion_function != NULL &&
        strcmp(node->var->name, desugar_conversion_function) == 0) {
        ppMinExp(result->val);
        eprintf("\n");
        exit(0);
    }
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

    MinExp *left = desugarLamExp(node->left);
    int save = PROTECT(left);
    MinExp *right = desugarLamExp(node->right);
    PROTECT(right);
    MinAmb *result = newMinAmb(CPI(node), left, right);
    UNPROTECT(save);
    LEAVE(desugarLamAmb);
    return result;
}

static MinExp *desugarLamTypeOf(LamExp *exp) {
    ENTER(desugarLamTypeOf);
    LamTypeOf *node = getLamExp_TypeOf(exp);
    MinExp *result = desugarLamExp(node->typeString);
    LEAVE(desugarLamTypeOf);
    return result;
}

static MinExp *desugarLamTypeDefs(LamExp *exp) {
    ENTER(desugarLamTypeDefs);
    MinExp *result = desugarLamExp(getLamExp_TypeDefs(exp)->body);
    LEAVE(desugarLamTypeDefs);
    return result;
}

static MinExp *desugarLamPrint(LamExp *node) {
    MinExp *printer = desugarLamExp(getLamExp_Print(node)->printer);
    int save = PROTECT(printer);
    MinExp *arg = desugarLamExp(getLamExp_Print(node)->exp);
    PROTECT(arg);
    MinExprList *args = newMinExprList(CPI(node), arg, NULL);
    PROTECT(args);
    MinExp *result = makeMinExp_Apply(CPI(node), printer, args);
    UNPROTECT(save);
    return result;
}

static LamMakeVec *tupleToMakeVec(ParserInfo PI, LamArgs *tuple) {
    int nArgs = countLamArgs(tuple);
    LamMakeVec *res = newLamMakeVec(PI, nArgs, tuple);
    return res;
}

static MinExp *desugarLamMakeTuple(LamExp *exp) {
    ENTER(desugarLamMakeTuple);
    LamMakeVec *makeVec = tupleToMakeVec(CPI(exp), getLamExp_MakeTuple(exp));
    int save = PROTECT(makeVec);
    MinExprList *minMakeVec = desugarLamMakeVec(makeVec);
    PROTECT(minMakeVec);
    MinExp *result = newMinExp_MakeVec(CPI(exp), minMakeVec);
    UNPROTECT(save);
    LEAVE(desugarLamMakeTuple);
    return result;
}

static LamPrimApp *tagToPrimApp(LamExp *tagged) {
    LamExp *zero_index = newLamExp_Stdint(CPI(tagged), 0);
    int save = PROTECT(zero_index);
    LamPrimApp *res =
        newLamPrimApp(CPI(tagged), LAMPRIMOP_TYPE_VEC, zero_index, tagged);
    UNPROTECT(save);
    return res;
}

static MinExp *desugarLamTag(LamExp *exp) {
    ENTER(desugarLamTag);
    LamPrimApp *primApp = tagToPrimApp(getLamExp_Tag(exp));
    int save = PROTECT(primApp);
    MinPrimApp *newApp = desugarLamPrimApp(primApp);
    PROTECT(newApp);
    MinExp *result = newMinExp_Prim(CPI(exp), newApp);
    UNPROTECT(save);
    LEAVE(desugarLamTag);
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
        struct MinExp *new = desugarLamExp(element);
        int save2 = PROTECT(new);
        pushMinNameSpaceArray(result, new);
        UNPROTECT(save2);
    }

    UNPROTECT(save);
    LEAVE(desugarLamNameSpaceArray);
    return result;
}

// Main desugaring function and public interface
MinExp *desugarLamExp(LamExp *node) {
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
        MinExprList *new = desugarLamArgs(getLamExp_Args(node));
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
        MinExp *new_callcc = desugarLamExp(getLamExp_CallCC(node));
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
    case LAMEXP_TYPE_CONSTANT:
        result = desugarLamConstant(node);
        break;
    case LAMEXP_TYPE_CONSTRUCT:
        result = desugarLamConstruct(node);
        break;
    case LAMEXP_TYPE_DECONSTRUCT:
        result = desugarLamDeconstruct(node);
        break;
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
    case LAMEXP_TYPE_LET:
        result = desugarLamLet(node);
        break;
    case LAMEXP_TYPE_LETREC: {
        MinLetRec *new = desugarLamLetRec(getLamExp_LetRec(node));
        PROTECT(new);
        result = newMinExp_LetRec(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_LETSTAR:
        result = desugarLamLetStar(node);
        break;
    case LAMEXP_TYPE_LOOKUP: {
        MinLookUp *new = desugarLamLookUp(getLamExp_LookUp(node));
        PROTECT(new);
        result = newMinExp_LookUp(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_MAKETUPLE:
        result = desugarLamMakeTuple(node);
        break;
    case LAMEXP_TYPE_MAKEVEC: {
        MinExprList *new = desugarLamMakeVec(getLamExp_MakeVec(node));
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
        // Check if the type checker created a replacement (bespoke comparator)
        LamPrimApp *prim = getLamExp_Prim(node);
        if (prim->replacement != NULL) {
            // Use the replacement instead of the primitive
            result = desugarLamExp(prim->replacement);
        } else {
            MinPrimApp *new = desugarLamPrimApp(prim);
            PROTECT(new);
            result = newMinExp_Prim(CPI(node), new);
        }
        break;
    }
    case LAMEXP_TYPE_PRINT:
        result = desugarLamPrint(node);
        break;
    case LAMEXP_TYPE_SEQUENCE: {
        MinExprList *new = desugarLamSequence(getLamExp_Sequence(node));
        PROTECT(new);
        result = newMinExp_Sequence(CPI(node), new);
        break;
    }
    case LAMEXP_TYPE_STDINT:
        result = newMinExp_Stdint(CPI(node), getLamExp_Stdint(node));
        break;
    case LAMEXP_TYPE_TAG:
        result = desugarLamTag(node);
        break;
    case LAMEXP_TYPE_TUPLEINDEX:
        result = desugarLamTupleIndex(node);
        break;
    case LAMEXP_TYPE_TYPEDEFS:
        result = desugarLamTypeDefs(node);
        break;
    case LAMEXP_TYPE_TYPEOF:
        result = desugarLamTypeOf(node);
        break;
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