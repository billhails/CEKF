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

#include <assert.h>

#include "tc_analyze.h"
#include "symbols.h"
#include "symbol.h"
#include "memory.h"
#include "hash.h"
#include "tc_debug.h"
#include "tc_helper.h"
#include "print_compiler.h"
#include "lambda_pp.h"

#ifdef DEBUG_TC
#  include "debugging_on.h"
#  include "lambda_pp.h"
#else
#  include "debugging_off.h"
#endif

static TcEnv *extendEnv(TcEnv *parent);
static TcNg *extendNg(TcNg *parent);
static void addToEnv(TcEnv *env, HashSymbol *key, TcType *type);
static void addToNg(TcNg *env, TcType *type);
static void addFreshVarToEnv(TcEnv *env, HashSymbol *key);
static void addCmpToEnv(TcEnv *env, HashSymbol *key);
static TcType *makeBoolean(void);
static TcType *makeSpaceship(void);
static TcType *makeSmallInteger(void);
static TcType *makeBigInteger(void);
static TcType *makeCharacter(void);
static TcType *makeUnknown(HashSymbol *var);
static TcType *makeFreshVar(char *name __attribute__((unused)));
static TcType *makeVar(HashSymbol *t);
static TcType *makeFn(TcType *arg, TcType *result);
static TcType *makeTuple(int size);
static void addBoolBinOpToEnv(TcEnv *env, HashSymbol *symbol);
static void addHereToEnv(TcEnv *env);
static void addIfToEnv(TcEnv *env);
static void addIntBinOpToEnv(TcEnv *env, HashSymbol *symbol);
static void addNegToEnv(TcEnv *env);
static void addNotToEnv(TcEnv *env);
static void addPutcToEnv(TcEnv *env);
static void addThenToEnv(TcEnv *env);
static TcType *analyzeExp(LamExp *exp, TcEnv *env, TcNg *ng);
static TcType *analyzeLam(LamLam *lam, TcEnv *env, TcNg *ng);
static TcType *analyzeVar(HashSymbol *var, TcEnv *env, TcNg *ng);
static TcType *analyzeSmallInteger();
static TcType *analyzeBigInteger();
static TcType *analyzePrim(LamPrimApp *app, TcEnv *env, TcNg *ng);
static TcType *analyzeUnary(LamUnaryApp *app, TcEnv *env, TcNg *ng);
static TcType *analyzeSequence(LamSequence *sequence, TcEnv *env, TcNg *ng);
static TcType *analyzeConstruct(LamConstruct *construct, TcEnv *env,
                                TcNg *ng);
static TcType *analyzeDeconstruct(LamDeconstruct *deconstruct, TcEnv *env,
                                  TcNg *ng);
static TcType *analyzeTag(LamExp *tag, TcEnv *env, TcNg *ng);
static TcType *analyzeConstant(LamConstant *constant, TcEnv *env, TcNg *ng);
static TcType *analyzeApply(LamApply *apply, TcEnv *env, TcNg *ng);
static TcType *analyzeIff(LamIff *iff, TcEnv *env, TcNg *ng);
static TcType *analyzeCallCC(LamExp *called, TcEnv *env, TcNg *ng);
static TcType *analyzePrint(LamPrint *print, TcEnv *env, TcNg *ng);
static TcType *analyzeLetRec(LamLetRec *letRec, TcEnv *env, TcNg *ng);
static TcType *analyzeTypeDefs(LamTypeDefs *typeDefs, TcEnv *env, TcNg *ng);
static TcType *analyzeLet(LamLet *let, TcEnv *env, TcNg *ng);
static TcType *analyzeMatch(LamMatch *match, TcEnv *env, TcNg *ng);
static TcType *analyzeCond(LamCond *cond, TcEnv *env, TcNg *ng);
static TcType *analyzeAnd(LamAnd *and, TcEnv *env, TcNg *ng);
static TcType *analyzeOr(LamOr *or, TcEnv *env, TcNg *ng);
static TcType *analyzeAmb(LamAmb *amb, TcEnv *env, TcNg *ng);
static TcType *analyzeTupleIndex(LamTupleIndex *index, TcEnv *env, TcNg *ng);
static TcType *analyzeMakeTuple(LamList *tuple, TcEnv *env, TcNg *ng);
static TcType *analyzeCharacter();
static TcType *analyzeBack();
static TcType *analyzeError();
static bool unify(TcType *a, TcType *b, char *trace __attribute__((unused)));
static TcType *prune(TcType *t);
static bool occursInType(TcType *a, TcType *b);
static bool occursIn(TcType *a, TcType *b);
static bool sameType(TcType *a, TcType *b);
static TcType *analyzeBigIntegerExp(LamExp *exp, TcEnv *env, TcNg *ng);
static TcType *analyzeSmallIntegerExp(LamExp *exp, TcEnv *env, TcNg *ng)
    __attribute__((unused));
static TcType *analyzeBooleanExp(LamExp *exp, TcEnv *env, TcNg *ng);
static TcType *analyzeCharacterExp(LamExp *exp, TcEnv *env, TcNg *ng);
static TcType *freshRec(TcType *type, TcNg *ng, TcTypeTable *map);
static TcType *lookup(TcEnv *env, HashSymbol *symbol, TcNg *ng);
static TcType *makeUserType(HashSymbol *name, TcUserTypeArgs *args);

static int id_counter = 0;

TcEnv *tc_init(void) {
    TcEnv *env = extendEnv(NULL);
    int save = PROTECT(env);
    addBoolBinOpToEnv(env, andSymbol());
    addBoolBinOpToEnv(env, orSymbol());
    addBoolBinOpToEnv(env, xorSymbol());
    addCmpToEnv(env, eqSymbol());
    addCmpToEnv(env, geSymbol());
    addCmpToEnv(env, gtSymbol());
    addCmpToEnv(env, leSymbol());
    addCmpToEnv(env, ltSymbol());
    addCmpToEnv(env, neSymbol());
    addFreshVarToEnv(env, backSymbol());
    addFreshVarToEnv(env, errorSymbol());
    addHereToEnv(env);
    addIfToEnv(env);
    addIntBinOpToEnv(env, addSymbol());
    addIntBinOpToEnv(env, divSymbol());
    addIntBinOpToEnv(env, mulSymbol());
    addIntBinOpToEnv(env, powSymbol());
    addIntBinOpToEnv(env, subSymbol());
    addNegToEnv(env);
    addNotToEnv(env);
    addPutcToEnv(env);
    addThenToEnv(env);
    UNPROTECT(save);
    return env;
}

TcType *tc_analyze(LamExp *exp, TcEnv *env) {
    TcNg *ng = extendNg(NULL);
    int save = PROTECT(ng);
    TcType *res = analyzeExp(exp, env, ng);
    UNPROTECT(save);
    return res;
}

static TcType *analyzeExp(LamExp *exp, TcEnv *env, TcNg *ng) {
    if (exp == NULL)
        return NULL;
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            return prune(analyzeLam(exp->val.lam, env, ng));
        case LAMEXP_TYPE_VAR:
            return prune(analyzeVar(exp->val.var, env, ng));
        case LAMEXP_TYPE_STDINT:
            return prune(analyzeSmallInteger());
        case LAMEXP_TYPE_BIGINTEGER:
            return prune(analyzeBigInteger());
        case LAMEXP_TYPE_PRIM:
            return prune(analyzePrim(exp->val.prim, env, ng));
        case LAMEXP_TYPE_UNARY:
            return prune(analyzeUnary(exp->val.unary, env, ng));
        case LAMEXP_TYPE_LIST:
            return prune(analyzeSequence(exp->val.list, env, ng));
        case LAMEXP_TYPE_MAKEVEC:
            cant_happen("encountered make-vec in analyzeExp");
        case LAMEXP_TYPE_CONSTRUCT:
            return prune(analyzeConstruct(exp->val.construct, env, ng));
        case LAMEXP_TYPE_DECONSTRUCT:
            return prune(analyzeDeconstruct(exp->val.deconstruct, env, ng));
        case LAMEXP_TYPE_TAG:
            return prune(analyzeTag(exp->val.tag, env, ng));
        case LAMEXP_TYPE_CONSTANT:
            return prune(analyzeConstant(exp->val.constant, env, ng));
        case LAMEXP_TYPE_APPLY:
            return prune(analyzeApply(exp->val.apply, env, ng));
        case LAMEXP_TYPE_IFF:
            return prune(analyzeIff(exp->val.iff, env, ng));
        case LAMEXP_TYPE_CALLCC:
            return prune(analyzeCallCC(exp->val.callcc, env, ng));
        case LAMEXP_TYPE_PRINT:
            return prune(analyzePrint(exp->val.print, env, ng));
        case LAMEXP_TYPE_LETREC:
            return prune(analyzeLetRec(exp->val.letrec, env, ng));
        case LAMEXP_TYPE_TYPEDEFS:
            return prune(analyzeTypeDefs(exp->val.typedefs, env, ng));
        case LAMEXP_TYPE_LET:
            return prune(analyzeLet(exp->val.let, env, ng));
        case LAMEXP_TYPE_MATCH:
            return prune(analyzeMatch(exp->val.match, env, ng));
        case LAMEXP_TYPE_COND:
            return prune(analyzeCond(exp->val.cond, env, ng));
        case LAMEXP_TYPE_AND:
            return prune(analyzeAnd(exp->val.and, env, ng));
        case LAMEXP_TYPE_OR:
            return prune(analyzeOr(exp->val.or, env, ng));
        case LAMEXP_TYPE_AMB:
            return prune(analyzeAmb(exp->val.amb, env, ng));
        case LAMEXP_TYPE_CHARACTER:
            return prune(analyzeCharacter());
        case LAMEXP_TYPE_BACK:
            return prune(analyzeBack());
        case LAMEXP_TYPE_ERROR:
            return prune(analyzeError());
        case LAMEXP_TYPE_TUPLE_INDEX:
            return prune(analyzeTupleIndex(exp->val.tuple_index, env, ng));
        case LAMEXP_TYPE_MAKE_TUPLE:
            return prune(analyzeMakeTuple(exp->val.make_tuple, env, ng));
        case LAMEXP_TYPE_COND_DEFAULT:
            cant_happen("encountered cond default in analyzeExp");
        default:
            cant_happen("unrecognized type %s", lamExpTypeName(exp->type));
    }
}

static TcType *makeFunctionType(LamVarList *args, TcEnv *env,
                                TcType *returnType) {
    // ENTER(makeFunctionType);
    if (args == NULL) {
        // LEAVE(makeFunctionType);
        return returnType;
    }
    TcType *next = makeFunctionType(args->next, env, returnType);
    int save = PROTECT(next);
    TcType *this = NULL;
    if (!getFromTcEnv(env, args->var, &this)) {
        cant_happen("cannot find var in env in makeFunctionType");
    }
    TcType *ret = makeFn(this, next);
    UNPROTECT(save);
    // LEAVE(makeFunctionType);
    return ret;
}

static TcType *analyzeLam(LamLam *lam, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeLam);
    env = extendEnv(env);
    int save = PROTECT(env);
    ng = extendNg(ng);
    PROTECT(ng);
    for (LamVarList *args = lam->args; args != NULL; args = args->next) {
        TcType *freshType = makeFreshVar(args->var->name);
        int save2 = PROTECT(freshType);
        addToEnv(env, args->var, freshType);
        addToNg(ng, freshType);
        UNPROTECT(save2);
    }
    TcType *returnType = analyzeExp(lam->exp, env, ng);
    PROTECT(returnType);
    TcType *functionType = makeFunctionType(lam->args, env, returnType);
    UNPROTECT(save);
    // LEAVE(analyzeLam);
    return functionType;
}

static TcType *analyzeVar(HashSymbol *var, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeVar);
    TcType *res = lookup(env, var, ng);
    if (res == NULL) {
        can_happen("undefined variable %s in analyzeVar", var->name);
        return makeUnknown(var);
    }
    // LEAVE(analyzeVar);
    return res;
}

static TcType *analyzeSmallInteger() {
    return makeSmallInteger();
}

static TcType *analyzeBigInteger() {
    // ENTER(analyzeBigInteger);
    TcType *res = makeBigInteger();
    // LEAVE(analyzeBigInteger);
    return res;
}

static TcType *analyzeBinaryArith(LamExp *exp1, LamExp *exp2, TcEnv *env,
                                  TcNg *ng) {
    // ENTER(analyzeBinaryArith);
    (void) analyzeBigIntegerExp(exp1, env, ng);
    TcType *res = analyzeBigIntegerExp(exp2, env, ng);
    // LEAVE(analyzeBinaryArith);
    return res;
}

static TcType *analyzeUnaryArith(LamExp *exp, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeBinaryArith);
    TcType *res = analyzeBigIntegerExp(exp, env, ng);
    // LEAVE(analyzeBinaryArith);
    return res;
}

static TcType *analyzeComparison(LamExp *exp1, LamExp *exp2, TcEnv *env,
                                 TcNg *ng) {
    // ENTER(analyzeComparison);
    TcType *type1 = analyzeExp(exp1, env, ng);
    int save = PROTECT(type1);
    TcType *type2 = analyzeExp(exp2, env, ng);
    PROTECT(type2);
    if (!unify(type1, type2, "comparison")) {
        eprintf("while unifying comparison:\n");
        ppLamExp(exp1);
        eprintf("\nwith\n");
        ppLamExp(exp1);
        eprintf("\n");
    }
    UNPROTECT(save);
    TcType *res = makeBoolean();
    // LEAVE(analyzeComparison);
    return res;
}

static TcType *analyzeSpaceship(LamExp *exp1, LamExp *exp2, TcEnv *env,
                               TcNg *ng) {
    // ENTER(analyzeComparison);
    TcType *type1 = analyzeExp(exp1, env, ng);
    int save = PROTECT(type1);
    TcType *type2 = analyzeExp(exp2, env, ng);
    PROTECT(type2);
    if (!unify(type1, type2, "<=>")) {
        eprintf("while unifying <=>:\n");
        ppLamExp(exp1);
        eprintf("\nwith\n");
        ppLamExp(exp1);
        eprintf("\n");
    }
    UNPROTECT(save);
    TcType *res = makeSpaceship();
    // LEAVE(analyzeComparison);
    return res;
}

static TcType *analyzeBinaryBool(LamExp *exp1, LamExp *exp2, TcEnv *env,
                                 TcNg *ng) {
    // ENTER(analyzeBinaryBool);
    (void) analyzeBooleanExp(exp1, env, ng);
    TcType *res = analyzeBooleanExp(exp2, env, ng);
    // LEAVE(analyzeBinaryBool);
    return res;
}

static TcType *analyzeUnaryBool(LamExp *exp, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeUnaryBool);
    TcType *res = analyzeBooleanExp(exp, env, ng);
    // LEAVE(analyzeUnaryBool);
    return res;
}

static TcType *analyzeUnaryChar(LamExp *exp, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeUnaryChar);
    TcType *res = analyzeCharacterExp(exp, env, ng);
    // LEAVE(analyzeUnaryChar);
    return res;
}

static TcType *analyzePrim(LamPrimApp *app, TcEnv *env, TcNg *ng) {
    // ENTER(analyzePrim);
    TcType *res = NULL;
    switch (app->type) {
        case LAMPRIMOP_TYPE_ADD:
        case LAMPRIMOP_TYPE_SUB:
        case LAMPRIMOP_TYPE_MUL:
        case LAMPRIMOP_TYPE_DIV:
        case LAMPRIMOP_TYPE_MOD:
        case LAMPRIMOP_TYPE_POW:
            res = analyzeBinaryArith(app->exp1, app->exp2, env, ng);
            break;
        case LAMPRIMOP_TYPE_EQ:
        case LAMPRIMOP_TYPE_NE:
        case LAMPRIMOP_TYPE_GT:
        case LAMPRIMOP_TYPE_LT:
        case LAMPRIMOP_TYPE_GE:
        case LAMPRIMOP_TYPE_LE:
            res = analyzeComparison(app->exp1, app->exp2, env, ng);
            break;
        case LAMPRIMOP_TYPE_CMP:
            res = analyzeSpaceship(app->exp1, app->exp2, env, ng);
            break;
        case LAMPRIMOP_TYPE_VEC:
            cant_happen("encountered VEC in analyzePrim");
            break;
        case LAMPRIMOP_TYPE_XOR:
            res = analyzeBinaryBool(app->exp1, app->exp2, env, ng);
            break;
        default:
            cant_happen("unrecognised type %d in analyzePrim", app->type);
    }
    // LEAVE(analyzePrim);
    return res;
}

static TcType *analyzeUnary(LamUnaryApp *app, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeUnary);
    TcType *res = NULL;
    switch (app->type) {
        case LAMUNARYOP_TYPE_NEG:
        case LAMUNARYOP_TYPE_PUTN:
            res = analyzeUnaryArith(app->exp, env, ng);
            break;
        case LAMUNARYOP_TYPE_NOT:
            res = analyzeUnaryBool(app->exp, env, ng);
            break;
        case LAMUNARYOP_TYPE_PUTC:
            res = analyzeUnaryChar(app->exp, env, ng);
            break;
        case LAMUNARYOP_TYPE_PUTV:
            res = analyzeExp(app->exp, env, ng);
            break;
        default:
            cant_happen("unrecognized type %d in analyzeUnary", app->type);
    }
    // LEAVE(analyzeUnary);
    return res;
}

static TcType *analyzeSequence(LamSequence *sequence, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeSequence);
    if (sequence == NULL) {
        cant_happen("NULL sequence in analyzeSequence");
    }
    TcType *type = analyzeExp(sequence->exp, env, ng);
    if (sequence->next != NULL) {
        TcType *res = analyzeSequence(sequence->next, env, ng);
        // LEAVE(analyzeSequence);
        return res;
    }
    // LEAVE(analyzeSequence);
    return type;
}

static LamApply *constructToApply(LamConstruct *construct) {
    // ENTER(constructToApply);
    LamExp *constructor =
        newLamExp(LAMEXP_TYPE_VAR, LAMEXP_VAL_VAR(construct->name));
    int save = PROTECT(constructor);
    LamApply *apply = newLamApply(constructor, countLamList(construct->args),
                                  construct->args);
    UNPROTECT(save);
    // LEAVE(constructToApply);
    return apply;
}

static TcType *analyzeConstruct(LamConstruct *construct, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeConstruct);
    LamApply *apply = constructToApply(construct);
    int save = PROTECT(apply);
    TcType *res = analyzeApply(apply, env, ng);
    UNPROTECT(save);
    // LEAVE(analyzeConstruct);
    return res;
}

static TcType *findNthArg(int n, TcType *fn) {
    if (fn == NULL) {
        cant_happen("findNthArg hit NULL");
    }
    if (fn->type != TCTYPE_TYPE_FUNCTION) {
        cant_happen("findNthArg given non-function type %d", fn->type);
    }
    if (n == 0) {
        return fn->val.function->arg;
    }
    TcType *res = findNthArg(n - 1, fn->val.function->result);
    return res;
}

static TcType *findResultType(TcType *fn) {
    if (fn == NULL) {
        cant_happen("findResultType hit NULL");
    }
    if (fn->type != TCTYPE_TYPE_FUNCTION) {
        return fn;
    }
    TcType *res = findResultType(fn->val.function->result);
    return res;
}

static TcType *analyzeDeconstruct(LamDeconstruct *deconstruct, TcEnv *env,
                                  TcNg *ng) {
    // ENTER(analyzeDeconstruct);
    TcType *constructor = lookup(env, deconstruct->name, ng);
    int save = PROTECT(constructor);
    if (constructor == NULL) {
        can_happen("undefined type deconstructor %s",
                   deconstruct->name->name);
        TcType *res = makeFreshVar(deconstruct->name->name);
        // LEAVE(analyzeDeconstruct);
        return res;
    }
    TcType *fieldType = findNthArg(deconstruct->vec - 1, constructor);
    TcType *resultType = findResultType(constructor);
    TcType *expType = analyzeExp(deconstruct->exp, env, ng);
    PROTECT(expType);
    if (!unify(expType, resultType, "deconstruct")) {
        eprintf("while unifying deconstruct:\n");
        ppLamDeconstruct(deconstruct);
        eprintf("\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeDeconstruct);
    return fieldType;
}

static TcType *analyzeTupleIndex(LamTupleIndex *index, TcEnv *env, TcNg *ng) {
    TcType *tuple = analyzeExp(index->exp, env, ng);
    int save = PROTECT(tuple);
    TcType *template = makeTuple(index->size);
    PROTECT(template);
    if (!unify(tuple, template, "tuple index")) {
        eprintf("while analyzing tuple ");
        ppTcType(tuple);
        HashSymbol *name = newSymbol("tuple");
        UNPROTECT(save);
        return makeUnknown(name);
    }
    UNPROTECT(save);
    return template->val.tuple->entries[index->vec];
}

static TcType *analyzeMakeTuple(LamList *tuple, TcEnv *env, TcNg *ng) {
    TcTypeArray *values = newTcTypeArray();
    int save = PROTECT(values);
    while (tuple != NULL) {
        TcType *part = analyzeExp(tuple->exp, env, ng);
        int save2 = PROTECT(part);
        pushTcTypeArray(values, part);
        UNPROTECT(save2);
        tuple = tuple->next;
    }
    TcType *res = newTcType(TCTYPE_TYPE_TUPLE, TCTYPE_VAL_TUPLE(values));
    UNPROTECT(save);
    return res;
}

static TcType *analyzeTag(LamExp *tagged, TcEnv *env, TcNg *ng) {
    return analyzeExp(tagged, env, ng);
}

static TcType *analyzeConstant(LamConstant *constant, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeConstant);
    TcType *constType = lookup(env, constant->name, ng);
    if (constType == NULL) {
        can_happen("undefined constant %s", constant->name->name);
        TcType *res = makeFreshVar("err");
        // LEAVE(analyzeConstant);
        return res;
    }
    // LEAVE(analyzeConstant);
    return constType;
}

// apply(fn) => fn
// apply(fn, arg_1, arg_2, arg_3) => apply(apply(apply(fn, arg1), arg_2), arg_3)
static LamApply *curryLamApplyHelper(int nargs, LamExp *function,
                                     LamList *args) {
    if (nargs == 1) {
        LamApply *res = newLamApply(function, 1, args);
        return res;
    }
    LamList *singleArg = newLamList(args->exp, NULL);
    int save = PROTECT(singleArg);
    LamApply *new = newLamApply(function, 1, singleArg);
    PROTECT(new);
    LamExp *newFunction = newLamExp(LAMEXP_TYPE_APPLY, LAMEXP_VAL_APPLY(new));
    PROTECT(newFunction);
    LamApply *curried =
        curryLamApplyHelper(nargs - 1, newFunction, args->next);
    UNPROTECT(save);
    return curried;
}

static LamApply *curryLamApply(LamApply *apply) {
    return curryLamApplyHelper(apply->nargs, apply->function, apply->args);
}

static TcType *analyzeApply(LamApply *apply, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeApply);
    switch (apply->nargs) {
        case 0:{
                TcType *res = analyzeExp(apply->function, env, ng);
                // LEAVE(analyzeApply);
                return res;
            }
        case 1:{
                // fn :: #a -> #b
                TcType *fn = analyzeExp(apply->function, env, ng);
                int save = PROTECT(fn);
                // arg :: #c
                TcType *arg = analyzeExp(apply->args->exp, env, ng);
                PROTECT(arg);
                // res :: #d
                TcType *res = makeFreshVar("apply");
                PROTECT(res);
                // functionType :: #c -> #d
                TcType *functionType = makeFn(arg, res);
                PROTECT(functionType);
                // unify(#a -> #b, #c -> #d)
                if (!unify(fn, functionType, "apply")) {
                    eprintf("while analyzing apply ");
                    ppLamExp(apply->function);
                    eprintf(" to ");
                    ppLamExp(apply->args->exp);
                    eprintf("\n");
                }
                UNPROTECT(save);
                // LEAVE(analyzeApply);
                res = prune(res);
                // #d/#b
                return res;
            }
        default:{
                LamApply *curried = curryLamApply(apply);
                int save = PROTECT(curried);
                TcType *res = analyzeApply(curried, env, ng);
                UNPROTECT(save);
                // LEAVE(analyzeApply);
                return res;
            }
    }
}

static TcType *analyzeIff(LamIff *iff, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeIff);
    (void) analyzeBooleanExp(iff->condition, env, ng);
    TcType *consequent = analyzeExp(iff->consequent, env, ng);
    int save = PROTECT(consequent);
    TcType *alternative = analyzeExp(iff->alternative, env, ng);
    PROTECT(alternative);
    if (!unify(consequent, alternative, "iff")) {
        eprintf("while unifying consequent:\n");
        ppLamExp(iff->consequent);
        eprintf("\nwith alternative:\n");
        ppLamExp(iff->alternative);
        eprintf("\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeIff);
    return consequent;
}

static TcType *analyzeCallCC(LamExp *called, TcEnv *env, TcNg *ng) {
    // 'call/cc' is ((a -> b) -> a) -> a
    TcType *a = makeFreshVar("callccA");
    int save = PROTECT(a);
    TcType *b = makeFreshVar("callccB");
    PROTECT(b);
    TcType *ab = makeFn(a, b);
    PROTECT(ab);
    TcType *aba = makeFn(ab, a);
    PROTECT(aba);
    TcType *calledType = analyzeExp(called, env, ng);
    PROTECT(calledType);
    if (!unify(calledType, aba, "call/cc")) {
        eprintf("while unifying call/cc:\n");
        ppLamExp(called);
        eprintf("\n");
    }
    UNPROTECT(save);
    return a;
}

static TcType *analyzePrint(LamPrint *print, TcEnv *env, TcNg *ng) {
    // a -> a, but installs a printer for type a
    // ENTER(analyzePrint);
    TcType *type = analyzeExp(print->exp, env, ng);
    int save = PROTECT(type);
    print->printer = compilePrinterForType(type, env);
    UNPROTECT(save);
    // LEAVE(analyzePrint);
    IFDEBUG(ppTcType(type));
    return type;
}

static bool isLambdaBinding(LamLetRecBindings *bindings) {
    return bindings->val->type == LAMEXP_TYPE_LAM;
}

static void prepareLetRecEnv(LamLetRecBindings *bindings, TcEnv *env) {
    TcType *freshType = makeFreshVar(bindings->var->name);
    int save = PROTECT(freshType);
    addToEnv(env, bindings->var, freshType);
    UNPROTECT(save);
}

static void processLetRecBinding(LamLetRecBindings *bindings, TcEnv *env,
                                 TcNg *ng) {
    TcType *existingType = NULL;
    if (!getFromTcEnv(env, bindings->var, &existingType)) {
        cant_happen("failed to retrieve fresh var from env in analyzeLetRec");
    }
    int save = PROTECT(existingType);
    // Recursive functions need to be statically typed inside their own context:
    TcNg *ng2 = extendNg(ng);
    PROTECT(ng2);
    addToNg(ng2, existingType);
    TcType *type = analyzeExp(bindings->val, env, ng2);
    PROTECT(type);
    unify(existingType, type, "letrec");
    DEBUGN("analyzeLetRec %s :: ", bindings->var->name);
    IFDEBUGN(ppTcType(existingType));
    UNPROTECT(save);
}

static TcType *analyzeLetRec(LamLetRec *letRec, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeLetRec);
    env = extendEnv(env);
    int save = PROTECT(env);
    ng = extendNg(ng);
    PROTECT(ng);
    // bind lambdas early
    for (LamLetRecBindings *bindings = letRec->bindings; bindings != NULL;
         bindings = bindings->next) {
        if (isLambdaBinding(bindings)) {
            prepareLetRecEnv(bindings, env);
        }
    }
    for (LamLetRecBindings *bindings = letRec->bindings; bindings != NULL;
         bindings = bindings->next) {
        DEBUGN("analyzeLetRec %s => ", bindings->var->name);
        IFDEBUGN(ppLamExp(bindings->val));
        if (!isLambdaBinding(bindings)) {
            prepareLetRecEnv(bindings, env);
        }
        processLetRecBinding(bindings, env, ng);
    }
    // HACK! second pass through fixes up forward references
    if (!hadErrors()) {
        for (LamLetRecBindings *bindings = letRec->bindings; bindings != NULL;
             bindings = bindings->next) {
            if (isLambdaBinding(bindings)) {
                processLetRecBinding(bindings, env, ng);
            }
        }
    }
    TcType *res = analyzeExp(letRec->body, env, ng);
    UNPROTECT(save);
    // LEAVE(analyzeLetRec);
    return res;
}

static TcUserTypeArgs *makeTcUserTypeArgs(LamTypeArgs *lamTypeArgs,
                                          TcTypeTable *map) {
    if (lamTypeArgs == NULL) {
        return NULL;
    }
    TcUserTypeArgs *next = makeTcUserTypeArgs(lamTypeArgs->next, map);
    int save = PROTECT(next);
    TcType *name = NULL;
    if (!getTcTypeTable(map, lamTypeArgs->name, &name)) {
        name = makeVar(lamTypeArgs->name);
        int save2 = PROTECT(name);
        setTcTypeTable(map, lamTypeArgs->name, name);
        UNPROTECT(save2);
    }
    TcUserTypeArgs *this = newTcUserTypeArgs(name, next);
    UNPROTECT(save);
    return this;
}

static TcType *makeUserType(HashSymbol *name, TcUserTypeArgs *args) {
    TcUserType *tcUserType = newTcUserType(name, args);
    int save = PROTECT(tcUserType);
    TcType *res =
        newTcType(TCTYPE_TYPE_USERTYPE, TCTYPE_VAL_USERTYPE(tcUserType));
    UNPROTECT(save);
    return res;
}

static TcType *makeTcUserType(LamType *lamType, TcTypeTable *map) {
    TcUserTypeArgs *args = makeTcUserTypeArgs(lamType->args, map);
    int save = PROTECT(args);
    TcType *res = makeUserType(lamType->name, args);
    UNPROTECT(save);
    return res;
}

static TcType *makeTuple(int size) {
    TcTypeArray *array = newTcTypeArray();
    int save = PROTECT(array);
    while (size-- > 0) {
        TcType *part = makeFreshVar("tuple");
        int save2 = PROTECT(part);
        pushTcTypeArray(array, part);
        UNPROTECT(save2);
    }
    TcType *res = newTcType(TCTYPE_TYPE_TUPLE, TCTYPE_VAL_TUPLE(array));
    UNPROTECT(save);
    return res;
}

static TcType *makeTypeConstructorArg(LamTypeConstructorType *arg,
                                      TcTypeTable *map);

static TcUserTypeArgs *makeUserTypeArgs(LamTypeConstructorArgs *args,
                                        TcTypeTable *map) {
    if (args == NULL) {
        return NULL;
    }
    TcUserTypeArgs *next = makeUserTypeArgs(args->next, map);
    int save = PROTECT(next);
    TcType *arg = makeTypeConstructorArg(args->arg, map);
    PROTECT(arg);
    TcUserTypeArgs *this = newTcUserTypeArgs(arg, next);
    UNPROTECT(save);
    return this;
}

static TcType *makeTypeConstructorApplication(LamTypeFunction *func,
                                              TcTypeTable *map) {
    // this code is building the inner application of a type, i.e.
    // list(t) in the context of t -> list(t) -> list(t)
    TcUserTypeArgs *args = makeUserTypeArgs(func->args, map);
    int save = PROTECT(args);
    TcType *res = makeUserType(func->name, args);
    UNPROTECT(save);
    return res;
}

static TcType *makeTypeConstructorArg(LamTypeConstructorType *arg,
                                      TcTypeTable *map) {
    TcType *res = NULL;
    switch (arg->type) {
        case LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER:
            res = makeBigInteger();
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER:
            res = makeCharacter();
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_VAR:{
                if (!getTcTypeTable(map, arg->val.var, &res)) {
                    res = makeVar(arg->val.var);
                    int save = PROTECT(res);
                    setTcTypeTable(map, arg->val.var, res);
                    UNPROTECT(save);
                }
            }
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
            res = makeTypeConstructorApplication(arg->val.function, map);
            break;
        default:
            cant_happen("unrecognised type %d in collectTypeConstructorArg",
                        arg->type);
    }
    return res;
}

static TcType *makeTypeDefConstructor(LamTypeConstructorArgs *args,
                                      TcType *result, TcTypeTable *map) {
    // this code is building the top-level type of a type constructor, i.e.
    // pair => t -> list(t) -> list(t)
    if (args == NULL) {
        return result;
    }
    TcType *next = makeTypeDefConstructor(args->next, result, map);
    int save = PROTECT(next);
    TcType *this = makeTypeConstructorArg(args->arg, map);
    PROTECT(this);
    TcType *res = makeFn(this, next);
    UNPROTECT(save);
    return res;
}

static void collectTypeDefConstructor(LamTypeConstructor *constructor,
                                      TcType *type, TcEnv *env,
                                      TcTypeTable *map) {
    TcType *res = makeTypeDefConstructor(constructor->args, type, map);
    int save = PROTECT(res);
    addToEnv(env, constructor->name, res);
    UNPROTECT(save);
}

static void collectTypeDef(LamTypeDef *lamTypeDef, TcEnv *env) {
    TcTypeTable *map = newTcTypeTable();
    int save = PROTECT(map);
    LamType *lamType = lamTypeDef->type;
    TcType *tcType = makeTcUserType(lamType, map);
    PROTECT(tcType);
    for (LamTypeConstructorList *list = lamTypeDef->constructors;
         list != NULL; list = list->next) {
        collectTypeDefConstructor(list->constructor, tcType, env, map);
    }
    UNPROTECT(save);
}

static TcType *analyzeTypeDefs(LamTypeDefs *typeDefs, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeTypeDefs);
    env = extendEnv(env);
    int save = PROTECT(env);
    for (LamTypeDefList *list = typeDefs->typeDefs; list != NULL;
         list = list->next) {
        collectTypeDef(list->typeDef, env);
    }
    TcType *res = analyzeExp(typeDefs->body, env, ng);
    UNPROTECT(save);
    // LEAVE(analyzeTypeDefs);
    return res;
}

static TcType *analyzeLet(LamLet *let, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeLet);
    // let expression is evaluated in the current environment
    TcType *valType = analyzeExp(let->value, env, ng);
    int save = PROTECT(valType);
    env = extendEnv(env);
    PROTECT(env);
    addToEnv(env, let->var, valType);
    TcType *res = analyzeExp(let->body, env, ng);
    UNPROTECT(save);
    // LEAVE(analyzeLet);
    return res;
}

static TcType *analyzeMatchCases(LamMatchList *cases, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeMatchCases);
    if (cases == NULL) {
        TcType *res = makeFreshVar("matchCases");
        // LEAVE(analyzeMatchCases);
        return res;
    }
    TcType *rest = analyzeMatchCases(cases->next, env, ng);
    int save = PROTECT(rest);
    TcType *this = analyzeExp(cases->body, env, ng);
    PROTECT(this);
    if (!unify(this, rest, "match cases")) {
        eprintf("while unifying match cases:\n");
        ppLamExp(cases->body);
        eprintf("\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeMatchCases);
    return this;
}

static TcType *analyzeBigIntegerExp(LamExp *exp, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeBigIntegerExp);
    TcType *type = analyzeExp(exp, env, ng);
    int save = PROTECT(type);
    TcType *integer = makeBigInteger();
    PROTECT(integer);
    if (!unify(type, integer, "big integer exp")) {
        eprintf("while analyzing bigint expr:\n");
        ppLamExp(exp);
        eprintf("\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeBigIntegerExp);
    return integer;
}

static TcType *analyzeSmallIntegerExp(LamExp *exp, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeSmallIntegerExp);
    TcType *type = analyzeExp(exp, env, ng);
    int save = PROTECT(type);
    TcType *integer = makeSmallInteger();
    PROTECT(integer);
    if (!unify(type, integer, "small integer exp")) {
        eprintf("while analyzing smallint expr:\n");
        ppLamExp(exp);
        eprintf("\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeSmallIntegerExp);
    return integer;
}

static TcType *analyzeBooleanExp(LamExp *exp, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeBooleanExp);
    TcType *type = analyzeExp(exp, env, ng);
    int save = PROTECT(type);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    if (!unify(type, boolean, "boolean exp")) {
        eprintf("while analyzing boolean expr:\n");
        ppLamExp(exp);
        eprintf("\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeBooleanExp);
    return boolean;
}

static TcType *analyzeCharacterExp(LamExp *exp, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeCharacterExp);
    TcType *type = analyzeExp(exp, env, ng);
    int save = PROTECT(type);
    TcType *character = makeCharacter();
    PROTECT(character);
    if (!unify(type, character, "character exp")) {
        eprintf("while analyzing character expr:\n");
        ppLamExp(exp);
        eprintf("\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeCharacterExp);
    return character;
}

static TcType *lookupConstructorType(HashSymbol *name, TcEnv *env, TcNg *ng) {
    // ENTER(lookupConstructorType);
    TcType *res = lookup(env, name, ng);
    if (res == NULL) {
        cant_happen("lookupConstructorType %s failed", name->name);
    }
    res = findResultType(res);
    // LEAVE(lookupConstructorType);
    return res;
}

static TcType *analyzeIntList(LamIntList *intList, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeIntList);
    if (intList == NULL) {
        // LEAVE(analyzeIntList);
        return makeFreshVar("intList");
    }
    TcType *next = analyzeIntList(intList->next, env, ng);
    int save = PROTECT(next);
    TcType *this = lookupConstructorType(intList->name, env, ng);
    PROTECT(this);
    if (!unify(next, this, "int list")) {
        eprintf("while analyzing intList case %s\n", intList->name->name);
    }
    // LEAVE(analyzeIntList);
    UNPROTECT(save);
    return this;
}

static TcType *findCaseType(LamMatchList *matchList, TcEnv *env, TcNg *ng) {
    // ENTER(findCaseType);
    if (matchList == NULL) {
        // LEAVE(findCaseType);
        return makeFreshVar("caseType");
    }
    TcType *next = findCaseType(matchList->next, env, ng);
    int save = PROTECT(next);
    TcType *this = analyzeIntList(matchList->matches, env, ng);
    PROTECT(this);
    if (!unify(this, next, "find case type")) {
        eprintf("while finding case type\n");
    }
    UNPROTECT(save);
    // LEAVE(findCaseType);
    return this;
}

static TcType *analyzeMatch(LamMatch *match, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeMatch);
    TcType *caseType = findCaseType(match->cases, env, ng);
    int save = PROTECT(caseType);
    TcType *indexType = analyzeExp(match->index, env, ng);
    PROTECT(indexType);
    if (!unify(caseType, indexType, "match")) {
        eprintf("while analyzing match\n");
    }
    TcType *res = analyzeMatchCases(match->cases, env, ng);
    // LEAVE(analyzeMatch);
    UNPROTECT(save);
    return res;
}

static TcType *analyzeIntCondCases(LamIntCondCases *cases, TcEnv *env,
                                   TcNg *ng) {
    // ENTER(analyzeIntCondCases);
    if (cases == NULL) {
        // LEAVE(analyzeIntCondCases);
        return makeFreshVar("intCondCases");
    }
    TcType *rest = analyzeIntCondCases(cases->next, env, ng);
    int save = PROTECT(rest);
    TcType *this = analyzeExp(cases->body, env, ng);
    PROTECT(this);
    if (!unify(this, rest, "cond cases")) {
        eprintf("while analyzing int cond cases\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeIntCondCases);
    return this;
}

static TcType *analyzeCharCondCases(LamCharCondCases *cases, TcEnv *env,
                                    TcNg *ng) {
    // ENTER(analyzeCharCondCases);
    if (cases == NULL) {
        // LEAVE(analyzeCharCondCases);
        return makeFreshVar("charCondCases");
    }
    TcType *rest = analyzeCharCondCases(cases->next, env, ng);
    int save = PROTECT(rest);
    TcType *this = analyzeExp(cases->body, env, ng);
    PROTECT(this);
    if (!unify(this, rest, "char cond cases")) {
        eprintf("while analyzing char cond cases\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeCharCondCases);
    return this;
}

static TcType *analyzeCond(LamCond *cond, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeCond);
    TcType *result = NULL;
    int save = PROTECT(result);
    TcType *value = analyzeExp(cond->value, env, ng);
    PROTECT(value);
    switch (cond->cases->type) {
        case LAMCONDCASES_TYPE_INTEGERS:{
                TcType *integer = makeBigInteger();
                PROTECT(integer);
                if (!unify(value, integer, "cond[1]")) {
                    eprintf("while analyzing integer cond:\n");
                    ppLamExp(cond->value);
                    eprintf("\n");
                }
                result =
                    analyzeIntCondCases(cond->cases->val.integers, env, ng);
            }
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:{
                TcType *character = makeCharacter();
                PROTECT(character);
                if (!unify(value, character, "cond[2]")) {
                    eprintf("while analyzing character cond:\n");
                    ppLamExp(cond->value);
                    eprintf("\n");
                }
                result =
                    analyzeCharCondCases(cond->cases->val.characters, env,
                                         ng);
            }
            break;
        default:
            cant_happen("unrecognized type %d in analyzeCond",
                        cond->cases->type);
    }
    UNPROTECT(save);
    // LEAVE(analyzeCond);
    return result;
}

static TcType *analyzeAnd(LamAnd *and, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeAnd);
    TcType *res = analyzeBinaryBool(and->left, and->right, env, ng);
    // LEAVE(analyzeAnd);
    return res;
}

static TcType *analyzeOr(LamOr *or, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeOr);
    TcType *res = analyzeBinaryBool(or->left, or->right, env, ng);
    // LEAVE(analyzeOr);
    return res;
}

static TcType *analyzeAmb(LamAmb *amb, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeAmb);
    TcType *left = analyzeExp(amb->left, env, ng);
    int save = PROTECT(left);
    TcType *right = analyzeExp(amb->right, env, ng);
    PROTECT(right);
    if (!unify(left, right, "amb")) {
        eprintf("while unifying amb:\n");
        ppLamExp(amb->left);
        eprintf("\nwith:\n");
        ppLamExp(amb->right);
        eprintf("\n");
    }
    UNPROTECT(save);
    // LEAVE(analyzeAmb);
    return left;
}

static TcType *analyzeCharacter() {
    // ENTER(analyzeCharacter);
    TcType *res = makeCharacter();
    // LEAVE(analyzeCharacter);
    return res;
}

static TcType *analyzeBack() {
    // ENTER(analyzeBack);
    TcType *res = makeFreshVar("back");
    // LEAVE(analyzeBack);
    return res;
}

static TcType *analyzeError() {
    // ENTER(analyzeError);
    TcType *res = makeFreshVar("error");
    // LEAVE(analyzeError);
    return res;
}

static void addToEnv(TcEnv *env, HashSymbol *symbol, TcType *type) {
    setTcTypeTable(env->table, symbol, type);
}

bool getFromTcEnv(TcEnv *env, HashSymbol *symbol, TcType **type) {
    if (env == NULL) {
        return false;
    }
    if (getTcTypeTable(env->table, symbol, type)) {
        return true;
    }
    return getFromTcEnv(env->next, symbol, type);
}

static TcType *freshFunction(TcFunction *fn, TcNg *ng, TcTypeTable *map) {
    TcType *arg = freshRec(fn->arg, ng, map);
    int save = PROTECT(arg);
    TcType *result = freshRec(fn->result, ng, map);
    PROTECT(result);
    TcType *res = makeFn(arg, result);
    UNPROTECT(save);
    return res;
}

static TcType *makePair(TcType *first, TcType *second) {
    TcPair *resPair = newTcPair(first, second);
    int save = PROTECT(resPair);
    TcType *res = newTcType(TCTYPE_TYPE_PAIR, TCTYPE_VAL_PAIR(resPair));
    UNPROTECT(save);
    return res;
}

static TcType *freshPair(TcPair *pair, TcNg *ng, TcTypeTable *map) {
    TcType *first = freshRec(pair->first, ng, map);
    int save = PROTECT(first);
    TcType *second = freshRec(pair->second, ng, map);
    PROTECT(second);
    TcType *res = makePair(first, second);
    UNPROTECT(save);
    return res;
}

static TcUserTypeArgs *freshUserTypeArgs(TcUserTypeArgs *args, TcNg *ng,
                                         TcTypeTable *map) {
    if (args == NULL)
        return NULL;
    TcUserTypeArgs *next = freshUserTypeArgs(args->next, ng, map);
    int save = PROTECT(next);
    TcType *type = freshRec(args->type, ng, map);
    PROTECT(type);
    TcUserTypeArgs *this = newTcUserTypeArgs(type, next);
    UNPROTECT(save);
    return this;
}

static TcType *freshUserType(TcUserType *userType, TcNg *ng, TcTypeTable *map) {
    TcUserTypeArgs *args = freshUserTypeArgs(userType->args, ng, map);
    int save = PROTECT(args);
    TcType *res = makeUserType(userType->name, args);
    UNPROTECT(save);
    return res;
}

static TcType *freshTuple(TcTypeArray *tuple, TcNg *ng, TcTypeTable *map) {
    TcTypeArray *fresh = newTcTypeArray();
    int save = PROTECT(fresh);
    for (int i = 0; i < tuple->size; i ++) {
        TcType *part = freshRec(tuple->entries[i], ng, map);
        int save2 = PROTECT(part);
        pushTcTypeArray(fresh, part);
        UNPROTECT(save2);
    }
    TcType *res = newTcType(TCTYPE_TYPE_TUPLE, TCTYPE_VAL_TUPLE(fresh));
    UNPROTECT(save);
    return res;
}

static bool isGeneric(TcType *typeVar, TcNg *ng) {
    while (ng != NULL) {
        int i = 0;
        TcType *entry = NULL;
        HashSymbol *s = NULL;
        while ((s = iterateTcTypeTable(ng->table, &i, &entry)) != NULL) {
            if (occursInType(typeVar, entry)) {
                return false;
            }
        }
        ng = ng->next;
    }
    return true;
}

static TcType *typeGetOrPut(TcTypeTable *map, TcType *typeVar,
                            TcType *defaultValue) {
    HashSymbol *name = typeVar->val.var->name;
    TcType *res = NULL;
    if (getTcTypeTable(map, name, &res)) {
        return res;
    }
    setTcTypeTable(map, name, defaultValue);
    return defaultValue;
}

static TcType *freshRec(TcType *type, TcNg *ng, TcTypeTable *map) {
    type = prune(type);
    switch (type->type) {
        case TCTYPE_TYPE_FUNCTION: {
            TcType *res = freshFunction(type->val.function, ng, map);
            return res;
        }
        case TCTYPE_TYPE_PAIR:{
                TcType *res = freshPair(type->val.pair, ng, map);
                return res;
            }
        case TCTYPE_TYPE_VAR:
            if (isGeneric(type, ng)) {
                TcType *freshType = makeFreshVar(type->val.var->name->name);
                int save = PROTECT(freshType);
                TcType *res = typeGetOrPut(map, type, freshType);
                UNPROTECT(save);
                return res;
            }
            return type;
        case TCTYPE_TYPE_SMALLINTEGER:
        case TCTYPE_TYPE_BIGINTEGER:
        case TCTYPE_TYPE_CHARACTER:
        case TCTYPE_TYPE_UNKNOWN:
            return type;
        case TCTYPE_TYPE_USERTYPE:{
                TcType *res = freshUserType(type->val.userType, ng, map);
                return res;
            }
        case TCTYPE_TYPE_TUPLE:
            return freshTuple(type->val.tuple, ng, map);
        default:
            cant_happen("unrecognised type %s", tcTypeTypeName(type->type));
    }
}

static TcType *fresh(TcType *type, TcNg *ng) {
    // ENTER(fresh);
    TcTypeTable *map = newTcTypeTable();
    int save = PROTECT(map);
    TcType *res = freshRec(type, ng, map);
    UNPROTECT(save);
    // LEAVE(fresh);
    return res;
}

static TcType *lookup(TcEnv *env, HashSymbol *symbol, TcNg *ng) {
    // ENTER(lookup);
    TcType *type = NULL;
    if (getFromTcEnv(env, symbol, &type)) {
        TcType *res = fresh(type, ng);
        // LEAVE(lookup);
        DEBUGN("lookup %s => ", symbol->name);
        IFDEBUGN(ppTcType(res));
        return res;
    }
    // LEAVE(lookup);
    DEBUG("lookup %s => NULL", symbol->name);
    return NULL;
}

static void addToNg(TcNg *ng, TcType *type) {
#ifdef SAFETY_CHECKS
    if (type->type != TCTYPE_TYPE_VAR) {
        cant_happen("non-var type passed to addToNg");
    }
#endif
    setTcTypeTable(ng->table, type->val.var->name, type);
}

static TcType *makeBoolean() {
    TcType *res = makeUserType(boolSymbol(), NULL);
    return res;
}

static TcType *makeSpaceship() {
    TcType *res = makeUserType(spaceshipSymbol(), NULL);
    return res;
}

static TcType *makeFn(TcType *arg, TcType *result) {
    arg = prune(arg);
    result = prune(result);
    TcFunction *fn = newTcFunction(arg, result);
    int save = PROTECT(fn);
    assert(fn != NULL);
    TcType *type = newTcType(TCTYPE_TYPE_FUNCTION, TCTYPE_VAL_FUNCTION(fn));
    UNPROTECT(save);
    return type;
}

static TcEnv *extendEnv(TcEnv *parent) {
    TcEnv *env = newTcEnv(parent);
    return env;
}

static TcNg *extendNg(TcNg *parent) {
    TcNg *ng = newTcNg(parent);
    return ng;
}

static TcType *makeVar(HashSymbol *t) {
    TcVar *var = newTcVar(t, id_counter++);
    int save = PROTECT(var);
    TcType *res = newTcType(TCTYPE_TYPE_VAR, TCTYPE_VAL_VAR(var));
    UNPROTECT(save);
    return res;
}

static TcType *makeFreshVar(char *name __attribute__((unused))) {
    return makeVar(genAlphaSym("#"));
}

static TcType *makeSmallInteger() {
    TcType *res =
        newTcType(TCTYPE_TYPE_SMALLINTEGER, TCTYPE_VAL_SMALLINTEGER());
    return res;
}

static TcType *makeBigInteger() {
    TcType *res = newTcType(TCTYPE_TYPE_BIGINTEGER, TCTYPE_VAL_BIGINTEGER());
    return res;
}

static TcType *makeUnknown(HashSymbol *var) {
    TcType *res = newTcType(TCTYPE_TYPE_UNKNOWN, TCTYPE_VAL_UNKNOWN(var));
    return res;
}

static TcType *makeCharacter() {
    TcType *res = newTcType(TCTYPE_TYPE_CHARACTER, TCTYPE_VAL_CHARACTER());
    return res;
}

static void addUnOpToEnv(TcEnv *env, HashSymbol *symbol, TcType *type) {
    TcType *aa = makeFn(type, type);
    int save = PROTECT(aa);
    addToEnv(env, symbol, aa);
    UNPROTECT(save);
}

static void addNegToEnv(TcEnv *env) {
    TcType *integer = makeBigInteger();
    int save = PROTECT(integer);
    addUnOpToEnv(env, negSymbol(), integer);
    UNPROTECT(save);
}

static void addNotToEnv(TcEnv *env) {
    TcType *boolean = makeBoolean();
    int save = PROTECT(boolean);
    addUnOpToEnv(env, negSymbol(), boolean);
    UNPROTECT(save);
}

static void addPutcToEnv(TcEnv *env) {
    TcType *character = makeCharacter();
    int save = PROTECT(character);
    addUnOpToEnv(env, putcSymbol(), character);
    UNPROTECT(save);
}

static void addIfToEnv(TcEnv *env) {
    // 'if' is bool -> a -> a -> a
    TcType *boolean = makeBoolean();
    int save = PROTECT(boolean);
    TcType *a = makeFreshVar("if");
    (void) PROTECT(a);
    TcType *aa = makeFn(a, a);
    (void) PROTECT(aa);
    TcType *aaa = makeFn(a, aa);
    (void) PROTECT(aaa);
    TcType *baaa = makeFn(boolean, aaa);
    (void) PROTECT(baaa);
    addToEnv(env, ifSymbol(), baaa);
    UNPROTECT(save);
}

static void addHereToEnv(TcEnv *env) {
    // 'call/cc' is ((a -> b) -> a) -> a
    TcType *a = makeFreshVar("hereA");
    int save = PROTECT(a);
    TcType *b = makeFreshVar("hereB");
    (void) PROTECT(b);
    TcType *ab = makeFn(a, b);
    (void) PROTECT(ab);
    TcType *aba = makeFn(ab, a);
    (void) PROTECT(aba);
    TcType *abaa = makeFn(aba, a);
    (void) PROTECT(abaa);
    addToEnv(env, hereSymbol(), abaa);
    UNPROTECT(save);
}

static void addCmpToEnv(TcEnv *env, HashSymbol *symbol) {
    // all binary comparisons are a -> a -> bool
    TcType *freshType = makeFreshVar(symbol->name);
    int save = PROTECT(freshType);
    TcType *boolean = makeBoolean();
    (void) PROTECT(boolean);
    TcType *unOp = makeFn(freshType, boolean);
    (void) PROTECT(unOp);
    TcType *binOp = makeFn(freshType, unOp);
    (void) PROTECT(binOp);
    addToEnv(env, symbol, binOp);
    UNPROTECT(save);
}

static void addFreshVarToEnv(TcEnv *env, HashSymbol *symbol) {
    // 'error' and 'back' both have unconstrained types
    TcType *freshType = makeFreshVar(symbol->name);
    int save = PROTECT(freshType);
    addToEnv(env, symbol, freshType);
    UNPROTECT(save);
}

static void addBinOpToEnv(TcEnv *env, HashSymbol *symbol, TcType *type) {
    // handle all fonctions of the form a -> a -> a
    TcType *unOp = makeFn(type, type);
    int save = PROTECT(unOp);
    TcType *binOp = makeFn(type, unOp);
    (void) PROTECT(binOp);
    addToEnv(env, symbol, binOp);
    UNPROTECT(save);
}

static void addIntBinOpToEnv(TcEnv *env, HashSymbol *symbol) {
    // int -> int -> int
    TcType *integer = makeBigInteger();
    int save = PROTECT(integer);
    addBinOpToEnv(env, symbol, integer);
    UNPROTECT(save);
}

static void addBoolBinOpToEnv(TcEnv *env, HashSymbol *symbol) {
    // bool -> bool -> bool
    TcType *boolean = makeBoolean();
    int save = PROTECT(boolean);
    addBinOpToEnv(env, symbol, boolean);
    UNPROTECT(save);
}

static void addThenToEnv(TcEnv *env) {
    // a -> a -> a
    TcType *freshType = makeFreshVar("then");
    int save = PROTECT(freshType);
    addBinOpToEnv(env, thenSymbol(), freshType);
    UNPROTECT(save);
}

static bool unifyFunctions(TcFunction *a, TcFunction *b) {
    bool res = unify(a->arg, b->arg, "functions[arg]")
        && unify(a->result, b->result, "functions[result]");
    return res;
}

static bool unifyPairs(TcPair *a, TcPair *b) {
    bool res = unify(a->first, b->first, "pairs[first]")
        && unify(a->second, b->second, "pairs[second]");
    return res;
}

static bool unifyTuples(TcTypeArray *a, TcTypeArray *b) {
    if (a->size != b->size) {
        can_happen("tuple sizes differ: %d vs %d", a->size, b->size);
        return false;
    }
    bool unified = true;
    for (int i = 0; i < a->size; i++) {
        if (!unify(a->entries[i], b->entries[i], "tuples")) {
            unified = false;
        }
    }
    return unified;
}

static bool unifyUserTypes(TcUserType *a, TcUserType *b) {
    if (a->name != b->name) {
        can_happen("unification failed[1]");
        ppTcUserType(a);
        eprintf(" vs ");
        ppTcUserType(b);
        eprintf("\n");
        return false;
    }
    TcUserTypeArgs *aArgs = a->args;
    TcUserTypeArgs *bArgs = b->args;
    while (aArgs != NULL && bArgs != NULL) {
        if (!unify(aArgs->type, bArgs->type, "user types")) {
            return false;
        }
        aArgs = aArgs->next;
        bArgs = bArgs->next;
    }
    if (aArgs != NULL || bArgs != NULL) {
        can_happen("unification failed[2]");
        ppTcUserType(a);
        eprintf(" vs ");
        ppTcUserType(b);
        eprintf("\n");
        return false;
    }
    return true;
}

static bool _unify(TcType *a, TcType *b) {
    a = prune(a);
    b = prune(b);
    if (a == b) {
        if (a->type == TCTYPE_TYPE_UNKNOWN)
            return false;
        return true;
    }
    if (a->type == TCTYPE_TYPE_VAR) {
        if (b->type != TCTYPE_TYPE_VAR) {
            if (occursInType(a, b)) {
                can_happen("occurs-in check failed");
                return false;
            }
            a->val.var->instance = b;
            return true;
        }
        if (a->val.var->name != b->val.var->name) {
            a->val.var->instance = b;
        }
        return true;
    } else if (b->type == TCTYPE_TYPE_VAR) {
        return unify(b, a, "unify");
    } else {
        if (a->type != b->type) {
            can_happen("unification failed[3]");
            ppTcType(a);
            eprintf(" vs ");
            ppTcType(b);
            eprintf("\n");
            return false;
        }
        switch (a->type) {
            case TCTYPE_TYPE_FUNCTION:
                return unifyFunctions(a->val.function, b->val.function);
            case TCTYPE_TYPE_PAIR:
                return unifyPairs(a->val.pair, b->val.pair);
            case TCTYPE_TYPE_VAR:
                cant_happen("encountered var in unify");
            case TCTYPE_TYPE_SMALLINTEGER:
            case TCTYPE_TYPE_BIGINTEGER:
            case TCTYPE_TYPE_CHARACTER:
                return true;
            case TCTYPE_TYPE_UNKNOWN:
                return false;
            case TCTYPE_TYPE_USERTYPE:
                return unifyUserTypes(a->val.userType, b->val.userType);
            case TCTYPE_TYPE_TUPLE:
                return unifyTuples(a->val.tuple, b->val.tuple);
            default:
                cant_happen("unrecognised type %s", tcTypeTypeName(a->type));
        }
    }
    cant_happen("reached end of unify");
}

static bool unify(TcType *a, TcType *b, char *trace __attribute__((unused))) {
    // *INDENT-OFF*
    DEBUGN("unify(%s) :> ", trace);
    IFDEBUGN(ppTcType(a); eprintf(" =?= "); ppTcType(b));
    bool res = _unify(a, b);
    DEBUGN("unify(%s) <: ", trace);
    IFDEBUGN(ppTcType(a); eprintf(" === "); ppTcType(b));
    return res;
    // *INDENT-ON*
}

static void pruneUserTypeArgs(TcUserTypeArgs *args) {
    while (args != NULL) {
        args->type = prune(args->type);
        args = args->next;
    }
}

static TcType *prune(TcType *t) {
    if (t == NULL)
        return NULL;
    if (t->type == TCTYPE_TYPE_VAR) {
        if (t->val.var->instance != NULL) {
            t->val.var->instance = prune(t->val.var->instance);
            return t->val.var->instance;
        }
    } else if (t->type == TCTYPE_TYPE_USERTYPE) {
        pruneUserTypeArgs(t->val.userType->args);
    } else if (t->type == TCTYPE_TYPE_FUNCTION) {
        t->val.function->arg = prune(t->val.function->arg);
        t->val.function->result = prune(t->val.function->result);
    }
    return t;
}

static bool sameFunctionType(TcFunction *a, TcFunction *b) {
    return sameType(a->arg, b->arg) && sameType(a->result, b->result);
}

static bool samePairType(TcPair *a, TcPair *b) {
    return sameType(a->first, b->first) && sameType(a->second, b->second);
}

static bool sameUserType(TcUserType *a, TcUserType *b) {
    if (a->name != b->name) {
        return false;
    }
    TcUserTypeArgs *aArgs = a->args;
    TcUserTypeArgs *bArgs = b->args;
    while (aArgs != NULL && bArgs != NULL) {
        if (!sameType(aArgs->type, bArgs->type))
            return false;
        aArgs = aArgs->next;
        bArgs = bArgs->next;
    }
    if (aArgs != NULL || bArgs != NULL) {
        return false;
    }
    return true;
}

static bool sameType(TcType *a, TcType *b) {
    a = prune(a);
    b = prune(b);
    if (a == NULL || b == NULL) {
        cant_happen("NULL in sameType");
    }
    if (a->type != b->type) {
        return false;
    }
    switch (a->type) {
        case TCTYPE_TYPE_FUNCTION:
            return sameFunctionType(a->val.function, b->val.function);
        case TCTYPE_TYPE_PAIR:
            return samePairType(a->val.pair, b->val.pair);
        case TCTYPE_TYPE_VAR:
            return a->val.var->id == b->val.var->id;
        case TCTYPE_TYPE_BIGINTEGER:
        case TCTYPE_TYPE_SMALLINTEGER:
        case TCTYPE_TYPE_CHARACTER:
            return true;
        case TCTYPE_TYPE_UNKNOWN:
            return false;
        case TCTYPE_TYPE_USERTYPE:
            return sameUserType(a->val.userType, b->val.userType);
        default:
            cant_happen("unrecognised type %d in sameType", a->type);
    }
}

static bool occursInType(TcType *a, TcType *b) {
    b = prune(b);
    if (b->type == TCTYPE_TYPE_VAR) {
        return sameType(a, b);
    } else {
        return occursIn(a, b);
    }
}

static bool occursInFunction(TcType *var, TcFunction *fun) {
    assert(fun != NULL);
    return occursInType(var, fun->arg) || occursInType(var, fun->result);
}

static bool occursInPair(TcType *var, TcPair *pair) {
    return occursInType(var, pair->first) || occursInType(var, pair->second);
}

static bool occursInUserType(TcType *var, TcUserType *userType) {
    for (TcUserTypeArgs *args = userType->args; args != NULL;
         args = args->next) {
        if (occursInType(var, args->type))
            return true;
    }
    return false;
}

static bool occursInTuple(TcType *var, TcTypeArray *tuple) {
    for (int i = 0; i < tuple->size; ++i) {
        if (occursInType(var, tuple->entries[i])) {
            return true;
        }
    }
    return false;
}

static bool occursIn(TcType *a, TcType *b) {
    switch (b->type) {
        case TCTYPE_TYPE_FUNCTION:
            return occursInFunction(a, b->val.function);
        case TCTYPE_TYPE_PAIR:
            return occursInPair(a, b->val.pair);
        case TCTYPE_TYPE_VAR:
            cant_happen("occursIn 2nd arg should not be a var");
        case TCTYPE_TYPE_SMALLINTEGER:
        case TCTYPE_TYPE_BIGINTEGER:
        case TCTYPE_TYPE_CHARACTER:
        case TCTYPE_TYPE_UNKNOWN:
            return false;
        case TCTYPE_TYPE_USERTYPE:
            return occursInUserType(a, b->val.userType);
        case TCTYPE_TYPE_TUPLE:
            return occursInTuple(a, b->val.tuple);
        default:
            cant_happen("unrecognised type %s", tcTypeTypeName(b->type));
    }
}
