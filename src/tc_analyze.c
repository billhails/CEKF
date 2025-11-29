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
#include "print_generator.h"
#include "lambda_pp.h"
#include "types.h"

#ifdef DEBUG_TC
#  include "debugging_on.h"
#  include "lambda_pp.h"
#else
#  include "debugging_off.h"
#endif

static void addToEnv(TcEnv * env, HashSymbol * key, TcType * type);
static void addToNg(TcNg * env, TcType * type);
static void addFreshVarToEnv(TcEnv * env, HashSymbol * key);
static void addCmpToEnv(TcEnv * env, HashSymbol * key);
static void addBuiltinsToEnv(TcEnv * env, BuiltIns * builtIns);
static void addNamespacesToEnv(TcEnv * env);
static TcType *makeSpaceship(void);
static TcType *makeBigInteger(void);
static TcType *makeCharacter(void);
static TcType *makeUnknown(HashSymbol * var);
static TcType *makeVar(HashSymbol * t);
static TcType *makeFn(TcType * arg, TcType * result);
static TcType *makeThunk(TcType * type);
static TcType *makeTuple(int size);
static void addHereToEnv(TcEnv * env);
static void addIfToEnv(TcEnv * env);
static void addIntBinOpToEnv(TcEnv * env, HashSymbol * symbol);
static void addNegToEnv(TcEnv * env);
static void addNotToEnv(TcEnv * env);
static void addThenToEnv(TcEnv * env);
static TcType *analyzeExp(LamExp * exp, TcEnv * env, TcNg * ng);
static TcType *analyzeLam(LamLam * lam, TcEnv * env, TcNg * ng);
static TcType *analyzeVar(ParserInfo I, HashSymbol * var, TcEnv * env,
                          TcNg * ng);
static TcType *analyzeSmallInteger();
static TcType *analyzeBigInteger();
static TcType *analyzePrim(LamPrimApp * app, TcEnv * env, TcNg * ng);
static TcType *analyzeSequence(LamSequence * sequence, TcEnv * env,
                               TcNg * ng);
static TcType *analyzeConstruct(LamConstruct * construct, TcEnv * env,
                                TcNg * ng);
static TcType *analyzeDeconstruct(LamDeconstruct * deconstruct, TcEnv * env,
                                  TcNg * ng);
static TcType *analyzeTag(LamExp * tag, TcEnv * env, TcNg * ng);
static TcType *analyzeConstant(LamConstant * constant, TcEnv * env,
                               TcNg * ng);
static TcType *analyzeApply(LamApply * apply, TcEnv * env, TcNg * ng);
static TcType *analyzeIff(LamIff * iff, TcEnv * env, TcNg * ng);
static TcType *analyzeCallCC(LamExp * called, TcEnv * env, TcNg * ng);
static TcType *analyzePrint(LamPrint * print, TcEnv * env, TcNg * ng);
static TcType *analyzeLetRec(LamLetRec * letRec, TcEnv * env, TcNg * ng);
static TcType *analyzeTypeDefs(LamTypeDefs * typeDefs, TcEnv * env,
                               TcNg * ng);
static TcType *analyzeLet(LamLet * let, TcEnv * env, TcNg * ng);
static TcType *analyzeMatch(LamMatch * match, TcEnv * env, TcNg * ng);
static TcType *analyzeCond(LamCond * cond, TcEnv * env, TcNg * ng);
static TcType *analyzeAmb(LamAmb * amb, TcEnv * env, TcNg * ng);
static TcType *analyzeTupleIndex(LamTupleIndex * index, TcEnv * env,
                                 TcNg * ng);
static TcType *analyzeMakeTuple(LamArgs * tuple, TcEnv * env, TcNg * ng);
static TcType *analyzeNamespaces(LamNamespaceArray * nsArray, TcEnv * env,
                                 TcNg * ng);
static TcType *analyzeCharacter();
static TcType *analyzeBack();
static TcType *analyzeError();
static TcType *analyzeEnv(TcEnv * env);
static bool unify(TcType * a, TcType * b, char *trace
                  __attribute__((unused)));
static TcType *prune(TcType * t);
static bool occursInType(TcType * a, TcType * b);
static bool occursIn(TcType * a, TcType * b);
static bool sameType(TcType * a, TcType * b);
static TcType *analyzeBigIntegerExp(LamExp * exp, TcEnv * env, TcNg * ng);
static TcType *analyzeSmallIntegerExp(LamExp * exp, TcEnv * env, TcNg * ng)
    __attribute__((unused));
static TcType *analyzeBooleanExp(LamExp * exp, TcEnv * env, TcNg * ng);
static TcType *freshRec(TcType * type, TcNg * ng, TcTypeTable * map);
static TcType *lookup(TcEnv * env, HashSymbol * symbol, TcNg * ng);
static TcType *analyzeLookup(LamLookup *, TcEnv *, TcNg *);
static TcType *lookupConstructorType(HashSymbol * name, int namespace,
                                     TcEnv * env, TcNg * ng);
static void addTypeSigToEnv(TcEnv * env, HashSymbol * symbol,
                            TcTypeSig * type);
static bool failUnify(TcType *a, TcType *b, char *reason);
static bool failUnifyTypeSigs(TcTypeSig *a, TcTypeSig *b, char *reason);
static bool failUnifyFunctions(TcFunction *a, TcFunction *b, char *reason) __attribute__((unused));
bool getTypeSigFromTcEnv(TcEnv * env, HashSymbol * symbol, TcTypeSig ** type);

static int id_counter = 0;

TcEnv *tc_init(BuiltIns *builtIns) {
    TcEnv *env = newTcEnv(NULL);
    int save = PROTECT(env);
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
    addThenToEnv(env);
    addBuiltinsToEnv(env, builtIns);
    addNamespacesToEnv(env);
    UNPROTECT(save);
    return env;
}

TcType *tc_analyze(LamExp *exp, TcEnv *env) {
    TcNg *ng = newTcNg(NULL);
    int save = PROTECT(ng);
    TcType *nsType = newTcType_Nsid(NS_GLOBAL); // global ns
    PROTECT(nsType);
    addToEnv(env, namespaceSymbol(), nsType);
    TcType *res = analyzeExp(exp, env, ng);
    UNPROTECT(save);
    return res;
}

TcType *makeListType(TcType *content) {
    TcTypeSigArgs *args = newTcTypeSigArgs(content, NULL);
    int save = PROTECT(args);
    TcTypeSig *typeSig = newTcTypeSig(newSymbol("list"), args, -1);
    PROTECT(typeSig);
    TcType *res = newTcType_TypeSig(typeSig);
    UNPROTECT(save);
    return res;
}

TcType *makeMaybeType(TcType *content) {
    TcTypeSigArgs *args = newTcTypeSigArgs(content, NULL);
    int save = PROTECT(args);
    TcTypeSig *typeSig = newTcTypeSig(newSymbol("maybe"), args, -1);
    PROTECT(typeSig);
    TcType *res = newTcType_TypeSig(typeSig);
    UNPROTECT(save);
    return res;
}

TcType *makeMaybeStringType() {
    TcType *stringType = makeStringType();
    int save = PROTECT(stringType);
    TcType *maybeStringType = makeMaybeType(stringType);
    UNPROTECT(save);
    return maybeStringType;
}

TcType *makeTryType(TcType *failure, TcType *success) {
    TcTypeSigArgs *args = newTcTypeSigArgs(success, NULL);
    int save = PROTECT(args);
    args = newTcTypeSigArgs(failure, args);
    PROTECT(args);
    TcTypeSig *typeSig = newTcTypeSig(newSymbol("try"), args, -1);
    PROTECT(typeSig);
    TcType *res = newTcType_TypeSig(typeSig);
    UNPROTECT(save);
    return res;
}

TcType *makeStringType(void) {
    TcType *character = newTcType_Character();
    int save = PROTECT(character);
    TcType *res = makeListType(character);
    UNPROTECT(save);
    return res;
}

static TcType *makeNamedType(char *name) {
    TcTypeSig *typeSig = newTcTypeSig(newSymbol(name), NULL, -1);
    int save = PROTECT(typeSig);
    TcType *res = newTcType_TypeSig(typeSig);
    UNPROTECT(save);
    return res;
}

TcType *makeBasicType(void) {
    return makeNamedType("basic_type");
}

TcType *makeIOType(void) {
    return makeNamedType("io_mode");
}

TcType *makeFTypeType(void) {
    return makeNamedType("ftype_type");
}

static TcType *analyzeTypeOf(LamExp *exp, TcEnv *env, TcNg *ng) {
    // Analyze the inner expression to get its type
    TcType *type = analyzeExp(exp->val.typeOf->exp, env, ng);
    int save = PROTECT(type);
    // Convert the type to a string representation
    char *typeString = tcTypeToString(prune(type));
    // Convert the C string to a lambda list of chars
    LamExp *stringExp = stringToLamArgs(CPI(exp), typeString);
    PROTECT(stringExp);
    free(typeString);           // Free the temporary C string
    // Replace just the type discriminator and union value, preserving header
    exp->type = stringExp->type;
    exp->val = stringExp->val;
    // Also copy the parser info
    exp->_yy_parser_info = CPI(stringExp);
    // Create the return type before unprotecting
    TcType *stringType = makeStringType();
    UNPROTECT(save);
    return stringType;
}

static TcType *analyzeExp(LamExp *exp, TcEnv *env, TcNg *ng) {
    if (exp == NULL)
        return NULL;
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            return prune(analyzeLam(exp->val.lam, env, ng));
        case LAMEXP_TYPE_VAR:
            return prune(analyzeVar(CPI(exp), exp->val.var, env, ng));
        case LAMEXP_TYPE_STDINT:
            return prune(analyzeSmallInteger());
        case LAMEXP_TYPE_BIGINTEGER:
            return prune(analyzeBigInteger());
        case LAMEXP_TYPE_PRIM:
            return prune(analyzePrim(exp->val.prim, env, ng));
        case LAMEXP_TYPE_SEQUENCE:
            return prune(analyzeSequence(exp->val.sequence, env, ng));
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
        case LAMEXP_TYPE_TYPEOF:
            return analyzeTypeOf(exp, env, ng);
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
        case LAMEXP_TYPE_NAMESPACES:
            return prune(analyzeNamespaces(exp->val.namespaces, env, ng));
        case LAMEXP_TYPE_ENV:
            return prune(analyzeEnv(env));
        case LAMEXP_TYPE_LOOKUP:
            return prune(analyzeLookup(exp->val.lookup, env, ng));
        case LAMEXP_TYPE_CONSTRUCTOR:
            return
                prune(analyzeVar
                      (CPI(exp), exp->val.constructor->name, env, ng));
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
    env = newTcEnv(env);
    int save = PROTECT(env);
    ng = newTcNg(ng);
    PROTECT(ng);
    for (LamVarList * args = lam->args; args != NULL; args = args->next) {
        TcType *freshType = makeFreshVar(args->var->name);
        int save2 = PROTECT(freshType);
        addToEnv(env, args->var, freshType);
        addToNg(ng, freshType);
        UNPROTECT(save2);
    }
    TcType *returnType = analyzeExp(lam->exp, env, ng);
    PROTECT(returnType);
    
    // Zero-argument lambda creates a thunk type, not a function type
    TcType *functionType;
    if (lam->args == NULL) {
        // fn() { expr } produces thunk(T) where T is the type of expr
        functionType = makeThunk(returnType);
    } else {
        // Regular function with arguments
        functionType = makeFunctionType(lam->args, env, returnType);
    }
    UNPROTECT(save);
    // LEAVE(analyzeLam);
    return functionType;
}

static TcType *analyzeVar(ParserInfo I, HashSymbol *var, TcEnv *env, TcNg *ng) {
    // ENTER(analyzeVar);
    TcType *res = lookup(env, var, ng);
    if (res == NULL) {
        // ppTcEnv(env);
        can_happen("undefined variable %s in %s, line %d", var->name,
                    I.filename, I.lineno);
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
        eprintf(" (type: ");
        ppTcType(prune(type1));
        eprintf(")\nwith\n");
        ppLamExp(exp2);
        eprintf(" (type: ");
        ppTcType(prune(type2));
        eprintf(")\n");
        REPORT_PARSER_INFO(exp1);
        if (!EQ_PARSER_INFO(exp1, exp2)) {
            REPORT_PARSER_INFO(exp2);
        }
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
        eprintf(" (type: ");
        ppTcType(prune(type1));
        eprintf(")\nwith\n");
        ppLamExp(exp2);
        eprintf(" (type: ");
        ppTcType(prune(type2));
        eprintf(")\n");
        REPORT_PARSER_INFO(exp1);
        if (!EQ_PARSER_INFO(exp1, exp2)) {
            REPORT_PARSER_INFO(exp2);
        }
    }
    UNPROTECT(save);
    TcType *res = makeSpaceship();
    // LEAVE(analyzeComparison);
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
        default:
            cant_happen("unrecognised type %d in analyzePrim", app->type);
    }
    // LEAVE(analyzePrim);
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
    LamExp *constructor = newLamExp_Var(CPI(construct), construct->name);
    int save = PROTECT(constructor);
    LamApply *apply =
        newLamApply(CPI(construct), constructor, construct->args);
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
        cant_happen("findNthArg given non-function type %s",
                    tcTypeTypeName(fn->type));
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
    // eprintf("analyze deconstruct %s\n", deconstruct->name->name);
    // ppTcEnv(env);
    TcType *constructor =
        lookupConstructorType(deconstruct->name, deconstruct->nsid, env, ng);
    int save = PROTECT(constructor);
    // ppTcType(constructor); eprintf("\n");
    if (constructor == NULL) {
        can_happen("undefined type deconstructor %s",
                   deconstruct->name->name);
        REPORT_PARSER_INFO(deconstruct);
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
        REPORT_PARSER_INFO(deconstruct);
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
        REPORT_PARSER_INFO(index->exp);
        ppTcType(tuple);
        eprintf("\n");
        ppLamTupleIndex(index);
        eprintf("\n");
        HashSymbol *name = newSymbol("tuple");
        UNPROTECT(save);
        return makeUnknown(name);
    }
    UNPROTECT(save);
    return template->val.tuple->entries[index->vec];
}

static TcType *analyzeMakeTuple(LamArgs *tuple, TcEnv *env, TcNg *ng) {
    TcTypeArray *values = newTcTypeArray();
    int save = PROTECT(values);
    while (tuple != NULL) {
        TcType *part = analyzeExp(tuple->exp, env, ng);
        int save2 = PROTECT(part);
        pushTcTypeArray(values, part);
        UNPROTECT(save2);
        tuple = tuple->next;
    }
    TcType *res = newTcType_Tuple(values);
    UNPROTECT(save);
    return res;
}

TcType *lookupNsRef(int index, TcEnv *env) {
#ifdef SAFETY_CHECKS
    if (index == NS_GLOBAL) {
        cant_happen("lookupNsRef passed global namespace");
    }
#endif
    Index i = index;
    TcType *nsType = NULL;
    if (!getFromTcEnv(env, namespacesSymbol(), &nsType)) {
        cant_happen("failed to retrieve namespaces");
    }
#ifdef SAFETY_CHECKS
    if (nsType->type != TCTYPE_TYPE_NAMESPACES) {
        cant_happen("expected namespaces");
    }
    if (i >= nsType->val.namespaces->size) {
        cant_happen("index out of range");
    }
#endif
    return nsType->val.namespaces->entries[i];
}

static TcType *analyzeLookup(LamLookup *lookup, TcEnv *env, TcNg *ng) {
    TcType *nsType = lookupNsRef(lookup->nsid, env);
    return analyzeExp(lookup->exp, nsType->val.env, ng);
}

static TcType *analyzeNamespaces(LamNamespaceArray *nsArray, TcEnv *env,
                                 TcNg *ng) {
    TcType *nsType = NULL;
    if (!getFromTcEnv(env, namespacesSymbol(), &nsType)) {
        cant_happen("failed to retrieve namespaces");
    }
    for (Index i = 0; i < nsArray->size; i++) {
        TcEnv *env2 = newTcEnv(env);
        int save = PROTECT(env2);
        TcNg *ng2 = newTcNg(ng);
        PROTECT(ng2);
        TcType *nsId = newTcType_Nsid((int) i);
        PROTECT(nsId);
        addToEnv(env2, namespaceSymbol(), nsId);
        TcType *res = analyzeExp(nsArray->entries[i], env2, ng2);
        PROTECT(res);
        pushTcNamespaceArray(nsType->val.namespaces, res);
        UNPROTECT(save);
    }
    return nsType;
}

static TcType *analyzeEnv(TcEnv *env) {
    return newTcType_Env(env);
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
                                     LamArgs *args) {
    if (nargs == 1) {
        LamApply *res = newLamApply(CPI(function), function, args);
        return res;
    }
    LamArgs *singleArg = newLamArgs(CPI(args), args->exp, NULL);
    int save = PROTECT(singleArg);
    LamApply *new = newLamApply(CPI(function), function, singleArg);
    PROTECT(new);
    LamExp *newFunction = newLamExp_Apply(CPI(new), new);
    PROTECT(newFunction);
    LamApply *curried =
        curryLamApplyHelper(nargs - 1, newFunction, args->next);
    UNPROTECT(save);
    return curried;
}

static LamApply *curryLamApply(LamApply *apply) {
    return curryLamApplyHelper(countLamArgs(apply->args), apply->function,
                               apply->args);
}

static TcType *analyzeApply(LamApply *apply, TcEnv *env, TcNg *ng) {
// ENTER(analyzeApply);
    switch (countLamArgs(apply->args)) {
        case 0:
            {
                TcType *fnType = analyzeExp(apply->function, env, ng);
                int save = PROTECT(fnType);
                fnType = prune(fnType);
                
                // Check if this is a thunk being forced
                if (fnType->type == TCTYPE_TYPE_THUNK) {
                    // Forcing a thunk extracts its underlying type
                    TcType *res = fnType->val.thunk->type;
                    UNPROTECT(save);
                    return res;
                }
                
                // If it's a type variable, constrain it to be a thunk
                if (fnType->type == TCTYPE_TYPE_VAR) {
                    // Create a fresh result type
                    TcType *resultType = makeFreshVar("thunk_result");
                    PROTECT(resultType);
                    // Create a thunk type wrapping the result
                    TcType *thunkType = makeThunk(resultType);
                    PROTECT(thunkType);
                    // Unify the variable with the thunk type
                    if (!unify(fnType, thunkType, "zero-arg apply")) {
                        eprintf("while analyzing zero-arg application of:\n");
                        ppLamExp(apply->function);
                        eprintf("\n");
                        REPORT_PARSER_INFO(apply->function);
                    }
                    UNPROTECT(save);
                    return resultType;
                }
                
                // Otherwise, return the function type as-is
                // (for zero-arg functions that return function types)
                UNPROTECT(save);
                // LEAVE(analyzeApply);
                return fnType;
            }
        case 1:
            {
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
                    eprintf(" (type: ");
                    ppTcType(prune(fn));
                    eprintf(") to ");
                    ppLamExp(apply->args->exp);
                    eprintf(" (type: ");
                    ppTcType(prune(arg));
                    eprintf(")\n");
                    REPORT_PARSER_INFO(apply->function);
                    if (!EQ_PARSER_INFO(apply->function, apply->args)) {
                        REPORT_PARSER_INFO(apply->args);
                    }
                }
                UNPROTECT(save);
                // LEAVE(analyzeApply);
                res = prune(res);
                // #d/#b
                return res;
            }
        default:
            {
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
        eprintf(" (type: ");
        ppTcType(prune(consequent));
        eprintf(")\nwith alternative:\n");
        ppLamExp(iff->alternative);
        eprintf(" (type: ");
        ppTcType(prune(alternative));
        eprintf(")\n");
        REPORT_PARSER_INFO(iff->consequent);
        if (!EQ_PARSER_INFO(iff->consequent, iff->alternative)) {
            REPORT_PARSER_INFO(iff->alternative);
        }
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
        REPORT_PARSER_INFO(called);
    }
    UNPROTECT(save);
    return a;
}

static TcType *analyzePrint(LamPrint *print, TcEnv *env, TcNg *ng) {
// a -> a, but installs a printer for type a
// ENTER(analyzePrint);
    TcType *type = analyzeExp(print->exp, env, ng);
    int save = PROTECT(type);
    print->printer = compilePrinterForType(CPI(print), type, env);
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
    TcNg *ng2 = newTcNg(ng);
    PROTECT(ng2);
    addToNg(ng2, existingType);
    TcType *type = analyzeExp(bindings->val, env, ng2);
    PROTECT(type);
    if (!unify(existingType, type, "letrec")) {
        eprintf("while unifying letrec %s with ", bindings->var->name);
        ppLamExp(bindings->val);
        eprintf("\nExisting Type: ");
        ppTcType(existingType);
        eprintf("\nNew Type: ");
        ppTcType(type);
        eprintf("\n");
        REPORT_PARSER_INFO(bindings->val);
    }
    DEBUGN("analyzeLetRec %s :: ", bindings->var->name);
    IFDEBUGN(ppTcType(existingType));
    UNPROTECT(save);
}

// Helper to capture a snapshot of all binding types as a single string
// Used to detect convergence in iterative type checking
static char *snapshotBindingTypes(LamLetRecBindings *bindings, TcEnv *env) {
    int capacity = 256;
    int size = 0;
    char *snapshot = malloc(capacity);

    for (LamLetRecBindings * b = bindings; b != NULL; b = b->next) {
        if (isLambdaBinding(b)) {
            TcType *type = NULL;
            if (getFromTcEnv(env, b->var, &type)) {
                TcType *pruned = prune(type);
                char *typeStr = tcTypeToString(pruned);
                int nameLen = strlen(b->var->name);
                int typeLen = strlen(typeStr);
                int needed = size + nameLen + typeLen + 3;      // "::" + ";"

                if (needed >= capacity) {
                    capacity = needed * 2;
                    char *newSnapshot = malloc(capacity);
                    memcpy(newSnapshot, snapshot, size);
                    free(snapshot);
                    snapshot = newSnapshot;
                }

                memcpy(snapshot + size, b->var->name, nameLen);
                size += nameLen;
                snapshot[size++] = ':';
                snapshot[size++] = ':';
                memcpy(snapshot + size, typeStr, typeLen);
                size += typeLen;
                snapshot[size++] = ';';

                free(typeStr);  // tcTypeToString uses malloc, so we use free
            }
        }
    }
    snapshot[size] = '\0';
    return snapshot;
}

static TcType *analyzeLetRec(LamLetRec *letRec, TcEnv *env, TcNg *ng) {
// ENTER(analyzeLetRec);
    env = newTcEnv(env);
    int save = PROTECT(env);
    ng = newTcNg(ng);
    PROTECT(ng);
// bind lambdas early
    for (LamLetRecBindings * bindings = letRec->bindings; bindings != NULL;
         bindings = bindings->next) {
        if (isLambdaBinding(bindings)) {
            prepareLetRecEnv(bindings, env);
        }
    }
    for (LamLetRecBindings * bindings = letRec->bindings; bindings != NULL;
         bindings = bindings->next) {
        DEBUGN("analyzeLetRec %s => ", bindings->var->name);
        IFDEBUGN(ppLamExp(bindings->val));
        if (!isLambdaBinding(bindings)) {
            prepareLetRecEnv(bindings, env);
        }
        processLetRecBinding(bindings, env, ng);
    }
// Iterate additional passes to allow type constraints to propagate through
// forward references. Stop early when types converge (no changes between passes).
// In practice, most code needs 2-3 passes, complex mutual recursion might need more.
    const int MAX_PASSES = 10;
    int passCount __attribute__((unused)) = 1;
    char *prevSnapshot = NULL;

    for (int pass = 2; pass <= MAX_PASSES && !hadErrors(); pass++) {
        passCount = pass;

        for (LamLetRecBindings * bindings = letRec->bindings;
             bindings != NULL; bindings = bindings->next) {
            if (isLambdaBinding(bindings)) {
                processLetRecBinding(bindings, env, ng);
            }
        }

// Check if types have converged
        char *currentSnapshot = snapshotBindingTypes(letRec->bindings, env);
        if (prevSnapshot != NULL
            && strcmp(prevSnapshot, currentSnapshot) == 0) {
            // No changes this pass - we've converged!
            DEBUGN("analyzeLetRec converged after %d passes\n", passCount);
            free(currentSnapshot);
            free(prevSnapshot);
            prevSnapshot = NULL;
            break;
        }

        if (prevSnapshot != NULL) {
            // eprintf("snapshot %s != %s\n", prevSnapshot, currentSnapshot);
            free(prevSnapshot);
        }
        prevSnapshot = currentSnapshot;
    }

    if (prevSnapshot != NULL) {
        // eprintf("analyzeLetRec completed after %d passes\n", passCount);
        free(prevSnapshot);
    }

    TcType *res = analyzeExp(letRec->body, env, ng);
    UNPROTECT(save);
// LEAVE(analyzeLetRec);
    return res;
}

static TcTypeSigArgs *makeTcTypeSigArgs(LamTypeSigArgs *lamTypeArgs,
                                        TcTypeTable *map) {
    if (lamTypeArgs == NULL) {
        return NULL;
    }
    TcTypeSigArgs *next = makeTcTypeSigArgs(lamTypeArgs->next, map);
    int save = PROTECT(next);
    TcType *name = NULL;
    if (!getTcTypeTable(map, lamTypeArgs->name, &name)) {
        name = makeVar(lamTypeArgs->name);
        int save2 = PROTECT(name);
        setTcTypeTable(map, lamTypeArgs->name, name);
        UNPROTECT(save2);
    }
    TcTypeSigArgs *this = newTcTypeSigArgs(name, next);
    UNPROTECT(save);
    return this;
}

TcType *makeTypeSig(HashSymbol *name, TcTypeSigArgs *args, int nsid) {
    if (strcmp(name->name, "list") == 0 && nsid != -1) {
        cant_happen("list in ns %d", nsid);
    }
    TcTypeSig *tcTypeSig = newTcTypeSig(name, args, nsid);
    int save = PROTECT(tcTypeSig);
    TcType *res = newTcType_TypeSig(tcTypeSig);
    UNPROTECT(save);
    return res;
}

static TcType *makeTcTypeSig(LamTypeSig *lamType, TcTypeTable *map, int nsid) {
    TcTypeSigArgs *args = makeTcTypeSigArgs(lamType->args, map);
    int save = PROTECT(args);
    TcType *res = makeTypeSig(lamType->name, args, nsid);
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
    TcType *res = newTcType_Tuple(array);
    UNPROTECT(save);
    return res;
}

static TcType *makeTypeConstructorArg(LamTypeConstructorType * arg,
                                      TcTypeTable * map, TcEnv * env);

static TcTypeArray *makeTupleArray(LamTypeConstructorArgs *args,
                                   TcTypeTable *map, TcEnv *env) {
    TcTypeArray *array = newTcTypeArray();
    int save = PROTECT(array);
    while (args != NULL) {
        TcType *arg = makeTypeConstructorArg(args->arg, map, env);
        int save2 = PROTECT(arg);
        pushTcTypeArray(array, arg);
        UNPROTECT(save2);
        args = args->next;
    }
    UNPROTECT(save);
    return array;
}

static TcTypeSigArgs *makeTypeSigArgs(LamTypeConstructorArgs *args,
                                      TcTypeTable *map, TcEnv *env) {
    if (args == NULL) {
        return NULL;
    }
    TcTypeSigArgs *next = makeTypeSigArgs(args->next, map, env);
    int save = PROTECT(next);
    TcType *arg = makeTypeConstructorArg(args->arg, map, env);
    PROTECT(arg);
    TcTypeSigArgs *this = newTcTypeSigArgs(arg, next);
    UNPROTECT(save);
    return this;
}

static int findNamespace(LamLookupOrSymbol *los, TcEnv *env) {
    switch (los->type) {
        case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:
            return los->val.lookup->nsid;
        case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
            {
                // eprintf("looking for %s in ", los->val.symbol->name);
                // ppTcEnv(env);
                TcTypeSig *typeSig;
                if (getTypeSigFromTcEnv(env, los->val.symbol, &typeSig)) {
                    return typeSig->ns;
                }
                TcType *ns = NULL;
                getFromTcEnv(env, namespaceSymbol(), &ns);
#ifdef SAFETY_CHECKS
                if (ns == NULL) {
                    cant_happen("cannot locate current namespace");
                }
#endif
                return ns->val.nsid;
            }
        default:
            cant_happen("unrecognized %s",
                        lamLookupOrSymbolTypeName(los->type));
    }
}

static HashSymbol *getUnderlyingFunction(LamLookupOrSymbol *los) {
    switch (los->type) {
        case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:
            return los->val.lookup->symbol;
        case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
            return los->val.symbol;
        default:
            cant_happen("unrecognized %s",
                        lamLookupOrSymbolTypeName(los->type));
    }
}

static TcType *makeTypeConstructorApplication(LamTypeFunction *func,
                                              TcTypeTable *map, TcEnv *env) {
// this code is building the inner application of a type, i.e.
// list(t) in the context of t -> list(t) -> list(t)
    TcTypeSigArgs *args = makeTypeSigArgs(func->args, map, env);
    int save = PROTECT(args);
    int ns = findNamespace(func->name, env);
    TcType *res = makeTypeSig(getUnderlyingFunction(func->name), args, ns);
    UNPROTECT(save);
    return res;
}

static TcType *makeTupleApplication(LamTypeConstructorArgs *tuple,
                                    TcTypeTable *map, TcEnv *env) {
    TcTypeArray *array = makeTupleArray(tuple, map, env);
    int save = PROTECT(array);
    TcType *res = newTcType_Tuple(array);
    UNPROTECT(save);
    return res;
}

static TcType *makeTypeConstructorArg(LamTypeConstructorType *arg,
                                      TcTypeTable *map, TcEnv *env) {
    TcType *res = NULL;
    switch (arg->type) {
        case LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER:
            res = makeBigInteger();
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER:
            res = makeCharacter();
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_VAR:
            {
                if (!getTcTypeTable(map, arg->val.var, &res)) {
                    res = makeVar(arg->val.var);
                    int save = PROTECT(res);
                    setTcTypeTable(map, arg->val.var, res);
                    UNPROTECT(save);
                }
            }
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
            res = makeTypeConstructorApplication(arg->val.function, map, env);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_TUPLE:
            res = makeTupleApplication(arg->val.tuple, map, env);
            break;
        default:
            cant_happen("unrecognised type %s in makeTypeConstructorArg",
                        lamTypeConstructorTypeTypeName(arg->type));
    }
    return res;
}

static TcType *makeTypeDefConstructor(LamTypeConstructorArgs *args,
                                      TcType *result, TcTypeTable *map,
                                      TcEnv *env) {
// this code is building the top-level type of a type constructor, i.e.
// pair => t -> list(t) -> list(t)
    if (args == NULL) {
        return result;
    }
    TcType *next = makeTypeDefConstructor(args->next, result, map, env);
    int save = PROTECT(next);
    TcType *this = makeTypeConstructorArg(args->arg, map, env);
    PROTECT(this);
    TcType *res = makeFn(this, next);
    UNPROTECT(save);
    return res;
}

static void collectTypeDefConstructor(LamTypeConstructor *constructor,
                                      TcType *type, TcEnv *env,
                                      TcTypeTable *map) {
    TcType *res = makeTypeDefConstructor(constructor->args, type, map, env);
    int save = PROTECT(res);
    addToEnv(env, constructor->name, res);
    UNPROTECT(save);
}

static void collectTypeDef(LamTypeDef *lamTypeDef, TcEnv *env) {
    TcTypeTable *map = newTcTypeTable();
    int save = PROTECT(map);
    LamTypeSig *lamType = lamTypeDef->type;
    TcType *ns = NULL;
    getFromTcEnv(env, namespaceSymbol(), &ns);
#ifdef SAFETY_CHECKS
    if (ns == NULL) {
        cant_happen("cannot find namespace in env");
    }
    if (ns->type != TCTYPE_TYPE_NSID) {
        cant_happen("namespace corrupted");
    }
#endif
    TcType *tcType = makeTcTypeSig(lamType, map, ns->val.nsid);
    PROTECT(tcType);
    addTypeSigToEnv(env, tcType->val.typeSig->name, tcType->val.typeSig);
    for (LamTypeConstructorList * list = lamTypeDef->constructors;
         list != NULL; list = list->next) {
        collectTypeDefConstructor(list->constructor, tcType, env, map);
    }
    UNPROTECT(save);
}

static TcType *analyzeTypeDefs(LamTypeDefs *typeDefs, TcEnv *env, TcNg *ng) {
// ENTER(analyzeTypeDefs);
    env = newTcEnv(env);
    int save = PROTECT(env);
    for (LamTypeDefList * list = typeDefs->typeDefs; list != NULL;
         list = list->next) {
        collectTypeDef(list->typeDef, env);
    }
    TcType *res = analyzeExp(typeDefs->body, env, ng);
    UNPROTECT(save);
// LEAVE(analyzeTypeDefs);
    return res;
}

static void analyzeLetBindings(LamLetBindings *bindings, TcEnv *env,
                                TcNg *ng) {
    for (LamLetBindings * b = bindings; b != NULL; b = b->next) {
        TcType *type = analyzeExp(b->val, env, ng);
        int save = PROTECT(type);
        addToEnv(env, b->var, type);
        UNPROTECT(save);
    }
}

static TcType *analyzeLet(LamLet *let, TcEnv *env, TcNg *ng) {
// ENTER(analyzeLet);
// let expression is evaluated in the current environment
    env = newTcEnv(env);
    int save = PROTECT(env);
    analyzeLetBindings(let->bindings, env, ng);
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
        REPORT_PARSER_INFO(cases->body);
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
        REPORT_PARSER_INFO(exp);
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
        REPORT_PARSER_INFO(exp);
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
        REPORT_PARSER_INFO(exp);
    }
    UNPROTECT(save);
// LEAVE(analyzeBooleanExp);
    return boolean;
}

static TcType *lookupConstructorType(HashSymbol *name, int nsid, TcEnv *env,
                                     TcNg *ng) {
    TcType *currentNamespace = NULL;
    getFromTcEnv(env, namespaceSymbol(), &currentNamespace);
#ifdef SAFETY_CHECKS
    if (currentNamespace == NULL) {
        cant_happen("cannot locate current namespace");
    }
#endif
    TcType *res = NULL;
    if (currentNamespace->val.nsid == nsid || nsid == NS_GLOBAL) {
        res = lookup(env, name, ng);
    } else {
        TcType *nsType = lookupNsRef(nsid, env);
        res = lookup(nsType->val.env, name, ng);
    }
    if (res == NULL) {
        cant_happen("lookupConstructorType %s failed (nsid %d)", name->name,
                    nsid);
    }
    return res;
}

static TcType *analyzeIntList(LamIntList *intList, TcEnv *env, TcNg *ng) {
    if (intList == NULL) {
        return makeFreshVar("intList");
    }
    TcType *next = analyzeIntList(intList->next, env, ng);
    int save = PROTECT(next);
    TcType *this =
        lookupConstructorType(intList->name, intList->nsid, env, ng);
    PROTECT(this);
    this = findResultType(this);
    PROTECT(this);
    if (!unify(next, this, "int list")) {
        eprintf("while analyzing intList case %s\n", intList->name->name);
        REPORT_PARSER_INFO(intList);
    }
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
        REPORT_PARSER_INFO(matchList);
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
        REPORT_PARSER_INFO(match);
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
        REPORT_PARSER_INFO(cases->body);
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
        REPORT_PARSER_INFO(cases->body);
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
        case LAMCONDCASES_TYPE_INTEGERS:
            {
                TcType *integer = makeBigInteger();
                PROTECT(integer);
                if (!unify(value, integer, "cond[1]")) {
                    eprintf("while analyzing integer cond:\n");
                    REPORT_PARSER_INFO(cond->value);
                    ppLamExp(cond->value);
                    eprintf("\n");
                }
                result =
                    analyzeIntCondCases(cond->cases->val.integers, env, ng);
            }
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:
            {
                TcType *character = makeCharacter();
                PROTECT(character);
                if (!unify(value, character, "cond[2]")) {
                    eprintf("while analyzing character cond:\n");
                    REPORT_PARSER_INFO(cond->value);
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
        REPORT_PARSER_INFO(amb->left);
        if (!EQ_PARSER_INFO(amb->left, amb->right)) {
            REPORT_PARSER_INFO(amb->right);
        }
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

static void addTypeSigToEnv(TcEnv *env, HashSymbol *symbol, TcTypeSig *type) {
    setTcTypeSigTable(env->typeSigs, symbol, type);
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

bool getTypeSigFromTcEnv(TcEnv *env, HashSymbol *symbol, TcTypeSig **type) {
    if (env == NULL) {
        return false;
    }
    if (getTcTypeSigTable(env->typeSigs, symbol, type)) {
        return true;
    }
    return getTypeSigFromTcEnv(env->next, symbol, type);
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
    TcType *res = newTcType_Pair(resPair);
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

static TcType *freshThunk(TcThunk *thunk, TcNg *ng, TcTypeTable *map) {
    TcType *type = freshRec(thunk->type, ng, map);
    int save = PROTECT(type);
    TcType *res = makeThunk(type);
    UNPROTECT(save);
    return res;
}

static TcTypeSigArgs *freshTypeSigArgs(TcTypeSigArgs *args, TcNg *ng,
                                       TcTypeTable *map) {
    if (args == NULL)
        return NULL;
    TcTypeSigArgs *next = freshTypeSigArgs(args->next, ng, map);
    int save = PROTECT(next);
    TcType *type = freshRec(args->type, ng, map);
    PROTECT(type);
    TcTypeSigArgs *this = newTcTypeSigArgs(type, next);
    UNPROTECT(save);
    return this;
}

static TcType *freshTypeSig(TcTypeSig *typeSig, TcNg *ng, TcTypeTable *map) {
    TcTypeSigArgs *args = freshTypeSigArgs(typeSig->args, ng, map);
    int save = PROTECT(args);
    TcType *res = makeTypeSig(typeSig->name, args, typeSig->ns);
    UNPROTECT(save);
    return res;
}

static TcType *freshTuple(TcTypeArray *tuple, TcNg *ng, TcTypeTable *map) {
    TcTypeArray *fresh = newTcTypeArray();
    int save = PROTECT(fresh);
    for (Index i = 0; i < tuple->size; i++) {
        TcType *part = freshRec(tuple->entries[i], ng, map);
        int save2 = PROTECT(part);
        pushTcTypeArray(fresh, part);
        UNPROTECT(save2);
    }
    TcType *res = newTcType_Tuple(fresh);
    UNPROTECT(save);
    return res;
}

static bool isGeneric(TcType *typeVar, TcNg *ng) {
    while (ng != NULL) {
        Index i = 0;
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
        case TCTYPE_TYPE_FUNCTION:
            {
                TcType *res = freshFunction(type->val.function, ng, map);
                return res;
            }
        case TCTYPE_TYPE_PAIR:
            {
                TcType *res = freshPair(type->val.pair, ng, map);
                return res;
            }
        case TCTYPE_TYPE_THUNK:
            {
                TcType *res = freshThunk(type->val.thunk, ng, map);
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
        case TCTYPE_TYPE_OPAQUE:
            return type;
        case TCTYPE_TYPE_TYPESIG:
            {
                TcType *res = freshTypeSig(type->val.typeSig, ng, map);
                return res;
            }
        case TCTYPE_TYPE_TUPLE:
            return freshTuple(type->val.tuple, ng, map);
        case TCTYPE_TYPE_ENV:
            return type;
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

TcType *makeBoolean() {
    TcType *res = makeTypeSig(boolSymbol(), NULL, NS_GLOBAL);
    return res;
}

static TcType *makeSpaceship() {
    TcType *res = makeTypeSig(spaceshipSymbol(), NULL, NS_GLOBAL);
    return res;
}

static TcType *makeFn(TcType *arg, TcType *result) {
    arg = prune(arg);
    result = prune(result);
    TcFunction *fn = newTcFunction(arg, result);
    int save = PROTECT(fn);
    TcType *type = newTcType_Function(fn);
    UNPROTECT(save);
    return type;
}

static TcType *makeThunk(TcType *type) {
    type = prune(type);
    TcThunk *thunk = newTcThunk(type);
    int save = PROTECT(thunk);
    TcType *res = newTcType_Thunk(thunk);
    UNPROTECT(save);
    return res;
}

static TcType *makeVar(HashSymbol *t) {
    TcVar *var = newTcVar(t, id_counter++);
    int save = PROTECT(var);
    TcType *res = newTcType_Var(var);
    UNPROTECT(save);
    return res;
}

TcType *makeFreshVar(char *name __attribute__((unused))) {
    return makeVar(genAlphaSym("#"));
}

TcType *makeSmallInteger() {
    TcType *res = newTcType_Smallinteger();
    return res;
}

static TcType *makeBigInteger() {
    TcType *res = newTcType_Biginteger();
    return res;
}

static TcType *makeUnknown(HashSymbol *var) {
    TcType *res = newTcType_Unknown(var);
    return res;
}

static TcType *makeCharacter() {
    TcType *res = newTcType_Character();
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

static TcType *builtInArgsToType(BuiltInArgs *args, int pos, TcType *follows) {
    int save = PROTECT(follows);
    while (pos > 0) {
        TcType *this = args->entries[--pos];
        PROTECT(this);
        follows = makeFn(this, follows);
        PROTECT(follows);
    }
    UNPROTECT(save);
    return follows;
}

static TcType *constructBuiltInType(BuiltIn *builtIn) {
    return builtInArgsToType(builtIn->args, builtIn->args->size,
                             builtIn->result);
}

static void addBuiltInToEnv(TcEnv *env, BuiltIn *builtIn) {
    TcType *type = constructBuiltInType(builtIn);
    int save = PROTECT(type);
    // Bind only internal name; external name supplied by wrapper definitions.
    addToEnv(env, builtIn->internalName, type);
    UNPROTECT(save);
}

static void addBuiltinsToEnv(TcEnv *env, BuiltIns *builtIns) {
    for (Index i = 0; i < builtIns->size; i++) {
        addBuiltInToEnv(env, builtIns->entries[i]);
    }
}

static void addNamespacesToEnv(TcEnv *env) {
    TcNamespaceArray *namespaces = newTcNamespaceArray();
    int save = PROTECT(namespaces);
    TcType *nsType = newTcType_Namespaces(namespaces);
    PROTECT(nsType);
    addToEnv(env, namespacesSymbol(), nsType);
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

static void addThenToEnv(TcEnv *env) {
// a -> a -> a
    TcType *freshType = makeFreshVar(thenSymbol()->name);
    int save = PROTECT(freshType);
    addBinOpToEnv(env, thenSymbol(), freshType);
    UNPROTECT(save);
}

static bool failUnify(TcType *a, TcType *b, char *reason) {
    // can_happen sets a flag that will prevent later stages
    can_happen("\nunification failed [%s]", reason);
    ppTcType(a);
    eprintf(" vs ");
    ppTcType(b);
    eprintf("\n");
    return false;
}

static bool failUnifyFunctions(TcFunction *a, TcFunction *b, char *reason) {
    // can_happen sets a flag that will prevent later stages
    can_happen("\nunification failed [%s]", reason);
    ppTcFunction(a);
    eprintf(" vs ");
    ppTcFunction(b);
    eprintf("\n");
    return false;
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

static bool unifyThunks(TcThunk *a, TcThunk *b) {
    return unify(a->type, b->type, "thunks");
}

static bool unifyTuples(TcTypeArray *a, TcTypeArray *b) {
    if (a->size != b->size) {
        can_happen("tuple sizes differ: %d vs %d", a->size, b->size);
        return false;
    }
    bool unified = true;
    for (Index i = 0; i < a->size; i++) {
        if (!unify(a->entries[i], b->entries[i], "tuples")) {
            unified = false;
        }
    }
    return unified;
}

static bool unifyOpaque(HashSymbol *a, HashSymbol *b) {
    if (a != b) {
        can_happen("opaque type mismatch %s vs. %s", a->name, b->name);
        return false;
    }
    return true;
}

static bool failUnifyTypeSigs(TcTypeSig *a, TcTypeSig *b, char *reason) {
    can_happen("\nunification failed [%s]", reason);
    ppTcTypeSig(a);
    eprintf(" vs ");
    ppTcTypeSig(b);
    eprintf("\n");
    return false;
}

static bool unifyTypeSigs(TcTypeSig *a, TcTypeSig *b) {
    if (a->name != b->name) {
        return failUnifyTypeSigs(a, b, "usertype name mismatch");
    }
    if (a->ns != b->ns) {
        return failUnifyTypeSigs(a, b, "usertype namespace mismatch");
    }

    TcTypeSigArgs *aArgs = a->args;
    TcTypeSigArgs *bArgs = b->args;
    while (aArgs != NULL && bArgs != NULL) {
        if (!unify(aArgs->type, bArgs->type, "user types")) {
            return false;
        }
        aArgs = aArgs->next;
        bArgs = bArgs->next;
    }
    if (aArgs != NULL || bArgs != NULL) {
        return failUnifyTypeSigs(a, b, "usertype arg count mismatch");
    }
    return true;
}

static bool _unify(TcType *a, TcType *b) {
    a = prune(a);
    b = prune(b);
    if (a == b) {
        return a->type != TCTYPE_TYPE_UNKNOWN;
    }
    if (a->type == TCTYPE_TYPE_VAR) {
        if (b->type != TCTYPE_TYPE_VAR) {
            if (occursInType(a, b)) {
                return failUnify(a, b, "occurs-in check failed");
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
            return failUnify(a, b, "type mismatch");
        }
        switch (a->type) {
            case TCTYPE_TYPE_FUNCTION:
                return unifyFunctions(a->val.function, b->val.function);
            case TCTYPE_TYPE_PAIR:
                return unifyPairs(a->val.pair, b->val.pair);
            case TCTYPE_TYPE_THUNK:
                return unifyThunks(a->val.thunk, b->val.thunk);
            case TCTYPE_TYPE_VAR:
                cant_happen("encountered var in unify");
            case TCTYPE_TYPE_SMALLINTEGER:
            case TCTYPE_TYPE_BIGINTEGER:
            case TCTYPE_TYPE_CHARACTER:
                return true;
            case TCTYPE_TYPE_UNKNOWN:
                return false;
            case TCTYPE_TYPE_TYPESIG:
                return unifyTypeSigs(a->val.typeSig, b->val.typeSig);
            case TCTYPE_TYPE_OPAQUE:
                return unifyOpaque(a->val.opaque, b->val.opaque);
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
IFDEBUGN(eprintf("unify(%s) :> ", trace); ppTcType(a); eprintf(" =?= "); ppTcType(b));
bool res = _unify(a, b);
IFDEBUGN(eprintf("unify(%s) <: ", trace); ppTcType(a); eprintf(" === "); ppTcType(b));
return res;
// *INDENT-ON*
}

static void pruneTypeSigArgs(TcTypeSigArgs *args) {
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
    } else if (t->type == TCTYPE_TYPE_TYPESIG) {
        pruneTypeSigArgs(t->val.typeSig->args);
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

static bool sameTypeSig(TcTypeSig *a, TcTypeSig *b) {
    if (a->name != b->name) {
        return false;
    }
    TcTypeSigArgs *aArgs = a->args;
    TcTypeSigArgs *bArgs = b->args;
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
        case TCTYPE_TYPE_THUNK:
            return sameType(a->val.thunk->type, b->val.thunk->type);
        case TCTYPE_TYPE_VAR:
            return a->val.var->id == b->val.var->id;
        case TCTYPE_TYPE_BIGINTEGER:
        case TCTYPE_TYPE_SMALLINTEGER:
        case TCTYPE_TYPE_CHARACTER:
            return true;
        case TCTYPE_TYPE_UNKNOWN:
            return false;
        case TCTYPE_TYPE_TYPESIG:
            return sameTypeSig(a->val.typeSig, b->val.typeSig);
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

static bool occursInThunk(TcType *var, TcThunk *thunk) {
    return occursInType(var, thunk->type);
}

static bool occursInTypeSig(TcType *var, TcTypeSig *typeSig) {
    for (TcTypeSigArgs * args = typeSig->args; args != NULL;
         args = args->next) {
        if (occursInType(var, args->type))
            return true;
    }
    return false;
}

static bool occursInTuple(TcType *var, TcTypeArray *tuple) {
    for (Index i = 0; i < tuple->size; ++i) {
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
        case TCTYPE_TYPE_THUNK:
            return occursInThunk(a, b->val.thunk);
        case TCTYPE_TYPE_VAR:
            cant_happen("occursIn 2nd arg should not be a var");
        case TCTYPE_TYPE_SMALLINTEGER:
        case TCTYPE_TYPE_BIGINTEGER:
        case TCTYPE_TYPE_CHARACTER:
        case TCTYPE_TYPE_UNKNOWN:
        case TCTYPE_TYPE_ENV:
        case TCTYPE_TYPE_OPAQUE:
            return false;
        case TCTYPE_TYPE_TYPESIG:
            return occursInTypeSig(a, b->val.typeSig);
        case TCTYPE_TYPE_TUPLE:
            return occursInTuple(a, b->val.tuple);
        default:
            cant_happen("unrecognised type %s", tcTypeTypeName(b->type));
    }
}