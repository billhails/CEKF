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

#include "algorithm_W.h"
#include "tin_helper.h"
#include "symbol.h"
#include "debug_tin.h"
#include "debug_ast.h"

static TinMonoType *astTypeToMonoType(AstType *type);
static TinMonoType *constantTypeFunction(HashSymbol *name);
static TinMonoType *singleArgTypeFunction(HashSymbol*name, TinMonoType *arg);
static void addMonoTypeToContext(TinContext *context, HashSymbol *symbol, TinMonoType *monoType);
static void generalizeMonoTypeToContext(TinContext *context, HashSymbol *symbol, TinMonoType *monoType);

static WResult *WNest(TinContext *context, AstNest *nest, int depth);
static WResult *WExpression(TinContext *context, AstExpression *expr, int depth);
static WResult *WApplication(TinContext *context, AstFunCall *funCall, int depth);
static WResult *WFarg(TinContext *context, AstArg *arg, int depth);
static void addVars(TinContext *context, AstArgList *args);

#ifdef DEBUG_ALGORITHM_W
static int idSource = 0;

static void enter(char *name, int id, int depth) {
    bool newline = true;
    if (depth < 0) {
        newline = false;
        depth = -depth;
    }
    int pad = depth * 4 - 8;
    if (pad < 0) pad = 0;
    printf("[%03d]>>>%*s%s", id, pad, "", name);
    if (newline)
        printf("\n");
    else
        printf(" ");
}

static void leave(char *name, int id, int depth) {
    int pad = depth * 4 - 8;
    if (pad < 0) pad = 0;
    printf("[%03d]<<<%*s%s\n", id, pad, "", name);
}
#endif

void markWResult(WResult *result) {
    if (result == NULL) return;
    if (MARKED(result)) return;
    MARK(result);
    markTinSubstitution(result->substitution);
    markTinMonoType(result->monoType);
}

void freeWResult(WResult *x) {
    FREE(x, WResult);
}

void markWResultObj(Header *h) {
    markWResult((WResult *)h );
}

void freeWResultObj(Header *h) {
    freeWResult((WResult *)h );
}

void printWResult(WResult *result, int depth) {
    bool save = quietPrintHashTable;
    quietPrintHashTable = false;
    printf("%*sWResult [\n", depth * 4, "");
    printTinSubstitution(result->substitution, depth + 1);
    printf(",\n%*s", (depth + 1) * 4, "");
    showTinMonoType(result->monoType);
    printf("\n");
    printf("%*s]", depth * 4, "");
    quietPrintHashTable = save;
}

static WResult *newWResult(TinSubstitution *substitution, TinMonoType *monoType) {
    WResult *x = NEW(WResult, OBJTYPE_WRESULT);
    x->substitution = substitution;
    x->monoType = monoType;
    return x;
}

static HashSymbol *arrowSymbol() {
    return newSymbol("->");
}

static HashSymbol *intSymbol() {
    return newSymbol("Int");
}

static HashSymbol *charSymbol() {
    return newSymbol("Char");
}

static HashSymbol *boolSymbol() {
    return newSymbol("Bool");
}

static HashSymbol *stringSymbol() {
    return newSymbol("String");
}

static HashSymbol *listSymbol() {
    return newSymbol("List");
}

static HashSymbol *envSymbol() {
    return newSymbol("Env");
}

static TinPolyType *monoToPolyType(TinMonoType *monoType) {
    return newTinPolyType(
        TINPOLYTYPE_TYPE_MONOTYPE,
        TINPOLYTYPE_VAL_MONOTYPE(monoType)
    );
}

static void generalizeMonoTypeToContext(TinContext *context, HashSymbol *symbol, TinMonoType *monoType) {
    TinPolyType *polyType = generalize(context, monoType);
    int save = PROTECT(polyType);
    addVarToContext(context, symbol, polyType);
    UNPROTECT(save);
}

static void generalizeTypeConstructorToContext(TinContext *context, HashSymbol *symbol, TinMonoType *monoType) {
    TinPolyType *polyType = generalize(context, monoType);
    int save = PROTECT(polyType);
    addConstructorToContext(context, symbol, polyType);
    UNPROTECT(save);
}

static void addMonoTypeToContext(TinContext *context, HashSymbol *symbol, TinMonoType *monoType) {
    TinPolyType *polyType = monoToPolyType(monoType);
    int save = PROTECT(polyType);
    addVarToContext(context, symbol, polyType);
    UNPROTECT(save);
}

static TinMonoType *monoTypeFunApp(TinFunctionApplication *funApp) {
    return newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(funApp)
    );
}

static TinMonoType *monoTypeVar(HashSymbol *var) {
    return newTinMonoType(
        TINMONOTYPE_TYPE_VAR,
        TINMONOTYPE_VAL_VAR(var)
    );
}

static TinMonoType *freshMonoTypeVar(const char *prefix) {
    return monoTypeVar(freshTypeVariable(prefix));
}

static TinFunctionApplication *constantFunApp(HashSymbol *name) {
    return newTinFunctionApplication(name, 0, NULL);
}

static TinFunctionApplication *singleArgFunApp(HashSymbol*name, TinMonoType *arg) {
    TinMonoTypeList *args = newTinMonoTypeList(arg, NULL);
    int save = PROTECT(args);
    TinFunctionApplication *funApp = newTinFunctionApplication(name, 1, args);
    UNPROTECT(save);
    return funApp;
}

static TinMonoType *anyArrowApplication(HashSymbol *arrow, TinMonoType *type1, TinMonoType *type2) {
    TinMonoTypeList *args = newTinMonoTypeList(type2, NULL);
    int save = PROTECT(args);
    args = newTinMonoTypeList(type1, args);
    UNPROTECT(save);
    save = PROTECT(args);
    TinFunctionApplication *funApp = newTinFunctionApplication(arrow, 2, args);
    UNPROTECT(save);
    save = PROTECT(funApp);
    TinMonoType *result = monoTypeFunApp(funApp);
    UNPROTECT(save);
    return result;
}

static TinMonoType *arrowApplication(TinMonoType *type1, TinMonoType *type2) {
    HashSymbol *arrow = arrowSymbol();
    return anyArrowApplication(arrow, type1, type2);
}

static TinMonoType *stringMonoType() {
    TinMonoType *charType = constantTypeFunction(charSymbol());
    int save = PROTECT(charType);
    TinMonoType *result = singleArgTypeFunction(listSymbol(), charType);
    UNPROTECT(save);
    return result;
}

static void collectDefine(AstDefine *define, TinContext *context) {
    TinMonoType *var = freshMonoTypeVar(define->symbol->name);
    int save = PROTECT(var);
    addMonoTypeToContext(context, define->symbol, var);
    UNPROTECT(save);
}

static void collectPrototype(AstPrototype *prototype, TinContext *context) {
    TinMonoType *var = freshMonoTypeVar(prototype->symbol->name);
    int save = PROTECT(var);
    addMonoTypeToContext(context, prototype->symbol, var);
    UNPROTECT(save);
}

static void collectLoad(AstLoad *load, TinContext *context) {
}

static TinMonoTypeList *typeSymbolsToMonoTypeList(AstTypeSymbols *typeSymbols) {
    if (typeSymbols == NULL) return NULL;
    TinMonoTypeList *next = typeSymbolsToMonoTypeList(typeSymbols->next);
    int save = PROTECT(next);
    TinMonoType *monoType = monoTypeVar(typeSymbols->typeSymbol);
    (void) PROTECT(monoType);
    TinMonoTypeList *this = newTinMonoTypeList(monoType, next);
    UNPROTECT(save);
    return this;
}

static int countTypeFunctionArgs(TinMonoTypeList *args) {
    int count = 0;
    while (args != NULL) {
        args = args->next;
        count++;
    }
    return count;
}

static TinMonoType *monoTypeFunctionApplication(HashSymbol *name, TinMonoTypeList *args) {
   int nargs = countTypeFunctionArgs(args);
   TinFunctionApplication *functionApplication = newTinFunctionApplication(name, nargs, args);
   int save = PROTECT(functionApplication);
   TinMonoType *monoType = monoTypeFunApp(functionApplication);
   UNPROTECT(save);
   return monoType;
}

static TinMonoType *flatTypeToFunctionApplication(AstFlatType *flatType) {
   TinMonoTypeList *typeFunctionArgs = typeSymbolsToMonoTypeList(flatType->typeSymbols);
   int save = PROTECT(typeFunctionArgs);
   TinMonoType *monoType = monoTypeFunctionApplication(flatType->symbol, typeFunctionArgs);
   UNPROTECT(save);
   return monoType;
}

static TinMonoType *makeCallMonoType(TinMonoType *this, TinMonoType *rest) {
    TinMonoTypeList *monoTypeList = newTinMonoTypeList(rest, NULL);
    int save = PROTECT(monoTypeList);
    monoTypeList = newTinMonoTypeList(this, monoTypeList);
    UNPROTECT(save);
    save = PROTECT(monoTypeList);
    HashSymbol *symbol = arrowSymbol();
    TinMonoType *result = monoTypeFunctionApplication(symbol, monoTypeList);
    UNPROTECT(save);
    return result;
}

static TinMonoType *makeTypeCallMonoType(TinMonoType *this, TinMonoType *rest) {
    TinMonoTypeList *monoTypeList = newTinMonoTypeList(rest, NULL);
    int save = PROTECT(monoTypeList);
    monoTypeList = newTinMonoTypeList(this, monoTypeList);
    UNPROTECT(save);
    save = PROTECT(monoTypeList);
    HashSymbol *symbol = arrowSymbol();
    TinMonoType *result = monoTypeFunctionApplication(symbol, monoTypeList);
    UNPROTECT(save);
    return result;
}

static TinMonoType *constantTypeFunction(HashSymbol *name) {
    TinFunctionApplication *funApp = constantFunApp(name);
    int save = PROTECT(funApp);
    TinMonoType *monoType = monoTypeFunApp(funApp);
    UNPROTECT(save);
    return monoType;
}

static TinMonoType *singleArgTypeFunction(HashSymbol*name, TinMonoType *arg) {
    TinFunctionApplication *funApp = singleArgFunApp(name, arg);
    int save = PROTECT(funApp);
    TinMonoType *monoType = monoTypeFunApp(funApp);
    UNPROTECT(save);
    return monoType;
}

static TinMonoTypeList *typeListToMonoTypeList(AstTypeList *typeList) {
    if (typeList == NULL) return NULL;
    TinMonoTypeList *next = typeListToMonoTypeList(typeList->next);
    int save = PROTECT(next);
    TinMonoType *this = astTypeToMonoType(typeList->type);
    (void) PROTECT(this);
    TinMonoTypeList *result = newTinMonoTypeList(this, next);
    UNPROTECT(save);
    return result;
}

static TinMonoType *astTypeConstructorToMonoType(AstTypeConstructor *typeConstructor) {
    TinMonoTypeList *typeList = typeListToMonoTypeList(typeConstructor->typeList);
    int save = PROTECT(typeList);
    TinMonoType *type = monoTypeFunctionApplication(typeConstructor->symbol, typeList);
    UNPROTECT(save);
    return type;
}

static TinMonoType *astTypeClauseToMonoType(AstTypeClause *typeClause) {
    switch (typeClause->type) {
        case AST_TYPECLAUSE_TYPE_INTEGER:
            return constantTypeFunction(intSymbol());
        case AST_TYPECLAUSE_TYPE_CHARACTER:
            return constantTypeFunction(charSymbol());
        case AST_TYPECLAUSE_TYPE_BOOLEAN:
            return constantTypeFunction(boolSymbol());
        case AST_TYPECLAUSE_TYPE_STRING:
            return stringMonoType();
        case AST_TYPECLAUSE_TYPE_LIST: {
            TinMonoType *type = astTypeToMonoType(typeClause->val.list);
            int save = PROTECT(type);
            TinMonoType *result = singleArgTypeFunction(listSymbol(), type);
            UNPROTECT(save);
            return result;
        }
        case AST_TYPECLAUSE_TYPE_TYPE:
            return astTypeToMonoType(typeClause->val.type);
        case AST_TYPECLAUSE_TYPE_VAR:
            return monoTypeVar(typeClause->val.var);
        case AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR: {
            return astTypeConstructorToMonoType(typeClause->val.typeConstructor);
        }
        default:
            cant_happen("unrecognised type %d in astTypeClauseToMonoType", typeClause->type);
    }
}

static TinMonoType *astTypeToMonoType(AstType *type) {
    if (type == NULL) return NULL;
    TinMonoType *rest = astTypeToMonoType(type->next);
    int save = PROTECT(rest);
    TinMonoType *this = astTypeClauseToMonoType(type->typeClause);
    if (rest == NULL) {
        UNPROTECT(save);
        return this;
    }
    (void) PROTECT(this);
    TinMonoType *funApp = makeCallMonoType(this, rest);
    UNPROTECT(save);
    return funApp;
}

static TinMonoType *collectTypeList(AstTypeList *typeList, TinMonoType *final) {
    if (typeList == NULL) return final;
    TinMonoType *rest = collectTypeList(typeList->next, final);
    int save = PROTECT(rest);
    TinMonoType *this = astTypeToMonoType(typeList->type);
    (void) PROTECT(this);
    TinMonoType *funMonoType = makeTypeCallMonoType(this, rest);
    UNPROTECT(save);
    return funMonoType;
}

static TinMonoType *collectTypeConstructor(AstTypeConstructor *typeConstructor, TinMonoType *type, TinContext *context) {
    TinMonoType *functionType = collectTypeList(typeConstructor->typeList, type);
    int save = PROTECT(functionType);
    generalizeTypeConstructorToContext(context, typeConstructor->symbol, functionType);
    UNPROTECT(save);
}

static void collectTypeDef(AstTypeDef *typeDef, TinContext *context) {
    TinMonoType *type = flatTypeToFunctionApplication(typeDef->flatType);
    int save = PROTECT(type);
    for (AstTypeBody *typeBody = typeDef->typeBody; typeBody != NULL; typeBody = typeBody->next) {
        AstTypeConstructor *typeConstructor = typeBody->typeConstructor;
        collectTypeConstructor(typeConstructor, type, context);
    }
}

static void collectDefinition(AstDefinition *definition, TinContext *context) {
    switch (definition->type) {
        case AST_DEFINITION_TYPE_DEFINE:
            collectDefine(definition->val.define, context);
            return;
        case AST_DEFINITION_TYPE_PROTOTYPE:
            collectPrototype(definition->val.prototype, context);
            return;
        case AST_DEFINITION_TYPE_LOAD:
            collectLoad(definition->val.load, context);
            return;
        case AST_DEFINITION_TYPE_TYPEDEF:
            collectTypeDef(definition->val.typeDef, context);
            return;
        default:
            cant_happen("unrecognised type %d in collectDefinition", definition->type);
    }
}

static WResult *anyResult() {
    TinMonoType *fresh = freshMonoTypeVar("any");
    int save = PROTECT(fresh);
    TinSubstitution *empty = makeEmptySubstitution();
    (void) PROTECT(empty);
    WResult *r = newWResult(empty, fresh);
    UNPROTECT(save);
    return r;
}

static WResult *constantResult(HashSymbol *typeName) {
    TinMonoType *type = constantTypeFunction(typeName);
    int save = PROTECT(type);
    TinSubstitution *empty = makeEmptySubstitution();
    (void) PROTECT(empty);
    WResult *r = newWResult(empty, type);
    UNPROTECT(save);
    return r;
}

static WResult *stringResult() {
    TinMonoType *type = stringMonoType();
    int save = PROTECT(type);
    TinSubstitution *empty = makeEmptySubstitution();
    (void) PROTECT(empty);
    WResult *r = newWResult(empty, type);
    UNPROTECT(save);
    return r;
}

static WResult *WSymbol(TinContext *context, HashSymbol *symbol, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WSymbol", myId, -depth);
    printHashSymbol(symbol);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    TinPolyType *value = lookupInContext(context, symbol);
    if (value == NULL) {
        can_happen("undefined variable '%s' in WSymbol", symbol->name);
        return anyResult();
    }
    int save = PROTECT(value);
    TinMonoType *instantiated = instantiate(value);
    UNPROTECT(save);
    save = PROTECT(instantiated);
    TinSubstitution *empty = makeEmptySubstitution();
    (void) PROTECT(empty);
    WResult *result = newWResult(empty, instantiated);
    UNPROTECT(save);
#ifdef DEBUG_ALGORITHM_W
    leave("WSymbol", myId, depth);
    printWResult(result, depth+ 1);
    printf("\n");
#endif
    return result;
}

static WResult *WCond(TinContext *context, AstConditional *cond, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WCond", myId, depth);
    printAstConditional(cond, depth + 1);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    // slightly hacky, but DRY, we convert the cond to a 3 valued function
    // "if" and type check that against a pre-supplied context type for "if"
    AstExpression *arg = newAstExpression(
        AST_EXPRESSION_TYPE_NEST,
        AST_EXPRESSION_VAL_NEST(cond->alternative)
    );
    int save = PROTECT(arg);
    AstExpressions *args = newAstExpressions(arg, NULL);
    UNPROTECT(save);
    save = PROTECT(args);
    arg = newAstExpression(
        AST_EXPRESSION_TYPE_NEST,
        AST_EXPRESSION_VAL_NEST(cond->consequent)
    );
    (void) PROTECT(arg);
    args = newAstExpressions(arg, args);
    UNPROTECT(save);
    save = PROTECT(args);
    args = newAstExpressions(cond->expression, args);
    UNPROTECT(save);
    save = PROTECT(args);
    AstExpression *fun = newAstExpression(
        AST_EXPRESSION_TYPE_SYMBOL,
        AST_EXPRESSION_VAL_SYMBOL(newSymbol("if"))
    );
    (void) PROTECT(fun);
    AstFunCall *call = newAstFunCall(fun, args);
    UNPROTECT(save);
    save = PROTECT(call);
    WResult *result = WApplication(context, call, depth + 1);
    UNPROTECT(save);
#ifdef DEBUG_ALGORITHM_W
    leave("WCond", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static int countExpressions(AstExpressions *expressions) {
    int count = 0;
    while (expressions != NULL) {
        count++;
        expressions = expressions->next;
    }
    return count;
}

static AstExpression *nthExpression(int n, AstExpressions *expressions) {
    while (n > 1) { // 1-indexed
        n--;
        expressions = expressions->next;
    }
    return expressions->expression;
}

static AstArg *nthArg(int n, AstArgList *args) {
    while (n > 1) { // 1-indexed
        n--;
        args = args->next;
    }
    return args->arg;
}

static WResult *WApplicationRec(TinContext *context, AstFunCall *funCall, int nargs, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WapplicationRec", myId, -depth);
    printf("%d\n", nargs);
    if (nargs == 0) {
        printAstExpression(funCall->function, depth + 1);
    } else {
        printAstExpression(nthExpression(nargs, funCall->arguments), depth + 1);
    }
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    WResult *result;
    int save = -1;
    if (nargs == 0) {
        result = WExpression(context, funCall->function, depth + 1);
    } else {
        WResult *Rn_1 = WApplicationRec(context, funCall, nargs - 1, depth);                    save = PROTECT(Rn_1);
        TinSubstitution *S1 = Rn_1->substitution;
        TinMonoType *t1 = Rn_1->monoType;
        TinContext *S1Gamma = applyContextSubstitution(S1, context);                            (void) PROTECT(S1Gamma);
        AstExpression *en = nthExpression(nargs, funCall->arguments);
        WResult *Rn = WExpression(S1Gamma, en, depth + 1);                                      (void) PROTECT(Rn);
        TinMonoType *t2 = Rn->monoType;
        TinSubstitution *S2 = Rn->substitution;
        TinMonoType *beta = freshMonoTypeVar("apprec");                                         (void) PROTECT(beta);
        TinMonoType *arrow = arrowApplication(t2, beta);                                        (void) PROTECT(arrow);
        TinMonoType *S2t1 = applyMonoTypeSubstitution(S2, t1);                                  (void) PROTECT(S2t1);
        TinSubstitution *S3 = unify(S2t1, arrow, "WApplicationRec");                            (void) PROTECT(S3);
        TinMonoType *S3Beta = applyMonoTypeSubstitution(S3, beta);                              (void) PROTECT(S3Beta);
        TinSubstitution *S2S1 = applySubstitutionSubstitution(S2, S1);                          (void) PROTECT(S2S1);
        TinSubstitution *S3S2S1 = applySubstitutionSubstitution(S3, S2S1);                      (void) PROTECT(S3S2S1);
        result = newWResult(S3S2S1, S3Beta);
    }
    if (save != -1) {
        UNPROTECT(save);
    }
#ifdef DEBUG_ALGORITHM_W
    leave("WApplicatonRec", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static WResult *WUnpackRec(TinContext *context, AstUnpack *unpack, int nargs, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WUnpackRec", myId, -depth);
    printf("%d\n", nargs);
    if (nargs == 0) {
        printf("%*s", (depth + 1) * 4, "");
        printHashSymbol(unpack->symbol);
    } else {
        printAstArg(nthArg(nargs, unpack->argList), depth + 1);
    }
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    int save = -1;
    WResult *result;
    if (nargs == 0) {
        result = WSymbol(context, unpack->symbol, depth + 1);
    } else {
        WResult *Rn_1 = WUnpackRec(context, unpack, nargs - 1, depth);
        save = PROTECT(Rn_1);
        TinContext *Cn_1 = applyContextSubstitution(Rn_1->substitution, context);
        (void) PROTECT(Cn_1);
        AstArg *e_n = nthArg(nargs, unpack->argList);
        WResult *Rn = WFarg(Cn_1, e_n, depth + 1);
        (void) PROTECT(Rn);
        TinMonoType *beta = freshMonoTypeVar("unpackrec");
        (void) PROTECT(beta);
        TinMonoType *arrow = arrowApplication(Rn->monoType, beta);
        (void) PROTECT(arrow);
        TinMonoType *SnTn_1 = applyMonoTypeSubstitution(Rn->substitution, Rn_1->monoType);
        (void) PROTECT(SnTn_1);
        TinSubstitution *Sprime = unify(SnTn_1, arrow, "WUnpackRec");
        (void) PROTECT(Sprime);
        TinMonoType *SprimeBeta = applyMonoTypeSubstitution(Sprime, beta);
        (void) PROTECT(SprimeBeta);
        TinSubstitution *SnSn_1 = applySubstitutionSubstitution(Rn->substitution, Rn_1->substitution);
        (void) PROTECT(SnSn_1);
        TinSubstitution *SprimeSnSn_1 = applySubstitutionSubstitution(Sprime, SnSn_1);
        (void) PROTECT(SprimeSnSn_1);
        result = newWResult(SprimeSnSn_1, SprimeBeta);
    }
    if (save != -1) UNPROTECT(save);
#ifdef DEBUG_ALGORITHM_W
    leave("WUnpackRec", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static WResult *WApplication(TinContext *context, AstFunCall *funCall, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WApplication", myId, depth);
    printAstFunCall(funCall, depth + 1);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    int nargs = countExpressions(funCall->arguments);
    WResult *result = WApplicationRec(context, funCall, nargs, depth);
#ifdef DEBUG_ALGORITHM_W
    leave("WApplication", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static int countArgList(AstArgList *args) {
    int count = 0;
    while (args != NULL) {
        args = args->next;
        count++;
    }
    return count;
}

static WResult *WUnpack(TinContext *context, AstUnpack *unpack, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WUnpack", myId, depth);
    printAstUnpack(unpack, depth + 1);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    int nargs = countArgList(unpack->argList);
    WResult *result = WUnpackRec(context, unpack, nargs, depth);
#ifdef DEBUG_ALGORITHM_W
    leave("WUnpack", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static WResult *WFarg_d(TinContext *context, AstArg *arg, int depth) {
    switch (arg->type) {
        case AST_ARG_TYPE_WILDCARD:
            return anyResult();
        case AST_ARG_TYPE_SYMBOL:
            return WSymbol(context, arg->val.symbol, depth + 1);
        case AST_ARG_TYPE_NAMED: {
            WResult *r = WFarg(context, arg->val.named->arg, depth + 1);
            int save = PROTECT(r);
            addToSubstitution(r->substitution, arg->val.named->name, r->monoType);
            UNPROTECT(save);
            return r;
        }
        /*
        case AST_ARG_TYPE_ENV:
        */
        case AST_ARG_TYPE_UNPACK:
            /* equivalent to Funcall, but only type constructors are allowed */
            return WUnpack(context, arg->val.unpack, depth + 1);
        case AST_ARG_TYPE_NUMBER:
            return constantResult(intSymbol());
        case AST_ARG_TYPE_STRING:
            return stringResult();
        case AST_ARG_TYPE_CHARACTER:
            return constantResult(charSymbol());
        case AST_ARG_TYPE_YES:
        case AST_ARG_TYPE_NO:
            return constantResult(boolSymbol());
    }
}

static WResult *WFarg(TinContext *context, AstArg *arg, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WFarg", myId, depth);
    printAstArg(arg, depth + 1);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    WResult *result = WFarg_d(context, arg, depth);
#ifdef DEBUG_ALGORITHM_W
    leave("WFarg", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static void addVar(TinContext *context, AstArg *arg) {
    switch (arg->type) {
        case AST_ARG_TYPE_WILDCARD:
        case AST_ARG_TYPE_NUMBER:
        case AST_ARG_TYPE_STRING:
        case AST_ARG_TYPE_CHARACTER:
        case AST_ARG_TYPE_YES:
        case AST_ARG_TYPE_NO:
        /*
        case AST_ARG_TYPE_ENV:
        */
            return;
        case AST_ARG_TYPE_SYMBOL: {
            if (!isTypeConstructor(context, arg->val.symbol)) {
                TinMonoType *var = freshMonoTypeVar(arg->val.symbol->name);
                int save = PROTECT(var);
                addMonoTypeToContext(context, arg->val.symbol, var);
                UNPROTECT(save);
            }
            return;
        }
        case AST_ARG_TYPE_NAMED: {
            addVar(context, arg->val.named->arg);
            TinMonoType *var = freshMonoTypeVar(arg->val.named->name->name);
            int save = PROTECT(var);
            addMonoTypeToContext(context, arg->val.named->name, var);
            UNPROTECT(save);
            return;
        }
        case AST_ARG_TYPE_UNPACK:
            addVars(context, arg->val.unpack->argList);
            return;
    }
}

static void addVars(TinContext *context, AstArgList *args) {
    while (args != NULL) {
        addVar(context, args->arg);
        args = args->next;
    }
}

static TinVarsResult *vars(TinContext *context, HashTable *V, AstArg *arg, int depth) {
    TinVarsResult *result;
    int save = -1;
    switch (arg->type) {
        case AST_ARG_TYPE_SYMBOL: {
            if (hashGet(V, arg->val.symbol, NULL)) {
                result = newTinVarsResult(context, V);
            } else if (isTypeConstructor(context, arg->val.symbol)) {
                result = newTinVarsResult(context, V);
            } else {
                TinMonoType *fresh = freshMonoTypeVar(arg->val.symbol->name);
                save = PROTECT(fresh);
                addMonoTypeToContext(context, arg->val.symbol, fresh);
                hashSet(V, arg->val.symbol, NULL);
                result = newTinVarsResult(context, V);
            }
        }
        break;
        case AST_ARG_TYPE_NAMED: {
            WResult *wr = WFarg(context, arg->val.named->arg, depth + 1);
            save = PROTECT(wr);
            TinVarsResult *r = vars(context, V, arg->val.named->arg, depth + 1);
            (void) PROTECT(r);
            TinMonoType *subs = applyMonoTypeSubstitution(wr->substitution, wr->monoType);
            (void) PROTECT(subs);
            addMonoTypeToContext(context, arg->val.named->name, subs);
            hashSet(V, arg->val.named->name, NULL);
            result = newTinVarsResult(context, V);
        }
        break;
        case AST_ARG_TYPE_ENV: {
            cant_happen("env not implemented yet (in vars)");
        }
        break;
        case AST_ARG_TYPE_UNPACK: {
            result = newTinVarsResult(context, V);
            save = PROTECT(result);
            for (AstArgList *argList = arg->val.unpack->argList; argList != NULL; argList = argList->next) {
                result = vars(result->context, result->set, argList->arg, depth + 1);
                UNPROTECT(save);
                save = PROTECT(result);
            }
        }
        break;
        case AST_ARG_TYPE_WILDCARD:
        case AST_ARG_TYPE_NUMBER:
        case AST_ARG_TYPE_STRING:
        case AST_ARG_TYPE_CHARACTER:
        case AST_ARG_TYPE_YES:
        case AST_ARG_TYPE_NO: {
            result = newTinVarsResult(context, V);
        }
        break;
        default:
            cant_happen("unrecognised arg type %d in vars", arg->type);
    }
    if (save != -1) {
        UNPROTECT(save);
    }
    return result;
}

static TinVarResult *var(TinContext *context, HashTable *V, AstArg *arg, int depth) {
    TinVarResult *result;
    int save = -1;
    switch (arg->type) {
        case AST_ARG_TYPE_WILDCARD: {
            WResult *r = anyResult();
            save = PROTECT(r);
            result = newTinVarResult(r->substitution, context, r->monoType, V);
        }
        break;
        case AST_ARG_TYPE_SYMBOL: {
            if (isTypeConstructor(context, arg->val.symbol)) {
                WResult *r = WSymbol(context, arg->val.symbol, depth + 1);
                save = PROTECT(r);
                result = newTinVarResult(r->substitution, context, r->monoType, V);
            } else if (hashGet(V, arg->val.symbol, NULL)) {
                TinSubstitution *empty = makeEmptySubstitution();
                save = PROTECT(empty);
                TinPolyType *tpt = lookupInContext(context, arg->val.symbol);
                TinMonoType *tmt = instantiate(tpt); // TODO verify this is correct
                (void) PROTECT(tmt);
                result = newTinVarResult(empty, context, tmt, V);
            } else {
                TinMonoType *fresh = freshMonoTypeVar(arg->val.symbol->name);
                save = PROTECT(fresh);
                TinSubstitution *empty = makeEmptySubstitution();
                (void) PROTECT(empty);
                addMonoTypeToContext(context, arg->val.symbol, fresh);
                hashSet(V, arg->val.symbol, NULL);
                result = newTinVarResult(empty, context, fresh, V);
            }
        }
        break;
        case AST_ARG_TYPE_NAMED: {
            TinVarResult *vr = var(context, V, arg->val.named->arg, depth + 1);
            save = PROTECT(vr);
            hashSet(V, arg->val.named->name, NULL);
            addMonoTypeToContext(vr->context, arg->val.named->name, vr->monoType);
            result = newTinVarResult(vr->substitution, vr->context, vr->monoType, V);
        }
        break;
        case AST_ARG_TYPE_ENV: {
            cant_happen("env not implemented yet (in var())");
        }
        break;
        case AST_ARG_TYPE_UNPACK: {
            TinVarsResult *vsr = vars(context, V, arg, depth + 1);
            save = PROTECT(vsr);
            WResult *wr = WUnpack(vsr->context, arg->val.unpack, depth + 1);
            (void) PROTECT(wr);
            result = newTinVarResult(wr->substitution, vsr->context, wr->monoType, vsr->set);
        }
        break;
        case AST_ARG_TYPE_NUMBER: {
            WResult *wr = constantResult(intSymbol());
            save = PROTECT(wr);
            result = newTinVarResult(wr->substitution, context, wr->monoType, V);
        }
        break;
        case AST_ARG_TYPE_STRING: {
            WResult *wr = stringResult();
            save = PROTECT(wr);
            result = newTinVarResult(wr->substitution, context, wr->monoType, V);
        }
        break;
        case AST_ARG_TYPE_CHARACTER: {
            WResult *wr = constantResult(charSymbol());
            save = PROTECT(wr);
            result = newTinVarResult(wr->substitution, context, wr->monoType, V);
        }
        break;
        case AST_ARG_TYPE_YES:
        case AST_ARG_TYPE_NO: {
            WResult *wr = constantResult(boolSymbol());
            save = PROTECT(wr);
            result = newTinVarResult(wr->substitution, context, wr->monoType, V);
        }
        break;
        default:
            cant_happen("unrecognised arg type %d in var", arg->type);
    }
    if (save != -1) {
        UNPROTECT(save);
    }
    return result;
}

static WResult *WFunctionPrime(TinContext *context, AstFunction *fun, HashTable *V, AstArgList *args, int depth) {
    WResult *result;
    if (args == NULL) {
        result = WNest(context, fun->nest, depth + 1);
    } else {
        TinVarResult *vr = var(context, V, args->arg, depth + 1);
        int save = PROTECT(vr);
        WResult *wPrime = WFunctionPrime(vr->context, fun, vr->set, args->next, depth);
        (void) PROTECT(wPrime);
        TinSubstitution *S = applySubstitutionSubstitution(vr->substitution, wPrime->substitution);
        (void) PROTECT(S);
        TinMonoType *beta = applyMonoTypeSubstitution(S, vr->monoType);
        (void) PROTECT(beta);
        TinMonoType *application = arrowApplication(beta, wPrime->monoType);
        (void) PROTECT(application);
        result = newWResult(S, application);
        UNPROTECT(save);
    }
    return result;
}

static WResult *WFunction(TinContext *context, AstFunction *fun, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WFunction", myId, depth);
    printAstFunction(fun, depth + 1);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    TinContext *fnContext = extendTinContext(context);
    int save = PROTECT(fnContext);
    HashTable *V = newHashTable(0, NULL, NULL);
    (void) PROTECT(V);
    WResult *result = WFunctionPrime(fnContext, fun, V, fun->argList, depth + 1);
    UNPROTECT(save);
#ifdef DEBUG_ALGORITHM_W
    leave("WFunction", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

/*
 * unifies between distinct function cases in a composite function
 */
static WResult *WAbstraction(TinContext *context, AstCompositeFunction *fun, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WAbstraction", myId, depth);
    printAstCompositeFunction(fun, depth + 1);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    WResult *result;
    if (fun->next == NULL) {
        result = WFunction(context, fun->function, depth + 1);
    } else {
        WResult *Wn_1 = WAbstraction(context, fun->next, depth);
        int save = PROTECT(Wn_1);
        TinSubstitution *Sn1 = Wn_1->substitution;
        TinMonoType *tn1 = Wn_1->monoType;
        TinMonoType *Sn1tn1 = applyMonoTypeSubstitution(Sn1, tn1);
        (void) PROTECT(Sn1tn1);

        WResult *Wn = WFunction(context, fun->function, depth + 1);
        (void) PROTECT(Wn);
        TinSubstitution *Sn = Wn->substitution;
        TinMonoType *tn = Wn->monoType;
        TinMonoType *Sntn = applyMonoTypeSubstitution(Sn, tn);
        (void) PROTECT(Sntn);

        TinSubstitution *S = unify(Sntn, Sn1tn1, "WAbstraction");
        int save2 = PROTECT(S);
        S = applySubstitutionSubstitution(Sn, S);
        UNPROTECT(save2);
        save2 = PROTECT(S);
        S = applySubstitutionSubstitution(Sn1, S);
        UNPROTECT(save2);
        (void) PROTECT(S);
        TinMonoType *Stn = applyMonoTypeSubstitution(S, Wn->monoType);
        (void) PROTECT(Stn);
        result = newWResult(S, Stn);
        UNPROTECT(save);
    }
#ifdef DEBUG_ALGORITHM_W
    leave("WAbstraction", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static WResult *WExpression_d(TinContext *context, AstExpression *expr, int depth) {
    switch (expr->type) {
        case AST_EXPRESSION_TYPE_BACK:
            return anyResult();
        case AST_EXPRESSION_TYPE_FUNCALL:
            return WApplication(context, expr->val.funCall, depth + 1);
        case AST_EXPRESSION_TYPE_SYMBOL:
            return WSymbol(context, expr->val.symbol, depth + 1);
        case AST_EXPRESSION_TYPE_NUMBER:
            return constantResult(intSymbol());
        case AST_EXPRESSION_TYPE_STRING:
            return stringResult();
        case AST_EXPRESSION_TYPE_CHARACTER:
            return constantResult(charSymbol());
        case AST_EXPRESSION_TYPE_YES:
        case AST_EXPRESSION_TYPE_NO:
            return constantResult(boolSymbol());
        case AST_EXPRESSION_TYPE_FUN:
            return WAbstraction(context, expr->val.fun, depth + 1);
        case AST_EXPRESSION_TYPE_NEST:
            return WNest(context, expr->val.nest, depth + 1);
        /*
        case AST_EXPRESSION_TYPE_ENV:
            return WEnv(context, expr->val.env);
        */
        case AST_EXPRESSION_TYPE_CONDITIONAL:
            return WCond(context, expr->val.conditional, depth + 1);
        default:
            cant_happen("unrecognised type %d in WExpression", expr->type);
    }
}

static WResult *WExpression(TinContext *context, AstExpression *expr, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WExpression", myId, depth);
    printAstExpression(expr, depth + 1);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    WResult *result = WExpression_d(context, expr, depth);
#ifdef DEBUG_ALGORITHM_W
    leave("WExpression", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static TinMonoType *extractMonoType(TinPolyType *tpt) {
    if (tpt == NULL) {
        cant_happen("expected non-null TinPolyType in extractMonoType");
    }
    if (tpt->type != TINPOLYTYPE_TYPE_MONOTYPE) {
        cant_happen("expected TINPOLYTYPE_TYPE_MONOTYPE");
    }
    return tpt->val.monoType;
}

static void WDefine(TinContext *context, AstDefine *define, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WDefine", myId, depth);
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    HashSymbol *F = define->symbol;
    AstExpression *expression = define->expression;
    TinMonoType *originalTypeOfF = extractMonoType(lookupInContext(context, F));
    WResult *wResult = WExpression(context, expression, depth + 1);
    int save = PROTECT(wResult);
    TinSubstitution *substitutionsCalculatedFromF = wResult->substitution;
    TinMonoType *substitutedOriginalTypeOfF = applyMonoTypeSubstitution(substitutionsCalculatedFromF, originalTypeOfF);
    PROTECT(substitutedOriginalTypeOfF);
    TinMonoType *calculatedTypeOfF = wResult->monoType;
    TinSubstitution *unifyingSubstitution = unify(substitutedOriginalTypeOfF, calculatedTypeOfF, "WDefine");
    PROTECT(unifyingSubstitution);
    TinMonoType *finalMonoTypeOfF = applyMonoTypeSubstitution(unifyingSubstitution, calculatedTypeOfF);
    PROTECT(finalMonoTypeOfF);
    TinSubstitution *finalSubstitution = applySubstitutionSubstitution(unifyingSubstitution, substitutionsCalculatedFromF);
    PROTECT(finalSubstitution);
    applyContextSubstitutionInPlace(finalSubstitution, context);
    TinPolyType *finalTypeOfF = generalize(context, finalMonoTypeOfF);
    PROTECT(finalTypeOfF);
    addVarToContext(context, F, finalTypeOfF);
    UNPROTECT(save);
#ifdef DEBUG_ALGORITHM_W
    leave("WDefine", myId, depth);
    printHashSymbol(F);
    printf(" =\n%*s", depth * 4, "");
    showTinPolyType(finalTypeOfF);
    printf("\n");
#endif
}

static void WDefinition(TinContext *context, AstDefinition *definition, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WDefinition", myId, depth);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    switch (definition->type) {
        case AST_DEFINITION_TYPE_DEFINE:
            WDefine(context, definition->val.define, depth + 1);
            break;
        case AST_DEFINITION_TYPE_PROTOTYPE:
            // WPrototype(context, definition->val.prototype);
            break;
        case AST_DEFINITION_TYPE_LOAD:
            // WLoad(context, definition->val.load);
            break;
        case AST_DEFINITION_TYPE_TYPEDEF:
            // WTypeDef(context, definition->val.typeDef);
            break;
        default:
            cant_happen("unrecognised type %d in WDefinition", definition->type);
    }
#ifdef DEBUG_ALGORITHM_W
    leave("WDefinition", myId, depth);
    printf("\n");
#endif
}


static WResult *WNest(TinContext *context, AstNest *nest, int depth) {
#ifdef DEBUG_ALGORITHM_W
    int myId = idSource++;
    enter("WNest", myId, depth);
    printAstNest(nest, depth + 1);
    printf("\n");
    printTinContext(context, depth + 1);
    printf("\n");
#endif
    if (nest == NULL) return newWResult(NULL, NULL);
    TinContext *nestContext = extendTinContext(context);
    int save = PROTECT(nestContext);
    AstDefinitions *definitions;
    // the outer let of a nest is effectively letrec, so we collect the functions being defined first
    for (definitions = nest->definitions; definitions != NULL; definitions = definitions->next) {
        collectDefinition(definitions->definition, nestContext);
    }
    for (definitions = nest->definitions; definitions != NULL; definitions = definitions->next) {
        WDefinition(nestContext, definitions->definition, depth + 1);
    }
    WResult *result = NULL;
    int save2 = PROTECT(nestContext);
    for (AstExpressions *expressions = nest->expressions; expressions != NULL; expressions = expressions->next) {
        result = WExpression(nestContext, expressions->expression, depth + 1);
        UNPROTECT(save2);
        save2 = PROTECT(result);
    }
    UNPROTECT(save);
#ifdef DEBUG_ALGORITHM_W
    leave("WNest", myId, depth);
    printWResult(result, depth + 1);
    printf("\n");
#endif
    return result;
}

static void addBinOp(TinContext *context, HashSymbol *op, TinMonoType *a, TinMonoType *b, TinMonoType *c) {
    // #a -> ~b -> #c
    TinMonoType *funApp = arrowApplication(b, c);
    int save = PROTECT(funApp);
    funApp = arrowApplication(a, funApp);
    UNPROTECT(save);
    save = PROTECT(funApp);
    generalizeMonoTypeToContext(context, op, funApp);
    UNPROTECT(save);
}

static void addTypeBinOp(TinContext *context, HashSymbol *op, TinMonoType *a, TinMonoType *b, TinMonoType *c) {
    // #a -> ~b -> #c
    TinMonoType *funApp = arrowApplication(b, c);
    int save = PROTECT(funApp);
    funApp = arrowApplication(a, funApp);
    UNPROTECT(save);
    save = PROTECT(funApp);
    generalizeTypeConstructorToContext(context, op, funApp);
    UNPROTECT(save);
}

static void addIntBinOp(TinContext *context, char *token) {
    // Int -> Int -> Int
    HashSymbol *symbol = newSymbol(token);
    int save = PROTECT(symbol);
    TinMonoType *intSym = constantTypeFunction(intSymbol());
    (void) PROTECT(intSym);
    addBinOp(context, symbol, intSym, intSym, intSym);
    UNPROTECT(save);
}

static void addBoolBinOp(TinContext *context, char *token) {
    // Bool -> Bool -> Bool
    HashSymbol *symbol = newSymbol(token);
    int save = PROTECT(symbol);
    TinMonoType *boolSym = constantTypeFunction(boolSymbol());
    (void) PROTECT(boolSym);
    addBinOp(context, symbol, boolSym, boolSym, boolSym);
    UNPROTECT(save);
}

static void addThen(TinContext *context) {
    // #a -> #a -> #a
    TinMonoType *fresh = freshMonoTypeVar("then");
    int save = PROTECT(fresh);
    HashSymbol *then = newSymbol("then");
    addBinOp(context, then, fresh, fresh, fresh);
    UNPROTECT(save);
}

static void addBack(TinContext *context) {
    // #a
    TinMonoType *fresh = freshMonoTypeVar("back");
    int save = PROTECT(fresh);
    HashSymbol *back = newSymbol("back");
    generalizeMonoTypeToContext(context, back, fresh);
    UNPROTECT(save);
}

static void addComparisonBinOp(TinContext *context, char *token) {
    // #a -> #a -> Bool
    HashSymbol *op = newSymbol(token);
    TinMonoType *fresh = freshMonoTypeVar(token);
    int save = PROTECT(fresh);
    TinMonoType *boolSym = constantTypeFunction(boolSymbol());
    (void) PROTECT(boolSym);
    addBinOp(context, op, fresh, fresh, boolSym);
    UNPROTECT(save);
}

static void addCons(TinContext *context) {
    // #a -> List(#a) -> List(#a)
    HashSymbol *op = newSymbol("@");
    TinMonoType *fresh = freshMonoTypeVar("@");
    int save = PROTECT(fresh);
    TinMonoType *list = singleArgTypeFunction(listSymbol(), fresh);
    (void) PROTECT(list);
    addTypeBinOp(context, op, fresh, list, list);
    UNPROTECT(save);
}

static void addAppend(TinContext *context) {
    // List(#a) -> List(#a) -> List(#a)
    HashSymbol *op = newSymbol("@@");
    TinMonoType *fresh = freshMonoTypeVar("@@");
    int save = PROTECT(fresh);
    TinMonoType *list = singleArgTypeFunction(listSymbol(), fresh);
    (void) PROTECT(list);
    addBinOp(context, op, list, list, list);
    UNPROTECT(save);
}

static void addNot(TinContext *context) {
    // Bool -> Bool
    HashSymbol *op = newSymbol("not");
    TinMonoType *boolSym = constantTypeFunction(boolSymbol());
    int save = PROTECT(boolSym);
    TinMonoType *funApp = arrowApplication(boolSym, boolSym);
    UNPROTECT(save);
    save = PROTECT(funApp);
    generalizeMonoTypeToContext(context, op, funApp);
    UNPROTECT(save);
}

static void addNegate(TinContext *context) {
    // Int -> Int
    HashSymbol *op = newSymbol("neg");
    TinMonoType *intSym = constantTypeFunction(intSymbol());
    int save = PROTECT(intSym);
    TinMonoType *funApp = arrowApplication(intSym, intSym);
    UNPROTECT(save);
    save = PROTECT(funApp);
    generalizeMonoTypeToContext(context, op, funApp);
    UNPROTECT(save);
}

static void addHere(TinContext *context) {
    // ((#a -> #b) -> #a) -> #a
    TinMonoType *a = freshMonoTypeVar("here-a");
    int save = PROTECT(a);
    TinMonoType *b = freshMonoTypeVar("here-b");
    (void) PROTECT(b);
    TinMonoType *arrow = arrowApplication(a, b);
    UNPROTECT(save);
    save = PROTECT(arrow);
    arrow = arrowApplication(arrow, a);
    UNPROTECT(save);
    save = PROTECT(arrow);
    arrow = arrowApplication(arrow, a);
    UNPROTECT(save);
    save = PROTECT(arrow);
    HashSymbol *op = newSymbol("here");
    generalizeMonoTypeToContext(context, op, arrow);
    UNPROTECT(save);
}

static void addIf(TinContext *context) {
    // Bool -> #a -> #a -> #a
    TinMonoType *a = freshMonoTypeVar("if");
    int save = PROTECT(a);
    TinMonoType *arrow = arrowApplication(a, a);
    UNPROTECT(save);
    save = PROTECT(arrow);
    arrow = arrowApplication(a, arrow);
    UNPROTECT(save);
    save = PROTECT(arrow);
    TinMonoType *b = constantTypeFunction(boolSymbol());
    (void) PROTECT(b);
    arrow = arrowApplication(b, arrow);
    UNPROTECT(save);
    save = PROTECT(arrow);
    HashSymbol *op = newSymbol("if");
    generalizeMonoTypeToContext(context, op, arrow);
    UNPROTECT(save);
}

static void addNil(TinContext *context) {
    // List(#t)
    TinMonoType *fresh = freshMonoTypeVar("nil");
    int save = PROTECT(fresh);
    TinMonoType *list = singleArgTypeFunction(listSymbol(), fresh);
    (void) PROTECT(list);
    HashSymbol *op = newSymbol("nil");
    generalizeTypeConstructorToContext(context, op, list);
    UNPROTECT(save);
}

static void addError(TinContext *context) {
    // #t1 -> #t2
    TinMonoType *fresh1 = freshMonoTypeVar("error");
    int save = PROTECT(fresh1);
    TinMonoType *fresh2 = freshMonoTypeVar("error");
    PROTECT(fresh2);
    TinMonoType *funApp = arrowApplication(fresh1, fresh2);
    PROTECT(funApp);
    HashSymbol *op = newSymbol("error");
    generalizeMonoTypeToContext(context, op, funApp);
    UNPROTECT(save);
}

WResult *WTop(AstNest *nest) {
    TinContext *context = freshTinContext();
    int save = PROTECT(context);
    addIntBinOp(context, "+");
    addIntBinOp(context, "-");
    addIntBinOp(context, "*");
    addIntBinOp(context, "/");
    addIntBinOp(context, "%");
    addIntBinOp(context, "^");
    addBoolBinOp(context, "and");
    addBoolBinOp(context, "or");
    addBoolBinOp(context, "xor");
    addComparisonBinOp(context, "==");
    addComparisonBinOp(context, "<");
    addComparisonBinOp(context, ">");
    addComparisonBinOp(context, "<=");
    addComparisonBinOp(context, ">=");
    addComparisonBinOp(context, "!=");
    addThen(context);
    addCons(context);
    addAppend(context);
    addNot(context);
    addNegate(context);
    addHere(context);
    addIf(context);
    addNil(context);
    addBack(context);
    addError(context);
    return WNest(context, nest, 0);
}
