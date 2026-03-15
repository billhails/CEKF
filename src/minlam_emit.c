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
 * Minimal AST after desugaring
 * Generated from src/minlam.yaml by tools/generate.py
 */

#include "minlam_emit.h"
#include "memory.h"
#include "minlam.h"
#include "minlam_pp.h"
#include "symbol.h"
#include "utils_helper.h"
#include <ctype.h>
#include <sys/param.h>

#ifdef DEBUG_MINLAM_EMIT
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static void emitMinLam(MinLam *node, EmitContext *context);
static void emitMinAnnotatedVar(MinAnnotatedVar *node, EmitContext *context);
static void emitMinExprList(MinExprList *node, EmitContext *context);
static void emitMinApply(MinApply *node, EmitContext *context);
static void emitMinIff(MinIff *node, EmitContext *context);
static void emitMinCond(MinCond *node, EmitContext *context);
static void emitMinIntCondCases(MinIntCondCases *node, EmitContext *context);
static void emitMinCharCondCases(MinCharCondCases *node, EmitContext *context);
static void emitMinMatch(MinMatch *node, EmitContext *context);
static void emitMinMatchList(MinMatchList *node, EmitContext *context);
static void emitMinLetRec(MinLetRec *node, EmitContext *context);
static void emitMinBindings(MinBindings *node, EmitContext *context);
static void emitMinExp(MinExp *node, EmitContext *context);
static void emitMinCondCases(MinCondCases *node, EmitContext *context);
static void emitMakeVec(MinExprList *vecs, char *target, EmitContext *context);
static void emitSimpleExp(MinExp *exp, char *target, EmitContext *context);

///////////
// Helpers
///////////

typedef struct EmitBuffer {
    FILE *fh;
    char *buffer;
    size_t size;
} EmitBuffer;

static EmitBuffer *newEmitBuffer();
static char *getEmitBuffer(EmitBuffer *buffer);
static void cleanEmitBuffer(void *buffer);
static void printEmitBuffer(void *buffer);

static inline Opaque *newOpaque_EmitBuffer() {
    EmitBuffer *buffer = newEmitBuffer();
    return newOpaque(buffer, cleanEmitBuffer, printEmitBuffer, NULL);
}

static inline char *opaqueEmitBufferContent(Opaque *container) {
    return getEmitBuffer((EmitBuffer *)container->data);
}

static inline FILE *opaqueEmitBufferFh(Opaque *container) {
    return ((EmitBuffer *)(container->data))->fh;
}

static inline FILE *FH(EmitContext *context) {
    return opaqueEmitBufferFh(context->body);
}

static char *getPrimOpName(MinPrimOp type) {
    switch (type) {
    case MINPRIMOP_TYPE_VEC:
        return "vec";
    case MINPRIMOP_TYPE_MUL:
        return "nmul";
    case MINPRIMOP_TYPE_DIV:
        return "ndiv";
    case MINPRIMOP_TYPE_CMP:
        return "cmp";
    case MINPRIMOP_TYPE_NE:
        return "ne";
    case MINPRIMOP_TYPE_LT:
        return "lt";
    case MINPRIMOP_TYPE_LE:
        return "le";
    case MINPRIMOP_TYPE_EQ:
        return "eq";
    case MINPRIMOP_TYPE_GE:
        return "ge";
    case MINPRIMOP_TYPE_GT:
        return "gt";
    case MINPRIMOP_TYPE_ADD:
        return "nadd";
    case MINPRIMOP_TYPE_MOD:
        return "nmod";
    case MINPRIMOP_TYPE_POW:
        return "npow";
    case MINPRIMOP_TYPE_SUB:
        return "nsub";
    case MINPRIMOP_TYPE_CANON:
        return "ncanon";
    default:
        cant_happen("unhandled %s", minPrimOpName(type));
    }
}

static HashSymbol *getSymbol(EmitContext *context, bool *isNew) {
    if (context->symbols == NULL) {
        cant_happen("null context symbols in getSymbol");
    }
    if (context->symbols->array->size > 0) {
        *isNew = false;
        return popSymbolArray(context->symbols->array);
    } else {
        *isNew = true;
        return genSym("tmp_");
    }
}

static void releaseSymbol(HashSymbol *symbol, EmitContext *context) {
    if (context->symbols == NULL) {
        cant_happen("null context symbols in releaseSymbol");
    }
    pushSymbolArray(context->symbols->array, symbol);
}

static void enterScope(EmitContext *context) {
    fprintf(FH(context), "{\n");
    SymbolArray *array = newSymbolArray();
    int save = PROTECT(array);
    context->symbols = newSymbolStash(array, context->symbols);
    UNPROTECT(save);
}

static void leaveScope(EmitContext *context) {
    if (context->symbols == NULL) {
        cant_happen("null context symbols in leaveScope");
    }
    fprintf(FH(context), "}\n");
    context->symbols = context->symbols->parent;
}

static EmitBuffer *newEmitBuffer() {
    EmitBuffer *result = ALLOCATE(EmitBuffer);
    result->buffer = NULL;
    result->size = 0;
    result->fh = open_memstream(&result->buffer, &result->size);
    return result;
}

static char *getEmitBuffer(EmitBuffer *buffer) {
    fflush(buffer->fh);
    if (buffer->buffer == NULL)
        return "";
    else
        return buffer->buffer;
}

static void cleanEmitBuffer(void *buffer) {
    EmitBuffer *b = (EmitBuffer *)buffer;
    fclose(b->fh);
    free(b->buffer);
    FREE(b, EmitBuffer);
}

static void printEmitBuffer(void *buffer) {
    EmitBuffer *b = (EmitBuffer *)buffer;
    fflush(b->fh);
    if (b->buffer != NULL)
        eprintf("%s", b->buffer);
}

// an expression is atomic if it causes no memory allocation
static bool isAtomic(MinExp *exp) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_STDINT:
        return true;
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *prim = getMinExp_Prim(exp);
        if (prim->type == MINPRIMOP_TYPE_VEC) {
            return isAtomic(prim->exp1) && isAtomic(prim->exp2);
        } else {
            return false;
        }
    }
    default:
        return false;
    }
}

static void emitAtomic(MinExp *exp, EmitContext *context) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
        fprintf(FH(context), "reg[%d]", getMinExp_Avar(exp)->position);
        break;
    case MINEXP_TYPE_BIGINTEGER:
        fprintf(FH(context), "value_Stdint(");
        fprintMaybeBigInt(FH(context), getMinExp_BigInteger(exp));
        fprintf(FH(context), ")");
        break;
    case MINEXP_TYPE_CHARACTER:
        fprintf(FH(context), "value_Character(%d)",
                (int)getMinExp_Character(exp));
        break;
    case MINEXP_TYPE_STDINT:
        fprintf(FH(context), "value_Stdint(%d)", getMinExp_Stdint(exp));
        break;
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *prim = getMinExp_Prim(exp);
        if (prim->type == MINPRIMOP_TYPE_VEC) {
            fprintf(FH(context), "vec(");
            emitAtomic(prim->exp1, context);
            fprintf(FH(context), ", ");
            emitAtomic(prim->exp2, context);
            fprintf(FH(context), ")");
        } else {
            cant_happen("emitAtomic passed non-atomic %s",
                        minExpTypeName(exp->type));
        }
        break;
    }
    default:
        cant_happen("emitAtomic passed non-atomic %s",
                    minExpTypeName(exp->type));
    }
}

// assertive retrieval, all letrec bindings are expected to be closures
static MinLam *extractLambda(MinExp *node) {
    MinExprList *makeVec = getMinExp_MakeVec(node);
#ifdef SAFETY_CHECKS
    if (makeVec == NULL)
        cant_happen("letrec binding must be a 2-vec");
#endif
    return getMinExp_Lam(makeVec->exp);
}

static MinExprList *extractEnv(MinExp *node) {
    MinExprList *makeVec = getMinExp_MakeVec(node);
#ifdef SAFETY_CHECKS
    if (makeVec == NULL)
        cant_happen("letrec binding must be a 2-vec");
#endif
    makeVec = makeVec->next;
#ifdef SAFETY_CHECKS
    if (makeVec == NULL)
        cant_happen("letrec binding must be a 2-vec");
#endif
    return getMinExp_MakeVec(makeVec->exp);
}

static SCharArray *makeLabel(char *prefix, HashSymbol *symbol) {
    SCharArray *label = newSCharArray();
    int save = PROTECT(label);
    static char buffer[128];
    for (char *c = prefix; *c; c++) {
        pushSCharArray(label, *c);
    }
    for (char *c = symbol->name; *c; c++) {
        if (isalnum(*c) || *c == '_')
            pushSCharArray(label, *c);
        else {
            sprintf(buffer, "%d", (int)*c);
            for (char *d = buffer; *d; d++)
                pushSCharArray(label, *d);
        }
    }
    pushSCharArray(label, '\0');
    UNPROTECT(save);
    return label;
}

static SCharArray *makeLambdaLabel(HashSymbol *symbol) {
    return makeLabel("LAMBDA_", symbol);
}

__attribute__((unused)) static SCharArray *makeVarName(HashSymbol *symbol) {
    return makeLabel("var_", symbol);
}

static inline void incrDepth(EmitContext *context) {
    context->currentDepth++;
    context->maxReg = MAX(context->maxReg, context->currentDepth);
}

static inline void retrieveMaxReg(EmitContext *to, EmitContext *from) {
    to->maxReg = MAX(to->maxReg, from->maxReg);
}

// shallow copy
static EmitContext *copyContext(EmitContext *context, Opaque *body) {
    EmitContext *new =
        newEmitContext(body, context->builtIns, context->symbols);
    int save = PROTECT(new);
    new->lambdas = context->lambdas;
    new->indexes = context->indexes;
    new->currentDepth = context->currentDepth;
    new->maxReg = context->maxReg;
    UNPROTECT(save);
    return new;
}

static void emitPrimOp(MinPrimApp *app, char *target, EmitContext *context) {
    char *name = getPrimOpName(app->type);
    // Value arg1 = ...;
    // Value arg2 = ...;
    // result = op(arg1, arg2);
    HashSymbol *arg1 = NULL, *arg2 = NULL;
    if (!isAtomic(app->exp1)) {
        bool isNew;
        arg1 = getSymbol(context, &isNew);
        if (isNew)
            fprintf(FH(context), "Value %s;\n", arg1->name);
        emitSimpleExp(app->exp1, arg1->name, context);
        releaseSymbol(arg1, context);
    }
    if (!isAtomic(app->exp2)) {
        bool isNew;
        arg2 = getSymbol(context, &isNew);
        if (isNew)
            fprintf(FH(context), "Value %s;\n", arg2->name);
        emitSimpleExp(app->exp2, arg2->name, context);
        releaseSymbol(arg2, context);
    }
    fprintf(FH(context), "%s = %s(", target, name);
    if (isAtomic(app->exp1))
        emitAtomic(app->exp1, context);
    else {
        fprintf(FH(context), "%s", arg1->name);
    }
    fprintf(FH(context), ", ");
    if (isAtomic(app->exp2))
        emitAtomic(app->exp2, context);
    else {
        fprintf(FH(context), "%s", arg2->name);
    }
    fprintf(FH(context), ");\n");
}

static void emitAnonymousLambda(MinLam *lambda, char *target,
                                EmitContext *context) {
    // emit the lambda to the prelude, this then reduces to &&LABEL
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    EmitContext *lamContext = copyContext(context, body);
    REPLACE_PROTECT(save, lamContext);
    HashSymbol *name = genSym("anon");
    setBufferBag(context->lambdas, name, body);
    setIntMap(context->indexes, name, context->currentDepth);
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(context, lamContext);
    SCharArray *label = makeLambdaLabel(name);
    fprintf(FH(context), "%s = value_Addr(&&%s);\n", target, label->entries);
    UNPROTECT(save);
}

static void emitSimpleExp(MinExp *exp, char *target, EmitContext *context) {
    switch (exp->type) {
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *app = getMinExp_Prim(exp);
        emitPrimOp(app, target, context);
        break;
    }
    case MINEXP_TYPE_AVAR: {
        MinAnnotatedVar *avar = getMinExp_Avar(exp);
        fprintf(FH(context), "%s = ", target);
        emitMinAnnotatedVar(avar, context);
        fprintf(FH(context), ";\n");
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *vecs = getMinExp_MakeVec(exp);
        emitMakeVec(vecs, target, context);
        break;
    }
    case MINEXP_TYPE_LAM: {
        MinLam *lambda = getMinExp_Lam(exp);
        emitAnonymousLambda(lambda, target, context);
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        MaybeBigInt *mbi = getMinExp_BigInteger(exp);
        fprintf(FH(context), "%s = value_Stdint(", target);
        fprintMaybeBigInt(FH(context), mbi); // FIXME
        fprintf(FH(context), ");\n");
        break;
    }
    case MINEXP_TYPE_STDINT: {
        fprintf(FH(context), "%s = value_Stdint(%d);\n", target,
                getMinExp_Stdint(exp));
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        fprintf(FH(context), "%s = value_Character(%d);\n", target,
                (int)getMinExp_Character(exp));
        break;
    }
    default:
        cant_happen("unhandled %s", minExpTypeName(exp->type));
    }
}

static void emitMakeVec(MinExprList *vecs, char *target, EmitContext *context) {
    SymbolArray *temps = newSymbolArray();
    int save = PROTECT(temps);

    int count = countMinExprList(vecs);
    HashSymbol *placeHolder = newSymbol("fn"); // any existing token
    MinExprList *v = vecs;
    while (v != NULL) {
        if (isAtomic(v->exp)) {
            pushSymbolArray(temps, placeHolder);
        } else {
            bool isNew;
            HashSymbol *temp = getSymbol(context, &isNew);
            pushSymbolArray(temps, temp);
            if (isNew)
                fprintf(FH(context), "Value %s;\n", temp->name);
            emitSimpleExp(v->exp, temp->name, context);
        }
        v = v->next;
    }
    fprintf(FH(context), "%s = make_vec(%d", target, count);
    v = vecs;
    for (Index i = 0; i < temps->size; i++) {
        HashSymbol *temp = temps->entries[i];
        fprintf(FH(context), ", ");
        if (temp == placeHolder) { // was atomic
            emitAtomic(v->exp, context);
        } else {
            fprintf(FH(context), "%s", temp->name);
            releaseSymbol(temp, context);
        }
        v = v->next;
    }
    fprintf(FH(context), ");\n");
    UNPROTECT(save);
}

///////////////////////////
// Visitor implementations
///////////////////////////

static void emitMinLam(MinLam *node, EmitContext *context) {
    context->currentDepth = 0; // reset
    // emitSymbolList(node->args, context);
    if (node->exp != NULL) {
        enterScope(context);
        emitMinExp(node->exp, context);
        leaveScope(context);
    }
}

static void emitMinAnnotatedVar(MinAnnotatedVar *node, EmitContext *context) {
    fprintf(FH(context), "reg[%d] /* %s */", node->position, node->var->name);
}

static void emitMinExprList(MinExprList *node, EmitContext *context) {
    if (node == NULL)
        return;
    emitMinExp(node->exp, context);
    emitMinExprList(node->next, context);
}

// all apply are apply-closure
// body: (k<3> (+ a<1> b<2>) f<4>)
// This calls continuation k with (a + b), passing f along.
//
// k is a closure (2-vec) in reg[3].
// The call target is k's code:   vec 0 of reg[3]
// The call passes:
//   arg 0 (env):    vec 1 of reg[3]   -- k's captured env
//   arg 1 (result): a + b             -- the computed value!
//   arg 2 (fail):   reg[4]            -- thread the failure cont through
//
//  void *target = reg[3].vec[0];   // k's code label
//  Value env    = reg[3].vec[1];   // k's env
//  Value result = add(reg[1], reg[2]);  // a + b
//  Value fail   = reg[4];
//
//  reg[0] = env;       // callee's param 0: env
//  reg[1] = result;    // callee's param 1: the answer (5)
//  reg[2] = fail;      // callee's param 2: fail cont
//  goto *target;       // jump to k's code
static void emitMinApply(MinApply *node, EmitContext *context) {
    if (node->isBuiltin) {
        fprintf(FH(context), "// built-in\n");
        for (Index i = 0; i < context->builtIns->size; i++) {
            BuiltIn *bi = context->builtIns->entries[i];
            if (bi->internalName == getMinExp_Var(node->function)) {
                MinExprList *myArgs = node->args;
                for (Index i = 0; i < bi->args->size; i++) {
                    char buffer[32];
                    sprintf(buffer, "arg%d", i);
                    fprintf(FH(context), "Value %s;\n", buffer);
                    emitSimpleExp(myArgs->exp, buffer, context);
                    myArgs = myArgs->next;
                }
                fprintf(FH(context), "Value args = make_vec(%d",
                        bi->args->size);
                for (Index i = 0; i < bi->args->size; i++) {
                    char buffer[32];
                    sprintf(buffer, "arg%d", i);
                    fprintf(FH(context), ", %s", buffer);
                }
                fprintf(FH(context), ");\n");
                fprintf(FH(context), "extern Value %s(Vec *);\n",
                        bi->linkerName->name);
                fprintf(FH(context), "Value result = %s(args.val.vec);\n",
                        bi->linkerName->name);
                fprintf(FH(context), "Value closure;\n");
                emitSimpleExp(myArgs->exp, "closure", context);
                fprintf(
                    FH(context),
                    "void *target = vec(value_Stdint(0), closure).val.none;\n");
                fprintf(FH(context),
                        "Value env = vec(value_Stdint(1), closure);\n");
                myArgs = myArgs->next;
                fprintf(FH(context), "Value fail;\n");
                emitSimpleExp(myArgs->exp, "fail", context);
                fprintf(FH(context), "reg[0] = env;\n");
                fprintf(FH(context), "reg[1] = result;\n");
                fprintf(FH(context), "reg[2] = fail;\n");
                fprintf(FH(context), "goto *target;\n");

                return;
            }
        }
        cant_happen("could not find builtin %s",
                    getMinExp_Var(node->function)->name);
    }
    if (isAtomic(node->function)) {
        fprintf(FH(context), "void *target = vec(value_Stdint(0), \n");
        emitAtomic(node->function, context);
        fprintf(FH(context), ").val.none;\n");
        fprintf(FH(context), "Value env = vec(value_Stdint(1), ");
        emitAtomic(node->function, context);
        fprintf(FH(context), ");\n");
    } else {
        fprintf(FH(context), "Value closure;\n");
        emitSimpleExp(node->function, "closure", context);
        fprintf(FH(context),
                "void *target = vec(value_Stdint(0), closure).val.none;\n");
        fprintf(FH(context), "Value env = vec(value_Stdint(1), closure);\n");
    }
    MinExprList *args = node->args;
    int i = 1;
    while (args != NULL) {
        char arg[32];
        sprintf(arg, "arg%d", i);
        fprintf(FH(context), "Value %s;\n", arg);
        emitSimpleExp(args->exp, arg, context);
        args = args->next;
        i++;
    }
    fprintf(FH(context), "reg[0] = env;\n");
    int j = 1;
    while (j < i) {
        fprintf(FH(context), "reg[%d] = arg%d;\n", j, j);
        j++;
    }
    fprintf(FH(context), "goto *target;\n\n");
}

static void emitMinIff(MinIff *node, EmitContext *context) {
    enterScope(context);
    fprintf(FH(context), "    Value test;\n");
    emitSimpleExp(node->condition, "test", context);
    fprintf(FH(context), "    if (isTrue(test))");
    enterScope(context);
    emitMinExp(node->consequent, context);
    leaveScope(context);
    fprintf(FH(context), " else ");
    enterScope(context);
    emitMinExp(node->alternative, context);
    leaveScope(context);
    leaveScope(context);
}

static void emitMinCond(MinCond *node, EmitContext *context) {
    enterScope(context);
    fprintf(FH(context), "Value cond;\n");
    emitSimpleExp(node->value, "cond", context);
    if (node->cases->type == MINCONDCASES_TYPE_INTEGERS) {
        fprintf(FH(context), "switch(cond.val.stdint) {\n");
    } else {
        fprintf(FH(context), "switch(cond.val.character) {\n");
    }
    emitMinCondCases(node->cases, context);
    fprintf(FH(context), "}\n");
    leaveScope(context);
}

static void emitMinCondCases(MinCondCases *node, EmitContext *context) {
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        emitMinIntCondCases(variant, context);
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        emitMinCharCondCases(variant, context);
        break;
    }
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
}

static void emitMinIntCondCases(MinIntCondCases *node, EmitContext *context) {
    if (node == NULL) {
        return;
    }
    if (node->constant == NULL) {
        fprintf(FH(context), "default:\n");
    } else {
        fprintf(FH(context), "case ");
        fprintMaybeBigInt(FH(context), node->constant);
        fprintf(FH(context), ": \n");
    }
    enterScope(context);
    emitMinExp(node->body, context);
    fprintf(FH(context), "break;\n");
    leaveScope(context);
    emitMinIntCondCases(node->next, context);
}

static void emitMinCharCondCases(MinCharCondCases *node, EmitContext *context) {
    if (node == NULL) {
        fprintf(FH(context), "default:\n");
        fprintf(FH(context), "fprintf(stderr, \"conditions exhausted\\n\");\n");
        fprintf(FH(context), "exit(1);\n");
        return;
    }
    fprintf(FH(context), "case %d: \n", (int)(node->constant));
    enterScope(context);
    emitMinExp(node->body, context);
    fprintf(FH(context), "break;\n");
    leaveScope(context);
    emitMinCharCondCases(node->next, context);
}

static void emitMinMatch(MinMatch *node, EmitContext *context) {
    enterScope(context);
    fprintf(FH(context), "Value match;\n");
    emitSimpleExp(node->index, "match", context);
    fprintf(FH(context), "switch (match.val.stdint) {\n");
    emitMinMatchList(node->cases, context);
    fprintf(FH(context), "}\n");
    leaveScope(context);
}

static void emitMinMatchList(MinMatchList *node, EmitContext *context) {
    if (node == NULL)
        return;
    MinIntList *match = node->matches;
    while (match != NULL) {
        fprintf(FH(context), "case %d:\n", match->item);
        match = match->next;
    }

    enterScope(context);
    emitMinExp(node->body, context);
    fprintf(FH(context), "break;\n");
    leaveScope(context);
    emitMinMatchList(node->next, context);
}

static void emitMinLetRec(MinLetRec *node, EmitContext *context) {
    emitMinBindings(node->bindings, context);
    emitMinExp(node->body, context);
}

static void emitMakeClosure(MinExprList *exprs, HashSymbol *var,
                            EmitContext *context) {
    FILE *out = FH(context);
    SCharArray *label = makeLambdaLabel(var);
    HashSymbol *tmp = genSym("tmp_");
    fprintf(out, "Value %s;\n", tmp->name);
    emitMakeVec(exprs, tmp->name, context);
    fprintf(out, "reg[%d] = make_vec(2, value_Addr(&&%s), %s);\n",
            context->currentDepth, label->entries, tmp->name);
}

static void emitMinBindings(MinBindings *node, EmitContext *context) {
    if (node == NULL)
        return;
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    EmitContext *lamContext = copyContext(context, body);
    REPLACE_PROTECT(save, lamContext);
    setBufferBag(context->lambdas, node->var, body);
    setIntMap(context->indexes, node->var, context->currentDepth);
    MinLam *lambda = extractLambda(node->val);
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(context, lamContext);
    MinExprList *makeEnv = extractEnv(node->val);
    emitMakeClosure(makeEnv, node->var, context);
    UNPROTECT(save);
    incrDepth(context);
    emitMinBindings(node->next, context);
}

static void emitMinExp(MinExp *node, EmitContext *context) {
    switch (node->type) {
    case MINEXP_TYPE_APPLY: {
        MinApply *variant = getMinExp_Apply(node);
        emitMinApply(variant, context);
        break;
    }
    case MINEXP_TYPE_ARGS: {
        MinExprList *variant = getMinExp_Args(node);
        emitMinExprList(variant, context);
        break;
    }
    case MINEXP_TYPE_AVAR: {
        emitMinAnnotatedVar(getMinExp_Avar(node), context);
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        fprintf(FH(context), "value_Stdint(");
        fprintMaybeBigInt(FH(context), getMinExp_BigInteger(node)); // FIXME
        fprintf(FH(context), ")");
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        fprintf(FH(context), "value_Character(%d)",
                (int)getMinExp_Character(node));
        break;
    }
    case MINEXP_TYPE_COND: {
        MinCond *variant = getMinExp_Cond(node);
        emitMinCond(variant, context);
        break;
    }
    case MINEXP_TYPE_DONE: {
        fprintf(FH(context), "exit(0);");
        break;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        emitMinIff(variant, context);
        break;
    }
    case MINEXP_TYPE_LAM: {
        MinLam *variant = getMinExp_Lam(node);
        emitMinLam(variant, context);
        break;
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        emitMinLetRec(variant, context);
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList *variant = getMinExp_MakeVec(node);
        // emitMakeVec(variant, context);
        cant_happen("non-simple makevec");
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        emitMinMatch(variant, context);
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // emitSimpleExp(node, context);
        cant_happen("non-simple prim");
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        emitMinExprList(variant, context);
        break;
    }
    case MINEXP_TYPE_STDINT: {
        fprintf(FH(context), "%d", getMinExp_Stdint(node));
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %s", minExpTypeName(node->type));
    }
}

//////////////
// Public API
//////////////

void emitProgram(MinExp *node, BuiltIns *builtIns, FILE *out) {
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    EmitContext *context = newEmitContext(body, builtIns, NULL);
    REPLACE_PROTECT(save, context);
    fprintf(out, "#include <stdarg.h>\n");
    fprintf(out, "#include \"cekfs.h\"\n");
    fprintf(out, "#include \"arithmetic_next.h\"\n");
    fprintf(out, "#include \"common.h\"\n");
    fprintf(out, "#include \"types.h\"\n");
    fprintf(out, "static inline bool isTrue(Value v) {\n");
    fprintf(out, "    return v.val.stdint != 0;");
    fprintf(out, "}\n");
    fprintf(out, "static inline Value vec(Value index, Value array) {\n");
    fprintf(out, "    return array.val.vec->entries[index.val.stdint];\n");
    fprintf(out, "}\n");
    fprintf(out, "static inline Value eq(Value v1, Value v2) {\n");
    fprintf(out, "    return value_Stdint(eqValue(v1, v2));\n");
    fprintf(out, "}\n");
    fprintf(out, "Value ne(Value, Value);\n");
    fprintf(out, "Value lt(Value, Value);\n");
    fprintf(out, "Value gt(Value, Value);\n");
    fprintf(out, "Value cmp(Value, Value);\n");
    fprintf(out, "static Value make_vec(int count, ...) {\n");
    fprintf(out, "    va_list ap;\n");
    fprintf(out, "    va_start(ap, count);\n");
    fprintf(out, "    Vec *v = newVec(count);\n");
    fprintf(out, "    int save = PROTECT(v);\n");
    fprintf(out, "    for (int j = 0; j < count; j++) {\n");
    fprintf(out, "        v->entries[j] = va_arg(ap, Value);\n");
    fprintf(out, "    }\n");
    fprintf(out, "    va_end(ap);\n");
    fprintf(out, "    UNPROTECT(save);\n");
    fprintf(out, "    return value_Vec(v);\n");
    fprintf(out, "}\n");
    fprintf(out, "\n");
    fprintf(out, "int main(int argc, char *argv[]) {");
    enterScope(context);
    emitMinExp(node, context);
    leaveScope(context);
    fprintf(out, "Value reg[%d];\n", context->maxReg);
    Opaque *buf = NULL;
    Index i = 0;
    HashSymbol *label = NULL;
    while ((label = iterateBufferBag(context->lambdas, &i, &buf)) != NULL) {
        SCharArray *l = makeLambdaLabel(label);
        fprintf(out, "%s:\n", l->entries);
        fprintf(out, "%s\n", opaqueEmitBufferContent(buf));
    }
    fprintf(out, "%s\n", opaqueEmitBufferContent(context->body));
    fprintf(out, "}\n");
}