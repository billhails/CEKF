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
static void emitMinPrimApp(MinPrimApp *node, EmitContext *context);
static void emitMinApply(MinApply *node, EmitContext *context);
static void emitMinIff(MinIff *node, EmitContext *context);
static void emitMinCond(MinCond *node, EmitContext *context);
static void emitMinIntCondCases(MinIntCondCases *node, EmitContext *context);
static void emitMinCharCondCases(MinCharCondCases *node, EmitContext *context);
static void emitMinMatch(MinMatch *node, EmitContext *context);
static void emitMinMatchList(MinMatchList *node, EmitContext *context);
static void emitMinIntList(MinIntList *node, EmitContext *context);
static void emitMinLetRec(MinLetRec *node, EmitContext *context);
static void emitMinBindings(MinBindings *node, EmitContext *context);
static void emitMinExp(MinExp *node, EmitContext *context);
static void emitMinCondCases(MinCondCases *node, EmitContext *context);

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

static SCharVec *makeLabel(char *prefix, HashSymbol *symbol) {
    SCharVec *label = newSCharVec(strlen(prefix) + strlen(symbol->name) + 1);
    sprintf(label->entries, "%s", prefix);
    char *from = symbol->name;
    char *to = &label->entries[strlen(prefix)];
    while (*from) {
        if (isalnum(*from))
            *to = *from;
        else
            *to = '_';
        from++;
        to++;
    }
    return label;
}

static SCharVec *makeLambdaLabel(HashSymbol *symbol) {
    return makeLabel("LAMBDA_", symbol);
}

__attribute__((unused)) static SCharVec *makeVarName(HashSymbol *symbol) {
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
    EmitContext *new = newEmitContext(body);
    int save = PROTECT(new);
    new->lambdas = context->lambdas;
    new->indexes = context->indexes;
    new->currentDepth = context->currentDepth;
    new->maxReg = context->maxReg;
    UNPROTECT(save);
    return new;
}

///////////////////////////
// Visitor implementations
///////////////////////////

static void emitMinLam(MinLam *node, EmitContext *context) {
    context->currentDepth = 0; // reset
    // emitSymbolList(node->args, context);
    emitMinExp(node->exp, context);
}

static void emitMinAnnotatedVar(MinAnnotatedVar *node, EmitContext *context) {
    (void)context;
    fprintf(opaqueEmitBufferFh(context->body),
            "var[%d] /* emitMinAnnotatedVar %s */\n", node->position,
            node->var->name);
}

static void emitMinExprList(MinExprList *node, EmitContext *context) {
    if (node == NULL)
        return;
    emitMinExp(node->exp, context);
    emitMinExprList(node->next, context);
}

static void emitMinPrimApp(MinPrimApp *node, EmitContext *context) {
    fprintf(opaqueEmitBufferFh(context->body), "%s(",
            minPrimOpName(node->type));
    emitMinExp(node->exp1, context);
    fprintf(opaqueEmitBufferFh(context->body), ",");
    emitMinExp(node->exp2, context);
    fprintf(opaqueEmitBufferFh(context->body), ")");
}

static void emitVecChain(MinPrimApp *app, EmitContext *context) {
    fprintf(opaqueEmitBufferFh(context->body), "vec(%d, ",
            getMinExp_Stdint(app->exp1));
    if (isMinExp_Avar(app->exp2)) {
        fprintf(opaqueEmitBufferFh(context->body), "reg[%d]",
                getMinExp_Avar(app->exp2)->position);
    } else {
        emitVecChain(getMinExp_Prim(app->exp2), context);
    }
    fprintf(opaqueEmitBufferFh(context->body), ")");
}
// all apply are apply-closure
// body: ((vec 0 k<3>) (vec 1 k<3>) (+ a<1> b<2>) f<4>)
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
        return;
    }
    ppMinPrimApp(getMinExp_Prim(node->function));
    eprintf("\n\n");
    fprintf(opaqueEmitBufferFh(context->body), "void *target = ");
    MinPrimApp *fn = getMinExp_Prim(node->function); // (vec 0 k<3>)
    emitVecChain(fn, context);
    fprintf(opaqueEmitBufferFh(context->body), ".ptr;\n");
    MinExprList *args = node->args;
    fprintf(opaqueEmitBufferFh(context->body), "Value env = ");
    MinPrimApp *env = getMinExp_Prim(args->exp); // (vec 1 k<3>)
    emitVecChain(env, context);
    fprintf(opaqueEmitBufferFh(context->body), ";\n");
    int i = 1;
    args = args->next;
    while (args != NULL) {
        fprintf(opaqueEmitBufferFh(context->body), "Value arg%d = ", i);
        emitMinExp(args->exp, context);
        fprintf(opaqueEmitBufferFh(context->body), ";\n");
        args = args->next;
        i++;
    }
    fprintf(opaqueEmitBufferFh(context->body), "reg[0] = env;\n");
    int j = 1;
    while (j < i) {
        fprintf(opaqueEmitBufferFh(context->body), "reg[%d] = arg%d;\n", j, j);
        j++;
    }
    fprintf(opaqueEmitBufferFh(context->body), "goto *target;\n");
}

static void emitMinIff(MinIff *node, EmitContext *context) {
    emitMinExp(node->condition, context);
    emitMinExp(node->consequent, context);
    emitMinExp(node->alternative, context);
}

static void emitMinCond(MinCond *node, EmitContext *context) {
    emitMinExp(node->value, context);
    emitMinCondCases(node->cases, context);
}

static void emitMinIntCondCases(MinIntCondCases *node, EmitContext *context) {
    if (node == NULL)
        return;
    emitMinExp(node->body, context);
    emitMinIntCondCases(node->next, context);
}

static void emitMinCharCondCases(MinCharCondCases *node, EmitContext *context) {
    if (node == NULL)
        return;
    emitMinExp(node->body, context);
    emitMinCharCondCases(node->next, context);
}

static void emitMinMatch(MinMatch *node, EmitContext *context) {
    emitMinExp(node->index, context);
    emitMinMatchList(node->cases, context);
}

static void emitMinMatchList(MinMatchList *node, EmitContext *context) {
    if (node == NULL)
        return;
    emitMinIntList(node->matches, context);
    emitMinExp(node->body, context);
    emitMinMatchList(node->next, context);
}

static void emitMinIntList(MinIntList *node, EmitContext *context) {
    if (node == NULL)
        return;
    emitMinIntList(node->next, context);
}

static void emitMinLetRec(MinLetRec *node, EmitContext *context) {
    emitMinBindings(node->bindings, context);
    emitMinExp(node->body, context);
}

static void emitMakeClosure(MinExprList *exprs, HashSymbol *var,
                            EmitContext *context) {
    FILE *out = opaqueEmitBufferFh(context->body);
    SCharVec *label = makeLambdaLabel(var);
    fprintf(out, "vec[%d] = make_2_vec(&&%s, make_%d_vec(...));\n",
            context->currentDepth, label->entries, countMinExprList(exprs));
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
        fprintMaybeBigInt(opaqueEmitBufferFh(context->body),
                          getMinExp_BigInteger(node));
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        break;
    }
    case MINEXP_TYPE_COND: {
        MinCond *variant = getMinExp_Cond(node);
        emitMinCond(variant, context);
        break;
    }
    case MINEXP_TYPE_DONE: {
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
        MinExprList *variant = getMinExp_MakeVec(node);
        emitMinExprList(variant, context);
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        emitMinMatch(variant, context);
        break;
    }
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *variant = getMinExp_Prim(node);
        emitMinPrimApp(variant, context);
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        MinExprList *variant = getMinExp_Sequence(node);
        emitMinExprList(variant, context);
        break;
    }
    case MINEXP_TYPE_STDINT: {
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %s", minExpTypeName(node->type));
    }
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

//////////////
// Public API
//////////////

void emitProgram(MinExp *node, FILE *out) {
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    EmitContext *context = newEmitContext(body);
    REPLACE_PROTECT(save, context);
    emitMinExp(node, context);
    fprintf(out, "Value reg[%d];\n", context->maxReg);
    Opaque *buf = NULL;
    Index i = 0;
    HashSymbol *label = NULL;
    while ((label = iterateBufferBag(context->lambdas, &i, &buf)) != NULL) {
        SCharVec *l = makeLambdaLabel(label);
        fprintf(out, "%s:\n", l->entries);
        fprintf(out, "%s\n", opaqueEmitBufferContent(buf));
    }
    fprintf(out, "%s\n", opaqueEmitBufferContent(context->body));
}