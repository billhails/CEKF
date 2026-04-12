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

#include "memory.h"
#include "minlam.h"
#include "minlam_emit_c.h"
#include "minlam_pp.h"
#include "symbol.h"
#include "utils_helper.h"
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <sys/param.h>

#ifdef DEBUG_MINLAM_EMIT
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static void emitMinLam(MinLam *, CEmitterContext *);
static void emitMinAnnotatedVar(MinAnnotatedVar *, CEmitterContext *);
static void emitMinApply(MinApply *, CEmitterContext *);
static void emitMinIff(MinIff *, CEmitterContext *);
static void emitMinCond(MinCond *, CEmitterContext *);
static void emitMinIntCondCases(MinIntCondCases *, const char *,
                                CEmitterContext *);
static void emitMinCharCondCases(MinCharCondCases *, CEmitterContext *);
static void emitMinMatch(MinMatch *, CEmitterContext *);
static void emitMinMatchList(MinMatchList *, CEmitterContext *);
static void emitMinLetRec(MinLetRec *, CEmitterContext *);
static void emitMinBindings(MinBindings *, Integer, CResultArray *,
                            CEmitterContext *);
static void emitMinExp(MinExp *, CEmitterContext *);
static void emitMinCondCases(MinCondCases *, CEmitterContext *);
static EmitCResult *emitMakeVec(MinExprList *, CEmitterContext *);
static EmitCResult *emitSimpleExp(MinExp *, CEmitterContext *);
static CEmitterContext *extendContext(CEmitterContext *, Opaque *);
static void emitMaybeBigInt(MaybeBigInt *, CEmitterContext *);
static bool emitAtomic(MinExp *exp, CEmitterContext *ctx);
static char *resultText(EmitCResult *data, CEmitterContext *ctx);

///////////////////////
// Emit Buffer Helpers
///////////////////////

#define EMITLOC(name, node, ctx)                                               \
    if (node == NULL || CPI(node).lineNo == 0)                                 \
        fprintf(FH(ctx), "// %s\n", name);                                     \
    else                                                                       \
        fprintf(FH(ctx), "// %s +%d %s\n", name, CPI(node).lineNo,             \
                CPI(node).fileName)

typedef struct EmitBuffer {
    FILE *fh;
    char *buffer;
    size_t size;
} EmitBuffer;

static EmitBuffer *newEmitBuffer();
static char *getEmitBuffer(EmitBuffer *);
static void cleanEmitBuffer(void *);
static void printEmitBuffer(FILE *, void *);

static inline Opaque *newOpaque_EmitBuffer() {
    // no protection as buffer is manually cleaned
    EmitBuffer *buffer = newEmitBuffer();
    return newOpaque(buffer, cleanEmitBuffer, printEmitBuffer, NULL);
}

static inline char *opaqueEmitBufferContent(Opaque *container) {
    return getEmitBuffer((EmitBuffer *)container->data);
}

static inline FILE *opaqueEmitBufferFh(Opaque *container) {
    return ((EmitBuffer *)(container->data))->fh;
}

static inline FILE *FH(CEmitterContext *ctx) {
    return opaqueEmitBufferFh(ctx->body);
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

static void printEmitBuffer(FILE *fp, void *buffer) {
    EmitBuffer *b = (EmitBuffer *)buffer;
    if (b != NULL) {
        fflush(b->fh);
        if (b->buffer != NULL)
            fprintf(fp, "%s", b->buffer);
    }
}

static void comment(CEmitterContext *ctx, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(FH(ctx), "// ");
    vfprintf(FH(ctx), fmt, args);
    fprintf(FH(ctx), "\n");
    va_end(args);
}

static void emitCharacter(Character character, CEmitterContext *ctx) {
    fprintf(FH(ctx), "value_Character(%d)", (int)character);
}

static void emitInteger(Integer i, CEmitterContext *ctx) {
    fprintf(FH(ctx), "value_Stdint(%d)", i);
}

static bool emitVec(MinExp *exp1, MinExp *exp2, CEmitterContext *ctx) {
    bool isConst = true;
    fprintf(FH(ctx), "vec(");
    isConst = isConst && emitAtomic(exp1, ctx);
    fprintf(FH(ctx), ", ");
    isConst = isConst && emitAtomic(exp2, ctx);
    fprintf(FH(ctx), ")");
    return isConst;
}

static void emitAssignPrimOp2(EmitCResult *target, char *opName,
                              EmitCResult *arg1, EmitCResult *arg2,
                              CEmitterContext *ctx) {
    fprintf(FH(ctx), "%s = %s(%s, %s);\n", resultText(target, ctx), opName,
            resultText(arg1, ctx), resultText(arg2, ctx));
}

static void emitAssignPrimOp1(EmitCResult *target, char *opName,
                              EmitCResult *arg1, CEmitterContext *ctx) {
    fprintf(FH(ctx), "%s = %s(%s);\n", resultText(target, ctx), opName,
            resultText(arg1, ctx));
}

static void emitAddrResult(Opaque *buf, SCharArray *label) {
    fprintf(opaqueEmitBufferFh(buf), "value_Addr(&&%s)", label->entries);
}

static void emitIntegerResult(Opaque *buf, Integer i) {
    fprintf(opaqueEmitBufferFh(buf), "value_Stdint(%d)", i);
}

static void emitCharacterResult(Opaque *buf, Character c) {
    fprintf(opaqueEmitBufferFh(buf), "value_Character(%d)", (int)c);
}

static void emitTextResult(Opaque *buf, char *text) {
    fprintf(opaqueEmitBufferFh(buf), "%s", text);
}

static void emitMaybeBigInt(MaybeBigInt *mbi, CEmitterContext *ctx) {
    switch (mbi->type) {
    case BI_BIG: {
        if (mbi->imag) {
            fprintf(FH(ctx), "value_Bigint_imag(");
        } else {
            fprintf(FH(ctx), "value_Bigint(");
        }
        bigint bi = mbi->big;
        fprintf(FH(ctx), "minlam_runtime_BigInt(%d, %d, %d", bi.size,
                bi.capacity, bi.neg);
        for (int i = 0; i < bi.capacity; i++) {
            fprintf(FH(ctx), ", %u", bi.words[i]);
        }
        fprintf(FH(ctx), "))");
        // minlam_runtime_BigInt will have done a PROTECT that needs restoring
        ctx->context.needsUnprotect = true;
        break;
    }
    case BI_SMALL: {
        if (mbi->imag) {
            fprintf(FH(ctx), "value_Stdint_imag(%d)", mbi->small);
        } else {
            fprintf(FH(ctx), "value_Stdint(%d)", mbi->small);
        }
        break;
    }
    case BI_IRRATIONAL: {
        if (mbi->imag) {
            if (isnan(mbi->irrational)) {
                fprintf(FH(ctx), "value_Irrational_imag(NAN)");
            } else {
                fprintf(FH(ctx), "value_Irrational_imag(%f)", mbi->irrational);
            }
        } else {
            if (isnan(mbi->irrational)) {
                fprintf(FH(ctx), "value_Irrational(NAN)");
            } else {
                fprintf(FH(ctx), "value_Irrational(%f)", mbi->irrational);
            }
        }
        break;
    }
    default:
        cant_happen("unhandled numeric type %d", mbi->type);
    }
}

///////////////////
// Context Helpers
///////////////////

static CEmitterContext *extendContextForLambda(HashSymbol *var,
                                               CEmitterContext *ctx) {
    // create a new memstream for the lambda body
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    // add it to the global set of lambdas
    setCBufferBag(ctx->lambdas, var, body);
    SymbolArray *heap = emitter_createHeap();
    PROTECT(heap);
    EmitterContext context = ctx->context;
    context.heap = heap;
    context.slots = newSlotPool();
    PROTECT(context.slots);
    context.activeSlots = 0;
    context.totalSlots = 0;
    context.currentReg = 0;
    context.needsUnprotect = false;
    CEmitterContext *new = newCEmitterContext(body, context);
    PROTECT(new);
    new->lambdas = ctx->lambdas;
    UNPROTECT(save);
    return new;
}

// only for atomics: sub-context must not allocate slots
static CEmitterContext *extendContext(CEmitterContext *ctx, Opaque *body) {
    CEmitterContext *new = newCEmitterContext(body, ctx->context);
    int save = PROTECT(new);
    new->lambdas = ctx->lambdas;
    UNPROTECT(save);
    return new;
}

////////////////////
// SlotPool Helpers
////////////////////

static EmitCResult *claimSlot(CEmitterContext *ctx) {
    HashSymbol *symbol = emitter_claimSlotSymbol(&ctx->context);
    return newEmitCResult_Var(symbol);
}

static void releaseSlot(EmitCResult *result, EmitterContext *ctx) {
    if (isEmitCResult_Var(result))
        emitter_releaseSlotSymbol(getEmitCResult_Var(result), ctx);
}

static void releaseSlots(CResultArray *slots, EmitterContext *ctx) {
    for (Index i = 0; i < slots->size; i++) {
        releaseSlot(slots->entries[i], ctx);
    }
}

static CResultArray *claimSlotsBelow(int N, CEmitterContext *ctx) {
    comment(ctx, "ensuring reg[0..%d]", N - 1);
    CResultArray *slots = newCResultArray();
    int save = PROTECT(slots);
    while (emitter_slotsAvailableBelow(N, &ctx->context)) {
        EmitCResult *slot = claimSlot(ctx);
        int save2 = PROTECT(slot);
        pushCResultArray(slots, slot);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return slots;
}

static CResultArray *claimSlots(int N, CEmitterContext *ctx) {
    CResultArray *slots = newCResultArray();
    int save = PROTECT(slots);
    for (int i = 0; i < N; ++i) {
        EmitCResult *slot = claimSlot(ctx);
        int save2 = PROTECT(slot);
        pushCResultArray(slots, slot);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return slots;
}

static char *resultText(EmitCResult *data, CEmitterContext *ctx) {
    switch (data->type) {
    case EMITCRESULT_TYPE_BUF:
        return opaqueEmitBufferContent(getEmitCResult_Buf(data));
    case EMITCRESULT_TYPE_CONSTANT:
        return opaqueEmitBufferContent(getEmitCResult_Constant(data));
    case EMITCRESULT_TYPE_VAR:
        return emitter_getSlot(getEmitCResult_Var(data), &ctx->context)
            ->text->entries;
    default:
        cant_happen("unrecognised %s", emitCResultTypeName(data->type));
    }
}

//////////////////
// MinExp Helpers
//////////////////

// an expression is atomic if it causes no memory allocation
static bool isAtomic(MinExp *exp) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
    case MINEXP_TYPE_BIGINTEGER: // not true, but there is a workaround
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

// assertive retrieval, all letrec bindings are expected to be closures
static MinLam *extractLambda(MinExp *closure) {
    MinExprList *makeVec = getMinExp_MakeVec(closure);
#ifdef SAFETY_CHECKS
    if (makeVec == NULL)
        cant_happen("letrec binding must be a 2-vec");
#endif
    return getMinExp_Lam(makeVec->exp);
}

static MinExprList *extractEnv(MinExp *closure) {
    MinExprList *makeVec = getMinExp_MakeVec(closure);
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

static int getPrimOpArity(MinPrimOp type) {
    switch (type) {
    case MINPRIMOP_TYPE_CANON:
        return 1;
    default:
        return 2;
    }
}

static char *getPrimOpCName(MinPrimOp type) {
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

/////////////////
// Label Helpers
/////////////////

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
            sprintf(buffer, "_%d", (int)*c);
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

////////////
// Emitters
////////////

static bool emitAtomic(MinExp *exp, CEmitterContext *ctx) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
        emitMinAnnotatedVar(getMinExp_Avar(exp), ctx);
        return false;
    case MINEXP_TYPE_BIGINTEGER:
        emitMaybeBigInt(getMinExp_BigInteger(exp), ctx);
        return true;
    case MINEXP_TYPE_CHARACTER:
        emitCharacter(getMinExp_Character(exp), ctx);
        return true;
    case MINEXP_TYPE_STDINT:
        emitInteger(getMinExp_Stdint(exp), ctx);
        return true;
    case MINEXP_TYPE_PRIM: {
        bool isConst = true;
        MinPrimApp *prim = getMinExp_Prim(exp);
        if (prim->type == MINPRIMOP_TYPE_VEC) {
            isConst = isConst && emitVec(prim->exp1, prim->exp2, ctx);
        } else {
            cant_happen("emitAtomic passed non-atomic %s",
                        minExpTypeName(exp->type));
        }
        return isConst;
    }
    default:
        cant_happen("emitAtomic passed non-atomic %s",
                    minExpTypeName(exp->type));
    }
}

static EmitCResult *emitNewAtomic(MinExp *exp, CEmitterContext *ctx) {
    Opaque *buffer = newOpaque_EmitBuffer();
    int save = PROTECT(buffer);
    CEmitterContext *atomicContext = extendContext(ctx, buffer);
    PROTECT(atomicContext);
    bool isConst = emitAtomic(exp, atomicContext);
    setProtectionStatus(&ctx->context, &atomicContext->context);
    EmitCResult *result =
        isConst ? newEmitCResult_Constant(buffer) : newEmitCResult_Buf(buffer);
    UNPROTECT(save);
    return result;
}

static EmitCResult *emitArg(MinExp *arg, CEmitterContext *ctx) {
    if (isAtomic(arg)) {
        return emitNewAtomic(arg, ctx);
    } else {
        return emitSimpleExp(arg, ctx);
    }
}

static EmitCResult *emitPrimOp(MinPrimApp *app, CEmitterContext *ctx) {
    EmitCResult *arg1 = emitArg(app->exp1, ctx);
    int save = PROTECT(arg1);
    EmitCResult *arg2 = emitArg(app->exp2, ctx);
    PROTECT(arg2);
    // deliberate early release of any temps prior to claiming a new one
    // allows i.e. `reg[1] = op(reg[1], reg[2]);` as long as we don't emit
    // anything that could clobber the released slots before we are done
    // with them.
    releaseSlot(arg1, &ctx->context);
    releaseSlot(arg2, &ctx->context);
    EmitCResult *target = claimSlot(ctx);
    PROTECT(target);
    char *opName = getPrimOpCName(app->type);
    int nargs = getPrimOpArity(app->type);
    // the first thing we emit consumes the released but not yet clobbered
    // slots.
    if (nargs == 2) {
        emitAssignPrimOp2(target, opName, arg1, arg2, ctx);
    } else {
        emitAssignPrimOp1(target, opName, arg1, ctx);
    }
    UNPROTECT(save);
    return target; // the temp register that the caller can refer to
}

static EmitCResult *emitAnonymousLambda(MinLam *lambda, CEmitterContext *ctx) {
    EMITLOC("emitAnonymousLambda", lambda, ctx);
    // emit the lambda to the prelude, this then reduces to &&LABEL
    HashSymbol *name = genSym("anon");
    CEmitterContext *lamContext = extendContextForLambda(name, ctx);
    int save = PROTECT(lamContext);
    SCharArray *containing_label = makeLambdaLabel(ctx->context.currentBinding);
    PROTECT(containing_label);
    comment(lamContext, "inside %s, %d args", containing_label->entries,
            countSymbolList(lambda->args));
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(&ctx->context, &lamContext->context);
    SCharArray *label = makeLambdaLabel(name);
    PROTECT(label);
    Opaque *buf = newOpaque_EmitBuffer();
    PROTECT(buf);
    emitAddrResult(buf, label);
    EmitCResult *result = newEmitCResult_Buf(buf);
    UNPROTECT(save);
    return result;
}

static EmitCResult *emitSimpleExp(MinExp *exp, CEmitterContext *ctx) {
    switch (exp->type) {
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *app = getMinExp_Prim(exp);
        return emitPrimOp(app, ctx);
    }
    case MINEXP_TYPE_AVAR: {
        MinAnnotatedVar *avar = getMinExp_Avar(exp);
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        CEmitterContext *aVarContext = extendContext(ctx, buf);
        setProtectionStatus(&ctx->context, &aVarContext->context);
        PROTECT(aVarContext);
        emitMinAnnotatedVar(avar, aVarContext);
        EmitCResult *result = newEmitCResult_Buf(buf);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *vecs = getMinExp_MakeVec(exp);
        return emitMakeVec(vecs, ctx);
    }
    case MINEXP_TYPE_LAM: {
        MinLam *lambda = getMinExp_Lam(exp);
        return emitAnonymousLambda(lambda, ctx);
    }
    case MINEXP_TYPE_BIGINTEGER: {
        MaybeBigInt *mbi = getMinExp_BigInteger(exp);
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        CEmitterContext *biContext = extendContext(ctx, buf);
        PROTECT(biContext);
        emitMaybeBigInt(mbi, biContext);
        EmitCResult *result = newEmitCResult_Constant(buf);
        setProtectionStatus(&ctx->context, &biContext->context);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_STDINT: {
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        emitIntegerResult(buf, getMinExp_Stdint(exp));
        EmitCResult *result = newEmitCResult_Constant(buf);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_CHARACTER: {
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        emitCharacterResult(buf, getMinExp_Character(exp));
        EmitCResult *result = newEmitCResult_Constant(buf);
        UNPROTECT(save);
        return result;
    }
    default:
        ppMinExp(stderr, exp);
        eprintf("\n");
        cant_happen("unhandled %s", minExpTypeName(exp->type));
    }
}

static EmitCResult *emitConstant(char *text) {
    Opaque *buf = newOpaque_EmitBuffer();
    emitTextResult(buf, text);
    int save = PROTECT(buf);
    EmitCResult *target = newEmitCResult_Constant(buf);
    UNPROTECT(save);
    return target;
}

// prefer to emit right to left because the common case for deeply nested
// vecs is lists which are right-associative, and by emitting r2l we need
// only one temp for the entire backbone of the list.
static CResultArray *emitRL(MinExprList *vecs, CEmitterContext *ctx) {
    if (vecs == NULL)
        return newCResultArray();
    CResultArray *results = emitRL(vecs->next, ctx);
    int save = PROTECT(results);
    EmitCResult *result = emitArg(vecs->exp, ctx);
    PROTECT(result);
    pushCResultArray(results, result);
    UNPROTECT(save);
    return results;
}

static EmitCResult *emitMakeVec(MinExprList *vecs, CEmitterContext *ctx) {
    CResultArray *results = emitRL(vecs, ctx);
    int save = PROTECT(results);
    int count = countMinExprList(vecs);
    releaseSlots(results, &ctx->context);
    EmitCResult *target = NULL;
    if (count == 0) {
        target = emitConstant("value_None()");
    } else {
        target = claimSlot(ctx);
        PROTECT(target);
        fprintf(FH(ctx), "%s = make_vec(%d", resultText(target, ctx), count);
        for (Index i = results->size; i > 0; i--) {
            fprintf(FH(ctx), ", %s", resultText(results->entries[i - 1], ctx));
        }
        fprintf(FH(ctx), ");\n");
    }
    UNPROTECT(save);
    return target;
}

static void emitMinLam(MinLam *node, CEmitterContext *ctx) {
#ifdef SAFETY_CHECKS
    if (node == NULL)
        cant_happen("NULL MinLam");
#endif
    int nargs = countSymbolList(node->args);
    fprintf(FH(ctx), "// emitMinLam reserving %d incoming slots\n", nargs);
    CResultArray *slots = claimSlots(nargs, ctx);
    int save = PROTECT(slots);
    ctx->context.currentReg += nargs;
    ASSERT_SLOTS(&ctx->context);
    if (node->exp != NULL)
        emitMinExp(node->exp, ctx);
    releaseSlots(slots, &ctx->context);
    ctx->context.currentReg -= nargs;
    UNPROTECT(save);
}

static void emitMinAnnotatedVar(MinAnnotatedVar *node, CEmitterContext *ctx) {
    fprintf(FH(ctx), "reg[%d] /* %s */", node->position, node->var->name);
}

static inline void emitAssign(EmitCResult *to, EmitCResult *from,
                              CEmitterContext *ctx) {
    fprintf(FH(ctx), "%s = %s;\n", resultText(to, ctx), resultText(from, ctx));
}

static inline void emitAssignReg(Index i, EmitCResult *value,
                                 CEmitterContext *ctx) {
    fprintf(FH(ctx), "reg[%d] = %s;\n", (int)i, resultText(value, ctx));
}

static void emitGoto(EmitCResult *target, CResultArray *args,
                     CEmitterContext *ctx) {
    fprintf(FH(ctx), "// emitGoto - %u outgoing slots\n", args->size);
    CResultArray *sourceSlots = newCResultArray();
    int save = PROTECT(sourceSlots);
    CResultArray *claimedSlots = newCResultArray();
    PROTECT(claimedSlots);

    for (Index i = 0; i < args->size; i++) {
        EmitCResult *tempSlot = claimSlot(ctx);
        int save2 = PROTECT(tempSlot);
        EmitCResult *arg = args->entries[i];
        if (isEmitCResult_Buf(arg)) {
            emitAssign(tempSlot, arg, ctx);
            pushCResultArray(sourceSlots, tempSlot);
        } else {
            // a var has already claimed a safe slot, use it directly for
            // copy-down, likewise if this is a constant expression.
            pushCResultArray(sourceSlots, arg);
        }
        pushCResultArray(claimedSlots, tempSlot);
        UNPROTECT(save2);
    }

    EmitCResult *savedTarget = claimSlot(ctx);
    PROTECT(savedTarget);
    pushCResultArray(claimedSlots, savedTarget);
    if (isEmitCResult_Buf(target)) {
        emitAssign(savedTarget, target, ctx);
    } else {
        savedTarget = target;
    }

    // Copy-down to reg[0..N-1]
    for (Index i = 0; i < sourceSlots->size; i++) {
        emitAssignReg(i, sourceSlots->entries[i], ctx);
    }

    if (ctx->context.needsUnprotect) {
        fprintf(FH(ctx), "minlam_runtime_unprotect();\n");
        ctx->context.needsUnprotect = false;
    }

    fprintf(FH(ctx), "TRACE(\"%s\", getValue_Addr(%s), %d);\n",
            ctx->context.currentBinding->name, resultText(savedTarget, ctx),
            (int)args->size);
    fprintf(FH(ctx), "goto *getValue_Addr(%s);\n",
            resultText(savedTarget, ctx));
    releaseSlots(claimedSlots, &ctx->context);
    UNPROTECT(save);
}

static EmitCResult *emitExtractFromClosure(EmitCResult *closure, int index,
                                           CEmitterContext *ctx) {
    // this must never write to slots except this one
    EmitCResult *result = claimSlot(ctx);
    int save = PROTECT(result);
    fprintf(FH(ctx), "%s = vec(value_Stdint(%d), %s);\n",
            resultText(result, ctx), index, resultText(closure, ctx));
    UNPROTECT(save);
    return result;
}

static EmitCResult *emitCallBuiltin(BuiltIn *bi, MinExprList *builtinArgs,
                                    CEmitterContext *ctx) {
    EmitCResult *builtinArg = NULL;
    if (countBuiltInArgs(bi->args) == 0)
        builtinArg = emitConstant("make_vec(0)"); // just for builtins
    else
        builtinArg = emitMakeVec(builtinArgs, ctx);
    int save = PROTECT(builtinArg);
    EmitCResult *builtinResult = claimSlot(ctx);
    PROTECT(builtinResult);
    fprintf(FH(ctx), "extern Value %s(Vec *);\n", bi->linkerName->name);
    fprintf(FH(ctx), "%s = %s(getValue_Vec(%s));\n",
            resultText(builtinResult, ctx), bi->linkerName->name,
            resultText(builtinArg, ctx));
    releaseSlot(builtinArg, &ctx->context);
    UNPROTECT(save);
    return builtinResult;
}

// (builtin arg_1 ... arg_n k f)
// basically: "give the result of calling the builtin to k, passing f along"
// Where k itself is a closure (2-vec)
static void emitApplyBuiltin(MinApply *node, CEmitterContext *ctx) {
    EMITLOC("emitApplyBuiltin", node, ctx);

    CResultArray *contArgSlots =
        claimSlotsBelow(3, ctx); // (3 args: env, result, f)
    int save = PROTECT(contArgSlots);

    BuiltIn *bi = emitter_findBuiltIn(node, &ctx->context);
    int numBuiltinArgs = countBuiltInArgs(bi->args);

    // k and f
    MinExprList *continuations = dropMinExprList(node->args, numBuiltinArgs);

    // k
    EmitCResult *k = emitSimpleExp(continuations->exp, ctx);
    PROTECT(k);

    CResultArray *targetArgs = newCResultArray();
    PROTECT(targetArgs);

    // (env)
    EmitCResult *env = emitExtractFromClosure(k, 1, ctx);
    PROTECT(env);
    pushCResultArray(targetArgs, env);

    EmitCResult *target = emitExtractFromClosure(k, 0, ctx);
    PROTECT(target);

    releaseSlot(k, &ctx->context);

    // (env result)
    MinExprList *builtinArgs = takeMinExprList(node->args, numBuiltinArgs);
    PROTECT(builtinArgs);
    EmitCResult *builtinResult = emitCallBuiltin(bi, builtinArgs, ctx);
    PROTECT(builtinResult);
    pushCResultArray(targetArgs, builtinResult);

    continuations = continuations->next;

    // (env result f)
    EmitCResult *f = emitSimpleExp(continuations->exp, ctx);
    PROTECT(f);
    pushCResultArray(targetArgs, f);

    // (k env result f)
    emitGoto(target, targetArgs, ctx);
    releaseSlots(targetArgs, &ctx->context);
    releaseSlots(contArgSlots, &ctx->context);
    releaseSlot(target, &ctx->context);
    UNPROTECT(save);
    return;
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

static void emitApplyClosure(MinApply *apply, CEmitterContext *ctx) {
    EMITLOC("emitApplyClosure", apply, ctx);

    CResultArray *closureArgSlots =
        claimSlotsBelow(countMinExprList(apply->args) + 1, // +1 for closure env
                        ctx);
    int save = PROTECT(closureArgSlots);

    EmitCResult *closure = emitArg(apply->function, ctx);
    PROTECT(closure);

    EmitCResult *target = emitExtractFromClosure(closure, 0, ctx);
    PROTECT(target);
    // deliberate early release allows for behaviour like
    // tmp_1 = vec(..., tmp_1);
    releaseSlot(closure, &ctx->context);

    EmitCResult *env = emitExtractFromClosure(closure, 1, ctx);
    PROTECT(env);

    CResultArray *targetArgs = newCResultArray();
    PROTECT(targetArgs);
    pushCResultArray(targetArgs, env);

    for (MinExprList *arg = apply->args; arg != NULL; arg = arg->next) {
        EmitCResult *result = emitSimpleExp(arg->exp, ctx);
        int save2 = PROTECT(result);
        pushCResultArray(targetArgs, result);
        UNPROTECT(save2);
    }

    emitGoto(target, targetArgs, ctx);
    releaseSlot(target, &ctx->context);
    releaseSlots(targetArgs, &ctx->context);
    releaseSlots(closureArgSlots, &ctx->context);
    UNPROTECT(save);
}

static void emitMinApply(MinApply *node, CEmitterContext *ctx) {
    if (node->isBuiltin) {
        emitApplyBuiltin(node, ctx);
    } else {
        emitApplyClosure(node, ctx);
    }
}

static void emitMinIff(MinIff *node, CEmitterContext *ctx) {
    EMITLOC("emitMinIff", node, ctx);
    EmitCResult *test = emitSimpleExp(node->condition, ctx);
    int save = PROTECT(test);
    fprintf(FH(ctx), "if (isTrue(%s)) {\n", resultText(test, ctx));
    releaseSlot(test, &ctx->context);
    UNPROTECT(save);
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->consequent, ctx);
    fprintf(FH(ctx), "} else {\n");
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->alternative, ctx);
    fprintf(FH(ctx), "}\n");
}

static void emitMinCond(MinCond *node, CEmitterContext *ctx) {
    EMITLOC("emitMinCond", node, ctx);
    EmitCResult *cond = emitSimpleExp(node->value, ctx);
    int save = PROTECT(cond);
    if (node->cases->type == MINCONDCASES_TYPE_INTEGERS) {
        const char *condText = resultText(cond, ctx);
        releaseSlot(cond, &ctx->context);
        emitMinIntCondCases(getMinCondCases_Integers(node->cases), condText,
                            ctx);
    } else {
        fprintf(FH(ctx), "switch(getValue_Character(%s)) {\n",
                resultText(cond, ctx));
        releaseSlot(cond, &ctx->context);
        emitMinCondCases(node->cases, ctx);
        fprintf(FH(ctx), "}\n");
    }
    UNPROTECT(save);
}

static void emitMinCondCases(MinCondCases *node, CEmitterContext *ctx) {
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        cant_happen("integer cond cases handled in emitMinCond");
        break;
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        emitMinCharCondCases(variant, ctx);
        break;
    }
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
}

static void emitMinIntCondCases(MinIntCondCases *node, const char *condText,
                                CEmitterContext *ctx) {
    if (node == NULL) {
        return;
    }
    EMITLOC("emitMinIntCondCases", node, ctx);
    if (node->constant == NULL) {
        // default case
        fprintf(FH(ctx), "{ // default\n");
    } else {
        fprintf(FH(ctx), "if (minlam_runtime_cmp(%s, ", condText);
        emitMaybeBigInt(node->constant, ctx);
        fprintf(FH(ctx), ") == CMP_EQ) {\n");
    }
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->body, ctx);
    if (node->next != NULL) {
        fprintf(FH(ctx), "} else ");
        emitMinIntCondCases(node->next, condText, ctx);
    } else {
        fprintf(FH(ctx), "}\n");
    }
}

static void emitMinCharCondCases(MinCharCondCases *node, CEmitterContext *ctx) {
    EMITLOC("emitMinCharCondCases", node, ctx);
    if (node == NULL) {
        fprintf(FH(ctx), "default:\n");
        fprintf(FH(ctx),
                "fprintf(stderr, \"conditions exhausted in %s\\n\");\n",
                ctx->context.currentBinding->name);
        fprintf(FH(ctx), "abort();\n");
        return;
    }
    if (node->isDefault) {
        fprintf(FH(ctx), "default: \n");
        ASSERT_SLOTS(&ctx->context);
        emitMinExp(node->body, ctx);
        fprintf(FH(ctx), "break;\n");
        return;
    }
    fprintf(FH(ctx), "case %d: \n", (int)(node->constant));
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->body, ctx);
    fprintf(FH(ctx), "break;\n");
    emitMinCharCondCases(node->next, ctx);
}

static void emitMinMatch(MinMatch *node, CEmitterContext *ctx) {
    EMITLOC("emitMinMatch", node, ctx);
    EmitCResult *match = emitSimpleExp(node->index, ctx);
    int save = PROTECT(match);
    fprintf(FH(ctx), "switch (getValue_Stdint(%s)) {\n",
            resultText(match, ctx));
    releaseSlot(match, &ctx->context);
    UNPROTECT(save);
    emitMinMatchList(node->cases, ctx);
    fprintf(FH(ctx), "}\n");
}

static void emitMinMatchList(MinMatchList *node, CEmitterContext *ctx) {
    if (node == NULL)
        return;
    EMITLOC("emitMinMatchList", node, ctx);
    MinIntList *match = node->matches;
    while (match != NULL) {
        fprintf(FH(ctx), "case %d:\n", match->item);
        match = match->next;
    }
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->body, ctx);
    fprintf(FH(ctx), "break;\n");
    emitMinMatchList(node->next, ctx);
}

// emitMinLetRec -> emitMinBindings -> emitMakeClosure
static void emitMakeClosure(HashSymbol *var, Integer depth,
                            CResultArray *bindings, CEmitterContext *ctx) {
    fprintf(FH(ctx), "// emitMakeClosure %d\n", depth);
    SCharArray *label = makeLambdaLabel(var);
    int save = PROTECT(label);
    fprintf(FH(ctx), "%s = make_vec(2, value_Addr(&&%s), value_None());\n",
            resultText(bindings->entries[depth], ctx), label->entries);
    UNPROTECT(save);
}

// emitMinLetRec -> emitBackpatchBindings -> emitBackpatchClosure
static void emitBackpatchClosure(MinExprList *exprs, int depth,
                                 CResultArray *slots, CEmitterContext *ctx) {
    if (exprs == NULL)
        return; // emitMakeClosure has already assigned the appropriate value
    fprintf(FH(ctx), "// emitBackpatchClosure %d\n", depth);
    EmitCResult *tmp = emitMakeVec(exprs, ctx);
    int save = PROTECT(tmp);
    fprintf(FH(ctx), "getValue_Vec(%s)->entries[1] = %s;\n",
            resultText(slots->entries[depth], ctx), resultText(tmp, ctx));
    releaseSlot(tmp, &ctx->context);
    UNPROTECT(save);
}

static void emitMinBindings(MinBindings *node, Integer depth,
                            CResultArray *bindings, CEmitterContext *ctx) {
    if (node == NULL)
        return;
    // Use a unique name to avoid CBufferBag hash collisions when the
    // same binding name (e.g. h1$0) appears in multiple letrecs.
    HashSymbol *uniqueName = genSym(node->var->name);
    CEmitterContext *lamContext = extendContextForLambda(uniqueName, ctx);
    int save = PROTECT(lamContext);
    lamContext->context.currentBinding = uniqueName;
    MinLam *lambda = extractLambda(node->val);
    // emit the lambda body to the new memstream
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(&ctx->context, &lamContext->context);
    // but emit the closure creation to the current memstream
    emitMakeClosure(uniqueName, depth, bindings, ctx);
    UNPROTECT(save);
    emitMinBindings(node->next, depth + 1, bindings, ctx);
}

static void emitBackpatchBindings(MinBindings *node, Integer depth,
                                  CResultArray *bindings,
                                  CEmitterContext *ctx) {
    if (node == NULL)
        return;
    MinExprList *makeEnv = extractEnv(node->val);
    emitBackpatchClosure(makeEnv, depth, bindings, ctx);
    emitBackpatchBindings(node->next, depth + 1, bindings, ctx);
}

static void emitMinLetRec(MinLetRec *node, CEmitterContext *ctx) {
    EMITLOC("emitMinLetRec", node, ctx);
    ASSERT_SLOTS(&ctx->context);
    int numBindings = countMinBindings(node->bindings);
    CResultArray *bindings = claimSlots(numBindings, ctx);
    int save = PROTECT(bindings);
    emitMinBindings(node->bindings, 0, bindings, ctx);
    emitBackpatchBindings(node->bindings, 0, bindings, ctx);
    ctx->context.currentReg += numBindings;
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->body, ctx);
    ctx->context.currentReg -= numBindings;
    releaseSlots(bindings, &ctx->context);
    ASSERT_SLOTS(&ctx->context);
    UNPROTECT(save);
}

static void emitMinExp(MinExp *node, CEmitterContext *ctx) {
    switch (node->type) {
    case MINEXP_TYPE_APPLY: {
        emitMinApply(getMinExp_Apply(node), ctx);
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // bare variable in tail position is dead code (CPS artifact)
        fprintf(FH(ctx), "// dead tail: ");
        emitMinAnnotatedVar(getMinExp_Avar(node), ctx);
        fprintf(FH(ctx), ";\n");
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        emitMaybeBigInt(getMinExp_BigInteger(node), ctx);
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        fprintf(FH(ctx), "value_Character(%d)", (int)getMinExp_Character(node));
        break;
    }
    case MINEXP_TYPE_COND: {
        MinCond *variant = getMinExp_Cond(node);
        emitMinCond(variant, ctx);
        break;
    }
    case MINEXP_TYPE_DONE: {
        fprintf(FH(ctx), "exit(%d);", getMinExp_Done(node));
        break;
    }
    case MINEXP_TYPE_IFF: {
        MinIff *variant = getMinExp_Iff(node);
        emitMinIff(variant, ctx);
        break;
    }
    case MINEXP_TYPE_LAM: {
        MinLam *variant = getMinExp_Lam(node);
        emitMinLam(variant, ctx);
        break;
    }
    case MINEXP_TYPE_LETREC: {
        MinLetRec *variant = getMinExp_LetRec(node);
        emitMinLetRec(variant, ctx);
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        emitMinMatch(variant, ctx);
        break;
    }
    case MINEXP_TYPE_STDINT: {
        fprintf(FH(ctx), "%d", getMinExp_Stdint(node));
        break;
    }
    default:
        cant_happen("unexpected MinExp type %s", minExpTypeName(node->type));
    }
}