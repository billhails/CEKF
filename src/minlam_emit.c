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
#include <math.h>
#include <sys/param.h>

#ifdef DEBUG_MINLAM_EMIT
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static void emitMinLam(MinLam *, EmitterContext *);
static void emitMinAnnotatedVar(MinAnnotatedVar *, EmitterContext *);
static void emitMinApply(MinApply *, EmitterContext *);
static void emitMinIff(MinIff *, EmitterContext *);
static void emitMinCond(MinCond *, EmitterContext *);
static void emitMinIntCondCases(MinIntCondCases *, const char *,
                                EmitterContext *);
static void emitMinCharCondCases(MinCharCondCases *, EmitterContext *);
static void emitMinMatch(MinMatch *, EmitterContext *);
static void emitMinMatchList(MinMatchList *, EmitterContext *);
static void emitMinLetRec(MinLetRec *, EmitterContext *);
static void emitMinBindings(MinBindings *, Integer, ResultArray *,
                            EmitterContext *);
static void emitMinExp(MinExp *, EmitterContext *);
static void emitMinCondCases(MinCondCases *, EmitterContext *);
static EmitResult *emitMakeVec(MinExprList *, EmitterContext *);
static EmitResult *emitSimpleExp(MinExp *, EmitterContext *);
static EmitterContext *extendContext(EmitterContext *, Opaque *);
static void emitMaybeBigInt(MaybeBigInt *, EmitterContext *);

/////////////////
// Debug Helpers
/////////////////

#ifdef SAFETY_CHECKS
static void assertNoLeakedSlots(EmitterContext *, int);
#define ASSERT_SLOTS(ctx) assertNoLeakedSlots(ctx, __LINE__)
#else
#define ASSERT_SLOTS(ctx)
#endif

#define EMITLOC(name, node, ctx)                                               \
    if (node == NULL || CPI(node).lineNo == 0)                                 \
        fprintf(FH(ctx), "// %s\n", name);                                     \
    else                                                                       \
        fprintf(FH(ctx), "// %s +%d %s\n", name, CPI(node).lineNo,             \
                CPI(node).fileName)

///////////////////////
// Emit Buffer Helpers
///////////////////////

typedef struct EmitBuffer {
    FILE *fh;
    char *buffer;
    size_t size;
} EmitBuffer;

static EmitBuffer *newEmitBuffer();
static char *getEmitBuffer(EmitBuffer *);
static void cleanEmitBuffer(void *);
static void printEmitBuffer(void *);

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

static inline FILE *FH(EmitterContext *ctx) {
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

static void printEmitBuffer(void *buffer) {
    EmitBuffer *b = (EmitBuffer *)buffer;
    fflush(b->fh);
    if (b->buffer != NULL)
        eprintf("%s", b->buffer);
}

///////////////////
// Context Helpers
///////////////////

static EmitterContext *extendContextForLambda(HashSymbol *var,
                                              EmitterContext *ctx) {
    // create a new memstream for the lambda body
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    // add it to the global set of lambdas
    setBufferBag(ctx->lambdas, var, body);
    SymbolArray *heap = emit_createHeap();
    PROTECT(heap);
    EmitterContext *new =
        newEmitterContext(ctx->currentBinding, body, ctx->builtIns, heap);
    PROTECT(new);
    new->lambdas = ctx->lambdas;
    new->maxReg = ctx->maxReg;
    UNPROTECT(save);
    return new;
}

// only for atomics: sub-ctx must not allocate slots
static EmitterContext *extendContext(EmitterContext *ctx, Opaque *body) {
    EmitterContext *new =
        newEmitterContext(ctx->currentBinding, body, ctx->builtIns, ctx->heap);
    int save = PROTECT(new);
    new->lambdas = ctx->lambdas;
    new->slots = ctx->slots;
    new->maxReg = ctx->maxReg;
    new->currentReg = ctx->currentReg;
    new->totalSlots = ctx->totalSlots;
    new->activeSlots = ctx->activeSlots;
    UNPROTECT(save);
    return new;
}

static inline void setMaxReg(EmitterContext *ctx) {
    ctx->maxReg = MAX(ctx->maxReg, ctx->totalSlots);
}

static inline void setProtectionStatus(EmitterContext *to,
                                       EmitterContext *from) {
    to->needsUnprotect = to->needsUnprotect || from->needsUnprotect;
}

static inline void retrieveMaxReg(EmitterContext *to, EmitterContext *from) {
    to->maxReg = MAX(to->maxReg, from->maxReg);
}

static BuiltIn *findBuiltIn(MinApply *node, EmitterContext *ctx) {
    BuiltIn *bi = NULL;
    for (Index i = 0; i < ctx->builtIns->size; i++) {
        if (ctx->builtIns->entries[i]->internalName ==
            getMinExp_Var(node->function)) {
            bi = ctx->builtIns->entries[i];
            break;
        }
    }
    if (bi == NULL) {
        cant_happen("could not find builtin %s",
                    getMinExp_Var(node->function)->name);
    }
    return bi;
}

////////////////////
// SlotPool Helpers
////////////////////

static void reportUnreleasedSlots(EmitterContext *ctx) {
    fprintf(stderr, "%d unreleased slots\n", ctx->activeSlots);
}

static Slot *createNewSlot(EmitterContext *ctx) {
    SCharArray *text = newSCharArray();
    int save = PROTECT(text);
    char buf[64];
    sprintf(buf, "reg[%d]", ctx->totalSlots);
    for (char *c = buf; *c; c++) {
        pushSCharArray(text, *c);
    }
    pushSCharArray(text, '\0');
    Slot *result = newSlot(false, text, ctx->totalSlots);
    UNPROTECT(save);
    return result;
}

static HashSymbol *claimSlotSymbol(EmitterContext *ctx) {
    Slot *resultSlot = NULL;
    HashSymbol *result = emit_removeFromHeap(ctx);
    if (result != NULL) {
        if (getSlotPool(ctx->slots, result, &resultSlot)) {
            resultSlot->isAvailable = false;
            ctx->activeSlots++;
            return result;
        } else {
            cant_happen("key \"%s\" from heap not in pool", result->name);
        }
    }
    // no available slots, create a new one
    result = genSym("tmp_");
    resultSlot = createNewSlot(ctx);
    int save = PROTECT(resultSlot);
    setSlotPool(ctx->slots, result, resultSlot);
    ctx->activeSlots++;
    ctx->totalSlots++;
    setMaxReg(ctx);
    UNPROTECT(save);
    return result;
}

static EmitResult *claimSlot(EmitterContext *ctx) {
    HashSymbol *symbol = claimSlotSymbol(ctx);
    return newEmitResult_Var(symbol);
}

static void releaseSlotSymbol(HashSymbol *temp, EmitterContext *ctx) {
    Slot *slot = NULL;
    if (getSlotPool(ctx->slots, temp, &slot)) {
        if (!slot->isAvailable) {
            ctx->activeSlots--;
            slot->isAvailable = true;
            emit_addToHeap(ctx, temp);
        }
    } else {
        cant_happen("slot not found");
    }
}

static void releaseSlot(EmitResult *result, EmitterContext *ctx) {
    if (isEmitResult_Var(result))
        releaseSlotSymbol(getEmitResult_Var(result), ctx);
}

static Slot *getSlot(HashSymbol *temp, EmitterContext *ctx) {
    Slot *slot = NULL;
    if (getSlotPool(ctx->slots, temp, &slot)) {
        return slot;
    } else {
        cant_happen("slot not found");
    }
}

static void releaseSlots(ResultArray *slots, EmitterContext *ctx) {
    for (Index i = 0; i < slots->size; i++) {
        releaseSlot(slots->entries[i], ctx);
    }
}

static bool slotsAvailableBelow(int N, EmitterContext *ctx) {
    if (N == 0)
        return false;
    if (ctx->totalSlots < N) // we've never allocated them
        return true;
    if (emit_peekHeap(ctx) < N)
        return true;
    return false;
}

static ResultArray *claimSlotsBelow(int N, EmitterContext *ctx) {
    fprintf(FH(ctx), "// ensuring reg[0..%d]\n", N - 1);
    ResultArray *slots = newResultArray();
    int save = PROTECT(slots);
    while (slotsAvailableBelow(N, ctx)) {
        EmitResult *slot = claimSlot(ctx);
        int save2 = PROTECT(slot);
        pushResultArray(slots, slot);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return slots;
}

static ResultArray *claimSlots(int N, EmitterContext *ctx) {
    ResultArray *slots = newResultArray();
    int save = PROTECT(slots);
    for (int i = 0; i < N; ++i) {
        EmitResult *slot = claimSlot(ctx);
        int save2 = PROTECT(slot);
        pushResultArray(slots, slot);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return slots;
}

static char *resultText(EmitResult *data, EmitterContext *ctx) {
    switch (data->type) {
    case EMITRESULT_TYPE_BUF:
        return opaqueEmitBufferContent(getEmitResult_Buf(data));
    case EMITRESULT_TYPE_CONSTANT:
        return opaqueEmitBufferContent(getEmitResult_Constant(data));
    case EMITRESULT_TYPE_VAR:
        return getSlot(getEmitResult_Var(data), ctx)->text->entries;
    default:
        cant_happen("unrecognised %s", emitResultTypeName(data->type));
    }
}

#ifdef SAFETY_CHECKS
static void assertNoLeakedSlots(EmitterContext *ctx, int line) {
    if (ctx->activeSlots != ctx->currentReg) {
        cant_happen("slot leak: active %d, expected %d line %d",
                    ctx->activeSlots, ctx->currentReg, line);
    }
}
#endif

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

static bool emitAtomic(MinExp *exp, EmitterContext *ctx) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
        emitMinAnnotatedVar(getMinExp_Avar(exp), ctx);
        return false;
    case MINEXP_TYPE_BIGINTEGER:
        emitMaybeBigInt(getMinExp_BigInteger(exp), ctx);
        return true;
    case MINEXP_TYPE_CHARACTER:
        fprintf(FH(ctx), "value_Character(%d)", (int)getMinExp_Character(exp));
        return true;
    case MINEXP_TYPE_STDINT:
        fprintf(FH(ctx), "value_Stdint(%d)", getMinExp_Stdint(exp));
        return true;
    case MINEXP_TYPE_PRIM: {
        bool isConst = true;
        MinPrimApp *prim = getMinExp_Prim(exp);
        if (prim->type == MINPRIMOP_TYPE_VEC) {
            fprintf(FH(ctx), "vec(");
            isConst = isConst && emitAtomic(prim->exp1, ctx);
            fprintf(FH(ctx), ", ");
            isConst = isConst && emitAtomic(prim->exp2, ctx);
            fprintf(FH(ctx), ")");
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

static EmitResult *emitNewAtomic(MinExp *exp, EmitterContext *ctx) {
    Opaque *buffer = newOpaque_EmitBuffer();
    int save = PROTECT(buffer);
    EmitterContext *atomicContext = extendContext(ctx, buffer);
    PROTECT(atomicContext);
    bool isConst = emitAtomic(exp, atomicContext);
    setProtectionStatus(ctx, atomicContext);
    EmitResult *result =
        isConst ? newEmitResult_Constant(buffer) : newEmitResult_Buf(buffer);
    UNPROTECT(save);
    return result;
}

static EmitResult *emitArg(MinExp *arg, EmitterContext *ctx) {
    if (isAtomic(arg)) {
        return emitNewAtomic(arg, ctx);
    } else {
        return emitSimpleExp(arg, ctx);
    }
}

static EmitResult *emitPrimOp(MinPrimApp *app, EmitterContext *ctx) {
    EmitResult *arg1 = emitArg(app->exp1, ctx);
    int save = PROTECT(arg1);
    EmitResult *arg2 = emitArg(app->exp2, ctx);
    PROTECT(arg2);
    // deliberate early release of any temps prior to claiming a new one
    // allows i.e. `reg[1] = op(reg[1], reg[2]);` as long as we don't emit
    // anything that could clobber the released slots before we are done
    // with them.
    releaseSlot(arg1, ctx);
    releaseSlot(arg2, ctx);
    EmitResult *target = claimSlot(ctx);
    PROTECT(target);
    char *opName = getPrimOpCName(app->type);
    int nargs = getPrimOpArity(app->type);
    // the first thing we emit consumes the released but not yet clobbered
    // slots.
    if (nargs == 2) {
        fprintf(FH(ctx), "%s = %s(%s, %s);\n", resultText(target, ctx), opName,
                resultText(arg1, ctx), resultText(arg2, ctx));
    } else {
        fprintf(FH(ctx), "%s = %s(%s);\n", resultText(target, ctx), opName,
                resultText(arg1, ctx));
    }
    UNPROTECT(save);
    return target; // the temp register that the caller can refer to
}

static EmitResult *emitAnonymousLambda(MinLam *lambda, EmitterContext *ctx) {
    EMITLOC("emitAnonymousLambda", lambda, ctx);
    // emit the lambda to the prelude, this then reduces to &&LABEL
    HashSymbol *name = genSym("anon");
    EmitterContext *lamContext = extendContextForLambda(name, ctx);
    int save = PROTECT(lamContext);
    SCharArray *containing_label = makeLambdaLabel(ctx->currentBinding);
    PROTECT(containing_label);
    fprintf(FH(lamContext), "// inside %s, %d args\n",
            containing_label->entries, countSymbolList(lambda->args));
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(ctx, lamContext);
    SCharArray *label = makeLambdaLabel(name);
    PROTECT(label);
    Opaque *buf = newOpaque_EmitBuffer();
    PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "value_Addr(&&%s)", label->entries);
    EmitResult *result = newEmitResult_Buf(buf);
    UNPROTECT(save);
    return result;
}

static void emitMaybeBigInt(MaybeBigInt *mbi, EmitterContext *ctx) {
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
        ctx->needsUnprotect = true;
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

static EmitResult *emitSimpleExp(MinExp *exp, EmitterContext *ctx) {
    switch (exp->type) {
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *app = getMinExp_Prim(exp);
        return emitPrimOp(app, ctx);
    }
    case MINEXP_TYPE_AVAR: {
        MinAnnotatedVar *avar = getMinExp_Avar(exp);
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        EmitterContext *aVarContext = extendContext(ctx, buf);
        setProtectionStatus(ctx, aVarContext);
        PROTECT(aVarContext);
        emitMinAnnotatedVar(avar, aVarContext);
        EmitResult *result = newEmitResult_Buf(buf);
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
        EmitterContext *biContext = extendContext(ctx, buf);
        PROTECT(biContext);
        emitMaybeBigInt(mbi, biContext);
        EmitResult *result = newEmitResult_Constant(buf);
        setProtectionStatus(ctx, biContext);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_STDINT: {
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        fprintf(opaqueEmitBufferFh(buf), "value_Stdint(%d)",
                getMinExp_Stdint(exp));
        EmitResult *result = newEmitResult_Constant(buf);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_CHARACTER: {
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        fprintf(opaqueEmitBufferFh(buf), "value_Character(%d)",
                (int)getMinExp_Character(exp));
        EmitResult *result = newEmitResult_Constant(buf);
        UNPROTECT(save);
        return result;
    }
    default:
        cant_happen("unhandled %s", minExpTypeName(exp->type));
    }
}

static EmitResult *emitConstant(char *text) {
    Opaque *buf = newOpaque_EmitBuffer();
    fprintf(opaqueEmitBufferFh(buf), "%s", text);
    int save = PROTECT(buf);
    EmitResult *target = newEmitResult_Constant(buf);
    UNPROTECT(save);
    return target;
}

// prefer to emit right to left because the common case for deeply nested
// vecs is lists which are right-associative, and by emitting r2l we need
// only one temp for the entire backbone of the list.
static ResultArray *emitRL(MinExprList *vecs, EmitterContext *ctx) {
    if (vecs == NULL)
        return newResultArray();
    ResultArray *results = emitRL(vecs->next, ctx);
    int save = PROTECT(results);
    EmitResult *result = emitArg(vecs->exp, ctx);
    PROTECT(result);
    pushResultArray(results, result);
    UNPROTECT(save);
    return results;
}

static void emitReverseResults(ResultArray *results, EmitterContext *ctx) {
    for (Index i = results->size; i > 0; i--) {
        fprintf(FH(ctx), ", %s", resultText(results->entries[i - 1], ctx));
    }
}

static EmitResult *emitMakeVec(MinExprList *vecs, EmitterContext *ctx) {
    ResultArray *results = emitRL(vecs, ctx);
    int save = PROTECT(results);
    int count = countMinExprList(vecs);
    releaseSlots(results, ctx);
    EmitResult *target = NULL;
    if (count == 0) {
        target = emitConstant("value_None()");
    } else {
        target = claimSlot(ctx);
        PROTECT(target);
        fprintf(FH(ctx), "%s = make_vec(%d", resultText(target, ctx), count);
        emitReverseResults(results, ctx);
        fprintf(FH(ctx), ");\n");
    }
    UNPROTECT(save);
    return target;
}

static void emitMinLam(MinLam *node, EmitterContext *ctx) {
#ifdef SAFETY_CHECKS
    if (node == NULL)
        cant_happen("NULL MinLam");
#endif
    int nargs = countSymbolList(node->args);
    fprintf(FH(ctx), "// emitMinLam reserving %d incoming slots\n", nargs);
    ResultArray *slots = claimSlots(nargs, ctx);
    int save = PROTECT(slots);
    ctx->currentReg += nargs;
    ASSERT_SLOTS(ctx);
    if (node->exp != NULL)
        emitMinExp(node->exp, ctx);
    releaseSlots(slots, ctx);
    ctx->currentReg -= nargs;
    UNPROTECT(save);
}

static void emitMinAnnotatedVar(MinAnnotatedVar *node, EmitterContext *ctx) {
    fprintf(FH(ctx), "reg[%d] /* %s */", node->position, node->var->name);
}

static inline void emitAssign(EmitResult *to, EmitResult *from,
                              EmitterContext *ctx) {
    fprintf(FH(ctx), "%s = %s;\n", resultText(to, ctx), resultText(from, ctx));
}

static inline void emitAssignReg(Index i, EmitResult *value,
                                 EmitterContext *ctx) {
    fprintf(FH(ctx), "reg[%d] = %s;\n", (int)i, resultText(value, ctx));
}

static void emitGoto(EmitResult *target, ResultArray *args,
                     EmitterContext *ctx) {
    fprintf(FH(ctx), "// emitGoto - %u outgoing slots\n", args->size);
    ResultArray *sourceSlots = newResultArray();
    int save = PROTECT(sourceSlots);
    ResultArray *claimedSlots = newResultArray();
    PROTECT(claimedSlots);

    for (Index i = 0; i < args->size; i++) {
        EmitResult *tempSlot = claimSlot(ctx);
        int save2 = PROTECT(tempSlot);
        EmitResult *arg = args->entries[i];
        if (isEmitResult_Buf(arg)) {
            emitAssign(tempSlot, arg, ctx);
            pushResultArray(sourceSlots, tempSlot);
        } else {
            // a var has already claimed a safe slot, use it directly for
            // copy-down, likewise if this is a constant expression.
            pushResultArray(sourceSlots, arg);
        }
        pushResultArray(claimedSlots, tempSlot);
        UNPROTECT(save2);
    }

    EmitResult *savedTarget = claimSlot(ctx);
    PROTECT(savedTarget);
    pushResultArray(claimedSlots, savedTarget);
    if (isEmitResult_Buf(target)) {
        emitAssign(savedTarget, target, ctx);
    } else {
        savedTarget = target;
    }

    // Copy-down to reg[0..N-1]
    for (Index i = 0; i < sourceSlots->size; i++) {
        emitAssignReg(i, sourceSlots->entries[i], ctx);
    }

    if (ctx->needsUnprotect) {
        fprintf(FH(ctx), "minlam_runtime_unprotect();\n");
        ctx->needsUnprotect = false;
    }

    fprintf(FH(ctx), "TRACE(\"%s\", getValue_Addr(%s), %d);\n",
            ctx->currentBinding->name, resultText(savedTarget, ctx),
            (int)args->size);
    fprintf(FH(ctx), "goto *getValue_Addr(%s);\n",
            resultText(savedTarget, ctx));
    releaseSlots(claimedSlots, ctx);
    UNPROTECT(save);
}

static EmitResult *emitExtractFromClosure(EmitResult *closure, int index,
                                          EmitterContext *ctx) {
    // this must never write to slots except this one
    EmitResult *result = claimSlot(ctx);
    int save = PROTECT(result);
    fprintf(FH(ctx), "%s = vec(value_Stdint(%d), %s);\n",
            resultText(result, ctx), index, resultText(closure, ctx));
    UNPROTECT(save);
    return result;
}

static EmitResult *emitCallBuiltin(BuiltIn *bi, MinExprList *builtinArgs,
                                   EmitterContext *ctx) {
    EmitResult *builtinArg = NULL;
    if (countBuiltInArgs(bi->args) == 0)
        builtinArg = emitConstant("make_vec(0)"); // just for builtins
    else
        builtinArg = emitMakeVec(builtinArgs, ctx);
    int save = PROTECT(builtinArg);
    EmitResult *builtinResult = claimSlot(ctx);
    PROTECT(builtinResult);
    fprintf(FH(ctx), "extern Value %s(Vec *);\n", bi->linkerName->name);
    fprintf(FH(ctx), "%s = %s(getValue_Vec(%s));\n",
            resultText(builtinResult, ctx), bi->linkerName->name,
            resultText(builtinArg, ctx));
    releaseSlot(builtinArg, ctx);
    UNPROTECT(save);
    return builtinResult;
}

// (builtin arg_1 ... arg_n k f)
// basically: "give the result of calling the builtin to k, passing f along"
// Where k itself is a closure (2-vec)
static void emitApplyBuiltin(MinApply *node, EmitterContext *ctx) {
    EMITLOC("emitApplyBuiltin", node, ctx);

    ResultArray *contArgSlots =
        claimSlotsBelow(3, ctx); // (3 args: env, result, f)
    int save = PROTECT(contArgSlots);

    BuiltIn *bi = findBuiltIn(node, ctx);
    int numBuiltinArgs = countBuiltInArgs(bi->args);

    // k and f
    MinExprList *continuations = dropMinExprList(node->args, numBuiltinArgs);

    // k
    EmitResult *k = emitSimpleExp(continuations->exp, ctx);
    PROTECT(k);

    ResultArray *targetArgs = newResultArray();
    PROTECT(targetArgs);

    // (env)
    EmitResult *env = emitExtractFromClosure(k, 1, ctx);
    PROTECT(env);
    pushResultArray(targetArgs, env);
    releaseSlot(k, ctx);

    // (env result)
    MinExprList *builtinArgs = takeMinExprList(node->args, numBuiltinArgs);
    PROTECT(builtinArgs);
    EmitResult *builtinResult = emitCallBuiltin(bi, builtinArgs, ctx);
    PROTECT(builtinResult);
    pushResultArray(targetArgs, builtinResult);

    continuations = continuations->next;

    // (env result f)
    EmitResult *f = emitSimpleExp(continuations->exp, ctx);
    PROTECT(f);
    pushResultArray(targetArgs, f);

    EmitResult *target = emitExtractFromClosure(k, 0, ctx);
    PROTECT(target);

    // (k env result f)
    emitGoto(target, targetArgs, ctx);
    releaseSlots(targetArgs, ctx);
    releaseSlots(contArgSlots, ctx);
    releaseSlot(target, ctx);
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

static void emitApplyClosure(MinApply *apply, EmitterContext *ctx) {
    EMITLOC("emitApplyClosure", apply, ctx);

    ResultArray *closureArgSlots =
        claimSlotsBelow(countMinExprList(apply->args) + 1, // +1 for closure env
                        ctx);
    int save = PROTECT(closureArgSlots);

    EmitResult *closure = emitArg(apply->function, ctx);
    PROTECT(closure);

    EmitResult *target = emitExtractFromClosure(closure, 0, ctx);
    PROTECT(target);
    // deliberate early release allows for behaviour like
    // tmp_1 = vec(..., tmp_1);
    releaseSlot(closure, ctx);

    EmitResult *env = emitExtractFromClosure(closure, 1, ctx);
    PROTECT(env);

    ResultArray *targetArgs = newResultArray();
    PROTECT(targetArgs);
    pushResultArray(targetArgs, env);

    for (MinExprList *arg = apply->args; arg != NULL; arg = arg->next) {
        EmitResult *result = emitSimpleExp(arg->exp, ctx);
        int save2 = PROTECT(result);
        pushResultArray(targetArgs, result);
        UNPROTECT(save2);
    }

    emitGoto(target, targetArgs, ctx);
    releaseSlot(target, ctx);
    releaseSlots(targetArgs, ctx);
    releaseSlots(closureArgSlots, ctx);
    UNPROTECT(save);
}

static void emitMinApply(MinApply *node, EmitterContext *ctx) {
    if (node->isBuiltin) {
        emitApplyBuiltin(node, ctx);
    } else {
        emitApplyClosure(node, ctx);
    }
}

static void emitMinIff(MinIff *node, EmitterContext *ctx) {
    EMITLOC("emitMinIff", node, ctx);
    EmitResult *test = emitSimpleExp(node->condition, ctx);
    int save = PROTECT(test);
    fprintf(FH(ctx), "    if (isTrue(%s)) {\n", resultText(test, ctx));
    releaseSlot(test, ctx);
    UNPROTECT(save);
    ASSERT_SLOTS(ctx);
    emitMinExp(node->consequent, ctx);
    fprintf(FH(ctx), "} else {\n");
    ASSERT_SLOTS(ctx);
    emitMinExp(node->alternative, ctx);
    fprintf(FH(ctx), "}\n");
}

static void emitMinCond(MinCond *node, EmitterContext *ctx) {
    EMITLOC("emitMinCond", node, ctx);
    EmitResult *cond = emitSimpleExp(node->value, ctx);
    if (node->cases->type == MINCONDCASES_TYPE_INTEGERS) {
        const char *condText = resultText(cond, ctx);
        releaseSlot(cond, ctx);
        emitMinIntCondCases(getMinCondCases_Integers(node->cases), condText,
                            ctx);
    } else {
        fprintf(FH(ctx), "switch(getValue_Character(%s)) {\n",
                resultText(cond, ctx));
        releaseSlot(cond, ctx);
        emitMinCondCases(node->cases, ctx);
        fprintf(FH(ctx), "}\n");
    }
}

static void emitMinCondCases(MinCondCases *node, EmitterContext *ctx) {
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
                                EmitterContext *ctx) {
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
    ASSERT_SLOTS(ctx);
    emitMinExp(node->body, ctx);
    if (node->next != NULL) {
        fprintf(FH(ctx), "} else ");
        emitMinIntCondCases(node->next, condText, ctx);
    } else {
        fprintf(FH(ctx), "}\n");
    }
}

static void emitMinCharCondCases(MinCharCondCases *node, EmitterContext *ctx) {
    EMITLOC("emitMinCharCondCases", node, ctx);
    if (node == NULL) {
        fprintf(FH(ctx), "default:\n");
        fprintf(FH(ctx),
                "fprintf(stderr, \"conditions exhausted in %s\\n\");\n",
                ctx->currentBinding->name);
        fprintf(FH(ctx), "abort();\n");
        return;
    }
    if (node->isDefault) {
        fprintf(FH(ctx), "default: \n");
        ASSERT_SLOTS(ctx);
        emitMinExp(node->body, ctx);
        fprintf(FH(ctx), "break;\n");
        return;
    }
    fprintf(FH(ctx), "case %d: \n", (int)(node->constant));
    ASSERT_SLOTS(ctx);
    emitMinExp(node->body, ctx);
    fprintf(FH(ctx), "break;\n");
    emitMinCharCondCases(node->next, ctx);
}

static void emitMinMatch(MinMatch *node, EmitterContext *ctx) {
    EMITLOC("emitMinMatch", node, ctx);
    EmitResult *match = emitSimpleExp(node->index, ctx);
    int save = PROTECT(match);
    fprintf(FH(ctx), "switch (getValue_Stdint(%s)) {\n",
            resultText(match, ctx));
    releaseSlot(match, ctx);
    UNPROTECT(save);
    emitMinMatchList(node->cases, ctx);
    fprintf(FH(ctx), "}\n");
}

static void emitMinMatchList(MinMatchList *node, EmitterContext *ctx) {
    if (node == NULL)
        return;
    EMITLOC("emitMinMatchList", node, ctx);
    MinIntList *match = node->matches;
    while (match != NULL) {
        fprintf(FH(ctx), "case %d:\n", match->item);
        match = match->next;
    }
    ASSERT_SLOTS(ctx);
    emitMinExp(node->body, ctx);
    fprintf(FH(ctx), "break;\n");
    emitMinMatchList(node->next, ctx);
}

// emitMinLetRec -> emitMinBindings -> emitMakeClosure
static void emitMakeClosure(HashSymbol *var, Integer depth,
                            ResultArray *bindings, EmitterContext *ctx) {
    fprintf(FH(ctx), "// emitMakeClosure %d\n", depth);
    SCharArray *label = makeLambdaLabel(var);
    int save = PROTECT(label);
    fprintf(FH(ctx), "%s = make_vec(2, value_Addr(&&%s), value_None());\n",
            resultText(bindings->entries[depth], ctx), label->entries);
    UNPROTECT(save);
}

static void emitBackpatchClosure(MinExprList *exprs, int depth,
                                 ResultArray *slots, EmitterContext *ctx) {
    if (exprs == NULL)
        return; // emitMakeClosure has already assigned the appropriate value
    fprintf(FH(ctx), "// emitBackpatchClosure %d\n", depth);
    EmitResult *tmp = emitMakeVec(exprs, ctx);
    int save = PROTECT(tmp);
    fprintf(FH(ctx), "getValue_Vec(%s)->entries[1] = %s;\n",
            resultText(slots->entries[depth], ctx), resultText(tmp, ctx));
    releaseSlot(tmp, ctx);
    UNPROTECT(save);
}

static void emitMinBindings(MinBindings *node, Integer depth,
                            ResultArray *bindings, EmitterContext *ctx) {
    if (node == NULL)
        return;
    // Use a unique name to avoid BufferBag hash collisions when the
    // same binding name (e.g. h1$0) appears in multiple letrecs.
    HashSymbol *uniqueName = genSym(node->var->name);
    EmitterContext *lamContext = extendContextForLambda(uniqueName, ctx);
    int save = PROTECT(lamContext);
    lamContext->currentBinding = uniqueName;
    MinLam *lambda = extractLambda(node->val);
    // emit the lambda body to the new memstream
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(ctx, lamContext);
    // but emit the closure creation to the current memstream
    emitMakeClosure(uniqueName, depth, bindings, ctx);
    UNPROTECT(save);
    emitMinBindings(node->next, depth + 1, bindings, ctx);
}

static void emitBackpatchBindings(MinBindings *node, Integer depth,
                                  ResultArray *bindings, EmitterContext *ctx) {
    if (node == NULL)
        return;
    MinExprList *makeEnv = extractEnv(node->val);
    emitBackpatchClosure(makeEnv, depth, bindings, ctx);
    emitBackpatchBindings(node->next, depth + 1, bindings, ctx);
}

static void emitMinLetRec(MinLetRec *node, EmitterContext *ctx) {
    EMITLOC("emitMinLetRec", node, ctx);
    ASSERT_SLOTS(ctx);
    int numBindings = countMinBindings(node->bindings);
    ResultArray *bindings = claimSlots(numBindings, ctx);
    int save = PROTECT(bindings);
    emitMinBindings(node->bindings, 0, bindings, ctx);
    emitBackpatchBindings(node->bindings, 0, bindings, ctx);
    ctx->currentReg += numBindings;
    ASSERT_SLOTS(ctx);
    emitMinExp(node->body, ctx);
    ctx->currentReg -= numBindings;
    releaseSlots(bindings, ctx);
    ASSERT_SLOTS(ctx);
    UNPROTECT(save);
}

static void emitMinExp(MinExp *node, EmitterContext *ctx) {
    switch (node->type) {
    case MINEXP_TYPE_APPLY: {
        MinApply *variant = getMinExp_Apply(node);
        emitMinApply(variant, ctx);
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
        fprintf(FH(ctx), "exit(0);");
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

//////////////
// Public API
//////////////

void emitProgram(MinExp *node, BuiltIns *builtIns, FILE *out) {
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    HashSymbol *main = newSymbol("main");
    SymbolArray *heap = emit_createHeap();
    PROTECT(heap);
    EmitterContext *ctx = newEmitterContext(main, body, builtIns, heap);
    REPLACE_PROTECT(save, ctx);
    fprintf(out, "// GENERATED CODE DO NOT EDIT\n");
    fprintf(out, "#include \"minlam_runtime.h\"\n");
    fprintf(out, "extern int forceGcFlag;\n");
    fprintf(out, "#ifdef TRACE_GOTO\n");
    fprintf(out, "#  define TRACE(from, target, nargs) "
                 "fprintf(stderr, \"%%s -> %%p (%%d args)\\n\", (from), (void "
                 "*)(target), (nargs))\n");
    fprintf(out, "#else\n");
    fprintf(out, "#  define TRACE(from, target, nargs)\n");
    fprintf(out, "#endif\n");
    emitMinExp(node, ctx);
    fprintf(out, "#define MAX_REG %d\n", ctx->maxReg);
    fprintf(out, "static Value reg[MAX_REG];\n");
    fprintf(out, "\n");

    fprintf(out, "int main(int argc, char *argv[]) {\n");
    fprintf(out, "for (int i = 0; i < MAX_REG; i++) {\n");
    fprintf(out, "reg[i] = value_None();\n");
    fprintf(out, "}\n");
    fprintf(out, "minlam_runtime_init(reg, MAX_REG, argc, argv);\n");
    fprintf(out, "// forceGcFlag = 1;\n");
    fprintf(out, "goto ENTRY;\n");
    Index i = 0;
    HashSymbol *label = NULL;
    Opaque *buf = NULL;
    i = 0;
    while ((label = iterateBufferBag(ctx->lambdas, &i, &buf)) != NULL) {
        SCharArray *l = makeLambdaLabel(label);
        fprintf(out, "%s:\n", l->entries);
        fprintf(out, "%s\n", opaqueEmitBufferContent(buf));
    }
    fprintf(out, "\nENTRY:\n");
    fprintf(out, "%s\n", opaqueEmitBufferContent(ctx->body));
    fprintf(out, "}\n");
    reportUnreleasedSlots(ctx);
}