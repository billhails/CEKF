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
static void emitMinExprList(MinExprList *, EmitterContext *);
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
static void emitMaybeBigInt(MaybeBigInt *mbi, EmitterContext *context);

/////////////////
// Debug Helpers
/////////////////

#ifdef SAFETY_CHECKS
static void assertNoLeakedSlots(EmitterContext *, int);
#define ASSERT_SLOTS(context) assertNoLeakedSlots(context, __LINE__)
#else
#define ASSERT_SLOTS(context)
#endif

#define EMITLOC(name, node, context)                                           \
    if (node == NULL || CPI(node).lineNo == 0)                                 \
        fprintf(FH(context), "// %s\n", name);                                 \
    else                                                                       \
        fprintf(FH(context), "// %s +%d %s\n", name, CPI(node).lineNo,         \
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

static inline FILE *FH(EmitterContext *context) {
    return opaqueEmitBufferFh(context->body);
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
                                              EmitterContext *context) {
    // create a new memstream for the lambda body
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    // add it to the global set of lambdas
    setBufferBag(context->lambdas, var, body);
    EmitterContext *new =
        newEmitterContext(context->currentBinding, body, context->builtIns);
    PROTECT(new);
    new->lambdas = context->lambdas;
    new->maxReg = context->maxReg;
    UNPROTECT(save);
    return new;
}

// only for atomics: sub-context must not allocate slots
static EmitterContext *extendContext(EmitterContext *context, Opaque *body) {
    EmitterContext *new =
        newEmitterContext(context->currentBinding, body, context->builtIns);
    int save = PROTECT(new);
    new->lambdas = context->lambdas;
    new->slots = context->slots;
    new->currentDepth = context->currentDepth;
    new->maxReg = context->maxReg;
    new->currentReg = context->currentReg;
    new->totalSlots = context->totalSlots;
    new->activeSlots = context->activeSlots;
    UNPROTECT(save);
    return new;
}

static inline void setMaxReg(EmitterContext *context) {
    context->maxReg = MAX(context->maxReg, context->currentDepth);
}

static inline void setProtectionStatus(EmitterContext *to,
                                       EmitterContext *from) {
    to->needsUnprotect = to->needsUnprotect || from->needsUnprotect;
}

static inline void incrDepth(EmitterContext *context) {
    context->currentDepth++;
    setMaxReg(context);
}

static inline void retrieveMaxReg(EmitterContext *to, EmitterContext *from) {
    to->maxReg = MAX(to->maxReg, from->maxReg);
}

static BuiltIn *findBuiltIn(MinApply *node, EmitterContext *context) {
    BuiltIn *bi = NULL;
    for (Index i = 0; i < context->builtIns->size; i++) {
        if (context->builtIns->entries[i]->internalName ==
            getMinExp_Var(node->function)) {
            bi = context->builtIns->entries[i];
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

static HashSymbol *claimSlotSymbol(EmitterContext *context) {
    HashSymbol *result = NULL;
    HashSymbol *key = NULL;
    Slot *slot = NULL;
    Slot *resultSlot = NULL;
    Index i = 0;
    while ((key = iterateSlotPool(context->slots, &i, &slot)) != NULL) {
        if (slot->isAvailable) {
            if (resultSlot == NULL || slot->index < resultSlot->index) {
                resultSlot = slot;
                result = key;
            }
        }
    }
    if (resultSlot != NULL) {
        resultSlot->isAvailable = false;
        context->activeSlots++;
        return result;
    }
    result = genSym("tmp_");
    SCharArray *text = newSCharArray();
    int save = PROTECT(text);
    char buf[64];
    sprintf(buf, "reg[%d]", context->currentDepth);
    for (char *c = buf; *c; c++) {
        pushSCharArray(text, *c);
    }
    pushSCharArray(text, '\0');
    slot = newSlot(false, text, context->currentDepth);
    incrDepth(context);
    PROTECT(slot);
    setSlotPool(context->slots, result, slot);
    context->activeSlots++;
    context->totalSlots++;
    UNPROTECT(save);
    return result;
}

static void reportUnreleasedSlots(EmitterContext *context) {
    HashSymbol *result = NULL;
    Slot *slot = NULL;
    Index i = 0;
    int count = 0;
    while ((result = iterateSlotPool(context->slots, &i, &slot)) != NULL) {
        if (!slot->isAvailable) {
            ++count;
        }
    }
    fprintf(stderr, "%d unreleased slots\n", count);
}

static EmitResult *claimSlot(EmitterContext *context) {
    HashSymbol *symbol = claimSlotSymbol(context);
    return newEmitResult_Var(symbol);
}

static void releaseSlotSymbol(HashSymbol *temp, EmitterContext *context) {
    Slot *slot = NULL;
    if (getSlotPool(context->slots, temp, &slot)) {
        if (!slot->isAvailable) {
            context->activeSlots--;
            slot->isAvailable = true;
        }
    } else {
        cant_happen("slot not found");
    }
}

static void releaseSlot(EmitResult *result, EmitterContext *context) {
    if (isEmitResult_Buf(result))
        return;
    releaseSlotSymbol(getEmitResult_Var(result), context);
}

static Slot *getSlot(HashSymbol *temp, EmitterContext *context) {
    Slot *slot = NULL;
    if (getSlotPool(context->slots, temp, &slot)) {
        return slot;
    } else {
        cant_happen("slot not found");
    }
}

static void releaseSlots(ResultArray *slots, EmitterContext *context) {
    for (Index i = 0; i < slots->size; i++) {
        releaseSlot(slots->entries[i], context);
    }
}

static bool slotsAvailableBelow(int N, EmitterContext *context) {
    HashSymbol *key = NULL;
    Slot *slot = NULL;
    Index i = 0;
    if (N == 0)
        return false;
    if (context->currentDepth < N) // we've never allocated them
        return true;
    while ((key = iterateSlotPool(context->slots, &i, &slot)) != NULL) {
        if (slot->isAvailable && slot->index < N) {
            return true;
        }
    }
    return false;
}

static ResultArray *claimSlotsBelow(int N, EmitterContext *context) {
    ResultArray *slots = newResultArray();
    int save = PROTECT(slots);
    while (slotsAvailableBelow(N, context)) {
        EmitResult *slot = claimSlot(context);
        int save2 = PROTECT(slot);
        pushResultArray(slots, slot);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return slots;
}

static ResultArray *claimSlots(int N, EmitterContext *context) {
    ResultArray *slots = newResultArray();
    int save = PROTECT(slots);
    for (int i = 0; i < N; ++i) {
        EmitResult *slot = claimSlot(context);
        int save2 = PROTECT(slot);
        pushResultArray(slots, slot);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return slots;
}

#ifdef SAFETY_CHECKS
static void assertNoLeakedSlots(EmitterContext *context, int line) {
    if (context->activeSlots != context->currentReg) {
        cant_happen("slot leak: active %d, expected %d line %d",
                    context->activeSlots, context->currentReg, line);
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

static int getPrimOpArity(MinPrimOp type) {
    switch (type) {
    case MINPRIMOP_TYPE_CANON:
        return 1;
    default:
        return 2;
    }
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

static void emitAtomic(MinExp *exp, EmitterContext *context) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
        emitMinAnnotatedVar(getMinExp_Avar(exp), context);
        break;
    case MINEXP_TYPE_BIGINTEGER:
        emitMaybeBigInt(getMinExp_BigInteger(exp), context);
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

static char *emitResultText(EmitResult *data, EmitterContext *context) {
    switch (data->type) {
    case EMITRESULT_TYPE_BUF:
        return opaqueEmitBufferContent(getEmitResult_Buf(data));
    case EMITRESULT_TYPE_VAR:
        return getSlot(getEmitResult_Var(data), context)->text->entries;
    default:
        cant_happen("unrecognised %s", emitResultTypeName(data->type));
    }
}

static EmitResult *emitNewAtomic(MinExp *exp, EmitterContext *context) {
    Opaque *buffer = newOpaque_EmitBuffer();
    int save = PROTECT(buffer);
    EmitterContext *atomicContext = extendContext(context, buffer);
    PROTECT(atomicContext);
    emitAtomic(exp, atomicContext);
    setProtectionStatus(context, atomicContext);
    EmitResult *result = newEmitResult_Buf(buffer);
    UNPROTECT(save);
    return result;
}

static EmitResult *emitArg(MinExp *arg, EmitterContext *context) {
    if (isAtomic(arg)) {
        return emitNewAtomic(arg, context);
    } else {
        return emitSimpleExp(arg, context);
    }
}

static EmitResult *emitPrimOp(MinPrimApp *app, EmitterContext *context) {
    EmitResult *arg1 = emitArg(app->exp1, context);
    int save = PROTECT(arg1);
    EmitResult *arg2 = emitArg(app->exp2, context);
    PROTECT(arg2);
    // deliberate early release of any temps prior to claiming a new one
    // allows i.e. `reg[1] = op(reg[1], reg[2]);` as long as we don't emit
    // anything that could clobber the released slots before we are done
    // with them.
    releaseSlot(arg1, context);
    releaseSlot(arg2, context);
    EmitResult *result = claimSlot(context);
    PROTECT(result);
    char *opName = getPrimOpName(app->type);
    int nargs = getPrimOpArity(app->type);
    // the first thing we emit consumes the released but not yet clobbered
    // slots.
    if (nargs == 2) {
        fprintf(FH(context), "%s = %s(%s, %s);\n",
                emitResultText(result, context), opName,
                emitResultText(arg1, context), emitResultText(arg2, context));
    } else {
        fprintf(FH(context), "%s = %s(%s);\n", emitResultText(result, context),
                opName, emitResultText(arg1, context));
    }
    UNPROTECT(save);
    return result; // the temp register that the caller can refer to
}

static EmitResult *emitAnonymousLambda(MinLam *lambda,
                                       EmitterContext *context) {
    EMITLOC("emitAnonymousLambda", lambda, context);
    // emit the lambda to the prelude, this then reduces to &&LABEL
    HashSymbol *name = genSym("anon");
    EmitterContext *lamContext = extendContextForLambda(name, context);
    int save = PROTECT(lamContext);
    SCharArray *containing_label = makeLambdaLabel(context->currentBinding);
    PROTECT(containing_label);
    fprintf(FH(lamContext), "// inside %s\n", containing_label->entries);
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(context, lamContext);
    SCharArray *label = makeLambdaLabel(name);
    PROTECT(label);
    Opaque *buf = newOpaque_EmitBuffer();
    PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "value_Addr(&&%s)", label->entries);
    EmitResult *result = newEmitResult_Buf(buf);
    UNPROTECT(save);
    return result;
}

static void emitMaybeBigInt(MaybeBigInt *mbi, EmitterContext *context) {
    switch (mbi->type) {
    case BI_BIG: {
        if (mbi->imag) {
            fprintf(FH(context), "value_Bigint_imag(");
        } else {
            fprintf(FH(context), "value_Bigint(");
        }
        bigint bi = mbi->big;
        fprintf(FH(context), "minlam_runtime_BigInt(%d, %d, %d", bi.size,
                bi.capacity, bi.neg);
        for (int i = 0; i < bi.capacity; i++) {
            fprintf(FH(context), ", %u", bi.words[i]);
        }
        fprintf(FH(context), "))");
        // minlam_runtime_BigInt will have done a PROTECT that needs restoring
        context->needsUnprotect = true;
        break;
    }
    case BI_SMALL: {
        if (mbi->imag) {
            fprintf(FH(context), "value_Stdint_imag(%d)", mbi->small);
        } else {
            fprintf(FH(context), "value_Stdint(%d)", mbi->small);
        }
        break;
    }
    case BI_IRRATIONAL: {
        if (mbi->imag) {
            if (isnan(mbi->irrational)) {
                fprintf(FH(context), "value_Irrational_imag(NAN)");
            } else {
                fprintf(FH(context), "value_Irrational_imag(%f)",
                        mbi->irrational);
            }
        } else {
            if (isnan(mbi->irrational)) {
                fprintf(FH(context), "value_Irrational(NAN)");
            } else {
                fprintf(FH(context), "value_Irrational(%f)", mbi->irrational);
            }
        }
        break;
    }
    default:
        cant_happen("unhandled numeric type %d", mbi->type);
    }
}

static EmitResult *emitSimpleExp(MinExp *exp, EmitterContext *context) {
    EMITLOC("emitSimpleExp", exp, context);
    switch (exp->type) {
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *app = getMinExp_Prim(exp);
        return emitPrimOp(app, context);
    }
    case MINEXP_TYPE_AVAR: {
        MinAnnotatedVar *avar = getMinExp_Avar(exp);
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        EmitterContext *aVarContext = extendContext(context, buf);
        setProtectionStatus(context, aVarContext);
        PROTECT(aVarContext);
        emitMinAnnotatedVar(avar, aVarContext);
        EmitResult *result = newEmitResult_Buf(buf);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_MAKEVEC: {
        MinExprList *vecs = getMinExp_MakeVec(exp);
        return emitMakeVec(vecs, context);
    }
    case MINEXP_TYPE_LAM: {
        MinLam *lambda = getMinExp_Lam(exp);
        return emitAnonymousLambda(lambda, context);
    }
    case MINEXP_TYPE_BIGINTEGER: {
        MaybeBigInt *mbi = getMinExp_BigInteger(exp);
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        EmitterContext *biContext = extendContext(context, buf);
        PROTECT(biContext);
        emitMaybeBigInt(mbi, biContext);
        EmitResult *result = newEmitResult_Buf(buf);
        setProtectionStatus(context, biContext);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_STDINT: {
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        fprintf(opaqueEmitBufferFh(buf), "value_Stdint(%d)",
                getMinExp_Stdint(exp));
        EmitResult *result = newEmitResult_Buf(buf);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_CHARACTER: {
        Opaque *buf = newOpaque_EmitBuffer();
        int save = PROTECT(buf);
        fprintf(opaqueEmitBufferFh(buf), "value_Character(%d)",
                (int)getMinExp_Character(exp));
        EmitResult *result = newEmitResult_Buf(buf);
        UNPROTECT(save);
        return result;
    }
    default:
        cant_happen("unhandled %s", minExpTypeName(exp->type));
    }
}

static EmitResult *emitZeroVec(EmitterContext *context) {
    EmitResult *target = claimSlot(context);
    int save = PROTECT(target);
    fprintf(FH(context), "%s = make_vec(0);\n",
            emitResultText(target, context));
    UNPROTECT(save);
    return target;
}

static EmitResult *emitMakeVec(MinExprList *vecs, EmitterContext *context) {
    EMITLOC("emitMakeVec", vecs, context);
    ResultArray *results = newResultArray();
    int save = PROTECT(results);
    int count = countMinExprList(vecs);
    MinExprList *v = vecs;
    while (v != NULL) {
        EmitResult *result = emitArg(v->exp, context);
        int save = PROTECT(result);
        pushResultArray(results, result);
        UNPROTECT(save);
        v = v->next;
    }
    for (Index i = 0; i < results->size; i++) {
        releaseSlot(results->entries[i], context);
    }
    EmitResult *target = claimSlot(context);
    PROTECT(target);
    if (count == 0) {
        fprintf(FH(context), "%s = value_None();",
                emitResultText(target, context));
    } else {
        fprintf(FH(context), "%s = make_vec(%d",
                emitResultText(target, context), count);
        for (Index i = 0; i < results->size; i++) {
            EmitResult *result = results->entries[i];
            fprintf(FH(context), ", %s", emitResultText(result, context));
        }
        fprintf(FH(context), ");\n");
    }
    UNPROTECT(save);
    return target;
}

static void emitMinLam(MinLam *node, EmitterContext *context) {
    EMITLOC("emitMinLam", node, context);
#ifdef SAFETY_CHECKS
    if (node == NULL)
        cant_happen("NULL MinLam");
#endif
    int nargs = countSymbolList(node->args);
    ResultArray *slots = claimSlots(nargs, context);
    int save = PROTECT(slots);
    context->currentReg += nargs;
    ASSERT_SLOTS(context);
    if (node->exp != NULL)
        emitMinExp(node->exp, context);
    releaseSlots(slots, context);
    context->currentReg -= nargs;
    UNPROTECT(save);
}

static void emitMinAnnotatedVar(MinAnnotatedVar *node,
                                EmitterContext *context) {
    fprintf(FH(context), "reg[%d] /* %s */", node->position, node->var->name);
}

static void emitMinExprList(MinExprList *node, EmitterContext *context) {
    if (node == NULL)
        return;
    EMITLOC("emitMinExprList", node, context);
    emitMinExp(node->exp, context);
    emitMinExprList(node->next, context);
}

static inline void emitAssign(EmitResult *to, EmitResult *from,
                              EmitterContext *context) {
    fprintf(FH(context), "%s = %s;\n", emitResultText(to, context),
            emitResultText(from, context));
}

static inline void emitAssignReg(Index i, EmitResult *value,
                                 EmitterContext *context) {
    fprintf(FH(context), "reg[%d] = %s;\n", (int)i,
            emitResultText(value, context));
}

static void emitGoto(EmitResult *target, ResultArray *args,
                     EmitterContext *context) {
    fprintf(FH(context), "// emitGoto\n");
    ResultArray *sourceSlots = newResultArray();
    int save = PROTECT(sourceSlots);
    ResultArray *claimedSlots = newResultArray();
    PROTECT(claimedSlots);

    for (Index i = 0; i < args->size; i++) {
        EmitResult *tempSlot = claimSlot(context);
        int save2 = PROTECT(tempSlot);
        if (isEmitResult_Buf(args->entries[i])) {
            emitAssign(tempSlot, args->entries[i], context);
            pushResultArray(sourceSlots, tempSlot);
        } else {
            // a var has already claimed a safe slot, use it directly for
            // copy-down
            pushResultArray(sourceSlots, args->entries[i]);
        }
        pushResultArray(claimedSlots, tempSlot);
        UNPROTECT(save2);
    }

    EmitResult *savedTarget = claimSlot(context);
    PROTECT(savedTarget);
    pushResultArray(claimedSlots, savedTarget);
    if (isEmitResult_Buf(target)) {
        emitAssign(savedTarget, target, context);
    } else {
        savedTarget = target;
    }

    // Copy down to reg[0..N-1]
    for (Index i = 0; i < sourceSlots->size; i++) {
        emitAssignReg(i, sourceSlots->entries[i], context);
    }

    if (context->needsUnprotect) {
        fprintf(FH(context), "minlam_runtime_unprotect();\n");
        context->needsUnprotect = false;
    }

    fprintf(FH(context), "TRACE(\"%s\", getValue_Addr(%s), %d);\n",
            context->currentBinding->name, emitResultText(savedTarget, context),
            (int)args->size);
    fprintf(FH(context), "goto *getValue_Addr(%s);\n",
            emitResultText(savedTarget, context));
    releaseSlots(claimedSlots, context);
    UNPROTECT(save);
}

static EmitResult *emitExtractFromClosure(EmitResult *closure, int index,
                                          EmitterContext *context) {
    // this must never write to slots except this one
    EmitResult *result = claimSlot(context);
    int save = PROTECT(result);
    fprintf(FH(context), "%s = vec(value_Stdint(%d), %s);\n",
            emitResultText(result, context), index,
            emitResultText(closure, context));
    UNPROTECT(save);
    return result;
}

static EmitResult *emitCallBuiltin(BuiltIn *bi, MinExprList *builtinArgs,
                                   EmitterContext *context) {
    EmitResult *builtinArg = NULL;
    if (countBuiltInArgs(bi->args) == 0)
        builtinArg = emitZeroVec(context); // just for builtins
    else
        builtinArg = emitMakeVec(builtinArgs, context);
    int save = PROTECT(builtinArg);
    EmitResult *builtinResult = claimSlot(context);
    PROTECT(builtinResult);
    fprintf(FH(context), "extern Value %s(Vec *);\n", bi->linkerName->name);
    fprintf(FH(context), "%s = %s(getValue_Vec(%s));\n",
            emitResultText(builtinResult, context), bi->linkerName->name,
            emitResultText(builtinArg, context));
    releaseSlot(builtinArg, context);
    UNPROTECT(save);
    return builtinResult;
}

// (builtin arg_1 ... arg_n k f)
// basically: "give the result of calling the builtin to k, passing f along"
// Where k itself is a closure (2-vec)
static void emitApplyBuiltin(MinApply *node, EmitterContext *context) {
    EMITLOC("emitApplyBuiltin", node, context);

    ResultArray *contArgSlots = claimSlotsBelow(3, context); // (env result f)
    int save = PROTECT(contArgSlots);

    BuiltIn *bi = findBuiltIn(node, context);
    int numBuiltinArgs = countBuiltInArgs(bi->args);

    // k and f
    MinExprList *continuations = dropMinExprList(node->args, numBuiltinArgs);

    // k
    EmitResult *k = emitSimpleExp(continuations->exp, context);
    PROTECT(k);

    ResultArray *targetArgs = newResultArray();
    PROTECT(targetArgs);

    // (env)
    EmitResult *env = emitExtractFromClosure(k, 1, context);
    PROTECT(env);
    pushResultArray(targetArgs, env);
    releaseSlot(k, context);

    // (env result)
    MinExprList *builtinArgs = takeMinExprList(node->args, numBuiltinArgs);
    PROTECT(builtinArgs);
    EmitResult *builtinResult = emitCallBuiltin(bi, builtinArgs, context);
    PROTECT(builtinResult);
    pushResultArray(targetArgs, builtinResult);

    continuations = continuations->next;

    // (env result f)
    EmitResult *f = emitSimpleExp(continuations->exp, context);
    PROTECT(f);
    pushResultArray(targetArgs, f);

    EmitResult *target = emitExtractFromClosure(k, 0, context);
    PROTECT(target);

    // (k env result f)
    emitGoto(target, targetArgs, context);
    releaseSlots(targetArgs, context);
    releaseSlots(contArgSlots, context);
    releaseSlot(target, context);
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

static void emitApplyClosure(MinApply *apply, EmitterContext *context) {
    EMITLOC("emitApplyClosure", apply, context);

    ResultArray *closureArgSlots =
        claimSlotsBelow(countMinExprList(apply->args) + 1, // +1 for closure env
                        context);
    int save = PROTECT(closureArgSlots);

    EmitResult *closure = emitArg(apply->function, context);
    PROTECT(closure);

    EmitResult *target = emitExtractFromClosure(closure, 0, context);
    PROTECT(target);
    // deliberate early release allows for behaviour like
    // tmp_1 = vec(..., tmp_1);
    releaseSlot(closure, context);

    EmitResult *env = emitExtractFromClosure(closure, 1, context);
    PROTECT(env);

    ResultArray *targetArgs = newResultArray();
    PROTECT(targetArgs);
    pushResultArray(targetArgs, env);

    for (MinExprList *arg = apply->args; arg != NULL; arg = arg->next) {
        EmitResult *result = emitSimpleExp(arg->exp, context);
        int save2 = PROTECT(result);
        pushResultArray(targetArgs, result);
        UNPROTECT(save2);
    }

    emitGoto(target, targetArgs, context);
    releaseSlot(target, context);
    releaseSlots(targetArgs, context);
    releaseSlots(closureArgSlots, context);
    UNPROTECT(save);
}

static void emitMinApply(MinApply *node, EmitterContext *context) {
    if (node->isBuiltin) {
        emitApplyBuiltin(node, context);
    } else {
        emitApplyClosure(node, context);
    }
}

static void emitMinIff(MinIff *node, EmitterContext *context) {
    EMITLOC("emitMinIff", node, context);
    EmitResult *test = emitSimpleExp(node->condition, context);
    int save = PROTECT(test);
    fprintf(FH(context), "    if (isTrue(%s)) {\n",
            emitResultText(test, context));
    releaseSlot(test, context);
    UNPROTECT(save);
    ASSERT_SLOTS(context);
    emitMinExp(node->consequent, context);
    fprintf(FH(context), "} else {\n");
    ASSERT_SLOTS(context);
    emitMinExp(node->alternative, context);
    fprintf(FH(context), "}\n");
}

static void emitMinCond(MinCond *node, EmitterContext *context) {
    EMITLOC("emitMinCond", node, context);
    EmitResult *cond = emitSimpleExp(node->value, context);
    if (node->cases->type == MINCONDCASES_TYPE_INTEGERS) {
        const char *condText = emitResultText(cond, context);
        releaseSlot(cond, context);
        emitMinIntCondCases(getMinCondCases_Integers(node->cases), condText,
                            context);
    } else {
        fprintf(FH(context), "switch(getValue_Character(%s)) {\n",
                emitResultText(cond, context));
        releaseSlot(cond, context);
        emitMinCondCases(node->cases, context);
        fprintf(FH(context), "}\n");
    }
}

static void emitMinCondCases(MinCondCases *node, EmitterContext *context) {
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        cant_happen("integer cond cases handled in emitMinCond");
        break;
    case MINCONDCASES_TYPE_CHARACTERS: {
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        emitMinCharCondCases(variant, context);
        break;
    }
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
}

static void emitMinIntCondCases(MinIntCondCases *node, const char *condText,
                                EmitterContext *context) {
    if (node == NULL) {
        return;
    }
    EMITLOC("emitMinIntCondCases", node, context);
    if (node->constant == NULL) {
        // default case
        fprintf(FH(context), "{ // default\n");
    } else {
        fprintf(FH(context), "if (minlam_runtime_cmp(%s, ", condText);
        emitMaybeBigInt(node->constant, context);
        fprintf(FH(context), ") == CMP_EQ) {\n");
    }
    ASSERT_SLOTS(context);
    emitMinExp(node->body, context);
    if (node->next != NULL) {
        fprintf(FH(context), "} else ");
        emitMinIntCondCases(node->next, condText, context);
    } else {
        fprintf(FH(context), "}\n");
    }
}

static void emitMinCharCondCases(MinCharCondCases *node,
                                 EmitterContext *context) {
    EMITLOC("emitMinCharCondCases", node, context);
    if (node == NULL) {
        fprintf(FH(context), "default:\n");
        fprintf(FH(context),
                "fprintf(stderr, \"conditions exhausted in %s\\n\");\n",
                context->currentBinding->name);
        fprintf(FH(context), "abort();\n");
        return;
    }
    if (node->isDefault) {
        fprintf(FH(context), "default: \n");
        ASSERT_SLOTS(context);
        emitMinExp(node->body, context);
        fprintf(FH(context), "break;\n");
        return;
    }
    fprintf(FH(context), "case %d: \n", (int)(node->constant));
    ASSERT_SLOTS(context);
    emitMinExp(node->body, context);
    fprintf(FH(context), "break;\n");
    emitMinCharCondCases(node->next, context);
}

static void emitMinMatch(MinMatch *node, EmitterContext *context) {
    EMITLOC("emitMinMatch", node, context);
    EmitResult *match = emitSimpleExp(node->index, context);
    int save = PROTECT(match);
    fprintf(FH(context), "switch (getValue_Stdint(%s)) {\n",
            emitResultText(match, context));
    releaseSlot(match, context);
    UNPROTECT(save);
    emitMinMatchList(node->cases, context);
    fprintf(FH(context), "}\n");
}

static void emitMinMatchList(MinMatchList *node, EmitterContext *context) {
    if (node == NULL)
        return;
    EMITLOC("emitMinMatchList", node, context);
    MinIntList *match = node->matches;
    while (match != NULL) {
        fprintf(FH(context), "case %d:\n", match->item);
        match = match->next;
    }
    ASSERT_SLOTS(context);
    emitMinExp(node->body, context);
    fprintf(FH(context), "break;\n");
    emitMinMatchList(node->next, context);
}

// emitMinLetRec -> emitMinBindings -> emitMakeClosure
static void emitMakeClosure(HashSymbol *var, Integer depth,
                            ResultArray *bindings, EmitterContext *context) {
    fprintf(FH(context), "// emitMakeClosure %d\n", depth);
    SCharArray *label = makeLambdaLabel(var);
    int save = PROTECT(label);
    fprintf(FH(context), "%s = make_vec(2, value_Addr(&&%s), value_None());\n",
            emitResultText(bindings->entries[depth], context), label->entries);
    UNPROTECT(save);
}

static void emitBackpatchClosure(MinExprList *exprs, int depth,
                                 ResultArray *slots, EmitterContext *context) {
    fprintf(FH(context), "// emitBackpatchClosure %d\n", depth);
    EmitResult *tmp = emitMakeVec(exprs, context);
    int save = PROTECT(tmp);
    fprintf(FH(context), "getValue_Vec(%s)->entries[1] = %s;\n",
            emitResultText(slots->entries[depth], context),
            emitResultText(tmp, context));
    releaseSlot(tmp, context);
    UNPROTECT(save);
}

static void emitMinBindings(MinBindings *node, Integer depth,
                            ResultArray *bindings, EmitterContext *context) {
    if (node == NULL)
        return;
    EMITLOC("emitMinBindings", node, context);
    // Use a unique name to avoid BufferBag hash collisions when the
    // same binding name (e.g. h1$0) appears in multiple letrecs.
    HashSymbol *uniqueName = genSym(node->var->name);
    EmitterContext *lamContext = extendContextForLambda(uniqueName, context);
    int save = PROTECT(lamContext);
    lamContext->currentBinding = uniqueName;
    MinLam *lambda = extractLambda(node->val);
    // emit the lambda body to the new memstream
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(context, lamContext);
    // but emit the closure creation to the current memstream
    emitMakeClosure(uniqueName, depth, bindings, context);
    UNPROTECT(save);
    emitMinBindings(node->next, depth + 1, bindings, context);
}

static void emitBackpatchBindings(MinBindings *node, Integer depth,
                                  ResultArray *bindings,
                                  EmitterContext *context) {
    if (node == NULL)
        return;
    EMITLOC("emitBackpatchBindings", node, context);
    MinExprList *makeEnv = extractEnv(node->val);
    emitBackpatchClosure(makeEnv, depth, bindings, context);
    emitBackpatchBindings(node->next, depth + 1, bindings, context);
}

static void emitMinLetRec(MinLetRec *node, EmitterContext *context) {
    EMITLOC("emitMinLetRec", node, context);
    ASSERT_SLOTS(context);
    int numBindings = countMinBindings(node->bindings);
    ResultArray *bindings = claimSlots(numBindings, context);
    int save = PROTECT(bindings);
    emitMinBindings(node->bindings, 0, bindings, context);
    emitBackpatchBindings(node->bindings, 0, bindings, context);
    context->currentReg += numBindings;
    ASSERT_SLOTS(context);
    emitMinExp(node->body, context);
    context->currentReg -= numBindings;
    releaseSlots(bindings, context);
    ASSERT_SLOTS(context);
    UNPROTECT(save);
}

static void emitMinExp(MinExp *node, EmitterContext *context) {
    EMITLOC("emitMinExp", node, context);
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
        // bare variable in tail position is dead code (CPS artifact)
        fprintf(FH(context), "// dead tail: ");
        emitMinAnnotatedVar(getMinExp_Avar(node), context);
        fprintf(FH(context), ";\n");
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        emitMaybeBigInt(getMinExp_BigInteger(node), context);
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
        cant_happen("non-simple makevec");
        break;
    }
    case MINEXP_TYPE_MATCH: {
        MinMatch *variant = getMinExp_Match(node);
        emitMinMatch(variant, context);
        break;
    }
    case MINEXP_TYPE_PRIM: {
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
    HashSymbol *main = newSymbol("main");
    EmitterContext *context = newEmitterContext(main, body, builtIns);
    REPLACE_PROTECT(save, context);
    fprintf(out, "// CODE GENERATED DO NOT EDIT\n");
    fprintf(out, "#include \"minlam_runtime.h\"\n");
    fprintf(out, "extern int forceGcFlag;\n");
    fprintf(out, "#ifdef TRACE_GOTO\n");
    fprintf(out, "#  define TRACE(from, target, nargs) "
                 "fprintf(stderr, \"%%s -> %%p (%%d args)\\n\", (from), (void "
                 "*)(target), (nargs))\n");
    fprintf(out, "#else\n");
    fprintf(out, "#  define TRACE(from, target, nargs)\n");
    fprintf(out, "#endif\n");
    emitMinExp(node, context);
    fprintf(out, "#define MAX_REG %d\n", context->maxReg);
    fprintf(out, "static Value reg[MAX_REG];\n");
    fprintf(out, "\n");

    fprintf(out, "int main(int argc, char *argv[]) {\n");
    fprintf(out, "for (int i = 0; i < MAX_REG; i++) {\n");
    fprintf(out, "reg[i] = value_None();\n");
    fprintf(out, "}\n");
    fprintf(out, "minlam_runtime_init(reg, MAX_REG, argc, argv);\n");
    fprintf(out, "goto ENTRY;\n");
    Index i = 0;
    HashSymbol *label = NULL;
    Opaque *buf = NULL;
    i = 0;
    while ((label = iterateBufferBag(context->lambdas, &i, &buf)) != NULL) {
        SCharArray *l = makeLambdaLabel(label);
        fprintf(out, "%s:\n", l->entries);
        fprintf(out, "%s\n", opaqueEmitBufferContent(buf));
    }
    fprintf(out, "\nENTRY:\n");
    fprintf(out, "%s\n", opaqueEmitBufferContent(context->body));
    fprintf(out, "}\n");
    reportUnreleasedSlots(context);
}