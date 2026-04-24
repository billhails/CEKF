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

#include "minlam_emit_c.h"
#include "memory.h"
#include "minlam.h"
#include "minlam_pp.h"
#include "symbol.h"
#include "utils_helper.h"
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <sys/param.h>

///////////////////////
// Emit Buffer Helpers
///////////////////////

typedef CEmitterContext EC;
typedef EmitCResult ER;
typedef CResultArray RA;
typedef CResultMap RM;

typedef struct EmitBuffer {
    FILE *fh;
    char *buffer;
    size_t size;
} EmitBuffer;

static bool isConst(MinExp *);
static bool isAtomic(MinExp *);
static char *getEmitBuffer(EmitBuffer *);
static char *resultText(ER *, EC *);
static char *getPrimOpCName(MinPrimOp);
static EmitBuffer *newEmitBuffer(void);
static ER *emitConstant(char *);
static ER *emitNewAtomic(MinExp *, EC *);
static int getPrimOpArity(MinPrimOp);
static void cleanEmitBuffer(void *);
static void emitAtomic(MinExp *, EC *);
static void emitAssignPrimOp1(ER *, char *, ER *, EC *);
static void emitAssignPrimOp2(ER *, char *, ER *, ER *, EC *);
static void emitMinCharCondCases(MinCharCondCases *, EC *);
static void emitMinCondCases(MinCondCases *, EC *);
static void emitMinIntCondCases(MinIntCondCases *, ER *, EC *);
static void emitMinMatchList(MinMatchList *, EC *);
static void emitStdint(Integer, EC *);
static void emitVec(MinExp *, MinExp *, EC *);
static void printEmitBuffer(FILE *, void *);
static inline Opaque *newOpaque_EmitBuffer();
static inline char *opaqueEmitBufferContent(Opaque *container);
static inline FILE *opaqueEmitBufferFh(Opaque *container);
static inline FILE *FH(EC *ctx);
static void emitMinAnnotatedVar(MinAnnotatedVar *node, EC *ctx);
static void emitCharacter(Character character, EC *ctx);

#include "minlam_emit_contract.h"

// also used by minlam_emit.inc:
#define EMITLOC(name, node, ctx)                                               \
    if (node == NULL || CPI(node).lineNo == 0)                                 \
        fprintf(FH(ctx), "// %s\n", name);                                     \
    else                                                                       \
        fprintf(FH(ctx), "// %s +%d %s\n", name, CPI(node).lineNo,             \
                CPI(node).fileName)

static inline RA *newRA() { return newCResultArray(); }

static inline void pushRA(RA *ra, ER *r) { pushCResultArray(ra, r); }

static inline ER *erForSlot(Index i, EC *ctx) {
    HashSymbol *symbol = symbolForSlot(i, &ctx->context);
    return newEmitCResult_Var(symbol);
}

__attribute__((unused)) static inline RM *newRM() { return newCResultMap(); }

static inline void setRM(RM *map, HashSymbol *k, ER *r) {
    setCResultMap(map, k, r);
}

__attribute__((unused)) static inline bool getRM(RM *map, HashSymbol *k,
                                                 ER **r) {
    return getCResultMap(map, k, r);
}

__attribute__((unused)) static inline HashSymbol *iterateRM(RM *m, Index *i,
                                                            ER **r) {
    return iterateCResultMap(m, i, r);
}

static HashSymbol *tokenForER(ER *er, EC *ctx __attribute__((unused))) {
    if (isEmitCResult_Var(er)) {
        return getEmitCResult_Var(er);
    } else {
        return NULL;
    }
}

static inline ER *newResultSlotSymbol(HashSymbol *s) {
    return newEmitCResult_Var(s);
}
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

static inline FILE *FH(EC *ctx) { return opaqueEmitBufferFh(ctx->body); }

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

static char *resultText(ER *data, EC *ctx) {
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

static bool isConst(MinExp *exp) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
        return false;
    case MINEXP_TYPE_BIGINTEGER:
        return true;
    case MINEXP_TYPE_CHARACTER:
        return true;
    case MINEXP_TYPE_STDINT:
        return true;
    case MINEXP_TYPE_PRIM: {
        bool res = true;
        MinPrimApp *prim = getMinExp_Prim(exp);
        if (prim->type == MINPRIMOP_TYPE_VEC) {
            res = isConst(prim->exp1) && isConst(prim->exp2);
        } else {
            cant_happen("isConst passed non-atomic %s",
                        minExpTypeName(exp->type));
        }
        return res;
    }
    default:
        cant_happen("isConst passed non-atomic %s", minExpTypeName(exp->type));
    }
}

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

///////////////////
// Context Helpers
///////////////////

static EC *extendContextForLambda(HashSymbol *var, EC *ctx) {
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
    context.slotSymbols = newSymbolArray();
    PROTECT(context.slotSymbols);
    context.activeSlots = 0;
    context.totalSlots = 0;
    context.currentReg = 0;
    context.needsUnprotect = false;
    EC *new = newCEmitterContext(body, context);
    PROTECT(new);
    new->lambdas = ctx->lambdas;
    UNPROTECT(save);
    return new;
}

// only for atomics: sub-context must not allocate slots
static EC *extendContext(EC *ctx) {
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    EC *new = newCEmitterContext(body, ctx->context);
    PROTECT(new);
    new->lambdas = ctx->lambdas;
    UNPROTECT(save);
    return new;
}

//////////////////
// Result Helpers
//////////////////

static inline bool resultIsVar(ER *result) { return isEmitCResult_Var(result); }
static inline HashSymbol *getResultSlotSymbol(ER *result) {
    return getEmitCResult_Var(result);
}
static inline bool resultNeedsMaterialization(ER *result) {
    return isEmitCResult_Buf(result);
}

/////////////////
// Leaf Emitters
/////////////////

static void emitDone(int status, EC *ctx) {
    fprintf(FH(ctx), "exit(%d);\n", status);
}

static void emitClosureSetEnv(ER *closure, ER *env, EC *ctx) {
    fprintf(FH(ctx), "getValue_Vec(%s)->entries[1] = %s;\n",
            resultText(closure, ctx), resultText(env, ctx));
}

static void emitClosureNew(ER *target, HashSymbol *label, EC *ctx) {
    fprintf(FH(ctx), "%s = make_vec(2, value_Addr(&&%s), value_None());\n",
            resultText(target, ctx), label->name);
}

static void emitIfThenElse(ER *test, MinExp *con, MinExp *alt, EC *ctx) {
    fprintf(FH(ctx), "if (isTrue(%s)) {\n", resultText(test, ctx));
    emitMinExp(con, ctx);
    fprintf(FH(ctx), "} else {\n");
    emitMinExp(alt, ctx);
    fprintf(FH(ctx), "}\n");
}

static void emitCallBuiltin(BuiltIn *bi, ER *target, ER *arg, EC *ctx) {
    fprintf(FH(ctx), "extern Value %s(Vec *);\n", bi->linkerName->name);
    fprintf(FH(ctx), "%s = %s(getValue_Vec(%s));\n", resultText(target, ctx),
            bi->linkerName->name, resultText(arg, ctx));
}

static void emitVecGetImm(ER *target, ER *closure, int index, EC *ctx) {
    fprintf(FH(ctx), "%s = vec(value_Stdint(%d), %s);\n",
            resultText(target, ctx), index, resultText(closure, ctx));
}

static void emitUnprotect(EC *ctx) {
    fprintf(FH(ctx), "minlam_runtime_unprotect();\n");
}

static void emitTrace(ER *target, RA *args, EC *ctx) {
    fprintf(FH(ctx), "TRACE(\"%s\", getValue_Addr(%s), %d);\n",
            ctx->context.currentBinding->name, resultText(target, ctx),
            (int)args->size);
}

static void emitJumpToLambda(ER *target, EC *ctx) {
    fprintf(FH(ctx), "goto *getValue_Addr(%s);\n", resultText(target, ctx));
}

static void emitConstructVec(ER *target, int count, RA *results, EC *ctx) {
    fprintf(FH(ctx), "%s = make_vec(%d", resultText(target, ctx), count);
    for (Index i = results->size; i > 0; i--) {
        fprintf(FH(ctx), ", %s", resultText(results->entries[i - 1], ctx));
    }
    fprintf(FH(ctx), ");\n");
}

static ER *emitConstant(char *text) {
    Opaque *buf = newOpaque_EmitBuffer();
    int save = PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "%s", text);
    ER *target = newEmitCResult_Constant(buf);
    UNPROTECT(save);
    return target;
}

static ER *emitNone(EC *ctx __attribute__((unused))) {
    return emitConstant("value_None()");
}
static ER *emitEmptyVec(EC *ctx __attribute__((unused))) {
    return emitConstant("make_vec(0)");
}

// this and related code looks like it should belong in the
// generic minlam_emit.inc base but in fact it is concerned
// with optimisations using chained C function calls to avoid
// the overhead of registers.
static void emitAtomic(MinExp *exp, EC *ctx) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
        emitMinAnnotatedVar(getMinExp_Avar(exp), ctx);
        break;
    case MINEXP_TYPE_BIGINTEGER: {
        ER *bi = emitMaybeBigInt(getMinExp_BigInteger(exp), ctx);
        fprintf(FH(ctx), "%s",
                opaqueEmitBufferContent(getEmitCResult_Constant(bi)));
        break;
    }
    case MINEXP_TYPE_CHARACTER:
        emitCharacter(getMinExp_Character(exp), ctx);
        break;
    case MINEXP_TYPE_STDINT:
        emitStdint(getMinExp_Stdint(exp), ctx);
        break;
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *prim = getMinExp_Prim(exp);
        if (prim->type == MINPRIMOP_TYPE_VEC) {
            emitVec(prim->exp1, prim->exp2, ctx);
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

static ER *getConstantResult(EC *ctx) {
    return newEmitCResult_Constant(ctx->body);
}

static ER *getBufResult(EC *ctx) { return newEmitCResult_Buf(ctx->body); }

static ER *emitNewAtomic(MinExp *exp, EC *ctx) {
    EC *atomicContext = extendContext(ctx);
    int save = PROTECT(atomicContext);
    bool ic = isConst(exp);
    emitAtomic(exp, atomicContext);
    setProtectionStatus(&ctx->context, &atomicContext->context);
    ER *result =
        ic ? getConstantResult(atomicContext) : getBufResult(atomicContext);
    UNPROTECT(save);
    return result;
}

static ER *emitArg(MinExp *arg, EC *ctx) {
    if (isAtomic(arg)) {
        return emitNewAtomic(arg, ctx);
    } else {
        return emitSimpleExp(arg, ctx);
    }
}

static ER *emitAddrResult(HashSymbol *label, EC *ctx __attribute__((unused))) {
    Opaque *buf = newOpaque_EmitBuffer();
    int save = PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "value_Addr(&&%s)", label->name);
    ER *result = newEmitCResult_Buf(buf);
    UNPROTECT(save);
    return result;
}

static ER *emitIntegerResult(Integer i, EC *ctx __attribute__((unused))) {
    Opaque *buf = newOpaque_EmitBuffer();
    int save = PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "value_Stdint(%d)", i);
    ER *result = newEmitCResult_Constant(buf);
    UNPROTECT(save);
    return result;
}

static ER *emitCharacterResult(Character c, EC *ctx __attribute__((unused))) {
    Opaque *buf = newOpaque_EmitBuffer();
    int save = PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "value_Character(%d)", (int)c);
    ER *result = newEmitCResult_Constant(buf);
    UNPROTECT(save);
    return result;
}

static void comment(EC *ctx, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(FH(ctx), "// ");
    vfprintf(FH(ctx), fmt, args);
    fprintf(FH(ctx), "\n");
    va_end(args);
}

static void emitCharacter(Character character, EC *ctx) {
    fprintf(FH(ctx), "value_Character(%d)", (int)character);
}

static void emitStdint(Integer i, EC *ctx) {
    fprintf(FH(ctx), "value_Stdint(%d)", i);
}

static void emitVec(MinExp *exp1, MinExp *exp2, EC *ctx) {
    fprintf(FH(ctx), "vec(");
    emitAtomic(exp1, ctx);
    fprintf(FH(ctx), ", ");
    emitAtomic(exp2, ctx);
    fprintf(FH(ctx), ")");
}

static void emitAssignPrimOp2(ER *target, char *opName, ER *arg1, ER *arg2,
                              EC *ctx) {
    fprintf(FH(ctx), "%s = %s(%s, %s);\n", resultText(target, ctx), opName,
            resultText(arg1, ctx), resultText(arg2, ctx));
}

static void emitAssignPrimOp1(ER *target, char *opName, ER *arg1, EC *ctx) {
    fprintf(FH(ctx), "%s = %s(%s);\n", resultText(target, ctx), opName,
            resultText(arg1, ctx));
}

static void emitAssignPrimOp(MinPrimOp type, ER *target, ER *arg1, ER *arg2,
                             EC *ctx) {
    char *opName = getPrimOpCName(type);
    int nargs = getPrimOpArity(type);
    if (nargs == 2) {
        emitAssignPrimOp2(target, opName, arg1, arg2, ctx);
    } else {
        emitAssignPrimOp1(target, opName, arg1, ctx);
    }
}

static ER *emitMaybeBigInt(MaybeBigInt *mbi, EC *oldCtx) {
    EC *ctx = extendContext(oldCtx);
    int save = PROTECT(ctx);
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
    ER *result = getConstantResult(ctx);
    setProtectionStatus(&oldCtx->context, &ctx->context);
    UNPROTECT(save);
    return result;
}

static void emitMinAnnotatedVar(MinAnnotatedVar *node, EC *ctx) {
    fprintf(FH(ctx), "reg[%d] /* %s */", node->position, node->var->name);
}

static inline void emitAssign(ER *to, ER *from, EC *ctx) {
    fprintf(FH(ctx), "%s = %s;\n", resultText(to, ctx), resultText(from, ctx));
}

__attribute__((unused)) static inline void emitAssignReg(Index i, ER *value,
                                                         EC *ctx) {
    fprintf(FH(ctx), "reg[%d] = %s;\n", (int)i, resultText(value, ctx));
}

static ER *emitAnnotatedVarResult(MinAnnotatedVar *avar, EC *ctx) {
    EC *aVarContext = extendContext(ctx);
    setProtectionStatus(&ctx->context, &aVarContext->context); // ?
    int save = PROTECT(aVarContext);
    emitMinAnnotatedVar(avar, aVarContext);
    ER *result = getBufResult(aVarContext);
    UNPROTECT(save);
    return result;
}

// characters are immediate so we can dispatch on them, numbers
// can be bigints and therefore must be compared individually.
// ...it might be worth investigating sorting + binary search or
// nested dispatch tables on each consecutive bigint word..
static void emitMinCond(MinCond *node, EC *ctx) {
    EMITLOC("emitMinCond", node, ctx);
    ER *cond = emitSimpleExp(node->value, ctx);
    int save = PROTECT(cond);
    releaseSlot(cond, ctx);
    if (node->cases->type == MINCONDCASES_TYPE_INTEGERS) {
        emitMinIntCondCases(getMinCondCases_Integers(node->cases), cond, ctx);
    } else {
        fprintf(FH(ctx), "switch(getValue_Character(%s)) {\n",
                resultText(cond, ctx));
        emitMinCondCases(node->cases, ctx);
        fprintf(FH(ctx), "}\n");
    }
    UNPROTECT(save);
}

static void emitMinCondCases(MinCondCases *node, EC *ctx) {
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

static void emitMinIntCondCases(MinIntCondCases *node, ER *cond, EC *ctx) {
    if (node == NULL) {
        return;
    }
    EMITLOC("emitMinIntCondCases", node, ctx);
    if (node->constant == NULL) {
        // default case
        fprintf(FH(ctx), "{ // default\n");
    } else {
        fprintf(FH(ctx), "if (minlam_runtime_cmp(%s, ", resultText(cond, ctx));
        ER *bi = emitMaybeBigInt(node->constant, ctx);
        fprintf(FH(ctx), "%s",
                opaqueEmitBufferContent(getEmitCResult_Constant(bi)));
        fprintf(FH(ctx), ") == CMP_EQ) {\n");
    }
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->body, ctx);
    if (node->next != NULL) {
        fprintf(FH(ctx), "} else ");
        emitMinIntCondCases(node->next, cond, ctx);
    } else {
        fprintf(FH(ctx), "}\n");
    }
}

static void emitMinCharCondCases(MinCharCondCases *node, EC *ctx) {
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

static void emitMinMatch(MinMatch *node, EC *ctx) {
    EMITLOC("emitMinMatch", node, ctx);
    ER *match = emitSimpleExp(node->index, ctx);
    int save = PROTECT(match);
    fprintf(FH(ctx), "switch (getValue_Stdint(%s)) {\n",
            resultText(match, ctx));
    releaseSlot(match, ctx);
    UNPROTECT(save);
    emitMinMatchList(node->cases, ctx);
    fprintf(FH(ctx), "}\n");
}

static void emitMinMatchList(MinMatchList *node, EC *ctx) {
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

//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////

#define DISABLE_INNER_TYPEDEFS

#include "minlam_emit.inc"

//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////

//////////////
// Public API
//////////////

void emitCProgram(MinExp *node, BuiltIns *builtIns, FILE *out) {
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    HashSymbol *main = newSymbol("main");
    SymbolArray *heap = emitter_createHeap();
    PROTECT(heap);
    SymbolArray *symbols = newSymbolArray();
    PROTECT(symbols);
    EmitterContext context = newEmitterContext(main, builtIns, symbols, heap);
    PROTECT(context.slots);
    EC *ctx = newCEmitterContext(body, context);
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
    fprintf(out, "#define MAX_REG %d\n", ctx->context.maxReg);
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
    while ((label = iterateCBufferBag(ctx->lambdas, &i, &buf)) != NULL) {
        HashSymbol *l = makeLambdaLabel(label);
        fprintf(out, "%s:\n", l->name);
        fprintf(out, "%s\n", opaqueEmitBufferContent(buf));
    }
    fprintf(out, "\nENTRY:\n");
    fprintf(out, "%s\n", opaqueEmitBufferContent(ctx->body));
    fprintf(out, "}\n");
}