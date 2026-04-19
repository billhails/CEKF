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

#include "minlam_emit_b.h"
#include "emit_b_helper.h"
#include "emit_helper.h"
#include "memory.h"
#include "minlam.h"
#include "minlam_pp.h"
#include "minlam_runtime.h"
#include "symbol.h"
#include "utils_helper.h"
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <sys/param.h>

///////////////////////
// Emit Buffer Helpers
///////////////////////

typedef BEmitterContext EC;
typedef EmitBResult ER;
typedef BResultArray RA;

/////////////////////////////////////////////////////
// Forward declarations only used in this file
/////////////////////////////////////////////////////

static int getPrimOpArity(MinPrimOp);
static void emitAssignPrimOp1(ER *, BBC, ER *, EC *);
static void emitAssignPrimOp2(ER *, BBC, ER *, ER *, EC *);
static void emitMinCharCondCases(MinCharCondCases *, EC *, CharCondSwitch *,
                                 HashSymbol *);
static void emitMinIntCondCases(MinIntCondCases *, EC *, IntCondSwitch *,
                                HashSymbol *);
static void emitMinMatchList(MinMatchList *, EC *, IndexArray *, HashSymbol *);

#include "minlam_emit_contract.h"

// also used by minlam_emit.inc:
#define EMITLOC(name, node, ctx)                                               \
    do {                                                                       \
        comment(ctx, name);                                                    \
        if (node != NULL || CPI(node).lineNo != 0)                             \
            bemit_location(ctx, CPI(node));                                    \
    } while (0)

static inline int IX(ER *r, EC *c) {
    if (isEmitBResult_Slot(r)) {
        return slotIndex(getEmitBResult_Slot(r), &c->context);
    }
    if (isEmitBResult_Immediate(r)) {
        return getEmitBResult_Immediate(r);
    }
    cant_happen("unrecognised EmitBResult");
}

static inline RA *newRA() { return newBResultArray(); }

static inline void pushRA(RA *ra, ER *r) { pushBResultArray(ra, r); }

static inline ER *newResultSlotSymbol(HashSymbol *s) {
    return newEmitBResult_Slot(s);
}

static BBC getPrimOpBBC(MinPrimOp op) {
    switch (op) {
    case MINPRIMOP_TYPE_ADD:
        return BBC_TYPE_PRIM_ADD;
    case MINPRIMOP_TYPE_SUB:
        return BBC_TYPE_PRIM_SUB;
    case MINPRIMOP_TYPE_MUL:
        return BBC_TYPE_PRIM_MUL;
    case MINPRIMOP_TYPE_DIV:
        return BBC_TYPE_PRIM_DIV;
    case MINPRIMOP_TYPE_MOD:
        return BBC_TYPE_PRIM_MOD;
    case MINPRIMOP_TYPE_GCD:
        return BBC_TYPE_PRIM_GCD;
    case MINPRIMOP_TYPE_LCM:
        return BBC_TYPE_PRIM_LCM;
    case MINPRIMOP_TYPE_CANON:
        return BBC_TYPE_PRIM_CANON;
    case MINPRIMOP_TYPE_POW:
        return BBC_TYPE_PRIM_POW;
    case MINPRIMOP_TYPE_EQ:
        return BBC_TYPE_PRIM_EQ;
    case MINPRIMOP_TYPE_NE:
        return BBC_TYPE_PRIM_NE;
    case MINPRIMOP_TYPE_GT:
        return BBC_TYPE_PRIM_GT;
    case MINPRIMOP_TYPE_LT:
        return BBC_TYPE_PRIM_LT;
    case MINPRIMOP_TYPE_GE:
        return BBC_TYPE_PRIM_GE;
    case MINPRIMOP_TYPE_LE:
        return BBC_TYPE_PRIM_LE;
    case MINPRIMOP_TYPE_CMP:
        return BBC_TYPE_PRIM_CMP;
    case MINPRIMOP_TYPE_VEC:
        return BBC_TYPE_PRIM_VEC;
    default:
        cant_happen("unrecognised MinPrimOp %s", minPrimOpName(op));
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

///////////////////
// Context Helpers
///////////////////

static EC *extendContextForLambda(HashSymbol *var, EC *ctx) {
    // create a new buffer for the lambda body
    BBuffer *body = bemitter_newBuffer();
    int save = PROTECT(body);
    // add it to the global set of lambdas
    setBBufferBag(ctx->lambdas, var, body);
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
    EC *new = newBEmitterContext(body, context, ctx->constants);
    PROTECT(new);
    new->lambdas = ctx->lambdas;
    UNPROTECT(save);
    return new;
}

static Index addToConstants(MaybeBigInt *bi, EC *ctx) {
    Value v;
    switch (bi->type) {
    case BI_BIG: {
        bigint owned;
        bigint_init(&owned);
        bigint_cpy(&owned, &bi->big);
        BigInt *big = newBigInt(owned);
        if (bi->imag)
            v = value_Bigint_imag(big);
        else
            v = value_Bigint(big);
        break;
    }
    case BI_SMALL:
        if (bi->imag)
            v = value_Stdint_imag(bi->small);
        else
            v = value_Stdint(bi->small);
        break;
    case BI_IRRATIONAL:
        if (bi->imag)
            v = value_Irrational_imag(bi->irrational);
        else
            v = value_Irrational(bi->irrational);
        break;
    default:
        cant_happen("unrecognised MBI %d", bi->type);
    }
    Index i;
    BConstantArray *constants = ctx->constants;
    for (i = 0; i < constants->size; i++) {
        if (constants->entries[i].type == v.type &&
            minlam_runtime_cmp(constants->entries[i], v) == CMP_EQ) {
            return i;
        }
    }
    i = constants->size;
    int save = protectValue(v);
    pushBConstantArray(constants, v);
    UNPROTECT(save);
    return i;
}

//////////////////
// Result Helpers
//////////////////

static inline bool resultIsVar(ER *result) {
    return isEmitBResult_Slot(result);
}

static inline HashSymbol *getResultSlotSymbol(ER *result) {
    return getEmitBResult_Slot(result);
}

static inline bool resultNeedsMaterialization(ER *result) {
    (void)result;
    return false;
}

/////////////////
// Leaf Emitters
/////////////////

static void emitAssignPrimOp2(ER *target, BBC op, ER *arg1, ER *arg2, EC *ctx) {
    bemit_code(ctx, op, IX(target, ctx), IX(arg1, ctx), IX(arg2, ctx));
}

static void emitAssignPrimOp1(ER *target, BBC op, ER *arg1, EC *ctx) {
    bemit_code(ctx, op, IX(target, ctx), IX(arg1, ctx), 0);
}

static void emitAssignPrimOp(MinPrimOp type, ER *target, ER *arg1, ER *arg2,
                             EC *ctx) {
    BBC op = getPrimOpBBC(type);
    int nargs = getPrimOpArity(type);
    if (nargs == 2) {
        emitAssignPrimOp2(target, op, arg1, arg2, ctx);
    } else {
        emitAssignPrimOp1(target, op, arg1, ctx);
    }
}

static inline void emitAssign(ER *to, ER *from, EC *ctx) {
    bemit_code(ctx, BBC_TYPE_MOVE, IX(to, ctx), IX(from, ctx), 0);
}

static inline void emitAssignReg(Index i, ER *value, EC *ctx) {
    bemit_code(ctx, BBC_TYPE_MOVE, i, IX(value, ctx), 0);
}

static void emitDone(int status, EC *ctx) {
    bemit_code(ctx, BBC_TYPE_DONE, status, 0, 0);
}

static void emitClosureSetEnv(ER *clo, ER *env, EC *ctx) {
    bemit_code(ctx, BBC_TYPE_CLOSURE_SET_ENV, IX(clo, ctx), IX(env, ctx), 0);
}

static void emitClosureNew(ER *target, HashSymbol *label, EC *ctx) {
    bemit_code(ctx, BBC_TYPE_CLOSURE_NEW, IX(target, ctx), 0, 0);
    comment(ctx, "%s", label->name);
    bemit_fixup_code(ctx, label, bemitter_pos(ctx));
    bemit_word(ctx, 0);
}

static void emitIfThenElse(ER *test, MinExp *con, MinExp *alt, EC *ctx) {
    HashSymbol *ELSE = genSym("ELSE");
    HashSymbol *END = genSym("END");
    bemit_code(ctx, BBC_TYPE_JMP_FALSE, IX(test, ctx), 0, 0);
    bemit_fixup_code(ctx, ELSE, bemitter_pos(ctx));
    bemit_word(ctx, 0);
    bemit_label(ctx, ELSE, bemitter_pos(ctx));
    emitMinExp(con, ctx);
    bemit_code(ctx, BBC_TYPE_JMP, 0, 0, 0);
    bemit_fixup_code(ctx, END, bemitter_pos(ctx));
    bemit_word(ctx, 0);
    emitMinExp(alt, ctx);
    bemit_label(ctx, END, bemitter_pos(ctx));
}

static void emitCallBuiltin(BuiltIn *bi, ER *target, ER *arg, EC *ctx) {
    bemit_code(ctx, BBC_TYPE_CALL_BUILTIN, bi->id, IX(target, ctx),
               IX(arg, ctx));
}

static void emitVecGetImm(ER *target, ER *closure, int index, EC *ctx) {
    bemit_code(ctx, BBC_TYPE_VEC_GET_IMM, IX(target, ctx), index,
               IX(closure, ctx));
}

static void emitUnprotect(EC *ctx) {
    bemit_code(ctx, BBC_TYPE_UNPROTECT, 0, 0, 0);
}

// not supported for now
static void emitTrace(ER *target __attribute__((unused)),
                      RA *args __attribute__((unused)),
                      EC *ctx __attribute__((unused))) {}

static void emitJumpToLambda(ER *target, EC *ctx) {
    bemit_code(ctx, BBC_TYPE_JMP_REG, IX(target, ctx), 0, 0);
}

static void emitConstructVec(ER *target, int count, RA *results, EC *ctx) {
    int target_reg = IX(target, ctx);
    bemit_code(ctx, BBC_TYPE_MAKE_VEC, target_reg, count, 0);
    for (Index i = results->size; i > 0; i--) {
        bemit_code(ctx, BBC_TYPE_VEC_SET, target_reg,
                   IX(results->entries[i - 1], ctx), 0);
    }
}

static ER *emitNone(EC *ctx) {
    ER *target = claimSlot(ctx);
    int save = PROTECT(target);
    bemit_code(ctx, BBC_TYPE_LOAD_NONE, IX(target, ctx), 0, 0);
    UNPROTECT(save);
    return target;
}

static ER *emitEmptyVec(EC *ctx) {
    ER *target = claimSlot(ctx);
    int save = PROTECT(target);
    bemit_code(ctx, BBC_TYPE_MAKE_VEC, IX(target, ctx), 0, 0);
    UNPROTECT(save);
    return target;
}

void comment(BEmitterContext *context, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    SCharArray *msg = newSCharArray();
    int save = PROTECT(msg);
    vpsprintf(msg, fmt, args);
    va_end(args);
    bemit_comment(context, msg);
    UNPROTECT(save);
}

static ER *emitArg(MinExp *arg, EC *ctx) { return emitSimpleExp(arg, ctx); }

static ER *emitAddrResult(HashSymbol *label, EC *ctx) {
    ER *target = claimSlot(ctx);
    int save = PROTECT(target);
    bemit_code(ctx, BBC_TYPE_LOAD_ADDR, IX(target, ctx), 0, 0);
    bemit_fixup_code(ctx, label, bemitter_pos(ctx));
    bemit_word(ctx, 0);
    UNPROTECT(save);
    return target;
}

static ER *emitIntegerResult(Integer i, EC *ctx) {
    ER *target = claimSlot(ctx);
    int save = PROTECT(target);
    bemit_code(ctx, BBC_TYPE_LOAD_I32, IX(target, ctx), 0, 0);
    bemit_word(ctx, i);
    UNPROTECT(save);
    return target;
}

static ER *emitCharacterResult(Character c, EC *ctx) {
    ER *target = claimSlot(ctx);
    int save = PROTECT(target);
    bemit_code(ctx, BBC_TYPE_LOAD_CHAR, IX(target, ctx), 0, 0);
    bemit_word(ctx, c);
    UNPROTECT(save);
    return target;
}

static ER *emitAnnotatedVarResult(MinAnnotatedVar *avar,
                                  EC *ctx __attribute__((unused))) {
    return newEmitBResult_Immediate(avar->position);
}

static ER *emitMaybeBigInt(MaybeBigInt *mbi, EC *ctx) {
    Index i = addToConstants(mbi, ctx);
    ER *target = claimSlot(ctx);
    int save = PROTECT(target);
    bemit_code(ctx, BBC_TYPE_LOAD_CONST, IX(target, ctx), i, 0);
    // not needed on the emit_b branch:
    ctx->context.needsUnprotect = false;
    UNPROTECT(save);
    return target;
}

static void emitMinCond(MinCond *node, EC *ctx) {
    EMITLOC("emitMinCond", node, ctx);
    ER *cond = emitSimpleExp(node->value, ctx);
    int save = PROTECT(cond);
    HashSymbol *condEnd = genSymDollar("condEnd");
    if (node->cases->type == MINCONDCASES_TYPE_INTEGERS) {
        IntCondCaseArray *intCondCaseArray = newIntCondCaseArray();
        PROTECT(intCondCaseArray);
        IntCondSwitch *intCondSwitch = newIntCondSwitch(0, intCondCaseArray);
        PROTECT(intCondSwitch);
        bemit_code(ctx, BBC_TYPE_INTCOND, IX(cond, ctx), 0, 0);
        Index tableId = pushIntCondTable(ctx->body->intConds, intCondSwitch);
        bemit_fixup_inttable(ctx, tableId, ctx->body->codes->size);
        bemit_word(ctx, tableId); // <-- FIXUP
        releaseSlot(cond, ctx);
        emitMinIntCondCases(getMinCondCases_Integers(node->cases), ctx,
                            intCondSwitch, condEnd);
    } else {
        CharCondCaseArray *charCondCaseArray = newCharCondCaseArray();
        PROTECT(charCondCaseArray);
        CharCondSwitch *charCondSwitch =
            newCharCondSwitch(0, charCondCaseArray);
        PROTECT(charCondSwitch);
        bemit_code(ctx, BBC_TYPE_CHARCOND, IX(cond, ctx), 0, 0);
        Index tableId = pushCharCondTable(ctx->body->charConds, charCondSwitch);
        bemit_fixup_chartable(ctx, tableId, ctx->body->codes->size);
        bemit_word(ctx, tableId); // <-- FIXUP
        releaseSlot(cond, ctx);
        emitMinCharCondCases(getMinCondCases_Characters(node->cases), ctx,
                             charCondSwitch, condEnd);
    }
    bemit_label(ctx, condEnd, bemitter_pos(ctx));
    UNPROTECT(save);
}

static void emitMinIntCondCases(MinIntCondCases *node, EC *ctx,
                                IntCondSwitch *table, HashSymbol *end) {
    if (node == NULL) {
        cant_happen("empty int cond cases");
    }
    EMITLOC("emitMinIntCondCases", node, ctx);
    if (node->constant == NULL) {
        table->default_target = bemitter_pos(ctx);
    } else {
        Index constantId = addToConstants(node->constant, ctx);
        IntCondCase *thisCase = newIntCondCase(constantId, bemitter_pos(ctx));
        pushIntCondCaseArray(table->cases, thisCase);
    }
    emitMinExp(node->body, ctx);
    if (node->next != NULL) {
        bemit_code(ctx, BBC_TYPE_JMP, 0, 0, 0);
        bemit_fixup_code(ctx, end, bemitter_pos(ctx));
        bemit_word(ctx, 0);
        emitMinIntCondCases(node->next, ctx, table, end);
    }
}

static void emitMinCharCondCases(MinCharCondCases *node, EC *ctx,
                                 CharCondSwitch *table, HashSymbol *end) {
    if (node == NULL) {
        cant_happen("empty char cond cases");
    }
    EMITLOC("emitMinCharCondCases", node, ctx);
    if (node->isDefault) {
        table->default_target = bemitter_pos(ctx);
    } else {
        CharCondCase *thisCase =
            newCharCondCase(node->constant, bemitter_pos(ctx));
        pushCharCondCaseArray(table->cases, thisCase);
    }
    emitMinExp(node->body, ctx);
    if (node->next != NULL) {
        bemit_code(ctx, BBC_TYPE_JMP, 0, 0, 0);
        bemit_fixup_code(ctx, end, bemitter_pos(ctx));
        bemit_word(ctx, 0);
        emitMinCharCondCases(node->next, ctx, table, end);
    }
}

static void emitMinMatch(MinMatch *node, EC *ctx) {
    EMITLOC("emitMinMatch", node, ctx);
    ER *match = emitSimpleExp(node->index, ctx);
    int save = PROTECT(match);
    HashSymbol *matchEnd = genSymDollar("matchEnd");
    IndexArray *indexes = newIndexArray();
    PROTECT(indexes);
    Index tableId = pushMatchTable(ctx->body->matches, indexes);
    bemit_code(ctx, BBC_TYPE_MATCH, IX(match, ctx), 0, 0);
    bemit_fixup_matchtable(ctx, tableId, bemitter_pos(ctx));
    bemit_word(ctx, 0);
    releaseSlot(match, ctx);
    emitMinMatchList(node->cases, ctx, indexes, matchEnd);
    bemit_label(ctx, matchEnd, bemitter_pos(ctx));
    UNPROTECT(save);
}

//////// NOT YET CONVERTED ////////

static void emitMinMatchList(MinMatchList *node, EC *ctx, IndexArray *indexes,
                             HashSymbol *end) {
    if (node == NULL)
        return;
    EMITLOC("emitMinMatchList", node, ctx);
    MinIntList *match = node->matches;
    while (match != NULL) {
        if (match->item < 0)
            cant_happen("negative tag");

        if (indexes->size <= (Index)match->item) {
            pushnIndexArray(indexes,
                            (int)((Index)match->item + 1 - indexes->size), 0);
        }
        pokeIndexArray(indexes, match->item, bemitter_pos(ctx));
        match = match->next;
    }
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->body, ctx);
    if (node->next != NULL) {
        bemit_code(ctx, BBC_TYPE_JMP, 0, 0, 0);
        bemit_fixup_code(ctx, end, bemitter_pos(ctx));
        bemit_word(ctx, 0);
        emitMinMatchList(node->next, ctx, indexes, end);
    }
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

BBuffer *emitBProgram(MinExp *node, BuiltIns *builtIns) {
    HashSymbol *main = newSymbol("main");
    SymbolArray *heap = emitter_createHeap();
    int save = PROTECT(heap);
    EmitterContext context = newEmitterContext(main, builtIns, heap);
    PROTECT(context.slots);
    EC *ctx = bemitter_newContext(context);
    REPLACE_PROTECT(save, ctx);
    HashSymbol *entry = newSymbol("ENTRY");
    bemit_code(ctx, BBC_TYPE_JMP, 0, 0, 0);
    comment(ctx, "ENTRY");
    bemit_fixup_code(ctx, entry, bemitter_pos(ctx));
    bemit_word(ctx, 0);

    emitMinExp(node, ctx);
    // fprintf(out, "#define MAX_REG %d\n", ctx->context.maxReg);
    // fprintf(out, "static Value reg[MAX_REG];\n");
    // fprintf(out, "minlam_runtime_init(reg, MAX_REG, argc, argv);\n");
    Index i = 0;
    HashSymbol *label = NULL;
    BBuffer *buf = NULL;
    BBuffer *final = bemitter_newBuffer();
    PROTECT(final);
    while ((label = iterateBBufferBag(ctx->lambdas, &i, &buf)) != NULL) {
        HashSymbol *l = makeLambdaLabel(label);
        bemit_buffer_label(final, l, bemitter_buffer_pos(final));
        bemitter_append(final, buf, 0);
    }
    final->constants = ctx->constants;
    bemit_buffer_label(final, entry, bemitter_buffer_pos(final));
    bemitter_append(final, ctx->body, 0);
    UNPROTECT(save);
    return final;
}