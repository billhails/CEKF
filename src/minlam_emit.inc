#ifndef cekf_minlam_emit_inc
#define cekf_minlam_emit_inc
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
 */

#include "memory.h"
#include "minlam.h"
#include "minlam_pp.h"
#include "symbol.h"
#include "utils_helper.h"
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <sys/param.h>

// this is just so the IDE doesn't highlight undefined types
#ifndef DISABLE_INNER_TYPEDEFS
#include "minlam_emit_b.h"
typedef BEmitterContext EC;
typedef EmitBResult ER;
typedef BResultArray RA;
typedef BResultMap RM;
#endif

static ER *claimSlot(EC *);
static ER *emitMakeVec(MinExprList *, EC *);
static void emitMinApply(MinApply *, EC *);
static void emitMinBindings(MinBindings *, Integer, RA *, EC *);
static void emitMinIff(MinIff *, EC *);
static void emitMinLam(MinLam *, EC *);
static void emitMinLetRec(MinLetRec *, EC *);
static void recordAssignment(RM *, SymbolMap *, ER *, ER *, EC *);

////////////////////
// SlotPool Helpers
////////////////////

static HashSymbol *addRM(RM *map, ER *res, bool fatal, EC *ctx) {
    HashSymbol *symbol = tokenForER(res, ctx);
    if (symbol == NULL) {
        if (fatal)
            cant_happen("assignment to non-register");
        else
            symbol = genSym("er");
    }
    setRM(map, symbol, res);
    return symbol;
}

static void recordAssignment(RM *map, SymbolMap *symbols, ER *to, ER *from,
                             EC *ctx) {
    HashSymbol *toSymbol = addRM(map, to, true, ctx);
    HashSymbol *fromSymbol = addRM(map, from, false, ctx);
    setSymbolMap(symbols, toSymbol, fromSymbol);
}

static ER *claimSlot(EC *ctx) {
    HashSymbol *symbol = emitter_claimSlotSymbol(&ctx->context);
    return newResultSlotSymbol(symbol);
}

static void releaseSlot(ER *result, EC *ctx) {
    if (resultIsVar(result))
        emitter_releaseSlotSymbol(getResultSlotSymbol(result), &ctx->context);
}

static void releaseSlots(RA *slots, EC *ctx) {
    for (Index i = 0; i < slots->size; i++) {
        releaseSlot(slots->entries[i], ctx);
    }
}

static RA *claimSlotsBelow(int N, EC *ctx) {
    comment(ctx, "ensuring reg[0..%d]", N - 1);
    RA *slots = newRA();
    int save = PROTECT(slots);
    while (emitter_slotsAvailableBelow(N, &ctx->context)) {
        ER *slot = claimSlot(ctx);
        int save2 = PROTECT(slot);
        pushRA(slots, slot);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return slots;
}

static RA *claimSlots(int N, EC *ctx) {
    RA *slots = newRA();
    int save = PROTECT(slots);
    for (int i = 0; i < N; ++i) {
        ER *slot = claimSlot(ctx);
        int save2 = PROTECT(slot);
        pushRA(slots, slot);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return slots;
}

//////////////////
// MinExp Helpers
//////////////////

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

/////////////////
// Label Helpers
/////////////////

static HashSymbol *makeLabel(char *prefix, HashSymbol *symbol) {
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
    HashSymbol *result = newSymbol(label->entries);
    UNPROTECT(save);
    return result;
}

static HashSymbol *makeLambdaLabel(HashSymbol *symbol) {
    return makeLabel("LAMBDA_", symbol);
}

////////////
// Emitters
////////////

static ER *emitPrimOp(MinPrimApp *app, EC *ctx) {
    ER *arg1 = emitArg(app->exp1, ctx);
    int save = PROTECT(arg1);
    ER *arg2 = emitArg(app->exp2, ctx);
    PROTECT(arg2);
    // deliberate early release of any temps prior to claiming a new one
    // allows i.e. `reg[1] = op(reg[1], reg[2]);` as long as we don't emit
    // anything that could clobber the released slots before we are done
    // with them.
    releaseSlot(arg1, ctx);
    releaseSlot(arg2, ctx);
    ER *target = claimSlot(ctx);
    PROTECT(target);
    emitAssignPrimOp(app->type, target, arg1, arg2, ctx);
    UNPROTECT(save);
    return target; // the temp register that the caller can refer to
}

static ER *emitAnonymousLambda(MinLam *lambda, EC *ctx) {
    EMITLOC("emitAnonymousLambda", lambda, ctx);
    // emit the lambda to the prelude, this then reduces to LABEL
    HashSymbol *name = genSym("anon");
    EC *lamContext = extendContextForLambda(name, ctx);
    int save = PROTECT(lamContext);
    HashSymbol *containing_label = makeLambdaLabel(ctx->context.currentBinding);
    comment(lamContext, "inside %s, %d args", containing_label->name,
            countSymbolList(lambda->args));
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(&ctx->context, &lamContext->context);
    HashSymbol *label = makeLambdaLabel(name);
    ER *result = emitAddrResult(label, ctx);
    UNPROTECT(save);
    return result;
}

static ER *emitSimpleExp(MinExp *exp, EC *ctx) {
    switch (exp->type) {
    case MINEXP_TYPE_PRIM: {
        return emitPrimOp(getMinExp_Prim(exp), ctx);
    }
    case MINEXP_TYPE_AVAR: {
        return emitAnnotatedVarResult(getMinExp_Avar(exp), ctx);
    }
    case MINEXP_TYPE_MAKEVEC: {
        return emitMakeVec(getMinExp_MakeVec(exp), ctx);
    }
    case MINEXP_TYPE_LAM: {
        return emitAnonymousLambda(getMinExp_Lam(exp), ctx);
    }
    case MINEXP_TYPE_BIGINTEGER: {
        return emitMaybeBigInt(getMinExp_BigInteger(exp), ctx);
    }
    case MINEXP_TYPE_STDINT: {
        return emitIntegerResult(getMinExp_Stdint(exp), ctx);
    }
    case MINEXP_TYPE_CHARACTER: {
        return emitCharacterResult(getMinExp_Character(exp), ctx);
    }
    default:
        ppMinExp(stderr, exp);
        eprintf("\n");
        cant_happen("unhandled %s", minExpTypeName(exp->type));
    }
}

// prefer to emit right to left because the common case for deeply nested
// vecs is lists which are right-associative, and by emitting r2l we need
// only one temp for the entire backbone of the list.
static RA *emitRL(MinExprList *vecs, EC *ctx) {
    if (vecs == NULL)
        return newRA();
    RA *results = emitRL(vecs->next, ctx);
    int save = PROTECT(results);
    ER *result = emitArg(vecs->exp, ctx);
    PROTECT(result);
    pushRA(results, result);
    UNPROTECT(save);
    return results;
}

static ER *emitMakeVec(MinExprList *vecs, EC *ctx) {
    RA *results = emitRL(vecs, ctx);
    int save = PROTECT(results);
    int count = countMinExprList(vecs);
    ER *target = NULL;
    if (count == 0) {
        target = emitNone(ctx);
        PROTECT(target);
        releaseSlots(results, ctx);
    } else {
        target = claimSlot(ctx);
        PROTECT(target);
        releaseSlots(results, ctx);
        emitConstructVec(target, count, results, ctx);
    }
    UNPROTECT(save);
    return target;
}

static void emitMinLam(MinLam *node, EC *ctx) {
#ifdef SAFETY_CHECKS
    if (node == NULL)
        cant_happen("NULL MinLam");
#endif
    int nargs = countSymbolList(node->args);
    comment(ctx, "emitMinLam reserving %d incoming slots", nargs);
    RA *slots = claimSlots(nargs, ctx);
    int save = PROTECT(slots);
    ctx->context.currentReg += nargs;
    ASSERT_SLOTS(&ctx->context);
    if (node->exp != NULL)
        emitMinExp(node->exp, ctx);
    releaseSlots(slots, ctx);
    ctx->context.currentReg -= nargs;
    UNPROTECT(save);
}

static void emitGoto(ER *target, RA *args, EC *ctx) {
    comment(ctx, "emitGoto - %u outgoing slots", args->size);
    comment(ctx, "emitGoto stage outgoing args into temporaries");
    RA *sourceSlots = newRA();
    int save = PROTECT(sourceSlots);
    RA *allClaimedSlots = newRA();
    PROTECT(allClaimedSlots);
    SymbolMap *copyMap = newSymbolMap();
    PROTECT(copyMap);
    RM *ERMap = newRM();
    PROTECT(ERMap);

    for (Index i = 0; i < args->size; i++) {
        ER *tempSlot = claimSlot(ctx);
        int save2 = PROTECT(tempSlot);
        ER *arg = args->entries[i];
        if (resultNeedsMaterialization(arg)) {
            // in the emit_c case this is an expression involving
            // registers, not just a register.
            emitAssign(tempSlot, arg, ctx);
            pushRA(sourceSlots, tempSlot);
        } else {
            // a var has already claimed a safe slot, use it directly for
            // copy-down, likewise if this is a constant expression.
            pushRA(sourceSlots, arg);
        }
        pushRA(allClaimedSlots, tempSlot);
        UNPROTECT(save2);
    }

    ER *savedTarget = claimSlot(ctx);
    PROTECT(savedTarget);
    pushRA(allClaimedSlots, savedTarget);
    if (resultNeedsMaterialization(target)) {
        emitAssign(savedTarget, target, ctx);
    } else {
        savedTarget = target;
    }

    // Copy-down to reg[0..N-1]
    comment(ctx, "emitGoto copy-down into reg[0..%u]", sourceSlots->size);
    for (Index i = 0; i < sourceSlots->size; i++) {
        ER *dest = erForSlot(i, ctx);
        int save2 = PROTECT(dest);
        recordAssignment(ERMap, copyMap, dest, sourceSlots->entries[i], ctx);
        UNPROTECT(save2);
    }

    ER *tmp = claimSlot(ctx);
    PROTECT(tmp);
    pushRA(allClaimedSlots, tmp);
    addRM(ERMap, tmp, true, ctx);

    SlotAssignArray *schedule =
        resolveCopy(copyMap, tokenForER(tmp, ctx), NULL);
    PROTECT(schedule);

    for (Index i = 0; i < countSlotAssignArray(schedule); i++) {
        SlotAssign *assign = getSlotAssignArray(schedule, i);
        ER *dest;
        ER *source;
        if (!getRM(ERMap, assign->dest, &dest)) {
            cant_happen("failed to locate token in RM");
        }
        if (!getRM(ERMap, assign->source, &source)) {
            cant_happen("failed to locate token in RM");
        }
        emitAssign(dest, source, ctx);
    }

    if (ctx->context.needsUnprotect) {
        emitUnprotect(ctx);
        ctx->context.needsUnprotect = false;
    }

    comment(ctx, "emitGoto jump to staged target");
    emitTrace(savedTarget, args, ctx);
    emitJumpToLambda(savedTarget, ctx);
    releaseSlots(allClaimedSlots, ctx);
    UNPROTECT(save);
}

static ER *emitExtractFromClosure(ER *closure, int index, EC *ctx) {
    // this must never write to slots except this one
    comment(ctx, "emitExtractFromClosure index=%d", index);
    ER *target = claimSlot(ctx);
    int save = PROTECT(target);
    emitVecGetImm(target, closure, index, ctx);
    UNPROTECT(save);
    return target;
}

static ER *emitBuiltin(BuiltIn *bi, MinExprList *builtinArgs, EC *ctx) {
    ER *arg = NULL;
    if (countBuiltInArgs(bi->args) == 0)
        arg = emitEmptyVec(ctx); // just for builtins
    else
        arg = emitMakeVec(builtinArgs, ctx);
    int save = PROTECT(arg);
    ER *target = claimSlot(ctx);
    PROTECT(target);
    emitCallBuiltin(bi, target, arg, ctx);
    releaseSlot(arg, ctx);
    UNPROTECT(save);
    return target;
}

// (builtin arg_1 ... arg_n k f)
// basically: "give the result of calling the builtin to k, passing f along"
// Where k itself is a closure (2-vec)
static void emitApplyBuiltin(MinApply *node, EC *ctx) {
    EMITLOC("emitApplyBuiltin", node, ctx);

    RA *contArgSlots = claimSlotsBelow(3, ctx); // (3 args: env, result, f)
    int save = PROTECT(contArgSlots);

    BuiltIn *bi = emitter_findBuiltIn(node, &ctx->context);
    int numBuiltinArgs = countBuiltInArgs(bi->args);

    // k and f
    MinExprList *continuations = dropMinExprList(node->args, numBuiltinArgs);

    // k
    ER *k = emitSimpleExp(continuations->exp, ctx);
    PROTECT(k);

    RA *targetArgs = newRA();
    PROTECT(targetArgs);

    // (env)
    ER *env = emitExtractFromClosure(k, 1, ctx);
    PROTECT(env);
    pushRA(targetArgs, env);

    ER *target = emitExtractFromClosure(k, 0, ctx);
    PROTECT(target);

    releaseSlot(k, ctx);

    // (env result)
    MinExprList *builtinArgs = takeMinExprList(node->args, numBuiltinArgs);
    PROTECT(builtinArgs);
    ER *builtinResult = emitBuiltin(bi, builtinArgs, ctx);
    PROTECT(builtinResult);
    pushRA(targetArgs, builtinResult);

    continuations = continuations->next;

    // (env result f)
    ER *f = emitSimpleExp(continuations->exp, ctx);
    PROTECT(f);
    pushRA(targetArgs, f);

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
//   arg 1 (result): a + b             -- the computed value
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

static void emitApplyClosure(MinApply *apply, EC *ctx) {
    EMITLOC("emitApplyClosure", apply, ctx);
    comment(ctx, "emitApplyClosure argc=%u", countMinExprList(apply->args));

    RA *closureArgSlots =
        claimSlotsBelow(countMinExprList(apply->args) + 1, // +1 for closure env
                        ctx);
    int save = PROTECT(closureArgSlots);

    ER *closure = emitArg(apply->function, ctx);
    PROTECT(closure);

    comment(ctx, "emitApplyClosure extract target from closure");
    ER *target = emitExtractFromClosure(closure, 0, ctx);
    PROTECT(target);
    // deliberate early release allows for behaviour like
    // tmp_1 = vec(..., tmp_1);
    releaseSlot(closure, ctx);

    comment(ctx, "emitApplyClosure extract env from closure");
    ER *env = emitExtractFromClosure(closure, 1, ctx);
    PROTECT(env);

    RA *targetArgs = newRA();
    PROTECT(targetArgs);
    pushRA(targetArgs, env);

    for (MinExprList *arg = apply->args; arg != NULL; arg = arg->next) {
        ER *result = emitSimpleExp(arg->exp, ctx);
        int save2 = PROTECT(result);
        pushRA(targetArgs, result);
        UNPROTECT(save2);
    }

    emitGoto(target, targetArgs, ctx);
    releaseSlot(target, ctx);
    releaseSlots(targetArgs, ctx);
    releaseSlots(closureArgSlots, ctx);
    UNPROTECT(save);
}

static void emitMinApply(MinApply *node, EC *ctx) {
    if (node->isBuiltin) {
        emitApplyBuiltin(node, ctx);
    } else {
        emitApplyClosure(node, ctx);
    }
}

static void emitMinIff(MinIff *node, EC *ctx) {
    EMITLOC("emitMinIff", node, ctx);
    ER *test = emitSimpleExp(node->condition, ctx);
    int save = PROTECT(test);
    releaseSlot(test, ctx);
    emitIfThenElse(test, node->consequent, node->alternative, ctx);
    UNPROTECT(save);
    ASSERT_SLOTS(&ctx->context);
}

// emitMinLetRec -> emitMinBindings -> emitMakeClosure
static void emitMakeClosure(HashSymbol *var, Integer depth, RA *bindings,
                            EC *ctx) {
    comment(ctx, "emitMakeClosure %d", depth);
    HashSymbol *label = makeLambdaLabel(var);
    emitClosureNew(bindings->entries[depth], label, ctx);
}

// emitMinLetRec -> emitBackpatchBindings -> emitBackpatchClosure
static void emitBackpatchClosure(MinExprList *exprs, int depth, RA *slots,
                                 EC *ctx) {
    if (exprs == NULL)
        return; // emitMakeClosure has already assigned the appropriate value
    comment(ctx, "emitBackpatchClosure %d", depth);
    ER *tmp = emitMakeVec(exprs, ctx);
    int save = PROTECT(tmp);
    emitClosureSetEnv(slots->entries[depth], tmp, ctx);
    releaseSlot(tmp, ctx);
    UNPROTECT(save);
}

static void emitMinBindings(MinBindings *node, Integer depth, RA *bindings,
                            EC *ctx) {
    if (node == NULL)
        return;
    // Use a unique name to avoid CBufferBag hash collisions when the
    // same binding name (e.g. h1$0) appears in multiple letrecs.
    HashSymbol *uniqueName = genSym(node->var->name);
    EC *lamContext = extendContextForLambda(uniqueName, ctx);
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
                                  RA *bindings, EC *ctx) {
    if (node == NULL)
        return;
    MinExprList *makeEnv = extractEnv(node->val);
    emitBackpatchClosure(makeEnv, depth, bindings, ctx);
    emitBackpatchBindings(node->next, depth + 1, bindings, ctx);
}

static void emitMinLetRec(MinLetRec *node, EC *ctx) {
    EMITLOC("emitMinLetRec", node, ctx);
    ASSERT_SLOTS(&ctx->context);
    int numBindings = countMinBindings(node->bindings);
    RA *bindings = claimSlots(numBindings, ctx);
    int save = PROTECT(bindings);
    emitMinBindings(node->bindings, 0, bindings, ctx);
    emitBackpatchBindings(node->bindings, 0, bindings, ctx);
    ctx->context.currentReg += numBindings;
    ASSERT_SLOTS(&ctx->context);
    emitMinExp(node->body, ctx);
    ctx->context.currentReg -= numBindings;
    releaseSlots(bindings, ctx);
    ASSERT_SLOTS(&ctx->context);
    UNPROTECT(save);
}

static void emitMinExp(MinExp *node, EC *ctx) {
    switch (node->type) {
    case MINEXP_TYPE_APPLY: {
        emitMinApply(getMinExp_Apply(node), ctx);
        break;
    }
    case MINEXP_TYPE_COND: {
        emitMinCond(getMinExp_Cond(node), ctx);
        break;
    }
    case MINEXP_TYPE_DONE: {
        emitDone(getMinExp_Done(node), ctx);
        break;
    }
    case MINEXP_TYPE_IFF: {
        emitMinIff(getMinExp_Iff(node), ctx);
        break;
    }
    case MINEXP_TYPE_LAM: {
        emitMinLam(getMinExp_Lam(node), ctx);
        break;
    }
    case MINEXP_TYPE_LETREC: {
        emitMinLetRec(getMinExp_LetRec(node), ctx);
        break;
    }
    case MINEXP_TYPE_MATCH: {
        emitMinMatch(getMinExp_Match(node), ctx);
        break;
    }
    default:
        cant_happen("unexpected MinExp type %s", minExpTypeName(node->type));
    }
}
#endif