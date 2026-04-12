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
#include "minlam_emit_c.h"
#include "minlam_pp.h"
#include "symbol.h"
#include "utils_helper.h"
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <sys/param.h>

static void emitMinLam(MinLam *, EC *);
static void emitMinApply(MinApply *, EC *);
static void emitMinIff(MinIff *, EC *);
static void emitMinCond(MinCond *, EC *);
static void emitMinIntCondCases(MinIntCondCases *, ER *, EC *);
static void emitMinCharCondCases(MinCharCondCases *, EC *);
static void emitMinMatch(MinMatch *, EC *);
static void emitMinMatchList(MinMatchList *, EC *);
static void emitMinLetRec(MinLetRec *, EC *);
static void emitMinBindings(MinBindings *, Integer, RA *, EC *);
static void emitMinCondCases(MinCondCases *, EC *);
static ER *emitMakeVec(MinExprList *, EC *);

////////////////////
// SlotPool Helpers
////////////////////

static ER *claimSlot(EC *ctx) {
    HashSymbol *symbol = emitter_claimSlotSymbol(&ctx->context);
    return newEmitCResult_Var(symbol);
}

static void releaseSlot(ER *result, EmitterContext *ctx) {
    if (isEmitCResult_Var(result))
        emitter_releaseSlotSymbol(getEmitCResult_Var(result), ctx);
}

static void releaseSlots(RA *slots, EmitterContext *ctx) {
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

static ER *emitPrimOp(MinPrimApp *app, EC *ctx) {
    ER *arg1 = emitArg(app->exp1, ctx);
    int save = PROTECT(arg1);
    ER *arg2 = emitArg(app->exp2, ctx);
    PROTECT(arg2);
    // deliberate early release of any temps prior to claiming a new one
    // allows i.e. `reg[1] = op(reg[1], reg[2]);` as long as we don't emit
    // anything that could clobber the released slots before we are done
    // with them.
    releaseSlot(arg1, &ctx->context);
    releaseSlot(arg2, &ctx->context);
    ER *target = claimSlot(ctx);
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

static ER *emitAnonymousLambda(MinLam *lambda, EC *ctx) {
    EMITLOC("emitAnonymousLambda", lambda, ctx);
    // emit the lambda to the prelude, this then reduces to &&LABEL
    HashSymbol *name = genSym("anon");
    EC *lamContext = extendContextForLambda(name, ctx);
    int save = PROTECT(lamContext);
    SCharArray *containing_label = makeLambdaLabel(ctx->context.currentBinding);
    PROTECT(containing_label);
    comment(lamContext, "inside %s, %d args", containing_label->entries,
            countSymbolList(lambda->args));
    emitMinLam(lambda, lamContext);
    retrieveMaxReg(&ctx->context, &lamContext->context);
    SCharArray *label = makeLambdaLabel(name);
    PROTECT(label);
    ER *result = emitAddrResult(label);
    UNPROTECT(save);
    return result;
}

static ER *emitAnnotatedVarResult(MinAnnotatedVar *avar, EC *ctx) {
    EC *aVarContext = extendContext(ctx);
    setProtectionStatus(&ctx->context, &aVarContext->context);
    int save = PROTECT(aVarContext);
    emitMinAnnotatedVar(avar, aVarContext);
    ER *result = getBufResult(aVarContext);
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
        MaybeBigInt *mbi = getMinExp_BigInteger(exp);
        EC *biContext = extendContext(ctx);
        int save = PROTECT(biContext);
        emitMaybeBigInt(mbi, biContext);
        ER *result = getConstantResult(biContext);
        setProtectionStatus(&ctx->context, &biContext->context);
        UNPROTECT(save);
        return result;
    }
    case MINEXP_TYPE_STDINT: {
        return emitIntegerResult(getMinExp_Stdint(exp));
    }
    case MINEXP_TYPE_CHARACTER: {
        return emitCharacterResult(getMinExp_Character(exp));
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
    releaseSlots(results, &ctx->context);
    ER *target = NULL;
    if (count == 0) {
        target = emitNone();
    } else {
        target = claimSlot(ctx);
        PROTECT(target);
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
    releaseSlots(slots, &ctx->context);
    ctx->context.currentReg -= nargs;
    UNPROTECT(save);
}

static void emitGoto(ER *target, RA *args, EC *ctx) {
    comment(ctx, "emitGoto - %u outgoing slots", args->size);
    RA *sourceSlots = newRA();
    int save = PROTECT(sourceSlots);
    RA *claimedSlots = newRA();
    PROTECT(claimedSlots);

    for (Index i = 0; i < args->size; i++) {
        ER *tempSlot = claimSlot(ctx);
        int save2 = PROTECT(tempSlot);
        ER *arg = args->entries[i];
        if (isEmitCResult_Buf(arg)) {
            emitAssign(tempSlot, arg, ctx);
            pushRA(sourceSlots, tempSlot);
        } else {
            // a var has already claimed a safe slot, use it directly for
            // copy-down, likewise if this is a constant expression.
            pushRA(sourceSlots, arg);
        }
        pushRA(claimedSlots, tempSlot);
        UNPROTECT(save2);
    }

    ER *savedTarget = claimSlot(ctx);
    PROTECT(savedTarget);
    pushRA(claimedSlots, savedTarget);
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
        emitUnprotect(ctx);
        ctx->context.needsUnprotect = false;
    }

    emitTrace(savedTarget, args, ctx);
    emitJumpToLambda(savedTarget, ctx);
    releaseSlots(claimedSlots, &ctx->context);
    UNPROTECT(save);
}

static ER *emitExtractFromClosure(ER *closure, int index, EC *ctx) {
    // this must never write to slots except this one
    ER *target = claimSlot(ctx);
    int save = PROTECT(target);
    emitVecGetImm(target, closure, index, ctx);
    UNPROTECT(save);
    return target;
}

static ER *emitBuiltin(BuiltIn *bi, MinExprList *builtinArgs, EC *ctx) {
    ER *arg = NULL;
    if (countBuiltInArgs(bi->args) == 0)
        arg = emitEmptyVec(); // just for builtins
    else
        arg = emitMakeVec(builtinArgs, ctx);
    int save = PROTECT(arg);
    ER *target = claimSlot(ctx);
    PROTECT(target);
    emitCallBuiltin(bi, target, arg, ctx);
    releaseSlot(arg, &ctx->context);
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

    releaseSlot(k, &ctx->context);

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

static void emitApplyClosure(MinApply *apply, EC *ctx) {
    EMITLOC("emitApplyClosure", apply, ctx);

    RA *closureArgSlots =
        claimSlotsBelow(countMinExprList(apply->args) + 1, // +1 for closure env
                        ctx);
    int save = PROTECT(closureArgSlots);

    ER *closure = emitArg(apply->function, ctx);
    PROTECT(closure);

    ER *target = emitExtractFromClosure(closure, 0, ctx);
    PROTECT(target);
    // deliberate early release allows for behaviour like
    // tmp_1 = vec(..., tmp_1);
    releaseSlot(closure, &ctx->context);

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
    releaseSlot(target, &ctx->context);
    releaseSlots(targetArgs, &ctx->context);
    releaseSlots(closureArgSlots, &ctx->context);
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
    releaseSlot(test, &ctx->context);
    emitIfThenElse(test, node->consequent, node->alternative, ctx);
    UNPROTECT(save);
    ASSERT_SLOTS(&ctx->context);
}

static void emitMinCond(MinCond *node, EC *ctx) {
    EMITLOC("emitMinCond", node, ctx);
    ER *cond = emitSimpleExp(node->value, ctx);
    int save = PROTECT(cond);
    releaseSlot(cond, &ctx->context);
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
        emitMaybeBigInt(node->constant, ctx);
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
    releaseSlot(match, &ctx->context);
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

// emitMinLetRec -> emitMinBindings -> emitMakeClosure
static void emitMakeClosure(HashSymbol *var, Integer depth, RA *bindings,
                            EC *ctx) {
    comment(ctx, "emitMakeClosure %d", depth);
    SCharArray *label = makeLambdaLabel(var);
    int save = PROTECT(label);
    emitClosureNew(bindings->entries[depth], label, ctx);
    UNPROTECT(save);
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
    releaseSlot(tmp, &ctx->context);
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
    releaseSlots(bindings, &ctx->context);
    ASSERT_SLOTS(&ctx->context);
    UNPROTECT(save);
}

static void emitMinExp(MinExp *node, EC *ctx) {
    switch (node->type) {
    case MINEXP_TYPE_APPLY: {
        emitMinApply(getMinExp_Apply(node), ctx);
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // bare variable in tail position is dead code (CPS artifact)
        comment(ctx, "dead tail: %s", getMinExp_Avar(node)->var->name);
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