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
#include "memory.h"

#ifdef DEBUG_MINLAM_EMIT_B
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static void emitMinLam(MinLam *, BEmitterContext *);
static void emitMinAnnotatedVar(MinAnnotatedVar *, BEmitterContext *);
static void emitMinExprList(MinExprList *, BEmitterContext *);
static void emitMinPrimApp(MinPrimApp *, BEmitterContext *);
static void emitMinApply(MinApply *, BEmitterContext *);
static void emitMinIff(MinIff *, BEmitterContext *);
static void emitMinCond(MinCond *, BEmitterContext *);
static void emitMinIntCondCases(MinIntCondCases *, BEmitterContext *);
static void emitMinCharCondCases(MinCharCondCases *, BEmitterContext *);
static void emitMinMatch(MinMatch *, BEmitterContext *);
static void emitMinMatchList(MinMatchList *, BEmitterContext *);
static void emitMinIntList(MinIntList *, BEmitterContext *);
static void emitMinLetRec(MinLetRec *, BEmitterContext *);
static void emitMinBindings(MinBindings *, BEmitterContext *);
static void emitMinAmb(MinAmb *, BEmitterContext *);
static void emitMinExp(MinExp *, BEmitterContext *);
static void emitMinCondCases(MinCondCases *, BEmitterContext *);

///////
// API
///////

void emitBProgram(MinExp *node, BuiltIns *builtIns) {
    UIntArray *codes = newUIntArray();
    int save = PROTECT(codes);
    BFixupArray *fixups = newBFixupArray();
    PROTECT(fixups);
    BLocationArray *locations = newBLocationArray();
    PROTECT(locations);
    BCommentArray *comments = newBCommentArray();
    PROTECT(comments);
    BBuffer *bBuffer = newBBuffer(codes, fixups, locations, comments);
    PROTECT(bBuffer);
    HashSymbol *currentBinding = newSymbol("main");
    SymbolArray *heap = newSymbolArray();
    PROTECT(heap);
    EmitterContext context = newEmitterContext(currentBinding, builtIns, heap);
    PROTECT(context.slots);
    BEmitterContext *ctx = newBEmitterContext(bBuffer, context);
    PROTECT(ctx);
    emitMinExp(node, ctx);
    UNPROTECT(save);
}

///////////////////////////
// Visitor implementations
///////////////////////////

static void emitSymbolList(SymbolList *node, BEmitterContext *ctx) {
    ENTER(emitSymbolList);
    (void)node;
    (void)ctx;
    LEAVE(emitSymbolList);
}

static void emitMinLam(MinLam *node, BEmitterContext *ctx) {
    ENTER(emitMinLam);
    emitSymbolList(node->args, ctx);
    emitMinExp(node->exp, ctx);
    LEAVE(emitMinLam);
}

static void emitMinAnnotatedVar(MinAnnotatedVar *node, BEmitterContext *ctx) {
    ENTER(emitMinAnnotatedVar);
    (void)node;
    (void)ctx;
    LEAVE(emitMinAnnotatedVar);
}

static void emitMinExprList(MinExprList *node, BEmitterContext *ctx) {
    if (node == NULL)
        return;
    ENTER(emitMinExprList);
    emitMinExp(node->exp, ctx);
    emitMinExprList(node->next, ctx);
    LEAVE(emitMinExprList);
}

static void emitMinPrimApp(MinPrimApp *node, BEmitterContext *ctx) {
    ENTER(emitMinPrimApp);
    emitMinExp(node->exp1, ctx);
    emitMinExp(node->exp2, ctx);
    LEAVE(emitMinPrimApp);
}

static void emitMinApply(MinApply *node, BEmitterContext *ctx) {
    ENTER(emitMinApply);
    emitMinExp(node->function, ctx);
    emitMinExprList(node->args, ctx);
    LEAVE(emitMinApply);
}

static void emitMinIff(MinIff *node, BEmitterContext *ctx) {
    ENTER(emitMinIff);
    emitMinExp(node->condition, ctx);
    emitMinExp(node->consequent, ctx);
    emitMinExp(node->alternative, ctx);
    LEAVE(emitMinIff);
}

static void emitMinCond(MinCond *node, BEmitterContext *ctx) {
    ENTER(emitMinCond);
    emitMinExp(node->value, ctx);
    emitMinCondCases(node->cases, ctx);
    LEAVE(emitMinCond);
}

static void emitMinIntCondCases(MinIntCondCases *node, BEmitterContext *ctx) {
    if (node == NULL)
        return;
    ENTER(emitMinIntCondCases);
    emitMinExp(node->body, ctx);
    emitMinIntCondCases(node->next, ctx);
    LEAVE(emitMinIntCondCases);
}

static void emitMinCharCondCases(MinCharCondCases *node, BEmitterContext *ctx) {
    if (node == NULL)
        return;
    ENTER(emitMinCharCondCases);
    emitMinExp(node->body, ctx);
    emitMinCharCondCases(node->next, ctx);
    LEAVE(emitMinCharCondCases);
}

static void emitMinMatch(MinMatch *node, BEmitterContext *ctx) {
    ENTER(emitMinMatch);
    emitMinExp(node->index, ctx);
    emitMinMatchList(node->cases, ctx);
    LEAVE(emitMinMatch);
}

static void emitMinMatchList(MinMatchList *node, BEmitterContext *ctx) {
    if (node == NULL)
        return;
    ENTER(emitMinMatchList);
    emitMinIntList(node->matches, ctx);
    emitMinExp(node->body, ctx);
    emitMinMatchList(node->next, ctx);
    LEAVE(emitMinMatchList);
}

static void emitMinIntList(MinIntList *node, BEmitterContext *ctx) {
    if (node == NULL)
        return;
    ENTER(emitMinIntList);
    emitMinIntList(node->next, ctx);
    LEAVE(emitMinIntList);
}

static void emitMinLetRec(MinLetRec *node, BEmitterContext *ctx) {
    ENTER(emitMinLetRec);
    emitMinBindings(node->bindings, ctx);
    emitMinExp(node->body, ctx);
    LEAVE(emitMinLetRec);
}

static void emitMinBindings(MinBindings *node, BEmitterContext *ctx) {
    if (node == NULL)
        return;
    ENTER(emitMinBindings);
    emitMinExp(node->val, ctx);
    emitMinBindings(node->next, ctx);
    LEAVE(emitMinBindings);
}

static void emitMinAmb(MinAmb *node, BEmitterContext *ctx) {
    ENTER(emitMinAmb);
    emitMinExp(node->left, ctx);
    emitMinExp(node->right, ctx);
    LEAVE(emitMinAmb);
}

static void emitMinExp(MinExp *node, BEmitterContext *ctx) {
    ENTER(emitMinExp);
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        emitMinAmb(getMinExp_Amb(node), ctx);
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        emitMinApply(getMinExp_Apply(node), ctx);
        break;
    }
    case MINEXP_TYPE_ARGS: {
        // MinExprList
        emitMinExprList(getMinExp_Args(node), ctx);
        break;
    }
    case MINEXP_TYPE_AVAR: {
        // MinAnnotatedVar
        emitMinAnnotatedVar(getMinExp_Avar(node), ctx);
        break;
    }
    case MINEXP_TYPE_VAR: {
        // HashSymbol
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        // MaybeBigInt
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        // character
        break;
    }
    case MINEXP_TYPE_COND: {
        // MinCond
        emitMinCond(getMinExp_Cond(node), ctx);
        break;
    }
    case MINEXP_TYPE_DONE: {
        // int
        break;
    }
    case MINEXP_TYPE_IFF: {
        // MinIff
        emitMinIff(getMinExp_Iff(node), ctx);
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        emitMinLam(getMinExp_Lam(node), ctx);
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        emitMinLetRec(getMinExp_LetRec(node), ctx);
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        emitMinExprList(getMinExp_MakeVec(node), ctx);
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        emitMinMatch(getMinExp_Match(node), ctx);
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        emitMinPrimApp(getMinExp_Prim(node), ctx);
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        emitMinExprList(getMinExp_Sequence(node), ctx);
        break;
    }
    case MINEXP_TYPE_STDINT: {
        // int
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %s", minExpTypeName(node->type));
    }
    LEAVE(emitMinExp);
}

static void emitMinCondCases(MinCondCases *node, BEmitterContext *ctx) {
    ENTER(emitMinCondCases);
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        emitMinIntCondCases(getMinCondCases_Integers(node), ctx);
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        emitMinCharCondCases(getMinCondCases_Characters(node), ctx);
        break;
    }
    default:
        cant_happen("unrecognized MinCondCases type %s",
                    minCondCasesTypeName(node->type));
    }
    LEAVE(emitMinCondCases);
}