/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2023  Bill Hails
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
 * This file contains code for performing static lexical analysis
 * of variables, annotating those variables with their run-time locations.
 */

#include <stdbool.h>
#include <stdio.h>

#include "common.h"
#include "analysis.h"
#include "exp.h"

// #define DEBUG_ANALIZE

#ifdef DEBUG_ANALIZE
#include "debug.h"
#endif

static bool locate(HashSymbol *var, CTEnv *env, int *frame, int *offset);
static void populateCTEnv(CTEnv *env, HashSymbol *var);

static void analizeAexpLam(AexpLam *x, CTEnv *env);
static AexpAnnotatedVar *analizeAexpVar(HashSymbol *x, CTEnv *env);
static void analizeAexpPrimApp(AexpPrimApp *x, CTEnv *env);
static void analizeAexpUnaryApp(AexpUnaryApp *x, CTEnv *env);
static void analizeAexpList(AexpList *x, CTEnv *env);
static void analizeCexpApply(CexpApply *x, CTEnv *env);
static void analizeCexpCond(CexpCond *x, CTEnv *env);
static void analizeCexpLetRec(CexpLetRec *x, CTEnv *env);
static void analizeCexpAmb(CexpAmb *x, CTEnv *env);
static void analizeCexpCut(CexpCut *x, CTEnv *env);
static void analizeExpLet(ExpLet *x, CTEnv *env);
static void analizeAexp(Aexp *x, CTEnv *env);
static void analizeCexp(Cexp *x, CTEnv *env);

static void hashAddCTVar(HashTable *table, HashSymbol *var) {
    int count = table->count;
    hashSet(table, var, &count);
}

static void analizeAexpLam(AexpLam *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeAexpLam "); printAexpLam(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    int save = PROTECT(env);
    env = newCTEnv(false, env);
    UNPROTECT(save);
    save = PROTECT(env);
    AexpVarList *args = x->args;
    while (args != NULL) {
        populateCTEnv(env, args->var);
        args = args->next;
    }
    analizeExp(x->exp, env);
    UNPROTECT(save);
}

static AexpAnnotatedVar *analizeAexpVar(HashSymbol *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeAexpVar "); printAexpVar(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    int frame;
    int offset;
    if (locate(x, env, &frame, &offset)) {
        if (frame == 0) {
            return newAexpAnnotatedVar(VAR_TYPE_STACK, frame, offset, x);
        } else {
            return newAexpAnnotatedVar(VAR_TYPE_ENV, frame - 1, offset, x);
        }
    }
    cant_happen("no binding for var '%s' in analizeAexpVar", x->name);
}

static void analizeAexpPrimApp(AexpPrimApp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeAexpPrimApp "); printAexpPrimApp(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeAexp(x->exp1, env);
    analizeAexp(x->exp2, env);
}

static void analizeAexpUnaryApp(AexpUnaryApp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeAexpPrimApp "); printAexpUnaryApp(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeAexp(x->exp, env);
}

static void analizeAexpList(AexpList *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeAexpList "); printAexpList(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    while(x != NULL) {
        analizeAexp(x->exp, env);
        x = x->next;
    }
}

static void analizeCexpApply(CexpApply *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeCexpApply "); printCexpApply(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeAexp(x->function, env);
    analizeAexpList(x->args, env);
}

static void analizeCexpCond(CexpCond *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeCexpCond "); printCexpCond(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeAexp(x->condition, env);
    analizeExp(x->consequent, env);
    analizeExp(x->alternative, env);
}

static void analizeLetRecLam(Aexp *x, CTEnv *env, int letRecOffset) {
    switch (x->type) {
        case AEXP_TYPE_LAM:
            AexpLam *lam = x->val.lam;
            analizeAexpLam(lam, env);
            lam->letRecOffset = letRecOffset;
            break;
        default:
            cant_happen("letrec bindings can only contain lambdas");
    }
}

static void analizeCexpLetRec(CexpLetRec *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeCexpLetRec "); printCexpLetRec(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    int save = PROTECT(env);
    env = newCTEnv(true, env);
    UNPROTECT(save);
    save = PROTECT(env);
    LetRecBindings *bindings = x->bindings;
    while (bindings != NULL) {
        populateCTEnv(env, bindings->var);
        bindings = bindings->next;
    }
    bindings = x->bindings;
    int letRecOffset = 0;
    while (bindings != NULL) {
        analizeLetRecLam(bindings->val, env, letRecOffset);
        bindings = bindings->next;
        letRecOffset++;
    }
    analizeExp(x->body, env);
    UNPROTECT(save);
}

static void analizeCexpAmb(CexpAmb *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeCexpAmb "); printCexpAmb(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeExp(x->exp1, env);
    analizeExp(x->exp2, env);
}

static void analizeCexpCut(CexpCut *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeCexpCut "); printCexpCut(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeExp(x->exp, env);
}

static void analizeExpLet(ExpLet *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeExpLet "); printExpLet(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeExp(x->val, env);
    int save = PROTECT(env);
    env = newCTEnv(true, env);
    UNPROTECT(save);
    save = PROTECT(env);
    populateCTEnv(env, x->var);
    analizeExp(x->body, env);
    UNPROTECT(save);
}

static void analizeAexpMakeVec(AexpMakeVec *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeAexpMakeVec "); printAexpMakeVec(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeAexpList(x->args, env);
}

static void analizeAexp(Aexp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeAexp "); printAexp(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    switch (x->type) {
        case AEXP_TYPE_LAM:
            analizeAexpLam(x->val.lam, env);
            break;
        case AEXP_TYPE_VAR:
            x->val.annotatedVar = analizeAexpVar(x->val.var, env);
            x->type = AEXP_TYPE_ANNOTATEDVAR;
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            cant_happen("analizeAexp called on annotated var %s", x->val.annotatedVar->var->name);
            break;
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
        case AEXP_TYPE_CHAR:
        case AEXP_TYPE_DEFAULT:
        case AEXP_TYPE_VOID:
            break;
        case AEXP_TYPE_PRIM:
            analizeAexpPrimApp(x->val.prim, env);
            break;
        case AEXP_TYPE_UNARY:
            analizeAexpUnaryApp(x->val.unary, env);
            break;
        case AEXP_TYPE_MAKEVEC:
            analizeAexpMakeVec(x->val.makeVec, env);
            break;
        default:
            cant_happen("unrecognized type %d in analizeAexp", x->type);
    }
}

static void analizeMatchList(MatchList *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeMatchList "); printMatchList(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    if (x == NULL) return;
    analizeAexpList(x->matches, env);
    analizeExp(x->body, env);
    analizeMatchList(x->next, env);
}

static void analizeCexpMatch(CexpMatch *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeCexpMatch "); printCexpMatch(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    analizeAexp(x->condition, env);
    analizeMatchList(x->clauses, env);
}

static void analizeCexp(Cexp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeCexp "); printCexp(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            analizeCexpApply(x->val.apply, env);
            break;
        case CEXP_TYPE_COND:
            analizeCexpCond(x->val.cond, env);
            break;
        case CEXP_TYPE_CALLCC:
            analizeAexp(x->val.callCC, env);
            break;
        case CEXP_TYPE_LETREC:
            analizeCexpLetRec(x->val.letRec, env);
            break;
        case CEXP_TYPE_AMB:
            analizeCexpAmb(x->val.amb, env);
            break;
        case CEXP_TYPE_CUT:
            analizeCexpCut(x->val.cut, env);
            break;
        case CEXP_TYPE_MATCH:
            analizeCexpMatch(x->val.match, env);
            break;
        case CEXP_TYPE_BACK:
        case CEXP_TYPE_ERROR:
            break;
        default:
            cant_happen("unrecognized type %d in analizeCexp", x->type);
    }
}

void analizeExp(Exp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "analizeExp "); printExp(x); fprintf(stderr, "  "); printCTEnv(env); fprintf(stderr, "\n");
#endif
    int save = -1;
    if (env == NULL) {
        env = newCTEnv(false, NULL);
        save = PROTECT(env);
    }
    switch (x->type) {
        case EXP_TYPE_AEXP:
            analizeAexp(x->val.aexp, env);
            break;
        case EXP_TYPE_CEXP:
            analizeCexp(x->val.cexp, env);
            break;
        case EXP_TYPE_LET:
            analizeExpLet(x->val.let, env);
            break;
        case EXP_TYPE_DONE:
            break;
        default:
            cant_happen("unrecognized type in analizeExp");
    }
    if (save != -1) {
        UNPROTECT(save);
    }
}

static void printCTHashTableValue(void *intptr, int depth) {
    fprintf(stderr, "%d", *((int *)intptr));
}

CTEnv *newCTEnv(bool isLocal, CTEnv *next) {
    CTEnv *x = NEW(CTEnv, OBJTYPE_CTENV);
    int save = PROTECT(x);
    x->isLocal = isLocal;
    x->next = next;
    x->table = NULL;
    x->table = newHashTable(sizeof(int), NULL, printCTHashTableValue);
    validateLastAlloc();
    UNPROTECT(save);
    return x;
}

void markCTEnv(Header *h) {
    CTEnv *env = (CTEnv *) h;
    if (env == NULL) return;
    if (MARKED(env)) return;
    MARK(env);
    markHashTableObj((Header *)env->table);
}

void freeCTEnv(Header *h) {
    reallocate((void *)h, sizeof(CTEnv), 0);
}

static void populateCTEnv(CTEnv *env, HashSymbol *var) {
    hashAddCTVar(env->table, var);
}

static int calculateAdjustment(CTEnv *env) {
    int adjustment = 0;
    while (env != NULL) {
        if (env->isLocal) {
            if (env->next) {
                adjustment += env->next->table->count;
            }
            env = env->next;
        } else {
            return adjustment;
        }
    }
    return adjustment;
}

static bool locate(HashSymbol *var, CTEnv *env, int *frame, int *offset) {
#ifdef DEBUG_ANALIZE
    fprintf(stderr, "locate ");
    printAexpVar(var);
    fprintf(stderr, " in ");
    printCTEnv(env);
#endif
    *frame = 0;
    while (env != NULL) {
        if (hashGet(env->table, var, offset)) {
#ifdef DEBUG_ANALIZE
            fprintf(stderr, " -> [%d:%d]\n", *frame, *offset);
#endif
            *offset += calculateAdjustment(env);
            return true;
        }
        if (!env->isLocal) {
            (*frame)++;
        }
        env = env->next;
    }
#ifdef DEBUG_ANALIZE
    fprintf(stderr, " FAILED!\n");
#endif
    return false;
}
