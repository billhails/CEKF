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
 */

#include <stdbool.h>
#include "common.h"
#include "analysis.h"
#include "exp.h"

// #define DEBUG_ANALIZE

#ifdef DEBUG_ANALIZE
#include <stdio.h>
#include "debug.h"
#endif

static bool locate(AexpVar *var, CTEnv *env, int *frame, int *offset);
static void populateCTEnv(CTEnv *env, AexpVar *var);

static void analizeAexpLam(AexpLam *x, CTEnv *env);
static AexpAnnotatedVar *analizeAexpVar(AexpVar *x, CTEnv *env);
static void analizeAexpPrimApp(AexpPrimApp *x, CTEnv *env);
static void analizeAexpUnaryApp(AexpUnaryApp *x, CTEnv *env);
static void analizeAexpList(AexpList *x, CTEnv *env);
static void analizeCexpApply(CexpApply *x, CTEnv *env);
static void analizeCexpCond(CexpCond *x, CTEnv *env);
static void analizeCexpLetRec(CexpLetRec *x, CTEnv *env);
static void analizeCexpAmb(CexpAmb *x, CTEnv *env);
static void analizeExpLet(ExpLet *x, CTEnv *env);
static void analizeAexp(Aexp *x, CTEnv *env);
static void analizeCexp(Cexp *x, CTEnv *env);

static void analizeAexpLam(AexpLam *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeAexpLam "); printAexpLam(x); printf("  "); printCTEnv(env); printf("\n");
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

static AexpAnnotatedVar *analizeAexpVar(AexpVar *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeAexpVar "); printAexpVar(x); printf("  "); printCTEnv(env); printf("\n");
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
    cant_happen("no binding for var in analizeAexpVar");
}

static void analizeAexpPrimApp(AexpPrimApp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeAexpPrimApp "); printAexpPrimApp(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeAexp(x->exp1, env);
    analizeAexp(x->exp2, env);
}

static void analizeAexpUnaryApp(AexpUnaryApp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeAexpPrimApp "); printAexpUnaryApp(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeAexp(x->exp, env);
}

static void analizeAexpList(AexpList *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeAexpList "); printAexpList(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    while(x != NULL) {
        analizeAexp(x->exp, env);
        x = x->next;
    }
}

static void analizeCexpApply(CexpApply *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeCexpApply "); printCexpApply(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeAexp(x->function, env);
    analizeAexpList(x->args, env);
}

static void analizeCexpCond(CexpCond *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeCexpCond "); printCexpCond(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeAexp(x->condition, env);
    analizeExp(x->consequent, env);
    analizeExp(x->alternative, env);
}

static void analizeLetRecLam(Aexp *x, CTEnv *env, int letRecOffset) {
    if (x->type != AEXP_TYPE_LAM) {
        cant_happen("non-lambda value (%d) for letrec in AnalizeLetRecLam");
    }
    AexpLam *lam = x->val.lam;
    analizeAexpLam(lam, env);
    lam->letRecOffset = letRecOffset;
}

static void analizeCexpLetRec(CexpLetRec *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeCexpLetRec "); printCexpLetRec(x); printf("  "); printCTEnv(env); printf("\n");
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
    printf("analizeCexpAmb "); printCexpAmb(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeExp(x->exp1, env);
    analizeExp(x->exp2, env);
}

static void analizeExpLet(ExpLet *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeExpLet "); printExpLet(x); printf("  "); printCTEnv(env); printf("\n");
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

static void analizeAexp(Aexp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeAexp "); printAexp(x); printf("  "); printCTEnv(env); printf("\n");
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
            cant_happen("analizeAexp called on annotated var");
            break;
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
        case AEXP_TYPE_VOID:
            break;
        case AEXP_TYPE_PRIM:
            analizeAexpPrimApp(x->val.prim, env);
            break;
        case AEXP_TYPE_UNARY:
            analizeAexpUnaryApp(x->val.unary, env);
            break;
        default:
            cant_happen("unrecognized type in analizeAexp");
    }
}

static void analizeCexp(Cexp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeCexp "); printCexp(x); printf("  "); printCTEnv(env); printf("\n");
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
        case CEXP_TYPE_BACK:
            break;
        default:
            cant_happen("unrecognized type in analizeCexp");
    }
}

void analizeExp(Exp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    printf("analizeExp "); printExp(x); printf("  "); printCTEnv(env); printf("\n");
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
            cant_happen("unrecognized type in analizeAexp");
    }
    if (save != -1) {
        UNPROTECT(save);
    }
}

CTEnv *newCTEnv(bool isLocal, CTEnv *next) {
    CTEnv *x = NEW(CTEnv, OBJTYPE_CTENV);
    int save = PROTECT(x);
    x->isLocal = isLocal;
    x->next = next;
    x->table = NULL;
    x->table = newHashTable();
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

static void populateCTEnv(CTEnv *env, AexpVar *var) {
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

static bool locate(AexpVar *var, CTEnv *env, int *frame, int *offset) {
#ifdef DEBUG_ANALIZE
    printf("locate ");
    printAexpVar(var);
    printf(" in ");
    printCTEnv(env);
#endif
    *frame = 0;
    while (env != NULL) {
        if (hashLocate(env->table, var, offset)) {
#ifdef DEBUG_ANALIZE
            printf(" -> [%d:%d]\n", *frame, *offset);
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
    printf(" FAILED!\n");
#endif
    return false;
}
