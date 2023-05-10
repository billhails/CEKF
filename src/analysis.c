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

void analizeAexpLam(AexpLam *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeAexpLam "); printAexpLam(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    int save = PROTECT(env);
    env = newCTEnv(env);
    UNPROTECT(save);
    save = PROTECT(env);
    AexpVarList *args = x->args;
    while (args != NULL) {
        populateCTEnv(env, args->var);
        args = args->next;
    }
    analizeExp(x->exp, env, depth + 1);
    UNPROTECT(save);
}

AexpAnnotatedVar *analizeAexpVar(AexpVar *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeAexpVar "); printAexpVar(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    int frame;
    int offset;
    if (locate(x, env, &frame, &offset)) {
        return newAexpAnnotatedVar(frame, offset, x);
    }
    cant_happen("no binding for var in analizeAexpVar");
}

void analizeAexpPrimApp(AexpPrimApp *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeAexpPrimApp "); printAexpPrimApp(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeAexp(x->exp1, env, depth + 1);
    analizeAexp(x->exp2, env, depth + 1);
}

void analizeAexpList(AexpList *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeAexpList "); printAexpList(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    while(x != NULL) {
        analizeAexp(x->exp, env, depth + 1);
        x = x->next;
    }
}

void analizeCexpApply(CexpApply *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeCexpApply "); printCexpApply(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeAexp(x->function, env, depth + 1);
    analizeAexpList(x->args, env, depth + 1);
}

void analizeCexpCond(CexpCond *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeCexpCond "); printCexpCond(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeAexp(x->condition, env, depth + 1);
    analizeExp(x->consequent, env, depth + 1);
    analizeExp(x->alternative, env, depth + 1);
}

void analizeCexpLetRec(CexpLetRec *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeCexpLetRec "); printCexpLetRec(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    int save = PROTECT(env);
    env = newCTEnv(env);
    UNPROTECT(save);
    save = PROTECT(env);
    LetRecBindings *bindings = x->bindings;
    while (bindings != NULL) {
        populateCTEnv(env, bindings->var);
        bindings = bindings->next;
    }
    bindings = x->bindings;
    while (bindings != NULL) {
        analizeAexp(bindings->val, env, depth + 1);
        bindings = bindings->next;
    }
    analizeExp(x->body, env, depth + 1);
    UNPROTECT(save);
}

void analizeCexpAmb(CexpAmb *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeCexpAmb "); printCexpAmb(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeExp(x->exp1, env, depth + 1);
    analizeExp(x->exp2, env, depth + 1);
}

void analizeExpLet(ExpLet *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeExpLet "); printExpLet(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    analizeExp(x->val, env, depth + 1);
    int save = PROTECT(env);
    env = newCTEnv(env);
    UNPROTECT(save);
    save = PROTECT(env);
    populateCTEnv(env, x->var);
    analizeExp(x->body, env, depth + 1);
    UNPROTECT(save);
}

void analizeAexp(Aexp *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeAexp "); printAexp(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    switch (x->type) {
        case AEXP_TYPE_LAM:
            analizeAexpLam(x->val.lam, env, depth + 1);
            break;
        case AEXP_TYPE_VAR:
            x->val.annotatedVar = analizeAexpVar(x->val.var, env, depth + 1);
            x->type = AEXP_TYPE_ANNOTATEDVAR;
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            cant_happen("analizeAexp called on annotated var");
            break;
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
            break;
        case AEXP_TYPE_PRIM:
            analizeAexpPrimApp(x->val.prim, env, depth + 1);
            break;
        default:
            cant_happen("unrecognized type in analizeAexp");
    }
}

void analizeCexp(Cexp *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeCexp "); printCexp(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            analizeCexpApply(x->val.apply, env, depth + 1);
            break;
        case CEXP_TYPE_COND:
            analizeCexpCond(x->val.cond, env, depth + 1);
            break;
        case CEXP_TYPE_CALLCC:
            analizeAexp(x->val.callCC, env, depth + 1);
            break;
        case CEXP_TYPE_LETREC:
            analizeCexpLetRec(x->val.letRec, env, depth + 1);
            break;
        case CEXP_TYPE_AMB:
            analizeCexpAmb(x->val.amb, env, depth + 1);
            break;
        case CEXP_TYPE_BACK:
            break;
        default:
            cant_happen("unrecognized type in analizeCexp");
    }
}

void analizeExp(Exp *x, CTEnv *env, int depth) {
#ifdef DEBUG_ANALIZE
    printf("%3d ", depth); printf("analizeExp "); printExp(x); printf("  "); printCTEnv(env); printf("\n");
#endif
    switch (x->type) {
        case EXP_TYPE_AEXP:
            analizeAexp(x->val.aexp, env, depth + 1);
            break;
        case EXP_TYPE_CEXP:
            analizeCexp(x->val.cexp, env, depth + 1);
            break;
        case EXP_TYPE_LET:
            analizeExpLet(x->val.let, env, depth + 1);
            break;
        case EXP_TYPE_DONE:
            break;
        default:
            cant_happen("unrecognized type in analizeAexp");
    }
}

CTEnv *newCTEnv(CTEnv *next) {
    CTEnv *x = NEW(CTEnv, OBJTYPE_CTENV);
    int save = PROTECT(x);
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
            return true;
        }
        (*frame)++;
        env = env->next;
    }
#ifdef DEBUG_ANALIZE
    printf(" FAILED!\n");
#endif
    return false;
}
