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
#include "anf.h"

// #define DEBUG_ANALIZE

#ifdef DEBUG_ANALIZE
#include "debug.h"
#include "anf_debug.h"
#include "cte_debug.h"

static bool debugging = false;
#endif

static bool locate(HashSymbol *var, CTEnv *env, int *frame, CTEntry **offset);
static bool lookUpInCTEnv(HashSymbol *var, CTEnv *env, CTEntry **entry);
static void populateCTEnv(CTEnv *env, HashSymbol *var, CTEnv *context);

static CTEnv *analizeAexpLam(AexpLam *x, CTEnv *env);
static AexpAnnotatedVar *analizeAexpVar(HashSymbol *x, CTEnv *env);
static CTEnv *analizeAexpPrimApp(AexpPrimApp *x, CTEnv *env);
static CTEnv *analizeAexpUnaryApp(AexpUnaryApp *x, CTEnv *env);
static CTEnv *analizeAexpList(AexpList *x, CTEnv *env);
static CTEnv *analizeCexpApply(CexpApply *x, CTEnv *env);
static CTEnv *analizeCexpIf(CexpIf *x, CTEnv *env);
static CTEnv *analizeCexpCond(CexpCond *x, CTEnv *env);
static CTEnv *analizeCexpEnv(CexpEnv *x, CTEnv *env);
static CTEnv *analizeCexpCondCases(CexpCondCases *x, CTEnv *env);
static CTEnv *analizeCexpLetRec(CexpLetRec *x, CTEnv *env);
static CTEnv *analizeCexpAmb(CexpAmb *x, CTEnv *env);
static CTEnv *analizeCexpCut(CexpCut *x, CTEnv *env);
static CTEnv *analizeExpLet(ExpLet *x, CTEnv *env);
static CTEnv *analizeAexp(Aexp *x, CTEnv *env);
static CTEnv *analizeCexp(Cexp *x, CTEnv *env);
static CTEnv *getEnvFromEnv(HashSymbol *var, CTEnv *env);
static CTEnv *makeCTEnv(bool isLocal, CTEnv *parent);

static void hashAddCTVar(HashTable *table, HashSymbol *var, CTEnv *context) {
    struct CTEntry *entry = newCTEntry(table->count, context);
    int save = PROTECT(entry);
    hashSet(table, var, &entry);
    UNPROTECT(save);
}

static CTEnv *analizeAexpLam(AexpLam *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeAexpLam "); printAexpLam(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    env = makeCTEnv(false, env);
    int save = PROTECT(env);
    AexpVarList *args = x->args;
    while (args != NULL) {
        populateCTEnv(env, args->var, NULL);
        args = args->next;
    }
    CTEnv *res = analizeExp(x->exp, env);
    UNPROTECT(save);
    return res;
}

static AexpAnnotatedVar *analizeAexpVar(HashSymbol *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeAexpVar "); eprintf("%s", x->name); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    int frame;
    CTEntry *offset = NULL;
    if (locate(x, env, &frame, &offset)) {
        AexpAnnotatedVar *result;
        int save = PROTECT(offset);
        if (frame == 0) {
            result = newAexpAnnotatedVar(AEXPANNOTATEDVARTYPE_TYPE_STACK, frame, offset->location, x);
        } else {
            result = newAexpAnnotatedVar(AEXPANNOTATEDVARTYPE_TYPE_ENV, frame - 1, offset->location, x);
        }
        UNPROTECT(save);
        return result;
    }
    cant_happen("no binding for var '%s' in analizeAexpVar", x->name);
}

static HashSymbol *getDotLhs(Aexp *aexp) {
    if (aexp->type != AEXP_TYPE_ANNOTATEDVAR) {
        cant_happen("lhs of dot operator must be an env");
    }
    return aexp->val.annotatedVar->var;
}

static CTEnv *analizeAexpPrimApp(AexpPrimApp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeAexpPrimApp "); printAexpPrimApp(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    if (x->type == AEXPPRIMOP_TYPE_DOT) {
#ifdef DEBUG_ANALIZE
        debugging = true;
#endif
        analizeAexp(x->exp1, env);
        HashSymbol *var = getDotLhs(x->exp1);
        CTEnv *context = getEnvFromEnv(var, env);
        analizeAexp(x->exp2, context);
#ifdef DEBUG_ANALIZE
        debugging = false;
#endif
    } else {
        analizeAexp(x->exp1, env);
        analizeAexp(x->exp2, env);
    }
    return env;
}

static CTEnv *analizeAexpUnaryApp(AexpUnaryApp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeAexpPrimApp "); printAexpUnaryApp(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    analizeAexp(x->exp, env);
    return env;
}

static CTEnv *analizeAexpList(AexpList *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeAexpList "); printAexpList(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    while(x != NULL) {
        analizeAexp(x->exp, env);
        x = x->next;
    }
    return env;
}

static CTEnv *analizeCexpApply(CexpApply *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexpApply "); printCexpApply(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    CTEnv *res = analizeAexp(x->function, env);
    analizeAexpList(x->args, env);
    return res;
}

static CTEnv *analizeCexpIf(CexpIf *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexpIf "); printCexpIf(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    analizeAexp(x->condition, env);
    analizeExp(x->consequent, env);
    analizeExp(x->alternative, env);
    return env;
}

static CTEnv *analizeCexpCond(CexpCond *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexpCond "); printCexpCond(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    analizeAexp(x->condition, env);
    analizeCexpCondCases(x->cases, env);
    return env;
}

static CTEnv *getEnvFromEnv(HashSymbol *var, CTEnv *env) {
    CTEntry *entry;
    if (!lookUpInCTEnv(var, env, &entry)) {
        cant_happen("cannot locate environment %s", var->name);
    }
    if (entry->env == NULL) {
        cant_happen("%s is not an environment", var->name);
    }
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("getEnvFromEnv "); printHashSymbol(var); eprintf(" found "); printCTEntry(entry, 0); eprintf("\n"); }
#endif
    return entry->env;
}

static CTEnv *walkExtendsPath(AexpVarList *path, CTEnv *env) {
    if (path == NULL) {
        return env;
    }
    CTEnv *context = getEnvFromEnv(path->var, env);
    return walkExtendsPath(path->next, context);
}

static CTEnv *analizeCexpEnv(CexpEnv *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("analizeCexpEnv "); printCexpEnv(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n");
#endif
    CTEnv *context = walkExtendsPath(x->context, env);
    return analizeExp(x->body, context);
}

static CTEnv *analizeCexpIntCondCases(CexpIntCondCases *x, CTEnv *env) {
    if (x == NULL) return env;
    analizeExp(x->body, env);
    analizeCexpIntCondCases(x->next, env);
    return env;
}

static CTEnv *analizeCexpCharCondCases(CexpCharCondCases *x, CTEnv *env) {
    if (x == NULL) return env;
    analizeExp(x->body, env);
    analizeCexpCharCondCases(x->next, env);
    return env;
}

static CTEnv *analizeCexpCondCases(CexpCondCases *x, CTEnv *env) {
    if (x == NULL) return env;
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexpCondCases "); printCexpCondCases(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    switch (x->type) {
        case CEXPCONDCASES_TYPE_INTCASES:
            return analizeCexpIntCondCases(x->val.intCases, env);
        case CEXPCONDCASES_TYPE_CHARCASES:
            return analizeCexpCharCondCases(x->val.charCases, env);
        default:
            cant_happen("unrecognised type %d in analizeCexpCondCases", x->type);
    }
}

static CTEnv *analizeLetRecLam(Aexp *x, CTEnv *env, int letRecOffset) {
    switch (x->type) {
        case AEXP_TYPE_LAM:
            AexpLam *lam = x->val.lam;
            analizeAexpLam(lam, env);
            lam->letRecOffset = letRecOffset;
            break;
        default:
            cant_happen("letrec bindings can only contain lambdas");
    }
    return env;
}

static CTEnv *analizeCexpLetRec(CexpLetRec *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexpLetRec "); printCexpLetRec(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    env = makeCTEnv(true, env);
    int save = PROTECT(env);
    LetRecBindings *bindings = x->bindings;
    while (bindings != NULL) {
        populateCTEnv(env, bindings->var, NULL);
        bindings = bindings->next;
    }
    bindings = x->bindings;
    int letRecOffset = 0;
    while (bindings != NULL) {
        analizeLetRecLam(bindings->val, env, letRecOffset);
        bindings = bindings->next;
        letRecOffset++;
    }
    CTEnv *res = analizeExp(x->body, env);
    UNPROTECT(save);
    return res;
}

static CTEnv *analizeCexpAmb(CexpAmb *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexpAmb "); printCexpAmb(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    analizeExp(x->exp1, env);
    analizeExp(x->exp2, env);
    return env;
}

static CTEnv *analizeCexpCut(CexpCut *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexpCut "); printCexpCut(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    return analizeExp(x->exp, env);
}

static bool expIsEnv(Exp *x) {
    return x->type == EXP_TYPE_CEXP && x->val.cexp->type == CEXP_TYPE_ENV;
}

static CTEnv *analizeExpLet(ExpLet *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeExpLet "); printExpLet(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    int save = PROTECT(env);
#ifdef DEBUG_ANALIZE
    if (expIsEnv(x->val)) { debugging = true; eprintf(">>>>>>>>>>>>>>>>>>>>\n"); }
#endif
    CTEnv *candidate = analizeExp(x->val, env);
#ifdef DEBUG_ANALIZE
    if (expIsEnv(x->val)) { debugging = false; eprintf("<<<<<<<<<<<<<<<<<<<<\n"); }
#endif
    PROTECT(candidate);
#ifdef DEBUG_ANALIZE
    if (expIsEnv(x->val)) { eprintf("candidate "); printHashSymbol(x->var); eprintf(" => "); printCTEnv(candidate, 0); eprintf("\n"); }
#endif
    env = makeCTEnv(true, env);
    PROTECT(env);
    populateCTEnv(env, x->var, expIsEnv(x->val) ? candidate : NULL);
    CTEnv *res = analizeExp(x->body, env);
    UNPROTECT(save);
    return res;
}

static CTEnv *analizeAexpMakeVec(AexpMakeVec *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeAexpMakeVec "); printAexpMakeVec(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    return analizeAexpList(x->args, env);
}

static CTEnv *analizeAexp(Aexp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeAexp "); printAexp(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    switch (x->type) {
        case AEXP_TYPE_LAM:
            return analizeAexpLam(x->val.lam, env);
        case AEXP_TYPE_VAR:
            x->val.annotatedVar = analizeAexpVar(x->val.var, env);
            x->type = AEXP_TYPE_ANNOTATEDVAR;
            return env;
        case AEXP_TYPE_ANNOTATEDVAR:
            cant_happen("analizeAexp called on annotated var %s", x->val.annotatedVar->var->name);
        case AEXP_TYPE_T:
        case AEXP_TYPE_F:
        case AEXP_TYPE_BIGINTEGER:
        case AEXP_TYPE_LITTLEINTEGER:
        case AEXP_TYPE_CHARACTER:
        case AEXP_TYPE_V:
            return env;
        case AEXP_TYPE_GETENV:
#ifdef DEBUG_ANALIZE
    eprintf("analizeAexp "); printAexp(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n");
#endif
            return env;
        case AEXP_TYPE_PRIM:
            return analizeAexpPrimApp(x->val.prim, env);
        case AEXP_TYPE_UNARY:
            return analizeAexpUnaryApp(x->val.unary, env);
        case AEXP_TYPE_MAKEVEC:
            return analizeAexpMakeVec(x->val.makeVec, env);
        default:
            cant_happen("unrecognized type %d in analizeAexp", x->type);
    }
}

static CTEnv *analizeMatchList(MatchList *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeMatchList "); printMatchList(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    if (x == NULL) return env;
    analizeExp(x->body, env);
    return analizeMatchList(x->next, env);
}

static CTEnv *analizeCexpMatch(CexpMatch *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexpMatch "); printCexpMatch(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    analizeAexp(x->condition, env);
    return analizeMatchList(x->clauses, env);
}

static CTEnv *analizeCexp(Cexp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeCexp "); printCexp(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            return analizeCexpApply(x->val.apply, env);
        case CEXP_TYPE_IFF:
            return analizeCexpIf(x->val.iff, env);
        case CEXP_TYPE_COND:
            return analizeCexpCond(x->val.cond, env);
        case CEXP_TYPE_CALLCC:
            return analizeAexp(x->val.callCC, env);
        case CEXP_TYPE_LETREC:
            return analizeCexpLetRec(x->val.letRec, env);
        case CEXP_TYPE_AMB:
            return analizeCexpAmb(x->val.amb, env);
        case CEXP_TYPE_CUT:
            return analizeCexpCut(x->val.cut, env);
        case CEXP_TYPE_MATCH:
            return analizeCexpMatch(x->val.match, env);
        case CEXP_TYPE_ENV:
            return analizeCexpEnv(x->val.env, env);
        case CEXP_TYPE_BACK:
        case CEXP_TYPE_ERROR:
            return env;
        default:
            cant_happen("unrecognized type %d in analizeCexp", x->type);
    }
}

CTEnv *analizeExp(Exp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("analizeExp "); printExp(x, 0); eprintf("  "); printCTEnv(env, 0); eprintf("\n"); }
#endif
    CTEnv *result = NULL;
    int save = -1;
    if (env == NULL) {
        env = makeCTEnv(false, NULL);
        save = PROTECT(env);
    }
    switch (x->type) {
        case EXP_TYPE_AEXP:
            result = analizeAexp(x->val.aexp, env);
            break;
        case EXP_TYPE_CEXP:
            result = analizeCexp(x->val.cexp, env);
            break;
        case EXP_TYPE_LET:
            result = analizeExpLet(x->val.let, env);
            break;
        case EXP_TYPE_DONE:
            result = env;
            break;
        default:
            cant_happen("unrecognized type in analizeExp");
    }
    if (save != -1) {
        UNPROTECT(save);
    }
    return result;
}

static CTEnv *makeCTEnv(bool isLocal, CTEnv *parent) {
    HashTable *table = newCTHashTable();
    int save = PROTECT(table);
    CTEnv *env = newCTEnv(isLocal, table, parent);
    UNPROTECT(save);
    return env;
}

static void populateCTEnv(CTEnv *env, HashSymbol *var, CTEnv *context) {
    hashAddCTVar(env->table, var, context);
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

static bool lookUpInCTEnv(HashSymbol *var, CTEnv *env, CTEntry **entry) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("lookUpInCTEnv "); eprintf("%s\n", var->name); }
#endif
    while (env != NULL) {
        if (hashGet(env->table, var, entry)) {
            return true;
        }
        env = env->next;
    }
    return false;
}

static bool locate(HashSymbol *var, CTEnv *env, int *frame, CTEntry **entry) {
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf("locate "); eprintf("%s", var->name); eprintf(" in "); printCTEnv(env, 0); }
#endif
    CTEntry *localEntry = NULL;
    *frame = 0;
    while (env != NULL) {
        if (hashGet(env->table, var, &localEntry)) {
#ifdef DEBUG_ANALIZE
            if (debugging) { eprintf(" -> [%d:%d]\n", *frame, localEntry->location); }
#endif
            *entry = newCTEntry(localEntry->location, localEntry->env);
            (*entry)->location += calculateAdjustment(env);
            return true;
        }
        if (!env->isLocal) {
            (*frame)++;
        }
        env = env->next;
    }
#ifdef DEBUG_ANALIZE
    if (debugging) { eprintf(" FAILED!\n"); }
#endif
    return false;
}
