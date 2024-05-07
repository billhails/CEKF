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
#include "annotate.h"
#include "anf.h"
#include "types.h"

#ifdef DEBUG_ANNOTATE
#  include "debug.h"
#endif

static bool locate(HashSymbol *var, CTEnv *env, int *frame, int *offset);
static void populateCTEnv(CTEnv *env, HashSymbol *var);

static void annotateExp(Exp *x, CTEnv *env);
static void annotateAexpLam(AexpLam *x, CTEnv *env);
static AexpAnnotatedVar *annotateAexpVar(HashSymbol *x, CTEnv *env);
static void annotateAexpPrimApp(AexpPrimApp *x, CTEnv *env);
static void annotateAexpUnaryApp(AexpUnaryApp *x, CTEnv *env);
static void annotateAexpList(AexpList *x, CTEnv *env);
static void annotateCexpApply(CexpApply *x, CTEnv *env);
static void annotateCexpIf(CexpIf *x, CTEnv *env);
static void annotateCexpCond(CexpCond *x, CTEnv *env);
static void annotateCexpCondCases(CexpCondCases *x, CTEnv *env);
static void annotateCexpLetRec(CexpLetRec *x, CTEnv *env);
static void annotateCexpAmb(CexpAmb *x, CTEnv *env);
static void annotateCexpCut(CexpCut *x, CTEnv *env);
static void annotateExpLet(ExpLet *x, CTEnv *env);
static void annotateAexp(Aexp *x, CTEnv *env);
static void annotateCexp(Cexp *x, CTEnv *env);

static void hashAddCTVar(CTIntTable *table, HashSymbol *var) {
    int count = countCTIntTable(table);
    setCTIntTable(table, var, count);
}

static void annotateAexpLam(AexpLam *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateAexpLam ");
    printAexpLam(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
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
    annotateExp(x->exp, env);
    UNPROTECT(save);
}

static AexpAnnotatedVar *annotateAexpVar(HashSymbol *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateAexpVar ");
    printAexpVar(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    int frame;
    int offset;
    if (locate(x, env, &frame, &offset)) {
        if (frame == 0) {
            return newAexpAnnotatedVar(AEXPANNOTATEDVARTYPE_TYPE_STACK, frame,
                                       offset, x);
        } else {
            return newAexpAnnotatedVar(AEXPANNOTATEDVARTYPE_TYPE_ENV,
                                       frame - 1, offset, x);
        }
    }
    cant_happen("no binding for var '%s' in annotateAexpVar", x->name);
}

static void annotateAexpPrimApp(AexpPrimApp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateAexpPrimApp ");
    printAexpPrimApp(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateAexp(x->exp1, env);
    annotateAexp(x->exp2, env);
}

static void annotateAexpUnaryApp(AexpUnaryApp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateAexpPrimApp ");
    printAexpUnaryApp(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateAexp(x->exp, env);
}

static void annotateAexpList(AexpList *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateAexpList ");
    printAexpList(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    while (x != NULL) {
        annotateAexp(x->exp, env);
        x = x->next;
    }
}

static void annotateCexpApply(CexpApply *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexpApply ");
    printCexpApply(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateAexp(x->function, env);
    annotateAexpList(x->args, env);
}

static void annotateCexpIf(CexpIf *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexpIf ");
    printCexpIf(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateExp(x->consequent, env);
    annotateExp(x->alternative, env);
}

static void annotateCexpCond(CexpCond *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexpCond ");
    printCexpCond(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateCexpCondCases(x->cases, env);
}

static void annotateCexpIntCondCases(CexpIntCondCases *x, CTEnv *env) {
    if (x == NULL)
        return;
    annotateExp(x->body, env);
    annotateCexpIntCondCases(x->next, env);
}

static void annotateCexpCharCondCases(CexpCharCondCases *x, CTEnv *env) {
    if (x == NULL)
        return;
    annotateExp(x->body, env);
    annotateCexpCharCondCases(x->next, env);
}

static void annotateCexpCondCases(CexpCondCases *x, CTEnv *env) {
    if (x == NULL)
        return;
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexpCondCases ");
    printCexpCondCases(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    switch (x->type) {
        case CEXPCONDCASES_TYPE_INTCASES:
            annotateCexpIntCondCases(x->val.intCases, env);
            break;
        case CEXPCONDCASES_TYPE_CHARCASES:
            annotateCexpCharCondCases(x->val.charCases, env);
            break;
        default:
            cant_happen("unrecognised type %d in annotateCexpCondCases",
                        x->type);
    }
}

static void annotateLetRecLam(Aexp *x, CTEnv *env, int letRecOffset) {
    switch (x->type) {
        case AEXP_TYPE_LAM:
            AexpLam *lam = x->val.lam;
            annotateAexpLam(lam, env);
            lam->letRecOffset = letRecOffset;
            break;
        default:
            cant_happen("letrec bindings can only contain lambdas, got %s", aexpTypeName(x->type));
    }
}

static void annotateCexpLetRec(CexpLetRec *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexpLetRec ");
    printCexpLetRec(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
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
    for (int letRecOffset = 0; bindings != NULL; bindings = bindings->next, letRecOffset++) {
        annotateLetRecLam(bindings->val, env, letRecOffset);
    }
    annotateExp(x->body, env);
    UNPROTECT(save);
}

static void annotateCexpAmb(CexpAmb *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexpAmb ");
    printCexpAmb(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateExp(x->exp1, env);
    annotateExp(x->exp2, env);
}

static void annotateCexpCut(CexpCut *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexpCut ");
    printCexpCut(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateExp(x->exp, env);
}

static void annotateExpLet(ExpLet *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateExpLet ");
    printExpLet(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateExp(x->val, env);
    int save = PROTECT(env);
    env = newCTEnv(true, env);
    UNPROTECT(save);
    save = PROTECT(env);
    populateCTEnv(env, x->var);
    annotateExp(x->body, env);
    UNPROTECT(save);
}

static void annotateAexpNameSpaceArray(AexpNameSpaceArray *x, CTEnv *env) {
    for (Index i = 0; i < x->size; ++i) {
        CTEnv *env2 = newCTEnv(true, env);
        int save = PROTECT(env2);
        env2->isNameSpace = true;
        annotateExp(x->entries[i]->body, env2);
        x->entries[i]->nbindings = env2->nbindings;
        UNPROTECT(save);
    }
}

static void annotateAexpNameSpaces(AexpNameSpaces *x, CTEnv *env) {
    annotateAexpNameSpaceArray(x->namespaces, env);
    annotateExp(x->body, env);
}

static void annotateAexpMakeVec(AexpMakeVec *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateAexpMakeVec ");
    printAexpMakeVec(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateAexpList(x->args, env);
}

static void annotateAexp(Aexp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateAexp ");
    printAexp(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    switch (x->type) {
        case AEXP_TYPE_LAM:
            annotateAexpLam(x->val.lam, env);
            break;
        case AEXP_TYPE_VAR:
            x->val.annotatedVar = annotateAexpVar(x->val.var, env);
            x->type = AEXP_TYPE_ANNOTATEDVAR;
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            cant_happen("annotateAexp called on annotated var %s",
                        x->val.annotatedVar->var->name);
            break;
        case AEXP_TYPE_T:
        case AEXP_TYPE_F:
        case AEXP_TYPE_BIGINTEGER:
        case AEXP_TYPE_LITTLEINTEGER:
        case AEXP_TYPE_CHARACTER:
        case AEXP_TYPE_V:
        case AEXP_TYPE_NSREF:
            break;
        case AEXP_TYPE_PRIM:
            annotateAexpPrimApp(x->val.prim, env);
            break;
        case AEXP_TYPE_UNARY:
            annotateAexpUnaryApp(x->val.unary, env);
            break;
        case AEXP_TYPE_MAKEVEC:
            annotateAexpMakeVec(x->val.makeVec, env);
            break;
        case AEXP_TYPE_NAMESPACES:
            annotateAexpNameSpaces(x->val.namespaces, env);
            break;
        default:
            cant_happen("unrecognized type %s in annotateAexp", aexpTypeName(x->type));
    }
}

static void annotateMatchList(MatchList *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateMatchList ");
    printMatchList(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    if (x == NULL)
        return;
    annotateExp(x->body, env);
    annotateMatchList(x->next, env);
}

static void annotateCexpMatch(CexpMatch *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexpMatch ");
    printCexpMatch(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateMatchList(x->clauses, env);
}

static void annotateCexp(Cexp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateCexp ");
    printCexp(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            annotateCexpApply(x->val.apply, env);
            break;
        case CEXP_TYPE_IFF:
            annotateCexpIf(x->val.iff, env);
            break;
        case CEXP_TYPE_COND:
            annotateCexpCond(x->val.cond, env);
            break;
        case CEXP_TYPE_CALLCC:
            annotateAexp(x->val.callCC, env);
            break;
        case CEXP_TYPE_LETREC:
            annotateCexpLetRec(x->val.letRec, env);
            break;
        case CEXP_TYPE_AMB:
            annotateCexpAmb(x->val.amb, env);
            break;
        case CEXP_TYPE_CUT:
            annotateCexpCut(x->val.cut, env);
            break;
        case CEXP_TYPE_MATCH:
            annotateCexpMatch(x->val.match, env);
            break;
        case CEXP_TYPE_BACK:
        case CEXP_TYPE_ERROR:
            break;
        default:
            cant_happen("unrecognized type %d in annotateCexp", x->type);
    }
}

static void annotateExpEnv(CTEnv *env) {
    int nbindings = 0;
    while (env != NULL) {
        nbindings += countCTIntTable(env->table);
        if (env->isNameSpace) {
            env->nbindings = nbindings;
            return;
        }
        env = env->next;
    }
    cant_happen("failed to find namespace env");
}

static void annotateExp(Exp *x, CTEnv *env) {
#ifdef DEBUG_ANALIZE
    eprintf("annotateExp ");
    printExp(x);
    eprintf("  ");
    printCTEnv(env, 0);
    eprintf("\n");
#endif
    switch (x->type) {
        case EXP_TYPE_AEXP:
            annotateAexp(x->val.aexp, env);
            break;
        case EXP_TYPE_CEXP:
            annotateCexp(x->val.cexp, env);
            break;
        case EXP_TYPE_LET:
            annotateExpLet(x->val.let, env);
            break;
        case EXP_TYPE_ENV:
            annotateExpEnv(env);
            break;
        case EXP_TYPE_DONE:
            break;
        default:
            cant_happen("unrecognized type %s", expTypeName(x->type));
    }
}

void addBuiltInsToCTEnv(CTEnv *env, BuiltIns *b) {
    for (Index i = 0; i < b->size; i++) {
        populateCTEnv(env, b->entries[i]->name);
    }
}

void annotateAnf(Exp *x, BuiltIns *b) {
    CTEnv *env = newCTEnv(false, NULL);
    int save = PROTECT(env);
    addBuiltInsToCTEnv(env, b);
    env = newCTEnv(false, env);
    REPLACE_PROTECT(save, env);
    annotateExp(x, env);
    UNPROTECT(save);
}

static void populateCTEnv(CTEnv *env, HashSymbol *var) {
    hashAddCTVar(env->table, var);
}

static int calculateAdjustment(CTEnv *env) {
    int adjustment = 0;
    while (env != NULL) {
        if (env->isLocal) {
            if (env->next) {
                adjustment += countCTIntTable(env->next->table);
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
    eprintf("locate ");
    printAexpVar(var);
    eprintf(" in ");
    printCTEnv(env, 0);
#endif
    *frame = 0;
    while (env != NULL) {
        if (getCTIntTable(env->table, var, offset)) {
#ifdef DEBUG_ANALIZE
            eprintf(" -> [%d:%d]\n", *frame, *offset);
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
    eprintf(" FAILED!\n");
#endif
    return false;
}
