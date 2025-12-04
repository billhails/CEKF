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
#include "anf_pp.h"
#include "types.h"
#include "symbol.h"
#include "lambda_helper.h"

#ifdef DEBUG_ANNOTATE
#  include "debug.h"
#  include "anf_debug.h"
#  include "anf_pp.h"
#endif

static bool locate(HashSymbol *var, CTEnv *env, int *frame, int *offset);
static void populateCTEnv(CTEnv *env, HashSymbol *var);

static CTEnv *annotateExp(AnfExp *x, CTEnv *env);
static CTEnv *annotateAexpLam(AexpLam *x, CTEnv *env);
static AexpAnnotatedVar *annotateAexpVar(ParserInfo I, HashSymbol *x, CTEnv *env);
static CTEnv *annotateAexpPrimApp(AexpPrimApp *x, CTEnv *env);
static CTEnv *annotateAexpList(AexpList *x, CTEnv *env);
static CTEnv *annotateCexpApply(CexpApply *x, CTEnv *env);
static CTEnv *annotateCexpIf(CexpIf *x, CTEnv *env);
static CTEnv *annotateCexpCond(CexpCond *x, CTEnv *env);
static CTEnv *annotateCexpCondCases(CexpCondCases *x, CTEnv *env);
static CTEnv *annotateCexpLetRec(CexpLetRec *x, CTEnv *env);
static CTEnv *annotateCexpAmb(CexpAmb *x, CTEnv *env);
static CTEnv *annotateCexpCut(CexpCut *x, CTEnv *env);
static CTEnv *annotateExpLet(ExpLet *x, CTEnv *env);
static CTEnv *annotateAexp(Aexp *x, CTEnv *env);
static CTEnv *annotateCexp(Cexp *x, CTEnv *env);
static CTEnvArray *getNsEnvs(CTEnv *env);

static void hashAddCTVar(CTIntTable *table, HashSymbol *var) {
    int count = countCTIntTable(table);
    setCTIntTable(table, var, count);
}

static CTEnv *annotateAexpLam(AexpLam *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpLam ");
    ppAexpLam(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    int save = PROTECT(env);
    env = newCTEnv(CPI(x), false, env);
    UNPROTECT(save);
    save = PROTECT(env);
    AexpVarList *args = x->args;
    while (args != NULL) {
        populateCTEnv(env, args->var);
        args = args->next;
    }
    annotateExp(x->exp, env);
    UNPROTECT(save);
    return env;
}

static AexpAnnotatedVar *annotateAexpVar(ParserInfo I, HashSymbol *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpVar ");
    ppAexpVar(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    int frame;
    int offset;
    if (locate(x, env, &frame, &offset)) {
        if (frame == 0) {
            return newAexpAnnotatedVar(I, AEXPANNOTATEDVARTYPE_TYPE_STACK, frame,
                                       offset, x);
        } else {
            return newAexpAnnotatedVar(I, AEXPANNOTATEDVARTYPE_TYPE_ENV,
                                       frame - 1, offset, x);
        }
    }
    cant_happen("no binding for var '%s' in annotateAexpVar", x->name);
}

static CTEnv *annotateAexpPrimApp(AexpPrimApp *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpPrimApp ");
    ppAexpPrimApp(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->exp1, env);
    annotateAexp(x->exp2, env);
    return env;
}

static CTEnv *annotateAexpList(AexpList *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpList ");
    ppAexpList(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    CTEnv *res;
    while (x != NULL) {
        res = annotateAexp(x->exp, env);
        x = x->next;
    }
    return res;
}

static CTEnv *annotateCexpApply(CexpApply *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpApply ");
    ppCexpApply(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->function, env);
    annotateAexpList(x->args, env);
    return env;
}

static CTEnv *annotateCexpIf(CexpIf *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpIf ");
    ppCexpIf(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateExp(x->consequent, env);
    annotateExp(x->alternative, env);
    return env;
}

static CTEnv *annotateCexpCond(CexpCond *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpCond ");
    ppCexpCond(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateCexpCondCases(x->cases, env);
    return env;
}

static CTEnv *annotateCexpIntCondCases(CexpIntCondCases *x, CTEnv *env) {
    if (x == NULL)
        return env;
    annotateExp(x->body, env);
    annotateCexpIntCondCases(x->next, env);
    return env;
}

static CTEnv *annotateCexpCharCondCases(CexpCharCondCases *x, CTEnv *env) {
    if (x == NULL)
        return env;
    annotateExp(x->body, env);
    annotateCexpCharCondCases(x->next, env);
    return env;
}

static CTEnv *annotateCexpCondCases(CexpCondCases *x, CTEnv *env) {
    if (x == NULL)
        return env;
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpCondCases ");
    ppCexpCondCases(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    switch (x->type) {
        case CEXPCONDCASES_TYPE_INTCASES:
            return annotateCexpIntCondCases(x->val.intCases, env);
        case CEXPCONDCASES_TYPE_CHARCASES:
            return annotateCexpCharCondCases(x->val.charCases, env);
        default:
            cant_happen("unrecognised type %d in annotateCexpCondCases",
                        x->type);
    }
}

static CTEnv *annotateLetRecLam(Aexp *x, CTEnv *env, int letRecOffset) {
    switch (x->type) {
        case AEXP_TYPE_LAM: {
            AexpLam *lam = x->val.lam;
            annotateAexpLam(lam, env);
            lam->letRecOffset = letRecOffset;
        }
        break;
        case AEXP_TYPE_VAR: {
            x->type = AEXP_TYPE_ANNOTATEDVAR;
            x->val.annotatedVar = annotateAexpVar(CPI(x), x->val.var, env);
        }
        break;
        default:
            cant_happen("letrec bindings can only contain lambdas, got %s", aexpTypeName(x->type));
    }
    return env;
}

static CTEnv *annotateCexpLetRec(CexpLetRec *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpLetRec ");
    ppCexpLetRec(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    int save = PROTECT(env);
    env = newCTEnv(CPI(x), true, env);
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
    return env;
}

static CTEnv *annotateCexpAmb(CexpAmb *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpAmb ");
    ppCexpAmb(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateExp(x->exp1, env);
    annotateExp(x->exp2, env);
    return env;
}

static CTEnv *annotateCexpCut(CexpCut *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpCut ");
    ppCexpCut(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateExp(x->exp, env);
    return env;
}

static CTEnv *annotateExpLet(ExpLet *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateExpLet ");
    ppExpLet(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateExp(x->val, env);
    int save = PROTECT(env);
    env = newCTEnv(CPI(x), true, env);
    UNPROTECT(save);
    save = PROTECT(env);
    populateCTEnv(env, x->var);
    annotateExp(x->body, env);
    UNPROTECT(save);
    return env;
}

static CTEnvArray *getNsEnvs(CTEnv *env) {
    while (env->next != NULL) {
        env = env->next; // should be at the root
    }
#ifdef SAFETY_CHECKS
    if (env->nsEnvs == NULL) {
        cant_happen("namespaces not registered");
    }
#endif
    return env->nsEnvs;
}

static HashSymbol *makeNsName(Index index) {
    char buf[80];
    sprintf(buf, NS_FORMAT, index);
    return newSymbol(buf);
}

static CTEnv *annotateAexpNamespaceArray(AexpNamespaceArray *x, CTEnv *env) {
    CTEnvArray *nsEnvs = getNsEnvs(env);
    for (Index i = 0; i < x->size; ++i) {
        HashSymbol *nsName = makeNsName(i);
        populateCTEnv(env, nsName);
    }
    for (Index i = 0; i < x->size; ++i) {
        CTEnv *env2 = newCTEnv(CPI(x->entries[i]->body), true, env);
        int save = PROTECT(env2);
        env2->isNamespace = true;
        CTEnv *env3 = annotateExp(x->entries[i]->body, env2);
        PROTECT(env3);
        x->entries[i]->nbindings = env2->nbindings;
        pushCTEnvArray(nsEnvs, env3);
        UNPROTECT(save);
    }
    return env;
}

static CTEnv *annotateAexpNamespaces(AexpNamespaces *x, CTEnv *env) {
    annotateAexpNamespaceArray(x->namespaces, env);
    annotateExp(x->body, env);
    return env;
}

static CTEnv *annotateAexpMakeVec(AexpMakeVec *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpMakeVec ");
    ppAexpMakeVec(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateAexpList(x->args, env);
    return env;
}

static CTEnv *annotateAexp(Aexp *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexp ");
    ppAexp(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    switch (x->type) {
        case AEXP_TYPE_LAM:
            return annotateAexpLam(x->val.lam, env);
        case AEXP_TYPE_VAR:
            x->val.annotatedVar = annotateAexpVar(CPI(x), x->val.var, env);
            x->type = AEXP_TYPE_ANNOTATEDVAR;
            return env;
        case AEXP_TYPE_ANNOTATEDVAR:
            cant_happen("annotateAexp called on annotated var %s",
                        x->val.annotatedVar->var->name);
        case AEXP_TYPE_BIGINTEGER:
        case AEXP_TYPE_LITTLEINTEGER:
        case AEXP_TYPE_CHARACTER:
            return env;
        case AEXP_TYPE_PRIM:
            return annotateAexpPrimApp(x->val.prim, env);
        case AEXP_TYPE_MAKEVEC:
            return annotateAexpMakeVec(x->val.makeVec, env);
        case AEXP_TYPE_NAMESPACES:
            return annotateAexpNamespaces(x->val.namespaces, env);
        default:
            cant_happen("unrecognized type %s in annotateAexp", aexpTypeName(x->type));
    }
}

static CTEnv *annotateMatchList(MatchList *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateMatchList ");
    ppMatchList(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    if (x == NULL)
        return env;
    annotateExp(x->body, env);
    annotateMatchList(x->next, env);
    return env;
}

static CTEnv *annotateCexpMatch(CexpMatch *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpMatch ");
    ppCexpMatch(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateMatchList(x->clauses, env);
    return env;
}

static CTEnv *annotateCexp(Cexp *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexp ");
    ppCexp(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            return annotateCexpApply(x->val.apply, env);
        case CEXP_TYPE_IFF:
            return annotateCexpIf(x->val.iff, env);
        case CEXP_TYPE_COND:
            return annotateCexpCond(x->val.cond, env);
        case CEXP_TYPE_CALLCC:
            return annotateAexp(x->val.callCC, env);
        case CEXP_TYPE_LETREC:
            return annotateCexpLetRec(x->val.letRec, env);
        case CEXP_TYPE_AMB:
            return annotateCexpAmb(x->val.amb, env);
        case CEXP_TYPE_CUT:
            return annotateCexpCut(x->val.cut, env);
        case CEXP_TYPE_MATCH:
            return annotateCexpMatch(x->val.match, env);
        case CEXP_TYPE_BACK:
        case CEXP_TYPE_ERROR:
            return env;
        default:
            cant_happen("unrecognized type %d in annotateCexp", x->type);
    }
}

static CTEnv *annotateExpEnv(CTEnv *env) {
    int nbindings = 0;
    CTEnv *orig = env;
    while (env != NULL) {
        nbindings += countCTIntTable(env->table);
        if (env->isNamespace) {
            env->nbindings = nbindings;
            return orig;
        }
        env = env->next;
    }
    cant_happen("failed to find namespace env");
}

static AexpAnnotatedVar *lookupNamespaceInEnv(ParserInfo I, Index index, CTEnv *env) {
    HashSymbol *name = makeNsName(index);
    return annotateAexpVar(I, name, env);
}

static CTEnv *annotateExpLookup(ExpLookup *lookup, CTEnv *env) {
    CTEnvArray *envs = getNsEnvs(env);
#ifdef SAFETY_chECKS
    if (lookup->namespace >= envs->size) {
        cant_happen("namespace index %u out of range", lookup->namespace);
    }
#endif
    lookup->annotatedVar = lookupNamespaceInEnv(CPI(lookup), lookup->namespace, env);
    return annotateExp(lookup->body, envs->entries[lookup->namespace]);
}

static CTEnv * annotateExp(AnfExp *x, CTEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateExp ");
    ppExp(x);
    eprintf("  ");
    ppCTEnv(env);
    eprintf("\n");
#endif
    switch (x->type) {
        case ANFEXP_TYPE_AEXP:
            return annotateAexp(x->val.aexp, env);
        case ANFEXP_TYPE_CEXP:
            return annotateCexp(x->val.cexp, env);
        case ANFEXP_TYPE_LET:
            return annotateExpLet(x->val.let, env);
        case ANFEXP_TYPE_ENV:
            return annotateExpEnv(env);
        case ANFEXP_TYPE_DONE:
            return env;
        case ANFEXP_TYPE_LOOKUP:
            return annotateExpLookup(x->val.lookup, env);
        default:
            cant_happen("unrecognized type %s", anfExpTypeName(x->type));
    }
}

static void addBuiltInsToCTEnv(CTEnv *env, BuiltIns *b) {
    for (Index i = 0; i < b->size; i++) {
        // Bind only internal names; external names are provided by wrappers.
        populateCTEnv(env, b->entries[i]->internalName);
    }
}

void annotateAnf(AnfExp *x, BuiltIns *b) {
    CTEnv *env = newCTEnv(CPI(x), false, NULL);
    int save = PROTECT(env);
    env->nsEnvs = newCTEnvArray();
    addBuiltInsToCTEnv(env, b);
    env = newCTEnv(CPI(x), false, env);
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
#ifdef DEBUG_ANNOTATE
    eprintf("********************* locate ");
    ppAexpVar(var);
    eprintf(" in ");
    ppCTEnv(env);
#endif
    *frame = 0;
    while (env != NULL) {
        if (getCTIntTable(env->table, var, offset)) {
#ifdef DEBUG_ANNOTATE
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
#ifdef DEBUG_ANNOTATE
    eprintf(" FAILED!\n");
#endif
    return false;
}
