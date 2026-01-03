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

static bool locate(HashSymbol *var, AnfEnv *env, int *frame, int *offset);
static void populateAnfEnv(AnfEnv *env, HashSymbol *var);

static AnfEnv *annotateExp(AnfExp *x, AnfEnv *env);
static AnfEnv *annotateAexpLam(AexpLam *x, AnfEnv *env);
static AexpAnnotatedVar *annotateAexpVar(ParserInfo I, HashSymbol *x, AnfEnv *env);
static AnfEnv *annotateAexpPrimApp(AexpPrimApp *x, AnfEnv *env);
static AnfEnv *annotateAexpList(AexpList *x, AnfEnv *env);
static AnfEnv *annotateCexpApply(CexpApply *x, AnfEnv *env);
static AnfEnv *annotateCexpIf(CexpIf *x, AnfEnv *env);
static AnfEnv *annotateCexpCond(CexpCond *x, AnfEnv *env);
static AnfEnv *annotateCexpCondCases(CexpCondCases *x, AnfEnv *env);
static AnfEnv *annotateCexpLetRec(CexpLetRec *x, AnfEnv *env);
static AnfEnv *annotateCexpAmb(CexpAmb *x, AnfEnv *env);
static AnfEnv *annotateCexpCut(CexpCut *x, AnfEnv *env);
static AnfEnv *annotateAnfExpLet(AnfExpLet *x, AnfEnv *env);
static AnfEnv *annotateAexp(Aexp *x, AnfEnv *env);
static AnfEnv *annotateCexp(Cexp *x, AnfEnv *env);
static AnfEnvArray *getNsEnvs(AnfEnv *env);

static void hashAddCTVar(AnfIntTable *table, HashSymbol *var) {
    int count = countAnfIntTable(table);
    setAnfIntTable(table, var, count);
}

static AnfEnv *annotateAexpLam(AexpLam *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpLam ");
    ppAexpLam(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    int save = PROTECT(env);
    env = newAnfEnv(CPI(x), false, env);
    UNPROTECT(save);
    save = PROTECT(env);
    AexpVarList *args = x->args;
    while (args != NULL) {
        populateAnfEnv(env, args->var);
        args = args->next;
    }
    annotateExp(x->exp, env);
    UNPROTECT(save);
    return env;
}

static AexpAnnotatedVar *annotateAexpVar(ParserInfo I, HashSymbol *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpVar ");
    ppAexpVar(x);
    eprintf("  ");
    ppAnfEnv(env);
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
    cant_happen("no binding for var '%s' in annotateAexpVar [%s +%d]", x->name, I.fileName, I.lineNo);
}

static AnfEnv *annotateAexpPrimApp(AexpPrimApp *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpPrimApp ");
    ppAexpPrimApp(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->exp1, env);
    annotateAexp(x->exp2, env);
    return env;
}

static AnfEnv *annotateAexpList(AexpList *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpList ");
    ppAexpList(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    AnfEnv *res;
    while (x != NULL) {
        res = annotateAexp(x->exp, env);
        x = x->next;
    }
    return res;
}

static AnfEnv *annotateCexpApply(CexpApply *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpApply ");
    ppCexpApply(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->function, env);
    annotateAexpList(x->args, env);
    return env;
}

static AnfEnv *annotateCexpIf(CexpIf *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpIf ");
    ppCexpIf(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateExp(x->consequent, env);
    annotateExp(x->alternative, env);
    return env;
}

static AnfEnv *annotateCexpCond(CexpCond *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpCond ");
    ppCexpCond(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateCexpCondCases(x->cases, env);
    return env;
}

static AnfEnv *annotateCexpIntCondCases(CexpIntCondCases *x, AnfEnv *env) {
    if (x == NULL)
        return env;
    annotateExp(x->body, env);
    annotateCexpIntCondCases(x->next, env);
    return env;
}

static AnfEnv *annotateCexpCharCondCases(CexpCharCondCases *x, AnfEnv *env) {
    if (x == NULL)
        return env;
    annotateExp(x->body, env);
    annotateCexpCharCondCases(x->next, env);
    return env;
}

static AnfEnv *annotateCexpCondCases(CexpCondCases *x, AnfEnv *env) {
    if (x == NULL)
        return env;
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpCondCases ");
    ppCexpCondCases(x);
    eprintf("  ");
    ppAnfEnv(env);
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

static AnfEnv *annotateLetRecLam(Aexp *x, AnfEnv *env, int letRecOffset) {
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

static AnfEnv *annotateCexpLetRec(CexpLetRec *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpLetRec ");
    ppCexpLetRec(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    int save = PROTECT(env);
    env = newAnfEnv(CPI(x), true, env);
    UNPROTECT(save);
    save = PROTECT(env);
    AnfLetRecBindings *bindings = x->bindings;
    while (bindings != NULL) {
        populateAnfEnv(env, bindings->var);
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

static AnfEnv *annotateCexpAmb(CexpAmb *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpAmb ");
    ppCexpAmb(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateExp(x->exp1, env);
    annotateExp(x->exp2, env);
    return env;
}

static AnfEnv *annotateCexpCut(CexpCut *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpCut ");
    ppCexpCut(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateExp(x->exp, env);
    return env;
}

static AnfEnv *annotateAnfExpLet(AnfExpLet *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAnfExpLet ");
    ppAnfExpLet(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateExp(x->val, env);
    int save = PROTECT(env);
    env = newAnfEnv(CPI(x), true, env);
    UNPROTECT(save);
    save = PROTECT(env);
    populateAnfEnv(env, x->var);
    annotateExp(x->body, env);
    UNPROTECT(save);
    return env;
}

static AnfEnvArray *getNsEnvs(AnfEnv *env) {
    while (env->next != NULL) {
        env = env->next; // should be at the root
    }
#ifdef SAFETY_CHECKS
    if (env->nsEnvs == NULL) {
        cant_happen("nameSpaces not registered");
    }
#endif
    return env->nsEnvs;
}

static HashSymbol *makeNsName(Index index) {
    char buf[80];
    sprintf(buf, NS_FORMAT, index);
    return newSymbol(buf);
}

static AnfEnv *annotateAexpNameSpaceArray(AexpNameSpaceArray *x, AnfEnv *env) {
    AnfEnvArray *nsEnvs = getNsEnvs(env);
    for (Index i = 0; i < x->size; ++i) {
        HashSymbol *nsName = makeNsName(i);
        populateAnfEnv(env, nsName);
    }
    for (Index i = 0; i < x->size; ++i) {
        AnfEnv *env2 = newAnfEnv(CPI(x->entries[i]->body), true, env);
        int save = PROTECT(env2);
        env2->isNameSpace = true;
        AnfEnv *env3 = annotateExp(x->entries[i]->body, env2);
        PROTECT(env3);
        x->entries[i]->nBindings = env2->nBindings;
        pushAnfEnvArray(nsEnvs, env3);
        UNPROTECT(save);
    }
    return env;
}

static AnfEnv *annotateAexpNameSpaces(AexpNameSpaces *x, AnfEnv *env) {
    annotateAexpNameSpaceArray(x->nameSpaces, env);
    annotateExp(x->body, env);
    return env;
}

static AnfEnv *annotateAexpMakeVec(AexpMakeVec *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexpMakeVec ");
    ppAexpMakeVec(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateAexpList(x->args, env);
    return env;
}

static AnfEnv *annotateAexp(Aexp *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAexp ");
    ppAexp(x);
    eprintf("  ");
    ppAnfEnv(env);
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
            return annotateAexpNameSpaces(x->val.nameSpaces, env);
        default:
            cant_happen("unrecognized type %s in annotateAexp", aexpTypeName(x->type));
    }
}

static AnfEnv *annotateAnfMatchList(AnfMatchList *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateAnfMatchList ");
    ppAnfMatchList(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    if (x == NULL)
        return env;
    annotateExp(x->body, env);
    annotateAnfMatchList(x->next, env);
    return env;
}

static AnfEnv *annotateCexpMatch(CexpMatch *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexpMatch ");
    ppCexpMatch(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    annotateAexp(x->condition, env);
    annotateAnfMatchList(x->clauses, env);
    return env;
}

static AnfEnv *annotateCexp(Cexp *x, AnfEnv *env) {
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateCexp ");
    ppCexp(x);
    eprintf("  ");
    ppAnfEnv(env);
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

static AnfEnv *annotateExpEnv(AnfEnv *env) {
    int nBindings = 0;
    AnfEnv *orig = env;
    while (env != NULL) {
        nBindings += countAnfIntTable(env->table);
        if (env->isNameSpace) {
            env->nBindings = nBindings;
            return orig;
        }
        env = env->next;
    }
    cant_happen("failed to find nameSpace env");
}

static AexpAnnotatedVar *lookUpNameSpaceInEnv(ParserInfo I, Index index, AnfEnv *env) {
    HashSymbol *name = makeNsName(index);
    return annotateAexpVar(I, name, env);
}

static AnfEnv *annotateAnfExpLookUp(AnfExpLookUp *lookUp, AnfEnv *env) {
    AnfEnvArray *envs = getNsEnvs(env);
#ifdef SAFETY_chECKS
    if (lookUp->nameSpace >= envs->size) {
        cant_happen("nameSpace index %u out of range", lookUp->nameSpace);
    }
#endif
    lookUp->annotatedVar = lookUpNameSpaceInEnv(CPI(lookUp), lookUp->nameSpace, env);
    return annotateExp(lookUp->body, envs->entries[lookUp->nameSpace]);
}

static AnfEnv * annotateExp(AnfExp *x, AnfEnv *env) {
    if (x == NULL) {
        return env;
    }
#ifdef DEBUG_ANNOTATE2
    eprintf("annotateExp ");
    ppExp(x);
    eprintf("  ");
    ppAnfEnv(env);
    eprintf("\n");
#endif
    switch (x->type) {
        case ANFEXP_TYPE_AEXP:
            return annotateAexp(x->val.aexp, env);
        case ANFEXP_TYPE_CEXP:
            return annotateCexp(x->val.cexp, env);
        case ANFEXP_TYPE_LET:
            return annotateAnfExpLet(x->val.let, env);
        case ANFEXP_TYPE_ENV:
            return annotateExpEnv(env);
        case ANFEXP_TYPE_DONE:
            return env;
        case ANFEXP_TYPE_LOOKUP:
            return annotateAnfExpLookUp(x->val.lookUp, env);
        default:
            cant_happen("unrecognized type %s", anfExpTypeName(x->type));
    }
}

static void addBuiltInsToAnfEnv(AnfEnv *env, BuiltIns *b) {
    for (Index i = 0; i < b->size; i++) {
        // Bind only internal names; external names are provided by wrappers.
        populateAnfEnv(env, b->entries[i]->internalName);
    }
}

void annotateAnf(AnfExp *x, BuiltIns *b) {
    AnfEnv *env = newAnfEnv(CPI(x), false, NULL);
    int save = PROTECT(env);
    env->nsEnvs = newAnfEnvArray();
    addBuiltInsToAnfEnv(env, b);
    env = newAnfEnv(CPI(x), false, env);
    REPLACE_PROTECT(save, env);
    annotateExp(x, env);
    UNPROTECT(save);
}

static void populateAnfEnv(AnfEnv *env, HashSymbol *var) {
    hashAddCTVar(env->table, var);
}

static int calculateAdjustment(AnfEnv *env) {
    int adjustment = 0;
    while (env != NULL) {
        if (env->isLocal) {
            if (env->next) {
                adjustment += countAnfIntTable(env->next->table);
            }
            env = env->next;
        } else {
            return adjustment;
        }
    }
    return adjustment;
}

static bool locate(HashSymbol *var, AnfEnv *env, int *frame, int *offset) {
#ifdef DEBUG_ANNOTATE
    eprintf("********************* locate ");
    ppAexpVar(var);
    eprintf(" in ");
    ppAnfEnv(env);
#endif
    *frame = 0;
    while (env != NULL) {
        if (getAnfIntTable(env->table, var, offset)) {
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
