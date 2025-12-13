/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2024  Bill Hails
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

#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "lambda_simplification.h"
#include "symbol.h"
#include "lambda_pp.h"
#include "lambda_debug.h"

#ifdef DEBUG_LAMBDA_SIMPLIFICATION
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static LamExp *performLamSimplifications(LamLam *lam) {
    ENTER(performLamSimplifications);
    lam->exp = lamPerformSimplifications(lam->exp);
#ifdef OVERSIMPLIFICATION
    // fn () { a() } == a
    // A thunk that just calls another thunk can be simplified to the second thunk
    if (   lam->args == NULL
        && lam->exp->type == LAMEXP_TYPE_APPLY
        && lam->exp->val.apply->args == NULL
        && lam->exp->val.apply->function->type != LAMEXP_TYPE_CONSTRUCTOR) {
        LEAVE(performLamSimplifications);
        IFDEBUG(printLamLam(lam, 0));
        return lam->exp->val.apply->function;
    }
#endif
    LEAVE(performLamSimplifications);
    return newLamExp_Lam(CPI(lam), lam);
}

static HashSymbol *performVarSimplifications(HashSymbol *var) {
    return var;
}

static LamPrimApp *performPrimSimplifications(LamPrimApp *prim) {
    ENTER(performPrimSimplifications);
    prim->exp1 = lamPerformSimplifications(prim->exp1);
    prim->exp2 = lamPerformSimplifications(prim->exp2);
    LEAVE(performPrimSimplifications);
    return prim;
}

static LamSequence *_performSequenceSimplifications(LamSequence *sequence) {
    ENTER(_performSequenceSimplifications);
    if (sequence == NULL) {
        LEAVE(_performSequenceSimplifications);
        return NULL;
    }
    sequence->next =
        _performSequenceSimplifications(sequence->next);
    sequence->exp = lamPerformSimplifications(sequence->exp);
    LEAVE(_performSequenceSimplifications);
    return sequence;
}

#define SIMPLIFY_SINGLE_SEQUENCE

static LamExp *performSequenceSimplifications(ParserInfo I, LamSequence *sequence) {
    sequence = _performSequenceSimplifications(sequence);
#ifdef SIMPLIFY_SINGLE_SEQUENCE
    if (countLamSequence(sequence) == 1) {
        return sequence->exp;
    }
#endif
    int save = PROTECT(sequence);
    LamExp *res = newLamExp_Sequence(I, sequence);
    UNPROTECT(save);
    return res;
}

static LamArgs *performArgsSimplifications(LamArgs *list) {
    ENTER(performArgsSimplifications);
    if (list == NULL) {
        LEAVE(performArgsSimplifications);
        return NULL;
    }
    list->next = performArgsSimplifications(list->next);
    list->exp = lamPerformSimplifications(list->exp);
    LEAVE(performArgsSimplifications);
    return list;
}

static LamTupleIndex *performTupleIndexSimplifications(LamTupleIndex *tupleIndex) {
    tupleIndex->exp = lamPerformSimplifications(tupleIndex->exp);
    return tupleIndex;
}

static LamPrint *performPrintSimplifications(LamPrint *print) {
    print->exp = lamPerformSimplifications(print->exp);
    print->printer = lamPerformSimplifications(print->printer);
    return print;
}

static LamTypeof *performTypeofSimplifications(LamTypeof *typeOf) {
    typeOf->exp = lamPerformSimplifications(typeOf->exp);
    if (typeOf->typestring != NULL) {
        typeOf->typestring = lamPerformSimplifications(typeOf->typestring);
    }
    return typeOf;
}

static LamLookup *performLookupSimplifications(LamLookup *lookup) {
    lookup->exp = lamPerformSimplifications(lookup->exp);
    return lookup;
}

static LamMakeVec *performMakeVecSimplifications(LamMakeVec *makeVec) {
    ENTER(performMakeVecSimplifications);
    makeVec->args = performArgsSimplifications(makeVec->args);
    LEAVE(performMakeVecSimplifications);
    return makeVec;
}

static LamDeconstruct *performDeconstructSimplifications(LamDeconstruct *deconstruct) {
    ENTER(performDeconstructSimplifications);
    deconstruct->exp = lamPerformSimplifications(deconstruct->exp);
    LEAVE(performDeconstructSimplifications);
    return deconstruct;
}

static LamConstruct *performConstructSimplifications(LamConstruct *construct) {
    ENTER(performConstructSimplifications);
    construct->args = performArgsSimplifications(construct->args);
    LEAVE(performConstructSimplifications);
    return construct;
}

static LamBindings *makeLetBindings(ParserInfo I, LamArgs *aargs, LamVarList *fargs) {
    ENTER(makeLetBindings);
    if (aargs == NULL || fargs == NULL) {
        LEAVE(makeLetBindings);
        return NULL;
    }
    LamBindings *next = makeLetBindings(I, aargs->next, fargs->next);
    int save = PROTECT(next);
    LamBindings *this = newLamBindings(I, fargs->var, aargs->exp, next);
    UNPROTECT(save);
    LEAVE(makeLetBindings);
    return this;
}

static LamExp *performApplySimplifications(LamApply *apply) {
    ENTER(performApplySimplifications);
    apply->function = lamPerformSimplifications(apply->function);
    apply->args = performArgsSimplifications(apply->args);
    if (apply->function->type == LAMEXP_TYPE_LAM) {
        // Convert inline lambdas to let expressions
        LamLam *lam = apply->function->val.lam;
        LamArgs *aargs = apply->args;
        LamVarList *fargs = lam->args;
        if (countLamArgs(aargs) == countLamVarList(fargs)) {
            LamBindings *bindings = makeLetBindings(CPI(apply), aargs, fargs);
            int save = PROTECT(bindings);
            LamLet *let = newLamLet(CPI(apply), bindings, lam->exp);
            PROTECT(let);
            LamExp *exp = newLamExp_Let(CPI(apply), let);
            UNPROTECT(save);
            LEAVE(performApplySimplifications);
            return exp;
        }
    }
    LEAVE(performApplySimplifications);
    return newLamExp_Apply(CPI(apply), apply);
}

static LamIff *performIffSimplifications(LamIff *iff) {
    ENTER(performIffSimplifications);
    iff->condition = lamPerformSimplifications(iff->condition);
    iff->consequent = lamPerformSimplifications(iff->consequent);
    iff->alternative = lamPerformSimplifications(iff->alternative);
    LEAVE(performIffSimplifications);
    return iff;
}

static LamBindings *performBindingsSimplifications(LamBindings *bindings) {
    ENTER(performBindingsSimplifications);
    if (bindings == NULL) {
        LEAVE(performBindingsSimplifications);
        return NULL;
    }
    bindings->next = performBindingsSimplifications(bindings->next);
    bindings->val = lamPerformSimplifications(bindings->val);
    LEAVE(performBindingsSimplifications);
    return bindings;
}

static LamBindings *performLetBindingsSimplifications(LamBindings *bindings) {
    ENTER(performLetBindingsSimplifications);
    if (bindings == NULL) {
        LEAVE(performLetBindingsSimplifications);
        return NULL;
    }
    bindings->next = performLetBindingsSimplifications(bindings->next);
    bindings->val = lamPerformSimplifications(bindings->val);
    LEAVE(performLetBindingsSimplifications);
    return bindings;
}

static LamLet *performLetSimplifications(LamLet *let) {
    ENTER(performLetSimplifications);
    let->bindings = performLetBindingsSimplifications(let->bindings);
    let->body = lamPerformSimplifications(let->body);
    LEAVE(performLetSimplifications);
    return let;
}

static LamLetRec *performLetRecSimplifications(LamLetRec *letrec) {
    ENTER(performLetRecSimplifications);
    letrec->bindings = performBindingsSimplifications(letrec->bindings);
    letrec->body = lamPerformSimplifications(letrec->body);
    LEAVE(performLetRecSimplifications);
    return letrec;
}

static LamTypeDefs *performTypeDefsSimplifications(LamTypeDefs *typedefs) {
    ENTER(performTypeDefsSimplifications);
    typedefs->body = lamPerformSimplifications(typedefs->body);
    LEAVE(performTypeDefsSimplifications);
    return typedefs;
}

static LamMatchList *performCaseSimplifications(LamMatchList *cases) {
    ENTER(performCaseSimplifications);
    if (cases == NULL) {
        LEAVE(performCaseSimplifications);
        return NULL;
    }
    cases->next = performCaseSimplifications(cases->next);
    cases->body = lamPerformSimplifications(cases->body);
    LEAVE(performCaseSimplifications);
    return cases;
}

static LamMatch *performMatchSimplifications(LamMatch *match) {
    ENTER(performMatchSimplifications);
    match->index = lamPerformSimplifications(match->index);
    match->cases = performCaseSimplifications(match->cases);
    LEAVE(performMatchSimplifications);
    return match;
}

static LamAmb *performAmbSimplifications(LamAmb *amb) {
    ENTER(performAmbSimplifications);
    amb->left = lamPerformSimplifications(amb->left);
    amb->right = lamPerformSimplifications(amb->right);
    LEAVE(performAmbSimplifications);
    return amb;
}

static LamIntCondCases *performIntCondCaseSimplifications(LamIntCondCases *cases) {
    ENTER(performIntCondCaseSimplifications);
    if (cases == NULL) {
        LEAVE(performIntCondCaseSimplifications);
        return NULL;
    }
    cases->body = lamPerformSimplifications(cases->body);
    cases->next = performIntCondCaseSimplifications(cases->next);
    LEAVE(performIntCondCaseSimplifications);
    return cases;
}

static LamCharCondCases *performCharCondCaseSimplifications(LamCharCondCases *cases) {
    ENTER(performCharCondCaseSimplifications);
    if (cases == NULL) {
        LEAVE(performCharCondCaseSimplifications);
        return NULL;
    }
    cases->body = lamPerformSimplifications(cases->body);
    cases->next = performCharCondCaseSimplifications(cases->next);
    LEAVE(performCharCondCaseSimplifications);
    return cases;
}

static LamCondCases *performCondCaseSimplifications(LamCondCases *cases) {
    ENTER(performCondCaseSimplifications);
    if (cases == NULL) {
        LEAVE(performCondCaseSimplifications);
        return NULL;
    }
    switch (cases->type) {
        case LAMCONDCASES_TYPE_INTEGERS:
            cases->val.integers = performIntCondCaseSimplifications(cases->val.integers);
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:
            cases->val.characters = performCharCondCaseSimplifications(cases->val.characters);
            break;
        default:
            cant_happen("unrecognised %s", lamCondCasesTypeName(cases->type));
    }
    LEAVE(performCondCaseSimplifications);
    return cases;
}

static LamCond *performCondSimplifications(LamCond *cond) {
    ENTER(performCondSimplifications);
    cond->value = lamPerformSimplifications(cond->value);
    cond->cases = performCondCaseSimplifications(cond->cases);
    LEAVE(performCondSimplifications);
    return cond;
}

static LamNamespaceArray *performNamespacesSimplifications(LamNamespaceArray *namespaces) {
    for (Index i = 0; i < namespaces->size; i++) {
        namespaces->entries[i] = lamPerformSimplifications(namespaces->entries[i]);
    }
    return namespaces;
}

LamExp *lamPerformSimplifications(LamExp *exp) {
    ENTER(lamPerformSimplifications);
    // ppLamExp(exp);
    // eprintf("\n");
    if (exp != NULL) {
        switch (exp->type) {
            case LAMEXP_TYPE_BIGINTEGER:
            case LAMEXP_TYPE_STDINT:
            case LAMEXP_TYPE_CHARACTER:
            case LAMEXP_TYPE_BACK:
            case LAMEXP_TYPE_ERROR:
            case LAMEXP_TYPE_CONSTANT:
            case LAMEXP_TYPE_CONSTRUCTOR:
            case LAMEXP_TYPE_ENV:
                break;
            case LAMEXP_TYPE_LAM:
                exp = performLamSimplifications(exp->val.lam);
                break;
            case LAMEXP_TYPE_APPLY:
                exp = performApplySimplifications(exp->val.apply);
                break;
            case LAMEXP_TYPE_VAR:
                exp->val.var = performVarSimplifications(exp->val.var);
                break;
            case LAMEXP_TYPE_PRIM:
                exp->val.prim = performPrimSimplifications(exp->val.prim);
                break;
            case LAMEXP_TYPE_SEQUENCE:
                exp = performSequenceSimplifications(CPI(exp), exp->val.sequence);
                break;
            case LAMEXP_TYPE_MAKEVEC:
                exp->val.makeVec = performMakeVecSimplifications(exp->val.makeVec);
                break;
            case LAMEXP_TYPE_DECONSTRUCT:
                exp->val.deconstruct = performDeconstructSimplifications(exp->val.deconstruct);
                break;
            case LAMEXP_TYPE_CONSTRUCT:
                exp->val.construct = performConstructSimplifications(exp->val.construct);
                break;
            case LAMEXP_TYPE_TAG:
                exp->val.tag = lamPerformSimplifications(exp->val.tag);
                break;
            case LAMEXP_TYPE_IFF:
                exp->val.iff = performIffSimplifications(exp->val.iff);
                break;
            case LAMEXP_TYPE_COND:
                exp->val.cond = performCondSimplifications(exp->val.cond);
                break;
            case LAMEXP_TYPE_CALLCC:
                exp->val.callcc = lamPerformSimplifications(exp->val.callcc);
                break;
            case LAMEXP_TYPE_LET:
                exp->val.let = performLetSimplifications(exp->val.let);
                break;
            case LAMEXP_TYPE_LETREC:
                exp->val.letrec = performLetRecSimplifications(exp->val.letrec);
                break;
            case LAMEXP_TYPE_TYPEDEFS:
                exp->val.typedefs = performTypeDefsSimplifications(exp->val.typedefs);
                break;
            case LAMEXP_TYPE_MATCH:
                exp->val.match = performMatchSimplifications(exp->val.match);
                break;
            case LAMEXP_TYPE_AMB:
                exp->val.amb = performAmbSimplifications(exp->val.amb);
                break;
            case LAMEXP_TYPE_MAKE_TUPLE:
                exp->val.make_tuple = performArgsSimplifications(exp->val.make_tuple);
                break;
            case LAMEXP_TYPE_TUPLE_INDEX:
                exp->val.tuple_index = performTupleIndexSimplifications(exp->val.tuple_index);
                break;
            case LAMEXP_TYPE_PRINT:
                exp->val.print = performPrintSimplifications(exp->val.print);
                break;
            case LAMEXP_TYPE_TYPEOF:
                exp->val.typeOf = performTypeofSimplifications(exp->val.typeOf);
                break;
            case LAMEXP_TYPE_LOOKUP:
                exp->val.lookup = performLookupSimplifications(exp->val.lookup);
                break;
            case LAMEXP_TYPE_NAMESPACES:
                exp->val.namespaces = performNamespacesSimplifications(exp->val.namespaces);
                break;
            default:
                cant_happen("unrecognized %s", lamExpTypeName(exp->type));
        }
    }
    LEAVE(lamPerformSimplifications);
    return exp;
}
