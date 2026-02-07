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

// Substitution of variables in the bodies of functions, called by the TPMC.

#include "lambda_substitution.h"
#include "ast_debug.h"
#include "common.h"
#include "lambda_helper.h"
#include "print_generator.h"
#include "symbols.h"
#include "tpmc_logic.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef DEBUG_LAMBDA_SUBSTITUTE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static HashSymbol *performVarSubstitutions(HashSymbol *var,
                                           SymbolMap *substitutions);

static void substError(ParserInfo PI, const char *message, ...)
    __attribute__((format(printf, 2, 3)));

static void substError(ParserInfo PI, const char *message, ...)
    __attribute__((unused));

static void substError(ParserInfo PI, const char *message, ...) {
    va_list args;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    can_happen(PI, "");
}

static SymbolList *performVarListSubstitutions(SymbolList *varList,
                                               SymbolMap *substitutions) {
    ENTER(performVarListSubstitutions);
    if (varList == NULL) {
        LEAVE(performVarListSubstitutions);
        return NULL;
    }
    varList->next = performVarListSubstitutions(varList->next, substitutions);
    varList->symbol = performVarSubstitutions(varList->symbol, substitutions);
    LEAVE(performVarListSubstitutions);
    return varList;
}

static LamLam *performLamSubstitutions(LamLam *lam, SymbolMap *substitutions) {
    ENTER(performLamSubstitutions);
    lam->args = performVarListSubstitutions(lam->args, substitutions);
    lam->exp = lamPerformSubstitutions(lam->exp, substitutions);
    LEAVE(performLamSubstitutions);
    return lam;
}

static HashSymbol *performVarSubstitutions(HashSymbol *var,
                                           SymbolMap *substitutions) {
    ENTER(performVarSubstitutions);
    HashSymbol *replacement = NULL;
    if (getSymbolMap(substitutions, var, &replacement)) {
        return replacement;
    }
    LEAVE(performVarSubstitutions);
    return var;
}

static LamPrimApp *performPrimSubstitutions(LamPrimApp *prim,
                                            SymbolMap *substitutions) {
    ENTER(performPrimSubstitutions);
    prim->exp1 = lamPerformSubstitutions(prim->exp1, substitutions);
    prim->exp2 = lamPerformSubstitutions(prim->exp2, substitutions);
    LEAVE(performPrimSubstitutions);
    return prim;
}

static LamSequence *performSequenceSubstitutions(LamSequence *sequence,
                                                 SymbolMap *substitutions) {
    ENTER(performSequenceSubstitutions);
    if (sequence == NULL) {
        LEAVE(performSequenceSubstitutions);
        return NULL;
    }
    sequence->next =
        performSequenceSubstitutions(sequence->next, substitutions);
    sequence->exp = lamPerformSubstitutions(sequence->exp, substitutions);
    LEAVE(performSequenceSubstitutions);
    return sequence;
}

static LamArgs *performArgsSubstitutions(LamArgs *list,
                                         SymbolMap *substitutions) {
    ENTER(performArgsSubstitutions);
    if (list == NULL) {
        LEAVE(performArgsSubstitutions);
        return NULL;
    }
    list->next = performArgsSubstitutions(list->next, substitutions);
    list->exp = lamPerformSubstitutions(list->exp, substitutions);
    LEAVE(performArgsSubstitutions);
    return list;
}

static LamTupleIndex *performTupleIndexSubstitutions(LamTupleIndex *tupleIndex,
                                                     SymbolMap *substitutions) {
    tupleIndex->exp = lamPerformSubstitutions(tupleIndex->exp, substitutions);
    return tupleIndex;
}

static LamPrint *performPrintSubstitutions(LamPrint *print,
                                           SymbolMap *substitutions) {
    print->exp = lamPerformSubstitutions(print->exp, substitutions);
    print->printer = lamPerformSubstitutions(print->printer, substitutions);
    return print;
}

static LamLookUp *performLookUpSubstitutions(LamLookUp *lookUp,
                                             SymbolMap *substitutions) {
    lookUp->exp = lamPerformSubstitutions(lookUp->exp, substitutions);
    return lookUp;
}

static LamTypeOf *performTypeOfSubstitutions(LamTypeOf *typeOf,
                                             SymbolMap *substitutions) {
    typeOf->exp = lamPerformSubstitutions(typeOf->exp, substitutions);
    return typeOf;
}

static LamMakeVec *performMakeVecSubstitutions(LamMakeVec *makeVec,
                                               SymbolMap *substitutions) {
    ENTER(performMakeVecSubstitutions);
    makeVec->args = performArgsSubstitutions(makeVec->args, substitutions);
    LEAVE(performMakeVecSubstitutions);
    return makeVec;
}

static LamDeconstruct *
performDeconstructSubstitutions(LamDeconstruct *deconstruct,
                                SymbolMap *substitutions) {
    ENTER(performDeconstructSubstitutions);
    deconstruct->exp = lamPerformSubstitutions(deconstruct->exp, substitutions);
    LEAVE(performDeconstructSubstitutions);
    return deconstruct;
}

static LamConstruct *performConstructSubstitutions(LamConstruct *construct,
                                                   SymbolMap *substitutions) {
    ENTER(performConstructSubstitutions);
    construct->args = performArgsSubstitutions(construct->args, substitutions);
    LEAVE(performConstructSubstitutions);
    return construct;
}

static LamApply *performApplySubstitutions(LamApply *apply,
                                           SymbolMap *substitutions) {
    ENTER(performApplySubstitutions);
    apply->function = lamPerformSubstitutions(apply->function, substitutions);
    apply->args = performArgsSubstitutions(apply->args, substitutions);
    LEAVE(performApplySubstitutions);
    return apply;
}

static LamIff *performIffSubstitutions(LamIff *iff, SymbolMap *substitutions) {
    ENTER(performIffSubstitutions);
    iff->condition = lamPerformSubstitutions(iff->condition, substitutions);
    iff->consequent = lamPerformSubstitutions(iff->consequent, substitutions);
    iff->alternative = lamPerformSubstitutions(iff->alternative, substitutions);
    LEAVE(performIffSubstitutions);
    return iff;
}

static LamBindings *performBindingsSubstitutions(LamBindings *bindings,
                                                 SymbolMap *substitutions) {
    ENTER(performBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performBindingsSubstitutions);
        return NULL;
    }
    bindings->next =
        performBindingsSubstitutions(bindings->next, substitutions);
    bindings->var = performVarSubstitutions(bindings->var, substitutions);
    bindings->val = lamPerformSubstitutions(bindings->val, substitutions);
    LEAVE(performBindingsSubstitutions);
    return bindings;
}

static LamBindings *performLetBindingsSubstitutions(LamBindings *bindings,
                                                    SymbolMap *substitutions) {
    ENTER(performLetBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performLetBindingsSubstitutions);
        return NULL;
    }
    bindings->next =
        performLetBindingsSubstitutions(bindings->next, substitutions);
    bindings->var = performVarSubstitutions(bindings->var, substitutions);
    bindings->val = lamPerformSubstitutions(bindings->val, substitutions);
    LEAVE(performLetBindingsSubstitutions);
    return bindings;
}

static LamLet *performLetSubstitutions(LamLet *let, SymbolMap *substitutions) {
    ENTER(performLetSubstitutions);
    let->bindings =
        performLetBindingsSubstitutions(let->bindings, substitutions);
    let->body = lamPerformSubstitutions(let->body, substitutions);
    LEAVE(performLetSubstitutions);
    return let;
}

static LamLetRec *performLetRecSubstitutions(LamLetRec *letrec,
                                             SymbolMap *substitutions) {
    ENTER(performLetRecSubstitutions);
    letrec->bindings =
        performBindingsSubstitutions(letrec->bindings, substitutions);
    letrec->body = lamPerformSubstitutions(letrec->body, substitutions);
    LEAVE(performLetRecSubstitutions);
    return letrec;
}

static LamLetStar *performLetStarSubstitutions(LamLetStar *letStar,
                                               SymbolMap *substitutions) {
    ENTER(performLetStarSubstitutions);
    letStar->bindings =
        performBindingsSubstitutions(letStar->bindings, substitutions);
    letStar->body = lamPerformSubstitutions(letStar->body, substitutions);
    LEAVE(performLetStarSubstitutions);
    return letStar;
}

static LamTypeDefs *performTypeDefsSubstitutions(LamTypeDefs *typeDefs,
                                                 SymbolMap *substitutions) {
    ENTER(performTypeDefsSubstitutions);
    typeDefs->body = lamPerformSubstitutions(typeDefs->body, substitutions);
    LEAVE(performTypeDefsSubstitutions);
    return typeDefs;
}

static LamMatchList *performCaseSubstitutions(LamMatchList *cases,
                                              SymbolMap *substitutions) {
    ENTER(performCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCaseSubstitutions);
        return NULL;
    }
    cases->next = performCaseSubstitutions(cases->next, substitutions);
    cases->body = lamPerformSubstitutions(cases->body, substitutions);
    LEAVE(performCaseSubstitutions);
    return cases;
}

static LamMatch *performMatchSubstitutions(LamMatch *match,
                                           SymbolMap *substitutions) {
    ENTER(performMatchSubstitutions);
    match->index = lamPerformSubstitutions(match->index, substitutions);
    match->cases = performCaseSubstitutions(match->cases, substitutions);
    LEAVE(performMatchSubstitutions);
    return match;
}

static LamAmb *performAmbSubstitutions(LamAmb *amb, SymbolMap *substitutions) {
    ENTER(performAmbSubstitutions);
    amb->left = lamPerformSubstitutions(amb->left, substitutions);
    amb->right = lamPerformSubstitutions(amb->right, substitutions);
    LEAVE(performAmbSubstitutions);
    return amb;
}

static LamIntCondCases *
performIntCondCaseSubstitutions(LamIntCondCases *cases,
                                SymbolMap *substitutions) {
    ENTER(performIntCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performIntCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformSubstitutions(cases->body, substitutions);
    cases->next = performIntCondCaseSubstitutions(cases->next, substitutions);
    LEAVE(performIntCondCaseSubstitutions);
    return cases;
}

static LamCharCondCases *
performCharCondCaseSubstitutions(LamCharCondCases *cases,
                                 SymbolMap *substitutions) {
    ENTER(performCharCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCharCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformSubstitutions(cases->body, substitutions);
    cases->next = performCharCondCaseSubstitutions(cases->next, substitutions);
    LEAVE(performCharCondCaseSubstitutions);
    return cases;
}

static LamCondCases *performCondCaseSubstitutions(LamCondCases *cases,
                                                  SymbolMap *substitutions) {
    ENTER(performCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCondCaseSubstitutions);
        return NULL;
    }
    switch (cases->type) {
    case LAMCONDCASES_TYPE_INTEGERS:
        setLamCondCases_Integers(
            cases, performIntCondCaseSubstitutions(
                       getLamCondCases_Integers(cases), substitutions));
        break;
    case LAMCONDCASES_TYPE_CHARACTERS:
        setLamCondCases_Characters(
            cases, performCharCondCaseSubstitutions(
                       getLamCondCases_Characters(cases), substitutions));
        break;
    default:
        cant_happen("unrecognised type %d in performCondCaseSubstitutions",
                    cases->type);
    }
    LEAVE(performCondCaseSubstitutions);
    return cases;
}

static LamCond *performCondSubstitutions(LamCond *cond,
                                         SymbolMap *substitutions) {
    ENTER(performCondSubstitutions);
    cond->value = lamPerformSubstitutions(cond->value, substitutions);
    cond->cases = performCondCaseSubstitutions(cond->cases, substitutions);
    LEAVE(performCondSubstitutions);
    return cond;
}

LamExp *lamPerformSubstitutions(LamExp *exp, SymbolMap *substitutions) {
    ENTER(lamPerformSubstitutions);
    if (exp != NULL) {
        switch (exp->type) {
        case LAMEXP_TYPE_BIGINTEGER:
        case LAMEXP_TYPE_STDINT:
        case LAMEXP_TYPE_CHARACTER:
        case LAMEXP_TYPE_BACK:
        case LAMEXP_TYPE_ERROR:
        case LAMEXP_TYPE_CONSTANT:
        case LAMEXP_TYPE_CONSTRUCTOR:
            break;
        case LAMEXP_TYPE_LAM:
            setLamExp_Lam(exp, performLamSubstitutions(getLamExp_Lam(exp),
                                                       substitutions));
            break;
        case LAMEXP_TYPE_VAR:
            setLamExp_Var(exp, performVarSubstitutions(getLamExp_Var(exp),
                                                       substitutions));
            break;
        case LAMEXP_TYPE_PRIM:
            setLamExp_Prim(exp, performPrimSubstitutions(getLamExp_Prim(exp),
                                                         substitutions));
            break;
        case LAMEXP_TYPE_SEQUENCE:
            setLamExp_Sequence(
                exp, performSequenceSubstitutions(getLamExp_Sequence(exp),
                                                  substitutions));
            break;
        case LAMEXP_TYPE_MAKEVEC:
            setLamExp_MakeVec(exp, performMakeVecSubstitutions(
                                       getLamExp_MakeVec(exp), substitutions));
            break;
        case LAMEXP_TYPE_DECONSTRUCT:
            setLamExp_Deconstruct(
                exp, performDeconstructSubstitutions(getLamExp_Deconstruct(exp),
                                                     substitutions));
            break;
        case LAMEXP_TYPE_CONSTRUCT:
            setLamExp_Construct(
                exp, performConstructSubstitutions(getLamExp_Construct(exp),
                                                   substitutions));
            break;
        case LAMEXP_TYPE_TAG:
            setLamExp_Tag(exp, lamPerformSubstitutions(getLamExp_Tag(exp),
                                                       substitutions));
            break;
        case LAMEXP_TYPE_APPLY:
            setLamExp_Apply(exp, performApplySubstitutions(getLamExp_Apply(exp),
                                                           substitutions));
            break;
        case LAMEXP_TYPE_IFF:
            setLamExp_Iff(exp, performIffSubstitutions(getLamExp_Iff(exp),
                                                       substitutions));
            break;
        case LAMEXP_TYPE_COND:
            setLamExp_Cond(exp, performCondSubstitutions(getLamExp_Cond(exp),
                                                         substitutions));
            break;
        case LAMEXP_TYPE_CALLCC:
            setLamExp_CallCC(exp, lamPerformSubstitutions(getLamExp_CallCC(exp),
                                                          substitutions));
            break;
        case LAMEXP_TYPE_LET:
            setLamExp_Let(exp, performLetSubstitutions(getLamExp_Let(exp),
                                                       substitutions));
            break;
        case LAMEXP_TYPE_LETREC:
            setLamExp_LetRec(exp, performLetRecSubstitutions(
                                      getLamExp_LetRec(exp), substitutions));
            break;
        case LAMEXP_TYPE_LETSTAR:
            setLamExp_LetStar(exp, performLetStarSubstitutions(
                                       getLamExp_LetStar(exp), substitutions));
            break;
        case LAMEXP_TYPE_TYPEDEFS:
            setLamExp_TypeDefs(
                exp, performTypeDefsSubstitutions(getLamExp_TypeDefs(exp),
                                                  substitutions));
            break;
        case LAMEXP_TYPE_MATCH:
            setLamExp_Match(exp, performMatchSubstitutions(getLamExp_Match(exp),
                                                           substitutions));
            break;
        case LAMEXP_TYPE_AMB:
            setLamExp_Amb(exp, performAmbSubstitutions(getLamExp_Amb(exp),
                                                       substitutions));
            break;
        case LAMEXP_TYPE_MAKETUPLE:
            setLamExp_MakeTuple(
                exp, performArgsSubstitutions(getLamExp_MakeTuple(exp),
                                              substitutions));
            break;
        case LAMEXP_TYPE_TUPLEINDEX:
            setLamExp_TupleIndex(
                exp, performTupleIndexSubstitutions(getLamExp_TupleIndex(exp),
                                                    substitutions));
            break;
        case LAMEXP_TYPE_PRINT:
            setLamExp_Print(exp, performPrintSubstitutions(getLamExp_Print(exp),
                                                           substitutions));
            break;
        case LAMEXP_TYPE_LOOKUP:
            setLamExp_LookUp(exp, performLookUpSubstitutions(
                                      getLamExp_LookUp(exp), substitutions));
            break;
        case LAMEXP_TYPE_TYPEOF:
            setLamExp_TypeOf(exp, performTypeOfSubstitutions(
                                      getLamExp_TypeOf(exp), substitutions));
            break;
        default:
            cant_happen("unrecognized LamExp type %s",
                        lamExpTypeName(exp->type));
        }
    }
    LEAVE(lamPerformSubstitutions);
    return exp;
}
