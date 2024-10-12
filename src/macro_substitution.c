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
#include "macro_substitution.h"
#include "lambda_pp.h"

#ifdef DEBUG_MACRO_SUBSTITUTE
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static LamExp *performVarSubstitutions(HashSymbol *var, LamExpTable
                                           *substitutions);

static LamLam *performLamSubstitutions(LamLam *lam,
                                       LamExpTable *substitutions) {
    ENTER(performLamSubstitutions);
    lam->exp = lamPerformMacroSubstitutions(lam->exp, substitutions);
    LEAVE(performLamSubstitutions);
    return lam;
}

static LamExp *performVarSubstitutions(HashSymbol *var, LamExpTable
                                           *substitutions) {
    ENTER(performVarSubstitutions);
    LamExp *replacement = NULL;
    getLamExpTable(substitutions, var, &replacement);
    LEAVE(performVarSubstitutions);
    return replacement;
}

static LamPrimApp *performPrimSubstitutions(LamPrimApp *prim, LamExpTable
                                            *substitutions) {
    ENTER(performPrimSubstitutions);
    prim->exp1 = lamPerformMacroSubstitutions(prim->exp1, substitutions);
    prim->exp2 = lamPerformMacroSubstitutions(prim->exp2, substitutions);
    LEAVE(performPrimSubstitutions);
    return prim;
}

static LamUnaryApp *performUnarySubstitutions(LamUnaryApp *unary, LamExpTable
                                              *substitutions) {
    ENTER(performUnarySubstitutions);
    unary->exp = lamPerformMacroSubstitutions(unary->exp, substitutions);
    LEAVE(performUnarySubstitutions);
    return unary;
}

static LamSequence *performSequenceSubstitutions(LamSequence *sequence, LamExpTable
                                                 *substitutions) {
    ENTER(performSequenceSubstitutions);
    if (sequence == NULL) {
        LEAVE(performSequenceSubstitutions);
        return NULL;
    }
    sequence->next =
        performSequenceSubstitutions(sequence->next, substitutions);
    sequence->exp = lamPerformMacroSubstitutions(sequence->exp, substitutions);
    LEAVE(performSequenceSubstitutions);
    return sequence;
}

static LamList *performListSubstitutions(LamList *list, LamExpTable
                                         *substitutions) {
    ENTER(performListSubstitutions);
    if (list == NULL) {
        LEAVE(performListSubstitutions);
        return NULL;
    }
    list->next = performListSubstitutions(list->next, substitutions);
    list->exp = lamPerformMacroSubstitutions(list->exp, substitutions);
    LEAVE(performListSubstitutions);
    return list;
}

static LamTupleIndex *performTupleIndexSubstitutions(LamTupleIndex *tupleIndex,
                                                     LamExpTable *substitutions) {
    tupleIndex->exp = lamPerformMacroSubstitutions(tupleIndex->exp, substitutions);
    return tupleIndex;
}

static LamPrint *performPrintSubstitutions(LamPrint *print, LamExpTable *substitutions) {
    print->exp = lamPerformMacroSubstitutions(print->exp, substitutions);
    print->printer = lamPerformMacroSubstitutions(print->printer, substitutions);
    return print;
}

static LamLookup *performLookupSubstitutions(LamLookup *lookup, LamExpTable *substitutions) {
    lookup->exp = lamPerformMacroSubstitutions(lookup->exp, substitutions);
    return lookup;
}

static LamMakeVec *performMakeVecSubstitutions(LamMakeVec *makeVec, LamExpTable
                                               *substitutions) {
    ENTER(performMakeVecSubstitutions);
    makeVec->args = performListSubstitutions(makeVec->args, substitutions);
    LEAVE(performMakeVecSubstitutions);
    return makeVec;
}

static LamDeconstruct *performDeconstructSubstitutions(LamDeconstruct
                                                       *deconstruct, LamExpTable
                                                       *substitutions) {
    ENTER(performDeconstructSubstitutions);
    deconstruct->exp =
        lamPerformMacroSubstitutions(deconstruct->exp, substitutions);
    LEAVE(performDeconstructSubstitutions);
    return deconstruct;
}

static LamConstruct *performConstructSubstitutions(LamConstruct *construct, LamExpTable
                                                   *substitutions) {
    ENTER(performConstructSubstitutions);
    construct->args =
        performListSubstitutions(construct->args, substitutions);
    LEAVE(performConstructSubstitutions);
    return construct;
}

static LamApply *performApplySubstitutions(LamApply *apply, LamExpTable
                                           *substitutions) {
    ENTER(performApplySubstitutions);
    apply->function = lamPerformMacroSubstitutions(apply->function, substitutions);
    apply->args = performListSubstitutions(apply->args, substitutions);
    LEAVE(performApplySubstitutions);
    return apply;
}

static LamIff *performIffSubstitutions(LamIff *iff,
                                       LamExpTable *substitutions) {
    ENTER(performIffSubstitutions);
    iff->condition = lamPerformMacroSubstitutions(iff->condition, substitutions);
    iff->consequent = lamPerformMacroSubstitutions(iff->consequent, substitutions);
    iff->alternative =
        lamPerformMacroSubstitutions(iff->alternative, substitutions);
    LEAVE(performIffSubstitutions);
    return iff;
}

static LamLetRecBindings *performBindingsSubstitutions(LamLetRecBindings
                                                       *bindings, LamExpTable
                                                       *substitutions) {
    ENTER(performBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performBindingsSubstitutions);
        return NULL;
    }
    bindings->next =
        performBindingsSubstitutions(bindings->next, substitutions);
    bindings->val = lamPerformMacroSubstitutions(bindings->val, substitutions);
    LEAVE(performBindingsSubstitutions);
    return bindings;
}

static LamLet *performLetSubstitutions(LamLet *let,
                                       LamExpTable *substitutions) {
    ENTER(performLetSubstitutions);
    let->value = lamPerformMacroSubstitutions(let->value, substitutions);
    let->body = lamPerformMacroSubstitutions(let->body, substitutions);
    LEAVE(performLetSubstitutions);
    return let;
}

static LamLetRec *performLetRecSubstitutions(LamLetRec *letrec, LamExpTable
                                             *substitutions) {
    ENTER(performLetRecSubstitutions);
    letrec->bindings =
        performBindingsSubstitutions(letrec->bindings, substitutions);
    letrec->body = lamPerformMacroSubstitutions(letrec->body, substitutions);
    LEAVE(performLetRecSubstitutions);
    return letrec;
}

static LamTypeDefs *performTypeDefsSubstitutions(LamTypeDefs *typedefs, LamExpTable
                                                 *substitutions) {
    ENTER(performTypeDefsSubstitutions);
    typedefs->body = lamPerformMacroSubstitutions(typedefs->body, substitutions);
    LEAVE(performTypeDefsSubstitutions);
    return typedefs;
}

static LamMatchList *performCaseSubstitutions(LamMatchList *cases, LamExpTable
                                              *substitutions) {
    ENTER(performCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCaseSubstitutions);
        return NULL;
    }
    cases->next = performCaseSubstitutions(cases->next, substitutions);
    cases->body = lamPerformMacroSubstitutions(cases->body, substitutions);
    LEAVE(performCaseSubstitutions);
    return cases;
}

static LamMatch *performMatchSubstitutions(LamMatch *match, LamExpTable
                                           *substitutions) {
    ENTER(performMatchSubstitutions);
    match->index = lamPerformMacroSubstitutions(match->index, substitutions);
    match->cases = performCaseSubstitutions(match->cases, substitutions);
    LEAVE(performMatchSubstitutions);
    return match;
}

static LamAmb *performAmbSubstitutions(LamAmb *amb,
                                       LamExpTable *substitutions) {
    ENTER(performAmbSubstitutions);
    amb->left = lamPerformMacroSubstitutions(amb->left, substitutions);
    amb->right = lamPerformMacroSubstitutions(amb->right, substitutions);
    LEAVE(performAmbSubstitutions);
    return amb;
}

static LamIntCondCases *performIntCondCaseSubstitutions(LamIntCondCases
                                                        *cases, LamExpTable
                                                        *substitutions) {
    ENTER(performIntCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performIntCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformMacroSubstitutions(cases->body, substitutions);
    cases->next = performIntCondCaseSubstitutions(cases->next, substitutions);
    LEAVE(performIntCondCaseSubstitutions);
    return cases;
}

static LamCharCondCases *performCharCondCaseSubstitutions(LamCharCondCases
                                                          *cases, LamExpTable
                                                          *substitutions) {
    ENTER(performCharCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCharCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformMacroSubstitutions(cases->body, substitutions);
    cases->next =
        performCharCondCaseSubstitutions(cases->next, substitutions);
    LEAVE(performCharCondCaseSubstitutions);
    return cases;
}

static LamCondCases *performCondCaseSubstitutions(LamCondCases *cases, LamExpTable
                                                  *substitutions) {
    ENTER(performCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCondCaseSubstitutions);
        return NULL;
    }
    switch (cases->type) {
        case LAMCONDCASES_TYPE_INTEGERS:
            cases->val.integers =
                performIntCondCaseSubstitutions(cases->val.integers,
                                                substitutions);
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:
            cases->val.characters =
                performCharCondCaseSubstitutions(cases->val.characters,
                                                 substitutions);
            break;
        default:
            cant_happen
                ("unrecognised type %d in performCondCaseSubstitutions",
                 cases->type);
    }
    LEAVE(performCondCaseSubstitutions);
    return cases;
}

static LamCond *performCondSubstitutions(LamCond *cond, LamExpTable
                                         *substitutions) {
    ENTER(performCondSubstitutions);
    cond->value = lamPerformMacroSubstitutions(cond->value, substitutions);
    cond->cases = performCondCaseSubstitutions(cond->cases, substitutions);
    LEAVE(performCondSubstitutions);
    return cond;
}

LamExp *lamPerformMacroSubstitutions(LamExp *exp,
                                LamExpTable *substitutions) {
    ENTER(lamPerformMacroSubstitutions);
    // ppLamExp(exp);
    // eprintf("\n");
    if (exp != NULL) {
        switch (exp->type) {
            case LAMEXP_TYPE_BIGINTEGER:
            case LAMEXP_TYPE_STDINT:
            case LAMEXP_TYPE_CHARACTER:
            case LAMEXP_TYPE_BACK:
            case LAMEXP_TYPE_COND_DEFAULT:
            case LAMEXP_TYPE_ERROR:
            case LAMEXP_TYPE_CONSTANT:
            case LAMEXP_TYPE_CONSTRUCTOR:
                break;
            case LAMEXP_TYPE_LAM:
                exp->val.lam =
                    performLamSubstitutions(exp->val.lam, substitutions);
                break;
            case LAMEXP_TYPE_VAR: {
                LamExp *rep = performVarSubstitutions(exp->val.var, substitutions);
                if (rep) {
                    exp = rep;
                }
            }
            break;
            case LAMEXP_TYPE_PRIM:
                exp->val.prim =
                    performPrimSubstitutions(exp->val.prim, substitutions);
                break;
            case LAMEXP_TYPE_UNARY:
                exp->val.unary =
                    performUnarySubstitutions(exp->val.unary, substitutions);
                break;
            case LAMEXP_TYPE_LIST:
                exp->val.list =
                    performSequenceSubstitutions(exp->val.list, substitutions);
                break;
            case LAMEXP_TYPE_MAKEVEC:
                exp->val.makeVec =
                    performMakeVecSubstitutions(exp->val.makeVec, substitutions);
                break;
            case LAMEXP_TYPE_DECONSTRUCT:
                exp->val.deconstruct =
                    performDeconstructSubstitutions(exp->val.deconstruct,
                                                    substitutions);
                break;
            case LAMEXP_TYPE_CONSTRUCT:
                exp->val.construct =
                    performConstructSubstitutions(exp->val.construct,
                                                  substitutions);
                break;
            case LAMEXP_TYPE_TAG:
                exp->val.tag =
                    lamPerformMacroSubstitutions(exp->val.tag, substitutions);
                break;
            case LAMEXP_TYPE_APPLY:
                exp->val.apply =
                    performApplySubstitutions(exp->val.apply, substitutions);
                break;
            case LAMEXP_TYPE_IFF:
                exp->val.iff =
                    performIffSubstitutions(exp->val.iff, substitutions);
                break;
            case LAMEXP_TYPE_COND:
                exp->val.cond =
                    performCondSubstitutions(exp->val.cond, substitutions);
                break;
            case LAMEXP_TYPE_CALLCC:
                exp->val.callcc =
                    lamPerformMacroSubstitutions(exp->val.callcc, substitutions);
                break;
            case LAMEXP_TYPE_LET:
                exp->val.let =
                    performLetSubstitutions(exp->val.let, substitutions);
                break;
            case LAMEXP_TYPE_LETREC:
                exp->val.letrec =
                    performLetRecSubstitutions(exp->val.letrec, substitutions);
                break;
            case LAMEXP_TYPE_TYPEDEFS:
                exp->val.typedefs =
                    performTypeDefsSubstitutions(exp->val.typedefs,
                                                 substitutions);
                break;
            case LAMEXP_TYPE_MATCH:
                exp->val.match =
                    performMatchSubstitutions(exp->val.match, substitutions);
                break;
            case LAMEXP_TYPE_AMB:
                exp->val.amb =
                    performAmbSubstitutions(exp->val.amb, substitutions);
                break;
            case LAMEXP_TYPE_MAKE_TUPLE:
                exp->val.make_tuple =
                    performListSubstitutions(exp->val.make_tuple, substitutions);
                break;
            case LAMEXP_TYPE_TUPLE_INDEX:
                exp->val.tuple_index =
                    performTupleIndexSubstitutions(exp->val.tuple_index, substitutions);
                break;
            case LAMEXP_TYPE_PRINT:
                exp->val.print = performPrintSubstitutions(exp->val.print, substitutions);
                break;
            case LAMEXP_TYPE_LOOKUP:
                exp->val.lookup = performLookupSubstitutions(exp->val.lookup, substitutions);
                break;
            default:
                cant_happen
                    ("unrecognized LamExp type %s", lamExpTypeName(exp->type));
        }
    }
    LEAVE(lamPerformMacroSubstitutions);
    return exp;
}
