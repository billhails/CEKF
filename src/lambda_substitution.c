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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "common.h"
#include "lambda_substitution.h"
#include "lambda_helper.h"
#include "symbols.h"
#include "tpmc_logic.h"
#include "ast_debug.h"
#include "print_generator.h"

#ifdef DEBUG_LAMBDA_SUBSTITUTE
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static HashSymbol *performVarSubstitutions(HashSymbol *var, TpmcSubstitutionTable
                                           *substitutions);

static void substError(ParserInfo PI, const char *message, ...) __attribute__((format(printf, 2, 3)));

static void substError(ParserInfo PI, const char *message, ...) __attribute__((unused));

static void substError(ParserInfo PI, const char *message, ...) {
    va_list args;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    can_happen(" at +%d %s", PI.lineno, PI.filename);
}

static LamVarList *performVarListSubstitutions(LamVarList *varList, TpmcSubstitutionTable
                                               *substitutions) {
    ENTER(performVarListSubstitutions);
    if (varList == NULL) {
        LEAVE(performVarListSubstitutions);
        return NULL;
    }
    varList->next = performVarListSubstitutions(varList->next, substitutions);
    varList->var = performVarSubstitutions(varList->var, substitutions);
    LEAVE(performVarListSubstitutions);
    return varList;
}

static LamLam *performLamSubstitutions(LamLam *lam,
                                       TpmcSubstitutionTable *substitutions) {
    ENTER(performLamSubstitutions);
    lam->args = performVarListSubstitutions(lam->args, substitutions);
    lam->exp = lamPerformSubstitutions(lam->exp, substitutions);
    LEAVE(performLamSubstitutions);
    return lam;
}

static HashSymbol *performVarSubstitutions(HashSymbol *var, TpmcSubstitutionTable
                                           *substitutions) {
    ENTER(performVarSubstitutions);
    HashSymbol *replacement = NULL;
    if (getTpmcSubstitutionTable(substitutions, var, &replacement)) {
        return replacement;
    }
    LEAVE(performVarSubstitutions);
    return var;
}

static LamPrimApp *performPrimSubstitutions(LamPrimApp *prim, TpmcSubstitutionTable
                                            *substitutions) {
    ENTER(performPrimSubstitutions);
    prim->exp1 = lamPerformSubstitutions(prim->exp1, substitutions);
    prim->exp2 = lamPerformSubstitutions(prim->exp2, substitutions);
    LEAVE(performPrimSubstitutions);
    return prim;
}

static LamUnaryApp *performUnarySubstitutions(LamUnaryApp *unary, TpmcSubstitutionTable
                                              *substitutions) {
    ENTER(performUnarySubstitutions);
    unary->exp = lamPerformSubstitutions(unary->exp, substitutions);
    LEAVE(performUnarySubstitutions);
    return unary;
}

static LamSequence *performSequenceSubstitutions(LamSequence *sequence, TpmcSubstitutionTable
                                                 *substitutions) {
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

static LamList *performListSubstitutions(LamList *list, TpmcSubstitutionTable
                                         *substitutions) {
    ENTER(performListSubstitutions);
    if (list == NULL) {
        LEAVE(performListSubstitutions);
        return NULL;
    }
    list->next = performListSubstitutions(list->next, substitutions);
    list->exp = lamPerformSubstitutions(list->exp, substitutions);
    LEAVE(performListSubstitutions);
    return list;
}

static LamTupleIndex *performTupleIndexSubstitutions(LamTupleIndex *tupleIndex,
                                                     TpmcSubstitutionTable *substitutions) {
    tupleIndex->exp = lamPerformSubstitutions(tupleIndex->exp, substitutions);
    return tupleIndex;
}

static LamPrint *performPrintSubstitutions(LamPrint *print, TpmcSubstitutionTable *substitutions) {
    print->exp = lamPerformSubstitutions(print->exp, substitutions);
    print->printer = lamPerformSubstitutions(print->printer, substitutions);
    return print;
}

static LamLookup *performLookupSubstitutions(LamLookup *lookup, TpmcSubstitutionTable *substitutions) {
    lookup->exp = lamPerformSubstitutions(lookup->exp, substitutions);
    return lookup;
}

static LamMakeVec *performMakeVecSubstitutions(LamMakeVec *makeVec, TpmcSubstitutionTable
                                               *substitutions) {
    ENTER(performMakeVecSubstitutions);
    makeVec->args = performListSubstitutions(makeVec->args, substitutions);
    LEAVE(performMakeVecSubstitutions);
    return makeVec;
}

static LamDeconstruct *performDeconstructSubstitutions(LamDeconstruct
                                                       *deconstruct, TpmcSubstitutionTable
                                                       *substitutions) {
    ENTER(performDeconstructSubstitutions);
    deconstruct->exp =
        lamPerformSubstitutions(deconstruct->exp, substitutions);
    LEAVE(performDeconstructSubstitutions);
    return deconstruct;
}

static LamConstruct *performConstructSubstitutions(LamConstruct *construct, TpmcSubstitutionTable
                                                   *substitutions) {
    ENTER(performConstructSubstitutions);
    construct->args =
        performListSubstitutions(construct->args, substitutions);
    LEAVE(performConstructSubstitutions);
    return construct;
}

static LamApply *performApplySubstitutions(LamApply *apply, TpmcSubstitutionTable
                                           *substitutions) {
    ENTER(performApplySubstitutions);
    apply->function = lamPerformSubstitutions(apply->function, substitutions);
    apply->args = performListSubstitutions(apply->args, substitutions);
    LEAVE(performApplySubstitutions);
    return apply;
}

static LamIff *performIffSubstitutions(LamIff *iff,
                                       TpmcSubstitutionTable *substitutions) {
    ENTER(performIffSubstitutions);
    iff->condition = lamPerformSubstitutions(iff->condition, substitutions);
    iff->consequent = lamPerformSubstitutions(iff->consequent, substitutions);
    iff->alternative =
        lamPerformSubstitutions(iff->alternative, substitutions);
    LEAVE(performIffSubstitutions);
    return iff;
}

static LamLetRecBindings *performBindingsSubstitutions(LamLetRecBindings
                                                       *bindings, TpmcSubstitutionTable
                                                       *substitutions) {
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

static LamLet *performLetSubstitutions(LamLet *let,
                                       TpmcSubstitutionTable *substitutions) {
    ENTER(performLetSubstitutions);
    let->var = performVarSubstitutions(let->var, substitutions);
    let->value = lamPerformSubstitutions(let->value, substitutions);
    let->body = lamPerformSubstitutions(let->body, substitutions);
    LEAVE(performLetSubstitutions);
    return let;
}

static LamLetRec *performLetRecSubstitutions(LamLetRec *letrec, TpmcSubstitutionTable
                                             *substitutions) {
    ENTER(performLetRecSubstitutions);
    letrec->bindings =
        performBindingsSubstitutions(letrec->bindings, substitutions);
    letrec->body = lamPerformSubstitutions(letrec->body, substitutions);
    LEAVE(performLetRecSubstitutions);
    return letrec;
}

static LamTypeDefs *performTypeDefsSubstitutions(LamTypeDefs *typedefs, TpmcSubstitutionTable
                                                 *substitutions) {
    ENTER(performTypeDefsSubstitutions);
    typedefs->body = lamPerformSubstitutions(typedefs->body, substitutions);
    LEAVE(performTypeDefsSubstitutions);
    return typedefs;
}

static LamMatchList *performCaseSubstitutions(LamMatchList *cases, TpmcSubstitutionTable
                                              *substitutions) {
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

static LamMatch *performMatchSubstitutions(LamMatch *match, TpmcSubstitutionTable
                                           *substitutions) {
    ENTER(performMatchSubstitutions);
    match->index = lamPerformSubstitutions(match->index, substitutions);
    match->cases = performCaseSubstitutions(match->cases, substitutions);
    LEAVE(performMatchSubstitutions);
    return match;
}

static LamAmb *performAmbSubstitutions(LamAmb *amb,
                                       TpmcSubstitutionTable *substitutions) {
    ENTER(performAmbSubstitutions);
    amb->left = lamPerformSubstitutions(amb->left, substitutions);
    amb->right = lamPerformSubstitutions(amb->right, substitutions);
    LEAVE(performAmbSubstitutions);
    return amb;
}

static LamIntCondCases *performIntCondCaseSubstitutions(LamIntCondCases
                                                        *cases, TpmcSubstitutionTable
                                                        *substitutions) {
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

static LamCharCondCases *performCharCondCaseSubstitutions(LamCharCondCases
                                                          *cases, TpmcSubstitutionTable
                                                          *substitutions) {
    ENTER(performCharCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCharCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformSubstitutions(cases->body, substitutions);
    cases->next =
        performCharCondCaseSubstitutions(cases->next, substitutions);
    LEAVE(performCharCondCaseSubstitutions);
    return cases;
}

static LamCondCases *performCondCaseSubstitutions(LamCondCases *cases, TpmcSubstitutionTable
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

static LamCond *performCondSubstitutions(LamCond *cond, TpmcSubstitutionTable
                                         *substitutions) {
    ENTER(performCondSubstitutions);
    cond->value = lamPerformSubstitutions(cond->value, substitutions);
    cond->cases = performCondCaseSubstitutions(cond->cases, substitutions);
    LEAVE(performCondSubstitutions);
    return cond;
}

LamExp *lamPerformSubstitutions(LamExp *exp,
                                TpmcSubstitutionTable *substitutions) {
    ENTER(lamPerformSubstitutions);
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
            case LAMEXP_TYPE_VAR:
                exp->val.var =
                    performVarSubstitutions(exp->val.var, substitutions);
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
                    lamPerformSubstitutions(exp->val.tag, substitutions);
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
                    lamPerformSubstitutions(exp->val.callcc, substitutions);
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
    LEAVE(lamPerformSubstitutions);
    return exp;
}
