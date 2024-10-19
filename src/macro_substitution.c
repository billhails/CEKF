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
#include "symbol.h"
#include "lambda_pp.h"

#ifdef DEBUG_MACRO_SUBSTITUTE
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static HashSymbol *lookupOrCreateGenSym(HashSymbol *symbol, LamGenSymTable *gensyms) __attribute__((unused));

static HashSymbol *lookupOrCreateGenSym(HashSymbol *symbol, LamGenSymTable *gensyms) {
    HashSymbol *replacement = NULL;
    getLamGenSymTable(gensyms, symbol, &replacement);
    if (!replacement) {
        replacement = genSymDollar(symbol->name);
        setLamGenSymTable(gensyms, symbol, replacement);
    }
    return replacement;
}

static LamLam *performLamSubstitutions(LamLam *lam, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performLamSubstitutions);
    lam->exp = lamPerformMacroSubstitutions(lam->exp, substitutions, symbols);
    LEAVE(performLamSubstitutions);
    return lam;
}

static LamExp *performVarSubstitutions(HashSymbol *var, LamExpTable *substitutions, LamGenSymTable *symbols __attribute__((unused))) {
    ENTER(performVarSubstitutions);
    LamExp *replacement = NULL;
    getLamExpTable(substitutions, var, &replacement);
    LEAVE(performVarSubstitutions);
    return replacement;
}

static LamPrimApp *performPrimSubstitutions(LamPrimApp *prim, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performPrimSubstitutions);
    prim->exp1 = lamPerformMacroSubstitutions(prim->exp1, substitutions, symbols);
    prim->exp2 = lamPerformMacroSubstitutions(prim->exp2, substitutions, symbols);
    LEAVE(performPrimSubstitutions);
    return prim;
}

static LamUnaryApp *performUnarySubstitutions(LamUnaryApp *unary, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performUnarySubstitutions);
    unary->exp = lamPerformMacroSubstitutions(unary->exp, substitutions, symbols);
    LEAVE(performUnarySubstitutions);
    return unary;
}

static LamSequence *performSequenceSubstitutions(LamSequence *sequence, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performSequenceSubstitutions);
    if (sequence == NULL) {
        LEAVE(performSequenceSubstitutions);
        return NULL;
    }
    sequence->next =
        performSequenceSubstitutions(sequence->next, substitutions, symbols);
    sequence->exp = lamPerformMacroSubstitutions(sequence->exp, substitutions, symbols);
    LEAVE(performSequenceSubstitutions);
    return sequence;
}

static LamList *performListSubstitutions(LamList *list, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performListSubstitutions);
    if (list == NULL) {
        LEAVE(performListSubstitutions);
        return NULL;
    }
    list->next = performListSubstitutions(list->next, substitutions, symbols);
    list->exp = lamPerformMacroSubstitutions(list->exp, substitutions, symbols);
    LEAVE(performListSubstitutions);
    return list;
}

static LamTupleIndex *performTupleIndexSubstitutions(LamTupleIndex *tupleIndex, LamExpTable *substitutions, LamGenSymTable *symbols) {
    tupleIndex->exp = lamPerformMacroSubstitutions(tupleIndex->exp, substitutions, symbols);
    return tupleIndex;
}

static LamPrint *performPrintSubstitutions(LamPrint *print, LamExpTable *substitutions, LamGenSymTable *symbols) {
    print->exp = lamPerformMacroSubstitutions(print->exp, substitutions, symbols);
    print->printer = lamPerformMacroSubstitutions(print->printer, substitutions, symbols);
    return print;
}

static LamLookup *performLookupSubstitutions(LamLookup *lookup, LamExpTable *substitutions, LamGenSymTable *symbols) {
    lookup->exp = lamPerformMacroSubstitutions(lookup->exp, substitutions, symbols);
    return lookup;
}

static LamMakeVec *performMakeVecSubstitutions(LamMakeVec *makeVec, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performMakeVecSubstitutions);
    makeVec->args = performListSubstitutions(makeVec->args, substitutions, symbols);
    LEAVE(performMakeVecSubstitutions);
    return makeVec;
}

static LamDeconstruct *performDeconstructSubstitutions(LamDeconstruct *deconstruct, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performDeconstructSubstitutions);
    deconstruct->exp =
        lamPerformMacroSubstitutions(deconstruct->exp, substitutions, symbols);
    LEAVE(performDeconstructSubstitutions);
    return deconstruct;
}

static LamConstruct *performConstructSubstitutions(LamConstruct *construct, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performConstructSubstitutions);
    construct->args =
        performListSubstitutions(construct->args, substitutions, symbols);
    LEAVE(performConstructSubstitutions);
    return construct;
}

static LamApply *performApplySubstitutions(LamApply *apply, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performApplySubstitutions);
    apply->function = lamPerformMacroSubstitutions(apply->function, substitutions, symbols);
    apply->args = performListSubstitutions(apply->args, substitutions, symbols);
    LEAVE(performApplySubstitutions);
    return apply;
}

static LamIff *performIffSubstitutions(LamIff *iff, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performIffSubstitutions);
    iff->condition = lamPerformMacroSubstitutions(iff->condition, substitutions, symbols);
    iff->consequent = lamPerformMacroSubstitutions(iff->consequent, substitutions, symbols);
    iff->alternative =
        lamPerformMacroSubstitutions(iff->alternative, substitutions, symbols);
    LEAVE(performIffSubstitutions);
    return iff;
}

static LamLetRecBindings *performBindingsSubstitutions(LamLetRecBindings *bindings, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performBindingsSubstitutions);
        return NULL;
    }
    bindings->next =
        performBindingsSubstitutions(bindings->next, substitutions, symbols);
    if (bindings->isGenSym) {
        bindings->var = lookupOrCreateGenSym(bindings->var, symbols);
        bindings->isGenSym = false;
    }
    bindings->val = lamPerformMacroSubstitutions(bindings->val, substitutions, symbols);
    LEAVE(performBindingsSubstitutions);
    return bindings;
}

static LamLet *performLetSubstitutions(LamLet *let, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performLetSubstitutions);
    let->value = lamPerformMacroSubstitutions(let->value, substitutions, symbols);
    let->body = lamPerformMacroSubstitutions(let->body, substitutions, symbols);
    LEAVE(performLetSubstitutions);
    return let;
}

static LamLetRec *performLetRecSubstitutions(LamLetRec *letrec, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performLetRecSubstitutions);
    letrec->bindings =
        performBindingsSubstitutions(letrec->bindings, substitutions, symbols);
    letrec->body = lamPerformMacroSubstitutions(letrec->body, substitutions, symbols);
    LEAVE(performLetRecSubstitutions);
    return letrec;
}

static LamTypeDefs *performTypeDefsSubstitutions(LamTypeDefs *typedefs, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performTypeDefsSubstitutions);
    typedefs->body = lamPerformMacroSubstitutions(typedefs->body, substitutions, symbols);
    LEAVE(performTypeDefsSubstitutions);
    return typedefs;
}

static LamMatchList *performCaseSubstitutions(LamMatchList *cases, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCaseSubstitutions);
        return NULL;
    }
    cases->next = performCaseSubstitutions(cases->next, substitutions, symbols);
    cases->body = lamPerformMacroSubstitutions(cases->body, substitutions, symbols);
    LEAVE(performCaseSubstitutions);
    return cases;
}

static LamMatch *performMatchSubstitutions(LamMatch *match, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performMatchSubstitutions);
    match->index = lamPerformMacroSubstitutions(match->index, substitutions, symbols);
    match->cases = performCaseSubstitutions(match->cases, substitutions, symbols);
    LEAVE(performMatchSubstitutions);
    return match;
}

static LamAmb *performAmbSubstitutions(LamAmb *amb, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performAmbSubstitutions);
    amb->left = lamPerformMacroSubstitutions(amb->left, substitutions, symbols);
    amb->right = lamPerformMacroSubstitutions(amb->right, substitutions, symbols);
    LEAVE(performAmbSubstitutions);
    return amb;
}

static LamIntCondCases *performIntCondCaseSubstitutions(LamIntCondCases *cases, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performIntCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performIntCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformMacroSubstitutions(cases->body, substitutions, symbols);
    cases->next = performIntCondCaseSubstitutions(cases->next, substitutions, symbols);
    LEAVE(performIntCondCaseSubstitutions);
    return cases;
}

static LamCharCondCases *performCharCondCaseSubstitutions(LamCharCondCases *cases, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performCharCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCharCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformMacroSubstitutions(cases->body, substitutions, symbols);
    cases->next =
        performCharCondCaseSubstitutions(cases->next, substitutions, symbols);
    LEAVE(performCharCondCaseSubstitutions);
    return cases;
}

static LamCondCases *performCondCaseSubstitutions(LamCondCases *cases, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCondCaseSubstitutions);
        return NULL;
    }
    switch (cases->type) {
        case LAMCONDCASES_TYPE_INTEGERS:
            cases->val.integers =
                performIntCondCaseSubstitutions(cases->val.integers,
                                                substitutions, symbols);
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:
            cases->val.characters =
                performCharCondCaseSubstitutions(cases->val.characters,
                                                 substitutions, symbols);
            break;
        default:
            cant_happen
                ("unrecognised type %d in performCondCaseSubstitutions",
                 cases->type);
    }
    LEAVE(performCondCaseSubstitutions);
    return cases;
}

static LamCond *performCondSubstitutions(LamCond *cond, LamExpTable *substitutions, LamGenSymTable *symbols) {
    ENTER(performCondSubstitutions);
    cond->value = lamPerformMacroSubstitutions(cond->value, substitutions, symbols);
    cond->cases = performCondCaseSubstitutions(cond->cases, substitutions, symbols);
    LEAVE(performCondSubstitutions);
    return cond;
}

LamExp *lamPerformMacroSubstitutions(LamExp *exp, LamExpTable *substitutions, LamGenSymTable *symbols) {
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
                    performLamSubstitutions(exp->val.lam, substitutions, symbols);
                break;
            case LAMEXP_TYPE_VAR: {
                LamExp *rep = performVarSubstitutions(exp->val.var, substitutions, symbols);
                if (rep) {
                    exp = rep;
                }
            }
            break;
            case LAMEXP_TYPE_GENSYM: {
                exp = newLamExp_Var(CPI(exp), lookupOrCreateGenSym(exp->val.gensym, symbols));
            }
            break;
            case LAMEXP_TYPE_PRIM:
                exp->val.prim =
                    performPrimSubstitutions(exp->val.prim, substitutions, symbols);
                break;
            case LAMEXP_TYPE_UNARY:
                exp->val.unary =
                    performUnarySubstitutions(exp->val.unary, substitutions, symbols);
                break;
            case LAMEXP_TYPE_LIST:
                exp->val.list =
                    performSequenceSubstitutions(exp->val.list, substitutions, symbols);
                break;
            case LAMEXP_TYPE_MAKEVEC:
                exp->val.makeVec =
                    performMakeVecSubstitutions(exp->val.makeVec, substitutions, symbols);
                break;
            case LAMEXP_TYPE_DECONSTRUCT:
                exp->val.deconstruct =
                    performDeconstructSubstitutions(exp->val.deconstruct,
                                                    substitutions, symbols);
                break;
            case LAMEXP_TYPE_CONSTRUCT:
                exp->val.construct =
                    performConstructSubstitutions(exp->val.construct,
                                                  substitutions, symbols);
                break;
            case LAMEXP_TYPE_TAG:
                exp->val.tag =
                    lamPerformMacroSubstitutions(exp->val.tag, substitutions, symbols);
                break;
            case LAMEXP_TYPE_APPLY:
                exp->val.apply =
                    performApplySubstitutions(exp->val.apply, substitutions, symbols);
                break;
            case LAMEXP_TYPE_IFF:
                exp->val.iff =
                    performIffSubstitutions(exp->val.iff, substitutions, symbols);
                break;
            case LAMEXP_TYPE_COND:
                exp->val.cond =
                    performCondSubstitutions(exp->val.cond, substitutions, symbols);
                break;
            case LAMEXP_TYPE_CALLCC:
                exp->val.callcc =
                    lamPerformMacroSubstitutions(exp->val.callcc, substitutions, symbols);
                break;
            case LAMEXP_TYPE_LET:
                exp->val.let =
                    performLetSubstitutions(exp->val.let, substitutions, symbols);
                break;
            case LAMEXP_TYPE_LETREC:
                exp->val.letrec =
                    performLetRecSubstitutions(exp->val.letrec, substitutions, symbols);
                break;
            case LAMEXP_TYPE_TYPEDEFS:
                exp->val.typedefs =
                    performTypeDefsSubstitutions(exp->val.typedefs,
                                                 substitutions, symbols);
                break;
            case LAMEXP_TYPE_MATCH:
                exp->val.match =
                    performMatchSubstitutions(exp->val.match, substitutions, symbols);
                break;
            case LAMEXP_TYPE_AMB:
                exp->val.amb =
                    performAmbSubstitutions(exp->val.amb, substitutions, symbols);
                break;
            case LAMEXP_TYPE_MAKE_TUPLE:
                exp->val.make_tuple =
                    performListSubstitutions(exp->val.make_tuple, substitutions, symbols);
                break;
            case LAMEXP_TYPE_TUPLE_INDEX:
                exp->val.tuple_index =
                    performTupleIndexSubstitutions(exp->val.tuple_index, substitutions, symbols);
                break;
            case LAMEXP_TYPE_PRINT:
                exp->val.print = performPrintSubstitutions(exp->val.print, substitutions, symbols);
                break;
            case LAMEXP_TYPE_LOOKUP:
                exp->val.lookup = performLookupSubstitutions(exp->val.lookup, substitutions, symbols);
                break;
            default:
                cant_happen
                    ("unrecognized LamExp type %s", lamExpTypeName(exp->type));
        }
    }
    LEAVE(lamPerformMacroSubstitutions);
    return exp;
}
