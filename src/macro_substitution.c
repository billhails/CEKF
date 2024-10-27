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

static bool isReplacementSymbol(HashSymbol *var, LamMacroArgsTable *symbols) {
    return getLamMacroArgsTable(symbols, var, NULL);
}

static LamExp *performLamSubstitutions(LamLam *lam, LamMacroArgsTable *symbols) {
    ENTER(performLamSubstitutions);
    // fn () { a() } == a
    if (   lam->args == NULL
        && lam->exp->type == LAMEXP_TYPE_VAR
        && isReplacementSymbol(lam->exp->val.var, symbols)) {
        return lam->exp;
    }
    lam->exp = lamPerformMacroSubstitutions(lam->exp, symbols);
    LEAVE(performLamSubstitutions);
    return newLamExp_Lam(CPI(lam), lam);
}

static bool containsReplacementSymbols(LamVarList *vars, LamMacroArgsTable *symbols) {
    while (vars != NULL) {
        if (isReplacementSymbol(vars->var, symbols)) {
            return true;
        }
        vars = vars->next;
    }
    return false;
}

static LamMacroArgsTable *excludeSymbol(HashSymbol *var, LamMacroArgsTable *symbols) {
    LamMacroArgsTable *new = newLamMacroArgsTable();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateLamMacroArgsTable(symbols, &i, NULL)) != NULL) {
        if (current != var) {
            setLamMacroArgsTable(new, current, NULL);
        }
    }
    UNPROTECT(save);
    return new;
}

static bool varInVarList(HashSymbol *var, LamVarList *vars) {
    while (vars != NULL) {
        if (var == vars->var) {
            return true;
        }
        vars = vars->next;
    }
    return false;
}

static LamMacroArgsTable *excludeSymbols(LamVarList *vars, LamMacroArgsTable *symbols) {
    LamMacroArgsTable *new = newLamMacroArgsTable();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateLamMacroArgsTable(symbols, &i, NULL)) != NULL) {
        if (!varInVarList(current, vars)) {
            setLamMacroArgsTable(new, current, NULL);
        }
    }
    UNPROTECT(save);
    return new;
}

static LamVarList *collectLetRecNames(LamLetRecBindings *bindings) {
    if (bindings == NULL) {
        return NULL;
    }
    LamVarList *next = collectLetRecNames(bindings->next);
    int save = PROTECT(next);
    LamVarList *this = newLamVarList(CPI(bindings), bindings->var, next);
    UNPROTECT(save);
    return this;
}

static LamExp *performVarSubstitutions(ParserInfo PI, HashSymbol *var, LamMacroArgsTable *symbols) {
    ENTER(performVarSubstitutions);
    LamExp *replacement = NULL;
    if (isReplacementSymbol(var, symbols)) {
        LamExp *name = newLamExp_Var(PI, var);
        int save = PROTECT(name);
        LamApply *apply = newLamApply(PI, name, NULL);
        PROTECT(apply);
        replacement = newLamExp_Apply(PI, apply);
        UNPROTECT(save);
    }
    LEAVE(performVarSubstitutions);
    return replacement;
}

static LamPrimApp *performPrimSubstitutions(LamPrimApp *prim, LamMacroArgsTable *symbols) {
    ENTER(performPrimSubstitutions);
    prim->exp1 = lamPerformMacroSubstitutions(prim->exp1, symbols);
    prim->exp2 = lamPerformMacroSubstitutions(prim->exp2, symbols);
    LEAVE(performPrimSubstitutions);
    return prim;
}

static LamUnaryApp *performUnarySubstitutions(LamUnaryApp *unary, LamMacroArgsTable *symbols) {
    ENTER(performUnarySubstitutions);
    unary->exp = lamPerformMacroSubstitutions(unary->exp, symbols);
    LEAVE(performUnarySubstitutions);
    return unary;
}

static LamSequence *performSequenceSubstitutions(LamSequence *sequence, LamMacroArgsTable *symbols) {
    ENTER(performSequenceSubstitutions);
    if (sequence == NULL) {
        LEAVE(performSequenceSubstitutions);
        return NULL;
    }
    sequence->next =
        performSequenceSubstitutions(sequence->next, symbols);
    sequence->exp = lamPerformMacroSubstitutions(sequence->exp, symbols);
    LEAVE(performSequenceSubstitutions);
    return sequence;
}

static LamList *performListSubstitutions(LamList *list, LamMacroArgsTable *symbols) {
    ENTER(performListSubstitutions);
    if (list == NULL) {
        LEAVE(performListSubstitutions);
        return NULL;
    }
    list->next = performListSubstitutions(list->next, symbols);
    list->exp = lamPerformMacroSubstitutions(list->exp, symbols);
    LEAVE(performListSubstitutions);
    return list;
}

static LamTupleIndex *performTupleIndexSubstitutions(LamTupleIndex *tupleIndex, LamMacroArgsTable *symbols) {
    tupleIndex->exp = lamPerformMacroSubstitutions(tupleIndex->exp, symbols);
    return tupleIndex;
}

static LamPrint *performPrintSubstitutions(LamPrint *print, LamMacroArgsTable *symbols) {
    print->exp = lamPerformMacroSubstitutions(print->exp, symbols);
    print->printer = lamPerformMacroSubstitutions(print->printer, symbols);
    return print;
}

static LamLookup *performLookupSubstitutions(LamLookup *lookup, LamMacroArgsTable *symbols) {
    lookup->exp = lamPerformMacroSubstitutions(lookup->exp, symbols);
    return lookup;
}

static LamMakeVec *performMakeVecSubstitutions(LamMakeVec *makeVec, LamMacroArgsTable *symbols) {
    ENTER(performMakeVecSubstitutions);
    makeVec->args = performListSubstitutions(makeVec->args, symbols);
    LEAVE(performMakeVecSubstitutions);
    return makeVec;
}

static LamDeconstruct *performDeconstructSubstitutions(LamDeconstruct *deconstruct, LamMacroArgsTable *symbols) {
    ENTER(performDeconstructSubstitutions);
    deconstruct->exp =
        lamPerformMacroSubstitutions(deconstruct->exp, symbols);
    LEAVE(performDeconstructSubstitutions);
    return deconstruct;
}

static LamConstruct *performConstructSubstitutions(LamConstruct *construct, LamMacroArgsTable *symbols) {
    ENTER(performConstructSubstitutions);
    construct->args =
        performListSubstitutions(construct->args, symbols);
    LEAVE(performConstructSubstitutions);
    return construct;
}

static LamApply *performApplySubstitutions(LamApply *apply, LamMacroArgsTable *symbols) {
    ENTER(performApplySubstitutions);
    apply->function = lamPerformMacroSubstitutions(apply->function, symbols);
    apply->args = performListSubstitutions(apply->args, symbols);
    LEAVE(performApplySubstitutions);
    return apply;
}

static LamIff *performIffSubstitutions(LamIff *iff, LamMacroArgsTable *symbols) {
    ENTER(performIffSubstitutions);
    iff->condition = lamPerformMacroSubstitutions(iff->condition, symbols);
    iff->consequent = lamPerformMacroSubstitutions(iff->consequent, symbols);
    iff->alternative =
        lamPerformMacroSubstitutions(iff->alternative, symbols);
    LEAVE(performIffSubstitutions);
    return iff;
}

static LamLetRecBindings *performBindingsSubstitutions(LamLetRecBindings *bindings, LamMacroArgsTable *symbols) {
    ENTER(performBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performBindingsSubstitutions);
        return NULL;
    }
    bindings->next = performBindingsSubstitutions(bindings->next, symbols);
    bindings->val = lamPerformMacroSubstitutions(bindings->val, symbols);
    LEAVE(performBindingsSubstitutions);
    return bindings;
}

static LamExp *performMacroSubstitutionsMinusReplacement(HashSymbol *replacement,
                                                         LamExp *exp,
                                                         LamMacroArgsTable *symbols) {
    LamMacroArgsTable *remaining = excludeSymbol(replacement, symbols);
    int save = PROTECT(remaining);
    LamExp *res = lamPerformMacroSubstitutions(exp, remaining);
    UNPROTECT(save);
    return res;
}

static LamLet *performLetSubstitutions(LamLet *let, LamMacroArgsTable *symbols) {
    ENTER(performLetSubstitutions);
    let->value = lamPerformMacroSubstitutions(let->value, symbols);
    if (isReplacementSymbol(let->var, symbols)) {
        let->body = performMacroSubstitutionsMinusReplacement(let->var, let->body, symbols);
    } else {
        let->body = lamPerformMacroSubstitutions(let->body, symbols);
    }
    LEAVE(performLetSubstitutions);
    return let;
}

static LamLetRec *performLetRecSubstitutions(LamLetRec *letrec, LamMacroArgsTable *symbols) {
    ENTER(performLetRecSubstitutions);
    LamVarList *names = collectLetRecNames(letrec->bindings);
    int save = PROTECT(names);
    if (containsReplacementSymbols(names, symbols)) {
        LamMacroArgsTable *reduced = excludeSymbols(names, symbols);
        PROTECT(reduced);
        letrec->bindings = performBindingsSubstitutions(letrec->bindings, reduced);
        letrec->body = lamPerformMacroSubstitutions(letrec->body, reduced);
    } else {
        letrec->bindings = performBindingsSubstitutions(letrec->bindings, symbols);
        letrec->body = lamPerformMacroSubstitutions(letrec->body, symbols);
    }
    LEAVE(performLetRecSubstitutions);
    UNPROTECT(save);
    return letrec;
}

static LamTypeDefs *performTypeDefsSubstitutions(LamTypeDefs *typedefs, LamMacroArgsTable *symbols) {
    ENTER(performTypeDefsSubstitutions);
    typedefs->body = lamPerformMacroSubstitutions(typedefs->body, symbols);
    LEAVE(performTypeDefsSubstitutions);
    return typedefs;
}

static LamMatchList *performCaseSubstitutions(LamMatchList *cases, LamMacroArgsTable *symbols) {
    ENTER(performCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCaseSubstitutions);
        return NULL;
    }
    cases->next = performCaseSubstitutions(cases->next, symbols);
    cases->body = lamPerformMacroSubstitutions(cases->body, symbols);
    LEAVE(performCaseSubstitutions);
    return cases;
}

static LamMatch *performMatchSubstitutions(LamMatch *match, LamMacroArgsTable *symbols) {
    ENTER(performMatchSubstitutions);
    match->index = lamPerformMacroSubstitutions(match->index, symbols);
    match->cases = performCaseSubstitutions(match->cases, symbols);
    LEAVE(performMatchSubstitutions);
    return match;
}

static LamAmb *performAmbSubstitutions(LamAmb *amb, LamMacroArgsTable *symbols) {
    ENTER(performAmbSubstitutions);
    amb->left = lamPerformMacroSubstitutions(amb->left, symbols);
    amb->right = lamPerformMacroSubstitutions(amb->right, symbols);
    LEAVE(performAmbSubstitutions);
    return amb;
}

static LamIntCondCases *performIntCondCaseSubstitutions(LamIntCondCases *cases, LamMacroArgsTable *symbols) {
    ENTER(performIntCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performIntCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformMacroSubstitutions(cases->body, symbols);
    cases->next = performIntCondCaseSubstitutions(cases->next, symbols);
    LEAVE(performIntCondCaseSubstitutions);
    return cases;
}

static LamCharCondCases *performCharCondCaseSubstitutions(LamCharCondCases *cases, LamMacroArgsTable *symbols) {
    ENTER(performCharCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCharCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformMacroSubstitutions(cases->body, symbols);
    cases->next =
        performCharCondCaseSubstitutions(cases->next, symbols);
    LEAVE(performCharCondCaseSubstitutions);
    return cases;
}

static LamCondCases *performCondCaseSubstitutions(LamCondCases *cases, LamMacroArgsTable *symbols) {
    ENTER(performCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCondCaseSubstitutions);
        return NULL;
    }
    switch (cases->type) {
        case LAMCONDCASES_TYPE_INTEGERS:
            cases->val.integers =
                performIntCondCaseSubstitutions(cases->val.integers,
                                                symbols);
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:
            cases->val.characters =
                performCharCondCaseSubstitutions(cases->val.characters,
                                                 symbols);
            break;
        default:
            cant_happen
                ("unrecognised type %d in performCondCaseSubstitutions",
                 cases->type);
    }
    LEAVE(performCondCaseSubstitutions);
    return cases;
}

static LamCond *performCondSubstitutions(LamCond *cond, LamMacroArgsTable *symbols) {
    ENTER(performCondSubstitutions);
    cond->value = lamPerformMacroSubstitutions(cond->value, symbols);
    cond->cases = performCondCaseSubstitutions(cond->cases, symbols);
    LEAVE(performCondSubstitutions);
    return cond;
}

LamExp *lamPerformMacroSubstitutions(LamExp *exp, LamMacroArgsTable *symbols) {
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
                exp = performLamSubstitutions(exp->val.lam, symbols);
                break;
            case LAMEXP_TYPE_VAR: {
                LamExp *rep = performVarSubstitutions(CPI(exp), exp->val.var, symbols);
                if (rep) {
                    exp = rep;
                }
            }
            break;
            case LAMEXP_TYPE_PRIM:
                exp->val.prim = performPrimSubstitutions(exp->val.prim, symbols);
                break;
            case LAMEXP_TYPE_UNARY:
                exp->val.unary = performUnarySubstitutions(exp->val.unary, symbols);
                break;
            case LAMEXP_TYPE_LIST:
                exp->val.list = performSequenceSubstitutions(exp->val.list, symbols);
                break;
            case LAMEXP_TYPE_MAKEVEC:
                exp->val.makeVec = performMakeVecSubstitutions(exp->val.makeVec, symbols);
                break;
            case LAMEXP_TYPE_DECONSTRUCT:
                exp->val.deconstruct = performDeconstructSubstitutions(exp->val.deconstruct, symbols);
                break;
            case LAMEXP_TYPE_CONSTRUCT:
                exp->val.construct = performConstructSubstitutions(exp->val.construct, symbols);
                break;
            case LAMEXP_TYPE_TAG:
                exp->val.tag = lamPerformMacroSubstitutions(exp->val.tag, symbols);
                break;
            case LAMEXP_TYPE_APPLY:
                exp->val.apply = performApplySubstitutions(exp->val.apply, symbols);
                break;
            case LAMEXP_TYPE_IFF:
                exp->val.iff = performIffSubstitutions(exp->val.iff, symbols);
                break;
            case LAMEXP_TYPE_COND:
                exp->val.cond = performCondSubstitutions(exp->val.cond, symbols);
                break;
            case LAMEXP_TYPE_CALLCC:
                exp->val.callcc = lamPerformMacroSubstitutions(exp->val.callcc, symbols);
                break;
            case LAMEXP_TYPE_LET:
                exp->val.let = performLetSubstitutions(exp->val.let, symbols);
                break;
            case LAMEXP_TYPE_LETREC:
                exp->val.letrec = performLetRecSubstitutions(exp->val.letrec, symbols);
                break;
            case LAMEXP_TYPE_TYPEDEFS:
                exp->val.typedefs = performTypeDefsSubstitutions(exp->val.typedefs, symbols);
                break;
            case LAMEXP_TYPE_MATCH:
                exp->val.match = performMatchSubstitutions(exp->val.match, symbols);
                break;
            case LAMEXP_TYPE_AMB:
                exp->val.amb = performAmbSubstitutions(exp->val.amb, symbols);
                break;
            case LAMEXP_TYPE_MAKE_TUPLE:
                exp->val.make_tuple = performListSubstitutions(exp->val.make_tuple, symbols);
                break;
            case LAMEXP_TYPE_TUPLE_INDEX:
                exp->val.tuple_index = performTupleIndexSubstitutions(exp->val.tuple_index, symbols);
                break;
            case LAMEXP_TYPE_PRINT:
                exp->val.print = performPrintSubstitutions(exp->val.print, symbols);
                break;
            case LAMEXP_TYPE_LOOKUP:
                exp->val.lookup = performLookupSubstitutions(exp->val.lookup, symbols);
                break;
            default:
                cant_happen("unrecognized %s", lamExpTypeName(exp->type));
        }
    }
    LEAVE(lamPerformMacroSubstitutions);
    return exp;
}
