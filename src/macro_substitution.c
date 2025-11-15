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

/**
 * @brief True if the variable is an argument to the macro being converted.
 * 
 * @param var The variable to check.
 * @param symbols The set of macro arguments.
 * @return True if the variable is an argument to the macro, false otherwise.
 */
static bool isMacroArgument(HashSymbol *var, LamMacroArgsSet *symbols) {
    return getLamMacroArgsSet(symbols, var);
}

/**
 * @brief Perform macro substitutions on a lambda defined in the body of a macro.
 * 
 * @param lam The lambda expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified lambda expression.
 */
static LamExp *performLamSubstitutions(LamLam *lam, LamMacroArgsSet *symbols) {
    ENTER(performLamSubstitutions);
    // fn () { a() } == a
    if (   lam->args == NULL
        && lam->exp->type == LAMEXP_TYPE_VAR
        && isMacroArgument(lam->exp->val.var, symbols)) {
        return lam->exp;
    }
    lam->exp = lamPerformMacroSubstitutions(lam->exp, symbols);
    LEAVE(performLamSubstitutions);
    return newLamExp_Lam(CPI(lam), lam);
}

/**
 * @brief Check if any variables in the list are macro arguments.
 * 
 * @param vars The list of variables to check.
 * @param symbols The current set of macro arguments.
 * @return True if any variable is a macro argument, false otherwise.
 */
static bool containsMacroArguments(LamVarList *vars, LamMacroArgsSet *symbols) {
    while (vars != NULL) {
        if (isMacroArgument(vars->var, symbols)) {
            return true;
        }
        vars = vars->next;
    }
    return false;
}

/**
 * @brief Exclude a symbol from the set of macro arguments.
 * 
 * @param var The variable to exclude.
 * @param symbols The current set of macro arguments.
 * @return A new set of macro arguments without the excluded symbol.
 */
static LamMacroArgsSet *excludeSymbol(HashSymbol *var, LamMacroArgsSet *symbols) {
    LamMacroArgsSet *new = newLamMacroArgsSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateLamMacroArgsSet(symbols, &i)) != NULL) {
        if (current != var) {
            setLamMacroArgsSet(new, current);
        }
    }
    UNPROTECT(save);
    return new;
}

/**
 * @brief Copy a set of macro arguments.
 * 
 * @param symbols The current set of macro arguments.
 * @return A new set of macro arguments.
 */
static LamMacroArgsSet *copyLamMacroArgsSet(LamMacroArgsSet *symbols) {
    LamMacroArgsSet *new = newLamMacroArgsSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateLamMacroArgsSet(symbols, &i)) != NULL) {
        setLamMacroArgsSet(new, current);
    }
    UNPROTECT(save);
    return new;
}

/**
 * @brief Check if a variable is in a list of variables.
 * 
 * @param var The variable to check.
 * @param vars The list of variables to search in.
 * @return True if the variable is found, false otherwise.
 */
static bool varInVarList(HashSymbol *var, LamVarList *vars) {
    while (vars != NULL) {
        if (var == vars->var) {
            return true;
        }
        vars = vars->next;
    }
    return false;
}

/**
 * @brief Exclude a list of variables from the set of macro arguments.
 * 
 * @param vars The list of variables to exclude.
 * @param symbols The current set of macro arguments.
 * @return A new set of macro arguments without the excluded variables.
 */
static LamMacroArgsSet *excludeSymbols(LamVarList *vars, LamMacroArgsSet *symbols) {
    LamMacroArgsSet *new = newLamMacroArgsSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateLamMacroArgsSet(symbols, &i)) != NULL) {
        if (!varInVarList(current, vars)) {
            setLamMacroArgsSet(new, current);
        }
    }
    UNPROTECT(save);
    return new;
}

/**
 * @brief Collect the names of all letrec bindings in the argument list of letrecs.
 * 
 * @param bindings The letrec bindings to collect names from.
 * @return A list of variable names for the letrec bindings.
 */
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

/**
 * @brief Replaces a macro argument with an invocation of the argument.
 *
 * i.e `a` becomes `a()`
 * 
 * This is the second half of the lazy evaluation of macros.
 * The first part, `thunkMacroArg` in `lambda_conversion.c`, wrap all the unevaluated
 * arguments to macros in thunks.
 * So in the example above, `a` is already a thunk at this point.
 *
 * @param PI The parser information.
 * @param var The variable to consider for substitution.
 * @param symbols The current set of macro arguments.
 * @return The expression with the variable substituted, or just the variable if it is not a macro argument.
 */
static LamExp *performVarSubstitution(ParserInfo PI, HashSymbol *var, LamMacroArgsSet *symbols) {
    ENTER(performVarSubstitution);
    LamExp *replacement = NULL;
    if (isMacroArgument(var, symbols)) {
        // Macro arguments are thunks that need to be forced
        // Call the thunk with no arguments: var()
        LamExp *name = newLamExp_Var(PI, var);
        int save = PROTECT(name);
        LamApply *apply = newLamApply(PI, name, NULL);
        PROTECT(apply);
        replacement = newLamExp_Apply(PI, apply);
        UNPROTECT(save);
    }
    LEAVE(performVarSubstitution);
    return replacement;
}

/**
 * @brief Recurse into a primitive application performing substitutions on its arguments.
 * @param prim The primitive application to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified primitive application.
 */
static LamPrimApp *performPrimSubstitutions(LamPrimApp *prim, LamMacroArgsSet *symbols) {
    ENTER(performPrimSubstitutions);
    prim->exp1 = lamPerformMacroSubstitutions(prim->exp1, symbols);
    prim->exp2 = lamPerformMacroSubstitutions(prim->exp2, symbols);
    LEAVE(performPrimSubstitutions);
    return prim;
}

/**
 * @brief iterates over a sequence, performing substitutions on each element.
 * 
 * @param sequence The sequence to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified sequence.
 */
static LamSequence *performSequenceSubstitutions(LamSequence *sequence, LamMacroArgsSet *symbols) {
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

/**
 * @brief iterates over a list of arguments in a function application.
 * Performs substitutions on each argument.
 * @param list The list of arguments to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified list of arguments.
 */
static LamArgs *performArgsSubstitutions(LamArgs *list, LamMacroArgsSet *symbols) {
    ENTER(performArgsSubstitutions);
    if (list == NULL) {
        LEAVE(performArgsSubstitutions);
        return NULL;
    }
    list->next = performArgsSubstitutions(list->next, symbols);
    list->exp = lamPerformMacroSubstitutions(list->exp, symbols);
    LEAVE(performArgsSubstitutions);
    return list;
}

/**
 * @brief Performs substitutions on the expression in a tuple index.
 * @param tupleIndex The tuple index to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified tuple index expression.
 */
static LamTupleIndex *performTupleIndexSubstitutions(LamTupleIndex *tupleIndex, LamMacroArgsSet *symbols) {
    tupleIndex->exp = lamPerformMacroSubstitutions(tupleIndex->exp, symbols);
    return tupleIndex;
}

/**
 * @brief Performs substitutions on the argument to a print expression.
 * @param print The print expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified print expression.
 */
static LamPrint *performPrintSubstitutions(LamPrint *print, LamMacroArgsSet *symbols) {
    print->exp = lamPerformMacroSubstitutions(print->exp, symbols);
    print->printer = lamPerformMacroSubstitutions(print->printer, symbols);
    return print;
}

/**
 * @brief Performs macro substitutions on a lookup expression.
 * @param lookup The lookup expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified lookup expression.
 */
static LamLookup *performLookupSubstitutions(LamLookup *lookup, LamMacroArgsSet *symbols) {
    lookup->exp = lamPerformMacroSubstitutions(lookup->exp, symbols);
    return lookup;
}

/**
 * @brief Performs macro substitutions on a make vector expression.
 * @param makeVec The make vector expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified make vector expression.
 */
static LamMakeVec *performMakeVecSubstitutions(LamMakeVec *makeVec, LamMacroArgsSet *symbols) {
    ENTER(performMakeVecSubstitutions);
    makeVec->args = performArgsSubstitutions(makeVec->args, symbols);
    LEAVE(performMakeVecSubstitutions);
    return makeVec;
}

/**
 * @brief Performs macro substitutions on a deconstruct expression.
 * @param deconstruct The deconstruct expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified deconstruct expression.
 */
static LamDeconstruct *performDeconstructSubstitutions(LamDeconstruct *deconstruct, LamMacroArgsSet *symbols) {
    ENTER(performDeconstructSubstitutions);
    deconstruct->exp =
        lamPerformMacroSubstitutions(deconstruct->exp, symbols);
    LEAVE(performDeconstructSubstitutions);
    return deconstruct;
}

/**
 * @brief Performs macro substitutions on a construct expression.
 * @param construct The construct expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified construct expression.
 */
static LamConstruct *performConstructSubstitutions(LamConstruct *construct, LamMacroArgsSet *symbols) {
    ENTER(performConstructSubstitutions);
    construct->args =
        performArgsSubstitutions(construct->args, symbols);
    LEAVE(performConstructSubstitutions);
    return construct;
}

/**
 * @brief Performs macro substitutions on a function application expression.
 * @param apply The function application expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified function application expression.
 */
static LamApply *performApplySubstitutions(LamApply *apply, LamMacroArgsSet *symbols) {
    ENTER(performApplySubstitutions);
    apply->function = lamPerformMacroSubstitutions(apply->function, symbols);
    apply->args = performArgsSubstitutions(apply->args, symbols);
    LEAVE(performApplySubstitutions);
    return apply;
}

/**
 * @brief Performs macro substitutions on an if expression.
 * @param iff The if expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified if expression.
 */
static LamIff *performIffSubstitutions(LamIff *iff, LamMacroArgsSet *symbols) {
    ENTER(performIffSubstitutions);
    iff->condition = lamPerformMacroSubstitutions(iff->condition, symbols);
    iff->consequent = lamPerformMacroSubstitutions(iff->consequent, symbols);
    iff->alternative =
        lamPerformMacroSubstitutions(iff->alternative, symbols);
    LEAVE(performIffSubstitutions);
    return iff;
}

/**
 * @brief Performs macro substitutions on a list of letrec bindings.
 * @param let The letrec bindings to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified letrec bindings.
 */
static LamLetRecBindings *performBindingsSubstitutions(LamLetRecBindings *bindings, LamMacroArgsSet *symbols) {
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

/**
 * @brief Performs macro substitutions on a list of let bindings.
 * @param bindings The let bindings to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified let bindings.
 */
static LamLetBindings *performLetBindingsSubstitutions(LamLetBindings *bindings, LamMacroArgsSet **symbols) {
    ENTER(performLetBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performLetBindingsSubstitutions);
        return NULL;
    }
    if (isMacroArgument(bindings->var, *symbols)) {
        *symbols = excludeSymbol(bindings->var, *symbols);
        PROTECT(symbols); // caller will UNPROTECT
    }
    bindings->val = lamPerformMacroSubstitutions(bindings->val, *symbols);
    bindings->next = performLetBindingsSubstitutions(bindings->next, symbols);
    bindings->val = lamPerformMacroSubstitutions(bindings->val, *symbols);
    LEAVE(performLetBindingsSubstitutions);
    return bindings;
}

/**
 * @brief Performs macro substitutions on a let expression.
 * @param let The let expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified let expression.
 */
static LamLet *performLetSubstitutions(LamLet *let, LamMacroArgsSet *symbols) {
    ENTER(performLetSubstitutions);
    LamMacroArgsSet *remaining = copyLamMacroArgsSet(symbols);
    int save = PROTECT(remaining);
    let->bindings = performLetBindingsSubstitutions(let->bindings, &remaining);
    let->body = lamPerformMacroSubstitutions(let->body, remaining);
    UNPROTECT(save);
    LEAVE(performLetSubstitutions);
    return let;
}

/**
 * @brief Performs macro substitutions on a letrec expression.
 * 
 * Arranges to exclude *all* letrec bindings from the list of macro arguments
 * before recursing into the letrec bodies.
 * 
 * @param letrec The letrec expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified letrec expression.
 */
static LamLetRec *performLetRecSubstitutions(LamLetRec *letrec, LamMacroArgsSet *symbols) {
    ENTER(performLetRecSubstitutions);
    LamVarList *names = collectLetRecNames(letrec->bindings);
    int save = PROTECT(names);
    if (containsMacroArguments(names, symbols)) {
        LamMacroArgsSet *reduced = excludeSymbols(names, symbols);
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

/**
 * @brief Performs macro substitutions on the body of a typedef.
 * @param typedefs The typedef to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified typedef.
 */
static LamTypeDefs *performTypeDefsSubstitutions(LamTypeDefs *typedefs, LamMacroArgsSet *symbols) {
    ENTER(performTypeDefsSubstitutions);
    typedefs->body = lamPerformMacroSubstitutions(typedefs->body, symbols);
    LEAVE(performTypeDefsSubstitutions);
    return typedefs;
}

/**
 * @brief Performs macro substitutions on the bodies of a case expression.
 * @param cases The case expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified case expression.
 */
static LamMatchList *performCaseSubstitutions(LamMatchList *cases, LamMacroArgsSet *symbols) {
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

/**
 * @brief Performs macro substitutions on the cases of a match expression.
 * @param match The match expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified match expression.
 */
static LamMatch *performMatchSubstitutions(LamMatch *match, LamMacroArgsSet *symbols) {
    ENTER(performMatchSubstitutions);
    match->index = lamPerformMacroSubstitutions(match->index, symbols);
    match->cases = performCaseSubstitutions(match->cases, symbols);
    LEAVE(performMatchSubstitutions);
    return match;
}

/**
 * @brief Performs macro substitutions on an amb (ambivalent) expression.
 * @param amb The amb expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified amb expression.
 */
static LamAmb *performAmbSubstitutions(LamAmb *amb, LamMacroArgsSet *symbols) {
    ENTER(performAmbSubstitutions);
    amb->left = lamPerformMacroSubstitutions(amb->left, symbols);
    amb->right = lamPerformMacroSubstitutions(amb->right, symbols);
    LEAVE(performAmbSubstitutions);
    return amb;
}

/**
 * @brief Performs macro substitutions on integer conditional cases.
 * @param cases The integer conditional cases to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified integer conditional cases.
 */
static LamIntCondCases *performIntCondCaseSubstitutions(LamIntCondCases *cases,
                                                        LamMacroArgsSet *symbols) {
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

/**
 * @brief Performs macro substitutions on character conditional cases.
 * @param cases The character conditional cases to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified character conditional cases.
 */
static LamCharCondCases *performCharCondCaseSubstitutions(LamCharCondCases *cases, LamMacroArgsSet *symbols) {
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

/**
 * @brief Performs macro substitutions on conditional cases.
 *
 * Switches between char and int conditional cases appropriately.
 * 
 * @param cases The conditional cases to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified conditional cases.
 */
static LamCondCases *performCondCaseSubstitutions(LamCondCases *cases, LamMacroArgsSet *symbols) {
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

/**
 * @brief Performs macro substitutions on a cond expression.
 * @param cond The conditional expression to modify.
 * @param symbols The current set of macro arguments.
 * @return The modified conditional expression.
 */
static LamCond *performCondSubstitutions(LamCond *cond, LamMacroArgsSet *symbols) {
    ENTER(performCondSubstitutions);
    cond->value = lamPerformMacroSubstitutions(cond->value, symbols);
    cond->cases = performCondCaseSubstitutions(cond->cases, symbols);
    LEAVE(performCondSubstitutions);
    return cond;
}

/**
 * @brief Performs macro substitutions on a lambda expression.
 *
 * When called externally, the `exp` is the body of the macro
 * and the `symbols` table contains the arguments to the macro.
 * 
 * @param exp The lambda expression to modify.
 * @param symbols The set of macro arguments.
 * @return The modified lambda expression.
 */
LamExp *lamPerformMacroSubstitutions(LamExp *exp, LamMacroArgsSet *symbols) {
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
                LamExp *rep = performVarSubstitution(CPI(exp), exp->val.var, symbols);
                if (rep) {
                    exp = rep;
                }
            }
            break;
            case LAMEXP_TYPE_PRIM:
                exp->val.prim = performPrimSubstitutions(exp->val.prim, symbols);
                break;
            case LAMEXP_TYPE_SEQUENCE:
                exp->val.sequence = performSequenceSubstitutions(exp->val.sequence, symbols);
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
                exp->val.make_tuple = performArgsSubstitutions(exp->val.make_tuple, symbols);
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
