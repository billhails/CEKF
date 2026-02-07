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

#include "lazy_substitution.h"
#include "common.h"
#include "lambda_pp.h"
#include "symbol.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef DEBUG_MACRO_SUBSTITUTE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static SymbolSet *excludeSymbols(LamVarList *, SymbolSet *);

/**
 * @brief True if the variable is an argument to the function being converted.
 *
 * @param var The variable to check.
 * @param symbols The set of arguments.
 * @return True if the variable is an argument to the function, false otherwise.
 */
static bool isLazyArgument(HashSymbol *var, SymbolSet *symbols) {
    return getSymbolSet(symbols, var);
}

/**
 * @brief Perform substitutions on a lambda defined in the body of a
 * function.
 *
 * @param lam The lambda expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified lambda expression.
 */
static LamExp *performLamSubstitutions(LamLam *lam, SymbolSet *symbols) {
    ENTER(performLamSubstitutions);
#if 1
    // fn () { a() } == a
    if (lam->args == NULL && lam->exp->type == LAMEXP_TYPE_VAR &&
        isLazyArgument(getLamExp_Var(lam->exp), symbols)) {
        return lam->exp;
    }
#endif
    SymbolSet *newSymbols = excludeSymbols(lam->args, symbols);
    int save = PROTECT(newSymbols);
    lam->exp = lamPerformLazySubstitutions(lam->exp, newSymbols);
    UNPROTECT(save);
    LEAVE(performLamSubstitutions);
    return newLamExp_Lam(CPI(lam), lam);
}

/**
 * @brief Check if any variables in the list are arguments.
 *
 * @param vars The list of variables to check.
 * @param symbols The current set of arguments.
 * @return True if any variable is a argument, false otherwise.
 */
static bool containsLazyArguments(LamVarList *vars, SymbolSet *symbols) {
    while (vars != NULL) {
        if (isLazyArgument(vars->var, symbols)) {
            return true;
        }
        vars = vars->next;
    }
    return false;
}

/**
 * @brief Exclude a symbol from the set of arguments.
 *
 * @param var The variable to exclude.
 * @param symbols The current set of arguments.
 * @return A new set of arguments without the excluded symbol.
 */
static SymbolSet *excludeSymbol(HashSymbol *var, SymbolSet *symbols) {
    SymbolSet *new = newSymbolSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateSymbolSet(symbols, &i)) != NULL) {
        if (current != var) {
            setSymbolSet(new, current);
        }
    }
    UNPROTECT(save);
    return new;
}

/**
 * @brief Copy a set of arguments.
 *
 * @param symbols The current set of arguments.
 * @return A new set of arguments.
 */
static SymbolSet *copySymbolSet(SymbolSet *symbols) {
    SymbolSet *new = newSymbolSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateSymbolSet(symbols, &i)) != NULL) {
        setSymbolSet(new, current);
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
 * @brief Exclude a list of variables from the set of arguments.
 *
 * @param vars The list of variables to exclude.
 * @param symbols The current set of arguments.
 * @return A new set of arguments without the excluded variables.
 */
static SymbolSet *excludeSymbols(LamVarList *vars, SymbolSet *symbols) {
    SymbolSet *new = newSymbolSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateSymbolSet(symbols, &i)) != NULL) {
        if (!varInVarList(current, vars)) {
            setSymbolSet(new, current);
        }
    }
    UNPROTECT(save);
    return new;
}

/**
 * @brief Collect the names of all letrec bindings in the argument list of
 * letrecs.
 *
 * @param bindings The letrec bindings to collect names from.
 * @return A list of variable names for the letrec bindings.
 */
static LamVarList *collectLetRecNames(LamBindings *bindings) {
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
 * @brief Replaces an argument with an invocation of the argument.
 *
 * i.e `a` becomes `a()`
 *
 * This is the second half of the lazy evaluation of functions.
 * The first part, `thunkLazyArg` in `lambda_conversion.c`, wraps all the
 * unevaluated arguments to lazy functions in thunks. So in the example
 * above, `a` is already a thunk at this point.
 *
 * @param PI The parser information.
 * @param exp The variable expression to consider for substitution.
 * @param symbols The current set of arguments.
 * @return The modified or original expression.
 */
static LamExp *performVarSubstitution(ParserInfo PI, LamExp *exp,
                                      SymbolSet *symbols) {
    ENTER(performVarSubstitution);
    if (isLazyArgument(getLamExp_Var(exp), symbols)) {
        exp = makeLamExp_Apply(PI, exp, NULL);
    }
    LEAVE(performVarSubstitution);
    return exp;
}

/**
 * @brief Recurse into a primitive application performing substitutions on its
 * arguments.
 * @param prim The primitive application to modify.
 * @param symbols The current set of arguments.
 * @return The modified primitive application.
 */
static LamPrimApp *performPrimSubstitutions(LamPrimApp *prim,
                                            SymbolSet *symbols) {
    ENTER(performPrimSubstitutions);
    prim->exp1 = lamPerformLazySubstitutions(prim->exp1, symbols);
    prim->exp2 = lamPerformLazySubstitutions(prim->exp2, symbols);
    LEAVE(performPrimSubstitutions);
    return prim;
}

/**
 * @brief iterates over a sequence, performing substitutions on each element.
 *
 * @param sequence The sequence to modify.
 * @param symbols The current set of arguments.
 * @return The modified sequence.
 */
static LamSequence *performSequenceSubstitutions(LamSequence *sequence,
                                                 SymbolSet *symbols) {
    ENTER(performSequenceSubstitutions);
    if (sequence == NULL) {
        LEAVE(performSequenceSubstitutions);
        return NULL;
    }
    sequence->next = performSequenceSubstitutions(sequence->next, symbols);
    sequence->exp = lamPerformLazySubstitutions(sequence->exp, symbols);
    LEAVE(performSequenceSubstitutions);
    return sequence;
}

/**
 * @brief iterates over a list of arguments in a function application.
 * Performs substitutions on each argument.
 * @param list The list of arguments to modify.
 * @param symbols The current set of arguments.
 * @return The modified list of arguments.
 */
static LamArgs *performArgsSubstitutions(LamArgs *list, SymbolSet *symbols) {
    ENTER(performArgsSubstitutions);
    if (list == NULL) {
        LEAVE(performArgsSubstitutions);
        return NULL;
    }
    list->next = performArgsSubstitutions(list->next, symbols);
    list->exp = lamPerformLazySubstitutions(list->exp, symbols);
    LEAVE(performArgsSubstitutions);
    return list;
}

/**
 * @brief Performs substitutions on the expression in a tuple index.
 * @param tupleIndex The tuple index to modify.
 * @param symbols The current set of arguments.
 * @return The modified tuple index expression.
 */
static LamTupleIndex *performTupleIndexSubstitutions(LamTupleIndex *tupleIndex,
                                                     SymbolSet *symbols) {
    tupleIndex->exp = lamPerformLazySubstitutions(tupleIndex->exp, symbols);
    return tupleIndex;
}

/**
 * @brief Performs substitutions on the argument to a print expression.
 * @param print The print expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified print expression.
 */
static LamPrint *performPrintSubstitutions(LamPrint *print,
                                           SymbolSet *symbols) {
    print->exp = lamPerformLazySubstitutions(print->exp, symbols);
    print->printer = lamPerformLazySubstitutions(print->printer, symbols);
    return print;
}

/**
 * @brief Performs substitutions on a lookUp expression.
 * @param lookUp The lookUp expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified lookUp expression.
 */
static LamLookUp *performLookUpSubstitutions(LamLookUp *lookUp,
                                             SymbolSet *symbols) {
    lookUp->exp = lamPerformLazySubstitutions(lookUp->exp, symbols);
    return lookUp;
}

#ifdef NOTDEF
/**
 * @brief Performs substitutions on a make vector expression.
 * @param makeVec The make vector expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified make vector expression.
 */
static LamMakeVec *performMakeVecSubstitutions(LamMakeVec *makeVec,
                                               SymbolSet *symbols) {
    ENTER(performMakeVecSubstitutions);
    makeVec->args = performArgsSubstitutions(makeVec->args, symbols);
    LEAVE(performMakeVecSubstitutions);
    return makeVec;
}
#endif

/**
 * @brief Performs substitutions on a deconstruct expression.
 * @param deconstruct The deconstruct expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified deconstruct expression.
 */
static LamDeconstruct *
performDeconstructSubstitutions(LamDeconstruct *deconstruct,
                                SymbolSet *symbols) {
    ENTER(performDeconstructSubstitutions);
    deconstruct->exp = lamPerformLazySubstitutions(deconstruct->exp, symbols);
    LEAVE(performDeconstructSubstitutions);
    return deconstruct;
}

/**
 * @brief Performs substitutions on a construct expression.
 * @param construct The construct expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified construct expression.
 */
static LamConstruct *performConstructSubstitutions(LamConstruct *construct,
                                                   SymbolSet *symbols) {
    ENTER(performConstructSubstitutions);
    construct->args = performArgsSubstitutions(construct->args, symbols);
    LEAVE(performConstructSubstitutions);
    return construct;
}

/**
 * @brief Performs substitutions on a function application expression.
 * @param apply The function application expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified function application expression.
 */
static LamApply *performApplySubstitutions(LamApply *apply,
                                           SymbolSet *symbols) {
    ENTER(performApplySubstitutions);
    apply->function = lamPerformLazySubstitutions(apply->function, symbols);
    apply->args = performArgsSubstitutions(apply->args, symbols);
    LEAVE(performApplySubstitutions);
    return apply;
}

/**
 * @brief Performs substitutions on an if expression.
 * @param iff The if expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified if expression.
 */
static LamIff *performIffSubstitutions(LamIff *iff, SymbolSet *symbols) {
    ENTER(performIffSubstitutions);
    iff->condition = lamPerformLazySubstitutions(iff->condition, symbols);
    iff->consequent = lamPerformLazySubstitutions(iff->consequent, symbols);
    iff->alternative = lamPerformLazySubstitutions(iff->alternative, symbols);
    LEAVE(performIffSubstitutions);
    return iff;
}

/**
 * @brief Performs substitutions on a list of letrec bindings.
 * @param let The letrec bindings to modify.
 * @param symbols The current set of arguments.
 * @return The modified letrec bindings.
 */
static LamBindings *performBindingsSubstitutions(LamBindings *bindings,
                                                 SymbolSet *symbols) {
    ENTER(performBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performBindingsSubstitutions);
        return NULL;
    }
    bindings->next = performBindingsSubstitutions(bindings->next, symbols);
    bindings->val = lamPerformLazySubstitutions(bindings->val, symbols);
    LEAVE(performBindingsSubstitutions);
    return bindings;
}

/**
 * @brief Performs substitutions on a list of let bindings.
 * @param bindings The let bindings to modify.
 * @param symbols The current set of arguments.
 * @return The modified let bindings.
 */
static LamBindings *performLetBindingsSubstitutions(LamBindings *bindings,
                                                    SymbolSet **symbols) {
    ENTER(performLetBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performLetBindingsSubstitutions);
        return NULL;
    }
    bindings->val = lamPerformLazySubstitutions(bindings->val, *symbols);
    bindings->next = performLetBindingsSubstitutions(bindings->next, symbols);
    // exclude *after* performing the other substitutions
    if (isLazyArgument(bindings->var, *symbols)) {
        *symbols = excludeSymbol(bindings->var, *symbols);
        PROTECT(*symbols); // caller will UNPROTECT
    }
    LEAVE(performLetBindingsSubstitutions);
    return bindings;
}

/**
 * @brief Performs substitutions on a list of let* bindings.
 * @param bindings The let bindings to modify.
 * @param symbols The current set of arguments.
 * @return The modified let bindings.
 */
static LamBindings *performLetStarBindingsSubstitutions(LamBindings *bindings,
                                                        SymbolSet **symbols) {
    ENTER(performLetStarBindingsSubstitutions);
    if (bindings == NULL) {
        LEAVE(performLetStarBindingsSubstitutions);
        return NULL;
    }
    if (isLazyArgument(bindings->var, *symbols)) {
        *symbols = excludeSymbol(bindings->var, *symbols);
        PROTECT(*symbols); // caller will UNPROTECT
    }
    bindings->val = lamPerformLazySubstitutions(bindings->val, *symbols);
    bindings->next =
        performLetStarBindingsSubstitutions(bindings->next, symbols);
    LEAVE(performLetStarBindingsSubstitutions);
    return bindings;
}

/**
 * @brief Performs substitutions on a let expression.
 * @param let The let expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified let expression.
 */
static LamLet *performLetSubstitutions(LamLet *let, SymbolSet *symbols) {
    ENTER(performLetSubstitutions);
    SymbolSet *remaining = copySymbolSet(symbols);
    int save = PROTECT(remaining);
    let->bindings = performLetBindingsSubstitutions(let->bindings, &remaining);
    let->body = lamPerformLazySubstitutions(let->body, remaining);
    UNPROTECT(save);
    LEAVE(performLetSubstitutions);
    return let;
}

/**
 * @brief Performs substitutions on a let* expression.
 * @param let The let expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified let expression.
 */
static LamLetStar *performLetStarSubstitutions(LamLetStar *let,
                                               SymbolSet *symbols) {
    ENTER(performLetStarSubstitutions);
    SymbolSet *remaining = copySymbolSet(symbols);
    int save = PROTECT(remaining);
    let->bindings =
        performLetStarBindingsSubstitutions(let->bindings, &remaining);
    let->body = lamPerformLazySubstitutions(let->body, remaining);
    UNPROTECT(save);
    LEAVE(performLetStarSubstitutions);
    return let;
}

/**
 * @brief Performs substitutions on a letrec expression.
 *
 * Arranges to exclude *all* letrec bindings from the list of arguments
 * before recursing into the letrec bodies.
 *
 * @param letrec The letrec expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified letrec expression.
 */
static LamLetRec *performLetRecSubstitutions(LamLetRec *letrec,
                                             SymbolSet *symbols) {
    ENTER(performLetRecSubstitutions);
    LamVarList *names = collectLetRecNames(letrec->bindings);
    int save = PROTECT(names);
    if (containsLazyArguments(names, symbols)) {
        symbols = excludeSymbols(names, symbols);
        PROTECT(symbols);
    }
    letrec->bindings = performBindingsSubstitutions(letrec->bindings, symbols);
    letrec->body = lamPerformLazySubstitutions(letrec->body, symbols);
    LEAVE(performLetRecSubstitutions);
    UNPROTECT(save);
    return letrec;
}

/**
 * @brief Performs substitutions on the body of a typedef.
 * @param typeDefs The typedef to modify.
 * @param symbols The current set of arguments.
 * @return The modified typedef.
 */
static LamTypeDefs *performTypeDefsSubstitutions(LamTypeDefs *typeDefs,
                                                 SymbolSet *symbols) {
    ENTER(performTypeDefsSubstitutions);
    typeDefs->body = lamPerformLazySubstitutions(typeDefs->body, symbols);
    LEAVE(performTypeDefsSubstitutions);
    return typeDefs;
}

/**
 * @brief Performs substitutions on the bodies of a case expression.
 * @param cases The case expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified case expression.
 */
static LamMatchList *performCaseSubstitutions(LamMatchList *cases,
                                              SymbolSet *symbols) {
    ENTER(performCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCaseSubstitutions);
        return NULL;
    }
    cases->next = performCaseSubstitutions(cases->next, symbols);
    cases->body = lamPerformLazySubstitutions(cases->body, symbols);
    LEAVE(performCaseSubstitutions);
    return cases;
}

/**
 * @brief Performs substitutions on the cases of a match expression.
 * @param match The match expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified match expression.
 */
static LamMatch *performMatchSubstitutions(LamMatch *match,
                                           SymbolSet *symbols) {
    ENTER(performMatchSubstitutions);
    match->index = lamPerformLazySubstitutions(match->index, symbols);
    match->cases = performCaseSubstitutions(match->cases, symbols);
    LEAVE(performMatchSubstitutions);
    return match;
}

/**
 * @brief Performs substitutions on an amb (ambivalent) expression.
 * @param amb The amb expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified amb expression.
 */
static LamAmb *performAmbSubstitutions(LamAmb *amb, SymbolSet *symbols) {
    ENTER(performAmbSubstitutions);
    amb->left = lamPerformLazySubstitutions(amb->left, symbols);
    amb->right = lamPerformLazySubstitutions(amb->right, symbols);
    LEAVE(performAmbSubstitutions);
    return amb;
}

/**
 * @brief Performs substitutions on integer conditional cases.
 * @param cases The integer conditional cases to modify.
 * @param symbols The current set of arguments.
 * @return The modified integer conditional cases.
 */
static LamIntCondCases *performIntCondCaseSubstitutions(LamIntCondCases *cases,
                                                        SymbolSet *symbols) {
    ENTER(performIntCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performIntCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformLazySubstitutions(cases->body, symbols);
    cases->next = performIntCondCaseSubstitutions(cases->next, symbols);
    LEAVE(performIntCondCaseSubstitutions);
    return cases;
}

/**
 * @brief Performs substitutions on character conditional cases.
 * @param cases The character conditional cases to modify.
 * @param symbols The current set of arguments.
 * @return The modified character conditional cases.
 */
static LamCharCondCases *
performCharCondCaseSubstitutions(LamCharCondCases *cases, SymbolSet *symbols) {
    ENTER(performCharCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCharCondCaseSubstitutions);
        return NULL;
    }
    cases->body = lamPerformLazySubstitutions(cases->body, symbols);
    cases->next = performCharCondCaseSubstitutions(cases->next, symbols);
    LEAVE(performCharCondCaseSubstitutions);
    return cases;
}

/**
 * @brief Performs substitutions on conditional cases.
 *
 * Switches between char and int conditional cases appropriately.
 *
 * @param cases The conditional cases to modify.
 * @param symbols The current set of arguments.
 * @return The modified conditional cases.
 */
static LamCondCases *performCondCaseSubstitutions(LamCondCases *cases,
                                                  SymbolSet *symbols) {
    ENTER(performCondCaseSubstitutions);
    if (cases == NULL) {
        LEAVE(performCondCaseSubstitutions);
        return NULL;
    }
    switch (cases->type) {
    case LAMCONDCASES_TYPE_INTEGERS:
        setLamCondCases_Integers(cases,
                                 performIntCondCaseSubstitutions(
                                     getLamCondCases_Integers(cases), symbols));
        break;
    case LAMCONDCASES_TYPE_CHARACTERS:
        setLamCondCases_Characters(
            cases, performCharCondCaseSubstitutions(
                       getLamCondCases_Characters(cases), symbols));
        break;
    default:
        cant_happen("unrecognised type %d in performCondCaseSubstitutions",
                    cases->type);
    }
    LEAVE(performCondCaseSubstitutions);
    return cases;
}

/**
 * @brief Performs substitutions on a cond expression.
 * @param cond The conditional expression to modify.
 * @param symbols The current set of arguments.
 * @return The modified conditional expression.
 */
static LamCond *performCondSubstitutions(LamCond *cond, SymbolSet *symbols) {
    ENTER(performCondSubstitutions);
    cond->value = lamPerformLazySubstitutions(cond->value, symbols);
    cond->cases = performCondCaseSubstitutions(cond->cases, symbols);
    LEAVE(performCondSubstitutions);
    return cond;
}

/**
 * @brief Performs substitutions on a lambda expression.
 *
 * When called externally, the `exp` is the body of the function
 * and the `symbols` table contains the arguments to the function.
 *
 * @param exp The lambda expression to modify.
 * @param symbols The set of arguments.
 * @return The modified lambda expression.
 */
LamExp *lamPerformLazySubstitutions(LamExp *exp, SymbolSet *symbols) {
    ENTER(lamPerformLazySubstitutions);
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
            break;
        case LAMEXP_TYPE_LAM:
            exp = performLamSubstitutions(getLamExp_Lam(exp), symbols);
            break;
        case LAMEXP_TYPE_VAR:
            exp = performVarSubstitution(CPI(exp), exp, symbols);
            break;
        case LAMEXP_TYPE_PRIM:
            setLamExp_Prim(
                exp, performPrimSubstitutions(getLamExp_Prim(exp), symbols));
            break;
        case LAMEXP_TYPE_SEQUENCE:
            setLamExp_Sequence(exp, performSequenceSubstitutions(
                                        getLamExp_Sequence(exp), symbols));
            break;
#ifdef NOTDEF
        case LAMEXP_TYPE_MAKEVEC:
            setLamExp_MakeVec(exp, performMakeVecSubstitutions(
                                       getLamExp_MakeVec(exp), symbols));
            break;
#endif
        case LAMEXP_TYPE_DECONSTRUCT:
            setLamExp_Deconstruct(
                exp, performDeconstructSubstitutions(getLamExp_Deconstruct(exp),
                                                     symbols));
            break;
        case LAMEXP_TYPE_CONSTRUCT:
            setLamExp_Construct(exp, performConstructSubstitutions(
                                         getLamExp_Construct(exp), symbols));
            break;
        case LAMEXP_TYPE_TAG:
            setLamExp_Tag(
                exp, lamPerformLazySubstitutions(getLamExp_Tag(exp), symbols));
            break;
        case LAMEXP_TYPE_APPLY:
            setLamExp_Apply(
                exp, performApplySubstitutions(getLamExp_Apply(exp), symbols));
            break;
        case LAMEXP_TYPE_IFF:
            setLamExp_Iff(exp,
                          performIffSubstitutions(getLamExp_Iff(exp), symbols));
            break;
        case LAMEXP_TYPE_COND:
            setLamExp_Cond(
                exp, performCondSubstitutions(getLamExp_Cond(exp), symbols));
            break;
        case LAMEXP_TYPE_CALLCC:
            setLamExp_CallCC(exp, lamPerformLazySubstitutions(
                                      getLamExp_CallCC(exp), symbols));
            break;
        case LAMEXP_TYPE_LET:
            setLamExp_Let(exp,
                          performLetSubstitutions(getLamExp_Let(exp), symbols));
            break;
        case LAMEXP_TYPE_LETREC:
            setLamExp_LetRec(exp, performLetRecSubstitutions(
                                      getLamExp_LetRec(exp), symbols));
            break;
        case LAMEXP_TYPE_LETSTAR:
            setLamExp_LetStar(exp, performLetStarSubstitutions(
                                       getLamExp_LetStar(exp), symbols));
            break;
        case LAMEXP_TYPE_TYPEDEFS:
            setLamExp_TypeDefs(exp, performTypeDefsSubstitutions(
                                        getLamExp_TypeDefs(exp), symbols));
            break;
        case LAMEXP_TYPE_MATCH:
            setLamExp_Match(
                exp, performMatchSubstitutions(getLamExp_Match(exp), symbols));
            break;
        case LAMEXP_TYPE_AMB:
            setLamExp_Amb(exp,
                          performAmbSubstitutions(getLamExp_Amb(exp), symbols));
            break;
        case LAMEXP_TYPE_MAKETUPLE:
            setLamExp_MakeTuple(exp, performArgsSubstitutions(
                                         getLamExp_MakeTuple(exp), symbols));
            break;
        case LAMEXP_TYPE_TUPLEINDEX:
            setLamExp_TupleIndex(exp, performTupleIndexSubstitutions(
                                          getLamExp_TupleIndex(exp), symbols));
            break;
        case LAMEXP_TYPE_PRINT:
            setLamExp_Print(
                exp, performPrintSubstitutions(getLamExp_Print(exp), symbols));
            break;
        case LAMEXP_TYPE_LOOKUP:
            setLamExp_LookUp(exp, performLookUpSubstitutions(
                                      getLamExp_LookUp(exp), symbols));
            break;
        default:
            cant_happen("unrecognized %s", lamExpTypeName(exp->type));
        }
    }
    LEAVE(lamPerformLazySubstitutions);
    return exp;
}
