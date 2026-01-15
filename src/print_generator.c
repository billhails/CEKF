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
 * print function generator
 * invoked during lambda conversion
 * generates a print function for each typedef
 */

/**
 * @file print_generator.c
 * @brief Print function generator, invoked during lambda conversion.
 *        Generates a print function for each typedef.
 * @details How print functions work:
 *          Print functions are generated for all user defined types, unless the
 *          user has explicityly defined a print function for the type.
 *          If the type has no type variables, then the generated print function
 * simply takes the single value as argument. If the type has type variables
 * however, then at the time of generating the print function it cannot know how
 * to print those argument types and so it takes extra arguments, one per type
 * variable, that are themselves print functions for the corresponding type
 * variables, followed by the value to print. For example given
 *          ```
 *          typedef list(#t) { null | pair(#t, list(#t)) }
 *          ```
 *          the generated print function would look something like
 *          ```
 *          fn print_list {
 *              (print_t, pair(x, y)) {
 *                  puts("pair(");
 *                  print_t(x);
 *                  puts(", ");
 *                  print_list(print_t, y);
 *                  puts(")");
 *              }
 *              (_, null) {
 *                  puts("null");
 *              }
 *          }
 *          ```
 *          This is nice because all generated print functions have a consistent
 *          structure and can easily be composed together.
 *
 *          The print compiler in `src/print_compiler.c` is responsible for
 *          generating the actual applications of these print functions when the
 *          types are known. It runs as part of the type-checking phase of the
 * compiler.
 */

#include "print_generator.h"
#include "cekf.h"
#include "common.h"
#include "lambda.h"
#include "lambda_helper.h"
#include "symbol.h"
#include "symbols.h"
#include <stdio.h>

#ifdef DEBUG_PRINT_GENERATOR
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static LamBindings *makePrintTypeLetrec(ParserInfo I, LamTypeDef *typeDef,
                                        LamContext *env, LamBindings *next);

/**
 * @brief Creates print functions for all type definitions in the list.
 * @param typeDefs The list of type definitions to create print functions for.
 * @param next The current set of letrec bindings.
 * @param env The current lambda context.
 * @param inPreamble Whether the print functions are being created in the
 * preamble.
 * @return The updated set of letrec bindings with the new print functions.
 */
LamBindings *makePrintFunctions(LamTypeDefList *typeDefs, LamBindings *next,
                                LamContext *env) {
    ENTER(makePrintFunctions);
    if (typeDefs == NULL) {
        LEAVE(makePrintFunctions);
        return next;
    }
    next = makePrintFunctions(typeDefs->next, next, env);
    int save = PROTECT(next);

    next = makePrintTypeLetrec(CPI(typeDefs->typeDef), typeDefs->typeDef, env,
                               next);

    UNPROTECT(save);
    LEAVE(makePrintFunctions);
    return next;
}

/**
 * @brief Constructs a name from a prefix and a base name.
 *
 * @param prefix The prefix to prepend to the base name.
 * @param name The base name to modify.
 * @return A new symbol containing the combined name.
 */
HashSymbol *makePrintName(char *prefix, char *name) {
    HashSymbol *res = NULL;
    int size = strlen(name) + strlen(prefix) + 1;
    if (size < 128) {
        static char buf[128];
        sprintf(buf, "%s%s", prefix, name);
        res = newSymbol(buf);
    } else {
        char *buf = NEW_ARRAY(char, size);
        sprintf(buf, "%s%s", prefix, name);
        res = newSymbol(buf);
        FREE_ARRAY(char, buf, size);
    }
    return res;
}

/**
 * @brief Creates a symbol for the print argument.
 * @return The symbol: "thing"
 */
static HashSymbol *printArgSymbol(void) {
    static HashSymbol *res = NULL;
    if (res == NULL)
        res = newSymbol("thing");
    return res;
}

/**
 * @brief Returns the symbol "thing" as a lambda expression.
 * @param I Parser information.
 * @return A new LamExp representing the print argument variable.
 */
static LamExp *thingName(ParserInfo I) {
    HashSymbol *name = printArgSymbol();
    return newLamExp_Var(I, name);
}

/**
 * Generates the last argument to the print function, which is the "thing" to be
 * printed.
 * @param I Parser information.
 * @return A new LamVarList representing the last argument.
 */
static LamVarList *makeLastArg(ParserInfo I) {
    HashSymbol *name = printArgSymbol();
    return newLamVarList(I, name, NULL);
}

/**
 * @brief Creates the formal argument list for a print function.
 *
 * @details Each print function takes one argument for each type variable in the
 * type signature, (another print function) followed by the thing being printed.
 *
 * @param I Parser information.
 * @param args The type signature arguments.
 * @return A new LamVarList representing the arguments.
 */
static LamVarList *makePrintTypeFunctionArgs(ParserInfo I,
                                             LamTypeSigArgs *args) {
    if (args == NULL)
        return makeLastArg(I);
    LamVarList *next = makePrintTypeFunctionArgs(I, args->next);
    int save = PROTECT(next);
    HashSymbol *name = makePrintName("print", args->name->name);
    LamVarList *res = newLamVarList(I, name, next);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Returns a cons of a lambda expression containing the character,
 *        and the growing cons list.
 * @details The list being constructed is not a simple lambda list, it is a
 * sequence of vector constructors representing pairs of a character expression
 * and the tail of the list. Note that because the plain lambda code is not yet
 *          typechecked, the cons structure includes the type constructor symbol
 * "cons".
 * @param I Parser information.
 * @param c The character to add to the cons list.
 * @param tail The tail of the cons list.
 * @return A new LamExp representing the cons list.
 */
static LamExp *makeCharList(ParserInfo I, char c, LamExp *tail) {
    LamExp *character = newLamExp_Character(I, c);
    int save = PROTECT(character);
    LamArgs *args = newLamArgs(I, tail, NULL);
    PROTECT(args);
    args = newLamArgs(I, character, args);
    PROTECT(args);
    LamExp *res = makeLamExp_Construct(I, consSymbol(), 1, args);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Converts a string to a cons chain.
 * @param I Parser information.
 * @param string The string to convert.
 * @return A new LamExp representing the list of arguments.
 */
LamExp *stringToLamArgs(ParserInfo I, char *string) {
    if (*string == 0) {
        return makeLamExp_Construct(I, nilSymbol(), 0, NULL);
    }
    LamExp *next = stringToLamArgs(I, string + 1);
    int save = PROTECT(next);
    LamExp *this = makeCharList(I, *string, next);
    UNPROTECT(save);
    return this;
}

/**
 * @brief takes a cons string and return a puts expression that will print that
 * string.
 * @param I Parser information.
 * @param string The string to print.
 * @return A new LamExp representing the puts expression.
 */
static LamExp *putsExp(ParserInfo I, LamExp *string) {
    LamExp *puts = newLamExp_Var(I, putsSymbol());
    int save = PROTECT(puts);
    LamArgs *args = newLamArgs(I, string, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(I, puts, args);
    PROTECT(apply);
    LamExp *res = newLamExp_Apply(I, apply);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Converts a C string to a cons-list of characters,
 *        then returns a puts expression that wil print it.
 * @param I Parser information.
 * @param string The string to print.
 * @return A new LamExp representing the puts expression.
 */
static LamExp *makePutsString(ParserInfo I, char *str) {
    LamExp *string = stringToLamArgs(I, str);
    int save = PROTECT(string);
    LamExp *res = putsExp(I, string);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Creates a puts expression for the name of a type constructor.
 * @param I Parser information.
 * @param constructor The type constructor to print.
 * @return A new LamExp representing the puts expression.
 */
static LamExp *makePutsConstructorName(ParserInfo I,
                                       LamTypeConstructor *constructor) {
    LamExp *string = stringToLamArgs(I, constructor->name->name);
    int save = PROTECT(string);
    LamExp *puts = putsExp(I, string);
    UNPROTECT(save);
    return puts;
}

/**
 * @brief Creates an accessor expression for a type constructor and an index.
 * @param I Parser information.
 * @param index The index of the argument in the type constructor.
 * @param info The type constructor information.
 * @return A new LamExp representing the accessor expression.
 */
static LamExp *makeIndexedDeconstruct(ParserInfo I, int index,
                                      LamTypeConstructorInfo *info) {
    LamExp *printArg = thingName(I);
    int save = PROTECT(printArg);
    LamDeconstruct *dec =
        newLamDeconstruct(I, info->type->name, info->nsId, index, printArg);
    PROTECT(dec);
    LamExp *res = newLamExp_Deconstruct(I, dec);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Creates a variable from a C string.
 * @param I Parser information.
 * @param name The name of the symbol.
 * @return A new LamExp representing the symbol expression.
 */
LamExp *makeVarExpr(ParserInfo I, char *name) {
    HashSymbol *symbol = newSymbol(name);
    LamExp *exp = newLamExp_Var(I, symbol);
    return exp;
}

/**
 * @brief Returns the name of the integer printer function as a lambda variable.
 * @param I Parser information.
 * @return A new LamExp representing the function name.
 */
LamExp *makePrintInt(ParserInfo I) { return makeVarExpr(I, "print$int"); }

/**
 * @brief Returns the name of the character printer function as a lambda
 * variable.
 * @param I Parser information.
 * @return A new LamExp representing the function name.
 */
LamExp *makePrintChar(ParserInfo I) {
    return makeVarExpr(I, "print$character");
}

/**
 * @brief Creates a formal name for a printer function argument to another print
 * function. i.e. name => printname
 * @param I Parser information.
 * @param var The variable to create the name for.
 * @return A new LamExp representing the printer function argument name.
 */
static LamExp *makeFarg(ParserInfo I, HashSymbol *var) {
    HashSymbol *name = makePrintName("print", var->name);
    LamExp *exp = newLamExp_Var(I, name);
    return exp;
}

static LamExp *makeAarg(ParserInfo I, LamTypeConstructorType *arg);

/**
 * @brief Creates the actual arguments for an internal print function
 * application.
 * @param I Parser information.
 * @param args The type constructor arguments to transform.
 * @return A new LamArgs representing the actual arguments.
 */
static LamArgs *makeAargs(ParserInfo I, LamTypeConstructorArgs *args) {
    if (args == NULL)
        return NULL;
    LamArgs *next = makeAargs(I, args->next);
    int save = PROTECT(next);
    LamExp *printer = makeAarg(I, args->arg);
    PROTECT(printer);
    LamArgs *this = newLamArgs(I, printer, next);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Checks if a function is a list constructor.
 * @param los The lookUp or symbol to check.
 * @return True if the function is a list constructor, false otherwise.
 */
static bool functionIsList(LamLookUpOrSymbol *los) {
    switch (los->type) {
    case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
        return los->val.symbol == listSymbol();
    case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:
        return false;
    default:
        cant_happen("unrecognized %s", lamLookUpOrSymbolTypeName(los->type));
    }
}

/**
 * @brief Gets the underlying function name from a lookUp or symbol.
 * @param los The lookUp or symbol to get the name from.
 * @return The underlying function name.
 */
static char *getUnderlyingFunctionName(LamLookUpOrSymbol *los) {
    switch (los->type) {
    case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
        return los->val.symbol->name;
    case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:
        return los->val.lookUp->symbol->name;
    default:
        cant_happen("unrecognized %s", lamLookUpOrSymbolTypeName(los->type));
    }
}

/**
 * @brief Wraps a print function in a lookUp expression if necessary.
 * @details The argument toPrint is the lookUp or symbol of the thing being
 * printed. The argument printer is the print function. If the toPrint is just a
 * symbol, then the printer is assumed to be in the current scope and returned
 * unchanged. If the toPrint is a lookUp, then the printer is assumed to be in
 * that scope and is wrapped in the same lookUp expression.
 * @param I Parser information.
 * @param printer The print function to wrap.
 * @param los The lookUp or symbol of the thing being printed.
 */
static LamExp *lookUpPrintFunction(ParserInfo I, LamExp *printer,
                                   LamLookUpOrSymbol *toPrint) {
    if (toPrint->type == LAMLOOKUPORSYMBOL_TYPE_LOOKUP) {
        LamLookUpSymbol *ls = toPrint->val.lookUp;
        LamLookUp *llu = newLamLookUp(I, ls->nsId, ls->nsSymbol, printer);
        int save = PROTECT(llu);
        printer = newLamExp_LookUp(I, llu);
        UNPROTECT(save);
    }
    return printer;
}

/**
 * @brief Creates an expression to print a type function.
 * @param I Parser information.
 * @param function The type function to print.
 * @return A new LamExp representing the print expression.
 */
static LamExp *makePrintTypeFunction(ParserInfo I, LamTypeFunction *function) {
    if (functionIsList(function->name)) {
        if (function->args && function->args->arg->type ==
                                  LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER) {
            HashSymbol *name = newSymbol("print$string");
            return newLamExp_Var(I, name);
        }
    }
    HashSymbol *name =
        makePrintName("print$", getUnderlyingFunctionName(function->name));
    LamExp *exp = newLamExp_Var(I, name);
    int save = PROTECT(exp);
    exp = lookUpPrintFunction(I, exp, function->name);
    REPLACE_PROTECT(save, exp);
    LamArgs *args = makeAargs(I, function->args);
    PROTECT(args);
    int nArgs = countLamArgs(args);
    if (nArgs == 0) {
        UNPROTECT(save);
        return exp;
    }
    LamApply *apply = newLamApply(I, exp, args);
    PROTECT(apply);
    LamExp *res = newLamExp_Apply(I, apply);
    UNPROTECT(save);
    return res;
}

/**
 * @brief creates an actual argument to a print function.such that the argument,
 *        when applied to a tuple, will print it.
 */
static LamExp *makePrintTuple(ParserInfo I, LamTypeConstructorArgs *tuple) {
    int size = countLamTypeConstructorArgs(tuple);
    HashSymbol *name = NULL;
    if (size <= 4) {
        char buf[64];
        sprintf(buf, "%d", size);
        name = makePrintName("print$tuple_", buf);
    } else {
        name = newSymbol("print$");
        LamExp *exp = newLamExp_Var(I, name);
        return exp;
    }
    LamExp *exp = newLamExp_Var(I, name);
    int save = PROTECT(exp);
    LamArgs *args = makeAargs(I, tuple);
    PROTECT(args);
    LamApply *apply = newLamApply(I, exp, args);
    PROTECT(apply);
    LamExp *res = newLamExp_Apply(I, apply);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Creates the actual argument for a print function passed to another
 * print function.
 * @details This function generates the appropriate printer expression for a
 * given type of type constructor argument (so without the final "thing"
 * argument):
 *          - integer: `print$int`.
 *          - character: `print$character`.
 *          - type variable `var`: `printvar` where `printvar` is expected to be
 * in scope.
 *          - type function: `print$function(print$arg1, ...)`
 *          - tuple: `print$tuple_<arity>(print$arg1, print$arg2, ...)`
 * @param I Parser information.
 * @param arg The type constructor argument to create the printer for.
 * @return A new LamExp representing the printer expression.
 */
static LamExp *makeAarg(ParserInfo I, LamTypeConstructorType *arg) {
    LamExp *printer = NULL;
    switch (arg->type) {
    case LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER:
        printer = makePrintInt(I);
        break;
    case LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER:
        printer = makePrintChar(I);
        break;
    case LAMTYPECONSTRUCTORTYPE_TYPE_VAR:
        printer = makeFarg(I, arg->val.var);
        break;
    case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
        printer = makePrintTypeFunction(I, arg->val.function);
        break;
    case LAMTYPECONSTRUCTORTYPE_TYPE_TUPLE:
        printer = makePrintTuple(I, arg->val.tuple);
        break;
    default:
        cant_happen("unrecognised type %s in makeAarg",
                    lamTypeConstructorTypeTypeName(arg->type));
    }
    return printer;
}

/**
 * @brief Builds the application of a printer for the given componentType to the
 * componentIndex-th component of the structure described by the constructorInfo
 * @param I Parser information.
 * @param componentType The type of the component to print.
 * @param constructorInfo The constructor information to build the accessor.
 * @param componentIndex The index of the component to print.
 * @return A new LamExp representing the print application.
 */
static LamExp *makeIndexedApplication(ParserInfo I,
                                      LamTypeConstructorType *componentType,
                                      LamTypeConstructorInfo *constructorInfo,
                                      int componentIndex) {
    LamExp *accessor =
        makeIndexedDeconstruct(I, componentIndex, constructorInfo);
    int save = PROTECT(accessor);
    LamExp *printer = makeAarg(I, componentType);
    PROTECT(printer);
    LamArgs *args = newLamArgs(I, accessor, NULL);
    PROTECT(args);
    LamApply *apply = newLamApply(I, printer, args);
    PROTECT(apply);
    LamExp *res = newLamExp_Apply(I, apply);
    UNPROTECT(save);
    return res;
}

/**
 * @brief extracts the n-th tag from a list of tags.
 * @param index The index of the tag to extract.
 * @param tags The list of tags to extract from.
 * @return the tag, or a fatal error if the list is exhausted.
 */
static HashSymbol *findNthTag(int index, LamTypeTags *tags) {
    if (tags == NULL) {
        cant_happen("reached end of tags");
    }
    if (index == 0) {
        return tags->tag;
    }
    return findNthTag(index - 1, tags->next);
}

/**
 * @brief builds the individual parts of the body of a vector match case
 *        which print each component of the type constructor.
 * @details called by `makeVectorMatchBody`.
 * @param I Parser information.
 * @param index The index of the component to print.
 * @param args The constructor arguments.
 * @param info The constructor information.
 * @return A new LamSequence representing the match parts.
 */
static LamSequence *makeVectorMatchParts(ParserInfo I, int index,
                                         LamTypeConstructorArgs *args,
                                         LamTypeConstructorInfo *info,
                                         LamSequence *tail) {
    if (args == NULL)
        return tail;
    LamSequence *next =
        makeVectorMatchParts(I, index + 1, args->next, info, tail);
    int save = PROTECT(next);
    LamExp *exp = makeIndexedApplication(I, args->arg, info, index + 1);
    PROTECT(exp);
    if (next != tail) {
        LamExp *comma = makePutsString(I, ", ");
        PROTECT(comma);
        next = newLamSequence(I, comma, next);
        PROTECT(next);
    }
    LamSequence *res = newLamSequence(I, exp, next);
    PROTECT(res);
    if (info->tags != NULL) {
        HashSymbol *tag = findNthTag(index, info->tags);
        LamExp *colon = makePutsString(I, ": ");
        PROTECT(colon);
        res = newLamSequence(I, colon, res);
        PROTECT(res);
        LamExp *name = makePutsString(I, tag->name);
        PROTECT(name);
        res = newLamSequence(I, name, res);
        PROTECT(next);
    }
    UNPROTECT(save);
    return res;
}

/**
 * @brief Builds the body of a match expression for a type constructor with
 * arity greater than 0.
 * @details This function creates the body of the match expression for a
 * vector-based type, which includes printing the constructor name, opening the
 * structure, printing each component, and closing the structure.
 * @param I Parser information.
 * @param info The type constructor information.
 * @return A new LamExp representing the match body.
 */
static LamExp *makeVectorMatchBody(ParserInfo I, LamTypeConstructorInfo *info) {
    LamTypeConstructor *constructor = info->type;
    LamExp *header = makePutsConstructorName(I, constructor);
    int save = PROTECT(header);
    bool isStruct = info->tags != NULL;
    LamExp *close = makePutsString(I, isStruct ? " }" : ")");
    PROTECT(close);
    LamSequence *seq = newLamSequence(I, close, NULL);
    PROTECT(seq);
    seq = makeVectorMatchParts(I, 0, constructor->args, info, seq);
    PROTECT(seq);
    LamExp *open = makePutsString(I, isStruct ? "{ " : "(");
    PROTECT(open);
    seq = newLamSequence(I, open, seq);
    PROTECT(seq);
    seq = newLamSequence(I, header, seq);
    PROTECT(seq);
    LamExp *res = newLamExp_Sequence(I, seq);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Builds the individual match cases for a type whose constructors create
 * scalars.
 * @details If none of the constructors have arity greater than 0, they can be
 * represented as simple scalars, for example `typedef colours { red, green,
 * blue }`. This function is called to recursively create the match cases for
 * those scalar-based types.
 * @param I Parser information.
 * @param constructors The list of type constructors for the type being matched.
 * @param env The current lambda context.
 * @return The match expression.
 */
static LamMatchList *makeScalarMatchList(ParserInfo I,
                                         LamTypeConstructorList *constructors,
                                         LamContext *env) {
    if (constructors == NULL)
        return NULL;
    LamMatchList *next = makeScalarMatchList(I, constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info =
        lookUpConstructorInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen(
            "cannot find info for type constructor %s in makeScalarMatchList",
            constructors->constructor->name->name);
    }
    LamIntList *matches =
        newLamIntList(I, info->index, info->type->name, info->nsId, NULL);
    PROTECT(matches);
    LamExp *body = makePutsConstructorName(I, constructors->constructor);
    PROTECT(body);
    LamMatchList *res = newLamMatchList(I, matches, body, next);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Builds a match expression for a type whose constructors create only
 * scalars.
 * @details If none of the constructors have arity greater than 0, they can be
 * represented as simple scalars, for example `typedef colours { red, green,
 * blue }`. This function is called to create a match expression for those
 * scalar-based types.
 * @param I Parser information.
 * @param constructors The list of type constructors for the type being matched.
 * @param env The current lambda context.
 * @return The match expression.
 */
static LamMatch *makeScalarMatch(ParserInfo I,
                                 LamTypeConstructorList *constructors,
                                 LamContext *env) {
    LamMatchList *cases = makeScalarMatchList(I, constructors, env);
    int save = PROTECT(cases);
    LamExp *var = thingName(I);
    PROTECT(var);
    LamMatch *res = newLamMatch(I, var, cases);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Builds the individual match cases for a type whose constructors create
 * vectors.
 * @details If any of the constructors have arity greater than 0, they all need
 * to create vectors (so that the match does not need to distinguish between
 * their structures). This function is called to recursively create the match
 * cases for those vector-based types.
 * @param I Parser information.
 * @param constructors The list of type constructors for the type being matched.
 * @param env The current lambda context.
 * @return The match expression.
 */
static LamMatchList *makeVectorMatchList(ParserInfo I,
                                         LamTypeConstructorList *constructors,
                                         LamContext *env) {
    if (constructors == NULL)
        return NULL;
    LamMatchList *next = makeVectorMatchList(I, constructors->next, env);
    int save = PROTECT(next);
    LamTypeConstructorInfo *info =
        lookUpConstructorInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen(
            "cannot find info for type constructor %s in makeVectorMatchList",
            constructors->constructor->name->name);
    }
    LamIntList *matches =
        newLamIntList(I, info->index, info->type->name, info->nsId, NULL);
    PROTECT(matches);
    LamExp *body = NULL;
    if (info->arity > 0) {
        body = makeVectorMatchBody(I, info);
    } else {
        body = makePutsConstructorName(I, constructors->constructor);
    }
    PROTECT(body);
    LamMatchList *res = newLamMatchList(I, matches, body, next);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Builds a match expression for a type whose constructors create
 * vectors.
 * @details If any of the constructors have arity greater than 0, they all need
 * to create vectors (so that the match does not need to distinguish between
 * their structures). This function is called to create a match expression for
 * those vector-based types.
 * @param I Parser information.
 * @param constructors The list of type constructors for the type being matched.
 * @param env The current lambda context.
 * @return The match expression.
 */
static LamMatch *makeVectorMatch(ParserInfo I,
                                 LamTypeConstructorList *constructors,
                                 LamContext *env) {
    LamMatchList *cases = makeVectorMatchList(I, constructors, env);
    int save = PROTECT(cases);
    LamExp *var = thingName(I);
    PROTECT(var);
    LamExp *prim = newLamExp_Tag(I, var);
    PROTECT(prim);
    LamMatch *res = newLamMatch(I, prim, cases);
    UNPROTECT(save);
    return res;
}

/**
 * @brief create the function body for a toplevel print function.
 * @param I Parser information.
 * @param constructors The list of type constructors for the type being printed.
 * @param env The current lambda context.
 * @return The function body expression.
 */
static LamExp *makeFunctionBody(ParserInfo I,
                                LamTypeConstructorList *constructors,
                                LamContext *env) {
    LamTypeConstructorInfo *info =
        lookUpConstructorInLamContext(env, constructors->constructor->name);
    if (info == NULL) {
        cant_happen(
            "cannot find info for type constructor %s in makeFunctionBody",
            constructors->constructor->name->name);
    }
    LamMatch *match = NULL;
    if (info->needsVec) {
        match = makeVectorMatch(I, constructors, env);
    } else {
        match = makeScalarMatch(I, constructors, env);
    }
    int save = PROTECT(match);
    LamExp *res = newLamExp_Match(I, match);
    PROTECT(res);
    // print functions should all return their argument
    LamExp *ret = thingName(I);
    PROTECT(ret);
    LamSequence *seq = newLamSequence(I, ret, NULL);
    PROTECT(seq);
    seq = newLamSequence(I, res, seq);
    PROTECT(seq);
    res = newLamExp_Sequence(I, seq);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Checks to see if there is already a print function defined.
 * @param printName The name of the print function to look for.
 * @param bindings The list of bindings to search.
 * @return True if the print function is already defined, false otherwise.
 */
static bool userDefined(HashSymbol *printName, LamBindings *bindings) {
    if (bindings == NULL)
        return false;
    if (bindings->var == printName)
        return true;
    return userDefined(printName, bindings->next);
}

/**
 * @brief Creates a letrec binding for a print function.
 * @param I The parser information.
 * @param typeDef The type definition for the type being printed.
 * @param env The current lambda context.
 * @param next The next letrec binding in the chain.
 * @return The new letrec binding for the print function.
 */
static LamBindings *makePrintTypeLetrec(ParserInfo I, LamTypeDef *typeDef,
                                        LamContext *env, LamBindings *next) {
    HashSymbol *name = makePrintName("print$", typeDef->type->name->name);
    if (userDefined(name, next)) {
        return next;
    }
    LamVarList *args = makePrintTypeFunctionArgs(I, typeDef->type->args);
    int save = PROTECT(args);
    LamExp *body = makeFunctionBody(I, typeDef->constructors, env);
    PROTECT(body);
    LamExp *val = makeLamExp_Lam(I, args, body);
    PROTECT(val);
    LamBindings *res = newLamBindings(I, name, val, next);
    UNPROTECT(save);
    return res;
}