/* * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

/**
 * @file pratt_parser.c
 * @brief The Pratt parser.
 */
#include <ctype.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>

#include "ast.h"
#include "bigint.h"
#include "file_id.h"
#include "memory.h"
#include "pratt.h"
#include "pratt_debug.h"
#include "pratt_functions.h"
#include "pratt_parser.h"
#include "pratt_scanner.h"
#include "preamble.h"
#include "print_generator.h"
#include "symbols.h"
#include "unicode.h"
#include "wrapper_synthesis.h"

#ifdef DEBUG_PRATT_PARSER
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

// minimal multiplier for converting declared precedence levels to
// internal values, to guarantee that adding or subtracting 1 from
// an internal precedence will not overlap with an adjacent internal
// precedence level: (1 * 3 + 1) < (2 * 3 - 1).
#define PRECEDENCE_SCALE 3

// Unicode
// re-entrant
// stacked input stream
// modified during parse
// integrates with existing AST

// only one precedence
// operators can't be both infix and postfix
// left associative infix operators parse the rhs with prec + 1
// right associative infix operators parse the rhs with prec - 1
// pareslets for grouping know their own matching close brace

AstStringArray *include_paths = NULL;

static AstAltArgs *alt_args(PrattParser *);
static AstAltFunction *alt_function(PrattParser *);
static AstFarg *astCharacterToFarg(ParserInfo, Character);
static AstFarg *astExpressionToFarg(PrattParser *parser, AstExpression *expr);
static AstFarg *astFunCallToFarg(PrattParser *parser, AstFunCall *funCall);
static AstFarg *astLookUpToFarg(PrattParser *parser, AstLookUp *lookUp);
static AstFarg *astNumberToFarg(ParserInfo, MaybeBigInt *);
static AstFarg *astStructureToFarg(PrattParser *parser, AstStruct *structure);
static AstFarg *astSymbolToFarg(ParserInfo, HashSymbol *);
static AstFarg *astTupleToFarg(PrattParser *parser, AstExpressions *tuple);
static AstFargList *astExpressionsToFargList(PrattParser *parser,
                                             AstExpressions *exprs);
static AstFargList *fargs(PrattParser *);
static AstCompositeFunction *composite_function(PrattParser *);
static AstCompositeFunction *functions(PrattParser *);
static AstDefinition *alias(PrattParser *);
static AstDefinition *assignment(PrattParser *);
static AstDefinition *definition(PrattParser *);
static AstDefinition *defmacro(PrattParser *);
static AstDefinition *defun(PrattParser *, bool, bool);
static AstDefinition *importop(PrattParser *);
static AstDefinition *exportop(PrattParser *);
static AstDefinition *link(PrattParser *);
static AstDefinition *typedefinition(PrattParser *);
static AstDefinition *multidefinition(PrattParser *);
static AstDefinitions *definitions(PrattParser *, HashSymbol *);
static AstDefinitions *prattParseLink(PrattParser *, char *, PrattParser **);
static AstExpression *back(PrattRecord *, PrattParser *, AstExpression *,
                           PrattToken *);
static AstExpression *call(PrattRecord *, PrattParser *, AstExpression *,
                           PrattToken *);
static AstExpression *makeStruct(PrattRecord *, PrattParser *, AstExpression *,
                                 PrattToken *);
static AstExpression *doPrefix(PrattRecord *, PrattParser *, AstExpression *,
                               PrattToken *);
static AstExpression *errorExpression(ParserInfo);
static AstExpression *error(PrattRecord *, PrattParser *, AstExpression *,
                            PrattToken *);
static AstExpression *exprAlias(PrattRecord *, PrattParser *, AstExpression *,
                                PrattToken *);
static AstExpression *expression(PrattParser *);
static AstExpression *expressionPrecedence(PrattParser *, int);
static AstExpression *fn(PrattRecord *, PrattParser *, AstExpression *,
                         PrattToken *);
static AstExpression *grouping(PrattRecord *, PrattParser *, AstExpression *,
                               PrattToken *);
static AstExpression *iff(PrattRecord *, PrattParser *, AstExpression *,
                          PrattToken *);
static AstExpression *infixLeft(PrattRecord *, PrattParser *, AstExpression *,
                                PrattToken *) __attribute__((unused));
static AstExpression *infixRight(PrattRecord *, PrattParser *, AstExpression *,
                                 PrattToken *);
static AstExpression *list(PrattRecord *, PrattParser *, AstExpression *,
                           PrattToken *);
static AstExpression *lookUp(PrattRecord *, PrattParser *, AstExpression *,
                             PrattToken *);
static AstExpression *macro(PrattRecord *, PrattParser *, AstExpression *,
                            PrattToken *);
static AstExpression *makeAtom(PrattRecord *, PrattParser *, AstExpression *,
                               PrattToken *);
static AstExpression *makeChar(PrattRecord *, PrattParser *, AstExpression *,
                               PrattToken *);
static AstExpression *makeNumber(PrattRecord *, PrattParser *, AstExpression *,
                                 PrattToken *);
static AstExpression *makeString(PrattRecord *, PrattParser *, AstExpression *,
                                 PrattToken *);
static AstExpression *nestexpr(PrattRecord *, PrattParser *, AstExpression *,
                               PrattToken *);
static AstExpression *passert(PrattRecord *, PrattParser *, AstExpression *,
                              PrattToken *);
static AstExpression *print(PrattRecord *, PrattParser *, AstExpression *,
                            PrattToken *);
static AstExpression *typeOfExp(PrattRecord *, PrattParser *, AstExpression *,
                                PrattToken *);
static AstExpression *switchExp(PrattRecord *, PrattParser *, AstExpression *,
                                PrattToken *);
static AstExpression *tuple(PrattRecord *, PrattParser *, AstExpression *,
                            PrattToken *);
static AstExpression *unsafe(PrattRecord *, PrattParser *, AstExpression *,
                             PrattToken *);
static AstExpression *userInfixLeft(PrattRecord *, PrattParser *,
                                    AstExpression *, PrattToken *);
static AstExpression *userInfixRight(PrattRecord *, PrattParser *,
                                     AstExpression *, PrattToken *);
static AstExpression *userInfixNone(PrattRecord *, PrattParser *,
                                    AstExpression *, PrattToken *);
static AstExpression *userPostfix(PrattRecord *, PrattParser *, AstExpression *,
                                  PrattToken *);
static AstExpression *userPrefix(PrattRecord *, PrattParser *, AstExpression *,
                                 PrattToken *);
static AstExpression *wildCard(PrattRecord *, PrattParser *, AstExpression *,
                               PrattToken *);
static AstExpressions *expressions(PrattParser *);
static AstExpressions *statements(PrattParser *, HashSymbol *);
static AstFileIdArray *fileIdStack = NULL;
static AstFunCall *switchFC(PrattParser *parser);
static AstLookUpOrSymbol *scoped_symbol(PrattParser *);
static AstNameSpace *parseLink(PrattParser *, unsigned char *, HashSymbol *);
static AstNest *nest_body(PrattParser *, HashSymbol *);
static AstNest *nest(PrattParser *);
static AstNest *top(PrattParser *parser);
static AstTypeBody *type_body(PrattParser *);
static AstTypeClause *type_clause(PrattParser *);
static AstTypeConstructor *type_constructor(PrattParser *);
static AstTypeFunction *type_function(PrattParser *);
static AstTypeList *type_list(PrattParser *);
static AstTypeList *type_tuple(PrattParser *);
static AstTypeMap *type_map(PrattParser *);
static AstTypeSymbols *type_variables(PrattParser *);
static AstType *type_type(PrattParser *);
static HashSymbol *symbol(PrattParser *);
static HashSymbol *type_variable(PrattParser *);
static PrattRecord *fetchRecord(PrattParser *, HashSymbol *);
static PrattTrie *makePrattTrie(PrattParser *, PrattTrie *);
static PrattUnicode *rawString(PrattParser *);
static PrattUnicode *str(PrattParser *);
static void storeNameSpace(PrattParser *, AstNameSpace *);
static void synchronize(PrattParser *parser);
static PrattExportedOps *captureNameSpaceOperatorExports(PrattParser *parser);
static PrattRecord *ensureTargetRecord(PrattParser *parser, HashSymbol *op);
static void mergeFixityImport(PrattParser *parser, PrattRecord *target,
                              PrattRecord *source, int nsRef,
                              HashSymbol *nsSymbol, bool importPrefix,
                              bool importInfix, bool importPostfix,
                              HashSymbol *op);
// if you're wondering where the arithmetic primitives are, they're
// defined in the preamble.

#ifdef DEBUG_PRATT_PARSER
void disablePrattDebug(void) { DEBUGGING_OFF(); }
#endif

static PrattParsers *parserStack = NULL;
static PrattNsOpsArray *nsOpsCache = NULL;

static HashSymbol *unicodeToSymbol(PrattUnicode *unicode) {
    size_t len = wcstombs(NULL, unicode->entries, 0);
    PrattCVec *mbStr = newPrattCVec(len + 1);
    int save = PROTECT(mbStr);
    wcstombs(mbStr->entries, unicode->entries, len + 1);
    HashSymbol *res = newSymbol(mbStr->entries);
    UNPROTECT(save);
    return res;
}

static inline PrattFixity getFixityFromPattern(PrattMixfixPattern *pattern) {
    if (pattern->startsWithHole) {
        if (pattern->arity == 1) {
            return PRATTFIXITY_TYPE_POSTFIX;
        } else {
            return PRATTFIXITY_TYPE_INFIX;
        }
    } else {
        return PRATTFIXITY_TYPE_PREFIX;
    }
}

/**
 * @brief Create a new AstExpression representing an error.
 *
 * The new expression includes ParserInfo containing fileName or string context,
 * and line number.
 */
static AstExpression *errorExpression(ParserInfo I) {
    return newAstExpression_Symbol(I, TOK_ERROR());
}

/**
 * @brief Add a record to the PrattRecordTable.
 *
 * Used during the construction and later modification of the PrattParser's
 * record table.
 *
 * @param table The PrattRecordTable to add the record to.
 * @param tok The HashSymbol representing the token.
 * @param prefix The prefix operator function.
 * @param prefixPrec The precedence for the prefix operator.
 * @param infix The infix operator function.
 * @param infixPrec The precedence for the infix operator.
 * @param postfix The postfix operator function.
 * @param postfixPrec The precedence for the postfix operator.
 */
static void addRecord(PrattRecordTable *table, HashSymbol *tok,
                      PrattParselet prefix, int prefixPrec, PrattParselet infix,
                      int infixPrec, PrattParselet postfix, int postfixPrec) {
    PrattRecord *record = newPrattRecord(
        tok,
        (PrattFixityConfig){prefix, prefixPrec * PRECEDENCE_SCALE, NULL, NULL,
                            false, false, NULL, -1, NULL},
        (PrattFixityConfig){infix, infixPrec * PRECEDENCE_SCALE, NULL, NULL,
                            false, false, NULL, -1, NULL},
        (PrattFixityConfig){postfix, postfixPrec * PRECEDENCE_SCALE, NULL, NULL,
                            false, false, NULL, -1, NULL});
    int save = PROTECT(record);
    setPrattRecordTable(table, record->symbol, record);
    UNPROTECT(save);
}

/**
 * @brief Creates the initial PrattParser with the default set of parser rules.
 */
static PrattParser *makePrattParser(void) {
    PrattParser *parser = newPrattParser(NULL);
    int save = PROTECT(parser);
    PrattRecordTable *table = parser->rules;
    addRecord(table, TOK_ALIAS(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_ARROW(), NULL, 0, infixRight, 1, NULL, 0);
    addRecord(table, TOK_AS(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_ASSERT(), passert, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_ASSIGN(), NULL, 0, exprAlias, 7, NULL, 0);
    addRecord(table, TOK_ATOM(), makeAtom, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_BACK(), back, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_BUILTINS(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_CHAR(), makeChar, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_CLOSE(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_COLON(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_COMMA(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_ELSE(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_EOF(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_ERROR(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_EXPORT(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_FN(), fn, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_HASH(), doPrefix, 13, NULL, 0, NULL, 0);
    addRecord(table, TOK_IF(), iff, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_IMPORT(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_IN(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_KW_CHAR(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_KW_ERROR(), error, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_KW_NUMBER(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_LCURLY(), nestexpr, 0, makeStruct, 0, NULL, 0);
    addRecord(table, TOK_LET(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_LINK(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_LSQUARE(), list, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_MACRO(), macro, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_NAMESPACE(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_NUMBER(), makeNumber, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_OPEN(), grouping, 0, call, 14, NULL, 0);
    addRecord(table, TOK_OPERATOR(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_PERIOD(), NULL, 0, lookUp, 15, NULL, 0);
    addRecord(table, TOK_PIPE(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_PRINT(), print, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_RCURLY(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_RSQUARE(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_SEMI(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_STRING(), makeString, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_SWITCH(), switchExp, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_TUPLE(), tuple, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_TYPEDEF(), NULL, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_TYPEOF(), typeOfExp, 11, NULL, 0, NULL, 0);
    addRecord(table, TOK_UNSAFE(), unsafe, 0, NULL, 0, NULL, 0);
    addRecord(table, TOK_WILDCARD(), wildCard, 0, NULL, 0, NULL, 0);

    parser->trie = makePrattTrie(parser, NULL);
    UNPROTECT(save);
    return parser;
}

/**
 * @brief Create a new AstCompositeFunction from a list of AstAltFunction.
 * Handles the case where the same alt function body has alternative argument
 * lists by copying the nest and creating a new AstFunction for each argument
 * list.
 */
static AstCompositeFunction *
makeAstCompositeFunction(AstAltFunction *functions,
                         AstCompositeFunction *rest) {
    int save = PROTECT(NULL);
    for (AstAltArgs *args = functions->altArgs; args != NULL;
         args = args->next) {
        AstNest *copy = copyAstNest(functions->nest);
        PROTECT(copy);
        AstFunction *this = newAstFunction(CPI(args), args->argList, copy);
        PROTECT(this);
        rest = newAstCompositeFunction(CPI(args), this, rest);
        PROTECT(rest);
    }
    UNPROTECT(save);
    return rest;
}

/**
 * @brief Try to create an AgnosticFileId from a prefix and a file name.
 *
 * This function constructs a file path by concatenating the prefix and file
 * name, and then attempts to create an AgnosticFileId from that path. If the
 * AgnosticFileId creation fails, it frees the allocated buffer.
 *
 * @param prefix The prefix path to prepend to the file name.
 * @param file The file name to append to the prefix.
 * @return A pointer to the AgnosticFileId if successful, or NULL if the file
 * does not exist.
 */
static AgnosticFileId *tryFile(char *prefix, char *file) {
    char *buf = malloc(sizeof(char) * (strlen(prefix) + 1 + strlen(file) + 10));
    if (buf == NULL) {
        perror("out of memory");
        exit(1);
    }
    sprintf(buf, "%s/%s", prefix, file);
    AgnosticFileId *result = makeAgnosticFileId(buf);
    if (result == NULL)
        free(buf);
    return result;
}

/**
 * @brief Search for a file in the current directory and include paths.
 *
 * This function attempts to find a file by first checking the current directory
 * and then searching through the include paths if the file is not found.
 *
 * @param initialPrefix The initial prefix to use for the search.
 * @param fileToFind The name of the file to search for.
 * @return A pointer to the AgnosticFileId if found, or NULL if not found.
 */
static AgnosticFileId *searchForFile(char *initialPrefix, char *fileToFind) {
    AgnosticFileId *result = NULL;
    result = tryFile(initialPrefix, fileToFind);
    if (result != NULL)
        return result;
    if (include_paths != NULL) {
        for (Index i = 0; i < include_paths->size; i++) {
            result = tryFile(include_paths->entries[i], fileToFind);
            if (result != NULL)
                return result;
        }
    }
    return NULL;
}

/**
 * @brief Get the current file name from the parser's lexer.
 *
 * @param parser The PrattParser.
 * @return The name of the current file, or "no_file" if not available.
 */
static char *currentPrattFile(PrattParser *parser) {
    char *no_file = "no_file";
    if (parser == NULL)
        return no_file;
    if (parser->lexer == NULL)
        return no_file;
    if (parser->lexer->bufList == NULL)
        return no_file;
    return parser->lexer->bufList->fileName->name;
}

/**
 * @brief Calculate the path for a file based on the current parser's context.
 */
static AgnosticFileId *calculatePath(unsigned char *file, PrattParser *parser) {
    if (*file == '/') {
        // Take ownership of the fileName by duplicating it so the
        // AgnosticFileId can free it during GC finalization.
        return makeAgnosticFileId(safeStrdup((char *)file));
    }
    char *currentFile = currentPrattFile(parser);
    if (currentFile == NULL) {
        return searchForFile(".", (char *)file);
    }
    currentFile = strdup(currentFile);
    char *slash = strrchr(currentFile, '/');
    if (slash == NULL) {
        free(currentFile);
        return searchForFile(".", (char *)file);
    }
    *slash = '\0';
    AgnosticFileId *result = searchForFile(currentFile, (char *)file);
    free(currentFile);
    return result;
}

/**
 * @brief Check if a file ID is already in the file ID stack.
 *
 * This function checks if the given AgnosticFileId is already present in the
 * AstFileIdArray.
 *
 * @param id The AgnosticFileId to check.
 * @param array The AstFileIdArray to search in.
 * @return true if the file ID is found, false otherwise.
 */
static bool fileIdInArray(AgnosticFileId *id, AstFileIdArray *array) {
    for (Index i = 0; i < array->size; ++i) {
        if (cmpAgnosticFileId(id, array->entries[i]) == CMP_EQ)
            return true;
    }
    return false;
}

/**
 * @brief If the parser is in panic mode then advance it to a more likely stable
 * state where parsing can resume.
 */
static void synchronize(PrattParser *parser) {
    if (!parser->panicMode)
        return;
    parser->panicMode = false;
    for (;;) {
        if (check(parser, TOK_EOF()))
            return;
        if (check(parser, TOK_SWITCH()))
            return;
        if (check(parser, TOK_FN()))
            return;
        if (check(parser, TOK_MACRO()))
            return;
        if (check(parser, TOK_IF()))
            return;
        if (check(parser, TOK_PRINT()))
            return;
        if (check(parser, TOK_IN()))
            return;
        if (check(parser, TOK_LINK()))
            return;
        if (check(parser, TOK_SEMI())) {
            next(parser);
            return;
        }
        next(parser);
    }
}

// only a few scenarios we actually need to support:
// 1. parse a standalone string, for tests ONLY
// 2. parse a file after parsing the prelude, the normal case
// 3. parse a string after parsing the prelude, allows a -e style command line
// option
// 4. parse a linked file (within the parser)

/**
 * @brief Parse a standalone string into an AstNest.
 *
 * Only used for testing.
 */
AstNest *prattParseStandaloneString(char *data, char *name) {
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromMbString(data, name);
    AstNest *nest = top(parser);
    UNPROTECT(save);
    return nest;
}

/**
 * @brief Parse a given PrattLexer into an AstProg.
 *
 * This function initializes a PrattParser, sets up the lexer, and parses the
 * input. It returns an AstProg containing the parsed definitions and
 * expressions.
 *
 * create a parser and a lexer
 * parse the preamble with them
 * create a child parser
 * give it the lexer for the main data
 * parse the main data
 * collect nameSpaces etc.
 *
 * @param thing The PrattLexer to parse.
 * @return An AstProg containing the parsed data.
 */
static AstProg *prattParseThing(PrattLexer *thing) {
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromMbString((char *)preamble, "preamble");
    parser->isPreamble = true;
    AstNest *nest = top(parser);
    if (parser->lexer->bufList != NULL) {
        parserError(parser, "unconsumed tokens");
    }
    AstDefinitions *definitions = NULL;
    if (nest) {
        definitions = nest->definitions;
        PROTECT(definitions);
    }
    PrattParser *child = newPrattParser(parser);
    PROTECT(child);
    child->lexer = thing;
    nest = top(child);
    if (child->lexer->bufList != NULL) {
        parserError(child, "unconsumed tokens");
    }
    PROTECT(nest);
    AstExpression *expr = newAstExpression_Nest(CPI(nest), nest);
    PROTECT(expr);
    AstExpressions *exprs = newAstExpressions(CPI(expr), expr, NULL);
    PROTECT(exprs);
    nest = newAstNest(CPI(expr), definitions, exprs);
    AstProg *prog = astNestToProg(nest); // has direct access to nameSpaces
    UNPROTECT(save);
    return prog;
}

/**
 * @brief Parse a file into an AstProg.
 *
 * This function creates a PrattLexer from the specified file, initializes a
 * PrattParser, and parses the contents of the file into an AstProg. This
 * includes handling the preamble.
 *
 * @param file The name of the file to parse.
 * @return An AstProg containing the parsed data.
 */
AstProg *prattParseFile(char *file) {
    PrattLexer *lexer = makePrattLexerFromFileName(file);
    int save = PROTECT(lexer);
    AstProg *prog = prattParseThing(lexer);
    UNPROTECT(save);
    return prog;
}

/**
 * @brief Parse a string into an AstProg.
 *
 * This function creates a PrattLexer from the provided string data,
 * initializes a PrattParser, and parses the string into an AstProg.
 * This includes handling the preamble.
 *
 * @param data The string data to parse.
 * @param name The name of the source for the string (for error reporting).
 * @return An AstProg containing the parsed data.
 */
AstProg *prattParseString(char *data, char *name) {
    PrattLexer *lexer = makePrattLexerFromMbString(data, name);
    int save = PROTECT(lexer);
    AstProg *prog = prattParseThing(lexer);
    UNPROTECT(save);
    return prog;
}

/**
 * @brief Find the preamble parser in the linked list of PrattParsers.
 */
static PrattParser *findPreambleParser(PrattParser *parser) {
#ifdef SAFETY_CHECKS
    if (parser == NULL) {
        cant_happen("cannot find preamble parser");
    }
#endif
    if (parser->isPreamble) {
        return parser;
    }
    return findPreambleParser(parser->next);
}

/**
 * @brief Parse a linked file and return its definitions.
 *
 * This function finds the preamble parser, creates a new parser for the linked
 * file, with the preamble parser as a child (for operators defined in the
 * preamble) and parses the file to extract its definitions. It also updates the
 * result parser.
 *
 * @param parser The current PrattParser.
 * @param file The name of the file to link.
 * @param resultParser Pointer to store the resulting PrattParser.
 * @return An AstDefinitions containing the parsed definitions from the linked
 * file.
 */
static AstDefinitions *prattParseLink(PrattParser *parser, char *file,
                                      PrattParser **resultParser) {
    parser = findPreambleParser(parser);
    parser = newPrattParser(parser);
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromFileName(file);
    AstDefinitions *definitions = NULL;
    AstNest *nest = top(parser);
    if (nest) {
        definitions = nest->definitions;
    }
    *resultParser = parser;
    UNPROTECT(save);
    return definitions;
}

/**
 * @brief Find a nameSpace in the parser's nameSpace table or in the next
 * parser.
 *
 * This function checks if a nameSpace with the given symbol exists in the
 * current parser's nameSpace table. If not found, it recursively checks the
 * next parser in the linked list.
 *
 * @param parser The PrattParser to search in.
 * @param symbol The HashSymbol representing the nameSpace to find.
 * @param result Pointer to store the result if found (can be NULL).
 * @return true if the nameSpace is found, false otherwise.
 */
static bool findNameSpace(PrattParser *parser, HashSymbol *symbol,
                          int *result) {
    if (parser == NULL)
        return false;
    if (getPrattNsIdTable(parser->nameSpaces, symbol, result))
        return true;
    return findNameSpace(parser->next, symbol, result);
}

/**
 * @brief Store a nameSpace in the parser's nameSpace table.
 *
 * This function checks if a nameSpace with the given symbol already exists.
 * If it does, it raises a parser error for redefinition. Otherwise, it stores
 * the nameSpace in the parser's nameSpace table.
 *
 * @param parser The PrattParser to store the nameSpace in.
 * @param ns The AstNameSpace to store.
 */
static void storeNameSpace(PrattParser *parser, AstNameSpace *ns) {
    if (findNameSpace(parser, ns->symbol, NULL)) {
        parserError(parser, "redefinition of nameSpace %s", ns->symbol->name);
    } else {
        setPrattNsIdTable(parser->nameSpaces, ns->symbol, ns->reference);
    }
}

/**
 * @brief Initialize the file ID stack for the parser.
 *
 * This function checks if the file ID stack is already initialized.
 * If not, it creates a new AstFileIdArray to hold file IDs.
 *
 * @return The Index of the newly created and protected fileIdStack
 */
int initFileIdStack() {
    if (fileIdStack == NULL) {
        fileIdStack = newAstFileIdArray();
    }
    return PROTECT(fileIdStack);
}

/**
 * @brief Initialize the parser stack for PrattParsers.
 *
 * @return The Index of the newly created and protected parserStack
 */
int initParserStack() {
    if (parserStack == NULL) {
        parserStack = newPrattParsers();
    }
    return PROTECT(parserStack);
}

int initNsOpsCache() {
    if (nsOpsCache == NULL) {
        nsOpsCache = newPrattNsOpsArray();
    }
    return PROTECT(nsOpsCache);
}

static PrattExportedOps *captureNameSpaceOperatorExports(PrattParser *parser) {
    if (parser == NULL)
        return NULL;
    PrattExportedOps *ops = newPrattExportedOps();
    int save = PROTECT(ops);
    // Iterate local rules; copy only fixities marked for export
    Index i = 0;
    HashSymbol *sym = NULL;
    PrattRecord *rec = NULL;
    while ((sym = iteratePrattRecordTable(parser->rules, &i, &rec)) != NULL) {
        if (!(rec->prefix.export || rec->infix.export || rec->postfix.export))
            continue;
        static PrattFixityConfig emptyConfig = {NULL,  0,    NULL, NULL, false,
                                                false, NULL, -1,   NULL};
        PrattFixityConfig prefixCfg =
            rec->prefix.export ? rec->prefix : emptyConfig;
        PrattFixityConfig infixCfg =
            rec->infix.export ? rec->infix : emptyConfig;
        PrattFixityConfig postfixCfg =
            rec->postfix.export ? rec->postfix : emptyConfig;
        PrattRecord *copy =
            newPrattRecord(rec->symbol, prefixCfg, infixCfg, postfixCfg);
        PROTECT(copy);
        setPrattRecordTable(ops->exportedRules, sym, copy);
    }
    UNPROTECT(save);
    return ops;
}

static PrattRecord *ensureTargetRecord(PrattParser *parser, HashSymbol *op) {
    PrattRecord *target = NULL;
    if (!getPrattRecordTable(parser->rules, op, &target) || target == NULL) {
        // Create a blank record so we can import individual fixities
        PrattFixityConfig empty = {NULL,  0,    NULL, NULL, false,
                                   false, NULL, -1,   NULL};
        target = newPrattRecord(op, empty, empty, empty);
        int save = PROTECT(target);
        setPrattRecordTable(parser->rules, op, target);
        parser->trie = insertPrattTrie(parser->trie, op);
        UNPROTECT(save);
    }
    return target;
}

static inline void mergeFixity(PrattParser *parser, PrattFixityConfig *target,
                               PrattFixityConfig *source, int nsRef,
                               HashSymbol *nsSymbol) {
    if (source->originalImpl != NULL) {
        if (target->originalImpl != NULL) {
            parserError(parser, "import redefines prefix operator");
        } else {
            target->originalImpl = source->originalImpl;
            target->hygienicFunc = source->hygienicFunc;
            target->isBareSymbol = source->isBareSymbol;
            target->prec = source->prec;
            target->op = source->op;
            target->importNsRef = nsRef;
            target->importNsSymbol = nsSymbol;
            // Add secondary keywords to importing parser's trie
            if (source->pattern != NULL) {
                for (Index i = 1; i < source->pattern->keywords->size; ++i) {
                    HashSymbol *inner =
                        unicodeToSymbol(source->pattern->keywords->entries[i]);
                    parser->trie = insertPrattTrie(parser->trie, inner);
                }
                target->pattern = source->pattern;
            }
        }
    }
}

static void mergeFixityImport(PrattParser *parser, PrattRecord *target,
                              PrattRecord *source, int nsRef,
                              HashSymbol *nsSymbol, bool importPrefix,
                              bool importInfix, bool importPostfix,
                              HashSymbol *op) {
    if (importPrefix) {
        mergeFixity(parser, &target->prefix, &source->prefix, nsRef, nsSymbol);
    }
    if (importInfix) {
        if (target->postfix.originalImpl) {
            parserError(parser,
                        "import defines infix operator %s over existing "
                        "postfix operator",
                        op->name);
        } else {
            mergeFixity(parser, &target->infix, &source->infix, nsRef,
                        nsSymbol);
        }
    }
    if (importPostfix) {
        if (target->infix.originalImpl) {
            parserError(parser,
                        "import defines postfix operator %s over existing "
                        "infix operator",
                        op->name);
        } else {
            mergeFixity(parser, &target->postfix, &source->postfix, nsRef,
                        nsSymbol);
        }
    }
}

/**
 * @brief Parse a link to a file and return its nameSpace.
 *
 * This function checks if the file exists, looks for an existing nameSpace,
 * and handles recursive includes. If the file is not found it returns with an
 * error. If the file is already parsed it returns the already existing
 * nameSpace.
 *
 * Careful. Somewhat accidentally this algorithm stores the nameSpaces
 * in the order that they need to be processed.
 * Specifically because a nameSpace is parsed before it is recorded,
 * all of its imports are recorded ahead of it.
 *
 * @param parser The PrattParser to use for parsing.
 * @param fileName The name of the file to link.
 * @param symbol The HashSymbol representing the nameSpace symbol.
 * @return An AstNameSpace containing the parsed nameSpace or an error.
 */
static AstNameSpace *parseLink(PrattParser *parser, unsigned char *fileName,
                               HashSymbol *symbol) {
    // check the file exists
    AgnosticFileId *fileId = calculatePath(fileName, parser);
    int save = PROTECT(fileId);
    if (fileId == NULL) {
        parserError(parser, "cannot find file \"%s\"", fileName);
        AstNameSpace *ns =
            newAstNameSpace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    // see if we've already parsed it, if so return the existing nameSpace id
    int found = lookUpNameSpace(fileId);
    if (found != -1) {
        AstNameSpace *ns =
            newAstNameSpace(BUFPI(parser->lexer->bufList), symbol, found);
        UNPROTECT(save);
        return ns;
    }
    // check for a recursive include
    if (fileIdInArray(fileId, fileIdStack)) {
        parserError(parser, "recursive include detected for %s", fileId->name);
        AstNameSpace *ns =
            newAstNameSpace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    // protect against recursive include
    pushAstFileIdArray(fileIdStack, fileId);
    // parse the file
    PrattParser *resultParser = NULL;
    // careful, 2 pushes in a row could realloc the save stack on push 1
    int save2 = PROTECT(fileId);
    AstDefinitions *definitions =
        prattParseLink(parser, fileId->name, &resultParser);
    REPLACE_PROTECT(save2, resultParser);
    PROTECT(definitions);
    // save the new nameSpace and it's parser
    AstNameSpaceImpl *impl =
        newAstNameSpaceImpl(BUFPI(parser->lexer->bufList), fileId, definitions);
    PROTECT(impl);
    found = pushAstNameSpaceArray(nameSpaces, impl);
    pushPrattParsers(parserStack, resultParser);
    // Capture exported operators for this nameSpace id (non-destructive cache
    // for later import)
    PrattExportedOps *ops = captureNameSpaceOperatorExports(resultParser);
    PROTECT(ops);
#ifdef SAFETY_CHECKS
    int found2 =
#endif
        pushPrattNsOpsArray(nsOpsCache, ops);
#ifdef SAFETY_CHECKS
    if (found != found2) {
        cant_happen("nameSpace ops cache index mismatch");
    }
#endif
    // un-protect against recursive include
    popAstFileIdArray(fileIdStack);
    // return the id of the nameSpace
    AstNameSpace *ns =
        newAstNameSpace(BUFPI(parser->lexer->bufList), symbol, found);
    UNPROTECT(save);
    return ns;
}

/**
 * @brief Create a PrattTrie from all the tokens in the parser's rules.
 *
 * This function iterates through the PrattRecordTable of the parser,
 * inserting each token into a growing PrattTrie structure.
 *
 * @param parser The PrattParser containing the rules.
 * @param C The initial PrattTrie to insert tokens into.
 * @return A new PrattTrie containing all tokens from the parser's rules.
 */
static PrattTrie *makePrattTrie(PrattParser *parser, PrattTrie *C) {
    HashSymbol *tok;
    Index i = 0;
    int save = PROTECT(
        parser); // not C because we need to have a slot for REPLACE_PROTECT
    while ((tok = iteratePrattRecordTable(parser->rules, &i, NULL)) != NULL) {
        C = insertPrattTrie(C, tok);
        REPLACE_PROTECT(save, C);
    }
    UNPROTECT(save);
    return C;
}

/**
 * @brief Construct an AstExpression from a symbol and two arguments.
 *
 * Used by infix pareselets to create binary expressions.
 *
 * @param I The ParserInfo containing context information.
 * @param op The HashSymbol representing the operator.
 * @param lhs The left-hand side AstExpression.
 * @param rhs The right-hand side AstExpression.
 * @return A new AstExpression representing the binary operation.
 */
static AstExpression *makePrattBinary(ParserInfo I, HashSymbol *op,
                                      AstExpression *lhs, AstExpression *rhs) {
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    int save = PROTECT(arguments);
    arguments = newAstExpressions(CPI(lhs), lhs, arguments);
    REPLACE_PROTECT(save, arguments);
    AstExpression *symbol = newAstExpression_Symbol(I, op);
    PROTECT(symbol);
    AstExpression *res = makeAstExpression_FunCall(I, symbol, arguments);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Construct an AstExpression for a unary operator.
 *
 * Used by prefix and postfix parselets to create unary expressions.
 *
 * @param I The ParserInfo containing context information.
 * @param op The HashSymbol representing the operator.
 * @param expr The AstExpression to apply the operator to.
 * @return A new AstExpression representing the unary operation.
 */
static AstExpression *makePrattUnary(ParserInfo I, HashSymbol *op,
                                     AstExpression *expr) {
    AstExpressions *arguments = newAstExpressions(CPI(expr), expr, NULL);
    int save = PROTECT(arguments);
    AstExpression *symbol = newAstExpression_Symbol(I, op);
    PROTECT(symbol);
    AstExpression *res = makeAstExpression_FunCall(I, symbol, arguments);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Create a new child PrattParser from a parent parser.
 *
 * This function initializes a new PrattParser with the parent parser as its
 * parent, and sets the lexer to be the same as the parent's lexer.
 *
 * @param parent The parent PrattParser to inherit from.
 * @return A new PrattParser that is a child of the given parent.
 */
static PrattParser *makeChildParser(PrattParser *parent) {
    PrattParser *child = newPrattParser(parent);
    // Share the same lexer so we read the same token stream
    child->lexer = parent->lexer;
    // Start with the parent's trie so keywords/operators are recognized;
    // child-specific additions will persistently extend this trie without
    // mutating the parent's structure.
    child->trie = parent->trie;
    // NameSpaces and rules resolve through the parent via parser->next;
    // child will add its own bindings locally as needed.
    return child;
}

/**
 * @brief Top-level entry point for the parser.
 *
 * This function runs the parser, then consumes the EOF token,
 * and returns the body of the parsed structure as an AstNest.
 *
 * @param parser The PrattParser to parse.
 * @return An AstNest containing the parsed body.
 */
static AstNest *top(PrattParser *parser) {
    ENTER(top);
    DEBUG("%s", parser->lexer->bufList->buffer->data);
    AstNest *body = nest_body(parser, TOK_EOF());
    int save = PROTECT(body);
    consume(parser, TOK_EOF());
    LEAVE(top);
    UNPROTECT(save);
    return body;
}

/**
 * @brief Parse the body of a nest, which can be a let block, a nameSpace, or
 * just statements.
 *
 * This function checks for a let block, a nameSpace declaration, or just
 * statements, and constructs an AstNest accordingly.
 *
 * @param parser The PrattParser to use for parsing.
 * @param terminal The HashSymbol that indicates the end of the nest body.
 * @return An AstNest containing the parsed definitions and statements.
 */
static AstNest *nest_body(PrattParser *parser, HashSymbol *terminal) {
    ENTER(nest_body);
    AstNest *res = NULL;
    int save = PROTECT(parser);
    if (match(parser, TOK_LET())) {
        // Create a child parser so operator/type/macro definitions are scoped
        // to the body of this let/in block and do not leak outward.
        PrattParser *child = makeChildParser(parser);
        save = PROTECT(child);
        AstDefinitions *defs = definitions(child, TOK_IN());
        PROTECT(defs);
        // The child shares the same lexer, so consuming with the parent is fine
        consume(parser, TOK_IN());
        AstExpressions *stats = statements(child, terminal);
        PROTECT(stats);
        res = newAstNest(CPI(defs), defs, stats);
    } else if (match(parser, TOK_NAMESPACE())) {
        // Keep nameSpace definitions in the current parser so preamble- and
        // file-level operator definitions remain visible globally. Operator
        // export control for nameSpaces can be added later.
        AstDefinitions *defs = definitions(parser, terminal);
        save = PROTECT(defs);
        res = newAstNest(CPI(defs), defs, NULL);
    } else {
        AstExpressions *stats = statements(parser, terminal);
        save = PROTECT(stats);
        res = newAstNest(CPI(stats), NULL, stats);
    }
    LEAVE(nest_body);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Parse a series of statements until a terminal token is reached.
 *
 * This function recursively parses expressions with optional semicolons between
 * them, and constructs an AstExpressions list until the terminal token is
 * found. Semicolons are optional and multiple sequential semicolons are
 * allowed.
 *
 * @param parser The PrattParser to use for parsing.
 * @param terminal The HashSymbol that indicates the end of the statements.
 * @return An AstExpressions containing the parsed statements.
 */
static AstExpressions *statements(PrattParser *parser, HashSymbol *terminal) {
    ENTER(statements);
    AstExpression *expr = expression(parser);
    int save = PROTECT(expr);
    // Centralized semicolon handling: allow optional semicolons (including
    // multiple) after statements
    while (match(parser, TOK_SEMI()))
        ;
    AstExpressions *rest = NULL;
    if (!check(parser, terminal)) {
        rest = statements(parser, terminal);
        PROTECT(rest);
    }
    AstExpressions *this = newAstExpressions(CPI(expr), expr, rest);
    LEAVE(statements);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Parse an expression with an initial precedence of zero.
 *
 * This function handles the parsing of expressions based on their precedence,
 * allowing for different levels of operator precedence to be respected.
 *
 * @param parser The PrattParser to use for parsing.
 * @param precedence The precedence level to parse at.
 * @return An AstExpression representing the parsed expression.
 */
static AstExpression *expression(PrattParser *parser) {
    AstExpression *res = expressionPrecedence(parser, 0);
    int save = PROTECT(res);
    synchronize(parser);
    UNPROTECT(save);
    return res;
}

static AstDefinitions *prependBuiltinWrappers(ParserInfo PI,
                                              AstDefinitions *next) {
    int save = PROTECT(generatedBuiltins);
    for (AstDefinitions *wrapper = generatedBuiltins; wrapper != NULL;
         wrapper = wrapper->next) {
        next = newAstDefinitions(PI, wrapper->definition, next);
        REPLACE_PROTECT(save, next);
    }
    UNPROTECT(save);
    return next;
}

/**
 * @brief Parse a sequence of definitions.
 *
 * @param parser The PrattParser to use for parsing.
 * @param terminal The HashSymbol that indicates the end of definitions.
 * @return An AstDefinitions containing the parsed definitions, or NULL if no
 * definitions are found.
 */
static AstDefinitions *definitions(PrattParser *parser, HashSymbol *terminal) {
    ENTER(definitions);
    if (check(parser, terminal)) {
        LEAVE(definitions);
        return NULL;
    }
    if (check(parser, TOK_EOF())) {
        LEAVE(definitions);
        return NULL;
    }
    AstDefinition *def = definition(parser);
    int save = PROTECT(def);
    // Centralized semicolon handling: allow optional semicolons (including
    // multiple) after any definition
    while (match(parser, TOK_SEMI()))
        ;
    AstDefinitions *next = definitions(parser, terminal);
    PROTECT(next);
    AstDefinitions *this = NULL;
    if (def->type == AST_DEFINITION_TYPE_BUILTINSSLOT) {
        this = prependBuiltinWrappers(CPI(def), next);
    } else {
        this = newAstDefinitions(CPI(def), def, next);
    }
    LEAVE(definitions);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Validate an operator string for use in the parser.
 *
 * This function checks if the operator string is valid according to certain
 * rules:
 * - It cannot be empty.
 * - It cannot start with a numeric digit.
 * - It cannot start with an opening or closing bracket.
 * - It cannot contain whitespace.
 *
 * @param parser The PrattParser to report errors to.
 * @param operator The PrattUnicode operator string to validate.
 */
static bool validateOperator(PrattParser *parser, PrattUnicode *operator) {
    if (wcslen(operator->entries) == 0) {
        parserError(parser, "operator cannot be empty string");
        return false;
    } else if (iswdigit(operator->entries[0])) {
        parserError(parser, "operator cannot start with a numeric digit");
        return false;
    } else if (unicode_isopen(operator->entries[0])) {
        parserError(parser, "operator cannot start with an opening bracket");
        return false;
    } else if (unicode_isclose(operator->entries[0])) {
        parserError(parser, "operator cannot start with a closing bracket");
        return false;
    } else {
        for (Index i = 0; i < operator->size; i++) {
            if (iswspace(operator->entries[i])) {
                parserError(parser, "operator cannot contain whitespace");
                return false;
            }
        }
    }
    return true;
}

/**
 * @brief Create an AstFargList from a name and the next argument list.
 *
 * This function creates a new AstFargList node with the given name
 * and links it to the provided next argument list.
 *
 * @param PI The ParserInfo containing context information.
 * @param name The name of the argument.
 * @param next The next AstFargList in the sequence.
 * @return A new AstFargList containing the argument.
 */
static AstFargList *makeAstFargList(ParserInfo PI, char *name,
                                    AstFargList *next) {
    HashSymbol *sym = newSymbol(name);
    AstFarg *arg = newAstFarg_Symbol(PI, sym);
    int save = PROTECT(arg);
    AstFargList *res = newAstFargList(PI, arg, next);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Create an AstExpressions list from a name and the next expression
 * list.
 *
 * This function creates a new AstExpressions node with the given name
 * and links it to the provided next expression list.
 *
 * @param PI The ParserInfo containing context information.
 * @param name The name of the argument.
 * @param next The next AstExpressions in the sequence.
 * @return A new AstExpressions containing the argument.
 */
static AstExpressions *makeAstAarglist(ParserInfo PI, char *name,
                                       AstExpressions *next) {
    HashSymbol *sym = newSymbol(name);
    AstExpression *expr = newAstExpression_Symbol(PI, sym);
    int save = PROTECT(expr);
    AstExpressions *res = newAstExpressions(PI, expr, next);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Generate a hygienic operator macro body.
 */
static AstDefinition *makeHygenicOperatorBody(ParserInfo PI, HashSymbol *symbol,
                                              AstFargList *fargs,
                                              AstExpressions *aargs,
                                              AstExpression *impl) {
    AstExpression *bodyExpr = makeAstExpression_FunCall(PI, impl, aargs);
    int save = PROTECT(bodyExpr);
    AstAltArgs *altArgs = newAstAltArgs(PI, fargs, NULL);
    PROTECT(altArgs);
    AstExpressions *exprs = newAstExpressions(PI, bodyExpr, NULL);
    PROTECT(exprs);
    AstNest *body = newAstNest(PI, NULL, exprs);
    PROTECT(body);
    AstAltFunction *altFun = newAstAltFunction(PI, altArgs, body);
    PROTECT(altFun);
    // Generate a macro instead of a function
    // This creates: macro symbol(x, y) { impl(x, y) }
    AstDefinition *res = makeAstDefinition_Macro(PI, symbol, altFun);
    // Unprotect all in one go
    UNPROTECT(save);
    return res;
}

static inline HashSymbol *makeMacroName() { return genSymDollar("opMacro"); }

static AstDefinition *makeHygienicNaryOperatorDef(ParserInfo PI, int arity,
                                                  HashSymbol *macroName,
                                                  AstExpression *impl) {
    char buffer[32];
    // make the formal argument list ()
    AstFargList *argList = NULL;
    int save = PROTECT(argList);
    // make the actual argument list ()
    AstExpressions *callArgs = NULL;
    while (arity > 0) {
        snprintf(buffer, sizeof(buffer), "x%d", arity);
        argList = makeAstFargList(PI, buffer, argList);
        PROTECT(argList);
        callArgs = makeAstAarglist(PI, buffer, callArgs);
        PROTECT(callArgs);
        arity--;
    }
    // make the macro definition
    AstDefinition *res =
        makeHygenicOperatorBody(PI, macroName, argList, callArgs, impl);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Add a new user-defined operator to the parser's operator table.
 *
 * This function adds a new operator with its fixity, precedence, and
 * implementation to the parser's operator table.
 *
 * It is an error to redefine an operator of the same fixity type.
 *
 * @param parser The PrattParser to add the operator to.
 * @param fixity The fixity type of the operator (prefix, infix, postfix).
 * @param associativity The associativity type of the operator (left, right,
 * none).
 * @param precedence The precedence level of the operator.
 * @param operator The PrattUnicode representation of the operator.
 * @param impl The AstExpression implementation of the operator.
 */
static AstDefinition *addOperator(PrattParser *parser, PrattFixity fixity,
                                  PrattAssoc associativity, int precedence,
                                  PrattUnicode *operator, AstExpression *impl) {
    HashSymbol *op = unicodeToSymbol(operator);
    // Only look for an existing operator in the current (local) parser scope.
    // This allows inner scopes to shadow operators defined in outer scopes.
    PrattRecord *record = NULL;
    getPrattRecordTable(parser->rules, op, &record);
    int save = PROTECT(record);
    bool isNewOperator = (record == NULL);
    if (record) {
        record = copyPrattRecord(record);
        PROTECT(record);
    } else {
        PrattFixityConfig empty = {NULL,  0,    NULL, NULL, false,
                                   false, NULL, -1,   NULL};
        record = newPrattRecord(op, empty, empty, empty);
        PROTECT(record);
    }
    HashSymbol *hygienicFunc = makeMacroName();
    bool isBareSymbol = (impl && impl->type == AST_EXPRESSION_TYPE_SYMBOL);
    int scaledPrec = precedence * PRECEDENCE_SCALE;
    AstDefinition *def = NULL;
    PrattFixityConfig *fixityConfig = NULL;
    switch (fixity) {
    case PRATTFIXITY_TYPE_PREFIX: {
        if (record->prefix.op) {
            parserErrorAt(CPI(impl), parser,
                          "attempt to redefine prefix operator \"%ls\"",
                          operator->entries);
        }
        fixityConfig = &record->prefix;
        fixityConfig->op = userPrefix;
        def = makeHygienicNaryOperatorDef(CPI(impl), 1, hygienicFunc, impl);
    } break;
    case PRATTFIXITY_TYPE_INFIX: {
        if (record->infix.op) {
            parserErrorAt(CPI(impl), parser,
                          "attempt to redefine infix operator \"%ls\"",
                          operator->entries);
        } else if (record->postfix.op) {
            parserErrorAt(
                CPI(impl), parser,
                "attempt to define existing postfix operator \"%ls\" as infix",
                operator->entries);
        }
        fixityConfig = &record->infix;
        fixityConfig->op =
            (associativity == PRATTASSOC_TYPE_LEFT)    ? userInfixLeft
            : (associativity == PRATTASSOC_TYPE_RIGHT) ? userInfixRight
                                                       : userInfixNone;
        def = makeHygienicNaryOperatorDef(CPI(impl), 2, hygienicFunc, impl);
    } break;
    case PRATTFIXITY_TYPE_POSTFIX: {
        if (record->postfix.op) {
            parserErrorAt(CPI(impl), parser,
                          "attempt to redefine postfix operator \"%ls\"",
                          operator->entries);
        } else if (record->infix.op) {
            parserErrorAt(
                CPI(impl), parser,
                "attempt to define existing infix operator \"%ls\" as postfix",
                operator->entries);
        }
        fixityConfig = &record->postfix;
        fixityConfig->op = userPostfix;
        def = makeHygienicNaryOperatorDef(CPI(impl), 1, hygienicFunc, impl);
    } break;
    default:
        cant_happen("unknown fixity type %s", prattFixityName(fixity));
    }
    PROTECT(def);
    // Common finalization: update parser state
    fixityConfig->prec = scaledPrec;
    fixityConfig->originalImpl = impl;
    fixityConfig->hygienicFunc = hygienicFunc;
    fixityConfig->isBareSymbol = isBareSymbol;
    if (isNewOperator) {
        parser->trie = insertPrattTrie(parser->trie, op);
    }
    setPrattRecordTable(parser->rules, op, record);
    UNPROTECT(save);
    return def;
}

/**
 * @brief Parse a user-defined mixfix operator expression.
 *
 * This function handles the parsing of mixfix operators based on their
 * defined patterns, collecting arguments and constructing the appropriate
 * function call expression.
 *
 * @param record The PrattRecord containing the mixfix operator definition.
 * @param parser The PrattParser to use for parsing.
 * @param lhs The left-hand side AstExpression (if applicable).
 * @param tok The PrattToken representing the operator.
 * @return An AstExpression representing the parsed mixfix operation.
 */
AstExpression *userMixfix(PrattRecord *record, PrattParser *parser,
                          AstExpression *lhs, PrattToken *tok,
                          PrattFixity fixity) {
    // Mixfix operators are implemented via their stored pattern
    PrattFixityConfig *fixityConfig =
        fixity == PRATTFIXITY_TYPE_PREFIX  ? &record->prefix
        : fixity == PRATTFIXITY_TYPE_INFIX ? &record->infix
                                           : &record->postfix;

    PrattMixfixPattern *pattern = fixityConfig->pattern;

    AstExpressionArray *args = newAstExpressionArray();
    int save = PROTECT(args);

    PrattStrings *keywords = copyPrattStrings(pattern->keywords);
    PROTECT(keywords);
    int arity = pattern->arity;

    // Collect the left-hand side expression if applicable
    if (fixity == PRATTFIXITY_TYPE_INFIX ||
        fixity == PRATTFIXITY_TYPE_POSTFIX) {
        pushAstExpressionArray(args, lhs);
        arity--;
    }
    int precedence = fixityConfig->prec +
                     (pattern->associativity == PRATTASSOC_TYPE_LEFT    ? 1
                      : pattern->associativity == PRATTASSOC_TYPE_RIGHT ? -1
                                                                        : 0);
    Index kwIndex = 1;
    // Parse and collect the arguments interleaved by the operator tokens
    while (arity > 0) {
        lhs = expressionPrecedence(parser, precedence);
        int save2 = PROTECT(lhs);
        pushAstExpressionArray(args, lhs);
        UNPROTECT(save2);
        arity--;
        if (arity > 0 || !(pattern->endsWithHole)) {
            // Consume the next keyword
            PrattUnicode *kw = keywords->entries[kwIndex++];
            PrattToken *nextTok = peek(parser);
            HashSymbol *nextSym = unicodeToSymbol(kw);
            if (!isAtomSymbol(nextTok, nextSym)) {
                parserErrorAt(
                    CPI(lhs), parser,
                    "expected mixfix operator keyword \"%ls\" got \"%s\"",
                    kw->entries, nextTok->type->name);
            }
            next(parser);
        }
    }
    // Build the argument list from the collected array of expressions
    AstExpressions *argList = NULL;
    for (Index i = args->size; i > 0; i--) {
        AstExpression *arg = args->entries[i - 1];
        argList = newAstExpressions(CPI(arg), arg, argList);
        PROTECT(argList);
    }
    // Create annotated symbol with both hygienic wrapper and original
    // implementation
    AstExpression *func = makeAstExpression_AnnotatedSymbol(
        TOKPI(tok), fixityConfig->hygienicFunc, fixityConfig->originalImpl);
    PROTECT(func);
    if (fixityConfig->importNsRef >= 0) {
        func = makeAstExpression_LookUp(TOKPI(tok), fixityConfig->importNsRef,
                                        fixityConfig->importNsSymbol, func);
        PROTECT(func);
    }
    AstExpression *res = makeAstExpression_FunCall(TOKPI(tok), func, argList);
    PROTECT(res);
    if (pattern->associativity == PRATTASSOC_TYPE_NONE) {
        PrattToken *next = peek(parser);
        PROTECT(next);
        if (next != NULL) {
            PrattRecord *nextRecord = fetchRecord(parser, next->type);
            if (nextRecord != NULL &&
                nextRecord->infix.prec == record->infix.prec &&
                next->type == tok->type) {
                parserErrorAt(
                    TOKPI(next), parser,
                    "non-associative operator %ls(%s) used in succession",
                    pattern->keywords->entries[0]->entries, tok->type->name);
            }
        }
    }
    UNPROTECT(save);
    return res;
}

static AstExpression *userPrefixMix(PrattRecord *record, PrattParser *parser,
                                    AstExpression *lhs, PrattToken *tok) {
    return userMixfix(record, parser, lhs, tok, PRATTFIXITY_TYPE_PREFIX);
}

static AstExpression *userInfixMix(PrattRecord *record, PrattParser *parser,
                                   AstExpression *lhs, PrattToken *tok) {
    return userMixfix(record, parser, lhs, tok, PRATTFIXITY_TYPE_INFIX);
}

static AstExpression *userPostfixMix(PrattRecord *record, PrattParser *parser,
                                     AstExpression *lhs, PrattToken *tok) {
    return userMixfix(record, parser, lhs, tok, PRATTFIXITY_TYPE_POSTFIX);
}
/**
 * @brief Add a new user-defined mixfix operator to the parser's operator table.
 *
 * This function adds a new mixfix operator with its pattern, associativity,
 * precedence, and implementation to the parser's operator table.
 *
 * It is an error to redefine an existing mixfix operator.
 *
 * @param parser The PrattParser to add the mixfix operator to.
 * @param pattern The PrattMixfixPattern defining the mixfix operator.
 * @param associativity The associativity type of the operator (left, right,
 * none).
 * @param precedence The precedence level of the operator.
 * @param impl The AstExpression implementation of the operator.
 */
static AstDefinition *addMixfixOperator(PrattParser *parser,
                                        PrattMixfixPattern *pattern,
                                        PrattAssoc associativity,
                                        int precedence, AstExpression *impl) {
    // Add secondary keywords to trie (no conflict check - precedence handles
    // disambiguation)
    for (Index i = 1; i < pattern->keywords->size; ++i) {
        HashSymbol *inner = unicodeToSymbol(pattern->keywords->entries[i]);
        parser->trie = insertPrattTrie(parser->trie, inner);
    }
    PrattFixity fixity = getFixityFromPattern(pattern);
    PrattUnicode *operator = pattern->keywords->entries[0];
    (void)addOperator(parser, fixity, associativity, precedence, operator,
                      impl);
    HashSymbol *op = unicodeToSymbol(operator);
    // Store the mixfix pattern in the PrattRecord for later parsing
    PrattRecord *record = NULL;
    getPrattRecordTable(parser->rules, op, &record);
    AstDefinition *def = NULL;
    switch (fixity) {
    case PRATTFIXITY_TYPE_PREFIX:
        record->prefix.op = userPrefixMix;
        def = makeHygienicNaryOperatorDef(CPI(impl), pattern->arity,
                                          record->prefix.hygienicFunc, impl);
        if (record->prefix.pattern != NULL) {
            parserErrorAt(CPI(impl), parser,
                          "attempt to redefine mixfix operator \"%ls\"",
                          operator->entries);
        }
        record->prefix.pattern = pattern;
        break;
    case PRATTFIXITY_TYPE_INFIX:
        record->infix.op = userInfixMix;
        def = makeHygienicNaryOperatorDef(CPI(impl), pattern->arity,
                                          record->infix.hygienicFunc, impl);
        if (record->infix.pattern != NULL) {
            parserErrorAt(CPI(impl), parser,
                          "attempt to redefine mixfix operator \"%ls\"",
                          operator->entries);
        }
        record->infix.pattern = pattern;
        break;
    case PRATTFIXITY_TYPE_POSTFIX:
        record->postfix.op = userPostfixMix;
        def = makeHygienicNaryOperatorDef(CPI(impl), pattern->arity,
                                          record->postfix.hygienicFunc, impl);
        if (record->postfix.pattern != NULL) {
            parserErrorAt(CPI(impl), parser,
                          "attempt to redefine mixfix operator \"%ls\"",
                          operator->entries);
        }
        record->postfix.pattern = pattern;
        break;
    }
    return def;
}

/**
 * @brief Shallow copy a PrattRecordTable.
 *
 * This function iterates through the source PrattRecordTable and copies each
 * record to the destination PrattRecordTable, ensuring that all operator
 * definitions are preserved.
 *
 * @param to The destination PrattRecordTable to copy to.
 * @param from The source PrattRecordTable to copy from.
 */
static void copyPrattRecordTable(PrattRecordTable *to, PrattRecordTable *from) {
    Index i = 0;
    HashSymbol *symbol = NULL;
    PrattRecord *record = NULL;
    while ((symbol = iteratePrattRecordTable(from, &i, &record)) != NULL) {
        setPrattRecordTable(to, symbol, record);
    }
}

/**
 * @brief Shallow copy a PrattNsIdTable.
 *
 * This function iterates through the source PrattNsIdTable and copies each
 * nameSpace symbol and its associated record to the destination PrattNsIdTable.
 *
 * @param to The destination PrattNsIdTable to copy to.
 * @param from The source PrattNsIdTable to copy from.
 */
static void copyPrattNsIdTable(PrattNsIdTable *to, PrattNsIdTable *from) {
    Index i = 0;
    HashSymbol *symbol = NULL;
    int record = 0;
    while ((symbol = iteratePrattNsIdTable(from, &i, &record)) != NULL) {
        setPrattNsIdTable(to, symbol, record);
    }
}

static PrattParser *meldParsers(PrattParser *to, PrattParser *from)
    __attribute__((unused));

/**
 * @brief Merge two PrattParsers, combining their operator definitions and
 * nameSpaces.
 *
 * This function creates a new PrattParser that combines the rules and
 * nameSpaces of the two given PrattParsers. It ensures that no operator is
 * redefined and merges the trie structures.
 *
 * CURRENTLY UNUSED, but might be useful in the future.
 *
 * @param to The first PrattParser to merge.
 * @param from The second PrattParser to merge.
 * @return A new PrattParser that contains the merged rules and nameSpaces.
 */
static PrattParser *meldParsers(PrattParser *to, PrattParser *from) {
    if (from->trie) {
        PrattParser *result = newPrattParser(to->next);
        int save = PROTECT(result);
        copyPrattRecordTable(result->rules, to->rules);
        copyPrattNsIdTable(result->nameSpaces, to->nameSpaces);
        result->lexer = to->lexer;
        result->trie = to->trie;
        Index i = 0;
        HashSymbol *op = NULL;
        PrattRecord *record = NULL;
        while ((op = iteratePrattRecordTable(from->rules, &i, &record)) !=
               NULL) {
            PrattRecord *target = NULL;
            getPrattRecordTable(to->rules, op, &target);
            if (target == NULL) {
                target = copyPrattRecord(record);
                PROTECT(target);
                setPrattRecordTable(result->rules, op, target);
                result->trie = insertPrattTrie(result->trie, op);
            } else {
                if (record->prefix.originalImpl) {
                    if (target->prefix.originalImpl) {
                        parserError(to, "import redefines prefix operator %s",
                                    op->name);
                    } else {
                        target->prefix.originalImpl =
                            record->prefix.originalImpl;
                        target->prefix.hygienicFunc =
                            record->prefix.hygienicFunc;
                        target->prefix.isBareSymbol =
                            record->prefix.isBareSymbol;
                        target->prefix.prec = record->prefix.prec;
                        target->prefix.op = record->prefix.op;
                    }
                }
                if (record->infix.originalImpl) {
                    if (target->infix.originalImpl) {
                        parserError(to, "import redefines infix operator %s",
                                    op->name);
                    } else if (target->postfix.originalImpl) {
                        parserError(to,
                                    "import defines infix operator %s"
                                    " over existing postfix operator",
                                    op->name);
                    } else {
                        target->infix.originalImpl = record->infix.originalImpl;
                        target->infix.hygienicFunc = record->infix.hygienicFunc;
                        target->infix.isBareSymbol = record->infix.isBareSymbol;
                        target->infix.prec = record->infix.prec;
                        target->infix.op = record->infix.op;
                    }
                }
                if (record->postfix.originalImpl) {
                    if (target->postfix.originalImpl) {
                        parserError(to, "import redefines postfix operator %s",
                                    op->name);
                    } else if (target->infix.originalImpl) {
                        parserError(to,
                                    "import defines postfix operator %s"
                                    " over existing infix operator",
                                    op->name);
                    } else {
                        target->postfix.originalImpl =
                            record->postfix.originalImpl;
                        target->postfix.hygienicFunc =
                            record->postfix.hygienicFunc;
                        target->postfix.isBareSymbol =
                            record->postfix.isBareSymbol;
                        target->postfix.prec = record->postfix.prec;
                        target->postfix.op = record->postfix.op;
                    }
                }
            }
        }
        UNPROTECT(save);
        return result;
    } else {
        return to;
    }
}

static PrattUnicode *prattUnicodeSubstr(PrattUnicode *str, Index start,
                                        Index end) {
    PrattUnicode *res = newPrattUnicode();
    int save = PROTECT(res);
    for (Index i = start; i < end; i++) {
        pushPrattUnicode(res, str->entries[i]);
    }
    pushPrattUnicode(res, '\0');
    UNPROTECT(save);
    return res;
}

typedef enum PatternStates {
    PATTERN_STATE_S,   // Start
    PATTERN_STATE_C,   // Initial Characters
    PATTERN_STATE_CU,  // Underscore after Characters
    PATTERN_STATE_CUC, // Characters after Underscore, starts with characters
    PATTERN_STATE_U,   // Initial Underscore
    PATTERN_STATE_UC,  // Characters after initial underscore
    PATTERN_STATE_UCU, // Underscore after characters starting with underscore
    PATTERN_STATE_ERR, // Error state
    PATTERN_STATE_CUF, // End of string initial characters final underscore
    PATTERN_STATE_CCF, // End of String starting and ending with characters
    PATTERN_STATE_UCF, // End of String starting with underscore, ending with
                       // characters
    PATTERN_STATE_UUF, // End of String starting and ending with underscore
} PatternStates;

typedef enum PatternEvents {
    PATTERN_EVENT_CHAR,
    PATTERN_EVENT_UNDERSCORE,
    PATTERN_EVENT_END,
} PatternEvents;

// see docs/OPERATOR_PATTERN.md
static int patternStateTable[11][3] = {
    // CHAR               UNDERSCORE          END
    {PATTERN_STATE_C, PATTERN_STATE_U, PATTERN_STATE_ERR},     // S
    {PATTERN_STATE_C, PATTERN_STATE_CU, PATTERN_STATE_ERR},    // C
    {PATTERN_STATE_CUC, PATTERN_STATE_ERR, PATTERN_STATE_CUF}, // CU
    {PATTERN_STATE_CUC, PATTERN_STATE_CU, PATTERN_STATE_CCF},  // CUC
    {PATTERN_STATE_UC, PATTERN_STATE_ERR, PATTERN_STATE_ERR},  // U
    {PATTERN_STATE_UC, PATTERN_STATE_UCU, PATTERN_STATE_UCF},  // UC
    {PATTERN_STATE_UC, PATTERN_STATE_ERR, PATTERN_STATE_UUF},  // UCU
    {PATTERN_STATE_ERR, PATTERN_STATE_ERR, PATTERN_STATE_ERR}, // ERR
    {PATTERN_STATE_ERR, PATTERN_STATE_ERR, PATTERN_STATE_ERR}, // FIN_CU
    {PATTERN_STATE_ERR, PATTERN_STATE_ERR, PATTERN_STATE_ERR}, // FIN_UC
    {PATTERN_STATE_ERR, PATTERN_STATE_ERR, PATTERN_STATE_ERR}, // FIN_UCU
};

static PrattMixfixPattern *
parseMixfixPattern(ParserInfo PI, PrattParser *parser, PrattUnicode *str) {
    // A mixfix pattern is a sequence of keywords and holes.
    // A hole is represented by an underscore character '_'.
    PrattStrings *strings = newPrattStrings();
    PROTECT(strings);
    Index i = 0;
    Index len = str->size - 1; // size includes the null
    Index start = 0;
    int state = PATTERN_STATE_S;
    int arity = 0;
    int event;
    while (state < PATTERN_STATE_ERR) {
        if (i >= len) {
            event = PATTERN_EVENT_END;
        } else if (str->entries[i] == '_') {
            event = PATTERN_EVENT_UNDERSCORE;
            arity++;
        } else {
            event = PATTERN_EVENT_CHAR;
        }

        state = patternStateTable[state][event];
        switch (state) {
        case PATTERN_STATE_C:
            break;
        case PATTERN_STATE_CU: {
            PrattUnicode *kw = prattUnicodeSubstr(str, start, i);
            int save = PROTECT(kw);
            pushPrattStrings(strings, kw);
            UNPROTECT(save);
            start = i + 1;
        } break;
        case PATTERN_STATE_CUC:
            break;
        case PATTERN_STATE_U:
            start = i + 1;
            break;
        case PATTERN_STATE_UC:
            break;
        case PATTERN_STATE_UCU: {
            PrattUnicode *kw = prattUnicodeSubstr(str, start, i);
            int save = PROTECT(kw);
            pushPrattStrings(strings, kw);
            UNPROTECT(save);
            start = i + 1;
        } break;
        case PATTERN_STATE_CUF:
            break;
        case PATTERN_STATE_CCF: {
            PrattUnicode *kw = prattUnicodeSubstr(str, start, i);
            int save = PROTECT(kw);
            pushPrattStrings(strings, kw);
            UNPROTECT(save);
        } break;
        case PATTERN_STATE_UCF: {
            PrattUnicode *kw = prattUnicodeSubstr(str, start, i);
            int save = PROTECT(kw);
            pushPrattStrings(strings, kw);
            UNPROTECT(save);
            start = i + 1;
        } break;
        case PATTERN_STATE_UUF:
            break;
        }
        i++;
    }
    if (state == PATTERN_STATE_ERR) {
        parserErrorAt(PI, parser, "invalid operator pattern \"%ls\"",
                      str->entries);
        return NULL;
    }
    bool isValid = true;
    for (i = 0; i < strings->size; i++) {
        if (!validateOperator(parser, strings->entries[i])) {
            isValid = false;
        }
    }
    PrattMixfixPattern *pattern = NULL;
    if (isValid) {
        pattern = newPrattMixfixPattern(
            strings, arity,
            state == PATTERN_STATE_UCF || state == PATTERN_STATE_UUF,
            state == PATTERN_STATE_CUF || state == PATTERN_STATE_UUF);
    }
    return pattern;
}

static PrattAssoc parseOptionalAssociativity(PrattParser *parser) {
    PrattAssoc associativity = PRATTASSOC_TYPE_NONE;
    if (check(parser, TOK_ATOM())) {
        PrattToken *atom = peek(parser);
        if (atom->value->val.atom == TOK_LEFT() ||
            atom->value->val.atom == TOK_RIGHT() ||
            atom->value->val.atom == TOK_NONE()) {
            next(parser);
            if (atom->value->val.atom == TOK_LEFT()) {
                associativity = PRATTASSOC_TYPE_LEFT;
            } else if (atom->value->val.atom == TOK_RIGHT()) {
                associativity = PRATTASSOC_TYPE_RIGHT;
            }
        }
    }
    return associativity;
}

/**
 * Parse `operator <pattern> [<associativity>] <precedence> <implementation>;`
 * Helper function that parses the operator definition given a pattern and token
 * for error reporting.
 */
static AstDefinition *operatorWithPattern(PrattParser *parser, PrattToken *tok,
                                          PrattMixfixPattern *pattern) {
    ENTER(operatorWithPattern);
    int save = PROTECT(tok);
    PROTECT(pattern);

    if (pattern == NULL) {
        LEAVE(operatorWithPattern);
        UNPROTECT(save);
        return newAstDefinition_Blank(TOKPI(tok));
    }

    PrattAssoc assoc = parseOptionalAssociativity(parser);
    pattern->associativity = assoc;

    if (!check(parser, TOK_NUMBER())) {
        parserErrorAt(TOKPI(tok), parser,
                      "expected numeric precedence after operator pattern");
        LEAVE(operatorWithPattern);
        UNPROTECT(save);
        return newAstDefinition_Blank(TOKPI(tok));
    }
    PrattToken *prec = next(parser);
    PROTECT(prec);
    MaybeBigInt *bi = prec->value->val.number;
    if (bi->type != BI_SMALL || bi->imag) {
        parserErrorAt(TOKPI(tok), parser,
                      "expected integer precedence after operator pattern");
        LEAVE(operatorWithPattern);
        UNPROTECT(save);
        return newAstDefinition_Blank(TOKPI(tok));
    }
    int precedence = bi->small;
    AstExpression *impl = expression(parser);
    PROTECT(impl);
    AstDefinition *def =
        addMixfixOperator(parser, pattern, assoc, precedence, impl);
    LEAVE(operatorWithPattern);
    UNPROTECT(save);
    return def;
}

/**
 * Parse `operator <pattern> [<associativity>] <precedence> <implementation>;`
 */
static AstDefinition *operator(PrattParser *parser) {
    ENTER(operator);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    PrattUnicode *str = rawString(parser);
    PROTECT(str);
    PrattMixfixPattern *pattern = parseMixfixPattern(TOKPI(tok), parser, str);
    PROTECT(pattern);
    AstDefinition *def = operatorWithPattern(parser, tok, pattern);
    LEAVE(operator);
    UNPROTECT(save);
    return def;
}

/**
 * @brief Parse a definition.
 *
 * This fuction parses a definition, which can be an assignment,
 * a typedef, a function, a printer, a macro, a link, an alias,
 * a prefix, an infix, or a postfix operator.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstDefinition representing the parsed definition, or NULL.
 */
static AstDefinition *definition(PrattParser *parser) {
    ENTER(definition);
    AstDefinition *res = NULL;
    int save;
    save = PROTECT(res);
    if (match(parser, TOK_BUILTINS())) {
        res = newAstDefinition_BuiltinsSlot(TOKPI(peek(parser)));
        save = PROTECT(res);
    } else if (check(parser, TOK_ATOM())) {
        res = assignment(parser);
        save = PROTECT(res);
    } else if (match(parser, TOK_TUPLE())) {
        res = multidefinition(parser);
        save = PROTECT(res);
    } else if (match(parser, TOK_TYPEDEF())) {
        res = typedefinition(parser);
        save = PROTECT(res);
    } else if (match(parser, TOK_UNSAFE())) {
        consume(parser, TOK_FN());
        res = defun(parser, true, false);
        save = PROTECT(res);
    } else if (match(parser, TOK_FN())) {
        res = defun(parser, false, false);
        save = PROTECT(res);
    } else if (match(parser, TOK_PRINT())) {
        res = defun(parser, false, true);
        save = PROTECT(res);
    } else if (match(parser, TOK_MACRO())) {
        res = defmacro(parser);
        save = PROTECT(res);
    } else if (match(parser, TOK_LINK())) {
        res = link(parser);
        save = PROTECT(res);
    } else if (match(parser, TOK_ALIAS())) {
        res = alias(parser);
        save = PROTECT(res);
    } else if (match(parser, TOK_IMPORT())) {
        res = importop(parser);
        save = PROTECT(res);
    } else if (match(parser, TOK_EXPORT())) {
        res = exportop(parser);
        save = PROTECT(res);
    } else if (match(parser, TOK_OPERATOR())) {
        res = operator(parser);
        save = PROTECT(res);
    } else {
        PrattToken *tok = next(parser);
        validateLastAlloc();
        // Provide a more informative error message that includes the
        // unexpected token kind and, where possible, its lexeme.
        if (tok->type == TOK_ATOM() && tok->value &&
            tok->value->type == PRATTVALUE_TYPE_ATOM && tok->value->val.atom) {
            parserErrorAt(TOKPI(tok), parser,
                          "expected definition; found atom '%s'",
                          tok->value->val.atom->name);
        } else if (tok->type == TOK_STRING() && tok->value &&
                   tok->value->type == PRATTVALUE_TYPE_STRING &&
                   tok->value->val.string) {
            parserErrorAt(TOKPI(tok), parser,
                          "expected definition; found string \"%ls\"",
                          tok->value->val.string->entries);
        } else if (tok->type == TOK_NUMBER() && tok->value &&
                   tok->value->type == PRATTVALUE_TYPE_NUMBER &&
                   tok->value->val.number) {
            parserErrorAt(TOKPI(tok), parser,
                          "expected definition; found number");
        } else if (tok->type == TOK_CHAR() && tok->value &&
                   tok->value->type == PRATTVALUE_TYPE_STRING &&
                   tok->value->val.string) {
            parserErrorAt(TOKPI(tok), parser,
                          "expected definition; found character '%ls'",
                          tok->value->val.string->entries);
        } else if (tok->type && tok->type->name) {
            parserErrorAt(TOKPI(tok), parser,
                          "expected definition; found token %s",
                          tok->type->name);
        } else {
            parserErrorAt(TOKPI(tok), parser,
                          "expected definition; found unexpected token");
        }
        res = newAstDefinition_Blank(TOKPI(tok));
        save = PROTECT(res);
    }
    synchronize(parser);
    LEAVE(definition);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Parse an export directive and set export flags on operator records.
 *
 * Syntax supported (non-destructive, flags only):
 *   export operators;
 *   export prefix "op";
 *   export infix  "op";
 *   export postfix "op";
 *
 * Notes:
 * - Only operators defined in the current parser scope may be exported.
 * - Exporting a non-local operator raises a parser error.
 */
static AstDefinition *exportop(PrattParser *parser) {
    ENTER(exportop);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    AstDefinition *res = NULL;
    if (check(parser, TOK_ATOM())) {
        PrattToken *atom = next(parser);
        PROTECT(atom);
        // Compare atom symbol to the interned "operators" symbol
        if (isAtomSymbol(atom, TOK_OPERATORS())) {
            // Mark all local operator records as exported for each defined
            // fixity
            Index i = 0;
            HashSymbol *sym = NULL;
            PrattRecord *rec = NULL;
            while ((sym = iteratePrattRecordTable(parser->rules, &i, &rec)) !=
                   NULL) {
                if (rec->prefix.op)
                    rec->prefix.export = true;
                if (rec->infix.op)
                    rec->infix.export = true;
                if (rec->postfix.op)
                    rec->postfix.export = true;
            }
            res = newAstDefinition_Blank(TOKPI(tok));
        } else {
            parserErrorAt(TOKPI(atom), parser,
                          "expected 'operators' or a fixity after export");
            res = newAstDefinition_Blank(TOKPI(atom));
        }
    } else if (match(parser, TOK_OPERATOR())) {
        // for export operator, the syntax includes the definition of the
        // operator: export operator <pattern> [<associativity>] <precedence>
        // <implementation>; Parse the pattern string first
        PrattToken *opTok = peek(parser);
        PROTECT(opTok);
        PrattUnicode *str = rawString(parser);
        PROTECT(str);
        PrattMixfixPattern *pattern =
            parseMixfixPattern(TOKPI(opTok), parser, str);
        PROTECT(pattern);

        // Now parse the operator definition using the already-parsed pattern
        res = operatorWithPattern(parser, opTok, pattern);
        PROTECT(res);

        // Mark the operator as exported
        if (pattern != NULL) {
            HashSymbol *op = unicodeToSymbol(pattern->keywords->entries[0]);
            PrattRecord *rec = NULL;
            if (!getPrattRecordTable(parser->rules, op, &rec) || rec == NULL) {
                parserError(parser,
                            "cannot export non-local operator '%s' in pattern",
                            op->name);
            } else {
                // Determine which fixity to export based on the pattern
                PrattFixity fixity = getFixityFromPattern(pattern);
                switch (fixity) {
                case PRATTFIXITY_TYPE_PREFIX:
                    rec->prefix.export = true;
                    if (rec->prefix.pattern == NULL ||
                        !eqPrattStrings(pattern->keywords,
                                        rec->prefix.pattern->keywords)) {
                        parserError(parser,
                                    "cannot export non-local operator '%s' "
                                    "with different pattern",
                                    op->name);
                    }
                    break;
                case PRATTFIXITY_TYPE_INFIX:
                    rec->infix.export = true;
                    if (rec->infix.pattern == NULL ||
                        !eqPrattStrings(pattern->keywords,
                                        rec->infix.pattern->keywords)) {
                        parserError(parser,
                                    "cannot export non-local operator '%s' "
                                    "with different pattern",
                                    op->name);
                    }
                    break;
                case PRATTFIXITY_TYPE_POSTFIX:
                    rec->postfix.export = true;
                    if (rec->postfix.pattern == NULL ||
                        !eqPrattStrings(pattern->keywords,
                                        rec->postfix.pattern->keywords)) {
                        parserError(parser,
                                    "cannot export non-local operator '%s' "
                                    "with different pattern",
                                    op->name);
                    }
                    break;
                }
            }
        }
    } else {
        parserError(parser,
                    "expected 'operator', 'operators' or fixity after export");
        res = newAstDefinition_Blank(TOKPI(tok));
    }
    LEAVE(exportop);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Parse an import directive to import exported operators from a
 * nameSpace.
 *
 * Syntax:
 *   import <ns> operators;
 *   import <ns> prefix "op";
 *   import <ns> infix  "op";
 *   import <ns> postfix "op";
 */
static AstDefinition *importop(PrattParser *parser) {
    ENTER(importop);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    AstDefinition *res = NULL;
    HashSymbol *nsSymbol = symbol(parser);
    int nsRef = -1;
    if (!findNameSpace(parser, nsSymbol, &nsRef)) {
        parserErrorAt(TOKPI(tok), parser, "unknown nameSpace %s",
                      nsSymbol->name);
        res = newAstDefinition_Blank(TOKPI(tok));
        LEAVE(importop);
        UNPROTECT(save);
        return res;
    }
    PrattExportedOps *ops = NULL;
    if (nsOpsCache && nsRef >= 0 && nsRef < (int)nsOpsCache->size) {
        ops = nsOpsCache->entries[nsRef];
    }
    if (ops == NULL || ops->exportedRules == NULL) {
        parserErrorAt(TOKPI(tok), parser,
                      "nameSpace %s has no exported operators", nsSymbol->name);
        res = newAstDefinition_Blank(TOKPI(tok));
        LEAVE(importop);
        UNPROTECT(save);
        return res;
    }
    if (check(parser, TOK_ATOM())) {
        // Support: import <ns> operators;  (operators is an atom)
        PrattToken *atom = next(parser);
        PROTECT(atom);
        if (isAtomSymbol(atom, TOK_OPERATORS())) {
            // Import all exported operators
            Index i = 0;
            HashSymbol *op = NULL;
            PrattRecord *source = NULL;
            while ((op = iteratePrattRecordTable(ops->exportedRules, &i,
                                                 &source)) != NULL) {
                PrattRecord *target = ensureTargetRecord(parser, op);
                mergeFixityImport(parser, target, source, nsRef, nsSymbol,
                                  source->prefix.originalImpl != NULL,
                                  source->infix.originalImpl != NULL,
                                  source->postfix.originalImpl != NULL, op);
            }
            res = newAstDefinition_Blank(TOKPI(tok));
        } else {
            parserErrorAt(TOKPI(atom), parser,
                          "expected 'operators' or fixity after import <ns>");
            res = newAstDefinition_Blank(TOKPI(atom));
        }
    } else if (match(parser, TOK_OPERATOR())) {
        // import <ns> operator <pattern>;
        PrattUnicode *str = rawString(parser);
        PROTECT(str);
        PrattMixfixPattern *pattern =
            parseMixfixPattern(TOKPI(tok), parser, str);
        PROTECT(pattern);
        if (pattern == NULL) {
            res = newAstDefinition_Blank(TOKPI(tok));
        } else {
            HashSymbol *op = unicodeToSymbol(pattern->keywords->entries[0]);
            PrattRecord *source = NULL;
            if (!getPrattRecordTable(ops->exportedRules, op, &source) ||
                source == NULL) {
                parserErrorAt(
                    TOKPI(tok), parser,
                    "nameSpace %s did not export operator with pattern",
                    nsSymbol->name);
            } else {
                PrattFixity fixity = getFixityFromPattern(pattern);
                PrattMixfixPattern *sourcePattern =
                    fixity == PRATTFIXITY_TYPE_PREFIX ? source->prefix.pattern
                    : fixity == PRATTFIXITY_TYPE_INFIX
                        ? source->infix.pattern
                        : source->postfix.pattern;
                if (sourcePattern == NULL ||
                    !eqPrattStrings(pattern->keywords,
                                    sourcePattern->keywords)) {
                    parserErrorAt(TOKPI(tok), parser,
                                  "nameSpace %s did not export operator with "
                                  "matching pattern",
                                  nsSymbol->name);
                } else {
                    PrattRecord *target = ensureTargetRecord(parser, op);
                    mergeFixityImport(parser, target, source, nsRef, nsSymbol,
                                      source->prefix.originalImpl != NULL,
                                      source->infix.originalImpl != NULL,
                                      source->postfix.originalImpl != NULL, op);
                }
            }
            res = newAstDefinition_Blank(TOKPI(tok));
        }
    } else {
        parserErrorAt(TOKPI(tok), parser,
                      "expected 'operators' or fixity after import <ns>");
        res = newAstDefinition_Blank(TOKPI(tok));
    }
    LEAVE(importop);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Parse an alias declaration.
 *
 * This function parses an alias definition, which consists of an alias name,
 * an assignment operator, and a type. It creates an AstAlias and wraps it in
 * an AstDefinition.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstDefinition representing the alias declaration.
 */
static AstDefinition *alias(PrattParser *parser) {
    ENTER(alias);
    HashSymbol *s = symbol(parser);
    consume(parser, TOK_ASSIGN());
    AstType *t = type_type(parser);
    int save = PROTECT(t);
    AstDefinition *d = makeAstDefinition_Alias(CPI(t), s, t);
    LEAVE(alias);
    UNPROTECT(save);
    return d;
}

/**
 * @brief Parse a single type expression.
 * This function handles the parsing of a type expression,
 * which can be a nested type, a type clause, or a type function.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstType representing the parsed type expression, or NULL.
 */
static AstType *type_type(PrattParser *parser) {
    ENTER(type_type);
    AstType *type = NULL;
    int save = PROTECT(type);
    if (match(parser, TOK_OPEN())) {
        type = type_type(parser);
        PROTECT(type);
        consume(parser, TOK_CLOSE());
    } else {
        AstTypeClause *clause = type_clause(parser);
        PROTECT(clause);
        type = newAstType(CPI(clause), clause, NULL);
        PROTECT(type);
        if (match(parser, TOK_ARROW())) {
            type->next = type_type(parser);
        }
    }
    LEAVE(type_type);
    UNPROTECT(save);
    return type;
}

/**
 * @brief Parse a type clause, which can be a number, character, variable,
 * function, or tuple.
 *
 * This function checks the next token and parses it accordingly,
 * returning an AstTypeClause representing the parsed type.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeClause representing the parsed type clause, or NULL.
 */
static AstTypeClause *type_clause(PrattParser *parser) {
    ENTER(type_clause);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    AstTypeClause *ret = NULL;
    if (match(parser, TOK_KW_NUMBER())) {
        ret = newAstTypeClause_Integer(TOKPI(tok));
    } else if (match(parser, TOK_KW_CHAR())) {
        ret = newAstTypeClause_Character(TOKPI(tok));
    } else if (tok->type == TOK_HASH()) {
        HashSymbol *typeVar = type_variable(parser);
        ret = newAstTypeClause_Var(TOKPI(tok), typeVar);
    } else if (tok->type == TOK_ATOM()) {
        AstTypeFunction *fun = type_function(parser);
        PROTECT(fun);
        ret = newAstTypeClause_TypeFunction(CPI(fun), fun);
    } else if (tok->type == TOK_TUPLE()) {
        AstTypeList *lst = type_tuple(parser);
        PROTECT(lst);
        ret = newAstTypeClause_TypeTuple(CPI(lst), lst);
    } else {
        parserError(parser, "expected type clause");
        ret = newAstTypeClause_Integer(TOKPI(tok));
    }
    LEAVE(type_clause);
    UNPROTECT(save);
    return ret;
}

/**
 * @brief Parse a type variable, which is a hash symbol followed by a symbol.
 *
 * This function consumes the hash token and then parses the symbol,
 * returning an AstTypeVariable representing the type variable.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeVariable representing the parsed type variable.
 */
static HashSymbol *type_variable(PrattParser *parser) {
    ENTER(type_variable);
    HashSymbol *res = NULL;
    consume(parser, TOK_HASH());
    res = symbol(parser);
    LEAVE(type_variable);
    return res;
}

/**
 * @brief Parse a comma-separated list of types.
 *
 * This function recursively parses a list of types and constructs an
 * AstTypeList.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeList representing the parsed list of types.
 */
static AstTypeList *type_list(PrattParser *parser) {
    ENTER(type_list);
    AstType *type = type_type(parser);
    int save = PROTECT(type);
    AstTypeList *this = newAstTypeList(CPI(type), type, NULL);
    PROTECT(this);
    if (match(parser, TOK_COMMA())) {
        // Allow trailing comma: only continue if not at closing delimiter
        if (!check(parser, TOK_CLOSE()) && !check(parser, TOK_RCURLY())) {
            this->next = type_list(parser);
        }
    }
    LEAVE(type_list);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Parse a type map, which is a mapping of symbols to types.
 *
 * This function parses a sequence of symbol-type pairs, separated by commas,
 * and constructs an AstTypeMap representing the parsed mapping.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeMap representing the parsed type mapping.
 */
static AstTypeMap *type_map(PrattParser *parser) {
    ENTER(type_map);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    consume(parser, TOK_COLON());
    AstType *type = type_type(parser);
    PROTECT(type);
    AstTypeMap *this = newAstTypeMap(TOKPI(tok), s, type, NULL);
    PROTECT(this);
    if (match(parser, TOK_COMMA())) {
        // Allow trailing comma: only continue if not at closing curly
        if (!check(parser, TOK_RCURLY())) {
            this->next = type_map(parser);
        }
    }
    LEAVE(type_map);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Parse a single function definition within a composite function body.
 *
 * This function checks for an alt function definition, parses its arguments and
 * body, and constructs an AstAltFunction.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstAltFunction representing the parsed function definition.
 */
static AstAltFunction *alt_function(PrattParser *parser) {
    ENTER(alt_function);
    AstAltArgs *args = alt_args(parser);
    int save = PROTECT(args);
    AstNest *body = nest(parser);
    PROTECT(body);
    AstAltFunction *fun = newAstAltFunction(CPI(args), args, body);
    LEAVE(alt_function);
    UNPROTECT(save);
    return fun;
}

/**
 * @brief Parse a type function, which can have a name and an optional type
 * list.
 *
 * This function parses a scoped symbol as the name of the type function,
 * and if it matches an open parenthesis, it parses a list of types.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeFunction representing the parsed type function.
 */
static AstTypeFunction *type_function(PrattParser *parser) {
    ENTER(type_function);
    AstLookUpOrSymbol *name = scoped_symbol(parser);
    int save = PROTECT(name);
    AstTypeFunction *this = newAstTypeFunction(CPI(name), name, NULL);
    PROTECT(this);
    if (match(parser, TOK_OPEN())) {
        this->typeList = type_list(parser);
        consume(parser, TOK_CLOSE());
    }
    LEAVE(type_function);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Parse a tuple type, which is a sequence of types enclosed in
 * parentheses, preceeded by a hash.
 *
 * This function consumes the hash and opening parenthesis, parses a list of
 * types, and then consumes the closing parenthesis, returning an AstTypeList.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeList representing the parsed tuple type.
 */
static AstTypeList *type_tuple(PrattParser *parser) {
    ENTER(type_tuple);
    consume(parser, TOK_TUPLE());
    AstTypeList *body = type_list(parser);
    int save = PROTECT(body);
    consume(parser, TOK_CLOSE());
    LEAVE(type_tuple);
    UNPROTECT(save);
    return body;
}

/**
 * @brief Parse alternative arguments for a function, separated by pipes.
 *
 * This function recursively parses a list of function arguments, allowing for
 * multiple sets of arguments separated by the pipe token.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstAltArgs representing the parsed alternative arguments.
 */
static AstAltArgs *alt_args(PrattParser *parser) {
    ENTER(alt_args);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    AstFargList *args = fargs(parser);
    PROTECT(args);
    AstAltArgs *this = newAstAltArgs(TOKPI(tok), args, NULL);
    PROTECT(this);
    if (match(parser, TOK_PIPE())) {
        this->next = alt_args(parser);
    }
    LEAVE(alt_args);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Parse a nested block of code, which can contain definitions and
 * statements.
 *
 * This function consumes the opening curly brace, creates a child parser,
 * and parses the body of the nest until the closing curly brace is found.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstNest representing the parsed nested block.
 */
static AstNest *nest(PrattParser *parser) {
    ENTER(nest);
    consume(parser, TOK_LCURLY());
    PrattParser *child = makeChildParser(parser);
    int save = PROTECT(child);
    AstNest *body = nest_body(child, TOK_RCURLY());
    PROTECT(body);
    consume(parser, TOK_RCURLY());
    LEAVE(nest);
    UNPROTECT(save);
    return body;
}

/**
 * @brief Parse the body of a nested block, which can contain definitions or
 * statements.
 *
 * This function creates a child parser and calls nest_body to parse the body.
 * then it consumes the closing curly brace.
 *
 * It differs from nest() above in that it does not consume the opening curly
 * brace, and it returns an AstExpression rather than a bare AstNest. Its
 * signature also matches the PrattParser's nestexpr function as it is called
 * directly by the parser.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstExpression representing the parsed body of the nest.
 */
static AstExpression *nestexpr(PrattRecord *record __attribute__((unused)),
                               PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused))) {
    ENTER(nestexpr);
    PrattParser *child = makeChildParser(parser);
    int save = PROTECT(child);
    AstNest *body = nest_body(child, TOK_RCURLY());
    PROTECT(body);
    consume(parser, TOK_RCURLY());
    AstExpression *res = newAstExpression_Nest(CPI(body), body);
    LEAVE(nestexpr);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Parse a scoped symbol, that is a symbol optionally prefixed by a
 * nameSpace and a period.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstLookUpOrSymbol representing the parsed scoped symbol.
 */
static AstLookUpOrSymbol *scoped_symbol(PrattParser *parser) {
    ENTER(scoped_symbol);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *sym1 = symbol(parser);
    if (match(parser, TOK_PERIOD())) {
        HashSymbol *sym2 = symbol(parser);
        int index = 0;
        if (findNameSpace(parser, sym1, &index)) {
            AstLookUpOrSymbol *res =
                makeAstLookUpOrSymbol_LookUp(TOKPI(tok), index, sym1, sym2);
            LEAVE(scoped_symbol);
            UNPROTECT(save);
            return res;
        } else {
            parserError(parser, "cannot resolve nameSpace %s", sym1->name);
        }
    }
    AstLookUpOrSymbol *res = newAstLookUpOrSymbol_Symbol(TOKPI(tok), sym1);
    LEAVE(scoped_symbol);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Parse a list of formal arguments for a function.
 *
 * This function consumes the opening parenthesis, parses the expressions,
 * and converts them into an AstFargList. It also consumes the closing
 * parenthesis.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstFargList representing the parsed formal arguments.
 */
static AstFargList *fargs(PrattParser *parser) {
    ENTER(fargs);
    consume(parser, TOK_OPEN());
    AstExpressions *exprs = expressions(parser);
    int save = PROTECT(exprs);
    consume(parser, TOK_CLOSE());
    AstFargList *args = astExpressionsToFargList(parser, exprs);
    LEAVE(fargs);
    UNPROTECT(save);
    return args;
}

/**
 * @brief Convert an AstLookUp to an AstLookUpSymbol.
 *
 * @param parser The PrattParser (used only for error reporting).
 * @param lookUp The AstLookUp to convert.
 * @return An AstLookUpSymbol representing the converted lookUp.
 */
static AstLookUpSymbol *astLookUpToLus(PrattParser *parser, AstLookUp *lookUp) {
    AstLookUpSymbol *lus =
        newAstLookUpSymbol(CPI(lookUp), lookUp->nsId, lookUp->nsSymbol, NULL);
    int save = PROTECT(lus);
    switch (lookUp->expression->type) {
    case AST_EXPRESSION_TYPE_SYMBOL:
        lus->symbol = lookUp->expression->val.symbol;
        break;
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL:
        parserErrorAt(CPI(lookUp), parser, "invalid use of operator in lookUp");
        lus->symbol = lookUp->nsSymbol;
        break;
    default:
        parserErrorAt(CPI(lookUp), parser,
                      "invalid lookUp in formal arguments");
        lus->symbol = lookUp->nsSymbol;
        break;
    }
    UNPROTECT(save);
    return lus;
}

/**
 * @brief Wraps an AstLookUp in an AstLookUpSymbol and wraps that in an
 * AstLookUpOrSymbol.
 *
 * @param parser The PrattParser (used only for error reporting).
 * @param lookUp The AstLookUp to convert.
 * @return An AstLookUpOrSymbol representing the converted lookUp.
 */
static AstLookUpOrSymbol *astLookUpToLos(PrattParser *parser,
                                         AstLookUp *lookUp) {
    AstLookUpSymbol *lus = astLookUpToLus(parser, lookUp);
    int save = PROTECT(lus);
    AstLookUpOrSymbol *res = newAstLookUpOrSymbol_LookUp(CPI(lus), lus);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Create an AstLookUpOrSymbol representing an error.
 *
 * This function creates a new AstLookUpOrSymbol with a TOK_ERROR() symbol,
 * this is only so that the parser has something to return when it encounters an
 * error.
 *
 * @param PI The ParserInfo for the context of the error.
 * @return An AstLookUpOrSymbol representing the error.
 */
static AstLookUpOrSymbol *makeLosError(ParserInfo PI) {
    return newAstLookUpOrSymbol_Symbol(PI, TOK_ERROR());
}

/**
 * @brief Wrals a symbol in a new AstLookUpOrSymbol.
 *
 * @param PI The ParserInfo for the context of the symbol.
 * @param symbol The HashSymbol to wrap.
 * @return An AstLookUpOrSymbol containing the symbol.
 */
static AstLookUpOrSymbol *astSymbolToLos(ParserInfo PI, HashSymbol *symbol) {
    return newAstLookUpOrSymbol_Symbol(PI, symbol);
}

/**
 * @brief Convert an AstExpression representing a function to an
 * AstLookUpOrSymbol.
 *
 * This function checks the type of the expression and converts it to an
 * AstLookUpOrSymbol or returns an error if the expression is not a lookUp or a
 * symbol. It is used to parse type constructor names in the formal arguments to
 * functions.
 *
 * @param parser The PrattParser (used only for error reporting).
 * @param function The AstExpression representing the function.
 * @return An AstLookUpOrSymbol representing the function, or an error.
 */
static AstLookUpOrSymbol *astFunctionToLos(PrattParser *parser,
                                           AstExpression *function) {
    switch (function->type) {
    case AST_EXPRESSION_TYPE_LOOKUP:
        return astLookUpToLos(parser, function->val.lookUp);
    case AST_EXPRESSION_TYPE_SYMBOL:
        return astSymbolToLos(CPI(function), function->val.symbol);
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL:
        // For annotated symbols (hygienic operators), extract the original
        // implementation This allows operators like @ (cons) to be used in
        // patterns
        if (function->val.annotatedSymbol->originalImpl->type ==
            AST_EXPRESSION_TYPE_SYMBOL) {
            return astSymbolToLos(
                CPI(function),
                function->val.annotatedSymbol->originalImpl->val.symbol);
        } else {
            parserErrorAt(CPI(function), parser,
                          "invalid use of operator as structure name");
            return makeLosError(CPI(function));
        }
    case AST_EXPRESSION_TYPE_BACK:
        parserErrorAt(CPI(function), parser,
                      "invalid use of \"back\" as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_FUNCALL:
        parserErrorAt(CPI(function), parser,
                      "invalid use of function call as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_NUMBER:
        parserErrorAt(CPI(function), parser,
                      "invalid use of number as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_CHARACTER:
        parserErrorAt(CPI(function), parser,
                      "invalid use of character as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_FUN:
        parserErrorAt(CPI(function), parser,
                      "invalid use of function as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_NEST:
        parserErrorAt(CPI(function), parser,
                      "invalid use of nest as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_IFF:
        parserErrorAt(CPI(function), parser,
                      "invalid use of conditional as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_PRINT:
        parserErrorAt(CPI(function), parser,
                      "invalid use of \"print\" as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_TUPLE:
        parserErrorAt(CPI(function), parser,
                      "invalid use of tuple as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_ENV:
        parserErrorAt(CPI(function), parser,
                      "encountered ENV as formal argument");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_STRUCTURE:
        parserErrorAt(CPI(function), parser,
                      "invalid use of tuple as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_ASSERTION:
        parserErrorAt(CPI(function), parser,
                      "invalid use of \"assert\" as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_ALIAS:
        parserErrorAt(CPI(function), parser,
                      "invalid use of alias as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_WILDCARD:
        parserErrorAt(CPI(function), parser,
                      "invalid use of wildCard as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_ERROR:
        parserErrorAt(CPI(function), parser,
                      "invalid use of \"error\" as structure name");
        return makeLosError(CPI(function));
    default:
        cant_happen("unrecognised %s", astExpressionTypeName(function->type));
    }
}

/**
 * @brief Converts a function call (type-constructor) to a formal argument.
 *
 * @param parser The parser for error reporting
 * @param funCall the function cal to convert
 * @return The formal argument AstFarg
 */

static AstFarg *astFunCallToFarg(PrattParser *parser, AstFunCall *funCall) {
    AstLookUpOrSymbol *los = astFunctionToLos(parser, funCall->function);
    int save = PROTECT(los);
    AstFargList *args = astExpressionsToFargList(parser, funCall->arguments);
    PROTECT(args);
    AstFarg *res = makeAstFarg_Unpack(CPI(los), los, args);
    UNPROTECT(save);
    return res;
}

/**
 * @brief wraps an AstLookUp in an AstLookUpSymbol and wraps that in an AstFarg
 *
 * @param parser The parser for error reporting.
 * @param lookUp the AstLookUp to wrap.
 */
static AstFarg *astLookUpToFarg(PrattParser *parser, AstLookUp *lookUp) {
    AstLookUpSymbol *lus = astLookUpToLus(parser, lookUp);
    int save = PROTECT(lus);
    AstFarg *res = newAstFarg_LookUp(CPI(lus), lus);
    UNPROTECT(save);
    return res;
}

/**
 * @brief creates a Formal Argument from a Symbol.
 *
 * @param PI parser info for the Farg.
 * @param symbol the symbol to be wrapped.
 * @return an AstFarg representing the formal argument.
 */
static AstFarg *astSymbolToFarg(ParserInfo PI, HashSymbol *symbol) {
    return newAstFarg_Symbol(PI, symbol);
}

/**
 * @brief converts a number to a formal argument.
 */
static AstFarg *astNumberToFarg(ParserInfo PI, MaybeBigInt *bi) {
    return newAstFarg_Number(PI, bi);
}

/**
 * @brief converts a character to a formal argument.
 */
static AstFarg *astCharacterToFarg(ParserInfo PI, Character c) {
    return newAstFarg_Character(PI, c);
}

/**
 * @brief converts a list of expressions to a list of formal arguments.
 */
static AstFargList *astExpressionsToFargList(PrattParser *parser,
                                             AstExpressions *exprs) {
    if (exprs == NULL)
        return NULL;
    AstFargList *next = astExpressionsToFargList(parser, exprs->next);
    int save = PROTECT(next);
    AstFarg *arg = astExpressionToFarg(parser, exprs->expression);
    PROTECT(arg);
    AstFargList *this = newAstFargList(CPI(arg), arg, next);
    UNPROTECT(save);
    return this;
}

/**
 * @brief converts a list of expressions to a single tuple formal argument.
 */
static AstFarg *astTupleToFarg(PrattParser *parser, AstExpressions *tuple) {
    AstFargList *args = astExpressionsToFargList(parser, tuple);
    int save = PROTECT(args);
    AstFarg *res = newAstFarg_Tuple(CPI(tuple), args);
    UNPROTECT(save);
    return res;
}

/**
 * @brief converts a map of expressions to a map of formal arguments.
 */
static AstTaggedArgList *
astTaggedExpressionsToTaggedFargList(PrattParser *parser,
                                     AstTaggedExpressions *exprs) {
    if (exprs == NULL)
        return NULL;
    AstTaggedArgList *next =
        astTaggedExpressionsToTaggedFargList(parser, exprs->next);
    int save = PROTECT(next);
    AstFarg *arg = astExpressionToFarg(parser, exprs->expression);
    PROTECT(arg);
    AstTaggedArgList *this =
        newAstTaggedArgList(CPI(arg), exprs->tag, arg, next);
    UNPROTECT(save);
    return this;
}

/**
 * @brief converts a structure containing a map of expressions to a formal
 * argument.
 */
static AstFarg *astStructureToFarg(PrattParser *parser, AstStruct *structure) {
    AstTaggedArgList *args =
        astTaggedExpressionsToTaggedFargList(parser, structure->expressions);
    int save = PROTECT(args);
    AstFarg *res =
        makeAstFarg_UnpackStruct(CPI(structure), structure->symbol, args);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Converts an aliased expression to a named formal argument.
 */
static AstFarg *astAliasToFarg(PrattParser *parser, AstExprAlias *alias) {
    AstFarg *farg = astExpressionToFarg(parser, alias->value);
    int save = PROTECT(farg);
    AstFarg *res = makeAstFarg_Named(CPI(alias), alias->name, farg);
    UNPROTECT(save);
    return res;
}

/**
 * @brief convert an expression to a formal argument.
 */
static AstFarg *astExpressionToFarg(PrattParser *parser, AstExpression *expr) {
    switch (expr->type) {
    case AST_EXPRESSION_TYPE_BACK:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of \"back\" as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_FUNCALL:
        return astFunCallToFarg(parser, expr->val.funCall);
    case AST_EXPRESSION_TYPE_LOOKUP:
        return astLookUpToFarg(parser, expr->val.lookUp);
    case AST_EXPRESSION_TYPE_SYMBOL:
        return astSymbolToFarg(CPI(expr), expr->val.symbol);
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of operator as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_NUMBER:
        return astNumberToFarg(CPI(expr), expr->val.number);
    case AST_EXPRESSION_TYPE_CHARACTER:
        return astCharacterToFarg(CPI(expr), expr->val.character);
    case AST_EXPRESSION_TYPE_FUN:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of function as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_NEST:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of nest as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_IFF:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of conditional as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_PRINT:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of \"print\" as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_TUPLE:
        return astTupleToFarg(parser, expr->val.tuple);
    case AST_EXPRESSION_TYPE_ENV:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of ENV as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_STRUCTURE:
        return astStructureToFarg(parser, expr->val.structure);
    case AST_EXPRESSION_TYPE_ASSERTION:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of \"assert\" as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_ALIAS:
        return astAliasToFarg(parser, expr->val.alias);
    case AST_EXPRESSION_TYPE_WILDCARD:
        return newAstFarg_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_ERROR:
        parserErrorAt(CPI(expr), parser,
                      "invalid use of \"error\" as formal argument");
        return newAstFarg_WildCard(CPI(expr));
    default:
        cant_happen("unrecognised %s", astExpressionTypeName(expr->type));
    }
}

/**
 * @brief validate that the macro arguments are conforming (symbols only, and no
 * alternative args)
 */
static void validateMacroArgs(PrattParser *parser, AstAltFunction *definition) {
    AstAltArgs *altArgs = definition->altArgs;
    if (altArgs->next) {
        parserErrorAt(CPI(altArgs->next), parser,
                      "cannot supply alternative arguments to a macro");
    } else {
        AstFargList *args = altArgs->argList;
        while (args) {
            if (args->arg->type != AST_FARG_TYPE_SYMBOL) {
                parserErrorAt(CPI(args->arg), parser,
                              "macro arguments can only be simple symbols");
                break;
            }
            args = args->next;
        }
    }
}

/**
 * @brief parse a macro definition.
 *
 * the `macro` token has already been consumed when this function triggers.
 */
static AstDefinition *defmacro(PrattParser *parser) {
    ENTER(defmacro);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstAltFunction *definition = alt_function(parser);
    PROTECT(definition);
    validateMacroArgs(parser, definition);
    AstDefinition *res = makeAstDefinition_Macro(TOKPI(tok), s, definition);
    LEAVE(defmacro);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parse a function definition
 *
 * The `fn` token has already been consumed when this function is triggered.
 */
static AstDefinition *defun(PrattParser *parser, bool unsafe, bool isPrinter) {
    ENTER(defun);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstCompositeFunction *f = composite_function(parser);
    f->unsafe = unsafe;
    PROTECT(f);
    if (isPrinter) {
        s = makePrintName("print$", s->name);
    }
    AstExpression *expr = newAstExpression_Fun(CPI(f), f);
    PROTECT(expr);
    AstDefinition *res = makeAstDefinition_Define(TOKPI(tok), s, expr);
    LEAVE(defun);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parse a symbol
 */
static HashSymbol *symbol(PrattParser *parser) {
    ENTER(symbol);
    PrattToken *tok = next(parser);
    validateLastAlloc();
    int save = PROTECT(tok);
    if (tok->type != TOK_ATOM()) {
        parserError(parser, "expected ATOM, got %s", tok->type->name);
        LEAVE(symbol);
        UNPROTECT(save);
        return TOK_ERROR();
    }
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_ATOM) {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    HashSymbol *s = tok->value->val.atom;
    DEBUG("symbol: %s", s->name);
    LEAVE(symbol);
    UNPROTECT(save);
    return s;
}

/**
 * @brief parses an assignment (definition)
 */
static AstDefinition *assignment(PrattParser *parser) {
    ENTER(assignment);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    consume(parser, TOK_ASSIGN());
    AstExpression *expr = expression(parser);
    PROTECT(expr);
    AstDefinition *res = makeAstDefinition_Define(TOKPI(tok), s, expr);
    LEAVE(assignment);
    UNPROTECT(save);
    return res;
}

static AstSymbolList *symbol_list(PrattParser *parser) {
    ENTER(symbol_list);
    if (match(parser, TOK_CLOSE())) {
        LEAVE(symbol_list);
        return NULL;
    }
    PrattToken *symbol = next(parser);
    int save = PROTECT(symbol);
    HashSymbol *s = NULL;
    if (symbol->type == TOK_ATOM()) {
        s = symbol->value->val.atom;
    } else {
        parserError(parser, "expected ATOM, got %s", symbol->type->name);
        s = TOK_ERROR();
    }
    AstSymbolList *this = NULL;
    if (match(parser, TOK_COMMA())) {
        AstSymbolList *rest = symbol_list(parser);
        PROTECT(rest);
        this = newAstSymbolList(TOKPI(symbol), s, rest);
    } else {
        consume(parser, TOK_CLOSE());
        this = newAstSymbolList(TOKPI(symbol), s, NULL);
    }
    LEAVE(symbol_list);
    UNPROTECT(save);
    return this;
}

/**
 * @brief parses a multi-definition
 */
static AstDefinition *multidefinition(PrattParser *parser) {
    ENTER(multidefinition);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    AstSymbolList *symbols = symbol_list(parser);
    PROTECT(symbols);
    consume(parser, TOK_ASSIGN());
    AstExpression *expr = expression(parser);
    PROTECT(expr);
    AstDefinition *res = makeAstDefinition_Multi(TOKPI(tok), symbols, expr);
    LEAVE(multidefinition);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a typedef
 */
static AstDefinition *typedefinition(PrattParser *parser) {
    ENTER(typedefinition);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstTypeSig *typeSig = NULL;
    if (check(parser, TOK_OPEN())) {
        next(parser);
        validateLastAlloc();
        AstTypeSymbols *variables = type_variables(parser);
        PROTECT(variables);
        consume(parser, TOK_CLOSE());
        typeSig = newAstTypeSig(TOKPI(tok), s, variables);
    } else {
        typeSig = newAstTypeSig(TOKPI(tok), s, NULL);
    }
    PROTECT(typeSig);
    consume(parser, TOK_LCURLY());
    AstTypeBody *typeBody = type_body(parser);
    PROTECT(typeBody);
    consume(parser, TOK_RCURLY());
    AstDefinition *res =
        makeAstDefinition_TypeDef(CPI(typeSig), typeSig, typeBody);
    LEAVE(typedefinition);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses the body of a typedef
 */
static AstTypeBody *type_body(PrattParser *parser) {
    ENTER(type_body);
    AstTypeConstructor *tc = type_constructor(parser);
    int save = PROTECT(tc);
    AstTypeBody *this = NULL;
    if (match(parser, TOK_PIPE())) {
        AstTypeBody *rest = type_body(parser);
        PROTECT(rest);
        this = newAstTypeBody(CPI(tc), tc, rest);
    } else {
        this = newAstTypeBody(CPI(tc), tc, NULL);
    }
    LEAVE(type_body);
    UNPROTECT(save);
    return this;
}

/**
 * @brief parses a type constructor within the body of a typedef
 */
static AstTypeConstructor *type_constructor(PrattParser *parser) {
    ENTER(type_constructor);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstTypeConstructorArgs *args = NULL;
    if (match(parser, TOK_OPEN())) {
        AstTypeList *typeList = type_list(parser);
        PROTECT(typeList);
        consume(parser, TOK_CLOSE());
        args = newAstTypeConstructorArgs_List(CPI(typeList), typeList);
        PROTECT(args);
    } else if (match(parser, TOK_LCURLY())) {
        AstTypeMap *typeMap = type_map(parser);
        PROTECT(typeMap);
        consume(parser, TOK_RCURLY());
        args = newAstTypeConstructorArgs_Map(CPI(typeMap), typeMap);
        PROTECT(args);
    }
    AstTypeConstructor *res = newAstTypeConstructor(TOKPI(tok), s, args);
    LEAVE(type_constructor);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses the type variable arguments to the type signature of a typedef
 */
static AstTypeSymbols *type_variables(PrattParser *parser) {
    ENTER(type_variables);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = type_variable(parser);
    AstTypeSymbols *t = NULL;
    if (check(parser, TOK_CLOSE())) {
        t = newAstTypeSymbols(TOKPI(tok), s, NULL);
    } else {
        consume(parser, TOK_COMMA());
        // Allow trailing comma: only continue if not at closing paren
        if (!check(parser, TOK_CLOSE())) {
            AstTypeSymbols *rest = type_variables(parser);
            PROTECT(rest);
            t = newAstTypeSymbols(TOKPI(tok), s, rest);
        } else {
            t = newAstTypeSymbols(TOKPI(tok), s, NULL);
        }
    }
    LEAVE(type_variables);
    UNPROTECT(save);
    return t;
}

/**
 * @brief parses a link (nameSpace import) directive.
 */
static AstDefinition *link(PrattParser *parser) {
    ENTER(link);
    PrattUnicode *path = rawString(parser);
    int save = PROTECT(path);
    AstDefinition *res = NULL;
    if (path == NULL) {
        res = newAstDefinition_Blank(LEXPI(parser->lexer));
    } else {
        consume(parser, TOK_AS());
        HashSymbol *name = symbol(parser);
        // Convert wide character path to multibyte
        size_t len = wcstombs(NULL, path->entries, 0);
        PrattCVec *mbPath = newPrattCVec(len + 1);
        PROTECT(mbPath);
        wcstombs(mbPath->entries, path->entries, len + 1);
        AstNameSpace *ns =
            parseLink(parser, (unsigned char *)mbPath->entries, name);
        PROTECT(ns);
        storeNameSpace(parser, ns);
        res = newAstDefinition_Blank(CPI(ns));
    }
    LEAVE(link);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a raw double-quoted string for a link directive.
 */
static PrattUnicode *rawString(PrattParser *parser) {
    ENTER(rawString);
    PrattToken *tok = next(parser);
    validateLastAlloc();
    if (tok->type == TOK_STRING()) {
#ifdef SAFETY_CHECKS
        if (tok->value->type != PRATTVALUE_TYPE_STRING) {
            cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
        }
#endif
        LEAVE(rawString);
        return tok->value->val.string;
    } else {
        parserError(parser, "expected string, got %s", tok->type->name);
        PrattUnicode *err = newPrattUnicode();
        int save = PROTECT(err);
        pushPrattUnicode(err, 0);
        LEAVE(rawString);
        UNPROTECT(save);
        return err;
    }
}

/**
 * @brief parses a subsequent string, appending it to the current.
 */
static void appendString(PrattParser *parser, PrattUnicode *this) {
    ENTER(appendString);
    PrattUnicode *next = rawString(parser);
    int save = PROTECT(next);
    this->size--; // backup over '\0'
    for (Index i = 0; i < next->size; i++) {
        pushPrattUnicode(this, next->entries[i]);
    }
    UNPROTECT(save);
    if (check(parser, TOK_STRING())) {
        appendString(parser, this);
    }
    LEAVE(appendString);
}

/**
 * @brief parses any sequence of adjacent strings into a single string.
 */
static PrattUnicode *str(PrattParser *parser) {
    ENTER(str);
    PrattUnicode *this = rawString(parser);
    int save = PROTECT(this);
    if (check(parser, TOK_STRING())) {
        appendString(parser, this);
    }
    LEAVE(str);
    UNPROTECT(save);
    return this;
}

/**
 * @brief parses a composite (or simple) function body
 */
static AstCompositeFunction *composite_function(PrattParser *parser) {
    ENTER(composite_function);
    AstCompositeFunction *res = NULL;
    int save = PROTECT(res);
    if (match(parser, TOK_LCURLY())) {
        res = functions(parser);
        PROTECT(res);
        consume(parser, TOK_RCURLY());
    } else {
        AstAltFunction *f = alt_function(parser);
        PROTECT(f);
        res = makeAstCompositeFunction(f, NULL);
    }
    LEAVE(composite_function);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a list of functions from a composite function body.
 */
static AstCompositeFunction *functions(PrattParser *parser) {
    ENTER(functions);
    AstAltFunction *f = alt_function(parser);
    int save = PROTECT(f);
    AstCompositeFunction *rest = NULL;
    if (check(parser, TOK_OPEN())) {
        rest = functions(parser);
        PROTECT(rest);
    }
    AstCompositeFunction *this = makeAstCompositeFunction(f, rest);
    LEAVE(functions);
    UNPROTECT(save);
    return this;
}

/**
 * @brief find a parser record for a token in a (nested) PrattParser.
 */
static PrattRecord *fetchRecord(PrattParser *parser, HashSymbol *symbol) {
    PrattRecord *record = NULL;
    if (getPrattRecordTable(parser->rules, symbol, &record)) {
        return record;
    } else if (parser->next != NULL) {
        return fetchRecord(parser->next, symbol);
    } else {
        return NULL;
    }
}

/**
 * @brief triggered by an open round bracket, parses an expression up to the
 * closing bracket.
 */
static AstExpression *grouping(PrattRecord *record, PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused))) {
    ENTER(grouping);
    AstExpression *res = expressionPrecedence(parser, record->prefix.prec);
    int save = PROTECT(res);
    consume(parser, TOK_CLOSE());
    LEAVE(grouping);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a comma separated list of expressions up to a closing square
 * bracket.
 * @return a nested chain of cons function applications.
 */
static AstFunCall *conslist(PrattParser *parser) {
    ENTER(conslist);
    AstFunCall *res = NULL;
    int save = PROTECT(res);
    if (check(parser, TOK_RSQUARE())) {
        ParserInfo PI = LEXPI(parser->lexer);
        DEBUG("conslist parser info %d %s", PI.lineNo, PI.fileName);
        AstExpression *nil = newAstExpression_Symbol(PI, nilSymbol());
        PROTECT(nil);
        res = newAstFunCall(PI, nil, NULL);
    } else if (check(parser, TOK_EOF())) {
        parserError(parser, "unexpected EOF");
        UNPROTECT(save);
        return NULL;
    } else {
        AstExpression *expr = expression(parser);
        PROTECT(expr);
        match(parser, TOK_COMMA());
        AstFunCall *rest = conslist(parser);
        if (rest == NULL) {
            UNPROTECT(save);
            return NULL;
        }
        PROTECT(rest);
        AstExpression *fc = newAstExpression_FunCall(CPI(rest), rest);
        PROTECT(fc);
        AstExpression *cons = newAstExpression_Symbol(CPI(fc), consSymbol());
        PROTECT(cons);
        AstExpressions *args = newAstExpressions(CPI(fc), fc, NULL);
        PROTECT(args);
        args = newAstExpressions(CPI(expr), expr, args);
        PROTECT(args);
        res = newAstFunCall(CPI(expr), cons, args);
    }
    LEAVE(conslist);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a list in square brackets to a nested list of calls to cons.
 */
static AstExpression *list(PrattRecord *record __attribute__((unused)),
                           PrattParser *parser,
                           AstExpression *lhs __attribute__((unused)),
                           PrattToken *tok __attribute__((unused))) {
    ENTER(list);
    AstFunCall *conses = conslist(parser);
    int save = PROTECT(conses);
    consume(parser, TOK_RSQUARE());
    AstExpression *res = newAstExpression_FunCall(CPI(conses), conses);
    LEAVE(list);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet that parses the expressiomn following a prefix operator
 * and creates a unary operation node.
 */
static AstExpression *doPrefix(PrattRecord *record, PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused))) {
    ENTER(doPrefix);
    AstExpression *res = expressionPrecedence(parser, record->prefix.prec + 1);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), record->symbol, res);
    LEAVE(doPrefix);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parse the non-empty actual arguments to a function application.
 *
 * The open round brace has already been consumed.
 */
static AstExpressions *collectArguments(PrattParser *parser) {
    ENTER(collectArguments);
    AstExpression *arg = expressionPrecedence(parser, 0);
    int save = PROTECT(arg);
    AstExpressions *next = NULL;
    if (match(parser, TOK_COMMA())) {
        // Allow trailing comma: only continue if there's more content
        if (!check(parser, TOK_CLOSE())) {
            next = collectArguments(parser);
            PROTECT(next);
        }
    }
    AstExpressions *this = newAstExpressions(CPI(arg), arg, next);
    LEAVE(collectArguments);
    UNPROTECT(save);
    return this;
}

/**
 * @brief parse the potentially empty list of arguments to a function
 * application.
 */
static AstExpressions *expressions(PrattParser *parser) {
    ENTER(expressions);
    AstExpressions *args = NULL;
    int save = PROTECT(args);
    if (!check(parser, TOK_CLOSE())) {
        args = collectArguments(parser);
        PROTECT(args);
    }
    LEAVE(expressions);
    UNPROTECT(save);
    return args;
}

/**
 * @brief triggered by an infix open round brace (function application), parses
 * the arguments and creates the funcall.
 */
static AstExpression *call(PrattRecord *record __attribute__((unused)),
                           PrattParser *parser, AstExpression *lhs,
                           PrattToken *tok __attribute__((unused))) {
    ENTER(call);
    AstExpressions *args = expressions(parser);
    int save = PROTECT(args);
    consume(parser, TOK_CLOSE());
    AstExpression *res = makeAstExpression_FunCall(CPI(lhs), lhs, args);
    LEAVE(call);
    UNPROTECT(save);
    return res;
}

static AstTaggedExpressions *taggedExpressions(PrattParser *parser) {
    ENTER(taggedExpressions);
    PrattToken *tag = peek(parser);
    int save = PROTECT(tag);
    HashSymbol *tagSymbol = symbol(parser);
    consume(parser, TOK_COLON());
    AstExpression *expr = expressionPrecedence(parser, 0);
    PROTECT(expr);
    AstTaggedExpressions *next = NULL;
    if (match(parser, TOK_COMMA())) {
        // Allow trailing comma: only continue if there's more content
        if (!check(parser, TOK_RCURLY())) {
            next = taggedExpressions(parser);
            PROTECT(next);
        }
    }
    AstTaggedExpressions *this =
        newAstTaggedExpressions(CPI(expr), tagSymbol, expr, next);
    LEAVE(taggedExpressions);
    UNPROTECT(save);
    return this;
}

static AstLookUpSymbol *astStructLookUpToLus(PrattParser *parser,
                                             AstLookUp *lookUp) {
    if (lookUp->expression->type != AST_EXPRESSION_TYPE_SYMBOL) {
        parserErrorAt(CPI(lookUp->expression), parser,
                      "expected symbol as lookUp expression, got %s",
                      astExpressionTypeName(lookUp->expression->type));
        return newAstLookUpSymbol(CPI(lookUp), -1, TOK_ERROR(), TOK_ERROR());
    }
    int index = 0;
    if (findNameSpace(parser, lookUp->nsSymbol, &index)) {
        AstLookUpSymbol *lus =
            newAstLookUpSymbol(CPI(lookUp), index, lookUp->nsSymbol,
                               lookUp->expression->val.symbol);
        return lus;
    } else {
        parserErrorAt(CPI(lookUp), parser, "unknown nameSpace '%s' in lookUp",
                      lookUp->nsSymbol->name);
        return newAstLookUpSymbol(CPI(lookUp), -1, lookUp->nsSymbol,
                                  TOK_ERROR());
    }
}

static AstLookUpOrSymbol *astExpressionToLosOrSymbol(PrattParser *parser,
                                                     AstExpression *expr) {
    switch (expr->type) {
    case AST_EXPRESSION_TYPE_LOOKUP: {
        AstLookUpSymbol *lus = astStructLookUpToLus(parser, expr->val.lookUp);
        int save = PROTECT(lus);
        AstLookUpOrSymbol *res = newAstLookUpOrSymbol_LookUp(CPI(expr), lus);
        UNPROTECT(save);
        return res;
    }
    case AST_EXPRESSION_TYPE_SYMBOL:
        return newAstLookUpOrSymbol_Symbol(CPI(expr), expr->val.symbol);
    default:
        parserErrorAt(
            CPI(expr), parser,
            "expected structure name (symbol or lookUp) on lhs of infix "
            "'{', got %s",
            astExpressionTypeName(expr->type));
        return newAstLookUpOrSymbol_Symbol(CPI(expr), TOK_ERROR());
    }
}

/**
 * @brief parselet triggered by an infix `{` token, parses a structure
 * expression.
 */
static AstExpression *makeStruct(PrattRecord *record __attribute__((unused)),
                                 PrattParser *parser, AstExpression *lhs,
                                 PrattToken *tok __attribute__((unused))) {
    ENTER(makeStruct);
    AstLookUpOrSymbol *los = astExpressionToLosOrSymbol(parser, lhs);
    int save = PROTECT(los);
    AstTaggedExpressions *fields = taggedExpressions(parser);
    PROTECT(fields);
    ;
    consume(parser, TOK_RCURLY());
    AstExpression *res = makeAstExpression_Structure(CPI(lhs), los, fields);
    LEAVE(makeStruct);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a switch statement into an anonymous function definition and
 * application
 *
 * This code is used by both safe and unsafe switch statement parsers.
 */
static AstFunCall *switchFC(PrattParser *parser) {
    ENTER(switchFC);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    consume(parser, TOK_OPEN());
    AstExpressions *args = expressions(parser);
    PROTECT(args);
    consume(parser, TOK_CLOSE());
    AstCompositeFunction *body = composite_function(parser);
    PROTECT(body);
    AstExpression *fun = newAstExpression_Fun(TOKPI(tok), body);
    PROTECT(fun);
    AstFunCall *res = newAstFunCall(TOKPI(tok), fun, args);
    LEAVE(switchFC);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `switch` token.
 */
static AstExpression *switchExp(PrattRecord *record __attribute__((unused)),
                                PrattParser *parser,
                                AstExpression *lhs __attribute__((unused)),
                                PrattToken *tok __attribute__((unused))) {
    ENTER(switchExp);
    AstFunCall *f = switchFC(parser);
    int save = PROTECT(f);
    AstExpression *expr = newAstExpression_FunCall(CPI(f), f);
    LEAVE(switchExp);
    UNPROTECT(save);
    return expr;
}

/**
 * @brief parselet triggered by a prefix `back` token.
 */
static AstExpression *back(PrattRecord *record __attribute__((unused)),
                           PrattParser *parser,
                           AstExpression *lhs __attribute__((unused)),
                           PrattToken *tok __attribute__((unused))) {
    ENTER(back);
    AstExpression *res = newAstExpression_Back(LEXPI(parser->lexer));
    LEAVE(back);
    return res;
}

/**
 * @brief parselet triggered by a prefix wildCard (`_`) token.
 */
static AstExpression *wildCard(PrattRecord *record __attribute__((unused)),
                               PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused))) {
    ENTER(wildCard);
    AstExpression *res = newAstExpression_WildCard(LEXPI(parser->lexer));
    LEAVE(wildCard);
    return res;
}

/**
 * @brief parselet triggered by a prefix `error` token.
 */
static AstExpression *error(PrattRecord *record, PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok __attribute__((unused))) {
    ENTER(error);
    AstExpression *toError = expressionPrecedence(parser, record->prefix.prec);
    int save = PROTECT(toError);
    AstExpression *res = newAstExpression_Error(CPI(toError), toError);
    LEAVE(error);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `print` token.
 */
static AstExpression *print(PrattRecord *record, PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok __attribute__((unused))) {
    ENTER(print);
    AstExpression *toPrint = expressionPrecedence(parser, record->prefix.prec);
    int save = PROTECT(toPrint);
    AstExpression *res = makeAstExpression_Print(CPI(toPrint), toPrint);
    LEAVE(print);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `typeof` token.
 */
static AstExpression *typeOfExp(PrattRecord *record, PrattParser *parser,
                                AstExpression *lhs __attribute__((unused)),
                                PrattToken *tok __attribute__((unused))) {
    ENTER(typeOfExp);
    AstExpression *exp = expressionPrecedence(parser, record->prefix.prec);
    int save = PROTECT(exp);
    AstExpression *res = makeAstExpression_TypeOf(CPI(exp), exp);
    LEAVE(typeOfExp);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `assert` token.
 */
static AstExpression *passert(PrattRecord *record, PrattParser *parser,
                              AstExpression *lhs __attribute__((unused)),
                              PrattToken *tok __attribute__((unused))) {
    ENTER(passert);
    AstExpression *toAssert = expressionPrecedence(parser, record->prefix.prec);
    int save = PROTECT(toAssert);
    AstExpression *res = newAstExpression_Assertion(CPI(toAssert), toAssert);
    LEAVE(passert);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `unsafe` token.
 *
 * Will parse either a subsequent switch or function definition.
 */
static AstExpression *unsafe(PrattRecord *record __attribute__((unused)),
                             PrattParser *parser,
                             AstExpression *lhs __attribute__((unused)),
                             PrattToken *tok) {
    ENTER(unsafe);
    AstExpression *expr = NULL;
    int save = PROTECT(expr);
    if (match(parser, TOK_FN())) {
        AstCompositeFunction *f = composite_function(parser);
        PROTECT(f);
        f->unsafe = true;
        expr = newAstExpression_Fun(CPI(f), f);
    } else if (match(parser, TOK_SWITCH())) {
        AstFunCall *f = switchFC(parser);
        PROTECT(f);
        if (f->function->type == AST_EXPRESSION_TYPE_FUN) {
            f->function->val.fun->unsafe = true;
        }
        expr = newAstExpression_FunCall(CPI(f), f);
    } else {
        parserError(parser, "expected fn or switch after unsafe");
        expr = errorExpression(TOKPI(tok));
    }
    LEAVE(unsafe);
    UNPROTECT(save);
    return expr;
}

/**
 * @brief parselet triggered by a prefix `macro` token.
 *
 * We can't actually allow anonymous macro expressions but need to ensure
 * `macro` is registered as a prefix operator so that it can't be
 * overridden.
 */
static AstExpression *macro(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok) {
    parserErrorAt(TOKPI(tok), parser, "can't declare macros as expressions");
    return errorExpression(TOKPI(tok));
}

/**
 * @brief parselet triggered by a prefix `fn` token.
 */
static AstExpression *fn(PrattRecord *record __attribute__((unused)),
                         PrattParser *parser,
                         AstExpression *lhs __attribute__((unused)),
                         PrattToken *tok __attribute__((unused))) {
    ENTER(fn);
    AstCompositeFunction *f = composite_function(parser);
    int save = PROTECT(f);
    f->unsafe = false;
    AstExpression *expr = newAstExpression_Fun(CPI(f), f);
    UNPROTECT(save);
    LEAVE(fn);
    return expr;
}

/**
 * @brief parselet triggered by a prefix tuple (`#(`) token
 */
static AstExpression *tuple(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok) {
    ENTER(tuple);
    AstExpressions *args = expressions(parser);
    int save = PROTECT(args);
    consume(parser, TOK_CLOSE());
    AstExpression *res = newAstExpression_Tuple(TOKPI(tok), args);
    LEAVE(tuple);
    UNPROTECT(save);
    return res;
}

static AstExpression *infixLeft(PrattRecord *record, PrattParser *parser,
                                AstExpression *lhs,
                                PrattToken *tok __attribute__((unused))) {
    ENTER(infixLeft);
    AstExpression *rhs = expressionPrecedence(parser, record->infix.prec + 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(lhs), record->symbol, lhs, rhs);
    LEAVE(infixLeft);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief parselet triggered by an infix period.
 */
static AstExpression *lookUp(PrattRecord *record, PrattParser *parser,
                             AstExpression *lhs,
                             PrattToken *tok __attribute__((unused))) {
    ENTER(lookUp);
    AstExpression *rhs = expressionPrecedence(parser, record->infix.prec - 1);
    int save = PROTECT(rhs);
    if (lhs->type == AST_EXPRESSION_TYPE_SYMBOL) {
        int index = 0;
        if (!findNameSpace(parser, lhs->val.symbol, &index)) {
            parserError(parser, "cannot resolve nameSpace %s",
                        lhs->val.symbol->name);
        }
        rhs = makeAstExpression_LookUp(LEXPI(parser->lexer), index,
                                       lhs->val.symbol, rhs);
    } else {
        parserError(parser, "expected nameSpace on lhs of '.', got %s",
                    astExpressionTypeName(lhs->type));
    }
    LEAVE(lookUp);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief parselet triggered by an infix `->` token.
 */
static AstExpression *infixRight(PrattRecord *record, PrattParser *parser,
                                 AstExpression *lhs,
                                 PrattToken *tok __attribute__((unused))) {
    ENTER(infixRight);
    AstExpression *rhs = expressionPrecedence(parser, record->infix.prec - 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(rhs), record->symbol, lhs, rhs);
    LEAVE(infixRight);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief parselet triggered by an infix `=` operator in an expression.
 */
static AstExpression *exprAlias(PrattRecord *record, PrattParser *parser,
                                AstExpression *lhs,
                                PrattToken *tok __attribute__((unused))) {
    ENTER(exprAlias);
    AstExpression *rhs = expressionPrecedence(parser, record->infix.prec - 1);
    int save = PROTECT(rhs);
    HashSymbol *alias = NULL;
    if (lhs->type == AST_EXPRESSION_TYPE_SYMBOL) {
        alias = lhs->val.symbol;
    } else {
        parserErrorAt(CPI(lhs), parser, "invalid lhs for alias");
        alias = TOK_ERROR();
    }
    AstExpression *res = makeAstExpression_Alias(CPI(lhs), alias, rhs);
    LEAVE(exprAlias);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet installed at parser run time to handle a user-defined prefix
 * operator.
 */
static AstExpression *userPrefix(PrattRecord *record, PrattParser *parser,
                                 AstExpression *lhs __attribute__((unused)),
                                 PrattToken *tok) {
    ENTER(userPrefix);
    AstExpression *rhs = expressionPrecedence(parser, record->prefix.prec);
    int save = PROTECT(rhs);
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    PROTECT(arguments);
#ifdef SAFETY_CHECKS
    if (record->prefix.originalImpl == NULL) {
        cant_happen("prefix operator %s has no original implementation",
                    record->symbol->name);
    }
#endif
    // Create annotated symbol with both hygienic wrapper and original
    // implementation
    AstExpression *func = makeAstExpression_AnnotatedSymbol(
        TOKPI(tok), record->prefix.hygienicFunc, record->prefix.originalImpl);
    PROTECT(func);
    if (record->prefix.importNsRef >= 0) {
        func = makeAstExpression_LookUp(TOKPI(tok), record->prefix.importNsRef,
                                        record->prefix.importNsSymbol, func);
        PROTECT(func);
    }
    rhs = makeAstExpression_FunCall(TOKPI(tok), func, arguments);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief common handler for user defined left, right and nonassoc infix
 * operators.
 */
static AstExpression *userInfixCommon(PrattRecord *record, PrattParser *parser,
                                      AstExpression *lhs, PrattToken *tok,
                                      int precShift, bool nonassoc) {
    ENTER(userInfixCommon);
    AstExpression *rhs =
        expressionPrecedence(parser, record->infix.prec + precShift);
    int save = PROTECT(rhs);
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    REPLACE_PROTECT(save, arguments);
    arguments = newAstExpressions(CPI(lhs), lhs, arguments);
    REPLACE_PROTECT(save, arguments);
#ifdef SAFETY_CHECKS
    if (record->infix.originalImpl == NULL) {
        cant_happen("infix operator %s has no original implementation",
                    record->symbol->name);
    }
#endif
    // Create annotated symbol with both hygienic wrapper and original
    // implementation
    AstExpression *func = makeAstExpression_AnnotatedSymbol(
        TOKPI(tok), record->infix.hygienicFunc, record->infix.originalImpl);
    PROTECT(func);
    if (record->infix.importNsRef >= 0) {
        func = makeAstExpression_LookUp(TOKPI(tok), record->infix.importNsRef,
                                        record->infix.importNsSymbol, func);
        PROTECT(func);
    }
    rhs = makeAstExpression_FunCall(TOKPI(tok), func, arguments);
    REPLACE_PROTECT(save, rhs);
    if (nonassoc) {
        PrattToken *next = peek(parser);
        PROTECT(next);
        if (next != NULL) {
            PrattRecord *nextRecord = fetchRecord(parser, next->type);
            if (nextRecord != NULL &&
                nextRecord->infix.prec == record->infix.prec &&
                next->type == tok->type) {
                parserErrorAt(TOKPI(next), parser,
                              "non-associative operator used in succession");
            }
        }
    }
    LEAVE(userInfixCommon);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief parselet installed at parser run time to handle a user-defined infix
 * nonassoc operator.
 */
static AstExpression *userInfixNone(PrattRecord *record, PrattParser *parser,
                                    AstExpression *lhs, PrattToken *tok) {
    return userInfixCommon(record, parser, lhs, tok, 0, true);
}

/**
 * @brief parselet installed at parser run time to handle a user-defined infix
 * left operator.
 */
static AstExpression *userInfixLeft(PrattRecord *record, PrattParser *parser,
                                    AstExpression *lhs, PrattToken *tok) {
    return userInfixCommon(record, parser, lhs, tok, +1, false);
}

/**
 * @brief parselet installed at parser run time to handle a user-defined infix
 * right operator.
 */
static AstExpression *userInfixRight(PrattRecord *record, PrattParser *parser,
                                     AstExpression *lhs, PrattToken *tok) {
    return userInfixCommon(record, parser, lhs, tok, -1, false);
}

/**
 * @brief parselet installed at parser run time to handle a user-defined postfix
 * operator.
 */
static AstExpression *userPostfix(PrattRecord *record,
                                  PrattParser *parser __attribute__((unused)),
                                  AstExpression *lhs, PrattToken *tok) {
    AstExpressions *arguments = newAstExpressions(CPI(lhs), lhs, NULL);
    int save = PROTECT(arguments);
#ifdef SAFETY_CHECKS
    if (record->postfix.originalImpl == NULL) {
        cant_happen("postfix operator %s has no original implementation",
                    record->symbol->name);
    }
#endif
    // Create annotated symbol with both hygienic wrapper and original
    // implementation
    AstExpression *func = makeAstExpression_AnnotatedSymbol(
        TOKPI(tok), record->postfix.hygienicFunc, record->postfix.originalImpl);
    PROTECT(func);
    if (record->postfix.importNsRef >= 0) {
        func = makeAstExpression_LookUp(TOKPI(tok), record->postfix.importNsRef,
                                        record->postfix.importNsSymbol, func);
        PROTECT(func);
    }
    AstExpression *res = makeAstExpression_FunCall(TOKPI(tok), func, arguments);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `if` token.
 */
static AstExpression *iff(PrattRecord *record __attribute__((unused)),
                          PrattParser *parser,
                          AstExpression *lhs __attribute__((unused)),
                          PrattToken *tok __attribute__((unused))) {
    ENTER(iff);
    consume(parser, TOK_OPEN());
    AstExpression *condition = expression(parser);
    int save = PROTECT(condition);
    consume(parser, TOK_CLOSE());
    AstNest *consequent = nest(parser);
    PROTECT(consequent);
    consume(parser, TOK_ELSE());
    AstNest *alternative = NULL;
    if (match(parser, TOK_IF())) {
        PrattToken *tok2 = peek(parser);
        PROTECT(tok2);
        AstExpression *iff_nest = iff(NULL, parser, NULL, tok2);
        PROTECT(iff_nest);
        AstExpressions *nest_body =
            newAstExpressions(CPI(iff_nest), iff_nest, NULL);
        PROTECT(nest_body);
        alternative = newAstNest(CPI(nest_body), NULL, nest_body);
    } else {
        alternative = nest(parser);
    }
    PROTECT(alternative);
    AstExpression *res =
        makeAstExpression_Iff(TOKPI(tok), condition, consequent, alternative);
    LEAVE(iff);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix atom not recognised as special.
 */
static AstExpression *makeAtom(PrattRecord *record __attribute__((unused)),
                               PrattParser *parser __attribute__((unused)),
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok) {
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_ATOM) {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    HashSymbol *name = tok->value->val.atom;

    // Special handling for currentLine - replace with line number
    if (name == currentLineSymbol()) {
        MaybeBigInt *lineNum = fakeBigInt(TOKPI(tok).lineNo, false);
        int save = PROTECT(lineNum);
        AstExpression *result = newAstExpression_Number(TOKPI(tok), lineNum);
        UNPROTECT(save);
        return result;
    }

    return newAstExpression_Symbol(TOKPI(tok), name);
}

/**
 * @brief parselet triggered by a prefix number.
 */
static AstExpression *makeNumber(PrattRecord *record __attribute__((unused)),
                                 PrattParser *parser __attribute__((unused)),
                                 AstExpression *lhs __attribute__((unused)),
                                 PrattToken *tok) {
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_NUMBER) {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    MaybeBigInt *mbi = tok->value->val.number;
    int save = PROTECT(mbi);
    AstExpression *res = newAstExpression_Number(TOKPI(tok), mbi);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix character
 */
static AstExpression *makeChar(PrattRecord *record __attribute__((unused)),
                               PrattParser *parser __attribute__((unused)),
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok) {
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_STRING) {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    Character c = tok->value->val.string->entries[0];
    AstExpression *res = newAstExpression_Character(TOKPI(tok), c);
    return res;
}

/**
 * @brief utility to convert a string to a nested list of conses of characters.
 */
static AstFunCall *makeStringList(ParserInfo PI, PrattUnicode *str) {
    AstExpression *nil = newAstExpression_Symbol(PI, nilSymbol());
    int save = PROTECT(nil);
    AstFunCall *res = newAstFunCall(PI, nil, NULL);
    PROTECT(res);
    for (int size = str->size; size > 0; size--) {
        Character c = str->entries[size - 1];
        if (c == 0) {
            continue;
        }
        AstExpression *character = newAstExpression_Character(PI, c);
        int save2 = PROTECT(character);
        AstExpression *cons = newAstExpression_Symbol(PI, consSymbol());
        PROTECT(cons);
        AstExpression *tail = newAstExpression_FunCall(PI, res);
        PROTECT(tail);
        AstExpressions *rhs = newAstExpressions(PI, tail, NULL);
        PROTECT(rhs);
        AstExpressions *args = newAstExpressions(PI, character, rhs);
        PROTECT(args);
        res = newAstFunCall(PI, cons, args);
        REPLACE_PROTECT(save, res);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a string.
 */
static AstExpression *makeString(PrattRecord *record __attribute__((unused)),
                                 PrattParser *parser __attribute__((unused)),
                                 AstExpression *lhs __attribute__((unused)),
                                 PrattToken *tok) {
    ENTER(makeString);
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_STRING) {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    enqueueToken(parser->lexer, tok);
    PrattUnicode *uni = str(parser);
    int save = PROTECT(uni);
    AstFunCall *list = makeStringList(TOKPI(tok), uni);
    PROTECT(list);
    AstExpression *res = newAstExpression_FunCall(TOKPI(tok), list);
    LEAVE(makeString);
    UNPROTECT(save);
    return res;
}

/**
 * @brief the core algorithm of the Pratt Parser.
 */
static AstExpression *expressionPrecedence(PrattParser *parser,
                                           int minimumPrecedence) {
    ENTER(expressionPrecedence);
    AstExpression *lhs = NULL;
    PrattToken *tok = next(parser);
    int save = PROTECT(tok);
    PrattRecord *record = fetchRecord(parser, tok->type);
    if (record == NULL) {
        parserError(parser, "unrecognised prefix token: %s", tok->type->name);
        lhs = errorExpression(TOKPI(tok));
    } else if (record->prefix.op == NULL) {
        parserError(parser, "not a prefix operator: %s", tok->type->name);
        lhs = errorExpression(TOKPI(tok));
    } else {
        lhs = record->prefix.op(record, parser, NULL, tok);
    }
    REPLACE_PROTECT(save, lhs);
    for (;;) {
        PrattToken *op = peek(parser);
        PROTECT(op);
        if (op->type == TOK_EOF()) {
            DEBUG("PEEKED EOF");
            break;
        } else {
            DEBUG("PEEKED OP %s", op->type->name);
            PrattRecord *record = fetchRecord(parser, op->type);
            if (record == NULL) {
                // allow tokens with no records to terminate expressions
                // on the assumption that they are secondary mixfix operators
                DEBUG("NULL record for token: %s", op->type->name);
                break;
            } else if (record->postfix.op != NULL) {
                DEBUG("postfix %d %d", record->postfix.prec, minimumPrecedence);
                if (record->postfix.prec < minimumPrecedence) {
                    break;
                }
                next(parser);
                lhs = record->postfix.op(record, parser, lhs, op);
                REPLACE_PROTECT(save, lhs);
            } else if (record->infix.op != NULL) {
                DEBUG("infix %d %d", record->infix.prec, minimumPrecedence);
                if (record->infix.prec < minimumPrecedence) {
                    break;
                }
                next(parser);
                validateLastAlloc();
                lhs = record->infix.op(record, parser, lhs, op);
                REPLACE_PROTECT(save, lhs);
            } else {
                DEBUG("prefix");
                break;
            }
        }
    }
    LEAVE(expressionPrecedence);
    UNPROTECT(save);
    return lhs;
}

void initLocale(char *locale) { setlocale(LC_ALL, locale); }