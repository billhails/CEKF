/* * CEKF - VM supporting amb
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

/**
 * @file pratt_parser.c
 * @brief The Pratt parser.
 */
#include <stdio.h>
#include <ctype.h>

#include "pratt.h"
#include "pratt_parser.h"
#include "pratt_debug.h"
#include "pratt_functions.h"
#include "pratt_scanner.h"
#include "symbols.h"
#include "ast.h"
#include "bigint.h"
#include "utf8.h"
#include "print_generator.h"
#include "file_id.h"
#include "preamble.h"

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

// UTF8
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
static AstFarg *astLookupToFarg(PrattParser *parser, AstLookup *lookup);
static AstFarg *astNumberToFarg(ParserInfo, MaybeBigInt *);
static AstFarg *astStructureToFarg(PrattParser *parser, AstStruct *structure);
static AstFarg *astSymbolToFarg(ParserInfo, HashSymbol *);
static AstFarg *astTupleToFarg(PrattParser *parser, AstExpressions *tuple);
static AstFargList *astExpressionsToFargList(PrattParser *parser, AstExpressions *exprs);
static AstFargList *fargs(PrattParser *);
static AstCompositeFunction *composite_function(PrattParser *);
static AstCompositeFunction *functions(PrattParser *);
static AstDefinition *alias(PrattParser *);
static AstDefinition *assignment(PrattParser *);
static AstDefinition *definition(PrattParser *);
static AstDefinition *defmacro(PrattParser *);
static AstDefinition *defun(PrattParser *, bool, bool);
static AstDefinition *link(PrattParser *);
static AstDefinition *typedefinition(PrattParser *);
static AstDefinitions *definitions(PrattParser *, HashSymbol *);
static AstDefinitions *prattParseLink(PrattParser *, char *, PrattParser **);
static AstExpression *back(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *call(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *doPrefix(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *errorExpression(ParserInfo);
static AstExpression *error(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *exprAlias(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *expression(PrattParser *);
static AstExpression *expressionPrecedence(PrattParser *, int);
static AstExpression *fn(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *grouping(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *iff(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
// static AstExpression        *infixLeft(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *infixRight(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *list(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *lookup(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *macro(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *makeAtom(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *makeChar(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *makeNumber(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *makeString(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *nestexpr(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *passert(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *print(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *typeofExp(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *switchExp(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *tuple(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *unsafe(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userInfixLeft(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userInfixRight(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userInfixNone(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userPostfix(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userPrefix(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *wildcard(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpressions *expressions(PrattParser *);
static AstExpressions *statements(PrattParser *, HashSymbol *);
static AstFileIdArray *fileIdStack = NULL;
static AstFunCall *switchFC(PrattParser *parser);
static AstLookupOrSymbol *scoped_symbol(PrattParser *);
static AstNamespace *parseLink(PrattParser *, unsigned char *, HashSymbol *);
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
static PrattRecord *fetchRecord(PrattParser *, HashSymbol *, bool);
static PrattTrie *makePrattTrie(PrattParser *, PrattTrie *);
static PrattUnicode *PrattUTF8ToUnicode(PrattUTF8 *);
static PrattUTF8 *rawString(PrattParser *);
static PrattUTF8 *str(PrattParser *);
static void storeNamespace(PrattParser *, AstNamespace *);
static void synchronize(PrattParser *parser);
// if you're wondering where the arithmetic primitives are, they're
// defined in the preamble.

#ifdef DEBUG_PRATT_PARSER
void disablePrattDebug(void)
{
    DEBUGGING_OFF();
}
#endif

static PrattParsers *parserStack = NULL;

/**
 * @brief Create a new AstExpression representing an error.
 *
 * The new expression includes ParserInfo containing filename or string context, and line number.
 */
static AstExpression *errorExpression(ParserInfo I)
{
    return newAstExpression_Symbol(I, TOK_ERROR());
}

/**
 * @brief Add a record to the PrattRecordTable.
 *
 * Used during the construction and later modification of the PrattParser's record table.
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
static void addRecord(PrattRecordTable *table,
                      HashSymbol *tok,
                      PrattParselet prefix,
                      int prefixPrec,
                      PrattParselet infix,
                      int infixPrec,
                      PrattParselet postfix,
                      int postfixPrec,
                      PrattAssoc associativity)
{
    PrattRecord *record = newPrattRecord(tok,
                                         prefix,
                                         prefixPrec * PRECEDENCE_SCALE,
                                         infix,
                                         infixPrec * PRECEDENCE_SCALE,
                                         postfix,
                                         postfixPrec * PRECEDENCE_SCALE,
                                         associativity);
    int save = PROTECT(record);
    setPrattRecordTable(table, record->symbol, record);
    UNPROTECT(save);
}

/**
 * @brief Creates the initial PrattParser with the default set of parser rules.
 */
static PrattParser *makePrattParser(void)
{
    PrattParser *parser = newPrattParser(NULL);
    int save = PROTECT(parser);
    PrattRecordTable *table = parser->rules;
    addRecord(table, TOK_SEMI(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_PREFIX(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_INFIX(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_POSTFIX(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_ATOM(), makeAtom, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_CHAR(), makeChar, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_NUMBER(), makeNumber, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_STRING(), makeString, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_TYPEDEF(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_PRINT(), print, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_TYPEOF(), typeofExp, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_BACK(), back, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_ASSERT(), passert, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_UNSAFE(), unsafe, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_FN(), fn, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_MACRO(), macro, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_LINK(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_SWITCH(), switchExp, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_AS(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_ALIAS(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_KW_ERROR(), error, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_NAMESPACE(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_LET(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_IN(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_TUPLE(), tuple, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_CLOSE(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_IF(), iff, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_ELSE(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_LSQUARE(), list, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_RSQUARE(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_COMMA(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_LCURLY(), nestexpr, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_RCURLY(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_PIPE(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_WILDCARD(), wildcard, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_KW_NUMBER(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_KW_CHAR(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_EOF(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_ERROR(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_ARROW(), NULL, 0, infixRight, 1, NULL, 0, PRATTASSOC_TYPE_RIGHT);
    addRecord(table, TOK_ASSIGN(), NULL, 0, exprAlias, 7, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_COLON(), NULL, 0, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_HASH(), doPrefix, 13, NULL, 0, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_OPEN(), grouping, 0, call, 14, NULL, 0, PRATTASSOC_TYPE_NONE);
    addRecord(table, TOK_PERIOD(), NULL, 0, lookup, 15, NULL, 0, PRATTASSOC_TYPE_NONE);
    parser->trie = makePrattTrie(parser, NULL);
    UNPROTECT(save);
    return parser;
}

/**
 * @brief Create a new AstCompositeFunction from a list of AstAltFunction.
 * Handles the case where the same alt function body has alternative argument lists by
 * copying the nest and creating a new AstFunction for each argument list.
 */
static AstCompositeFunction *makeAstCompositeFunction(AstAltFunction *functions, AstCompositeFunction *rest)
{
    int save = PROTECT(NULL);
    for (AstAltArgs *args = functions->altArgs; args != NULL; args = args->next)
    {
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
 * @brief Convert a PrattUTF8 to a PrattUnicode.
 *
 * This function reads the UTF-8 encoded characters from the PrattUTF8 structure,
 * converts them to unencoded (UTF-32) Unicode characters, and stores them in a new PrattUnicode structure.
 *
 * Since the final run-time representation of strings is as lists of characters and the vector
 * representation of lists has two 32 bit fields there is no additional overhead in using U-32.
 *
 * @param utf8 The PrattUTF8 structure containing UTF-8 encoded characters.
 * @return A new PrattUnicode structure containing the unencoded Unicode characters.
 */
static PrattUnicode *PrattUTF8ToUnicode(PrattUTF8 *utf8)
{
    PrattUnicode *uni = newPrattUnicode();
    int save = PROTECT(uni);
    unsigned char *entry = utf8->entries;
    while (*entry != 0)
    {
        Character c;
        entry = utf8_to_unicode_char(&c, entry);
        pushPrattUnicode(uni, c);
    }
    UNPROTECT(save);
    return uni;
}

/**
 * @brief Try to create an AgnosticFileId from a prefix and a file name.
 *
 * This function constructs a file path by concatenating the prefix and file name,
 * and then attempts to create an AgnosticFileId from that path.
 * If the AgnosticFileId creation fails, it frees the allocated buffer.
 *
 * @param prefix The prefix path to prepend to the file name.
 * @param file The file name to append to the prefix.
 * @return A pointer to the AgnosticFileId if successful, or NULL if the file does not exist.
 */
static AgnosticFileId *tryFile(char *prefix, char *file)
{
    char *buf = malloc(sizeof(char) * (strlen(prefix) + 1 + strlen(file) + 10));
    if (buf == NULL)
    {
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
static AgnosticFileId *searchForFile(char *initialPrefix, char *fileToFind)
{
    AgnosticFileId *result = NULL;
    result = tryFile(initialPrefix, fileToFind);
    if (result != NULL)
        return result;
    if (include_paths != NULL)
    {
        for (Index i = 0; i < include_paths->size; i++)
        {
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
 * @param parser The PrattParser to get the current file name from.
 * @return The name of the current file, or "no_file" if not available.
 */
static char *currentPrattFile(PrattParser *parser)
{
    char *no_file = "no_file";
    if (parser == NULL)
        return no_file;
    if (parser->lexer == NULL)
        return no_file;
    if (parser->lexer->bufList == NULL)
        return no_file;
    return parser->lexer->bufList->filename->name;
}

/**
 * @brief Calculate the path for a file based on the current parser's context.
 */
static AgnosticFileId *calculatePath(unsigned char *file, PrattParser *parser)
{
    if (*file == '/') {
        return makeAgnosticFileId((char *)file);
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
 * This function checks if the given AgnosticFileId is already present in the AstFileIdArray.
 *
 * @param id The AgnosticFileId to check.
 * @param array The AstFileIdArray to search in.
 * @return true if the file ID is found, false otherwise.
 */
static bool fileIdInArray(AgnosticFileId *id, AstFileIdArray *array)
{
    for (Index i = 0; i < array->size; ++i)
    {
        if (cmpAgnosticFileId(id, array->entries[i]) == CMP_EQ)
        {
            return true;
        }
    }
    return false;
}
/**
 * @brief If the parser is in panic mode then advance it to a more likely stable state where parsing can resume.
 */
static void synchronize(PrattParser *parser)
{
    if (!parser->panicMode)
        return;
    parser->panicMode = false;
    for (;;)
    {
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
        if (check(parser, TOK_SEMI()))
        {
            next(parser);
            return;
        }
        next(parser);
    }
}

// only a few scenarios we actually need to support:
// 1. parse a standalone string, for tests ONLY
// 2. parse a file after parsing the prelude, the normal case
// 3. parse a string after parsing the prelude, allows a -e style command line option
// 4. parse a linked file (within the parser)

/**
 * @brief Parse a standalone string into an AstNest.
 *
 * This is only used for testing purposes.
 */
AstNest *prattParseStandaloneString(char *data, char *name)
{
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromString(data, name);
    AstNest *nest = top(parser);
    UNPROTECT(save);
    return nest;
}

/**
 * @brief Parse a given PrattLexer into an AstProg.
 *
 * This function initializes a PrattParser, sets up the lexer, and parses the input.
 * It returns an AstProg containing the parsed definitions and expressions.
 *
 * create a parser and a lexer
 * parse the preamble with them
 * create a child parser
 * give it the lexer for the main data
 * parse the main data
 * collect namespaces etc.
 *
 * @param thing The PrattLexer to parse.
 * @return An AstProg containing the parsed data.
 */
static AstProg *prattParseThing(PrattLexer *thing)
{
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromString((char *)preamble, "preamble");
    parser->isPreamble = true;
    AstNest *nest = top(parser);
    if (parser->lexer->bufList != NULL)
    {
        parserError(parser, "unconsumed tokens");
    }
    AstDefinitions *definitions = NULL;
    if (nest)
    {
        definitions = nest->definitions;
        PROTECT(definitions);
    }
    PrattParser *child = newPrattParser(parser);
    PROTECT(child);
    child->lexer = thing;
    nest = top(child);
    if (child->lexer->bufList != NULL)
    {
        parserError(child, "unconsumed tokens");
    }
    PROTECT(nest);
    AstExpression *expression = newAstExpression_Nest(CPI(nest), nest);
    PROTECT(expression);
    AstExpressions *exprs = newAstExpressions(CPI(expression), expression, NULL);
    PROTECT(exprs);
    nest = newAstNest(CPI(expression), definitions, exprs);
    AstProg *prog = astNestToProg(nest); // has direct access to namespaces
    UNPROTECT(save);
    return prog;
}

/**
 * @brief Parse a file into an AstProg.
 *
 * This function creates a PrattLexer from the specified file, initializes a PrattParser,
 * and parses the contents of the file into an AstProg. This includes handling the preamble.
 *
 * @param file The name of the file to parse.
 * @return An AstProg containing the parsed data.
 */
AstProg *prattParseFile(char *file)
{
    PrattLexer *lexer = makePrattLexerFromFilename(file);
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
AstProg *prattParseString(char *data, char *name)
{
    PrattLexer *lexer = makePrattLexerFromString(data, name);
    int save = PROTECT(lexer);
    AstProg *prog = prattParseThing(lexer);
    UNPROTECT(save);
    return prog;
}

/**
 * @brief Find the preamble parser in the linked list of PrattParsers.
 */
static PrattParser *findPreambleParser(PrattParser *parser)
{
#ifdef SAFETY_CHECKS
    if (parser == NULL)
    {
        cant_happen("cannot find preamble parser");
    }
#endif
    if (parser->isPreamble)
    {
        return parser;
    }
    return findPreambleParser(parser->next);
}

/**
 * @brief Parse a linked file and return its definitions.
 *
 * This function finds the preamble parser, creates a new parser for the linked file,
 * with the preamble parser as a child (for operators defined in the preamble)
 * and parses the file to extract its definitions. It also updates the result parser.
 *
 * @param parser The current PrattParser.
 * @param file The name of the file to link.
 * @param resultParser Pointer to store the resulting PrattParser.
 * @return An AstDefinitions containing the parsed definitions from the linked file.
 */
static AstDefinitions *prattParseLink(PrattParser *parser, char *file, PrattParser **resultParser)
{
    parser = findPreambleParser(parser);
    parser = newPrattParser(parser);
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromFilename(file);
    AstDefinitions *definitions = NULL;
    AstNest *nest = top(parser);
    if (nest)
    {
        definitions = nest->definitions;
    }
    *resultParser = parser;
    UNPROTECT(save);
    return definitions;
}

/**
 * @brief Find a namespace in the parser's namespace table or in the next parser.
 *
 * This function checks if a namespace with the given symbol exists in the current parser's
 * namespace table. If not found, it recursively checks the next parser in the linked list.
 *
 * @param parser The PrattParser to search in.
 * @param symbol The HashSymbol representing the namespace to find.
 * @param result Pointer to store the result if found (can be NULL).
 * @return true if the namespace is found, false otherwise.
 */
static bool findNamespace(PrattParser *parser, HashSymbol *symbol, int *result)
{
    if (parser == NULL)
        return false;
    if (getPrattNsIdTable(parser->namespaces, symbol, result))
        return true;
    return findNamespace(parser->next, symbol, result);
}

/**
 * @brief Store a namespace in the parser's namespace table.
 *
 * This function checks if a namespace with the given symbol already exists.
 * If it does, it raises a parser error for redefinition. Otherwise, it stores
 * the namespace in the parser's namespace table.
 *
 * @param parser The PrattParser to store the namespace in.
 * @param ns The AstNamespace to store.
 */
static void storeNamespace(PrattParser *parser, AstNamespace *ns)
{
    if (findNamespace(parser, ns->symbol, NULL))
    {
        parserError(parser, "redefinition of namespace %s", ns->symbol->name);
    }
    else
    {
        setPrattNsIdTable(parser->namespaces, ns->symbol, ns->reference);
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
int initFileIdStack()
{
    if (fileIdStack == NULL)
    {
        fileIdStack = newAstFileIdArray();
    }
    return PROTECT(fileIdStack);
}

/**
 * @brief Initialize the parser stack for PrattParsers.
 *
 * This function checks if the parser stack is already initialized.
 * If not, it creates a new PrattParsers stack to hold PrattParser instances.
 *
 * @return The Index of the newly created and protected parserStack
 */
int initParserStack()
{
    if (parserStack == NULL)
    {
        parserStack = newPrattParsers();
    }
    return PROTECT(parserStack);
}

/**
 * @brief Parse a link to a file and return its namespace.
 *
 * This function checks if the file exists, looks for an existing namespace,
 * and handles recursive includes. If the file is not found it returns with an error.
 * If the file is already parsed it returns the already existing namespace.
 *
 * Careful. Somewhat accidentally this algorithm stores the namespaces
 * in the order that they need to be processed.
 * Specifically because a namespace is parsed before it is recorded,
 * all of its imports are recorded ahead of it.
 *
 * @param parser The PrattParser to use for parsing.
 * @param filename The name of the file to link.
 * @param symbol The HashSymbol representing the namespace symbol.
 * @return An AstNamespace containing the parsed namespace or an error.
 */
static AstNamespace *parseLink(PrattParser *parser, unsigned char *filename, HashSymbol *symbol)
{
    // check the file exists
    AgnosticFileId *fileId = calculatePath(filename, parser);
    int save = PROTECT(fileId);
    if (fileId == NULL)
    {
        parserError(parser, "cannot find file \"%s\"", filename);
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    // see if we've already parsed it, if so return the existing namespace id
    int found = lookupNamespace(fileId);
    if (found != -1)
    {
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, found);
        UNPROTECT(save);
        return ns;
    }
    // check for a recursive include
    if (fileIdInArray(fileId, fileIdStack))
    {
        parserError(parser, "recursive include detected for %s", fileId->name);
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    // protect against recursive include
    pushAstFileIdArray(fileIdStack, fileId);
    // parse the file
    PrattParser *resultParser = NULL;
    // careful, 2 pushes in a row could realloc the save stack on push 1
    int save2 = PROTECT(fileId);
    AstDefinitions *definitions = prattParseLink(parser, fileId->name, &resultParser);
    REPLACE_PROTECT(save2, resultParser);
    PROTECT(definitions);
    // save the new namespace and it's parser
    AstNamespaceImpl *impl = newAstNamespaceImpl(BUFPI(parser->lexer->bufList), fileId, definitions);
    PROTECT(impl);
    found = pushAstNamespaceArray(namespaces, impl);
    pushPrattParsers(parserStack, resultParser);
    // un-protect against recursive include
    popAstFileIdArray(fileIdStack);
    // return the id of the namespace
    AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, found);
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
static PrattTrie *makePrattTrie(PrattParser *parser, PrattTrie *C)
{
    HashSymbol *tok;
    Index i = 0;
    int save = PROTECT(parser); // not C because we need to have a slot for REPLACE_PROTECT
    while ((tok = iteratePrattRecordTable(parser->rules, &i, NULL)) != NULL)
    {
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
static AstExpression *makePrattBinary(ParserInfo I, HashSymbol *op, AstExpression *lhs, AstExpression *rhs)
{
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    int save = PROTECT(arguments);
    arguments = newAstExpressions(CPI(lhs), lhs, arguments);
    REPLACE_PROTECT(save, arguments);
    AstExpression *symbol = newAstExpression_Symbol(I, op);
    PROTECT(symbol);
    AstFunCall *funCall = newAstFunCall(I, symbol, arguments);
    REPLACE_PROTECT(save, funCall);
    AstExpression *res = newAstExpression_FunCall(I, funCall);
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
static AstExpression *makePrattUnary(ParserInfo I, HashSymbol *op, AstExpression *expr)
{
    AstExpressions *arguments = newAstExpressions(CPI(expr), expr, NULL);
    int save = PROTECT(arguments);
    AstExpression *symbol = newAstExpression_Symbol(I, op);
    PROTECT(symbol);
    AstFunCall *funCall = newAstFunCall(I, symbol, arguments);
    REPLACE_PROTECT(save, funCall);
    AstExpression *res = newAstExpression_FunCall(I, funCall);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Create a new child PrattParser from a parent parser.
 *
 * This function initializes a new PrattParser with the parent parser as its parent,
 * and sets the lexer to be the same as the parent's lexer.
 *
 * @param parent The parent PrattParser to inherit from.
 * @return A new PrattParser that is a child of the given parent.
 */
static PrattParser *makeChildParser(PrattParser *parent)
{
    PrattParser *child = newPrattParser(parent);
    child->lexer = parent->lexer;
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
static AstNest *top(PrattParser *parser)
{
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
 * @brief Parse the body of a nest, which can be a let block, a namespace, or just statements.
 *
 * This function checks for a let block, a namespace declaration, or just statements,
 * and constructs an AstNest accordingly.
 *
 * @param parser The PrattParser to use for parsing.
 * @param terminal The HashSymbol that indicates the end of the nest body.
 * @return An AstNest containing the parsed definitions and statements.
 */
static AstNest *nest_body(PrattParser *parser, HashSymbol *terminal)
{
    ENTER(nest_body);
    AstNest *res = NULL;
    int save = PROTECT(parser);
    if (match(parser, TOK_LET()))
    {
        AstDefinitions *defs = definitions(parser, TOK_IN());
        save = PROTECT(defs);
        consume(parser, TOK_IN());
        AstExpressions *stats = statements(parser, terminal);
        PROTECT(stats);
        res = newAstNest(CPI(defs), defs, stats);
    }
    else if (match(parser, TOK_NAMESPACE()))
    {
        AstDefinitions *defs = definitions(parser, terminal);
        save = PROTECT(defs);
        res = newAstNest(CPI(defs), defs, NULL);
    }
    else
    {
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
 * This function recursively parses expressions followed by semicolons,
 * and constructs an AstExpressions list until the terminal token is found.
 *
 * @param parser The PrattParser to use for parsing.
 * @param terminal The HashSymbol that indicates the end of the statements.
 * @return An AstExpressions containing the parsed statements.
 */
static AstExpressions *statements(PrattParser *parser, HashSymbol *terminal)
{
    ENTER(statements);
    AstExpression *expr = expression(parser);
    int save = PROTECT(expr);
    AstExpressions *this = NULL;
    if (check(parser, TOK_SEMI()))
    {
        next(parser);
        validateLastAlloc();
        if (check(parser, terminal))
        {
            this = newAstExpressions(CPI(expr), expr, NULL);
        }
        else
        {
            AstExpressions *rest = statements(parser, terminal);
            PROTECT(rest);
            this = newAstExpressions(CPI(expr), expr, rest);
        }
    }
    else
    {
        this = newAstExpressions(CPI(expr), expr, NULL);
    }
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
static AstExpression *expression(PrattParser *parser)
{
    AstExpression *res = expressionPrecedence(parser, 0);
    int save = PROTECT(res);
    synchronize(parser);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Parse a sequence of definitions.
 *
 * @param parser The PrattParser to use for parsing.
 * @param terminal The HashSymbol that indicates the end of definitions.
 * @return An AstDefinitions containing the parsed definitions, or NULL if no definitions are found.
 */
static AstDefinitions *definitions(PrattParser *parser, HashSymbol *terminal)
{
    ENTER(definitions);
    if (check(parser, terminal))
    {
        LEAVE(definitions);
        return NULL;
    }
    if (check(parser, TOK_EOF()))
    {
        LEAVE(definitions);
        return NULL;
    }
    AstDefinition *def = definition(parser);
    int save = PROTECT(def);
    AstDefinitions *next = definitions(parser, terminal);
    PROTECT(next);
    AstDefinitions *this = newAstDefinitions(CPI(def), def, next);
    LEAVE(definitions);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Validate an operator string for use in the parser.
 *
 * This function checks if the operator string is valid according to certain rules:
 * - It cannot be empty.
 * - It cannot start with a numeric digit.
 * - It cannot start with an opening or closing bracket.
 * - It cannot contain whitespace.
 *
 * @param parser The PrattParser to report errors to.
 * @param operator The PrattUTF8 operator string to validate.
 */
static void validateOperator(PrattParser *parser, PrattUTF8 *operator)
{
    if (strlen((char *)operator->entries) == 0)
    {
        parserError(parser, "operator cannot be empty string");
    }
    else if (isdigit(operator->entries[0]))
    {
        parserError(parser, "operator cannot start with a numeric digit");
    }
    else if (utf8_isopen(operator->entries))
    {
        parserError(parser, "operator cannot start with an opening bracket");
    }
    else if (utf8_isclose(operator->entries))
    {
        parserError(parser, "operator cannot start with a closing bracket");
    }
    else
    {
        for (Index i = 0; i < operator->size; i++)
        {
            if (isspace(operator->entries[i]))
            {
                parserError(parser, "operator cannot contain whitespace");
                break;
            }
        }
    }
}

/**
 * @brief Add a new user-defined operator to the parser's operator table.
 *
 * This function adds a new operator with its fixity, precedence, and implementation
 * to the parser's operator table.
 *
 * It is an error to redefine an operator of the same fixity type.
 *
 * @param parser The PrattParser to add the operator to.
 * @param fixity The fixity type of the operator (prefix, infix, postfix).
 * @param associativity The associativity type of the operator (left, right, none).
 * @param precedence The precedence level of the operator.
 * @param operator The PrattUTF8 representation of the operator.
 * @param impl The AstExpression implementation of the operator.
 */
static void addOperator(PrattParser *parser,
                        PrattFixity fixity,
                        PrattAssoc associativity,
                        int precedence,
                        PrattUTF8 *operator,
                        AstExpression *impl)
{
    HashSymbol *op = newSymbol((char *)operator->entries);
    PrattRecord *record = fetchRecord(parser, op, false);
    int save = PROTECT(record);
    if (record)
    {
        record = copyPrattRecord(record);
        PROTECT(record);
        switch (fixity)
        {
        case PRATTFIXITY_TYPE_PREFIX:
        {
            if (record->prefixOp)
            {
                parserErrorAt(CPI(impl),
                              parser,
                              "attempt to redefine prefix operator \"%s\"",
                              operator->entries);
            }
            record->prefixOp = userPrefix;
            record->prefixPrec = precedence * PRECEDENCE_SCALE;
            record->prefixImpl = impl;
        }
        break;
        case PRATTFIXITY_TYPE_INFIX:
        {
            if (record->infixOp)
            {
                parserErrorAt(CPI(impl),
                              parser,
                              "attempt to redefine infix operator \"%s\"",
                              operator->entries);
            }
            else if (record->postfixOp)
            {
                parserErrorAt(CPI(impl),
                              parser,
                              "attempt to define existing postfix operator \"%s\" as infix",
                              operator->entries);
            }
            record->infixOp = associativity == PRATTASSOC_TYPE_LEFT ? userInfixLeft : associativity == PRATTASSOC_TYPE_RIGHT ? userInfixRight
                                                                                                                             : userInfixNone;
            record->infixPrec = precedence * PRECEDENCE_SCALE;
            record->infixImpl = impl;
        }
        break;
        case PRATTFIXITY_TYPE_POSTFIX:
        {
            if (record->postfixOp)
            {
                parserErrorAt(CPI(impl),
                              parser,
                              "attempt to redefine postfix operator \"%s\"",
                              operator->entries);
            }
            else if (record->infixOp)
            {
                parserErrorAt(CPI(impl),
                              parser,
                              "attempt to define existing infix operator \"%s\" as postfix",
                              operator->entries);
            }
            record->postfixOp = userPostfix;
            record->postfixPrec = precedence * PRECEDENCE_SCALE;
            record->postfixImpl = impl;
        }
        break;
        }
    }
    else
    {
        switch (fixity)
        {
        case PRATTFIXITY_TYPE_PREFIX:
        {
            record = newPrattRecord(op, userPrefix, precedence * PRECEDENCE_SCALE, NULL, 0, NULL, 0, associativity);
            PROTECT(record);
            record->prefixImpl = impl;
        }
        break;
        case PRATTFIXITY_TYPE_INFIX:
        {
            record = newPrattRecord(op, NULL, 0,
                                    associativity == PRATTASSOC_TYPE_LEFT
                                        ? userInfixLeft
                                    : associativity == PRATTASSOC_TYPE_RIGHT
                                        ? userInfixRight
                                        : userInfixNone,
                                    precedence * PRECEDENCE_SCALE, NULL, 0, associativity);
            PROTECT(record);
            record->infixImpl = impl;
        }
        break;
        case PRATTFIXITY_TYPE_POSTFIX:
        {
            record = newPrattRecord(op, NULL, 0, NULL, 0, userPostfix, precedence * PRECEDENCE_SCALE, associativity);
            PROTECT(record);
            record->postfixImpl = impl;
        }
        break;
        }
        parser->trie = insertPrattTrie(parser->trie, op);
    }
    if (record)
    {
        // hoist - might have come from parent env
        setPrattRecordTable(parser->rules, op, record);
    }
    UNPROTECT(save);
}

/**
 * @brief Shallow copy a PrattRecordTable.
 *
 * This function iterates through the source PrattRecordTable and copies each record
 * to the destination PrattRecordTable, ensuring that all operator definitions are preserved.
 *
 * @param to The destination PrattRecordTable to copy to.
 * @param from The source PrattRecordTable to copy from.
 */
static void copyPrattRecordTable(PrattRecordTable *to, PrattRecordTable *from)
{
    Index i = 0;
    HashSymbol *symbol = NULL;
    PrattRecord *record = NULL;
    while ((symbol = iteratePrattRecordTable(from, &i, &record)) != NULL)
    {
        setPrattRecordTable(to, symbol, record);
    }
}

/**
 * @brief Shallow copy a PrattNsIdTable.
 *
 * This function iterates through the source PrattNsIdTable and copies each namespace
 * symbol and its associated record to the destination PrattNsIdTable.
 *
 * @param to The destination PrattNsIdTable to copy to.
 * @param from The source PrattNsIdTable to copy from.
 */
static void copyPrattNsIdTable(PrattNsIdTable *to, PrattNsIdTable *from)
{
    Index i = 0;
    HashSymbol *symbol = NULL;
    int record = 0;
    while ((symbol = iteratePrattNsIdTable(from, &i, &record)) != NULL)
    {
        setPrattNsIdTable(to, symbol, record);
    }
}

static PrattParser *meldParsers(PrattParser *to, PrattParser *from) __attribute__((unused));

/**
 * @brief Merge two PrattParsers, combining their operator definitions and namespaces.
 *
 * This function creates a new PrattParser that combines the rules and namespaces
 * of the two given PrattParsers. It ensures that no operator is redefined and
 * merges the trie structures.
 *
 * CURRENTLY UNUSED, but might be useful in the future.
 *
 * @param to The first PrattParser to merge.
 * @param from The second PrattParser to merge.
 * @return A new PrattParser that contains the merged rules and namespaces.
 */
static PrattParser *meldParsers(PrattParser *to, PrattParser *from) {
    if (from->trie) {
        PrattParser *result = newPrattParser(to->next);
        int save = PROTECT(result);
        copyPrattRecordTable(result->rules, to->rules);
        copyPrattNsIdTable(result->namespaces, to->namespaces);
        result->lexer = to->lexer;
        result->trie = to->trie;
        Index i = 0;
        HashSymbol *op = NULL;
        PrattRecord *record = NULL;
        while ((op = iteratePrattRecordTable(from->rules, &i, &record)) != NULL) {
            PrattRecord *target = NULL;
            getPrattRecordTable(to->rules, op, &target);
            if (target == NULL) {
                target = copyPrattRecord(record);
                PROTECT(target);
                setPrattRecordTable(result->rules, op, target);
                result->trie = insertPrattTrie(result->trie, op);
            } else {
                if (record->prefixImpl) {
                    if (target->prefixImpl) {
                        parserError(to, "import redefines prefix operator %s", op->name);
                    } else {
                        target->prefixImpl = record->prefixImpl;
                        target->prefixPrec = record->prefixPrec;
                        target->prefixOp = record->prefixOp;
                    }
                }
                if (record->infixImpl) {
                    if (target->infixImpl) {
                        parserError(to, "import redefines infix operator %s", op->name);
                    } else if (target->postfixImpl) {
                        parserError(to, "import defines infix operator %s"
                                        " over existing postfix operator",
                                    op->name);
                    } else {
                        target->infixImpl = record->infixImpl;
                        target->infixPrec = record->infixPrec;
                        target->infixOp = record->infixOp;
                    }
                }
                if (record->postfixImpl) {
                    if (target->postfixImpl) {
                        parserError(to, "import redefines postfix operator %s", op->name);
                    } else if (target->infixImpl) {
                        parserError(to, "import defines postfix operator %s"
                                        " over existing infix operator",
                                    op->name);
                    } else {
                        target->postfixImpl = record->postfixImpl;
                        target->postfixPrec = record->postfixPrec;
                        target->postfixOp = record->postfixOp;
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

/**
 * @brief Parse a prefix or postfix operator definition.
 *
 * This function checks for a prefix or postfix operator definition, validates the precedence,
 * and adds it to the parser's operator table.
 *
 * @param parser The PrattParser to use for parsing.
 * @param isPostfix A boolean indicating whether to parse a postfix or prefix operator.
 * @return An AstDefinition representing the prefix or postfix operator definition.
 */
static AstDefinition *preOrPostfix(PrattParser *parser, bool isPostfix)
{
    ENTER(preOrPostfix);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    if (check(parser, TOK_NUMBER()))
    {
        PrattToken *prec = next(parser);
        PROTECT(prec);
#ifdef SAFETY_CHECKS
        if (prec->value->type != PRATTVALUE_TYPE_NUMBER)
        {
            cant_happen("unexpected %s", prattValueTypeName(prec->value->type));
        }
#endif
        MaybeBigInt *bi = prec->value->val.number;
        if (bi->type == BI_SMALL && !bi->imag)
        {
            int precedence = bi->small;
            PrattUTF8 *str = rawString(parser);
            PROTECT(str);
            validateOperator(parser, str);
            AstExpression *impl = expression(parser);
            PROTECT(impl);
            consume(parser, TOK_SEMI());
            addOperator(parser, isPostfix ? PRATTFIXITY_TYPE_POSTFIX : PRATTFIXITY_TYPE_PREFIX,
                        false, precedence, str, impl);
        }
        else
        {
            parserErrorAt(TOKPI(prec), parser, "expected small integer");
        }
    }
    LEAVE(preOrPostfix);
    UNPROTECT(save);
    return newAstDefinition_Blank(TOKPI(tok));
}

/**
 * @brief Parse a postfix operator definition.
 *
 * This function checks for a postfix operator definition, validates the precedence,
 * and adds it to the parser's operator table.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstDefinition representing the postfix operator definition.
 */
static AstDefinition *postfix(PrattParser *parser)
{
    return preOrPostfix(parser, true);
}

/**
 * @brief Parse a prefix operator definition.
 *
 * This function checks for a prefix operator definition, validates the precedence,
 * and adds it to the parser's operator table.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstDefinition representing the prefix operator definition.
 */
static AstDefinition *prefix(PrattParser *parser)
{
    return preOrPostfix(parser, false);
}

/**
 * @brief Parse an infix operator definition.
 *
 * This function checks for an infix operator definition, validates the associativity
 * and precedence, and adds it to the parser's operator table.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstDefinition representing the infix operator definition.
 */
static AstDefinition *infix(PrattParser *parser)
{
    ENTER(infix);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    if (check(parser, TOK_ATOM())) {
        PrattToken *atom = next(parser);
        PROTECT(atom);
#ifdef SAFETY_CHECKS
        if (atom->value->type != PRATTVALUE_TYPE_ATOM) {
            cant_happen("unexpected %s", prattValueTypeName(atom->value->type));
        }
#endif
        PrattAssoc associativity;
        if (atom->value->val.atom == TOK_LEFT()) {
            associativity = PRATTASSOC_TYPE_LEFT;
        } else if (atom->value->val.atom == TOK_RIGHT()) {
            associativity = PRATTASSOC_TYPE_RIGHT;
        } else if (atom->value->val.atom == TOK_NONE()) {
            associativity = PRATTASSOC_TYPE_NONE;
        } else {
            parserErrorAt(TOKPI(atom), parser, "expected \"left\", \"right\" or \"none\" after infix keyword");
        }
        if (check(parser, TOK_NUMBER())) {
            PrattToken *prec = next(parser);
            PROTECT(prec);
#ifdef SAFETY_CHECKS
            if (prec->value->type != PRATTVALUE_TYPE_NUMBER) {
                cant_happen("unexpected %s", prattValueTypeName(prec->value->type));
            }
#endif
            MaybeBigInt *bi = prec->value->val.number;
            if (bi->type == BI_SMALL && !bi->imag) {
                int precedence = bi->small;
                PrattUTF8 *str = rawString(parser);
                PROTECT(str);
                validateOperator(parser, str);
                AstExpression *impl = expression(parser);
                PROTECT(impl);
                consume(parser, TOK_SEMI());
                addOperator(parser, PRATTFIXITY_TYPE_INFIX, associativity, precedence, str, impl);
            } else {
                parserErrorAt(TOKPI(prec), parser, "expected small integer");
            }
        }
    }
    LEAVE(infix);
    UNPROTECT(save);
    return newAstDefinition_Blank(TOKPI(tok));
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
static AstDefinition *definition(PrattParser *parser)
{
    ENTER(definition);
    AstDefinition *res = NULL;
    if (check(parser, TOK_ATOM())) {
        res = assignment(parser);
    } else if (match(parser, TOK_TYPEDEF())) {
        res = typedefinition(parser);
    } else if (match(parser, TOK_UNSAFE())) {
        consume(parser, TOK_FN());
        res = defun(parser, true, false);
    } else if (match(parser, TOK_FN())) {
        res = defun(parser, false, false);
    } else if (match(parser, TOK_PRINT())) {
        res = defun(parser, false, true);
    } else if (match(parser, TOK_MACRO())) {
        res = defmacro(parser);
    } else if (match(parser, TOK_LINK())) {
        res = link(parser);
    } else if (match(parser, TOK_ALIAS())) {
        res = alias(parser);
    } else if (match(parser, TOK_PREFIX())) {
        res = prefix(parser);
    } else if (match(parser, TOK_INFIX())) {
        res = infix(parser);
    } else if (match(parser, TOK_POSTFIX())) {
        res = postfix(parser);
    } else {
        PrattToken *tok = next(parser);
        validateLastAlloc();
        parserError(parser, "expecting definition");
        res = newAstDefinition_Blank(TOKPI(tok));
    }
    int save = PROTECT(res);
    synchronize(parser);
    LEAVE(definition);
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
static AstDefinition *alias(PrattParser *parser)
{
    ENTER(alias);
    HashSymbol *s = symbol(parser);
    consume(parser, TOK_ASSIGN());
    AstType *t = type_type(parser);
    int save = PROTECT(t);
    consume(parser, TOK_SEMI());
    AstAlias *a = newAstAlias(CPI(t), s, t);
    PROTECT(a);
    AstDefinition *d = newAstDefinition_Alias(CPI(a), a);
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
static AstType *type_type(PrattParser *parser)
{
    ENTER(type_type);
    AstType *type = NULL;
    int save = PROTECT(type);
    if (match(parser, TOK_OPEN()))
    {
        type = type_type(parser);
        PROTECT(type);
        consume(parser, TOK_CLOSE());
    }
    else
    {
        AstTypeClause *clause = type_clause(parser);
        PROTECT(clause);
        type = newAstType(CPI(clause), clause, NULL);
        PROTECT(type);
        if (match(parser, TOK_ARROW()))
        {
            type->next = type_type(parser);
        }
    }
    LEAVE(type_type);
    UNPROTECT(save);
    return type;
}

/**
 * @brief Parse a type clause, which can be a number, character, variable, function, or tuple.
 *
 * This function checks the next token and parses it accordingly,
 * returning an AstTypeClause representing the parsed type.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeClause representing the parsed type clause, or NULL.
 */
static AstTypeClause *type_clause(PrattParser *parser)
{
    ENTER(type_clause);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    AstTypeClause *ret = NULL;
    if (match(parser, TOK_KW_NUMBER()))
    {
        ret = newAstTypeClause_Integer(TOKPI(tok));
    }
    else if (match(parser, TOK_KW_CHAR()))
    {
        ret = newAstTypeClause_Character(TOKPI(tok));
    }
    else if (tok->type == TOK_HASH())
    {
        HashSymbol *typeVar = type_variable(parser);
        ret = newAstTypeClause_Var(TOKPI(tok), typeVar);
    }
    else if (tok->type == TOK_ATOM())
    {
        AstTypeFunction *fun = type_function(parser);
        PROTECT(fun);
        ret = newAstTypeClause_TypeFunction(CPI(fun), fun);
    }
    else if (tok->type == TOK_TUPLE())
    {
        AstTypeList *lst = type_tuple(parser);
        PROTECT(lst);
        ret = newAstTypeClause_TypeTuple(CPI(lst), lst);
    }
    else
    {
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
static HashSymbol *type_variable(PrattParser *parser)
{
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
 * This function recursively parses a list of types and constructs an AstTypeList.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeList representing the parsed list of types.
 */
static AstTypeList *type_list(PrattParser *parser)
{
    ENTER(type_list);
    AstType *type = type_type(parser);
    int save = PROTECT(type);
    AstTypeList *this = newAstTypeList(CPI(type), type, NULL);
    PROTECT(this);
    if (match(parser, TOK_COMMA()))
    {
        this->next = type_list(parser);
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
static AstTypeMap *type_map(PrattParser *parser)
{
    ENTER(type_map);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    consume(parser, TOK_COLON());
    AstType *type = type_type(parser);
    PROTECT(type);
    AstTypeMap *this = newAstTypeMap(TOKPI(tok), s, type, NULL);
    PROTECT(this);
    if (match(parser, TOK_COMMA()))
    {
        this->next = type_map(parser);
    }
    LEAVE(type_map);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Parse a single function definition within a composite function body.
 *
 * This function checks for an alt function definition, parses its arguments and body,
 * and constructs an AstAltFunction.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstAltFunction representing the parsed function definition.
 */
static AstAltFunction *alt_function(PrattParser *parser)
{
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
 * @brief Parse a type function, which can have a name and an optional type list.
 *
 * This function parses a scoped symbol as the name of the type function,
 * and if it matches an open parenthesis, it parses a list of types.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeFunction representing the parsed type function.
 */
static AstTypeFunction *type_function(PrattParser *parser)
{
    ENTER(type_function);
    AstLookupOrSymbol *name = scoped_symbol(parser);
    int save = PROTECT(name);
    AstTypeFunction *this = newAstTypeFunction(CPI(name), name, NULL);
    PROTECT(this);
    if (match(parser, TOK_OPEN()))
    {
        this->typeList = type_list(parser);
        consume(parser, TOK_CLOSE());
    }
    LEAVE(type_function);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Parse a tuple type, which is a sequence of types enclosed in parentheses, preceeded by a hash.
 *
 * This function consumes the hash and opening parenthesis, parses a list of types,
 * and then consumes the closing parenthesis, returning an AstTypeList.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstTypeList representing the parsed tuple type.
 */
static AstTypeList *type_tuple(PrattParser *parser)
{
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
static AstAltArgs *alt_args(PrattParser *parser)
{
    ENTER(alt_args);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    AstFargList *args = fargs(parser);
    PROTECT(args);
    AstAltArgs *this = newAstAltArgs(TOKPI(tok), args, NULL);
    PROTECT(this);
    if (match(parser, TOK_PIPE()))
    {
        this->next = alt_args(parser);
    }
    LEAVE(alt_args);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Parse a nested block of code, which can contain definitions and statements.
 *
 * This function consumes the opening curly brace, creates a child parser,
 * and parses the body of the nest until the closing curly brace is found.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstNest representing the parsed nested block.
 */
static AstNest *nest(PrattParser *parser)
{
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
 * @brief Parse the body of a nested block, which can contain definitions or statements.
 *
 * This function creates a child parser and calls nest_body to parse the body.
 * then it consumes the closing curly brace.
 *
 * It differs from nest() above in that it does not consume the opening curly brace,
 * and it returns an AstExpression rather than a bare AstNest. Its signature also
 * matches the PrattParser's nestexpr function as it is called directly by the parser.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstExpression representing the parsed body of the nest.
 */
static AstExpression *nestexpr(PrattRecord *record __attribute__((unused)),
                               PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused)))
{
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
 * @brief Parse a scoped symbol, that is a symbol optionally prefixed by a namespace and a period.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstLookupOrSymbol representing the parsed scoped symbol.
 */
static AstLookupOrSymbol *scoped_symbol(PrattParser *parser)
{
    ENTER(scoped_symbol);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *sym1 = symbol(parser);
    if (match(parser, TOK_PERIOD()))
    {
        HashSymbol *sym2 = symbol(parser);
        int index = 0;
        if (findNamespace(parser, sym1, &index))
        {
            AstLookupSymbol *lus = newAstLookupSymbol(TOKPI(tok), index, sym1, sym2);
            PROTECT(lus);
            AstLookupOrSymbol *res = newAstLookupOrSymbol_Lookup(CPI(lus), lus);
            LEAVE(scoped_symbol);
            UNPROTECT(save);
            return res;
        }
        else
        {
            parserError(parser, "cannot resolve namespace %s", sym1->name);
        }
    }
    AstLookupOrSymbol *res = newAstLookupOrSymbol_Symbol(TOKPI(tok), sym1);
    LEAVE(scoped_symbol);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Parse a list of formal arguments for a function.
 *
 * This function consumes the opening parenthesis, parses the expressions,
 * and converts them into an AstFargList. It also consumes the closing parenthesis.
 *
 * @param parser The PrattParser to use for parsing.
 * @return An AstFargList representing the parsed formal arguments.
 */
static AstFargList *fargs(PrattParser *parser)
{
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
 * @brief Convert an AstLookup to an AstLookupSymbol.
 *
 * @param parser The PrattParser (used only for error reporting).
 * @param lookup The AstLookup to convert.
 * @return An AstLookupSymbol representing the converted lookup.
 */
static AstLookupSymbol *astLookupToLus(PrattParser *parser, AstLookup *lookup)
{
    AstLookupSymbol *lus = newAstLookupSymbol(CPI(lookup), lookup->nsid, lookup->nsSymbol, NULL);
    int save = PROTECT(lus);
    switch (lookup->expression->type)
    {
    case AST_EXPRESSION_TYPE_SYMBOL:
        lus->symbol = lookup->expression->val.symbol;
        break;
    default:
        parserErrorAt(CPI(lookup), parser, "invalid lookup in formal arguments");
        lus->symbol = lookup->nsSymbol;
        break;
    }
    UNPROTECT(save);
    return lus;
}

/**
 * @brief Wraps an AstLookup in an AstLookupSymbol and wraps that in an AstLookupOrSymbol.
 *
 * @param parser The PrattParser (used only for error reporting).
 * @param lookup The AstLookup to convert.
 * @return An AstLookupOrSymbol representing the converted lookup.
 */
static AstLookupOrSymbol *astLookupToLos(PrattParser *parser, AstLookup *lookup)
{
    AstLookupSymbol *lus = astLookupToLus(parser, lookup);
    int save = PROTECT(lus);
    AstLookupOrSymbol *res = newAstLookupOrSymbol_Lookup(CPI(lus), lus);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Create an AstLookupOrSymbol representing an error.
 *
 * This function creates a new AstLookupOrSymbol with a TOK_ERROR() symbol,
 * this is only so that the parser has something to return when it encounters an error.
 *
 * @param PI The ParserInfo for the context of the error.
 * @return An AstLookupOrSymbol representing the error.
 */
static AstLookupOrSymbol *makeLosError(ParserInfo PI)
{
    return newAstLookupOrSymbol_Symbol(PI, TOK_ERROR());
}

/**
 * @brief Wrals a symbol in a new AstLookupOrSymbol.
 *
 * @param PI The ParserInfo for the context of the symbol.
 * @param symbol The HashSymbol to wrap.
 * @return An AstLookupOrSymbol containing the symbol.
 */
static AstLookupOrSymbol *astSymbolToLos(ParserInfo PI, HashSymbol *symbol)
{
    return newAstLookupOrSymbol_Symbol(PI, symbol);
}

/**
 * @brief Convert an AstExpression representing a function to an AstLookupOrSymbol.
 *
 * This function checks the type of the expression and converts it to an AstLookupOrSymbol
 * or returns an error if the expression is not a lookup or a symbol.
 * It is used to parse type constructor names in the formal arguments to functions.
 *
 * @param parser The PrattParser (used only for error reporting).
 * @param function The AstExpression representing the function.
 * @return An AstLookupOrSymbol representing the function, or an error.
 */
static AstLookupOrSymbol *astFunctionToLos(PrattParser *parser, AstExpression *function)
{
    switch (function->type)
    {
    case AST_EXPRESSION_TYPE_LOOKUP:
        return astLookupToLos(parser, function->val.lookup);
    case AST_EXPRESSION_TYPE_SYMBOL:
        return astSymbolToLos(CPI(function), function->val.symbol);
    case AST_EXPRESSION_TYPE_BACK:
        parserErrorAt(CPI(function), parser, "invalid use of \"back\" as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_FUNCALL:
        parserErrorAt(CPI(function), parser, "invalid use of function call as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_NUMBER:
        parserErrorAt(CPI(function), parser, "invalid use of number as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_CHARACTER:
        parserErrorAt(CPI(function), parser, "invalid use of character as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_FUN:
        parserErrorAt(CPI(function), parser, "invalid use of function as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_NEST:
        parserErrorAt(CPI(function), parser, "invalid use of nest as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_IFF:
        parserErrorAt(CPI(function), parser, "invalid use of conditional as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_PRINT:
        parserErrorAt(CPI(function), parser, "invalid use of \"print\" as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_TUPLE:
        parserErrorAt(CPI(function), parser, "invalid use of tuple as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_ENV:
        cant_happen("encountered ENV as formal argument");
    case AST_EXPRESSION_TYPE_STRUCTURE:
        parserErrorAt(CPI(function), parser, "invalid use of tuple as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_ASSERTION:
        parserErrorAt(CPI(function), parser, "invalid use of \"assert\" as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_ALIAS:
        parserErrorAt(CPI(function), parser, "invalid use of alias as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_WILDCARD:
        parserErrorAt(CPI(function), parser, "invalid use of wildcard as structure name");
        return makeLosError(CPI(function));
    case AST_EXPRESSION_TYPE_ERROR:
        parserErrorAt(CPI(function), parser, "invalid use of \"error\" as structure name");
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

static AstFarg *astFunCallToFarg(PrattParser *parser, AstFunCall *funCall)
{
    AstLookupOrSymbol *los = astFunctionToLos(parser, funCall->function);
    int save = PROTECT(los);
    AstFargList *args = astExpressionsToFargList(parser, funCall->arguments);
    PROTECT(args);
    AstUnpack *unpack = newAstUnpack(CPI(los), los, args);
    PROTECT(unpack);
    AstFarg *res = newAstFarg_Unpack(CPI(unpack), unpack);
    UNPROTECT(save);
    return res;
}

/**
 * @brief wraps an AstLookup in an AstLookupSymbol and wraps that in an AstFarg
 *
 * @param parser The parser for error reporting.
 * @param lookup the AstLookup to wrap.
 */
static AstFarg *astLookupToFarg(PrattParser *parser, AstLookup *lookup)
{
    AstLookupSymbol *lus = astLookupToLus(parser, lookup);
    int save = PROTECT(lus);
    AstFarg *res = newAstFarg_Lookup(CPI(lus), lus);
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
static AstFarg *astSymbolToFarg(ParserInfo PI, HashSymbol *symbol)
{
    return newAstFarg_Symbol(PI, symbol);
}

/**
 * @brief converts a number to a formal argument.
 */
static AstFarg *astNumberToFarg(ParserInfo PI, MaybeBigInt *bi)
{
    return newAstFarg_Number(PI, bi);
}

/**
 * @brief converts a character to a formal argument.
 */
static AstFarg *astCharacterToFarg(ParserInfo PI, Character c)
{
    return newAstFarg_Character(PI, c);
}

/**
 * @brief converts a list of expressions to a list of formal arguments.
 */
static AstFargList *astExpressionsToFargList(PrattParser *parser, AstExpressions *exprs)
{
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
static AstFarg *astTupleToFarg(PrattParser *parser, AstExpressions *tuple)
{
    AstFargList *args = astExpressionsToFargList(parser, tuple);
    int save = PROTECT(args);
    AstFarg *res = newAstFarg_Tuple(CPI(tuple), args);
    UNPROTECT(save);
    return res;
}

/**
 * @brief converts a map of expressions to a map of formal arguments.
 */
static AstTaggedArgList *astTaggedExpressionsToTaggedFargList(PrattParser *parser,
                                                              AstTaggedExpressions *exprs)
{
    if (exprs == NULL)
        return NULL;
    AstTaggedArgList *next = astTaggedExpressionsToTaggedFargList(parser, exprs->next);
    int save = PROTECT(next);
    AstFarg *arg = astExpressionToFarg(parser, exprs->expression);
    PROTECT(arg);
    AstTaggedArgList *this = newAstTaggedArgList(CPI(arg), exprs->tag, arg, next);
    UNPROTECT(save);
    return this;
}

/**
 * @brief converts a structure containing a map of expressions to a formal argument.
 */
static AstFarg *astStructureToFarg(PrattParser *parser, AstStruct *structure)
{
    AstTaggedArgList *args = astTaggedExpressionsToTaggedFargList(parser, structure->expressions);
    int save = PROTECT(args);
    AstUnpackStruct *unpack = newAstUnpackStruct(CPI(structure), structure->symbol, args);
    PROTECT(unpack);
    AstFarg *res = newAstFarg_UnpackStruct(CPI(unpack), unpack);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Converts an aliased expression to a named formal argument.
 */
static AstFarg *astAliasToFarg(PrattParser *parser, AstExprAlias *alias)
{
    AstFarg *farg = astExpressionToFarg(parser, alias->value);
    int save = PROTECT(farg);
    AstNamedArg *narg = newAstNamedArg(CPI(alias), alias->name, farg);
    PROTECT(narg);
    AstFarg *res = newAstFarg_Named(CPI(narg), narg);
    UNPROTECT(save);
    return res;
}

/**
 * @brief convert an expression to a formal argument.
 */
static AstFarg *astExpressionToFarg(PrattParser *parser, AstExpression *expr)
{
    switch (expr->type)
    {
    case AST_EXPRESSION_TYPE_BACK:
        parserErrorAt(CPI(expr), parser, "invalid use of \"back\" as formal argument");
        return newAstFarg_Wildcard(CPI(expr));
    case AST_EXPRESSION_TYPE_FUNCALL:
        return astFunCallToFarg(parser, expr->val.funCall);
    case AST_EXPRESSION_TYPE_LOOKUP:
        return astLookupToFarg(parser, expr->val.lookup);
    case AST_EXPRESSION_TYPE_SYMBOL:
        return astSymbolToFarg(CPI(expr), expr->val.symbol);
    case AST_EXPRESSION_TYPE_NUMBER:
        return astNumberToFarg(CPI(expr), expr->val.number);
    case AST_EXPRESSION_TYPE_CHARACTER:
        return astCharacterToFarg(CPI(expr), expr->val.character);
    case AST_EXPRESSION_TYPE_FUN:
        parserErrorAt(CPI(expr), parser, "invalid use of function as formal argument");
        return newAstFarg_Wildcard(CPI(expr));
    case AST_EXPRESSION_TYPE_NEST:
        parserErrorAt(CPI(expr), parser, "invalid use of nest as formal argument");
        return newAstFarg_Wildcard(CPI(expr));
    case AST_EXPRESSION_TYPE_IFF:
        parserErrorAt(CPI(expr), parser, "invalid use of conditional as formal argument");
        return newAstFarg_Wildcard(CPI(expr));
    case AST_EXPRESSION_TYPE_PRINT:
        parserErrorAt(CPI(expr), parser, "invalid use of \"print\" as formal argument");
        return newAstFarg_Wildcard(CPI(expr));
    case AST_EXPRESSION_TYPE_TUPLE:
        return astTupleToFarg(parser, expr->val.tuple);
    case AST_EXPRESSION_TYPE_ENV:
        cant_happen("encountered ENV as formal argument");
    case AST_EXPRESSION_TYPE_STRUCTURE:
        return astStructureToFarg(parser, expr->val.structure);
    case AST_EXPRESSION_TYPE_ASSERTION:
        parserErrorAt(CPI(expr), parser, "invalid use of \"assert\" as formal argument");
        return newAstFarg_Wildcard(CPI(expr));
    case AST_EXPRESSION_TYPE_ALIAS:
        return astAliasToFarg(parser, expr->val.alias);
    case AST_EXPRESSION_TYPE_WILDCARD:
        return newAstFarg_Wildcard(CPI(expr));
    case AST_EXPRESSION_TYPE_ERROR:
        parserErrorAt(CPI(expr), parser, "invalid use of \"error\" as formal argument");
        return newAstFarg_Wildcard(CPI(expr));
    default:
        cant_happen("unrecognised %s", astExpressionTypeName(expr->type));
    }
}

/**
 * @brief validate that the macro arguments are conforming (symbols only, and no alternative args)
 */
static void validateMacroArgs(PrattParser *parser, AstAltFunction *definition)
{
    AstAltArgs *altArgs = definition->altArgs;
    if (altArgs->next)
    {
        parserErrorAt(CPI(altArgs->next), parser, "cannot supply alternative arguments to a macro");
    }
    else
    {
        AstFargList *args = altArgs->argList;
        while (args)
        {
            if (args->arg->type != AST_FARG_TYPE_SYMBOL)
            {
                parserErrorAt(CPI(args->arg), parser, "macro arguments can only be simple symbols");
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
static AstDefinition *defmacro(PrattParser *parser)
{
    ENTER(defmacro);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstAltFunction *definition = alt_function(parser);
    PROTECT(definition);
    validateMacroArgs(parser, definition);
    AstDefMacro *defMacro = newAstDefMacro(TOKPI(tok), s, definition);
    PROTECT(defMacro);
    AstDefinition *res = newAstDefinition_Macro(CPI(defMacro), defMacro);
    LEAVE(defmacro);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parse a function definition
 *
 * The `fn` token has already been consumed when this function is triggered.
 */
static AstDefinition *defun(PrattParser *parser, bool unsafe, bool isPrinter)
{
    ENTER(defun);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstCompositeFunction *f = composite_function(parser);
    f->unsafe = unsafe;
    PROTECT(f);
    if (isPrinter)
    {
        s = makePrintName("print$", s->name);
    }
    AstExpression *expr = newAstExpression_Fun(CPI(f), f);
    PROTECT(expr);
    AstDefine *def = newAstDefine(TOKPI(tok), s, expr);
    PROTECT(def);
    AstDefinition *res = newAstDefinition_Define(CPI(def), def);
    LEAVE(defun);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parse a symbol
 */
static HashSymbol *symbol(PrattParser *parser)
{
    ENTER(symbol);
    PrattToken *tok = next(parser);
    validateLastAlloc();
    int save = PROTECT(tok);
    if (tok->type != TOK_ATOM())
    {
        parserError(parser, "expected ATOM");
        LEAVE(symbol);
        UNPROTECT(save);
        return TOK_ERROR();
    }
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_ATOM)
    {
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
static AstDefinition *assignment(PrattParser *parser)
{
    ENTER(assignment);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    consume(parser, TOK_ASSIGN());
    AstExpression *expr = expression(parser);
    PROTECT(expr);
    consume(parser, TOK_SEMI());
    AstDefine *def = newAstDefine(TOKPI(tok), s, expr);
    PROTECT(def);
    AstDefinition *res = newAstDefinition_Define(CPI(def), def);
    LEAVE(assignment);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a typedef
 */
static AstDefinition *typedefinition(PrattParser *parser)
{
    ENTER(typedefinition);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstTypeSig *typeSig = NULL;
    if (check(parser, TOK_OPEN()))
    {
        next(parser);
        validateLastAlloc();
        AstTypeSymbols *variables = type_variables(parser);
        PROTECT(variables);
        consume(parser, TOK_CLOSE());
        typeSig = newAstTypeSig(TOKPI(tok), s, variables);
    }
    else
    {
        typeSig = newAstTypeSig(TOKPI(tok), s, NULL);
    }
    PROTECT(typeSig);
    consume(parser, TOK_LCURLY());
    AstTypeBody *typeBody = type_body(parser);
    PROTECT(typeBody);
    consume(parser, TOK_RCURLY());
    AstTypeDef *typeDef = newAstTypeDef(CPI(typeSig), typeSig, typeBody);
    PROTECT(typeDef);
    AstDefinition *res = newAstDefinition_TypeDef(CPI(typeDef), typeDef);
    LEAVE(typedefinition);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses the body of a typedef
 */
static AstTypeBody *type_body(PrattParser *parser)
{
    ENTER(type_body);
    AstTypeConstructor *tc = type_constructor(parser);
    int save = PROTECT(tc);
    AstTypeBody *this = NULL;
    if (match(parser, TOK_PIPE()))
    {
        AstTypeBody *rest = type_body(parser);
        PROTECT(rest);
        this = newAstTypeBody(CPI(tc), tc, rest);
    }
    else
    {
        this = newAstTypeBody(CPI(tc), tc, NULL);
    }
    LEAVE(type_body);
    UNPROTECT(save);
    return this;
}

/**
 * @brief parses a type constructor within the body of a typedef
 */
static AstTypeConstructor *type_constructor(PrattParser *parser)
{
    ENTER(type_constructor);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstTypeConstructorArgs *args = NULL;
    if (match(parser, TOK_OPEN()))
    {
        AstTypeList *typeList = type_list(parser);
        PROTECT(typeList);
        consume(parser, TOK_CLOSE());
        args = newAstTypeConstructorArgs_List(CPI(typeList), typeList);
        PROTECT(args);
    }
    else if (match(parser, TOK_LCURLY()))
    {
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
static AstTypeSymbols *type_variables(PrattParser *parser)
{
    ENTER(type_variables);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = type_variable(parser);
    AstTypeSymbols *t = NULL;
    if (check(parser, TOK_CLOSE()))
    {
        t = newAstTypeSymbols(TOKPI(tok), s, NULL);
    }
    else
    {
        consume(parser, TOK_COMMA());
        AstTypeSymbols *rest = type_variables(parser);
        PROTECT(rest);
        t = newAstTypeSymbols(TOKPI(tok), s, rest);
    }
    LEAVE(type_variables);
    UNPROTECT(save);
    return t;
}

/**
 * @brief parses a link (namespace import) directive.
 */
static AstDefinition *link(PrattParser *parser)
{
    ENTER(link);
    PrattUTF8 *path = rawString(parser);
    int save = PROTECT(path);
    AstDefinition *res = NULL;
    if (path == NULL) {
        res = newAstDefinition_Blank(LEXPI(parser->lexer));
    } else {
        consume(parser, TOK_AS());
        HashSymbol *name = symbol(parser);
        consume(parser, TOK_SEMI());
        AstNamespace *ns = parseLink(parser, path->entries, name);
        PROTECT(ns);
        storeNamespace(parser, ns);
        res = newAstDefinition_Blank(CPI(ns));
    }
    LEAVE(link);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a raw double-quoted string for a link directive.
 */
static PrattUTF8 *rawString(PrattParser *parser)
{
    ENTER(rawString);
    PrattToken *tok = next(parser);
    validateLastAlloc();
    if (tok->type == TOK_STRING())
    {
#ifdef SAFETY_CHECKS
        if (tok->value->type != PRATTVALUE_TYPE_STRING)
        {
            cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
        }
#endif
        LEAVE(rawString);
        return tok->value->val.string;
    }
    else
    {
        parserError(parser, "expected string, got %s", tok->type->name);
        PrattUTF8 *err = newPrattUTF8();
        int save = PROTECT(err);
        pushPrattUTF8(err, 0);
        LEAVE(rawString);
        UNPROTECT(save);
        return err;
    }
}

/**
 * @brief parses a subsequent string, appendin it to the current.
 */
static void appendString(PrattParser *parser, PrattUTF8 *this)
{
    ENTER(appendString);
    PrattUTF8 *next = rawString(parser);
    int save = PROTECT(next);
    this->size--; // backup over '\0'
    for (Index i = 0; i < next->size; i++)
    {
        pushPrattUTF8(this, next->entries[i]);
    }
    UNPROTECT(save);
    if (check(parser, TOK_STRING()))
    {
        appendString(parser, this);
    }
    LEAVE(appendString);
}

/**
 * @brief parses any sequence of adjacent strings into a single string.
 */
static PrattUTF8 *str(PrattParser *parser)
{
    ENTER(str);
    PrattUTF8 *this = rawString(parser);
    int save = PROTECT(this);
    if (check(parser, TOK_STRING()))
    {
        appendString(parser, this);
    }
    LEAVE(str);
    UNPROTECT(save);
    return this;
}

/**
 * @brief parses a composite (or simple) function body
 */
static AstCompositeFunction *composite_function(PrattParser *parser)
{
    ENTER(composite_function);
    AstCompositeFunction *res = NULL;
    int save = PROTECT(res);
    if (match(parser, TOK_LCURLY()))
    {
        res = functions(parser);
        PROTECT(res);
        consume(parser, TOK_RCURLY());
    }
    else
    {
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
static AstCompositeFunction *functions(PrattParser *parser)
{
    ENTER(functions);
    AstAltFunction *f = alt_function(parser);
    int save = PROTECT(f);
    AstCompositeFunction *rest = NULL;
    if (check(parser, TOK_OPEN()))
    {
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
static PrattRecord *fetchRecord(PrattParser *parser, HashSymbol *symbol, bool fatal)
{
    PrattRecord *record = NULL;
    if (getPrattRecordTable(parser->rules, symbol, &record))
    {
        return record;
    }
    else if (parser->next != NULL)
    {
        return fetchRecord(parser->next, symbol, fatal);
    }
    else
    {
        if (fatal)
        {
            cant_happen("unrecognised op %s", symbol->name);
        }
        else
        {
            return NULL;
        }
    }
}

/**
 * @brief triggered by an open round bracket, parses an expression up to the closing bracket.
 */
static AstExpression *grouping(PrattRecord *record,
                               PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused)))
{
    ENTER(grouping);
    AstExpression *res = expressionPrecedence(parser, record->prefixPrec);
    int save = PROTECT(res);
    consume(parser, TOK_CLOSE());
    LEAVE(grouping);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a comma separated list of expressions up to a closing square bracket.
 * @return a nested chain of cons function applications.
 */
static AstFunCall *conslist(PrattParser *parser)
{
    ENTER(conslist);
    AstFunCall *res = NULL;
    int save = PROTECT(res);
    if (check(parser, TOK_RSQUARE()))
    {
        ParserInfo PI = LEXPI(parser->lexer);
        DEBUG("conslist parser info %d %s", PI.lineno, PI.filename);
        AstExpression *nil = newAstExpression_Symbol(PI, nilSymbol());
        PROTECT(nil);
        res = newAstFunCall(PI, nil, NULL);
    }
    else if (check(parser, TOK_EOF()))
    {
        parserError(parser, "unexpected EOF");
        UNPROTECT(save);
        return NULL;
    }
    else
    {
        AstExpression *expr = expression(parser);
        PROTECT(expr);
        match(parser, TOK_COMMA());
        AstFunCall *rest = conslist(parser);
        if (rest == NULL)
        {
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
                           PrattToken *tok __attribute__((unused)))
{
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
 * @brief parses the argument to a prefix operator returning an expression that calls the operator on it.
 */
static AstExpression *doPrefix(PrattRecord *record,
                               PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused)))
{
    ENTER(doPrefix);
    AstExpression *res = expressionPrecedence(parser, record->prefixPrec + 1);
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
static AstExpressions *collectArguments(PrattParser *parser)
{
    ENTER(collectArguments);
    AstExpression *arg = expressionPrecedence(parser, 0);
    int save = PROTECT(arg);
    AstExpressions *next = NULL;
    if (match(parser, TOK_COMMA()))
    {
        next = collectArguments(parser);
        PROTECT(next);
    }
    AstExpressions *this = newAstExpressions(CPI(arg), arg, next);
    LEAVE(collectArguments);
    UNPROTECT(save);
    return this;
}

/**
 * @brief parse the potentially empty list of arguments to a function application.
 */
static AstExpressions *expressions(PrattParser *parser)
{
    ENTER(expressions);
    AstExpressions *args = NULL;
    int save = PROTECT(args);
    if (!check(parser, TOK_CLOSE()))
    {
        args = collectArguments(parser);
        PROTECT(args);
    }
    LEAVE(expressions);
    UNPROTECT(save);
    return args;
}

/**
 * @brief triggered by an infix open round brace (function application), parses the arguments and creates the funcall.
 */
static AstExpression *call(PrattRecord *record __attribute__((unused)),
                           PrattParser *parser,
                           AstExpression *lhs,
                           PrattToken *tok __attribute__((unused)))
{
    ENTER(call);
    AstExpressions *args = expressions(parser);
    int save = PROTECT(args);
    consume(parser, TOK_CLOSE());
    AstFunCall *funCall = newAstFunCall(CPI(lhs), lhs, args);
    PROTECT(funCall);
    AstExpression *res = newAstExpression_FunCall(CPI(funCall), funCall);
    LEAVE(call);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parses a switch statement into an anonymous function definition and application
 *
 * This code is used by both safe and unsafe switch statement parsers.
 */
static AstFunCall *switchFC(PrattParser *parser)
{
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
                                PrattToken *tok __attribute__((unused)))
{
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
                           PrattToken *tok __attribute__((unused)))
{
    ENTER(back);
    AstExpression *res = newAstExpression_Back(LEXPI(parser->lexer));
    LEAVE(back);
    return res;
}

/**
 * @brief parselet triggered by a prefix wildcard (`_`) token.
 */
static AstExpression *wildcard(PrattRecord *record __attribute__((unused)),
                               PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused)))
{
    ENTER(wildcard);
    AstExpression *res = newAstExpression_Wildcard(LEXPI(parser->lexer));
    LEAVE(wildcard);
    return res;
}

/**
 * @brief parselet triggered by a prefix `error` token.
 */
static AstExpression *error(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok __attribute__((unused)))
{
    ENTER(error);
    AstExpression *toError = expression(parser);
    int save = PROTECT(toError);
    AstExpression *res = newAstExpression_Error(CPI(toError), toError);
    LEAVE(error);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `print` token.
 */
static AstExpression *print(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok __attribute__((unused)))
{
    ENTER(print);
    AstExpression *toPrint = expression(parser);
    int save = PROTECT(toPrint);
    AstPrint *printer = newAstPrint(CPI(toPrint), toPrint);
    PROTECT(printer);
    AstExpression *res = newAstExpression_Print(CPI(printer), printer);
    LEAVE(print);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `typeof` token.
 */
static AstExpression *typeofExp(PrattRecord *record __attribute__((unused)),
                                PrattParser *parser,
                                AstExpression *lhs __attribute__((unused)),
                                PrattToken *tok __attribute__((unused)))
{
    ENTER(typeofExp);
    AstExpression *exp = expression(parser);
    int save = PROTECT(exp);
    AstTypeof *typeofNode = newAstTypeof(CPI(exp), exp);
    PROTECT(typeofNode);
    AstExpression *res = newAstExpression_TypeOf(CPI(typeofNode), typeofNode);
    LEAVE(typeofExp);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `assert` token.
 */
static AstExpression *passert(PrattRecord *record __attribute__((unused)),
                              PrattParser *parser,
                              AstExpression *lhs __attribute__((unused)),
                              PrattToken *tok __attribute__((unused)))
{
    ENTER(passert);
    AstExpression *toAssert = expression(parser);
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
                             PrattToken *tok)
{
    ENTER(unsafe);
    AstExpression *expr = NULL;
    int save = PROTECT(expr);
    if (match(parser, TOK_FN()))
    {
        AstCompositeFunction *f = composite_function(parser);
        PROTECT(f);
        f->unsafe = true;
        expr = newAstExpression_Fun(CPI(f), f);
    }
    else if (match(parser, TOK_SWITCH()))
    {
        AstFunCall *f = switchFC(parser);
        PROTECT(f);
        if (f->function->type == AST_EXPRESSION_TYPE_FUN)
        {
            f->function->val.fun->unsafe = true;
        }
        expr = newAstExpression_FunCall(CPI(f), f);
    }
    else
    {
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
                            PrattToken *tok)
{
    parserErrorAt(TOKPI(tok), parser, "can't declare macros as expressions");
    return errorExpression(TOKPI(tok));
}

/**
 * @brief parselet triggered by a prefix `fn` token.
 */
static AstExpression *fn(PrattRecord *record __attribute__((unused)),
                         PrattParser *parser,
                         AstExpression *lhs __attribute__((unused)),
                         PrattToken *tok __attribute__((unused)))
{
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
                            PrattToken *tok __attribute__((unused)))
{
    ENTER(tuple);
    AstExpressions *args = expressions(parser);
    int save = PROTECT(args);
    consume(parser, TOK_CLOSE());
    AstExpression *res = newAstExpression_Tuple(CPI(args), args);
    LEAVE(tuple);
    UNPROTECT(save);
    return res;
}

/*
static AstExpression *infixLeft(PrattRecord *record, PrattParser *parser, AstExpression *lhs,
PrattToken *tok __attribute__((unused))) {
    ENTER(infixLeft);
    AstExpression *rhs = expressionPrecedence(parser, record->infixPrec + 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(lhs), record->symbol, lhs, rhs);
    LEAVE(infixLeft);
    UNPROTECT(save);
    return rhs;
}
*/

/**
 * @brief parselet triggered by an infix period.
 */
static AstExpression *lookup(PrattRecord *record,
                             PrattParser *parser,
                             AstExpression *lhs,
                             PrattToken *tok __attribute__((unused)))
{
    ENTER(lookup);
    AstExpression *rhs = expressionPrecedence(parser, record->infixPrec - 1);
    int save = PROTECT(rhs);
    if (lhs->type == AST_EXPRESSION_TYPE_SYMBOL)
    {
        int index = 0;
        if (!findNamespace(parser, lhs->val.symbol, &index))
        {
            parserError(parser, "cannot resolve namespace %s", lhs->val.symbol->name);
        }
        AstLookup *lup = newAstLookup(LEXPI(parser->lexer), index, lhs->val.symbol, rhs);
        PROTECT(lup);
        rhs = newAstExpression_Lookup(CPI(lup), lup);
    }
    else
    {
        parserError(parser, "expected namespace on lhs of '.', got %s", astExpressionTypeName(lhs->type));
    }
    LEAVE(lookup);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief parselet triggered by an infix `->` token.
 */
static AstExpression *infixRight(PrattRecord *record,
                                 PrattParser *parser,
                                 AstExpression *lhs,
                                 PrattToken *tok __attribute__((unused)))
{
    ENTER(infixRight);
    AstExpression *rhs = expressionPrecedence(parser, record->infixPrec - 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(rhs), record->symbol, lhs, rhs);
    LEAVE(infixRight);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief parselet triggered by an infix `=` operator in an expression.
 */
static AstExpression *exprAlias(PrattRecord *record,
                                PrattParser *parser,
                                AstExpression *lhs,
                                PrattToken *tok __attribute__((unused)))
{
    ENTER(exprAlias);
    AstExpression *rhs = expressionPrecedence(parser, record->infixPrec - 1);
    int save = PROTECT(rhs);
    HashSymbol *alias = NULL;
    if (lhs->type == AST_EXPRESSION_TYPE_SYMBOL)
    {
        alias = lhs->val.symbol;
    }
    else
    {
        parserErrorAt(CPI(lhs), parser, "invalid lhs for alias");
        alias = TOK_ERROR();
    }
    AstExprAlias *exprAlias = newAstExprAlias(CPI(lhs), alias, rhs);
    PROTECT(exprAlias);
    AstExpression *res = newAstExpression_Alias(CPI(exprAlias), exprAlias);
    LEAVE(exprAlias);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet installed at parser run time to handle a user-defined prefix operator.
 */
static AstExpression *userPrefix(PrattRecord *record,
                                 PrattParser *parser,
                                 AstExpression *lhs __attribute__((unused)),
                                 PrattToken *tok)
{
    ENTER(userPrefix);
    AstExpression *rhs = expressionPrecedence(parser, record->prefixPrec);
    int save = PROTECT(rhs);
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    PROTECT(arguments);
    AstFunCall *funCall = newAstFunCall(TOKPI(tok), record->prefixImpl, arguments);
    PROTECT(funCall);
    rhs = newAstExpression_FunCall(CPI(funCall), funCall);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief common handler for user defined left, right and nonassoc infix operators.
 */
static AstExpression *userInfixCommon(PrattRecord *record,
                                      PrattParser *parser,
                                      AstExpression *lhs,
                                      PrattToken *tok,
                                      int precShift,
                                      bool nonassoc)
{
    ENTER(userInfixCommon);
    AstExpression *rhs = expressionPrecedence(parser, record->infixPrec + precShift);
    int save = PROTECT(rhs);
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    REPLACE_PROTECT(save, arguments);
    arguments = newAstExpressions(CPI(lhs), lhs, arguments);
    REPLACE_PROTECT(save, arguments);
    AstFunCall *funCall = newAstFunCall(TOKPI(tok), record->infixImpl, arguments);
    REPLACE_PROTECT(save, funCall);
    rhs = newAstExpression_FunCall(CPI(funCall), funCall);
    REPLACE_PROTECT(save, rhs);
    if (nonassoc)
    {
        PrattToken *next = peek(parser);
        PROTECT(next);
        if (next != NULL)
        {
            PrattRecord *nextRecord = fetchRecord(parser, next->type, false);
            if (nextRecord != NULL &&
                nextRecord->infixPrec == record->infixPrec &&
                nextRecord->associativity == PRATTASSOC_TYPE_NONE &&
                next->type == tok->type)
            {
                parserErrorAt(TOKPI(next), parser, "non-associative operator used in succession");
            }
        }
    }
    LEAVE(userInfixCommon);
    UNPROTECT(save);
    return rhs;
}

/**
 * @brief parselet installed at parser run time to handle a user-defined infix nonassoc operator.
 */
static AstExpression *userInfixNone(PrattRecord *record,
                                    PrattParser *parser,
                                    AstExpression *lhs,
                                    PrattToken *tok)
{
    return userInfixCommon(record, parser, lhs, tok, +1, true);
}

/**
 * @brief parselet installed at parser run time to handle a user-defined infix left operator.
 */
static AstExpression *userInfixLeft(PrattRecord *record,
                                    PrattParser *parser,
                                    AstExpression *lhs,
                                    PrattToken *tok)
{
    return userInfixCommon(record, parser, lhs, tok, +1, false);
}

/**
 * @brief parselet installed at parser run time to handle a user-defined infix right operator.
 */
static AstExpression *userInfixRight(PrattRecord *record,
                                     PrattParser *parser,
                                     AstExpression *lhs,
                                     PrattToken *tok)
{
    return userInfixCommon(record, parser, lhs, tok, -1, false);
}

/**
 * @brief parselet installed at parser run time to handle a user-defined postfix operator.
 */
static AstExpression *userPostfix(PrattRecord *record,
                                  PrattParser *parser __attribute__((unused)),
                                  AstExpression *lhs,
                                  PrattToken *tok)
{
    AstExpressions *arguments = newAstExpressions(CPI(lhs), lhs, NULL);
    int save = PROTECT(arguments);
    AstFunCall *funCall = newAstFunCall(TOKPI(tok), record->postfixImpl, arguments);
    REPLACE_PROTECT(save, funCall);
    AstExpression *res = newAstExpression_FunCall(CPI(funCall), funCall);
    UNPROTECT(save);
    return res;
}

/**
 * @brief parselet triggered by a prefix `if` token.
 */
static AstExpression *iff(PrattRecord *record __attribute__((unused)),
                          PrattParser *parser,
                          AstExpression *lhs __attribute__((unused)),
                          PrattToken *tok __attribute__((unused)))
{
    ENTER(iff);
    consume(parser, TOK_OPEN());
    AstExpression *condition = expression(parser);
    int save = PROTECT(condition);
    consume(parser, TOK_CLOSE());
    AstNest *consequent = nest(parser);
    PROTECT(consequent);
    consume(parser, TOK_ELSE());
    AstNest *alternative = NULL;
    if (match(parser, TOK_IF()))
    {
        AstExpression *iff_nest = iff(NULL, parser, NULL, NULL);
        PROTECT(iff_nest);
        AstExpressions *nest_body = newAstExpressions(CPI(iff_nest), iff_nest, NULL);
        PROTECT(nest_body);
        alternative = newAstNest(CPI(nest_body), NULL, nest_body);
    }
    else
    {
        alternative = nest(parser);
    }
    PROTECT(alternative);
    AstIff *body = newAstIff(TOKPI(tok), condition, consequent, alternative);
    PROTECT(body);
    AstExpression *res = newAstExpression_Iff(CPI(body), body);
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
                               PrattToken *tok)
{
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_ATOM)
    {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    HashSymbol *name = tok->value->val.atom;
    return newAstExpression_Symbol(TOKPI(tok), name);
}

/**
 * @brief parselet triggered by a prefix number.
 */
static AstExpression *makeNumber(PrattRecord *record __attribute__((unused)),
                                 PrattParser *parser __attribute__((unused)),
                                 AstExpression *lhs __attribute__((unused)),
                                 PrattToken *tok)
{
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_NUMBER)
    {
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
                               PrattToken *tok)
{
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_STRING)
    {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    Character c;
    (void)utf8_to_unicode_char(&c, (unsigned char *)tok->value->val.string->entries);
    AstExpression *res = newAstExpression_Character(TOKPI(tok), c);
    return res;
}

/**
 * @brief utility to convert a string to a nested list of conses of characters.
 */
static AstFunCall *makeStringList(ParserInfo PI, PrattUnicode *str)
{
    AstExpression *nil = newAstExpression_Symbol(PI, nilSymbol());
    int save = PROTECT(nil);
    AstFunCall *res = newAstFunCall(PI, nil, NULL);
    PROTECT(res);
    for (int size = str->size; size > 0; size--)
    {
        AstExpression *character = newAstExpression_Character(PI, str->entries[size - 1]);
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
                                 PrattToken *tok)
{
    ENTER(makeString);
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_STRING)
    {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    enqueueToken(parser->lexer, tok);
    PrattUTF8 *utf8 = str(parser);
    int save = PROTECT(utf8);
    PrattUnicode *uni = PrattUTF8ToUnicode(utf8);
    PROTECT(uni);
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
static AstExpression *expressionPrecedence(PrattParser *parser, int minimumPrecedence)
{
    ENTER(expressionPrecedence);
    AstExpression *lhs = NULL;
    PrattToken *tok = next(parser);
    int save = PROTECT(tok);
    PrattRecord *record = fetchRecord(parser, tok->type, true);
    if (record->prefixOp == NULL)
    {
        parserError(parser, "not a prefix operator: %s", tok->type->name);
        lhs = errorExpression(TOKPI(tok));
    }
    else
    {
        lhs = record->prefixOp(record, parser, NULL, tok);
    }
    REPLACE_PROTECT(save, lhs);
    for (;;)
    {
        PrattToken *op = peek(parser);
        PROTECT(op);
        if (op->type == TOK_EOF())
        {
            DEBUG("PEEKED EOF");
            break;
        }
        else
        {
            DEBUG("PEEKED OP %s", op->type->name);
            PrattRecord *record = fetchRecord(parser, op->type, true);
            if (record->postfixOp != NULL)
            {
                DEBUG("postfix %d %d", record->postfixPrec, minimumPrecedence);
                if (record->postfixPrec < minimumPrecedence)
                {
                    break;
                }
                next(parser);
                lhs = record->postfixOp(record, parser, lhs, op);
                REPLACE_PROTECT(save, lhs);
            }
            else if (record->infixOp != NULL)
            {
                DEBUG("infix %d %d", record->infixPrec, minimumPrecedence);
                if (record->infixPrec < minimumPrecedence)
                {
                    break;
                }
                next(parser);
                validateLastAlloc();
                lhs = record->infixOp(record, parser, lhs, op);
                REPLACE_PROTECT(save, lhs);
            }
            else
            {
                DEBUG("prefix");
                break;
            }
        }
    }
    LEAVE(expressionPrecedence);
    UNPROTECT(save);
    return lhs;
}
