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
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

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

static AstExpression *expr_bp(PrattParser *parser, int min_bp);
static AstExpression *errorExpression(ParserInfo);
static PrattRecord *fetchRecord(PrattParser *parser, HashSymbol *symbol, bool fatal);
static PrattTrie *makePrattTrie(PrattParser *parser, PrattTrie *C);

static AstExpression *grouping(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *list(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *doPrefix(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *tuple(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *unsafe(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *fn(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *macro(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *gensym(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *call(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *infixLeft(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *infixRight(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *lookup(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *iff(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *switchExp(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *print(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *nestexpr(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *error(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *back(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *passert(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *makeChar(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *makeAtom(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *makeNumber(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *makeString(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *wildcard(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *exprAlias(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userPrefix(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userInfixLeft(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userInfixRight(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *userPostfix(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);

static AstExpressions *expressions(PrattParser *parser);
static AstDefinitions *definitions(PrattParser *, HashSymbol *);
static AstExpressions *statements(PrattParser *, HashSymbol *);
static AstExpression *expression(PrattParser *);
static AstDefinition *definition(PrattParser *);
static AstDefinition *assignment(PrattParser *);
static AstDefinition *gensym_assignment(PrattParser *);
static AstDefinition *typedefinition(PrattParser *);
static AstDefinition *defun(PrattParser *, bool, bool);
static AstDefinition *defmacro(PrattParser *);
static AstDefinition *link(PrattParser *);
static AstDefinition *alias(PrattParser *);
static HashSymbol *symbol(PrattParser *);
static AstTypeSymbols *type_variables(PrattParser *);
static AstTypeBody *type_body(PrattParser *);
static AstCompositeFunction *composite_function(PrattParser *);
static AstCompositeFunction *functions(PrattParser *);
static PrattUTF8 *rawString(PrattParser *);
static PrattUTF8 *str(PrattParser *);
static AstNamespace *parseLink(PrattParser *, unsigned char *, HashSymbol *);
static void storeNamespace(PrattParser *, AstNamespace *);
static AstType *type_type(PrattParser *);
static AstTypeClause *type_clause(PrattParser *);
static HashSymbol *type_variable(PrattParser *);
static AstTypeConstructor *type_constructor(PrattParser *);
static AstTypeList *type_list(PrattParser *);
static AstTypeMap *type_map(PrattParser *);
static AstAltFunction *alt_function(PrattParser *);
static AstTypeFunction *type_function(PrattParser *);
static AstTypeList *type_tuple(PrattParser *);
static AstAltArgs *alt_args(PrattParser *);
static AstNest *nest(PrattParser *);
static AstNest *nest_body(PrattParser *, HashSymbol *);
static AstLookupOrSymbol *scoped_symbol(PrattParser *);
static AstArgList *fargs(PrattParser *);
static AstFunCall *switchFC(PrattParser *parser);
static PrattUnicode *PrattUTF8ToUnicode(PrattUTF8 *);
static void synchronize(PrattParser *parser);

static AstArg *astFunCallToFarg(PrattParser *parser, AstFunCall *funCall);
static AstArg *astLookupToFarg(PrattParser *parser, AstLookup *lookup);
static AstArg *astSymbolToFarg(ParserInfo, HashSymbol *);
static AstArg *astNumberToFarg(ParserInfo, MaybeBigInt *);
static AstArg *astCharacterToFarg(ParserInfo, Character);
static AstArg *astTupleToFarg(PrattParser *parser, AstExpressions *tuple);
static AstArg *astStructureToFarg(PrattParser *parser, AstStruct *structure);
static AstArg *astExpressionToFarg(PrattParser *parser, AstExpression *expr);
static AstArgList *astExpressionsToArgList(PrattParser *parser, AstExpressions *exprs);
static AstDefinitions *prattParseLink(PrattParser *, char *);
static AstNest *top(PrattParser *parser);

static AstFileIdArray *fileIdStack = NULL;

#ifdef DEBUG_PRATT_PARSER
void disablePrattDebug(void) {
    DEBUGGING_OFF();
}
#endif

static AstExpression *errorExpression(ParserInfo I) {
    return newAstExpression_Symbol(I, TOK_ERROR());
}

static void addRecord(PrattTable *table, HashSymbol *tok,
                      PrattOp prefix, int prefixPrec,
                      PrattOp infix, int infixPrec,
                      PrattOp postfix, int postfixPrec) {
    PrattRecord *record = newPrattRecord(tok, prefix, prefixPrec, infix, infixPrec, postfix, postfixPrec);
    int save = PROTECT(record);
    setPrattTable(table, record->symbol, record);
    UNPROTECT(save);
}

static PrattParser *makePrattParser(void) {
    PrattParser *parser = newPrattParser(NULL);
    int save = PROTECT(parser);
    PrattTable *table = parser->rules;
    addRecord(table, TOK_SEMI(),      NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_PREFIX(),    NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_INFIX(),     NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_POSTFIX(),   NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_ATOM(),      makeAtom, 0,   NULL, 0,         NULL, 0);
    addRecord(table, TOK_CHAR(),      makeChar, 0,   NULL, 0,         NULL, 0);
    addRecord(table, TOK_NUMBER(),    makeNumber, 0, NULL, 0,         NULL, 0);
    addRecord(table, TOK_STRING(),    makeString, 0, NULL, 0,         NULL, 0);
    addRecord(table, TOK_TYPEDEF(),   NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_PRINT(),     print, 0,      NULL, 0,         NULL, 0);
    addRecord(table, TOK_BACK(),      back, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_ASSERT(),    passert, 0,    NULL, 0,         NULL, 0);
    addRecord(table, TOK_UNSAFE(),    unsafe, 0,     NULL, 0,         NULL, 0);
    addRecord(table, TOK_FN(),        fn, 0,         NULL, 0,         NULL, 0);
    addRecord(table, TOK_MACRO(),     macro, 0,      NULL, 0,         NULL, 0);
    addRecord(table, TOK_LINK(),      NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_SWITCH(),    switchExp, 0,  NULL, 0,         NULL, 0);
    addRecord(table, TOK_AS(),        NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_ALIAS(),     NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_KW_ERROR(),  error, 0,      NULL, 0,         NULL, 0);
    addRecord(table, TOK_NAMESPACE(), NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_LET(),       NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_IN(),        NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_TUPLE(),     tuple, 0,      NULL, 0,         NULL, 0);
    addRecord(table, TOK_CLOSE(),     NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_IF(),        iff, 0,        NULL, 0,         NULL, 0);
    addRecord(table, TOK_ELSE(),      NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_LSQUARE(),   list, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_RSQUARE(),   NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_COMMA(),     NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_LCURLY(),    nestexpr, 0,   NULL, 0,         NULL, 0);
    addRecord(table, TOK_RCURLY(),    NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_PIPE(),      NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_WILDCARD(),  wildcard, 0,   NULL, 0,         NULL, 0);
    addRecord(table, TOK_KW_NUMBER(), NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_KW_CHAR(),   NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_EOF(),       NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_ERROR(),     NULL, 0,       NULL, 0,         NULL, 0);

    addRecord(table, TOK_ARROW(),     NULL, 0,       infixRight, 10,  NULL, 0);

    addRecord(table, TOK_ASSIGN(),    NULL, 0,       exprAlias, 60,   NULL, 0);

    addRecord(table, TOK_COLON(),     NULL, 0,       infixLeft, 70,   NULL, 0);

    addRecord(table, TOK_HASH(),      doPrefix, 120, NULL, 0,         NULL, 0);

    addRecord(table, TOK_OPEN(),      grouping, 0,   call, 130,       NULL, 0);

    addRecord(table, TOK_PERIOD(),    NULL, 0,       lookup, 140,     NULL, 0);

    addRecord(table, TOK_DOLLAR(),    gensym, 0,     NULL, 150,       NULL, 0);

    parser->trie = makePrattTrie(parser, NULL);
    UNPROTECT(save);
    return parser;
}

static AstCompositeFunction *makeAstCompositeFunction(AstAltFunction *functions, AstCompositeFunction *rest) {
    int save = PROTECT(NULL);
    for (AstAltArgs *args = functions->altArgs; args != NULL; args = args->next) {
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

static PrattUnicode *PrattUTF8ToUnicode(PrattUTF8 *utf8) {
    PrattUnicode *uni = newPrattUnicode();
    int save = PROTECT(uni);
    unsigned char *entry = utf8->entries;
    while (*entry != 0) {
        Character c;
        entry = utf8_to_unicode_char(&c, entry);
        pushPrattUnicode(uni, c);
    }
    UNPROTECT(save);
    return uni;
}

static AgnosticFileId *tryFile(char *prefix, char *file) {
    char *buf = malloc(sizeof(char) * (strlen(prefix) + 1 + strlen(file) + 10));
    if (buf == NULL) {
        perror("out of memory");
        exit(1);
    }
    sprintf(buf, "%s/%s", prefix, file);
    AgnosticFileId *result = makeAgnosticFileId(buf);
    if (result == NULL) free(buf);
    return result;
}

static AgnosticFileId *searchForFile(char *initialPrefix, char *fileToFind) {
    AgnosticFileId *result = NULL;
    result = tryFile(initialPrefix, fileToFind);
    if (result != NULL) return result;
    if (include_paths != NULL) {
        for (Index i = 0; i < include_paths->size; i++) {
            result = tryFile(include_paths->entries[i], fileToFind);
            if (result != NULL) return result;
        }
    }
    return NULL;
}

static char *currentPrattFile(PrattParser *parser) {
    char *no_file = "no_file";
    if (parser == NULL) return no_file;
    if (parser->lexer == NULL) return no_file;
    if (parser->lexer->bufList == NULL) return no_file;
    return parser->lexer->bufList->filename->name;
}

static AgnosticFileId *calculatePath(unsigned char *file, PrattParser *parser) {
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

static bool fileIdInArray(AgnosticFileId *id, AstFileIdArray *array) {
    for (Index i = 0; i < array->size; ++i) {
        if (cmpAgnosticFileId(id, array->entries[i]) == CMP_EQ) {
            return true;
        }
    }
    return false;
}

static void synchronize(PrattParser *parser) {
    if (!parser->panicMode) return;
    parser->panicMode = false;
    for (;;) {
        if (check(parser, TOK_EOF())) return;
        if (check(parser, TOK_SWITCH())) return;
        if (check(parser, TOK_FN())) return;
        if (check(parser, TOK_MACRO())) return;
        if (check(parser, TOK_IF())) return;
        if (check(parser, TOK_PRINT())) return;
        if (check(parser, TOK_IN())) return;
        if (check(parser, TOK_LINK())) return;
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
// 3. parse a string after parsing the prelude, allows a -e style command line option
// 4. parse a linked file (within the parser)

AstNest *prattParseStandaloneString(char *data, char *name) {
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromString(data, name);
    AstNest *nest = top(parser);
    UNPROTECT(save);
    return nest;
}

// create a parser and a lexer
// parse the preamble with them
// create a child parser
// give it the lexer for the main data
// parse the main data
// collect namespaces etc.
static AstProg *prattParseThing(PrattLexer *thing) {
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromString((char *) preamble, "preamble");
    AstDefinitions *definitions = NULL;
    AstNest *nest = top(parser);
    if (parser->lexer->bufList != NULL) {
        parserError(parser, "unconsumed tokens");
    }
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
    AstExpression *expression = newAstExpression_Nest(CPI(nest), nest);
    PROTECT(expression);
    AstExpressions *exprs = newAstExpressions(CPI(expression), expression, NULL);
    PROTECT(exprs);
    nest = newAstNest(CPI(expression), definitions, exprs);
    AstProg *prog = astNestToProg(nest);
    UNPROTECT(save);
    return prog;
}

AstProg *prattParseFile(char *file) {
    PrattLexer *lexer = makePrattLexerFromFilename(file);
    int save = PROTECT(lexer);
    AstProg *prog = prattParseThing(lexer);
    UNPROTECT(save);
    return prog;
}

AstProg *prattParseString(char *data, char *name) {
    PrattLexer *lexer = makePrattLexerFromString(data, name);
    int save = PROTECT(lexer);
    AstProg *prog = prattParseThing(lexer);
    UNPROTECT(save);
    return prog;
}

static AstDefinitions *prattParseLink(PrattParser *parser, char *file) {
    parser = newPrattParser(parser->next); // linked files should not see the linking file's parse env
    int save = PROTECT(parser);
    parser->lexer = makePrattLexerFromFilename(file);
    AstDefinitions *definitions = NULL;
    AstNest *nest = top(parser);
    if (nest) {
        definitions = nest->definitions;
    }
    UNPROTECT(save);
    return definitions;
}

static bool findNamespace(PrattParser *parser, HashSymbol *symbol, int *result) {
    if (parser == NULL) return false;
    if (getPrattIntTable(parser->namespaces, symbol, result)) return true;
    return findNamespace(parser->next, symbol, result);
}

static void storeNamespace(PrattParser *parser, AstNamespace *ns) {
    if (findNamespace(parser, ns->symbol, NULL)) {
        parserError(parser, "redefinition of namespace %s", ns->symbol->name);
    } else {
        setPrattIntTable(parser->namespaces, ns->symbol, ns->reference);
    }
}

int initFileIdStack() {
    if (fileIdStack == NULL) {
        fileIdStack = newAstFileIdArray();
    }
    return PROTECT(fileIdStack);
}

// Careful. Somewhat accidentally this algorithm stores the namespaces
// in the order that they need to be processed.
// Specifically because a namespace is parsed before it is recorded,
// all of its imports are recorded ahead of it.
static AstNamespace *parseLink(PrattParser *parser, unsigned char *file, HashSymbol *symbol) {
    // check the file exists
    AgnosticFileId *fileId = calculatePath(file, parser);
    int save = PROTECT(fileId);
    if (fileId == NULL) {
        parserError(parser, "cannot find file \"%s\"", file);
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    // see if we've already parsed it, if so return the existing namespace id
    int found = lookupNamespace(fileId);
    if (found != -1) {
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, found);
        UNPROTECT(save);
        return ns;
    }
    // check for a recursive include
    if (fileIdInArray(fileId, fileIdStack)) {
        parserError(parser, "recursive include detected for %s", fileId->name);
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    // protect against recursive include
    pushAstFileIdArray(fileIdStack, fileId);
    // parse the file
    AstDefinitions *definitions = prattParseLink(parser, fileId->name);
    PROTECT(definitions);
    if (definitions == NULL) {
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    // save the new namespace
    AstNamespaceImpl *impl = newAstNamespaceImpl(BUFPI(parser->lexer->bufList), fileId, definitions);
    PROTECT(impl);
    found = pushAstNamespaceArray(namespaces, impl);
    // un-protect against recursive include
    popAstFileIdArray(fileIdStack);
    // return the id of the namespace
    AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, found);
    UNPROTECT(save);
    return ns;
}

static PrattTrie *makePrattTrie(PrattParser *parser, PrattTrie *C) {
    HashSymbol *tok;
    Index i = 0;
    int save = PROTECT(parser); // not C because we need to have a slot for REPLACE_PROTECT
    while ((tok = iteratePrattTable(parser->rules, &i, NULL)) != NULL) {
        C = insertPrattTrie(C, tok);
        REPLACE_PROTECT(save, C);
    }
    UNPROTECT(save);
    return C;
}

static AstExpression *makePrattBinary(ParserInfo I, HashSymbol *op, AstExpression *lhs, AstExpression *rhs) {
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

static AstExpression *makePrattUnary(ParserInfo I, HashSymbol *op, AstExpression *expr) {
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

static PrattParser *makeChildParser(PrattParser *parent) {
    PrattParser *child = newPrattParser(parent);
    child->lexer = parent->lexer;
    return child;
}

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

static AstNest *nest_body(PrattParser *parser, HashSymbol *terminal) {
    ENTER(nest_body);
    AstNest *res = NULL;
    int save = PROTECT(parser);
    if (match(parser, TOK_LET())) {
        AstDefinitions *defs = definitions(parser, TOK_IN());
        save = PROTECT(defs);
        consume(parser, TOK_IN());
        AstExpressions *stats = statements(parser, terminal);
        PROTECT(stats);
        res = newAstNest(CPI(defs), defs, stats);
    } else if (match(parser, TOK_NAMESPACE())) {
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

static AstExpressions *statements(PrattParser *parser, HashSymbol *terminal) {
    ENTER(statements);
    AstExpression *expr = expression(parser);
    int save = PROTECT(expr);
    AstExpressions *this = NULL;
    if (check(parser, TOK_SEMI())) {
        next(parser);
        validateLastAlloc();
        if (check(parser, terminal)) {
            this = newAstExpressions(CPI(expr), expr, NULL);
        } else {
            AstExpressions *rest = statements(parser, terminal);
            PROTECT(rest);
            this = newAstExpressions(CPI(expr), expr, rest);
        }
    } else {
        this = newAstExpressions(CPI(expr), expr, NULL);
    }
    LEAVE(statements);
    UNPROTECT(save);
    return this;
}

static AstExpression *expression(PrattParser *parser) {
    AstExpression *res = expr_bp(parser, 0);
    int save = PROTECT(res);
    synchronize(parser);
    UNPROTECT(save);
    return res;
}

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
    AstDefinitions *next = definitions(parser, terminal);
    PROTECT(next);
    AstDefinitions *this = newAstDefinitions(CPI(def), def, next);
    LEAVE(definitions);
    UNPROTECT(save);
    return this;
}

static void validateOperator(PrattParser *parser, PrattUTF8 *operator) {
    if (strlen((char *) operator->entries) == 0) {
        parserError(parser, "operator cannot be empty string");
    } else if (isdigit(operator->entries[0])) {
        parserError(parser, "operator cannot start with a numeric digit");
    } else if (utf8_isopen(operator->entries)) {
        parserError(parser, "operator cannot start with an opening bracket");
    } else if (utf8_isclose(operator->entries)) {
        parserError(parser, "operator cannot start with a closing bracket");
    } else {
        for (Index i = 0; i < operator->size; i++) {
            if (isspace(operator->entries[i])) {
                parserError(parser, "operator cannot contain whitespace");
                break;
            }
        }
    }
}

static void addOperator(PrattParser *parser,
                        PrattFixity fixity,
                        bool left,
                        int precedence,
                        PrattUTF8 *operator,
                        AstExpression *impl) {
    HashSymbol *op = newSymbol((char *) operator->entries);
    PrattRecord *record = fetchRecord(parser, op, false);
    int save = PROTECT(record);
    if (record) {
        record = copyPrattRecord(record);
        PROTECT(record);
        switch (fixity) {
            case PRATTFIXITY_TYPE_PREFIX: {
                if (record->prefixOp) {
                    parserErrorAt(CPI(impl),
                                  parser,
                                  "attempt to redefine prefix operator \"%s\"",
                                  operator->entries);
                }
                record->prefixOp = userPrefix;
                record->prefixPrec = precedence;
                record->prefixImpl = impl;
            }
            break;
            case PRATTFIXITY_TYPE_INFIX: {
                if (record->infixOp) {
                    parserErrorAt(CPI(impl),
                                  parser,
                                  "attempt to redefine infix operator \"%s\"",
                                  operator->entries);
                } else if (record->postfixOp) {
                    parserErrorAt(CPI(impl),
                                  parser,
                                  "attempt to define existing postfix operator \"%s\" as infix",
                                  operator->entries);
                }
                record->infixOp = left ? userInfixLeft : userInfixRight;
                record->infixPrec = precedence;
                record->infixImpl = impl;
            }
            break;
            case PRATTFIXITY_TYPE_POSTFIX: {
                if (record->postfixOp) {
                    parserErrorAt(CPI(impl),
                                  parser,
                                  "attempt to redefine postfix operator \"%s\"",
                                  operator->entries);
                } else if (record->infixOp) {
                    parserErrorAt(CPI(impl),
                                  parser,
                                  "attempt to define existing infix operator \"%s\" as postfix",
                                  operator->entries);
                }
                record->postfixOp = userPostfix;
                record->postfixPrec = precedence;
                record->postfixImpl = impl;
            }
            break;
        }
    } else {
        switch (fixity) {
            case PRATTFIXITY_TYPE_PREFIX: {
                record = newPrattRecord(op, userPrefix, precedence, NULL, 0, NULL, 0);
                PROTECT(record);
                record->prefixImpl = impl;
            }
            break;
            case PRATTFIXITY_TYPE_INFIX: {
                record = newPrattRecord(op, NULL, 0, left ? userInfixLeft : userInfixRight, precedence, NULL, 0);
                PROTECT(record);
                record->infixImpl = impl;
            }
            break;
            case PRATTFIXITY_TYPE_POSTFIX: {
                record = newPrattRecord(op, NULL, 0, NULL, 0, userPostfix, precedence);
                PROTECT(record);
                record->postfixImpl = impl;
            }
            break;
        }
        parser->trie = insertPrattTrie(parser->trie, op);
    }
    if (record) {
        // hoist - might have come from parent env
        setPrattTable(parser->rules, op, record);
    }
    UNPROTECT(save);
}

static AstDefinition *postfix(PrattParser *parser) {
    ENTER(postfix);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    consume(parser, TOK_POSTFIX());
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
            addOperator(parser, PRATTFIXITY_TYPE_POSTFIX, false, precedence, str, impl);
        } else {
            parserErrorAt(TOKPI(prec), parser, "expected small integer");
        }
    }
    LEAVE(postfix);
    UNPROTECT(save);
    return newAstDefinition_Blank(TOKPI(tok));
}

static AstDefinition *prefix(PrattParser *parser) {
    ENTER(prefix);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    consume(parser, TOK_PREFIX());
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
            addOperator(parser, PRATTFIXITY_TYPE_PREFIX, false, precedence, str, impl);
        } else {
            parserErrorAt(TOKPI(prec), parser, "expected small integer");
        }
    }
    LEAVE(prefix);
    UNPROTECT(save);
    return newAstDefinition_Blank(TOKPI(tok));
}

static AstDefinition *infix(PrattParser *parser) {
    ENTER(infix);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    consume(parser, TOK_INFIX());
    if (check(parser, TOK_ATOM())) {
        PrattToken *atom = next(parser);
        PROTECT(atom);
#ifdef SAFETY_CHECKS
        if (atom->value->type != PRATTVALUE_TYPE_ATOM) {
            cant_happen("unexpected %s", prattValueTypeName(atom->value->type));
        }
#endif
        bool leftassoc = true;
        if (atom->value->val.atom == TOK_LEFT()) {
            leftassoc = true;
        } else if (atom->value->val.atom == TOK_RIGHT()) {
            leftassoc = false;
        } else {
            parserErrorAt(TOKPI(atom), parser, "expected \"left\" or \"right\" after infix keyword");
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
                addOperator(parser, PRATTFIXITY_TYPE_INFIX, leftassoc, precedence, str, impl);
            } else {
                parserErrorAt(TOKPI(prec), parser, "expected small integer");
            }
        }
    }
    LEAVE(infix);
    UNPROTECT(save);
    return newAstDefinition_Blank(TOKPI(tok));
}

static AstDefinition *definition(PrattParser *parser) {
    ENTER(definition);
    AstDefinition *res = NULL;
    if (check(parser, TOK_ATOM())) {
        res = assignment(parser);
    } else if (check(parser, TOK_DOLLAR())) {
        next(parser);
        res = gensym_assignment(parser);
    } else if (check(parser, TOK_TYPEDEF())) {
        res = typedefinition(parser);
    } else if (check(parser, TOK_UNSAFE())) {
        next(parser);
        validateLastAlloc();
        consume(parser, TOK_FN());
        res = defun(parser, true, false);
    } else if (check(parser, TOK_FN())) {
        next(parser);
        validateLastAlloc();
        res = defun(parser, false, false);
    } else if (check(parser, TOK_PRINT())) {
        next(parser);
        validateLastAlloc();
        res = defun(parser, false, true);
    } else if (check(parser, TOK_MACRO())) {
        next(parser);
        res = defmacro(parser);
    } else if (check(parser, TOK_LINK())) {
        res = link(parser);
    } else if (check(parser, TOK_ALIAS())) {
        res = alias(parser);
    } else if (check(parser, TOK_PREFIX())) {
        res = prefix(parser);
    } else if (check(parser, TOK_INFIX())) {
        res = infix(parser);
    } else if (check(parser, TOK_POSTFIX())) {
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

static AstDefinition *alias(PrattParser *parser) {
    ENTER(alias);
    consume(parser, TOK_ALIAS());
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

static HashSymbol *type_variable(PrattParser *parser) {
    ENTER(type_variable);
    HashSymbol *res = NULL;
    consume(parser, TOK_HASH());
    res = symbol(parser);
    LEAVE(type_variable);
    return res;
}

static AstTypeList *type_list(PrattParser *parser) {
    ENTER(type_list);
    AstType *type = type_type(parser);
    int save = PROTECT(type);
    AstTypeList *this = newAstTypeList(CPI(type), type, NULL);
    PROTECT(this);
    if (match(parser, TOK_COMMA())) {
        this->next = type_list(parser);
    }
    LEAVE(type_list);
    UNPROTECT(save);
    return this;
}

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
        this->next = type_map(parser);
    }
    LEAVE(type_map);
    UNPROTECT(save);
    return this;
}

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

static AstTypeFunction *type_function(PrattParser *parser) {
    ENTER(type_function);
    AstLookupOrSymbol *name = scoped_symbol(parser);
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

static AstAltArgs *alt_args(PrattParser *parser) {
    ENTER(alt_args);
    PrattToken *tok= peek(parser);
    int save = PROTECT(tok);
    AstArgList *args = fargs(parser);
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

static AstLookupOrSymbol *scoped_symbol(PrattParser *parser) {
    ENTER(scoped_symbol);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *sym1 = symbol(parser);
    if (match(parser, TOK_PERIOD())) {
        HashSymbol *sym2 = symbol(parser);
        int index = 0;
        if (findNamespace(parser, sym1, &index)) {
            AstLookupSymbol *lus = newAstLookupSymbol(TOKPI(tok), index, sym1, sym2);
            PROTECT(lus);
            AstLookupOrSymbol *res = newAstLookupOrSymbol_Lookup(CPI(lus), lus);
            LEAVE(scoped_symbol);
            UNPROTECT(save);
            return res;
        } else {
            parserError(parser, "cannot resolve namespace %s", sym1->name);
        }
    }
    AstLookupOrSymbol *res = newAstLookupOrSymbol_Symbol(TOKPI(tok), sym1);
    LEAVE(scoped_symbol);
    UNPROTECT(save);
    return res;
}

static AstArgList *fargs(PrattParser *parser) {
    ENTER(fargs);
    consume(parser, TOK_OPEN());
    AstExpressions *exprs = expressions(parser);
    int save = PROTECT(exprs);
    consume(parser, TOK_CLOSE());
    AstArgList *args = astExpressionsToArgList(parser, exprs);
    LEAVE(fargs);
    UNPROTECT(save);
    return args;
}

static AstLookupSymbol *astLookupToLus(PrattParser *parser, AstLookup *lookup) {
    AstLookupSymbol *lus = newAstLookupSymbol(CPI(lookup), lookup->nsid, lookup->nsSymbol, NULL);
    int save = PROTECT(lus);
    switch (lookup->expression->type) {
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

static AstLookupOrSymbol *astLookupToLos(PrattParser *parser, AstLookup *lookup) {
    AstLookupSymbol *lus = astLookupToLus(parser, lookup);
    int save = PROTECT(lus);
    AstLookupOrSymbol *res = newAstLookupOrSymbol_Lookup(CPI(lus), lus);
    UNPROTECT(save);
    return res;
}

static AstLookupOrSymbol *makeLosError(ParserInfo PI) {
    return newAstLookupOrSymbol_Symbol(PI, TOK_ERROR());
}

static AstLookupOrSymbol *astSymbolToLos(ParserInfo PI, HashSymbol *symbol) {
    return newAstLookupOrSymbol_Symbol(PI, symbol);
}

static AstLookupOrSymbol *astFunctionToLos(PrattParser *parser, AstExpression *function) {
    switch (function->type) {
        case AST_EXPRESSION_TYPE_BACK:
            parserErrorAt(CPI(function), parser, "invalid use of \"back\" as structure name");
            return makeLosError(CPI(function));
        case AST_EXPRESSION_TYPE_FUNCALL:
            parserErrorAt(CPI(function), parser, "invalid use of function call as structure name");
            return makeLosError(CPI(function));
        case AST_EXPRESSION_TYPE_LOOKUP:
            return astLookupToLos(parser, function->val.lookup);
        case AST_EXPRESSION_TYPE_SYMBOL:
            return astSymbolToLos(CPI(function), function->val.symbol);
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
        case AST_EXPRESSION_TYPE_GENSYM:
            parserErrorAt(CPI(function), parser, "invalid use of macro symbol as structure name");
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

static AstArg *astFunCallToFarg(PrattParser *parser, AstFunCall *funCall) {
    AstLookupOrSymbol *los = astFunctionToLos(parser, funCall->function);
    int save = PROTECT(los);
    AstArgList *args = astExpressionsToArgList(parser, funCall->arguments);
    PROTECT(args);
    AstUnpack *unpack = newAstUnpack(CPI(los), los, args);
    PROTECT(unpack);
    AstArg *res = newAstArg_Unpack(CPI(unpack), unpack);
    UNPROTECT(save);
    return res;
}

static AstArg *astLookupToFarg(PrattParser *parser, AstLookup *lookup) {
    AstLookupSymbol *lus = astLookupToLus(parser, lookup);
    int save = PROTECT(lus);
    AstArg *res = newAstArg_Lookup(CPI(lus), lus);
    UNPROTECT(save);
    return res;
}

static AstArg *astSymbolToFarg(ParserInfo PI, HashSymbol *symbol) {
    return newAstArg_Symbol(PI, symbol);
}

static AstArg *astNumberToFarg(ParserInfo PI, MaybeBigInt *bi) {
    return newAstArg_Number(PI, bi);
}

static AstArg *astCharacterToFarg(ParserInfo PI, Character c) {
    return newAstArg_Character(PI, c);
}

static AstArgList *astExpressionsToArgList(PrattParser *parser, AstExpressions *exprs) {
    if (exprs == NULL) return NULL;
    AstArgList *next = astExpressionsToArgList(parser, exprs->next);
    int save = PROTECT(next);
    AstArg *arg = astExpressionToFarg(parser, exprs->expression);
    PROTECT(arg);
    AstArgList *this = newAstArgList(CPI(arg), arg, next);
    UNPROTECT(save);
    return this;
}

static AstArg *astTupleToFarg(PrattParser *parser, AstExpressions *tuple) {
    AstArgList *args = astExpressionsToArgList(parser, tuple);
    int save = PROTECT(args);
    AstArg *res = newAstArg_Tuple(CPI(tuple), args);
    UNPROTECT(save);
    return res;
}

static AstTaggedArgList *astTaggedExpressionsToTaggedArgList(PrattParser *parser,
                                                             AstTaggedExpressions *exprs) {
    if (exprs == NULL) return NULL;
    AstTaggedArgList *next = astTaggedExpressionsToTaggedArgList(parser, exprs->next);
    int save = PROTECT(next);
    AstArg *arg = astExpressionToFarg(parser, exprs->expression);
    PROTECT(arg);
    AstTaggedArgList *this = newAstTaggedArgList(CPI(arg), exprs->tag, arg, next);
    UNPROTECT(save);
    return this;
}

static AstArg *astStructureToFarg(PrattParser *parser, AstStruct *structure) {
    AstTaggedArgList *args = astTaggedExpressionsToTaggedArgList(parser, structure->expressions);
    int save = PROTECT(args);
    AstUnpackStruct *unpack = newAstUnpackStruct(CPI(structure), structure->symbol, args);
    PROTECT(unpack);
    AstArg *res = newAstArg_UnpackStruct(CPI(unpack), unpack);
    UNPROTECT(save);
    return res;
}

static AstArg *astAliasToFarg(PrattParser *parser, AstExprAlias *alias) {
    AstArg *arg = astExpressionToFarg(parser, alias->value);
    int save = PROTECT(arg);
    AstNamedArg *narg = newAstNamedArg(CPI(alias), alias->name, arg);
    PROTECT(narg);
    AstArg *res = newAstArg_Named(CPI(narg), narg);
    UNPROTECT(save);
    return res;
}

static AstArg *astExpressionToFarg(PrattParser *parser, AstExpression *expr) {
    switch (expr->type) {
        case AST_EXPRESSION_TYPE_BACK:
            parserErrorAt(CPI(expr), parser, "invalid use of \"back\" as formal argument");
            return newAstArg_Wildcard(CPI(expr));
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
            return newAstArg_Wildcard(CPI(expr));
        case AST_EXPRESSION_TYPE_NEST:
            parserErrorAt(CPI(expr), parser, "invalid use of nest as formal argument");
            return newAstArg_Wildcard(CPI(expr));
        case AST_EXPRESSION_TYPE_IFF:
            parserErrorAt(CPI(expr), parser, "invalid use of conditional as formal argument");
            return newAstArg_Wildcard(CPI(expr));
        case AST_EXPRESSION_TYPE_PRINT:
            parserErrorAt(CPI(expr), parser, "invalid use of \"print\" as formal argument");
            return newAstArg_Wildcard(CPI(expr));
        case AST_EXPRESSION_TYPE_TUPLE:
            return astTupleToFarg(parser, expr->val.tuple);
        case AST_EXPRESSION_TYPE_ENV:
            cant_happen("encountered ENV as formal argument");
        case AST_EXPRESSION_TYPE_STRUCTURE:
            return astStructureToFarg(parser, expr->val.structure);
        case AST_EXPRESSION_TYPE_ASSERTION:
            parserErrorAt(CPI(expr), parser, "invalid use of \"assert\" as formal argument");
            return newAstArg_Wildcard(CPI(expr));
        case AST_EXPRESSION_TYPE_GENSYM:
            parserErrorAt(CPI(expr), parser, "invalid use of macro variable as formal argument");
            return newAstArg_Wildcard(CPI(expr));
        case AST_EXPRESSION_TYPE_ALIAS:
            return astAliasToFarg(parser, expr->val.alias);
        case AST_EXPRESSION_TYPE_WILDCARD:
            return newAstArg_Wildcard(CPI(expr));
        case AST_EXPRESSION_TYPE_ERROR:
            parserErrorAt(CPI(expr), parser, "invalid use of \"error\" as formal argument");
            return newAstArg_Wildcard(CPI(expr));
        default:
            cant_happen("unrecognised %s", astExpressionTypeName(expr->type));
    }
}

static void validateMacroArgs(PrattParser *parser, AstAltFunction *definition) {
    AstAltArgs *altArgs = definition->altArgs;
    if (altArgs->next) {
        parserErrorAt(CPI(altArgs->next), parser, "cannot supply alternative arguments to a macro");
    } else {
        AstArgList *args = altArgs->argList;
        while(args) {
            if (args->arg->type != AST_ARG_TYPE_SYMBOL) {
                parserErrorAt(CPI(args->arg), parser, "macro arguments can only be simple symbols");
                break;
            }
            args = args->next;
        }
    }
}

static AstDefinition *defmacro(PrattParser *parser) {
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
    AstDefine *def = newAstDefine(TOKPI(tok), s, expr);
    PROTECT(def);
    AstDefinition *res = newAstDefinition_Define(CPI(def), def);
    LEAVE(defun);
    UNPROTECT(save);
    return res;
}

static HashSymbol *symbol(PrattParser *parser) {
    ENTER(symbol);
    PrattToken *tok = next(parser);
    validateLastAlloc();
    int save = PROTECT(tok);
    if (tok->type != TOK_ATOM()) {
        parserError(parser, "expected ATOM");
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
static AstDefinition *gensym_assignment(PrattParser *parser) {
    ENTER(gensym_assignment);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    consume(parser, TOK_ASSIGN());
    AstExpression *expr = expression(parser);
    PROTECT(expr);
    consume(parser, TOK_SEMI());
    AstGensymDefine *def = newAstGensymDefine(TOKPI(tok), s, expr);
    PROTECT(def);
    AstDefinition *res = newAstDefinition_GensymDefine(CPI(def), def);
    LEAVE(gensym_assignment);
    UNPROTECT(save);
    return res;
}

static AstDefinition *assignment(PrattParser* parser) {
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

static AstDefinition *typedefinition(PrattParser *parser) {
    ENTER(typedefinition);
    consume(parser, TOK_TYPEDEF());
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstUserType *userType = NULL;
    if (check(parser, TOK_OPEN())) {
        next(parser);
        validateLastAlloc();
        AstTypeSymbols *variables = type_variables(parser);
        PROTECT(variables);
        consume(parser, TOK_CLOSE());
        userType = newAstUserType(TOKPI(tok), s, variables);
    } else {
        userType = newAstUserType(TOKPI(tok), s, NULL);
    }
    PROTECT(userType);
    consume(parser, TOK_LCURLY());
    AstTypeBody *typeBody = type_body(parser);
    PROTECT(typeBody);
    consume(parser, TOK_RCURLY());
    AstTypeDef *typeDef = newAstTypeDef(CPI(userType), userType, typeBody);
    PROTECT(typeDef);
    AstDefinition *res = newAstDefinition_TypeDef(CPI(typeDef), typeDef);
    LEAVE(typedefinition);
    UNPROTECT(save);
    return res;
}

static AstTypeBody *type_body(PrattParser *parser) {
    ENTER(typedefinition);
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
    LEAVE(typedefinition);
    UNPROTECT(save);
    return this;
}

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
        AstTypeSymbols *rest = type_variables(parser);
        PROTECT(rest);
        t = newAstTypeSymbols(TOKPI(tok), s, rest);
    }
    LEAVE(type_variables);
    UNPROTECT(save);
    return t;
}

static AstDefinition *link(PrattParser *parser) {
    ENTER(link);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    consume(parser, TOK_LINK());
    PrattUTF8 *path = rawString(parser);
    PROTECT(path);
    AstDefinition *res = NULL;
    if (path == NULL) {
        res = newAstDefinition_Blank(TOKPI(tok));
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

static PrattUTF8 *rawString(PrattParser *parser) {
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
        PrattUTF8 *err = newPrattUTF8();
        int save = PROTECT(err);
        pushPrattUTF8(err, 0);
        LEAVE(rawString);
        UNPROTECT(save);
        return err;
    }
}

static void appendString(PrattParser *parser, PrattUTF8 *this) {
    ENTER(appendString);
    PrattUTF8 *next = rawString(parser);
    int save = PROTECT(next);
    this->size--; // backup over '\0'
    for (Index i = 0; i < next->size; i++) {
        pushPrattUTF8(this, next->entries[i]);
    }
    UNPROTECT(save);
    if (check(parser, TOK_STRING())) {
        appendString(parser, this);
    }
    LEAVE(appendString);
}

static PrattUTF8 *str(PrattParser *parser) {
    ENTER(str);
    PrattUTF8 *this = rawString(parser);
    int save = PROTECT(this);
    if (check(parser, TOK_STRING())) {
        appendString(parser, this);
    }
    LEAVE(str);
    UNPROTECT(save);
    return this;
}

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

static PrattRecord *fetchRecord(PrattParser *parser, HashSymbol *symbol, bool fatal) {
    PrattRecord *record = NULL;
    if (getPrattTable(parser->rules, symbol, &record)) {
        return record;
    } else if (parser->next != NULL) {
        return fetchRecord(parser->next, symbol, fatal);
    } else {
        if (fatal) {
            cant_happen("unrecognised op %s", symbol->name);
        } else {
            return NULL;
        }
    }
}

static AstExpression *grouping(PrattRecord *record, PrattParser *parser, AstExpression *lhs __attribute__((unused)),
PrattToken *tok __attribute__((unused))) {
    ENTER(grouping);
    AstExpression *res = expr_bp(parser, record->prefixPrec);
    int save = PROTECT(res);
    consume(parser, TOK_CLOSE());
    LEAVE(grouping);
    UNPROTECT(save);
    return res;
}

static AstFunCall *conslist(PrattParser *parser) {
    ENTER(conslist);
    AstFunCall *res = NULL;
    int save = PROTECT(res);
    if (check(parser, TOK_RSQUARE())) {
        ParserInfo PI = LEXPI(parser->lexer);
        DEBUG("conslist parser info %d %s", PI.lineno, PI.filename);
        AstExpression *nil = newAstExpression_Symbol(PI, nilSymbol());
        PROTECT(nil);
        res = newAstFunCall(PI, nil, NULL);
    } else {
        AstExpression *expr = expression(parser);
        PROTECT(expr);
        match(parser, TOK_COMMA());
        AstFunCall *rest = conslist(parser);
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

static AstExpression *list(PrattRecord *record __attribute__((unused)), PrattParser *parser, AstExpression *lhs __attribute__((unused)),
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

static AstExpression *doPrefix(PrattRecord *record, PrattParser *parser, AstExpression *lhs __attribute__((unused)),
PrattToken *tok __attribute__((unused))) {
    ENTER(doPrefix);
    AstExpression *res = expr_bp(parser, record->prefixPrec + 1);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), record->symbol, res);
    LEAVE(doPrefix);
    UNPROTECT(save);
    return res;
}

static AstExpressions *collectArguments(PrattParser *parser) {
    ENTER(collectArguments);
    AstExpression *arg = expr_bp(parser, 0);
    int save = PROTECT(arg);
    AstExpressions *next = NULL;
    if (match(parser, TOK_COMMA())) {
        next = collectArguments(parser);
        PROTECT(next);
    }
    AstExpressions *this = newAstExpressions(CPI(arg), arg, next);
    LEAVE(collectArguments);
    UNPROTECT(save);
    return this;
}

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

static AstExpression *call(PrattRecord *record __attribute__((unused)),
                           PrattParser *parser,
                           AstExpression *lhs,
                           PrattToken *tok __attribute__((unused))) {
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

static AstExpression *back(PrattRecord *record __attribute__((unused)),
                           PrattParser *parser,
                           AstExpression *lhs __attribute__((unused)),
                           PrattToken *tok __attribute__((unused))) {
    ENTER(back);
    AstExpression *res = newAstExpression_Back(LEXPI(parser->lexer));
    LEAVE(back);
    return res;
}

static AstExpression *wildcard(PrattRecord *record __attribute__((unused)),
                               PrattParser *parser,
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok __attribute__((unused))) {
    ENTER(wildcard);
    AstExpression *res = newAstExpression_Wildcard(LEXPI(parser->lexer));
    LEAVE(wildcard);
    return res;
}

static AstExpression *error(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok __attribute__((unused))) {
    ENTER(error);
    AstExpression *toError = expression(parser);
    int save = PROTECT(toError);
    AstExpression *res = newAstExpression_Error(CPI(toError), toError);
    LEAVE(error);
    UNPROTECT(save);
    return res;
}

static AstExpression *print(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok __attribute__((unused))) {
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

static AstExpression *passert(PrattRecord *record __attribute__((unused)),
                              PrattParser *parser,
                              AstExpression *lhs __attribute__((unused)),
                              PrattToken *tok __attribute__((unused))) {
    ENTER(passert);
    AstExpression *toAssert = expression(parser);
    int save = PROTECT(toAssert);
    AstExpression *res = newAstExpression_Assertion(CPI(toAssert), toAssert);
    LEAVE(passert);
    UNPROTECT(save);
    return res;
}

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

// can't actually allow anonymous macro expressions but need to ensure
// `macro` is registered as a prefix operator so that it can't be
// overridden.
static AstExpression *macro(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok) {
    parserErrorAt(TOKPI(tok), parser, "can't declare macros as expressions");
    return errorExpression(TOKPI(tok));
}

static AstExpression *gensym(PrattRecord *record __attribute__((unused)),
                             PrattParser *parser,
                             AstExpression *lhs __attribute__((unused)),
                             PrattToken *tok) {
    ENTER(gensym);
    HashSymbol *s = symbol(parser);
    AstExpression *gs = newAstExpression_Gensym(TOKPI(tok), s);
    LEAVE(gensym);
    return gs;
}

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

static AstExpression *tuple(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused)),
                            PrattToken *tok __attribute__((unused))) {
    ENTER(tuple);
    AstExpressions *args = expressions(parser);
    int save = PROTECT(args);
    consume(parser, TOK_CLOSE());
    AstExpression *res = newAstExpression_Tuple(CPI(args), args);
    LEAVE(tuple);
    UNPROTECT(save);
    return res;
}

static AstExpression *infixLeft(PrattRecord *record, PrattParser *parser, AstExpression *lhs,
PrattToken *tok __attribute__((unused))) {
    ENTER(infixLeft);
    AstExpression *rhs = expr_bp(parser, record->infixPrec + 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(lhs), record->symbol, lhs, rhs);
    LEAVE(infixLeft);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *lookup(PrattRecord *record, PrattParser *parser, AstExpression *lhs,
PrattToken *tok __attribute__((unused))) {
    ENTER(lookup);
    AstExpression *rhs = expr_bp(parser, record->infixPrec - 1);
    int save = PROTECT(rhs);
    if (lhs->type == AST_EXPRESSION_TYPE_SYMBOL) {
        int index = 0;
        if (!findNamespace(parser, lhs->val.symbol, &index)) {
            parserError(parser, "cannot resolve namespace %s", lhs->val.symbol->name);
        }
        AstLookup *lup = newAstLookup(LEXPI(parser->lexer), index, lhs->val.symbol, rhs);
        PROTECT(lup);
        rhs = newAstExpression_Lookup(CPI(lup), lup);
    } else {
        parserError(parser, "expected namespace on lhs of '.', got %s", astExpressionTypeName(lhs->type));
    }
    LEAVE(lookup);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *infixRight(PrattRecord *record, PrattParser *parser, AstExpression *lhs,
PrattToken *tok __attribute__((unused))) {
    ENTER(infixRight);
    AstExpression *rhs = expr_bp(parser, record->infixPrec - 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(rhs), record->symbol, lhs, rhs);
    LEAVE(infixRight);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *exprAlias(PrattRecord *record,
                                PrattParser *parser,
                                AstExpression *lhs,
                                PrattToken *tok __attribute__((unused))) {
    ENTER(exprAlias);
    AstExpression *rhs = expr_bp(parser, record->infixPrec - 1);
    int save = PROTECT(rhs);
    HashSymbol *alias = NULL;
    if (lhs->type == AST_EXPRESSION_TYPE_SYMBOL) {
        alias = lhs->val.symbol;
    } else {
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

static AstExpression *userPrefix(PrattRecord *record,
                                 PrattParser *parser,
                                 AstExpression *lhs __attribute__((unused)),
                                 PrattToken *tok) {
    ENTER(userPrefix);
    AstExpression *rhs = expr_bp(parser, record->prefixPrec);
    int save = PROTECT(rhs);
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    PROTECT(arguments);
    AstFunCall *funCall = newAstFunCall(TOKPI(tok), record->prefixImpl, arguments);
    PROTECT(funCall);
    rhs = newAstExpression_FunCall(CPI(funCall), funCall);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *userInfix(PrattRecord *record,
                                PrattParser *parser,
                                AstExpression *lhs,
                                PrattToken *tok,
                                int precShift) {
    ENTER(userInfix);
    AstExpression *rhs = expr_bp(parser, record->infixPrec + precShift);
    int save = PROTECT(rhs);
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    PROTECT(arguments);
    arguments = newAstExpressions(CPI(lhs), lhs, arguments);
    PROTECT(arguments);
    AstFunCall *funCall = newAstFunCall(TOKPI(tok), record->infixImpl, arguments);
    PROTECT(funCall);
    rhs = newAstExpression_FunCall(CPI(funCall), funCall);
    LEAVE(userInfix);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *userInfixLeft(PrattRecord *record,
                                    PrattParser *parser,
                                    AstExpression *lhs,
                                    PrattToken *tok) {
    return userInfix(record, parser, lhs, tok, +1);
}

static AstExpression *userInfixRight(PrattRecord *record,
                                     PrattParser *parser,
                                     AstExpression *lhs,
                                     PrattToken *tok) {
    return userInfix(record, parser, lhs, tok, -1);
}

static AstExpression *userPostfix(PrattRecord *record,
                                  PrattParser *parser __attribute__((unused)),
                                  AstExpression *lhs,
                                  PrattToken *tok) {
    AstExpressions *arguments = newAstExpressions(CPI(lhs), lhs, NULL);
    int save = PROTECT(arguments);
    AstFunCall *funCall = newAstFunCall(TOKPI(tok), record->postfixImpl, arguments);
    REPLACE_PROTECT(save, funCall);
    AstExpression *res = newAstExpression_FunCall(CPI(funCall), funCall);
    UNPROTECT(save);
    return res;
}

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
        AstExpression *iff_nest = iff(NULL, parser, NULL, NULL);
        PROTECT(iff_nest);
        AstExpressions *nest_body = newAstExpressions(CPI(iff_nest), iff_nest, NULL);
        PROTECT(nest_body);
        alternative = newAstNest(CPI(nest_body), NULL, nest_body);
    } else {
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
    return newAstExpression_Symbol(TOKPI(tok), name);
}

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

static AstExpression *makeChar(PrattRecord *record __attribute__((unused)),
                               PrattParser *parser __attribute__((unused)),
                               AstExpression *lhs __attribute__((unused)),
                               PrattToken *tok) {
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_STRING) {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    Character c;
    (void) utf8_to_unicode_char(&c, (unsigned char *) tok->value->val.string->entries);
    AstExpression *res = newAstExpression_Character(TOKPI(tok), c);
    return res;
}

static AstFunCall *makeStringList(ParserInfo PI, PrattUnicode *str) {
    AstExpression *nil = newAstExpression_Symbol(PI, nilSymbol());
    int save = PROTECT(nil);
    AstFunCall *res = newAstFunCall(PI, nil, NULL);
    PROTECT(res);
    for (int size = str->size; size > 0; size--) {
        AstExpression *character = newAstExpression_Character(PI, str->entries[size-1]);
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

static AstExpression *expr_bp(PrattParser *parser, int min_bp) {
    ENTER(expr_bp);
    AstExpression *lhs = NULL;
    PrattToken *tok = next(parser);
    int save = PROTECT(tok);
    PrattRecord *record = fetchRecord(parser, tok->type, true);
    if (record->prefixOp == NULL) {
        parserError(parser, "not a prefix operator: %s", tok->type->name);
        lhs = errorExpression(TOKPI(tok));
    } else {
        lhs = record->prefixOp(record, parser, NULL, tok);
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
            PrattRecord *record = fetchRecord(parser, op->type, true);
            if(record->postfixOp != NULL) {
                DEBUG("postfix %d %d", record->postfixPrec, min_bp);
                if (record->postfixPrec < min_bp) {
                    break;
                }
                next(parser);
                lhs = record->postfixOp(record, parser, lhs, op);
                REPLACE_PROTECT(save, lhs);
            } else if (record->infixOp != NULL) {
                DEBUG("infix %d %d", record->infixPrec, min_bp);
                if (record->infixPrec < min_bp) {
                    break;
                }
                next(parser);
                validateLastAlloc();
                lhs = record->infixOp(record, parser, lhs, op);
                REPLACE_PROTECT(save, lhs);
            } else {
                DEBUG("prefix");
                break;
            }
        }
    }
    LEAVE(expr_bp);
    UNPROTECT(save);
    return lhs;
}
