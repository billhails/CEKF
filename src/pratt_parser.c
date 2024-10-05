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

static AstExpression *grouping(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *list(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *prefix(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *prefixC(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *prefixCar(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *prefixCdr(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *postfix(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *tuple(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *unsafe(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *fn(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *call(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *infixLeft(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *infixRight(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *lookup(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *infixAppend(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
static AstExpression *infixCons(PrattRecord *, PrattParser *, AstExpression *, PrattToken *);
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

static AstDefinitions *definitions(PrattParser *, HashSymbol *);
static AstExpressions *statements(PrattParser *, HashSymbol *);
static AstExpression *expression(PrattParser *);
static AstDefinition *definition(PrattParser *);
static AstDefinition *assignment(PrattParser *);
static AstDefinition *typedefinition(PrattParser *);
static AstDefinition *defun(PrattParser *, bool, bool);
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
static AstArg *farg(PrattParser *);
static AstTaggedArgList *tagged_fargs(PrattParser *);
static AstTaggedArgList *tagged_farg(PrattParser *);
static AstUnpack *consfargs(PrattParser *);
static AstUnpack *stringarg(PrattParser *);
static AstFunCall *switchFC(PrattParser *parser);
static PrattUnicode *PrattUTF8ToUnicode(PrattUTF8 *);
static HashSymbol *substitute(PrattParser *parser, HashSymbol *symbol);
static void synchronize(PrattParser *parser);

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

PrattParser *makePrattParser() {
    PrattParser *res = newPrattParser(NULL, NULL);
    int save = PROTECT(res);
    PrattTable *table = res->rules;
    addRecord(table, TOK_SEMI(),      NULL, 0,       NULL, 0,         NULL, 0);
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
    addRecord(table, TOK_LINK(),      NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_SWITCH(),    switchExp, 0,  NULL, 0,         NULL, 0);
    addRecord(table, TOK_AS(),        NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_ALIAS(),     NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_KW_ERROR(),  error, 0,      NULL, 0,         NULL, 0);
    addRecord(table, TOK_NS(),        NULL, 0,       NULL, 0,         NULL, 0);
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
    addRecord(table, TOK_WILDCARD(),  NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_KW_NUMBER(), NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_KW_CHAR(),   NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_EOF(),       NULL, 0,       NULL, 0,         NULL, 0);
    addRecord(table, TOK_ERROR(),     NULL, 0,       NULL, 0,         NULL, 0);

    addRecord(table, TOK_ARROW(),     NULL, 0,       infixRight, 10,  NULL, 0);

    addRecord(table, TOK_THEN(),      NULL, 0,       infixRight, 20,  NULL, 0);

    addRecord(table, TOK_AND(),       NULL, 0,       infixLeft, 30,   NULL, 0);
    addRecord(table, TOK_OR(),        NULL, 0,       infixLeft, 30,   NULL, 0);
    addRecord(table, TOK_XOR(),       NULL, 0,       infixLeft, 30,   NULL, 0);
    addRecord(table, TOK_NAND(),      NULL, 0,       infixLeft, 30,   NULL, 0);
    addRecord(table, TOK_NOR(),       NULL, 0,       infixLeft, 30,   NULL, 0);
    addRecord(table, TOK_XNOR(),      NULL, 0,       infixLeft, 30,   NULL, 0);

    addRecord(table, TOK_NOT(),       prefix, 40,    NULL, 0,         NULL, 0);

    addRecord(table, TOK_EQ(),        NULL, 0,       infixLeft, 50,   NULL, 0);
    addRecord(table, TOK_NE(),        NULL, 0,       infixLeft, 50,   NULL, 0);
    addRecord(table, TOK_GT(),        prefixCdr, 50, infixLeft, 50,   NULL, 0);
    addRecord(table, TOK_LT(),        prefixCar, 50, infixLeft, 50,   NULL, 0);
    addRecord(table, TOK_GE(),        NULL, 0,       infixLeft, 50,   NULL, 0);
    addRecord(table, TOK_LE(),        NULL, 0,       infixLeft, 50,   NULL, 0);
    addRecord(table, TOK_CMP(),       NULL, 0,       infixLeft, 50,   NULL, 0);

    addRecord(table, TOK_ASSIGN(),    NULL, 0,       infixRight, 60,  NULL, 0);

    addRecord(table, TOK_COLON(),     NULL, 0,       infixLeft, 70,   NULL, 0);

    addRecord(table, TOK_APPEND(),    NULL, 0,       infixAppend, 80, NULL, 0);

    addRecord(table, TOK_CONS(),      NULL, 0,       infixCons, 90,   NULL, 0);
    addRecord(table, TOK_PLUS(),      prefixC, 90,   infixLeft, 90,   NULL, 0);
    addRecord(table, TOK_MINUS(),     prefixC, 90,   infixLeft, 90,   NULL, 0);

    addRecord(table, TOK_TIMES(),     NULL, 0,       infixLeft, 100,  NULL, 0);
    addRecord(table, TOK_DIVIDE(),    NULL, 0,       infixLeft, 100,  NULL, 0);
    addRecord(table, TOK_MOD(),       NULL, 0,       infixLeft, 100,  NULL, 0);

    addRecord(table, TOK_EXP(),       NULL, 0,       infixRight, 110, NULL, 0);

    addRecord(table, TOK_HERE(),      prefix, 120,   NULL, 120,       NULL, 0);
    addRecord(table, TOK_HASH(),      prefix, 120,   NULL, 120,       NULL, 0);
    addRecord(table, TOK_BANG(),      NULL, 0,       NULL, 0,         postfix, 120);

    addRecord(table, TOK_OPEN(),      grouping, 0,  call, 130,        NULL, 0);

    addRecord(table, TOK_PERIOD(),    NULL, 0,      lookup, 140,      NULL, 0);

    PrattSymbolTable *replacements = res->replacements;
    setPrattSymbolTable(replacements, TOK_EQ(), eqSymbol());
    setPrattSymbolTable(replacements, TOK_NE(), neSymbol());
    setPrattSymbolTable(replacements, TOK_GT(), gtSymbol());
    setPrattSymbolTable(replacements, TOK_LT(), ltSymbol());
    setPrattSymbolTable(replacements, TOK_GE(), geSymbol());
    setPrattSymbolTable(replacements, TOK_LE(), leSymbol());
    setPrattSymbolTable(replacements, TOK_CMP(), cmpSymbol());
    setPrattSymbolTable(replacements, TOK_PLUS(), addSymbol());
    setPrattSymbolTable(replacements, TOK_MINUS(), subSymbol());
    setPrattSymbolTable(replacements, TOK_TIMES(), mulSymbol());
    setPrattSymbolTable(replacements, TOK_DIVIDE(), divSymbol());
    setPrattSymbolTable(replacements, TOK_MOD(), modSymbol());
    setPrattSymbolTable(replacements, TOK_EXP(), powSymbol());
    UNPROTECT(save);
    return res;
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

static AstUnpack *makeAstUnpack(ParserInfo I, HashSymbol *symbol, AstArgList *args) {
    AstLookupOrSymbol *los = newAstLookupOrSymbol_Symbol(I, symbol);
    int save = PROTECT(los);
    AstUnpack *res = newAstUnpack(I, los, args);
    UNPROTECT(save);
    return res;
}

static AstUnpack *makeStringUnpack(ParserInfo I, PrattUTF8 *str) {
    PrattUnicode *unicode = PrattUTF8ToUnicode(str);
    int save = PROTECT(unicode);
    AstUnpack *res = makeAstUnpack(I, nilSymbol(), NULL);
    PROTECT(res);
    for (int size = unicode->size; size > 0; size--) {
        AstArg *arg = newAstArg_Unpack(I, res);
        PROTECT(arg);
        AstArgList *args = newAstArgList(I, arg, NULL);
        PROTECT(args);
        AstArg *chr = newAstArg_Character(I, unicode->entries[size - 1]);
        PROTECT(chr);
        args = newAstArgList(I, chr, args);
        PROTECT(args);
        res = makeAstUnpack(I, consSymbol(), args);
        UNPROTECT(save);
        PROTECT(res);
    }
    return res;
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

static AstDefinitions *namespaceFromBufList(PrattParser *parser, PrattBufList *bufList) {
    PrattBufList *bl = prattBufListFromString(TOK_NS()->name, parser->lexer->bufList->filename->name, bufList);
    int save = PROTECT(bl);
    PrattLexer *lexer = newPrattLexer(bl, parser->lexer->trie);
    REPLACE_PROTECT(save, lexer);
    PrattParser *new = newPrattParser(lexer, parser);
    REPLACE_PROTECT(save, new);
    AstNest *nest = top(new);
    UNPROTECT(save);
    if (nest) return nest->definitions;
    return NULL;
}

static AstDefinitions *namespaceFromString(PrattParser *parser, char *string, char *name) {
    PrattBufList *bufList = prattBufListFromString(string, name, NULL);
    int save = PROTECT(bufList);
    AstDefinitions *res = namespaceFromBufList(parser, bufList);
    UNPROTECT(save);
    return res;
}

static AstDefinitions *namespaceFromFileName(PrattParser *parser, char *filename) {
    PrattBufList *bufList = prattBufListFromFileName(filename, NULL);
    int save = PROTECT(bufList);
    AstDefinitions *res = namespaceFromBufList(parser, bufList);
    UNPROTECT(save);
    return res;
}

AstDefinitions *prattParsePreamble(PrattParser *parser) {
    return namespaceFromString(parser, (char *) preamble, "src/preamble.fn");
}

AstNest *prattParseTopLevel(PrattParser *parser) {
    AstDefinitions *definitions = prattParsePreamble(parser);
    if (definitions == NULL) return NULL;
    int save = PROTECT(definitions);
    AstNest *nest = top(parser);
    PROTECT(nest);
    AstExpression *expression = newAstExpression_Nest(CPI(nest), nest);
    PROTECT(expression);
    AstExpressions *expressions = newAstExpressions(CPI(expression), expression, NULL);
    PROTECT(expressions);
    nest = newAstNest(CPI(expression), definitions, expressions);
    UNPROTECT(save);
    return nest;
}

static void storeNamespace(PrattParser *parser, AstNamespace *ns) {
    if (getPrattIntTable(parser->namespaces, ns->symbol, NULL)) {
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
    AgnosticFileId *fileId = calculatePath(file, parser);
    int save = PROTECT(fileId);
    if (fileId == NULL) {
        parserError(parser, "cannot find file \"%s\"", file);
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    int found = lookupNamespace(fileId);
    if (found != -1) {
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, found);
        UNPROTECT(save);
        return ns;
    }
    if (fileIdInArray(fileId, fileIdStack)) {
        parserError(parser, "recursive include detected for %s", fileId->name);
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    pushAstFileIdArray(fileIdStack, fileId);
    AstDefinitions *definitions = namespaceFromFileName(parser, fileId->name);
    PROTECT(definitions);
    if (definitions == NULL) {
        parserError(parser, "errors detected while parsing %s", fileId->name);
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, -1);
        UNPROTECT(save);
        return ns;
    }
    AstNamespaceImpl *impl = newAstNamespaceImpl(BUFPI(parser->lexer->bufList), fileId, definitions);
    PROTECT(impl);
    found = pushAstNamespaceArray(namespaces, impl);
    popAstFileIdArray(fileIdStack);
    AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, found);
    UNPROTECT(save);
    return ns;
}

PrattTrie *makePrattTrie(PrattParser *parser, PrattTrie *C) {
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

AstNest *top(PrattParser *parser) {
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
    int save = -1;
    if (match(parser, TOK_LET())) {
        AstDefinitions *defs = definitions(parser, TOK_IN());
        save = PROTECT(defs);
        consume(parser, TOK_IN());
        AstExpressions *stats = statements(parser, terminal);
        PROTECT(stats);
        res = newAstNest(CPI(defs), defs, stats);
    } else if (match(parser, TOK_NS())) {
        consume(parser, TOK_NAMESPACE());
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

static AstDefinition *definition(PrattParser *parser) {
    ENTER(definition);
    AstDefinition *res = NULL;
    if (check(parser, TOK_ATOM())) {
        res = assignment(parser);
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
    } else if (check(parser, TOK_LINK())) {
        res = link(parser);
    } else if (check(parser, TOK_ALIAS())) {
        res = alias(parser);
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
        AstTypeFunction *fn = type_function(parser);
        PROTECT(fn);
        ret = newAstTypeClause_TypeFunction(CPI(fn), fn);
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
    AstAltFunction *fn = newAstAltFunction(CPI(args), args, body);
    LEAVE(alt_function);
    UNPROTECT(save);
    return fn;
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
    consume(parser, TOK_OPEN());
    AstArgList *args = fargs(parser);
    PROTECT(args);
    consume(parser, TOK_CLOSE());
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
    AstNest *body = nest_body(parser, TOK_RCURLY());
    int save = PROTECT(body);
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
    AstNest *body = nest_body(parser, TOK_RCURLY());
    int save = PROTECT(body);
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
        if (getPrattIntTable(parser->namespaces, sym1, &index)) {
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
    if (check(parser, TOK_CLOSE())) {
        LEAVE(fargs);
        return NULL;
    }
    AstArg *arg = farg(parser);
    int save = PROTECT(arg);
    AstArgList *this = newAstArgList(CPI(arg), arg, NULL);
    PROTECT(this);
    if (match(parser, TOK_COMMA())) {
        this->next = fargs(parser);
    }
    LEAVE(fargs);
    UNPROTECT(save);
    return this;
}

static Character tokenToUnicodeChar(PrattToken *token) {
#ifdef SAFETY_CHECKS
    if (token->value->type != PRATTVALUE_TYPE_CHARACTER) {
        cant_happen("unexpected %s", prattValueTypeName(token->value->type));
    }
#endif
    Character c;
    utf8_to_unicode_char(&c, (unsigned char *) token->value->val.character->entries);
    return c;
}

static AstArg *farg(PrattParser *parser) {
    ENTER(farg);
    AstArg *res = NULL;
    PrattToken *first = peek(parser); // for ParserInfo
    int save = PROTECT(first);
    if (check(parser, TOK_ATOM())) {
        AstLookupOrSymbol *los = scoped_symbol(parser);
        PROTECT(los);
        if (match(parser, TOK_OPEN())) {
            AstArgList *args = fargs(parser);
            PROTECT(args);
            consume(parser, TOK_CLOSE());
            AstUnpack *unp = newAstUnpack(CPI(los), los, args);
            PROTECT(unp);
            res = newAstArg_Unpack(CPI(unp), unp);
            PROTECT(res);
        } else if (match(parser, TOK_LCURLY())) {
            AstTaggedArgList *tfargs = tagged_fargs(parser);
            PROTECT(tfargs);
            consume(parser, TOK_RCURLY());
            AstUnpackStruct *unp = newAstUnpackStruct(CPI(los), los, tfargs);
            PROTECT(unp);
            res = newAstArg_UnpackStruct(CPI(unp), unp);
            PROTECT(res);
        } else if (match(parser, TOK_ASSIGN())) {
            if (los->type == AST_LOOKUPORSYMBOL_TYPE_LOOKUP) {
                parserError(parser, "[0] unexpected '='");
                res = newAstArg_Symbol(CPI(los), TOK_ERROR());
                PROTECT(res);
            } else {
                AstArg *arg = farg(parser);
                PROTECT(arg);
                AstNamedArg *narg = newAstNamedArg(CPI(los), los->val.symbol, arg);
                PROTECT(narg); 
                res = newAstArg_Named(CPI(narg), narg);
                PROTECT(res);
            }
        } else {
            if (los->type == AST_LOOKUPORSYMBOL_TYPE_SYMBOL) {
                res = newAstArg_Symbol(CPI(los), los->val.symbol);
            } else {
                res = newAstArg_Lookup(CPI(los), los->val.lookup);
            }
            PROTECT(res);
        }
    } else if (match(parser, TOK_LSQUARE())) {
        if (match(parser, TOK_RSQUARE())) {
            res = newAstArg_Symbol(TOKPI(first), nilSymbol());
            PROTECT(res);
        } else {
            AstUnpack *args = consfargs(parser);
            PROTECT(args);
            consume(parser, TOK_RSQUARE());
            res = newAstArg_Unpack(CPI(args), args);
            PROTECT(res);
        }
    } else if (check(parser, TOK_STRING())) {
        AstUnpack *str = stringarg(parser);
        PROTECT(str);
        res = newAstArg_Unpack(CPI(str), str);
        PROTECT(res);
    } else if (match(parser, TOK_NUMBER())) {
        MaybeBigInt *mbi = first->value->val.number;
        res = newAstArg_Number(TOKPI(first), mbi);
        PROTECT(res);
    } else if (match(parser, TOK_CHAR())) {
        Character c = tokenToUnicodeChar(first);
        res = newAstArg_Character(TOKPI(first), c);
        PROTECT(res);
    } else if (match(parser, TOK_WILDCARD())) {
        res = newAstArg_Wildcard(TOKPI(first));
        PROTECT(res);
    } else if (match(parser, TOK_TUPLE())) {
        AstArgList *args = fargs(parser);
        PROTECT(args);
        consume(parser, TOK_CLOSE());
        res = newAstArg_Tuple(TOKPI(first), args);
        PROTECT(res);
    } else {
        parserError(parser, "[1] unexpected %s", first->type->name);
        next(parser);
        validateLastAlloc();
        res = newAstArg_Symbol(TOKPI(first), TOK_ERROR());
        PROTECT(res);
    }
    if (match(parser, TOK_CONS())) {
        AstArg *cdr = farg(parser);
        PROTECT(cdr);
        AstArgList *cdrs = newAstArgList(CPI(cdr), cdr, NULL);
        PROTECT(cdrs);
        AstArgList *pair = newAstArgList(CPI(res), res, cdrs);
        PROTECT(pair);
        AstUnpack *cons = makeAstUnpack(CPI(pair), consSymbol(), pair);
        PROTECT(cons);
        res = newAstArg_Unpack(CPI(cons), cons);
        PROTECT(res);
    }
    DEBUG("farg = %s", astArgTypeName(res->type));
    LEAVE(farg);
    UNPROTECT(save);
    return res;
}

static AstTaggedArgList *tagged_farg(PrattParser *parser) {
    ENTER(tagged_farg);
    PrattToken *first = peek(parser);
    int save = PROTECT(first);
    HashSymbol *s = symbol(parser);
    consume(parser, TOK_COLON());
    AstArg *arg = farg(parser);
    PROTECT(arg);
    AstTaggedArgList *this = newAstTaggedArgList(TOKPI(first), s, arg, NULL);
    LEAVE(tagged_farg);
    UNPROTECT(save);
    return this;
}

static AstTaggedArgList *tagged_fargs(PrattParser *parser) {
    ENTER(tagged_fargs);
    AstTaggedArgList *this = tagged_farg(parser);
    int save = PROTECT(this);
    if (match(parser, TOK_COMMA())) {
        this->next = tagged_fargs(parser);
    }
    LEAVE(tagged_fargs);
    UNPROTECT(save);
    return this;
}

static AstUnpack *consfargs(PrattParser *parser) {
    ENTER(consfargs);
    AstArg *car = farg(parser);
    int save = PROTECT(car);
    AstArg *cdr = NULL;
    if (match(parser, TOK_COMMA())) {
        AstUnpack *rest = consfargs(parser);
        PROTECT(rest);
        cdr = newAstArg_Unpack(CPI(rest), rest);
        PROTECT(cdr);
    } else {
        cdr = newAstArg_Symbol(CPI(car), nilSymbol());
        PROTECT(cdr);
    }
    AstArgList *cdrArgs = newAstArgList(CPI(cdr), cdr, NULL);
    PROTECT(cdrArgs);
    AstArgList *consArgs = newAstArgList(CPI(car), car, cdrArgs);
    PROTECT(consArgs);
    AstUnpack *res = makeAstUnpack(CPI(consArgs), consSymbol(), consArgs);
    LEAVE(consfargs);
    UNPROTECT(save);
    return res;
}

static AstUnpack *stringarg(PrattParser *parser) {
    ENTER(stringarg);
    PrattToken *tok = peek(parser);
    int save = PROTECT(tok);
    PrattUTF8 *s = str(parser);
    PROTECT(s);
    AstUnpack *u = makeStringUnpack(TOKPI(tok), s);
    LEAVE(stringarg);
    UNPROTECT(save);
    return u;
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

static PrattRecord *fetchRecord(PrattParser *parser, HashSymbol *symbol) {
    PrattRecord *record = NULL;
    if (getPrattTable(parser->rules, symbol, &record)) {
        return record;
    } else if (parser->next != NULL) {
        return fetchRecord(parser->next, symbol);
    } else {
        cant_happen("unrecognised op %s", symbol->name);
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

static AstExpression *prefix(PrattRecord *record, PrattParser *parser, AstExpression *lhs __attribute__((unused)),
PrattToken *tok __attribute__((unused))) {
    ENTER(prefix);
    AstExpression *res = expr_bp(parser, record->prefixPrec + 1);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), record->symbol, res);
    LEAVE(prefix);
    UNPROTECT(save);
    return res;
}

static AstExpression *prefixC(PrattRecord *record, PrattParser *parser, AstExpression *lhs __attribute__((unused)),
PrattToken *tok __attribute__((unused))) {
    ENTER(prefixC);
    AstExpression *res = expr_bp(parser, 100);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), record->symbol == TOK_MINUS() ? negSymbol() : record->symbol, res);
    LEAVE(prefixC);
    UNPROTECT(save);
    return res;
}

static AstExpression *prefixCar(PrattRecord *record __attribute__((unused)), PrattParser *parser, AstExpression *lhs __attribute__((unused)),
PrattToken *tok __attribute__((unused))) {
    ENTER(prefixCar);
    AstExpression *res = expr_bp(parser, 100);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), carSymbol(), res);
    LEAVE(prefixCar);
    UNPROTECT(save);
    return res;
}

static AstExpression *prefixCdr(PrattRecord *record __attribute__((unused)), PrattParser *parser, AstExpression *lhs __attribute__((unused)),
PrattToken *tok __attribute__((unused))) {
    ENTER(prefixCdr);
    AstExpression *res = expr_bp(parser, 100);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), cdrSymbol(), res);
    LEAVE(prefixCdr);
    UNPROTECT(save);
    return res;
}

static AstExpression *postfix(PrattRecord *record,
                          PrattParser *parser __attribute__((unused)),
                          AstExpression *lhs,
                          PrattToken *tok __attribute__((unused))) {
    ENTER(postfix);
    AstExpression *res = makePrattUnary(CPI(lhs), record->symbol, lhs);
    LEAVE(postfix);
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

static AstExpressions *collectArgs(PrattParser *parser) {
    ENTER(collectArgs);
    AstExpressions *args = NULL;
    int save = PROTECT(args);
    if (!check(parser, TOK_CLOSE())) {
        args = collectArguments(parser);
        PROTECT(args);
    }
    consume(parser, TOK_CLOSE());
    LEAVE(collectArgs);
    UNPROTECT(save);
    return args;
}

static AstExpression *call(PrattRecord *record __attribute__((unused)),
                           PrattParser *parser,
                           AstExpression *lhs,
                           PrattToken *tok __attribute__((unused))) {
    ENTER(call);
    AstExpressions *args = collectArgs(parser);
    int save = PROTECT(args);
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
    AstExpressions *args = collectArgs(parser);
    PROTECT(args);
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
    PrattToken *peeked = peek(parser);
    AstExpressions *args = collectArgs(parser);
    int save = PROTECT(args);
    AstExpression *res = newAstExpression_Tuple(TOKPI(peeked), args);
    LEAVE(tuple);
    UNPROTECT(save);
    return res;
}

static HashSymbol *substitute(PrattParser *parser, HashSymbol *symbol) {
    HashSymbol *replacement = NULL;
    if (getPrattSymbolTable(parser->replacements, symbol, &replacement)) {
        return replacement;
    } else if (parser->next != NULL) {
        return substitute(parser->next, symbol);
    } else {
        return symbol;
    }
}

static AstExpression *infixLeft(PrattRecord *record, PrattParser *parser, AstExpression *lhs,
PrattToken *tok __attribute__((unused))) {
    ENTER(infixLeft);
    AstExpression *rhs = expr_bp(parser, record->infixPrec + 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(lhs), substitute(parser, record->symbol), lhs, rhs);
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
        if (!getPrattIntTable(parser->namespaces, lhs->val.symbol, &index)) {
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
    rhs = makePrattBinary(CPI(rhs), substitute(parser, record->symbol), lhs, rhs);
    LEAVE(infixRight);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *infixAppend(PrattRecord *record, PrattParser *parser, AstExpression *lhs,
PrattToken *tok __attribute__((unused))) {
    ENTER(infixAppend);
    AstExpression *rhs = expr_bp(parser, record->infixPrec - 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(rhs), appendSymbol(), lhs, rhs);
    LEAVE(infixAppend);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *infixCons(PrattRecord *record, PrattParser *parser, AstExpression *lhs,
PrattToken *tok __attribute__((unused))) {
    ENTER(infixCons);
    AstExpression *rhs = expr_bp(parser, record->infixPrec - 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(rhs), consSymbol(), lhs, rhs);
    LEAVE(infixCons);
    UNPROTECT(save);
    return rhs;
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
    PrattRecord *record = fetchRecord(parser, tok->type);
    if (record->prefixOp == NULL) {
        parserError(parser, "not a prefix operator");
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
            PrattRecord *record = fetchRecord(parser, op->type);
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
