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
#include <stdarg.h>

#include "pratt.h"
#include "pratt_debug.h"
#include "pratt_functions.h"
#include "pratt_scanner.h"
#include "symbols.h"
#include "ast.h"
#include "bigint.h"
#include "utf8.h"
#include "print_generator.h"
#include "file_id.h"

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

extern AstStringArray *include_paths;

void ppAstExpression(AstExpression *expr);
AstNest *top(PrattParser *);
AstExpression *expr_bp(PrattParser *parser, int min_bp);
static AstExpression *grouping(PrattRecord *, PrattParser *, AstExpression *);
static AstExpression *prefix(PrattRecord *, PrattParser *, AstExpression *);
static AstExpression *prefixC(PrattRecord *, PrattParser *, AstExpression *);
static AstExpression *postfix(PrattRecord *, PrattParser *, AstExpression *);
static AstExpression *tuple(PrattRecord *, PrattParser *, AstExpression *);
static AstExpression *postfixArg(PrattRecord *, PrattParser *, AstExpression *);
static AstExpression *infixLeft(PrattRecord *, PrattParser *, AstExpression *);
static AstExpression *infixRight(PrattRecord *, PrattParser *, AstExpression *);
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
static AstCompositeFunction *fun(PrattParser *);
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
static PrattUnicode *PrattUTF8ToUnicode(PrattUTF8 *);

static void addRecord(PrattTable *table, HashSymbol *tok, PrattOp prefix, PrattOp infix, PrattOp postfix, int precedence) {
    PrattRecord *record = newPrattRecord(tok, prefix, infix, postfix, precedence);
    int save = PROTECT(record);
    setPrattTable(table, record->symbol, record);
    UNPROTECT(save);
}

static PrattParser *makePrattParser() {
    PrattParser *res = newPrattParser(NULL, NULL);
    int save = PROTECT(res);
    PrattTable *table = res->rules;
    addRecord(table, TOK_SEMI(),      NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_ATOM(),      NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_STRING(),    NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_TYPEDEF(),   NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_UNSAFE(),    NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_FN(),        NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_LINK(),      NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_AS(),        NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_ALIAS(),     NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_ERROR(),     NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_NS(),        NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_NAMESPACE(), NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_LET(),       NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_IN(),        NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_TUPLE(),     tuple,    NULL,       NULL,       0);
    addRecord(table, TOK_OPEN(),      grouping, NULL,       postfixArg, 0);
    addRecord(table, TOK_CLOSE(),     NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_LSQUARE(),   NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_RSQUARE(),   NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_COMMA(),     NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_LCURLY(),    NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_RCURLY(),    NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_PIPE(),      NULL,     NULL,       NULL,       0);
    addRecord(table, TOK_WILDCARD(),  NULL,     NULL,       NULL,       0);

    addRecord(table, TOK_ARROW(),     NULL,     infixRight, NULL,      10);

    addRecord(table, TOK_THEN(),      NULL,     infixRight, NULL,      20);

    addRecord(table, TOK_AND(),       NULL,     infixLeft,  NULL,      30);
    addRecord(table, TOK_OR(),        NULL,     infixLeft,  NULL,      30);
    addRecord(table, TOK_XOR(),       NULL,     infixLeft,  NULL,      30);
    addRecord(table, TOK_NAND(),      NULL,     infixLeft,  NULL,      30);
    addRecord(table, TOK_NOR(),       NULL,     infixLeft,  NULL,      30);
    addRecord(table, TOK_NXOR(),      NULL,     infixLeft,  NULL,      30);

    addRecord(table, TOK_NOT(),       prefix,   NULL,       NULL,      40);

    addRecord(table, TOK_EQ(),        NULL,     infixLeft,  NULL,      50);
    addRecord(table, TOK_NE(),        NULL,     infixLeft,  NULL,      50);
    addRecord(table, TOK_GT(),        prefixC,  infixLeft,  NULL,      50);
    addRecord(table, TOK_LT(),        prefixC,  infixLeft,  NULL,      50);
    addRecord(table, TOK_GE(),        NULL,     infixLeft,  NULL,      50);
    addRecord(table, TOK_LE(),        NULL,     infixLeft,  NULL,      50);
    addRecord(table, TOK_CMP(),       NULL,     infixLeft,  NULL,      50);

    addRecord(table, TOK_ASSIGN(),    NULL,     infixRight, NULL,      60);

    addRecord(table, TOK_COLON(),     NULL,     infixLeft,  NULL,      70);

    addRecord(table, TOK_APPEND(),    NULL,     infixRight, NULL,      80);

    addRecord(table, TOK_CONS(),      NULL,     infixRight, NULL,      90);
    addRecord(table, TOK_PLUS(),      prefixC,  infixLeft,  NULL,      90);
    addRecord(table, TOK_MINUS(),     prefixC,  infixLeft,  NULL,      90);

    addRecord(table, TOK_TIMES(),     NULL,     infixLeft,  NULL,     100);
    addRecord(table, TOK_DIVIDE(),    NULL,     infixLeft,  NULL,     100);
    addRecord(table, TOK_MOD(),       NULL,     infixLeft,  NULL,     100);

    addRecord(table, TOK_EXP(),       NULL,     infixRight, NULL,     110);

    addRecord(table, TOK_HERE(),      prefix,   NULL,       NULL,     120);
    addRecord(table, TOK_HASH(),      prefix,   NULL,       NULL,     120);
    addRecord(table, TOK_BANG(),      NULL,     NULL,       postfix,  120);

    addRecord(table, TOK_PERIOD(),    NULL,     infixRight, NULL,     130);

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

static void parserError(PrattParser *parser, const char *message, ...)
__attribute__((format(printf, 2, 3)));

static void parserError(PrattParser *parser, const char *message, ...) {
    va_list args;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    PrattBufList *bufList = parser->lexer->bufList;
    can_happen(" at %s line %d", bufList->filename->name, bufList->lineno);
}

static AstDefinitions *namespaceFromFileName(PrattParser *parser, char *filename) {
    PrattBufList *bufList = prattBufListFromFileName(filename, NULL);
    int save = PROTECT(bufList);
    bufList = prattBufListFromString(parser->lexer->bufList->filename->name, "__NAMESPACE__", bufList);
    REPLACE_PROTECT(save, bufList);
    PrattLexer *lexer = newPrattLexer(bufList, parser->lexer->trie);
    REPLACE_PROTECT(save, lexer);
    PrattParser *new = newPrattParser(lexer, parser);
    REPLACE_PROTECT(save, new);
    AstNest *nest = top(new);
    UNPROTECT(save);
    if (nest) return nest->definitions;
    return NULL;
}

static void storeNamespace(PrattParser *parser, AstNamespace *ns) {
    if (getPrattIntTable(parser->namespaces, ns->symbol, NULL)) {
        parserError(parser, "redefinition of namespace %s", ns->symbol->name);
    } else {
        setPrattIntTable(parser->namespaces, ns->symbol, ns->reference);
    }
}

// Careful. Somewhat accidentally this algorithm stores the namespaces
// in the order that they need to be processed.
// Specifically because a namespace is parsed before it is recorded,
// all of its imports are recorded ahead of it.
static AstNamespace *parseLink(PrattParser *parser, unsigned char *file, HashSymbol *symbol) {
    static AstFileIdArray *fileIdStack = NULL;

    if (fileIdStack == NULL) {
        fileIdStack = newAstFileIdArray();
    }
    AgnosticFileId *fileId = calculatePath(file, parser);
    int save = PROTECT(fileId);
    if (fileId == NULL) {
        parserError(parser, "cannot find file \"%s\"", file);
        UNPROTECT(save);
        return NULL;
    }
    int found = lookupNamespace(fileId);
    if (found != -1) {
        AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, found);
        UNPROTECT(save);
        return ns;
    }
    if (fileIdInArray(fileId, fileIdStack)) {
        parserError(parser, "recursive include detected for %s", fileId->name);
        UNPROTECT(save);
        return NULL;
    }
    pushAstFileIdArray(fileIdStack, fileId);
    AstDefinitions *definitions = namespaceFromFileName(parser, fileId->name);
    if (definitions == NULL) {
        parserError(parser, "errors detected while parsing %s", fileId->name);
        UNPROTECT(save);
        return NULL;
    }
    AstNamespaceImpl *impl = newAstNamespaceImpl(BUFPI(parser->lexer->bufList), fileId, definitions);
    found = pushAstNamespaceArray(namespaces, impl);
    popAstFileIdArray(fileIdStack);
    AstNamespace *ns = newAstNamespace(BUFPI(parser->lexer->bufList), symbol, found);
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

void ppAstFunCall(AstFunCall *funCall) {
    ppAstExpression(funCall->function);
    printf("(");
    for (AstExpressions *expressions = funCall->arguments; expressions != NULL; expressions = expressions->next) {
        ppAstExpression(expressions->expression);
        if (expressions->next) {
            printf(", ");
        }
    }
    printf(")");
}

void ppAstCharacter(Character c) {
    unsigned char buffer[8];
    unsigned char *end = writeChar(buffer, c);
    *end = 0;
    printf("'%s'", buffer);
}

void ppAstTuple(AstExpressions *expressions) {
    printf("#(");
    while (expressions) {
        ppAstExpression(expressions->expression);
        if (expressions->next)
            printf(", ");
        expressions = expressions->next;
    }
    printf(")");
}

void ppAstExpression(AstExpression *expr) {
    switch (expr->type) {
        case AST_EXPRESSION_TYPE_NUMBER:
            fprintMaybeBigInt(stdout, expr->val.number);
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            printf("%s", expr->val.symbol->name);
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            ppAstFunCall(expr->val.funCall);
            break;
        case AST_EXPRESSION_TYPE_CHARACTER:
            ppAstCharacter(expr->val.character);
            break;
        case AST_EXPRESSION_TYPE_TUPLE:
            ppAstTuple(expr->val.tuple);
            break;
        default:
            cant_happen("unexpected %s", astExpressionTypeName(expr->type));
    }
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
    AstNest *body = nest_body(parser, TOK_EOF());
    int save = PROTECT(body);
    consume(parser->lexer, TOK_EOF());
    UNPROTECT(save);
    return body;
}

AstNest *nest_body(PrattParser *parser, HashSymbol *terminal) {
    if (match(parser->lexer, TOK_LET())) {
        AstDefinitions *defs = definitions(parser, TOK_IN());
        int save = PROTECT(defs);
        consume(parser->lexer, TOK_IN());
        AstExpressions *stats = statements(parser, terminal);
        PROTECT(stats);
        AstNest *res = newAstNest(CPI(defs), defs, stats);
        UNPROTECT(save);
        return res;
    } else if (match(parser->lexer, TOK_NS())) {
        consume(parser->lexer, TOK_NAMESPACE());
        AstDefinitions *defs = definitions(parser, terminal);
        int save = PROTECT(defs);
        AstNest *res = newAstNest(CPI(defs), defs, NULL);
        UNPROTECT(save);
        return res;
    } else {
        AstExpressions *stats = statements(parser, terminal);
        int save = PROTECT(stats);
        AstNest *res = newAstNest(CPI(stats), NULL, stats);
        UNPROTECT(save);
        return res;
    }
}

static AstExpressions *statements(PrattParser *parser, HashSymbol *terminal) {
    AstExpression *expr = expression(parser);
    int save = PROTECT(expr);
    if (check(parser->lexer, TOK_SEMI())) {
        next(parser->lexer);
        if (check(parser->lexer, terminal)) {
            AstExpressions *this = newAstExpressions(CPI(expr), expr, NULL);
            UNPROTECT(save);
            return this;
        } else {
            AstExpressions *rest = statements(parser, terminal);
            PROTECT(rest);
            AstExpressions *this = newAstExpressions(CPI(expr), expr, rest);
            UNPROTECT(save);
            return this;
        }
    } else {
        AstExpressions *this = newAstExpressions(CPI(expr), expr, NULL);
        UNPROTECT(save);
        return this;
    }
}

static AstExpression *expression(PrattParser *parser) {
    return expr_bp(parser, 0);
}

static AstDefinitions *definitions(PrattParser *parser, HashSymbol *terminal) {
    if (check(parser->lexer, terminal)) {
        return NULL;
    }
    AstDefinition *def = definition(parser);
    int save = PROTECT(def);
    AstDefinitions *next = definitions(parser, terminal);
    PROTECT(next);
    AstDefinitions *this = newAstDefinitions(CPI(def), def, next);
    UNPROTECT(save);
    return this;
}

static AstDefinition *definition(PrattParser *parser) {
    if (check(parser->lexer, TOK_ATOM())) {
        return assignment(parser);
    } else if (check(parser->lexer, TOK_TYPEDEF())) {
        return typedefinition(parser);
    } else if (check(parser->lexer, TOK_UNSAFE())) {
        next(parser->lexer);
        consume(parser->lexer, TOK_FN());
        return defun(parser, true, false);
    } else if (check(parser->lexer, TOK_FN())) {
        next(parser->lexer);
        return defun(parser, false, false);
    } else if (check(parser->lexer, TOK_PRINT())) {
        next(parser->lexer);
        return defun(parser, false, true);
    } else if (check(parser->lexer, TOK_LINK())) {
        return link(parser);
    } else if (check(parser->lexer, TOK_ALIAS())) {
        return alias(parser);
    } else {
        PrattToken *tok = next(parser->lexer);
        errorAt(tok, "expecting definition");
        return newAstDefinition_Blank(TOKPI(tok));
    }
}

static AstDefinition *alias(PrattParser *parser) {
    consume(parser->lexer, TOK_ALIAS());
    HashSymbol *s = symbol(parser);
    consume(parser->lexer, TOK_ASSIGN());
    AstType *t = type_type(parser);
    int save = PROTECT(t);
    consume(parser->lexer, TOK_SEMI());
    AstAlias *a = newAstAlias(CPI(t), s, t);
    PROTECT(a);
    AstDefinition *d = newAstDefinition_Alias(CPI(a), a);
    UNPROTECT(save);
    return d;
}

static AstType *type_type(PrattParser *parser) {
    if (match(parser->lexer, TOK_OPEN())) {
        AstType *type = type_type(parser);
        int save = PROTECT(type);
        consume(parser->lexer, TOK_CLOSE());
        UNPROTECT(save);
        return type;
    } else {
        AstTypeClause *clause = type_clause(parser);
        int save = PROTECT(clause);
        AstType *type = newAstType(CPI(clause), clause, NULL);
        PROTECT(type);
        if (match(parser->lexer, TOK_ARROW())) {
            type->next = type_type(parser);
        }
        UNPROTECT(save);
        return type;
    }
}

static AstTypeClause *type_clause(PrattParser *parser) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    if (match(parser->lexer, TOK_KW_NUMBER())) {
        AstTypeClause *ret = newAstTypeClause_Integer(TOKPI(tok));
        UNPROTECT(save);
        return ret;
    } else if (match(parser->lexer, TOK_KW_CHAR())) {
        AstTypeClause *ret = newAstTypeClause_Character(TOKPI(tok));
        UNPROTECT(save);
        return ret;
    } else if (tok->type == TOK_HASH()) {
        HashSymbol *typeVar = type_variable(parser);
        AstTypeClause *ret = newAstTypeClause_Var(TOKPI(tok), typeVar);
        UNPROTECT(save);
        return ret;
    } else if (tok->type == TOK_ATOM()) {
        AstTypeFunction *fn = type_function(parser);
        PROTECT(fn);
        AstTypeClause *ret = newAstTypeClause_TypeFunction(CPI(fn), fn);
        UNPROTECT(save);
        return ret;
    } else if (tok->type == TOK_TUPLE()) {
        AstTypeList *lst = type_tuple(parser);
        PROTECT(lst);
        AstTypeClause *ret = newAstTypeClause_TypeTuple(CPI(lst), lst);
        UNPROTECT(save);
        return ret;
    } else {
        parserError(parser, "expected type clause");
        UNPROTECT(save);
        return NULL;
    }
}

static HashSymbol *type_variable(PrattParser *parser) {
    consume(parser->lexer, TOK_HASH());
    return symbol(parser);
}

static AstTypeList *type_list(PrattParser *parser) {
    AstType *type = type_type(parser);
    int save = PROTECT(type);
    AstTypeList *this = newAstTypeList(CPI(type), type, NULL);
    PROTECT(this);
    if (match(parser->lexer, TOK_COMMA())) {
        this->next = type_list(parser);
    }
    UNPROTECT(save);
    return this;
}

static AstTypeMap *type_map(PrattParser *parser) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    consume(parser->lexer, TOK_COLON());
    AstType *type = type_type(parser);
    PROTECT(type);
    AstTypeMap *this = newAstTypeMap(TOKPI(tok), s, type, NULL);
    PROTECT(this);
    if (match(parser->lexer, TOK_COMMA())) {
        this->next = type_map(parser);
    }
    UNPROTECT(save);
    return this;
}

static AstAltFunction *alt_function(PrattParser *parser) {
    AstAltArgs *args = alt_args(parser);
    int save = PROTECT(args);
    AstNest *body = nest(parser);
    PROTECT(body);
    AstAltFunction *fn = newAstAltFunction(CPI(args), args, body);
    UNPROTECT(save);
    return fn;
}

static AstTypeFunction *type_function(PrattParser *parser) {
    AstLookupOrSymbol *name = scoped_symbol(parser);
    int save = PROTECT(name);
    AstTypeFunction *this = newAstTypeFunction(CPI(name), name, NULL);
    PROTECT(this);
    if (match(parser->lexer, TOK_OPEN())) {
        this->typeList = type_list(parser);
        consume(parser->lexer, TOK_CLOSE());
    }
    UNPROTECT(save);
    return this;
}

static AstTypeList *type_tuple(PrattParser *parser) {
    consume(parser->lexer, TOK_TUPLE());
    AstTypeList *body = type_list(parser);
    int save = PROTECT(body);
    consume(parser->lexer, TOK_CLOSE());
    UNPROTECT(save);
    return body;
}

static AstAltArgs *alt_args(PrattParser *parser) {
    PrattToken *tok= peek(parser->lexer);
    int save = PROTECT(tok);
    consume(parser->lexer, TOK_OPEN());
    AstArgList *args = fargs(parser);
    PROTECT(args);
    consume(parser->lexer, TOK_CLOSE());
    AstAltArgs *this = newAstAltArgs(TOKPI(tok), args, NULL);
    PROTECT(this);
    if (match(parser->lexer, TOK_PIPE())) {
        this->next = alt_args(parser);
    }
    UNPROTECT(save);
    return this;
}

static AstNest *nest(PrattParser *parser) {
    consume(parser->lexer, TOK_LCURLY());
    AstNest *body = nest_body(parser, TOK_RCURLY());
    int save = PROTECT(body);
    consume(parser->lexer, TOK_RCURLY());
    UNPROTECT(save);
    return body;
}

static AstLookupOrSymbol *scoped_symbol(PrattParser *parser) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    HashSymbol *sym1 = symbol(parser);
    if (match(parser->lexer, TOK_PERIOD())) {
        HashSymbol *sym2 = symbol(parser);
        int index = 0;
        if (getPrattIntTable(parser->namespaces, sym1, &index)) {
            AstLookupSymbol *lus = newAstLookupSymbol(TOKPI(tok), index, sym1, sym2);
            PROTECT(lus);
            AstLookupOrSymbol *res = newAstLookupOrSymbol_Lookup(CPI(lus), lus);
            UNPROTECT(save);
            return res;
        } else {
            parserError(parser, "cannot resolve namespace %s", sym2->name);
        }
    }
    AstLookupOrSymbol *res = newAstLookupOrSymbol_Symbol(TOKPI(tok), sym1);
    UNPROTECT(save);
    return res;
}

static AstArgList *fargs(PrattParser *parser) {
    if (check(parser->lexer, TOK_CLOSE())) {
        return NULL;
    }
    AstArg *arg = farg(parser);
    int save = PROTECT(arg);
    AstArgList *this = newAstArgList(CPI(arg), arg, NULL);
    PROTECT(this);
    if (match(parser->lexer, TOK_COMMA())) {
        this->next = fargs(parser);
    }
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
    AstArg *res = NULL;
    PrattToken *first = peek(parser->lexer); // for ParserInfo
    int save = PROTECT(first);
    if (check(parser->lexer, TOK_ATOM())) {
        AstLookupOrSymbol *los = scoped_symbol(parser);
        save = PROTECT(los);
        if (match(parser->lexer, TOK_OPEN())) {
            AstArgList *args = fargs(parser);
            PROTECT(args);
            consume(parser->lexer, TOK_CLOSE());
            AstUnpack *unp = newAstUnpack(CPI(los), los, args);
            PROTECT(unp);
            res = newAstArg_Unpack(CPI(unp), unp);
            PROTECT(res);
        } else if (match(parser->lexer, TOK_LCURLY())) {
            AstTaggedArgList *tfargs = tagged_fargs(parser);
            PROTECT(tfargs);
            consume(parser->lexer, TOK_RCURLY());
            AstUnpackStruct *unp = newAstUnpackStruct(CPI(los), los, tfargs);
            PROTECT(unp);
            res = newAstArg_UnpackStruct(CPI(unp), unp);
            PROTECT(res);
        } else if (match(parser->lexer, TOK_ASSIGN())) {
            if (los->type == AST_LOOKUPORSYMBOL_TYPE_LOOKUP) {
                parserError(parser, "unexpected '='");
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
    } else if (match(parser->lexer, TOK_LSQUARE())) {
        if (match(parser->lexer, TOK_RSQUARE())) {
            res = newAstArg_Symbol(TOKPI(first), nilSymbol());
            PROTECT(res);
        } else {
            AstUnpack *args = consfargs(parser);
            PROTECT(args);
            consume(parser->lexer, TOK_RSQUARE());
            res = newAstArg_Unpack(CPI(args), args);
            PROTECT(res);
        }
    } else if (check(parser->lexer, TOK_STRING())) {
        AstUnpack *str = stringarg(parser);
        PROTECT(str);
        res = newAstArg_Unpack(CPI(str), str);
        PROTECT(res);
    } else if (match(parser->lexer, TOK_CHAR())) {
        Character c = tokenToUnicodeChar(first);
        res = newAstArg_Character(TOKPI(first), c);
        PROTECT(res);
    } else if (match(parser->lexer, TOK_WILDCARD())) {
        res = newAstArg_Wildcard(TOKPI(first));
        PROTECT(res);
    } else if (match(parser->lexer, TOK_TUPLE())) {
        AstArgList *args = fargs(parser);
        PROTECT(args);
        consume(parser->lexer, TOK_CLOSE());
        res = newAstArg_Tuple(TOKPI(first), args);
        PROTECT(res);
    } else {
        parserError(parser, "unexpected %s", first->type->name);
        next(parser->lexer);
        res = newAstArg_Symbol(TOKPI(first), TOK_ERROR());
        PROTECT(res);
    }
    if (match(parser->lexer, TOK_CONS())) {
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
    UNPROTECT(save);
    return res;
}

static AstTaggedArgList *tagged_farg(PrattParser *parser) {
    PrattToken *first = peek(parser->lexer);
    int save = PROTECT(first);
    HashSymbol *s = symbol(parser);
    consume(parser->lexer, TOK_COLON());
    AstArg *arg = farg(parser);
    PROTECT(arg);
    AstTaggedArgList *this = newAstTaggedArgList(TOKPI(first), s, arg, NULL);
    UNPROTECT(save);
    return this;
}

static AstTaggedArgList *tagged_fargs(PrattParser *parser) {
    AstTaggedArgList *this = tagged_farg(parser);
    int save = PROTECT(this);
    if (match(parser->lexer, TOK_COMMA())) {
        this->next = tagged_fargs(parser);
    }
    UNPROTECT(save);
    return this;
}

static AstUnpack *consfargs(PrattParser *parser) {
    AstArg *car = farg(parser);
    int save = PROTECT(car);
    AstArg *cdr = NULL;
    if (match(parser->lexer, TOK_COMMA())) {
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
    UNPROTECT(save);
    return res;
}

static AstUnpack *stringarg(PrattParser *parser) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    PrattUTF8 *s = str(parser);
    PROTECT(s);
    AstUnpack *u = makeStringUnpack(TOKPI(tok), s);
    UNPROTECT(save);
    return u;
}

static AstDefinition *defun(PrattParser *parser, bool unsafe, bool isPrinter) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstCompositeFunction *f = fun(parser);
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
    UNPROTECT(save);
    return res;
}

static HashSymbol *symbol(PrattParser *parser) {
    PrattToken *tok = next(parser->lexer);
    int save = PROTECT(tok);
    if (tok->type != TOK_ATOM()) {
        errorAt(tok, "expected ATOM");
        UNPROTECT(save);
        return TOK_ERROR();
    }
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_ATOM) {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    HashSymbol *s = tok->value->val.atom;
    UNPROTECT(save);
    return s;
}

static AstDefinition *assignment(PrattParser* parser) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    consume(parser->lexer, TOK_ASSIGN());
    AstExpression *expr = expression(parser);
    PROTECT(expr);
    consume(parser->lexer, TOK_SEMI());
    AstDefine *def = newAstDefine(TOKPI(tok), s, expr);
    PROTECT(def);
    AstDefinition *res = newAstDefinition_Define(CPI(def), def);
    UNPROTECT(save);
    return res;
}

static AstDefinition *typedefinition(PrattParser *parser) {
    consume(parser->lexer, TOK_TYPEDEF());
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstUserType *userType = NULL;
    if (check(parser->lexer, TOK_OPEN())) {
        next(parser->lexer);
        AstTypeSymbols *variables = type_variables(parser);
        PROTECT(variables);
        consume(parser->lexer, TOK_CLOSE());
        userType = newAstUserType(TOKPI(tok), s, variables);
    } else {
        userType = newAstUserType(TOKPI(tok), s, NULL);
    }
    PROTECT(userType);
    consume(parser->lexer, TOK_LCURLY());
    AstTypeBody *typeBody = type_body(parser);
    PROTECT(typeBody);
    consume(parser->lexer, TOK_RCURLY());
    AstTypeDef *typeDef = newAstTypeDef(CPI(userType), userType, typeBody);
    PROTECT(typeDef);
    AstDefinition *res = newAstDefinition_TypeDef(CPI(typeDef), typeDef);
    UNPROTECT(save);
    return res;
}

static AstTypeBody *type_body(PrattParser *parser) {
    AstTypeConstructor *tc = type_constructor(parser);
    int save = PROTECT(tc);
    if (match(parser->lexer, TOK_PIPE())) {
        AstTypeBody *rest = type_body(parser);
        PROTECT(rest);
        AstTypeBody *this = newAstTypeBody(CPI(tc), tc, rest);
        UNPROTECT(save);
        return this;
    } else {
        AstTypeBody *this = newAstTypeBody(CPI(tc), tc, NULL);
        UNPROTECT(save);
        return this;
    }
}

static AstTypeConstructor *type_constructor(PrattParser *parser) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    HashSymbol *s = symbol(parser);
    AstTypeConstructorArgs *args = NULL;
    if (match(parser->lexer, TOK_OPEN())) {
        AstTypeList *typeList = type_list(parser);
        PROTECT(typeList);
        consume(parser->lexer, TOK_CLOSE());
        args = newAstTypeConstructorArgs_List(CPI(typeList), typeList);
        PROTECT(args);
    } else if (match(parser->lexer, TOK_LCURLY())) {
        AstTypeMap *typeMap = type_map(parser);
        PROTECT(typeMap);
        consume(parser->lexer, TOK_RCURLY());
        args = newAstTypeConstructorArgs_Map(CPI(typeMap), typeMap);
        PROTECT(args);
    }
    AstTypeConstructor *res = newAstTypeConstructor(TOKPI(tok), s, args);
    UNPROTECT(save);
    return res;
}

static AstTypeSymbols *type_variables(PrattParser *parser) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    HashSymbol *s = type_variable(parser);
    if (check(parser->lexer, TOK_CLOSE())) {
        AstTypeSymbols *t = newAstTypeSymbols(TOKPI(tok), s, NULL);
        UNPROTECT(save);
        return t;
    } else {
        consume(parser->lexer, TOK_COMMA());
        AstTypeSymbols *rest = type_variables(parser);
        PROTECT(rest);
        AstTypeSymbols *t = newAstTypeSymbols(TOKPI(tok), s, rest);
        UNPROTECT(save);
        return t;
    }
}

static AstDefinition *link(PrattParser *parser) {
    PrattToken *tok = peek(parser->lexer);
    int save = PROTECT(tok);
    consume(parser->lexer, TOK_LINK());
    PrattUTF8 *path = rawString(parser);
    PROTECT(path);
    if (path == NULL) {
        AstDefinition *res = newAstDefinition_Blank(TOKPI(tok));
        UNPROTECT(save);
        return res;
    } else {
        consume(parser->lexer, TOK_AS());
        HashSymbol *name = symbol(parser);
        consume(parser->lexer, TOK_SEMI());
        AstNamespace *ns = parseLink(parser, path->entries, name);
        PROTECT(ns);
        storeNamespace(parser, ns);
        AstDefinition *res = newAstDefinition_Blank(CPI(ns));
        UNPROTECT(save);
        return res;
    }
}

static PrattUTF8 *rawString(PrattParser *parser) {
    PrattToken *tok = next(parser->lexer);
    if (tok->type == TOK_STRING()) {
#ifdef SAFETY_CHECKS
        if (tok->value->type != PRATTVALUE_TYPE_STRING) {
            cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
        }
#endif
        return tok->value->val.string;
    } else {
        parserError(parser, "expected string, got %s", tok->type->name);
        PrattUTF8 *error = newPrattUTF8();
        int save = PROTECT(error);
        pushPrattUTF8(error, 0);
        UNPROTECT(save);
        return error;
    }
}

static void appendString(PrattParser *parser, PrattUTF8 *this) {
    PrattUTF8 *next = rawString(parser);
    int save = PROTECT(next);
    this->size--; // backup over '\0'
    for (Index i = 0; i < next->size; i++) {
        pushPrattUTF8(this, next->entries[i]);
    }
    UNPROTECT(save);
    if (check(parser->lexer, TOK_STRING())) {
        appendString(parser, this);
    }
}

static PrattUTF8 *str(PrattParser *parser) {
    PrattUTF8 *this = rawString(parser);
    int save = PROTECT(this);
    if (check(parser->lexer, TOK_STRING())) {
        appendString(parser, this);
    }
    UNPROTECT(save);
    return this;
}

static AstCompositeFunction *fun(PrattParser *parser) {
    if (match(parser->lexer, TOK_LCURLY())) {
        AstCompositeFunction *res = functions(parser);
        int save = PROTECT(res);
        consume(parser->lexer, TOK_RCURLY());
        UNPROTECT(save);
        return res;
    } else {
        AstAltFunction *f = alt_function(parser);
        int save = PROTECT(f);
        AstCompositeFunction *res = makeAstCompositeFunction(f, NULL);
        UNPROTECT(save);
        return res;
    }
}

static AstCompositeFunction *functions(PrattParser *parser) {
    AstAltFunction *f = alt_function(parser);
    int save = PROTECT(f);
    AstCompositeFunction *rest = NULL;
    if (check(parser->lexer, TOK_OPEN())) {
        rest = functions(parser);
        PROTECT(rest);
    }
    AstCompositeFunction *this = makeAstCompositeFunction(f, rest);
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

static AstExpression *grouping(PrattRecord *record, PrattParser *parser, AstExpression *lhs __attribute__((unused))) {
    ENTER(grouping);
    AstExpression *res = expr_bp(parser, record->precedence);
    int save = PROTECT(res);
    consume(parser->lexer, S(")"));
    LEAVE(grouping);
    UNPROTECT(save);
    return res;
}

static AstExpression *prefix(PrattRecord *record, PrattParser *parser, AstExpression *lhs __attribute__((unused))) {
    ENTER(prefix);
    AstExpression *res = expr_bp(parser, record->precedence + 1);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), record->symbol, res);
    LEAVE(prefix);
    UNPROTECT(save);
    return res;
}

static AstExpression *prefixC(PrattRecord *record, PrattParser *parser, AstExpression *lhs __attribute__((unused))) {
    ENTER(prefixC);
    AstExpression *res = expr_bp(parser, 100);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), record->symbol, res);
    LEAVE(prefixC);
    UNPROTECT(save);
    return res;
}

static AstExpression *postfix(PrattRecord *record,
                          PrattParser *parser __attribute__((unused)),
                          AstExpression *lhs) {
    return makePrattUnary(CPI(lhs), record->symbol, lhs);
}

static AstExpressions *collectArguments(PrattParser *parser) {
    AstExpression *arg = expr_bp(parser, 0);
    int save = PROTECT(arg);
    AstExpressions *next = NULL;
    if (match(parser->lexer, S(","))) {
        next = collectArguments(parser);
        PROTECT(next);
    }
    AstExpressions *this = newAstExpressions(CPI(arg), arg, next);
    UNPROTECT(save);
    return this;
}

static AstExpressions *collectArgs(PrattParser *parser) {
    AstExpressions *args = NULL;
    int save = PROTECT(args);
    if (!check(parser->lexer, S(")"))) {
        args = collectArguments(parser);
        PROTECT(args);
    }
    consume(parser->lexer, S(")"));
    UNPROTECT(save);
    return args;
}

static AstExpression *postfixArg(PrattRecord *record __attribute__((unused)),
                                 PrattParser *parser,
                                 AstExpression *lhs) {
    ENTER(postfixArg);
    AstExpressions *args = collectArgs(parser);
    int save = PROTECT(args);
    AstFunCall *funCall = newAstFunCall(CPI(lhs), lhs, args);
    PROTECT(funCall);
    AstExpression *res = newAstExpression_FunCall(CPI(funCall), funCall);
    LEAVE(postfixArg);
    UNPROTECT(save);
    return res;
}

static AstExpression *tuple(PrattRecord *record __attribute__((unused)),
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused))) {
    ENTER(tuple);
    PrattToken *peeked = peek(parser->lexer);
    AstExpressions *args = collectArgs(parser);
    int save = PROTECT(args);
    AstExpression *res = newAstExpression_Tuple(TOKPI(peeked), args);
    LEAVE(tuple);
    UNPROTECT(save);
    return res;
}

static AstExpression *infixLeft(PrattRecord *record, PrattParser *parser, AstExpression *lhs) {
    ENTER(infixLeft);
    AstExpression *rhs = expr_bp(parser, record->precedence + 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(lhs), record->symbol, lhs, rhs);
    LEAVE(infixLeft);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *infixRight(PrattRecord *record, PrattParser *parser, AstExpression *lhs) {
    ENTER(infixRight);
    AstExpression *rhs = expr_bp(parser, record->precedence - 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(rhs), record->symbol, lhs, rhs);
    LEAVE(infixRight);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *makeAtom(PrattToken *tok) {
#ifdef SAFETY_CHECKS
    if (tok->value->type != PRATTVALUE_TYPE_ATOM) {
        cant_happen("unexpected %s", prattValueTypeName(tok->value->type));
    }
#endif
    HashSymbol *name = tok->value->val.atom;
    return newAstExpression_Symbol(TOKPI(tok), name);
}

static AstExpression *makeNumber(PrattToken *tok) {
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

static AstExpression *makeChar(PrattToken *tok) {
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

AstExpression *expr_bp(PrattParser *parser, int min_bp) {
    ENTER(expr_bp);
    AstExpression *lhs = NULL;
    PrattToken *tok = next(parser->lexer);
    int save = PROTECT(tok);
    if (tok->type == S("ATOM")) {
        lhs = makeAtom(tok);
    } else if (tok->type == S("INT")) {
        lhs = makeNumber(tok);
    } else if (tok->type == S("FLOAT")) {
        lhs = makeNumber(tok);
    } else if (tok->type == S("CHAR")) {
        lhs = makeChar(tok);
    } else {
        PrattRecord *record = fetchRecord(parser, tok->type);
        if (record->prefixOp == NULL) {
            errorAt(tok, "not a prefix operator");
        } else {
            lhs = record->prefixOp(record, parser, NULL);
        }
    }
    REPLACE_PROTECT(save, lhs);
    for (;;) {
        PrattToken *op = peek(parser->lexer);
        if (op->type == S("EOF")) {
            DEBUG("PEEKED EOF");
            break;
        } else {
            DEBUG("PEEKED OP %s", op->type->name);
            PrattRecord *record = fetchRecord(parser, op->type);
            if(record->postfixOp != NULL) {
                DEBUG("postfix %d %d", record->precedence, min_bp);
                if (record->precedence < min_bp) {
                    break;
                }
                next(parser->lexer);
                lhs = record->postfixOp(record,parser, lhs);
                REPLACE_PROTECT(save, lhs);
            } else if (record->infixOp != NULL) {
                DEBUG("infix %d %d", record->precedence, min_bp);
                if (record->precedence < min_bp) {
                    break;
                }
                next(parser->lexer);
                lhs = record->infixOp(record, parser, lhs);
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

static void test(PrattParser *parser, PrattTrie *trie, char *expr) {
    eprintf("%-30s ", expr);
    NEWLINE();
    parser->lexer = makePrattLexer(trie, "test", expr);
    AstNest *result = top(parser);
    int save = PROTECT(result);
    if (parser->lexer->bufList != NULL) {
        PrattToken *tok = next(parser->lexer);
        errorAt(tok, "unconsumed tokens");
    }
    ppAstNest(result);
    eprintf("\n");
    UNPROTECT(save);
}

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    initProtection();
    PrattParser *p = makePrattParser();
    PROTECT(p);
    PrattTrie *t = makePrattTrie(p, NULL);
    PROTECT(t);
    test(p, t, "1");
    test(p, t, "5!");
    test(p, t, "1i");
    test(p, t, "Σ");
    test(p, t, "'Σ'");
    test(p, t, "a123");
    test(p, t, "1 + 2");
    test(p, t, "1 <=> 2 <=> 3");
    test(p, t, "1 + 2 * 3");
    test(p, t, "1 * 2 + 3");
    test(p, t, "an and android");
    test(p, t, "1 * 2 * 3");
    test(p, t, "1 . 2 . 3");
    test(p, t, "- 1 . - 2 . 3");
    test(p, t, "--1 * 2");
    test(p, t, "--1 * 2!");
    test(p, t, "1 * ((2 + 3))");
    test(p, t, "a -> b -> c");
    test(p, t, "(a -> b) -> c");
    test(p, t, "<a @ b");
    test(p, t, "a @ b < c");
    test(p, t, "1 then 2 then 3");
    test(p, t, "a(b)");
    test(p, t, "a(b, c)");
    test(p, t, "#(b)");
    test(p, t, "#(b, c)");
    test(p, t, "#a + b");
    test(p, t, "a + #b");
    test(p, t, "a #b");
    test(p, t, "a @ b @@ c @ d");
    test(p, t, "123456789012345678901234567890");
    test(p, t, "12345.6789i");

    // test("1 * ((2 + 3[4 + 5]))");
    // test("aa = bb = 3 ? 4 ? 5 : 6 : 7 ? 8 : 9");
    return 0;
}
