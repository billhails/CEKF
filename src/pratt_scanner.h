#ifndef cekf_pratt_scanner_h
#  define cekf_pratt_scanner_h
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

#include "pratt.h"
#include "symbol.h"

PrattLexer *makePrattLexer(PrattTrie *trie, char *input, char *origin);
PrattTrie *insertPrattTrie(PrattTrie *current, HashSymbol *symbol);

void enqueueToken(PrattLexer *lexer, PrattToken *token);

ParserInfo LEXPI(PrattLexer *);

static inline ParserInfo BUFPI(PrattBufList *buf) {
    return (ParserInfo) { .lineno = buf->lineno, .filename = buf->filename->name };
}

static inline ParserInfo TOKPI(PrattToken *token) {
    return (ParserInfo) { .lineno = token->lineno, .filename = token->filename->name };
}

PrattLexer *makePrattLexerFromFilename(PrattTrie *trie, char *filename);
PrattBufList *prattBufListFromFileName(char *fileName, PrattBufList *next);
PrattBufList *prattBufListFromString(char *string, char *origin, PrattBufList *next);
bool consume(PrattParser *, HashSymbol *);
bool check(PrattParser *, HashSymbol *);
bool match(PrattParser *, HashSymbol *);
PrattToken *next(PrattParser *);

void parserError(PrattParser *parser, const char *message, ...) __attribute__((format(printf, 2, 3)));
PrattToken *peek(PrattParser *);

HashSymbol *TOK_ALIAS(void);
HashSymbol *TOK_AND(void);
HashSymbol *TOK_APPEND(void);
HashSymbol *TOK_ARROW(void);
HashSymbol *TOK_ASSERT(void);
HashSymbol *TOK_ASSIGN(void);
HashSymbol *TOK_AS(void);
HashSymbol *TOK_ATOM(void);
HashSymbol *TOK_BACK(void);
HashSymbol *TOK_BANG(void);
HashSymbol *TOK_CHAR(void);
HashSymbol *TOK_CLOSE(void);
HashSymbol *TOK_CMP(void);
HashSymbol *TOK_COLON(void);
HashSymbol *TOK_COMMA(void);
HashSymbol *TOK_CONS(void);
HashSymbol *TOK_DIVIDE(void);
HashSymbol *TOK_ELSE(void);
HashSymbol *TOK_EOF(void);
HashSymbol *TOK_EQ(void);
HashSymbol *TOK_ERROR(void);
HashSymbol *TOK_EXP(void);
HashSymbol *TOK_FN(void);
HashSymbol *TOK_GE(void);
HashSymbol *TOK_GT(void);
HashSymbol *TOK_HASH(void);
HashSymbol *TOK_HERE(void);
HashSymbol *TOK_IF(void);
HashSymbol *TOK_INFIX(void);
HashSymbol *TOK_IN(void);
HashSymbol *TOK_KW_CHAR(void);
HashSymbol *TOK_KW_ERROR(void);
HashSymbol *TOK_KW_NUMBER(void);
HashSymbol *TOK_LCURLY(void);
HashSymbol *TOK_LET(void);
HashSymbol *TOK_LE(void);
HashSymbol *TOK_LINK(void);
HashSymbol *TOK_LSQUARE(void);
HashSymbol *TOK_LT(void);
HashSymbol *TOK_MINUS(void);
HashSymbol *TOK_MOD(void);
HashSymbol *TOK_NAMESPACE(void);
HashSymbol *TOK_NAND(void);
HashSymbol *TOK_NE(void);
HashSymbol *TOK_NOR(void);
HashSymbol *TOK_NOT(void);
HashSymbol *TOK_NS(void);
HashSymbol *TOK_NUMBER(void);
HashSymbol *TOK_OPEN(void);
HashSymbol *TOK_OR(void);
HashSymbol *TOK_PERIOD(void);
HashSymbol *TOK_PIPE(void);
HashSymbol *TOK_PLUS(void);
HashSymbol *TOK_POSTFIX(void);
HashSymbol *TOK_PREFIX(void);
HashSymbol *TOK_PRINT(void);
HashSymbol *TOK_RCURLY(void);
HashSymbol *TOK_RSQUARE(void);
HashSymbol *TOK_SEMI(void);
HashSymbol *TOK_STRING(void);
HashSymbol *TOK_SWITCH(void);
HashSymbol *TOK_THEN(void);
HashSymbol *TOK_TIMES(void);
HashSymbol *TOK_TUPLE(void);
HashSymbol *TOK_TYPEDEF(void);
HashSymbol *TOK_UNSAFE(void);
HashSymbol *TOK_WILDCARD(void);
HashSymbol *TOK_XNOR(void);
HashSymbol *TOK_XOR(void);

#endif
