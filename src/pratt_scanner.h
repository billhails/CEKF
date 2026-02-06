#ifndef cekf_pratt_scanner_h
#define cekf_pratt_scanner_h
/*
 * CEKF - VM supporting amb
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

#include "pratt.h"
#include "symbol.h"

PrattLexer *makePrattLexerFromMbString(char *input, char *origin);
PrattTrie *insertPrattTrie(PrattTrie *current, HashSymbol *symbol);

void enqueueToken(PrattLexer *lexer, PrattToken *token);

ParserInfo LEXPI(PrattLexer *);

static inline ParserInfo BUFPI(PrattBufList *buf) {
    return (ParserInfo){.lineNo = buf->lineNo, .fileName = buf->fileName->name};
}

static inline ParserInfo TOKPI(PrattToken *token) {
    return (ParserInfo){.lineNo = token->lineNo,
                        .fileName = token->fileName->name};
}

PrattLexer *makePrattLexerFromFileName(char *fileName);
PrattBufList *prattBufListFromFileName(char *fileName, PrattBufList *next);

bool consume(PrattParser *, HashSymbol *);
bool check(PrattParser *, HashSymbol *);
bool match(PrattParser *, HashSymbol *);
PrattToken *peek(PrattParser *);
void poke(PrattParser *, PrattToken *);
PrattToken *next(PrattParser *);

void parserError(PrattParser *, const char *, ...)
    __attribute__((format(printf, 2, 3)));
void parserErrorAt(ParserInfo, PrattParser *, const char *, ...)
    __attribute__((format(printf, 3, 4)));

static inline bool isAtomSymbol(PrattToken *token, HashSymbol *symbol) {
    return (token->value->type == PRATTVALUE_TYPE_ATOM &&
            token->value->val.atom == symbol);
}

HashSymbol *TOK_ALIAS(void);
HashSymbol *TOK_ARROW(void);
HashSymbol *TOK_ASSERT(void);
HashSymbol *TOK_ASSIGN(void);
HashSymbol *TOK_AS(void);
HashSymbol *TOK_ATOM(void);
HashSymbol *TOK_BACK(void);
HashSymbol *TOK_BANG(void);
HashSymbol *TOK_BUILTINS(void);
HashSymbol *TOK_CHAR(void);
HashSymbol *TOK_CLOSE(void);
HashSymbol *TOK_COLON(void);
HashSymbol *TOK_COMMA(void);
HashSymbol *TOK_ELSE(void);
HashSymbol *TOK_EOF(void);
HashSymbol *TOK_EQ(void);
HashSymbol *TOK_ERROR(void);
HashSymbol *TOK_EXPORT(void);
HashSymbol *TOK_FN(void);
HashSymbol *TOK_HASH(void);
HashSymbol *TOK_IF(void);
HashSymbol *TOK_IMPORT(void);
HashSymbol *TOK_IN(void);
HashSymbol *TOK_KW_CHAR(void);
HashSymbol *TOK_KW_ERROR(void);
HashSymbol *TOK_KW_NUMBER(void);
HashSymbol *TOK_LCURLY(void);
HashSymbol *TOK_LEFT(void);
HashSymbol *TOK_LET(void);
HashSymbol *TOK_LINK(void);
HashSymbol *TOK_LAZY(void);
HashSymbol *TOK_LSQUARE(void);
HashSymbol *TOK_NAMESPACE(void);
HashSymbol *TOK_NONE(void);
HashSymbol *TOK_NUMBER(void);
HashSymbol *TOK_OPEN(void);
HashSymbol *TOK_OPERATOR(void);
HashSymbol *TOK_OPERATORS(void);
HashSymbol *TOK_PERIOD(void);
HashSymbol *TOK_PIPE(void);
HashSymbol *TOK_PRINT(void);
HashSymbol *TOK_RCURLY(void);
HashSymbol *TOK_RIGHT(void);
HashSymbol *TOK_RSQUARE(void);
HashSymbol *TOK_SEMI(void);
HashSymbol *TOK_STRING(void);
HashSymbol *TOK_SWITCH(void);
HashSymbol *TOK_TUPLE(void);
HashSymbol *TOK_TYPEDEF(void);
HashSymbol *TOK_TYPEOF(void);
HashSymbol *TOK_UNSAFE(void);
HashSymbol *TOK_WILDCARD(void);

#endif
