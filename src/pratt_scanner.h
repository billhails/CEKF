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

PrattLexer *makePrattLexer(PrattTrie *trie, char *input);
PrattTrie *insertPrattTrie(PrattTrie *current, HashSymbol *symbol);

PrattToken *next(PrattLexer *lexer);

PrattToken *peek(PrattLexer *lexer);

static inline ParserInfo TOKPI(PrattToken *token) { return (ParserInfo) { .lineno = token->lineno, .filename = token->filename->name }; }

static inline HashSymbol *S(char *name) { return newSymbol(name); }

PrattBufList *prattBufListFromFileName(char *fileName, PrattBufList *next);
PrattBufList *prattBufListFromString(char *origin, char *string, PrattBufList *next);
#endif
