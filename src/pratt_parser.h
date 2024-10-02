#ifndef cekf_pratt_parser_h
#  define cekf_pratt_parser_h
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

#include "common.h"
#include "pratt.h"
#include "ast.h"

#ifdef DEBUG_PRATT_PARSER
void disablePrattDebug(void);
#endif
AstNest *top(PrattParser *);
void ppAstNest(PrattUTF8 *, AstNest *);
PrattParser *makePrattParser(void);
PrattTrie *makePrattTrie(PrattParser *parser, PrattTrie *C);
int initFileIdStack(void);
#endif
