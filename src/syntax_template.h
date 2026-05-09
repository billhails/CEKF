#ifndef cekf_syntax_template_h
#define cekf_syntax_template_h
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

#include "ast.h"
#include "pratt.h"

HashSymbol *prattSyntaxQuoteSymbol(void);
HashSymbol *prattSyntaxUnquoteSymbol(void);
bool prattIsSyntaxQuoteToken(PrattToken *token);
bool prattIsSyntaxPatternBoundaryToken(PrattToken *token);

HashSymbol *prattSyntaxSymbol(PrattParser *parser);
SymbolArray *prattParseSyntaxCallArguments(PrattParser *parser);
SymbolArray *prattParseOptionalSyntaxParameters(PrattParser *parser);
PrattMacroAlternative *
prattParseSyntaxAlternative(PrattParser *parser, SymbolArray *parameters,
                            PrattSyntaxResultKind expectedResultKind);
AstSyntaxTemplate *
prattConvertSyntaxExprTemplate(PrattParser *parser, ParserInfo PI,
                               AstExpression *template, SymbolArray *parameters,
                               PrattMacroPatternItems *patternItems,
                               bool quotedTemplate);
AstSyntaxTemplate *
prattConvertSyntaxDefTemplate(PrattParser *parser, ParserInfo PI,
                              AstExpression *template, SymbolArray *parameters,
                              PrattMacroPatternItems *patternItems,
                              bool quotedTemplate);

#endif