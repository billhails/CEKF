#ifndef cekf_syntax_parse_h
#define cekf_syntax_parse_h
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

PrattMacroSpec *prattFindLocalSyntaxSpecForHead(PrattParser *parser,
                                                HashSymbol *head);
PrattMacroSpec *
prattFindLocalSyntaxSpecForHeadAndEntry(PrattParser *parser, HashSymbol *head,
                                        PrattSyntaxEntryKind entryKind);
PrattMacroSpec *prattFindSyntaxSpecByName(PrattParser *parser,
                                          HashSymbol *name);
PrattMacroSpec *prattFindSyntaxSpecForHead(PrattParser *parser,
                                           HashSymbol *head);
PrattMacroSpec *
prattFindSyntaxSpecForHeadAndEntry(PrattParser *parser, HashSymbol *head,
                                   PrattSyntaxEntryKind entryKind);

void prattValidateSyntaxAlternatives(PrattParser *parser, HashSymbol *ruleName,
                                     PrattMacroAlternatives *alternatives);

AstExpression *prattExpandSyntaxExprWithBindings(PrattParser *parser,
                                                 PrattToken *tok,
                                                 PrattMacroSpec *spec,
                                                 bool headAlreadyConsumed,
                                                 SyntaxExprBindings *inherited);
AstExpression *prattExpandSyntaxExpr(PrattParser *parser, PrattToken *tok,
                                     PrattMacroSpec *spec);
AstDefinition *prattExpandSyntaxDef(PrattParser *parser, PrattToken *tok,
                                    PrattMacroSpec *spec);
int prattNextDeclarationId(void);
AstSyntaxPatternItems *prattConvertPatternItems(ParserInfo PI,
                                                PrattMacroPatternItems *items);

#endif