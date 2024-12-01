#ifndef cekf_print_generator_h
#  define cekf_print_generator_h
/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2023  Bill Hails
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

#  include "lambda.h"
#  include "value.h"
#  include "tc.h"

LamLetRecBindings *makePrintFunctions(LamTypeDefList *typeDefs,
                                      LamLetRecBindings *rest,
                                      LamContext *env, bool inPreamble);
LamExp *makeSymbolExpr(ParserInfo I, char *name);
LamExp *makePrintInt(ParserInfo);
LamExp *makePrintChar(ParserInfo);
HashSymbol *makePrintName(char *prefix, char *name);
LamExp *stringToLamArgs(ParserInfo I, char *name);

#endif
