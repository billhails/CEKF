#ifndef cekf_tpmc_logic_h
#  define cekf_tpmc_logic_h
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

#  include "ast.h"
#  include "lambda.h"
#include "parser_info.h"

LamLam *tpmcConvert(bool allow_unsafe, ParserInfo I, int nargs, int nbodies, AstArgList **argList,
                    LamExp **actions, LamContext *env);
#endif
