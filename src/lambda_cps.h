#ifndef cekf_lambda_cps_h
#  define cekf_lambda_cps_h
/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2025  Bill Hails
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

#include "lambda.h"
#include "cps_kont.h"

LamExp *cpsTk(LamExp *, CpsKont *);
LamArgs *appendLamArg(LamArgs *, LamExp *);
LamExp *makeVar(ParserInfo, char *);
LamVarList *appendLamVar(ParserInfo, LamVarList *, HashSymbol *);
LamExp *cpsTs_k(LamExp *, CpsKont *);
bool isAexpr(LamExp *);
LamExp *cpsTc(LamExp *, LamExp *);
LamExp *cpsM(LamExp *);
LamBindings *mapMOverBindings(LamBindings *);
LamMatchList *mapTcOverMatchCases(LamMatchList *, LamExp *);
void cpsUnzipLamBindings(LamBindings *, LamVarList **, LamArgs **);
LamExp *cpsNestLets(LamBindings *, LamExp *);

#endif
