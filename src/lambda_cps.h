#ifndef cekf_lambda_cps_h
#define cekf_lambda_cps_h
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

#include "cps_kont.h"
#include "minlam.h"

MinExp *cpsTk(MinExp *, CpsKont *);
MinExprList *appendMinArg(MinExprList *, MinExp *);
MinExp *makeVar(ParserInfo, char *);
SymbolList *appendMinVar(ParserInfo, SymbolList *, HashSymbol *);
MinExp *cpsTs_k(MinExp *, CpsKont *);
bool isAexpr(MinExp *);
MinExp *cpsTc(MinExp *, MinExp *);
MinExp *cpsM(MinExp *);
MinBindings *mapMOverBindings(MinBindings *);
MinMatchList *mapTcOverMatchCases(MinMatchList *, MinExp *);
void cpsUnzipMinBindings(MinBindings *, SymbolList **, MinExprList **);
MinExp *cpsNestLets(MinBindings *, MinExp *);
MinExp *nsaToArgs(MinNameSpaceArray *nsa);
MinNameSpaceArray *argsToNsa(MinExp *seq);

#endif
