#ifndef cekf_tc_pp_h
#define cekf_tc_pp_h
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

#include "tc.h"
#include <stdio.h>

void ppTcType(FILE *out, TcType *type);
void ppTcFunction(FILE *out, TcFunction *function);
void ppTcPair(FILE *out, TcPair *pair);
void ppTcThunk(FILE *out, TcThunk *thunk);
void ppTcVar(FILE *out, TcVar *var);
void ppTcTypeSig(FILE *out, TcTypeSig *typeSig);
void ppTcTuple(FILE *out, TcTypeArray *tuple);
void ppTcEnv(FILE *out, TcEnv *env);

#endif
