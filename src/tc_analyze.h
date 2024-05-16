#ifndef cekf_tc_analyze_h
#  define cekf_tc_analyze_h
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

#  include "tc.h"
#  include "lambda.h"
#  include "builtins.h"

#define NS_GLOBAL -1
#define NS_UNKNOWN -2

TcEnv *tc_init(BuiltIns *);
TcType *tc_analyze(LamExp *exp, TcEnv *env);
TcType *lookupNsRef(int index, TcEnv *env);
bool getFromTcEnv(TcEnv *env, HashSymbol *symbol, TcType **type);

#endif
