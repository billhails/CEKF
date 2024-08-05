#ifndef cekf_builtins_helper_h
#  define cekf_builtin_helper_h
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

#  include "cekfs.h"
#  include "builtins.h"

BuiltIns *registerBuiltIns(int argc, int cargc, char *argv[]);
struct Value makeTryResult(int code, struct Value val);
struct Value makeSome(struct Value val);
struct Value makeNothing(void);
struct Value makeEmptyList(void);
struct Value makeBasic(struct Value v, int code);
TcType *pushIntegerArg(BuiltInArgs *args);
TcType *pushCharacterArg(BuiltInArgs *args);
void pushNewBuiltIn(BuiltIns *reg, char *name, TcType *ret, BuiltInArgs *args, void *impl);
TcType *pushStringArg(BuiltInArgs *args);
TcType *pushAnyArg(BuiltInArgs *args);


#endif
