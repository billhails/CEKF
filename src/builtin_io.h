#ifndef cekf_builtin_io_h
#  define cekf_builtin_io_h
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

#  include <stdio.h>
#  include "builtins.h"

void markMemBufs(void);
void registerIO(BuiltIns *registry);
void putValue(Value x);
void fputValue(FILE *fh, Value x);
void putVec(Vec *x);
void fputVec(FILE *fh, Vec *x);

#endif
