#ifndef cekf_debug_tin_h
#define cekf_debug_tin_h
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

// generated by makeAST.py



#include "tin_helper.h"

void printTinFunctionApplication(struct TinFunctionApplication * x, int depth);
void printTinMonoTypeList(struct TinMonoTypeList * x, int depth);
void printTinTypeQuantifier(struct TinTypeQuantifier * x, int depth);
void printTinContext(struct TinContext * x, int depth);
void printTinSubstitution(struct TinSubstitution * x, int depth);
void printTinArgsResult(struct TinArgsResult * x, int depth);
void printTinMonoType(struct TinMonoType * x, int depth);
void printTinPolyType(struct TinPolyType * x, int depth);

#endif
