#ifndef cekf_arithmetic_h
#define cekf_arithmetic_h
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

#include "value.h"

typedef Value (*IntegerBinOp)(Value, Value);

extern IntegerBinOp add;
extern IntegerBinOp sub;
extern IntegerBinOp mul;
extern IntegerBinOp divide;
extern IntegerBinOp power;
extern IntegerBinOp modulo;

void init_arithmetic(void);

#endif
