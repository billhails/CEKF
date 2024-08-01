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
#include "cmp.h"

Value nadd(Value, Value);
Value nsub(Value, Value);
Value nmul(Value, Value);
Value ndiv(Value, Value);
Value npow(Value, Value);
Value nmod(Value, Value);
Value nneg(Value);
Value nrand(Value);
Value real_part(Value);
Value imag_part(Value);
Value mag_part(Value);
Value theta_part(Value);

Cmp ncmp(Value, Value);

void init_arithmetic(void);
void markArithmetic(void);

#endif
