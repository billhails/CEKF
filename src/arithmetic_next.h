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

#ifndef cekf_arithmetic_next_h
#define cekf_arithmetic_next_h

#include "cmp.h"
#include "value.h"

Value nadd(Value left, Value right);
Value nsub(Value left, Value right);
Value nmul(Value left, Value right);
Value ndiv(Value left, Value right);
Value npow(Value left, Value right);
Value nmod(Value left, Value right);
Value nneg(Value value);
Value nrand(Value prev);
Value real_part(Value value);
Value imag_part(Value value);
Value mag_part(Value value);
Value theta_part(Value value);
Cmp ncmp(Value left, Value right);

#endif
