#ifndef cekf_types_h
#  define cekf_types_h
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
 *
 * typedefs to allow different sizes of data and to avoid using primitive types directly.
 * for example we may want characters to be wchar_t at some point.
 */

#  include <stdint.h>
#  include <stddef.h>
#  include <wchar.h>

/*
 * on my machine:
 *
 * char: 8
 * char *: 64
 * wchar_t: 32
 * short int: 16
 * int: 32
 * long int: 64
 * long long int: 64
 * float: 32
 * double: 64
 * long double: 128
 * size_t: 64
 */

typedef uint8_t Byte;
typedef char Character;
typedef unsigned short int Word;
typedef int Integer;
typedef unsigned int Index;
typedef double Double;
typedef size_t Control;

#define END_CONTROL UINT64_MAX

 #endif
