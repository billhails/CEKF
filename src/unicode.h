#ifndef cekf_unicode_h
#  define cekf_unicode_h
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

#include <stdbool.h>
#include "types.h"

#define UNICODE_MAX 0x10FFFD

#define GC_MASK 0x07

#define GC_L    0x00
#define GC_M    0x01
#define GC_N    0x02
#define GC_P    0x03
#define GC_S    0x04
#define GC_Z    0x05
#define GC_C    0x06

#define GC_Ll   (GC_L | (0x00 << 3))
#define GC_Lm   (GC_L | (0x01 << 3))
#define GC_Lo   (GC_L | (0x02 << 3))
#define GC_Lt   (GC_L | (0x03 << 3))
#define GC_Lu   (GC_L | (0x04 << 3))

#define GC_Mc   (GC_M | (0x00 << 3))
#define GC_Me   (GC_M | (0x01 << 3))
#define GC_Mn   (GC_M | (0x02 << 3))

#define GC_Nd   (GC_N | (0x00 << 3))
#define GC_Nl   (GC_N | (0x01 << 3))
#define GC_No   (GC_N | (0x02 << 3))

#define GC_Pc   (GC_P | (0x00 << 3))
#define GC_Pd   (GC_P | (0x01 << 3))
#define GC_Pe   (GC_P | (0x02 << 3))
#define GC_Pf   (GC_P | (0x03 << 3))
#define GC_Pi   (GC_P | (0x04 << 3))
#define GC_Po   (GC_P | (0x05 << 3))
#define GC_Ps   (GC_P | (0x06 << 3))

#define GC_Sc   (GC_S | (0x00 << 3))
#define GC_Sk   (GC_S | (0x01 << 3))
#define GC_Sm   (GC_S | (0x02 << 3))
#define GC_So   (GC_S | (0x03 << 3))

#define GC_Zl   (GC_Z | (0x00 << 3))
#define GC_Zp   (GC_Z | (0x01 << 3))
#define GC_Zs   (GC_Z | (0x02 << 3))

#define GC_Cc   (GC_C | (0x00 << 3))
#define GC_Cf   (GC_C | (0x01 << 3))
#define GC_Co   (GC_C | (0x02 << 3))
#define GC_Cs   (GC_C | (0x03 << 3))
#define GC_Cn   (GC_C | (0x04 << 3))

bool unicode_isalnum(Character c);
bool unicode_isalpha(Character c);
bool unicode_isascii(Character c);
bool unicode_isblank(Character c);
bool unicode_iscntrl(Character c);
bool unicode_isdigit(Character c);
bool unicode_isgraph(Character c);
bool unicode_islower(Character c);
bool unicode_isnumber(Character c); // includes digits, roman numerals, vulgar fractions etc.
bool unicode_isopen(Character c);
bool unicode_isprint(Character c);
bool unicode_ispunct(Character c);
bool unicode_isspace(Character c);
bool unicode_issymbol(Character c);
bool unicode_isupper(Character c);
bool unicode_isvalid(Character c);
bool unicode_isxdigit(Character c);
int unicode_category(Character c);
int unicode_getdec(Character c);

#endif
