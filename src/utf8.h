#ifndef cekf_utf8_h
#  define cekf_utf8_h
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

#include <stdlib.h>
#include "cekfs.h"

int decodedLength(unsigned char *string);
unsigned char *utf8_to_unicode_char(wchar_t *dest, unsigned char *src);
void utf8_to_unicode_string(wchar_t *dest, unsigned char *src);
int byteSize(wchar_t c);
int encodedLength(wchar_t *s);
unsigned char *writeChar(unsigned char *dest, wchar_t character);
void unicode_to_utf8_string(unsigned char *dest, wchar_t *src);
char *listToUtf8(Value v);
Value utf8ToList(const char *utf8);


#endif
