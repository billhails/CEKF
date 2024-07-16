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

#define UTF8_ONE_BYTE_MASK         0b10000000
#define UTF8_ONE_BYTE_FLAG         0b00000000
#define UTF8_ONE_BYTE_PAYLOAD      (~UTF8_ONE_BYTE_MASK)

#define UTF8_TWO_BYTE_START        0x80
#define UTF8_TWO_BYTE_MASK         0b11100000
#define UTF8_TWO_BYTE_FLAG         0b11000000
#define UTF8_TWO_BYTE_PAYLOAD      (~UTF8_TWO_BYTE_MASK)

#define UTF8_THREE_BYTE_START      0x800
#define UTF8_THREE_BYTE_MASK       0b11110000
#define UTF8_THREE_BYTE_FLAG       0b11100000
#define UTF8_THREE_BYTE_PAYLOAD    (~UTF8_THREE_BYTE_MASK)

#define UTF8_FOUR_BYTE_START       0x10000
#define UTF8_FOUR_BYTE_MASK        0b11111000
#define UTF8_FOUR_BYTE_FLAG        0b11110000
#define UTF8_FOUR_BYTE_PAYLOAD     (~UTF8_FOUR_BYTE_MASK)
#define UTF8_FOUR_BYTE_END         0x10FFFF

#define UTF8_TRAILING_BYTE_MASK    0b11000000
#define UTF8_TRAILING_BYTE_FLAG    0b10000000
#define UTF8_TRAILING_BYTE_PAYLOAD (~UTF8_TRAILING_BYTE_MASK)
#define UTF8_TRAILING_BYTE_SIZE    6

static inline bool isOneByteUtf8(Byte x) { return (x & UTF8_ONE_BYTE_MASK) == UTF8_ONE_BYTE_FLAG; }
static inline bool isTwoByteUtf8(Byte x) { return (x & UTF8_TWO_BYTE_MASK) == UTF8_TWO_BYTE_FLAG; }
static inline bool isThreeByteUtf8(Byte x) { return (x & UTF8_THREE_BYTE_MASK) == UTF8_THREE_BYTE_FLAG; }
static inline bool isFourByteUtf8(Byte x) { return (x & UTF8_FOUR_BYTE_MASK) == UTF8_FOUR_BYTE_FLAG; }
static inline bool isTrailingByteUtf8(Byte x) { return (x & UTF8_TRAILING_BYTE_MASK) == UTF8_TRAILING_BYTE_FLAG; }

int decodedLength(unsigned char *string);
unsigned char *utf8_to_unicode_char(wchar_t *dest, unsigned char *src);
void utf8_to_unicode_string(wchar_t *dest, unsigned char *src);
int byteSize(wchar_t c);
int encodedLength(wchar_t *s);
unsigned char *writeChar(unsigned char *dest, wchar_t character);
void unicode_to_utf8_string(unsigned char *dest, wchar_t *src);
char *listToUtf8(Value v);
Value utf8ToList(const char *utf8);
wchar_t utf8Fgetc(FILE *fh);


#endif
