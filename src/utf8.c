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

#include "common.h"
#include "utf8.h"
#include "cekfs.h"
#include "cekf.h"

#define ONE_BYTE_MASK         0b10000000
#define ONE_BYTE_FLAG         0b00000000
#define ONE_BYTE_PAYLOAD      (~ONE_BYTE_MASK)

#define TWO_BYTE_START        0x80
#define TWO_BYTE_MASK         0b11100000
#define TWO_BYTE_FLAG         0b11000000
#define TWO_BYTE_PAYLOAD      (~TWO_BYTE_MASK)

#define THREE_BYTE_START      0x800
#define THREE_BYTE_MASK       0b11110000
#define THREE_BYTE_FLAG       0b11100000
#define THREE_BYTE_PAYLOAD    (~THREE_BYTE_MASK)

#define FOUR_BYTE_START       0x10000
#define FOUR_BYTE_MASK        0b11111000
#define FOUR_BYTE_FLAG        0b11110000
#define FOUR_BYTE_PAYLOAD     (~FOUR_BYTE_MASK)
#define FOUR_BYTE_END         0x10FFFF

#define TRAILING_BYTE_MASK    0b11000000
#define TRAILING_BYTE_FLAG    0b10000000
#define TRAILING_BYTE_PAYLOAD (~TRAILING_BYTE_MASK)
#define TRAILING_BYTE_SIZE    6

#define IS_ONE_BYTE(x) (((x) & ONE_BYTE_MASK) == ONE_BYTE_FLAG)
#define IS_TWO_BYTE(x) (((x) & TWO_BYTE_MASK) == TWO_BYTE_FLAG)
#define IS_THREE_BYTE(x) (((x) & THREE_BYTE_MASK) == THREE_BYTE_FLAG)
#define IS_FOUR_BYTE(x) (((x) & FOUR_BYTE_MASK) == FOUR_BYTE_FLAG)
#define IS_TRAILING_BYTE(x) (((x) & TRAILING_BYTE_MASK) == TRAILING_BYTE_FLAG)

typedef enum ParseState {
    START,
    TWO_BYTE,
    THREE_BYTE,
    FOUR_BYTE,
    ERROR
} ParseState;

// validates and returns the number of characters in the utf8 encoded string
// returns -1 if the string is invalid
int decodedLength(unsigned char *string) {
    int len = 0;

    ParseState state = START;

    while (*string) {
        switch (state) {
            case FOUR_BYTE:
                state = IS_TRAILING_BYTE(*string) ? THREE_BYTE : ERROR;
                break;
            case THREE_BYTE:
                state = IS_TRAILING_BYTE(*string) ? TWO_BYTE : ERROR;
                break;
            case TWO_BYTE:
                state = IS_TRAILING_BYTE(*string) ? START : ERROR;
                break;
            case START:
                if (IS_FOUR_BYTE(*string)) {
                    ++len;
                    state = FOUR_BYTE;
                }
                else if (IS_THREE_BYTE(*string)) {
                    ++len;
                    state = THREE_BYTE;
                }
                else if (IS_TWO_BYTE(*string)) {
                    ++len;
                    state = TWO_BYTE;
                }
                else if (IS_ONE_BYTE(*string)) {
                    ++len;
                    state = START;
                }
                else {
                    state = ERROR;
                }
                break;
            case ERROR:
                return -1;
        }
        ++string;
    }

    return state == START ? len : -1;
}

// translates a utf8-encoded character to a wchar_t.
// assumes the string has been validated.
// assumes the string has at least one character in it.
// returns the char * pointer to the next UTF-8 character.
unsigned char *utf8_to_unicode_char(wchar_t *dest, unsigned char *src) {
    ParseState state = START;
    do {
        switch (state) {
            case START:
                if (IS_FOUR_BYTE(*src)) {
                    *dest = *src & FOUR_BYTE_PAYLOAD;
                    state = FOUR_BYTE;
                }
                else if (IS_THREE_BYTE(*src)) {
                    *dest = *src & THREE_BYTE_PAYLOAD;
                    state = THREE_BYTE;
                }
                else if (IS_TWO_BYTE(*src)) {
                    *dest = *src & TWO_BYTE_PAYLOAD;
                    state = TWO_BYTE;
                }
                else if (IS_ONE_BYTE(*src)) {
                    *dest = *src & ONE_BYTE_PAYLOAD;
                }
                break;
            case TWO_BYTE:
                *dest <<= TRAILING_BYTE_SIZE;
                *dest |= (*src & TRAILING_BYTE_PAYLOAD);
                state = START;
                break;
            case THREE_BYTE:
                *dest <<= TRAILING_BYTE_SIZE;
                *dest |= (*src & TRAILING_BYTE_PAYLOAD);
                state = TWO_BYTE;
                break;
            case FOUR_BYTE:
                *dest <<= TRAILING_BYTE_SIZE;
                *dest |= (*src & TRAILING_BYTE_PAYLOAD);
                state = THREE_BYTE;
                break;
            case ERROR:
                cant_happen("error state");
        }
        ++src;
    } while (state != START);
    return src;
}

// translates a utf8-encoded string to a wchar_t array.
// assumes the string has been validated.
// assumes the target array is big enough.
// appends a trailing NULL wchar_t.
void utf8_to_unicode_string(wchar_t *dest, unsigned char *src) {
    while (*src) {
        src = utf8_to_unicode_char(dest, src);
        ++dest;
    }
    *dest = 0;
}

// returns the number of bytes required to convert the unicode wchar_t
// to UTF-8, not including the trailing NULL
int byteSize(wchar_t c) {
    if (c < TWO_BYTE_START) return 1;
    if (c < THREE_BYTE_START) return 2;
    if (c < FOUR_BYTE_START) return 3;
    if (c <= FOUR_BYTE_END) return 4;
    cant_happen("maximum unicode character exceeded: %x", c);
}

// returns the number of bytes required to convert the unicode wchar_t
// array to UTF-8, not including the trailing NULL
int encodedLength(wchar_t *s) {
    int size = 0;
    while (*s) {
        size += byteSize(*s);
        s++;
    }
    return size;
}

// writes the wchar_t to the string, returns the pointer
// past the end of the char written, assumes there is enough space in
// the string, does not append a trailing NULL
unsigned char *writeChar(unsigned char *utf8, wchar_t unicode) {
    switch (byteSize(unicode)) {
        case 1:
            *utf8++ = (unsigned char) unicode;
            break;
        case 2:
            *utf8++ = (unicode >> TRAILING_BYTE_SIZE) | TWO_BYTE_FLAG;
            *utf8++ = (unicode & TRAILING_BYTE_PAYLOAD) | TRAILING_BYTE_FLAG;
            break;
        case 3:
            *utf8++ = (unicode >> (TRAILING_BYTE_SIZE * 2)) | THREE_BYTE_FLAG;
            *utf8++ = ((unicode >> TRAILING_BYTE_SIZE) & TRAILING_BYTE_PAYLOAD) | TRAILING_BYTE_FLAG;
            *utf8++ = (unicode & TRAILING_BYTE_PAYLOAD) | TRAILING_BYTE_FLAG;
            break;
        case 4:
            *utf8++ = (unicode >> (TRAILING_BYTE_SIZE * 3)) | FOUR_BYTE_FLAG;
            *utf8++ = ((unicode >> (TRAILING_BYTE_SIZE * 2)) & TRAILING_BYTE_PAYLOAD) | TRAILING_BYTE_FLAG;
            *utf8++ = ((unicode >> TRAILING_BYTE_SIZE) & TRAILING_BYTE_PAYLOAD) | TRAILING_BYTE_FLAG;
            *utf8++ = (unicode & TRAILING_BYTE_PAYLOAD) | TRAILING_BYTE_FLAG;
            break;
        default:
            cant_happen("invalid byteSize");
    }
    return utf8;
}

// converts the wchar_t array to a utf8 string
// expects the wchar_t array to have a trailing null wchar_t
// assumes there is enough space in dest
// appends a trailing null byte.
void unicode_to_utf8_string(unsigned char *dest, wchar_t *src) {
    while (*src) {
        dest = writeChar(dest, *src);
        src++;
    }
    *dest = '\0';
}

// converts a list of char to a utf8 string.
char *listToUtf8(Value v) {
#ifdef SAFETY_CHECKS
    if (v.type != VALUE_TYPE_VEC) {
        cant_happen("unexpected %s", valueTypeName(v.type));
    }
#endif
    CharArray *unicode = listToCharArray(v);
    int save = PROTECT(unicode);
    int size = encodedLength(unicode->entries);
    unsigned char *buf = NEW_ARRAY(unsigned char, size + 1);
    unicode_to_utf8_string(buf, unicode->entries);
    UNPROTECT(save);
    return (char *)buf;
}

// converts a utf8 string to a list of char (Value)
// returns the empty list if the string is invalid
Value utf8ToList(const char *utf8) {
    int size = decodedLength((unsigned char *)utf8);
    CharArray *unicode = newCharArray();
    int save = PROTECT(unicode);
    if (size == -1) {
        pushCharArray(unicode, (Character) 0);
    } else {
        extendCharArray(unicode, (Index)(size + 1));
        utf8_to_unicode_string(unicode->entries, (unsigned char *)utf8);
        unicode->size = (Index)(size + 1);
    }
    Value v = charArrayToList(unicode);
    UNPROTECT(save);
    return v;
}
