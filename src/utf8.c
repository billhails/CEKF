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
#include "unicode.h"
#include "cekfs.h"
#include "cekf.h"

typedef enum ParseState {
    START,
    TWO_BYTE,
    THREE_BYTE,
    FOUR_BYTE,
    ERROR,
    RECOVERY
} ParseState;

// validates and returns the number of characters in the utf8 encoded string
// returns -1 if the string is invalid
int decodedLength(unsigned char *string) {
    int len = 0;

    ParseState state = START;

    while (*string) {
        switch (state) {
            case FOUR_BYTE:
                state = isTrailingByteUtf8(*string) ? THREE_BYTE : ERROR;
                break;
            case THREE_BYTE:
                state = isTrailingByteUtf8(*string) ? TWO_BYTE : ERROR;
                break;
            case TWO_BYTE:
                state = isTrailingByteUtf8(*string) ? START : ERROR;
                break;
            case START:
                if (isFourByteUtf8(*string)) {
                    ++len;
                    state = FOUR_BYTE;
                }
                else if (isThreeByteUtf8(*string)) {
                    ++len;
                    state = THREE_BYTE;
                }
                else if (isTwoByteUtf8(*string)) {
                    ++len;
                    state = TWO_BYTE;
                }
                else if (isOneByteUtf8(*string)) {
                    ++len;
                    state = START;
                }
                else {
                    state = ERROR;
                }
                break;
            case RECOVERY:
            case ERROR:
                return -1;
        }
        ++string;
    }

    return state == START ? len : -1;
}

// generic parse state transition function
static ParseState transition(ParseState state, wchar_t *dest, Byte src) {
    switch (state) {
        case RECOVERY:
        case START:
            if (isFourByteUtf8(src)) {
                *dest = src & UTF8_FOUR_BYTE_PAYLOAD;
                state = FOUR_BYTE;
            } else if (isThreeByteUtf8(src)) {
                *dest = src & UTF8_THREE_BYTE_PAYLOAD;
                state = THREE_BYTE;
            } else if (isTwoByteUtf8(src)) {
                *dest = src & UTF8_TWO_BYTE_PAYLOAD;
                state = TWO_BYTE;
            } else if (isOneByteUtf8(src)) {
                *dest = src & UTF8_ONE_BYTE_PAYLOAD;
                state = START;
            } else if (isTrailingByteUtf8(src)) {
                state = RECOVERY;
            }
            break;
        case TWO_BYTE:
            if (isTrailingByteUtf8(src)) {
                *dest <<= UTF8_TRAILING_BYTE_SIZE;
                *dest |= (src & UTF8_TRAILING_BYTE_PAYLOAD);
                state = START;
            } else {
                state = ERROR;
            }
            break;
        case THREE_BYTE:
            if (isTrailingByteUtf8(src)) {
                *dest <<= UTF8_TRAILING_BYTE_SIZE;
                *dest |= (src & UTF8_TRAILING_BYTE_PAYLOAD);
                state = TWO_BYTE;
            } else {
                state = ERROR;
            }
            break;
        case FOUR_BYTE:
            if (isTrailingByteUtf8(src)) {
                *dest <<= UTF8_TRAILING_BYTE_SIZE;
                *dest |= (src & UTF8_TRAILING_BYTE_PAYLOAD);
                state = THREE_BYTE;
            } else {
                state = ERROR;
            }
            break;
        case ERROR:
            *dest = 0xfffd;
            state = START;
            break;
    }
    return state;
}

// fetches the next whole unicode character from the argument file handle.
// will skip any initial trailing bytes.
// Will return '\0' on EOF.
// will return 0xfffd (unicode replacement character) on error.
wchar_t utf8Fgetc(FILE *fh) {
    ParseState state = START;
    wchar_t dest = 0;
    do {
        int c = fgetc(fh);
        if (c == EOF) return 0;
        Byte src = (Byte) c;
        state = transition(state, &dest, src);
    } while (state != START);
    return dest;
}

// fetches the next whole unicode character from the argument string,
// assigning it to the argument *dest pointer.
// will skip any initial trailing bytes.
// Returns the pointer to the start of the next character.
// will assign 0xfffd (unicode replacement character) on error.
unsigned char *utf8Sgetc(unsigned char *string, wchar_t *dest) {
    ParseState state = START;
    *dest = 0;
    do {
        Byte src = *string;
        if (src == 0) {
            *dest = 0;
            return string;
        }
        state = transition(state, dest, src);
        string++;
    } while (state != START);
    return string;
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
                if (isFourByteUtf8(*src)) {
                    *dest = *src & UTF8_FOUR_BYTE_PAYLOAD;
                    state = FOUR_BYTE;
                }
                else if (isThreeByteUtf8(*src)) {
                    *dest = *src & UTF8_THREE_BYTE_PAYLOAD;
                    state = THREE_BYTE;
                }
                else if (isTwoByteUtf8(*src)) {
                    *dest = *src & UTF8_TWO_BYTE_PAYLOAD;
                    state = TWO_BYTE;
                }
                else if (isOneByteUtf8(*src)) {
                    *dest = *src & UTF8_ONE_BYTE_PAYLOAD;
                }
                break;
            case TWO_BYTE:
                *dest <<= UTF8_TRAILING_BYTE_SIZE;
                *dest |= (*src & UTF8_TRAILING_BYTE_PAYLOAD);
                state = START;
                break;
            case THREE_BYTE:
                *dest <<= UTF8_TRAILING_BYTE_SIZE;
                *dest |= (*src & UTF8_TRAILING_BYTE_PAYLOAD);
                state = TWO_BYTE;
                break;
            case FOUR_BYTE:
                *dest <<= UTF8_TRAILING_BYTE_SIZE;
                *dest |= (*src & UTF8_TRAILING_BYTE_PAYLOAD);
                state = THREE_BYTE;
                break;
            case RECOVERY:
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
    if (c < UTF8_TWO_BYTE_START) return 1;
    if (c < UTF8_THREE_BYTE_START) return 2;
    if (c < UTF8_FOUR_BYTE_START) return 3;
    if (c <= UTF8_FOUR_BYTE_END) return 4;
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

// writes the wchar_t to the string as a utf8 byte sequence, returns the pointer
// past the end of the char written, assumes there is enough space in
// the string, does not append a trailing NULL
unsigned char *writeChar(unsigned char *utf8, wchar_t unicode) {
    switch (byteSize(unicode)) {
        case 1:
            *utf8++ = (unsigned char) unicode;
            break;
        case 2:
            *utf8++ = (unicode >> UTF8_TRAILING_BYTE_SIZE) | UTF8_TWO_BYTE_FLAG;
            *utf8++ = (unicode & UTF8_TRAILING_BYTE_PAYLOAD) | UTF8_TRAILING_BYTE_FLAG;
            break;
        case 3:
            *utf8++ = (unicode >> (UTF8_TRAILING_BYTE_SIZE * 2)) | UTF8_THREE_BYTE_FLAG;
            *utf8++ = ((unicode >> UTF8_TRAILING_BYTE_SIZE) & UTF8_TRAILING_BYTE_PAYLOAD) | UTF8_TRAILING_BYTE_FLAG;
            *utf8++ = (unicode & UTF8_TRAILING_BYTE_PAYLOAD) | UTF8_TRAILING_BYTE_FLAG;
            break;
        case 4:
            *utf8++ = (unicode >> (UTF8_TRAILING_BYTE_SIZE * 3)) | UTF8_FOUR_BYTE_FLAG;
            *utf8++ = ((unicode >> (UTF8_TRAILING_BYTE_SIZE * 2)) & UTF8_TRAILING_BYTE_PAYLOAD) | UTF8_TRAILING_BYTE_FLAG;
            *utf8++ = ((unicode >> UTF8_TRAILING_BYTE_SIZE) & UTF8_TRAILING_BYTE_PAYLOAD) | UTF8_TRAILING_BYTE_FLAG;
            *utf8++ = (unicode & UTF8_TRAILING_BYTE_PAYLOAD) | UTF8_TRAILING_BYTE_FLAG;
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
    CharacterArray *unicode = listToCharArray(v);
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
    CharacterArray *unicode = newCharacterArray();
    int save = PROTECT(unicode);
    if (size == -1) {
        pushCharacterArray(unicode, (Character) 0);
    } else {
        extendCharacterArray(unicode, (Index)(size + 1));
        utf8_to_unicode_string(unicode->entries, (unsigned char *)utf8);
        unicode->size = (Index)(size + 1);
    }
    Value v = charArrayToList(unicode);
    UNPROTECT(save);
    return v;
}

bool utf8_isalnum(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isalnum(c);
}

bool utf8_isalpha(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isalpha(c);
}

bool utf8_isascii(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isascii(c);
}

bool utf8_isblank(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isblank(c);
}

bool utf8_iscntrl(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_iscntrl(c);
}

bool utf8_isdigit(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isdigit(c);
}

bool utf8_isgraph(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isgraph(c);
}

bool utf8_islower(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_islower(c);
}

bool utf8_isnumber(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isnumber(c);
}

bool utf8_isprint(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isprint(c);
}

bool utf8_ispunct(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_ispunct(c);
}

bool utf8_isspace(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isspace(c);
}

bool utf8_issymbol(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_issymbol(c);
}

bool utf8_isupper(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isupper(c);
}

bool utf8_isvalid(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isvalid(c);
}

bool utf8_isxdigit(unsigned char *s) {
    Character c = 0;
    utf8Sgetc(s, &c);
    return unicode_isxdigit(c);
}

