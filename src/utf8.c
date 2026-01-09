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

// converts a list of char to a utf8 string.
char *listToUtf8(Value v) {
#ifdef SAFETY_CHECKS
    if (v.type != VALUE_TYPE_VEC) {
        cant_happen("unexpected %s", valueTypeName(v.type));
    }
#endif
    CharacterArray *unicode = listToCharArray(v);
    int save = PROTECT(unicode);
    size_t size = wcstombs(NULL, unicode->entries, 0);
    unsigned char *buf = NEW_ARRAY(unsigned char, size + 1);
    wcstombs((char *)buf, unicode->entries, size + 1);
    UNPROTECT(save);
    return (char *)buf;
}

// converts a utf8 string to a list of char (Value)
// returns the empty list if the string is invalid
Value utf8ToList(const char *utf8) {
    size_t size = mbstowcs(NULL, utf8, 0);
    CharacterArray *unicode = newCharacterArray();
    int save = PROTECT(unicode);
    if (size == (size_t) -1) {
        pushCharacterArray(unicode, (Character) 0);
    } else {
        extendCharacterArray(unicode, (Index)(size + 1));
        mbstowcs(unicode->entries, utf8, size + 1);
        unicode->size = (Index)(size + 1);
    }
    Value v = charArrayToList(unicode);
    UNPROTECT(save);
    return v;
}
