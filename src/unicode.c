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

// This file includes a lookup table for the unicode general category
// values of every unicode character, plus a set of unicode equivalents
// to the functions defined in ctype.h.
// Where unicode and ctype.h disagree (for example unicode considers TAB
// to be a control character and not a space) the ctype.h result is
// preferred.

#include <ctype.h>
#include "unicode.h"
#include "common.h"

static unsigned char category[] = {
#include "UnicodeData.inc"
};

bool unicode_isvalid(Character c) {
    return c >= 0 && c <= UNICODE_MAX;
}

bool unicode_isascii(Character c) {
    return c >= 0 && c < 0x80;
}

bool unicode_issymbol(Character c) {
    return unicode_isvalid(c) && ((category[c] & GC_MASK) == GC_S);
}

bool unicode_isalnum(Character c) {
    return unicode_isalpha(c) || unicode_isnumber(c);
}

bool unicode_isalpha(Character c) {
    return unicode_isvalid(c) && ((category[c] & GC_MASK) == GC_L || c == '_');
}

bool unicode_iscntrl(Character c) {
    return unicode_isvalid(c) && ((unicode_isascii(c) && iscntrl(c)) || category[c] == GC_Cc);
}

bool unicode_isnumber(Character c) {
    return unicode_isvalid(c) && ((category[c] & GC_MASK) == GC_N);
}

bool unicode_isdigit(Character c) {
    return unicode_isvalid(c) && (category[c] == GC_Nd);
}

bool unicode_isgraph(Character c) {
    return unicode_isvalid(c) && ((unicode_isascii(c) && isgraph(c)) || (category[c] & GC_MASK) == GC_C || category[c] == GC_Zl || category[c] == GC_Zp);
}

bool unicode_islower(Character c) {
    return unicode_isvalid(c) && (category[c] == GC_Ll);
}

bool unicode_isprint(Character c) {
    return unicode_isalnum(c) || unicode_isspace(c) || unicode_ispunct(c);
}

bool unicode_ispunct(Character c) {
    return unicode_isvalid(c) && ((unicode_isascii(c) && ispunct(c)) || ((category[c] & GC_MASK) == GC_P && category[c] != GC_Pc));
}

bool unicode_isspace(Character c) {
    return unicode_isvalid(c) && ((unicode_isascii(c) && isspace(c)) || category[c] == GC_Zs);
}

bool unicode_isupper(Character c) {
    return unicode_isvalid(c) && (category[c] == GC_Lu || category[c] == GC_Lt);
}

bool unicode_isblank(Character c) {
    return unicode_isvalid(c) && ((unicode_isascii(c) && isblank(c)) || category[c] == GC_Zs);
}

bool unicode_isxdigit(Character c) {
    return unicode_isascii(c) && isxdigit(c);
}

int unicode_category(Character c) {
    return unicode_isvalid(c) ? category[c] : GC_Cn;
}
