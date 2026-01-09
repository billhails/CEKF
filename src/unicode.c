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

// This file includes a lookUp table for the unicode general category
// values of every unicode character, plus a set of unicode equivalents
// to the functions defined in ctype.h.
// Where unicode and ctype.h disagree (for example unicode considers TAB
// to be a control character and not a space) the ctype.h result is
// preferred.

#include <ctype.h>
#include "unicode.h"
#include "common.h"
#include "UnicodeDigits.inc"

static unsigned char category[] = {
#include "UnicodeData.inc"
};

__attribute__((unused)) static inline bool isGC_L(Character c) { return (category[c] & GC_MASK) == GC_L; }
__attribute__((unused)) static inline bool isGC_M(Character c) { return (category[c] & GC_MASK) == GC_M; }
__attribute__((unused)) static inline bool isGC_N(Character c) { return (category[c] & GC_MASK) == GC_N; }
__attribute__((unused)) static inline bool isGC_P(Character c) { return (category[c] & GC_MASK) == GC_P; }
__attribute__((unused)) static inline bool isGC_S(Character c) { return (category[c] & GC_MASK) == GC_S; }
__attribute__((unused)) static inline bool isGC_Z(Character c) { return (category[c] & GC_MASK) == GC_Z; }
__attribute__((unused)) static inline bool isGC_C(Character c) { return (category[c] & GC_MASK) == GC_C; }

__attribute__((unused)) static inline bool isGC_Ll(Character c) { return category[c] == GC_Ll; }
__attribute__((unused)) static inline bool isGC_Lm(Character c) { return category[c] == GC_Lm; }
__attribute__((unused)) static inline bool isGC_Lo(Character c) { return category[c] == GC_Lo; }
__attribute__((unused)) static inline bool isGC_Lt(Character c) { return category[c] == GC_Lt; }
__attribute__((unused)) static inline bool isGC_Lu(Character c) { return category[c] == GC_Lu; }
__attribute__((unused)) static inline bool isGC_Mc(Character c) { return category[c] == GC_Mc; }
__attribute__((unused)) static inline bool isGC_Me(Character c) { return category[c] == GC_Me; }
__attribute__((unused)) static inline bool isGC_Mn(Character c) { return category[c] == GC_Mn; }
__attribute__((unused)) static inline bool isGC_Nd(Character c) { return category[c] == GC_Nd; }
__attribute__((unused)) static inline bool isGC_Nl(Character c) { return category[c] == GC_Nl; }
__attribute__((unused)) static inline bool isGC_No(Character c) { return category[c] == GC_No; }
__attribute__((unused)) static inline bool isGC_Pc(Character c) { return category[c] == GC_Pc; }
__attribute__((unused)) static inline bool isGC_Pd(Character c) { return category[c] == GC_Pd; }
__attribute__((unused)) static inline bool isGC_Pe(Character c) { return category[c] == GC_Pe; }
__attribute__((unused)) static inline bool isGC_Pf(Character c) { return category[c] == GC_Pf; }
__attribute__((unused)) static inline bool isGC_Pi(Character c) { return category[c] == GC_Pi; }
__attribute__((unused)) static inline bool isGC_Po(Character c) { return category[c] == GC_Po; }
__attribute__((unused)) static inline bool isGC_Ps(Character c) { return category[c] == GC_Ps; }
__attribute__((unused)) static inline bool isGC_Sc(Character c) { return category[c] == GC_Sc; }
__attribute__((unused)) static inline bool isGC_Sk(Character c) { return category[c] == GC_Sk; }
__attribute__((unused)) static inline bool isGC_Sm(Character c) { return category[c] == GC_Sm; }
__attribute__((unused)) static inline bool isGC_So(Character c) { return category[c] == GC_So; }
__attribute__((unused)) static inline bool isGC_Zl(Character c) { return category[c] == GC_Zl; }
__attribute__((unused)) static inline bool isGC_Zp(Character c) { return category[c] == GC_Zp; }
__attribute__((unused)) static inline bool isGC_Zs(Character c) { return category[c] == GC_Zs; }
__attribute__((unused)) static inline bool isGC_Cc(Character c) { return category[c] == GC_Cc; }
__attribute__((unused)) static inline bool isGC_Cf(Character c) { return category[c] == GC_Cf; }
__attribute__((unused)) static inline bool isGC_Co(Character c) { return category[c] == GC_Co; }
__attribute__((unused)) static inline bool isGC_Cs(Character c) { return category[c] == GC_Cs; }
__attribute__((unused)) static inline bool isGC_Cn(Character c) { return category[c] == GC_Cn; }

int unicode_getdec(Character c) {
    switch (c) {
        UNICODE_DIGITS_CASES
    }
}

bool unicode_isvalid(Character c) {
    return c >= 0 && c <= UNICODE_MAX;
}

static inline bool isValid(Character c) { return c >= 0 && c <= UNICODE_MAX; }

bool unicode_isascii(Character c) {
    return c >= 0 && c < 0x80;
}

bool unicode_isopen(Character c) {
    return isValid(c) && isGC_Ps(c);
}

bool unicode_isclose(Character c) {
    return isValid(c) && isGC_Pe(c);
}

bool unicode_issymbol(Character c) {
    return isValid(c) && isGC_S(c);
}

bool unicode_isalpha(Character c) {
    return isValid(c) && (isGC_L(c) || c == '_');
}

bool unicode_ismark(Character c) {
    return isValid(c) && isGC_M(c);
}

bool unicode_isnumber(Character c) {
    return isValid(c) && isGC_N(c);
}

bool unicode_isgraph(Character c) {
    return isValid(c) &&
           ((unicode_isascii(c) && isgraph(c)) || isGC_C(c) || isGC_Zl(c) || isGC_Zp(c));
}

bool unicode_isalnum(Character c) {
    return unicode_isalpha(c) || unicode_isnumber(c);
}

bool unicode_iscntrl(Character c) {
    return isValid(c) && ((unicode_isascii(c) && iscntrl(c)) || isGC_Cc(c));
}

bool unicode_isdigit(Character c) {
    return isValid(c) && isGC_Nd(c);
}

bool unicode_islower(Character c) {
    return isValid(c) && isGC_Ll(c);
}

bool unicode_isprint(Character c) {
    return unicode_isalnum(c) || unicode_isspace(c) || unicode_ispunct(c);
}

bool unicode_ispunct(Character c) {
    return isValid(c) &&
           ((unicode_isascii(c) && ispunct(c)) ||
            (isGC_P(c) && !isGC_Pc(c)));
}

bool unicode_isspace(Character c) {
    return isValid(c) && ((unicode_isascii(c) && isspace(c)) || isGC_Z(c));
}

bool unicode_isupper(Character c) {
    return isValid(c) && (isGC_Lu(c) || isGC_Lt(c));
}

bool unicode_isblank(Character c) {
    return isValid(c) && ((unicode_isascii(c) && isblank(c)) || isGC_Zs(c));
}

bool unicode_isxdigit(Character c) {
    return unicode_isascii(c) && isxdigit(c);
}

int unicode_category(Character c) {
    return isValid(c) ? category[c] : GC_Cn;
}
