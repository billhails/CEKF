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

#include "unicode.h"
#include "UnicodeCasing.inc"
#include "UnicodeNumbers.inc"
#include "UnicodeData.inc"
#include "UnicodeDigits.inc"
#include "common.h"
#include <ctype.h>
#include <stdint.h>

static inline unsigned char categoryAt(Character c) {
    uint32_t code = (uint32_t)c;
    return unicodeCategoryPages[unicodeCategoryPageIndex[code >> 8]]
                               [code & 0xFF];
}

static const UnicodeCaseEntry *findCaseEntry(Character c) {
    uint32_t code = (uint32_t)c;
    uint32_t low = 0;
    uint32_t high = unicodeCaseEntryCount;

    while (low < high) {
        uint32_t mid = low + (high - low) / 2;
        const UnicodeCaseEntry *entry = &unicodeCaseEntries[mid];

        if (entry->code == code) {
            return entry;
        }

        if (entry->code < code) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }

    return NULL;
}

static const UnicodeNumberEntry *findNumberEntry(Character c) {
    uint32_t code = (uint32_t)c;
    uint32_t low = 0;
    uint32_t high = unicodeNumberEntryCount;

    while (low < high) {
        uint32_t mid = low + (high - low) / 2;
        const UnicodeNumberEntry *entry = &unicodeNumberEntries[mid];

        if (entry->code == code) {
            return entry;
        }

        if (entry->code < code) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }

    return NULL;
}

static Index copyCaseMapping(Character c, int32_t delta, uint32_t specialOffset,
                             Character *buffer, Index bufferSize) {
    if (specialOffset != 0) {
        uint32_t length = unicodeSpecialCaseData[specialOffset];

        for (uint32_t i = 0; i < length && i < bufferSize; i++) {
            buffer[i] =
                (Character)unicodeSpecialCaseData[specialOffset + 1 + i];
        }

        return (Index)length;
    }

    if (bufferSize > 0) {
        buffer[0] = delta == 0 ? c : c + delta;
    }

    return 1;
}

static Index unicodeCaseMap(Character c, Character *buffer, Index bufferSize,
                            int whichCase) {
    const UnicodeCaseEntry *entry = findCaseEntry(c);

    if (entry == NULL) {
        if (bufferSize > 0) {
            buffer[0] = c;
        }

        return 1;
    }

    switch (whichCase) {
    case 0:
        return copyCaseMapping(c, entry->upperDelta, entry->upperSpecial,
                               buffer, bufferSize);
    case 1:
        return copyCaseMapping(c, entry->lowerDelta, entry->lowerSpecial,
                               buffer, bufferSize);
    case 2:
        return copyCaseMapping(c, entry->titleDelta, entry->titleSpecial,
                               buffer, bufferSize);
    default:
        cant_happen("invalid Unicode case index %d", whichCase);
    }
}

__attribute__((unused)) static inline bool isGC_L(Character c) {
    return (categoryAt(c) & GC_MASK) == GC_L;
}
__attribute__((unused)) static inline bool isGC_M(Character c) {
    return (categoryAt(c) & GC_MASK) == GC_M;
}
__attribute__((unused)) static inline bool isGC_N(Character c) {
    return (categoryAt(c) & GC_MASK) == GC_N;
}
__attribute__((unused)) static inline bool isGC_P(Character c) {
    return (categoryAt(c) & GC_MASK) == GC_P;
}
__attribute__((unused)) static inline bool isGC_S(Character c) {
    return (categoryAt(c) & GC_MASK) == GC_S;
}
__attribute__((unused)) static inline bool isGC_Z(Character c) {
    return (categoryAt(c) & GC_MASK) == GC_Z;
}
__attribute__((unused)) static inline bool isGC_C(Character c) {
    return (categoryAt(c) & GC_MASK) == GC_C;
}

__attribute__((unused)) static inline bool isGC_Ll(Character c) {
    return categoryAt(c) == GC_Ll;
}
__attribute__((unused)) static inline bool isGC_Lm(Character c) {
    return categoryAt(c) == GC_Lm;
}
__attribute__((unused)) static inline bool isGC_Lo(Character c) {
    return categoryAt(c) == GC_Lo;
}
__attribute__((unused)) static inline bool isGC_Lt(Character c) {
    return categoryAt(c) == GC_Lt;
}
__attribute__((unused)) static inline bool isGC_Lu(Character c) {
    return categoryAt(c) == GC_Lu;
}
__attribute__((unused)) static inline bool isGC_Mc(Character c) {
    return categoryAt(c) == GC_Mc;
}
__attribute__((unused)) static inline bool isGC_Me(Character c) {
    return categoryAt(c) == GC_Me;
}
__attribute__((unused)) static inline bool isGC_Mn(Character c) {
    return categoryAt(c) == GC_Mn;
}
__attribute__((unused)) static inline bool isGC_Nd(Character c) {
    return categoryAt(c) == GC_Nd;
}
__attribute__((unused)) static inline bool isGC_Nl(Character c) {
    return categoryAt(c) == GC_Nl;
}
__attribute__((unused)) static inline bool isGC_No(Character c) {
    return categoryAt(c) == GC_No;
}
__attribute__((unused)) static inline bool isGC_Pc(Character c) {
    return categoryAt(c) == GC_Pc;
}
__attribute__((unused)) static inline bool isGC_Pd(Character c) {
    return categoryAt(c) == GC_Pd;
}
__attribute__((unused)) static inline bool isGC_Pe(Character c) {
    return categoryAt(c) == GC_Pe;
}
__attribute__((unused)) static inline bool isGC_Pf(Character c) {
    return categoryAt(c) == GC_Pf;
}
__attribute__((unused)) static inline bool isGC_Pi(Character c) {
    return categoryAt(c) == GC_Pi;
}
__attribute__((unused)) static inline bool isGC_Po(Character c) {
    return categoryAt(c) == GC_Po;
}
__attribute__((unused)) static inline bool isGC_Ps(Character c) {
    return categoryAt(c) == GC_Ps;
}
__attribute__((unused)) static inline bool isGC_Sc(Character c) {
    return categoryAt(c) == GC_Sc;
}
__attribute__((unused)) static inline bool isGC_Sk(Character c) {
    return categoryAt(c) == GC_Sk;
}
__attribute__((unused)) static inline bool isGC_Sm(Character c) {
    return categoryAt(c) == GC_Sm;
}
__attribute__((unused)) static inline bool isGC_So(Character c) {
    return categoryAt(c) == GC_So;
}
__attribute__((unused)) static inline bool isGC_Zl(Character c) {
    return categoryAt(c) == GC_Zl;
}
__attribute__((unused)) static inline bool isGC_Zp(Character c) {
    return categoryAt(c) == GC_Zp;
}
__attribute__((unused)) static inline bool isGC_Zs(Character c) {
    return categoryAt(c) == GC_Zs;
}
__attribute__((unused)) static inline bool isGC_Cc(Character c) {
    return categoryAt(c) == GC_Cc;
}
__attribute__((unused)) static inline bool isGC_Cf(Character c) {
    return categoryAt(c) == GC_Cf;
}
__attribute__((unused)) static inline bool isGC_Co(Character c) {
    return categoryAt(c) == GC_Co;
}
__attribute__((unused)) static inline bool isGC_Cs(Character c) {
    return categoryAt(c) == GC_Cs;
}
__attribute__((unused)) static inline bool isGC_Cn(Character c) {
    return categoryAt(c) == GC_Cn;
}

int unicode_getdec(Character c) {
    switch (c) { UNICODE_DIGITS_CASES }
}

Index unicode_toupper(Character c, Character *buffer, Index bufferSize) {
    return unicode_isvalid(c) ? unicodeCaseMap(c, buffer, bufferSize, 0) : 0;
}

Index unicode_tolower(Character c, Character *buffer, Index bufferSize) {
    return unicode_isvalid(c) ? unicodeCaseMap(c, buffer, bufferSize, 1) : 0;
}

Index unicode_totitle(Character c, Character *buffer, Index bufferSize) {
    return unicode_isvalid(c) ? unicodeCaseMap(c, buffer, bufferSize, 2) : 0;
}

bool unicode_getnum(Character c, UnicodeNumericValue *out) {
    const UnicodeNumberEntry *entry;

    if (!unicode_isvalid(c) || out == NULL) {
        return false;
    }

    entry = findNumberEntry(c);
    if (entry == NULL) {
        return false;
    }

    out->hasDecimalValue = entry->decimalValue >= 0;
    out->decimalValue = entry->decimalValue;
    out->hasIntegerValue = entry->hasIntegerValue != 0;
    out->integerValue = entry->integerValue;
    out->fractionValue = entry->fractionValue;
    return true;
}

bool unicode_isvalid(Character c) { return c >= 0 && c <= UNICODE_MAX; }

static inline bool isValid(Character c) { return c >= 0 && c <= UNICODE_MAX; }

bool unicode_isascii(Character c) { return c >= 0 && c < 0x80; }

bool unicode_isopen(Character c) { return isValid(c) && isGC_Ps(c); }

bool unicode_isclose(Character c) { return isValid(c) && isGC_Pe(c); }

bool unicode_issymbol(Character c) { return isValid(c) && isGC_S(c); }

bool unicode_isalpha(Character c) {
    return isValid(c) && (isGC_L(c) || c == '_');
}

bool unicode_ismark(Character c) { return isValid(c) && isGC_M(c); }

bool unicode_isnumber(Character c) { return isValid(c) && isGC_N(c); }

bool unicode_isgraph(Character c) {
    return isValid(c) && ((unicode_isascii(c) && isgraph(c)) || isGC_C(c) ||
                          isGC_Zl(c) || isGC_Zp(c));
}

bool unicode_isalnum(Character c) {
    return unicode_isalpha(c) || unicode_isnumber(c);
}

bool unicode_iscntrl(Character c) {
    return isValid(c) && ((unicode_isascii(c) && iscntrl(c)) || isGC_Cc(c));
}

bool unicode_isdigit(Character c) { return isValid(c) && isGC_Nd(c); }

bool unicode_islower(Character c) { return isValid(c) && isGC_Ll(c); }

bool unicode_isprint(Character c) {
    return unicode_isalnum(c) || unicode_isspace(c) || unicode_ispunct(c);
}

bool unicode_ispunct(Character c) {
    return isValid(c) &&
           ((unicode_isascii(c) && ispunct(c)) || (isGC_P(c) && !isGC_Pc(c)));
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

bool unicode_isxdigit(Character c) { return unicode_isascii(c) && isxdigit(c); }

int unicode_category(Character c) { return isValid(c) ? categoryAt(c) : GC_Cn; }
