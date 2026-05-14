/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "builtin_regex.h"
#include "builtins_helper.h"
#include "cekf.h"
#include "common.h"
#include "regex_helper.h"
#include "regex_source.h"
#include "symbol.h"
#include "tc_analyze.h"
#include "value.h"

static void registerRegexMatch(BuiltIns *registry);
static Value builtin_regex_match(Vec *args);
static TcType *makeRegexType(void);
static TcType *makeMaybeStringPairType(void);
static Value unpackRegexPattern(Value regexValue);

void registerRegex(BuiltIns *registry) { registerRegexMatch(registry); }

static TcType *makeRegexType(void) {
    return makeTypeSig(newSymbol("regex"), NULL);
}

static TcType *makeMaybeStringPairType(void) {
    int save = STARTPROTECT();
    TcTypeArray *pair = newTcTypeArray();
    PROTECT(pair);
    TcType *left = makeStringType();
    PROTECT(left);
    TcType *right = makeStringType();
    PROTECT(right);
    pushTcTypeArray(pair, left);
    pushTcTypeArray(pair, right);
    TcType *tuple = newTcType_Tuple(pair);
    PROTECT(tuple);
    TcType *maybe = makeMaybeType(tuple);
    UNPROTECT(save);
    return maybe;
}

static void registerRegexMatch(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *regexType = makeRegexType();
    PROTECT(regexType);
    pushBuiltInArgs(args, regexType);
    pushStringArg(args);
    TcType *resultType = makeMaybeStringPairType();
    PROTECT(resultType);
    pushNewBuiltIn(registry, "regex_match", resultType, args,
                   (void *)builtin_regex_match, "builtin_regex_match");
    UNPROTECT(save);
}

static Value unpackRegexPattern(Value regexValue) {
    Vec *regex = getValue_Vec(regexValue);
    if (regex->size != 2) {
        cant_happen("malformed regex value: expected 2 fields, got %d",
                    regex->size);
    }
    if (!isValue_Stdint(regex->entries[0]) ||
        getValue_Stdint(regex->entries[0]) != 0) {
        cant_happen("malformed regex value: expected constructor tag 0");
    }
    return regex->entries[1];
}

static Value builtin_regex_match(Vec *args) {
    int save = STARTPROTECT();
    Value patternValue = unpackRegexPattern(args->entries[0]);
    CharacterArray *pattern = listToCharArray(patternValue);
    PROTECT(pattern);

    RegexStatus status = REGEX_STATUS_OK;
    Index errorOffset = 0;
    Regex *compiled = regexCompile(pattern->entries, &status, &errorOffset);
    if (compiled == NULL || status != REGEX_STATUS_OK) {
        cant_happen("invalid regex pattern at offset %d", errorOffset);
    }
    PROTECT(compiled);

    RegexSource *source = regexSourceFromStringList(args->entries[1].val.vec);
    PROTECT(source);

    Index matchLength = 0;
    int matchStart = regexMatchSourcep(compiled, source, &matchLength);
    if (matchStart != 0) {
        UNPROTECT(save);
        return makeNothing();
    }

    Value prefix;
    Value rest;
    regexSourceSplitAt(source, matchLength, &prefix, &rest);

    Vec *tuple = newVec(2);
    PROTECT(tuple);
    tuple->entries[0] = prefix;
    tuple->entries[1] = rest;

    Value result = makeSome(value_Vec(tuple));
    UNPROTECT(save);
    return result;
}