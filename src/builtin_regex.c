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
#include "regex.h"
#include "regex_helper.h"
#include "regex_source.h"
#include "symbol.h"
#include "tc_analyze.h"
#include "value.h"

static RegexMap *regexCache = NULL;
static bool regexCachingEnabled = true;

static void registerRegexMatch(BuiltIns *registry);
static void registerRegexMatchFile(BuiltIns *registry);
Value builtin_regex_match(Vec *args);
Value builtin_regex_match_file(Vec *args);
static RegexMap *getRegexCache(void);
static Regex *compileRegexPattern(Value patternValue, RegexStatus *status,
                                  Index *errorOffset);
static TcType *makeFileType(void);
static TcType *makeRegexType(void);
static TcType *makeMaybeStringPairType(void);
static Value unpackRegexPattern(Value regexValue);

void markRegexCache(void) {
    if (regexCache != NULL) {
        markHashTable((HashTable *)regexCache);
    }
}

bool enableRegexCaching(void) {
    bool previous = regexCachingEnabled;
    regexCachingEnabled = true;
    return previous;
}

bool disableRegexCaching(void) {
    bool previous = regexCachingEnabled;
    regexCachingEnabled = false;
    return previous;
}

void registerRegex(BuiltIns *registry) {
    registerRegexMatch(registry);
    registerRegexMatchFile(registry);
}

static RegexMap *getRegexCache(void) {
    if (regexCache == NULL) {
        regexCache = newRegexMap();
    }
    return regexCache;
}

static Regex *compileRegexPattern(Value patternValue, RegexStatus *status,
                                  Index *errorOffset) {
    if (!regexCachingEnabled) {
        CharacterArray *pattern = listToCharArray(patternValue);
        int save = PROTECT(pattern);
        Regex *compiled = regexCompile(pattern->entries, status, errorOffset);
        UNPROTECT(save);
        return compiled;
    }

    int save = STARTPROTECT();
    SCharVec *patternUtf8 = listToUtf8(patternValue);
    PROTECT(patternUtf8);

    HashSymbol *key = newSymbol(patternUtf8->entries);
    Regex *compiled = NULL;
    if (getRegexMap(getRegexCache(), key, &compiled)) {
        if (status != NULL) {
            *status = REGEX_STATUS_OK;
        }
        if (errorOffset != NULL) {
            *errorOffset = 0;
        }
        UNPROTECT(save);
        return compiled;
    }

    CharacterArray *pattern = listToCharArray(patternValue);
    PROTECT(pattern);
    compiled = regexCompile(pattern->entries, status, errorOffset);
    if (compiled != NULL && (status == NULL || *status == REGEX_STATUS_OK)) {
        PROTECT(compiled);
        setRegexMap(getRegexCache(), key, compiled);
    }
    UNPROTECT(save);
    return compiled;
}

static TcType *makeFileType(void) {
    return newTcType_Opaque(newSymbol("file"));
}

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

static void registerRegexMatchFile(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *regexType = makeRegexType();
    PROTECT(regexType);
    pushBuiltInArgs(args, regexType);
    TcType *fileType = makeFileType();
    PROTECT(fileType);
    pushBuiltInArgs(args, fileType);
    TcType *maybeStringType = makeMaybeStringType();
    PROTECT(maybeStringType);
    pushNewBuiltIn(registry, "regex_match_file", maybeStringType, args,
                   (void *)builtin_regex_match_file,
                   "builtin_regex_match_file");
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

Value builtin_regex_match(Vec *args) {
    int save = STARTPROTECT();
    Value patternValue = unpackRegexPattern(args->entries[0]);

    RegexStatus status = REGEX_STATUS_OK;
    Index errorOffset = 0;
    Regex *compiled = compileRegexPattern(patternValue, &status, &errorOffset);
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
    protectValue(prefix);
    protectValue(rest);

    Vec *tuple = newVec(2);
    PROTECT(tuple);
    tuple->entries[0] = prefix;
    tuple->entries[1] = rest;

    Value result = makeSome(value_Vec(tuple));
    UNPROTECT(save);
    return result;
}

Value builtin_regex_match_file(Vec *args) {
    int save = STARTPROTECT();
    Value patternValue = unpackRegexPattern(args->entries[0]);

#ifdef SAFETY_CHECKS
    if (args->entries[1].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(args->entries[1].type));
    }
#endif
    Opaque *data = args->entries[1].val.opaque;
    if (data == NULL || data->data == NULL) {
        cant_happen("regex_match_file on closed file handle");
    }

    RegexStatus status = REGEX_STATUS_OK;
    Index errorOffset = 0;
    Regex *compiled = compileRegexPattern(patternValue, &status, &errorOffset);
    if (compiled == NULL || status != REGEX_STATUS_OK) {
        cant_happen("invalid regex pattern at offset %d", errorOffset);
    }
    PROTECT(compiled);

    RegexSource *source = regexSourceFromFileHandle((FILE *)data->data);
    PROTECT(source);

    Index matchLength = 0;
    int matchStart = regexMatchSourcep(compiled, source, &matchLength);
    if (matchStart != 0) {
        UNPROTECT(save);
        return makeNothing();
    }

    Value prefix;
    regexSourceSplitAt(source, matchLength, &prefix, NULL);
    protectValue(prefix);

    Value result = makeSome(prefix);
    UNPROTECT(save);
    return result;
}