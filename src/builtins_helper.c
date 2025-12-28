/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2023  Bill Hails
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

#include "builtins_helper.h"
#include <string.h>
#include <stdio.h>
#include "builtins_impl.h"
#include "memory.h"
#include "symbol.h"
#include "tc_analyze.h"
#include "builtin_sqlite.h"
#include "builtin_io.h"

static void registerRand(BuiltIns *registry);
static void registerIncr(BuiltIns *registry);
static void registerAssert(BuiltIns *registry);
static void registerOrd(BuiltIns *registry);
static void registerUnicodeCategory(BuiltIns *registry);
static void registerIsAlnum(BuiltIns *registry);
static void registerIsAlpha(BuiltIns *registry);
static void registerIsAscii(BuiltIns *registry);
static void registerIsBlank(BuiltIns *registry);
static void registerIsClose(BuiltIns *registry);
static void registerIsCntrl(BuiltIns *registry);
static void registerIsDigit(BuiltIns *registry);
static void registerIsGraph(BuiltIns *registry);
static void registerIsLower(BuiltIns *registry);
static void registerIsNumber(BuiltIns *registry);
static void registerIsOpen(BuiltIns *registry);
static void registerIsPrint(BuiltIns *registry);
static void registerIsPunct(BuiltIns *registry);
static void registerIsSpace(BuiltIns *registry);
static void registerIsSymbol(BuiltIns *registry);
static void registerIsUpper(BuiltIns *registry);
static void registerIsValid(BuiltIns *registry);
static void registerIsXdigit(BuiltIns *registry);
static void registerChr(BuiltIns *registry);
static void registerArgv(BuiltIns *registry, int argc, int cargc, char *argv[]);
static void registerGetEnv(BuiltIns *registry);
static void registerRealPart(BuiltIns *registry);
static void registerImagPart(BuiltIns *registry);
static void registerThetaPart(BuiltIns *registry);
static void registerMagPart(BuiltIns *registry);
static void registerExit(BuiltIns *registry);

Value makeTryResult(int code, Value val) {
    Vec *v = newVec(2);
    v->entries[0] = value_Stdint(code);
    v->entries[1] = val;
    return value_Vec(v);
}

Value makeSome(Value val) {
    Vec *v = newVec(2);
    v->entries[0] = value_Stdint(1);
    v->entries[1] = val;
    return value_Vec(v);
}

Value makeNothing(void) {
    Vec *v = newVec(1);
    v->entries[0] = value_Stdint(0);
    return value_Vec(v);
}

Value makeEmptyList(void) {
    Vec *v = newVec(1);
    v->entries[0] = value_Stdint(0);
    return value_Vec(v);
}

Value makeBasic(Value v, int code) {
    if (code == BASIC_TYPE_NULL) {
        Vec *null = newVec(1);
        null->entries[0] = value_Stdint(BASIC_TYPE_NULL);
        return value_Vec(null);
    } else {
        Vec *val = newVec(2);
        val->entries[0] = value_Stdint(code);
        val->entries[1] = v;
        return value_Vec(val);
    }
}

static TcType *makeAnyType(void) {
    return makeFreshVar("any");
}

TcType *pushAnyArg(BuiltInArgs *args) {
    TcType *anyType = makeAnyType();
    int save = PROTECT(anyType);
    pushBuiltInArgs(args, anyType);
    UNPROTECT(save);
    return anyType;
}

TcType *pushIntegerArg(BuiltInArgs *args) {
    TcType *integer = newTcType_BigInteger();
    int save = PROTECT(integer);
    pushBuiltInArgs(args, integer);
    UNPROTECT(save);
    return integer;
}

TcType *pushCharacterArg(BuiltInArgs *args) {
    TcType *character = newTcType_Character();
    int save = PROTECT(character);
    pushBuiltInArgs(args, character);
    UNPROTECT(save);
    return character;
}

TcType *pushStringArg(BuiltInArgs *args) {
    TcType *string = makeStringType();
    int save = PROTECT(string);
    pushBuiltInArgs(args, string);
    UNPROTECT(save);
    return string;
}

static char *makeInternalName(char *external) {
    // allocate buffer for "builtin$" + external + NUL
    size_t n = strlen(external);
    size_t len = n + 8 + 1; // 8 = strlen("builtin$")
    char *buf = (char *) safeMalloc(len);
    sprintf(buf, "builtin$%s", external);
    return buf;
}

void pushNewBuiltIn(BuiltIns *registry, char *name, TcType *ret, BuiltInArgs *args, void *impl) {
    HashSymbol *external = newSymbol(name);
    char *internalC = makeInternalName(name);
    HashSymbol *internal = newSymbol(internalC);
    FREE_ARRAY(char, internalC, strlen(internalC) + 1);
    BuiltIn *decl = newBuiltIn(external, internal, ret, args, impl);
    int save = PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

BuiltIns *registerBuiltIns(int argc, int cargc, char *argv[]) {
    BuiltIns *res = newBuiltIns();
    int save = PROTECT(res);
    registerRand(res);
    registerAssert(res);
    registerIncr(res);
    registerOrd(res);
    registerUnicodeCategory(res);
    registerIsAlnum(res);
    registerIsAlpha(res);
    registerIsAscii(res);
    registerIsBlank(res);
    registerIsClose(res);
    registerIsCntrl(res);
    registerIsDigit(res);
    registerIsGraph(res);
    registerIsLower(res);
    registerIsNumber(res);
    registerIsOpen(res);
    registerIsPrint(res);
    registerIsPunct(res);
    registerIsSpace(res);
    registerIsSymbol(res);
    registerIsUpper(res);
    registerIsValid(res);
    registerIsXdigit(res);
    registerChr(res);
    registerIO(res);
    registerSQLite(res);
    registerArgv(res, argc, cargc, argv);
    registerGetEnv(res);
    registerRealPart(res);
    registerImagPart(res);
    registerMagPart(res);
    registerThetaPart(res);
    registerExit(res);
    UNPROTECT(save);
    return res;
}

static void registerRand(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integer = pushIntegerArg(args);
    pushNewBuiltIn(registry, "rand", integer, args, (void *)builtin_rand);
    UNPROTECT(save);
}

static void registerIncr(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integer = newTcType_BigInteger();
    pushNewBuiltIn(registry, "incr", integer, args, (void *)builtin_incr);
    UNPROTECT(save);
}

static void registerAssert(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushNewBuiltIn(registry, "assertion", boolean, args, (void *)builtin_assert);
    UNPROTECT(save);
}

static void registerOrd(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integer = newTcType_BigInteger();
    PROTECT(integer);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "ord", integer, args, (void *)builtin_ord);
    UNPROTECT(save);
}

static void registerUnicodeCategory(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *resultType = makeTypeSig(newSymbol("unicode_general_category_type"), NULL, -1);
    PROTECT(resultType);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "unicode_category", resultType, args, (void *)builtin_unicode_category);
    UNPROTECT(save);
}

static void registerChr(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *character = newTcType_Character();
    PROTECT(character);
    pushIntegerArg(args);
    pushNewBuiltIn(registry, "chr", character, args, (void *)builtin_chr);
    UNPROTECT(save);
}

static void registerArgv(BuiltIns *registry, int argc, int cargc, char *argv[]) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *maybeStringType = makeMaybeStringType();
    PROTECT(maybeStringType);
    pushNewBuiltIn(registry, "argv", maybeStringType, args, (void *)builtin_args);
    UNPROTECT(save);
    builtin_args_argc = argc;
    builtin_args_cargc = cargc;
    builtin_args_argv = argv;
}

static void registerGetEnv(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushStringArg(args);
    TcType *maybeStringType = makeMaybeStringType();
    PROTECT(maybeStringType);
    pushNewBuiltIn(registry, "getenv", maybeStringType, args, (void *)builtin_getenv);
    UNPROTECT(save);
}

static void registerRealPart(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integerType = pushIntegerArg(args);
    pushNewBuiltIn(registry, "com_real", integerType, args, (void *)builtin_real_part);
    UNPROTECT(save);
}

static void registerImagPart(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integerType = pushIntegerArg(args);
    pushNewBuiltIn(registry, "com_imag", integerType, args, (void *)builtin_imag_part);
    UNPROTECT(save);
}

static void registerMagPart(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integerType = pushIntegerArg(args);
    pushNewBuiltIn(registry, "com_mag", integerType, args, (void *)builtin_mag_part);
    UNPROTECT(save);
}

static void registerThetaPart(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integerType = pushIntegerArg(args);
    pushNewBuiltIn(registry, "com_theta", integerType, args, (void *)builtin_theta_part);
    UNPROTECT(save);
}

static void registerExit(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *anyType = makeAnyType();
    PROTECT(anyType);
    pushNewBuiltIn(registry, "exit", anyType, args, (void *)builtin_exit);
    UNPROTECT(save);
}

static void registerIsAlnum(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isalnum", boolean, args, (void *)builtin_isalnum);
    UNPROTECT(save);
}

static void registerIsAlpha(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isalpha", boolean, args, (void *)builtin_isalpha);
    UNPROTECT(save);
}

static void registerIsAscii(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isascii", boolean, args, (void *)builtin_isascii);
    UNPROTECT(save);
}

static void registerIsBlank(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isblank", boolean, args, (void *)builtin_isblank);
    UNPROTECT(save);
}

static void registerIsClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isclose", boolean, args, (void *)builtin_isclose);
    UNPROTECT(save);
}

static void registerIsCntrl(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "iscntrl", boolean, args, (void *)builtin_iscntrl);
    UNPROTECT(save);
}

static void registerIsDigit(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isdigit", boolean, args, (void *)builtin_isdigit);
    UNPROTECT(save);
}

static void registerIsGraph(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isgraph", boolean, args, (void *)builtin_isgraph);
    UNPROTECT(save);
}

static void registerIsLower(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "islower", boolean, args, (void *)builtin_islower);
    UNPROTECT(save);
}

static void registerIsNumber(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isnumber", boolean, args, (void *)builtin_isnumber);
    UNPROTECT(save);
}

static void registerIsOpen(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isopen", boolean, args, (void *)builtin_isopen);
    UNPROTECT(save);
}

static void registerIsPrint(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isprint", boolean, args, (void *)builtin_isprint);
    UNPROTECT(save);
}

static void registerIsPunct(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "ispunct", boolean, args, (void *)builtin_ispunct);
    UNPROTECT(save);
}

static void registerIsSpace(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isspace", boolean, args, (void *)builtin_isspace);
    UNPROTECT(save);
}

static void registerIsSymbol(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "issymbol", boolean, args, (void *)builtin_issymbol);
    UNPROTECT(save);
}

static void registerIsUpper(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isupper", boolean, args, (void *)builtin_isupper);
    UNPROTECT(save);
}

static void registerIsValid(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isvalid", boolean, args, (void *)builtin_isvalid);
    UNPROTECT(save);
}

static void registerIsXdigit(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    pushCharacterArg(args);
    pushNewBuiltIn(registry, "isxdigit", boolean, args, (void *)builtin_isxdigit);
    UNPROTECT(save);
}
