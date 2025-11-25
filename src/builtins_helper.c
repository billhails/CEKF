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
    TcType *integer = newTcType_Biginteger();
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
    TcType *integer = newTcType_Biginteger();
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
    TcType *integer = newTcType_Biginteger();
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
