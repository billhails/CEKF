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
#include "builtins_impl.h"
#include "memory.h"
#include "symbol.h"
#include "tc_analyze.h"
#include "builtin_sqlite.h"
#include "builtin_io.h"

static void registerRand(BuiltIns *registry);
static void registerAssert(BuiltIns *registry);
static void registerOrd(BuiltIns *registry);
static void registerChr(BuiltIns *registry);

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

BuiltIns *registerBuiltIns() {
    BuiltIns *res = newBuiltIns();
    int save = PROTECT(res);
    registerRand(res);
    registerAssert(res);
    registerOrd(res);
    registerChr(res);
    registerIO(res);
    registerSQLite(res);
    UNPROTECT(save);
    return res;
}

static void registerRand(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integer = newTcType_Biginteger();
    PROTECT(integer);
    pushBuiltInArgs(args, integer);
    BuiltIn *decl = newBuiltIn(newSymbol("rand"), integer, args, (void *)builtin_rand);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerAssert(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *boolean = makeBoolean();
    PROTECT(boolean);
    BuiltIn *decl = newBuiltIn(newSymbol("assertion"), boolean, args, (void *)builtin_assert);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerOrd(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integer = newTcType_Biginteger();
    PROTECT(integer);
    TcType *character = newTcType_Character();
    PROTECT(character);
    pushBuiltInArgs(args, character);
    BuiltIn *decl = newBuiltIn(newSymbol("ord"), integer, args, (void *)builtin_ord);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerChr(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integer = newTcType_Biginteger();
    PROTECT(integer);
    TcType *character = newTcType_Character();
    PROTECT(character);
    pushBuiltInArgs(args, integer);
    BuiltIn *decl = newBuiltIn(newSymbol("chr"), character, args, (void *)builtin_chr);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}
