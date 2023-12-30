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

#include "test.h"
#include "symbol.h"

static bool compareTcTypes(TcType *a, TcType *b) {
    HashTable *map = newHashTable(sizeof(HashSymbol *), NULL, NULL);
    int save = PROTECT(map);
    bool res = eqTcType(a, b, map);
    UNPROTECT(save);
    return res;
}

static AstNest *parseWrapped(char *string) {
    disableGC();
    PmModule *mod = newPmToplevelFromString(string, string);
    int save = PROTECT(mod);
    int res = pmParseModule(mod);
    assert(res == 0);
    enableGC();
    UNPROTECT(save);
    assert(mod->nest != NULL);
    return mod->nest;
}

static AstNest *parseSolo(char *string) {
    disableGC();
    PmModule *mod = newPmModuleFromString(string, string);
    int save = PROTECT(mod);
    int res = pmParseModule(mod);
    assert(res == 0);
    enableGC();
    UNPROTECT(save);
    assert(mod->nest != NULL);
    return mod->nest;
}

static TcType *charToVar(char *name) {
    HashSymbol *t = newSymbol(name);
    TcVar *v = newTcVar(t, 0);
    int save = PROTECT(v);
    TcType *var = newTcType(TCTYPE_TYPE_VAR, TCTYPE_VAL_VAR(v));
    UNPROTECT(save);
    return var;
}

static TcType *listOf(TcType *type) {
    TcTypeDefArgs *args = newTcTypeDefArgs(type, NULL);
    int save = PROTECT(args);
    HashSymbol *list = newSymbol("list");
    TcTypeDef *typeDef = newTcTypeDef(list, args);
    PROTECT(typeDef);
    TcType *td = newTcType(TCTYPE_TYPE_TYPEDEF, TCTYPE_VAL_TYPEDEF(typeDef));
    UNPROTECT(save);
    return td;
}

static TcType *makeFunction2(TcType *arg, TcType *result) {
    TcFunction *fun = newTcFunction(arg, result);
    int save = PROTECT(fun);
    TcType *f = newTcType(TCTYPE_TYPE_FUNCTION, TCTYPE_VAL_FUNCTION(fun));
    UNPROTECT(save);
    return f;
}

static TcType *makeFunction3(TcType *arg1, TcType *arg2, TcType *result) {
    TcType *f1 = makeFunction2(arg2, result);
    int save = PROTECT(f1);
    TcType *f2 = makeFunction2(arg1, f1);
    UNPROTECT(save);
    return f2;
}

static TcType *makeBigInteger() {
    return newTcType(TCTYPE_TYPE_BIGINTEGER, TCTYPE_VAL_BIGINTEGER());
}

static TcType *analyze(AstNest *nest) {
    LamExp *exp = lamConvertNest(nest, NULL);
    int save = PROTECT(exp);
    TcEnv *env = tc_init();
    PROTECT(env);
    TcType *res = tc_analyze(exp, env);
    PROTECT(res);
    ppTcType(res);
    eprintf("\n");
    assert(!hadErrors());
    UNPROTECT(save);
    return res;
}

static void test_cdr() {
    printf("test_cdr\n");
    AstNest *result = parseWrapped("cdr");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *var = charToVar("#t");
    PROTECT(var);
    TcType *td = listOf(var);
    PROTECT(td);
    TcType *f = makeFunction2(td, td);
    PROTECT(f);
    assert(compareTcTypes(f, res));
	UNPROTECT(save);
}

static void test_car() {
    printf("test_car\n");
    AstNest *result = parseWrapped("car");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *var = charToVar("#t");
    PROTECT(var);
    TcType *td = listOf(var);
    PROTECT(td);
    TcType *f = makeFunction2(td, var);
    PROTECT(f);
    assert(compareTcTypes(f, res));
	UNPROTECT(save);
}

static void test_car_of() {
    printf("test_car_of\n");
    AstNest *result = parseWrapped("<[1]");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *expected = makeBigInteger();
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_adder() {
    printf("test_adder\n");
    AstNest *result = parseSolo("fn(a,b){a+b}");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *bigInt = makeBigInteger();
    PROTECT(bigInt);
    TcType *expected = makeFunction3(bigInt, bigInt, bigInt);
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_fact() {
    printf("test_fact\n");
    AstNest *result = parseSolo("let fn fact {(0) {1} (n) {n * fact(n - 1)} } in fact");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *bigInt = makeBigInteger();
    PROTECT(bigInt);
    TcType *expected = makeFunction2(bigInt, bigInt);
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_add1() {
    printf("test_add1\n");
    AstNest *result = parseSolo("let fn add1(x) { 1 + x } in add1(2)");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *expected = makeBigInteger();
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_caddr() {
    printf("test_caddr\n");
    AstNest *result = parseWrapped("let x = [1, 2, 3, 4]; in <>>x");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *expected = makeBigInteger();
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}


int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    initProtection();
    test_car();
    test_cdr();
    test_car_of();
    test_adder();
    test_fact();
    test_add1();
    test_caddr();
}

