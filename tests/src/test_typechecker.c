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
#include "builtins_helper.h"
#include "tc_analyze.h"
#include "pratt_parser.h"
#include "pratt_scanner.h"
#include "init.h"
#include "wrapper_synthesis.h"
static BuiltIns *builtIns = NULL;

static bool compareTcTypes(TcType *a, TcType *b) {
    HashTable *map = newHashTable(sizeof(HashSymbol *), NULL, NULL);
    int save = PROTECT(map);
    bool res = eqTcType(a, b, map);
    UNPROTECT(save);
    return res;
}

static AstProg *parseWrapped(char *string, char *origin) {
    forceInitNameSpaces();
    AstProg *prog = prattParseString(string, origin);
    return prog;
}

static AstProg *parseSolo(char *string, char *origin) {
    forceInitNameSpaces();
    AstNest *nest = prattParseStandaloneString(string, origin);
    int save = PROTECT(nest);
    AstProg *prog = astNestToProg(nest);
    UNPROTECT(save);
    return prog;
}

static TcType *makeVar(char *name) {
    HashSymbol *t = newSymbol(name);
    TcVar *v = newTcVar(t, 0);
    int save = PROTECT(v);
    TcType *var = newTcType(TCTYPE_TYPE_VAR, TCTYPE_VAL_VAR(v));
    UNPROTECT(save);
    return var;
}

static TcType *_makeTypeSig(char *name, TcTypeSigArgs *args, int nsid) {
    HashSymbol *sym = newSymbol(name);
    TcTypeSig *typeDef = newTcTypeSig(sym, args, nsid);
    int save = PROTECT(typeDef);
    TcType *td = newTcType(TCTYPE_TYPE_TYPESIG, TCTYPE_VAL_TYPESIG(typeDef));
    UNPROTECT(save);
    return td;
}

static TcType *listOf(TcType *type) {
    TcTypeSigArgs *args = newTcTypeSigArgs(type, NULL);
    int save = PROTECT(args);
    TcType *td = _makeTypeSig("list", args, NS_GLOBAL);
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

static TcType *makeCharacter() {
    return newTcType(TCTYPE_TYPE_CHARACTER, TCTYPE_VAL_CHARACTER());
}

static TcType *analyze(AstProg *prog) {
    int save = PROTECT(prog);
    LamExp *exp = lamConvertProg(prog);
    PROTECT(exp);
    TcEnv *env = tc_init(builtIns);
    PROTECT(env);
    TcType *res = tc_analyze(exp, env);
    PROTECT(res);
    ppTcType(res);
    eprintf("\n");
    UNPROTECT(save);
    return res;
}

static void test_cdr() {
    printf("test_cdr\n");
    AstProg *result = parseWrapped("cdr", "test_cdr");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *var = makeVar("#t");
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
    AstProg *result = parseWrapped("car", "test_car");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *var = makeVar("#t");
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
    AstProg *result = parseWrapped("<[1]", "test_car_of");
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
    AstProg *result = parseWrapped("fn(a,b){a+b}", "test_adder");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *bigInt = makeBigInteger();
    PROTECT(bigInt);
    TcType *expected = makeFunction3(bigInt, bigInt, bigInt);
    PROTECT(expected);
    ppTcType(expected);
    eprintf(" -- expected\n");
    ppTcType(res);
    eprintf(" -- res\n");
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_fact() {
    printf("test_fact\n");
    AstProg *result = parseWrapped("let fn fact {(0) {1} (n) {n * fact(n - 1)} } in fact", "test_fact");
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
    AstProg *result = parseWrapped("let fn add1(x) { 1 + x } in add1(2)", "test_add1");
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
    AstProg *result = parseWrapped("let x = [1, 2, 3, 4]; in <>>x", "test_caddr");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *expected = makeBigInteger();
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_curry() {
    printf("test_curry\n");
    AstProg *result = parseWrapped("let fn add3(a, b, c) { a + b + c } in add3(1)(2)(3)", "test_curry");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *expected = makeBigInteger();
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_here() {
    printf("test_here\n");
    AstProg *result = parseWrapped(
"let"
"    fn funky(k) { k(1) }"
"in"
"    4 + here fn (k) {"
"        if (funky(k)) {"
"            2"
"        } else {"
"            3"
"        }"
"    }",
        "test_here"
    );
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *expected = makeBigInteger();
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_if() {
    printf("test_if\n");
    AstProg *result = parseWrapped("if (true and true) { 10 } else { 20 }", "test_if");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *expected = makeBigInteger();
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_id() {
    printf("test_id\n");
    AstProg *result = parseWrapped(
"let"
"    fn id (x) { x }"
""
"    fn length {"
"        ([]) { 0 }"
"        (_ @ t) { 1 + length(t) }"
"    }"
""
"    fn even(n) { n % 2 == 0 }"
""
"    fn checkId(x) {"
"        id(even(id(length(id(x)))))"
"    }"
""
"in"
"    checkId(\"hello\")",
        "test_id"
    );
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *expected = _makeTypeSig("bool", NULL, NS_GLOBAL);
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_either_1() {
    printf("test_either_1\n");
    AstProg *result = parseWrapped("let typedef either(#a, #b) { a(#a) | b(#b) } in a(1)", "test_either");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *big = makeBigInteger();
    PROTECT(big);
    TcType *var = makeVar("#t");
    PROTECT(var);
    TcTypeSigArgs *args = newTcTypeSigArgs(var, NULL);
    PROTECT(args);
    args = newTcTypeSigArgs(big, args);
    PROTECT(args);
    TcType *expected = _makeTypeSig("either", args, NS_GLOBAL);
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_tostr() {
    printf("test_tostr\n");
    AstProg *result = parseWrapped(
"let"
"    typedef colour { red | green | blue }"
"    fn tostr {"
"        (red) { \"red\" }"
"        (green) { \"green\" }"
"        (blue) { \"blue\" }"
"    }"
"in"
"    tostr(red)", "test_tostr"
    );
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *character = makeCharacter();
    PROTECT(character);
    TcType *expected = listOf(character);
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_lol() {
    printf("test_lol\n");
    AstProg *result = parseWrapped("[[1]]", "test_lol");
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *big = makeBigInteger();
    PROTECT(big);
    TcType *loi = listOf(big);
    PROTECT(loi);
    TcType *expected = listOf(loi);
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}

static void test_map() {
    printf("test_map\n");
    AstProg *result = parseWrapped(
"let"
"    fn map {"
"        (_, []) { [] }"
"        (f, h @ t) { f(h) @ map(f, t) }"
"    }"
"in"
"    map", "test_map"
    );
    int save = PROTECT(result);
    TcType *res = analyze(result);
    PROTECT(res);
    TcType *t1 = makeVar("#t1");
    PROTECT(t1);
    TcType *t2 = makeVar("#t2");
    PROTECT(t2);
    TcType *f = makeFunction2(t1, t2);
    PROTECT(f);
    TcType *listT1 = listOf(t1);
    PROTECT(listT1);
    TcType *listT2 = listOf(t2);
    PROTECT(listT2);
    TcType *expected = makeFunction3(f, listT1, listT2);
    PROTECT(expected);
    assert(compareTcTypes(res, expected));
	UNPROTECT(save);
}


int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    disableGC();
    initAll();
    builtIns = registerBuiltIns(0, 0, NULL);
    // Synthesize wrappers so external builtin names are ordinary curriable functions
    generateBuiltinWrappers(builtIns);
    test_car();
    test_cdr();
    test_car_of();
    test_adder();
    test_fact();
    test_add1();
    test_caddr();
    test_either_1();
    test_tostr();
    test_curry();
    test_here();
    test_id();
    test_if();
    test_lol();
    test_map();
    assert(!hadErrors());
}

