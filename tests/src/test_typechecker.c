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

/*
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
*/

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
/*

static void test_car() {
    printf("test_car\n");
    AstNest *result = parseWrapped("car");
    int save = PROTECT(result);
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
	UNPROTECT(save);
    assert(!hadErrors());
}

static void test_cdr() {
    printf("test_cdr\n");
    AstNest *result = parseWrapped("cdr");
    int save = PROTECT(result);
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
	UNPROTECT(save);
    assert(!hadErrors());
}

static void test_car_of() {
    printf("test_car_of\n");
    AstNest *result = parseWrapped("<[1]");
    int save = PROTECT(result);
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
	UNPROTECT(save);
    assert(!hadErrors());
}

static void test_adder() {
    printf("test_adder\n");
    AstNest *result = parseSolo("fn(a,b){a+b}");
    int save = PROTECT(result);
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
	UNPROTECT(save);
    assert(!hadErrors());
}

static void test_fact() {
    printf("test_fact\n");
    AstNest *result = parseSolo("let fn fact {(0) {1} (n) {n * fact(n - 1)} } in fact");
    int save = PROTECT(result);
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
	UNPROTECT(save);
    assert(!hadErrors());
}
*/

static void test_small_car() {
    printf("test_small_car\n");
    AstNest *result = parseSolo("let typedef mylist(#t) { mynull | mypair(#t, mylist(#t)) }"
            " fn myhead { (mynull) { mynull } (mypair(a,b)) { a } }"
            " in myhead(mypair(2, mynull))");
    int save = PROTECT(result);
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
	UNPROTECT(save);
    assert(!hadErrors());
}

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    initProtection();
    /*
    test_car();
    test_cdr();
    test_car_of();
    test_adder();
    */
    test_small_car();
}

