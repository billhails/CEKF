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

static AstNest *parse(char *string) {
    disableGC();
    PmModule *mod = newPmToplevelFromString(string, string);
    int save = PROTECT(mod);
    int res = pmParseModule(mod);
    fprintf(stderr, "pmParseModule returned %d\n", res);
    enableGC();
    UNPROTECT(save);
    assert(mod->nest != NULL);
    return mod->nest;
}

static void test_car() {
    AstNest *result = parse("car");
    int save = PROTECT(result);
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
	UNPROTECT(save);
    assert(!hadErrors());
}

static void test_cdr() {
    AstNest *result = parse("cdr");
    int save = PROTECT(result);
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
	UNPROTECT(save);
    assert(!hadErrors());
}


int main(int argc, char *argv[]) {
    initProtection();
    test_car();
    test_cdr();
}

