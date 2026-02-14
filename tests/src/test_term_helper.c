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

#include <assert.h>
#include <stdio.h>

#include "bigint.h"
#include "common.h"
#include "init.h"
#include "term_helper.h"

typedef void (*TestFn)(void);

static void runTest(char *name, TestFn testFn) {
    printf("%s\n", name);
    testFn();
}

static void assertRoundTripMinExp(MinExp *orig) {
    int save = PROTECT(orig);
    Term *term = minExpToTerm(orig);
    PROTECT(term);
    MinExp *back = termToMinExp(term);
    PROTECT(back);
    assert(eqMinExp(orig, back));
    UNPROTECT(save);
}

static void assertTermNumMapsToSmall(Value value, bool imag, int expected) {
    Term *term = makeTerm_Num(NULLPI, value);
    int save = PROTECT(term);
    MinExp *exp = termToMinExp(term);
    PROTECT(exp);
    assert(isMinExp_BigInteger(exp));
    MaybeBigInt *mbi = getMinExp_BigInteger(exp);
    assert(mbi->type == BI_SMALL);
    assert(mbi->imag == imag);
    assert(mbi->small == expected);
    UNPROTECT(save);
}

static void assertTermNumMapsToIrrational(Value value, bool imag,
                                          Double expected) {
    Term *term = makeTerm_Num(NULLPI, value);
    int save = PROTECT(term);
    MinExp *exp = termToMinExp(term);
    PROTECT(exp);
    assert(isMinExp_BigInteger(exp));
    MaybeBigInt *mbi = getMinExp_BigInteger(exp);
    assert(mbi->type == BI_IRRATIONAL);
    assert(mbi->imag == imag);
    assert(mbi->irrational == expected);
    UNPROTECT(save);
}

static void assertTermNumMapsToBig(Value value, bool imag, BigInt *expected) {
    Term *term = makeTerm_Num(NULLPI, value);
    int save = PROTECT(term);
    MinExp *exp = termToMinExp(term);
    PROTECT(exp);
    assert(isMinExp_BigInteger(exp));
    MaybeBigInt *mbi = getMinExp_BigInteger(exp);
    assert(mbi->type == BI_BIG);
    assert(mbi->imag == imag);
    assert(bigint_cmp(&mbi->big, &expected->bi) == 0);
    UNPROTECT(save);
}

static MinExp *makeSmallBigInteger(int n, bool imag) {
    MaybeBigInt *mbi = fakeBigInt(n, imag);
    int save = PROTECT(mbi);
    MinExp *exp = newMinExp_BigInteger(NULLPI, mbi);
    UNPROTECT(save);
    return exp;
}

static MinExp *makeLargeBigInteger(int n, bool imag) {
    bigint bi;
    bigint_init(&bi);
    bigint_from_int(&bi, n);
    MaybeBigInt *mbi = newMaybeBigInt(bi, imag);
    int save = PROTECT(mbi);
    MinExp *exp = newMinExp_BigInteger(NULLPI, mbi);
    UNPROTECT(save);
    return exp;
}

static void test_round_trip_large_bigint(void) {
    MinExp *orig = makeLargeBigInteger(123456, false);
    assertRoundTripMinExp(orig);
}

static void test_round_trip_small_bigint_is_preserved(void) {
    MinExp *orig = makeSmallBigInteger(42, false);
    assertRoundTripMinExp(orig);
}

static void test_round_trip_small_imag_bigint_is_preserved(void) {
    MinExp *orig = makeSmallBigInteger(7, true);
    assertRoundTripMinExp(orig);
}

static void test_round_trip_irrational_bigint_is_preserved(void) {
    MaybeBigInt *mbi = irrationalBigInt(3.5, false);
    int save = PROTECT(mbi);
    MinExp *orig = newMinExp_BigInteger(NULLPI, mbi);
    PROTECT(orig);
    assertRoundTripMinExp(orig);
    UNPROTECT(save);
}

static void test_round_trip_minexp_stdint_is_other(void) {
    MinExp *orig = newMinExp_Stdint(NULLPI, 9);
    int save = PROTECT(orig);
    Term *term = minExpToTerm(orig);
    PROTECT(term);
    assert(term->type == TERM_TYPE_OTHER);
    MinExp *back = termToMinExp(term);
    PROTECT(back);
    assert(eqMinExp(orig, back));
    UNPROTECT(save);
}

static void test_term_num_stdint_maps_to_biginteger_small(void) {
    assertTermNumMapsToSmall(value_Stdint(21), false, 21);
}

static void test_term_num_stdint_imag_maps_to_biginteger_small_imag(void) {
    assertTermNumMapsToSmall(value_Stdint_imag(22), true, 22);
}

static void test_term_num_irrational_maps_to_biginteger_irrational(void) {
    assertTermNumMapsToIrrational(value_Irrational(2.5), false, 2.5);
}

static void
test_term_num_irrational_imag_maps_to_biginteger_irrational_imag(void) {
    assertTermNumMapsToIrrational(value_Irrational_imag(2.75), true, 2.75);
}

static void test_term_num_bigint_maps_to_biginteger_big(void) {
    BigInt *runtimeBigInt = bigIntFromInt(314159);
    int save = PROTECT(runtimeBigInt);
    assertTermNumMapsToBig(value_Bigint(runtimeBigInt), false, runtimeBigInt);
    UNPROTECT(save);
}

static void test_term_num_bigint_imag_maps_to_biginteger_big_imag(void) {
    BigInt *runtimeBigInt = bigIntFromInt(271828);
    int save = PROTECT(runtimeBigInt);
    assertTermNumMapsToBig(value_Bigint_imag(runtimeBigInt), true,
                           runtimeBigInt);
    UNPROTECT(save);
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    initAll();

    runTest("test_round_trip_large_bigint", test_round_trip_large_bigint);
    runTest("test_round_trip_small_bigint_is_preserved",
            test_round_trip_small_bigint_is_preserved);
    runTest("test_round_trip_small_imag_bigint_is_preserved",
            test_round_trip_small_imag_bigint_is_preserved);
    runTest("test_round_trip_irrational_bigint_is_preserved",
            test_round_trip_irrational_bigint_is_preserved);
    runTest("test_round_trip_minexp_stdint_is_other",
            test_round_trip_minexp_stdint_is_other);

    runTest("test_term_num_stdint_maps_to_biginteger_small",
            test_term_num_stdint_maps_to_biginteger_small);
    runTest("test_term_num_stdint_imag_maps_to_biginteger_small_imag",
            test_term_num_stdint_imag_maps_to_biginteger_small_imag);
    runTest("test_term_num_irrational_maps_to_biginteger_irrational",
            test_term_num_irrational_maps_to_biginteger_irrational);
    runTest("test_term_num_irrational_imag_maps_to_biginteger_irrational_imag",
            test_term_num_irrational_imag_maps_to_biginteger_irrational_imag);
    runTest("test_term_num_bigint_maps_to_biginteger_big",
            test_term_num_bigint_maps_to_biginteger_big);
    runTest("test_term_num_bigint_imag_maps_to_biginteger_big_imag",
            test_term_num_bigint_imag_maps_to_biginteger_big_imag);

    return 0;
}
