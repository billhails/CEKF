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

static void assertSmallBigIntegerLiteral(MinExp *exp, bool imag, int expected) {
    assert(isMinExp_BigInteger(exp));
    MaybeBigInt *mbi = getMinExp_BigInteger(exp);
    assert(mbi->type == BI_SMALL);
    assert(mbi->imag == imag);
    assert(mbi->small == expected);
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

static void test_term_num_rational_maps_to_div_expression(void) {
    Vec *ratio = newVec(2);
    int save = PROTECT(ratio);
    ratio->entries[0] = value_Stdint(3);
    ratio->entries[1] = value_Stdint(4);

    Term *term = makeTerm_Num(NULLPI, value_Rational(ratio));
    PROTECT(term);
    MinExp *exp = termToMinExp(term);
    PROTECT(exp);

    assert(isMinExp_Prim(exp));
    MinPrimApp *prim = getMinExp_Prim(exp);
    assert(prim->type == MINPRIMOP_TYPE_DIV);
    assertSmallBigIntegerLiteral(prim->exp1, false, 3);
    assertSmallBigIntegerLiteral(prim->exp2, false, 4);

    UNPROTECT(save);
}

static void test_term_num_rational_imag_maps_to_div_expression(void) {
    Vec *ratio = newVec(2);
    int save = PROTECT(ratio);
    ratio->entries[0] = value_Stdint(5);
    ratio->entries[1] = value_Stdint(6);

    Term *term = makeTerm_Num(NULLPI, value_Rational_imag(ratio));
    PROTECT(term);
    MinExp *exp = termToMinExp(term);
    PROTECT(exp);

    assert(isMinExp_Prim(exp));
    MinPrimApp *prim = getMinExp_Prim(exp);
    assert(prim->type == MINPRIMOP_TYPE_DIV);
    assertSmallBigIntegerLiteral(prim->exp1, true, 5);
    assertSmallBigIntegerLiteral(prim->exp2, false, 6);

    UNPROTECT(save);
}

static void test_term_num_complex_maps_to_add_expression(void) {
    Vec *complex = newVec(2);
    int save = PROTECT(complex);
    complex->entries[0] = value_Stdint(2);
    complex->entries[1] = value_Stdint_imag(7);

    Term *term = makeTerm_Num(NULLPI, value_Complex(complex));
    PROTECT(term);
    MinExp *exp = termToMinExp(term);
    PROTECT(exp);

    assert(isMinExp_Prim(exp));
    MinPrimApp *prim = getMinExp_Prim(exp);
    assert(prim->type == MINPRIMOP_TYPE_ADD);
    assertSmallBigIntegerLiteral(prim->exp1, false, 2);
    assertSmallBigIntegerLiteral(prim->exp2, true, 7);

    UNPROTECT(save);
}

static void test_minexp_prim_gcd_maps_to_term_gcd(void) {
    MinExp *left = makeSmallBigInteger(12, false);
    int save = PROTECT(left);
    MinExp *right = makeSmallBigInteger(18, false);
    PROTECT(right);
    MinExp *prim = makeMinExp_Prim(NULLPI, MINPRIMOP_TYPE_GCD, left, right);
    PROTECT(prim);

    Term *term = minExpToTerm(prim);
    PROTECT(term);
    assert(term->type == TERM_TYPE_GCD);
    TermOp *gcd = getTerm_Gcd(term);
    assert(gcd->left->type == TERM_TYPE_NUM);
    assert(gcd->right->type == TERM_TYPE_NUM);

    UNPROTECT(save);
}

static void test_minexp_prim_lcm_maps_to_term_lcm(void) {
    MinExp *left = makeSmallBigInteger(12, false);
    int save = PROTECT(left);
    MinExp *right = makeSmallBigInteger(18, false);
    PROTECT(right);
    MinExp *prim = makeMinExp_Prim(NULLPI, MINPRIMOP_TYPE_LCM, left, right);
    PROTECT(prim);

    Term *term = minExpToTerm(prim);
    PROTECT(term);
    assert(term->type == TERM_TYPE_LCM);
    TermOp *lcm = getTerm_Lcm(term);
    assert(lcm->left->type == TERM_TYPE_NUM);
    assert(lcm->right->type == TERM_TYPE_NUM);

    UNPROTECT(save);
}

static void test_term_gcd_maps_to_minexp_prim_gcd(void) {
    Term *left = makeTerm_Num(NULLPI, value_Stdint(12));
    int save = PROTECT(left);
    Term *right = makeTerm_Num(NULLPI, value_Stdint(18));
    PROTECT(right);
    Term *term = makeTerm_Gcd(NULLPI, left, right);
    PROTECT(term);

    MinExp *exp = termToMinExp(term);
    PROTECT(exp);
    assert(isMinExp_Prim(exp));
    MinPrimApp *prim = getMinExp_Prim(exp);
    assert(prim->type == MINPRIMOP_TYPE_GCD);

    UNPROTECT(save);
}

static void test_term_lcm_maps_to_minexp_prim_lcm(void) {
    Term *left = makeTerm_Num(NULLPI, value_Stdint(12));
    int save = PROTECT(left);
    Term *right = makeTerm_Num(NULLPI, value_Stdint(18));
    PROTECT(right);
    Term *term = makeTerm_Lcm(NULLPI, left, right);
    PROTECT(term);

    MinExp *exp = termToMinExp(term);
    PROTECT(exp);
    assert(isMinExp_Prim(exp));
    MinPrimApp *prim = getMinExp_Prim(exp);
    assert(prim->type == MINPRIMOP_TYPE_LCM);

    UNPROTECT(save);
}

static void test_eqterm_gcd_is_commutative(void) {
    Term *one = makeTerm_Num(NULLPI, value_Stdint(1));
    int save = PROTECT(one);
    Term *two = makeTerm_Num(NULLPI, value_Stdint(2));
    PROTECT(two);

    Term *lhs = makeTerm_Gcd(NULLPI, one, two);
    PROTECT(lhs);
    Term *rhs = makeTerm_Gcd(NULLPI, two, one);
    PROTECT(rhs);

    assert(eqTerm(lhs, rhs));
    UNPROTECT(save);
}

static void test_eqterm_lcm_is_commutative(void) {
    Term *one = makeTerm_Num(NULLPI, value_Stdint(1));
    int save = PROTECT(one);
    Term *two = makeTerm_Num(NULLPI, value_Stdint(2));
    PROTECT(two);

    Term *lhs = makeTerm_Lcm(NULLPI, one, two);
    PROTECT(lhs);
    Term *rhs = makeTerm_Lcm(NULLPI, two, one);
    PROTECT(rhs);

    assert(eqTerm(lhs, rhs));
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
    runTest("test_term_num_rational_maps_to_div_expression",
            test_term_num_rational_maps_to_div_expression);
    runTest("test_term_num_rational_imag_maps_to_div_expression",
            test_term_num_rational_imag_maps_to_div_expression);
    runTest("test_term_num_complex_maps_to_add_expression",
            test_term_num_complex_maps_to_add_expression);
    runTest("test_minexp_prim_gcd_maps_to_term_gcd",
            test_minexp_prim_gcd_maps_to_term_gcd);
    runTest("test_minexp_prim_lcm_maps_to_term_lcm",
            test_minexp_prim_lcm_maps_to_term_lcm);
    runTest("test_term_gcd_maps_to_minexp_prim_gcd",
            test_term_gcd_maps_to_minexp_prim_gcd);
    runTest("test_term_lcm_maps_to_minexp_prim_lcm",
            test_term_lcm_maps_to_minexp_prim_lcm);
    runTest("test_eqterm_gcd_is_commutative", test_eqterm_gcd_is_commutative);
    runTest("test_eqterm_lcm_is_commutative", test_eqterm_lcm_is_commutative);

    return 0;
}
