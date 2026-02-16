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

#include "arithmetic_next.h"
#include "bigint.h"
#include "cekf.h"
#include "init.h"

#include <limits.h>
#include <math.h>

static Value stdint(Integer i) { return value_Stdint(i); }
static Value asBigint(Integer i) { return value_Bigint(bigIntFromInt(i)); }
static Value asBigintPow(Integer base, Integer exponent) {
    return value_Bigint(bigIntFromPower(base, exponent));
}
static Value stdintImag(Integer i) { return value_Stdint_imag(i); }
static Value stdComplex(Integer real, Integer imag) {
    Vec *complex = newVec(2);
    complex->entries[0] = stdint(real);
    complex->entries[1] = stdintImag(imag);
    return value_Complex(complex);
}
static Value irrational(Double d) { return value_Irrational(d); }
static Value rational(Value numerator, Value denominator) {
    Vec *ratio = newVec(2);
    ratio->entries[0] = numerator;
    ratio->entries[1] = denominator;
    return value_Rational(ratio);
}

static Value stdRational(Integer numerator, Integer denominator) {
    return rational(stdint(numerator), stdint(denominator));
}

static void assertNAddParity(Value left, Value right) {
    int save = protectValue(left);
    protectValue(right);
    Value next = n_add(left, right);
    protectValue(next);
    Value legacy = nadd(left, right);
    protectValue(legacy);
    assert(ncmp(next, legacy) == CMP_EQ);
    UNPROTECT(save);
}

static void assertNSubParity(Value left, Value right) {
    int save = protectValue(left);
    protectValue(right);
    Value next = n_sub(left, right);
    protectValue(next);
    Value legacy = nsub(left, right);
    protectValue(legacy);
    assert(ncmp(next, legacy) == CMP_EQ);
    UNPROTECT(save);
}

static void assertNMulParity(Value left, Value right) {
    int save = protectValue(left);
    protectValue(right);
    Value next = n_mul(left, right);
    protectValue(next);
    Value legacy = nmul(left, right);
    protectValue(legacy);
    assert(ncmp(next, legacy) == CMP_EQ);
    UNPROTECT(save);
}

static Value cloneDivOperand(Value value) {
    if (value.type == VALUE_TYPE_BIGINT) {
        return value_Bigint(copyBigInt(value.val.bigint));
    }

    if (value.type == VALUE_TYPE_RATIONAL) {
        Value numerator = value.val.vec->entries[0];
        Value denominator = value.val.vec->entries[1];

        if (numerator.type == VALUE_TYPE_BIGINT) {
            numerator = value_Bigint(copyBigInt(numerator.val.bigint));
        }
        if (denominator.type == VALUE_TYPE_BIGINT) {
            denominator = value_Bigint(copyBigInt(denominator.val.bigint));
        }

        return rational(numerator, denominator);
    }

    return value;
}

static void assertNDivParity(Value left, Value right) {
    int save = protectValue(left);
    protectValue(right);
    Value nextLeft = cloneDivOperand(left);
    protectValue(nextLeft);
    Value nextRight = cloneDivOperand(right);
    protectValue(nextRight);
    Value legacyLeft = cloneDivOperand(left);
    protectValue(legacyLeft);
    Value legacyRight = cloneDivOperand(right);
    protectValue(legacyRight);

    Value next = n_div(nextLeft, nextRight);
    protectValue(next);
    Value legacy = ndiv(legacyLeft, legacyRight);
    protectValue(legacy);
    assert(ncmp(next, legacy) == CMP_EQ);
    UNPROTECT(save);
}

static void assertNModParity(Value left, Value right) {
    int save = protectValue(left);
    protectValue(right);
    Value nextLeft = cloneDivOperand(left);
    protectValue(nextLeft);
    Value nextRight = cloneDivOperand(right);
    protectValue(nextRight);
    Value legacyLeft = cloneDivOperand(left);
    protectValue(legacyLeft);
    Value legacyRight = cloneDivOperand(right);
    protectValue(legacyRight);

    Value next = n_mod(nextLeft, nextRight);
    protectValue(next);
    Value legacy = nmod(legacyLeft, legacyRight);
    protectValue(legacy);
    assert(ncmp(next, legacy) == CMP_EQ);
    UNPROTECT(save);
}

static void assertNCmpParity(Value left, Value right) {
    int save = protectValue(left);
    protectValue(right);
    Value nextLeft = cloneDivOperand(left);
    protectValue(nextLeft);
    Value nextRight = cloneDivOperand(right);
    protectValue(nextRight);
    Value legacyLeft = cloneDivOperand(left);
    protectValue(legacyLeft);
    Value legacyRight = cloneDivOperand(right);
    protectValue(legacyRight);

    Cmp next = n_cmp(nextLeft, nextRight);
    Cmp legacy = ncmp(legacyLeft, legacyRight);
    assert(next == legacy);
    UNPROTECT(save);
}

static void assertNPowParity(Value left, Value right) {
    int save = protectValue(left);
    protectValue(right);
    Value next = n_pow(left, right);
    protectValue(next);
    Value legacy = npow(left, right);
    protectValue(legacy);
    assert(ncmp(next, legacy) == CMP_EQ);
    UNPROTECT(save);
}

static void testNAddStdintEndToEnd() {
    Value a = stdint(7);
    Value b = stdint(3);
    Value res = n_add(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == 10);
    assertNAddParity(a, b);

    a = stdint(INT_MAX);
    b = stdint(1);
    res = n_add(a, b);
    assert(isValue_Bigint(res));
    assertNAddParity(a, b);

    a = stdint(INT_MIN);
    b = stdint(-1);
    res = n_add(a, b);
    assert(isValue_Bigint(res));
    assertNAddParity(a, b);

    a = stdint(INT_MAX);
    b = stdint(-1);
    res = n_add(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == INT_MAX - 1);
    assertNAddParity(a, b);
}

static void testNAddBigintEndToEnd() {
    Value a = asBigint(1000000);
    int save = protectValue(a);
    Value b = asBigint(2345678);
    protectValue(b);
    Value res = n_add(a, b);
    assert(isValue_Bigint(res));
    assertNAddParity(a, b);
    UNPROTECT(save);

    a = asBigint(42);
    b = stdint(8);
    res = n_add(a, b);
    assert(isValue_Bigint(res));
    assertNAddParity(a, b);

    a = stdint(8);
    b = asBigint(42);
    res = n_add(a, b);
    assert(isValue_Bigint(res));
    assertNAddParity(a, b);
}

static void testNSubStdintEndToEnd() {
    Value a = stdint(7);
    Value b = stdint(3);
    Value res = n_sub(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == 4);
    assertNSubParity(a, b);

    a = stdint(INT_MIN);
    b = stdint(1);
    res = n_sub(a, b);
    assert(isValue_Bigint(res));
    assertNSubParity(a, b);

    a = stdint(INT_MAX);
    b = stdint(-1);
    res = n_sub(a, b);
    assert(isValue_Bigint(res));
    assertNSubParity(a, b);

    a = stdint(INT_MIN);
    b = stdint(-1);
    res = n_sub(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == INT_MIN + 1);
    assertNSubParity(a, b);
}

static void testNSubBigintEndToEnd() {
    Value a = asBigint(2345678);
    int save = protectValue(a);
    Value b = asBigint(1000000);
    protectValue(b);
    Value res = n_sub(a, b);
    assert(isValue_Bigint(res));
    assertNSubParity(a, b);
    UNPROTECT(save);

    a = asBigint(42);
    b = stdint(8);
    res = n_sub(a, b);
    assert(isValue_Bigint(res));
    assertNSubParity(a, b);

    a = stdint(8);
    b = asBigint(42);
    res = n_sub(a, b);
    assert(isValue_Bigint(res));
    assertNSubParity(a, b);
}

static void testNAddIrrationalEndToEnd() {
    Value a = irrational(1.25);
    Value b = irrational(2.5);
    Value res = n_add(a, b);
    assert(res.type == VALUE_TYPE_IRRATIONAL);
    assert(res.val.irrational == 3.75);
    assertNAddParity(a, b);
}

static void testNAddImagEndToEnd() {
    Value a = stdintImag(2);
    Value b = stdintImag(3);
    Value res = n_add(a, b);
    assert(res.type == VALUE_TYPE_STDINT_IMAG);
    assert(res.val.stdint == 5);
    assertNAddParity(a, b);

    a = stdintImag(INT_MAX);
    b = stdintImag(1);
    res = n_add(a, b);
    assert(res.type == VALUE_TYPE_BIGINT_IMAG);
    assertNAddParity(a, b);
}

static void testNAddRationalEndToEnd() {
    Value a = stdRational(1, 2);
    int save = protectValue(a);
    Value b = stdRational(1, 3);
    protectValue(b);
    Value res = n_add(a, b);
    protectValue(res);
    Value expected = stdRational(5, 6);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNAddParity(a, b);
    UNPROTECT(save);

    a = stdRational(1, 2);
    save = protectValue(a);
    b = stdRational(1, 2);
    protectValue(b);
    res = n_add(a, b);
    protectValue(res);
    assert(res.type == VALUE_TYPE_STDINT);
    assert(res.val.stdint == 1);
    assertNAddParity(a, b);
    UNPROTECT(save);

    a = stdRational(1, 2);
    b = stdint(3);
    save = protectValue(a);
    protectValue(b);
    res = n_add(a, b);
    protectValue(res);
    expected = stdRational(7, 2);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNAddParity(a, b);
    UNPROTECT(save);
}

static void testNSubIrrationalEndToEnd() {
    Value a = irrational(5.5);
    Value b = irrational(2.0);
    Value res = n_sub(a, b);
    assert(res.type == VALUE_TYPE_IRRATIONAL);
    assert(res.val.irrational == 3.5);
    assertNSubParity(a, b);
}

static void testNSubImagEndToEnd() {
    Value a = stdintImag(7);
    Value b = stdintImag(3);
    Value res = n_sub(a, b);
    assert(res.type == VALUE_TYPE_STDINT_IMAG);
    assert(res.val.stdint == 4);
    assertNSubParity(a, b);

    a = stdintImag(INT_MIN);
    b = stdintImag(1);
    res = n_sub(a, b);
    assert(res.type == VALUE_TYPE_BIGINT_IMAG);
    assertNSubParity(a, b);
}

static void testNSubRationalEndToEnd() {
    Value a = stdRational(5, 6);
    int save = protectValue(a);
    Value b = stdRational(1, 2);
    protectValue(b);
    Value res = n_sub(a, b);
    protectValue(res);
    Value expected = stdRational(1, 3);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNSubParity(a, b);
    UNPROTECT(save);

    a = stdRational(1, 2);
    save = protectValue(a);
    b = stdRational(1, 2);
    protectValue(b);
    res = n_sub(a, b);
    protectValue(res);
    assert(ncmp(res, stdint(0)) == CMP_EQ);
    assertNSubParity(a, b);
    UNPROTECT(save);

    a = stdRational(7, 2);
    b = stdint(3);
    save = protectValue(a);
    protectValue(b);
    res = n_sub(a, b);
    protectValue(res);
    expected = stdRational(1, 2);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNSubParity(a, b);
    UNPROTECT(save);
}

static void testNMulStdintEndToEnd() {
    Value a = stdint(7);
    Value b = stdint(3);
    Value res = n_mul(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == 21);
    assertNMulParity(a, b);

    a = stdint(INT_MAX);
    b = stdint(2);
    res = n_mul(a, b);
    assert(isValue_Bigint(res));
    assertNMulParity(a, b);

    a = stdint(INT_MIN);
    b = stdint(2);
    res = n_mul(a, b);
    assert(isValue_Bigint(res));
    assertNMulParity(a, b);

    a = stdint(INT_MAX);
    b = stdint(1);
    res = n_mul(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == INT_MAX);
    assertNMulParity(a, b);
}

static void testNMulBigintEndToEnd() {
    Value a = asBigint(1000000);
    int save = protectValue(a);
    Value b = asBigint(2345678);
    protectValue(b);
    Value res = n_mul(a, b);
    assert(isValue_Bigint(res));
    assertNMulParity(a, b);
    UNPROTECT(save);

    a = asBigint(42);
    b = stdint(8);
    res = n_mul(a, b);
    assert(isValue_Bigint(res));
    assertNMulParity(a, b);

    a = stdint(8);
    b = asBigint(42);
    res = n_mul(a, b);
    assert(isValue_Bigint(res));
    assertNMulParity(a, b);
}

static void testNMulIrrationalEndToEnd() {
    Value a = irrational(1.25);
    Value b = irrational(2.0);
    Value res = n_mul(a, b);
    assert(res.type == VALUE_TYPE_IRRATIONAL);
    assert(res.val.irrational == 2.5);
    assertNMulParity(a, b);
}

static void testNMulImagEndToEnd() {
    Value a = stdintImag(2);
    Value b = stdintImag(3);
    Value res = n_mul(a, b);
    assert(res.type == VALUE_TYPE_STDINT);
    assert(res.val.stdint == -6);
    assertNMulParity(a, b);

    a = stdintImag(INT_MAX);
    b = stdintImag(INT_MAX);
    res = n_mul(a, b);
    assert(res.type == VALUE_TYPE_BIGINT);
    assertNMulParity(a, b);
}

static void testNMulRationalEndToEnd() {
    Value a = stdRational(2, 3);
    int save = protectValue(a);
    Value b = stdRational(3, 4);
    protectValue(b);
    Value res = n_mul(a, b);
    protectValue(res);
    Value expected = stdRational(1, 2);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNMulParity(a, b);
    UNPROTECT(save);

    a = stdRational(1, 2);
    save = protectValue(a);
    b = stdRational(2, 1);
    protectValue(b);
    res = n_mul(a, b);
    protectValue(res);
    assert(ncmp(res, stdint(1)) == CMP_EQ);
    assertNMulParity(a, b);
    UNPROTECT(save);

    a = stdRational(1, 2);
    save = protectValue(a);
    b = stdint(3);
    protectValue(b);
    res = n_mul(a, b);
    protectValue(res);
    expected = stdRational(3, 2);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNMulParity(a, b);
    UNPROTECT(save);
}

static void testNDivStdintEndToEnd() {
    Value a = stdint(8);
    Value b = stdint(2);
    Value res = n_div(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == 4);
    assertNDivParity(a, b);

    a = stdint(7);
    b = stdint(3);
    res = n_div(a, b);
    Value expected = stdRational(7, 3);
    int save = protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNDivParity(a, b);
    UNPROTECT(save);
}

static void testNDivBigintEndToEnd() {
    Value a = asBigint(42);
    Value b = stdint(7);
    Value res = n_div(a, b);
    assert(isValue_Bigint(res));
    assertNDivParity(a, b);

    a = asBigint(10);
    b = stdint(3);
    res = n_div(a, b);
    int saveRes = protectValue(res);
    Value expectedNum = asBigint(10);
    int save = protectValue(expectedNum);
    Value expectedDen = asBigint(3);
    protectValue(expectedDen);
    Value expected = rational(expectedNum, expectedDen);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNDivParity(a, b);
    UNPROTECT(save);
    UNPROTECT(saveRes);
}

static void testNDivIrrationalEndToEnd() {
    Value a = irrational(7.5);
    Value b = irrational(2.5);
    Value res = n_div(a, b);
    assert(res.type == VALUE_TYPE_IRRATIONAL);
    assert(res.val.irrational == 3.0);
    assertNDivParity(a, b);
}

static void testNDivRationalEndToEnd() {
    Value a = stdRational(1, 2);
    int save = protectValue(a);
    Value b = stdRational(3, 4);
    protectValue(b);
    Value res = n_div(a, b);
    protectValue(res);
    Value expected = stdRational(2, 3);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNDivParity(a, b);
    UNPROTECT(save);

    a = stdRational(3, 2);
    save = protectValue(a);
    b = stdint(3);
    protectValue(b);
    res = n_div(a, b);
    protectValue(res);
    expected = stdRational(1, 2);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNDivParity(a, b);
    UNPROTECT(save);
}

static void testNDivImagEndToEnd() {
    Value a = stdintImag(6);
    Value b = stdintImag(3);
    Value res = n_div(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == 2);
    assertNDivParity(a, b);
}

static void testNDivComplexEndToEnd() {
    Value a = stdComplex(4, 2);
    int save = protectValue(a);
    Value b = stdComplex(1, 1);
    protectValue(b);
    Value res = n_div(a, b);
    protectValue(res);
    Value expected = stdComplex(3, -1);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNDivParity(a, b);
    UNPROTECT(save);
}

static void testNModStdintEndToEnd() {
    Value a = stdint(8);
    Value b = stdint(3);
    Value res = n_mod(a, b);
    assert(isValue_Stdint(res));
    assert(getValue_Stdint(res) == 2);
    assertNModParity(a, b);
}

static void testNModBigintEndToEnd() {
    Value a = asBigint(42);
    int save = protectValue(a);
    Value b = stdint(5);
    protectValue(b);
    Value res = n_mod(a, b);
    protectValue(res);
    assert(ncmp(res, stdint(2)) == CMP_EQ);
    assertNModParity(a, b);
    UNPROTECT(save);

    a = asBigint(10);
    save = protectValue(a);
    b = stdint(3);
    protectValue(b);
    res = n_mod(a, b);
    protectValue(res);
    assert(ncmp(res, stdint(1)) == CMP_EQ);
    assertNModParity(a, b);
    UNPROTECT(save);
}

static void testNModIrrationalEndToEnd() {
    Value a = irrational(7.5);
    Value b = irrational(2.0);
    Value res = n_mod(a, b);
    assert(res.type == VALUE_TYPE_IRRATIONAL);
    assert(res.val.irrational == 1.5);
    assertNModParity(a, b);
}

static void testNModRationalEndToEnd() {
    Value a = stdRational(5, 6);
    int save = protectValue(a);
    Value b = stdRational(1, 2);
    protectValue(b);
    Value res = n_mod(a, b);
    protectValue(res);
    Value expected = stdRational(1, 3);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNModParity(a, b);
    UNPROTECT(save);

    a = stdRational(7, 2);
    save = protectValue(a);
    b = stdint(3);
    protectValue(b);
    res = n_mod(a, b);
    protectValue(res);
    expected = stdRational(1, 2);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNModParity(a, b);
    UNPROTECT(save);
}

static void testNModImagEndToEnd() {
    Value a = stdintImag(7);
    Value b = stdintImag(3);
    Value res = n_mod(a, b);
    assert(res.type == VALUE_TYPE_STDINT_IMAG);
    assert(res.val.stdint == 1);
}

static void testNModComplexEndToEnd() {
    Value a = stdComplex(5, 7);
    int save = protectValue(a);
    Value b = stdComplex(2, 3);
    protectValue(b);
    Value res = n_mod(a, b);
    protectValue(res);
    Value expected = stdComplex(1, 1);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    UNPROTECT(save);
}

static void testNAddComplexEndToEnd() {
    Value a = stdComplex(1, 2);
    int save = protectValue(a);
    Value b = stdComplex(3, 4);
    protectValue(b);
    Value res = n_add(a, b);
    protectValue(res);
    Value expected = stdComplex(4, 6);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNAddParity(a, b);
    UNPROTECT(save);
}

static void testNSubComplexEndToEnd() {
    Value a = stdComplex(5, 7);
    int save = protectValue(a);
    Value b = stdComplex(2, 3);
    protectValue(b);
    Value res = n_sub(a, b);
    protectValue(res);
    Value expected = stdComplex(3, 4);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNSubParity(a, b);
    UNPROTECT(save);
}

static void testNMulComplexEndToEnd() {
    Value a = stdComplex(1, 2);
    int save = protectValue(a);
    Value b = stdComplex(3, 4);
    protectValue(b);
    Value res = n_mul(a, b);
    protectValue(res);
    Value expected = stdComplex(-5, 10);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNMulParity(a, b);
    UNPROTECT(save);
}

static void testNCmpStdintEndToEnd() {
    assert(n_cmp(stdint(1), stdint(2)) == CMP_LT);
    assert(n_cmp(stdint(2), stdint(2)) == CMP_EQ);
    assert(n_cmp(stdint(3), stdint(2)) == CMP_GT);
    assertNCmpParity(stdint(1), stdint(2));
    assertNCmpParity(stdint(2), stdint(2));
    assertNCmpParity(stdint(3), stdint(2));
}

static void testNCmpBigintEndToEnd() {
    Value a = asBigint(10);
    int save = protectValue(a);
    Value b = asBigint(3);
    protectValue(b);
    assert(n_cmp(a, b) == CMP_GT);
    assertNCmpParity(a, b);
    UNPROTECT(save);

    a = asBigint(3);
    b = stdint(3);
    assert(n_cmp(a, b) == CMP_EQ);
    assertNCmpParity(a, b);

    a = stdint(2);
    b = asBigint(3);
    assert(n_cmp(a, b) == CMP_LT);
    assertNCmpParity(a, b);
}

static void testNCmpIrrationalEndToEnd() {
    Value a = irrational(1.5);
    Value b = irrational(2.5);
    assert(n_cmp(a, b) == CMP_LT);
    assertNCmpParity(a, b);

    a = irrational(2.5);
    b = irrational(2.5);
    assert(n_cmp(a, b) == CMP_EQ);
    assertNCmpParity(a, b);

    a = irrational(3.5);
    b = irrational(2.5);
    assert(n_cmp(a, b) == CMP_GT);
    assertNCmpParity(a, b);
}

static void testNCmpRationalEndToEnd() {
    Value a = stdRational(1, 2);
    int save = protectValue(a);
    Value b = stdRational(2, 3);
    protectValue(b);
    assert(n_cmp(a, b) == CMP_LT);
    assertNCmpParity(a, b);
    UNPROTECT(save);

    a = stdRational(3, 2);
    save = protectValue(a);
    b = stdint(1);
    protectValue(b);
    assert(n_cmp(a, b) == CMP_GT);
    assertNCmpParity(a, b);
    UNPROTECT(save);
}

static void testNCmpImagEndToEnd() {
    Value a = stdintImag(1);
    Value b = stdintImag(2);
    assert(n_cmp(a, b) == CMP_LT);
    assertNCmpParity(a, b);

    a = stdintImag(3);
    b = stdintImag(3);
    assert(n_cmp(a, b) == CMP_EQ);
    assertNCmpParity(a, b);
}

static void testNCmpComplexEndToEnd() {
    Value a = stdComplex(1, 2);
    int save = protectValue(a);
    Value b = stdComplex(3, 2);
    protectValue(b);
    assert(n_cmp(a, b) == CMP_LT);
    assertNCmpParity(a, b);
    UNPROTECT(save);

    a = stdComplex(4, 5);
    save = protectValue(a);
    b = stdComplex(4, 5);
    protectValue(b);
    assert(n_cmp(a, b) == CMP_EQ);
    assertNCmpParity(a, b);
    UNPROTECT(save);
}

static void testNPowIntEndToEnd() {
    Value a = stdint(2);
    Value b = stdint(3);
    Value res = n_pow(a, b);
    assert(ncmp(res, stdint(8)) == CMP_EQ);
    assertNPowParity(a, b);

    a = stdint(2);
    b = stdint(-3);
    int save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    Value expected = stdRational(1, 8);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = asBigint(2);
    save = protectValue(a);
    b = asBigint(10);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = stdint(1024);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = stdint(-2);
    b = stdint(5);
    res = n_pow(a, b);
    assert(ncmp(res, stdint(-32)) == CMP_EQ);
    assertNPowParity(a, b);

    a = stdint(7);
    b = stdint(0);
    res = n_pow(a, b);
    assert(ncmp(res, stdint(1)) == CMP_EQ);
    assertNPowParity(a, b);

    a = asBigint(2);
    save = protectValue(a);
    b = asBigint(31);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = asBigintPow(2, 31);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);
}

static void testNPowIrrationalEndToEnd() {
    Value a = irrational(9.0);
    Value b = irrational(0.5);
    Value res = n_pow(a, b);
    assert(res.type == VALUE_TYPE_IRRATIONAL);
    assert(fabs(res.val.irrational - 3.0) < 1e-12);
    assertNPowParity(a, b);

    a = irrational(2.0);
    b = irrational(10.0);
    res = n_pow(a, b);
    assert(res.type == VALUE_TYPE_IRRATIONAL);
    assert(fabs(res.val.irrational - 1024.0) < 1e-9);
    assertNPowParity(a, b);

    a = irrational(4.0);
    b = irrational(-1.0);
    res = n_pow(a, b);
    assert(res.type == VALUE_TYPE_IRRATIONAL);
    assert(fabs(res.val.irrational - 0.25) < 1e-12);
    assertNPowParity(a, b);
}

static void testNPowRationalIntEndToEnd() {
    Value a = stdRational(2, 3);
    Value b = stdint(3);
    int save = protectValue(a);
    protectValue(b);
    Value res = n_pow(a, b);
    protectValue(res);
    Value expected = stdRational(8, 27);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = stdRational(2, 3);
    b = stdint(-2);
    save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = stdRational(9, 4);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    UNPROTECT(save);

    a = stdRational(5, 7);
    b = stdint(0);
    save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = stdint(1);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = stdRational(2, 3);
    b = asBigint(4);
    save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = stdRational(16, 81);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);
}

static void testNPowComplexIntEndToEnd() {
    Value a = stdComplex(1, 1);
    Value b = stdint(2);
    int save = protectValue(a);
    protectValue(b);
    Value res = n_pow(a, b);
    protectValue(res);
    Value expected = stdintImag(2);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = stdintImag(2);
    b = stdint(3);
    save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = stdintImag(-8);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = stdComplex(1, 1);
    b = stdint(0);
    save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = stdint(1);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = stdComplex(1, 1);
    b = stdint(16);
    save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = stdint(256);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = stdintImag(2);
    b = stdint(10);
    save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    expected = stdint(-1024);
    protectValue(expected);
    assert(ncmp(res, expected) == CMP_EQ);
    assertNPowParity(a, b);
    UNPROTECT(save);

    a = stdComplex(1, 1);
    b = stdint(-3);
    save = protectValue(a);
    protectValue(b);
    res = n_pow(a, b);
    protectValue(res);
    assertNPowParity(a, b);
    UNPROTECT(save);
}

static void testNPowCrossDomainParity() {
    int save = PROTECT(NULL);

    Value rat = stdRational(9, 4);
    protectValue(rat);
    Value ratExp = stdRational(1, 2);
    protectValue(ratExp);
    assertNPowParity(rat, ratExp);

    Value intBase = stdint(5);
    protectValue(intBase);
    Value irrExp = irrational(1.5);
    protectValue(irrExp);
    assertNPowParity(intBase, irrExp);

    Value irrBase = irrational(2.25);
    protectValue(irrBase);
    Value ratTwoThirds = stdRational(2, 3);
    protectValue(ratTwoThirds);
    assertNPowParity(irrBase, ratTwoThirds);

    Value imagBase = stdintImag(2);
    protectValue(imagBase);
    Value irrHalf = irrational(0.5);
    protectValue(irrHalf);
    assertNPowParity(imagBase, irrHalf);

    Value complexBase = stdComplex(2, 3);
    protectValue(complexBase);
    Value complexExp = stdComplex(1, 1);
    protectValue(complexExp);
    assertNPowParity(complexBase, complexExp);

    Value complexRatExp = stdRational(3, 2);
    protectValue(complexRatExp);
    assertNPowParity(complexBase, complexRatExp);

    Value irrComplexExp = irrational(0.75);
    protectValue(irrComplexExp);
    assertNPowParity(complexBase, irrComplexExp);

    Value intComplexExp = stdComplex(1, -1);
    protectValue(intComplexExp);
    assertNPowParity(stdint(3), intComplexExp);

    Value posRatNegDen = stdRational(-4, -9);
    protectValue(posRatNegDen);
    Value half = stdRational(1, 2);
    protectValue(half);
    Value posRatRes = n_pow(posRatNegDen, half);
    protectValue(posRatRes);
    Value twoThirds = stdRational(2, 3);
    protectValue(twoThirds);
    assert(ncmp(posRatRes, twoThirds) == CMP_EQ);

    Value zeroRatNegDen = stdRational(0, -7);
    protectValue(zeroRatNegDen);
    Value third = stdRational(1, 3);
    protectValue(third);
    Value zeroRatRes = n_pow(zeroRatNegDen, third);
    protectValue(zeroRatRes);
    assert(ncmp(zeroRatRes, stdint(0)) == CMP_EQ);

    Value imagBaseNeg = stdintImag(-4);
    protectValue(imagBaseNeg);
    Value halfImagExp = stdRational(1, 2);
    protectValue(halfImagExp);
    Value imagRatRes = n_pow(imagBaseNeg, halfImagExp);
    protectValue(imagRatRes);
    assert(ncmp(imagRatRes, stdint(-2)) == CMP_EQ);

    UNPROTECT(save);
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    initAll();
    testNAddStdintEndToEnd();
    testNAddBigintEndToEnd();
    testNAddIrrationalEndToEnd();
    testNAddImagEndToEnd();
    testNAddComplexEndToEnd();
    testNAddRationalEndToEnd();
    testNSubStdintEndToEnd();
    testNSubBigintEndToEnd();
    testNSubIrrationalEndToEnd();
    testNSubImagEndToEnd();
    testNSubComplexEndToEnd();
    testNSubRationalEndToEnd();
    testNMulStdintEndToEnd();
    testNMulBigintEndToEnd();
    testNMulIrrationalEndToEnd();
    testNMulImagEndToEnd();
    testNMulComplexEndToEnd();
    testNMulRationalEndToEnd();
    testNDivStdintEndToEnd();
    testNDivBigintEndToEnd();
    testNDivIrrationalEndToEnd();
    testNDivRationalEndToEnd();
    testNDivImagEndToEnd();
    testNDivComplexEndToEnd();
    testNModStdintEndToEnd();
    testNModBigintEndToEnd();
    testNModIrrationalEndToEnd();
    testNModRationalEndToEnd();
    testNModImagEndToEnd();
    testNModComplexEndToEnd();
    testNCmpStdintEndToEnd();
    testNCmpBigintEndToEnd();
    testNCmpIrrationalEndToEnd();
    testNCmpRationalEndToEnd();
    testNCmpImagEndToEnd();
    testNCmpComplexEndToEnd();
    testNPowIntEndToEnd();
    testNPowIrrationalEndToEnd();
    testNPowRationalIntEndToEnd();
    testNPowComplexIntEndToEnd();
    testNPowCrossDomainParity();
    return 0;
}
