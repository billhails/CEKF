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

#include "arithmetic_next.h"
#include "arithmetic_dispatch.h"
#include "arithmetic_number_checks.h"
#include "bigint.h"
#include "cekf.h"
#include "common.h"
#include <limits.h>
#include <math.h>

typedef Value (*NextBinaryHandler)(Value left, Value right);
typedef Cmp (*NextCmpHandler)(Value left, Value right);

static Value imagToReal(Value value);
static Value realToImag(Value value);
static Value powComplexInt(Value left, Value right);
static Value powComplexComplex(Value base, Value exponent);
static Value n_add(Value left, Value right);
static Value n_sub(Value left, Value right);
static Value n_mul(Value left, Value right);
static Value n_div(Value left, Value right);
static Value n_mod(Value left, Value right);
static Value n_pow(Value left, Value right);
static Cmp n_cmp(Value left, Value right);
static Value n_neg(Value value);
static Value n_rand(Value prev);
static Value n_real_part(Value value);
static Value n_imag_part(Value value);
static Value n_mag_part(Value value);
static Value n_theta_part(Value value);

static inline Value softNaN(void) { return value_Irrational(NAN); }

static bool containsNaN(Value value) {
    switch (value.type) {
    case VALUE_TYPE_IRRATIONAL:
        return isnan(getValue_Irrational(value));
    case VALUE_TYPE_IRRATIONAL_IMAG:
        return isnan(getValue_Irrational_imag(value));
    default:
        return false;
    }
}

static bool containsCompositeNaN(Value value) {
    switch (value.type) {
    case VALUE_TYPE_RATIONAL: {
        Vec *ratio = getValue_Rational(value);
        return containsNaN(ratio->entries[0]) ||
               containsNaN(ratio->entries[1]) ||
               containsCompositeNaN(ratio->entries[0]) ||
               containsCompositeNaN(ratio->entries[1]);
    }
    case VALUE_TYPE_COMPLEX: {
        Vec *complex = getValue_Complex(value);
        return containsNaN(complex->entries[0]) ||
               containsNaN(complex->entries[1]) ||
               containsCompositeNaN(complex->entries[0]) ||
               containsCompositeNaN(complex->entries[1]);
    }
    default:
        return false;
    }
}

static void assertCanonicalNaN(Value value) {
    ASSERT(!containsCompositeNaN(value));
}

/////////////////////////////
// normalization utilities
/////////////////////////////

static ArithmeticNormalizationPlan requirePlan(ArithmeticOperator op,
                                               Value left, Value right) {
    ArithmeticNormalizationPlan plan =
        arithmeticMakeNormalizationPlan(op, left.type, right.type);
    if (plan.supported) {
        return plan;
    }
    cant_happen("unsupported numeric pair in next arithmetic layer (%s, %s)",
                valueTypeName(left.type), valueTypeName(right.type));
}

static NextBinaryHandler getDomainHandler(const NextBinaryHandler *handlers,
                                          size_t handlerCount,
                                          ArithmeticDomain domain,
                                          const char *opName) {
    if (domain < 0 || domain >= (ArithmeticDomain)handlerCount) {
        cant_happen("invalid %s domain %d", opName, domain);
    }
    NextBinaryHandler handler = handlers[domain];
    if (handler != NULL) {
        return handler;
    }
    cant_happen("missing %s handler for domain %d", opName, domain);
}

static Value applyCommonDomainBinary(ArithmeticOperator op, const char *opName,
                                     const NextBinaryHandler *handlers,
                                     size_t handlerCount, Value left,
                                     Value right) {
    ArithmeticNormalizationPlan plan = requirePlan(op, left, right);
    if (plan.kind != ARITH_NORM_COMMON_DOMAIN) {
        cant_happen("unexpected %s normalization kind %d", opName, plan.kind);
    }
    NextBinaryHandler handler =
        getDomainHandler(handlers, handlerCount, plan.commonDomain, opName);
    return handler(left, right);
}

static NextCmpHandler getCmpDomainHandler(const NextCmpHandler *handlers,
                                          size_t handlerCount,
                                          ArithmeticDomain domain,
                                          const char *opName) {
    if (domain < 0 || domain >= (ArithmeticDomain)handlerCount) {
        cant_happen("invalid %s domain %d", opName, domain);
    }
    NextCmpHandler handler = handlers[domain];
    if (handler != NULL) {
        return handler;
    }
    cant_happen("missing %s handler for domain %d", opName, domain);
}

static Cmp applyCommonDomainCompare(ArithmeticOperator op, const char *opName,
                                    const NextCmpHandler *handlers,
                                    size_t handlerCount, Value left,
                                    Value right) {
    ArithmeticNormalizationPlan plan = requirePlan(op, left, right);
    if (plan.kind != ARITH_NORM_COMMON_DOMAIN) {
        cant_happen("unexpected %s normalization kind %d", opName, plan.kind);
    }
    NextCmpHandler handler =
        getCmpDomainHandler(handlers, handlerCount, plan.commonDomain, opName);
    return handler(left, right);
}

static Value ratValue(Value numerator, Value denominator) {
    if (containsNaN(numerator) || containsNaN(denominator)) {
        return softNaN();
    }
    Vec *vec = newVec(2);
    vec->entries[0] = numerator;
    vec->entries[1] = denominator;
    return value_Rational(vec);
}

static Value toRat(Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT:
    case VALUE_TYPE_BIGINT:
        return ratValue(value, value_Stdint(1));
    case VALUE_TYPE_RATIONAL:
        return value;
    default:
        cant_happen("invalid rational-compatible type %s",
                    valueTypeName(value.type));
    }
}

static void unpackRationalPair(Value left, Value right, Value *a, Value *b,
                               Value *c, Value *d) {
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);

    Vec *leftRat = getValue_Rational(left);
    Vec *rightRat = getValue_Rational(right);
    *a = leftRat->entries[0];
    *b = leftRat->entries[1];
    *c = rightRat->entries[0];
    *d = rightRat->entries[1];
}

static Value ratFallbackBinary(Value left, Value right, NextBinaryHandler op) {
    Value leftRat = toRat(left);
    int save = protectValue(leftRat);
    Value rightRat = toRat(right);
    protectValue(rightRat);
    Value res = op(leftRat, rightRat);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Cmp ratFallbackCompare(Value left, Value right, NextCmpHandler op) {
    Value leftRat = toRat(left);
    int save = protectValue(leftRat);
    Value rightRat = toRat(right);
    protectValue(rightRat);
    Cmp res = op(leftRat, rightRat);
    UNPROTECT(save);
    return res;
}

static Value toIrr(Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT:
        return value_Irrational(getValue_Stdint(value));
    case VALUE_TYPE_BIGINT:
        return value_Irrational(bigIntToDouble(getValue_Bigint(value)));
    case VALUE_TYPE_RATIONAL: {
        Vec *rational = getValue_Rational(value);
        Value numerator = rational->entries[0];
        Value denominator = rational->entries[1];
        Value irrNum = toIrr(numerator);
        Value irrDen = toIrr(denominator);
        return value_Irrational(getValue_Irrational(irrNum) /
                                getValue_Irrational(irrDen));
    }
    case VALUE_TYPE_IRRATIONAL:
        return value;
    default:
        cant_happen("invalid real type %s", valueTypeName(value.type));
    }
}

static Value comValue(Value real, Value imag) {
    if (containsNaN(real) || containsNaN(imag)) {
        return softNaN();
    }
    Vec *vec = newVec(2);
    vec->entries[0] = real;
    vec->entries[1] = imag;
    return value_Complex(vec);
}

static Value complexRealPart(Value value) {
    ASSERT(value.type == VALUE_TYPE_COMPLEX);
    return getValue_Complex(value)->entries[0];
}

static Value complexImagPart(Value value) {
    ASSERT(value.type == VALUE_TYPE_COMPLEX);
    return getValue_Complex(value)->entries[1];
}

static Value complexImagAsReal(Value value) {
    return imagToReal(complexImagPart(value));
}

static void unpackComplexPair(Value left, Value right, Value *a, Value *b,
                              Value *c, Value *d) {
    ASSERT(left.type == VALUE_TYPE_COMPLEX);
    ASSERT(right.type == VALUE_TYPE_COMPLEX);

    *a = complexRealPart(left);
    *b = complexImagAsReal(left);
    *c = complexRealPart(right);
    *d = complexImagAsReal(right);
}

static Value toComplex(Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT:
    case VALUE_TYPE_BIGINT:
    case VALUE_TYPE_RATIONAL:
    case VALUE_TYPE_IRRATIONAL:
        return comValue(value, value_Stdint_imag(0));
    case VALUE_TYPE_STDINT_IMAG:
    case VALUE_TYPE_BIGINT_IMAG:
    case VALUE_TYPE_RATIONAL_IMAG:
    case VALUE_TYPE_IRRATIONAL_IMAG:
        return comValue(value_Stdint(0), value);
    case VALUE_TYPE_COMPLEX:
        return value;
    default:
        cant_happen("invalid numeric type %s", valueTypeName(value.type));
    }
}

static Cmp magCmp(Value leftReal, Value leftImag, Value rightReal,
                  Value rightImag) {
    Value leftC = n_add(leftReal, leftImag);
    int save = protectValue(leftC);
    Value rightC = n_add(rightReal, rightImag);
    protectValue(rightC);

    Cmp cmpMag = n_cmp(leftC, rightC);
    if (cmpMag == CMP_EQ) {
        Cmp cmpReal = n_cmp(leftReal, rightReal);
        UNPROTECT(save);
        return cmpReal == CMP_LT ? CMP_LT : CMP_GT;
    }

    UNPROTECT(save);
    return cmpMag;
}

static Value comSimplify(Value real, Value imagReal) {
    int save = protectValue(real);
    protectValue(imagReal);

    Value zero = value_Stdint(0);
    Value res;
    if (n_cmp(zero, imagReal) == CMP_EQ) {
        res = real;
        protectValue(res);
        UNPROTECT(save);
        return res;
    }

    if (n_cmp(zero, real) == CMP_EQ) {
        res = realToImag(imagReal);
        protectValue(res);
        UNPROTECT(save);
        return res;
    }

    Value imag = realToImag(imagReal);
    protectValue(imag);
    res = comValue(real, imag);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value imagToReal(Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT_IMAG:
        value.type = VALUE_TYPE_STDINT;
        return value;
    case VALUE_TYPE_BIGINT_IMAG:
        value.type = VALUE_TYPE_BIGINT;
        return value;
    case VALUE_TYPE_RATIONAL_IMAG:
        value.type = VALUE_TYPE_RATIONAL;
        return value;
    case VALUE_TYPE_IRRATIONAL_IMAG:
        value.type = VALUE_TYPE_IRRATIONAL;
        return value;
    default:
        cant_happen("invalid imag type %s", valueTypeName(value.type));
    }
}

static Value realToImag(Value value) {
    if (containsNaN(value)) {
        return softNaN();
    }
    switch (value.type) {
    case VALUE_TYPE_STDINT:
        value.type = VALUE_TYPE_STDINT_IMAG;
        return value;
    case VALUE_TYPE_BIGINT:
        value.type = VALUE_TYPE_BIGINT_IMAG;
        return value;
    case VALUE_TYPE_RATIONAL:
        value.type = VALUE_TYPE_RATIONAL_IMAG;
        return value;
    case VALUE_TYPE_IRRATIONAL:
        value.type = VALUE_TYPE_IRRATIONAL_IMAG;
        return value;
    default:
        cant_happen("invalid real type %s", valueTypeName(value.type));
    }
}

/////////////////////////////
// add operator
/////////////////////////////

static Value n_add_std(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);

    Integer sum;
    if (__builtin_add_overflow(getValue_Stdint(left), getValue_Stdint(right),
                               &sum)) {
        BigInt *big =
            bigIntFromAddition(getValue_Stdint(left), getValue_Stdint(right));
        int save = PROTECT(big);
        Value res = value_Bigint(big);
        UNPROTECT(save);
        return res;
    }

    return value_Stdint(sum);
}

static Value n_add_big(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_INT(right);
    ASSERT(IS_BIGINT(left) || IS_BIGINT(right));

    BigInt *sum;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            sum = addBigInt(getValue_Bigint(left), getValue_Bigint(right));
        } else {
            sum = addBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
        }
    } else {
        sum = addBigIntInt(getValue_Bigint(right), getValue_Stdint(left));
    }
    PROTECT(sum);
    Value res = value_Bigint(sum);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_add_rat(Value left, Value right) {
    if (!IS_RATIONAL(left) || !IS_RATIONAL(right)) {
        return ratFallbackBinary(left, right, n_add);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    unpackRationalPair(left, right, &a, &b, &c, &d);

    Value ad = n_mul(a, d);
    protectValue(ad);
    Value bc = n_mul(b, c);
    protectValue(bc);
    Value numerator = n_add(ad, bc);
    protectValue(numerator);
    Value denominator = n_mul(b, d);
    protectValue(denominator);
    Value res = n_div(numerator, denominator);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value n_add_irr(Value left, Value right) {
    Value leftIrr = toIrr(left);
    Value rightIrr = toIrr(right);
    return value_Irrational(getValue_Irrational(leftIrr) +
                            getValue_Irrational(rightIrr));
}

static Value n_add_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = imagToReal(left);
    Value rightReal = imagToReal(right);
    Value sum = n_add(leftReal, rightReal);
    int save = protectValue(sum);
    Value res = realToImag(sum);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_add_complex(Value left, Value right) {
    Value leftComplex = toComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = toComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    unpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

    Value real = n_add(a, c);
    protectValue(real);
    Value imag = n_add(b, d);
    protectValue(imag);
    Value res = comSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler addHandlers[] = {
    [ARITH_DOMAIN_INT_STD] = n_add_std, [ARITH_DOMAIN_INT_BIG] = n_add_big,
    [ARITH_DOMAIN_RAT] = n_add_rat,     [ARITH_DOMAIN_IRR] = n_add_irr,
    [ARITH_DOMAIN_IMAG] = n_add_imag,   [ARITH_DOMAIN_COMPLEX] = n_add_complex,
};

/////////////////////////////
// sub operator
/////////////////////////////

static Value n_sub_std(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);

    Integer difference;
    if (__builtin_sub_overflow(getValue_Stdint(left), getValue_Stdint(right),
                               &difference)) {
        BigInt *big = bigIntFromSubtraction(getValue_Stdint(left),
                                            getValue_Stdint(right));
        int save = PROTECT(big);
        Value res = value_Bigint(big);
        UNPROTECT(save);
        return res;
    }

    return value_Stdint(difference);
}

static Value n_sub_big(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_INT(right);
    ASSERT(IS_BIGINT(left) || IS_BIGINT(right));

    BigInt *difference;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            difference =
                subBigInt(getValue_Bigint(left), getValue_Bigint(right));
        } else {
            difference =
                subBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
        }
    } else {
        difference =
            subIntBigInt(getValue_Stdint(left), getValue_Bigint(right));
    }
    PROTECT(difference);
    Value res = value_Bigint(difference);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_sub_rat(Value left, Value right) {
    if (!IS_RATIONAL(left) || !IS_RATIONAL(right)) {
        return ratFallbackBinary(left, right, n_sub);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    unpackRationalPair(left, right, &a, &b, &c, &d);

    Value ad = n_mul(a, d);
    protectValue(ad);
    Value bc = n_mul(b, c);
    protectValue(bc);
    Value numerator = n_sub(ad, bc);
    protectValue(numerator);
    Value denominator = n_mul(b, d);
    protectValue(denominator);
    Value res = n_div(numerator, denominator);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value n_sub_irr(Value left, Value right) {
    Value leftIrr = toIrr(left);
    Value rightIrr = toIrr(right);
    return value_Irrational(getValue_Irrational(leftIrr) -
                            getValue_Irrational(rightIrr));
}

static Value n_sub_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = imagToReal(left);
    Value rightReal = imagToReal(right);
    Value difference = n_sub(leftReal, rightReal);
    int save = protectValue(difference);
    Value res = realToImag(difference);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_sub_complex(Value left, Value right) {
    Value leftComplex = toComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = toComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    unpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

    Value real = n_sub(a, c);
    protectValue(real);
    Value imag = n_sub(b, d);
    protectValue(imag);
    Value res = comSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler subHandlers[] = {
    [ARITH_DOMAIN_INT_STD] = n_sub_std, [ARITH_DOMAIN_INT_BIG] = n_sub_big,
    [ARITH_DOMAIN_RAT] = n_sub_rat,     [ARITH_DOMAIN_IRR] = n_sub_irr,
    [ARITH_DOMAIN_IMAG] = n_sub_imag,   [ARITH_DOMAIN_COMPLEX] = n_sub_complex,
};

/////////////////////////////
// mul operator
/////////////////////////////

static Value n_mul_std(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);

    Integer product;
    if (__builtin_mul_overflow(getValue_Stdint(left), getValue_Stdint(right),
                               &product)) {
        BigInt *big = bigIntFromMultiplication(getValue_Stdint(left),
                                               getValue_Stdint(right));
        int save = PROTECT(big);
        Value res = value_Bigint(big);
        UNPROTECT(save);
        return res;
    }

    return value_Stdint(product);
}

static Value n_mul_big(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_INT(right);
    ASSERT(IS_BIGINT(left) || IS_BIGINT(right));

    BigInt *product;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            product = mulBigInt(getValue_Bigint(left), getValue_Bigint(right));
        } else {
            product =
                mulBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
        }
    } else {
        product = mulBigIntInt(getValue_Bigint(right), getValue_Stdint(left));
    }
    PROTECT(product);
    Value res = value_Bigint(product);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_mul_rat(Value left, Value right) {
    if (!IS_RATIONAL(left) || !IS_RATIONAL(right)) {
        return ratFallbackBinary(left, right, n_mul);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    unpackRationalPair(left, right, &a, &b, &c, &d);

    Value numerator = n_mul(a, c);
    protectValue(numerator);
    Value denominator = n_mul(b, d);
    protectValue(denominator);
    Value res = n_div(numerator, denominator);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value n_mul_irr(Value left, Value right) {
    Value leftIrr = toIrr(left);
    Value rightIrr = toIrr(right);
    return value_Irrational(getValue_Irrational(leftIrr) *
                            getValue_Irrational(rightIrr));
}

static Value n_mul_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = imagToReal(left);
    Value rightReal = imagToReal(right);
    Value product = n_mul(leftReal, rightReal);
    int save = protectValue(product);
    Value negOne = value_Stdint(-1);
    Value res = n_mul(product, negOne);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_mul_complex(Value left, Value right) {
    Value leftComplex = toComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = toComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    unpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

    Value ac = n_mul(a, c);
    protectValue(ac);
    Value bd = n_mul(b, d);
    protectValue(bd);
    Value real = n_sub(ac, bd);
    protectValue(real);

    Value ad = n_mul(a, d);
    protectValue(ad);
    Value bc = n_mul(b, c);
    protectValue(bc);
    Value imag = n_add(ad, bc);
    protectValue(imag);

    Value res = comSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler mulHandlers[] = {
    [ARITH_DOMAIN_INT_STD] = n_mul_std, [ARITH_DOMAIN_INT_BIG] = n_mul_big,
    [ARITH_DOMAIN_RAT] = n_mul_rat,     [ARITH_DOMAIN_IRR] = n_mul_irr,
    [ARITH_DOMAIN_IMAG] = n_mul_imag,   [ARITH_DOMAIN_COMPLEX] = n_mul_complex,
};

/////////////////////////////
// div operator
/////////////////////////////

static Value n_div_std(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    if (getValue_Stdint(right) == 0) {
        return softNaN();
    }
    if ((getValue_Stdint(left) % getValue_Stdint(right)) == 0) {
        return value_Stdint(getValue_Stdint(left) / getValue_Stdint(right));
    }
    return ratValue(left, right);
}

static Value n_div_big(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_INT(right);
    ASSERT(IS_BIGINT(left) || IS_BIGINT(right));

    int save = PROTECT(NULL);
    BigInt *remainder;
    BigInt *quotient;
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            if (cmpBigIntInt(getValue_Bigint(right), 0) == CMP_EQ) {
                Value res = softNaN();
                protectValue(res);
                UNPROTECT(save);
                return res;
            }
            remainder =
                modBigInt(getValue_Bigint(left), getValue_Bigint(right));
            PROTECT(remainder);
            if (cmpBigIntInt(remainder, 0) != CMP_EQ) {
                Value res = ratValue(left, right);
                protectValue(res);
                UNPROTECT(save);
                return res;
            }
            quotient = divBigInt(getValue_Bigint(left), getValue_Bigint(right));
        } else {
            if (getValue_Stdint(right) == 0) {
                Value res = softNaN();
                protectValue(res);
                UNPROTECT(save);
                return res;
            }
            remainder =
                modBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
            PROTECT(remainder);
            if (cmpBigIntInt(remainder, 0) != CMP_EQ) {
                Value res = ratValue(left, right);
                protectValue(res);
                UNPROTECT(save);
                return res;
            }
            quotient =
                divBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
        }
    } else {
        if (cmpBigIntInt(getValue_Bigint(right), 0) == CMP_EQ) {
            Value res = softNaN();
            protectValue(res);
            UNPROTECT(save);
            return res;
        }
        remainder = modIntBigInt(getValue_Stdint(left), getValue_Bigint(right));
        PROTECT(remainder);
        if (cmpBigIntInt(remainder, 0) != CMP_EQ) {
            Value res = ratValue(left, right);
            protectValue(res);
            UNPROTECT(save);
            return res;
        }
        quotient = divIntBigInt(getValue_Stdint(left), getValue_Bigint(right));
    }

    PROTECT(quotient);
    Value res = value_Bigint(quotient);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_div_rat(Value left, Value right) {
    if (!IS_RATIONAL(left) || !IS_RATIONAL(right)) {
        return ratFallbackBinary(left, right, n_div);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    unpackRationalPair(left, right, &a, &b, &c, &d);

    Value numerator = n_mul(a, d);
    protectValue(numerator);
    Value denominator = n_mul(b, c);
    protectValue(denominator);
    Value res = n_div(numerator, denominator);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value n_div_irr(Value left, Value right) {
    Value leftIrr = toIrr(left);
    Value rightIrr = toIrr(right);
    if (getValue_Irrational(rightIrr) == 0.0) {
        return softNaN();
    }
    return value_Irrational(getValue_Irrational(leftIrr) /
                            getValue_Irrational(rightIrr));
}

static Value n_div_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = imagToReal(left);
    Value rightReal = imagToReal(right);
    return n_div(leftReal, rightReal);
}

static Value n_div_complex(Value left, Value right) {
    Value leftComplex = toComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = toComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    unpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

    Value ac = n_mul(a, c);
    protectValue(ac);
    Value bd = n_mul(b, d);
    protectValue(bd);
    Value bc = n_mul(b, c);
    protectValue(bc);
    Value ad = n_mul(a, d);
    protectValue(ad);
    Value cc = n_mul(c, c);
    protectValue(cc);
    Value dd = n_mul(d, d);
    protectValue(dd);

    Value realNum = n_add(ac, bd);
    protectValue(realNum);
    Value imagNum = n_sub(bc, ad);
    protectValue(imagNum);
    Value denom = n_add(cc, dd);
    protectValue(denom);

    Value real = n_div(realNum, denom);
    protectValue(real);
    Value imag = n_div(imagNum, denom);
    protectValue(imag);
    Value res = comSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler divHandlers[] = {
    [ARITH_DOMAIN_INT_STD] = n_div_std, [ARITH_DOMAIN_INT_BIG] = n_div_big,
    [ARITH_DOMAIN_RAT] = n_div_rat,     [ARITH_DOMAIN_IRR] = n_div_irr,
    [ARITH_DOMAIN_IMAG] = n_div_imag,   [ARITH_DOMAIN_COMPLEX] = n_div_complex,
};

/////////////////////////////
// mod operator
/////////////////////////////

static Value n_mod_std(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    if (getValue_Stdint(right) == 0) {
        return softNaN();
    }
    return value_Stdint(getValue_Stdint(left) % getValue_Stdint(right));
}

static Value n_mod_big(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_INT(right);
    ASSERT(IS_BIGINT(left) || IS_BIGINT(right));

    int save = PROTECT(NULL);
    BigInt *mod;
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            if (cmpBigIntInt(getValue_Bigint(right), 0) == CMP_EQ) {
                Value res = softNaN();
                protectValue(res);
                UNPROTECT(save);
                return res;
            }
            mod = modBigInt(getValue_Bigint(left), getValue_Bigint(right));
        } else {
            if (getValue_Stdint(right) == 0) {
                Value res = softNaN();
                protectValue(res);
                UNPROTECT(save);
                return res;
            }
            mod = modBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
        }
    } else {
        if (cmpBigIntInt(getValue_Bigint(right), 0) == CMP_EQ) {
            Value res = softNaN();
            protectValue(res);
            UNPROTECT(save);
            return res;
        }
        mod = modIntBigInt(getValue_Stdint(left), getValue_Bigint(right));
    }

    PROTECT(mod);
    Value res = value_Bigint(mod);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_mod_rat(Value left, Value right) {
    if (!IS_RATIONAL(left) || !IS_RATIONAL(right)) {
        return ratFallbackBinary(left, right, n_mod);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    unpackRationalPair(left, right, &a, &b, &c, &d);

    Value ad = n_mul(a, d);
    protectValue(ad);
    Value bc = n_mul(b, c);
    protectValue(bc);
    Value numerator = n_mod(ad, bc);
    protectValue(numerator);
    Value denominator = n_mul(b, d);
    protectValue(denominator);
    Value res = n_div(numerator, denominator);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value n_mod_irr(Value left, Value right) {
    Value leftIrr = toIrr(left);
    Value rightIrr = toIrr(right);
    if (getValue_Irrational(rightIrr) == 0.0) {
        return softNaN();
    }
    return value_Irrational(
        fmod(getValue_Irrational(leftIrr), getValue_Irrational(rightIrr)));
}

static Value n_mod_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = imagToReal(left);
    Value rightReal = imagToReal(right);
    Value imag = n_mod(leftReal, rightReal);
    int save = protectValue(imag);
    Value res = realToImag(imag);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_mod_complex(Value left, Value right) {
    Value leftComplex = toComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = toComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    unpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

    Value real = n_mod(a, c);
    protectValue(real);
    Value imag = n_mod(b, d);
    protectValue(imag);
    Value res = comSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler modHandlers[] = {
    [ARITH_DOMAIN_INT_STD] = n_mod_std, [ARITH_DOMAIN_INT_BIG] = n_mod_big,
    [ARITH_DOMAIN_RAT] = n_mod_rat,     [ARITH_DOMAIN_IRR] = n_mod_irr,
    [ARITH_DOMAIN_IMAG] = n_mod_imag,   [ARITH_DOMAIN_COMPLEX] = n_mod_complex,
};

/////////////////////////////
// cmp operator
/////////////////////////////

static Cmp n_cmp_std(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);

    return getValue_Stdint(left) < getValue_Stdint(right)    ? CMP_LT
           : getValue_Stdint(left) == getValue_Stdint(right) ? CMP_EQ
                                                             : CMP_GT;
}

static Cmp n_cmp_big(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_INT(right);
    ASSERT(IS_BIGINT(left) || IS_BIGINT(right));

    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            return cmpBigInt(getValue_Bigint(left), getValue_Bigint(right));
        }
        return cmpBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
    }

    return cmpIntBigInt(getValue_Stdint(left), getValue_Bigint(right));
}

static Cmp n_cmp_rat(Value left, Value right) {
    if (!IS_RATIONAL(left) || !IS_RATIONAL(right)) {
        return ratFallbackCompare(left, right, n_cmp);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    unpackRationalPair(left, right, &a, &b, &c, &d);

    Value ad = n_mul(a, d);
    protectValue(ad);
    Value bc = n_mul(b, c);
    protectValue(bc);
    Cmp res = n_cmp(ad, bc);

    UNPROTECT(save);
    return res;
}

static Cmp n_cmp_irr(Value left, Value right) {
    Value leftIrr = toIrr(left);
    Value rightIrr = toIrr(right);
    return getValue_Irrational(leftIrr) < getValue_Irrational(rightIrr) ? CMP_LT
           : getValue_Irrational(leftIrr) == getValue_Irrational(rightIrr)
               ? CMP_EQ
               : CMP_GT;
}

static Cmp n_cmp_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = imagToReal(left);
    Value rightReal = imagToReal(right);
    return n_cmp(leftReal, rightReal);
}

static Cmp n_cmp_complex(Value left, Value right) {
    Value leftComplex = toComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = toComplex(right);
    protectValue(rightComplex);

    Value leftReal, leftImag, rightReal, rightImag;
    unpackComplexPair(leftComplex, rightComplex, &leftReal, &leftImag,
                      &rightReal, &rightImag);

    Cmp realCmp = n_cmp(leftReal, rightReal);
    Cmp imagCmp = n_cmp(leftImag, rightImag);
    Cmp res = CMP_EQ;

    switch (realCmp) {
    case CMP_LT:
        if (imagCmp == CMP_GT) {
            res = magCmp(leftReal, leftImag, rightReal, rightImag);
        } else {
            res = CMP_LT;
        }
        break;
    case CMP_EQ:
        res = imagCmp;
        break;
    case CMP_GT:
        if (imagCmp == CMP_LT) {
            res = magCmp(leftReal, leftImag, rightReal, rightImag);
        } else {
            res = CMP_GT;
        }
        break;
    }

    UNPROTECT(save);
    return res;
}

static const NextCmpHandler cmpHandlers[] = {
    [ARITH_DOMAIN_INT_STD] = n_cmp_std, [ARITH_DOMAIN_INT_BIG] = n_cmp_big,
    [ARITH_DOMAIN_RAT] = n_cmp_rat,     [ARITH_DOMAIN_IRR] = n_cmp_irr,
    [ARITH_DOMAIN_IMAG] = n_cmp_imag,   [ARITH_DOMAIN_COMPLEX] = n_cmp_complex,
};

/////////////////////////////
// pow operator
/////////////////////////////

static bool intIsZero(Value value) {
    ASSERT_INT(value);
    if (IS_BIGINT(value)) {
        return cmpBigIntInt(getValue_Bigint(value), 0) == CMP_EQ;
    }
    return getValue_Stdint(value) == 0;
}

static bool intIsNegative(Value value) {
    ASSERT_INT(value);
    if (IS_BIGINT(value)) {
        return cmpBigIntInt(getValue_Bigint(value), 0) == CMP_LT;
    }
    return getValue_Stdint(value) < 0;
}

static bool intIsOdd(Value value) {
    ASSERT_INT(value);
    if (IS_BIGINT(value)) {
        return !isEvenBigInt(getValue_Bigint(value));
    }
    return (getValue_Stdint(value) & 1) != 0;
}

static bool intIsEven(Value value) { return !intIsOdd(value); }

static bool ratIsNegative(Value value) {
    ASSERT_RATIONAL(value);
    Vec *ratio = getValue_Rational(value);
    Value numerator = ratio->entries[0];
    Value denominator = ratio->entries[1];

    if (intIsZero(numerator)) {
        return false;
    }

    return intIsNegative(numerator) != intIsNegative(denominator);
}

static Value ratNumerator(Value value) {
    ASSERT_RATIONAL(value);
    return getValue_Rational(value)->entries[0];
}

static Value ratDenominator(Value value) {
    ASSERT_RATIONAL(value);
    return getValue_Rational(value)->entries[1];
}

static bool realIsNegative(Value value) {
    ASSERT_REAL(value);
    switch (value.type) {
    case VALUE_TYPE_STDINT:
    case VALUE_TYPE_BIGINT:
        return intIsNegative(value);
    case VALUE_TYPE_RATIONAL:
        return ratIsNegative(value);
    case VALUE_TYPE_IRRATIONAL:
        return getValue_Irrational(value) < 0.0;
    default:
        cant_happen("invalid real type %s", valueTypeName(value.type));
    }
}

static Value intAbs(Value value) {
    ASSERT_INT(value);
    if (!intIsNegative(value)) {
        return value;
    }

    if (IS_BIGINT(value)) {
        BigInt *copy = copyBigInt(getValue_Bigint(value));
        int save = PROTECT(copy);
        negateBigInt(copy);
        Value res = value_Bigint(copy);
        UNPROTECT(save);
        return res;
    }

    Integer std = getValue_Stdint(value);
    Integer abs;
    if (__builtin_sub_overflow(0, std, &abs)) {
        BigInt *copy = bigIntFromInt(std);
        int save = PROTECT(copy);
        negateBigInt(copy);
        Value res = value_Bigint(copy);
        UNPROTECT(save);
        return res;
    }

    return value_Stdint(abs);
}

static BigInt *powAbsExponent(Value exponent, bool *isNegative) {
    ASSERT_INT(exponent);
    if (IS_BIGINT(exponent)) {
        BigInt *copy = copyBigInt(getValue_Bigint(exponent));
        *isNegative = isNegBigInt(copy);
        if (*isNegative) {
            negateBigInt(copy);
        }
        return copy;
    }

    Integer e = getValue_Stdint(exponent);
    BigInt *copy = bigIntFromInt(e);
    *isNegative = e < 0;
    if (*isNegative) {
        negateBigInt(copy);
    }
    return copy;
}

static Value powIntInt(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_INT(right);

    if (intIsZero(right)) {
        return value_Stdint(1);
    }

    int save = PROTECT(NULL);
    bool isNegative = false;
    BigInt *absExponent = powAbsExponent(right, &isNegative);
    PROTECT(absExponent);

    BigInt *powBig;
    if (IS_BIGINT(left)) {
        powBig = powBigInt(getValue_Bigint(left), absExponent);
    } else {
        powBig = powIntBigInt(getValue_Stdint(left), absExponent);
    }
    PROTECT(powBig);

    Value powValue = value_Bigint(powBig);
    protectValue(powValue);
    if (!isNegative) {
        UNPROTECT(save);
        return powValue;
    }

    Value res = ratValue(value_Stdint(1), powValue);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value powIrrIrr(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return value_Irrational(
        pow(getValue_Irrational(left), getValue_Irrational(right)));
}

static Value powRealRat(Value base, Value exponent) {
    ASSERT_REAL(base);
    ASSERT_RATIONAL(exponent);

    int save = PROTECT(NULL);
    if (realIsNegative(base)) {
        Value posBase = n_mul(base, value_Stdint(-1));
        protectValue(posBase);
        Value negPow = powRealRat(posBase, exponent);
        protectValue(negPow);
        Value denominator = ratDenominator(exponent);

        Value res;
        if (intIsEven(denominator)) {
            res = n_mul(negPow, value_Stdint_imag(1));
        } else {
            res = n_mul(negPow, value_Stdint(-1));
        }
        protectValue(res);
        UNPROTECT(save);
        return res;
    }

    if (ratIsNegative(exponent)) {
        Value posExponent = n_mul(exponent, value_Stdint(-1));
        protectValue(posExponent);
        Value inv = powRealRat(base, posExponent);
        protectValue(inv);
        Value res = n_div(value_Stdint(1), inv);
        protectValue(res);
        UNPROTECT(save);
        return res;
    }

    if (IS_RATIONAL(base)) {
        Value partNumerator = ratNumerator(base);
        Value partDenominator = ratDenominator(base);

        if (intIsNegative(partDenominator)) {
            partNumerator = n_mul(partNumerator, value_Stdint(-1));
            protectValue(partNumerator);
            partDenominator = n_mul(partDenominator, value_Stdint(-1));
            protectValue(partDenominator);
        }

        Value num = powRealRat(partNumerator, exponent);
        protectValue(num);
        Value den = powRealRat(partDenominator, exponent);
        protectValue(den);

        Value res;
        if (IS_INT(num) && IS_INT(den)) {
            res = n_div(num, den);
        } else {
            Value irrNum = toIrr(num);
            protectValue(irrNum);
            Value irrDen = toIrr(den);
            protectValue(irrDen);
            res = value_Irrational(getValue_Irrational(irrNum) /
                                   getValue_Irrational(irrDen));
        }
        protectValue(res);
        UNPROTECT(save);
        return res;
    }

    Value fbase = toIrr(base);
    protectValue(fbase);
    Value fexponent = toIrr(exponent);
    protectValue(fexponent);
    Value res = value_Irrational(
        pow(getValue_Irrational(fbase), getValue_Irrational(fexponent)));
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value powRatInt(Value left, Value right) {
    ASSERT_RATIONAL(left);
    ASSERT_INT(right);

    Vec *leftRat = getValue_Rational(left);
    Value numerator = leftRat->entries[0];
    Value denominator = leftRat->entries[1];

    Value absExponent = intAbs(right);
    int save = protectValue(absExponent);

    Value numPow = powIntInt(numerator, absExponent);
    protectValue(numPow);
    Value denPow = powIntInt(denominator, absExponent);
    protectValue(denPow);
    Value res = n_div(numPow, denPow);
    protectValue(res);

    if (intIsNegative(right)) {
        Value inv = n_div(value_Stdint(1), res);
        protectValue(inv);
        UNPROTECT(save);
        return inv;
    }

    UNPROTECT(save);
    return res;
}

static Value powIrrComplex(Value c, Value right) {
    ASSERT_IRRATIONAL(c);
    ASSERT(IS_COMPLEX_LIKE_TYPE(right.type));

    Value rightComplex = toComplex(right);
    int save = protectValue(rightComplex);

    Value a = toIrr(complexRealPart(rightComplex));
    protectValue(a);
    Value b = toIrr(complexImagAsReal(rightComplex));
    protectValue(b);

    Value cPowA = n_pow(c, a);
    protectValue(cPowA);
    Double lnC = log(getValue_Irrational(c));
    Double bLnC = getValue_Irrational(b) * lnC;

    Value cosTerm = value_Irrational(cos(bLnC));
    protectValue(cosTerm);
    Value iSinTerm = value_Irrational_imag(sin(bLnC));
    protectValue(iSinTerm);
    Value com = comValue(cosTerm, iSinTerm);
    protectValue(com);
    Value res = n_mul(cPowA, com);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value comMag(Value value) {
    Value complex = toComplex(value);
    int save = protectValue(complex);

    Value a = complexRealPart(complex);
    Value b = complexImagAsReal(complex);
    Value aa = n_pow(a, value_Stdint(2));
    protectValue(aa);
    Value bb = n_pow(b, value_Stdint(2));
    protectValue(bb);
    Value sum = n_add(aa, bb);
    protectValue(sum);
    Value res = n_pow(sum, value_Irrational(0.5));
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value comTheta(Value value) {
    Value complex = toComplex(value);
    Value a = toIrr(complexRealPart(complex));
    Value b = toIrr(complexImagAsReal(complex));
    return value_Irrational(
        atan2(getValue_Irrational(b), getValue_Irrational(a)));
}

static Value comRoot(Value value, Value n) {
    ASSERT_INT(n);

    Value complex = toComplex(value);
    int save = protectValue(complex);
    Value r = comMag(complex);
    protectValue(r);
    Value theta = comTheta(complex);
    protectValue(theta);
    Value invN = ratValue(value_Stdint(1), n);
    protectValue(invN);
    Value rN = n_pow(r, invN);
    protectValue(rN);
    Value thetaN = n_div(theta, n);
    protectValue(thetaN);
    Value irrThetaN = toIrr(thetaN);
    protectValue(irrThetaN);
    Value cosTerm = value_Irrational(cos(getValue_Irrational(irrThetaN)));
    protectValue(cosTerm);
    Value iSinTerm = value_Irrational_imag(sin(getValue_Irrational(irrThetaN)));
    protectValue(iSinTerm);
    Value base = comValue(cosTerm, iSinTerm);
    protectValue(base);
    Value res = n_mul(rN, base);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value powComplexRat(Value left, Value right) {
    ASSERT(IS_COMPLEX_LIKE_TYPE(left.type));
    ASSERT_RATIONAL(right);

    Value root = comRoot(left, ratDenominator(right));
    int save = protectValue(root);
    Value res = powComplexInt(root, ratNumerator(right));
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value powIrrFromReal(Value left, Value right) {
    ASSERT_REAL(left);
    ASSERT_IRRATIONAL(right);
    return powIrrIrr(toIrr(left), right);
}

static Value powIrrComplexFromReal(Value left, Value right) {
    ASSERT_REAL(left);
    ASSERT(IS_COMPLEX_LIKE_TYPE(right.type));
    return powIrrComplex(toIrr(left), right);
}

static Value powRatIrr(Value left, Value right) {
    ASSERT_RATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return powIrrFromReal(left, right);
}

static Value powRatComplex(Value left, Value right) {
    ASSERT_RATIONAL(left);
    ASSERT(IS_COMPLEX_LIKE_TYPE(right.type));
    return powIrrComplexFromReal(left, right);
}

static Value powIrrInt(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_INT(right);
    return powIrrIrr(left, toIrr(right));
}

static Value powIntRat(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_RATIONAL(right);
    return powRealRat(left, right);
}

static Value powIntIrr(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT_IRRATIONAL(right);
    return powIrrFromReal(left, right);
}

static Value powIntComplex(Value left, Value right) {
    ASSERT_INT(left);
    ASSERT(IS_COMPLEX_LIKE_TYPE(right.type));
    return powIrrComplexFromReal(left, right);
}

static Value powImagReal(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_RATIONAL(right) || IS_IRRATIONAL(right));

    Value real = imagToReal(left);
    int save = protectValue(real);
    Value powRes = n_pow(real, right);
    protectValue(powRes);
    Value res = n_mul(powRes, value_Stdint_imag(1));
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value powComplexIrr(Value left, Value right) {
    ASSERT(left.type == VALUE_TYPE_COMPLEX);
    ASSERT_IRRATIONAL(right);

    Value rightComplex = toComplex(right);
    int save = protectValue(rightComplex);
    Value res = powComplexComplex(left, rightComplex);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value powComplexComplex(Value base, Value exponent) {
    ASSERT(IS_COMPLEX_LIKE_TYPE(base.type));
    ASSERT(IS_COMPLEX_LIKE_TYPE(exponent.type));

    Value baseComplex = toComplex(base);
    int save = protectValue(baseComplex);
    Value exponentComplex = toComplex(exponent);
    protectValue(exponentComplex);

    Value r = comMag(baseComplex);
    protectValue(r);
    Value theta = comTheta(baseComplex);
    protectValue(theta);
    Value irrR = toIrr(r);
    protectValue(irrR);
    Value lnR = value_Irrational(log(getValue_Irrational(irrR)));
    protectValue(lnR);
    Value lnRPlusITheta = comValue(lnR, realToImag(theta));
    protectValue(lnRPlusITheta);
    Value prod = n_mul(lnRPlusITheta, exponentComplex);
    protectValue(prod);
    Value res = n_pow(value_Irrational(exp(1.0)), prod);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static Value powComplexInt(Value left, Value right) {
    ASSERT(IS_COMPLEX_LIKE_TYPE(left.type));
    ASSERT_INT(right);

    if (intIsZero(right)) {
        return value_Stdint(1);
    }

    Value leftComplex = toComplex(left);
    int save = protectValue(leftComplex);

    Value exponent = intAbs(right);
    protectValue(exponent);

    Value res = value_Stdint(1);
    protectValue(res);

    Value factor = leftComplex;
    protectValue(factor);

    while (n_cmp(exponent, value_Stdint(0)) == CMP_GT) {
        if (intIsOdd(exponent)) {
            Value nextRes = n_mul(res, factor);
            protectValue(nextRes);
            res = nextRes;

            Value evenExponent = n_sub(exponent, value_Stdint(1));
            protectValue(evenExponent);
            exponent = evenExponent;
        }

        Value halfExponent = n_div(exponent, value_Stdint(2));
        protectValue(halfExponent);
        ASSERT_INT(halfExponent);
        exponent = halfExponent;

        if (n_cmp(exponent, value_Stdint(0)) == CMP_GT) {
            Value nextFactor = n_mul(factor, factor);
            protectValue(nextFactor);
            factor = nextFactor;
        }
    }

    if (intIsNegative(right)) {
        Value inv = n_div(value_Stdint(1), res);
        protectValue(inv);
        UNPROTECT(save);
        return inv;
    }

    UNPROTECT(save);
    return res;
}

#define NEXT_POW_DOMAIN_COUNT (ARITH_DOMAIN_COMPLEX + 1)

static const NextBinaryHandler powHandlers[][NEXT_POW_DOMAIN_COUNT] = {
    [ARITH_DOMAIN_INT_STD] =
        {
            [ARITH_DOMAIN_INT_STD] = powIntInt,
            [ARITH_DOMAIN_INT_BIG] = powIntInt,
            [ARITH_DOMAIN_RAT] = powIntRat,
            [ARITH_DOMAIN_IRR] = powIntIrr,
            [ARITH_DOMAIN_IMAG] = powIntComplex,
            [ARITH_DOMAIN_COMPLEX] = powIntComplex,
        },
    [ARITH_DOMAIN_INT_BIG] =
        {
            [ARITH_DOMAIN_INT_STD] = powIntInt,
            [ARITH_DOMAIN_INT_BIG] = powIntInt,
            [ARITH_DOMAIN_RAT] = powIntRat,
            [ARITH_DOMAIN_IRR] = powIntIrr,
            [ARITH_DOMAIN_IMAG] = powIntComplex,
            [ARITH_DOMAIN_COMPLEX] = powIntComplex,
        },
    [ARITH_DOMAIN_RAT] =
        {
            [ARITH_DOMAIN_INT_STD] = powRatInt,
            [ARITH_DOMAIN_INT_BIG] = powRatInt,
            [ARITH_DOMAIN_RAT] = powRealRat,
            [ARITH_DOMAIN_IRR] = powRatIrr,
            [ARITH_DOMAIN_IMAG] = powRatComplex,
            [ARITH_DOMAIN_COMPLEX] = powRatComplex,
        },
    [ARITH_DOMAIN_IRR] =
        {
            [ARITH_DOMAIN_INT_STD] = powIrrInt,
            [ARITH_DOMAIN_INT_BIG] = powIrrInt,
            [ARITH_DOMAIN_RAT] = powRealRat,
            [ARITH_DOMAIN_IRR] = powIrrIrr,
            [ARITH_DOMAIN_IMAG] = powIrrComplex,
            [ARITH_DOMAIN_COMPLEX] = powIrrComplex,
        },
    [ARITH_DOMAIN_IMAG] =
        {
            [ARITH_DOMAIN_INT_STD] = powComplexInt,
            [ARITH_DOMAIN_INT_BIG] = powComplexInt,
            [ARITH_DOMAIN_RAT] = powImagReal,
            [ARITH_DOMAIN_IRR] = powImagReal,
            [ARITH_DOMAIN_IMAG] = powComplexComplex,
            [ARITH_DOMAIN_COMPLEX] = powComplexComplex,
        },
    [ARITH_DOMAIN_COMPLEX] =
        {
            [ARITH_DOMAIN_INT_STD] = powComplexInt,
            [ARITH_DOMAIN_INT_BIG] = powComplexInt,
            [ARITH_DOMAIN_RAT] = powComplexRat,
            [ARITH_DOMAIN_IRR] = powComplexIrr,
            [ARITH_DOMAIN_IMAG] = powComplexComplex,
            [ARITH_DOMAIN_COMPLEX] = powComplexComplex,
        },
};

static NextBinaryHandler getPowDomainHandler(ArithmeticDomain leftDomain,
                                             ArithmeticDomain rightDomain) {
    if (leftDomain <= ARITH_DOMAIN_NONE || leftDomain > ARITH_DOMAIN_COMPLEX ||
        rightDomain <= ARITH_DOMAIN_NONE ||
        rightDomain > ARITH_DOMAIN_COMPLEX) {
        cant_happen("invalid pow domain pair (%d, %d)", leftDomain,
                    rightDomain);
    }

    NextBinaryHandler handler = powHandlers[leftDomain][rightDomain];
    if (handler == NULL) {
        cant_happen("missing pow handler for domain pair (%d, %d)", leftDomain,
                    rightDomain);
    }
    return handler;
}

/////////////////////////////
// public staged API
/////////////////////////////

Value nadd(Value left, Value right) { return n_add(left, right); }

Value nsub(Value left, Value right) { return n_sub(left, right); }

Value nmul(Value left, Value right) { return n_mul(left, right); }

Value ndiv(Value left, Value right) { return n_div(left, right); }

Value npow(Value left, Value right) { return n_pow(left, right); }

Value nmod(Value left, Value right) { return n_mod(left, right); }

Cmp ncmp(Value left, Value right) { return n_cmp(left, right); }

Value nneg(Value value) { return n_neg(value); }

Value nrand(Value prev) { return n_rand(prev); }

Value real_part(Value value) { return n_real_part(value); }

Value imag_part(Value value) { return n_imag_part(value); }

Value mag_part(Value value) { return n_mag_part(value); }

Value theta_part(Value value) { return n_theta_part(value); }

static Value n_add(Value left, Value right) {
    assertCanonicalNaN(left);
    assertCanonicalNaN(right);
    if (containsNaN(left) || containsNaN(right)) {
        return softNaN();
    }
    return applyCommonDomainBinary(ARITH_OP_ADD, "add", addHandlers,
                                   sizeof(addHandlers) / sizeof(addHandlers[0]),
                                   left, right);
}

static Value n_sub(Value left, Value right) {
    assertCanonicalNaN(left);
    assertCanonicalNaN(right);
    if (containsNaN(left) || containsNaN(right)) {
        return softNaN();
    }
    return applyCommonDomainBinary(ARITH_OP_SUB, "sub", subHandlers,
                                   sizeof(subHandlers) / sizeof(subHandlers[0]),
                                   left, right);
}

static Value n_mul(Value left, Value right) {
    assertCanonicalNaN(left);
    assertCanonicalNaN(right);
    if (containsNaN(left) || containsNaN(right)) {
        return softNaN();
    }
    return applyCommonDomainBinary(ARITH_OP_MUL, "mul", mulHandlers,
                                   sizeof(mulHandlers) / sizeof(mulHandlers[0]),
                                   left, right);
}

static Value n_div(Value left, Value right) {
    assertCanonicalNaN(left);
    assertCanonicalNaN(right);
    if (containsNaN(left) || containsNaN(right)) {
        return softNaN();
    }
    return applyCommonDomainBinary(ARITH_OP_DIV, "div", divHandlers,
                                   sizeof(divHandlers) / sizeof(divHandlers[0]),
                                   left, right);
}

static Value n_mod(Value left, Value right) {
    assertCanonicalNaN(left);
    assertCanonicalNaN(right);
    if (containsNaN(left) || containsNaN(right)) {
        return softNaN();
    }
    return applyCommonDomainBinary(ARITH_OP_MOD, "mod", modHandlers,
                                   sizeof(modHandlers) / sizeof(modHandlers[0]),
                                   left, right);
}

static Value n_pow(Value left, Value right) {
    assertCanonicalNaN(left);
    assertCanonicalNaN(right);
    if (containsNaN(left) || containsNaN(right)) {
        return softNaN();
    }
    ArithmeticNormalizationPlan plan = requirePlan(ARITH_OP_POW, left, right);
    if (plan.kind != ARITH_NORM_ASYMMETRIC) {
        cant_happen("unexpected pow normalization kind %d", plan.kind);
    }

    NextBinaryHandler handler =
        getPowDomainHandler(plan.leftDomain, plan.rightDomain);
    return handler(left, right);
}

static Cmp n_cmp(Value left, Value right) {
    assertCanonicalNaN(left);
    assertCanonicalNaN(right);
    bool leftNaN = containsNaN(left);
    bool rightNaN = containsNaN(right);
    if (leftNaN || rightNaN) {
        if (leftNaN && rightNaN) {
            return CMP_EQ;
        }
        return leftNaN ? CMP_GT : CMP_LT;
    }
    return applyCommonDomainCompare(
        ARITH_OP_CMP, "cmp", cmpHandlers,
        sizeof(cmpHandlers) / sizeof(cmpHandlers[0]), left, right);
}

static Value n_neg(Value value) {
    assertCanonicalNaN(value);
    if (containsNaN(value)) {
        return softNaN();
    }
    return n_sub(value_Stdint(0), value);
}

static Value n_rand(Value prev) {
    ASSERT_IRRATIONAL(prev);
    Double seed = fmod(prev.val.irrational, 1.0);
    if (seed < 0) {
        seed = -seed;
    }
    seed *= UINT_MAX;
    seed = fmod(seed * 1103515245.0 + 12345.0, (Double)UINT_MAX);
    seed /= UINT_MAX;
    return value_Irrational(seed);
}

static Value n_real_part(Value value) {
    assertCanonicalNaN(value);
    if (containsNaN(value)) {
        return softNaN();
    }
    switch (value.type) {
    case VALUE_TYPE_RATIONAL:
    case VALUE_TYPE_IRRATIONAL:
    case VALUE_TYPE_BIGINT:
    case VALUE_TYPE_STDINT:
        return value;
    case VALUE_TYPE_STDINT_IMAG:
    case VALUE_TYPE_BIGINT_IMAG:
    case VALUE_TYPE_RATIONAL_IMAG:
    case VALUE_TYPE_IRRATIONAL_IMAG:
        return value_Stdint(0);
    case VALUE_TYPE_COMPLEX:
        return complexRealPart(value);
    default:
        cant_happen("unrecognised number type %s", valueTypeName(value.type));
    }
}

static Value n_imag_part(Value value) {
    assertCanonicalNaN(value);
    if (containsNaN(value)) {
        return softNaN();
    }
    switch (value.type) {
    case VALUE_TYPE_RATIONAL:
    case VALUE_TYPE_IRRATIONAL:
    case VALUE_TYPE_BIGINT:
    case VALUE_TYPE_STDINT:
        return value_Stdint_imag(0);
    case VALUE_TYPE_STDINT_IMAG:
    case VALUE_TYPE_BIGINT_IMAG:
    case VALUE_TYPE_RATIONAL_IMAG:
    case VALUE_TYPE_IRRATIONAL_IMAG:
        return value;
    case VALUE_TYPE_COMPLEX:
        return complexImagPart(value);
    default:
        cant_happen("unrecognised number type %s", valueTypeName(value.type));
    }
}

static Value n_theta_part(Value value) {
    assertCanonicalNaN(value);
    if (containsNaN(value)) {
        return softNaN();
    }
    return comTheta(value);
}

static Value n_mag_part(Value value) {
    assertCanonicalNaN(value);
    if (containsNaN(value)) {
        return softNaN();
    }
    return comMag(value);
}
