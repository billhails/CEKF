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
#include <math.h>

typedef Value (*NextBinaryHandler)(Value left, Value right);
typedef Cmp (*NextCmpHandler)(Value left, Value right);

static Value nextImagToReal(Value value);
static Value nextRealToImag(Value value);

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

static Value nextRatValue(Value numerator, Value denominator) {
    Vec *vec = newVec(2);
    vec->entries[0] = numerator;
    vec->entries[1] = denominator;
    return value_Rational(vec);
}

static Value nextToRat(Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT:
    case VALUE_TYPE_BIGINT:
        return nextRatValue(value, value_Stdint(1));
    case VALUE_TYPE_RATIONAL:
        return value;
    default:
        cant_happen("invalid rational-compatible type %s",
                    valueTypeName(value.type));
    }
}

static void nextUnpackRationalPair(Value left, Value right, Value *a, Value *b,
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

static Value nextRatFallbackBinary(Value left, Value right,
                                   NextBinaryHandler op) {
    Value leftRat = nextToRat(left);
    int save = protectValue(leftRat);
    Value rightRat = nextToRat(right);
    protectValue(rightRat);
    Value res = op(leftRat, rightRat);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Cmp nextRatFallbackCompare(Value left, Value right, NextCmpHandler op) {
    Value leftRat = nextToRat(left);
    int save = protectValue(leftRat);
    Value rightRat = nextToRat(right);
    protectValue(rightRat);
    Cmp res = op(leftRat, rightRat);
    UNPROTECT(save);
    return res;
}

static Value nextToIrr(Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT:
        return value_Irrational(getValue_Stdint(value));
    case VALUE_TYPE_BIGINT:
        return value_Irrational(bigIntToDouble(getValue_Bigint(value)));
    case VALUE_TYPE_RATIONAL: {
        Vec *rational = getValue_Rational(value);
        Value numerator = rational->entries[0];
        Value denominator = rational->entries[1];
        Value irrNum = nextToIrr(numerator);
        Value irrDen = nextToIrr(denominator);
        return value_Irrational(getValue_Irrational(irrNum) /
                                getValue_Irrational(irrDen));
    }
    case VALUE_TYPE_IRRATIONAL:
        return value;
    default:
        cant_happen("invalid real type %s", valueTypeName(value.type));
    }
}

static Value nextComValue(Value real, Value imag) {
    Vec *vec = newVec(2);
    vec->entries[0] = real;
    vec->entries[1] = imag;
    return value_Complex(vec);
}

static Value nextComplexRealPart(Value value) {
    ASSERT(value.type == VALUE_TYPE_COMPLEX);
    return getValue_Complex(value)->entries[0];
}

static Value nextComplexImagPart(Value value) {
    ASSERT(value.type == VALUE_TYPE_COMPLEX);
    return getValue_Complex(value)->entries[1];
}

static Value nextComplexImagAsReal(Value value) {
    return nextImagToReal(nextComplexImagPart(value));
}

static void nextUnpackComplexPair(Value left, Value right, Value *a, Value *b,
                                  Value *c, Value *d) {
    ASSERT(left.type == VALUE_TYPE_COMPLEX);
    ASSERT(right.type == VALUE_TYPE_COMPLEX);

    *a = nextComplexRealPart(left);
    *b = nextComplexImagAsReal(left);
    *c = nextComplexRealPart(right);
    *d = nextComplexImagAsReal(right);
}

static Value nextToComplex(Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT:
    case VALUE_TYPE_BIGINT:
    case VALUE_TYPE_RATIONAL:
    case VALUE_TYPE_IRRATIONAL:
        return nextComValue(value, value_Stdint_imag(0));
    case VALUE_TYPE_STDINT_IMAG:
    case VALUE_TYPE_BIGINT_IMAG:
    case VALUE_TYPE_RATIONAL_IMAG:
    case VALUE_TYPE_IRRATIONAL_IMAG:
        return nextComValue(value_Stdint(0), value);
    case VALUE_TYPE_COMPLEX:
        return value;
    default:
        cant_happen("invalid numeric type %s", valueTypeName(value.type));
    }
}

static Cmp nextMagCmp(Value leftReal, Value leftImag, Value rightReal,
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

static Value nextComSimplify(Value real, Value imagReal) {
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
        res = nextRealToImag(imagReal);
        protectValue(res);
        UNPROTECT(save);
        return res;
    }

    Value imag = nextRealToImag(imagReal);
    protectValue(imag);
    res = nextComValue(real, imag);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value nextImagToReal(Value value) {
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

static Value nextRealToImag(Value value) {
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
        return nextRatFallbackBinary(left, right, n_add);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    nextUnpackRationalPair(left, right, &a, &b, &c, &d);

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
    Value leftIrr = nextToIrr(left);
    Value rightIrr = nextToIrr(right);
    return value_Irrational(getValue_Irrational(leftIrr) +
                            getValue_Irrational(rightIrr));
}

static Value n_add_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = nextImagToReal(left);
    Value rightReal = nextImagToReal(right);
    Value sum = n_add(leftReal, rightReal);
    int save = protectValue(sum);
    Value res = nextRealToImag(sum);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_add_complex(Value left, Value right) {
    Value leftComplex = nextToComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = nextToComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    nextUnpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

    Value real = n_add(a, c);
    protectValue(real);
    Value imag = n_add(b, d);
    protectValue(imag);
    Value res = nextComSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler nextAddHandlers[] = {
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
        return nextRatFallbackBinary(left, right, n_sub);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    nextUnpackRationalPair(left, right, &a, &b, &c, &d);

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
    Value leftIrr = nextToIrr(left);
    Value rightIrr = nextToIrr(right);
    return value_Irrational(getValue_Irrational(leftIrr) -
                            getValue_Irrational(rightIrr));
}

static Value n_sub_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = nextImagToReal(left);
    Value rightReal = nextImagToReal(right);
    Value difference = n_sub(leftReal, rightReal);
    int save = protectValue(difference);
    Value res = nextRealToImag(difference);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_sub_complex(Value left, Value right) {
    Value leftComplex = nextToComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = nextToComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    nextUnpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

    Value real = n_sub(a, c);
    protectValue(real);
    Value imag = n_sub(b, d);
    protectValue(imag);
    Value res = nextComSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler nextSubHandlers[] = {
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
        return nextRatFallbackBinary(left, right, n_mul);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    nextUnpackRationalPair(left, right, &a, &b, &c, &d);

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
    Value leftIrr = nextToIrr(left);
    Value rightIrr = nextToIrr(right);
    return value_Irrational(getValue_Irrational(leftIrr) *
                            getValue_Irrational(rightIrr));
}

static Value n_mul_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = nextImagToReal(left);
    Value rightReal = nextImagToReal(right);
    Value product = n_mul(leftReal, rightReal);
    int save = protectValue(product);
    Value negOne = value_Stdint(-1);
    Value res = n_mul(product, negOne);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_mul_complex(Value left, Value right) {
    Value leftComplex = nextToComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = nextToComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    nextUnpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

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

    Value res = nextComSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler nextMulHandlers[] = {
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
        cant_happen("attempted div zero");
    }
    if ((getValue_Stdint(left) % getValue_Stdint(right)) == 0) {
        return value_Stdint(getValue_Stdint(left) / getValue_Stdint(right));
    }
    return nextRatValue(left, right);
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
                cant_happen("attempted div zero");
            }
            remainder =
                modBigInt(getValue_Bigint(left), getValue_Bigint(right));
            PROTECT(remainder);
            if (cmpBigIntInt(remainder, 0) != CMP_EQ) {
                Value res = nextRatValue(left, right);
                protectValue(res);
                UNPROTECT(save);
                return res;
            }
            quotient = divBigInt(getValue_Bigint(left), getValue_Bigint(right));
        } else {
            if (getValue_Stdint(right) == 0) {
                cant_happen("attempted div zero");
            }
            remainder =
                modBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
            PROTECT(remainder);
            if (cmpBigIntInt(remainder, 0) != CMP_EQ) {
                Value res = nextRatValue(left, right);
                protectValue(res);
                UNPROTECT(save);
                return res;
            }
            quotient =
                divBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
        }
    } else {
        if (cmpBigIntInt(getValue_Bigint(right), 0) == CMP_EQ) {
            cant_happen("attempted div zero");
        }
        remainder = modIntBigInt(getValue_Stdint(left), getValue_Bigint(right));
        PROTECT(remainder);
        if (cmpBigIntInt(remainder, 0) != CMP_EQ) {
            Value res = nextRatValue(left, right);
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
        return nextRatFallbackBinary(left, right, n_div);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    nextUnpackRationalPair(left, right, &a, &b, &c, &d);

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
    Value leftIrr = nextToIrr(left);
    Value rightIrr = nextToIrr(right);
    return value_Irrational(getValue_Irrational(leftIrr) /
                            getValue_Irrational(rightIrr));
}

static Value n_div_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = nextImagToReal(left);
    Value rightReal = nextImagToReal(right);
    return n_div(leftReal, rightReal);
}

static Value n_div_complex(Value left, Value right) {
    Value leftComplex = nextToComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = nextToComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    nextUnpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

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
    Value res = nextComSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler nextDivHandlers[] = {
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
        cant_happen("attempted mod zero");
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
                cant_happen("attempted mod zero");
            }
            mod = modBigInt(getValue_Bigint(left), getValue_Bigint(right));
        } else {
            if (getValue_Stdint(right) == 0) {
                cant_happen("attempted mod zero");
            }
            mod = modBigIntInt(getValue_Bigint(left), getValue_Stdint(right));
        }
    } else {
        if (cmpBigIntInt(getValue_Bigint(right), 0) == CMP_EQ) {
            cant_happen("attempted mod zero");
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
        return nextRatFallbackBinary(left, right, n_mod);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    nextUnpackRationalPair(left, right, &a, &b, &c, &d);

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
    Value leftIrr = nextToIrr(left);
    Value rightIrr = nextToIrr(right);
    return value_Irrational(
        fmod(getValue_Irrational(leftIrr), getValue_Irrational(rightIrr)));
}

static Value n_mod_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = nextImagToReal(left);
    Value rightReal = nextImagToReal(right);
    Value imag = n_mod(leftReal, rightReal);
    int save = protectValue(imag);
    Value res = nextRealToImag(imag);
    protectValue(res);
    UNPROTECT(save);
    return res;
}

static Value n_mod_complex(Value left, Value right) {
    Value leftComplex = nextToComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = nextToComplex(right);
    protectValue(rightComplex);

    Value a, b, c, d;
    nextUnpackComplexPair(leftComplex, rightComplex, &a, &b, &c, &d);

    Value real = n_mod(a, c);
    protectValue(real);
    Value imag = n_mod(b, d);
    protectValue(imag);
    Value res = nextComSimplify(real, imag);
    protectValue(res);

    UNPROTECT(save);
    return res;
}

static const NextBinaryHandler nextModHandlers[] = {
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
        return nextRatFallbackCompare(left, right, n_cmp);
    }

    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    int save = protectValue(left);
    protectValue(right);

    Value a, b, c, d;
    nextUnpackRationalPair(left, right, &a, &b, &c, &d);

    Value ad = n_mul(a, d);
    protectValue(ad);
    Value bc = n_mul(b, c);
    protectValue(bc);
    Cmp res = n_cmp(ad, bc);

    UNPROTECT(save);
    return res;
}

static Cmp n_cmp_irr(Value left, Value right) {
    Value leftIrr = nextToIrr(left);
    Value rightIrr = nextToIrr(right);
    return getValue_Irrational(leftIrr) < getValue_Irrational(rightIrr) ? CMP_LT
           : getValue_Irrational(leftIrr) == getValue_Irrational(rightIrr)
               ? CMP_EQ
               : CMP_GT;
}

static Cmp n_cmp_imag(Value left, Value right) {
    ASSERT(IS_IMAG_TYPE(left.type));
    ASSERT(IS_IMAG_TYPE(right.type));

    Value leftReal = nextImagToReal(left);
    Value rightReal = nextImagToReal(right);
    return n_cmp(leftReal, rightReal);
}

static Cmp n_cmp_complex(Value left, Value right) {
    Value leftComplex = nextToComplex(left);
    int save = protectValue(leftComplex);
    Value rightComplex = nextToComplex(right);
    protectValue(rightComplex);

    Value leftReal, leftImag, rightReal, rightImag;
    nextUnpackComplexPair(leftComplex, rightComplex, &leftReal, &leftImag,
                          &rightReal, &rightImag);

    Cmp realCmp = n_cmp(leftReal, rightReal);
    Cmp imagCmp = n_cmp(leftImag, rightImag);
    Cmp res = CMP_EQ;

    switch (realCmp) {
    case CMP_LT:
        if (imagCmp == CMP_GT) {
            res = nextMagCmp(leftReal, leftImag, rightReal, rightImag);
        } else {
            res = CMP_LT;
        }
        break;
    case CMP_EQ:
        res = imagCmp;
        break;
    case CMP_GT:
        if (imagCmp == CMP_LT) {
            res = nextMagCmp(leftReal, leftImag, rightReal, rightImag);
        } else {
            res = CMP_GT;
        }
        break;
    }

    UNPROTECT(save);
    return res;
}

static const NextCmpHandler nextCmpHandlers[] = {
    [ARITH_DOMAIN_INT_STD] = n_cmp_std, [ARITH_DOMAIN_INT_BIG] = n_cmp_big,
    [ARITH_DOMAIN_RAT] = n_cmp_rat,     [ARITH_DOMAIN_IRR] = n_cmp_irr,
    [ARITH_DOMAIN_IMAG] = n_cmp_imag,   [ARITH_DOMAIN_COMPLEX] = n_cmp_complex,
};

/////////////////////////////
// public staged API
/////////////////////////////

Value n_add(Value left, Value right) {
    return applyCommonDomainBinary(
        ARITH_OP_ADD, "add", nextAddHandlers,
        sizeof(nextAddHandlers) / sizeof(nextAddHandlers[0]), left, right);
}

Value n_sub(Value left, Value right) {
    return applyCommonDomainBinary(
        ARITH_OP_SUB, "sub", nextSubHandlers,
        sizeof(nextSubHandlers) / sizeof(nextSubHandlers[0]), left, right);
}

Value n_mul(Value left, Value right) {
    return applyCommonDomainBinary(
        ARITH_OP_MUL, "mul", nextMulHandlers,
        sizeof(nextMulHandlers) / sizeof(nextMulHandlers[0]), left, right);
}

Value n_div(Value left, Value right) {
    return applyCommonDomainBinary(
        ARITH_OP_DIV, "div", nextDivHandlers,
        sizeof(nextDivHandlers) / sizeof(nextDivHandlers[0]), left, right);
}

Value n_mod(Value left, Value right) {
    return applyCommonDomainBinary(
        ARITH_OP_MOD, "mod", nextModHandlers,
        sizeof(nextModHandlers) / sizeof(nextModHandlers[0]), left, right);
}

Value n_pow(Value left, Value right) {
    requirePlan(ARITH_OP_POW, left, right);
    return npow(left, right);
}

Cmp n_cmp(Value left, Value right) {
    return applyCommonDomainCompare(
        ARITH_OP_CMP, "cmp", nextCmpHandlers,
        sizeof(nextCmpHandlers) / sizeof(nextCmpHandlers[0]), left, right);
}
