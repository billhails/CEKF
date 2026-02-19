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

#include "arithmetic_dispatch.h"

static void testFamilyClassification() {
    assert(arithmeticFamilyForValueType(VALUE_TYPE_STDINT) ==
           ARITH_FAMILY_SCALAR);
    assert(arithmeticFamilyForValueType(VALUE_TYPE_RATIONAL) ==
           ARITH_FAMILY_SCALAR);
    assert(arithmeticFamilyForValueType(VALUE_TYPE_COMPLEX) ==
           ARITH_FAMILY_SCALAR);
    assert(arithmeticFamilyForValueType(VALUE_TYPE_VEC) ==
           ARITH_FAMILY_COMPOSITE);
    assert(arithmeticFamilyForValueType(VALUE_TYPE_CHARACTER) ==
           ARITH_FAMILY_NONE);
}

static void testDomainClassification() {
    assert(arithmeticDomainForValueType(VALUE_TYPE_STDINT) ==
           ARITH_DOMAIN_INT_STD);
    assert(arithmeticDomainForValueType(VALUE_TYPE_BIGINT) ==
           ARITH_DOMAIN_INT_BIG);
    assert(arithmeticDomainForValueType(VALUE_TYPE_RATIONAL) ==
           ARITH_DOMAIN_RAT);
    assert(arithmeticDomainForValueType(VALUE_TYPE_IRRATIONAL) ==
           ARITH_DOMAIN_IRR);
    assert(arithmeticDomainForValueType(VALUE_TYPE_STDINT_IMAG) ==
           ARITH_DOMAIN_IMAG);
    assert(arithmeticDomainForValueType(VALUE_TYPE_COMPLEX) ==
           ARITH_DOMAIN_COMPLEX);
    assert(arithmeticDomainForValueType(VALUE_TYPE_CHARACTER) ==
           ARITH_DOMAIN_NONE);
}

static void testCommonDomainNormalization() {
    ArithmeticNormalizationPlan plan = arithmeticMakeNormalizationPlan(
        ARITH_OP_ADD, VALUE_TYPE_STDINT, VALUE_TYPE_BIGINT);
    assert(plan.supported);
    assert(plan.kind == ARITH_NORM_COMMON_DOMAIN);
    assert(plan.commonDomain == ARITH_DOMAIN_INT_BIG);

    plan = arithmeticMakeNormalizationPlan(ARITH_OP_MUL, VALUE_TYPE_RATIONAL,
                                           VALUE_TYPE_IRRATIONAL);
    assert(plan.supported);
    assert(plan.kind == ARITH_NORM_COMMON_DOMAIN);
    assert(plan.commonDomain == ARITH_DOMAIN_IRR);

    plan = arithmeticMakeNormalizationPlan(
        ARITH_OP_SUB, VALUE_TYPE_RATIONAL_IMAG, VALUE_TYPE_STDINT);
    assert(plan.supported);
    assert(plan.kind == ARITH_NORM_COMMON_DOMAIN);
    assert(plan.commonDomain == ARITH_DOMAIN_COMPLEX);

    plan = arithmeticMakeNormalizationPlan(ARITH_OP_GCD, VALUE_TYPE_STDINT,
                                           VALUE_TYPE_RATIONAL);
    assert(plan.supported);
    assert(plan.kind == ARITH_NORM_COMMON_DOMAIN);
    assert(plan.commonDomain == ARITH_DOMAIN_RAT);
}

static void testPowAsymmetricNormalization() {
    ArithmeticNormalizationPlan plan = arithmeticMakeNormalizationPlan(
        ARITH_OP_POW, VALUE_TYPE_STDINT, VALUE_TYPE_RATIONAL);
    assert(plan.supported);
    assert(plan.kind == ARITH_NORM_ASYMMETRIC);
    assert(plan.leftDomain == ARITH_DOMAIN_INT_STD);
    assert(plan.rightDomain == ARITH_DOMAIN_RAT);
    assert(plan.commonDomain == ARITH_DOMAIN_NONE);

    plan = arithmeticMakeNormalizationPlan(ARITH_OP_POW, VALUE_TYPE_COMPLEX,
                                           VALUE_TYPE_IRRATIONAL);
    assert(plan.supported);
    assert(plan.kind == ARITH_NORM_ASYMMETRIC);
    assert(plan.leftDomain == ARITH_DOMAIN_COMPLEX);
    assert(plan.rightDomain == ARITH_DOMAIN_IRR);
}

static void testUnsupportedPairs() {
    ArithmeticNormalizationPlan plan = arithmeticMakeNormalizationPlan(
        ARITH_OP_ADD, VALUE_TYPE_CHARACTER, VALUE_TYPE_STDINT);
    assert(!plan.supported);
    assert(plan.kind == ARITH_NORM_UNSUPPORTED);

    plan = arithmeticMakeNormalizationPlan(ARITH_OP_ADD, VALUE_TYPE_VEC,
                                           VALUE_TYPE_STDINT);
    assert(!plan.supported);
    assert(plan.kind == ARITH_NORM_UNSUPPORTED);
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    testFamilyClassification();
    testDomainClassification();
    testCommonDomainNormalization();
    testPowAsymmetricNormalization();
    testUnsupportedPairs();
    return 0;
}
