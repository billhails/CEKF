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

#include "arithmetic_dispatch.h"
#include "arithmetic_number_checks.h"

ArithmeticFamily arithmeticFamilyForValueType(ValueType type) {
    switch (type) {
    case VALUE_TYPE_STDINT:
    case VALUE_TYPE_BIGINT:
    case VALUE_TYPE_RATIONAL:
    case VALUE_TYPE_IRRATIONAL:
    case VALUE_TYPE_STDINT_IMAG:
    case VALUE_TYPE_BIGINT_IMAG:
    case VALUE_TYPE_RATIONAL_IMAG:
    case VALUE_TYPE_IRRATIONAL_IMAG:
    case VALUE_TYPE_COMPLEX:
        return ARITH_FAMILY_SCALAR;
    case VALUE_TYPE_VEC:
        return ARITH_FAMILY_COMPOSITE;
    default:
        return ARITH_FAMILY_NONE;
    }
}

ArithmeticDomain arithmeticDomainForValueType(ValueType type) {
    switch (type) {
    case VALUE_TYPE_STDINT:
        return ARITH_DOMAIN_INT_STD;
    case VALUE_TYPE_BIGINT:
        return ARITH_DOMAIN_INT_BIG;
    case VALUE_TYPE_RATIONAL:
        return ARITH_DOMAIN_RAT;
    case VALUE_TYPE_IRRATIONAL:
        return ARITH_DOMAIN_IRR;
    case VALUE_TYPE_STDINT_IMAG:
    case VALUE_TYPE_BIGINT_IMAG:
    case VALUE_TYPE_RATIONAL_IMAG:
    case VALUE_TYPE_IRRATIONAL_IMAG:
        return ARITH_DOMAIN_IMAG;
    case VALUE_TYPE_COMPLEX:
        return ARITH_DOMAIN_COMPLEX;
    default:
        return ARITH_DOMAIN_NONE;
    }
}

bool arithmeticIsNumericValueType(ValueType type) {
    return arithmeticDomainForValueType(type) != ARITH_DOMAIN_NONE;
}

static ArithmeticDomain commonScalarDomain(ValueType leftType,
                                           ValueType rightType) {
    if (leftType == VALUE_TYPE_COMPLEX || rightType == VALUE_TYPE_COMPLEX) {
        return ARITH_DOMAIN_COMPLEX;
    }

    if (IS_IMAG_TYPE(leftType) && IS_IMAG_TYPE(rightType)) {
        return ARITH_DOMAIN_IMAG;
    }

    if (IS_IMAG_TYPE(leftType) || IS_IMAG_TYPE(rightType)) {
        return ARITH_DOMAIN_COMPLEX;
    }

    if (leftType == VALUE_TYPE_IRRATIONAL ||
        rightType == VALUE_TYPE_IRRATIONAL) {
        return ARITH_DOMAIN_IRR;
    }

    if (leftType == VALUE_TYPE_RATIONAL || rightType == VALUE_TYPE_RATIONAL) {
        return ARITH_DOMAIN_RAT;
    }

    if (leftType == VALUE_TYPE_BIGINT || rightType == VALUE_TYPE_BIGINT) {
        return ARITH_DOMAIN_INT_BIG;
    }

    return ARITH_DOMAIN_INT_STD;
}

ArithmeticNormalizationPlan
arithmeticMakeNormalizationPlan(ArithmeticOperator op, ValueType leftType,
                                ValueType rightType) {
    ArithmeticNormalizationPlan plan = {
        .supported = false,
        .kind = ARITH_NORM_UNSUPPORTED,
        .leftDomain = arithmeticDomainForValueType(leftType),
        .rightDomain = arithmeticDomainForValueType(rightType),
        .commonDomain = ARITH_DOMAIN_NONE,
    };

    if (plan.leftDomain == ARITH_DOMAIN_NONE ||
        plan.rightDomain == ARITH_DOMAIN_NONE) {
        return plan;
    }

    plan.supported = true;

    if (op == ARITH_OP_POW) {
        plan.kind = ARITH_NORM_ASYMMETRIC;
        return plan;
    }

    plan.kind = ARITH_NORM_COMMON_DOMAIN;
    plan.commonDomain = commonScalarDomain(leftType, rightType);
    return plan;
}
