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

#ifndef cekf_arithmetic_dispatch_h
#define cekf_arithmetic_dispatch_h

#include <stdbool.h>

#include "value.h"

typedef enum ArithmeticFamily {
    ARITH_FAMILY_NONE,
    ARITH_FAMILY_SCALAR,
    ARITH_FAMILY_COMPOSITE,
    ARITH_FAMILY_EXT_SCALAR,
} ArithmeticFamily;

typedef enum ArithmeticDomain {
    ARITH_DOMAIN_NONE,
    ARITH_DOMAIN_INT_STD,
    ARITH_DOMAIN_INT_BIG,
    ARITH_DOMAIN_RAT,
    ARITH_DOMAIN_IRR,
    ARITH_DOMAIN_IMAG,
    ARITH_DOMAIN_COMPLEX,
} ArithmeticDomain;

typedef enum ArithmeticOperator {
    ARITH_OP_ADD,
    ARITH_OP_SUB,
    ARITH_OP_MUL,
    ARITH_OP_DIV,
    ARITH_OP_MOD,
    ARITH_OP_POW,
    ARITH_OP_CMP,
} ArithmeticOperator;

typedef enum ArithmeticNormalizationKind {
    ARITH_NORM_UNSUPPORTED,
    ARITH_NORM_COMMON_DOMAIN,
    ARITH_NORM_ASYMMETRIC,
} ArithmeticNormalizationKind;

typedef struct ArithmeticNormalizationPlan {
    bool supported;
    ArithmeticNormalizationKind kind;
    ArithmeticDomain leftDomain;
    ArithmeticDomain rightDomain;
    ArithmeticDomain commonDomain;
} ArithmeticNormalizationPlan;

ArithmeticFamily arithmeticFamilyForValueType(ValueType type);
ArithmeticDomain arithmeticDomainForValueType(ValueType type);
bool arithmeticIsNumericValueType(ValueType type);
ArithmeticNormalizationPlan
arithmeticMakeNormalizationPlan(ArithmeticOperator op, ValueType leftType,
                                ValueType rightType);

#endif
