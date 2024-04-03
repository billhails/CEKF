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

#include <math.h>

#include "common.h"
#include "bigint.h"
#include "arithmetic.h"

IntegerBinOp add;
IntegerBinOp sub;
IntegerBinOp mul;
IntegerBinOp divide;
IntegerBinOp power;
IntegerBinOp modulo;

static Value intValue(int i) {
    Value value;
    value.type = VALUE_TYPE_STDINT;
    value.val = VALUE_VAL_STDINT(i);
    return value;
}

static Value bigIntValue(BigInt *i) {
    Value value;
    value.type = VALUE_TYPE_BIGINT;
    value.val = VALUE_VAL_BIGINT(i);
    return value;
}

static Value bigAdd(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_BIGINT);
    ASSERT(right.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = addBigInt(left.val.bigint, right.val.bigint);
    return bigIntValue(result);
}

static Value littleAdd(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_STDINT);
    ASSERT(right.type == VALUE_TYPE_STDINT);
#endif
    return intValue(left.val.stdint + right.val.stdint);
}

static Value bigMul(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_BIGINT);
    ASSERT(right.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = mulBigInt(left.val.bigint, right.val.bigint);
    return bigIntValue(result);
}

static Value littleMul(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_STDINT);
    ASSERT(right.type == VALUE_TYPE_STDINT);
#endif
    return intValue(left.val.stdint * right.val.stdint);
}

static Value bigSub(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_BIGINT);
    ASSERT(right.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = subBigInt(left.val.bigint, right.val.bigint);
    return bigIntValue(result);
}

static Value littleSub(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_STDINT);
    ASSERT(right.type == VALUE_TYPE_STDINT);
#endif
    return intValue(left.val.stdint - right.val.stdint);
}

static Value bigDivide(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_BIGINT);
    ASSERT(right.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = divBigInt(left.val.bigint, right.val.bigint);
    return bigIntValue(result);
}

static Value littleDivide(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_STDINT);
    ASSERT(right.type == VALUE_TYPE_STDINT);
#endif
    return intValue(left.val.stdint / right.val.stdint);
}

static Value bigPower(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_BIGINT);
    ASSERT(right.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = powBigInt(left.val.bigint, right.val.bigint);
    return bigIntValue(result);
}

static Value littlePower(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_STDINT);
    ASSERT(right.type == VALUE_TYPE_STDINT);
#endif
    return intValue(pow(left.val.stdint, right.val.stdint));
}

static Value bigModulo(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_BIGINT);
    ASSERT(right.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = modBigInt(left.val.bigint, right.val.bigint);
    return bigIntValue(result);
}

static Value littleModulo(Value left, Value right) {
#ifdef SAFETY_CHECKS
    ASSERT(left.type == VALUE_TYPE_STDINT);
    ASSERT(right.type == VALUE_TYPE_STDINT);
#endif
    return intValue(left.val.stdint % right.val.stdint);
}


void init_arithmetic() {
    if (bigint_flag) {
        add = bigAdd;
        mul = bigMul;
        sub = bigSub;
        divide = bigDivide;
        power = bigPower;
        modulo = bigModulo;
    } else {
        add = littleAdd;
        mul = littleMul;
        sub = littleSub;
        divide = littleDivide;
        power = littlePower;
        modulo = littleModulo;
    }
}
