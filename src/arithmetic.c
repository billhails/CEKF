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
#include "cekf.h"
#include "debug.h"

#ifdef DEBUG_ARITHMETIC
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

#define NUMERATOR 0
#define DENOMINATOR 1

int rational_flag = 0;

IntegerBinOp add;
IntegerBinOp sub;
IntegerBinOp mul;
IntegerBinOp divide;
IntegerBinOp power;
IntegerBinOp modulo;

static IntegerBinOp int_add;
static IntegerBinOp int_sub;
static IntegerBinOp int_mul;
static IntegerBinOp int_divide;
static IntegerBinOp int_power;
static IntegerBinOp int_modulo;

static IntegerBinOp int_gcd;
static CmpBinOp int_cmp;
static voidOp int_neg;
static boolOp int_isneg;

static Value One = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(1)
};

static Value Zero = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(0)
};

#ifdef DEBUG_ARITHMETIC
static void ppNumber(Value number) {
    switch (number.type) {
        case VALUE_TYPE_STDINT:
            eprintf("%d", number.val.stdint);
            break;
        case VALUE_TYPE_BIGINT:
            eprintf("[");
            fprintBigInt(errout, number.val.bigint);
            eprintf("]");
            break;
        case VALUE_TYPE_RATIONAL:
            ppNumber(number.val.vec->values[0]);
            eprintf("/");
            ppNumber(number.val.vec->values[1]);
            break;
        default:
            eprintf("??? %d ???", number.type);
    }
}
#endif

////////////////////////////////
// fixed size integer operations
////////////////////////////////

static Value intValue(int i) {
    Value val;
    val.type = VALUE_TYPE_STDINT;
    val.val = VALUE_VAL_STDINT(i);
    return val;
}

#ifdef SAFETY_CHECKS
#  define ASSERT_STDINT(x) ASSERT((x).type == VALUE_TYPE_STDINT)
#else
#  define ASSERT_STDINT(x)
#endif

static int littleCmp(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    return left.val.stdint < right.val.stdint ? -1 : left.val.stdint == right.val.stdint ? 0 : 1;
}

static Value littleAdd(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    return intValue(left.val.stdint + right.val.stdint);
}

static Value littleMul(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    return intValue(left.val.stdint * right.val.stdint);
}

static Value littleSub(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    return intValue(left.val.stdint - right.val.stdint);
}

static Value littleDivide(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    if (littleCmp(right, Zero) == 0) {
        cant_happen("attempted div zero");
    }
    return intValue(left.val.stdint / right.val.stdint);
}

static Value littlePower(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    return intValue(pow(left.val.stdint, right.val.stdint));
}

static Value littleModulo(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    if (littleCmp(right, Zero) == 0) {
        cant_happen("attempted mod zero");
    }
    return intValue(left.val.stdint % right.val.stdint);
}

static int gcd (int a, int b) {
	int i = 0, min_num = a, gcd = 1;
	if (a > b) {
		min_num = b;
	}
	for (i = 1; i <= min_num; i++) {
		if (a % i == 0 && b % i == 0) {
			gcd = i;
		}
	}
	return gcd;
}

static Value littleGcd(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    return intValue(gcd(left.val.stdint, right.val.stdint));
}

static void littleNeg(Value v) {
    ASSERT_STDINT(v);
    v.val.stdint = -v.val.stdint;
}

static bool littleIsNeg(Value v) {
    ASSERT_STDINT(v);
    return v.val.stdint < 0;
}

////////////////////////////////////
// arbitrary size integer operations
////////////////////////////////////

#ifdef SAFETY_CHECKS
#  define ASSERT_BIGINT(x) ASSERT((x).type == VALUE_TYPE_BIGINT)
#else
#  define ASSERT_BIGINT(x)
#endif

static int bigCmp(Value left, Value right) {
    ENTER(bigCmp);
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    LEAVE(bigCmp);
    return cmpBigInt(left.val.bigint, right.val.bigint);
}

static Value bigIntValue(BigInt *i) {
    Value val;
    val.type = VALUE_TYPE_BIGINT;
    val.val = VALUE_VAL_BIGINT(i);
    return val;
}

static Value bigAdd(Value left, Value right) {
    ENTER(bigAdd);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    BigInt *result = addBigInt(left.val.bigint, right.val.bigint);
    int save = PROTECT(result);
    Value res = bigIntValue(result);
    LEAVE(bigAdd);
    UNPROTECT(save);
    return res;
}

static Value bigMul(Value left, Value right) {
    ENTER(bigMul);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    BigInt *result = mulBigInt(left.val.bigint, right.val.bigint);
    int save = PROTECT(result);
    Value res = bigIntValue(result);
    LEAVE(bigMul);
    UNPROTECT(save);
    return res;
}

static Value bigSub(Value left, Value right) {
    ENTER(bigSub);
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    BigInt *result = subBigInt(left.val.bigint, right.val.bigint);
    int save = PROTECT(result);
    Value res = bigIntValue(result);
    LEAVE(bigSub);
    UNPROTECT(save);
    return res;
}

static Value bigDivide(Value left, Value right) {
    ENTER(bigDivide);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    if (bigCmp(right, Zero) == 0) {
        cant_happen("attempted div zero");
    }
    BigInt *result = divBigInt(left.val.bigint, right.val.bigint);
    int save = PROTECT(result);
    Value res = bigIntValue(result);
    protectValue(res);
    LEAVE(bigDivide);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value bigPower(Value left, Value right) {
    ENTER(bigPower);
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    BigInt *result = powBigInt(left.val.bigint, right.val.bigint);
    int save = PROTECT(result);
    Value res = bigIntValue(result);
    LEAVE(bigPower);
    UNPROTECT(save);
    return res;
}

static Value bigModulo(Value left, Value right) {
    ENTER(bigModulo);
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    if (bigCmp(right, Zero) == 0) {
        cant_happen("attempted mod zero");
    }
    BigInt *result = modBigInt(left.val.bigint, right.val.bigint);
    int save = PROTECT(result);
    Value res = bigIntValue(result);
    LEAVE(bigModulo);
    UNPROTECT(save);
    return res;
}

static Value bigGcd(Value left, Value right) {
    ENTER(bigGcd);
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    BigInt *gcd = gcdBigInt(left.val.bigint, right.val.bigint);
    int save = PROTECT(gcd);
    Value res = bigIntValue(gcd);
    LEAVE(bigGcd);
    UNPROTECT(save);
    return res;
}

static void bigNeg(Value v) {
    ASSERT_BIGINT(v);
    negateBigInt(v.val.bigint);
}

static bool bigIsNeg(Value v) {
    ASSERT_BIGINT(v);
    return isNegBigInt(v.val.bigint);
}

///////////////////////////////////////
// unspecified size rational operations
///////////////////////////////////////

#ifdef SAFETY_CHECKS
#  define ASSERT_RATIONAL(x) ASSERT((x).type == VALUE_TYPE_RATIONAL)
#else
#  define ASSERT_RATIONAL(x)
#endif

static Value makeRational(Value numerator, Value denominator) {
    Vec *vec = newVec(2);
    vec->values[NUMERATOR] = numerator;
    vec->values[DENOMINATOR] = denominator;
    Value res = {
        .type = VALUE_TYPE_RATIONAL,
        .val = VALUE_VAL_RATIONAL(vec)
    };
    return res;
}

static Value ratOp(Value left, Value right, ParameterizedBinOp op, IntegerBinOp intOp, bool simplify) {
    ENTER(ratOp);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = protectValue(left);
    protectValue(right);
    if (left.type == VALUE_TYPE_RATIONAL) {
        if (right.type == VALUE_TYPE_RATIONAL) {
            // both rational
            res = op(intOp, left, right);
            protectValue(res);
        } else {
            // only left rational
            right = makeRational(right, One);
            protectValue(right);
            res = ratOp(left, right, op, intOp, false);
            protectValue(res);
        }
    } else if (right.type == VALUE_TYPE_RATIONAL) {
        // only right rational
        left = makeRational(left, One);
        protectValue(left);
        res = ratOp(left, right, op, intOp, false);
        protectValue(res);
    } else {
        // neither rational
        if (simplify) {
            res = intOp(left, right);
            protectValue(res);
        } else {
            left = makeRational(left, One);
            protectValue(left);
            right = makeRational(right, One);
            protectValue(right);
            res = ratOp(left, right, op, intOp, false);
            protectValue(res);
        }
    }
    LEAVE(ratOp);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratSimplify(Value numerator, Value denominator) {
    ENTER(ratSimplify);
    IFDEBUG(ppNumber(numerator));
    IFDEBUG(ppNumber(denominator));
    Value gcd = int_gcd(numerator, denominator);
    int save = protectValue(gcd);
    Value res;
    if (int_cmp(gcd, One) != 0) {
        numerator = int_divide(numerator, gcd);
        protectValue(numerator);
        denominator = int_divide(denominator, gcd);
        protectValue(denominator);
    }
    if (int_isneg(denominator)) {
        int_neg(numerator);
        int_neg(denominator);
    }
    if (int_cmp(denominator, One) == 0) {
        res = numerator;
    } else {
        res = makeRational(numerator, denominator);
        protectValue(res);
    }
    LEAVE(ratSimplify);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value _rat_add_sub(IntegerBinOp base_op, Value left, Value right) {
    ENTER(rat_add_sub);
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value a1b2 =
        int_mul(left.val.vec->values[NUMERATOR],
                right.val.vec->values[DENOMINATOR]);
    int save = protectValue(a1b2);
    Value a2b1 =
        int_mul(left.val.vec->values[DENOMINATOR],
                right.val.vec->values[NUMERATOR]);
    protectValue(a2b1);
    Value numerator = base_op(a1b2, a2b1);
    protectValue(numerator);
    Value denominator =
        int_mul(left.val.vec->values[DENOMINATOR],
                right.val.vec->values[DENOMINATOR]);
    protectValue(denominator);
    Value res = ratSimplify(numerator, denominator);
    UNPROTECT(save);
    LEAVE(rat_add_sub);
    return res;
}

static Value ratAdd(Value left, Value right) {
    ENTER(ratAdd);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratOp(left, right, _rat_add_sub, int_add, true);
    LEAVE(ratAdd);
    return res;
}

static Value ratSub(Value left, Value right) {
    ENTER(ratSub);
    Value res = ratOp(left, right, _rat_add_sub, int_sub, true);
    LEAVE(ratSub);
    return res;
}

static Value _rat_mul(IntegerBinOp base_op, Value left, Value right) {
    ENTER(_rat_mul);
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value numerator =
        base_op(left.val.vec->values[NUMERATOR],
                right.val.vec->values[NUMERATOR]);
    int save = protectValue(numerator);
    Value denominator =
        base_op(left.val.vec->values[DENOMINATOR],
                right.val.vec->values[DENOMINATOR]);
    protectValue(denominator);
    Value res = ratSimplify(numerator, denominator);
    protectValue(res);
    LEAVE(_rat_mul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratMul(Value left, Value right) {
    ENTER(ratMul);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratOp(left, right, _rat_mul, int_mul, true);
    int save = protectValue(res);
    LEAVE(ratMul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value _rat_div(IntegerBinOp base_op, Value left, Value right) {
    ENTER(_rat_div);
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value newRight = makeRational(right.val.vec->values[DENOMINATOR], right.val.vec->values[NUMERATOR]);
    int save = protectValue(newRight);
    Value res = _rat_mul(base_op, left, newRight);
    protectValue(res);
    LEAVE(_rat_div);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratDivide(Value left, Value right) {
    ENTER(ratDivide);
    // N.B. int_mul not int_div
    Value res = ratOp(left, right, _rat_div, int_mul, false);
    int save = protectValue(res);
    LEAVE(ratDivide);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratModulo(Value left, Value right) {
    ENTER(ratModulo);
    Value res = ratOp(left, right, _rat_add_sub, int_modulo, true);
    LEAVE(ratModulo);
    return res;
}

static Value _ratPower(Value left, Value right) {
    ENTER(_ratPower);
    ASSERT_RATIONAL(left);
    Value numerator = left.val.vec->values[NUMERATOR];
    Value denominator = left.val.vec->values[DENOMINATOR];
    numerator = int_power(numerator, right);
    int save = protectValue(numerator);
    denominator = int_power(denominator, right);
    protectValue(denominator);
    Value res = ratSimplify(numerator, denominator);
    protectValue(res);
    LEAVE(_ratPower);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratPower(Value left, Value right) {
    ENTER(ratPower);
    LEAVE(ratPower);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = protectValue(left);
    protectValue(right);
    if (left.type == VALUE_TYPE_RATIONAL) {
        if (right.type == VALUE_TYPE_RATIONAL) {
            cant_happen("raising numbers to a rational power not supported yet");
        } else {
            // only left rational
            res = _ratPower(left, right);
            protectValue(res);
        }
    } else if (right.type == VALUE_TYPE_RATIONAL) {
        cant_happen("raising numbers to a rational power not supported yet");
    } else {
        // neither rational
        res = int_power(left, right);
        protectValue(res);
    }
    LEAVE(ratPower);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

void init_arithmetic() {
    if (bigint_flag) {
        int_add = bigAdd;
        int_mul = bigMul;
        int_sub = bigSub;
        int_divide = bigDivide;
        int_power = bigPower;
        int_modulo = bigModulo;
        int_gcd = bigGcd;
        int_cmp = bigCmp;
        int_neg = bigNeg;
        int_isneg = bigIsNeg;
        BigInt *zero = bigIntFromInt(0);
        Zero.type = VALUE_TYPE_BIGINT;
        Zero.val = VALUE_VAL_BIGINT(zero);
        BigInt *one = bigIntFromInt(1);
        One.type = VALUE_TYPE_BIGINT;
        One.val = VALUE_VAL_BIGINT(one);
    } else {
        int_add = littleAdd;
        int_mul = littleMul;
        int_sub = littleSub;
        int_divide = littleDivide;
        int_power = littlePower;
        int_modulo = littleModulo;
        int_gcd = littleGcd;
        int_cmp = littleCmp;
        int_neg = littleNeg;
        int_isneg = littleIsNeg;
    }

    if (rational_flag) {
        add = ratAdd;
        sub = ratSub;
        mul = ratMul;
        divide = ratDivide;
        power = ratPower;
        modulo = ratModulo;
    } else {
        add = int_add;
        mul = int_mul;
        sub = int_sub;
        divide = int_divide;
        power = int_power;
        modulo = int_modulo;
    }
}

void markArithmetic() {
    markValue(Zero);
    markValue(One);
}