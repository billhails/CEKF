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
#include <limits.h>

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

typedef Value (*IntegerBinOp)(Value, Value);
typedef Value (*ParameterizedBinOp)(IntegerBinOp, Value, Value);

bool arithmetic_initialized = false;

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

#define IS_BIGINT(x) ((x).type == VALUE_TYPE_BIGINT)

static Value intValue(int i) {
    Value val;
    val.type = VALUE_TYPE_STDINT;
    val.val = VALUE_VAL_STDINT(i);
    return val;
}

static Value bigIntValue(BigInt *i) {
    Value val;
    val.type = VALUE_TYPE_BIGINT;
    val.val = VALUE_VAL_BIGINT(i);
    return val;
}

static inline Cmp int_cmp_bb(Value left, Value right) {
    return cmpBigInt(left.val.bigint, right.val.bigint);
}

static inline Cmp int_cmp_bi(Value left, Value right) {
    return cmpBigIntInt(left.val.bigint, right.val.stdint);
}

static inline Cmp int_cmp_ib(Value left, Value right) {
    return cmpIntBigInt(left.val.stdint, right.val.bigint);
}

static inline Cmp int_cmp_ii(Value left, Value right) {
    return left.val.stdint < right.val.stdint ? CMP_LT :
        left.val.stdint == right.val.stdint ? CMP_EQ :
        CMP_GT;
}

static Cmp intCmp(Value left, Value right) {
    ENTER(intCmp);
    Cmp res;
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            res = int_cmp_bb(left, right);
        } else {
            res = int_cmp_bi(left, right);
        }
    } else {
        if (IS_BIGINT(right)) {
            res = int_cmp_ib(left, right);
        } else {
            res = int_cmp_ii(left, right);
        }
    }
    LEAVE(intCmp);
    return res;
}

static Value safe_add(int a, int b) {
    int c;
    if (__builtin_add_overflow(a, b, &c)) {
        BigInt *big = bigIntFromAddition(a, b);
        int save = PROTECT(big);
        Value res = bigIntValue(big);
        UNPROTECT(save);
        return res;
    } else {
        return intValue(c);
    }
}

static Value intAdd(Value left, Value right) {
    ENTER(intAdd);
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *b = addBigInt(left.val.bigint, right.val.bigint);
            PROTECT(b);
            res = bigIntValue(b);
        } else {
            BigInt *b = addBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(b);
            res = bigIntValue(b);
        }
    } else {
        if (IS_BIGINT(right)) {
            BigInt *b = addBigIntInt(right.val.bigint, left.val.stdint);
            PROTECT(b);
            res = bigIntValue(b);
        } else {
            res = safe_add(left.val.stdint, right.val.stdint);
        }
    }
    LEAVE(intAdd);
    UNPROTECT(save);
    return res;
}

static Value safe_mul(int a, int b) {
    int c;
    if (__builtin_mul_overflow(a, b, &c)) {
        BigInt *big = bigIntFromMultiplication(a, b);
        int save = PROTECT(big);
        Value res = bigIntValue(big);
        UNPROTECT(save);
        return res;
    } else {
        return intValue(c);
    }
}

static Value intMul(Value left, Value right) {
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *bi = mulBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            BigInt *bi = mulBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = bigIntValue(bi);
        }
    } else {
        if (IS_BIGINT(right)) {
            BigInt *bi = mulBigIntInt(right.val.bigint, left.val.stdint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            res = safe_mul(left.val.stdint, right.val.stdint);
        }
    }
    UNPROTECT(save);
    return res;
}

static Value safe_sub(int a, int b) {
    int c;
    if (__builtin_sub_overflow(a, b, &c)) {
        BigInt *big = bigIntFromSubtraction(a, b);
        int save = PROTECT(big);
        Value res = bigIntValue(big);
        UNPROTECT(save);
        return res;
    } else {
        return intValue(c);
    }
}

static Value intSub(Value left, Value right) {
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *bi = subBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            BigInt *bi = subBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = bigIntValue(bi);
        }
    } else {
        if (IS_BIGINT(right)) {
            BigInt *bi = subIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            res = safe_sub(left.val.stdint, right.val.stdint);
        }
    }
    UNPROTECT(save);
    return res;
}

static Value intDiv(Value left, Value right) {
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            if (int_cmp_bb(right, Zero) == CMP_EQ) {
                cant_happen("attempted div zero");
            }
            BigInt *bi = divBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            if (right.val.stdint == 0) {
                cant_happen("attempted div zero");
            }
            BigInt *bi = divBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = bigIntValue(bi);
        }
    } else {
        if (IS_BIGINT(right)) {
            if (int_cmp_bb(right, Zero) == CMP_EQ) {
                cant_happen("attempted div zero");
            }
            BigInt *bi = divIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            if (right.val.stdint == 0) {
                cant_happen("attempted div zero");
            }
            // division can't overflow
            res = intValue(left.val.stdint / right.val.stdint);
        }
    }
    UNPROTECT(save);
    return res;
}

static Value safe_pow(int a, int b) {
    float f = powf((float) a, (float) b);
    if (f == HUGE_VALF || f > (float)INT_MAX || f < (float)INT_MIN) {
        BigInt *big = bigIntFromPower(a, b);
        int save = PROTECT(big);
        Value res = bigIntValue(big);
        UNPROTECT(save);
        return res;
    } else {
        return intValue((int) f);
    }
}

static Value intPow(Value left, Value right) {
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *bi = powBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            BigInt *bi = powBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = bigIntValue(bi);
        }
    } else {
        if (IS_BIGINT(right)) {
            BigInt *bi = powIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            res = safe_pow(left.val.stdint, right.val.stdint);
        }
    }
    UNPROTECT(save);
    return res;
}

static Value intMod(Value left, Value right) {
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            if (int_cmp_bb(right, Zero) == CMP_EQ) {
                cant_happen("attempted mod zero");
            }
            BigInt *bi = modBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            if (right.val.stdint == 0) {
                cant_happen("attempted mod zero");
            }
            BigInt *bi = modBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = bigIntValue(bi);
        }
    } else {
        if (IS_BIGINT(right)) {
            if (int_cmp_bb(right, Zero) == CMP_EQ) {
                cant_happen("attempted mod zero");
            }
            BigInt *bi = modIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            if (right.val.stdint == 0) {
                cant_happen("attempted mod zero");
            }
            // modulus can't overflow
            res = intValue(left.val.stdint % right.val.stdint);
        }
    }
    UNPROTECT(save);
    return res;
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

static Value intGcd(Value left, Value right) {
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *bi = gcdBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            BigInt *bi = gcdBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = bigIntValue(bi);
        }
    } else {
        if (IS_BIGINT(left)) {
            BigInt *bi = gcdIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = bigIntValue(bi);
        } else {
            res = intValue(gcd(left.val.stdint, right.val.stdint));
        }
    }
    UNPROTECT(save);
    return res;
}

static void intNegInPlace(Value *v) {
    if (IS_BIGINT(*v)) {
        negateBigInt(v->val.bigint);
    } else {
        v->val.stdint = -(v->val.stdint);
    }
}

static Value intNeg(Value v) {
    int save = PROTECT(NULL);
    if (IS_BIGINT(v)) {
        BigInt *bi = copyBigInt(v.val.bigint);
        PROTECT(bi);
        negateBigInt(bi);
        v = bigIntValue(bi);
    } else {
        v.val.stdint = -(v.val.stdint);
    }
    UNPROTECT(save);
    return v;
}

static bool intIsNeg(Value v) {
    if (IS_BIGINT(v)) {
        return isNegBigInt(v.val.bigint);
    } else {
        return v.val.stdint < 0;
    }
}

////////////////////////
// rational operations
////////////////////////

#ifdef SAFETY_CHECKS
#  define ASSERT_RATIONAL(x) ASSERT((x).type == VALUE_TYPE_RATIONAL)
#else
#  define ASSERT_RATIONAL(x)
#endif

static Cmp ratCmp(Value left, Value right) {
    ENTER(ratCmp);
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value ad = intMul(left.val.vec->values[NUMERATOR],
                       right.val.vec->values[DENOMINATOR]);
    int save = protectValue(ad);
    Value bc = intMul(left.val.vec->values[DENOMINATOR],
                       right.val.vec->values[NUMERATOR]);
    protectValue(bc);
    Cmp res = intCmp(ad, bc);
    LEAVE(ratCmp);
    UNPROTECT(save);
    return res;
}

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
    Value gcd = intGcd(numerator, denominator);
    int save = protectValue(gcd);
    Value res;
    if (intCmp(gcd, One) != CMP_EQ) {
        numerator = intDiv(numerator, gcd);
        protectValue(numerator);
        denominator = intDiv(denominator, gcd);
        protectValue(denominator);
    }
    if (intIsNeg(denominator)) {
        intNegInPlace(&numerator);
        intNegInPlace(&denominator);
    }
    if (intCmp(denominator, One) == CMP_EQ) {
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

// a/b o c/d = (ad o bc) / bd
static Value rat_ad_bc_cd(IntegerBinOp base_op, Value left, Value right) {
    ENTER(rat_add_sub);
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value a1b2 =
        intMul(left.val.vec->values[NUMERATOR],
                right.val.vec->values[DENOMINATOR]);
    int save = protectValue(a1b2);
    Value a2b1 =
        intMul(left.val.vec->values[DENOMINATOR],
                right.val.vec->values[NUMERATOR]);
    protectValue(a2b1);
    Value numerator = base_op(a1b2, a2b1);
    protectValue(numerator);
    Value denominator =
        intMul(left.val.vec->values[DENOMINATOR],
                right.val.vec->values[DENOMINATOR]);
    protectValue(denominator);
    Value res = ratSimplify(numerator, denominator);
    UNPROTECT(save);
    LEAVE(rat_add_sub);
    return res;
}

// a/b o c/d = ac o bd
static Value rat_ac_bd(IntegerBinOp base_op, Value left, Value right) {
    ENTER(rat_ac_bd);
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
    LEAVE(rat_ac_bd);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratDiv(IntegerBinOp base_op, Value left, Value right) {
    ENTER(ratDiv);
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value newRight = makeRational(right.val.vec->values[DENOMINATOR], right.val.vec->values[NUMERATOR]);
    int save = protectValue(newRight);
    Value res = rat_ac_bd(base_op, left, newRight);
    protectValue(res);
    LEAVE(ratDiv);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratPow(Value left, Value right) {
    ENTER(ratPow);
    ASSERT_RATIONAL(left);
    Value numerator = left.val.vec->values[NUMERATOR];
    Value denominator = left.val.vec->values[DENOMINATOR];
    numerator = intPow(numerator, right);
    int save = protectValue(numerator);
    denominator = intPow(denominator, right);
    protectValue(denominator);
    Value res = ratSimplify(numerator, denominator);
    protectValue(res);
    LEAVE(ratPow);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

#ifdef SAFETY_CHECKS
#  define CHECK_INITIALIZED() do { \
    if (!arithmetic_initialized) { \
        cant_happen("arithmetic not initialized yet"); \
    } \
} while(0)
#else
#  define CHECK_INITIALIZED()
#endif


Value nadd(Value left, Value right) {
    ENTER(nadd);
    CHECK_INITIALIZED();
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratOp(left, right, rat_ad_bc_cd, intAdd, true);
    LEAVE(nadd);
    return res;
}

Value nsub(Value left, Value right) {
    ENTER(nsub);
    CHECK_INITIALIZED();
    Value res = ratOp(left, right, rat_ad_bc_cd, intSub, true);
    LEAVE(nsub);
    return res;
}

Value nmul(Value left, Value right) {
    ENTER(nmul);
    CHECK_INITIALIZED();
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratOp(left, right, rat_ac_bd, intMul, true);
    int save = protectValue(res);
    LEAVE(nmul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Value ndiv(Value left, Value right) {
    ENTER(ndiv);
    CHECK_INITIALIZED();
    // N.B. intMul not intDiv
    Value res = ratOp(left, right, ratDiv, intMul, false);
    int save = protectValue(res);
    LEAVE(ndiv);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Value nmod(Value left, Value right) {
    ENTER(nmod);
    CHECK_INITIALIZED();
    Value res = ratOp(left, right, rat_ad_bc_cd, intMod, true);
    LEAVE(nmod);
    return res;
}

Value npow(Value left, Value right) {
    ENTER(npow);
    CHECK_INITIALIZED();
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
            res = ratPow(left, right);
            protectValue(res);
        }
    } else if (right.type == VALUE_TYPE_RATIONAL) {
        cant_happen("raising numbers to a rational power not supported yet");
    } else {
        // neither rational
        res = intPow(left, right);
        protectValue(res);
    }
    LEAVE(npow);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Cmp ncmp(Value left, Value right) {
    ENTER(ncmp);
    CHECK_INITIALIZED();
    Cmp res;
    int save = PROTECT(NULL);
    if (left.type == VALUE_TYPE_RATIONAL) {
        if (right.type == VALUE_TYPE_RATIONAL) {
            res = ratCmp(left, right);
        } else {
            right = makeRational(right, One);
            protectValue(right);
            res = ratCmp(left, right);
        }
    } else {
        if (right.type == VALUE_TYPE_RATIONAL) {
            left = makeRational(left, One);
            protectValue(left);
            res = ratCmp(left, right);
        } else {
            res = intCmp(left, right);
        }
    }
    LEAVE(ncmp);
    UNPROTECT(save);
    return res;
}

Value nneg(Value v) {
    ENTER(nneg);
    CHECK_INITIALIZED();
    Value res;
    if (v.type == VALUE_TYPE_RATIONAL) {
        Value numerator = intNeg(v.val.vec->values[NUMERATOR]);
        int save = protectValue(numerator);
        res = makeRational(numerator, v.val.vec->values[DENOMINATOR]);
        UNPROTECT(save);
    } else {
        res = intNeg(v);
    }
    LEAVE(nneg);
    return res;
}

void init_arithmetic() {
    BigInt *zero = bigIntFromInt(0);
    Zero.type = VALUE_TYPE_BIGINT;
    Zero.val = VALUE_VAL_BIGINT(zero);
    BigInt *one = bigIntFromInt(1);
    One.type = VALUE_TYPE_BIGINT;
    One.val = VALUE_VAL_BIGINT(one);
    arithmetic_initialized = true;
}

void markArithmetic() {
    markValue(Zero);
    markValue(One);
}
