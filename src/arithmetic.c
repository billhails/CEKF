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

#define IS_BIGINT(x) ((x).type == VALUE_TYPE_BIGINT)
#define IS_IRRATIONAL(x) ((x).type == VALUE_TYPE_IRRATIONAL)
#define IS_RATIONAL(x) ((x).type == VALUE_TYPE_RATIONAL)
#define IS_STDINT(x) ((x).type == VALUE_TYPE_STDINT)
#define IS_INT(x) (IS_STDINT(x) || IS_BIGINT(x))

#ifdef SAFETY_CHECKS
#  define ASSERT_RATIONAL(x) ASSERT(IS_RATIONAL(x))
#  define ASSERT_IRRATIONAL(x) ASSERT(IS_IRRATIONAL(x))
#  define ASSERT_BIGINT(x) ASSERT(IS_BIGINT(x))
#  define ASSERT_STDINT(x) ASSERT(IS_STDINT(x))
#  define ASSERT_INT(x) ASSERT(IS_INT(x))
#else
#  define ASSERT_RATIONAL(x)
#  define ASSERT_IRRATIONAL(x)
#  define ASSERT_BIGINT(x)
#  define ASSERT_STDINT(x)
#  define ASSERT_INT(x)
#endif

// promote adds this to its result if it malloc'd a right value
#define RIGHT_INDICATOR 1024

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
        case VALUE_TYPE_IRRATIONAL:
            eprintf("%f", number.val.irrational);
            break;
        default:
            eprintf("??? %d ???", number.type);
    }
}
#endif

static Value intValue(int i) {
    Value val;
    val.type = VALUE_TYPE_STDINT;
    val.val = VALUE_VAL_STDINT(i);
    return val;
}

static Value irrValue(double f) __attribute__((unused));

static Value irrValue(double f) {
    Value val;
    val.type = VALUE_TYPE_IRRATIONAL;
    val.val = VALUE_VAL_IRRATIONAL(f);
    return val;
}

static Value bigIntValue(BigInt *i) {
    Value val;
    val.type = VALUE_TYPE_BIGINT;
    val.val = VALUE_VAL_BIGINT(i);
    return val;
}

static Value ratValue(Value numerator, Value denominator) {
    Vec *vec = newVec(2);
    vec->values[NUMERATOR] = numerator;
    vec->values[DENOMINATOR] = denominator;
    Value res = {
        .type = VALUE_TYPE_RATIONAL,
        .val = VALUE_VAL_RATIONAL(vec)
    };
    return res;
}

static Value int_to_irrational(Value *integer) {
    ASSERT_INT(*integer);
    if (integer->type == VALUE_TYPE_BIGINT) {
        return irrValue(bigIntToDouble(integer->val.bigint));
    } else {
        return irrValue(integer->val.stdint);
    }
}

static Value rational_to_irrational(Value *rational) {
    ASSERT_RATIONAL(*rational);
    Value numerator = int_to_irrational(&(rational->val.vec->values[NUMERATOR]));
    Value denominator = int_to_irrational(&(rational->val.vec->values[DENOMINATOR]));
    return irrValue(numerator.val.irrational / denominator.val.irrational);
}

static Value int_to_rational(Value *integer) {
    ASSERT_INT(*integer);
    Value one = intValue(1);
    return ratValue(*integer, one);
}

static Value bigint_to_irrational(Value *v) {
    ASSERT_BIGINT(*v);
    return irrValue(bigIntToDouble(v->val.bigint));
}

static Value int_to_bigint(Value *v) {
    ASSERT_STDINT(*v);
    return bigIntValue(bigIntFromInt(v->val.stdint));
}

static int promote(Value *left, Value *right) {
    switch(left->type) {
        case VALUE_TYPE_RATIONAL:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                    return VALUE_TYPE_RATIONAL;
                case VALUE_TYPE_IRRATIONAL:
                    *left = rational_to_irrational(left);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_BIGINT:
                    *right = int_to_rational(right);
                    return VALUE_TYPE_RATIONAL + RIGHT_INDICATOR;
                case VALUE_TYPE_STDINT:
                    *right = int_to_rational(right);
                    return VALUE_TYPE_RATIONAL + RIGHT_INDICATOR;
                default:
                    cant_happen("unrecognised right number type %d", right->type);
            }
            break;
        case VALUE_TYPE_IRRATIONAL:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                    *right = rational_to_irrational(right);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_IRRATIONAL:
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_BIGINT:
                    *right = bigint_to_irrational(right);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_STDINT:
                    *right = int_to_irrational(right);
                    return VALUE_TYPE_IRRATIONAL;
                default:
                    cant_happen("unrecognised right number type %d", right->type);
            }
            break;
        case VALUE_TYPE_BIGINT:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                    *left = int_to_rational(left);
                    return VALUE_TYPE_RATIONAL;
                case VALUE_TYPE_IRRATIONAL:
                    *left = bigint_to_irrational(left);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_BIGINT:
                    return VALUE_TYPE_BIGINT;
                case VALUE_TYPE_STDINT:
                    *right = int_to_bigint(right);
                    return VALUE_TYPE_BIGINT + RIGHT_INDICATOR;
                default:
                    cant_happen("unrecognised right number type %d", right->type);
            }
            break;
        case VALUE_TYPE_STDINT:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                    *left = int_to_rational(left);
                    return VALUE_TYPE_RATIONAL;
                case VALUE_TYPE_IRRATIONAL:
                    *left = int_to_irrational(left);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_BIGINT:
                    *left = int_to_bigint(left);
                    return VALUE_TYPE_BIGINT;
                case VALUE_TYPE_STDINT:
                    return VALUE_TYPE_STDINT;
                default:
                    cant_happen("unrecognised right number type %d", right->type);
            }
            break;
        default:
            cant_happen("unrecognised left number type %d", left->type);
    }
}

static inline Cmp int_cmp_bb(Value left, Value right) {
    return cmpBigInt(left.val.bigint, right.val.bigint);
}

static inline Cmp int_cmp_bi(Value left, Value right) {
    return cmpBigIntInt(left.val.bigint, right.val.stdint);
}

static inline Cmp int_cmp_bf(Value left, Value right) {
    return cmpBigIntDouble(left.val.bigint, right.val.irrational);
}

static inline Cmp int_cmp_ib(Value left, Value right) {
    return cmpIntBigInt(left.val.stdint, right.val.bigint);
}

static inline Cmp int_cmp_ii(Value left, Value right) {
    return left.val.stdint < right.val.stdint ? CMP_LT :
        left.val.stdint == right.val.stdint ? CMP_EQ :
        CMP_GT;
}

static inline Cmp int_cmp_if(Value left, Value right) {
    return left.val.stdint < right.val.irrational ? CMP_LT :
        left.val.stdint == right.val.irrational ? CMP_EQ :
        CMP_GT;
}

static inline Cmp int_cmp_fb(Value left, Value right) {
    return cmpDoubleBigInt(left.val.stdint, right.val.bigint);
}

static inline Cmp int_cmp_fi(Value left, Value right) {
    return left.val.irrational < right.val.stdint ? CMP_LT :
        left.val.irrational == right.val.stdint ? CMP_EQ :
        CMP_GT;
}

static inline Cmp int_cmp_ff(Value left, Value right) {
    return left.val.irrational < right.val.irrational ? CMP_LT :
        left.val.irrational == right.val.irrational ? CMP_EQ :
        CMP_GT;
}

static Cmp numCmp(Value left, Value right) {
    ENTER(numCmp);
    Cmp res;
    switch (left.type) {
        case VALUE_TYPE_BIGINT:
            switch (right.type) {
                case VALUE_TYPE_BIGINT:
                    res = int_cmp_bb(left, right);
                    break;
                case VALUE_TYPE_STDINT:
                    res = int_cmp_bi(left, right);
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    res = int_cmp_bf(left, right);
                    break;
                default:
                    cant_happen("invalid number type");
            }
            break;
        case VALUE_TYPE_STDINT:
            switch (right.type) {
                case VALUE_TYPE_BIGINT:
                    res = int_cmp_ib(left, right);
                    break;
                case VALUE_TYPE_STDINT:
                    res = int_cmp_ii(left, right);
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    res = int_cmp_if(left, right);
                    break;
                default:
                    cant_happen("invalid number type");
            }
            break;
        case VALUE_TYPE_IRRATIONAL:
            switch (right.type) {
                case VALUE_TYPE_BIGINT:
                    res = int_cmp_fb(left, right);
                    break;
                case VALUE_TYPE_STDINT:
                    res = int_cmp_fi(left, right);
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    res = int_cmp_ff(left, right);
                    break;
                default:
                    cant_happen("invalid number type");
            }
            break;
        default:
            cant_happen("invalid number type");
    }

    LEAVE(numCmp);
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

static Value basicIntDiv(Value left, Value right) {
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
    ASSERT_INT(left);
    ASSERT_INT(right);
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
    ASSERT_INT(left);
    ASSERT_INT(right);
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
    ASSERT_INT(v);
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

static Value numNeg(Value v) {
    if (IS_IRRATIONAL(v)) {
        v.val.irrational = -v.val.irrational;
        return v;
    } else {
        return intNeg(v);
    }
}

static bool intIsNeg(Value v) {
    if (IS_BIGINT(v)) {
        return isNegBigInt(v.val.bigint);
    } else {
        return v.val.stdint < 0;
    }
}

////////////////////////
// bigint operations
////////////////////////

static inline Cmp bigCmp(Value left, Value right) {
    ASSERT_BIGINT(left);
    ASSERT_BIGINT(right);
    return cmpBigInt(left.val.bigint, right.val.bigint);
}

////////////////////////
// stdint operations
////////////////////////

static inline Cmp stdCmp(Value left, Value right) {
    ASSERT_STDINT(left);
    ASSERT_STDINT(right);
    return left.val.stdint < right.val.stdint ? CMP_LT :
           left.val.stdint == right.val.stdint ? CMP_EQ : CMP_GT;

}

////////////////////////
// rational operations
////////////////////////

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
    Cmp res = numCmp(ad, bc);
    LEAVE(ratCmp);
    UNPROTECT(save);
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
            right = ratValue(right, One);
            protectValue(right);
            res = ratOp(left, right, op, intOp, false);
            protectValue(res);
        }
    } else if (right.type == VALUE_TYPE_RATIONAL) {
        // only right rational
        left = ratValue(left, One);
        protectValue(left);
        res = ratOp(left, right, op, intOp, false);
        protectValue(res);
    } else {
        // neither rational
        eprintf("simplify\n");
        if (simplify) {
            res = intOp(left, right);
            protectValue(res);
        } else {
            left = ratValue(left, One);
            protectValue(left);
            right = ratValue(right, One);
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
    if (numCmp(gcd, One) != CMP_EQ) {
        numerator = basicIntDiv(numerator, gcd);
        protectValue(numerator);
        denominator = basicIntDiv(denominator, gcd);
        protectValue(denominator);
    }
    if (intIsNeg(denominator)) {
        intNegInPlace(&numerator);
        intNegInPlace(&denominator);
    }
    if (numCmp(denominator, One) == CMP_EQ) {
        res = numerator;
    } else {
        res = ratValue(numerator, denominator);
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

static Value ratDiv3(IntegerBinOp base_op, Value left, Value right) {
    ENTER(ratDiv3);
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value newRight = ratValue(right.val.vec->values[DENOMINATOR], right.val.vec->values[NUMERATOR]);
    int save = protectValue(newRight);
    Value res = rat_ac_bd(base_op, left, newRight);
    protectValue(res);
    LEAVE(ratDiv3);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratDiv(Value left, Value right) {
    return ratDiv3(intMul, left, right);
}

static Value ratPow(Value left, Value right) {
    ENTER(ratPow);
    ASSERT_RATIONAL(left);
    ASSERT_INT(right);
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

static Value ratMod(Value left, Value right) {
    ENTER(ratMod);
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value res = ratOp(left, right, rat_ad_bc_cd, intMod, true);
    LEAVE(ratMod);
    return res;
}

static Value ratMul(Value left, Value right) {
    return ratOp(left, right, rat_ac_bd, intMul, true);
}

static Value intDiv(Value left, Value right) {
    // N.B. intMul not basicIntDiv
    return ratOp(left, right, ratDiv3, intMul, false);
}

static Value ratSub(Value left, Value right) {
    return ratOp(left, right, rat_ad_bc_cd, intSub, true);
}

static Value ratAdd(Value left, Value right) {
    return ratOp(left, right, rat_ad_bc_cd, intAdd, true);
}

//////////////////////////
// irrational operations
//////////////////////////

static inline Cmp irrCmp(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return left.val.irrational < right.val.irrational ? CMP_LT :
           left.val.irrational == right.val.irrational ? CMP_EQ : CMP_GT;
}

static Value irrMod(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return irrValue(fmod(left.val.irrational, right.val.irrational));
}

static Value irrMul(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return irrValue(left.val.irrational * right.val.irrational);
}

static Value irrDiv(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return irrValue(left.val.irrational / right.val.irrational);
}

static Value irrSub(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return irrValue(left.val.irrational - right.val.irrational);
}

static Value irrAdd(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return irrValue(left.val.irrational * right.val.irrational);
}

////////////////////////
// generic operations
////////////////////////

#ifdef SAFETY_CHECKS
#  define CHECK_INITIALIZED() do { \
    if (!arithmetic_initialized) { \
        cant_happen("arithmetic not initialized yet"); \
    } \
} while(0)
#else
#  define CHECK_INITIALIZED()
#endif

typedef Value (*ValOp)(Value, Value);

static Value dispatch(Value left, Value right, ValOp intOp, ValOp bigOp, ValOp ratOp, ValOp irrOp) {
    ENTER(dispatch);
    int save = PROTECT(NULL);
    Value res;
    switch (promote(&left, &right)) {
        case VALUE_TYPE_RATIONAL:
            protectValue(left);
            res = ratOp(left, right);
            break;
        case VALUE_TYPE_RATIONAL + RIGHT_INDICATOR:
            protectValue(right);
            res = ratOp(left, right);
            break;
        case VALUE_TYPE_IRRATIONAL:
            res = irrOp(left, right);
            break;
        case VALUE_TYPE_STDINT:
            res = intOp(left, right);
            break;
        case VALUE_TYPE_BIGINT:
            protectValue(left);
            res = bigOp(left, right);
            break;
        case VALUE_TYPE_BIGINT + RIGHT_INDICATOR:
            protectValue(left);
            res = bigOp(left, right);
            break;
        default:
            cant_happen("unexpected result from promote");
    }
    LEAVE(dispatch);
    UNPROTECT(save);
    return res;
}

Value nadd(Value left, Value right) {
    ENTER(nadd);
    CHECK_INITIALIZED();
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = dispatch(left, right, intAdd, intAdd, ratAdd, irrAdd);
    LEAVE(nadd);
    return res;
}

Value nsub(Value left, Value right) {
    ENTER(nsub);
    CHECK_INITIALIZED();
    Value res = dispatch(left, right, intSub, intSub, ratSub, irrSub);
    LEAVE(nsub);
    return res;
}

Value nmul(Value left, Value right) {
    ENTER(nmul);
    CHECK_INITIALIZED();
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = dispatch(left, right, intMul, intMul, ratMul, irrMul);
    int save = protectValue(res);
    LEAVE(nmul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Value ndiv(Value left, Value right) {
    ENTER(ndiv);
    CHECK_INITIALIZED();
    Value res = dispatch(left, right, intDiv, intDiv, ratDiv, irrDiv);
    LEAVE(ndiv);
    IFDEBUG(ppNumber(res));
    return res;
}

Value nmod(Value left, Value right) {
    ENTER(nmod);
    CHECK_INITIALIZED();
    Value res = dispatch(left, right, intMod, intMod, ratMod, irrMod);
    LEAVE(nmod);
    return res;
}

Value npow(Value left, Value right) {
    ENTER(npow);
    CHECK_INITIALIZED();
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = PROTECT(NULL);
    switch(left.type) {
        case VALUE_TYPE_RATIONAL:
            switch(right.type) {
                case VALUE_TYPE_RATIONAL:
                    left = rational_to_irrational(&left);
                    right = rational_to_irrational(&right);
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    left = rational_to_irrational(&left);
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = ratPow(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %d", right.type);
            }
            break;
        case VALUE_TYPE_IRRATIONAL:
            switch(right.type) {
                case VALUE_TYPE_RATIONAL:
                    right = rational_to_irrational(&right);
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    right = int_to_irrational(&right);
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                default:
                    cant_happen("unrecognised right number type %d", right.type);
            }
            break;
        case VALUE_TYPE_BIGINT:
            switch(right.type) {
                case VALUE_TYPE_RATIONAL:
                    left = int_to_irrational(&left);
                    right = rational_to_irrational(&right);
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    left = int_to_irrational(&left);
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = intPow(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %d", right.type);
            }
            break;
        case VALUE_TYPE_STDINT:
            switch(right.type) {
                case VALUE_TYPE_RATIONAL:
                    left = int_to_irrational(&left);
                    right = rational_to_irrational(&right);
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    left = int_to_irrational(&left);
                    res = irrValue(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = intPow(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %d", right.type);
            }
            break;
        default:
            cant_happen("unrecognised left number type %d", left.type);
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
    switch (promote(&left, &right)) {
        case VALUE_TYPE_RATIONAL:
            protectValue(left);
            res = ratCmp(left, right);
            break;
        case VALUE_TYPE_RATIONAL + RIGHT_INDICATOR:
            protectValue(right);
            res = ratCmp(left, right);
            break;
        case VALUE_TYPE_IRRATIONAL:
            res = irrCmp(left, right);
            break;
        case VALUE_TYPE_STDINT:
            res = stdCmp(left, right);
            break;
        case VALUE_TYPE_BIGINT:
            protectValue(left);
            res = bigCmp(left, right);
            break;
        case VALUE_TYPE_BIGINT + RIGHT_INDICATOR:
            protectValue(left);
            res = bigCmp(left, right);
            break;
        default:
            cant_happen("unexpected result from promote");
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
        res = ratValue(numerator, v.val.vec->values[DENOMINATOR]);
        UNPROTECT(save);
    } else {
        res = numNeg(v);
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
