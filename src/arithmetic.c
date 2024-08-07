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
#include "types.h"

#ifdef DEBUG_ARITHMETIC
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

// indexes into vec
#define NUMERATOR 0
#define DENOMINATOR 1
#define REAL 0
#define IMAG 1

#define IS_COMPLEX(x) ((x).type == VALUE_TYPE_COMPLEX)
#define IS_BIGINT(x) ((x).type == VALUE_TYPE_BIGINT)
#define IS_BIGINT_IMAG(x) ((x).type == VALUE_TYPE_BIGINT_IMAG)
#define IS_IRRATIONAL(x) ((x).type == VALUE_TYPE_IRRATIONAL)
#define IS_RATIONAL(x) ((x).type == VALUE_TYPE_RATIONAL)
#define IS_STDINT(x) ((x).type == VALUE_TYPE_STDINT)
#define IS_STDINT_IMAG(x) ((x).type == VALUE_TYPE_STDINT_IMAG)
#define IS_INT(x) (IS_STDINT(x) || IS_BIGINT(x))
#define IS_IMAGINT(x) (IS_STDINT_IMAG(x) || IS_BIGINT_IMAG(x))
#define IS_RATIONAL_OR_INT(x) ((x).type == VALUE_TYPE_RATIONAL || IS_INT(x))
#define IS_REAL(x) (IS_RATIONAL_OR_INT(x) || IS_IRRATIONAL(x))
#define IS_NOT_REAL(x) (!IS_REAL(x))

#ifdef SAFETY_CHECKS
#  define ASSERT_COMPLEX(x) ASSERT(IS_COMPLEX(x))
#  define ASSERT_RATIONAL(x) ASSERT(IS_RATIONAL(x))
#  define ASSERT_IRRATIONAL(x) ASSERT(IS_IRRATIONAL(x))
#  define ASSERT_BIGINT(x) ASSERT(IS_BIGINT(x))
#  define ASSERT_STDINT(x) ASSERT(IS_STDINT(x))
#  define ASSERT_INT(x) ASSERT(IS_INT(x))
#  define ASSERT_RATIONAL_OR_INT(x) ASSERT(IS_RATIONAL_OR_INT(x))
#  define ASSERT_REAL(x) ASSERT(IS_REAL(x))
#  define ASSERT_NOT_REAL(x) ASSERT(IS_NOT_REAL(x))
#else
#  define ASSERT_COMPLEX(x)
#  define ASSERT_RATIONAL(x)
#  define ASSERT_IRRATIONAL(x)
#  define ASSERT_BIGINT(x)
#  define ASSERT_STDINT(x)
#  define ASSERT_INT(x)
#  define ASSERT_RATIONAL_OR_INT(x)
#  define ASSERT_REAL(x)
#  define ASSERT_NOT_REAL(x)
#endif

typedef Value (*ValOp)(Value, Value);
typedef Value (*IntegerBinOp)(Value, Value);
typedef Value (*ParameterizedBinOp)(IntegerBinOp, Value, Value);

static bool arithmetic_initialized = false;

static Value One = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(1)
};

static Value Zero = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(0)
};

static Value ratSimplify(Value numerator, Value denominator);
static Value comMag(Value v);
static Value comTheta(Value v);
static Value imag_to_real(Value v);

// be careful with this, printing a bigint can cause a GC
// so make sure everything is protected before calling.
static void ppNumber(Value number)__attribute__((unused));

static void ppNumber(Value number) {
    switch (number.type) {
        case VALUE_TYPE_STDINT:
            eprintf("%d", number.val.stdint);
            break;
        case VALUE_TYPE_STDINT_IMAG:
            eprintf("%di", number.val.stdint);
            break;
        case VALUE_TYPE_BIGINT:
            eprintf("[");
            fprintBigInt(errout, number.val.bigint);
            eprintf("]");
            break;
        case VALUE_TYPE_BIGINT_IMAG:
            eprintf("[");
            fprintBigInt(errout, number.val.bigint);
            eprintf("]i");
            break;
        case VALUE_TYPE_RATIONAL:
            ppNumber(number.val.vec->entries[0]);
            eprintf("/");
            ppNumber(number.val.vec->entries[1]);
            break;
        case VALUE_TYPE_RATIONAL_IMAG:
            eprintf("(");
            ppNumber(number.val.vec->entries[0]);
            eprintf("/");
            ppNumber(number.val.vec->entries[1]);
            eprintf(")i");
            break;
        case VALUE_TYPE_IRRATIONAL:
            eprintf("%f", number.val.irrational);
            break;
        case VALUE_TYPE_IRRATIONAL_IMAG:
            eprintf("%fi", number.val.irrational);
            break;
        case VALUE_TYPE_COMPLEX:
            ppNumber(number.val.vec->entries[0]);
            eprintf("+");
            ppNumber(number.val.vec->entries[1]);
            break;
        default:
            eprintf("??? %d ???", number.type);
    }
}

static inline Value realPart(Value v) {
    return v.val.vec->entries[REAL];
}

static inline Value imagPart(Value v) {
    return v.val.vec->entries[IMAG];
}

static inline void extractFromComplexArg(Value *a, Value *b, Value v) {
    ASSERT_COMPLEX(v);
    *a = realPart(v);
    *b = imag_to_real(imagPart(v));
}

static inline void extractFromComplexArgs(Value *a, Value *b, Value *c, Value *d, Value left, Value right) {
    extractFromComplexArg(a, b, left);
    extractFromComplexArg(c, d, right);
}

static inline Value denominatorPart(Value v) {
    return v.val.vec->entries[DENOMINATOR];
}

static inline Value numeratorPart(Value v) {
    return v.val.vec->entries[NUMERATOR];
}

static Value ratValue(Value numerator, Value denominator) {
    Vec *vec = newVec(2);
    // PROTECT(vec);
    vec->entries[NUMERATOR] = numerator;
    vec->entries[DENOMINATOR] = denominator;
    Value res = value_Rational(vec);
    return res;
}

static Value comValue(Value real, Value imag) {
    Vec *vec = newVec(2);
    // PROTECT(vec);
    vec->entries[REAL] = real;
    vec->entries[IMAG] = imag;
    Value res = value_Complex(vec);
    return res;
}

static Value int_to_irrational(Value integer) {
    ASSERT_INT(integer);
    if (integer.type == VALUE_TYPE_BIGINT) {
        return value_Irrational(bigIntToDouble(integer.val.bigint));
    } else {
        return value_Irrational(integer.val.stdint);
    }
}

static Value rational_to_irrational(Value rational) {
    ASSERT_RATIONAL(rational);
    Value num = numeratorPart(rational);
    Value numerator = int_to_irrational(num);
    Value denom = denominatorPart(rational);
    Value denominator = int_to_irrational(denom);
    return value_Irrational(numerator.val.irrational / denominator.val.irrational);
}

static Value to_irrational(Value v) {
    switch (v.type) {
        case VALUE_TYPE_STDINT:
        case VALUE_TYPE_BIGINT:
            return int_to_irrational(v);
        case VALUE_TYPE_RATIONAL:
            return rational_to_irrational(v);
        case VALUE_TYPE_IRRATIONAL:
            return v;
        default:
            cant_happen("invalid type %s", valueTypeName(v.type));
    }
    return v;
}

static Value int_to_rational(Value integer) {
    ASSERT_INT(integer);
    Value one = value_Stdint(1);
    return ratValue(integer, one);
}

static Value bigint_to_irrational(Value v) {
    ASSERT_BIGINT(v);
    return value_Irrational(bigIntToDouble(v.val.bigint));
}

static Value int_to_bigint(Value v) {
    ASSERT_STDINT(v);
    return value_Bigint(bigIntFromInt(v.val.stdint));
}

static Value real_to_complex(Value real) {
    Value imag = value_Stdint_imag(0);
    return comValue(real, imag);
}

static Value imag_to_complex(Value imag) {
    Value real = value_Stdint(0);
    return comValue(real, imag);
}

static Value to_complex(Value v) {
    switch (v.type) {
        case VALUE_TYPE_STDINT:
        case VALUE_TYPE_BIGINT:
        case VALUE_TYPE_RATIONAL:
        case VALUE_TYPE_IRRATIONAL:
            return real_to_complex(v);
        case VALUE_TYPE_STDINT_IMAG:
        case VALUE_TYPE_BIGINT_IMAG:
        case VALUE_TYPE_RATIONAL_IMAG:
        case VALUE_TYPE_IRRATIONAL_IMAG:
            return imag_to_complex(v);
        case VALUE_TYPE_COMPLEX:
            return v;
        default:
            cant_happen("invalid type %s", valueTypeName(v.type));
    }
}

// cast imaginary to real for basic arithmetic
static Value imag_to_real(Value v) {
    switch (v.type) {
        case VALUE_TYPE_STDINT_IMAG:
            v.type = VALUE_TYPE_STDINT;
            break;
        case VALUE_TYPE_BIGINT_IMAG:
            v.type = VALUE_TYPE_BIGINT;
            break;
        case VALUE_TYPE_RATIONAL_IMAG:
            v.type = VALUE_TYPE_RATIONAL;
            break;
        case VALUE_TYPE_IRRATIONAL_IMAG:
            v.type = VALUE_TYPE_IRRATIONAL;
            break;
        default:
            cant_happen("invalid imaginary type %s", valueTypeName(v.type));
    }
    return v;
}

static Value real_to_imag(Value v) {
    switch (v.type) {
        case VALUE_TYPE_STDINT:
            v.type = VALUE_TYPE_STDINT_IMAG;
            break;
        case VALUE_TYPE_BIGINT:
            v.type = VALUE_TYPE_BIGINT_IMAG;
            break;
        case VALUE_TYPE_RATIONAL:
            v.type = VALUE_TYPE_RATIONAL_IMAG;
            break;
        case VALUE_TYPE_IRRATIONAL:
            v.type = VALUE_TYPE_IRRATIONAL_IMAG;
            break;
        default:
            cant_happen("invalid real type %s", valueTypeName(v.type));
    }
    return v;
}

static Integer rec_to_polar(Value com, Value *r, Value *theta) {
    ASSERT_COMPLEX(com);
    *r = comMag(com);
    Integer save = protectValue(*r);
    *theta = comTheta(com);
    protectValue(*theta);
    return save;
}

static bool intIsNeg(Value v) {
    ASSERT_INT(v);
    if (IS_BIGINT(v)) {
        return isNegBigInt(v.val.bigint);
    } else {
        return v.val.stdint < 0;
    }
}

static bool ratIsNeg(Value v) {
    ASSERT_RATIONAL(v);
    return intIsNeg(numeratorPart(v));
}

static bool irratIsNeg(Value v) {
    ASSERT_IRRATIONAL(v);
    return v.val.irrational < 0.0;
}

static bool isNeg(Value v) {
    switch (v.type) {
        case VALUE_TYPE_STDINT:
        case VALUE_TYPE_BIGINT:
            return intIsNeg(v);
        case VALUE_TYPE_RATIONAL:
            return ratIsNeg(v);
        case VALUE_TYPE_IRRATIONAL:
            return irratIsNeg(v);
        default:
            cant_happen("invalid real type %s", valueTypeName(v.type));
    }
}

static bool intIsEven(Value v) {
    ASSERT_INT(v);
    if (IS_BIGINT(v)) {
        return isEvenBigInt(v.val.bigint);
    } else {
        return (v.val.stdint & 1) == 0;
    }
}

// RULE: rationals contain only plain integers, big or little.
// rationals can NOT contain imaginary numbers even imaginary integers.
// instead rationals can BE imaginary.

static Integer coerce(Value *left, Value *right, int *save) {
    *save = PROTECT(NULL);
    switch(left->type) {
        case VALUE_TYPE_RATIONAL:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                    return VALUE_TYPE_RATIONAL;
                case VALUE_TYPE_IRRATIONAL:
                    *left = rational_to_irrational(*left);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_BIGINT:
                    *right = int_to_rational(*right);
                    *save = protectValue(*right);
                    return VALUE_TYPE_RATIONAL;
                case VALUE_TYPE_STDINT:
                    *right = int_to_rational(*right);
                    *save = protectValue(*right);
                    return VALUE_TYPE_RATIONAL;
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                    *left = real_to_complex(*left);
                    *save = protectValue(*left);
                    *right = imag_to_complex(*right);
                    protectValue(*right);
                    return VALUE_TYPE_COMPLEX;
                case VALUE_TYPE_COMPLEX:
                    *left = real_to_complex(*left);
                    *save = protectValue(*left);
                    return VALUE_TYPE_COMPLEX;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right->type));
            }
            break;
        case VALUE_TYPE_IRRATIONAL:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                    *right = rational_to_irrational(*right);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_IRRATIONAL:
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_BIGINT:
                    *right = bigint_to_irrational(*right);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_STDINT:
                    *right = int_to_irrational(*right);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                    *left = real_to_complex(*left);
                    *save = protectValue(*left);
                    *right = imag_to_complex(*right);
                    protectValue(*right);
                    return VALUE_TYPE_COMPLEX;
                case VALUE_TYPE_COMPLEX:
                    *left = real_to_complex(*left);
                    *save = protectValue(*left);
                    return VALUE_TYPE_COMPLEX;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right->type));
            }
            break;
        case VALUE_TYPE_BIGINT:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                    *left = int_to_rational(*left);
                    return VALUE_TYPE_RATIONAL;
                case VALUE_TYPE_IRRATIONAL:
                    *left = bigint_to_irrational(*left);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_BIGINT:
                    return VALUE_TYPE_BIGINT;
                case VALUE_TYPE_STDINT:
                    *right = int_to_bigint(*right);
                    *save = protectValue(*right);
                    return VALUE_TYPE_BIGINT;
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                    *left = real_to_complex(*left);
                    *save = protectValue(*left);
                    *right = imag_to_complex(*right);
                    protectValue(*right);
                    return VALUE_TYPE_COMPLEX;
                case VALUE_TYPE_COMPLEX:
                    *left = real_to_complex(*left);
                    *save = protectValue(*left);
                    return VALUE_TYPE_COMPLEX;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right->type));
            }
            break;
        case VALUE_TYPE_STDINT:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                    *left = int_to_rational(*left);
                    return VALUE_TYPE_RATIONAL;
                case VALUE_TYPE_IRRATIONAL:
                    *left = int_to_irrational(*left);
                    return VALUE_TYPE_IRRATIONAL;
                case VALUE_TYPE_BIGINT:
                    *left = int_to_bigint(*left);
                    return VALUE_TYPE_BIGINT;
                case VALUE_TYPE_STDINT:
                    return VALUE_TYPE_STDINT;
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                    *left = real_to_complex(*left);
                    *save = protectValue(*left);
                    *right = imag_to_complex(*right);
                    protectValue(*right);
                    return VALUE_TYPE_COMPLEX;
                case VALUE_TYPE_COMPLEX:
                    *left = real_to_complex(*left);
                    *save = protectValue(*left);
                    return VALUE_TYPE_COMPLEX;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right->type));
            }
            break;
        case VALUE_TYPE_STDINT_IMAG:
        case VALUE_TYPE_BIGINT_IMAG:
        case VALUE_TYPE_RATIONAL_IMAG:
        case VALUE_TYPE_IRRATIONAL_IMAG:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                case VALUE_TYPE_IRRATIONAL:
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    *left = imag_to_complex(*left);
                    *save = protectValue(*left);
                    *right = real_to_complex(*right);
                    protectValue(*right);
                    return VALUE_TYPE_COMPLEX;
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                    *left = imag_to_complex(*left);
                    *save = protectValue(*left);
                    *right = imag_to_complex(*right);
                    protectValue(*right);
                    return VALUE_TYPE_COMPLEX;
                case VALUE_TYPE_COMPLEX:
                    *left = imag_to_complex(*left);
                    *save = protectValue(*left);
                    return VALUE_TYPE_COMPLEX;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right->type));
            }
            break;
        case VALUE_TYPE_COMPLEX:
            switch(right->type) {
                case VALUE_TYPE_RATIONAL:
                case VALUE_TYPE_IRRATIONAL:
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    *right = real_to_complex(*right);
                    *save = protectValue(*right);
                    return VALUE_TYPE_COMPLEX;
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                    *right = imag_to_complex(*right);
                    *save = protectValue(*right);
                    return VALUE_TYPE_COMPLEX;
                case VALUE_TYPE_COMPLEX:
                    return VALUE_TYPE_COMPLEX;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right->type));
            }
            break;
        default:
            cant_happen("unrecognised left number type %s", valueTypeName(left->type));
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

static Value safe_add(Integer a, Integer b) {
    Integer c;
    if (__builtin_add_overflow(a, b, &c)) {
        BigInt *big = bigIntFromAddition(a, b);
        int save = PROTECT(big);
        Value res = value_Bigint(big);
        UNPROTECT(save);
        return res;
    } else {
        return value_Stdint(c);
    }
}

static Value intAdd(Value left, Value right) {
    ENTER(intAdd);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *b = addBigInt(left.val.bigint, right.val.bigint);
            PROTECT(b);
            res = value_Bigint(b);
            protectValue(res);
        } else {
            BigInt *b = addBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(b);
            res = value_Bigint(b);
            protectValue(res);
        }
    } else {
        if (IS_BIGINT(right)) {
            BigInt *b = addBigIntInt(right.val.bigint, left.val.stdint);
            PROTECT(b);
            res = value_Bigint(b);
            protectValue(res);
        } else {
            res = safe_add(left.val.stdint, right.val.stdint);
            protectValue(res);
        }
    }
    LEAVE(intAdd);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value safe_mul(Integer a, Integer b) {
    Integer c;
    if (__builtin_mul_overflow(a, b, &c)) {
        BigInt *big = bigIntFromMultiplication(a, b);
        int save = PROTECT(big);
        Value res = value_Bigint(big);
        UNPROTECT(save);
        return res;
    } else {
        return value_Stdint(c);
    }
}

static Value intMul(Value left, Value right) {
    ENTER(intMul);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *bi = mulBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            BigInt *bi = mulBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        }
    } else {
        if (IS_BIGINT(right)) {
            BigInt *bi = mulBigIntInt(right.val.bigint, left.val.stdint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            res = safe_mul(left.val.stdint, right.val.stdint);
           protectValue(res);
        }
    }
    LEAVE(intMul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value safe_sub(Integer a, Integer b) {
    Integer c;
    if (__builtin_sub_overflow(a, b, &c)) {
        BigInt *big = bigIntFromSubtraction(a, b);
        int save = PROTECT(big);
        Value res = value_Bigint(big);
        protectValue(res);
        IFDEBUG(eprintf("SUB OVERFLOW"));
        IFDEBUG(ppNumber(res));
        UNPROTECT(save);
        return res;
    } else {
        return value_Stdint(c);
    }
}

static Value intSub(Value left, Value right) {
    ENTER(intSub);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *bi = subBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            BigInt *bi = subBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        }
    } else {
        if (IS_BIGINT(right)) {
            BigInt *bi = subIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            res = safe_sub(left.val.stdint, right.val.stdint);
            protectValue(res);
        }
    }
    LEAVE(intSub);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value basicIntDiv(Value left, Value right) {
    ENTER(basicIntDiv);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            if (int_cmp_bb(right, Zero) == CMP_EQ) {
                cant_happen("attempted div zero");
            }
            BigInt *bi = divBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            if (right.val.stdint == 0) {
                cant_happen("attempted div zero");
            }
            BigInt *bi = divBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        }
    } else {
        if (IS_BIGINT(right)) {
            if (int_cmp_bb(right, Zero) == CMP_EQ) {
                cant_happen("attempted div zero");
            }
            BigInt *bi = divIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            if (right.val.stdint == 0) {
                cant_happen("attempted div zero");
            }
            // division can't overflow
            res = value_Stdint(left.val.stdint / right.val.stdint);
            protectValue(res);
        }
    }
    LEAVE(basicIntDiv);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value safe_powf(Integer a, Integer b) {
    float f = powf((float) a, (float) b);
    if (f == HUGE_VALF || f > (float)INT_MAX || f < (float)INT_MIN) {
        BigInt *big = bigIntFromPower(a, b);
        int save = PROTECT(big);
        Value res = value_Bigint(big);
        protectValue(res);
        UNPROTECT(save);
        return res;
    } else {
        return value_Stdint((Integer) f);
    }
}

static Value irratSimplify(Double result) {
    Value res;
    int save = PROTECT(NULL);
    if(fmod(result, 1.0) == 0.0) {
        if (result > (Double)INT_MAX || result < (Double)INT_MIN) {
            // FIXME need doubleToBigInt
            res = value_Irrational(result);
            protectValue(res);
        } else {
            res = value_Stdint((Integer) result);
            protectValue(res);
        }
    } else {
        res = value_Irrational(result);
        protectValue(res);
    }
    UNPROTECT(save);
    return res;
}

// raise a real number to a rational power
static Value realPowRat(Value base, Value exponent) {
    ENTER(realPowRat);
    IFDEBUG(ppNumber(base));
    IFDEBUG(ppNumber(exponent));
    ASSERT_RATIONAL(exponent);
    Value res;
    int save = PROTECT(NULL);
    if (isNeg(base)) {
        Value pos = nmul(base, value_Stdint(-1)); // make the base positive
        protectValue(pos);
        Value neg = realPowRat(pos, exponent); // recurse on positive base
        protectValue(neg);
        if (intIsEven(denominatorPart(exponent))) {
            res = nmul(neg, value_Stdint_imag(1)); // return imaginary number
            protectValue(res);
        } else {
            res = nmul(neg, value_Stdint(-1)); // return the negation
            protectValue(res);
        }
    } else if (ratIsNeg(exponent)) {
        Value pos = nmul(exponent, value_Stdint(-1)); // make the exponent positive
        protectValue(pos);
        Value inv = realPowRat(base, pos); // recurse on positive base, positive exponent
        protectValue(inv);
        res = ndiv(value_Stdint(1), inv); // return the inverse
        protectValue(res);
    } else {
        if (IS_RATIONAL(base)) { // attempt to preserve
            Value num = realPowRat(numeratorPart(base), exponent);
            protectValue(num);
            Value denom = realPowRat(denominatorPart(base), exponent);
            protectValue(denom);
            if (IS_INT(num) && IS_INT(denom)) {
                res = ratSimplify(num, denom);
                protectValue(res);
            } else {
                num = to_irrational(num);
                protectValue(num);
                denom = to_irrational(denom);
                protectValue(denom);
                Double result = num.val.irrational / denom.val.irrational;
                res = irratSimplify(result);
                protectValue(res);
            }
        } else {
            Value fbase = to_irrational(base);
            protectValue(fbase);
            Value fexponent = rational_to_irrational(exponent);
            protectValue(fexponent);
            Double result = pow(fbase.val.irrational, fexponent.val.irrational);
            IFDEBUG(eprintf("doing pow(%f, %f) = %f", fbase.val.irrational, fexponent.val.irrational, result));
            res = irratSimplify(result);
            protectValue(res);
        }
    }
    LEAVE(realPowRat);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value intPow(Value left, Value right) {
    ENTER(intPow);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = PROTECT(NULL);
    switch (left.type) {
        case VALUE_TYPE_BIGINT:
            switch (right.type) {
                case VALUE_TYPE_BIGINT: {
                    BigInt *bi = powBigInt(left.val.bigint, right.val.bigint);
                    PROTECT(bi);
                    res = value_Bigint(bi);
                    protectValue(res);
                }
                break;
                case VALUE_TYPE_STDINT: {
                    BigInt *bi = powBigIntInt(left.val.bigint, right.val.stdint);
                    PROTECT(bi);
                    res = value_Bigint(bi);
                    protectValue(res);
                }
                break;
                case VALUE_TYPE_RATIONAL: {
                    res = realPowRat(left, right);
                    protectValue(res);
                }
                break;
                default:
                    cant_happen("invalid rhs arg to intPow %s", valueTypeName(left.type));
            }
            break;
        case VALUE_TYPE_STDINT:
            switch (right.type) {
                case VALUE_TYPE_BIGINT: {
                    BigInt *bi = powIntBigInt(left.val.stdint, right.val.bigint);
                    PROTECT(bi);
                    res = value_Bigint(bi);
                    protectValue(res);
                }
                break;
                case VALUE_TYPE_STDINT: {
                    res = safe_powf(left.val.stdint, right.val.stdint);
                    protectValue(res);
                }
                break;
                case VALUE_TYPE_RATIONAL: {
                    res = realPowRat(left, right);
                    protectValue(res);
                }
                break;
                default:
                    cant_happen("invalid rhs arg to intPow %s", valueTypeName(left.type));
            }
            break;
        default:
            cant_happen("invalid lhs arg to intPow %s", valueTypeName(left.type));
    }
    LEAVE(intPow);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value intMod(Value left, Value right) {
    ENTER(intMod);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
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
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            if (right.val.stdint == 0) {
                cant_happen("attempted mod zero");
            }
            BigInt *bi = modBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        }
    } else {
        if (IS_BIGINT(right)) {
            if (int_cmp_bb(right, Zero) == CMP_EQ) {
                cant_happen("attempted mod zero");
            }
            BigInt *bi = modIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            if (right.val.stdint == 0) {
                cant_happen("attempted mod zero");
            }
            // modulus can't overflow
            res = value_Stdint(left.val.stdint % right.val.stdint);
            protectValue(res);
        }
    }
    LEAVE(intMod);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Integer gcd (Integer a, Integer b) {
	Integer i = 0, min_num = a, gcd = 1;
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
    ENTER(intGcd);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res;
    int save = PROTECT(NULL);
    if (IS_BIGINT(left)) {
        if (IS_BIGINT(right)) {
            BigInt *bi = gcdBigInt(left.val.bigint, right.val.bigint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            BigInt *bi = gcdBigIntInt(left.val.bigint, right.val.stdint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        }
    } else {
        if (IS_BIGINT(left)) {
            BigInt *bi = gcdIntBigInt(left.val.stdint, right.val.bigint);
            PROTECT(bi);
            res = value_Bigint(bi);
            protectValue(res);
        } else {
            res = value_Stdint(gcd(left.val.stdint, right.val.stdint));
            protectValue(res);
        }
    }
    LEAVE(intGcd);
    IFDEBUG(ppNumber(res));
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
        v = value_Bigint(bi);
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
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value ad = intMul(numeratorPart(left), denominatorPart(right));
    int save = protectValue(ad);
    Value bc = intMul(denominatorPart(left), numeratorPart(right));
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
            right = int_to_rational(right);
            protectValue(right);
            res = ratOp(left, right, op, intOp, false);
            protectValue(res);
        }
    } else if (right.type == VALUE_TYPE_RATIONAL) {
        // only right rational
        left = int_to_rational(left);
        protectValue(left);
        res = ratOp(left, right, op, intOp, false);
        protectValue(res);
    } else {
        // neither rational
        if (simplify) {
            res = intOp(left, right);
            protectValue(res);
        } else {
            left = int_to_rational(left);
            protectValue(left);
            right = int_to_rational(right);
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
static Value rat_ad_bc_bd(IntegerBinOp base_op, Value left, Value right) {
    ENTER(rat_ad_bc_bd);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value a1b2 =
        intMul(numeratorPart(left), denominatorPart(right));
    int save = protectValue(a1b2);
    Value a2b1 =
        intMul(denominatorPart(left), numeratorPart(right));
    protectValue(a2b1);
    Value numerator = base_op(a1b2, a2b1);
    protectValue(numerator);
    Value denominator =
        intMul(left.val.vec->entries[DENOMINATOR],
                right.val.vec->entries[DENOMINATOR]);
    protectValue(denominator);
    Value res = ratSimplify(numerator, denominator);
    protectValue(res);
    LEAVE(rat_ad_bc_bd);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

// a/b o c/d = ac o bd
static Value rat_ac_bd(IntegerBinOp base_op, Value left, Value right) {
    ENTER(rat_ac_bd);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value numerator =
        base_op(numeratorPart(left), numeratorPart(right));
    int save = protectValue(numerator);
    Value denominator =
        base_op(denominatorPart(left), denominatorPart(right));
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
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value inverse = ratValue(denominatorPart(right), numeratorPart(right));
    int save = protectValue(inverse);
    Value res = rat_ac_bd(base_op, left, inverse);
    protectValue(res);
    LEAVE(ratDiv3);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratDiv(Value left, Value right) {
    ENTER(ratDiv);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratDiv3(intMul, left, right);
    protectValue(res);
    LEAVE(ratDiv);
    IFDEBUG(ppNumber(res));
    return res;
}

static Value ratPow(Value left, Value right) {
    ENTER(ratPow);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL_OR_INT(right);
    Value numerator = numeratorPart(left);
    Value denominator = denominatorPart(left);
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
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_RATIONAL(left);
    ASSERT_RATIONAL(right);
    Value res = ratOp(left, right, rat_ad_bc_bd, intMod, true);
    int save = protectValue(res);
    LEAVE(ratMod);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratMul(Value left, Value right) {
    ENTER(ratMul);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratOp(left, right, rat_ac_bd, intMul, true);
    int save = protectValue(res);
    LEAVE(ratMul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value intDiv(Value left, Value right) {
    ENTER(intDiv);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratOp(left, right, ratDiv3, intMul, false); // N.B. intMul not basicIntDiv
    int save = protectValue(res);
    LEAVE(intDiv);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratSub(Value left, Value right) {
    ENTER(ratSub);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratOp(left, right, rat_ad_bc_bd, intSub, true);
    int save = protectValue(res);
    LEAVE(ratSub);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value ratAdd(Value left, Value right) {
    ENTER(ratAdd);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = ratOp(left, right, rat_ad_bc_bd, intAdd, true);
    int save = protectValue(res);
    LEAVE(ratAdd);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
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
    return value_Irrational(fmod(left.val.irrational, right.val.irrational));
}

static Value irrMul(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return value_Irrational(left.val.irrational * right.val.irrational);
}

static Value irrDiv(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return value_Irrational(left.val.irrational / right.val.irrational);
}

static Value irrSub(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return value_Irrational(left.val.irrational - right.val.irrational);
}

static Value irrAdd(Value left, Value right) {
    ASSERT_IRRATIONAL(left);
    ASSERT_IRRATIONAL(right);
    return value_Irrational(left.val.irrational + right.val.irrational);
}

// c^(a + bi) = c^a [cos(b ln c) + i sin(b ln c)]
static Value irrPowCom(Value c, Value right) {
    ASSERT_IRRATIONAL(c);
    right = to_complex(right);
    int save = protectValue(right);
    Value a, b;
    extractFromComplexArg(&a, &b, right);
    a = to_irrational(a);
    b = to_irrational(b);
    Value c_a = npow(c, a);
    Double ln_c = log(c.val.irrational);
    Double b_ln_c = b.val.irrational * ln_c;
    Value cos_b_ln_c = value_Irrational(cos(b_ln_c));
    Value i_sin_b_ln_c = value_Irrational_imag(sin(b_ln_c));
    Value com = comValue(cos_b_ln_c, i_sin_b_ln_c);
    protectValue(com);
    Value res = nmul(c_a, com);
    UNPROTECT(save);
    return res;
}

//////////////////////////////
// complex number operations
//////////////////////////////

static Value comSimplify(Value real, Value imag) {
    if (ncmp(value_Stdint(0), imag_to_real(imag)) == CMP_EQ) {
        return real;
    }
    if (ncmp(value_Stdint(0), real) == CMP_EQ) {
        return imag;
    }
    return comValue(real, imag);
}

// (a + bi) + (c + di) = (a + c) + (b + d)i
static Value comAdd(Value left, Value right) {
    ENTER(comAdd);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_COMPLEX(left);
    ASSERT_COMPLEX(right);
    Value a, b, c, d;
    extractFromComplexArgs(&a, &b, &c, &d, left, right);
    Value a_c = nadd(a, c);
    int save = protectValue(a_c);
    Value b_d = nadd(b, d);
    protectValue(b_d);
    Value res = comSimplify(a_c, real_to_imag(b_d));
    protectValue(res);
    LEAVE(comAdd);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

// (a + bi) - (c + di) = (a - c) + (b - d)i
static Value comSub(Value left, Value right) {
    ENTER(comSub);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_COMPLEX(left);
    ASSERT_COMPLEX(right);
    Value a, b, c, d;
    extractFromComplexArgs(&a, &b, &c, &d, left, right);
    Value a_c = nsub(a, c);
    int save = protectValue(a_c);
    Value b_d = nsub(b, d);
    protectValue(b_d);
    Value res = comSimplify(a_c, real_to_imag(b_d));
    protectValue(res);
    LEAVE(comSub);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

// (a + bi)(c + di) = (ac - bd) + (ad + bc)i
static Value comMul(Value left, Value right) {
    ENTER(comMul);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_COMPLEX(left);
    ASSERT_COMPLEX(right);
    Value a, b, c, d;
    extractFromComplexArgs(&a, &b, &c, &d, left, right);
    Value ac = nmul(a, c);
    int save = protectValue(ac);
    Value bd = nmul(b, d);
    protectValue(bd);
    Value ac_bd = nsub(ac, bd);
    protectValue(ac_bd);
    Value ad = nmul(a, d);
    protectValue(ad);
    Value bc = nmul(b, c);
    protectValue(bc);
    Value ad_bc = nadd(ad, bc);
    protectValue(ad_bc);
    Value res = comSimplify(ac_bd, real_to_imag(ad_bc));
    protectValue(res);
    LEAVE(comMul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

// (a + bi) / (c + di) = ((ac + bd) + (bc - ad)i) / (cc + dd)
static Value comDiv(Value left, Value right) {
    ENTER(comMul);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_COMPLEX(left);
    ASSERT_COMPLEX(right);
    Value a, b, c, d;
    extractFromComplexArgs(&a, &b, &c, &d, left, right);
    Value ac = nmul(a, c);
    int save = protectValue(ac);
    Value bd = nmul(b, d);
    protectValue(bd);
    Value bc = nmul(b, c);
    protectValue(bc);
    Value ad = nmul(a, d);
    protectValue(ad);
    Value cc = nmul(c, c);
    protectValue(cc);
    Value dd = nmul(d, d);
    protectValue(dd);
    Value ac_bd = nadd(ac, bd);
    protectValue(ac_bd);
    Value bc_ad = nsub(bc, ad);
    protectValue(bc_ad);
    Value cc_dd = nadd(cc, dd);
    protectValue(cc_dd);
    Value ac_bd_cc_dd = ndiv(ac_bd, cc_dd);
    protectValue(ac_bd_cc_dd);
    Value bc_ad_cc_dd = ndiv(bc_ad, cc_dd);
    protectValue(bc_ad_cc_dd);
    Value bc_ad_cc_dd_i = real_to_imag(bc_ad_cc_dd);
    protectValue(bc_ad_cc_dd_i);
    Value res = comSimplify(ac_bd_cc_dd, bc_ad_cc_dd_i);
    protectValue(res);
    LEAVE(comMul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value comMod(Value left, Value right) {
    ENTER(comMod);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_COMPLEX(left);
    ASSERT_COMPLEX(right);
    Value a, b, c, d;
    extractFromComplexArgs(&a, &b, &c, &d, left, right);
    Value ac = nmod(a, c);
    int save = protectValue(ac);
    Value bd = nmod(b, d);
    protectValue(bd);
    Value bdi = real_to_imag(bd);
    protectValue(bdi);
    Value res = comSimplify(ac, bdi);
    protectValue(res);
    LEAVE(comMod);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value comPow(Value left, Value right) {
    ENTER(comPow);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    ASSERT_INT(right);
    int save;
    Value res;
    if (intIsNeg(right)) {
        Value pos = intNeg(right);
        save = protectValue(pos);
        Value pow = comPow(left, pos);
        protectValue(pow);
        res = ndiv(value_Stdint(1), pow);
        protectValue(res);
    } else {
        res = comValue(value_Stdint(1), value_Stdint_imag(0));
        save = protectValue(res);
        while (ncmp(right, value_Stdint(0)) == CMP_GT) {
            int save2 = protectValue(res);
            right = nsub(right, value_Stdint(1));
            protectValue(right);
            protectValue(res);
            res = nmul(res, left);
            UNPROTECT(save2);
        }
        protectValue(res);
    }
    LEAVE(comPow);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

static Value comMag(Value v) {
    ASSERT_COMPLEX(v);
    Value a, b;
    extractFromComplexArg(&a, &b, v);
    a = npow(a, value_Stdint(2));
    int save = protectValue(a);
    b = npow(b, value_Stdint(2));
    protectValue(b);
    Value sum = nadd(a, b);
    protectValue(sum);
    Value res = npow(sum, value_Irrational(0.5));
    UNPROTECT(save);
    return res;
}

static Value comTheta(Value v) {
    ASSERT_COMPLEX(v);
    Value a, b;
    extractFromComplexArg(&a, &b, v);
    a = to_irrational(a);
    b = to_irrational(b);
    Value res = value_Irrational(atan2(b.val.irrational, a.val.irrational));
    return res;
}

static Value comRoot(Value v, Value n) {
    ASSERT_COMPLEX(v);
    ASSERT_INT(n);
    Value r, theta;
    int save = rec_to_polar(v, &r, &theta);
    Value inv_n = ratValue(value_Stdint(1), n);
    protectValue(inv_n);
    Value r_n = npow(r, inv_n);
    protectValue(r_n);
    Value theta_n = ndiv(theta, n);
    protectValue(theta_n);
    theta_n = to_irrational(theta_n);
    Value cos_theta_n = value_Irrational(cos(theta_n.val.irrational));
    protectValue(cos_theta_n);
    Value i_sin_theta_n = value_Irrational_imag(sin(theta_n.val.irrational));
    protectValue(i_sin_theta_n);
    Value base = comValue(cos_theta_n, i_sin_theta_n);
    protectValue(base);
    Value res = nmul(r_n, base);
    UNPROTECT(save);
    return res;
}

static Value comPowRat(Value com, Value rat) {
    ASSERT_COMPLEX(com);
    ASSERT_RATIONAL(rat);
    Value root = comRoot(com, denominatorPart(rat));
    int save = protectValue(root);
    Value res = comPow(root, numeratorPart(rat));
    UNPROTECT(save);
    return res;
}

// (a + bi)^(c + di) = e^((ln(r) + theta * i) * (c + di))
static Value comPowCom(Value base, Value exponent) {
    ASSERT_NOT_REAL(base);
    ASSERT_NOT_REAL(exponent);
    base = to_complex(base);
    int save = protectValue(base);
    exponent = to_complex(exponent);
    protectValue(exponent);
    Value theta, r;
    rec_to_polar(base, &r, &theta);
    Value irr_r = to_irrational(r);
    Value ln_r = value_Irrational(log(irr_r.val.irrational));
    Value ln_r_i_theta = comValue(ln_r, real_to_imag(theta));
    protectValue(ln_r_i_theta);
    Value prod = nmul(ln_r_i_theta, exponent);
    protectValue(prod);
    Value e = value_Irrational(M_E);
    Value res = npow(e, prod);
    UNPROTECT(save);
    return res;
}

// tie breaker for unequal complex numbers
// *NOT* a general purpose comparison
static Cmp magCmp(Value left_real, Value left_imag, Value right_real, Value right_imag) {
    Value left_c = nadd(left_real, left_imag);
    int save = protectValue(left_c);
    Value right_c = nadd(right_real, right_imag);
    protectValue(right_c);
    Cmp res = CMP_EQ;
    Cmp res1 = ncmp(left_c, right_c);
    switch (res1) {
        case CMP_LT:
            UNPROTECT(save);
            res = CMP_LT;
            break;
        case CMP_EQ: {
            Cmp res2 = ncmp(left_real, right_real);
            UNPROTECT(save);
            // ensures that comparison is order-independant
            res = res2 == CMP_LT ? CMP_LT : CMP_GT;
        }
        break;
        case CMP_GT:
            UNPROTECT(save);
            res = CMP_GT;
            break;
    }
    return res;
}

static Cmp comCmp(Value left, Value right) {
    ASSERT_COMPLEX(left);
    ASSERT_COMPLEX(right);
    Value left_real, left_imag, right_real, right_imag;
    extractFromComplexArg(&left_real, &left_imag, left);
    extractFromComplexArg(&right_real, &right_imag, right);
    Cmp real_cmp = ncmp(left_real, right_real);
    Cmp imag_cmp = ncmp(left_imag, right_imag);
    Cmp res = CMP_EQ;
    switch (real_cmp) {
        case CMP_LT:
            switch (imag_cmp) {
                case CMP_LT:
                case CMP_EQ:
                    res = CMP_LT;
                    break;
                case CMP_GT:
                    res = magCmp(left_real, left_imag, right_real, right_imag);
                    break;
            }
            break;
        case CMP_EQ:
            switch (imag_cmp) {
                case CMP_LT:
                    res = CMP_LT;
                    break;
                case CMP_EQ:
                    res = CMP_EQ;
                    break;
                case CMP_GT:
                    res = CMP_GT;
                    break;
            }
            break;
        case CMP_GT:
            switch (imag_cmp) {
                case CMP_LT:
                    res = magCmp(left_real, left_imag, right_real, right_imag);
                    break;
                case CMP_EQ:
                case CMP_GT:
                    res = CMP_GT;
                    break;
            }
            break;
    }
    return res;
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


static Value dispatch(Value left, Value right, ValOp intOp, ValOp bigOp, ValOp ratOp, ValOp irrOp, ValOp comOp) {
    ENTER(dispatch);
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    int save = PROTECT(NULL);
    Value res;
    switch (coerce(&left, &right, &save)) {
        case VALUE_TYPE_RATIONAL:
            res = ratOp(left, right);
            break;
        case VALUE_TYPE_IRRATIONAL:
            res = irrOp(left, right);
            break;
        case VALUE_TYPE_STDINT:
            res = intOp(left, right);
            break;
        case VALUE_TYPE_BIGINT:
            res = bigOp(left, right);
            break;
        case VALUE_TYPE_COMPLEX:
            res = comOp(left, right);
            break;
        default:
            cant_happen("unexpected result from coerce");
    }
    protectValue(res);
    LEAVE(dispatch);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Value nadd(Value left, Value right) {
    ENTER(nadd);
    CHECK_INITIALIZED();
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = dispatch(left, right, intAdd, intAdd, ratAdd, irrAdd, comAdd);
    int save = protectValue(res);
    LEAVE(nadd);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Value nsub(Value left, Value right) {
    ENTER(nsub);
    CHECK_INITIALIZED();
    Value res = dispatch(left, right, intSub, intSub, ratSub, irrSub, comSub);
    LEAVE(nsub);
    return res;
}

Value nmul(Value left, Value right) {
    ENTER(nmul);
    CHECK_INITIALIZED();
    IFDEBUG(ppNumber(left));
    IFDEBUG(ppNumber(right));
    Value res = dispatch(left, right, intMul, intMul, ratMul, irrMul, comMul);
    int save = protectValue(res);
    LEAVE(nmul);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Value ndiv(Value left, Value right) {
    ENTER(ndiv);
    CHECK_INITIALIZED();
    Value res = dispatch(left, right, intDiv, intDiv, ratDiv, irrDiv, comDiv);
    int save = protectValue(res);
    LEAVE(ndiv);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Value nmod(Value left, Value right) {
    ENTER(nmod);
    CHECK_INITIALIZED();
    Value res = dispatch(left, right, intMod, intMod, ratMod, irrMod, comMod);
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
                    res = realPowRat(left, right);
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    left = rational_to_irrational(left);
                    res = irratSimplify(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = ratPow(left, right);
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX: {
                    left = rational_to_irrational(left);
                    res = irrPowCom(left, right);
                }
                break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        case VALUE_TYPE_IRRATIONAL:
            switch(right.type) {
                case VALUE_TYPE_RATIONAL:
                    res = realPowRat(left, right);
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    res = irratSimplify(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    right = int_to_irrational(right);
                    res = irratSimplify(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX:
                    res = irrPowCom(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        case VALUE_TYPE_BIGINT:
            switch(right.type) {
                case VALUE_TYPE_RATIONAL:
                    res = realPowRat(left, right);
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    left = int_to_irrational(left);
                    res = irratSimplify(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = intPow(left, right);
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX: {
                    left = int_to_irrational(left);
                    res = irrPowCom(left, right);
                }
                break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        case VALUE_TYPE_STDINT:
            switch(right.type) {
                case VALUE_TYPE_IRRATIONAL:
                    left = int_to_irrational(left);
                    res = irratSimplify(pow(left.val.irrational, right.val.irrational));
                    break;
                case VALUE_TYPE_RATIONAL:
                    res = realPowRat(left, right);
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = intPow(left, right);
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX: {
                    left = int_to_irrational(left);
                    res = irrPowCom(left, right);
                }
                break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        case VALUE_TYPE_BIGINT_IMAG:
            switch (right.type) {
                case VALUE_TYPE_RATIONAL:
                case VALUE_TYPE_IRRATIONAL:{
                    Value real = imag_to_real(left);
                    res = npow(real, right);
                    protectValue(res);
                    res = real_to_imag(res);
                }
                break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = comPow(left, right);
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX:
                    res = comPowCom(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        case VALUE_TYPE_STDINT_IMAG:
            switch (right.type) {
                case VALUE_TYPE_RATIONAL:
                case VALUE_TYPE_IRRATIONAL:{
                    Value real = imag_to_real(left);
                    res = npow(real, right);
                    protectValue(res);
                    res = real_to_imag(res);
                }
                break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = comPow(left, right);
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX:
                    res = comPowCom(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        case VALUE_TYPE_RATIONAL_IMAG:
            switch (right.type) {
                case VALUE_TYPE_RATIONAL:
                case VALUE_TYPE_IRRATIONAL:{
                    Value real = imag_to_real(left);
                    res = npow(real, right);
                    protectValue(res);
                    res = real_to_imag(res);
                }
                break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = comPow(left, right);
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX:
                    res = comPowCom(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        case VALUE_TYPE_IRRATIONAL_IMAG:
            switch (right.type) {
                case VALUE_TYPE_RATIONAL:
                case VALUE_TYPE_IRRATIONAL:{
                    Value real = imag_to_real(left);
                    res = npow(real, right);
                    protectValue(res);
                    res = real_to_imag(res);
                }
                break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = comPow(left, right);
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX:
                    res = comPowCom(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        case VALUE_TYPE_COMPLEX:
            switch (right.type) {
                case VALUE_TYPE_RATIONAL:
                    res = comPowRat(left, right);
                    break;
                case VALUE_TYPE_IRRATIONAL:
                    right = real_to_complex(right);
                    protectValue(right);
                    res = comPowCom(left, right);
                    break;
                case VALUE_TYPE_BIGINT:
                case VALUE_TYPE_STDINT:
                    res = comPow(left, right);
                    break;
                case VALUE_TYPE_BIGINT_IMAG:
                case VALUE_TYPE_STDINT_IMAG:
                case VALUE_TYPE_RATIONAL_IMAG:
                case VALUE_TYPE_IRRATIONAL_IMAG:
                case VALUE_TYPE_COMPLEX:
                    res = comPowCom(left, right);
                    break;
                default:
                    cant_happen("unrecognised right number type %s", valueTypeName(right.type));
            }
            break;
        default:
            cant_happen("unrecognised left number type %s", valueTypeName(left.type));
    }
    protectValue(res);
    LEAVE(npow);
    IFDEBUG(ppNumber(res));
    UNPROTECT(save);
    return res;
}

Cmp ncmp(Value left, Value right) {
    ENTER(ncmp);
    CHECK_INITIALIZED();
    Cmp res = CMP_EQ;
    int save = PROTECT(NULL);
    switch (coerce(&left, &right, &save)) {
        case VALUE_TYPE_RATIONAL:
            res = ratCmp(left, right);
            break;
        case VALUE_TYPE_IRRATIONAL:
            res = irrCmp(left, right);
            break;
        case VALUE_TYPE_STDINT:
            res = stdCmp(left, right);
            break;
        case VALUE_TYPE_BIGINT:
            res = bigCmp(left, right);
            break;
        case VALUE_TYPE_COMPLEX:
            res = comCmp(left, right);
            break;
        default:
            cant_happen("unexpected result from coerce");
    }
    LEAVE(ncmp);
    UNPROTECT(save);
    return res;
}

Value real_part(Value v) {
    switch(v.type) {
        case VALUE_TYPE_RATIONAL:
        case VALUE_TYPE_IRRATIONAL:
        case VALUE_TYPE_BIGINT:
        case VALUE_TYPE_STDINT:
            return v;
        case VALUE_TYPE_STDINT_IMAG:
        case VALUE_TYPE_BIGINT_IMAG:
        case VALUE_TYPE_RATIONAL_IMAG:
        case VALUE_TYPE_IRRATIONAL_IMAG:
            return value_Stdint(0);
        case VALUE_TYPE_COMPLEX:
            return realPart(v);
        default:
            cant_happen("unrecognised number type %s", valueTypeName(v.type));
    }
}

Value imag_part(Value v) {
    switch(v.type) {
        case VALUE_TYPE_RATIONAL:
        case VALUE_TYPE_IRRATIONAL:
        case VALUE_TYPE_BIGINT:
        case VALUE_TYPE_STDINT:
            return value_Stdint_imag(0);
        case VALUE_TYPE_STDINT_IMAG:
        case VALUE_TYPE_BIGINT_IMAG:
        case VALUE_TYPE_RATIONAL_IMAG:
        case VALUE_TYPE_IRRATIONAL_IMAG:
            return v;
        case VALUE_TYPE_COMPLEX:
            return imagPart(v);
        default:
            cant_happen("unrecognised number type %s", valueTypeName(v.type));
    }
}

Value theta_part(Value v) {
    Value c = to_complex(v);
    int save = protectValue(c);
    Value t = comTheta(c);
    UNPROTECT(save);
    return t;
}

Value mag_part(Value v) {
    Value c = to_complex(v);
    int save = protectValue(c);
    Value t = comMag(c);
    UNPROTECT(save);
    return t;
}

Value nneg(Value v) {
    ENTER(nneg);
    CHECK_INITIALIZED();
    Value res;
    if (v.type == VALUE_TYPE_RATIONAL) {
        Value numerator = intNeg(numeratorPart(v));
        int save = protectValue(numerator);
        res = ratValue(numerator, denominatorPart(v));
        UNPROTECT(save);
    } else {
        res = numNeg(v);
    }
    LEAVE(nneg);
    return res;
}

// very weak deterministic random number generator
// used by the `rand` built-in
Value nrand(Value prev) {
    CHECK_INITIALIZED();
    ASSERT_IRRATIONAL(prev);
    Double seed = fmod(prev.val.irrational, 1.0);
    if (seed < 0) seed = -seed;
    seed *= UINT_MAX;
    seed = fmod(seed * 1103515245.0 + 12345.0, (Double)UINT_MAX);
    seed /= UINT_MAX;
    Value v = value_Irrational(seed);
    return v;
}

void init_arithmetic() {
    if (!arithmetic_initialized) {
        BigInt *zero = bigIntFromInt(0);
        Zero.type = VALUE_TYPE_BIGINT;
        Zero.val = VALUE_VAL_BIGINT(zero);
        BigInt *one = bigIntFromInt(1);
        One.type = VALUE_TYPE_BIGINT;
        One.val = VALUE_VAL_BIGINT(one);
        arithmetic_initialized = true;
    }
}

void markArithmetic() {
    markValue(Zero);
    markValue(One);
}
