/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "minlam_simplify.h"

#include "arithmetic_next.h"
#include "term_helper.h"

static inline bool isTermNum(Term *term) {
    return term != NULL && isTerm_Num(term);
}

static inline Value termNumValue(Term *term) {
    return getTerm_Num(term)->value;
}

static inline bool isZeroValue(Value value) {
    return ncmp(value, value_Stdint(0)) == CMP_EQ;
}

static inline bool isOneValue(Value value) {
    return ncmp(value, value_Stdint(1)) == CMP_EQ;
}

static inline bool isZeroTerm(Term *term) {
    return isTermNum(term) && isZeroValue(termNumValue(term));
}

static inline bool isOneTerm(Term *term) {
    return isTermNum(term) && isOneValue(termNumValue(term));
}

static inline Term *Num(ParserInfo parserInfo, Value value) {
    Term *result = makeTerm_Num(parserInfo, value);
    PROTECT(result);
    return result;
}

static inline Term *NumInt(ParserInfo parserInfo, int n) {
    return Num(parserInfo, value_Stdint(n));
}

static inline Term *Add(ParserInfo parserInfo, Term *left, Term *right) {
    Term *result = makeTerm_Add(parserInfo, left, right);
    PROTECT(result);
    return result;
}

static inline Term *Sub(ParserInfo parserInfo, Term *left, Term *right) {
    Term *result = makeTerm_Sub(parserInfo, left, right);
    PROTECT(result);
    return result;
}

static inline Term *Mul(ParserInfo parserInfo, Term *left, Term *right) {
    Term *result = makeTerm_Mul(parserInfo, left, right);
    PROTECT(result);
    return result;
}

static inline Term *Div(ParserInfo parserInfo, Term *left, Term *right) {
    Term *result = makeTerm_Div(parserInfo, left, right);
    PROTECT(result);
    return result;
}

static inline Term *Pow(ParserInfo parserInfo, Term *left, Term *right) {
    Term *result = makeTerm_Pow(parserInfo, left, right);
    PROTECT(result);
    return result;
}

static inline Term *Mod(ParserInfo parserInfo, Term *left, Term *right) {
    Term *result = makeTerm_Mod(parserInfo, left, right);
    PROTECT(result);
    return result;
}

static inline Term *Gcd(ParserInfo parserInfo, Term *left, Term *right) {
    Term *result = makeTerm_Gcd(parserInfo, left, right);
    PROTECT(result);
    return result;
}

static inline Term *Lcm(ParserInfo parserInfo, Term *left, Term *right) {
    Term *result = makeTerm_Lcm(parserInfo, left, right);
    PROTECT(result);
    return result;
}

static inline Term *Canon(ParserInfo parserInfo, Term *inner) {
    Term *result = newTerm_Canon(parserInfo, inner);
    PROTECT(result);
    return result;
}

static inline Term *wrapCanon(ParserInfo parserInfo, Term *term) {
    if (term != NULL && isTerm_Canon(term)) {
        return term;
    }
    return Canon(parserInfo, term);
}

static inline bool isSubTerm(Term *term) {
    return term != NULL && isTerm_Sub(term);
}

static inline bool isAddTerm(Term *term) {
    return term != NULL && isTerm_Add(term);
}

static inline bool isDivTerm(Term *term) {
    return term != NULL && isTerm_Div(term);
}

static inline bool isMulTerm(Term *term) {
    return term != NULL && isTerm_Mul(term);
}

static inline bool isPowTerm(Term *term) {
    return term != NULL && isTerm_Pow(term);
}

static inline bool isModTerm(Term *term) {
    return term != NULL && isTerm_Mod(term);
}

static inline bool isGcdTerm(Term *term) {
    return term != NULL && isTerm_Gcd(term);
}

static inline bool isLcmTerm(Term *term) {
    return term != NULL && isTerm_Lcm(term);
}

static bool matchSubZero(Term *term, Term **inner) {
    if (!isSubTerm(term))
        return false;

    TermOp *sub = getTerm_Sub(term);
    if (!isZeroTerm(sub->left))
        return false;

    *inner = sub->right;
    return true;
}

static bool matchAddNumOther(Term *term, Value *num, Term **other) {
    if (!isAddTerm(term))
        return false;

    TermOp *add = getTerm_Add(term);
    if (isTermNum(add->left)) {
        *num = termNumValue(add->left);
        *other = add->right;
        return true;
    }
    if (isTermNum(add->right)) {
        *num = termNumValue(add->right);
        *other = add->left;
        return true;
    }
    return false;
}

static bool matchSubNumOther(Term *term, Value *num, Term **other) {
    if (!isSubTerm(term))
        return false;

    TermOp *sub = getTerm_Sub(term);
    if (!isTermNum(sub->left))
        return false;

    *num = termNumValue(sub->left);
    *other = sub->right;
    return true;
}

static bool matchSubOtherNum(Term *term, Term **other, Value *num) {
    if (!isSubTerm(term))
        return false;

    TermOp *sub = getTerm_Sub(term);
    if (!isTermNum(sub->right))
        return false;

    *other = sub->left;
    *num = termNumValue(sub->right);
    return true;
}

static bool matchMulNumOther(Term *term, Value *num, Term **other) {
    if (!isMulTerm(term))
        return false;

    TermOp *mul = getTerm_Mul(term);
    if (isTermNum(mul->left)) {
        *num = termNumValue(mul->left);
        *other = mul->right;
        return true;
    }
    if (isTermNum(mul->right)) {
        *num = termNumValue(mul->right);
        *other = mul->left;
        return true;
    }
    return false;
}

static bool matchDivNumOther(Term *term, Value *num, Term **other) {
    if (!isDivTerm(term))
        return false;

    TermOp *div = getTerm_Div(term);
    if (!isTermNum(div->left))
        return false;

    *num = termNumValue(div->left);
    *other = div->right;
    return true;
}

static bool matchDivOtherNum(Term *term, Term **other, Value *num) {
    if (!isDivTerm(term))
        return false;

    TermOp *div = getTerm_Div(term);
    if (!isTermNum(div->right))
        return false;

    *other = div->left;
    *num = termNumValue(div->right);
    return true;
}

static bool matchDivTerms(Term *term, Term **numerator, Term **denominator) {
    if (!isDivTerm(term))
        return false;

    TermOp *div = getTerm_Div(term);
    *numerator = div->left;
    *denominator = div->right;
    return true;
}

static bool matchMulWithFactor(Term *term, Term *factor, Term **other) {
    if (!isMulTerm(term))
        return false;

    TermOp *mul = getTerm_Mul(term);
    if (eqTerm(mul->left, factor)) {
        *other = mul->right;
        return true;
    }
    if (eqTerm(mul->right, factor)) {
        *other = mul->left;
        return true;
    }
    return false;
}

typedef Value (*NumericOpFn)(Value, Value);
typedef Term *(*BuildTermOpFn)(ParserInfo, Term *, Term *);
typedef TermOp *(*GetTermOpFn)(Term *);

typedef struct BinaryOpSpec {
    TermType op;
    NumericOpFn numeric;
    BuildTermOpFn build;
    GetTermOpFn get;
} BinaryOpSpec;

static const BinaryOpSpec *lookupBinaryOpSpec(TermType op) {
    static const BinaryOpSpec specs[] = {
        {TERM_TYPE_ADD, nadd, Add, getTerm_Add},
        {TERM_TYPE_SUB, nsub, Sub, getTerm_Sub},
        {TERM_TYPE_MUL, nmul, Mul, getTerm_Mul},
        {TERM_TYPE_DIV, ndiv, Div, getTerm_Div},
        {TERM_TYPE_MOD, nmod, Mod, getTerm_Mod},
        {TERM_TYPE_POW, npow, Pow, getTerm_Pow},
        {TERM_TYPE_GCD, ngcd, Gcd, getTerm_Gcd},
        {TERM_TYPE_LCM, nlcm, Lcm, getTerm_Lcm},
    };

    for (size_t i = 0; i < sizeof(specs) / sizeof(specs[0]); i++) {
        if (specs[i].op == op)
            return &specs[i];
    }
    return NULL;
}

static Term *makeNumericOpResult(ParserInfo parserInfo, TermType op, Value left,
                                 Value right) {
    const BinaryOpSpec *spec = lookupBinaryOpSpec(op);
    if (spec == NULL) {
        cant_happen("unsupported term op %d in makeNumericOpResult", op);
    }
    return Num(parserInfo, spec->numeric(left, right));
}

static Term *simplifyTerm(Term *term);

#define RETURN_MATCH(expr)                                                     \
    do {                                                                       \
        *result = (expr);                                                      \
        UNPROTECT(save);                                                       \
        return true;                                                           \
    } while (0)

#define RETURN_NO_MATCH()                                                      \
    do {                                                                       \
        UNPROTECT(save);                                                       \
        return false;                                                          \
    } while (0)

static bool tryConstantFold(Term *term, TermType op, Term *left, Term *right,
                            Term **result) {
    if (isTermNum(left) && isTermNum(right)) {
        *result = makeNumericOpResult(CPI(term), op, termNumValue(left),
                                      termNumValue(right));
        return true;
    }
    return false;
}

static bool tryAddIdentity(Term *term, Term *left, Term *right, Term **result) {
    int save = PROTECT(NULL);
    Term *inner = NULL;
    Term *leftNum = NULL;
    Term *leftDen = NULL;
    Term *rightNum = NULL;
    Term *rightDen = NULL;
    Value a;
    Value b;
    Value c;
    Term *x = NULL;
    Term *y = NULL;

    if (isZeroTerm(left)) {
        RETURN_MATCH(right);
    }
    if (isZeroTerm(right)) {
        RETURN_MATCH(left);
    }

    if (matchSubZero(right, &inner) && eqTerm(left, inner)) {
        RETURN_MATCH(NumInt(CPI(left), 0));
    }

    if (matchSubZero(left, &inner) && eqTerm(right, inner)) {
        RETURN_MATCH(NumInt(CPI(right), 0));
    }

    if (eqTerm(left, right)) {
        Term *two = NumInt(CPI(left), 2);
        RETURN_MATCH(Mul(CPI(left), two, left));
    }

    if (matchDivTerms(left, &leftNum, &leftDen) &&
        matchDivTerms(right, &rightNum, &rightDen) &&
        eqTerm(leftDen, rightDen)) {
        Term *numerator = Add(CPI(term), leftNum, rightNum);
        RETURN_MATCH(Div(CPI(term), numerator, leftDen));
    }

    if (isTermNum(left)) {
        a = termNumValue(left);

        if (matchAddNumOther(right, &b, &x)) {
            Term *sum = Num(CPI(term), nadd(a, b));
            RETURN_MATCH(Add(CPI(term), sum, x));
        }

        if (matchSubNumOther(right, &b, &x)) {
            Term *sum = Num(CPI(term), nadd(a, b));
            RETURN_MATCH(Sub(CPI(term), sum, x));
        }

        if (matchSubOtherNum(right, &x, &b)) {
            Term *diff = Num(CPI(term), nsub(a, b));
            RETURN_MATCH(Add(CPI(term), diff, x));
        }
    }

    if (isTermNum(right)) {
        a = termNumValue(right);

        if (matchAddNumOther(left, &b, &x)) {
            Term *sum = Num(CPI(term), nadd(a, b));
            RETURN_MATCH(Add(CPI(term), sum, x));
        }

        if (matchSubNumOther(left, &b, &x)) {
            Term *sum = Num(CPI(term), nadd(a, b));
            RETURN_MATCH(Sub(CPI(term), sum, x));
        }

        if (matchSubOtherNum(left, &x, &b)) {
            Term *diff = Num(CPI(term), nsub(a, b));
            RETURN_MATCH(Add(CPI(term), diff, x));
        }
    }

    if (matchAddNumOther(left, &a, &x) && matchAddNumOther(right, &b, &y)) {
        Term *sum = Num(CPI(term), nadd(a, b));
        Term *tail = Add(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), sum, tail));
    }

    if (matchAddNumOther(left, &a, &x) && matchSubNumOther(right, &b, &y)) {
        Term *sum = Num(CPI(term), nadd(a, b));
        Term *tail = Sub(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), sum, tail));
    }

    if (matchSubNumOther(left, &a, &x) && matchAddNumOther(right, &b, &y)) {
        Term *sum = Num(CPI(term), nadd(a, b));
        Term *tail = Sub(CPI(term), y, x);
        RETURN_MATCH(Add(CPI(term), sum, tail));
    }

    if (matchAddNumOther(left, &a, &x) && matchSubOtherNum(right, &y, &b)) {
        Term *diff = Num(CPI(term), nsub(a, b));
        Term *tail = Add(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), diff, tail));
    }

    if (matchSubOtherNum(left, &x, &a) && matchAddNumOther(right, &b, &y)) {
        Term *diff = Num(CPI(term), nsub(b, a));
        Term *tail = Add(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), diff, tail));
    }

    if (matchSubNumOther(left, &a, &x) && matchSubNumOther(right, &b, &y)) {
        Term *sum = Num(CPI(term), nadd(a, b));
        Term *tail = Add(CPI(term), x, y);
        RETURN_MATCH(Sub(CPI(term), sum, tail));
    }

    if (matchSubOtherNum(left, &x, &a) && matchSubNumOther(right, &b, &y)) {
        Term *diff = Num(CPI(term), nsub(b, a));
        Term *tail = Sub(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), diff, tail));
    }

    if (matchSubNumOther(left, &a, &x) && matchSubOtherNum(right, &y, &b)) {
        Term *diff = Num(CPI(term), nsub(a, b));
        Term *tail = Add(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), diff, tail));
    }

    if (matchSubOtherNum(left, &x, &a) && matchSubOtherNum(right, &y, &b)) {
        c = nadd(a, b);
        Term *sum = Num(CPI(term), c);
        Term *head = Add(CPI(term), x, y);
        RETURN_MATCH(Sub(CPI(term), head, sum));
    }

    RETURN_NO_MATCH();
}

static bool trySubIdentity(Term *term, Term *left, Term *right, Term **result) {
    int save = PROTECT(NULL);
    Term *inner = NULL;
    Term *leftNum = NULL;
    Term *leftDen = NULL;
    Term *rightNum = NULL;
    Term *rightDen = NULL;
    Value a;
    Value b;
    Term *x = NULL;
    Term *y = NULL;

    if (isZeroTerm(right)) {
        RETURN_MATCH(left);
    }
    if (eqTerm(left, right)) {
        RETURN_MATCH(NumInt(CPI(term), 0));
    }

    if (isZeroTerm(left) && matchSubZero(right, &inner)) {
        RETURN_MATCH(inner);
    }

    if (matchDivTerms(left, &leftNum, &leftDen) &&
        matchDivTerms(right, &rightNum, &rightDen) &&
        eqTerm(leftDen, rightDen)) {
        Term *numerator = Sub(CPI(term), leftNum, rightNum);
        RETURN_MATCH(Div(CPI(term), numerator, leftDen));
    }

    if (isTermNum(left)) {
        a = termNumValue(left);

        if (matchAddNumOther(right, &b, &x)) {
            Term *diff = Num(CPI(term), nsub(a, b));
            RETURN_MATCH(Sub(CPI(term), diff, x));
        }

        if (matchSubNumOther(right, &b, &x)) {
            Term *diff = Num(CPI(term), nsub(a, b));
            RETURN_MATCH(Add(CPI(term), diff, x));
        }

        if (matchSubOtherNum(right, &x, &b)) {
            Term *sum = Num(CPI(term), nadd(a, b));
            RETURN_MATCH(Sub(CPI(term), sum, x));
        }
    }

    if (isTermNum(right)) {
        b = termNumValue(right);

        if (matchAddNumOther(left, &a, &x)) {
            Term *diff = Num(CPI(term), nsub(a, b));
            RETURN_MATCH(Add(CPI(term), diff, x));
        }

        if (matchSubNumOther(left, &a, &x)) {
            Term *diff = Num(CPI(term), nsub(a, b));
            RETURN_MATCH(Sub(CPI(term), diff, x));
        }

        if (matchSubOtherNum(left, &x, &a)) {
            Term *sum = Num(CPI(term), nadd(a, b));
            RETURN_MATCH(Sub(CPI(term), x, sum));
        }
    }

    if (matchAddNumOther(left, &a, &x) && matchAddNumOther(right, &b, &y)) {
        Term *diff = Num(CPI(term), nsub(a, b));
        Term *tail = Sub(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), diff, tail));
    }

    if (matchSubNumOther(left, &a, &x) && matchSubNumOther(right, &b, &y)) {
        Term *diff = Num(CPI(term), nsub(a, b));
        Term *tail = Sub(CPI(term), x, y);
        RETURN_MATCH(Sub(CPI(term), diff, tail));
    }

    if (matchSubOtherNum(left, &x, &a) && matchSubOtherNum(right, &y, &b)) {
        Term *diff = Num(CPI(term), nsub(b, a));
        Term *tail = Sub(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), diff, tail));
    }

    if (matchSubNumOther(left, &a, &x) && matchAddNumOther(right, &b, &y)) {
        Term *diff = Num(CPI(term), nsub(a, b));
        Term *tail = Add(CPI(term), x, y);
        RETURN_MATCH(Sub(CPI(term), diff, tail));
    }

    if (matchSubOtherNum(left, &x, &a) && matchAddNumOther(right, &b, &y)) {
        Term *sum = Num(CPI(term), nadd(a, b));
        Term *head = Sub(CPI(term), x, sum);
        RETURN_MATCH(Sub(CPI(term), head, y));
    }

    if (matchAddNumOther(left, &a, &x) && matchSubNumOther(right, &b, &y)) {
        Term *diff = Num(CPI(term), nsub(a, b));
        Term *tail = Add(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), diff, tail));
    }

    if (matchAddNumOther(left, &a, &x) && matchSubOtherNum(right, &y, &b)) {
        Term *sum = Num(CPI(term), nadd(a, b));
        Term *tail = Sub(CPI(term), x, y);
        RETURN_MATCH(Add(CPI(term), sum, tail));
    }

    if (matchSubNumOther(left, &a, &x) && matchSubOtherNum(right, &y, &b)) {
        Term *sum = Num(CPI(term), nadd(a, b));
        Term *tail = Add(CPI(term), x, y);
        RETURN_MATCH(Sub(CPI(term), sum, tail));
    }

    if (matchSubOtherNum(left, &x, &a) && matchSubNumOther(right, &b, &y)) {
        Term *sum = Num(CPI(term), nadd(a, b));
        Term *head = Sub(CPI(term), x, sum);
        RETURN_MATCH(Add(CPI(term), head, y));
    }

    RETURN_NO_MATCH();
}

static bool tryMulIdentity(Term *term, Term *left, Term *right, Term **result) {
    int save = PROTECT(NULL);
    TermOp *div = NULL;
    TermOp *pow = NULL;
    Value a;
    Value b;
    Term *x = NULL;
    Term *y = NULL;
    Term *numA = NULL;

    if (isZeroTerm(left) || isZeroTerm(right)) {
        RETURN_MATCH(NumInt(CPI(term), 0));
    }
    if (isOneTerm(left)) {
        RETURN_MATCH(right);
    }
    if (isOneTerm(right)) {
        RETURN_MATCH(left);
    }

    if (eqTerm(left, right)) {
        Term *two = NumInt(CPI(term), 2);
        RETURN_MATCH(Pow(CPI(term), left, two));
    }

    if (isDivTerm(right)) {
        div = getTerm_Div(right);
        if (eqTerm(left, div->right)) {
            RETURN_MATCH(div->left);
        }
    }

    if (isDivTerm(left)) {
        div = getTerm_Div(left);
        if (eqTerm(right, div->right)) {
            RETURN_MATCH(div->left);
        }
    }

    if (isPowTerm(right)) {
        pow = getTerm_Pow(right);
        if (eqTerm(left, pow->left)) {
            Term *one = NumInt(CPI(term), 1);
            Term *exp = Add(CPI(term), pow->right, one);
            RETURN_MATCH(Pow(CPI(term), left, exp));
        }
    }

    if (isPowTerm(left)) {
        pow = getTerm_Pow(left);
        if (eqTerm(right, pow->left)) {
            Term *one = NumInt(CPI(term), 1);
            Term *exp = Add(CPI(term), pow->right, one);
            RETURN_MATCH(Pow(CPI(term), right, exp));
        }
    }

    if (isPowTerm(left) && isPowTerm(right)) {
        TermOp *lp = getTerm_Pow(left);
        TermOp *rp = getTerm_Pow(right);
        if (eqTerm(lp->left, rp->left)) {
            Term *exp = Add(CPI(term), lp->right, rp->right);
            RETURN_MATCH(Pow(CPI(term), lp->left, exp));
        }
    }

    if (matchDivOtherNum(left, &x, &a) && matchDivOtherNum(right, &y, &b)) {
        Term *num = Mul(CPI(term), x, y);
        Term *den = Num(CPI(term), nmul(a, b));
        RETURN_MATCH(Div(CPI(term), num, den));
    }

    if (matchDivNumOther(left, &a, &x) && matchDivNumOther(right, &b, &y)) {
        Term *num = Num(CPI(term), nmul(a, b));
        Term *den = Mul(CPI(term), x, y);
        RETURN_MATCH(Div(CPI(term), num, den));
    }

    if (isTermNum(left)) {
        a = termNumValue(left);
        numA = Num(CPI(term), a);

        if (matchDivNumOther(right, &b, &x)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            RETURN_MATCH(Div(CPI(term), ab, x));
        }

        if (matchDivOtherNum(right, &x, &b)) {
            Term *q = Num(CPI(term), ndiv(a, b));
            RETURN_MATCH(Mul(CPI(term), q, x));
        }

        if (matchMulNumOther(right, &b, &x)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            RETURN_MATCH(Mul(CPI(term), ab, x));
        }

        if (matchAddNumOther(right, &b, &x)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            Term *ax = Mul(CPI(term), numA, x);
            RETURN_MATCH(Add(CPI(term), ab, ax));
        }

        if (matchSubNumOther(right, &b, &x)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            Term *ax = Mul(CPI(term), numA, x);
            RETURN_MATCH(Sub(CPI(term), ab, ax));
        }

        if (matchSubOtherNum(right, &x, &b)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            Term *ax = Mul(CPI(term), numA, x);
            RETURN_MATCH(Sub(CPI(term), ax, ab));
        }
    }

    if (isTermNum(right)) {
        a = termNumValue(right);
        numA = Num(CPI(term), a);

        if (matchDivNumOther(left, &b, &x)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            RETURN_MATCH(Div(CPI(term), ab, x));
        }

        if (matchDivOtherNum(left, &x, &b)) {
            Term *q = Num(CPI(term), ndiv(a, b));
            RETURN_MATCH(Mul(CPI(term), q, x));
        }

        if (matchMulNumOther(left, &b, &x)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            RETURN_MATCH(Mul(CPI(term), ab, x));
        }

        if (matchAddNumOther(left, &b, &x)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            Term *ax = Mul(CPI(term), numA, x);
            RETURN_MATCH(Add(CPI(term), ab, ax));
        }

        if (matchSubNumOther(left, &b, &x)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            Term *ax = Mul(CPI(term), numA, x);
            RETURN_MATCH(Sub(CPI(term), ab, ax));
        }

        if (matchSubOtherNum(left, &x, &b)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            Term *ax = Mul(CPI(term), numA, x);
            RETURN_MATCH(Sub(CPI(term), ax, ab));
        }
    }

    RETURN_NO_MATCH();
}

static bool tryDivIdentity(Term *term, Term *left, Term *right, Term **result) {
    int save = PROTECT(NULL);
    TermOp *outer = NULL;
    TermOp *lpow = NULL;
    TermOp *rpow = NULL;
    TermOp *leftMul = NULL;
    TermOp *rightMul = NULL;
    Value a;
    Value b;
    Term *x = NULL;
    Term *y = NULL;
    Term *divisor = NULL;
    Term *other = NULL;

    if (isZeroTerm(left)) {
        RETURN_MATCH(NumInt(CPI(term), 0));
    }
    if (isOneTerm(right)) {
        RETURN_MATCH(left);
    }
    if (eqTerm(left, right)) {
        RETURN_MATCH(NumInt(CPI(term), 1));
    }

    if (isMulTerm(left) && isMulTerm(right)) {
        leftMul = getTerm_Mul(left);
        rightMul = getTerm_Mul(right);

        if (matchMulWithFactor(right, leftMul->left, &other)) {
            RETURN_MATCH(Div(CPI(term), leftMul->right, other));
        }
        if (matchMulWithFactor(right, leftMul->right, &other)) {
            RETURN_MATCH(Div(CPI(term), leftMul->left, other));
        }
        if (matchMulWithFactor(left, rightMul->left, &other)) {
            RETURN_MATCH(Div(CPI(term), other, rightMul->right));
        }
        if (matchMulWithFactor(left, rightMul->right, &other)) {
            RETURN_MATCH(Div(CPI(term), other, rightMul->left));
        }
    }

    if (matchDivOtherNum(left, &x, &a) && isTermNum(right)) {
        b = termNumValue(right);
        RETURN_MATCH(Div(CPI(term), x, Num(CPI(term), nmul(a, b))));
    }

    if (matchDivNumOther(left, &a, &x) && isTermNum(right)) {
        b = termNumValue(right);
        RETURN_MATCH(Div(CPI(term), Num(CPI(term), ndiv(a, b)), x));
    }

    if (matchDivOtherNum(left, &x, &a) && matchDivOtherNum(right, &y, &b)) {
        Term *xy = Div(CPI(term), x, y);
        Term *k = Num(CPI(term), ndiv(b, a));
        RETURN_MATCH(Mul(CPI(term), xy, k));
    }

    if (matchDivNumOther(left, &a, &x) && matchDivNumOther(right, &b, &y)) {
        Term *k = Num(CPI(term), ndiv(a, b));
        Term *yx = Div(CPI(term), y, x);
        RETURN_MATCH(Mul(CPI(term), k, yx));
    }

    if (isDivTerm(left)) {
        outer = getTerm_Div(left);
        Term *den = Mul(CPI(term), outer->right, right);
        RETURN_MATCH(Div(CPI(term), outer->left, den));
    }

    if (isPowTerm(left)) {
        lpow = getTerm_Pow(left);
        if (eqTerm(lpow->left, right)) {
            Term *one = NumInt(CPI(term), 1);
            Term *exp = Sub(CPI(term), lpow->right, one);
            RETURN_MATCH(Pow(CPI(term), lpow->left, exp));
        }
    }

    if (isPowTerm(left) && isPowTerm(right)) {
        lpow = getTerm_Pow(left);
        rpow = getTerm_Pow(right);
        if (eqTerm(lpow->left, rpow->left)) {
            Term *exp = Sub(CPI(term), lpow->right, rpow->right);
            RETURN_MATCH(Pow(CPI(term), lpow->left, exp));
        }
    }

    if (isTermNum(left)) {
        a = termNumValue(left);

        if (matchDivNumOther(right, &b, &x)) {
            Term *q = Num(CPI(term), ndiv(a, b));
            RETURN_MATCH(Mul(CPI(term), q, x));
        }

        if (matchDivOtherNum(right, &x, &b)) {
            Term *ab = Num(CPI(term), nmul(a, b));
            RETURN_MATCH(Div(CPI(term), ab, x));
        }

        if (matchMulNumOther(right, &b, &x)) {
            Term *q = Num(CPI(term), ndiv(a, b));
            RETURN_MATCH(Div(CPI(term), q, x));
        }
    }

    if (isTermNum(right)) {
        b = termNumValue(right);
        divisor = Num(CPI(term), b);

        if (matchMulNumOther(left, &a, &x)) {
            Term *q = Num(CPI(term), ndiv(a, b));
            RETURN_MATCH(Mul(CPI(term), q, x));
        }

        if (matchAddNumOther(left, &a, &x)) {
            Term *q = Num(CPI(term), ndiv(a, b));
            Term *xd = Div(CPI(term), x, divisor);
            RETURN_MATCH(Add(CPI(term), q, xd));
        }

        if (matchSubNumOther(left, &a, &x)) {
            Term *q = Num(CPI(term), ndiv(a, b));
            Term *xd = Div(CPI(term), x, divisor);
            RETURN_MATCH(Sub(CPI(term), q, xd));
        }

        if (matchSubOtherNum(left, &x, &a)) {
            Term *q = Num(CPI(term), ndiv(a, b));
            Term *xd = Div(CPI(term), x, divisor);
            RETURN_MATCH(Sub(CPI(term), xd, q));
        }
    }

    RETURN_NO_MATCH();
}

static bool tryModIdentity(Term *term, Term *left, Term *right, Term **result) {
    int save = PROTECT(NULL);
    TermOp *inner = NULL;

    if (isZeroTerm(left) || eqTerm(left, right)) {
        RETURN_MATCH(NumInt(CPI(term), 0));
    }

    if (isOneTerm(right)) {
        RETURN_MATCH(NumInt(CPI(term), 0));
    }

    if (isModTerm(left)) {
        inner = getTerm_Mod(left);
        if (eqTerm(inner->right, right)) {
            RETURN_MATCH(left);
        }
    }

    RETURN_NO_MATCH();
}

static bool tryPowIdentity(Term *term, Term *left, Term *right, Term **result) {
    int save = PROTECT(NULL);
    TermOp *outer = NULL;

    if (isZeroTerm(right)) {
        RETURN_MATCH(NumInt(CPI(term), 1));
    }
    if (isOneTerm(right)) {
        RETURN_MATCH(left);
    }
    if (isZeroTerm(left)) {
        RETURN_MATCH(NumInt(CPI(term), 0));
    }
    if (isOneTerm(left)) {
        RETURN_MATCH(NumInt(CPI(term), 1));
    }

    if (isPowTerm(left)) {
        outer = getTerm_Pow(left);
        Term *exp = Mul(CPI(term), outer->right, right);
        RETURN_MATCH(Pow(CPI(term), outer->left, exp));
    }

    RETURN_NO_MATCH();
}

static bool tryGcdIdentity(Term *term, Term *left, Term *right, Term **result) {
    int save = PROTECT(NULL);
    TermOp *inner = NULL;

    if (isZeroTerm(left) && isZeroTerm(right)) {
        RETURN_MATCH(NumInt(CPI(term), 0));
    }

    if (isZeroTerm(left)) {
        RETURN_MATCH(wrapCanon(CPI(term), right));
    }

    if (isZeroTerm(right)) {
        RETURN_MATCH(wrapCanon(CPI(term), left));
    }

    if (isOneTerm(left) || isOneTerm(right)) {
        RETURN_MATCH(NumInt(CPI(term), 1));
    }

    if (eqTerm(left, right)) {
        RETURN_MATCH(wrapCanon(CPI(term), left));
    }

    if (isLcmTerm(right)) {
        inner = getTerm_Lcm(right);
        if (eqTerm(left, inner->left) || eqTerm(left, inner->right)) {
            RETURN_MATCH(wrapCanon(CPI(term), left));
        }
    }

    if (isLcmTerm(left)) {
        inner = getTerm_Lcm(left);
        if (eqTerm(right, inner->left) || eqTerm(right, inner->right)) {
            RETURN_MATCH(wrapCanon(CPI(term), right));
        }
    }

    RETURN_NO_MATCH();
}

static bool tryLcmIdentity(Term *term, Term *left, Term *right, Term **result) {
    int save = PROTECT(NULL);
    TermOp *inner = NULL;

    if (isZeroTerm(left) || isZeroTerm(right)) {
        RETURN_MATCH(NumInt(CPI(term), 0));
    }

    if (isOneTerm(left)) {
        RETURN_MATCH(wrapCanon(CPI(term), right));
    }

    if (isOneTerm(right)) {
        RETURN_MATCH(wrapCanon(CPI(term), left));
    }

    if (eqTerm(left, right)) {
        RETURN_MATCH(wrapCanon(CPI(term), left));
    }

    if (isGcdTerm(right)) {
        inner = getTerm_Gcd(right);
        if (eqTerm(left, inner->left) || eqTerm(left, inner->right)) {
            RETURN_MATCH(wrapCanon(CPI(term), left));
        }
    }

    if (isGcdTerm(left)) {
        inner = getTerm_Gcd(left);
        if (eqTerm(right, inner->left) || eqTerm(right, inner->right)) {
            RETURN_MATCH(wrapCanon(CPI(term), right));
        }
    }

    RETURN_NO_MATCH();
}

#undef RETURN_MATCH
#undef RETURN_NO_MATCH

static bool tryIdentityFold(Term *term, TermType op, Term *left, Term *right,
                            Term **result) {
    switch (op) {
    case TERM_TYPE_ADD:
        return tryAddIdentity(term, left, right, result);
    case TERM_TYPE_SUB:
        return trySubIdentity(term, left, right, result);
    case TERM_TYPE_MUL:
        return tryMulIdentity(term, left, right, result);
    case TERM_TYPE_DIV:
        return tryDivIdentity(term, left, right, result);
    case TERM_TYPE_MOD:
        return tryModIdentity(term, left, right, result);
    case TERM_TYPE_POW:
        return tryPowIdentity(term, left, right, result);
    case TERM_TYPE_GCD:
        return tryGcdIdentity(term, left, right, result);
    case TERM_TYPE_LCM:
        return tryLcmIdentity(term, left, right, result);
    default:
        return false;
    }
}

static Term *rebuildBinaryIfChanged(Term *term, const BinaryOpSpec *spec,
                                    Term *left, Term *right, TermOp *original) {
    if (left == original->left && right == original->right)
        return term;

    return spec->build(CPI(term), left, right);
}

static Term *simplifyBinaryOp(Term *term, const BinaryOpSpec *spec,
                              TermOp *termOp) {
    int save = PROTECT(NULL);
    Term *left = simplifyTerm(termOp->left);
    PROTECT(left);
    Term *right = simplifyTerm(termOp->right);
    PROTECT(right);
    Term *result = NULL;

    if (tryConstantFold(term, spec->op, left, right, &result)) {
        UNPROTECT(save);
        return result;
    }

    if (tryIdentityFold(term, spec->op, left, right, &result)) {
        PROTECT(result);
        result = simplifyTerm(result);
        UNPROTECT(save);
        return result;
    }

    result = rebuildBinaryIfChanged(term, spec, left, right, termOp);
    UNPROTECT(save);
    return result;
}

static Term *simplifyTerm(Term *term) {
    if (term == NULL)
        return NULL;

    const BinaryOpSpec *spec = lookupBinaryOpSpec(term->type);
    if (spec != NULL)
        return simplifyBinaryOp(term, spec, spec->get(term));

    switch (term->type) {
    case TERM_TYPE_CANON: {
        Term *s = simplifyTerm(getTerm_Canon(term));
        int save = PROTECT(s);
        if (isTerm_Canon(s)) {
            UNPROTECT(save);
            return s;
        }
        Term *result = newTerm_Canon(CPI(term), s);
        UNPROTECT(save);
        return result;
    }
    case TERM_TYPE_NUM:
    case TERM_TYPE_OTHER:
        return term;
    default:
        cant_happen("unrecognised TermType %s in simplifyTerm",
                    termTypeName(term->type));
    }
}

MinExp *simplifyMinExp(MinExp *node) {
    if (node == NULL)
        return NULL;

    Term *term = minExpToTerm(node);
    int save = PROTECT(term);
    Term *simplifiedTerm = simplifyTerm(term);
    PROTECT(simplifiedTerm);
    MinExp *result = termToMinExp(simplifiedTerm);
    UNPROTECT(save);
    return result;
}
