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

#include "term_helper.h"

static MinExp *makeNumericPrimExp(ParserInfo parserInfo, MinPrimOp op,
                                  MinExp *left, MinExp *right) {
    int save = PROTECT(left);
    PROTECT(right);
    MinExp *result = makeMinExp_Prim(parserInfo, op, left, right);
    UNPROTECT(save);
    return result;
}

static Value valueToImag(Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT:
        return value_Stdint_imag(getValue_Stdint(value));
    case VALUE_TYPE_BIGINT:
        return value_Bigint_imag(getValue_Bigint(value));
    case VALUE_TYPE_IRRATIONAL:
        return value_Irrational_imag(getValue_Irrational(value));
    case VALUE_TYPE_RATIONAL:
        return value_Rational_imag(getValue_Rational(value));
    case VALUE_TYPE_STDINT_IMAG:
    case VALUE_TYPE_BIGINT_IMAG:
    case VALUE_TYPE_IRRATIONAL_IMAG:
    case VALUE_TYPE_RATIONAL_IMAG:
        return value;
    default:
        cant_happen("unsupported ValueType %s in valueToImag",
                    valueTypeName(value.type));
    }
}

static MinExp *termValueToMinExp(ParserInfo parserInfo, Value value) {
    switch (value.type) {
    case VALUE_TYPE_STDINT: {
        MaybeBigInt *bigInt = fakeBigInt(value.val.stdint, false);
        int save = PROTECT(bigInt);
        MinExp *result = newMinExp_BigInteger(parserInfo, bigInt);
        UNPROTECT(save);
        return result;
    }
    case VALUE_TYPE_IRRATIONAL: {
        MaybeBigInt *bigInt = irrationalBigInt(value.val.irrational, false);
        int save = PROTECT(bigInt);
        MinExp *result = newMinExp_BigInteger(parserInfo, bigInt);
        UNPROTECT(save);
        return result;
    }
    case VALUE_TYPE_BIGINT: {
        bigint copied;
        bigint_init(&copied);
        bigint_cpy(&copied, &value.val.bigint->bi);
        MaybeBigInt *bigInt = newMaybeBigInt(copied, false);
        int save = PROTECT(bigInt);
        MinExp *result = newMinExp_BigInteger(parserInfo, bigInt);
        UNPROTECT(save);
        return result;
    }
    case VALUE_TYPE_STDINT_IMAG: {
        MaybeBigInt *bigInt = fakeBigInt(value.val.stdint_imag, true);
        int save = PROTECT(bigInt);
        MinExp *result = newMinExp_BigInteger(parserInfo, bigInt);
        UNPROTECT(save);
        return result;
    }
    case VALUE_TYPE_BIGINT_IMAG: {
        bigint copied;
        bigint_init(&copied);
        bigint_cpy(&copied, &value.val.bigint_imag->bi);
        MaybeBigInt *bigInt = newMaybeBigInt(copied, true);
        int save = PROTECT(bigInt);
        MinExp *result = newMinExp_BigInteger(parserInfo, bigInt);
        UNPROTECT(save);
        return result;
    }
    case VALUE_TYPE_IRRATIONAL_IMAG: {
        MaybeBigInt *bigInt = irrationalBigInt(value.val.irrational_imag, true);
        int save = PROTECT(bigInt);
        MinExp *result = newMinExp_BigInteger(parserInfo, bigInt);
        UNPROTECT(save);
        return result;
    }
    case VALUE_TYPE_RATIONAL: {
        Vec *rational = getValue_Rational(value);
        MinExp *numerator = termValueToMinExp(parserInfo, rational->entries[0]);
        int save = PROTECT(numerator);
        MinExp *denominator =
            termValueToMinExp(parserInfo, rational->entries[1]);
        PROTECT(denominator);
        MinExp *result = makeNumericPrimExp(parserInfo, MINPRIMOP_TYPE_DIV,
                                            numerator, denominator);
        UNPROTECT(save);
        return result;
    }
    case VALUE_TYPE_RATIONAL_IMAG: {
        Vec *rationalImag = getValue_Rational_imag(value);
        Value numeratorImag = valueToImag(rationalImag->entries[0]);
        MinExp *numerator = termValueToMinExp(parserInfo, numeratorImag);
        int save = PROTECT(numerator);
        MinExp *denominator =
            termValueToMinExp(parserInfo, rationalImag->entries[1]);
        PROTECT(denominator);
        MinExp *result = makeNumericPrimExp(parserInfo, MINPRIMOP_TYPE_DIV,
                                            numerator, denominator);
        UNPROTECT(save);
        return result;
    }
    case VALUE_TYPE_COMPLEX: {
        Vec *complex = getValue_Complex(value);
        MinExp *real = termValueToMinExp(parserInfo, complex->entries[0]);
        int save = PROTECT(real);
        MinExp *imag = termValueToMinExp(parserInfo, complex->entries[1]);
        PROTECT(imag);
        MinExp *result =
            makeNumericPrimExp(parserInfo, MINPRIMOP_TYPE_ADD, real, imag);
        UNPROTECT(save);
        return result;
    }
    default:
        cant_happen("unsupported ValueType %s in termValueToMinExp",
                    valueTypeName(value.type));
    }
}

static Term *minBigIntegerToTerm(ParserInfo parserInfo,
                                 MaybeBigInt *maybeBigInt) {
    switch (maybeBigInt->type) {
    case BI_SMALL:
        if (maybeBigInt->imag) {
            return makeTerm_Num(parserInfo,
                                value_Stdint_imag(maybeBigInt->small));
        }
        return makeTerm_Num(parserInfo, value_Stdint(maybeBigInt->small));
    case BI_BIG: {
        bigint copied;
        bigint_init(&copied);
        bigint_cpy(&copied, &maybeBigInt->big);
        BigInt *runtimeBigInt = newBigInt(copied);
        int save = PROTECT(runtimeBigInt);
        Term *result = makeTerm_Num(
            parserInfo, maybeBigInt->imag ? value_Bigint_imag(runtimeBigInt)
                                          : value_Bigint(runtimeBigInt));
        UNPROTECT(save);
        return result;
    }
    case BI_IRRATIONAL:
        if (maybeBigInt->imag) {
            return makeTerm_Num(parserInfo,
                                value_Irrational_imag(maybeBigInt->irrational));
        }
        return makeTerm_Num(parserInfo,
                            value_Irrational(maybeBigInt->irrational));
    default:
        cant_happen("unsupported MaybeBigIntType %d in minBigIntegerToTerm",
                    maybeBigInt->type);
    }
}

static MinExp *termBinaryOpToMinExp(ParserInfo parserInfo, MinPrimOp op,
                                    TermOp *termOp) {
    MinExp *left = termToMinExp(termOp->left);
    int save = PROTECT(left);
    MinExp *right = termToMinExp(termOp->right);
    PROTECT(right);
    MinExp *result = makeMinExp_Prim(parserInfo, op, left, right);
    UNPROTECT(save);
    return result;
}

static MinExp *termUnaryOpToMinExp(ParserInfo parserInfo, MinPrimOp op,
                                   Term *termOp) {
    MinExp *left = termToMinExp(termOp);
    int save = PROTECT(left);
    MinExp *result = makeMinExp_Prim(parserInfo, op, left, left);
    UNPROTECT(save);
    return result;
}

bool eqTerm(Term *t1, Term *t2) {
    if (t1 == t2)
        return true;
    if (t1 == NULL || t2 == NULL)
        return false;
    if (t1->type != t2->type)
        return false;
    switch (t1->type) {
    case TERM_TYPE_ADD:
        return (eqTerm(getTerm_Add(t1)->left, getTerm_Add(t2)->left) &&
                eqTerm(getTerm_Add(t1)->right, getTerm_Add(t2)->right)) ||
               (eqTerm(getTerm_Add(t1)->left, getTerm_Add(t2)->right) &&
                eqTerm(getTerm_Add(t1)->right, getTerm_Add(t2)->left));
    case TERM_TYPE_SUB:
        return eqTerm(getTerm_Sub(t1)->left, getTerm_Sub(t2)->left) &&
               eqTerm(getTerm_Sub(t1)->right, getTerm_Sub(t2)->right);
    case TERM_TYPE_MUL:
        return (eqTerm(getTerm_Mul(t1)->left, getTerm_Mul(t2)->left) &&
                eqTerm(getTerm_Mul(t1)->right, getTerm_Mul(t2)->right)) ||
               (eqTerm(getTerm_Mul(t1)->left, getTerm_Mul(t2)->right) &&
                eqTerm(getTerm_Mul(t1)->right, getTerm_Mul(t2)->left));
    case TERM_TYPE_DIV:
        return eqTerm(getTerm_Div(t1)->left, getTerm_Div(t2)->left) &&
               eqTerm(getTerm_Div(t1)->right, getTerm_Div(t2)->right);
    case TERM_TYPE_MOD:
        return eqTerm(getTerm_Mod(t1)->left, getTerm_Mod(t2)->left) &&
               eqTerm(getTerm_Mod(t1)->right, getTerm_Mod(t2)->right);
    case TERM_TYPE_POW:
        return eqTerm(getTerm_Pow(t1)->left, getTerm_Pow(t2)->left) &&
               eqTerm(getTerm_Pow(t1)->right, getTerm_Pow(t2)->right);
    case TERM_TYPE_GCD:
        return (eqTerm(getTerm_Gcd(t1)->left, getTerm_Gcd(t2)->left) &&
                eqTerm(getTerm_Gcd(t1)->right, getTerm_Gcd(t2)->right)) ||
               (eqTerm(getTerm_Gcd(t1)->left, getTerm_Gcd(t2)->right) &&
                eqTerm(getTerm_Gcd(t1)->right, getTerm_Gcd(t2)->left));
    case TERM_TYPE_LCM:
        return (eqTerm(getTerm_Lcm(t1)->left, getTerm_Lcm(t2)->left) &&
                eqTerm(getTerm_Lcm(t1)->right, getTerm_Lcm(t2)->right)) ||
               (eqTerm(getTerm_Lcm(t1)->left, getTerm_Lcm(t2)->right) &&
                eqTerm(getTerm_Lcm(t1)->right, getTerm_Lcm(t2)->left));
    case TERM_TYPE_NUM:
        return eqTermValue(getTerm_Num(t1), getTerm_Num(t2));
    case TERM_TYPE_CANON:
        return eqTerm(getTerm_Canon(t1), getTerm_Canon(t2));
    case TERM_TYPE_OTHER:
        return eqMinExp(getTerm_Other(t1), getTerm_Other(t2));
    default:
        cant_happen("unrecognised TermType %s in eqTerm",
                    termTypeName(t1->type));
    }
}

Term *minExpToTerm(struct MinExp *minExp) {
    if (minExp == NULL)
        return NULL;
    if (isMinExp_Stdint(minExp)) {
        return newTerm_Other(CPI(minExp), minExp);
    }
    if (isMinExp_BigInteger(minExp)) {
        return minBigIntegerToTerm(CPI(minExp), getMinExp_BigInteger(minExp));
    }
    if (isMinExp_Prim(minExp)) {
        MinPrimApp *prim = getMinExp_Prim(minExp);
        Term *left = minExpToTerm(prim->exp1);
        int save = PROTECT(left);
        Term *right = minExpToTerm(prim->exp2);
        PROTECT(right);
        Term *result = NULL;
        switch (prim->type) {
        case MINPRIMOP_TYPE_ADD:
            result = makeTerm_Add(CPI(minExp), left, right);
            break;
        case MINPRIMOP_TYPE_SUB:
            result = makeTerm_Sub(CPI(minExp), left, right);
            break;
        case MINPRIMOP_TYPE_MUL:
            result = makeTerm_Mul(CPI(minExp), left, right);
            break;
        case MINPRIMOP_TYPE_DIV:
            result = makeTerm_Div(CPI(minExp), left, right);
            break;
        case MINPRIMOP_TYPE_MOD:
            result = makeTerm_Mod(CPI(minExp), left, right);
            break;
        case MINPRIMOP_TYPE_POW:
            result = makeTerm_Pow(CPI(minExp), left, right);
            break;
        case MINPRIMOP_TYPE_GCD:
            result = makeTerm_Gcd(CPI(minExp), left, right);
            break;
        case MINPRIMOP_TYPE_LCM:
            result = makeTerm_Lcm(CPI(minExp), left, right);
            break;
        case MINPRIMOP_TYPE_CANON:
            // shouldn't expect to see these except in tests
            result = newTerm_Canon(CPI(minExp), left);
            break;
        default:
            result = newTerm_Other(CPI(minExp), minExp);
        }
        UNPROTECT(save);
        return result;
    } else {
        return newTerm_Other(CPI(minExp), minExp);
    }
}

MinExp *termToMinExp(Term *term) {
    if (term == NULL)
        return NULL;
    switch (term->type) {
    case TERM_TYPE_ADD:
        return termBinaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_ADD,
                                    getTerm_Add(term));
    case TERM_TYPE_SUB:
        return termBinaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_SUB,
                                    getTerm_Sub(term));
    case TERM_TYPE_MUL:
        return termBinaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_MUL,
                                    getTerm_Mul(term));
    case TERM_TYPE_DIV:
        return termBinaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_DIV,
                                    getTerm_Div(term));
    case TERM_TYPE_MOD:
        return termBinaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_MOD,
                                    getTerm_Mod(term));
    case TERM_TYPE_POW:
        return termBinaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_POW,
                                    getTerm_Pow(term));
    case TERM_TYPE_GCD:
        return termBinaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_GCD,
                                    getTerm_Gcd(term));
    case TERM_TYPE_LCM:
        return termBinaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_LCM,
                                    getTerm_Lcm(term));
    case TERM_TYPE_CANON:
        return termUnaryOpToMinExp(CPI(term), MINPRIMOP_TYPE_CANON,
                                   getTerm_Canon(term));
    case TERM_TYPE_NUM:
        return termValueToMinExp(CPI(term), getTerm_Num(term)->value);
    case TERM_TYPE_OTHER: {
        MinExp *other = getTerm_Other(term);
        if (isMinExp_Args(other)) {
            // args should never survive into real MinExp expressions; it exists
            // only to support the alternative normalize_2 path.
            cant_happen(
                "MINEXP_TYPE_ARGS should not appear in termToMinExp OTHER");
        }
        return other;
    }
    default:
        cant_happen("unrecognised TermType %s in termToMinExp",
                    termTypeName(term->type));
    }
}