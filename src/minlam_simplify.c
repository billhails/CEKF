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

#include "arithmetic.h"
#include "term_helper.h"

static inline bool isTermNum(Term *term) {
    return term != NULL && term->type == TERM_TYPE_NUM;
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

static Term *makeNumericOpResult(ParserInfo parserInfo, TermType op, Value left,
                                 Value right) {
    Value result;
    switch (op) {
    case TERM_TYPE_ADD:
        result = nadd(left, right);
        break;
    case TERM_TYPE_SUB:
        result = nsub(left, right);
        break;
    case TERM_TYPE_MUL:
        result = nmul(left, right);
        break;
    case TERM_TYPE_DIV:
        result = ndiv(left, right);
        break;
    case TERM_TYPE_MOD:
        result = nmod(left, right);
        break;
    case TERM_TYPE_POW:
        result = npow(left, right);
        break;
    default:
        cant_happen("unsupported term op %d in makeNumericOpResult", op);
    }
    return makeTerm_Num(parserInfo, result);
}

static Term *simplifyTerm(Term *term);

static Term *simplifyBinaryOp(Term *term, TermType op, TermOp *termOp) {
    Term *left = simplifyTerm(termOp->left);
    int save = PROTECT(left);
    Term *right = simplifyTerm(termOp->right);
    PROTECT(right);

    if (isTermNum(left) && isTermNum(right)) {
        Term *result = makeNumericOpResult(CPI(term), op, termNumValue(left),
                                           termNumValue(right));
        UNPROTECT(save);
        return result;
    }

    switch (op) {
    case TERM_TYPE_ADD:
        if (isTermNum(left) && isZeroValue(termNumValue(left))) {
            UNPROTECT(save);
            return right;
        }
        if (isTermNum(right) && isZeroValue(termNumValue(right))) {
            UNPROTECT(save);
            return left;
        }
        break;
    case TERM_TYPE_SUB:
        if (isTermNum(right) && isZeroValue(termNumValue(right))) {
            UNPROTECT(save);
            return left;
        }
        if (eqTerm(left, right)) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(0));
            UNPROTECT(save);
            return result;
        }
        break;
    case TERM_TYPE_MUL:
        if ((isTermNum(left) && isZeroValue(termNumValue(left))) ||
            (isTermNum(right) && isZeroValue(termNumValue(right)))) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(0));
            UNPROTECT(save);
            return result;
        }
        if (isTermNum(left) && isOneValue(termNumValue(left))) {
            UNPROTECT(save);
            return right;
        }
        if (isTermNum(right) && isOneValue(termNumValue(right))) {
            UNPROTECT(save);
            return left;
        }
        break;
    case TERM_TYPE_DIV:
        if (isTermNum(left) && isZeroValue(termNumValue(left))) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(0));
            UNPROTECT(save);
            return result;
        }
        if (isTermNum(right) && isOneValue(termNumValue(right))) {
            UNPROTECT(save);
            return left;
        }
        if (eqTerm(left, right)) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(1));
            UNPROTECT(save);
            return result;
        }
        break;
    case TERM_TYPE_MOD:
        if (isTermNum(left) && isZeroValue(termNumValue(left))) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(0));
            UNPROTECT(save);
            return result;
        }
        if (eqTerm(left, right)) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(0));
            UNPROTECT(save);
            return result;
        }
        break;
    case TERM_TYPE_POW:
        if (isTermNum(right) && isZeroValue(termNumValue(right))) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(1));
            UNPROTECT(save);
            return result;
        }
        if (isTermNum(right) && isOneValue(termNumValue(right))) {
            UNPROTECT(save);
            return left;
        }
        if (isTermNum(left) && isZeroValue(termNumValue(left))) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(0));
            UNPROTECT(save);
            return result;
        }
        if (isTermNum(left) && isOneValue(termNumValue(left))) {
            Term *result = makeTerm_Num(CPI(term), value_Stdint(1));
            UNPROTECT(save);
            return result;
        }
        break;
    default:
        break;
    }

    if (left != termOp->left || right != termOp->right) {
        Term *result = NULL;
        switch (op) {
        case TERM_TYPE_ADD:
            result = makeTerm_Add(CPI(term), left, right);
            break;
        case TERM_TYPE_SUB:
            result = makeTerm_Sub(CPI(term), left, right);
            break;
        case TERM_TYPE_MUL:
            result = makeTerm_Mul(CPI(term), left, right);
            break;
        case TERM_TYPE_DIV:
            result = makeTerm_Div(CPI(term), left, right);
            break;
        case TERM_TYPE_MOD:
            result = makeTerm_Mod(CPI(term), left, right);
            break;
        case TERM_TYPE_POW:
            result = makeTerm_Pow(CPI(term), left, right);
            break;
        default:
            cant_happen("unsupported term op %d in simplifyBinaryOp", op);
        }
        UNPROTECT(save);
        return result;
    }

    UNPROTECT(save);
    return term;
}

static Term *simplifyTerm(Term *term) {
    if (term == NULL)
        return NULL;

    switch (term->type) {
    case TERM_TYPE_ADD:
        return simplifyBinaryOp(term, TERM_TYPE_ADD, getTerm_Add(term));
    case TERM_TYPE_SUB:
        return simplifyBinaryOp(term, TERM_TYPE_SUB, getTerm_Sub(term));
    case TERM_TYPE_MUL:
        return simplifyBinaryOp(term, TERM_TYPE_MUL, getTerm_Mul(term));
    case TERM_TYPE_DIV:
        return simplifyBinaryOp(term, TERM_TYPE_DIV, getTerm_Div(term));
    case TERM_TYPE_MOD:
        return simplifyBinaryOp(term, TERM_TYPE_MOD, getTerm_Mod(term));
    case TERM_TYPE_POW:
        return simplifyBinaryOp(term, TERM_TYPE_POW, getTerm_Pow(term));
    case TERM_TYPE_NUM:
    case TERM_TYPE_OTHER:
        return term;
    default:
        cant_happen("unrecognised TermType %d in simplifyTerm", term->type);
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
