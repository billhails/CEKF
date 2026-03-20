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

#include "minlam_runtime.h"

static Value *minlam_runtime_reg = NULL;
static int minlam_runtime_max_reg = 0;

static int minlam_runtime_save;

void minlam_runtime_init(Value *reg, int max_reg, int argc, char **argv) {
    minlam_runtime_reg = reg;
    minlam_runtime_max_reg = max_reg;
    builtin_args_argc = argc;
    builtin_args_cargc = 1;
    builtin_args_argv = argv;
    initAll();
    minlam_runtime_save = PROTECT(NULL);
}

void minlam_runtime_unprotect() { UNPROTECT(minlam_runtime_save); }

Value make_vec(int count, ...) {
    va_list ap;
    va_start(ap, count);
    Vec *v = newVec(count);
    int save = PROTECT(v);
    for (int j = 0; j < count; j++) {
        v->entries[j] = va_arg(ap, Value);
    }
    va_end(ap);
    UNPROTECT(save);
    return value_Vec(v);
}

void minlam_runtime_mark_reg() {
    if (minlam_runtime_reg != NULL) {
        for (int i = 0; i < minlam_runtime_max_reg; i++) {
            markValue(minlam_runtime_reg[i]);
        }
    }
}

static Cmp _vecCmp(Vec *left, Vec *right) {
    if (left == right) {
        return CMP_EQ;
    }
#ifdef SAFETY_CHECKS
    if (left == NULL || right == NULL) {
        cant_happen("null vecs in _vecCmp(%p, %p)", left, right);
    }
#endif
    for (Index i = 0; i < left->size && i < right->size; ++i) {
        int cmp = minlam_runtime_cmp(left->entries[i], right->entries[i]);
        if (cmp != CMP_EQ)
            return cmp;
    }
    if (left->size < right->size) {
        return CMP_LT;
    }
    if (left->size > right->size) {
        return CMP_GT;
    }
    return CMP_EQ;
}

#define _CMP_(left, right)                                                     \
    ((left) < (right) ? CMP_LT : (left) == (right) ? CMP_EQ : CMP_GT)

Cmp minlam_runtime_cmp(Value left, Value right) {
#ifdef SAFETY_CHECKS
    if (left.type != right.type) {
        switch (left.type) {
        case VALUE_TYPE_BIGINT:
        case VALUE_TYPE_STDINT:
        case VALUE_TYPE_RATIONAL:
        case VALUE_TYPE_IRRATIONAL:
        case VALUE_TYPE_BIGINT_IMAG:
        case VALUE_TYPE_STDINT_IMAG:
        case VALUE_TYPE_IRRATIONAL_IMAG:
        case VALUE_TYPE_RATIONAL_IMAG:
        case VALUE_TYPE_COMPLEX:
            switch (right.type) {
            case VALUE_TYPE_BIGINT:
            case VALUE_TYPE_STDINT:
            case VALUE_TYPE_RATIONAL:
            case VALUE_TYPE_IRRATIONAL:
            case VALUE_TYPE_BIGINT_IMAG:
            case VALUE_TYPE_STDINT_IMAG:
            case VALUE_TYPE_IRRATIONAL_IMAG:
            case VALUE_TYPE_RATIONAL_IMAG:
            case VALUE_TYPE_COMPLEX:
                break;
            default:
                cant_happen("different types in _cmp %s vs %s",
                            valueTypeName(left.type),
                            valueTypeName(right.type));
            }
            break;
        default:
            cant_happen("different types in _cmp %s vs %s",
                        valueTypeName(left.type), valueTypeName(right.type));
        }
    }
#endif
    switch (left.type) {
    case VALUE_TYPE_NONE:
        return 0;
    case VALUE_TYPE_BIGINT:
    case VALUE_TYPE_STDINT:
    case VALUE_TYPE_RATIONAL:
    case VALUE_TYPE_IRRATIONAL:
    case VALUE_TYPE_BIGINT_IMAG:
    case VALUE_TYPE_STDINT_IMAG:
    case VALUE_TYPE_RATIONAL_IMAG:
    case VALUE_TYPE_IRRATIONAL_IMAG:
    case VALUE_TYPE_COMPLEX:
        return ncmp(left, right);
    case VALUE_TYPE_CHARACTER:
        return _CMP_(left.val.character, right.val.character);
    case VALUE_TYPE_PCLO:
        return _CMP_(left.val.addr, right.val.addr);
    case VALUE_TYPE_VEC:
        return _vecCmp(left.val.vec, right.val.vec);
    default:
        cant_happen("unexpected type for _cmp (%s)", valueTypeName(left.type));
    }
}

BigInt *minlam_runtime_BigInt(int size, int capacity, int neg, ...) {
    va_list ap;
    va_start(ap, neg);
    bigint a;
    bigint_init(&a);
    bigint_reserve(&a, capacity);
    for (int j = 0; j < capacity; j++) {
        a.words[j] = va_arg(ap, bigint_word);
    }
    va_end(ap);
    a.size = size;
    a.neg = neg;
    BigInt *result = newBigInt(a);
    PROTECT(result);
    return result;
}