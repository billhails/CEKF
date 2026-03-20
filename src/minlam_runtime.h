#ifndef cekf_minlam_runtime_h
#define cekf_minlam_runtime_h
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

#include "arithmetic_next.h"
#include "cekfs.h"
#include "common.h"
#include "init.h"
#include "types.h"
#include <math.h>
#include <stdarg.h>

static inline bool isTrue(Value v) { return getValue_Stdint(v) != 0; }

static inline Value vec(Value index, Value array) {
    return getValue_Vec(array)->entries[getValue_Stdint(index)];
}

Cmp minlam_runtime_cmp(Value left, Value right);

static inline Value eq(Value v1, Value v2) {
    return value_Stdint(minlam_runtime_cmp(v1, v2) == CMP_EQ);
}

static inline Value ne(Value v1, Value v2) {
    return value_Stdint(minlam_runtime_cmp(v1, v2) != CMP_EQ);
}

static inline Value lt(Value v1, Value v2) {
    return value_Stdint(minlam_runtime_cmp(v1, v2) == CMP_LT);
}

static inline Value gt(Value v1, Value v2) {
    return value_Stdint(minlam_runtime_cmp(v1, v2) == CMP_GT);
}

static inline Value le(Value v1, Value v2) {
    return value_Stdint(minlam_runtime_cmp(v1, v2) != CMP_GT);
}

static inline Value ge(Value v1, Value v2) {
    return value_Stdint(minlam_runtime_cmp(v1, v2) != CMP_LT);
}

static inline Value cmp(Value v1, Value v2) {
    // CMP_LT=-1, CMP_EQ=0, CMP_GT=1 but language-level lt=0, eq=1, gt=2
    return value_Stdint(minlam_runtime_cmp(v1, v2) + 1);
}

Value make_vec(int count, ...);

void minlam_runtime_mark_reg();

void minlam_runtime_init(Value *reg, int max_reg);
void minlam_runtime_unprotect();

BigInt *minlam_runtime_BigInt(int size, int capacity, int neg, ...);

#endif
