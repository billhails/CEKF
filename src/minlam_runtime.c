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

Value *minlam_runtime_reg = NULL;
int minlam_runtime_max_reg = 0;

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