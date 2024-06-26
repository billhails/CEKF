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

#include "builtins_impl.h"
#include "arithmetic.h"

bool assertions_failed;
int assertions_accumulate = 0;

Value builtin_rand(Vec *v) {
    return nrand(v->entries[0]);
}

Value builtin_assert(Vec *v __attribute__((unused))) {
    assertions_failed = true;
    if (assertions_accumulate) {
        return vFalse;
    }
    exit(1);
}
