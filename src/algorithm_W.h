#ifndef cekf_algorithm_w_h
#define cekf_algorithm_w_h
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

#include "tin.h"
#include "ast.h"

typedef struct WResult {
    struct Header header;
    struct TinSubstitution *substitution;
    struct TinMonoType *monoType;
} WResult;

void markWResult(struct WResult *result);
void printWResult(struct WResult *result, int depth);

struct WResult *WTop(struct AstNest *nest);

#endif
