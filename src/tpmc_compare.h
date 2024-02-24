#ifndef cekf_tpmc_compare_h
#    define cekf_tpmc_compare_h
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
 *
 * Term Pattern Matching Compiler logic
 */

#    include <stdbool.h>
#    include "tpmc.h"

bool tpmcStateEq(TpmcState *a, TpmcState *b);
bool tpmcStateValueEq(TpmcStateValue *a, TpmcStateValue *b);
bool tpmcTestStateEq(TpmcTestState *a, TpmcTestState *b);
bool tpmcArcArrayEq(TpmcArcArray *a, TpmcArcArray *b);
bool tpmcArcEq(TpmcArc *a, TpmcArc *b);
bool tpmcPatternEq(TpmcPattern *a, TpmcPattern *b);
bool tpmcPatternValueEq(TpmcPatternValue *a, TpmcPatternValue *b);
bool tpmcComparisonPatternEq(TpmcComparisonPattern *a,
                             TpmcComparisonPattern *b);
bool tpmcAssignmentPatternEq(TpmcAssignmentPattern *a,
                             TpmcAssignmentPattern *b);
bool tpmcConstructorPatternEq(TpmcConstructorPattern *a,
                              TpmcConstructorPattern *b);
bool tpmcPatternArrayEq(TpmcPatternArray *a, TpmcPatternArray *b);
bool tpmcArcInArray(TpmcArc *arc, TpmcArcArray *arcArray);

#endif
