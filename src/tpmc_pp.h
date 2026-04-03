#ifndef cekf_tpmc_pp_h
#define cekf_tpmc_pp_h
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

#include "tpmc.h"
#include <stdio.h>

void ppTpmcMatrix(FILE *out, TpmcMatrix *matrix);
void ppTpmcPattern(FILE *out, TpmcPattern *pattern);
void ppTpmcPatternValue(FILE *out, TpmcPatternValue *patternValue);
void ppTpmcComparisonPattern(FILE *out,
                             TpmcComparisonPattern *comparisonPattern);
void ppTpmcAssignmentPattern(FILE *out,
                             TpmcAssignmentPattern *assignmentPattern);
void ppTpmcConstructorPattern(FILE *out,
                              TpmcConstructorPattern *constructorPattern);
void ppTpmcPatternArray(FILE *out, TpmcPatternArray *patternArray);
void ppTpmcState(FILE *out, TpmcState *state);
void ppTpmcVariableTable(FILE *out, SymbolSet *table);
void ppTpmcSymbol(FILE *out, HashSymbol *symbol);
void ppTpmcStateValue(FILE *out, TpmcStateValue *value);
void ppTpmcTestState(FILE *out, TpmcTestState *test);
void ppTpmcArcArray(FILE *out, TpmcArcArray *arcs);
void ppTpmcArc(FILE *out, TpmcArc *arc);
void ppTpmcFinalState(FILE *out, TpmcFinalState *final);
void ppTpmcIntArray(FILE *out, IntArray *array);
void ppTpmcStateArray(FILE *out, TpmcStateArray *array);

#endif
