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

void ppTpmcMatrix(TpmcMatrix *matrix);
void ppTpmcPattern(TpmcPattern *pattern);
void ppTpmcPatternValue(TpmcPatternValue *patternValue);
void ppTpmcComparisonPattern(TpmcComparisonPattern *comparisonPattern);
void ppTpmcAssignmentPattern(TpmcAssignmentPattern *assignmentPattern);
void ppTpmcConstructorPattern(TpmcConstructorPattern *constructorPattern);
void ppTpmcPatternArray(TpmcPatternArray *patternArray);
void ppTpmcState(TpmcState *state);
void ppTpmcVariableTable(SymbolSet *table);
void ppTpmcSymbol(HashSymbol *symbol);
void ppTpmcStateValue(TpmcStateValue *value);
void ppTpmcTestState(TpmcTestState *test);
void ppTpmcArcArray(TpmcArcArray *arcs);
void ppTpmcArc(TpmcArc *arc);
void ppTpmcFinalState(TpmcFinalState *final);
void ppTpmcIntArray(IntArray *array);
void ppTpmcStateArray(TpmcStateArray *array);

#endif
