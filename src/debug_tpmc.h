#ifndef cekf_debug_tpmc_h
#define cekf_debug_tpmc_h
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
 * Term Pattern Matching Compiler types
 *
 * generated from src/tpmc.yaml by makeAST.py
 */

#include "tpmc_helper.h"
#include "debug_lambda.h"
#include "lambda_pp.h"
#include "bigint.h"

void printTpmcMatchRules(struct TpmcMatchRules * x, int depth);
void printTpmcMatchRule(struct TpmcMatchRule * x, int depth);
void printTpmcComparisonPattern(struct TpmcComparisonPattern * x, int depth);
void printTpmcAssignmentPattern(struct TpmcAssignmentPattern * x, int depth);
void printTpmcConstructorPattern(struct TpmcConstructorPattern * x, int depth);
void printTpmcPattern(struct TpmcPattern * x, int depth);
void printTpmcTestState(struct TpmcTestState * x, int depth);
void printTpmcFinalState(struct TpmcFinalState * x, int depth);
void printTpmcState(struct TpmcState * x, int depth);
void printTpmcArc(struct TpmcArc * x, int depth);
void printTpmcArcList(struct TpmcArcList * x, int depth);
void printTpmcIntList(struct TpmcIntList * x, int depth);
void printTpmcPatternValue(struct TpmcPatternValue * x, int depth);
void printTpmcStateValue(struct TpmcStateValue * x, int depth);
void printTpmcMatchRuleArray(struct TpmcMatchRuleArray * x, int depth);
void printTpmcVariableArray(struct TpmcVariableArray * x, int depth);
void printTpmcPatternArray(struct TpmcPatternArray * x, int depth);
void printTpmcStateArray(struct TpmcStateArray * x, int depth);
void printTpmcArcArray(struct TpmcArcArray * x, int depth);
void printTpmcIntArray(struct TpmcIntArray * x, int depth);
void printTpmcMatrix(struct TpmcMatrix * x, int depth);

#endif
