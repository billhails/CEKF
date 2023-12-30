#ifndef cekf_tc_debug_h
#define cekf_tc_debug_h
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
 * Structures to support type inference
 *
 * generated from src/tc.yaml by makeAST.py
 */

#include "tc_helper.h"

void printTcEnv(struct TcEnv * x, int depth);
void printTcNg(struct TcNg * x, int depth);
void printTcFunction(struct TcFunction * x, int depth);
void printTcPair(struct TcPair * x, int depth);
void printTcTypeDef(struct TcTypeDef * x, int depth);
void printTcTypeDefArgs(struct TcTypeDefArgs * x, int depth);
void printTcVar(struct TcVar * x, int depth);
void printTcType(struct TcType * x, int depth);

bool eqTcEnv(struct TcEnv * a, struct TcEnv * b, HashTable *map);
bool eqTcNg(struct TcNg * a, struct TcNg * b, HashTable *map);
bool eqTcFunction(struct TcFunction * a, struct TcFunction * b, HashTable *map);
bool eqTcPair(struct TcPair * a, struct TcPair * b, HashTable *map);
bool eqTcTypeDef(struct TcTypeDef * a, struct TcTypeDef * b, HashTable *map);
bool eqTcTypeDefArgs(struct TcTypeDefArgs * a, struct TcTypeDefArgs * b, HashTable *map);
bool eqTcVar(struct TcVar * a, struct TcVar * b, HashTable *map);
bool eqTcType(struct TcType * a, struct TcType * b, HashTable *map);

#endif
