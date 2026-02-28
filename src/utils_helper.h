#ifndef cekf_utils_helper_h
#define cekf_utils_helper_h
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

#include "utils.h"

SCharVec *stringToSCharVec(char *str);     // keeps null terminator
SCharArray *stringToSCharArray(char *str); // drops null terminator
void appendStringToSCharArray(SCharArray *array,
                              char *str);     // drops null terminator
SCharVec *sCharArrayToVec(SCharArray *array); // adds null terminator
SCharArray *sCharVecToArray(SCharVec *vec);   // drops null terminator

WCharVec *wCharArrayToVec(WCharArray *array); // adds null terminator
WCharArray *wCharVecToArray(WCharVec *vec);   // drops null terminator

SymbolSet *symbolListToSet(SymbolList *list);
SymbolList *symbolSetToList(ParserInfo PI, SymbolSet *set);
SymbolSet *excludeSymbol(HashSymbol *var, SymbolSet *symbols);
SymbolSet *copySymbolSet(SymbolSet *symbols);
SymbolSet *symbolsNotInList(SymbolList *vars, SymbolSet *symbols);
bool symbolInList(HashSymbol *var, SymbolList *vars);
SymbolSet *excludeSymbols(SymbolList *vars, SymbolSet *symbols);
bool anySymbolInSet(SymbolList *vars, SymbolSet *symbols);
bool allSymbolsInSet(SymbolList *vars, SymbolSet *symbols);
SymbolSet *unionSymbolSet(SymbolSet *a, SymbolSet *b);
SymbolSet *intersectSymbolSet(SymbolSet *a, SymbolSet *b);
SymbolSet *differenceSymbolSet(SymbolSet *a, SymbolSet *b);
bool eqSymbolSet(SymbolSet *a, SymbolSet *b);

SymbolMap *copySymbolMap(SymbolMap *orig);

#endif
