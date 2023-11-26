#ifndef cekf_symbols_h
#define cekf_symbols_h
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

#include "symbol.h"

HashSymbol *negSymbol();
HashSymbol *notSymbol();
HashSymbol *hereSymbol();
HashSymbol *thenSymbol();
HashSymbol *backSymbol();
HashSymbol *errorSymbol();
HashSymbol *andSymbol();
HashSymbol *orSymbol();
HashSymbol *xorSymbol();
HashSymbol *eqSymbol();
HashSymbol *neSymbol();
HashSymbol *gtSymbol();
HashSymbol *ltSymbol();
HashSymbol *geSymbol();
HashSymbol *leSymbol();
HashSymbol *consSymbol();
HashSymbol *appendSymbol();
HashSymbol *addSymbol();
HashSymbol *subSymbol();
HashSymbol *mulSymbol();
HashSymbol *divSymbol();
HashSymbol *modSymbol();
HashSymbol *powSymbol();
HashSymbol *dotSymbol();
HashSymbol *ifSymbol();
HashSymbol *trueSymbol();
HashSymbol *falseSymbol();
HashSymbol *nilSymbol();
HashSymbol *arrowSymbol();
HashSymbol *intSymbol();
HashSymbol *charSymbol();
HashSymbol *boolSymbol();
HashSymbol *envSymbol();

#endif
