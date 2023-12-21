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

HashSymbol *negSymbol(void);
HashSymbol *notSymbol(void);
HashSymbol *hereSymbol(void);
HashSymbol *thenSymbol(void);
HashSymbol *backSymbol(void);
HashSymbol *errorSymbol(void);
HashSymbol *andSymbol(void);
HashSymbol *orSymbol(void);
HashSymbol *xorSymbol(void);
HashSymbol *eqSymbol(void);
HashSymbol *neSymbol(void);
HashSymbol *gtSymbol(void);
HashSymbol *ltSymbol(void);
HashSymbol *geSymbol(void);
HashSymbol *leSymbol(void);
HashSymbol *consSymbol(void);
HashSymbol *appendSymbol(void);
HashSymbol *addSymbol(void);
HashSymbol *subSymbol(void);
HashSymbol *mulSymbol(void);
HashSymbol *divSymbol(void);
HashSymbol *modSymbol(void);
HashSymbol *powSymbol(void);
HashSymbol *dotSymbol(void);
HashSymbol *ifSymbol(void);
HashSymbol *trueSymbol(void);
HashSymbol *falseSymbol(void);
HashSymbol *nilSymbol(void);
HashSymbol *arrowSymbol(void);
HashSymbol *intSymbol(void);
HashSymbol *charSymbol(void);
HashSymbol *boolSymbol(void);
HashSymbol *envSymbol(void);
HashSymbol *carSymbol(void);
HashSymbol *cdrSymbol(void);

#endif
