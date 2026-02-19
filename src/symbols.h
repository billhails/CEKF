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

HashSymbol *addSymbol(void);
HashSymbol *appendSymbol(void);
HashSymbol *arrowSymbol(void);
HashSymbol *assertSymbol(void);
HashSymbol *backSymbol(void);
HashSymbol *boolSymbol(void);
HashSymbol *canonSymbol(void);
HashSymbol *carSymbol(void);
HashSymbol *cdrSymbol(void);
HashSymbol *charSymbol(void);
HashSymbol *cmpSymbol(void);
HashSymbol *colonSymbol(void);
HashSymbol *consSymbol(void);
HashSymbol *currentFileSymbol(void);
HashSymbol *currentLineSymbol(void);
HashSymbol *divSymbol(void);
HashSymbol *envSymbol(void);
HashSymbol *eqSymbol(void);
HashSymbol *errorSymbol(void);
HashSymbol *falseSymbol(void);
HashSymbol *fnErrorSymbol(void);
HashSymbol *geSymbol(void);
HashSymbol *gcdSymbol(void);
HashSymbol *gtSymbol(void);
HashSymbol *hereSymbol(void);
HashSymbol *ifSymbol(void);
HashSymbol *leftCurlySymbol(void);
HashSymbol *leftRoundSymbol(void);
HashSymbol *leftSquareSymbol(void);
HashSymbol *leSymbol(void);
HashSymbol *listSymbol(void);
HashSymbol *ltSymbol(void);
HashSymbol *lcmSymbol(void);
HashSymbol *modSymbol(void);
HashSymbol *mulSymbol(void);
HashSymbol *nameSpacesSymbol(void);
HashSymbol *nameSpaceSymbol(void);
HashSymbol *negSymbol(void);
HashSymbol *neSymbol(void);
HashSymbol *nilSymbol(void);
HashSymbol *powSymbol(void);
HashSymbol *putsSymbol(void);
HashSymbol *questionMarkSymbol(void);
HashSymbol *rightCurlySymbol(void);
HashSymbol *rightRoundSymbol(void);
HashSymbol *rightSquareSymbol(void);
HashSymbol *spaceshipSymbol(void);
HashSymbol *subSymbol(void);
HashSymbol *thenSymbol(void);
HashSymbol *trueSymbol(void);

#endif
