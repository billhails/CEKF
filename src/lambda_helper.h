#ifndef cekf_lambda_helper_h
#  define cekf_lambda_helper_h
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

#  include "lambda.h"
#  include "lambda_debug.h"
#  include "hash.h"
#  include "memory.h"

#define NS_FORMAT "ns$%u"

void printLambdaSymbol(HashSymbol *x, int depth);
LamTypeConstructorInfo *lookupConstructorInLamContext(LamContext *context, HashSymbol *var);
LamContext *lookupNamespaceInLamContext(LamContext *context, Index index);
LamTypeConstructorInfo *lookupScopedAstConstructorInLamContext(LamContext *context, AstLookupOrSymbol *scoped);
LamTypeConstructorInfo *lookupScopedLamConstructorInLamContext(LamContext *context, LamLookupOrSymbol *scoped);
LamTypeConstructorInfo *lookupScopedLamSymbolInLamContext(LamContext *context, LamLookupSymbol *lookup);
LamTypeConstructorInfo *lookupScopedAstSymbolInLamContext(LamContext *context, AstLookupSymbol *lookup);
int lookupCurrentNamespaceInLamContext(LamContext *context);
LamTypeConstructorType *lookupConstructorTypeInLamContext(LamContext *context, HashSymbol *var);
void markLamExpFn(void *ptr);
void printLamExpFn(void *ptr, int depth);
#endif
