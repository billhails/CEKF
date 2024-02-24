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

#include <stdio.h>

#include "lambda_helper.h"
#include "lambda_pp.h"

void printLambdaSymbol(HashSymbol *x, int depth) {
    eprintf("%*s", depth * PAD_WIDTH, "");
    if (x == NULL) {
        eprintf("LambdaSymbol (NULL)");
        return;
    }
    eprintf("AstSymbol[\"%s\"]", x->name);
}

void markLamExpFn(void *ptr) {
    markLamExp(*((LamExp **) ptr));
}

void printLamExpFn(void *ptr, int depth) {
    ppLamExpD(*((LamExp **) ptr), depth);
}

void addToLamContext(LamContext *context, HashSymbol *symbol,
                     LamTypeConstructorInfo *info) {
    setLamInfoTable(context->frame, symbol, info);
}

LamTypeConstructorInfo *lookupInLamContext(LamContext *context,
                                           HashSymbol *var) {
    if (context == NULL)
        return NULL;
    LamTypeConstructorInfo *result;
    if (getLamInfoTable(context->frame, var, &result)) {
        return result;
    }
    return lookupInLamContext(context->parent, var);
}

LamContext *extendLamContext(LamContext *parent) {
    return newLamContext(parent);
}
