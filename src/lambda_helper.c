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
    fprintf(stderr, "%*s", depth * 4, "");
    if (x == NULL) { fprintf(stderr, "LambdaSymbol (NULL)"); return; }
    fprintf(stderr, "AstSymbol[\"%s\"]", x->name);
}

void markLamExpFn(void *ptr) {
    markLamExp(*((LamExp **) ptr));
}

void printLamExpFn(void *ptr, int depth) {
    ppLamExpD(*((LamExp **) ptr), depth);
}

static void printLamContextFn(void *ptr, int depth) {
    fprintf(stderr, "%*s", depth * 4, "");
    printLamTypeConstructorInfo(*(LamTypeConstructorInfo **)ptr, depth);
}

static void markLamContextFn(void *ptr) {
    markLamTypeConstructorInfo(*(LamTypeConstructorInfo **)ptr);
}

static HashTable *newLamContextTable() {
    HashTable *h = newHashTable(
        sizeof(LamTypeConstructorInfo *),
        markLamContextFn,
        printLamContextFn
    );
    h->shortEntries = false;
    return h;
}

void addToLamContext(LamContext *context, HashSymbol *symbol, LamTypeConstructorInfo *info) {
    hashSet(context->frame, symbol, &info);
}

LamTypeConstructorInfo *lookupInLamContext(LamContext *context, HashSymbol *var) {
    if (context == NULL) return NULL;
    LamTypeConstructorInfo *result;
    if (hashGet(context->frame, var, &result)) {
        return result;
    }
    return lookupInLamContext(context->parent, var);
}

LamContext *extendLamContext(LamContext *parent) {
    HashTable *frame = newLamContextTable();
    int save = PROTECT(frame);
    LamContext *context = newLamContext(frame, parent);
    UNPROTECT(save);
    return context;
}
