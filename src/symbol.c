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
#include "symbol.h"

// application-wide global symbol store

static HashTable *symbolTable = NULL;

void markVarTable() {
    markHashTableObj((Header *) symbolTable);
}

HashSymbol *newSymbol(char *name) {
    if (symbolTable == NULL) {
        symbolTable = newHashTable(0, NULL, NULL);
    }
    return uniqueHashSymbol(symbolTable, name, NULL);
}


HashSymbol *genSym(char *prefix) {
    static int symbolCounter = 0;
    char buffer[128];

    for (;;) {
        sprintf(buffer, "%s%d", prefix, symbolCounter++);
        if (hashGetVar(symbolTable, buffer) == NULL) {
            HashSymbol *x = NEW(HashSymbol, OBJTYPE_HASHSYMBOL);
            int save = PROTECT(x);
            x->name = safeStrdup(buffer);
            x->hash = hashString(buffer);
            hashSet(symbolTable, x, NULL);
            UNPROTECT(save);
            return x;
        }
    }
}

