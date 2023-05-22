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
#include "ast_helper.h"

static HashTable astSymbolTable;

void markAstSymbolTable() {
    astSymbolTable.header.keep = false;
    markHashTableObj((Header *) &astSymbolTable);
}


HashSymbol *getAstSymbol(AstSymbolType type, char *name) {
    return uniqueHashSymbol(&astSymbolTable, type, name);
}

void printAstSymbol(struct HashSymbol * x, int depth) {
    if (x == NULL) { printf("AstSymbol (NULL)"); return; }
    printf("AstSymbol[%d, \"%s\"]", x->type, x->name);
}
