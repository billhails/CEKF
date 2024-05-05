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
#include "symbol.h"

AstNameSpaceArray *nameSpaces = NULL;

void printAstSymbol(struct HashSymbol *x, int depth) {
    eprintf("%*s", depth * PAD_WIDTH, "");
    if (x == NULL) {
        eprintf("AstSymbol (NULL)");
        return;
    }
    eprintf("AstSymbol[\"%s\"]", x->name);
}

void markNameSpaces() {
    markAstNameSpaceArray(nameSpaces);
}

void initNameSpaces() {
    if (nameSpaces == NULL) {
        nameSpaces = newAstNameSpaceArray();
    }
}

int lookupNameSpace(AgnosticFileId *id) {
#ifdef SAFETY_CHECKS
    if (nameSpaces == NULL) {
        cant_happen("null namespace");
    }
#endif
    for (Index i = 0; i < nameSpaces->size; i++) {
        if (cmpAgnosticFileId(id, nameSpaces->entries[i]->id) == CMP_EQ) {
            return (int) i;
        }
    }
    return -1;
}

AstProg *astNestToProg(AstNest *nest) {
#ifdef SAFETY_CHECKS
    if (nameSpaces == NULL) {
        cant_happen("null namespace");
    }
#endif
    return newAstProg(nest->definitions, nameSpaces, nest->expressions);
}
