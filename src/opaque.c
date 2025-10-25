/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2024  Bill Hails
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
 * 
 * Opaque data type for wrapping arbitrary data structures.
 * Used by built-in functions to contain things like file handles that have no
 * intrinsic representation in the language.
 */

#include "opaque.h"
#include "common.h"

/**
 * Create a new opaque data structure.
 * 
 * @param data the data pointer
 * @param clean the clean-up operation (i.e. close etc.)
 * @param print the print operation
 * @return the new opaque data structure
 */
Opaque *newOpaque(void *data, OpaqueOperation clean, OpaqueOperation print) {
    Opaque *x = NEW(Opaque, OBJTYPE_OPAQUE);
    x->data = data;
    x->clean = clean;
    x->print = print;
    return x;
}

/**
 * Mark an opaque data structure for gc protection.
 * 
 * @param x the opaque data structure
 */
void markOpaque(Opaque *x) {
    if (x == NULL) return;
    MARK(x);
}

/**
 * Free an opaque data structure.
 * 
 * @param x the opaque data structure
 */
void freeOpaque(Opaque *x) {
    if (x->clean != NULL) x->clean(x->data);
    FREE(x, Opaque);
}

/**
 * Print an opaque data structure for debugging.
 * 
 * @param x the opaque data structure
 * @param depth indentation depth for pretty-printing
 */
void printOpaque(Opaque *x, int depth) {
    printf("%*s", depth * PAD_WIDTH, "");
    if (x == NULL)
        printf("<opaque:null>");
    else if (x->print)
        x->print(x->data);
    else
        printf("<opaque:%p>", x->data);
}

