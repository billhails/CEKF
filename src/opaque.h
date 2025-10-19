#ifndef cekf_opaque_h
#  define cekf_opaque_h
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

#  include "memory.h"

struct Opaque;

typedef void (*OpaqueOperation)(struct Opaque *);

typedef struct Opaque {
    Header header;
    void *data;
    OpaqueOperation clean;
    OpaqueOperation print;
} Opaque;

Opaque *newOpaque(void *data, OpaqueOperation clean, OpaqueOperation print);
void markOpaque(Opaque *);
void freeOpaque(Opaque *);
void printOpaque(Opaque *, int);

#endif
