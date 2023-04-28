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

#include "exp.h"
#include "hash.h"
#include "memory.h"

HashTable *newHashTable() {
    HashTable *x = NEW(HashTable, OBJTYPE_HASHTABLE);
    x->count = 0;
    x->capacity = 0;
    x->entries = NULL;
    return x;
}

hash_t hashString(const char *string) {
    hash_t hash = 2166136261u;
    for (; *string != '\0'; string++) {
        hash ^= (uint8_t)*string;
        hash *= 16777619;
    }
    return hash;
}

void markHashTableObj(Header *h) {
}
