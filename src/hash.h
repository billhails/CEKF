#ifndef cekf_hash_h
#define cekf_hash_h
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


#include <stdbool.h>

#include "common.h"
#include "memory.h"
#include "value.h"

#define HASH_MAX_LOAD 0.75

typedef struct HashSymbol {
    struct Header header;
    int type;
    hash_t hash;
    char *name;
} HashSymbol;

typedef struct HashEntry {
    struct HashSymbol *var;
    struct Value value;
} HashEntry;

typedef struct HashTable {
    struct Header header;
    int count;
    int capacity;
    HashEntry *entries;
} HashTable;

hash_t hashString(const char *string);
HashTable *newHashTable();
void hashSet(HashTable *table, struct HashSymbol *var, struct Value value);
struct Value hashGet(HashTable *table, struct HashSymbol *var);
struct HashSymbol *hashGetVar(HashTable *table, const char *name);
Value hashGet(HashTable *table, struct HashSymbol *var);
void hashAddCTVar(HashTable *table, struct HashSymbol *var);
bool hashLocate(HashTable *table, struct HashSymbol *var, int *location);
HashSymbol *uniqueHashSymbol(HashTable *table, int type, char *name);
void markHashSymbol(HashSymbol *x);
void freeHashSymbol(HashSymbol *x);

static inline void markHashSymbolObj(Header *h) { markHashSymbol((HashSymbol *)h); }
static inline void freeHashSymbolObj(Header *h) { freeHashSymbol((HashSymbol *)h); }
#endif
