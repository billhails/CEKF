#ifndef cekf_hash_h
#  define cekf_hash_h
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

#  include <stdbool.h>

#  include "common.h"
#  include "memory.h"
#  include "types.h"

// Maximum fraction of hash table capacity occupied before growing the table.
#  define HASH_MAX_LOAD 0.75

// Type used for hash values
typedef uint32_t hash_t;

// Symbol used as a hash table key
typedef struct HashSymbol {
    hash_t hash;
    char *name;
} HashSymbol;

// Function types for marking and printing hash table values
typedef void (*MarkHashValueFunction)(void *value);
typedef void (*PrintHashValueFunction)(void *value, int depth);

// Hash table structure
typedef struct HashTable {
    struct Header header;
    int id;
    Index count;
    Index capacity;
    size_t valuesize;
    HashSymbol **keys;
    void *values;
    MarkHashValueFunction markfunction;
    PrintHashValueFunction printfunction;
    bool shortEntries;
} HashTable;

hash_t hashString(const char *string);

HashTable *newHashTable(size_t valuesize, MarkHashValueFunction markfunction,
                        PrintHashValueFunction printfunction);

void hashSet(HashTable *table, struct HashSymbol *var, void *src);
bool hashContains(HashTable *table, HashSymbol *var);
bool hashGet(HashTable *table, struct HashSymbol *var, void *dest);
void copyHashTable(HashTable *to, HashTable *from);

HashSymbol *hashGetVar(HashTable *table, const char *name);
HashSymbol *uniqueHashSymbol(HashTable *table, char *name, void *valuePtr);

extern bool quietPrintHashTable;

void printHashTable(HashTable *table, int depth);
void printHashSymbol(HashSymbol *symbol);

HashSymbol *iterateHashTable(HashTable *table, Index *index, void *data);

void markHashTable(HashTable *table);

#endif
