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
    hash_t hash;
    char *name;
} HashSymbol;

typedef void (*MarkHashValueFunction)(void *value);
typedef void (*PrintHashValueFunction)(void *value, int depth);

typedef struct HashTable {
    struct Header header;
    int id;
    int count;
    int capacity;
    size_t valuesize;
    HashSymbol **keys;
    void *values;
    MarkHashValueFunction markfunction;
    PrintHashValueFunction printfunction;
    bool shortEntries;
} HashTable;

hash_t hashString(const char *string);

HashTable *newHashTable(size_t valuesize, MarkHashValueFunction markfunction, PrintHashValueFunction printfunction);

void hashSet(HashTable *table, struct HashSymbol *var, void *src);
bool hashContains(HashTable *table, HashSymbol *var);
bool hashGet(HashTable *table, struct HashSymbol *var, void *dest);
void copyHashTable(HashTable *to, HashTable *from);

HashSymbol *hashGetVar(HashTable *table, const char *name);
HashSymbol *uniqueHashSymbol(HashTable *table, char *name, void *valuePtr);

void markHashSymbol(HashSymbol *x);
void freeHashSymbol(HashSymbol *x);

extern bool quietPrintHashTable;

void printHashTable(HashTable *table, int depth);
void printHashSymbol(HashSymbol *symbol);

HashSymbol *iterateHashTable(HashTable *table, int *index, void *data);

void markHashTable(HashTable *table);

static inline void markHashSymbolObj(struct Header *h) { markHashSymbol((HashSymbol *)h); }
static inline void freeHashSymbolObj(struct Header *h) { freeHashSymbol((HashSymbol *)h); }
#endif
