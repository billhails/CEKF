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

#ifdef DEBUG_HASHTABLE
#include <stdio.h>
#endif

#include <string.h>

#include "common.h"
#include "cekf.h"
#include "hash.h"
#include "memory.h"

HashTable *newHashTable() {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "newHashTable()\n");
#endif
    HashTable *x = NEW(HashTable, OBJTYPE_HASHTABLE);
    x->count = 0;
    x->capacity = 0;
    x->entries = NULL;
    return x;
}

hash_t hashString(const char *string) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashString(%s)\n", string);
#endif
    hash_t hash = 2166136261u;
    for (; *string != '\0'; string++) {
        hash ^= (uint8_t)*string;
        hash *= 16777619;
    }
    return hash;
}

static HashEntry *findEntry(HashEntry *entries, int capacity, HashSymbol *var) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "findEntry()\n");
#endif
    hash_t index = var->hash & (capacity - 1);
    for (;;) {
        HashEntry *entry = &entries[index];
        if (entry->var == var || entry->var == NULL) {
            return entry;
        }
        index = (index + 1) & (capacity - 1);
    }
}

static void growCapacity(HashTable *table, int capacity) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "growCapacity(%d)\n", capacity);
#endif
    HashEntry *entries = NEW_ARRAY(HashEntry, capacity);

    for (int i = 0; i < capacity; i++) {
        entries[i].var = NULL;
        entries[i].value = vVoid;
    }

    for (int i = 0; i < table->capacity; i++) {
        HashEntry *entry = &table->entries[i];
        if (entry->var == NULL) continue;
        HashEntry *dest = findEntry(entries, capacity, entry->var);
        dest->var = entry->var;
        dest->value = entry->value;
    }

    FREE_ARRAY(HashEntry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

static void checkCapacity(HashTable *table) {
    if (table->count + 1 > table->capacity * HASH_MAX_LOAD) {
        int capacity = table->capacity < 8 ? 8 : table->capacity * 2;
        growCapacity(table, capacity);
    }
}

void hashSet(HashTable *table, HashSymbol *var, Value value) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashSet()\n");
#endif
    checkCapacity(table);
    HashEntry *entry = findEntry(table->entries, table->capacity, var);

    if (entry->var == NULL) table->count++;

    entry->var = var;
    entry->value = value;
}

void hashAddCTVar(HashTable *table, HashSymbol *var) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashSet()\n");
#endif
    checkCapacity(table);
    HashEntry *entry = findEntry(table->entries, table->capacity, var);
    if (entry->var != NULL) cant_happen("duplicate variable in hashAddCTVar");
    entry->var = var;
    entry->value.type = VALUE_TYPE_INTEGER;
    entry->value.val.z = table->count;
    table->count++;
}

bool hashLocate(HashTable *table, struct HashSymbol *var, int *location) {
    HashEntry *entry = findEntry(table->entries, table->capacity, var);
    if (entry->var == NULL) return false;

    if (entry->value.type != VALUE_TYPE_INTEGER) {
        cant_happen("non-integer value found in hashLocate");
    }
    (*location) = entry->value.val.z;
    return true;
}

Value hashGet(HashTable *table, HashSymbol *var) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashGet()\n");
#endif
    if (table->count == 0) return vVoid;
    HashEntry *entry = findEntry(table->entries, table->capacity, var);
    return entry->value;
}

HashSymbol *hashGetVar(HashTable *table, const char *name) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashGetVar()\n");
#endif
    if (table->count == 0) return NULL;
    hash_t hash = hashString(name);
    hash_t index = hash & (table->capacity - 1);

    for (;;) {
        if (table->entries[index].var == NULL) return NULL;
        if (   table->entries[index].var->hash == hash
            && strcmp(name, table->entries[index].var->name) == 0) {
            return table->entries[index].var;
        }
        index = (index + 1) & (table->capacity - 1);
    }
}

void markHashTableObj(Header *h) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "markHashTableObj()\n");
#endif
    HashTable *table = (HashTable *)h;
    if (table == NULL) return;
    if (MARKED(table)) return;
    MARK(table);
    for (int i = 0; i < table->capacity; i++) {
        if (table->entries[i].var != NULL) {
            markHashSymbol(table->entries[i].var);
            markValue(table->entries[i].value);
        }
    }
}

void freeHashTableObj(Header *h) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "freeHashTableObj()\n");
#endif
    HashTable *table = (HashTable *)h;
    if (table == NULL) return;
    if (table->count > 0) {
        FREE_ARRAY(HashEntry, table->entries, table->count);
    }
    FREE(h, HashTable);
}

HashSymbol *uniqueHashSymbol(HashTable *table, int type, char *name) {
    HashSymbol *x;
    x = hashGetVar(table, name);
    if (x != NULL) {
        return x;
    }
    x = NEW(HashSymbol, OBJTYPE_HASHSYMBOL);
    int save = PROTECT(x);
    x->type = type;
    x->name = safeStrdup(name);
    x->hash = hashString(name);
    hashSet(table, x, vVoid);
    UNPROTECT(save);
    return x;
}

void markHashSymbol(HashSymbol *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void freeHashSymbol(HashSymbol *x) {
    FREE(x, HashSymbol);
}
