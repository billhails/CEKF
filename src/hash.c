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

// #define DEBUG_HASHTABLE

#ifdef DEBUG_HASHTABLE
#include <stdio.h>
#endif

#include <string.h>

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

static HashEntry *findEntry(HashEntry *entries, int capacity, AexpVar *var) {
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
        entries[i].value = NULL;
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

void hashSet(HashTable *table, AexpVar *var, Value *value) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashSet()\n");
#endif
    if (table->count + 1 > table->capacity * HASH_MAX_LOAD) {
        int capacity = table->capacity < 8 ? 8 : table->capacity * 2;
        growCapacity(table, capacity);
    }
    HashEntry *entry = findEntry(table->entries, table->capacity, var);

    if (entry->var == NULL) table->count++;

    entry->var = var;
    entry->value = value;
}

Value *hashGet(HashTable *table, AexpVar *var) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashGet()\n");
#endif
    if (table->count == 0) return NULL;
    HashEntry *entry = findEntry(table->entries, table->capacity, var);
    return entry->value;
}

AexpVar *hashGetVar(HashTable *table, const char *name) {
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
            markAexpVar(table->entries[i].var);
            markValue(table->entries[i].value);
        }
    }
}
