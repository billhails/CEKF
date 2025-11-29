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

// Hash table implementation.
// Mostly a copy of Nystrom's clox implementation, with a few extensions.

#include <stdio.h>
#include <string.h>

#include "common.h"
#include "cekf.h"
#include "hash.h"
#include "memory.h"
#ifdef DEBUG_HASHTABLE
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

bool quietPrintHashTable = false;

static int idCounter = 0;

#ifdef DEBUG_LEAK
// allow very detailed specific debugging to be turned off and on at runtime
bool hash_debug_flag = false;
#endif

/**
 * Creates a new, empty hash table.
 * 
 * @param valuesize size of each value stored in the table, can be zero for no values.
 * @param markfunction function to mark each value during garbage collection, can be NULL if no values.
 * @param printfunction function to print each value, can be NULL if no values.
 * @return pointer to the newly created HashTable
 */
HashTable *newHashTable(size_t valuesize, MarkHashValueFunction markfunction,
                        PrintHashValueFunction printfunction) {
    DEBUG("newHashTable() [id=%d, valuesize=%lu]", idCounter, valuesize);
    HashTable *x = NEW(HashTable, OBJTYPE_HASHTABLE);
#ifdef DEBUG_ALLOC
    eprintf("%s:%-5d", __FILE__, __LINE__);
    eprintf("new HashTable %pn\n", x); // SimpleStruct.printNewFunction
#endif
    x->id = idCounter++;
    x->count = 0;
    x->capacity = 0;
    x->valuesize = valuesize;
    x->markfunction = markfunction;
    x->printfunction = printfunction;
    x->keys = NULL;
    x->values = NULL;
    x->shortEntries = false;
    return x;
}

/**
 * FNV-1a hash function for strings.
 * 
 * @param string null-terminated string to be hashed
 * @return hash value
 */
hash_t hashString(const char *string) {
    hash_t hash = 2166136261u;
    for (; *string != '\0'; string++) {
        hash ^= (uint8_t) * string;
        hash *= 16777619;
    }
    return hash;
}

/**
 * Returns a pointer to the value storage for the given index in the table.
 * 
 * @param table pointer to the HashTable
 * @param index index of the entry
 * @return pointer to the value storage, or NULL if valuesize is zero.
 */
static void *valuePtr(HashTable *table, int index) {
    if (table->valuesize == 0)
        return NULL;
    return (char *) table->values + (index * table->valuesize);
}

/**
 * Finds the index of the given variable in the keys array,
 * or the index where it should be inserted if not present.
 * Uses hashing to speed up the search.
 * 
 * @param keys array of HashSymbol pointers
 * @param capacity size of the keys array
 * @param key pointer to the HashSymbol to find
 * @return index of the found or insertion point
 */
static hash_t findEntry(HashSymbol **keys, int capacity, HashSymbol *key) {
    DEBUG("findEntry(%s)", key->name);
    hash_t index = key->hash & (capacity - 1);
    for (;;) {
        HashSymbol *k = keys[index];
        if (k == key || k == NULL) {
            return index;
        }
        index = (index + 1) & (capacity - 1);
    }
}

/**
 * Grows the capacity of the hash table to the given capacity.
 * Rehashes all existing entries into the new keys array.
 * Copies existing values to the new values array at their
 * new hash positions (if the table has values).
 * 
 * @param table pointer to the HashTable
 * @param capacity new capacity for the table
 */
static void growCapacity(HashTable *table, int capacity) {
    DEBUG("growCapacity(%d) [%d]", capacity, table->id);

    HashSymbol **keys = NEW_ARRAY(HashSymbol *, capacity);
    bzero(keys, sizeof(HashSymbol *) * capacity);

    void *values = NULL;
    if (table->valuesize > 0) {
        values = NEW_ARRAY(char, table->valuesize * capacity);
        bzero(values, table->valuesize * capacity);
    }

    for (Index old_index = 0; old_index < table->capacity; old_index++) {
        HashSymbol *var = table->keys[old_index];
        if (var == NULL)
            continue;
        hash_t new_index = findEntry(keys, capacity, var);
        keys[new_index] = var;
        if (table->valuesize > 0) {
            void *dst = (char *) values + (new_index * table->valuesize);
            void *src = valuePtr(table, old_index);
            memcpy(dst, src, table->valuesize);
        }
    }

    FREE_ARRAY(HashSymbol *, table->keys, table->capacity);
    FREE_ARRAY(char, table->values, table->valuesize * table->capacity);
    table->keys = keys;
    table->values = values;
    table->capacity = capacity;
}

/**
 * Checks if the table needs to grow, and grows it if necessary.
 * 
 * @param table pointer to the HashTable
 */
static void checkCapacity(HashTable *table) {
    if (table->count + 1 > table->capacity * HASH_MAX_LOAD) {
        int capacity = table->capacity < 8 ? 8 : table->capacity * 2;
        growCapacity(table, capacity);
    }
}

/**
 * Sets the given variable to the given value in the hash table.
 * If the variable is not already present, it is added.
 * Note the value is a pointer and the thing being pointed to is what gets copied.
 * This allows for tables with arbitrarily sized values.
 * 
 * @param table pointer to the HashTable
 * @param var pointer to the HashSymbol variable
 * @param value pointer to the value to copy, can be NULL.
 */
void hashSet(HashTable *table, HashSymbol *key, void *value) {
#if defined(DEBUG_HASHTABLE) || defined(DEBUG_LEAK)
    eprintf("hashSet(%s) [%d]\n", var->name, table->id);
#endif
    checkCapacity(table);
    hash_t index = findEntry(table->keys, table->capacity, key);
#ifdef DEBUG_LEAK
    eprintf("index == %d\n", index);
#endif

    if (table->keys[index] == NULL)
        table->count++;

    table->keys[index] = key;

    if (table->valuesize > 0 && value != NULL) {
        void *target = valuePtr(table, index);
#if defined(DEBUG_HASHTABLE) || defined(DEBUG_LEAK)
        eprintf("memcpy(%p, %p, %ld);\n", target, src, table->valuesize);
#  ifdef DEBUG_LEAK
        eprintf("// *%p == %p, table->values == %p\n", src, *((void **) src),
                table->values);
#  endif
#endif
        memcpy(target, value, table->valuesize);
    }
}

/**
 * Gets the value associated with the given variable in the hash table.
 * 
 * @param table pointer to the HashTable
 * @param var pointer to the HashSymbol variable
 * @param dest pointer to the destination where the value will be copied, can be NULL.
 * @return true if the variable is present, false otherwise
 */
bool hashGet(HashTable *table, HashSymbol *var, void *dest) {
    DEBUG("hashGet(%s) [%d]", var->name, table->id);
    if (table->count == 0)
        return false;
    hash_t index = findEntry(table->keys, table->capacity, var);
    if (table->keys[index] == NULL)
        return false;
    if (table->valuesize > 0 && dest != NULL) {
        void *src = valuePtr(table, index);
        memcpy(dest, src, table->valuesize);
    }
    return true;
}

/**
 * Looks up the key with the given name in the hash table.
 * 
 * @param table pointer to the HashTable
 * @param name null-terminated string name of the variable to find
 * @return pointer to the HashSymbol if found, NULL otherwise
 */
HashSymbol *hashGetVar(HashTable *table, const char *name) {
    DEBUG("hashGetVar() [%d]", table->id);
    if (table->count == 0)
        return NULL;
    hash_t hash = hashString(name);
    hash_t index = hash & (table->capacity - 1);

    for (;;) {
        if (table->keys[index] == NULL)
            return NULL;
        if (table->keys[index]->hash == hash
            && strcmp(name, table->keys[index]->name) == 0) {
            return table->keys[index];
        }
        index = (index + 1) & (table->capacity - 1);
    }
}

/**
 * Memory management access to markHashTable.
 * 
 * @param h pointer to the Header of the HashTable
 */
void markHashTableObj(Header *h) {
    markHashTable((HashTable *) h);
}

/**
 * Part of the mark phase of the mark-sweep garbage collection,
 * marks the given hash table and its contents.
 * 
 * Note that the keys (HashSymbol objects) are not themselves
 * memory-managed. It is assumed that symbols are permanent.
 *
 * @param table pointer to the HashTable to be marked
 */
void markHashTable(HashTable *table) {
    if (table == NULL)
        return;
    DEBUG("markHashTable() [%d]", table->id);
    if (MARKED(table))
        return;
    MARK(table);
    if (table->valuesize > 0 && table->markfunction != NULL) {
        for (Index i = 0; i < table->capacity; i++) {
            if (table->keys[i] != NULL) {
                DEBUG("markHashTable() [%d][%d][%p]", table->id, i,
                      (char *) table->values + (i * table->valuesize));
                table->markfunction((char *) table->values +
                                    (i * table->valuesize));
            }
        }
    }
}

/**
 * Frees the memory occupied by the given hash table.
 * 
 * @param h pointer to the Header of the HashTable
 */
void freeHashTableObj(Header *h) {
    HashTable *table = (HashTable *) h;
    DEBUG("freeHashTableObj() [%d]", table->id);
    if (table == NULL)
        return;
    if (table->count > 0) {
        DEBUG("freeHashTableObj keys: %p", table->keys);
        FREE_ARRAY(HashSymbol *, table->keys, table->capacity);
        if (table->valuesize > 0) {
            DEBUG("freeHashTableObj values: %p", table->values);
            FREE_ARRAY(char, table->values,
                       table->capacity * table->valuesize);
        }
    }
    FREE(h, HashTable);
}

/**
 * Ensures that the given name is associated with a unique HashSymbol
 * in the given hash table. If the name is already present,
 * the existing HashSymbol is returned. Otherwise a new
 * HashSymbol is created, added to the table with the given value,
 * and returned.
 * 
 * This should be the only function that directly creates HashSymbol objects.
 * 
 * @param table pointer to the HashTable.
 * @param name null-terminated string name of the variable.
 * @param src pointer to the value to associate with the variable, can be NULL.
 * @return pointer to the unique HashSymbol for the given name.
 */
HashSymbol *uniqueHashSymbol(HashTable *table, char *name, void *src) {
    HashSymbol *symbol;
    symbol = hashGetVar(table, name);
    if (symbol != NULL) {
        return symbol;
    }
    symbol = ALLOCATE(HashSymbol);
    symbol->name = safeStrdup(name);
    symbol->hash = hashString(name);
    hashSet(table, symbol, src);
    return symbol;
}

/**
 * Prints the given HashSymbol.
 * 
 * @param symbol pointer to the HashSymbol to be printed
 */
void printHashSymbol(HashSymbol *symbol) {
    eprintf("%s", symbol->name);
}

/**
 * Prints the given hash table and its contents.
 * 
 * @param table pointer to the HashTable to be printed
 * @param depth indentation depth for pretty printing
 */
void printHashTable(HashTable *table, int depth) {
    eprintf("%*s", depth * PAD_WIDTH, "");
    if (table == NULL) {
        eprintf("HashTable: (NULL)");
        return;
    }
    eprintf("HashTable %d: {", table->id);
    bool first = true;
    for (Index i = 0; i < table->capacity; ++i) {
        if (table->keys[i] != NULL) {
            if (first) {
                first = false;
                eprintf("\n");
            }
            eprintf("%*s", (depth + 1) * PAD_WIDTH, "");
            printHashSymbol(table->keys[i]);
            if (table->valuesize > 0 && table->printfunction != NULL
                && !quietPrintHashTable) {
                eprintf(" =>");
                if (table->shortEntries)
                    eprintf(" ");
                else
                    eprintf("\n");
                table->printfunction(valuePtr(table, i),
                                     table->shortEntries ? 0 : (depth + 2));
                eprintf("\n");
            } else {
                eprintf("\n");
            }
        }
    }
    if (first)
        eprintf("}");
    else
        eprintf("%*s}", depth * PAD_WIDTH, "");
}

/**
 * Iterates over the entries in the hash table.
 * 
 * @param table pointer to the HashTable
 * @param index pointer to the current index, updated by the function
 * @param data pointer to storage for the value, can be NULL.
 * @return pointer to the current HashSymbol, or NULL if end of table reached.
 */
HashSymbol *iterateHashTable(HashTable *table, Index *index, void *data) {
    while (*index < table->capacity) {
        if (table->keys[*index] != NULL) {
            if (data != NULL) {
                void *src = valuePtr(table, *index);
                if (src == NULL) {
                    memset(data, 0, table->valuesize);
                } else {
                    memcpy(data, src, table->valuesize);
                }
            }
            return table->keys[(*index)++];
        }
        (*index)++;
    }
    return NULL;
}

/**
 * Copies all entries from one hash table to another.
 * 
 * @param to pointer to the destination HashTable
 * @param from pointer to the source HashTable
 */
void copyHashTable(HashTable *to, HashTable *from) {
    if (from->valuesize != to->valuesize) {
        cant_happen
            ("attempt to copy between hash tables with different storage size: %ld vs %ld",
             from->valuesize, to->valuesize);
    }
    for (Index i = 0; i < from->capacity; ++i) {
        if (from->keys[i] != NULL) {
            void *data = valuePtr(from, i);
            hashSet(to, from->keys[i], data);
        }
    }
}
