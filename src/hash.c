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
#    include "debugging_on.h"
#else
#    include "debugging_off.h"
#endif

bool quietPrintHashTable = false;

static int idCounter = 0;

#ifdef DEBUG_LEAK
// allow very detailed specific debugging to be turned off and on at runtime
bool hash_debug_flag = false;
#endif

#ifdef DEBUG_HASHTABLE
static void printMemHeader(char *id, void *ptr) {
    /*
       if (ptr == NULL) return;
       int size = 16;

       char *b = (char *)ptr;
       b -= size;
       eprintf("%s mem header", id);
       for (int i = 0; i < size; i++) {
       eprintf(" %02x", b[i]);
       }
       eprintf("\n");
     */
}
#endif

HashTable *newHashTable(size_t valuesize, MarkHashValueFunction markfunction,
                        PrintHashValueFunction printfunction) {
    DEBUG("newHashTable() [id=%d, valuesize=%lu]", idCounter, valuesize);
    HashTable *x = NEW(HashTable, OBJTYPE_HASHTABLE);
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

hash_t hashString(const char *string) {
    hash_t hash = 2166136261u;
    for (; *string != '\0'; string++) {
        hash ^= (uint8_t) * string;
        hash *= 16777619;
    }
    return hash;
}

static void *valuePtr(HashTable *table, int index) {
    if (table->valuesize == 0)
        return NULL;
    return (char *) table->values + (index * table->valuesize);
}

static hash_t findEntry(HashSymbol **keys, int capacity, HashSymbol *var) {
    DEBUG("findEntry(%s)", var->name);
    hash_t index = var->hash & (capacity - 1);
    for (;;) {
        HashSymbol *key = keys[index];
        if (key == var || key == NULL) {
            return index;
        }
        index = (index + 1) & (capacity - 1);
    }
}

static void growCapacity(HashTable *table, int capacity) {
    DEBUG("growCapacity(%d) [%d]", capacity, table->id);
    HashSymbol **keys = NEW_ARRAY(HashSymbol *, capacity);
    DEBUG("growCapacity old keys: %p new keys: %p", table->keys, keys);
    IFDEBUG(printMemHeader("keys", keys));
    void *values = NULL;

    for (int i = 0; i < capacity; i++) {
        keys[i] = NULL;
    }

    if (table->valuesize > 0) {
        values = NEW_ARRAY(char, table->valuesize * capacity);
        bzero(values, table->valuesize * capacity);
        DEBUG("growCapacity old values: %p new values: %p", table->values,
              values);
        IFDEBUG(printMemHeader("values", values));
    }

    for (int old_index = 0; old_index < table->capacity; old_index++) {
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

static void checkCapacity(HashTable *table) {
    if (table->count + 1 > table->capacity * HASH_MAX_LOAD) {
        int capacity = table->capacity < 8 ? 8 : table->capacity * 2;
        growCapacity(table, capacity);
    }
}

void hashSet(HashTable *table, HashSymbol *var, void *src) {
#if defined(DEBUG_HASHTABLE) || defined(DEBUG_LEAK)
    eprintf("hashSet(%s) [%d]\n", var->name, table->id);
#endif
    checkCapacity(table);
    hash_t index = findEntry(table->keys, table->capacity, var);
#ifdef DEBUG_LEAK
    eprintf("index == %d\n", index);
#endif

    if (table->keys[index] == NULL)
        table->count++;

    table->keys[index] = var;

    if (table->valuesize > 0 && src != NULL) {
        void *target = valuePtr(table, index);
#if defined(DEBUG_HASHTABLE) || defined(DEBUG_LEAK)
        eprintf("memcpy(%p, %p, %ld);\n", target, src, table->valuesize);
#    ifdef DEBUG_LEAK
        eprintf("// *%p == %p, table->values == %p\n", src, *((void **) src),
                table->values);
#    endif
#endif
        memcpy(target, src, table->valuesize);
    }
}

bool hashContains(HashTable *table, HashSymbol *var) {
    DEBUG("hashContains(%s) [%d]", var->name, table->id);
    if (table->count == 0)
        return false;
    hash_t index = findEntry(table->keys, table->capacity, var);
    if (table->keys[index] == NULL)
        return false;
    return true;
}

bool hashGet(HashTable *table, HashSymbol *var, void *dest) {
    DEBUG("hashGet(%s) [%d]", var->name, table->id);
    IFDEBUG(printMemHeader("values", table->values));
    IFDEBUG(printMemHeader("keys", table->keys));
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

HashSymbol *hashGetVar(HashTable *table, const char *name) {
    DEBUG("hashGetVar() [%d]", table->id);
    IFDEBUG(printMemHeader("values", table->values));
    IFDEBUG(printMemHeader("keys", table->keys));
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

void markHashTableObj(Header *h) {
    markHashTable((HashTable *) h);
}

void markHashTable(HashTable *table) {
    if (table == NULL)
        return;
    DEBUG("markHashTable() [%d]", table->id);
    IFDEBUG(printMemHeader("values", table->values));
    IFDEBUG(printMemHeader("keys", table->keys));
    if (MARKED(table))
        return;
    MARK(table);
    for (int i = 0; i < table->capacity; i++) {
        if (table->keys[i] != NULL) {
            markHashSymbol(table->keys[i]);
            if (table->valuesize > 0 && table->markfunction != NULL) {
                DEBUG("markHashTable() [%d][%d][%p]", table->id, i,
                      (char *) table->values + (i * table->valuesize));
                table->markfunction((char *) table->values +
                                    (i * table->valuesize));
            }
        }
    }
}

void freeHashTableObj(Header *h) {
    HashTable *table = (HashTable *) h;
    DEBUG("freeHashTableObj() [%d]", table->id);
    IFDEBUG(printMemHeader("values", table->values));
    IFDEBUG(printMemHeader("keys", table->keys));
    if (table == NULL)
        return;
    if (table->count > 0) {
        DEBUG("freeHashTableObj keys: %p", table->keys);
        FREE_ARRAY(HashSymbol *, table->keys, table->capacity);
        if (table->valuesize > 0) {
            DEBUG("freeHashTableObj values: %p", table->values);
            IFDEBUG(printMemHeader("values", table->values));
            FREE_ARRAY(char, table->values,
                       table->capacity * table->valuesize);
        }
    }
    FREE(h, HashTable);
}

HashSymbol *uniqueHashSymbol(HashTable *table, char *name, void *src) {
    HashSymbol *x;
    x = hashGetVar(table, name);
    if (x != NULL) {
        return x;
    }
    x = NEW(HashSymbol, OBJTYPE_HASHSYMBOL);
    int save = PROTECT(x);
    x->name = safeStrdup(name);
    x->hash = hashString(name);
    hashSet(table, x, src);
    UNPROTECT(save);
    return x;
}

void printHashSymbol(HashSymbol *symbol) {
    eprintf("%s", symbol->name);
}

void printHashTable(HashTable *table, int depth) {
    int count = 0;
    eprintf("%*s", depth * PAD_WIDTH, "");
    if (table == NULL) {
        eprintf("HashTable: (NULL)");
        return;
    }
    eprintf("HashTable %d: {", table->id);
    bool first = true;
    for (int i = 0; i < table->capacity; ++i) {
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
            count++;
        }
    }
    if (first)
        eprintf("}");
    else
        eprintf("%*s}", depth * PAD_WIDTH, "");
}

HashSymbol *iterateHashTable(HashTable *table, int *index, void *data) {
    while (*index < table->capacity) {
        if (table->keys[*index] != NULL) {
            if (data != NULL) {
                void *src = valuePtr(table, *index);
                if (src != NULL) {
                    memcpy(data, src, table->valuesize);
                }
            }
            return table->keys[(*index)++];
        }
        (*index)++;
    }
    return NULL;
}

void copyHashTable(HashTable *to, HashTable *from) {
    if (from->valuesize != to->valuesize) {
        cant_happen
            ("attempt to copy between hash tables with different storage size: %ld vs %ld",
             from->valuesize, to->valuesize);
    }
    for (int i = 0; i < from->capacity; ++i) {
        if (from->keys[i] != NULL) {
            void *data = valuePtr(from, i);
            hashSet(to, from->keys[i], data);
        }
    }
}

void markHashSymbol(HashSymbol *x) {
    if (x == NULL)
        return;
    if (MARKED(x))
        return;
    MARK(x);
}

void freeHashSymbol(HashSymbol *x) {
    FREE(x, HashSymbol);
}
