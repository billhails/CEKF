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

bool quietPrintHashTable = false;

static int idCounter = 0;

#ifdef DEBUG_HASHTABLE
static void printMemHeader(char *id, void *ptr) {
    /*
    if (ptr == NULL) return;
    int size = 16;

    char *b = (char *)ptr;
    b -= size;
    fprintf(stderr, "%s mem header", id);
    for (int i = 0; i < size; i++) {
        fprintf(stderr, " %02x", b[i]);
    }
    fprintf(stderr, "\n");
    */
}
#endif

HashTable *newHashTable(size_t valuesize, MarkHashValueFunction markfunction, PrintHashValueFunction printfunction) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "newHashTable() [id=%d, valuesize=%lu]\n", idCounter, valuesize);
#endif
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
        hash ^= (uint8_t)*string;
        hash *= 16777619;
    }
    return hash;
}

static void* valuePtr(HashTable *table, int index) {
    if (table->valuesize == 0) return NULL;
    return (char *)table->values + (index * table->valuesize);
}

static hash_t findEntry(HashSymbol **keys, int capacity, HashSymbol *var) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "findEntry(%s)\n", var->name);
#endif
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
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "growCapacity(%d) [%d]\n", capacity, table->id);
#endif
    HashSymbol **keys = NEW_ARRAY(HashSymbol *, capacity);
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "growCapacity old keys: %p new keys: %p\n", table->keys, keys);
    printMemHeader("keys", keys);
#endif
    void *values = NULL;

    for (int i = 0; i < capacity; i++) {
        keys[i] = NULL;
    }

    if (table->valuesize > 0) {
        values = NEW_ARRAY(char, table->valuesize * capacity);
        bzero(values, table->valuesize * capacity);
#ifdef DEBUG_HASHTABLE
        fprintf(stderr, "growCapacity old values: %p new values: %p\n", table->values, values);
        printMemHeader("values", values);
#endif
    }

    for (int old_index = 0; old_index < table->capacity; old_index++) {
        HashSymbol *var = table->keys[old_index];
        if (var == NULL) continue;
        hash_t new_index = findEntry(keys, capacity, var);
        keys[new_index] = var;
        if (table->valuesize > 0) {
            void *dst = (char *)values + (new_index * table->valuesize);
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
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashSet(%s) [%d]\n", var->name, table->id);
    printMemHeader("values", table->values);
    printMemHeader("keys", table->keys);
#endif
    checkCapacity(table);
    hash_t index = findEntry(table->keys, table->capacity, var);

    if (table->keys[index] == NULL) table->count++;

    table->keys[index] = var;

    if (table->valuesize > 0) {
        void *target = valuePtr(table, index);
        memcpy(target, src, table->valuesize);
    }
}

bool hashGet(HashTable *table, HashSymbol *var, void *dest) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashGet(%s) [%d]\n", var->name, table->id);
    printMemHeader("values", table->values);
    printMemHeader("keys", table->keys);
#endif
    if (table->count == 0) return false;
    hash_t index = findEntry(table->keys, table->capacity, var);
#ifdef DEBUG_HASHTABLE
    printMemHeader("values", table->values);
    printMemHeader("keys", table->keys);
#endif
    if (table->keys[index] == NULL) return false;
    if (table->valuesize > 0 && dest != NULL) {
        void *src = valuePtr(table, index);
        memcpy(dest, src, table->valuesize);
    }
    return true;
}

HashSymbol *hashGetVar(HashTable *table, const char *name) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "hashGetVar() [%d]\n", table->id);
    printMemHeader("values", table->values);
    printMemHeader("keys", table->keys);
#endif
    if (table->count == 0) return NULL;
    hash_t hash = hashString(name);
    hash_t index = hash & (table->capacity - 1);

    for (;;) {
        if (table->keys[index] == NULL) return NULL;
        if (   table->keys[index]->hash == hash
            && strcmp(name, table->keys[index]->name) == 0) {
            return table->keys[index];
        }
        index = (index + 1) & (table->capacity - 1);
    }
}

void markHashTableObj(Header *h) {
    markHashTable((HashTable *)h);
}

void markHashTable(HashTable *table) {
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "markHashTableObj() [%d]\n", table->id);
    printMemHeader("values", table->values);
    printMemHeader("keys", table->keys);
#endif
    if (table == NULL) return;
    if (MARKED(table)) return;
    MARK(table);
    for (int i = 0; i < table->capacity; i++) {
        if (table->keys[i] != NULL) {
            markHashSymbol(table->keys[i]);
            if (table->valuesize > 0 && table->markfunction != NULL) {
                table->markfunction((char *)table->values + (i * table->valuesize));
            }
        }
    }
}

void freeHashTableObj(Header *h) {
    HashTable *table = (HashTable *)h;
#ifdef DEBUG_HASHTABLE
    fprintf(stderr, "freeHashTableObj() [%d]\n", table->id);
    printMemHeader("values", table->values);
    printMemHeader("keys", table->keys);
#endif
    if (table == NULL) return;
    if (table->count > 0) {
#ifdef DEBUG_HASHTABLE
        fprintf(stderr, "freeHashTableObj keys: %p\n", table->keys);
#endif
        FREE_ARRAY(HashSymbol *, table->keys, table->capacity);
        if (table->valuesize > 0) {
#ifdef DEBUG_HASHTABLE
            fprintf(stderr, "freeHashTableObj values: %p\n", table->values);
            printMemHeader("values", table->values);
#endif
            FREE_ARRAY(char, table->values, table->capacity * table->valuesize);
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
    printf("%s", symbol->name);
}

void printHashTable(HashTable *table, int depth) {
    int count = 0;
    printf("%*s", depth * 4, "");
    if (table == NULL) {
        printf("HashTable: (NULL)");
        return;
    }
    printf("{[id:%d]", table->id);
    bool first = true;
    for (int i = 0; i < table->capacity; ++i) {
        if (table->keys[i] != NULL) {
            if (first) {
                first = false;
                printf("\n");
            }
            printf("%*s", (depth + 1) * 4, "");
            printHashSymbol(table->keys[i]);
            if (table->valuesize > 0 && table->printfunction != NULL && !quietPrintHashTable) {
                printf(" =>");
                if (table->shortEntries)
                    printf(" ");
                else
                    printf("\n");
                table->printfunction(valuePtr(table, i), table->shortEntries ? 0 : (depth + 2));
                printf("\n");
            } else {
                printf("\n");
            }
            count++;
        }
    }
    if (first)
        printf("}");
    else
        printf("%*s}", depth * 4, "");
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
        cant_happen("attempt to copy between hash tables with different storage size: %d vs %d", from->valuesize, to->valuesize);
    }
    for (int i = 0; i < from->capacity; ++i) {
        if (from->keys[i] != NULL) {
            void *data = valuePtr(from, i);
            hashSet(to, from->keys[i], data);
        }
    }
}

void markHashSymbol(HashSymbol *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void freeHashSymbol(HashSymbol *x) {
    FREE(x, HashSymbol);
}
