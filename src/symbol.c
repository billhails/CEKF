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

#include <stdio.h>
#include "symbol.h"

typedef enum GenSymFmt {
    DECIMAL,
    ALPHABETIC
} GenSymFmt;

/**
 * Application-wide global symbol store.
 */
static HashTable *globalSymbolTable = NULL;

/**
 * Symbol generator table (maps base symbols to counters).
 * 
 * We use a separate counter per base symbol purely for
 * cosmetic reasons, it makes the resulting symbols easier to read.
 */
static HashTable *genSymTable = NULL;

/**
 * Mark the variable tables for garbage collection.
 */
void markVarTables() {
    markHashTableObj((Header *) globalSymbolTable);
    markHashTableObj((Header *) genSymTable);
}

/**
 * Initialize the global symbol table if not already done.
 */
static void initGlobalSymbolTable() {
    if (globalSymbolTable == NULL) {
        globalSymbolTable = newHashTable(0, NULL, NULL);
    }
}

/**
 * Initialize the gensym table if not already done.
 */
static void initGenSymTable() {
    if (genSymTable == NULL) {
        genSymTable = newHashTable(sizeof(int), NULL, NULL);
    }
}

/**
 * Check if a symbol with the given name already exists in the global symbol table.
 */
static bool symbolExists(char *name) {
    initGlobalSymbolTable();
    return hashGetVar(globalSymbolTable, name) != NULL;
}

/**
 * Create a new symbol with the given name, or return the existing one if it already exists.
 */
HashSymbol *newSymbol(char *name) {
    initGlobalSymbolTable();
    HashSymbol *res = uniqueHashSymbol(globalSymbolTable, name, NULL);
    validateLastAlloc();
    return res;
}

/**
 * Create a new symbol using the given number of characters from the character array.
 * 
 * Needed by the pratt parser which works with strings that are not null terminated.
 */
HashSymbol *newSymbolFromLength(char *charArray, int count) {
    if (count < 80) {
        static char buf[80];
        memcpy(buf, charArray, count);
        buf[count] = '\0';
        HashSymbol *res = newSymbol(buf);
        return res;
    } else {
        char *buf = (char *) safeMalloc(count + 1);
        memcpy(buf, charArray, count);
        buf[count] = '\0';
        HashSymbol *res = newSymbol(buf);
        reallocate(buf, count + 1, 0);
        return res;
    }
}

/**
 * Get the base symbol with the given name from the gensym table,
 * creating and initializing its counter to 0 it if it does not already exist.
 */
static HashSymbol *getCounterSymbol(char *baseName) {
    initGenSymTable();
    HashSymbol *base = newSymbol(baseName);
    if (!hashGet(genSymTable, base, NULL)) {
        int i = 0;
        hashSet(genSymTable, base, &i);
    }
    return base;
}

/**
 * Get the current counter value for the given base symbol.
 */
static int getCounter(HashSymbol *base) {
    int counter;
    hashGet(genSymTable, base, &counter);
    return counter;
}

/**
 * Sets the counter value for the given base symbol.
 */
static void setCounter(HashSymbol *base, int counter) {
    hashSet(genSymTable, base, &counter);
}

/**
 * Generate a base-26 alphabetic suffix for the given integer value.
 */
static char *_alphaSuffix(int value) {
    static char buffer[128];
    char *alphabet = "abcdefghijklmnopqrstuvwxyz";
    char *b = buffer;
    int index = 0;
    do {
        b[index++] = alphabet[value % 26];
        b[index] = '\0';
        value = value / 26;
    } while (value > 0);
    return buffer;
}

/**
 * Generate a unique symbol with the given prefix, format, and separator.
 */
static HashSymbol *_genSym(char *prefix, GenSymFmt fmt, char *sep) {
    int symbolCounter = 0;
    static char buffer[128];
    char *newName = buffer;
    int size = strlen(prefix) + 28;
    if (size > 128) {
        newName = NEW_ARRAY(char, size);
    }
    HashSymbol *base = getCounterSymbol(prefix);
    symbolCounter = getCounter(base);
    // ensure symbol is unique by retrying if necessary
    for (;;) {
        switch (fmt) {
            case DECIMAL:
                sprintf(newName, "%s%s%d", prefix, sep, symbolCounter++);
                break;
            case ALPHABETIC:{
                    sprintf(newName, "%s%s%s", prefix, sep, _alphaSuffix(symbolCounter++));
                }
                break;
        }
        if (!symbolExists(newName)) {
            HashSymbol *x = newSymbol(newName);
            setCounter(base, symbolCounter);
            if (newName != buffer) {
                FREE_ARRAY(char, newName, size);
            }
            return x;
        }
    }
}

/**
 * Generate a unique symbol with the given prefix, a dollar sign separator and a numeric suffix.
 */
HashSymbol *genSymDollar(char *prefix) {
    return _genSym(prefix, DECIMAL, "$");
}

/**
 * Generate a unique symbol with the given prefix, no separator and numeric suffix.
 */
HashSymbol *genSym(char *prefix) {
    return _genSym(prefix, DECIMAL, "");
}

/**
 * Generate a unique symbol with the given prefix, no separator and alphabetic suffix.
 */
HashSymbol *genAlphaSym(char *prefix) {
    return _genSym(prefix, ALPHABETIC, "");
}
