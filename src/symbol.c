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

// application-wide global symbol store

static HashTable *symbolTable = NULL;
static HashTable *genSymTable = NULL;

void markVarTable() {
    markHashTableObj((Header *) symbolTable);
    markHashTableObj((Header *) genSymTable);
}

static void initSymbolTable() {
    if (symbolTable == NULL) {
        symbolTable = newHashTable(0, NULL, NULL);
    }
}

static void initGenSymTable() {
    if (genSymTable == NULL) {
        genSymTable = newHashTable(sizeof(int), NULL, NULL);
    }
}

HashSymbol *newSymbol(char *name) {
    initSymbolTable();
    HashSymbol *res = uniqueHashSymbol(symbolTable, name, NULL);
    validateLastAlloc();
    return res;
}

HashSymbol *newSymbolLength(char *name, int length) {
    if (length < 80) {
        static char buf[80];
        memcpy(buf, name, length);
        buf[length] = '\0';
        HashSymbol *res = newSymbol(buf);
        return res;
    } else {
        char *buf = (char *) safeMalloc(length + 1);
        memcpy(buf, name, length);
        buf[length] = '\0';
        HashSymbol *res = newSymbol(buf);
        reallocate(buf, length + 1, 0);
        return res;
    }
}

HashSymbol *newSymbolCounter(char *baseName) {
    initGenSymTable();
    HashSymbol *base = newSymbol(baseName);
    if (!hashGet(genSymTable, base, NULL)) {
        int i = 0;
        hashSet(genSymTable, base, &i);
    }
    return base;
}

static HashSymbol *_genSym(char *prefix, GenSymFmt fmt, char *sep) {
    int symbolCounter = 0;
    static char buffer[128];
    char *b = buffer;
    int size = strlen(prefix) + 28;
    if (size > 128) {
        b = NEW_ARRAY(char, size);
    }
    initSymbolTable();
    HashSymbol *base = newSymbolCounter(prefix);
    hashGet(genSymTable, base, &symbolCounter);
    for (;;) {
        switch (fmt) {
            case DECIMAL:
                sprintf(buffer, "%s%s%d", prefix, sep, symbolCounter++);
                break;
            case ALPHABETIC:{
                    char *alphabet = "abcdefghijklmnopqrstuvwxyz";
                    char suffix[128];
                    int index = 0;
                    int value = symbolCounter++;
                    do {
                        suffix[index++] = alphabet[value % 26];
                        suffix[index] = '\0';
                        value = value / 26;
                    } while (value > 0);
                    sprintf(buffer, "%s%s%s", prefix, sep, suffix);
                }
                break;
        }
        if (hashGetVar(symbolTable, buffer) == NULL) {
            HashSymbol *x = uniqueHashSymbol(symbolTable, buffer, NULL);
            hashSet(genSymTable, base, &symbolCounter);
            if (b != buffer) {
                FREE_ARRAY(char, b, size);
            }
            return x;
        }
    }
}

HashSymbol *genSymDollar(char *prefix) {
    return _genSym(prefix, DECIMAL, "$");
}

HashSymbol *genSym(char *prefix) {
    return _genSym(prefix, DECIMAL, "");
}

HashSymbol *genAlphaSym(char *prefix) {
    return _genSym(prefix, ALPHABETIC, "");
}
