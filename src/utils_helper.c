/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "utils_helper.h"

// These little helpers are a bit too specific to be generated.

/**
 * @brief Creates a new SCharVec from a C string, including the null terminator.
 * @param str The C string to convert.
 * @return A new SCharVec containing the characters of the string.
 */
SCharVec *stringToSCharVec(char *str) {
    size_t len = strlen(str);
    SCharVec *vec = newSCharVec(len + 1);
    strcpy(vec->entries, str);
    return vec;
}

/**
 * @brief Creates a new SCharArray from a C string, excluding the null
 * terminator.
 * @param str The C string to convert.
 * @return A new SCharArray containing the characters of the string.
 */
SCharArray *stringToSCharArray(char *str) {
    SCharArray *array = newSCharArray();
    int save = PROTECT(array);
    size_t len = strlen(str);
    extendSCharArray(array, len);
    memcpy(array->entries, str, len);
    UNPROTECT(save);
    return array;
}

/**
 * @brief Appends a C string to an SCharArray, excluding the null terminator.
 * @param array The SCharArray to append to.
 * @param str The C string to append.
 */
void appendStringToSCharArray(SCharArray *array, char *str) {
    addSCharArray(array, strlen(str));
    while (*str) {
        pushSCharArray(array, *str);
        str++;
    }
}

/**
 * @brief Converts an SCharArray to an SCharVec, adding a null terminator.
 * @param array The SCharArray to convert.
 * @return A new SCharVec containing the characters of the array.
 */
SCharVec *sCharArrayToVec(SCharArray *array) {
    SCharVec *vec = newSCharVec(array->size + 1);
    memcpy(vec->entries, array->entries, array->size);
    vec->entries[array->size] = '\0';
    return vec;
}

/**
 * @brief Converts an SCharVec to an SCharArray, dropping the null terminator.
 * @param vec The SCharVec to convert.
 * @return A new SCharArray containing the characters of the vector.
 */
SCharArray *sCharVecToArray(SCharVec *vec) {
    SCharArray *array = newSCharArray();
    int save = PROTECT(array);
    extendSCharArray(array, vec->size - 1);
    memcpy(array->entries, vec->entries, vec->size - 1);
    UNPROTECT(save);
    return array;
}

/**
 * @brief Exclude a symbol from a set of symbols.
 *
 * @param var The symbol to exclude.
 * @param symbols The current set of symbols.
 * @return A new set of symbols without the excluded symbol.
 */
SymbolSet *excludeSymbol(HashSymbol *var, SymbolSet *symbols) {
    SymbolSet *new = newSymbolSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateSymbolSet(symbols, &i)) != NULL) {
        if (current != var) {
            setSymbolSet(new, current);
        }
    }
    UNPROTECT(save);
    return new;
}

/**
 * @brief Copy a set of symbols.
 *
 * @param symbols The current set of symbols.
 * @return A new set of symbols.
 */
SymbolSet *copySymbolSet(SymbolSet *symbols) {
    SymbolSet *new = newSymbolSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateSymbolSet(symbols, &i)) != NULL) {
        setSymbolSet(new, current);
    }
    UNPROTECT(save);
    return new;
}

/**
 * @brief Check if a symbol is in a list of symbols.
 *
 * @param var The symbol to check.
 * @param vars The list of symbols to search in.
 * @return True if the symbol is found, false otherwise.
 */
bool symbolInList(HashSymbol *var, SymbolList *vars) {
    while (vars != NULL) {
        if (var == vars->symbol) {
            return true;
        }
        vars = vars->next;
    }
    return false;
}

/**
 * @brief Exclude a list of symbols from a set of symbols.
 *
 * @param vars The list of symbols to exclude.
 * @param symbols The current set of symbols.
 * @return A new set of symbols without the excluded symbols.
 */
SymbolSet *excludeSymbols(SymbolList *vars, SymbolSet *symbols) {
    SymbolSet *new = newSymbolSet();
    int save = PROTECT(new);
    Index i = 0;
    HashSymbol *current;
    while ((current = iterateSymbolSet(symbols, &i)) != NULL) {
        if (!symbolInList(current, vars)) {
            setSymbolSet(new, current);
        }
    }
    UNPROTECT(save);
    return new;
}

/**
 * @brief Check if any symbols in the list are in the set.
 *
 * @param vars The list of symbols to check.
 * @param symbols The set of symbols.
 * @return True if any symbol is in the set, false otherwise.
 */
bool anySymbolInSet(SymbolList *vars, SymbolSet *symbols) {
    while (vars != NULL) {
        if (getSymbolSet(symbols, vars->symbol)) {
            return true;
        }
        vars = vars->next;
    }
    return false;
}