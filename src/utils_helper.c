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

SCharVec *stringToSCharVec(char *str) {
    size_t len = strlen(str);
    SCharVec *vec = newSCharVec(len + 1);
    strcpy(vec->entries, str);
    return vec;
}

// NB: does not copy null terminator
SCharArray *stringToSCharArray(char *str) {
    SCharArray *array = newSCharArray();
    int save = PROTECT(array);
    size_t len = strlen(str);
    extendSCharArray(array, len);
    memcpy(array->entries, str, len);
    UNPROTECT(save);
    return array;
}

void appendStringToSCharArray(SCharArray *array, char *str) {
    addSCharArray(array, strlen(str));
    while (*str) {
        pushSCharArray(array, *str);
        str++;
    }
}

// adds null terminator
SCharVec *sCharArrayToVec(SCharArray *array) {
    SCharVec *vec = newSCharVec(array->size + 1);
    memcpy(vec->entries, array->entries, array->size);
    vec->entries[array->size] = '\0';
    return vec;
}

// drops null terminator
SCharArray *sCharVecToArray(SCharVec *vec) {
    SCharArray *array = newSCharArray();
    int save = PROTECT(array);
    extendSCharArray(array, vec->size - 1);
    memcpy(array->entries, vec->entries, vec->size - 1);
    UNPROTECT(save);
    return array;
}