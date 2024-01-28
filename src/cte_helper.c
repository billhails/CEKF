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

#include "cte.h"
#include "cte_debug.h"

static void printCTHashTableValue(void *ptr, int depth) {
    if (depth > 20) {
        eprintf("%*sREDACTED...\n", depth * PAD_WIDTH, "");
        return;
    }
    printCTEntry(*((CTEntry **) ptr), depth);
}

static void markCTHashTableValue(void *ptr) {
    markCTEntry(*((CTEntry **) ptr));
}

HashTable *newCTHashTable() {
    return newHashTable(sizeof(CTEntry *), markCTHashTableValue, printCTHashTableValue);
}
