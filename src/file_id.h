#ifndef cekf_file_id_h
#  define cekf_file_id_h
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
 *
 * An attempt at an operating-system agnostic file id, at least in the sense
 * that it can be redefined for different operating systems without having
 * to change usage throughout the code, though the implementation here is
 * POSIX-specific.
 */


#include <sys/stat.h>

#include "memory.h"
#include "cmp.h"

typedef struct AgnosticFileId {
    Header header;
    dev_t st_dev;
    ino_t st_ino;
    char *name;
} AgnosticFileId;

void printAgnosticFileId(AgnosticFileId *, int);
void markAgnosticFileId(AgnosticFileId *);
void freeAgnosticFileId(AgnosticFileId *);
Cmp cmpAgnosticFileId(AgnosticFileId *, AgnosticFileId *);
AgnosticFileId *makeAgnosticFileId(char *);

#endif
