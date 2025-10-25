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

#include <sys/sysmacros.h>

#include "common.h"
#include "file_id.h"

/**
 * Print an agnostic file id for debugging.
 * 
 * @param id the agnostic file id
 * @param depth the depth for pretty-printing
 */
void printAgnosticFileId(AgnosticFileId *id, int depth) {
    if (id == NULL) {
        eprintf("%*sFileId(<NULL>)", depth * PAD_WIDTH, "");
    } else {
        eprintf("%*sFileId(%u:%u, %lu, \"%s\")",
                depth * PAD_WIDTH, "", major(id->st_dev), minor(id->st_dev), id->st_ino, id->name);
    }
}

/**
 * Mark an agnostic file id for gc protection.
 * 
 * @param id the agnostic file id
 */
void markAgnosticFileId(AgnosticFileId *id) {
    if (id == NULL) return;
    if (MARKED(id)) return;
    MARK(id);
}

/**
 * Free an agnostic file id.
 * 
 * @param id the agnostic file id
 */
void freeAgnosticFileId(AgnosticFileId *id) {
    FREE(id, AgnosticFileId);
}

/**
 * Compare two agnostic file ids.
 * 
 * @param a the first agnostic file id
 * @param b the second agnostic file id
 * @return the comparison result
 */
Cmp cmpAgnosticFileId(AgnosticFileId *a, AgnosticFileId *b) {
    if (major(a->st_dev) == major(b->st_dev)) {
        if (minor(a->st_dev) == minor(b->st_dev)) {
            if (a->st_ino == b->st_ino) {
                return CMP_EQ;
            } else if (a->st_ino > b->st_ino) {
                return CMP_GT;
            } else {
                return CMP_LT;
            }
        } else if (minor(a->st_dev) > minor(b->st_dev)) {
            return CMP_GT;
        } else {
            return CMP_LT;
        }
    } else if (major(a->st_dev) > major(b->st_dev)) {
        return CMP_GT;
    } else {
        return CMP_LT;
    }
}

/**
 * Create an agnostic file id from a filename.
 * 
 * @param filename the filename
 * @return the agnostic file id, or NULL if the file does not exist
 */
AgnosticFileId *makeAgnosticFileId(char *filename) {
    struct stat stats;
    if (stat(filename, &stats) == 0) {
        AgnosticFileId *res = NEW(AgnosticFileId, OBJTYPE_AGNOSTICFILEID);
        res->st_dev = stats.st_dev;
        res->st_ino = stats.st_ino;
        res->name = filename;
        return res;
    } else {
        return NULL;
    }
}
