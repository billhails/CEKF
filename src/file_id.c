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

#include <sys/sysmacros.h>

#include "common.h"
#include "file_id.h"

void printAgnosticFileId(AgnosticFileId *id, int depth) {
    if (id == NULL) {
        eprintf("%*sFileId(<NULL>)", depth * PAD_WIDTH, "");
    } else {
        eprintf("%*sFileId(%u:%u, %lu, \"%s\")",
                depth * PAD_WIDTH, "", major(id->st_dev), minor(id->st_dev), id->st_ino, id->name);
    }
}

void markAgnosticFileId(AgnosticFileId *id) {
    if (id == NULL) return;
    if (MARKED(id)) return;
    MARK(id);
}

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
