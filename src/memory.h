#ifndef cekf_memory_h
#define cekf_memory_h
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

#include <stdbool.h>
#include <stddef.h>

typedef enum {
    // exp types
    OBJTYPE_AMB,
    OBJTYPE_APPLY,
    OBJTYPE_BINDINGS,
    OBJTYPE_COND,
    OBJTYPE_AEXP,
    OBJTYPE_CEXP,
    OBJTYPE_EXP,
    OBJTYPE_EXPLIST,
    OBJTYPE_LAM,
    OBJTYPE_LET,
    OBJTYPE_LETREC,
    OBJTYPE_PRIMAPP,
    OBJTYPE_UNARYAPP,
    OBJTYPE_VAR,
    OBJTYPE_ANNOTATEDVAR,
    OBJTYPE_VARLIST,
    // cekf types
    OBJTYPE_CLO,
    OBJTYPE_ENV,
    OBJTYPE_CTENV,
    OBJTYPE_FAIL,
    OBJTYPE_KONT,
    OBJTYPE_CONS,
    OBJTYPE_VALUELIST,
    // hash table types
    OBJTYPE_HASHTABLE,
    OBJTYPE_AST,
} ObjType;

typedef struct Header {
    ObjType type;
    struct Header *next;
    bool keep;
} Header;

void *reallocate(void *ptr, size_t oldSize, size_t newSize);
int protect(Header *obj);
int startProtect();
void unProtect(int index);
void *allocate(size_t size, ObjType type);
char *safe_strdup(char *s);

void markObj(Header *h);
void markExpObj(Header *x);
void markCekfObj(Header *x);
void markHashTableObj(Header *x);

void freeObj(Header *h);
void freeExpObj(Header *x);
void freeCekfObj(Header *x);
void freeHashTableObj(Header *x);

bool enableGC();
bool disableGC();

#define EXIT_OOM 2

#define NEW(thing, type) ((thing *)allocate(sizeof(thing), type))
#define FREE(thing, type) ((void)reallocate(thing, sizeof(type), 0))
#define NEW_ARRAY(type, count) ((type *)reallocate(NULL, 0, sizeof(type) * (count)))
#define FREE_ARRAY(type, array, count) ((void)reallocate(array, sizeof(type) * (count), 0))
#define GROW_ARRAY(type, array, oldcount, newcount) ((type *)reallocate(array, sizeof(type) * (oldcount), sizeof(type) * (newcount)))
#define MOVE_ARRAY(type, dest, src, amount) (memmove((dest), (src), sizeof(type) * (amount)))
#define COPY_ARRAY(type, dest, src, amount) (memcpy((dest), (src), sizeof(type) * (amount)))

#define STARTPROTECT() protect(NULL);
#define PROTECT(x) protect((Header *)(x))
#define UNPROTECT(i) unProtect(i)

#define MARK(obj) (((Header *)(obj))->keep = true)
#define MARKED(obj) (((Header *)(obj))->keep == true)

#endif
