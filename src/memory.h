#ifndef cekf_memory_h
#  define cekf_memory_h
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

#  include <stdbool.h>
#  include <stddef.h>

struct Header;

#  include "ast_objtypes.h"
#  include "anf_objtypes.h"
#  include "lambda_objtypes.h"
#  include "tpmc_objtypes.h"
#  include "tc_objtypes.h"
#  include "cekfs_objtypes.h"
#  include "builtins_objtypes.h"
#  include "pratt_objtypes.h"
#  include "anf_kont_objtypes.h"
#  include "cps_kont_objtypes.h"
#  include "types.h"

// Definition of all object types for garbage collection
typedef enum {

    // opaque type
    OBJTYPE_OPAQUE,

    // hash table types
    OBJTYPE_HASHTABLE,

    // protection stack type
    OBJTYPE_PROTECTION,

    // arithmetic types
    OBJTYPE_BIGINT,
    OBJTYPE_MAYBEBIGINT,
    
    // file id type used to track linked namespaces
    OBJTYPE_AGNOSTICFILEID,

    // various generated object types
    ANF_OBJTYPES(),
    AST_OBJTYPES(),
    LAMBDA_OBJTYPES(),
    TPMC_OBJTYPES(),
    TC_OBJTYPES(),
    BUILTINS_OBJTYPES(),
    CEKFS_OBJTYPES(),
    PRATT_OBJTYPES(),
    ANF_KONT_OBJTYPES(),
    CPS_KONT_OBJTYPES(),
} ObjType;

typedef struct Header {
    ObjType type;
    struct Header *next;
    bool keep;
} Header;

void *reallocate(void *ptr, size_t oldSize, size_t newSize);
Index protect(Header *obj);
void replaceProtect(Index i, Header *obj);
Index startProtect(void);
void unProtect(Index index);
void *allocate(size_t size, ObjType type);
char *safeStrdup(char *s);
void markObj(Header *h, Index i);
void markCekfObj(Header *x);
void markHashTableObj(Header *x);
void freeObj(Header *h);
void freeCekfObj(Header *x);
void freeHashTableObj(Header *x);
bool enableGC(void);
bool disableGC(void);
void initProtection(void);
void validateLastAlloc(void);
void reportMemory(void);
void collectGarbage();

#  define EXIT_OOM 2

#define NEW_VECTOR(size, type, element_type, tag) ((type *)allocate(sizeof(type) + (size) * sizeof(element_type), (tag)))
#define FREE_VECTOR(vector, type, element_type, count) ((void)reallocate(vector, sizeof(type) + (count) * sizeof(element_type), 0))

#  define NEW_VEC(size) ((Vec *)allocate(sizeof(Vec) + size * sizeof(Value), OBJTYPE_VEC))
#  define FREE_VEC(vec) ((void)reallocate(vec, sizeof(vec) + vec->size * sizeof(Value), 0))

// Allocation for directly managed objects
#  define NEW(thing, type) ((thing *)allocate(sizeof(thing), type))
#  define FREE(thing, type) ((void)reallocate(thing, sizeof(type), 0))
// Allocation for indirectly managed objects
#  define ALLOCATE(type) ((type *)reallocate(NULL, 0, sizeof(type)))

#  define NEW_ARRAY(type, count) ((type *)reallocate(NULL, 0, sizeof(type) * (count)))
#  define FREE_ARRAY(type, array, count) ((void)reallocate(array, sizeof(type) * (count), 0))
#  define GROW_ARRAY(type, array, oldcount, newcount) ((type *)reallocate(array, sizeof(type) * (oldcount), sizeof(type) * (newcount)))
#  define MOVE_ARRAY(type, dest, src, amount) (memmove((dest), (src), sizeof(type) * (amount)))
#  define COPY_ARRAY(type, dest, src, amount) (memcpy((dest), (src), sizeof(type) * (amount)))

/**
 * VERY IMPORTANT macros:
 */
/**
 * PROTECT pushes its argument onto a ProtectionStack
 * which is marked by the mark stage of a mark-and-sweep algorithm
 * so that it will not be accidentally collected. PROTECT returns the
 * stack pointer index of the pushed object, which can be used
 * to unprotect the object later.
 */
#  define PROTECT(x) protect((Header *)(x))
/**
 * UNPROTECT restores the ProtectionStack to its argument index,
 * effectively removing all objects at and above that index from the
 * ProtectionStack.
 */
#  define UNPROTECT(i) unProtect(i)
/**
 * REPLACE_PROTECT replaces the object at the
 * given index on the ProtectionStack with the given replacement.
 * This is useful when an object being replaced is contained within
 * the object that replaces it, since mark and sweep will automatically
 * protect the contained object.
 */
#  define REPLACE_PROTECT(i, x) replaceProtect(i, (Header *)(x))
/**
 * STARTPROTECT just returns the current stack pointer, a later
 * UNPROTECT will restore the stack to this point, having no effect
 * if nothing has been pushed onto the stack since.
 */
#  define STARTPROTECT() protect(NULL);

#  define MARK(obj) (((Header *)(obj))->keep = true)
#  define MARKED(obj) (((Header *)(obj))->keep == true)

#define safeMalloc(size) reallocate(NULL, 0, size)
#endif
