#ifndef cekf_memory_h
#define cekf_memory_h

#include <stdbool.h>
#include <stddef.h>

typedef enum {
    // exp types
    OBJTYPE_AMB,
    OBJTYPE_APPLY,
    OBJTYPE_BINDINGS,
    OBJTYPE_COND,
    OBJTYPE_EXP,
    OBJTYPE_EXPLIST,
    OBJTYPE_LAM,
    OBJTYPE_LET,
    OBJTYPE_LETREC,
    OBJTYPE_PRIMAPP,
    OBJTYPE_VAR,
    OBJTYPE_VARLIST,
    // cekf types
    OBJTYPE_CLO,
    OBJTYPE_ENV,
    OBJTYPE_FAIL,
    OBJTYPE_KONT,
    OBJTYPE_VALUE,
    OBJTYPE_VALUELIST,
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

void markExpObj(Header *x);
void markCekfObj(Header *x);

#define EXIT_OOM 2

#define NEW(thing, type) ((thing *)allocate(sizeof(thing), type))

#define STARTPROTECT() protect(NULL);
#define PROTECT(x) protect((Header *)(x))
#define UNPROTECT(i) unProtect(i)

#define MARK(obj) (((Header *)(obj))->keep = true)
#define MARKED(obj) (((Header *)(obj))->keep == true)

#endif
