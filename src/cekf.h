#ifndef cekf_cekf_h
#define cekf_cekf_h

/**
 * The structures of the CEKF machine.
 */

#include <stddef.h>

#include "exp.h"

typedef struct {
    struct Exp *C;
    struct Env *E;
    struct Kont *K;
    struct Fail *F;
    struct Value *V;
} CEKF;

typedef struct Env {
    struct Env *next;
    struct AexpVar *var;
    struct Value *val;
} Env;

typedef enum {
    KONT_TYPE_LETK,
    KONT_TYPE_HALT,
} KontType;

typedef union {
    void *none;
    struct LetK *letK;
} KontVal;

typedef struct Kont {
    KontType type;
    KontVal val;
} Kont;

#define KONT_VAL_LETK(x) ((KontVal){.letK = (x)})
#define KONT_VAL_NONE()  ((KontVal){.none = NULL})

typedef struct LetK {
    struct AexpVar *var;
    struct Exp *body;
    struct Env *rho;
    struct Kont *k;
} LetK;

typedef enum {
    VALUE_TYPE_VOID,
    VALUE_TYPE_INTEGER,
    VALUE_TYPE_TRUE,
    VALUE_TYPE_FALSE,
    VALUE_TYPE_CLO,
    VALUE_TYPE_CONT,
} ValueType;

typedef union {
    void *none;
    AexpInteger z;
    struct Clo *clo;
    struct Kont *k;
} ValueVal;

typedef struct Value {
    ValueType type;
    ValueVal val;
} Value;

typedef struct ValueList {
    struct ValueList *next;
    struct Value *value;
} ValueList;

#define VALUE_VAL_INTEGER(x) ((ValueVal){.z    = (x)})
#define VALUE_VAL_CLO(x)     ((ValueVal){.clo  = (x)})
#define VALUE_VAL_CONT(x)    ((ValueVal){.k    = (x)})
#define VALUE_VAL_NONE()     ((ValueVal){.none = NULL})

typedef struct Clo {
    struct AexpLam *lam;
    struct Env *rho;
} Clo;

typedef enum {
    FAIL_TYPE_BACK,
    FAIL_TYPE_END,
} FailType;

typedef union {
    void *none;
    struct Back *back;
} FailVal;

typedef struct Fail {
    FailType type;
    FailVal val;
} Fail;

typedef struct Back {
    struct Exp *exp;
    struct Env *rho;
    struct Kont *k;
    struct Fail *f;
} Back;

#define FAIL_VAL_NONE()  ((FailVal){.none = NULL})
#define FAIL_VAL_BACK(x) ((FailVal){.back = (x)})

Value *newValue(ValueType type, ValueVal val);
ValueList *newValueList(ValueList *next, Value *value);
Clo *newClo(AexpLam *lam, Env *rho);
Env *newEnv(Env *next, AexpVar *var, Value *val);
Kont *newKont(KontType type, KontVal val);
LetK *newLetK(AexpVar *var, Exp *body, Env *rho, Kont *k);
Fail *newFail(FailType type, FailVal val);
Back *newBack(Exp *exp, Env *rho, Kont *k, Fail *f);

#endif
