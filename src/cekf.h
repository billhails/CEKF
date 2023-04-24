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
    VALUE_VOID_TYPE,
    VALUE_INTEGER_TYPE,
    VALUE_TRUE_TYPE,
    VALUE_FALSE_TYPE,
    VALUE_CLO_TYPE,
    VALUE_CONT_TYPE,
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
    FAIL_TYPE_BACKTRACK,
    FAIL_TYPE_END,
} FailType;

typedef union {
    void *none;
    struct BackTrack *backTrack;
} FailVal;

typedef struct Fail {
    FailType type;
    FailVal val;
} Fail;

typedef struct BackTrack {
    struct Exp *exp;
    struct Env *rho;
    struct Kont *k;
    struct Fail *f;
} BackTrack;

#define FAIL_VAL_NONE()       ((FailVal){.none      = NULL})
#define FAIL_VAL_BACKTRACK(x) ((FailVal){.backTrack = (x)})

Value *newValue(ValueType type, ValueVal val);
ValueList *newValueList(ValueList *next, Value *value);
Clo *newClo(AexpLam *lam, Env *rho);
CEKF *newCEKF(Exp *C, Env *E, Kont *K, Fail *F);
Env *newEnv(Env *next, AexpVar *var, Value *val);
Kont *newKont(KontType type, KontVal val);
LetK *newLetK(AexpVar *var, Exp *body, Env *rho, Kont *k);
Fail *newFail(FailType type, FailVal val);
BackTrack *newBackTrack(Exp *exp, Env *rho, Kont *k, Fail *f);

#endif
