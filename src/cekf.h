#ifndef cekf_cekf_h
#define cekf_cekf_h
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

/**
 * The structures of the CEKF machine.
 */

#include <stddef.h>

#include "exp.h"
#include "memory.h"

typedef struct {
    struct Exp *C;
    struct Env *E;
    struct Kont *K;
    struct Fail *F;
    struct Value *V;
} CEKF;

typedef struct Env {
    struct Header header;
    struct Env *next;
    struct AexpVar *var;
    struct Value *val;
} Env;

typedef struct Kont {
    struct Header header;
    struct AexpVar *var;
    struct Exp *body;
    struct Env *rho;
    struct Kont *next;
} Kont;

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
    struct Header header;
    ValueType type;
    ValueVal val;
} Value;

typedef struct ValueList {
    struct Header header;
    struct ValueList *next;
    struct Value *value;
} ValueList;

#define VALUE_VAL_INTEGER(x) ((ValueVal){.z    = (x)})
#define VALUE_VAL_CLO(x)     ((ValueVal){.clo  = (x)})
#define VALUE_VAL_CONT(x)    ((ValueVal){.k    = (x)})
#define VALUE_VAL_NONE()     ((ValueVal){.none = NULL})

typedef struct Clo {
    struct Header header;
    struct AexpLam *lam;
    struct Env *rho;
} Clo;


typedef struct Fail {
    struct Header header;
    struct Exp *exp;
    struct Env *rho;
    struct Kont *k;
    struct Fail *next;
} Fail;

Value *newValue(ValueType type, ValueVal val);
ValueList *newValueList(ValueList *next, Value *value);
Clo *newClo(AexpLam *lam, Env *rho);
Env *newEnv(Env *next, AexpVar *var, Value *val);
Kont *newKont(AexpVar *var, Exp *body, Env *rho, Kont *next);
Fail *newFail(Exp *exp, Env *rho, Kont *k, Fail *next);

void markValue(Value *x);
void markValueList(ValueList *x);
void markClo(Clo *x);
void markEnv(Env *x);
void markKont(Kont *x);
void markFail(Fail *x);


#endif
