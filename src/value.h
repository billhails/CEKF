#ifndef cekf_value_h
#  define cekf_value_h
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

#  include "bigint.h"

typedef enum {
    VALUE_TYPE_VOID,
    VALUE_TYPE_STDINT,
    VALUE_TYPE_BIGINT,
    VALUE_TYPE_RATIONAL,
    VALUE_TYPE_IRRATIONAL,
    VALUE_TYPE_CHARACTER,
    VALUE_TYPE_CLO,
    VALUE_TYPE_PCLO,
    VALUE_TYPE_CONT,
    VALUE_TYPE_VEC,
} ValueType;

typedef union {
    void *none;
    int stdint;
    BigInt *bigint;
    double irrational;
    char character;
    struct Clo *clo;
    struct Kont *kont;
    struct Vec *vec;
} ValueVal;

typedef struct Value {
    ValueType type;
    ValueVal val;
} Value;

#  define VALUE_VAL_STDINT(x)     ((ValueVal){.stdint     = (x)})
#  define VALUE_VAL_BIGINT(x)     ((ValueVal){.bigint     = (x)})
#  define VALUE_VAL_CHARACTER(x)  ((ValueVal){.character  = (x)})
#  define VALUE_VAL_IRRATIONAL(x) ((ValueVal){.irrational = (x)})
// CLO and PCLO share the same Clo struct
#  define VALUE_VAL_CLO(x)        ((ValueVal){.clo  = (x)})
#  define VALUE_VAL_PCLO(x)       ((ValueVal){.clo  = (x)})
#  define VALUE_VAL_CONT(x)       ((ValueVal){.kont = (x)})
// RATIONAL and VEC share the same Vec struct
#  define VALUE_VAL_VEC(x)        ((ValueVal){.vec  = (x)})
#  define VALUE_VAL_RATIONAL(x)   ((ValueVal){.vec  = (x)})
#  define VALUE_VAL_NONE()        ((ValueVal){.none = NULL})

// constants
extern Value vTrue;
extern Value vFalse;
extern Value vVoid;
extern Value vLt;
extern Value vEq;
extern Value vGt;

static inline Value voidValue() {
    Value v;
    v.type = VALUE_TYPE_VOID;
    v.val = VALUE_VAL_NONE();
    return v;
}

static inline Value stdintValue(int x) {
    Value v;
    v.type = VALUE_TYPE_STDINT;
    v.val = VALUE_VAL_STDINT(x);
    return v;
}
static inline Value bigintValue(BigInt * x) {
    Value v;
    v.type = VALUE_TYPE_BIGINT;
    v.val = VALUE_VAL_BIGINT(x);
    return v;
}

static inline Value irrationalValue(double x) {
    Value v;
    v.type = VALUE_TYPE_IRRATIONAL;
    v.val = VALUE_VAL_IRRATIONAL(x);
    return v;
}

static inline Value characterValue(char x) {
    Value v;
    v.type = VALUE_TYPE_CHARACTER;
    v.val = VALUE_VAL_CHARACTER(x);
    return v;
}

static inline Value cloValue(struct Clo * x) {
    Value v;
    v.type = VALUE_TYPE_CLO;
    v.val = VALUE_VAL_CLO(x);
    return v;
}

static inline Value pcloValue(struct Clo * x) {
    Value v;
    v.type = VALUE_TYPE_PCLO;
    v.val = VALUE_VAL_PCLO(x);
    return v;
}

static inline Value kontValue(struct Kont * x) {
    Value v;
    v.type = VALUE_TYPE_CONT;
    v.val = VALUE_VAL_CONT(x);
    return v;
}

static inline Value rationalValue(struct Vec * x) {
    Value v;
    v.type = VALUE_TYPE_RATIONAL;
    v.val = VALUE_VAL_RATIONAL(x);
    return v;
}

static inline Value vecValue(struct Vec * x) {
    Value v;
    v.type = VALUE_TYPE_VEC;
    v.val = VALUE_VAL_VEC(x);
    return v;
}



#endif
