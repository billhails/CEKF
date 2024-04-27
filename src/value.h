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
#  include "builtins.h"
#  include "types.h"

typedef enum ValueType {
    VALUE_TYPE_VOID,
    VALUE_TYPE_STDINT,
    VALUE_TYPE_BIGINT,
    VALUE_TYPE_RATIONAL,
    VALUE_TYPE_IRRATIONAL,
    VALUE_TYPE_STDINT_IMAG,
    VALUE_TYPE_BIGINT_IMAG,
    VALUE_TYPE_RATIONAL_IMAG,
    VALUE_TYPE_IRRATIONAL_IMAG,
    VALUE_TYPE_COMPLEX,
    VALUE_TYPE_CHARACTER,
    VALUE_TYPE_CLO,
    VALUE_TYPE_PCLO,
    VALUE_TYPE_CONT,
    VALUE_TYPE_VEC,
    VALUE_TYPE_BUILTIN,
} ValueType;

typedef union {
    void *none;
    Integer stdint;
    BigInt *bigint;
    Double irrational;
    Character character;
    struct Clo *clo;
    struct Kont *kont;
    struct Vec *vec;
    struct BuiltInImplementation *builtIn;
} ValueVal;

typedef struct Value {
    ValueType type;
    ValueVal val;
} Value;

#  define VALUE_VAL_STDINT(x)          ((ValueVal){.stdint     = (x)})
#  define VALUE_VAL_BIGINT(x)          ((ValueVal){.bigint     = (x)})
#  define VALUE_VAL_IRRATIONAL(x)      ((ValueVal){.irrational = (x)})
#  define VALUE_VAL_STDINT_IMAG(x)     ((ValueVal){.stdint     = (x)})
#  define VALUE_VAL_BIGINT_IMAG(x)     ((ValueVal){.bigint     = (x)})
#  define VALUE_VAL_IRRATIONAL_IMAG(x) ((ValueVal){.irrational = (x)})
#  define VALUE_VAL_CHARACTER(x)       ((ValueVal){.character  = (x)})
// CLO and PCLO share the same Clo struct
#  define VALUE_VAL_CLO(x)             ((ValueVal){.clo  = (x)})
#  define VALUE_VAL_PCLO(x)            ((ValueVal){.clo  = (x)})
#  define VALUE_VAL_CONT(x)            ((ValueVal){.kont = (x)})
// RATIONAL VEC and COMPLEX share the same Vec struct
#  define VALUE_VAL_VEC(x)             ((ValueVal){.vec  = (x)})
#  define VALUE_VAL_RATIONAL(x)        ((ValueVal){.vec  = (x)})
#  define VALUE_VAL_COMPLEX(x)         ((ValueVal){.vec  = (x)})
#  define VALUE_VAL_NONE()             ((ValueVal){.none = NULL})
#  define VALUE_VAL_BUILTIN(x)         ((ValueVal){.builtIn = (x)})

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

static inline Value stdintValue(Integer x) {
    Value v;
    v.type = VALUE_TYPE_STDINT;
    v.val = VALUE_VAL_STDINT(x);
    return v;
}

static inline Value stdintimagValue(Integer x) {
    Value v;
    v.type = VALUE_TYPE_STDINT_IMAG;
    v.val = VALUE_VAL_STDINT_IMAG(x);
    return v;
}

static inline Value bigintValue(BigInt * x) {
    Value v;
    v.type = VALUE_TYPE_BIGINT;
    v.val = VALUE_VAL_BIGINT(x);
    return v;
}

static inline Value bigintimagValue(BigInt * x) {
    Value v;
    v.type = VALUE_TYPE_BIGINT_IMAG;
    v.val = VALUE_VAL_BIGINT_IMAG(x);
    return v;
}

static inline Value irrationalValue(Double x) {
    Value v;
    v.type = VALUE_TYPE_IRRATIONAL;
    v.val = VALUE_VAL_IRRATIONAL(x);
    return v;
}

static inline Value irrationalimagValue(Double x) {
    Value v;
    v.type = VALUE_TYPE_IRRATIONAL_IMAG;
    v.val = VALUE_VAL_IRRATIONAL_IMAG(x);
    return v;
}

static inline Value characterValue(Character x) {
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

static inline Value complexValue(struct Vec * x) {
    Value v;
    v.type = VALUE_TYPE_COMPLEX;
    v.val = VALUE_VAL_COMPLEX(x);
    return v;
}

static inline Value builtInValue(struct BuiltInImplementation * x) {
    Value v;
    v.type = VALUE_TYPE_BUILTIN;
    v.val = VALUE_VAL_BUILTIN(x);
    return v;
}

#endif
