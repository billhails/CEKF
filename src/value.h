#ifndef cekf_value_h
#define cekf_value_h
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
    int z;
    struct Clo *clo;
    struct Kont *k;
    struct CallableKont *ck;
} ValueVal;

typedef struct Value {
    ValueType type;
    ValueVal val;
} Value; 

#define VALUE_VAL_INTEGER(x)      ((ValueVal){.z    = (x)})
#define VALUE_VAL_CLO(x)          ((ValueVal){.clo  = (x)})
#define VALUE_VAL_CONT(x)         ((ValueVal){.k    = (x)})
#define VALUE_VAL_CALLABLECONT(x) ((ValueVal){.ck   = (x)})
#define VALUE_VAL_NONE()          ((ValueVal){.none = NULL})

// constants
extern Value vTrue;
extern Value vFalse;
extern Value vVoid;


 #endif
