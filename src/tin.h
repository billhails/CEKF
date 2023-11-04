#ifndef cekf_tin_h
#define cekf_tin_h
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
 *
 * Type inference structures used by Algorithm W.
 *
 * generated from src/tin.yaml by makeAST.py
*/

#include "hash.h"
#include "memory.h"

typedef enum TinMonoTypeType {
    TINMONOTYPE_TYPE_VAR,
    TINMONOTYPE_TYPE_FUN,
} TinMonoTypeType;

typedef enum TinPolyTypeType {
    TINPOLYTYPE_TYPE_MONOTYPE,
    TINPOLYTYPE_TYPE_QUANTIFIER,
} TinPolyTypeType;



typedef union TinMonoTypeVal {
    HashSymbol * var;
    struct TinFunctionApplication * fun;
} TinMonoTypeVal;

typedef union TinPolyTypeVal {
    struct TinMonoType * monoType;
    struct TinTypeQuantifier * quantifier;
} TinPolyTypeVal;



typedef struct TinFunctionApplication {
    Header header;
    HashSymbol * name;
    int nargs;
    struct TinMonoTypeList * args;
} TinFunctionApplication;

typedef struct TinMonoTypeList {
    Header header;
    struct TinMonoType * monoType;
    struct TinMonoTypeList * next;
} TinMonoTypeList;

typedef struct TinTypeQuantifier {
    Header header;
    HashSymbol * var;
    struct TinPolyType * quantifiedType;
} TinTypeQuantifier;

typedef struct TinContext {
    Header header;
    HashTable * varFrame;
    HashTable * tcFrame;
    struct TinContext * next;
} TinContext;

typedef struct TinSubstitution {
    Header header;
    HashTable * map;
} TinSubstitution;

typedef struct TinArgsResult {
    Header header;
    struct TinContext * context;
    struct TinMonoTypeList * vec;
} TinArgsResult;

typedef struct TinVarResult {
    Header header;
    struct TinSubstitution * substitution;
    struct TinContext * context;
    struct TinMonoType * monoType;
    HashTable * set;
} TinVarResult;

typedef struct TinVarsResult {
    Header header;
    struct TinContext * context;
    HashTable * set;
} TinVarsResult;

typedef struct TinMonoType {
    Header header;
    enum TinMonoTypeType  type;
    union TinMonoTypeVal  val;
} TinMonoType;

typedef struct TinPolyType {
    Header header;
    enum TinPolyTypeType  type;
    union TinPolyTypeVal  val;
} TinPolyType;



struct TinFunctionApplication * newTinFunctionApplication(HashSymbol * name, int nargs, struct TinMonoTypeList * args);
struct TinMonoTypeList * newTinMonoTypeList(struct TinMonoType * monoType, struct TinMonoTypeList * next);
struct TinTypeQuantifier * newTinTypeQuantifier(HashSymbol * var, struct TinPolyType * quantifiedType);
struct TinContext * newTinContext(HashTable * varFrame, HashTable * tcFrame, struct TinContext * next);
struct TinSubstitution * newTinSubstitution(HashTable * map);
struct TinArgsResult * newTinArgsResult(struct TinContext * context, struct TinMonoTypeList * vec);
struct TinVarResult * newTinVarResult(struct TinSubstitution * substitution, struct TinContext * context, struct TinMonoType * monoType, HashTable * set);
struct TinVarsResult * newTinVarsResult(struct TinContext * context, HashTable * set);
struct TinMonoType * newTinMonoType(enum TinMonoTypeType  type, union TinMonoTypeVal  val);
struct TinPolyType * newTinPolyType(enum TinPolyTypeType  type, union TinPolyTypeVal  val);

void markTinFunctionApplication(struct TinFunctionApplication * x);
void markTinMonoTypeList(struct TinMonoTypeList * x);
void markTinTypeQuantifier(struct TinTypeQuantifier * x);
void markTinContext(struct TinContext * x);
void markTinSubstitution(struct TinSubstitution * x);
void markTinArgsResult(struct TinArgsResult * x);
void markTinVarResult(struct TinVarResult * x);
void markTinVarsResult(struct TinVarsResult * x);
void markTinMonoType(struct TinMonoType * x);
void markTinPolyType(struct TinPolyType * x);

void freeTinFunctionApplication(struct TinFunctionApplication * x);
void freeTinMonoTypeList(struct TinMonoTypeList * x);
void freeTinTypeQuantifier(struct TinTypeQuantifier * x);
void freeTinContext(struct TinContext * x);
void freeTinSubstitution(struct TinSubstitution * x);
void freeTinArgsResult(struct TinArgsResult * x);
void freeTinVarResult(struct TinVarResult * x);
void freeTinVarsResult(struct TinVarsResult * x);
void freeTinMonoType(struct TinMonoType * x);
void freeTinPolyType(struct TinPolyType * x);


#define TINMONOTYPE_VAL_VAR(x) ((union TinMonoTypeVal ){.var = (x)})
#define TINMONOTYPE_VAL_FUN(x) ((union TinMonoTypeVal ){.fun = (x)})
#define TINPOLYTYPE_VAL_MONOTYPE(x) ((union TinPolyTypeVal ){.monoType = (x)})
#define TINPOLYTYPE_VAL_QUANTIFIER(x) ((union TinPolyTypeVal ){.quantifier = (x)})

#endif
