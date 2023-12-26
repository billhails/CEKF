#ifndef cekf_tc_h
#define cekf_tc_h
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
 * Structures to support type inference
 *
 * generated from src/tc.yaml by makeAST.py
 */

#include "hash.h"
#include "memory.h"
#include "common.h"

typedef enum TcTypeType {
    TCTYPE_TYPE_FUNCTION,
    TCTYPE_TYPE_PAIR,
    TCTYPE_TYPE_VAR,
    TCTYPE_TYPE_INTEGER,
    TCTYPE_TYPE_CHARACTER,
    TCTYPE_TYPE_TYPEDEF,
} TcTypeType;



typedef union TcTypeVal {
    struct TcFunction * function;
    struct TcPair * pair;
    struct TcVar * var;
    void * integer;
    void * character;
    struct TcTypeDef * typeDef;
} TcTypeVal;



typedef struct TcEnv {
    Header header;
    HashTable * table;
    struct TcEnv * next;
} TcEnv;

typedef struct TcNg {
    Header header;
    HashTable * table;
    struct TcNg * next;
} TcNg;

typedef struct TcFunction {
    Header header;
    struct TcType * arg;
    struct TcType * result;
} TcFunction;

typedef struct TcPair {
    Header header;
    struct TcType * first;
    struct TcType * second;
} TcPair;

typedef struct TcTypeDef {
    Header header;
    HashSymbol * name;
    struct TcTypeDefArgs * args;
} TcTypeDef;

typedef struct TcTypeDefArgs {
    Header header;
    struct TcType * type;
    struct TcTypeDefArgs * next;
} TcTypeDefArgs;

typedef struct TcVar {
    Header header;
    HashSymbol * name;
    struct TcType * instance;
} TcVar;

typedef struct TcType {
    Header header;
    enum TcTypeType  type;
    union TcTypeVal  val;
} TcType;



struct TcEnv * newTcEnv(HashTable * table, struct TcEnv * next);
struct TcNg * newTcNg(HashTable * table, struct TcNg * next);
struct TcFunction * newTcFunction(struct TcType * arg, struct TcType * result);
struct TcPair * newTcPair(struct TcType * first, struct TcType * second);
struct TcTypeDef * newTcTypeDef(HashSymbol * name, struct TcTypeDefArgs * args);
struct TcTypeDefArgs * newTcTypeDefArgs(struct TcType * type, struct TcTypeDefArgs * next);
struct TcVar * newTcVar(HashSymbol * name);
struct TcType * newTcType(enum TcTypeType  type, union TcTypeVal  val);

void markTcEnv(struct TcEnv * x);
void markTcNg(struct TcNg * x);
void markTcFunction(struct TcFunction * x);
void markTcPair(struct TcPair * x);
void markTcTypeDef(struct TcTypeDef * x);
void markTcTypeDefArgs(struct TcTypeDefArgs * x);
void markTcVar(struct TcVar * x);
void markTcType(struct TcType * x);

void freeTcEnv(struct TcEnv * x);
void freeTcNg(struct TcNg * x);
void freeTcFunction(struct TcFunction * x);
void freeTcPair(struct TcPair * x);
void freeTcTypeDef(struct TcTypeDef * x);
void freeTcTypeDefArgs(struct TcTypeDefArgs * x);
void freeTcVar(struct TcVar * x);
void freeTcType(struct TcType * x);


#define TCTYPE_VAL_FUNCTION(x) ((union TcTypeVal ){.function = (x)})
#define TCTYPE_VAL_PAIR(x) ((union TcTypeVal ){.pair = (x)})
#define TCTYPE_VAL_VAR(x) ((union TcTypeVal ){.var = (x)})
#define TCTYPE_VAL_INTEGER() ((union TcTypeVal ){.integer = (NULL)})
#define TCTYPE_VAL_CHARACTER() ((union TcTypeVal ){.character = (NULL)})
#define TCTYPE_VAL_TYPEDEF(x) ((union TcTypeVal ){.typeDef = (x)})


#endif
