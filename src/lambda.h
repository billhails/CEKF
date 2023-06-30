#ifndef cekf_lambda_h
#define cekf_lambda_h
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

// generated from src/lambda.yaml by makeAST.py



#include "hash.h"
#include "memory.h"

typedef enum LamPrimOp {
    LAMPRIMOP_TYPE_LAM_PRIM_ADD,
    LAMPRIMOP_TYPE_LAM_PRIM_SUB,
    LAMPRIMOP_TYPE_LAM_PRIM_MUL,
    LAMPRIMOP_TYPE_LAM_PRIM_DIV,
    LAMPRIMOP_TYPE_LAM_PRIM_EQ,
    LAMPRIMOP_TYPE_LAM_PRIM_NE,
    LAMPRIMOP_TYPE_LAM_PRIM_GT,
    LAMPRIMOP_TYPE_LAM_PRIM_LT,
    LAMPRIMOP_TYPE_LAM_PRIM_GE,
    LAMPRIMOP_TYPE_LAM_PRIM_LE,
    LAMPRIMOP_TYPE_LAM_PRIM_CONS,
    LAMPRIMOP_TYPE_LAM_PRIM_VEC,
    LAMPRIMOP_TYPE_LAM_PRIM_XOR,
    LAMPRIMOP_TYPE_LAM_PRIM_AND,
    LAMPRIMOP_TYPE_LAM_PRIM_OR,
    LAMPRIMOP_TYPE_LAM_PRIM_AMB,
} LamPrimOp;

typedef enum LamUnaryOp {
    LAMUNARYOP_TYPE_LAM_UNARY_CAR,
    LAMUNARYOP_TYPE_LAM_UNARY_CDR,
    LAMUNARYOP_TYPE_LAM_UNARY_NOT,
    LAMUNARYOP_TYPE_LAM_UNARY_PRINT,
} LamUnaryOp;

typedef enum LamExpType {
    LAMEXP_TYPE_LAM,
    LAMEXP_TYPE_VAR,
    LAMEXP_TYPE_INTEGER,
    LAMEXP_TYPE_PRIM,
    LAMEXP_TYPE_UNARY,
    LAMEXP_TYPE_LIST,
    LAMEXP_TYPE_MAKEVEC,
    LAMEXP_TYPE_APPLY,
    LAMEXP_TYPE_COND,
    LAMEXP_TYPE_CALLCC,
    LAMEXP_TYPE_LETREC,
    LAMEXP_TYPE_MATCH,
    LAMEXP_TYPE_CHARACTER,
    LAMEXP_TYPE_STRING,
    LAMEXP_TYPE_BACK,
    LAMEXP_TYPE_T,
    LAMEXP_TYPE_F,
    LAMEXP_TYPE_NIL,
} LamExpType;



typedef union LamExpVal {
    struct LamLam * lam;
    HashSymbol * var;
    int integer;
    struct LamPrimApp * prim;
    struct LamUnaryApp * unary;
    struct LamList * list;
    struct LamMakeVec * makeVec;
    struct LamApply * apply;
    struct LamCond * cond;
    struct LamExp * callcc;
    struct LamLetRec * letrec;
    struct LamMatch * match;
    char character;
    char * string;
    void * back;
    void * t;
    void * f;
    void * nil;
} LamExpVal;



typedef struct LamLam {
    Header header;
    int nargs;
    struct LamVarList * args;
    struct LamExp * exp;
} LamLam;

typedef struct LamVarList {
    Header header;
    HashSymbol * var;
    struct LamVarList * next;
} LamVarList;

typedef struct LamPrimApp {
    Header header;
    enum LamPrimOp  type;
    struct LamExp * exp1;
    struct LamExp * exp2;
} LamPrimApp;

typedef struct LamUnaryApp {
    Header header;
    enum LamUnaryOp  type;
    struct LamExp * exp;
} LamUnaryApp;

typedef struct LamList {
    Header header;
    struct LamExp * exp;
    struct LamList * next;
} LamList;

typedef struct LamApply {
    Header header;
    struct LamExp * function;
    int nargs;
    struct LamList * args;
} LamApply;

typedef struct LamMakeVec {
    Header header;
    int nargs;
    struct LamList * args;
} LamMakeVec;

typedef struct LamCond {
    Header header;
    struct LamExp * condition;
    struct LamExp * consequent;
    struct LamExp * alternative;
} LamCond;

typedef struct LamMatch {
    Header header;
    struct LamExp * index;
    struct LamMatchList * cases;
} LamMatch;

typedef struct LamMatchList {
    Header header;
    struct LamList * matches;
    struct LamExp * body;
    struct LamMatchList * next;
} LamMatchList;

typedef struct LamLetRec {
    Header header;
    int nbindings;
    struct LamLetRecBindings * bindings;
    struct LamList * body;
} LamLetRec;

typedef struct LamLetRecBindings {
    Header header;
    HashSymbol * var;
    struct LamExp * val;
    struct LamLetRecBindings * next;
} LamLetRecBindings;

typedef struct LamContext {
    Header header;
    HashTable * frame;
    struct LamContext * parent;
} LamContext;

typedef struct LamTypeConstructorInfo {
    Header header;
    int nargs;
    int index;
} LamTypeConstructorInfo;

typedef struct LamExp {
    Header header;
    enum LamExpType  type;
    union LamExpVal  val;
} LamExp;

struct LamLam * newLamLam(int nargs, struct LamVarList * args, struct LamExp * exp);
struct LamVarList * newLamVarList(HashSymbol * var, struct LamVarList * next);
struct LamPrimApp * newLamPrimApp(enum LamPrimOp  type, struct LamExp * exp1, struct LamExp * exp2);
struct LamUnaryApp * newLamUnaryApp(enum LamUnaryOp  type, struct LamExp * exp);
struct LamList * newLamList(struct LamExp * exp, struct LamList * next);
struct LamApply * newLamApply(struct LamExp * function, int nargs, struct LamList * args);
struct LamMakeVec * newLamMakeVec(int nargs, struct LamList * args);
struct LamCond * newLamCond(struct LamExp * condition, struct LamExp * consequent, struct LamExp * alternative);
struct LamMatch * newLamMatch(struct LamExp * index, struct LamMatchList * cases);
struct LamMatchList * newLamMatchList(struct LamList * matches, struct LamExp * body, struct LamMatchList * next);
struct LamLetRec * newLamLetRec(int nbindings, struct LamLetRecBindings * bindings, struct LamList * body);
struct LamLetRecBindings * newLamLetRecBindings(HashSymbol * var, struct LamExp * val, struct LamLetRecBindings * next);
struct LamContext * newLamContext(HashTable * frame, struct LamContext * parent);
struct LamTypeConstructorInfo * newLamTypeConstructorInfo(int nargs, int index);
struct LamExp * newLamExp(enum LamExpType  type, union LamExpVal  val);

void markLamLam(struct LamLam * x);
void markLamVarList(struct LamVarList * x);
void markLamPrimApp(struct LamPrimApp * x);
void markLamUnaryApp(struct LamUnaryApp * x);
void markLamList(struct LamList * x);
void markLamApply(struct LamApply * x);
void markLamMakeVec(struct LamMakeVec * x);
void markLamCond(struct LamCond * x);
void markLamMatch(struct LamMatch * x);
void markLamMatchList(struct LamMatchList * x);
void markLamLetRec(struct LamLetRec * x);
void markLamLetRecBindings(struct LamLetRecBindings * x);
void markLamContext(struct LamContext * x);
void markLamTypeConstructorInfo(struct LamTypeConstructorInfo * x);
void markLamExp(struct LamExp * x);

void freeLamLam(struct LamLam * x);
void freeLamVarList(struct LamVarList * x);
void freeLamPrimApp(struct LamPrimApp * x);
void freeLamUnaryApp(struct LamUnaryApp * x);
void freeLamList(struct LamList * x);
void freeLamApply(struct LamApply * x);
void freeLamMakeVec(struct LamMakeVec * x);
void freeLamCond(struct LamCond * x);
void freeLamMatch(struct LamMatch * x);
void freeLamMatchList(struct LamMatchList * x);
void freeLamLetRec(struct LamLetRec * x);
void freeLamLetRecBindings(struct LamLetRecBindings * x);
void freeLamContext(struct LamContext * x);
void freeLamTypeConstructorInfo(struct LamTypeConstructorInfo * x);
void freeLamExp(struct LamExp * x);

#define LAMEXP_VAL_LAM(x) ((union LamExpVal ){.lam = (x)})
#define LAMEXP_VAL_VAR(x) ((union LamExpVal ){.var = (x)})
#define LAMEXP_VAL_INTEGER(x) ((union LamExpVal ){.integer = (x)})
#define LAMEXP_VAL_PRIM(x) ((union LamExpVal ){.prim = (x)})
#define LAMEXP_VAL_UNARY(x) ((union LamExpVal ){.unary = (x)})
#define LAMEXP_VAL_LIST(x) ((union LamExpVal ){.list = (x)})
#define LAMEXP_VAL_MAKEVEC(x) ((union LamExpVal ){.makeVec = (x)})
#define LAMEXP_VAL_APPLY(x) ((union LamExpVal ){.apply = (x)})
#define LAMEXP_VAL_COND(x) ((union LamExpVal ){.cond = (x)})
#define LAMEXP_VAL_CALLCC(x) ((union LamExpVal ){.callcc = (x)})
#define LAMEXP_VAL_LETREC(x) ((union LamExpVal ){.letrec = (x)})
#define LAMEXP_VAL_MATCH(x) ((union LamExpVal ){.match = (x)})
#define LAMEXP_VAL_CHARACTER(x) ((union LamExpVal ){.character = (x)})
#define LAMEXP_VAL_STRING(x) ((union LamExpVal ){.string = (x)})
#define LAMEXP_VAL_BACK() ((union LamExpVal ){.back = (NULL)})
#define LAMEXP_VAL_T() ((union LamExpVal ){.t = (NULL)})
#define LAMEXP_VAL_F() ((union LamExpVal ){.f = (NULL)})
#define LAMEXP_VAL_NIL() ((union LamExpVal ){.nil = (NULL)})

#endif
