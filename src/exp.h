#ifndef cekf_exp_h
#define cekf_exp_h
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

#include <stddef.h>

#include "common.h"
#include "memory.h"
#include "hash.h"
#include "symbol.h"

/**
 * This file defines the A-Normal form expressions
 * evaluated by the CEKF machine.
 *
 * The semantics in cekf.h refer to these expressions,
 * but these expressions make no reference to the semantics.
 */

typedef struct AexpLam {
    Header header;
    int nargs;
    int letRecOffset;
    struct AexpVarList *args;
    struct Exp *exp;
} AexpLam;

typedef struct AexpVarList {
    Header header;
    struct AexpVarList *next;
    struct HashSymbol *var;
} AexpVarList;

typedef enum {
    VAR_TYPE_STACK,
    VAR_TYPE_ENV,
} AexpAnnotatedVarType;

typedef struct AexpAnnotatedVar {
    Header header;
    AexpAnnotatedVarType type;
    int frame;
    int offset;
    struct HashSymbol *var;
} AexpAnnotatedVar;

typedef int AexpInteger; // you'll thank me later

typedef enum {
    AEXP_PRIM_ADD,
    AEXP_PRIM_SUB,
    AEXP_PRIM_MUL,
    AEXP_PRIM_DIV,
    AEXP_PRIM_EQ,
    AEXP_PRIM_NE,
    AEXP_PRIM_GT,
    AEXP_PRIM_LT,
    AEXP_PRIM_GE,
    AEXP_PRIM_LE,
    AEXP_PRIM_CONS,
    AEXP_PRIM_VEC,
    AEXP_PRIM_XOR,
} AexpPrimOp;

typedef struct AexpPrimApp {
    Header header;
    AexpPrimOp op;
    struct Aexp *exp1;
    struct Aexp *exp2;
} AexpPrimApp;

typedef enum {
    AEXP_UNARY_CAR,
    AEXP_UNARY_CDR,
    AEXP_UNARY_NOT,
    AEXP_UNARY_PRINT,
} AexpUnaryOp;

typedef struct AexpUnaryApp {
    Header header;
    AexpUnaryOp op;
    struct Aexp *exp;
} AexpUnaryApp;

typedef struct AexpList {
    Header header;
    struct AexpList *next;
    struct Aexp *exp;
} AexpList;

typedef struct CexpApply {
    Header header;
    struct Aexp *function;
    int nargs;
    struct AexpList *args;
} CexpApply;

typedef struct AexpMakeVec {
    Header header;
    int nargs;
    struct AexpList *args;
} AexpMakeVec;

typedef struct CexpCond {
    Header header;
    struct Aexp *condition;
    struct Exp *consequent;
    struct Exp *alternative;
} CexpCond;

typedef struct CexpMatch {
    Header header;
    struct Aexp *condition;
    struct MatchList *clauses;
} CexpMatch;

typedef struct MatchList {
    Header header;
    struct AexpList *matches;
    struct Exp *body;
    struct MatchList *next;
} MatchList;

typedef struct CexpLetRec {
    Header header;
    int nbindings;
    struct LetRecBindings *bindings;
    struct Exp *body;
} CexpLetRec;

typedef struct LetRecBindings {
    Header header;
    struct LetRecBindings *next;
    struct HashSymbol *var;
    struct Aexp *val;
} LetRecBindings;

typedef struct CexpAmb {
    Header header;
    struct Exp *exp1;
    struct Exp *exp2;
} CexpAmb;

typedef enum {
    BOOL_TYPE_AND,
    BOOL_TYPE_OR,
} CexpBoolType;

typedef struct CexpBool {
    Header header;
    CexpBoolType type;
    struct Exp *exp1;
    struct Exp *exp2;
} CexpBool;

typedef struct ExpLet {
    Header header;
    struct HashSymbol *var;
    struct Exp *val;
    struct Exp *body;
} ExpLet;

typedef enum {
    AEXP_TYPE_LAM,
    AEXP_TYPE_VAR,
    AEXP_TYPE_ANNOTATEDVAR,
    AEXP_TYPE_TRUE,
    AEXP_TYPE_FALSE,
    AEXP_TYPE_VOID,
    AEXP_TYPE_INT,
    AEXP_TYPE_PRIM,
    AEXP_TYPE_UNARY,
    AEXP_TYPE_LIST,
    AEXP_TYPE_MAKEVEC,
} AexpType;

typedef union {
    void *none;
    struct AexpLam *lam;
    struct HashSymbol *var;
    struct AexpAnnotatedVar *annotatedVar;
    AexpInteger integer;
    struct AexpPrimApp *prim;
    struct AexpUnaryApp *unary;
    struct AexpList *list;
    struct AexpMakeVec *makeVec;
} AexpVal;

typedef struct Aexp {
    Header header;
    AexpType type;
    AexpVal val;
} Aexp;

#define AEXP_VAL_LAM(x)          ((AexpVal){.lam          = (x)})
#define AEXP_VAL_VAR(x)          ((AexpVal){.var          = (x)})
#define AEXP_VAL_ANNOTATEDVAR(x) ((AexpVal){.annotatedVar = (x)})
#define AEXP_VAL_TRUE()          ((AexpVal){.none         = NULL})
#define AEXP_VAL_FALSE()         ((AexpVal){.none         = NULL})
#define AEXP_VAL_VOID()          ((AexpVal){.none         = NULL})
#define AEXP_VAL_INT(x)          ((AexpVal){.integer      = (x)})
#define AEXP_VAL_PRIM(x)         ((AexpVal){.prim         = (x)})
#define AEXP_VAL_UNARY(x)        ((AexpVal){.unary        = (x)})
#define AEXP_VAL_LIST(x)         ((AexpVal){.list         = (x)})
#define AEXP_VAL_MAKEVEC(x)      ((AexpVal){.makeVec      = (x)})

typedef enum {
    CEXP_TYPE_APPLY,
    CEXP_TYPE_COND,
    CEXP_TYPE_CALLCC,
    CEXP_TYPE_LETREC,
    CEXP_TYPE_AMB,
    CEXP_TYPE_BACK,
    CEXP_TYPE_BOOL,
    CEXP_TYPE_MATCH,
} CexpType;

typedef union {
    void *none;
    struct CexpApply *apply;
    struct CexpCond *cond;
    struct Aexp *callCC;
    struct CexpLetRec *letRec;
    struct CexpAmb *amb;
    struct CexpBool *boolean;
    struct CexpMatch *match;
} CexpVal;

typedef struct Cexp {
    Header header;
    CexpType type;
    CexpVal val;
} Cexp;

#define CEXP_VAL_APPLY(x)  ((CexpVal){.apply   = (x)})
#define CEXP_VAL_COND(x)   ((CexpVal){.cond    = (x)})
#define CEXP_VAL_CALLCC(x) ((CexpVal){.callCC  = (x)})
#define CEXP_VAL_LETREC(x) ((CexpVal){.letRec  = (x)})
#define CEXP_VAL_AMB(x)    ((CexpVal){.amb     = (x)})
#define CEXP_VAL_BOOL(x)   ((CexpVal){.boolean = (x)})
#define CEXP_VAL_MATCH(x)  ((CexpVal){.match   = (x)})
#define CEXP_VAL_BACK()    ((CexpVal){.none    = NULL})

typedef enum {
    EXP_TYPE_AEXP,
    EXP_TYPE_CEXP,
    EXP_TYPE_LET,
    EXP_TYPE_DONE,
} ExpType;

typedef union {
    void *none;
    Aexp *aexp;
    Cexp *cexp;
    struct ExpLet *let;
} ExpVal;

typedef struct Exp {
    Header header;
    ExpType type;
    ExpVal val;
} Exp;

#define EXP_VAL_AEXP(x) ((ExpVal){.aexp = (x)})
#define EXP_VAL_CEXP(x) ((ExpVal){.cexp = (x)})
#define EXP_VAL_LET(x)  ((ExpVal){.let  = (x)})
#define EXP_VAL_DONE()  ((ExpVal){.none = NULL})

AexpAnnotatedVar *newAexpAnnotatedVar(AexpAnnotatedVarType type, int frame, int offset, HashSymbol *var);
AexpLam *newAexpLam(AexpVarList *args, Exp *exp);
AexpList *newAexpList(AexpList *next, Aexp *exp);
Aexp *newAexp(AexpType type, AexpVal val);
AexpPrimApp *newAexpPrimApp(AexpPrimOp op, Aexp *exp1, Aexp *exp2);
AexpUnaryApp *newAexpUnaryApp(AexpUnaryOp op, Aexp *exp);
AexpVarList *newAexpVarList(AexpVarList *next, HashSymbol *var);
AexpMakeVec *newAexpMakeVec(AexpList *args);
HashSymbol *newAexpVar(char *name);
CexpAmb *newCexpAmb(Exp *exp1, Exp *exp2);
CexpBool *newCexpBool(CexpBoolType type, Exp *exp1, Exp *exp2);
CexpApply *newCexpApply(Aexp *function, AexpList *args);
CexpCond *newCexpCond(Aexp *condition, Exp *consequent, Exp *alternative);
CexpLetRec *newCexpLetRec(LetRecBindings *bindings, Exp *body);
CexpMatch *newCexpMatch(Aexp *condition, MatchList *clauses);
MatchList *newMatchList(AexpList *matches, Exp *body, MatchList *next);
Cexp *newCexp(CexpType type, CexpVal val);
ExpLet *newExpLet(HashSymbol *var, Exp *val, Exp *body);
Exp *newExp(ExpType type, ExpVal val);
LetRecBindings *newLetRecBindings(LetRecBindings *next, HashSymbol *var, Aexp *val);


void markAexpAnnotatedVar(AexpAnnotatedVar *x);
void markAexpLam(AexpLam *x);
void markAexpList(AexpList *x);
void markAexpPrimApp(AexpPrimApp *x);
void markAexpUnaryApp(AexpUnaryApp *x);
void markAexpVarList(AexpVarList *x);
void markAexpMakeVec(AexpMakeVec *x);
void markCexpAmb(CexpAmb *x);
void markCexpBool(CexpBool *x);
void markCexpApply(CexpApply *x);
void markCexpCond(CexpCond *x);
void markCexpLetRec(CexpLetRec *x);
void markCexpMatch(CexpMatch *x);
void markMatchList(MatchList *x);
void markAexp(Aexp *x);
void markCexp(Cexp *x);
void markExp(Exp *x);
void markExpLet(ExpLet *x);
void markLetRecBindings(LetRecBindings *x);

void markVarTable();

#endif
