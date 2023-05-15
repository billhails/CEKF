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
    struct AexpVarList *args;
    struct Exp *exp;
} AexpLam;

typedef struct AexpVarList {
    Header header;
    struct AexpVarList *next;
    struct AexpVar *var;
} AexpVarList;

typedef struct AexpVar {
    Header header;
    char *name;
    hash_t hash;
} AexpVar;

typedef enum {
    VAR_TYPE_STACK,
    VAR_TYPE_ENV,
} AexpAnnotatedVarType;

typedef struct AexpAnnotatedVar {
    Header header;
    AexpAnnotatedVarType type;
    int frame;
    int offset;
    struct AexpVar *var;
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
} AexpPrimOp;

typedef struct AexpPrimApp {
    Header header;
    AexpPrimOp op;
    struct Aexp *exp1;
    struct Aexp *exp2;
} AexpPrimApp;

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

typedef struct CexpCond {
    Header header;
    struct Aexp *condition;
    struct Exp *consequent;
    struct Exp *alternative;
} CexpCond;

typedef struct CexpLetRec {
    Header header;
    int nbindings;
    struct LetRecBindings *bindings;
    struct Exp *body;
} CexpLetRec;

typedef struct LetRecBindings {
    Header header;
    struct LetRecBindings *next;
    struct AexpVar *var;
    struct Aexp *val;
} LetRecBindings;

typedef struct CexpAmb {
    Header header;
    struct Exp *exp1;
    struct Exp *exp2;
} CexpAmb;

typedef struct ExpLet {
    Header header;
    struct AexpVar *var;
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
} AexpType;

typedef union {
    void *none;
    struct AexpLam *lam;
    struct AexpVar *var;
    struct AexpAnnotatedVar *annotatedVar;
    AexpInteger integer;
    struct AexpPrimApp *prim;
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

typedef enum {
    CEXP_TYPE_APPLY,
    CEXP_TYPE_COND,
    CEXP_TYPE_CALLCC,
    CEXP_TYPE_LETREC,
    CEXP_TYPE_AMB,
    CEXP_TYPE_BACK,
} CexpType;

typedef union {
    void *none;
    struct CexpApply *apply;
    struct CexpCond *cond;
    struct Aexp *callCC;
    struct CexpLetRec *letRec;
    struct CexpAmb *amb;
} CexpVal;

typedef struct Cexp {
    Header header;
    CexpType type;
    CexpVal val;
} Cexp;

#define CEXP_VAL_APPLY(x)  ((CexpVal){.apply  = (x)})
#define CEXP_VAL_COND(x)   ((CexpVal){.cond   = (x)})
#define CEXP_VAL_CALLCC(x) ((CexpVal){.callCC = (x)})
#define CEXP_VAL_LETREC(x) ((CexpVal){.letRec = (x)})
#define CEXP_VAL_AMB(x)    ((CexpVal){.amb    = (x)})
#define CEXP_VAL_BACK()    ((CexpVal){.none   = NULL})

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

AexpAnnotatedVar *newAexpAnnotatedVar(AexpAnnotatedVarType type, int frame, int offset, AexpVar *var);
AexpLam *newAexpLam(AexpVarList *args, Exp *exp);
AexpList *newAexpList(AexpList *next, Aexp *exp);
Aexp *newAexp(AexpType type, AexpVal val);
AexpPrimApp *newAexpPrimApp(AexpPrimOp op, Aexp *exp1, Aexp *exp2);
AexpVarList *newAexpVarList(AexpVarList *next, AexpVar *var);
AexpVar *newAexpVar(char *name);
CexpAmb *newCexpAmb(Exp *exp1, Exp *exp2);
CexpApply *newCexpApply(Aexp *function, AexpList *args);
CexpCond *newCexpCond(Aexp *condition, Exp *consequent, Exp *alternative);
CexpLetRec *newCexpLetRec(LetRecBindings *bindings, Exp *body);
Cexp *newCexp(CexpType type, CexpVal val);
ExpLet *newExpLet(AexpVar *var, Exp *val, Exp *body);
Exp *newExp(ExpType type, ExpVal val);
LetRecBindings *newLetRecBindings(LetRecBindings *next, AexpVar *var, Aexp *val);

void markAexpAnnotatedVar(AexpAnnotatedVar *x);
void markAexpLam(AexpLam *x);
void markAexpList(AexpList *x);
void markAexpPrimApp(AexpPrimApp *x);
void markAexpVar(AexpVar *x);
void markAexpVarList(AexpVarList *x);
void markCexpAmb(CexpAmb *x);
void markCexpApply(CexpApply *x);
void markCexpCond(CexpCond *x);
void markCexpLetRec(CexpLetRec *x);
void markAexp(Aexp *x);
void markCexp(Cexp *x);
void markExp(Exp *x);
void markExpLet(ExpLet *x);
void markLetRecBindings(LetRecBindings *x);

void markVarTable();

#endif
