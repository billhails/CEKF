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
} AexpVar;

typedef int AexpInteger; // you'll thank me later

typedef enum {
    AEXP_PRIM_ADD,
    AEXP_PRIM_SUB,
    AEXP_PRIM_MUL,
    AEXP_PRIM_DIV,
} AexpPrimOp;

typedef struct AexpPrimApp {
    Header header;
    AexpPrimOp op;
    struct AexpList *args;
} AexpPrimApp;

typedef struct AexpList {
    Header header;
    struct AexpList *next;
    struct Exp *exp;
} AexpList;

typedef struct CexpApply {
    Header header;
    struct Exp *function;
    struct AexpList *args;
} CexpApply;

typedef struct CexpCond {
    Header header;
    struct Exp *condition;
    struct Exp *consequent;
    struct Exp *alternative;
} CexpCond;

typedef struct CexpLetRec {
    Header header;
    struct LetRecBindings *bindings;
    struct Exp *body;
} CexpLetRec;

typedef struct LetRecBindings {
    Header header;
    struct LetRecBindings *next;
    struct AexpVar *var;
    struct Exp *val;
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
    AEXP_TYPE_TRUE,
    AEXP_TYPE_FALSE,
    AEXP_TYPE_INT,
    AEXP_TYPE_PRIM,
    CEXP_TYPE_APPLY,
    CEXP_TYPE_COND,
    CEXP_TYPE_CALLCC,
    CEXP_TYPE_LETREC,
    CEXP_TYPE_AMB,
    CEXP_TYPE_BACK,
    EXP_TYPE_LET,
    EXP_TYPE_DONE,
} ExpType;

typedef union {
    struct AexpLam *lam;
    struct AexpVar *var;
    AexpInteger integer;
    struct AexpPrimApp *prim;
} AexpVal;

typedef union {
    struct CexpApply *apply;
    struct CexpCond *cond;
    struct Exp *callCC;
    struct CexpLetRec *letRec;
    struct CexpAmb *amb;
} CexpVal;

typedef union {
    void *none;
    AexpVal aexp;
    CexpVal cexp;
    struct ExpLet *let;
} ExpVal;

typedef struct Exp {
    Header header;
    ExpType type;
    ExpVal val;
} Exp;

#define AEXP_VAL_LAM(x)    ((ExpVal){.aexp = ((AexpVal){.lam     = (x)})})
#define AEXP_VAL_VAR(x)    ((ExpVal){.aexp = ((AexpVal){.var     = (x)})})
#define AEXP_VAL_INT(x)    ((ExpVal){.aexp = ((AexpVal){.integer = (x)})})
#define AEXP_VAL_PRIM(x)   ((ExpVal){.aexp = ((AexpVal){.prim    = (x)})})

#define CEXP_VAL_APPLY(x)  ((ExpVal){.cexp = ((CexpVal){.apply   = (x)})})
#define CEXP_VAL_COND(x)   ((ExpVal){.cexp = ((CexpVal){.cond    = (x)})})
#define CEXP_VAL_CALLCC(x) ((ExpVal){.cexp = ((CexpVal){.callCC  = (x)})})
#define CEXP_VAL_LETREC(x) ((ExpVal){.cexp = ((CexpVal){.letRec  = (x)})})
#define CEXP_VAL_AMB(x)    ((ExpVal){.cexp = ((CexpVal){.amb     = (x)})})

#define EXP_VAL_LET(x)     ((ExpVal){.let          = (x)})

#define CEXP_VAL_NONE()    ((ExpVal){.none         = NULL})
#define EXP_VAL_NONE()     ((ExpVal){.none         = NULL})
#define AEXP_VAL_NONE()    ((ExpVal){.none         = NULL})

AexpLam *newAexpLam(AexpVarList *args, Exp *exp);
AexpVarList *newAexpVarList(AexpVarList *next, AexpVar *var);
AexpVar *newAexpVar(char *name);
AexpPrimApp *newAexpPrimApp(AexpPrimOp op, AexpList *args);
AexpList *newAexpList(AexpList *next, Exp *exp);
CexpApply *newCexpApply(Exp *function, AexpList *args);
CexpCond *newCexpCond(Exp *condition, Exp *consequent, Exp *alternative);
CexpLetRec *newCexpLetRec(LetRecBindings *bindings, Exp *body);
LetRecBindings *newLetRecBindings(LetRecBindings *next, AexpVar *var, Exp *val);
CexpAmb *newCexpAmb(Exp *exp1, Exp *exp2);
ExpLet *newExpLet(AexpVar *var, Exp *val, Exp *body);
Exp *newExp(ExpType type, ExpVal val);

void markAexpLam(AexpLam *x);
void markAexpVarList(AexpVarList *x);
void markAexpVar(AexpVar *x);
void markAexpPrimApp(AexpPrimApp *x);
void markAexpList(AexpList *x);
void markCexpApply(CexpApply *x);
void markCexpCond(CexpCond *x);
void markCexpLetRec(CexpLetRec *x);
void markLetRecBindings(LetRecBindings *x);
void markCexpAmb(CexpAmb *x);
void markExpLet(ExpLet *x);
void markExp(Exp *x);

#endif
