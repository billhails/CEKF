#ifndef cekf_anf_h
#define cekf_anf_h
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
 * ANF structures to be converted to bytecode.
 *
 * Generated from src/anf.yaml by tools/makeAST.py
 */

#include "hash.h"
#include "memory.h"
#include "common.h"
#include "bigint.h"
#include "ast_helper.h"

/*
 * typedefs
 */

typedef enum AexpAnnotatedVarType {
    AEXPANNOTATEDVARTYPE_TYPE_STACK,
    AEXPANNOTATEDVARTYPE_TYPE_ENV,
} AexpAnnotatedVarType;

typedef enum AexpPrimOp {
    AEXPPRIMOP_TYPE_ADD,
    AEXPPRIMOP_TYPE_SUB,
    AEXPPRIMOP_TYPE_MUL,
    AEXPPRIMOP_TYPE_DIV,
    AEXPPRIMOP_TYPE_POW,
    AEXPPRIMOP_TYPE_EQ,
    AEXPPRIMOP_TYPE_NE,
    AEXPPRIMOP_TYPE_LT,
    AEXPPRIMOP_TYPE_GT,
    AEXPPRIMOP_TYPE_LE,
    AEXPPRIMOP_TYPE_GE,
    AEXPPRIMOP_TYPE_CONS,
    AEXPPRIMOP_TYPE_VEC,
    AEXPPRIMOP_TYPE_XOR,
    AEXPPRIMOP_TYPE_MOD,
} AexpPrimOp;

typedef enum AexpUnaryOp {
    AEXPUNARYOP_TYPE_CAR,
    AEXPUNARYOP_TYPE_CDR,
    AEXPUNARYOP_TYPE_NOT,
    AEXPUNARYOP_TYPE_PRINT,
} AexpUnaryOp;

typedef enum CexpBoolType {
    CEXPBOOLTYPE_TYPE_AND,
    CEXPBOOLTYPE_TYPE_OR,
} CexpBoolType;

typedef enum CexpCondCasesType {
    CEXPCONDCASES_TYPE_CHARCASES,
    CEXPCONDCASES_TYPE_INTCASES,
} CexpCondCasesType;

typedef enum AexpType {
    AEXP_TYPE_T,
    AEXP_TYPE_F,
    AEXP_TYPE_V,
    AEXP_TYPE_LAM,
    AEXP_TYPE_VAR,
    AEXP_TYPE_ANNOTATEDVAR,
    AEXP_TYPE_BIGINTEGER,
    AEXP_TYPE_LITTLEINTEGER,
    AEXP_TYPE_CHARACTER,
    AEXP_TYPE_PRIM,
    AEXP_TYPE_UNARY,
    AEXP_TYPE_LIST,
    AEXP_TYPE_MAKEVEC,
} AexpType;

typedef enum CexpType {
    CEXP_TYPE_BACK,
    CEXP_TYPE_ERROR,
    CEXP_TYPE_APPLY,
    CEXP_TYPE_IFF,
    CEXP_TYPE_COND,
    CEXP_TYPE_CALLCC,
    CEXP_TYPE_LETREC,
    CEXP_TYPE_AMB,
    CEXP_TYPE_CUT,
    CEXP_TYPE_BOOLEAN,
    CEXP_TYPE_MATCH,
} CexpType;

typedef enum ExpType {
    EXP_TYPE_DONE,
    EXP_TYPE_AEXP,
    EXP_TYPE_CEXP,
    EXP_TYPE_LET,
} ExpType;



typedef union CexpCondCasesVal {
    struct CexpCharCondCases * charCases;
    struct CexpIntCondCases * intCases;
} CexpCondCasesVal;

typedef union AexpVal {
    void * t;
    void * f;
    void * v;
    struct AexpLam * lam;
    HashSymbol * var;
    struct AexpAnnotatedVar * annotatedVar;
    BigInt * biginteger;
    int littleinteger;
    char character;
    struct AexpPrimApp * prim;
    struct AexpUnaryApp * unary;
    struct AexpList * list;
    struct AexpMakeVec * makeVec;
} AexpVal;

typedef union CexpVal {
    void * back;
    void * error;
    struct CexpApply * apply;
    struct CexpIf * iff;
    struct CexpCond * cond;
    struct Aexp * callCC;
    struct CexpLetRec * letRec;
    struct CexpAmb * amb;
    struct CexpCut * cut;
    struct CexpBool * boolean;
    struct CexpMatch * match;
} CexpVal;

typedef union ExpVal {
    void * done;
    struct Aexp * aexp;
    struct Cexp * cexp;
    struct ExpLet * let;
} ExpVal;



typedef struct AexpLam {
    Header header;
    int nargs;
    int letRecOffset;
    struct AexpVarList * args;
    struct Exp * exp;
} AexpLam;

typedef struct AexpVarList {
    Header header;
    HashSymbol * var;
    struct AexpVarList * next;
} AexpVarList;

typedef struct AexpAnnotatedVar {
    Header header;
    enum AexpAnnotatedVarType  type;
    int frame;
    int offset;
    HashSymbol * var;
} AexpAnnotatedVar;

typedef struct AexpPrimApp {
    Header header;
    enum AexpPrimOp  type;
    struct Aexp * exp1;
    struct Aexp * exp2;
} AexpPrimApp;

typedef struct AexpUnaryApp {
    Header header;
    enum AexpUnaryOp  type;
    struct Aexp * exp;
} AexpUnaryApp;

typedef struct AexpList {
    Header header;
    struct Aexp * exp;
    struct AexpList * next;
} AexpList;

typedef struct AexpIntList {
    Header header;
    int integer;
    struct AexpIntList * next;
} AexpIntList;

typedef struct CexpApply {
    Header header;
    struct Aexp * function;
    int nargs;
    struct AexpList * args;
} CexpApply;

typedef struct AexpMakeVec {
    Header header;
    int nargs;
    struct AexpList * args;
} AexpMakeVec;

typedef struct CexpIf {
    Header header;
    struct Aexp * condition;
    struct Exp * consequent;
    struct Exp * alternative;
} CexpIf;

typedef struct CexpCond {
    Header header;
    struct Aexp * condition;
    struct CexpCondCases * cases;
} CexpCond;

typedef struct CexpIntCondCases {
    Header header;
    BigInt * option;
    struct Exp * body;
    struct CexpIntCondCases * next;
} CexpIntCondCases;

typedef struct CexpCharCondCases {
    Header header;
    char option;
    struct Exp * body;
    struct CexpCharCondCases * next;
} CexpCharCondCases;

typedef struct CexpMatch {
    Header header;
    struct Aexp * condition;
    struct MatchList * clauses;
} CexpMatch;

typedef struct MatchList {
    Header header;
    struct AexpIntList * matches;
    struct Exp * body;
    struct MatchList * next;
} MatchList;

typedef struct CexpLetRec {
    Header header;
    int nbindings;
    struct LetRecBindings * bindings;
    struct Exp * body;
} CexpLetRec;

typedef struct LetRecBindings {
    Header header;
    HashSymbol * var;
    struct Aexp * val;
    struct LetRecBindings * next;
} LetRecBindings;

typedef struct CexpAmb {
    Header header;
    struct Exp * exp1;
    struct Exp * exp2;
} CexpAmb;

typedef struct CexpCut {
    Header header;
    struct Exp * exp;
} CexpCut;

typedef struct CexpBool {
    Header header;
    enum CexpBoolType  type;
    struct Exp * exp1;
    struct Exp * exp2;
} CexpBool;

typedef struct ExpLet {
    Header header;
    HashSymbol * var;
    struct Exp * val;
    struct Exp * body;
} ExpLet;

typedef struct CexpCondCases {
    Header header;
    enum CexpCondCasesType  type;
    union CexpCondCasesVal  val;
} CexpCondCases;

typedef struct Aexp {
    Header header;
    enum AexpType  type;
    union AexpVal  val;
} Aexp;

typedef struct Cexp {
    Header header;
    enum CexpType  type;
    union CexpVal  val;
} Cexp;

typedef struct Exp {
    Header header;
    enum ExpType  type;
    union ExpVal  val;
} Exp;




/*
 * constructor declaration
 */

struct AexpLam * newAexpLam(int nargs, int letRecOffset, struct AexpVarList * args, struct Exp * exp);
struct AexpVarList * newAexpVarList(HashSymbol * var, struct AexpVarList * next);
struct AexpAnnotatedVar * newAexpAnnotatedVar(enum AexpAnnotatedVarType  type, int frame, int offset, HashSymbol * var);
struct AexpPrimApp * newAexpPrimApp(enum AexpPrimOp  type, struct Aexp * exp1, struct Aexp * exp2);
struct AexpUnaryApp * newAexpUnaryApp(enum AexpUnaryOp  type, struct Aexp * exp);
struct AexpList * newAexpList(struct Aexp * exp, struct AexpList * next);
struct AexpIntList * newAexpIntList(int integer, struct AexpIntList * next);
struct CexpApply * newCexpApply(struct Aexp * function, int nargs, struct AexpList * args);
struct AexpMakeVec * newAexpMakeVec(int nargs, struct AexpList * args);
struct CexpIf * newCexpIf(struct Aexp * condition, struct Exp * consequent, struct Exp * alternative);
struct CexpCond * newCexpCond(struct Aexp * condition, struct CexpCondCases * cases);
struct CexpIntCondCases * newCexpIntCondCases(BigInt * option, struct Exp * body, struct CexpIntCondCases * next);
struct CexpCharCondCases * newCexpCharCondCases(char option, struct Exp * body, struct CexpCharCondCases * next);
struct CexpMatch * newCexpMatch(struct Aexp * condition, struct MatchList * clauses);
struct MatchList * newMatchList(struct AexpIntList * matches, struct Exp * body, struct MatchList * next);
struct CexpLetRec * newCexpLetRec(int nbindings, struct LetRecBindings * bindings, struct Exp * body);
struct LetRecBindings * newLetRecBindings(HashSymbol * var, struct Aexp * val, struct LetRecBindings * next);
struct CexpAmb * newCexpAmb(struct Exp * exp1, struct Exp * exp2);
struct CexpCut * newCexpCut(struct Exp * exp);
struct CexpBool * newCexpBool(enum CexpBoolType  type, struct Exp * exp1, struct Exp * exp2);
struct ExpLet * newExpLet(HashSymbol * var, struct Exp * val, struct Exp * body);
struct CexpCondCases * newCexpCondCases(enum CexpCondCasesType  type, union CexpCondCasesVal  val);
struct Aexp * newAexp(enum AexpType  type, union AexpVal  val);
struct Cexp * newCexp(enum CexpType  type, union CexpVal  val);
struct Exp * newExp(enum ExpType  type, union ExpVal  val);

/*
 * copy declarations
 */

struct AexpLam * copyAexpLam(struct AexpLam * o);
struct AexpVarList * copyAexpVarList(struct AexpVarList * o);
struct AexpAnnotatedVar * copyAexpAnnotatedVar(struct AexpAnnotatedVar * o);
struct AexpPrimApp * copyAexpPrimApp(struct AexpPrimApp * o);
struct AexpUnaryApp * copyAexpUnaryApp(struct AexpUnaryApp * o);
struct AexpList * copyAexpList(struct AexpList * o);
struct AexpIntList * copyAexpIntList(struct AexpIntList * o);
struct CexpApply * copyCexpApply(struct CexpApply * o);
struct AexpMakeVec * copyAexpMakeVec(struct AexpMakeVec * o);
struct CexpIf * copyCexpIf(struct CexpIf * o);
struct CexpCond * copyCexpCond(struct CexpCond * o);
struct CexpIntCondCases * copyCexpIntCondCases(struct CexpIntCondCases * o);
struct CexpCharCondCases * copyCexpCharCondCases(struct CexpCharCondCases * o);
struct CexpMatch * copyCexpMatch(struct CexpMatch * o);
struct MatchList * copyMatchList(struct MatchList * o);
struct CexpLetRec * copyCexpLetRec(struct CexpLetRec * o);
struct LetRecBindings * copyLetRecBindings(struct LetRecBindings * o);
struct CexpAmb * copyCexpAmb(struct CexpAmb * o);
struct CexpCut * copyCexpCut(struct CexpCut * o);
struct CexpBool * copyCexpBool(struct CexpBool * o);
struct ExpLet * copyExpLet(struct ExpLet * o);
struct CexpCondCases * copyCexpCondCases(struct CexpCondCases * o);
struct Aexp * copyAexp(struct Aexp * o);
struct Cexp * copyCexp(struct Cexp * o);
struct Exp * copyExp(struct Exp * o);

/*
 * mark declarations
 */

void markAexpLam(struct AexpLam * x);
void markAexpVarList(struct AexpVarList * x);
void markAexpAnnotatedVar(struct AexpAnnotatedVar * x);
void markAexpPrimApp(struct AexpPrimApp * x);
void markAexpUnaryApp(struct AexpUnaryApp * x);
void markAexpList(struct AexpList * x);
void markAexpIntList(struct AexpIntList * x);
void markCexpApply(struct CexpApply * x);
void markAexpMakeVec(struct AexpMakeVec * x);
void markCexpIf(struct CexpIf * x);
void markCexpCond(struct CexpCond * x);
void markCexpIntCondCases(struct CexpIntCondCases * x);
void markCexpCharCondCases(struct CexpCharCondCases * x);
void markCexpMatch(struct CexpMatch * x);
void markMatchList(struct MatchList * x);
void markCexpLetRec(struct CexpLetRec * x);
void markLetRecBindings(struct LetRecBindings * x);
void markCexpAmb(struct CexpAmb * x);
void markCexpCut(struct CexpCut * x);
void markCexpBool(struct CexpBool * x);
void markExpLet(struct ExpLet * x);
void markCexpCondCases(struct CexpCondCases * x);
void markAexp(struct Aexp * x);
void markCexp(struct Cexp * x);
void markExp(struct Exp * x);

/*
 * free declarations
 */

void freeAexpLam(struct AexpLam * x);
void freeAexpVarList(struct AexpVarList * x);
void freeAexpAnnotatedVar(struct AexpAnnotatedVar * x);
void freeAexpPrimApp(struct AexpPrimApp * x);
void freeAexpUnaryApp(struct AexpUnaryApp * x);
void freeAexpList(struct AexpList * x);
void freeAexpIntList(struct AexpIntList * x);
void freeCexpApply(struct CexpApply * x);
void freeAexpMakeVec(struct AexpMakeVec * x);
void freeCexpIf(struct CexpIf * x);
void freeCexpCond(struct CexpCond * x);
void freeCexpIntCondCases(struct CexpIntCondCases * x);
void freeCexpCharCondCases(struct CexpCharCondCases * x);
void freeCexpMatch(struct CexpMatch * x);
void freeMatchList(struct MatchList * x);
void freeCexpLetRec(struct CexpLetRec * x);
void freeLetRecBindings(struct LetRecBindings * x);
void freeCexpAmb(struct CexpAmb * x);
void freeCexpCut(struct CexpCut * x);
void freeCexpBool(struct CexpBool * x);
void freeExpLet(struct ExpLet * x);
void freeCexpCondCases(struct CexpCondCases * x);
void freeAexp(struct Aexp * x);
void freeCexp(struct Cexp * x);
void freeExp(struct Exp * x);

/*
 * push declarations
 */


/*
 * defines
 */

#define CEXPCONDCASES_VAL_CHARCASES(x) ((union CexpCondCasesVal ){.charCases = (x)})
#define CEXPCONDCASES_VAL_INTCASES(x) ((union CexpCondCasesVal ){.intCases = (x)})
#define AEXP_VAL_T() ((union AexpVal ){.t = (NULL)})
#define AEXP_VAL_F() ((union AexpVal ){.f = (NULL)})
#define AEXP_VAL_V() ((union AexpVal ){.v = (NULL)})
#define AEXP_VAL_LAM(x) ((union AexpVal ){.lam = (x)})
#define AEXP_VAL_VAR(x) ((union AexpVal ){.var = (x)})
#define AEXP_VAL_ANNOTATEDVAR(x) ((union AexpVal ){.annotatedVar = (x)})
#define AEXP_VAL_BIGINTEGER(x) ((union AexpVal ){.biginteger = (x)})
#define AEXP_VAL_LITTLEINTEGER(x) ((union AexpVal ){.littleinteger = (x)})
#define AEXP_VAL_CHARACTER(x) ((union AexpVal ){.character = (x)})
#define AEXP_VAL_PRIM(x) ((union AexpVal ){.prim = (x)})
#define AEXP_VAL_UNARY(x) ((union AexpVal ){.unary = (x)})
#define AEXP_VAL_LIST(x) ((union AexpVal ){.list = (x)})
#define AEXP_VAL_MAKEVEC(x) ((union AexpVal ){.makeVec = (x)})
#define CEXP_VAL_BACK() ((union CexpVal ){.back = (NULL)})
#define CEXP_VAL_ERROR() ((union CexpVal ){.error = (NULL)})
#define CEXP_VAL_APPLY(x) ((union CexpVal ){.apply = (x)})
#define CEXP_VAL_IFF(x) ((union CexpVal ){.iff = (x)})
#define CEXP_VAL_COND(x) ((union CexpVal ){.cond = (x)})
#define CEXP_VAL_CALLCC(x) ((union CexpVal ){.callCC = (x)})
#define CEXP_VAL_LETREC(x) ((union CexpVal ){.letRec = (x)})
#define CEXP_VAL_AMB(x) ((union CexpVal ){.amb = (x)})
#define CEXP_VAL_CUT(x) ((union CexpVal ){.cut = (x)})
#define CEXP_VAL_BOOLEAN(x) ((union CexpVal ){.boolean = (x)})
#define CEXP_VAL_MATCH(x) ((union CexpVal ){.match = (x)})
#define EXP_VAL_DONE() ((union ExpVal ){.done = (NULL)})
#define EXP_VAL_AEXP(x) ((union ExpVal ){.aexp = (x)})
#define EXP_VAL_CEXP(x) ((union ExpVal ){.cexp = (x)})
#define EXP_VAL_LET(x) ((union ExpVal ){.let = (x)})

/*
 * access declarations
 */


#endif
