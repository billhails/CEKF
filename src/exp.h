#ifndef cekf_exp_h
#define cekf_exp_h

#include <stddef.h>

/**
 * This file defines the A-Normal form expressions
 * evaluated by the CEKF machine.
 *
 * The semantics in cekf.h refer to these expressions,
 * but these expressions make no reference to the semantics.
 */

typedef struct AexpLam {
    struct AexpVarList *args;
    struct Exp *exp;
} AexpLam;

typedef struct AexpVarList {
    struct AexpVarList *next;
    struct AexpVar *var;
} AexpVarList;

typedef struct AexpVar {
    char *name;
} AexpVar;

typedef enum {
    AEXP_LAM_TYPE,
    AEXP_VAR_TYPE,
    AEXP_TRUE_TYPE,
    AEXP_FALSE_TYPE,
    AEXP_INT_TYPE,
    AEXP_PRIM_TYPE,
} AexpType;

typedef int AexpInteger; // you'll thank me later

typedef union {
    struct AexpLam *lam;
    struct AexpVar *var;
    AexpInteger integer;
    struct AexpPrimApp *prim;
    void *none;
} AexpVal;

#define AEXP_VAL_LAM(x)  ((AexpVal){.lam     = (x)})
#define AEXP_VAL_VAR(x)  ((AexpVal){.var     = (x)})
#define AEXP_VAL_INT(x)  ((AexpVal){.integer = (x)})
#define AEXP_VAL_PRIM(x) ((AexpVal){.prim    = (x)})
#define AEXP_VAL_NONE()  ((AexpVal){.none    = NULL})

typedef struct Aexp {
    AexpType type;
    AexpVal val;
} Aexp;

typedef enum {
    AEXP_PRIM_ADD,
    AEXP_PRIM_SUB,
    AEXP_PRIM_MUL,
    AEXP_PRIM_DIV,
} AexpPrimOp;

typedef struct AexpPrimApp {
    AexpPrimOp op;
    struct AexpList *args;
} AexpPrimApp;

typedef struct AexpList {
    struct AexpList *next;
    struct Aexp *exp;
} AexpList;

typedef enum {
    CEXP_APPLY_TYPE,
    CEXP_CONDITIONAL_TYPE,
    CEXP_CALLCC_TYPE,
    CEXP_LETREC_TYPE,
    CEXP_AMB_TYPE,
    CEXP_BACK_TYPE,
} CexpType;

typedef union {
    struct CexpApply *apply;
    struct CexpConditional *conditional;
    struct Aexp *callCC;
    struct CexpLetRec *letRec;
    struct CexpAmb *amb;
    void *none;
} CexpVal;

typedef struct Cexp {
    CexpType type;
    CexpVal val;
} Cexp;

#define CEXP_VAL_APPLY(x)       ((CexpVal){.apply       = (x)})
#define CEXP_VAL_CONDITIONAL(x) ((CexpVal){.conditional = (x)})
#define CEXP_VAL_CALLCC(x)      ((CexpVal){.callCC      = (x)})
#define CEXP_VAL_LETREC(x)      ((CexpVal){.letRec      = (x)})
#define CEXP_VAL_AMB(x)         ((CexpVal){.amb         = (x)})
#define CEXP_VAL_NONE()         ((CexpVal){.none        = NULL})

typedef struct CexpApply {
    struct Aexp *function;
    struct AexpList *args;
} CexpApply;

typedef struct CexpConditional {
    struct Aexp *condition;
    struct Exp *consequent;
    struct Exp *alternative;
} CexpConditional;

typedef struct CexpLetRec {
    struct LetRecBindings *bindings;
    struct Exp *body;
} CexpLetRec;

typedef struct LetRecBindings {
    struct LetRecBindings *next;
    struct AexpVar *var;
    struct Aexp *val;
} LetRecBindings;

typedef struct CexpAmb {
    struct Exp *exp1;
    struct Exp *exp2;
} CexpAmb;

typedef enum {
    EXP_TYPE_AEXP,
    EXP_TYPE_CEXP,
    EXP_TYPE_LET,
    EXP_TYPE_DONE,
} ExpType;

typedef union {
    void *none;
    struct Aexp *aexp;
    struct Cexp *cexp;
    struct ExpLet *let;
} ExpVal;

typedef struct Exp {
    ExpType type;
    ExpVal val;
} Exp;

typedef struct ExpLet {
    struct AexpVar *var;
    struct Exp *val;
    struct Exp *body;
} ExpLet;

#define EXP_VAL_NONE()  ((ExpVal){.none = NULL})
#define EXP_VAL_AEXP(x) ((ExpVal){.aexp = (x)})
#define EXP_VAL_CEXP(x) ((ExpVal){.cexp = (x)})
#define EXP_VAL_LET(x)  ((ExpVal){.let  = (x)})

AexpLam *newAexpLam(AexpVarList *args, Exp *exp);
AexpVarList *newAexpVarList(AexpVarList *next, AexpVar *var);
AexpVar *newAexpVar(char *name);
Aexp *newAexp(AexpType type, AexpVal val);
AexpPrimApp *newAexpPrimApp(AexpPrimOp op, AexpList *args);
AexpList *newAexpList(AexpList *next, Aexp *exp);
Cexp *newCexp(CexpType type, CexpVal val);
CexpApply *newCexpApply(Aexp *function, AexpList *args);
CexpConditional *newCexpConditional(Aexp *condition, Exp *consequent, Exp *alternative);
CexpLetRec *newCexpLetRec(LetRecBindings *bindings, Exp *body);
LetRecBindings *newLetRecBindings(LetRecBindings *next, AexpVar *var, Aexp *val);
CexpAmb *newCexpAmb(Exp *exp1, Exp *exp2);
Exp *newExp(ExpType type, ExpVal val);
ExpLet *newExpLet(AexpVar *var, Exp *val, Exp *body);

#endif
