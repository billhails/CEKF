#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cekf.h"
#include "step.h"

/**
 * The step function of the CEKF machine.
 */

typedef Value *(*primitive)(ValueList *args);

static primitive O(AexpPrimOp op);
static CEKF *step(CEKF *state);
static CEKF *applyProc(Value *proc, ValueList *args, Kont *k, Fail *f);
static CEKF *applyKont(Kont *kont, Value *value, Fail *f);
static Value *lookUp(AexpVar *var, Env *env);
static ValueList *mapA(AexpList *args, Env *env);
static Env *mapExtend(Env *env, AexpVarList *vars, ValueList *vals);
static Env *extendLetRecVoid(Env *env, LetRecBindings *bindings);
static void mapLetRecReplace(Env *env, LetRecBindings *bindings);
static void cant_happen(const char *message);

static CEKF *inject(Exp *exp) {
    return newCEKF(
        exp,
        NULL,
        newKont(KONT_TYPE_HALT, KONT_VAL_NONE()),
        newFail(FAIL_TYPE_END, FAIL_VAL_NONE())
    );
}

void run(Exp *exp) {
    CEKF *state = inject(exp);
    while (state->C->type != EXP_TYPE_DONE) {
        state = step(state);
    }
}

static Value *A(Aexp *aexp, Env *env) {
    switch (aexp->type) {
        case AEXP_TYPE_LAM: {
            return newValue(
                VALUE_TYPE_CLO,
                VALUE_VAL_CLO(newClo(aexp->val.lam, env))
            );
        }
        case AEXP_TYPE_VAR: {
            return lookUp(aexp->val.var, env);
        }
        case AEXP_TYPE_TRUE: {
            return newValue(VALUE_TYPE_TRUE, VALUE_VAL_NONE());
            break;
        }
        case AEXP_TYPE_FALSE: {
            return newValue(VALUE_TYPE_FALSE, VALUE_VAL_NONE());
        }
        case AEXP_TYPE_INT: {
            return newValue(
                VALUE_TYPE_INTEGER,
                VALUE_VAL_INTEGER(aexp->val.integer)
            );
        }
        case AEXP_TYPE_PRIM: {
            return (O(aexp->val.prim->op))(mapA(aexp->val.prim->args, env));
        }
    }
}

static CEKF *step(CEKF *state) {
    Exp *C = state->C;
    Env *E = state->E;
    Kont *K = state->K;
    Fail *F = state->F;

    switch (C->type) {
        case EXP_TYPE_AEXP: {
            return applyKont(K, A(C->val.aexp, E), F);
        }
        case EXP_TYPE_CEXP: {
            Cexp *cexp = C->val.cexp;
            switch (cexp->type) {
                case CEXP_TYPE_APPLY: {
                    CexpApply *apply = cexp->val.apply;
                    Aexp *function = apply->function;
                    AexpList *args = apply->args;
                    Value *proc = A(function, E);
                    ValueList *values = mapA(args, E);
                    return applyProc(proc, values, K, F);
                }
                case CEXP_TYPE_CONDITIONAL: {
                    CexpConditional *conditional = cexp->val.conditional;
                    Aexp *condition = conditional->condition;
                    Exp *consequence;
                    Value *testResult = A(condition, E);
                    if (testResult->type == VALUE_TYPE_FALSE) {
                        consequence = conditional->alternative;
                    } else {
                        consequence = conditional->consequent;
                    }
                    return newCEKF(consequence, E, K, F);
                }
                case CEXP_TYPE_CALLCC: {
                    Aexp *aexp = cexp->val.callCC;
                    Value *proc = A(aexp, E);
                    ValueList *args = newValueList(
                        NULL,
                        newValue(VALUE_TYPE_CONT, VALUE_VAL_CONT(K))
                    );
                    return applyProc(proc, args, K, F);
                }
                case CEXP_TYPE_LETREC: {
                    CexpLetRec *letRec = cexp->val.letRec;
                    LetRecBindings *bindings = letRec->bindings;
                    Exp *body = letRec->body;
                    Env *rho = extendLetRecVoid(E, bindings);
                    mapLetRecReplace(rho, bindings);
                    return newCEKF(body, rho, K, F);
                }
                case CEXP_TYPE_AMB: {
                    CexpAmb *amb = cexp->val.amb;
                    Exp *exp1 = amb->exp1;
                    Exp *exp2 = amb->exp2;
                    BackTrack *backTrack = newBackTrack(exp2, E, K, F);
                    return newCEKF(
                        exp1,
                        E,
                        K,
                        newFail(
                            FAIL_TYPE_BACKTRACK,
                            FAIL_VAL_BACKTRACK(backTrack)
                        )
                    );
                }
                case CEXP_TYPE_BACK: {
                    if (F->type == FAIL_TYPE_BACKTRACK) {
                        BackTrack *backTrack = F->val.backTrack;
                        Exp *exp = backTrack->exp;
                        Env *rho = backTrack->rho;
                        Kont *k = backTrack->k;
                        Fail *f = backTrack->f;
                        return newCEKF(exp, rho, k, f);
                    } else {
                        Exp *exp = newExp(EXP_TYPE_DONE, EXP_VAL_NONE());
                        return newCEKF(exp, E, K, F);
                    }
                }
            }
        }
        case EXP_TYPE_LET: {
            ExpLet *let = C->val.let;
            AexpVar *var = let->var;
            Exp *val = let->val;
            Exp *body = let->body;
            Kont *k = newKont(
                KONT_TYPE_LETK,
                KONT_VAL_LETK(newLetK(var, body, E, K))
            );
            return newCEKF(val, E, k, F);
        }
    }
}

static Value *add(ValueList *list) {
    AexpInteger result = 0;
    while (list != NULL) {
        if (list->value->type == VALUE_TYPE_INTEGER) {
            result += list->value->val.z;
        }
        list = list->next;
    }
    return newValue(VALUE_TYPE_INTEGER, VALUE_VAL_INTEGER(result)); 
}

static Value *mul(ValueList *list) {
    AexpInteger result = 1;
    while (list != NULL) {
        if (list->value->type == VALUE_TYPE_INTEGER) {
            result *= list->value->val.z;
        }
        list = list->next;
    }
    return newValue(VALUE_TYPE_INTEGER, VALUE_VAL_INTEGER(result)); 
}

static Value *sub(ValueList *list) {
    AexpInteger result = 0;
    if (list != NULL) {
        result = list->value->val.z;
        list = list->next;
        while (list != NULL) {
            if (list->value->type == VALUE_TYPE_INTEGER) {
                result -= list->value->val.z;
            }
            list = list->next;
        }
    }
    return newValue(VALUE_TYPE_INTEGER, VALUE_VAL_INTEGER(result)); 
}

static Value *divide(ValueList *list) {
    AexpInteger result = 1;
    if (list != NULL) {
        result = list->value->val.z;
        list = list->next;
        while (list != NULL) {
            if (list->value->type == VALUE_TYPE_INTEGER) {
                result /= list->value->val.z;
            }
            list = list->next;
        }
    }
    return newValue(VALUE_TYPE_INTEGER, VALUE_VAL_INTEGER(result)); 
}

static primitive O(AexpPrimOp op) {
    switch (op) {
        case AEXP_PRIM_ADD: return add;
        case AEXP_PRIM_SUB: return sub;
        case AEXP_PRIM_MUL: return mul;
        case AEXP_PRIM_DIV: return divide;
    }
}

static CEKF *applyProc(Value *proc, ValueList *vals, Kont *k, Fail *f) {
    if (proc->type != VALUE_TYPE_CLO) {
        cant_happen("expected proc in applyproc");
    }

    Clo *clo = proc->val.clo;
    AexpLam *lam = clo->lam;
    Env *rho = clo->rho;
    AexpVarList *vars = lam->args;
    Exp *body = lam->exp;
    Env *newRho = mapExtend(rho, vars, vals);
    return newCEKF(body, newRho, k, f);
}

static CEKF *applyKont(Kont *k, Value *val, Fail *f) {
    switch (k->type) {
        case KONT_TYPE_LETK: {
            LetK *letK = k->val.letK;
            AexpVar *var = letK->var;
            Exp *body = letK->body;
            Env *rho = letK->rho;
            Kont *kappa = letK->k;
            Env *newRho = newEnv(rho, var, val);
            return newCEKF(body, newRho, kappa, f);
        }
        case KONT_TYPE_HALT: {
            Exp *exp = newExp(EXP_TYPE_DONE, EXP_VAL_NONE());
            return newCEKF(exp, NULL, k, f);
        }
    }
}

static Value *lookUp(AexpVar *var, Env *env) {
    while (env == NULL) {
        if (strcmp(var->name, env->var->name) == 0) {
            return env->val;
        }
        env = env->next;
    }
    cant_happen("no binding for var in env");
}

static ValueList *mapA(AexpList *args, Env *env) {
    if (args == NULL) {
        return NULL;
    }

    ValueList *list = mapA(args->next, env);
    Value *val = A(args->exp, env);

    return newValueList(list, val);
}

static Env *mapExtend(Env *env, AexpVarList *vars, ValueList *vals) {
    while (vars != NULL && vals != NULL) {
        env = newEnv(env, vars->var, vals->value);
        vars = vars->next;
        vals = vals->next;
    }

    return env;
}

static Env *extendLetRecVoid(Env *env, LetRecBindings *bindings) {
    if (bindings == NULL) {
        return env;
    }

    Env *next = extendLetRecVoid(env, bindings->next);
    Value *val = newValue(VALUE_TYPE_VOID, VALUE_VAL_NONE());

    return newEnv(next, bindings->var, val);
}

static void replaceInEnv(Env *env, AexpVar *var, Value *val) {
    while (env != NULL) {
        if (strcmp(env->var->name, var->name) == 0) {
            env->val = val;
            return;
        }
        env = env->next;
    }
    cant_happen("no binding for var in replaceInEnv");
}

static void mapLetRecReplace(Env *env, LetRecBindings *bindings) {
    while (bindings != NULL) {
        Value *val = A(bindings->val, env);
        replaceInEnv(env, bindings->var, val);
        bindings = bindings->next;
    }
}

static void cant_happen(const char *message) {
    fprintf(stderr, "can't happen: %s\n", message);
    exit(1);
}
