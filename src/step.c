#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "cekf.h"
#include "step.h"

/**
 * The step function of the CEKF machine.
 */

typedef Value *(*primitive)(ValueList *args);

static primitive O(AexpPrimOp op);
static CEKF *step(CEKF *state);
static CEKF *applyProc(Value *proc, ValueList *args, Kont *k, Fail *f, Value *v);
static CEKF *applyKont(Kont *kont, Value *value, Fail *f, Value *v);
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
        newFail(FAIL_TYPE_END, FAIL_VAL_NONE()),
        newValue(VALUE_TYPE_VOID, VALUE_VAL_NONE())
    );
}

void run(Exp *exp) {
    CEKF *state = inject(exp);
    printCEKF(state);
    while (state->C->type != EXP_TYPE_DONE) {
        state = step(state);
        printCEKF(state);
    }
}

static Value *A(Exp *aexp, Env *env) {
    switch (aexp->type) {
        case AEXP_TYPE_LAM: {
            return newValue(
                VALUE_TYPE_CLO,
                VALUE_VAL_CLO(newClo(aexp->val.aexp.lam, env))
            );
        }
        case AEXP_TYPE_VAR: {
            return lookUp(aexp->val.aexp.var, env);
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
                VALUE_VAL_INTEGER(aexp->val.aexp.integer)
            );
        }
        case AEXP_TYPE_PRIM: {
            return (O(aexp->val.aexp.prim->op))(
                mapA(aexp->val.aexp.prim->args, env)
            );
        }
    }
}

static CEKF *step(CEKF *state) {
    Exp *C = state->C;
    Env *E = state->E;
    Kont *K = state->K;
    Fail *F = state->F;
    Value *V = state->V;

    switch (C->type) {
        case AEXP_TYPE_LAM:
        case AEXP_TYPE_VAR:
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
        case AEXP_TYPE_PRIM: {
            return applyKont(K, A(C, E), F, V);
        }
        case CEXP_TYPE_APPLY: {
            CexpApply *apply = C->val.cexp.apply;
            Exp *function = apply->function;
            AexpList *args = apply->args;
            Value *proc = A(function, E);
            ValueList *values = mapA(args, E);
            return applyProc(proc, values, K, F, V);
        }
        case CEXP_TYPE_CONDITIONAL: {
            CexpConditional *conditional = C->val.cexp.conditional;
            Exp *condition = conditional->condition;
            Exp *consequence;
            Value *testResult = A(condition, E);
            if (testResult->type == VALUE_TYPE_FALSE) {
                consequence = conditional->alternative;
            } else {
                consequence = conditional->consequent;
            }
            return newCEKF(consequence, E, K, F, V);
        }
        case CEXP_TYPE_CALLCC: {
            Exp *aexp = C->val.cexp.callCC;
            Value *proc = A(aexp, E);
            ValueList *args = newValueList(
                NULL,
                newValue(VALUE_TYPE_CONT, VALUE_VAL_CONT(K))
            );
            return applyProc(proc, args, K, F, V);
        }
        case CEXP_TYPE_LETREC: {
            CexpLetRec *letRec = C->val.cexp.letRec;
            LetRecBindings *bindings = letRec->bindings;
            Exp *body = letRec->body;
            Env *rho = extendLetRecVoid(E, bindings);
            mapLetRecReplace(rho, bindings);
            return newCEKF(body, rho, K, F, V);
        }
        case CEXP_TYPE_AMB: {
            CexpAmb *amb = C->val.cexp.amb;
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
                ),
                V
            );
        }
        case CEXP_TYPE_BACK: {
            if (F->type == FAIL_TYPE_BACKTRACK) {
                BackTrack *backTrack = F->val.backTrack;
                Exp *exp = backTrack->exp;
                Env *rho = backTrack->rho;
                Kont *k = backTrack->k;
                Fail *f = backTrack->f;
                return newCEKF(exp, rho, k, f, V);
            } else {
                Exp *exp = newExp(EXP_TYPE_DONE, EXP_VAL_NONE());
                return newCEKF(exp, E, K, F, V);
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
            return newCEKF(val, E, k, F, V);
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

static CEKF *applyProc(Value *proc, ValueList *vals, Kont *k, Fail *f, Value *v) {
    if (proc->type != VALUE_TYPE_CLO) {
        cant_happen("expected proc in applyproc");
    }

    Clo *clo = proc->val.clo;
    AexpLam *lam = clo->lam;
    Env *rho = clo->rho;
    AexpVarList *vars = lam->args;
    Exp *body = lam->exp;
    Env *newRho = mapExtend(rho, vars, vals);
    return newCEKF(body, newRho, k, f, v);
}

static CEKF *applyKont(Kont *k, Value *val, Fail *f, Value *v) {
    switch (k->type) {
        case KONT_TYPE_LETK: {
            LetK *letK = k->val.letK;
            AexpVar *var = letK->var;
            Exp *body = letK->body;
            Env *rho = letK->rho;
            Kont *kappa = letK->k;
            Env *newRho = newEnv(rho, var, val);
            return newCEKF(body, newRho, kappa, f, v);
        }
        case KONT_TYPE_HALT: {
            Exp *exp = newExp(EXP_TYPE_DONE, EXP_VAL_NONE());
            return newCEKF(exp, NULL, k, f, val);
        }
    }
}

static Value *lookUp(AexpVar *var, Env *env) {
    while (env != NULL) {
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
