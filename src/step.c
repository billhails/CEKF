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
static void step();
static void applyProc(Value *proc, ValueList *args);
static void applyKont(Value *value);
static Value *lookUp(AexpVar *var, Env *env);
static ValueList *mapA(AexpList *args, Env *env);
static Env *mapExtend(Env *env, AexpVarList *vars, ValueList *vals);
static Env *extendLetRecVoid(Env *env, LetRecBindings *bindings);
static void mapLetRecReplace(Env *env, LetRecBindings *bindings);
static void cant_happen(const char *message);

static CEKF state;

static void inject(Exp *exp) {
    state.C = exp;
    state.E = NULL;
    state.K = newKont(KONT_TYPE_HALT, KONT_VAL_NONE());
    state.F = newFail(FAIL_TYPE_END, FAIL_VAL_NONE());
    state.V = newValue(VALUE_TYPE_VOID, VALUE_VAL_NONE());
}

void run(Exp *exp) {
    inject(exp);
    printCEKF(&state);
    while (state.C->type != EXP_TYPE_DONE) {
        step();
        printCEKF(&state);
    }
}

static Value *A(Exp *aexp, Env *env) {
    switch (aexp->type) {
        case AEXP_TYPE_LAM: {
            Clo *clo = newClo(aexp->val.aexp.lam, env);
            return newValue( VALUE_TYPE_CLO, VALUE_VAL_CLO(clo));
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

static void step() {
    Exp *C = state.C;
    Env *E = state.E;
    Kont *K = state.K;
    Fail *F = state.F;
    Value *V = state.V;

    switch (C->type) {
        case AEXP_TYPE_LAM:
        case AEXP_TYPE_VAR:
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
        case AEXP_TYPE_PRIM: {
            applyKont(A(C, E));
        }
        break;
        case CEXP_TYPE_APPLY: {
            CexpApply *apply = C->val.cexp.apply;
            Exp *function = apply->function;
            AexpList *args = apply->args;
            Value *proc = A(function, E);
            ValueList *values = mapA(args, E);
            applyProc(proc, values);
        }
        break;
        case CEXP_TYPE_COND: {
            CexpCond *cond = C->val.cexp.cond;
            Exp *condition = cond->condition;
            Value *testResult = A(condition, E);
            if (testResult->type == VALUE_TYPE_FALSE) {
                state.C = cond->alternative;
            } else {
                state.C = cond->consequent;
            }
        }
        break;
        case CEXP_TYPE_CALLCC: {
            Exp *aexp = C->val.cexp.callCC;
            Value *proc = A(aexp, E);
            Value *val = newValue(VALUE_TYPE_CONT, VALUE_VAL_CONT(K));
            ValueList *args = newValueList(NULL, val);
            applyProc(proc, args);
        }
        break;
        case CEXP_TYPE_LETREC: {
            CexpLetRec *letRec = C->val.cexp.letRec;
            LetRecBindings *bindings = letRec->bindings;
            state.C = letRec->body;
            state.E = extendLetRecVoid(E, bindings);
            mapLetRecReplace(state.E, bindings);
        }
        break;
        case CEXP_TYPE_AMB: {
            CexpAmb *amb = C->val.cexp.amb;
            Exp *exp2 = amb->exp2;
            Back *back = newBack(exp2, E, K, F);
            state.C = amb->exp1;
            state.F = newFail( FAIL_TYPE_BACK, FAIL_VAL_BACK(back));
        }
        break;
        case CEXP_TYPE_BACK: {
            if (F->type == FAIL_TYPE_BACK) {
                Back *back = F->val.back;
                state.C = back->exp;
                state.E = back->rho;
                state.K = back->k;
                state.F = back->f;
            } else {
                state.C = newExp(EXP_TYPE_DONE, EXP_VAL_NONE());
            }
        }
        break;
        case EXP_TYPE_LET: {
            ExpLet *let = C->val.let;
            AexpVar *var = let->var;
            Exp *body = let->body;
            state.C = let->val;
            LetK *letK = newLetK(var, body, E, K);
            state.K = newKont( KONT_TYPE_LETK, KONT_VAL_LETK(letK));
        }
        break;
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

static void applyProc(Value *proc, ValueList *vals) {
    if (proc->type != VALUE_TYPE_CLO) {
        cant_happen("expected proc in applyproc");
    }

    Clo *clo = proc->val.clo;
    AexpLam *lam = clo->lam;
    Env *rho = clo->rho;
    AexpVarList *vars = lam->args;
    state.C = lam->exp;
    state.E = mapExtend(rho, vars, vals);
}

static void applyKont(Value *val) {
    switch (state.K->type) {
        case KONT_TYPE_LETK: {
            LetK *letK = state.K->val.letK;
            AexpVar *var = letK->var;
            state.C = letK->body;
            Env *rho = letK->rho;
            state.E = newEnv(rho, var, val);
            state.K = letK->k;
        }
        break;
        case KONT_TYPE_HALT: {
            state.C = newExp(EXP_TYPE_DONE, EXP_VAL_NONE());
            state.V = val;
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
