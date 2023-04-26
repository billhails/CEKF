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

void markCEKF() {
    markExp(state.C);
    markEnv(state.E);
    markKont(state.K);
    markFail(state.F);
    markValue(state.V);
}

static void inject(Exp *exp) {
    state.C = exp;
    state.E = NULL;
    state.K = NULL;
    state.F = NULL;
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
            state.C = amb->exp1;
            state.F = newFail(exp2, E, K, F);
        }
        break;
        case CEXP_TYPE_BACK: {
            if (F != NULL) {
                state.C = F->exp;
                state.E = F->rho;
                state.K = F->k;
                state.F = F->next;
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
            state.K = newKont(var, body, E, K);
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
    Kont *K = state.K;
    if (K != NULL) {
        AexpVar *var = K->var;
        state.C = K->body;
        Env *rho = K->rho;
        state.E = newEnv(rho, var, val);
        state.K = K->next;
    } else {
        state.C = newExp(EXP_TYPE_DONE, EXP_VAL_NONE());
        state.V = val;
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
