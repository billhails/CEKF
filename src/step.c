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

#include "common.h"
#include "debug.h"
#include "cekf.h"
#include "step.h"
#include "hash.h"

/**
 * The step function of the CEKF machine.
 */

typedef Value (*primitive)(ValueList *args);

static primitive O(AexpPrimOp op);
static void step();
static void applyProc(Value proc, ValueList *args);
static void applyProcValue(Value proc, Value val);
static void applyKont(Value value);
static Value lookUp(AexpVar *var, Env *env);
static ValueList *mapA(AexpList *args, Env *env);
static Env *mapExtend(Env *env, AexpVarList *vars, ValueList *vals);
static Env *extendLetRecVoid(Env *env);
static void mapLetRecReplace(Env *env, LetRecBindings *bindings);
static void replaceInEnv(Env *env, AexpVar *var, Value val);
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
    state.V = vVoid;
}

void run(Exp *exp) {
    inject(exp);
    printCEKF(&state);
    while (state.C->type != EXP_TYPE_DONE) {
        step();
#ifdef DEBUG_STEP
        printCEKF(&state);
#endif
    }
    printCEKF(&state);
}

static Value A(Exp *aexp, Env *env) {
    switch (aexp->type) {
        case AEXP_TYPE_LAM: {
#ifdef DEBUG_STEP
            printf("A LAM\n");
#endif
            Clo *clo = newClo(aexp->val.aexp.lam, env);
            Value value;
            value .type = VALUE_TYPE_CLO;
            value.val = VALUE_VAL_CLO(clo);
            return value;
        }
        case AEXP_TYPE_VAR: {
#ifdef DEBUG_STEP
            printf("A VAR\n");
#endif
            return lookUp(aexp->val.aexp.var, env);
        }
        case AEXP_TYPE_TRUE: {
#ifdef DEBUG_STEP
            printf("A TRUE\n");
#endif
            return vTrue;
            break;
        }
        case AEXP_TYPE_FALSE: {
#ifdef DEBUG_STEP
            printf("A FALSE\n");
#endif
            return vFalse;
        }
        case AEXP_TYPE_INT: {
#ifdef DEBUG_STEP
            printf("A INT\n");
#endif
            Value value;
            value.type = VALUE_TYPE_INTEGER;
            value.val = VALUE_VAL_INTEGER(aexp->val.aexp.integer);
            return value;
        }
        case AEXP_TYPE_PRIM: {
#ifdef DEBUG_STEP
            printf("A PRIM\n");
#endif
            return (O(aexp->val.aexp.prim->op))(
                mapA(aexp->val.aexp.prim->args, env)
            );
        }
    }
}

static int protectValue(Value v) {
    switch (v.type) {
        case VALUE_TYPE_CLO:
            return PROTECT(v.val.clo);
        case VALUE_TYPE_CONT:
            return PROTECT(v.val.k);
        default:
            return PROTECT(NULL);
    }
}

static void step() {
    Exp *C = state.C;
    Env *E = state.E;
    Kont *K = state.K;
    Fail *F = state.F;
    Value V = state.V;

    switch (C->type) {
        case AEXP_TYPE_LAM:
        case AEXP_TYPE_VAR:
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
        case AEXP_TYPE_PRIM: {
#ifdef DEBUG_STEP
            printf("step AEXP\n");
#endif
            Value val = A(C, E);
            int save = protectValue(val);
            applyKont(val);
            UNPROTECT(save);
        }
        break;
        case CEXP_TYPE_APPLY: {
#ifdef DEBUG_STEP
            printf("step APPLY\n");
#endif
            CexpApply *apply = C->val.cexp.apply;
            Exp *function = apply->function;
            AexpList *args = apply->args;
            Value proc = A(function, E);
            int save = protectValue(proc);
            if (args == NULL) {
                applyProc(proc, NULL);
            } else if (args->next == NULL) {
                Value val = A(args->exp, E);
                protectValue(val);
                applyProcValue(proc, val);
            } else {
                ValueList *values = mapA(args, E);
                PROTECT(values);
                applyProc(proc, values);
            }
            UNPROTECT(save);
        }
        break;
        case CEXP_TYPE_COND: {
#ifdef DEBUG_STEP
            printf("step COND\n");
#endif
            CexpCond *cond = C->val.cexp.cond;
            Exp *condition = cond->condition;
            Value testResult = A(condition, E);
            if (testResult.type == VALUE_TYPE_FALSE) {
                state.C = cond->alternative;
            } else {
                state.C = cond->consequent;
            }
        }
        break;
        case CEXP_TYPE_CALLCC: {
#ifdef DEBUG_STEP
            printf("step CALLCC\n");
#endif
            Exp *aexp = C->val.cexp.callCC;
            Value proc = A(aexp, E);
            int save = protectValue(proc);
            Value cont;
            cont.type = VALUE_TYPE_CONT;
            cont.val = VALUE_VAL_CONT(K);
            protectValue(cont);
            applyProcValue(proc, cont);
            UNPROTECT(save);
        }
        break;
        case CEXP_TYPE_LETREC: {
#ifdef DEBUG_STEP
            printf("step LETREC\n");
#endif
            CexpLetRec *letRec = C->val.cexp.letRec;
            int save = PROTECT(letRec);
            LetRecBindings *bindings = letRec->bindings;
            state.C = letRec->body;
            state.E = extendLetRecVoid(E);
            mapLetRecReplace(state.E, bindings);
            UNPROTECT(save);
        }
        break;
        case CEXP_TYPE_AMB: {
#ifdef DEBUG_STEP
            printf("step AMB\n");
#endif
            CexpAmb *amb = C->val.cexp.amb;
            Exp *exp2 = amb->exp2;
            state.C = amb->exp1;
            state.F = newFail(exp2, E, K, F);
        }
        break;
        case CEXP_TYPE_BACK: {
#ifdef DEBUG_STEP
            printf("step BACK\n");
#endif
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
#ifdef DEBUG_STEP
            printf("step LET\n");
#endif
            ExpLet *let = C->val.let;
            AexpVar *var = let->var;
            Exp *body = let->body;
            state.C = let->val;
            state.K = newKont(var, body, E, K);
        }
        break;
    }
}

static Value intValue(int i) {
    Value value;
    value.type = VALUE_TYPE_INTEGER;
    value.val = VALUE_VAL_INTEGER(i); 
    return value;
}

static Value add(ValueList *list) {
    AexpInteger result = 0;
    if (list != NULL) {
        for (int i = 0; i < list->count; ++i) {
            if (list->values[i].type == VALUE_TYPE_INTEGER) {
                result += list->values[i].val.z;
            }
        }
    }
    return intValue(result);
}

static Value mul(ValueList *list) {
    AexpInteger result = 1;
    if (list != NULL) {
        for (int i = 0; i < list->count; ++i) {
            if (list->values[i].type == VALUE_TYPE_INTEGER) {
                result *= list->values[i].val.z;
            }
        }
    }
    return intValue(result);
}

static Value sub(ValueList *list) {
    AexpInteger result = 0;
    if (list != NULL) {
        if (list->count != 0) {
            result = list->values[0].val.z;
            for (int i = 1; i< list->count; i++) {
                if (list->values[i].type == VALUE_TYPE_INTEGER) {
                    result -= list->values[i].val.z;
                }
            }
        }
    }
    return intValue(result);
}

static Value divide(ValueList *list) {
    AexpInteger result = 1;
    if (list != NULL) {
        if (list->count != 0) {
            result = list->values[0].val.z;
            for (int i = 1; i< list->count; i++) {
                if (list->values[i].type == VALUE_TYPE_INTEGER) {
                    result /= list->values[i].val.z;
                }
            }
        }
    }
    return intValue(result);
}

static bool _eq(ValueList *list) {
    if (list == NULL || list->count < 2) return true;
    for (int i = 0; i < list->count - 1; i++) {
        if (list->values[i].val.z != list->values[i+1].val.z) return false;
    }
    return true;
}

static bool _gt(ValueList *list) {
    if (list == NULL || list->count < 2) return true;
    for (int i = 0; i < list->count - 1; i++) {
        if (list->values[i].val.z <= list->values[i+1].val.z) return false;
    }
    return true;
}

static bool _lt(ValueList *list) {
    if (list == NULL || list->count < 2) return true;
    for (int i = 0; i < list->count - 1; i++) {
        if (list->values[i].val.z >= list->values[i+1].val.z) return false;
    }
    return true;
}

static Value eq(ValueList *list) {
    bool result = _eq(list);
    return result ? vTrue : vFalse;
}

static Value ne(ValueList *list) {
    bool result = _eq(list);
    return result ? vFalse : vTrue;
}

static Value gt(ValueList *list) {
    bool result = _gt(list);
    return result ? vTrue : vFalse;
}

static Value lt(ValueList *list) {
    bool result = _lt(list);
    return result ? vTrue : vFalse;
}

static Value ge(ValueList *list) {
    bool result = _lt(list);
    return result ? vFalse : vTrue;
}

static Value le(ValueList *list) {
    bool result = _gt(list);
    return result ? vFalse : vTrue;
}


static primitive O(AexpPrimOp op) {
    switch (op) {
        case AEXP_PRIM_ADD: return add;
        case AEXP_PRIM_SUB: return sub;
        case AEXP_PRIM_MUL: return mul;
        case AEXP_PRIM_DIV: return divide;
        case AEXP_PRIM_EQ: return eq;
        case AEXP_PRIM_NE: return ne;
        case AEXP_PRIM_GT: return gt;
        case AEXP_PRIM_LT: return lt;
        case AEXP_PRIM_GE: return ge;
        case AEXP_PRIM_LE: return le;
    }
}

static void applyProc(Value proc, ValueList *vals) {
    if (proc.type != VALUE_TYPE_CLO) {
        cant_happen("expected proc in applyproc");
    }

    Clo *clo = proc.val.clo;
    AexpLam *lam = clo->lam;
    Env *rho = clo->rho;
    AexpVarList *vars = lam->args;
    state.C = lam->exp;
    state.E = mapExtend(rho, vars, vals);
}

static void applyProcValue(Value proc, Value val) {
    if (proc.type != VALUE_TYPE_CLO) {
        cant_happen("expected proc in applyproc");
    }

    Clo *clo = proc.val.clo;
    AexpLam *lam = clo->lam;
    Env *rho = clo->rho;
    AexpVarList *vars = lam->args;
    if (vars == NULL) {
        cant_happen("applyProcValue to proc with no args"); // yes it can
    }
    state.C = lam->exp;
    int save = PROTECT(state.E);
    state.E = newEnv(state.E);
    replaceInEnv(state.E, vars->var, val);
    UNPROTECT(save);
}

static void applyKont(Value val) {
    Kont *K = state.K;
    if (K != NULL) {
        AexpVar *var = K->var;
        state.C = K->body;
        Env *rho = K->rho;
        state.E = newEnv(rho);
        replaceInEnv(state.E, var, val);
        state.K = K->next;
    } else {
        state.C = newExp(EXP_TYPE_DONE, EXP_VAL_NONE());
        state.V = val;
    }
}

static Value lookUp(AexpVar *var, Env *env) {
    while (env != NULL) {
        Value value = hashGet(env->table, var);
        if (value.type != VALUE_TYPE_VOID) return value;
        env = env->next;
    }
    cant_happen("no binding for var in env");
}

static int countAexpList(AexpList *list) {
    int count = 0;
    while (list != NULL) {
        count++;
        list = list->next;
    }
    return count;
}

static ValueList *mapA(AexpList *args, Env *env) {
    if (args == NULL) {
        return NULL;
    }

    int count = countAexpList(args);

    ValueList *list = newValueList(count);

    int save = PROTECT(list);

    for (int i = 0; i < count; ++i) {
        list->values[i] = A(args->exp, env);
        args = args->next;
    }

    UNPROTECT(save);
    return list;
}

static Env *mapExtend(Env *env, AexpVarList *vars, ValueList *vals) {
    int save = PROTECT(env);
    env = newEnv(env);
    PROTECT(env);
    if (vals != NULL) {
        for (int i = 0; i < vals->count; i++) {
            if (vars == NULL) break;
            replaceInEnv(env, vars->var, vals->values[i]);
            vars = vars->next;
        }
    }

    UNPROTECT(save);

    return env;
}

static Env *extendLetRecVoid(Env *env) {
    int save = PROTECT(env);
    env = newEnv(env);
    UNPROTECT(save);
    return env;
}

static void replaceInEnv(Env *env, AexpVar *var, Value val) {
    hashSet(env->table, var, val);
}

static void mapLetRecReplace(Env *env, LetRecBindings *bindings) {
    while (bindings != NULL) {
        Value val = A(bindings->val, env);
        int save = protectValue(val);
        replaceInEnv(env, bindings->var, val);
        UNPROTECT(save);
        bindings = bindings->next;
    }
}

static void cant_happen(const char *message) {
    fprintf(stderr, "can't happen: %s\n", message);
    exit(1);
}
