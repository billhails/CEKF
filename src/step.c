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
#include <unistd.h>

#include "common.h"
#include "debug.h"
#include "cekf.h"
#include "step.h"
#include "hash.h"

/**
 * The step function of the CEKF machine.
 */

typedef Value (*primitive)(Value arg1, Value arg2);

static void step();
static Value lookup(int frame, int offset);
static Env *extendVoid(Env *env, int count);
static void replaceInEnv(Env *env, int index, Value val);

static CEKF state;

void markCEKF() {
    markEnv(state.E);
    markKont(state.K);
    markFail(state.F);
    markValue(state.V);
    markStack(&state.S);
}

static void push(Value v) {
    pushValue(&state.S, v);
}

static Value pop() {
    return popValue(&state.S);
}

static void inject(ByteCodeArray B) {
    state.C = 0;
    state.E = NULL;
    state.K = NULL;
    state.F = NULL;
    state.V = vVoid;
    initStack(&state.S);
    state.B = B;
}

void run(ByteCodeArray B) {
    inject(B);
    step();
    printCEKF(&state);
}

static int valueAt(int index) {
    return (state.B.entries[index] << 8) + state.B.entries[index + 1];
}

static int intAt(int index) {
    return
        (state.B.entries[index] << 24) +
        (state.B.entries[index + 1] << 16) +
        (state.B.entries[index + 2] << 8) +
        state.B.entries[index + 3];
}

static int offsetAt(int index) {
    return index + valueAt(index);
}

static Value intValue(int i) {
    Value value;
    value.type = VALUE_TYPE_INTEGER;
    value.val = VALUE_VAL_INTEGER(i); 
    return value;
}

static Value add(Value a, Value b) {
    AexpInteger result = a.val.z + b.val.z;
    return intValue(result);
}

static Value mul(Value a, Value b) {
    AexpInteger result = a.val.z * b.val.z;
    return intValue(result);
}

static Value sub(Value a, Value b) {
    AexpInteger result = a.val.z - b.val.z;
    return intValue(result);
}

static Value divide(Value a, Value b) {
    AexpInteger result = a.val.z / b.val.z;
    return intValue(result);
}

static bool _eq(Value a, Value b) {
    return a.val.z == b.val.z;
}

static bool _gt(Value a, Value b) {
    return a.val.z > b.val.z;
}

static bool _lt(Value a, Value b) {
    return a.val.z < b.val.z;
}

static Value eq(Value a, Value b) {
    bool result = _eq(a, b);
    return result ? vTrue : vFalse;
}

static Value ne(Value a, Value b) {
    bool result = _eq(a, b);
    return result ? vFalse : vTrue;
}

static Value gt(Value a, Value b) {
    bool result = _gt(a, b);
    return result ? vTrue : vFalse;
}

static Value lt(Value a, Value b) {
    bool result = _lt(a, b);
    return result ? vTrue : vFalse;
}

static Value ge(Value a, Value b) {
    bool result = _lt(a, b);
    return result ? vFalse : vTrue;
}

static Value le(Value a, Value b) {
    bool result = _gt(a, b);
    return result ? vFalse : vTrue;
}


static Value lookup(int frame, int offset) {
#ifdef DEBUG_STEP
            printf("* lookup [%d:%d] in ", frame, offset);
            printEnv(state.E);
            printf("\n");
#endif
    Env *env = state.E;
    while (frame > 0) {
        env = env->next;
        frame--;
    }
    return env->values[offset];
}

static Env *extendVoid(Env *env, int count) {
    int save = PROTECT(env);
    env = newEnv(env, count);
    UNPROTECT(save);
    return env;
}

static void replaceInEnv(Env *env, int index, Value val) {
    env->values[index] = val;
}

void cant_happen(const char *message) {
    fprintf(stderr, "can't happen: %s\n", message);
    exit(1);
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

static void applyProc() {
    Value aexp = pop();
    switch (aexp.type) {
        case VALUE_TYPE_CLO: {
            Clo *clo = aexp.val.clo;
            int save = PROTECT(clo);
            int nvar = clo->nvar;
            if (clo->nvar != 1) {
                cant_happen("wrong number of arguments in applyProc receiver");
            }
            state.E = extendVoid(clo->rho, nvar);
            for (; nvar > 0; nvar--) {
                replaceInEnv(state.E, nvar - 1, pop());
            }
            state.C = clo->c;
            UNPROTECT(save);
        }
        break;
        case VALUE_TYPE_CONT: {
            if (aexp.val.k == NULL) {
                state.V = pop();
                state.C = -1;
            } else {
                Kont *k = aexp.val.k;
                int save = PROTECT(k);
                state.E = extendVoid(k->rho, 1);
                replaceInEnv(state.E, 0, pop());
                state.C = k->body;
                state.K = k->next;
                UNPROTECT(save);
            }
        }
        break;
        default:
            cant_happen("unexpected type in APPLY");
    }
}

static void step() {
    while (state.C != -1) {
        switch (state.B.entries[state.C]) {
            case BYTECODE_NONE: {
                cant_happen("encountered NONE in step2()");
            }
            break;
            case BYTECODE_LAM: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d LAM\n", state.C);
#endif
                Clo *clo = newClo(state.B.entries[state.C + 1], state.C + 4, state.E);
                int save = PROTECT(clo);
                Value v;
                v.type = VALUE_TYPE_CLO;
                v.val = VALUE_VAL_CLO(clo);
                push(v);
                UNPROTECT(save);
                state.C = offsetAt(state.C + 2);
            }
            break;
            case BYTECODE_VAR: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d VAR\n", state.C);
#endif
                push(lookup(state.B.entries[state.C + 1], state.B.entries[state.C + 2]));
                state.C += 3;
            }
            break;
            case BYTECODE_PRIM_ADD: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d ADD\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(add(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_SUB: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d SUB\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(sub(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_MUL: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d MUL\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(mul(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_DIV: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d DIV\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(divide(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_EQ: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d EQ\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(eq(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_NE: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d NE\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(ne(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_GT: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d GT\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(gt(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_LT: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d LT\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(lt(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_GE: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d GE\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(ge(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_LE: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d LE\n", state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(le(a, b));
                state.C++;
            }
            break;
            case BYTECODE_APPLY: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d APPLY\n", state.C);
#endif
                applyProc();
            }
            break;
            case BYTECODE_IF: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d IF\n", state.C);
#endif
                Value aexp = pop();
                if (aexp.type == VALUE_TYPE_FALSE) {
                    state.C = offsetAt(state.C + 1);
                } else {
                    state.C += 3;
                }
            }
            break;
            case BYTECODE_ENV: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d ENV\n", state.C);
#endif
                state.E = extendVoid(state.E, state.B.entries[state.C + 1]);
                state.C += 2;
            }
            break;
            case BYTECODE_LETREC: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d LETREC\n", state.C);
#endif
                int nargs = state.B.entries[state.C + 1];
                for (; nargs > 0; nargs--) {
                    replaceInEnv(state.E, nargs - 1, pop());
                }
                state.C += 2;
            }
            break;
            case BYTECODE_AMB: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d AMB\n", state.C);
#endif
                state.F = newFail(offsetAt(state.C + 1), state.E, state.K, state.F);
                state.C += 3;
            }
            break;
            case BYTECODE_BACK: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d BACK\n", state.C);
#endif
                if (state.F == NULL) {
                    state.C = -1;
                } else {
                    state.C = state.F->exp;
                    state.E = state.F->rho;
                    state.K = state.F->k;
                    state.F = state.F->next;
                }
            }
            break;
            case BYTECODE_LET: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d LET\n", state.C);
#endif
                state.K = newKont(offsetAt(state.C + 1), state.E, state.K);
                state.C += 3;
            }
            break;
            case BYTECODE_JMP: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d JMP\n", state.C);
#endif
                state.C = offsetAt(state.C + 1);
            }
            break;
            case BYTECODE_CALLCC: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d CALLCC\n", state.C);
#endif
                Value aexp = pop();
                int save = protectValue(aexp);
                Value cc;
                cc.type = VALUE_TYPE_CONT;
                cc.val = VALUE_VAL_CONT(state.K);
                push(cc);
                push(aexp);
                UNPROTECT(save);
                applyProc();
            }
            break;
            case BYTECODE_TRUE: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d TRUE\n", state.C);
#endif
                push(vTrue);
                state.C++;
            }
            break;
            case BYTECODE_FALSE: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d FALSE\n", state.C);
#endif
                push(vFalse);
                state.C++;
            }
            break;
            case BYTECODE_INT: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d INT\n", state.C);
#endif
                Value v;
                v.type = VALUE_TYPE_INTEGER;
                v.val = VALUE_VAL_INTEGER(intAt(state.C + 1));
                push(v);
                state.C += 5;
            }
            break;
            case BYTECODE_RETURN: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d RET\n", state.C);
#endif
                if (state.K == NULL) {
                    state.V = pop();
                    state.C = -1;
                } else {
                    state.E = extendVoid(state.K->rho, 1);
                    replaceInEnv(state.E, 0, pop());
                    state.C = state.K->body;
                    state.K = state.K->next;
                }
            }
            break;
            case BYTECODE_DONE: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("# %04d DONE\n", state.C);
#endif
                state.C = -1;
            }
            break;
            default:
#ifdef DEBUG_STEP
                printCEKF(&state);
#endif
                cant_happen("unrecognised bytecode in dumpByteCode");
        }
    }
}
