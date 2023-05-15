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
#include <stdarg.h>
#include <signal.h>
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

static Value peek(int index) {
    return peekValue(&state.S, index);
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

static Value car(Value cons) {
    if (cons.type = VALUE_TYPE_CONS) {
        return cons.val.cons->car;
    } else {
        cant_happen("unrecognised type for car %d", cons.type);
    }
}

static Value cdr(Value cons) {
    if (cons.type = VALUE_TYPE_CONS) {
        return cons.val.cons->cdr;
    } else {
        cant_happen("unrecognised type for cdr %d", cons.type);
    }
}

static Value cons(Value a, Value b) {
    Cons *result = newCons(a, b);
    Value v;
    v.type = VALUE_TYPE_CONS;
    v.val = VALUE_VAL_CONS(result);
    return v;
}

static Value lookup(int frame, int offset) {
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

void cant_happen(const char *message, ...) {
    va_list args;

    va_start(args, message);
    vfprintf(stderr, message, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(1);
}

static int protectValue(Value v) {
    switch (v.type) {
        case VALUE_TYPE_CLO:
            return PROTECT(v.val.clo);
        case VALUE_TYPE_CONT:
            return PROTECT(v.val.k);
        case VALUE_TYPE_CONS:
            return PROTECT(v.val.cons);
        default:
            return PROTECT(NULL);
    }
}

/**
 * on reaching this point, the stack will contain a number
 * of arguments, and the callable on top.
 *
 * it pops the callable and invokes it, leaving the rest of the
 * arguments on the stack
 */
static void applyProc() {
    Value callable = pop();
    int save = protectValue(callable);
    switch (callable.type) {
        case VALUE_TYPE_CLO: {
            Clo *clo = callable.val.clo;
            state.C = clo->c;
            state.E = clo->rho;
            setFrame(&state.S, clo->nvar);
        }
        break;
        case VALUE_TYPE_CONT: {
            if (callable.val.k == NULL) {
                state.V = pop();
                state.C = -1;
            } else {
                Value result = pop();
                protectValue(result);
                Kont *k = callable.val.k;
                state.C = k->body;
                state.K = k->next;
                state.E = k->rho;
                clearFrame(&state.S);
                restoreKont(&state.S, k);
                push(result);
            }
        }
        break;
        default:
            cant_happen("unexpected type in APPLY");
    }
    UNPROTECT(save);
}

static void step() {
#ifdef DEBUG_STEP
    int count = 0;
#endif
    while (state.C != -1) {
        switch (state.B.entries[state.C]) {
            case BYTECODE_NONE: {
                cant_happen("encountered NONE in step()");
            }
            break;
            case BYTECODE_LAM: { // create a closure and push it
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LAM [%d] [%d]\n", ++count, state.C, state.B.entries[state.C + 1], offsetAt(state.C + 2));
#endif
                Env *env = NULL;
                Clo *clo = newClo(state.B.entries[state.C + 1], state.C + 4, state.E);
                int save = PROTECT(clo);
                snapshotClo(&state.S, clo);
                Value v;
                v.type = VALUE_TYPE_CLO;
                v.val = VALUE_VAL_CLO(clo);
                push(v);
                UNPROTECT(save);
                state.C = offsetAt(state.C + 2);
            }
            break;
            case BYTECODE_VAR: { // look up an environment variable and push it
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### VAR [%d:%d]\n", ++count, state.C, state.B.entries[state.C + 1], state.B.entries[state.C + 2]);
#endif
                push(lookup(state.B.entries[state.C + 1], state.B.entries[state.C + 2]));
                state.C += 3;
            }
            break;
            case BYTECODE_LVAR: { // look up a stack variable and push it
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LVAR [%d]\n", ++count, state.C, state.B.entries[state.C + 1]);
#endif
                push(peek(state.B.entries[state.C + 1]));
                state.C += 2;
            }
            break;
            case BYTECODE_PUSHN: { // allocate space for n variables on the stack
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### PUSHN [%d]\n", ++count, state.C, state.B.entries[state.C + 1]);
#endif
                pushN(&state.S, state.B.entries[state.C + 1]);
                state.C += 2;
            }
            break;
            case BYTECODE_PRIM_ADD: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### ADD\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(add(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_SUB: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### SUB\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(sub(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_MUL: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### MUL\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(mul(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_DIV: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### DIV\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(divide(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_EQ: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### EQ\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(eq(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_NE: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### NE\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(ne(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_GT: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### GT\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(gt(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_LT: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LT\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(lt(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_GE: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### GE\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(ge(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_LE: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LE\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(le(a, b));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_CAR: { // pop value, perform the op and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### CAR\n", ++count, state.C);
#endif
                Value a = pop();
                push(car(a));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_CDR: { // pop value, perform the op and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### CDR\n", ++count, state.C);
#endif
                Value a = pop();
                push(cdr(a));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_CONS: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### CONS\n", ++count, state.C);
#endif
                Value b = pop();
                int save = protectValue(b);
                Value a = pop();
                protectValue(a);
                Value result = cons(a, b);
                protectValue(result);
                push(result);
                UNPROTECT(save);
                state.C++;
            }
            break;
            case BYTECODE_APPLY: { // apply the callable at the top of the stack to the arguments beneath it
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### APPLY\n", ++count, state.C);
#endif
                applyProc();
            }
            break;
            case BYTECODE_IF: { // pop the test result and jump to the appropriate branch
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### IF [%d]\n", ++count, state.C, offsetAt(state.C + 1));
#endif
                Value aexp = pop();
                if (aexp.type == VALUE_TYPE_FALSE) {
                    state.C = offsetAt(state.C + 1);
                } else {
                    state.C += 3;
                }
            }
            break;
            case BYTECODE_LETREC: { // patch each of the lambdas environments with the current stack frame
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LETREC [%d]\n", ++count, state.C, state.B.entries[state.C + 1]);
#endif
                int nargs = state.B.entries[state.C + 1];
                for (int i = 0; i < nargs; i++) {
                    Value v = peek(i);
                    if (v.type == VALUE_TYPE_CLO) {
                        patchClo(&state.S, v.val.clo);
                    } else {
                        cant_happen("non-lambda value for letrec");
                    }
                }
                state.C += 2;
            }
            break;
            case BYTECODE_AMB: { // create a new failure continuation to resume at the alternative
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### AMB [%d]\n", ++count, state.C, offsetAt(state.C + 1));
#endif
                state.F = newFail(offsetAt(state.C + 1), state.E, state.K, state.F);
                snapshotFail(&state.S, state.F);
                state.C += 3;
            }
            break;
            case BYTECODE_BACK: { // restore the failure continuation or halt
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) # %04d BACK\n", ++count, state.C);
#endif
                if (state.F == NULL) {
                    state.C = -1;
                } else {
                    state.C = state.F->exp;
                    state.E = state.F->rho;
                    state.K = state.F->k;
                    restoreFail(&state.S, state.F);
                    state.F = state.F->next;
                }
            }
            break;
            case BYTECODE_LET: { // create a new continuation to resume the body, and transfer control to the expression
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LET [%d]\n", ++count, state.C, offsetAt(state.C + 1));
#endif
                state.K = newKont(offsetAt(state.C + 1), state.E, state.K);
                snapshotKont(&state.S, state.K);
                state.C += 3;
            }
            break;
            case BYTECODE_JMP: { // jump forward a specified amount
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### JMP [%d]\n", ++count, state.C, offsetAt(state.C + 1));
#endif
                state.C = offsetAt(state.C + 1);
            }
            break;
            case BYTECODE_CALLCC: { // pop the callable, push the current continuation, push the callable and apply
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### CALLCC\n", ++count, state.C);
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
            case BYTECODE_TRUE: { // push true
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### TRUE\n", ++count, state.C);
#endif
                push(vTrue);
                state.C++;
            }
            break;
            case BYTECODE_FALSE: { // push false
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### FALSE\n", ++count, state.C);
#endif
                push(vFalse);
                state.C++;
            }
            break;
            case BYTECODE_VOID: { // push void
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### VOID\n", ++count, state.C);
#endif
                push(vVoid);
                state.C++;
            }
            break;
            case BYTECODE_INT: { // push literal int
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### JMP [%d]\n", ++count, state.C, intAt(state.C + 1));
#endif
                Value v;
                v.type = VALUE_TYPE_INTEGER;
                v.val = VALUE_VAL_INTEGER(intAt(state.C + 1));
                push(v);
                state.C += 5;
            }
            break;
            case BYTECODE_RETURN: { // push the current continuation and apply
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### RETURN\n", ++count, state.C);
#endif
                Value k;
                k.type = VALUE_TYPE_CONT;
                k.val = VALUE_VAL_CONT(state.K);
                push(k);
                applyProc();
            }
            break;
            case BYTECODE_DONE: { // can't happen, probably
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) # %04d DONE\n", ++count, state.C);
#endif
                state.C = -1;
            }
            break;
            default:
#ifdef DEBUG_STEP
                printCEKF(&state);
#endif
                cant_happen("unrecognised bytecode in step()");
        }
#ifdef DEBUG_STEP
        sleep(1);
#endif
    }
}
