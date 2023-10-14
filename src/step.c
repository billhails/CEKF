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

static void step();
static Value lookup(int frame, int offset);

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

static void popn(int n) {
    popN(&state.S, n);
}

static Value peek(int index) {
    return peekValue(&state.S, index);
}

static void copyToVec(Vec *v) {
    copyTopToValues(&state.S, &(v->values[0]), v->size);
}

static void inject(ByteCodeArray B) {
    static bool first = true;
    state.C = 0;
    state.E = NULL;
    state.K = NULL;
    state.F = NULL;
    state.V = vVoid;
    if (first) {
        initStack(&state.S);
    } else {
        setFrame(&state.S, 0);
    }
    state.B = B;
    first = false;
}

Value run(ByteCodeArray B) {
    inject(B);
    step();
    return state.V;
}

static int byteAt(int i) {
    int index = state.C + i;
    return state.B.entries[index];
}

static int wordAt(int i) {
    return (byteAt(i) << 8) + byteAt(i + 1);
}

static int intAt(int i) {
    return
        (byteAt(i) << 24) +
        (byteAt(i + 1) << 16) +
        (byteAt(i + 2) << 8) +
        byteAt(i + 3);
}

static int offsetAt(int i) {
    int index = state.C + i;
    return index + wordAt(i);
}

static Value intValue(int i) {
    Value value;
    value.type = VALUE_TYPE_INTEGER;
    value.val = VALUE_VAL_INTEGER(i); 
    return value;
}

static bool truthy(Value v) {
    return v.type != VALUE_TYPE_FALSE && v.type != VALUE_TYPE_VOID;
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

static bool _xor(Value a, Value b) {
    return truthy(a) ? !truthy(b) : truthy(b);
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

static Value xor(Value a, Value b) {
    return _xor(a, b) ? vTrue : vFalse;
}

static Value le(Value a, Value b) {
    bool result = _gt(a, b);
    return result ? vFalse : vTrue;
}

static Value not(Value a) {
    return truthy(a) ? vFalse : vTrue;
}

static Value car(Value cons) {
    if (cons.type == VALUE_TYPE_CONS) {
        return cons.val.cons->car;
    } else {
        cant_happen("unrecognised type for car %d", cons.type);
    }
}

static Value cdr(Value cons) {
    if (cons.type == VALUE_TYPE_CONS) {
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

static Value vec(Value index, Value vector) {
    if (index.type != VALUE_TYPE_INTEGER)
        cant_happen("invalid index type for vec %d", index.type);
    if (vector.type != VALUE_TYPE_VEC)
        cant_happen("invalid vector type for vec %d", vector.type);
    int i = index.val.z;
    Vec *v = vector.val.vec;
    if (i < 0 || i >= v->size)
        cant_happen("index out of range 0 - %d for vec (%d)", v->size, i);
    return v->values[i];
}

static Value lookup(int frame, int offset) {
    Env *env = state.E;
    while (frame > 0) {
        env = env->next;
        frame--;
    }
    return env->values[offset];
}

static int protectValue(Value v) {
    switch (v.type) {
        case VALUE_TYPE_CLO:
            return PROTECT(v.val.clo);
        case VALUE_TYPE_CONT:
            return PROTECT(v.val.k);
        case VALUE_TYPE_CONS:
            return PROTECT(v.val.cons);
        case VALUE_TYPE_VEC:
            return PROTECT(v.val.vec);
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

// #define printCEKF(state)

static void step() {
#ifdef DEBUG_STEP
    int count = 0;
#endif
    while (state.C != -1) {
        switch (byteAt(0)) {
            case BYTECODE_NONE: {
                cant_happen("encountered NONE in step()");
            }
            break;
            case BYTECODE_LAM: { // create a closure and push it
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LAM [%d] [%d] [%d]\n", ++count, state.C, byteAt(1), byteAt(2), offsetAt(3));
#endif
                Env *env = NULL;
                Clo *clo = newClo(byteAt(1), state.C + 5, state.E);
                int letRecOffset = byteAt(2);
                int save = PROTECT(clo);
                snapshotClo(&state.S, clo, letRecOffset);
                Value v;
                v.type = VALUE_TYPE_CLO;
                v.val = VALUE_VAL_CLO(clo);
                push(v);
                UNPROTECT(save);
                state.C = offsetAt(3);
            }
            break;
            case BYTECODE_VAR: { // look up an environment variable and push it
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### VAR [%d:%d]\n", ++count, state.C, byteAt(1), byteAt(2));
#endif
                push(lookup(byteAt(1), byteAt(2)));
                state.C += 3;
            }
            break;
            case BYTECODE_LVAR: { // look up a stack variable and push it
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LVAR [%d]\n", ++count, state.C, byteAt(1));
#endif
                push(peek(byteAt(1)));
                state.C += 2;
            }
            break;
            case BYTECODE_PUSHN: { // allocate space for n variables on the stack
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### PUSHN [%d]\n", ++count, state.C, byteAt(1));
#endif
                pushN(&state.S, byteAt(1));
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
            case BYTECODE_PRIM_XOR: { // pop two values, perform the binop and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### XOR\n", ++count, state.C);
#endif
                Value b = pop();
                Value a = pop();
                push(xor(a, b));
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
            case BYTECODE_PRIM_NOT: { // pop value, perform the op and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### NOT\n", ++count, state.C);
#endif
                Value a = pop();
                push(not(a));
                state.C++;
            }
            break;
            case BYTECODE_PRIM_PRINT: { // pop value, perform the op and push the result
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### PRINT\n", ++count, state.C);
#endif
                Value a = pop();
                push(a);
                printContainedValue(a, 0);
                printf("\n");
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
            case BYTECODE_PRIM_VEC: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### VEC\n", ++count, state.C);
#endif
                Value b = pop();
                int save = protectValue(b);
                Value a = pop();
                protectValue(a);
                Value result = vec(a, b);
                protectValue(result);
                push(result);
                UNPROTECT(save);
                state.C++;
            }
            break;
            case BYTECODE_PRIM_MAKEVEC: {
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### MAKEVEC [%d]\n", ++count, state.C, byteAt(1));
#endif
                int size = byteAt(1);
                // at this point there will be `size` arguments on the stack. Rather than
                // popping then individually we can just memcpy them into a new struct Vec
                Vec *v = newVec(size);
                int save = PROTECT(v);
                copyToVec(v);
                popn(size);
                Value val;
                val.type = VALUE_TYPE_VEC;
                val.val = VALUE_VAL_VEC(v);
                push(val);
                UNPROTECT(save);
                state.C += 2;
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
                printf("%4d) %04d ### IF [%d]\n", ++count, state.C, offsetAt(1));
#endif
                Value aexp = pop();
                if (truthy(aexp)) {
                    state.C += 3;
                } else {
                    state.C = offsetAt(1);
                }
            }
            break;
            case BYTECODE_MATCH: { // pop the dispach code, verify it's an integer and in range, and dispatch
                int size = byteAt(1);
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### MATCH [%d]", ++count, state.C, byteAt(1));
                for (int c = 0; c < size; c++) {
                    printf("[%d]", offsetAt(2 + c * 2));
                }
                printf("\n");
#endif
                Value v = pop();
                if (v.type != VALUE_TYPE_INTEGER)
                    cant_happen("match expression must be an integer");
                if (v.val.z < 0 || v.val.z >= size)
                    cant_happen("match expression index out of range (%d)", v.val.z);
                state.C = offsetAt(2 + v.val.z * 2);
            }
            break;
            case BYTECODE_LETREC: { // patch each of the lambdas environments with the current stack frame
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### LETREC [%d]\n", ++count, state.C, byteAt(1));
#endif
                int nargs = byteAt(1);
                for (int i = frameSize(&state.S) - nargs; i < frameSize(&state.S); i++) {
                    Value v = peek(i);
                    if (v.type == VALUE_TYPE_CLO) {
                        patchClo(&state.S, v.val.clo);
                    } else {
                        cant_happen("non-lambda value (%d) for letrec", v.type);
                    }
                }
                state.C += 2;
            }
            break;
            case BYTECODE_AMB: { // create a new failure continuation to resume at the alternative
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### AMB [%d]\n", ++count, state.C, offsetAt(1));
#endif
                state.F = newFail(offsetAt(1), state.E, state.K, state.F);
                snapshotFail(&state.S, state.F);
                state.C += 3;
            }
            break;
            case BYTECODE_CUT: { // discard the current failure continuation
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### CUT\n", ++count, state.C);
#endif
                if (state.F == NULL) {
                    cant_happen("cut with no extant failure continuation");
                }
                state.F = state.F->next;
                state.C += 1;
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
                printf("%4d) %04d ### LET [%d]\n", ++count, state.C, offsetAt(1));
#endif
                state.K = newKont(offsetAt(1), state.E, state.K);
                snapshotKont(&state.S, state.K);
                state.C += 3;
            }
            break;
            case BYTECODE_JMP: { // jump forward a specified amount
#ifdef DEBUG_STEP
                printCEKF(&state);
                printf("%4d) %04d ### JMP [%d]\n", ++count, state.C, offsetAt(1));
#endif
                state.C = offsetAt(1);
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
                printf("%4d) %04d ### INT [%d]\n", ++count, state.C, intAt(1));
#endif
                Value v;
                v.type = VALUE_TYPE_INTEGER;
                v.val = VALUE_VAL_INTEGER(intAt(1));
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
