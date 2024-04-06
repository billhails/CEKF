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
#include <assert.h>

#include "common.h"
#include "debug.h"
#include "cekf.h"
#include "step.h"
#include "hash.h"
#include "arithmetic.h"

int dump_bytecode_flag = 0;

#ifdef DEBUG_STEP
#  define DEBUGPRINTF(...) printf(__VA_ARGS__)
#else
#  define DEBUGPRINTF(...)
#endif

/**
 * The step function of the CEKF machine.
 */

static void step();
static Value lookup(int frame, int offset);
void putValue(Value x);

static CEKF state;

void markCEKF() {
    markEnv(state.E);
    markKont(state.K);
    markFail(state.F);
    markValue(state.V);
    markStack(&state.S);
}

static inline void push(Value v) {
    pushValue(&state.S, v);
}

static inline Value pop() {
    return popValue(&state.S);
}

static inline void popn(int n) {
    popN(&state.S, n);
}

static inline Value peek(int index) {
    return peekValue(&state.S, index);
}

static inline Value tos(void) {
    return peekTop(&state.S);
}

static void copyToVec(Vec *vec) {
    copyTopToValues(&state.S, &(vec->values[0]), vec->size);
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
        clearFrame(&state.S);
    }
    state.B = B;
    first = false;
}

Value run(ByteCodeArray B) {
    inject(B);
    step();
    return state.V;
}

static inline int readCurrentByte(void) {
    return readByte(&state.B, &state.C);
}

static inline int readCurrentWord(void) {
    return readWord(&state.B, &state.C);
}

static inline int readCurrentInt(void) {
    return readInt(&state.B, &state.C);
}

static inline BigInt *readCurrentBigInt(void) {
    bigint bi = readBigint(&state.B, &state.C);
    return newBigInt(bi);
}

static inline int readCurrentOffset(void) {
    return readOffset(&state.B, &state.C);
}

// assumes state.C is at the start of the MATCH table
// i is the index of the match (i.e. it will be multiplied by the sizeof a word)
static inline int readCurrentOffsetAt(int i) {
    return readOffsetAt(&state.B, state.C, i);
}

static bool truthy(Value v) {
    return !((v.type == VALUE_TYPE_STDINT && v.val.stdint == 0)
             || v.type == VALUE_TYPE_VOID);
}

static int _cmp(Value left, Value right);

static int _vecCmp(Vec *left, Vec *right) {
    if (left == right) {
        return 0;
    }
#ifdef SAFETY_CHECKS
    if (left == NULL || right == NULL) {
        cant_happen("null vecs in _vecCmp(%p, %p)", left, right);
    }
    if (left->size == 0 || right->size == 0) {
        cant_happen("empty vecs in _vecCmp()");
    }
#endif
    for (int i = 0; i < left->size; ++i) {
        int cmp = _cmp(left->values[i], right->values[i]);
        if (cmp != 0)
            return cmp;
    }
    return 0;
}

#define _CMP_(left, right) ((left) < (right) ? -1 : (left) == (right) ? 0 : 1)

static int _cmp(Value left, Value right) {
#ifdef DEBUG_STEP
    eprintf("_cmp:\n");
    printContainedValue(left, 0);
    eprintf("\n");
    printContainedValue(right, 0);
    eprintf("\n");
#endif
#ifdef SAFETY_CHECKS
    if (left.type != right.type) {
        cant_happen("different types in _cmp");
    }
#endif
    switch (left.type) {
        case VALUE_TYPE_VOID:
            return 0;
        case VALUE_TYPE_BIGINT:
            return cmpBigInt(left.val.bigint, right.val.bigint);
        case VALUE_TYPE_STDINT:
            return _CMP_(left.val.stdint, right.val.stdint);
        case VALUE_TYPE_CHARACTER:
            return _CMP_(left.val.character, right.val.character);
        case VALUE_TYPE_CLO:
        case VALUE_TYPE_PCLO:
            return _CMP_(left.val.clo->ip, right.val.clo->ip);
        case VALUE_TYPE_CONT:
            return _CMP_(left.val.kont->body, right.val.kont->body);
        case VALUE_TYPE_VEC:
            return _vecCmp(left.val.vec, right.val.vec);
        default:
            cant_happen("unexpected type for _cmp (%d)", left.type);
    }
}

static Value cmp(Value left, Value right) {
    switch (_cmp(left, right)) {
        case -1:
            return vLt;
        case 0:
            return vEq;
        case 1:
            return vGt;
        default:
            cant_happen("unexpected value from _cmp");
    }
}

static bool _eq(Value left, Value right) {
    return _cmp(left, right) == 0;
}

static bool _gt(Value left, Value right) {
    return _cmp(left, right) == 1;
}

static bool _lt(Value left, Value right) {
    return _cmp(left, right) == -1;
}

static bool _xor(Value left, Value right) {
    return truthy(left) ? !truthy(right) : truthy(right);
}

static Value eq(Value left, Value right) {
    bool result = _eq(left, right);
    return result ? vTrue : vFalse;
}

static Value ne(Value left, Value right) {
    bool result = _eq(left, right);
    return result ? vFalse : vTrue;
}

static Value gt(Value left, Value right) {
    bool result = _gt(left, right);
    return result ? vTrue : vFalse;
}

static Value lt(Value left, Value right) {
    bool result = _lt(left, right);
    return result ? vTrue : vFalse;
}

static Value ge(Value left, Value right) {
    bool result = _lt(left, right);
    return result ? vFalse : vTrue;
}

static Value xor(Value left, Value right) {
    return _xor(left, right) ? vTrue : vFalse;
}

static Value le(Value left, Value right) {
    bool result = _gt(left, right);
    return result ? vFalse : vTrue;
}

static Value not(Value left) {
    return truthy(left) ? vFalse : vTrue;
}

static Value vec(Value index, Value vector) {
#ifdef SAFETY_CHECKS
    if (index.type != VALUE_TYPE_STDINT)
        cant_happen("invalid index type for vec %d location %04lx", index.type, state.C);
    if (vector.type != VALUE_TYPE_VEC)
        cant_happen("invalid vector type for vec %d location %04lx", vector.type, state.C);
#endif
    int i = index.val.stdint;
    Vec *vec = vector.val.vec;
    if (i < 0 || i >= vec->size)
        cant_happen("index out of range 0 - %d for vec (%d), location %04lx", vec->size, i, state.C);
    return vec->values[i];
}

static Value lookup(int frame, int offset) {
    Env *env = state.E;
    while (frame > 0) {
        env = env->next;
        frame--;
    }
    return env->values[offset];
}

/**
 * on reaching this point, the stack will contain a number
 * of arguments, and the callable on top.
 *
 * it pops the callable and invokes it, leaving the rest of the
 * arguments on the stack
 */
static void applyProc(int naargs) {
    Value callable = pop();
    int save = protectValue(callable);
    switch (callable.type) {
        case VALUE_TYPE_PCLO:{
                Clo *clo = callable.val.clo;
                int ncaptured = clo->env->count;
                if (clo->pending == naargs) {
                    state.C = clo->ip;
                    state.E = clo->env->next;
                    int stackSize = ncaptured + naargs;
                    // move the new args to the right place on the stack, leaving just enough
                    // space for the captured args below them
                    setFrame(&state.S, ncaptured, naargs);
                    // then copy the already captured args to the base of the stack
                    copyValues(state.S.stack, clo->env->values, ncaptured);
                    state.S.sp = stackSize;
                } else if (naargs == 0) {
                    push(callable);
                } else if (naargs < clo->pending) {
                    Env *env = newEnv(clo->env->next, naargs + ncaptured);
                    int save = PROTECT(env);
                    copyValues(env->values, clo->env->values, ncaptured);
                    copyValues(&(env->values[clo->env->count]),
                               &(state.S.stack[state.S.sp - naargs]), naargs);
                    Clo *pclo = newClo(clo->pending - naargs, clo->ip, env);
                    PROTECT(pclo);
                    callable.type = VALUE_TYPE_PCLO;
                    callable.val.clo = pclo;
                    push(callable);
                    UNPROTECT(save);
                } else {
                    cant_happen
                        ("too many arguments to partial closure, expected %d, got %d",
                         clo->pending, naargs);
                }
            }
            break;
        case VALUE_TYPE_CLO:{
                Clo *clo = callable.val.clo;
                if (clo->pending == naargs) {
                    state.C = clo->ip;
                    state.E = clo->env;
                    setFrame(&state.S, 0, clo->pending);
                } else if (naargs == 0) {
                    push(callable);
                } else if (naargs < clo->pending) {
                    Env *env = newEnv(clo->env, naargs);
                    int save = PROTECT(env);
                    copyTosToEnv(&state.S, env, naargs);
                    Clo *pclo = newClo(clo->pending - naargs, clo->ip, env);
                    PROTECT(pclo);
                    callable.type = VALUE_TYPE_PCLO;
                    callable.val.clo = pclo;
                    push(callable);
                    UNPROTECT(save);
                } else {
                    cant_happen
                        ("too many arguments to closure, expected %d, got %d",
                         clo->pending, naargs);
                }
            }
            break;
        case VALUE_TYPE_CONT:{
                if (callable.val.kont == NULL) {
                    state.V = pop();
                    state.C = UINT64_MAX;
                } else {
                    Value result = pop();
                    protectValue(result);
                    Kont *kont = callable.val.kont;
                    state.C = kont->body;
                    state.K = kont->next;
                    state.E = kont->env;
                    restoreKont(&state.S, kont);
                    push(result);
                }
            }
            break;
        default:
            cant_happen("unexpected type %d in APPLY", callable.type);
    }
    UNPROTECT(save);
}

#define printCEKF(state)

static int count = 0;

void reportSteps(void) {
    printf("%d instructions executed\n", count);
}

static void step() {
    if (dump_bytecode_flag)
        dumpByteCode(&state.B);
    init_arithmetic();
    state.C = 0;
    while (state.C != UINT64_MAX) {
        ++count;
        int bytecode;
#ifdef DEBUG_STEP
        printCEKF(&state);
        printf("%4d) %04lx ### ", count, state.C);
#endif
        switch (bytecode = readCurrentByte()) {
            case BYTECODE_NONE:{
                    cant_happen("encountered NONE in step()");
                }
                break;
            case BYTECODE_LAM:{// create a closure and push it
                    int nargs = readCurrentByte();
                    int letRecOffset = readCurrentByte();
                    int end = readCurrentOffset();
                    DEBUGPRINTF("LAM nargs:[%d] letrec:[%d] end:[%04x]\n",
                                nargs, letRecOffset, end);
                    Clo *clo = newClo(nargs, state.C, state.E);
                    int save = PROTECT(clo);
                    snapshotClo(&state.S, clo, letRecOffset);
                    Value v;
                    v.type = VALUE_TYPE_CLO;
                    v.val = VALUE_VAL_CLO(clo);
                    push(v);
                    UNPROTECT(save);
                    state.C = end;
                }
                break;
            case BYTECODE_VAR:{// look up an environment variable and push it
                    int frame = readCurrentByte();
                    int offset = readCurrentByte();
                    DEBUGPRINTF("VAR [%d:%d]\n", frame, offset);
                    push(lookup(frame, offset));
                }
                break;
            case BYTECODE_LVAR:{
                    // look up a stack variable and push it
                    int offset = readCurrentByte();
                    DEBUGPRINTF("LVAR [%d]\n", offset);
                    push(peek(offset));
                }
                break;
            case BYTECODE_PUSHN:{
                    // allocate space for n variables on the stack
                    int size = readCurrentByte();
                    DEBUGPRINTF("PUSHN [%d]\n", size);
                    pushN(&state.S, size);
                }
                break;
            case BYTECODE_PRIM_PUTC:{
                    // peek value, print it
                    DEBUGPRINTF("PUTC\n");
                    Value b = tos();
                    putchar(b.val.character);
                }
                break;
            case BYTECODE_PRIM_PUTV:{
                    // peek value, print it
                    DEBUGPRINTF("PUTC\n");
                    Value b = tos();
                    putValue(b);
                }
                break;
            case BYTECODE_PRIM_PUTN:{
                    // peek value, print it
                    DEBUGPRINTF("PUTN\n");
                    Value b = tos();
                    switch (b.type) {
                        case VALUE_TYPE_BIGINT:
                            fprintBigInt(stdout, b.val.bigint);
                            break;
                        case VALUE_TYPE_STDINT:
                            printf("%d", b.val.stdint);
                            break;
                        case VALUE_TYPE_RATIONAL:
                            putValue(b.val.vec->values[0]);
                            printf("/");
                            putValue(b.val.vec->values[1]);
                            break;
                        default:
                            cant_happen("unrecognised type %d", b.type);
                    }
                }
                break;
            case BYTECODE_PRIM_CMP:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("CMP\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(cmp(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_ADD:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("ADD\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(add(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_SUB:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("SUB\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(sub(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_MUL:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("MUL\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(mul(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_DIV:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("DIV\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(divide(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_POW:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("POW\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(power(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_MOD:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("MOD\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(modulo(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_EQ:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("EQ\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(eq(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_NE:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("NE\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(ne(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_GT:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("GT\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(gt(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_LT:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("LT\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(lt(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_GE:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("GE\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(ge(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_LE:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("LE\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(le(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_XOR:{
                    // pop two values, perform the binop and push the result
                    DEBUGPRINTF("XOR\n");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(xor(left, right));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_NOT:{
                    // pop value, perform the op and push the result
                    DEBUGPRINTF("NOT\n");
                    Value a = pop();
                    int save = protectValue(a);
                    push(not(a));
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_NEG:{
                    // pop value, perform the op and push the result
                    DEBUGPRINTF("NEG\n");
                    Value a = pop();
                    int save = protectValue(a);
                    push(neg(a));
                    UNPROTECT(save);
            }
            break;
            case BYTECODE_PRIM_VEC:{
                    DEBUGPRINTF("VEC\n");
                    Value b = pop();
                    int save = protectValue(b);
                    Value a = pop();
                    protectValue(a);
                    Value result = vec(a, b);
                    protectValue(result);
                    push(result);
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_PRIM_MAKEVEC:{
                    int size = readCurrentByte();
                    DEBUGPRINTF("MAKEVEC [%d]\n", size);
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
                }
                break;
            case BYTECODE_APPLY:{
                    // apply the callable at the top of the stack to the arguments beneath it
                    int nargs = readCurrentByte();
                    DEBUGPRINTF("APPLY [%d]\n", nargs);
                    applyProc(nargs);
                }
                break;
            case BYTECODE_IF:{ // pop the test result and jump to the appropriate branch
                    int branch = readCurrentOffset();
                    DEBUGPRINTF("IF [%04x]\n", branch);
                    Value aexp = pop();
                    if (!truthy(aexp)) {
                        state.C = branch;
                    }
                }
                break;
            case BYTECODE_MATCH:{
                    // pop the dispach code, verify it's an integer and in range, and dispatch
                    int size __attribute__((unused)) = readCurrentByte();
#ifdef DEBUG_STEP
                    printf("MATCH [%d]", size);
                    int save = state.C;
                    for (int ip = 0; ip < size; ip++) {
                        printf("[%04x]", readCurrentOffset());
                    }
                    state.C = save;
                    printf("\n");
#endif
                    Value v = pop();
#ifdef SAFETY_CHECKS
                    if (v.type != VALUE_TYPE_STDINT)
                        cant_happen
                            ("match expression must be an integer, expected type %d, got %d",
                             VALUE_TYPE_STDINT, v.type);
                    if (v.val.stdint < 0 || v.val.stdint >= size)
                        cant_happen
                            ("match expression index out of range (%d)",
                             v.val.stdint);
#endif
                    state.C = readCurrentOffsetAt(v.val.stdint);
                }
                break;
            case BYTECODE_INTCOND:{
                    // pop the value, walk the dispatch table looking for a match, or run the default
                    int size = readCurrentWord();
#ifdef DEBUG_STEP
                    printf("INTCOND [%d]", size);
                    int here = state.C;
                    for (int ip = 0; ip < size; ip++) {
                        printf(" ");
                        if (bigint_flag) {
                            BigInt *bigInt = readCurrentBigInt();
                            fprintBigInt(stdout, bigInt);
                        } else {
                            int Int = readCurrentInt();
                            printf("%d", Int);
                        }
                        int offset = readCurrentOffset();
                        printf(":[%04x]", offset);
                    }
                    printf("\n");
                    state.C = here;
#endif
                    Value v = pop();
                    int save = protectValue(v);
                    if (bigint_flag) {
                        for (int ip = 0; ip < size; ip++) {
                            BigInt *bigInt = readCurrentBigInt();
                            int offset = readCurrentOffset();
                            if (cmpBigInt(bigInt, v.val.bigint) == 0) {
                                state.C = offset;
                                break;
                            }
                        }
                    } else {
                        for (int ip = 0; ip < size; ip++) {
                            int option = readCurrentInt();
                            int offset = readCurrentOffset();
                            if (option == v.val.stdint) {
                                state.C = offset;
                                break;
                            }
                        }
                    }
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_CHARCOND:{
                    // pop the value, walk the dispatch table looking for a match, or run the default
                    int size = readCurrentWord();
#ifdef DEBUG_STEP
                    printf("CHARCOND [%d]", size);
                    int here = state.C;
                    for (int ip = 0; ip < size; ip++) {
                        int val = readCurrentInt();
                        int offset = readCurrentOffset();
                        printf(" %d:[%04x]", val, offset);
                    }
                    printf("\n");
                    state.C = here;
#endif
                    Value v = pop();
                    int option = 0;
                    switch (v.type) {
                        case VALUE_TYPE_STDINT:
                            option = v.val.stdint;
                            break;
                        case VALUE_TYPE_CHARACTER:
                            option = (int) v.val.character;
                            break;
                        default:
                            cant_happen
                                ("unexpected type %d for CHARCOND value",
                                 v.type);
                    }
                    for (int ip = 0; ip < size; ip++) {
                        int val = readCurrentInt();
                        int offset = readCurrentOffset();
                        if (option == val) {
                            state.C = offset;
                            break;
                        }
                    }
                }
                break;
            case BYTECODE_LETREC:{
                    // patch each of the lambdas environments with the current stack frame
                    int nargs = readCurrentByte();
                    DEBUGPRINTF("LETREC [%d]\n", nargs);
                    for (int i = frameSize(&state.S) - nargs;
                         i < frameSize(&state.S); i++) {
                        Value v = peek(i);
                        if (v.type == VALUE_TYPE_CLO) {
                            patchClo(&state.S, v.val.clo);
                        } else {
                            cant_happen("non-lambda value (%d) for letrec",
                                        v.type);
                        }
                    }
                }
                break;
            case BYTECODE_AMB:{// create a new failure continuation to resume at the alternative
                    int branch = readCurrentOffset();
                    DEBUGPRINTF("AMB [%04x]\n", branch);
                    state.F = newFail(branch, state.E, state.K, state.F);
                    snapshotFail(&state.S, state.F);
                }
                break;
            case BYTECODE_CUT:{// discard the current failure continuation
                    DEBUGPRINTF("CUT\n");
#ifdef SAFETY_CHECKS
                    if (state.F == NULL) {
                        cant_happen
                            ("cut with no extant failure continuation");
                    }
#endif
                    state.F = state.F->next;
                }
                break;
            case BYTECODE_BACK:{
                    // restore the failure continuation or halt
                    DEBUGPRINTF("BACK\n");
                    if (state.F == NULL) {
                        state.C = -1;
                    } else {
                        state.C = state.F->exp;
                        state.E = state.F->env;
                        state.K = state.F->kont;
                        restoreFail(&state.S, state.F);
                        state.F = state.F->next;
                    }
                }
                break;
            case BYTECODE_LET:{// create a new continuation to resume the body, and transfer control to the expression
                    int offset = readCurrentOffset();
                    DEBUGPRINTF("LET [%04x]\n", offset);
                    state.K = newKont(offset, state.E, state.K);
                    validateLastAlloc();
                    snapshotKont(&state.S, state.K);
                }
                break;
            case BYTECODE_JMP:{// jump forward a specified amount
                    int offset = readCurrentOffset();
                    DEBUGPRINTF("JMP [%04x]\n", offset);
                    state.C = offset;
                }
                break;
            case BYTECODE_CALLCC:{
                    // pop the callable, push the current continuation, push the callable and apply
                    DEBUGPRINTF("CALLCC\n");
                    Value aexp = pop();
                    int save = protectValue(aexp);
                    Value cc;
                    cc.type = VALUE_TYPE_CONT;
                    cc.val = VALUE_VAL_CONT(state.K);
                    push(cc);
                    push(aexp);
                    UNPROTECT(save);
                    applyProc(1);
                }
                break;
            case BYTECODE_TRUE:{
                    // push true
                    DEBUGPRINTF("TRUE\n");
                    push(vTrue);
                }
                break;
            case BYTECODE_FALSE:{
                    // push false
                    DEBUGPRINTF("FALSE\n");
                    push(vFalse);
                }
                break;
            case BYTECODE_VOID:{
                    // push void
                    DEBUGPRINTF("VOID\n");
                    push(vVoid);
                }
                break;
            case BYTECODE_STDINT:{
                    // push literal int
                    int val = readCurrentInt();
                    DEBUGPRINTF("STDINT [%d]\n", val);
                    Value v;
                    v.type = VALUE_TYPE_STDINT;
                    v.val = VALUE_VAL_STDINT(val);
                    push(v);
                }
                break;
            case BYTECODE_CHAR:{
                    // push literal char
                    char c = readCurrentByte();
                    DEBUGPRINTF("CHAR [%c]\n", c);
                    Value v;
                    v.type = VALUE_TYPE_CHARACTER;
                    v.val = VALUE_VAL_CHARACTER(c);
                    push(v);
                }
                break;
            case BYTECODE_BIGINT:{
                    BigInt *bigInt = readCurrentBigInt();
                    int save = PROTECT(bigInt);
#ifdef DEBUG_STEP
                    printf("BIGINT [");
                    fprintBigInt(stdout, bigInt);
                    printf("]\n");
#endif
                    Value v;
                    v.type = VALUE_TYPE_BIGINT;
                    v.val = VALUE_VAL_BIGINT(bigInt);
                    push(v);
                    UNPROTECT(save);
                }
                break;
            case BYTECODE_RETURN:{
                    // push the current continuation and apply
                    DEBUGPRINTF("RETURN\n");
                    Value kont;
                    kont.type = VALUE_TYPE_CONT;
                    kont.val = VALUE_VAL_CONT(state.K);
                    push(kont);
                    applyProc(1);
                }
                break;
            case BYTECODE_DONE:{
                    // can't happen, probably
                    DEBUGPRINTF("DONE\n");
                    state.C = UINT64_MAX;
                }
                break;
            case BYTECODE_ERROR:{
                    DEBUGPRINTF("ERROR\n");
                    state.C = UINT64_MAX;
                    eprintf("pattern match exhausted in step\n");
                }
                break;
            default:
                cant_happen("unrecognised bytecode %d in step()", bytecode);
        }
#ifdef DEBUG_STEP
#  ifdef DEBUG_SLOW_STEP
        sleep(1);
#  endif
#endif
    }
}

static void putVec(Vec *x);

void putValue(Value x) {
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printf("<void>");
            break;
        case VALUE_TYPE_STDINT:
            printf("%d", x.val.stdint);
            break;
        case VALUE_TYPE_BIGINT:
            fprintBigInt(stdout, x.val.bigint);
            break;
        case VALUE_TYPE_CHARACTER:
            switch (x.val.character) {
                case '\t':
                    printf("'\\t'");
                    break;
                case '\n':
                    printf("'\\n'");
                    break;
                default:
                    printf("'%c'", x.val.character);
                    break;
            }
            break;
        case VALUE_TYPE_CLO:
            printf("<closure>");
            break;
        case VALUE_TYPE_CONT:
            printf("<continuation>");
            break;
        case VALUE_TYPE_VEC:
            putVec(x.val.vec);
            break;
        default:
            cant_happen("unrecognised value type in putValue");
    }
}

static void putVec(Vec *x) {
    printf("#[");
    for (int i = 0; i < x->size; i++) {
        putValue(x->values[i]);
        if (i + 1 < x->size) {
            printf(" ");
        }
    }
    printf("]");
}
