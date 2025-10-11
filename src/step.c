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
#include <math.h>

#include "common.h"
#include "debug.h"
#include "cekf.h"
#include "step.h"
#include "hash.h"
#include "arithmetic.h"
#include "builtins_impl.h"
#include "builtins_debug.h"
#include "memory.h"
#include "utf8.h"
#include "builtin_io.h"

#ifdef UNIT_TESTS
#include "tests/step.h"
#endif

int dump_bytecode_flag = 0;

#ifdef DEBUG_STEP
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

/**
 * The step function of the CEKF machine.
 */

static void step();
static Value lookup(int frame, int offset);
void putCharacter(Character x);

static CEKF state;

void markState() {
    state.header.keep = false;
    markCEKF(&state);
}

static inline void patch(Value v, int num) {
    patchVec(v.val.namespace, state.S, num);
}

static inline void poke(int offset, Value v) {
    pokeStack(state.S, offset, v);
}

static inline void push(Value v) {
    pushStackEntry(state.S, v);
}

static inline void extend(int i) {
    pushnStack(state.S, i, vVoid);
}

static inline void discard(int num) {
    popnStack(state.S, num);
}

static inline Value pop() {
    return popStackEntry(state.S);
}

static inline void popn(int n) {
    popnStack(state.S, n);
}

static inline Value peek(int index) {
    return peeknStack(state.S, index);
}

static inline void copyToVec(Vec *vec) {
    copyTosToVec(vec, state.S);
}

static Env *builtInsToEnv(BuiltIns *b)__attribute__((unused));

static Env *builtInsToEnv(BuiltIns *b) {
    Env *env = makeEnv(NULL);
    int save = PROTECT(env);
    for (Index i = 0; i < b->size; i++) {
        BuiltIn *builtIn = b->entries[i];
        DEBUG("adding builtin %s at %p", builtIn->name->name, builtIn->implementation);
        BuiltInImplementation *implementation = newBuiltInImplementation(builtIn->implementation, builtIn->args->size);
        PROTECT(implementation);
        pushFrame(env->S, value_BuiltIn(implementation));
    }
    UNPROTECT(save);
    return env;
}

static void inject(ByteCodeArray B, LocationArray *L, BuiltIns *builtIns __attribute__((unused))) {
    static bool first = true;
    state.C = 0;
    state.E = builtInsToEnv(builtIns);
    state.K = NULL;
    state.F = NULL;
    if (first) {
        state.S = newStack();
    } else {
        clearStackFrames(state.S);
    }
    state.B = B;
    state.L = L;
    first = false;
}

void run(ByteCodeArray B, LocationArray *L, BuiltIns *builtIns) {
    inject(B, L, builtIns);
    step();
    state.E = NULL;
    state.K = NULL;
    state.F = NULL;
    state.S->frame = 0;
    state.S->offset = 0;
    state.S->frames_index = 0;
    collectGarbage();
}

static inline int readCurrentByte(void) {
    return readByte(&state.B, &state.C);
}

static inline Character readCurrentCharacter(void) {
    return readCharacter(&state.B, &state.C);
}

static inline int readCurrentWord(void) {
    return readWord(&state.B, &state.C);
}

static inline Integer readCurrentInt(void) {
    return readInteger(&state.B, &state.C);
}

static inline Double readCurrentIrrational(void) {
    return readDouble(&state.B, &state.C);
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
    // FIXME this can't be right now!
    return !((v.type == VALUE_TYPE_STDINT && v.val.stdint == 0)
             || v.type == VALUE_TYPE_NONE);
}

static Cmp _cmp(Value left, Value right);

static Cmp _vecCmp(Vec *left, Vec *right) {
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
    for (Index i = 0; i < left->size; ++i) {
        int cmp = _cmp(left->entries[i], right->entries[i]);
        if (cmp != CMP_EQ)
            return cmp;
    }
    return CMP_EQ;
}

#define _CMP_(left, right) ((left) < (right) ? CMP_LT : (left) == (right) ? CMP_EQ : CMP_GT)

static Cmp _cmp(Value left, Value right) {
#ifdef SAFETY_CHECKS
    if (left.type != right.type) {
        switch (left.type) {
            case VALUE_TYPE_BIGINT:
            case VALUE_TYPE_STDINT:
            case VALUE_TYPE_RATIONAL:
            case VALUE_TYPE_IRRATIONAL:
            case VALUE_TYPE_BIGINT_IMAG:
            case VALUE_TYPE_STDINT_IMAG:
            case VALUE_TYPE_IRRATIONAL_IMAG:
            case VALUE_TYPE_COMPLEX:
                switch (right.type) {
                    case VALUE_TYPE_BIGINT:
                    case VALUE_TYPE_STDINT:
                    case VALUE_TYPE_RATIONAL:
                    case VALUE_TYPE_BIGINT_IMAG:
                    case VALUE_TYPE_STDINT_IMAG:
                    case VALUE_TYPE_IRRATIONAL_IMAG:
                    case VALUE_TYPE_COMPLEX:
                        break;
                    default:
                        cant_happen("different types in _cmp");
                }
                break;
            default:
                cant_happen("different types in _cmp");
        }
    }
#endif
    switch (left.type) {
        case VALUE_TYPE_NONE:
            return 0;
        case VALUE_TYPE_BIGINT:
        case VALUE_TYPE_STDINT:
        case VALUE_TYPE_RATIONAL:
        case VALUE_TYPE_IRRATIONAL:
        case VALUE_TYPE_BIGINT_IMAG:
        case VALUE_TYPE_STDINT_IMAG:
        case VALUE_TYPE_RATIONAL_IMAG:
        case VALUE_TYPE_IRRATIONAL_IMAG:
        case VALUE_TYPE_COMPLEX:
            return ncmp(left, right);
        case VALUE_TYPE_CHARACTER:
            return _CMP_(left.val.character, right.val.character);
        case VALUE_TYPE_CLO:
        case VALUE_TYPE_PCLO:
            return _CMP_(left.val.clo->C, right.val.clo->C);
        case VALUE_TYPE_KONT:
            return _CMP_(left.val.kont->C, right.val.kont->C);
        case VALUE_TYPE_VEC:
            return _vecCmp(left.val.vec, right.val.vec);
        default:
            cant_happen("unexpected type for _cmp (%d)", left.type);
    }
}

static Value vcmp(Value left, Value right) {
    switch (_cmp(left, right)) {
        case CMP_LT:
            return vLt;
        case CMP_EQ:
            return vEq;
        case CMP_GT:
            return vGt;
        default:
            cant_happen("unexpected value from _cmp");
    }
}

static bool _eq(Value left, Value right) {
    return _cmp(left, right) == CMP_EQ;
}

static bool _gt(Value left, Value right) {
    return _cmp(left, right) == CMP_GT;
}

static bool _lt(Value left, Value right) {
    return _cmp(left, right) == CMP_LT;
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

static Value le(Value left, Value right) {
    bool result = _gt(left, right);
    return result ? vFalse : vTrue;
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
    if (i < 0 || i >= (int) vec->size)
        cant_happen("index out of range 0 - %d for vec (%d), location %04lx", vec->size, i, state.C);
    return vec->entries[i];
}

static Value lookup(int frame, int offset) {
    Env *env = state.E;
    while (frame > 0) {
        env = env->E;
        frame--;
    }
    return env->S->entries[offset];
}

static Value captureKont(void) {
    Kont *K = state.K;
    Value cc;
    if (K == NULL) {
        cc = value_Kont(NULL);
    } else if (K->S == NULL) {
        Stack *s = newStack();
        int save = PROTECT(s);
        copyStackContinuation(s, state.S);
        Kont *newK = newKont(K->C, K->E, s, K->K);
        cc = value_Kont(newK);
        UNPROTECT(save);
    } else {
        cc = value_Kont(state.K);
    }
    return cc;
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
                int ncaptured = clo->E->S->size;
                DEBUG("PCLO ncaptured = %d, naargs = %d, pending = %d", ncaptured, naargs, clo->pending);
                if (clo->pending == naargs) {
                    // move the new args to the right place on the stack,
                    // leaving just enough space for the captured args
                    // below them:
                    // | ..captured.. | ..aargs.. |
                    //                  ^^^^^^^^^ ^
                    //                    moved   SP
                    moveStack(state.S, ncaptured, naargs);
                    // then copy the already captured args to the base
                    // of the stack
                    copyValues(&state.S->entries[state.S->frame], clo->E->S->entries, ncaptured);
                    // set the stack pointer to the last arg
                    state.S->offset = ncaptured + naargs;
                    // and set up the machine for the next step into the
                    // body of the closure
                    state.E = clo->E->E;
                    state.C = clo->C;
                } else if (naargs == 0) {
                    // args expected, no args passed, no-op
                    push(callable);
                } else if (naargs < clo->pending) {
                    // create a new partial closure capturing the
                    // additional arguments so far
                    // create a new env which is a sibling of the
                    // partial closure's env.
                    Env *env = makeEnv(clo->E->E);
                    int save = PROTECT(env);
                    // make sure that the new env has enough space
                    extendFrame(env->S, ncaptured + naargs);
                    // copy already captured arguments into the new env
                    copyValues(env->S->entries, clo->E->S->entries, ncaptured);
                    // copy the additional arguments after them
                    copyValues(&(env->S->entries[clo->E->S->size]),
                               &(state.S->entries[totalSizeStack(state.S) - naargs]), naargs);
                    // set the size of the new env
                    env->S->size = ncaptured + naargs;
                    // create a new closure with correct pending, C and
                    // the new env
                    Clo *pclo = newClo(clo->pending - naargs, clo->C, env);
                    PROTECT(pclo);
                    callable.val.clo = pclo;
                    // and push it as the result
                    popn(naargs);
                    push(callable);
                    UNPROTECT(save);
                } else {
                    cant_happen("over-application not supported yet, expected %d, got %d", clo->pending, naargs);
                }
            }
            break;
        case VALUE_TYPE_CLO:{
                Clo *clo = callable.val.clo;
                DEBUG("CLO pending = %d, naargs = %d", clo->pending, naargs);
                if (clo->pending == naargs) {
                    state.C = clo->C;
                    state.E = clo->E;
                    moveStack(state.S, 0, clo->pending);
                } else if (naargs == 0) {
                    push(callable);
                } else if (naargs < clo->pending) {
                    Env *env = makeEnv(clo->E);
                    int save = PROTECT(env);
                    copyTosToEnv(env, state.S, naargs);
                    Clo *pclo = newClo(clo->pending - naargs, clo->C, env);
                    PROTECT(pclo);
#ifdef DEBUG_STEP
                    dumpFrame(env->S);
#endif
                    DEBUG("CREATED PCLO ncaptured = %d, naargs = %d, pending = %d", pclo->E->S->size, naargs, pclo->pending);
                    callable.type = VALUE_TYPE_PCLO;
                    callable.val.clo = pclo;
                    popn(naargs);
                    push(callable);
                    UNPROTECT(save);
                } else {
                    cant_happen("over-application not supported yet, expected %d, got %d", clo->pending, naargs);
                }
            }
            break;
        case VALUE_TYPE_KONT:{
                if (callable.val.kont == NULL) {
                    state.C = END_CONTROL;
                } else {
                    Value result = pop();
                    protectValue(result);
                    Kont *kont = callable.val.kont;
                    state.C = kont->C;
                    state.K = kont->K;
                    state.E = kont->E;
                    restoreKont(state.S, kont);
                    push(result);
                }
            }
            break;
        case VALUE_TYPE_BUILTIN:{
                BuiltInImplementation *impl = callable.val.builtIn;
                if (naargs == impl->nargs) {
                    BuiltInFunction fn = (BuiltInFunction) impl->implementation;
                    Vec *v = newVec(impl->nargs);
                    int save = PROTECT(v);
                    copyValues(v->entries, &(state.S->entries[totalSizeStack(state.S) - impl->nargs]), impl->nargs);
                    Value res = fn(v);
                    protectValue(res);
                    state.S->offset -= impl->nargs;
                    push(res);
                    UNPROTECT(save);
                } else if (naargs == 0) {
                    push(callable);
                } else {
                    cant_happen("curried built-ins not supported yet (expected %d got %d)", impl->nargs, naargs);
                }
            }
            break;
        default:
            cant_happen("unexpected type %s in APPLY", valueTypeName(callable.type));
    }
    UNPROTECT(save);
}

static unsigned long int count = 0;

void reportSteps(void) {
    printf("instructions executed: %lu\n", count);
    printf("max stack capacity: %d\n", state.S->entries_capacity);
#ifdef SAFETY_CHECKS
    reportKonts();
#endif
}

static void step() {
    if (dump_bytecode_flag)
        dumpByteCode(&state.B, state.L);
    state.L = NULL;
    state.C = 0;
    while (state.C != END_CONTROL) {
        ++count;
        int bytecode;
#ifdef DEBUG_STEP
        // dumpStack(state.S);
        // printf("%4ld) %04lx ### ", count, state.C);
        printf("%04lx ### ", state.C);
#endif
        switch (bytecode = readCurrentByte()) {
            case BYTECODES_TYPE_NONE:{
                    cant_happen("encountered NONE in step()");
                }
                break;

            case BYTECODES_TYPE_LAM:{
                    // create a closure and push it
                    int nargs = readCurrentByte();
                    int letRecOffset = readCurrentByte();
                    int end = readCurrentOffset();
                    DEBUG("LAM nargs:[%d] letrec:[%d] end:[%04x]",
                                nargs, letRecOffset, end);
                    Clo *clo = newClo(nargs, state.C, state.E);
                    int save = PROTECT(clo);
                    snapshotClo(clo, state.S, letRecOffset);
                    Value v = value_Clo(clo);
                    push(v);
                    UNPROTECT(save);
                    state.C = end;
                }
                break;

            case BYTECODES_TYPE_VAR:{
                    // look up an environment variable and push it
                    int frame = readCurrentByte();
                    int offset = readCurrentByte();
                    Value v = lookup(frame, offset);
                    DEBUG("VAR [%d:%d] == %s", frame, offset, valueTypeName(v.type));
                    push(v);
                }
                break;

            case BYTECODES_TYPE_LVAR:{
                    // look up a stack variable and push it
                    int offset = readCurrentByte();
                    Value v = peek(offset);
                    DEBUG("LVAR [%d] == %s", offset, valueTypeName(v.type));
                    push(v);
                }
                break;

            case BYTECODES_TYPE_PUSHN:{
                    // allocate space for n variables on the stack
                    int size = readCurrentByte();
                    DEBUG("PUSHN [%d]", size);
                    extend(size);
                }
                break;

            case BYTECODES_TYPE_PRIM_CMP:{
                    // pop two values, perform the binop and push the result
                    DEBUG("CMP");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(vcmp(left, right));
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_ADD:{
                    // pop two values, perform the binop and push the result
                    DEBUG("ADD");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    Value res = nadd(left, right);
                    protectValue(res);
                    push(res);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_SUB:{
                    // pop two values, perform the binop and push the result
                    DEBUG("SUB");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    Value res = nsub(left, right);
                    protectValue(res);
                    push(res);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_MUL:{
                    // pop two values, perform the binop and push the result
                    DEBUG("MUL");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    Value res = nmul(left, right);
                    protectValue(res);
                    push(res);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_DIV:{
                    // pop two values, perform the binop and push the result
                    DEBUG("DIV");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    Value res = ndiv(left, right);
                    protectValue(res);
                    push(res);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_POW:{
                    // pop two values, perform the binop and push the result
                    DEBUG("POW");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    Value res = npow(left, right);
                    protectValue(res);
                    push(res);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_MOD:{
                    // pop two values, perform the binop and push the result
                    DEBUG("MOD");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    Value res = nmod(left, right);
                    protectValue(res);
                    push(res);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_EQ:{
                    // pop two values, perform the binop and push the result
                    DEBUG("EQ");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(eq(left, right));
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_NE:{
                    // pop two values, perform the binop and push the result
                    DEBUG("NE");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(ne(left, right));
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_GT:{
                    // pop two values, perform the binop and push the result
                    DEBUG("GT");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(gt(left, right));
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_LT:{
                    // pop two values, perform the binop and push the result
                    DEBUG("LT");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(lt(left, right));
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_GE:{
                    // pop two values, perform the binop and push the result
                    DEBUG("GE");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(ge(left, right));
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_LE:{
                    // pop two values, perform the binop and push the result
                    DEBUG("LE");
                    Value right = pop();
                    int save = protectValue(right);
                    Value left = pop();
                    protectValue(left);
                    push(le(left, right));
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_PRIM_VEC:{
                    // index, vector => value at index
                    DEBUG("VEC");
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

            case BYTECODES_TYPE_PRIM_MAKEVEC:{
                    int size = readCurrentByte();
                    DEBUG("MAKEVEC [%d]", size);
                    // at this point there will be `size` arguments on the stack. Rather than
                    // popping then individually we can just memcpy them into a new struct Vec
                    Vec *v = newVec(size);
                    int save = PROTECT(v);
                    copyToVec(v);
                    popn(size);
                    Value val = value_Vec(v);
                    push(val);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_APPLY:{
                    // apply the callable at the top of the stack to the arguments beneath it
                    int nargs = readCurrentByte();
                    DEBUG("APPLY [%d]", nargs);
                    applyProc(nargs);
                }
                break;

            case BYTECODES_TYPE_IF:{
                    // pop the test result and jump to the appropriate branch
                    int branch = readCurrentOffset();
                    DEBUG("IF [%04x]", branch);
                    Value aexp = pop();
                    if (!truthy(aexp)) {
                        state.C = branch;
                    }
                }
                break;

            case BYTECODES_TYPE_MATCH:{
                    // pop the dispach code, verify it's an integer and in range, and dispatch
                    int size __attribute__((unused)) = readCurrentByte();
#ifdef DEBUG_STEP
                    printf("MATCH [%d]", size);
                    int save = state.C;
                    for (int C = 0; C < size; C++) {
                        printf("[%04x]", readCurrentOffset());
                    }
                    state.C = save;
                    printf("\n");
#endif
                    Value v = pop();
#ifdef SAFETY_CHECKS
                    if (v.type != VALUE_TYPE_STDINT)
                        cant_happen
                            ("match expression must be an integer, got %s at %lx",
                             valueTypeName(v.type), state.C);
                    if (v.val.stdint < 0 || v.val.stdint >= size)
                        cant_happen
                            ("match expression index out of range (%d)",
                             v.val.stdint);
#endif
                    state.C = readCurrentOffsetAt(v.val.stdint);
                }
                break;

            case BYTECODES_TYPE_INTCOND:{
                    // pop the value, walk the dispatch table looking for a match, or run the default
                    int size = readCurrentWord();
#ifdef DEBUG_STEP
                    printf("INTCOND [%d]", size);
                    int here = state.C;
                    for (int C = 0; C < size; C++) {
                        printf(" ");
                        switch(readCurrentByte()) {
                            case BYTECODES_TYPE_BIGINT: {
                                BigInt *bigInt = readCurrentBigInt();
                                fprintBigInt(stdout, bigInt);
                            }
                            break;
                            case BYTECODES_TYPE_STDINT: {
                                Integer Int = readCurrentInt();
                                printf("%d", Int);
                            }
                            break;
                            default:
                                cant_happen("expected int or bigint in INTCOND cases");
                        }
                        int offset = readCurrentOffset();
                        printf(":[%04x]", offset);
                    }
                    printf("\n");
                    state.C = here;
#endif
                    Value v = pop();
                    int save = protectValue(v);
                    for (int C = 0; C < size; C++) {
                        switch(readCurrentByte()) {
                            case BYTECODES_TYPE_BIGINT: {
                                BigInt *bigInt = readCurrentBigInt();
                                PROTECT(bigInt);
                                Value u = value_Bigint(bigInt);
                                protectValue(u);
                                int offset = readCurrentOffset();
                                if (ncmp(u, v) == CMP_EQ) {
                                    state.C = offset;
                                    goto FINISHED_INTCOND;
                                }
                            }
                            break;
                            case BYTECODES_TYPE_STDINT: {
                                Integer option = readCurrentInt();
                                Value u = value_Stdint(option);
                                int offset = readCurrentOffset();
                                if (ncmp(u, v) == CMP_EQ) {
                                    state.C = offset;
                                    goto FINISHED_INTCOND;
                                }
                            }
                            break;
                            case BYTECODES_TYPE_IRRATIONAL: {
                                Double option = readCurrentIrrational();
                                Value u = value_Irrational(option);
                                int offset = readCurrentOffset();
                                if (ncmp(u, v) == CMP_EQ) {
                                    state.C = offset;
                                    goto FINISHED_INTCOND;
                                }
                            }
                            break;
                            default:
                                cant_happen("expected int or bigint in INTCOND cases");
                        }
                    }
                FINISHED_INTCOND:
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_CHARCOND:{
                    // pop the value, walk the dispatch table looking for a match, or run the default
                    int size = readCurrentWord();
#ifdef DEBUG_STEP
                    printf("CHARCOND [%d]", size);
                    int here = state.C;
                    for (int C = 0; C < size; C++) {
                        Integer val = readCurrentInt();
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
                    for (int C = 0; C < size; C++) {
                        Integer val = readCurrentInt();
                        int offset = readCurrentOffset();
                        if (option == val) {
                            state.C = offset;
                            break;
                        }
                    }
                }
                break;

            case BYTECODES_TYPE_LETREC:{
                    // patch each of the lambdas environments with the current stack frame
                    // i.e. all the definitions in the current letrec.
                    int nargs = readCurrentByte();
                    DEBUG("LETREC [%d] state.S->offset = %d", nargs, state.S->offset);
                    for (Index i = state.S->offset - nargs; i < state.S->offset; i++) {
                        Value v = peek(i);
                        if (v.type == VALUE_TYPE_CLO) {
                            patchClo(v.val.clo, state.S);
                        } else {
                            cant_happen("non-lambda value (%d) for letrec", v.type);
                        }
                    }
                }
                break;

            case BYTECODES_TYPE_AMB:{
                    // create a new failure continuation to resume at the alternative
                    int branch = readCurrentOffset();
                    DEBUG("AMB [%04x]", branch);
                    state.F = makeFail(branch, state.E, state.K, state.F);
                    snapshotFail(state.F, state.S);
                }
                break;

            case BYTECODES_TYPE_CUT:{
                    // discard the current failure continuation
                    DEBUG("CUT");
#ifdef SAFETY_CHECKS
                    if (state.F == NULL) {
                        cant_happen
                            ("cut with no extant failure continuation");
                    }
#endif
                    state.F = state.F->F;
                }
                break;

            case BYTECODES_TYPE_BACK:{
                    // restore the failure continuation or halt
                    DEBUG("BACK");
                    if (state.F == NULL) {
                        state.C = END_CONTROL;
                    } else {
                        state.C = state.F->C;
                        state.E = state.F->E;
                        state.K = state.F->K;
                        restoreFail(state.S, state.F);
                        state.F = state.F->F;
                    }
                }
                break;

            case BYTECODES_TYPE_LET:{
                    // create a new continuation to resume the body, and transfer control to the expression
                    int offset = readCurrentOffset();
                    DEBUG("LET [%04x]", offset);
                    letStackFrame(state.S);
                    state.K = makeKont(offset, state.E, false, state.K);
                    validateLastAlloc();
                }
                break;

            case BYTECODES_TYPE_JMP:{
                    // jump forward a specified amount
                    int offset = readCurrentOffset();
                    DEBUG("JMP [%04x]", offset);
                    state.C = offset;
                }
                break;

            case BYTECODES_TYPE_CALLCC:{
                    // pop the callable, push the current continuation, push the callable and apply
                    DEBUG("CALLCC");
                    Value aexp = pop();
                    int save = protectValue(aexp);
                    Value cc = captureKont();
                    protectValue(cc);
                    push(cc);
                    push(aexp);
                    UNPROTECT(save);
                    applyProc(1);
                }
                break;

            case BYTECODES_TYPE_IRRATIONAL:{
                    // push literal Double
                    Double f = readCurrentIrrational();
                    DEBUG("IRRATIONAL [%f]", f);
                    Value v = value_Irrational(f);
                    push(v);
                }
                break;

            case BYTECODES_TYPE_IRRATIONAL_IMAG:{
                    // push literal Double
                    Double f = readCurrentIrrational();
                    DEBUG("IRRATIONAL_IMAG [%f]", f);
                    Value v = value_Irrational_imag(f);
                    push(v);
                }
                break;

            case BYTECODES_TYPE_STDINT:{
                    // push literal int
                    Integer val = readCurrentInt();
                    DEBUG("STDINT [%d]", val);
                    Value v = value_Stdint(val);
                    push(v);
                }
                break;

            case BYTECODES_TYPE_STDINT_IMAG:{
                    // push literal int
                    Integer val = readCurrentInt();
                    DEBUG("STDINT_IMAG [%d]", val);
                    Value v = value_Stdint_imag(val);
                    push(v);
                }
                break;

            case BYTECODES_TYPE_CHAR:{
                    // push literal char
                    Character c = readCurrentCharacter();
                    DEBUG("CHAR [%s]", charRep(c));
                    Value v = value_Character(c);
                    push(v);
                }
                break;

            case BYTECODES_TYPE_BIGINT:{
                    BigInt *bigInt = readCurrentBigInt();
                    int save = PROTECT(bigInt);
#ifdef DEBUG_STEP
                    printf("BIGINT [");
                    fprintBigInt(stdout, bigInt);
                    printf("]\n");
#endif
                    Value v = value_Bigint(bigInt);
                    push(v);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_BIGINT_IMAG:{
                    BigInt *bigInt = readCurrentBigInt();
                    int save = PROTECT(bigInt);
#ifdef DEBUG_STEP
                    printf("BIGINT_IMAG [");
                    fprintBigInt(stdout, bigInt);
                    printf("]\n");
#endif
                    Value v = value_Bigint_imag(bigInt);
                    push(v);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_RETURN:{
                    // push the current continuation and apply
                    DEBUG("RETURN");
                    Value kont = value_Kont(state.K);
                    push(kont);
                    applyProc(1);
                }
                break;

            case BYTECODES_TYPE_NS_START:{
                    int num = readCurrentWord();
                    DEBUG("NS_START [%d]", num);
                    extend(num);
                }
                break;

            case BYTECODES_TYPE_NS_END:{
                    int numLambdas = readCurrentWord();
                    int stackOffset = readCurrentWord();
                    DEBUG("NS_END [%d] [%d]", numLambdas, stackOffset);
                    Vec *snapshot = snapshotNamespace(state.S);
                    int save = PROTECT(snapshot);
                    Value ns = value_Namespace(snapshot);
                    poke(0 - (numLambdas + stackOffset), ns);
                    discard(numLambdas);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_NS_FINISH:{
                    int num = readCurrentWord();
                    DEBUG("NS_FINISH [%d]", num);
                    // at this point we need to patch each of the namespaces with the
                    // final block of populated namespaces, size num, and at TOS
                    for (int i = 1; i <= num; i++) {
                        Value ns = peek(-i);
#ifdef SAFETY_CHECKS
                        if (ns.type != VALUE_TYPE_NAMESPACE) {
                            cant_happen("expected namespace, got %d", ns.type);
                        }
#endif
                        patch(ns, num);
                    }
                }
                break;

            case BYTECODES_TYPE_NS_PUSHSTACK:{
                    int offset = readCurrentWord();
                    DEBUG("NS_PUSHSTACK [%d]", offset);
                    Value v = peek(offset);
#ifdef SAFETY_CHECKS
                    if (v.type != VALUE_TYPE_NAMESPACE) {
                        cant_happen("expected namespace, got type %d", v.type);
                    }
#endif
                    // new empty stack frame
                    pushStackFrame(state.S);
                    // copy the namespace contents to the top of the stack
                    restoreNamespace(state.S, v.val.namespace);
                }
                break;

            case BYTECODES_TYPE_NS_PUSHENV:{
                    int frame = readCurrentWord();
                    int offset = readCurrentWord();
                    DEBUG("NS_PUSHENV [%d][%d]", frame, offset);
                    Value v = lookup(frame, offset);
#ifdef SAFETY_CHECKS
                    if (v.type != VALUE_TYPE_NAMESPACE) {
                        cant_happen("expected namespace, got type %d", v.type);
                    }
#endif
                    // new empty stack frame 
                    pushStackFrame(state.S);
                    // copy the namespace contents to the top of the stack
                    restoreNamespace(state.S, v.val.namespace);
                }
                break;

            case BYTECODES_TYPE_NS_POP:{
                    DEBUG("NS_POP");
                    Value result = pop();
                    int save = protectValue(result);
                    // remove the top stack frame
                    popStackFrame(state.S);
                    push(result);
                    UNPROTECT(save);
                }
                break;

            case BYTECODES_TYPE_DONE:{
                    // can't happen, probably
                    DEBUG("DONE");
                    state.C = END_CONTROL;
                }
                break;

            case BYTECODES_TYPE_ERROR:{
                    DEBUG("ERROR");
                    state.C = END_CONTROL;
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


void putCharacter(Character c) {
    unsigned char buf[8];
    unsigned char *ptr = writeChar(buf, c);
    *ptr = 0;
    printf("%s", buf);
}

#ifdef UNIT_TESTS
#include "tests/step.c"
#endif
