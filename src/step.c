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

#ifdef DEBUG_STEP
#define DEBUGPRINTF(...) printf(__VA_ARGS__)
#else
#define DEBUGPRINTF(...)
#endif

/**
 * The step function of the CEKF machine.
 */

static void step();
static Value lookup(int frame, int offset);
static int protectValue(Value v);

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

static Value intValue(int i) {
    Value value;
    value.type = VALUE_TYPE_STDINT;
    value.val = VALUE_VAL_STDINT(i); 
    return value;
}

static Value bigIntValue(BigInt *i) {
    Value value;
    value.type = VALUE_TYPE_BIGINT;
    value.val = VALUE_VAL_BIGINT(i); 
    return value;
}

static bool truthy(Value v) {
    return !((v.type == VALUE_TYPE_STDINT && v.val.z == 0) || v.type == VALUE_TYPE_VOID);
}

typedef Value (*IntegerBinOp)(Value, Value);

static IntegerBinOp add;

static Value bigAdd(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_BIGINT);
    assert(b.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = addBigInt(a.val.b, b.val.b);
    return bigIntValue(result);
}

static Value littleAdd(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_STDINT);
    assert(b.type == VALUE_TYPE_STDINT);
#endif
    return intValue(a.val.z + b.val.z);
}

static IntegerBinOp mul;

static Value bigMul(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_BIGINT);
    assert(b.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = mulBigInt(a.val.b, b.val.b);
    return bigIntValue(result);
}

static Value littleMul(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_STDINT);
    assert(b.type == VALUE_TYPE_STDINT);
#endif
    return intValue(a.val.z * b.val.z);
}

static IntegerBinOp sub;

static Value bigSub(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_BIGINT);
    assert(b.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = subBigInt(a.val.b, b.val.b);
    return bigIntValue(result);
}

static Value littleSub(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_STDINT);
    assert(b.type == VALUE_TYPE_STDINT);
#endif
    return intValue(a.val.z - b.val.z);
}

static IntegerBinOp divide;

static Value bigDivide(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_BIGINT);
    assert(b.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = divBigInt(a.val.b, b.val.b);
    return bigIntValue(result);
}

static Value littleDivide(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_STDINT);
    assert(b.type == VALUE_TYPE_STDINT);
#endif
    return intValue(a.val.z / b.val.z);
}

static IntegerBinOp power;

static Value bigPower(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_BIGINT);
    assert(b.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = powBigInt(a.val.b, b.val.b);
    return bigIntValue(result);
}

static Value littlePower(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_STDINT);
    assert(b.type == VALUE_TYPE_STDINT);
#endif
    return intValue(pow(a.val.z, b.val.z));
}

static IntegerBinOp modulo;

static Value bigModulo(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_BIGINT);
    assert(b.type == VALUE_TYPE_BIGINT);
#endif
    BigInt *result = modBigInt(a.val.b, b.val.b);
    return bigIntValue(result);
}

static Value littleModulo(Value a, Value b) {
#ifdef SAFETY_CHECKS
    assert(a.type == VALUE_TYPE_STDINT);
    assert(b.type == VALUE_TYPE_STDINT);
#endif
    return intValue(a.val.z % b.val.z);
}

static int _cmp(Value a, Value b);

static int _consCmp(Cons *a, Cons *b) {
    if (a == b) {
        return 0;
    }
    if (a == NULL) {
        return 1;
    }
    if (b == NULL) {
        return -1;
    }
    int cmp = _cmp(a->car, b->car);
    if (cmp == 0) {
        return _cmp(a->cdr, b->cdr);
    }
    return cmp;
}

static int _vecCmp(Vec *a, Vec *b) {
    if (a == b) {
        return 0;
    }
#ifdef SAFETY_CHECKS
    if (a == NULL || b == NULL) {
        cant_happen("null vecs in _vecCmp(%p, %p)", a, b);
    }
    if (a->size == 0 || b->size == 0) {
        cant_happen("empty vecs in _vecCmp()");
    }
#endif
    for (int i = 0; i < a->size; ++i) {
        int cmp = _cmp(a->values[i], b->values[i]);
        if (cmp != 0) return cmp;
    }
    return 0;
}

static int _cmp(Value a, Value b) {
#ifdef DEBUG_STEP
    eprintf("_cmp:\n");
    printContainedValue(a, 0);
    eprintf("\n");
    printContainedValue(b, 0);
    eprintf("\n");
#endif
    if (a.type != b.type) {
        cant_happen("different types in _cmp");
    }
    switch (a.type) {
        case VALUE_TYPE_VOID:
            return 0;
        case VALUE_TYPE_BIGINT:
            return cmpBigInt(a.val.b, b.val.b);
        case VALUE_TYPE_STDINT:
            return a.val.z < b.val.z ? -1 : a.val.z == b.val.z ? 0 : 1;
        case VALUE_TYPE_CHARACTER:
            return a.val.c < b.val.c ? -1 : a.val.c == b.val.c ? 0 : 1;
        case VALUE_TYPE_CONS:
            return _consCmp(a.val.cons, b.val.cons);
        case VALUE_TYPE_VEC:
            return _vecCmp(a.val.vec, b.val.vec);
        default:
            cant_happen("unexpected type for _cmp (%d)", a.type);
    }
}

static bool _eq(Value a, Value b) {
    return _cmp(a, b) == 0;
}

static bool _gt(Value a, Value b) {
    return _cmp(a, b) == 1;
}

static bool _lt(Value a, Value b) {
    return _cmp(a, b) == -1;
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
    if (index.type != VALUE_TYPE_STDINT)
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
        case VALUE_TYPE_BIGINT:
            return PROTECT(v.val.b);
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
static void applyProc(int naargs) {
    Value callable = pop();
    int save = protectValue(callable);
    switch (callable.type) {
        case VALUE_TYPE_PCLO: {
            Clo *clo = callable.val.clo;
            if (clo->nvar == naargs) {
                state.C = clo->c;
                state.E = clo->rho->next;
                copyValues(state.S.stack, clo->rho->values, clo->rho->count);
                copyValues(&(state.S.stack[clo->rho->count]), &(state.S.stack[state.S.sp - clo->nvar]), clo->nvar);
                state.S.sp = clo->rho->count + clo->nvar;
            } else if (naargs == 0) {
                push(callable);
            } else if (naargs < clo->nvar) {
                Env *e = newEnv(clo->rho->next, naargs + clo->rho->count);
                int save = PROTECT(e);
                copyValues(e->values, clo->rho->values, clo->rho->count);
                copyValues(&(e->values[clo->rho->count]), &(state.S.stack[state.S.sp - naargs]), naargs);
                Clo *pclo = newClo(clo->nvar - naargs, clo->c, e);
                PROTECT(pclo);
                callable.type = VALUE_TYPE_PCLO;
                callable.val.clo = pclo;
                push(callable);
                UNPROTECT(save);
            } else {
                cant_happen("too many arguments to partial closure, expected %d, got %d", clo->nvar, naargs);
            }
        }
        break;
        case VALUE_TYPE_CLO: {
            Clo *clo = callable.val.clo;
            if (clo->nvar == naargs) {
                state.C = clo->c;
                state.E = clo->rho;
                setFrame(&state.S, clo->nvar);
            } else if (naargs == 0) {
                push(callable);
            } else if (naargs < clo->nvar) {
                Env *e = newEnv(clo->rho, naargs);
                int save = PROTECT(e);
                copyTosToEnv(&state.S, e, naargs);
                Clo *pclo = newClo(clo->nvar - naargs, clo->c, e);
                PROTECT(pclo);
                callable.type = VALUE_TYPE_PCLO;
                callable.val.clo = pclo;
                push(callable);
                UNPROTECT(save);
            } else {
                cant_happen("too many arguments to closure, expected %d, got %d", clo->nvar, naargs);
            }
        }
        break;
        case VALUE_TYPE_CONT: {
            if (callable.val.k == NULL) {
                state.V = pop();
                state.C = UINT64_MAX;
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
            cant_happen("unexpected type %d in APPLY", callable.type);
    }
    UNPROTECT(save);
}

#define printCEKF(state)

static void step() {
#ifdef DEBUG_STEP
    int count = 0;
    dumpByteCode(&state.B);
#endif
    if (bigint_flag) {
        add = bigAdd;
        mul = bigMul;
        sub = bigSub;
        divide = bigDivide;
        power = bigPower;
        modulo = bigModulo;
    } else {
        add = littleAdd;
        mul = littleMul;
        sub = littleSub;
        divide = littleDivide;
        power = littlePower;
        modulo = littleModulo;
    }
    state.C = 0;
    while (state.C != UINT64_MAX) {
        int bytecode;
#ifdef DEBUG_STEP
        printCEKF(&state);
        printf("%4d) %04lx ### ", ++count, state.C);
#endif
        switch (bytecode = readCurrentByte()) {
            case BYTECODE_NONE: {
                cant_happen("encountered NONE in step()");
            }
            break;
            case BYTECODE_LAM: { // create a closure and push it
                int nargs = readCurrentByte();
                int letRecOffset = readCurrentByte();
                int end = readCurrentOffset();
                DEBUGPRINTF("LAM nargs:[%d] letrec:[%d] end:[%04x]\n", nargs, letRecOffset, end);
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
            case BYTECODE_VAR: { // look up an environment variable and push it
                int frame = readCurrentByte();
                int offset = readCurrentByte();
                DEBUGPRINTF("VAR [%d:%d]\n", frame, offset);
                push(lookup(frame, offset));
            }
            break;
            case BYTECODE_LVAR: { // look up a stack variable and push it
                int offset = readCurrentByte();
                DEBUGPRINTF("LVAR [%d]\n", offset);
                push(peek(offset));
            }
            break;
            case BYTECODE_PUSHN: { // allocate space for n variables on the stack
                int size = readCurrentByte();
                DEBUGPRINTF("PUSHN [%d]\n", size);
                pushN(&state.S, size);
            }
            break;
            case BYTECODE_PRIM_ADD: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("ADD\n");
                Value b = pop();
                Value a = pop();
                push(add(a, b));
            }
            break;
            case BYTECODE_PRIM_SUB: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("SUB\n");
                Value b = pop();
                Value a = pop();
                push(sub(a, b));
            }
            break;
            case BYTECODE_PRIM_MUL: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("MUL\n");
                Value b = pop();
                Value a = pop();
                push(mul(a, b));
            }
            break;
            case BYTECODE_PRIM_DIV: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("DIV\n");
                Value b = pop();
                Value a = pop();
                push(divide(a, b));
            }
            break;
            case BYTECODE_PRIM_POW: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("POW\n");
                Value b = pop();
                Value a = pop();
                push(power(a, b));
            }
            break;
            case BYTECODE_PRIM_MOD: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("MOD\n");
                Value b = pop();
                Value a = pop();
                push(modulo(a, b));
            }
            break;
            case BYTECODE_PRIM_EQ: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("EQ\n");
                Value b = pop();
                Value a = pop();
                push(eq(a, b));
            }
            break;
            case BYTECODE_PRIM_NE: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("NE\n");
                Value b = pop();
                Value a = pop();
                push(ne(a, b));
            }
            break;
            case BYTECODE_PRIM_GT: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("GT\n");
                Value b = pop();
                Value a = pop();
                push(gt(a, b));
            }
            break;
            case BYTECODE_PRIM_LT: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("LT\n");
                Value b = pop();
                Value a = pop();
                push(lt(a, b));
            }
            break;
            case BYTECODE_PRIM_GE: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("GE\n");
                Value b = pop();
                Value a = pop();
                push(ge(a, b));
            }
            break;
            case BYTECODE_PRIM_LE: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("LE\n");
                Value b = pop();
                Value a = pop();
                push(le(a, b));
            }
            break;
            case BYTECODE_PRIM_XOR: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("XOR\n");
                Value b = pop();
                Value a = pop();
                push(xor(a, b));
            }
            break;
            case BYTECODE_PRIM_CAR: { // pop value, perform the op and push the result
                DEBUGPRINTF("CAR\n");
                Value a = pop();
                push(car(a));
            }
            break;
            case BYTECODE_PRIM_CDR: { // pop value, perform the op and push the result
                DEBUGPRINTF("CDR\n");
                Value a = pop();
                push(cdr(a));
            }
            break;
            case BYTECODE_PRIM_NOT: { // pop value, perform the op and push the result
                DEBUGPRINTF("NOT\n");
                Value a = pop();
                push(not(a));
            }
            break;
            case BYTECODE_PRIM_PRINT: { // pop value, perform the op and push the result
                DEBUGPRINTF("PRINT\n");
                Value a = pop();
                push(a);
                printContainedValue(a, 0);
                printf("\n");
            }
            break;
            case BYTECODE_PRIM_CONS: { // pop two values, perform the binop and push the result
                DEBUGPRINTF("CONS\n");
                Value b = pop();
                int save = protectValue(b);
                Value a = pop();
                protectValue(a);
                Value result = cons(a, b);
                protectValue(result);
                push(result);
                UNPROTECT(save);
            }
            break;
            case BYTECODE_PRIM_VEC: {
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
            case BYTECODE_PRIM_MAKEVEC: {
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
            case BYTECODE_APPLY: { // apply the callable at the top of the stack to the arguments beneath it
                int nargs = readCurrentByte();
                DEBUGPRINTF("APPLY [%d]\n", nargs);
                applyProc(nargs);
            }
            break;
            case BYTECODE_IF: { // pop the test result and jump to the appropriate branch
                int branch = readCurrentOffset();
                DEBUGPRINTF("IF [%04x]\n", branch);
                Value aexp = pop();
                if (!truthy(aexp)) {
                    state.C = branch;
                }
            }
            break;
            case BYTECODE_MATCH: { // pop the dispach code, verify it's an integer and in range, and dispatch
                int size = readCurrentByte();
#ifdef DEBUG_STEP
                printf("MATCH [%d]", size);
                int save = state.C;
                for (int c = 0; c < size; c++) {
                    printf("[%04x]", readCurrentOffset());
                }
                state.C = save;
                printf("\n");
#endif
                Value v = pop();
                if (v.type != VALUE_TYPE_STDINT)
                    cant_happen("match expression must be an integer, expected type %d, got %d", VALUE_TYPE_STDINT, v.type);
                if (v.val.z < 0 || v.val.z >= size)
                    cant_happen("match expression index out of range (%d)", v.val.z);
                state.C = readCurrentOffsetAt(v.val.z);
            }
            break;
            case BYTECODE_INTCOND: { // pop the value, walk the dispatch table looking for a match, or run the default
                int size = readCurrentWord();
#ifdef DEBUG_STEP
                printf("INTCOND [%d]", size);
                int here = state.C;
                for (int c = 0; c < size; c++) {
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
                    for (int c = 0; c < size; c++) {
                        BigInt *bigInt = readCurrentBigInt();
                        int offset = readCurrentOffset();
                        if (cmpBigInt(bigInt, v.val.b) == 0) {
                            state.C = offset;
                            break;
                        }
                    }
                } else {
                    for (int c = 0; c < size; c++) {
                        int option = readCurrentInt();
                        int offset = readCurrentOffset();
                        if (option == v.val.z) {
                            state.C = offset;
                            break;
                        }
                    }
                }
                UNPROTECT(save);
            }
            break;
            case BYTECODE_CHARCOND: { // pop the value, walk the dispatch table looking for a match, or run the default
                int size = readCurrentWord();
#ifdef DEBUG_STEP
                printf("CHARCOND [%d]", size);
                int here = state.C;
                for (int c = 0; c < size; c++) {
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
                        option = v.val.z;
                        break;
                    case VALUE_TYPE_CHARACTER:
                        option = (int) v.val.c;
                        break;
                    default:
                        cant_happen("unexpected type %d for CHARCOND value", v.type);
                }
                for (int c = 0; c < size; c++) {
                    int val = readCurrentInt();
                    int offset = readCurrentOffset();
                    if (option == val) {
                        state.C = offset;
                        break;
                    }
                }
            }
            break;
            case BYTECODE_LETREC: { // patch each of the lambdas environments with the current stack frame
                int nargs = readCurrentByte();
                DEBUGPRINTF("LETREC [%d]\n", nargs);
                for (int i = frameSize(&state.S) - nargs; i < frameSize(&state.S); i++) {
                    Value v = peek(i);
                    if (v.type == VALUE_TYPE_CLO) {
                        patchClo(&state.S, v.val.clo);
                    } else {
                        cant_happen("non-lambda value (%d) for letrec", v.type);
                    }
                }
            }
            break;
            case BYTECODE_AMB: { // create a new failure continuation to resume at the alternative
                int branch = readCurrentOffset();
                DEBUGPRINTF("AMB [%04x]\n", branch);
                state.F = newFail(branch, state.E, state.K, state.F);
                snapshotFail(&state.S, state.F);
            }
            break;
            case BYTECODE_CUT: { // discard the current failure continuation
                DEBUGPRINTF("CUT\n");
                if (state.F == NULL) {
                    cant_happen("cut with no extant failure continuation");
                }
                state.F = state.F->next;
            }
            break;
            case BYTECODE_BACK: { // restore the failure continuation or halt
                DEBUGPRINTF("BACK\n");
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
                int offset = readCurrentOffset();
                DEBUGPRINTF("LET [%04x]\n", offset);
                state.K = newKont(offset, state.E, state.K);
                validateLastAlloc();
                snapshotKont(&state.S, state.K);
            }
            break;
            case BYTECODE_JMP: { // jump forward a specified amount
                int offset = readCurrentOffset();
                DEBUGPRINTF("JMP [%04x]\n", offset);
                state.C = offset;
            }
            break;
            case BYTECODE_CALLCC: { // pop the callable, push the current continuation, push the callable and apply
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
            case BYTECODE_TRUE: { // push true
                DEBUGPRINTF("TRUE\n");
                push(vTrue);
            }
            break;
            case BYTECODE_FALSE: { // push false
                DEBUGPRINTF("FALSE\n");
                push(vFalse);
            }
            break;
            case BYTECODE_VOID: { // push void
                DEBUGPRINTF("VOID\n");
                push(vVoid);
            }
            break;
            case BYTECODE_STDINT: { // push literal int
                int val = readCurrentInt();
                DEBUGPRINTF("STDINT [%d]\n", val);
                Value v;
                v.type = VALUE_TYPE_STDINT;
                v.val = VALUE_VAL_STDINT(val);
                push(v);
            }
            break;
            case BYTECODE_CHAR: { // push literal char
                char c = readCurrentByte();
                DEBUGPRINTF("CHAR [%c]\n", c);
                Value v;
                v.type = VALUE_TYPE_CHARACTER;
                v.val = VALUE_VAL_CHARACTER(c);
                push(v);
            }
            break;
            case BYTECODE_BIGINT: {
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
            case BYTECODE_RETURN: { // push the current continuation and apply
                DEBUGPRINTF("RETURN\n");
                Value k;
                k.type = VALUE_TYPE_CONT;
                k.val = VALUE_VAL_CONT(state.K);
                push(k);
                applyProc(1);
            }
            break;
            case BYTECODE_DONE: { // can't happen, probably
                DEBUGPRINTF("DONE\n");
                state.C = UINT64_MAX;
            }
            break;
            case BYTECODE_ERROR: {
                DEBUGPRINTF("ERROR\n");
                state.C = UINT64_MAX;
                eprintf("pattern match exhausted in step\n");
            }
            break;
            default:
                cant_happen("unrecognised bytecode %d in step()", bytecode);
        }
#ifdef DEBUG_STEP
#ifdef DEBUG_SLOW_STEP
        sleep(1);
#endif
#endif
    }
}
