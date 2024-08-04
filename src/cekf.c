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
#include "common.h"
#include "cekf.h"
#include "memory.h"
#include "hash.h"
#include "debug.h"
#ifdef DEBUG_STACK
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

/*
 * constants and memory allocation functions for the CEKF machine
 */

Value vTrue = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(1)
};

Value vFalse = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(0)
};

Value vLt = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(0)
};

Value vEq = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(1)
};

Value vGt = {
    .type = VALUE_TYPE_STDINT,
    .val = VALUE_VAL_STDINT(2)
};

Value vVoid = {
    .type = VALUE_TYPE_NONE,
    .val = VALUE_VAL_NONE()
};

Env *makeEnv(Env *parent) {
    Stack *s = newStack();
    int save = PROTECT(s);
    Env *new = newEnv(s, parent);
    UNPROTECT(save);
    return new;
}

Kont *makeKont(Control offset, Env *env, Kont *next) {
    Stack *s = newStack();
    int save = PROTECT(s);
    Kont *k = newKont(offset, env, s, next);
    UNPROTECT(save);
    return k;
}

Fail *makeFail(Control offset, Env *env, Kont *k, Fail *next) {
    Stack *s = newStack();
    int save = PROTECT(s);
    Fail *f = newFail(offset, env, k, s, next);
    UNPROTECT(save);
    return f;
}

void dumpStack(Stack *s) {
    eprintf("=================================\n");
    eprintf("STACK DUMP sp = %d, capacity = %d\n", s->size, s->capacity);
    for (Index i = 0; i < s->size; i++) {
        eprintf("[%03d] *** %s\n", i, valueTypeName(s->entries[i].type));
    }
    eprintf("=================================\n");
}

void copyValues(Value *to, Value *from, int size) {
    COPY_ARRAY(Value, to, from, size);
}

void copyTosToVec(Vec *vec, Stack *s) {
#ifdef SAFETY_CHECKS
    if (vec->size > s->size) {
        cant_happen("copy too big %u/%u", vec->size, s->size);
    }
#endif
    copyValues(vec->entries, &(s->entries[s->size - vec->size]), vec->size);
}

void copyTosToEnv(Env *e, Stack *s, int n) {
    copyTopStack(e->stack, s, n);
}

void snapshotClo(Clo *target, Stack *s, int letRecOffset) {
    Env *env = makeEnv(target->env);
    target->env = env;
    copyExceptTopStack(env->stack, s, letRecOffset);
}

void patchVec(Vec *v, Stack *s, int num) {
#ifdef SAFETY_CHECKS
    if (num < 0) {
        cant_happen("negative count");
    }
    if (num > (int) s->size) {
        cant_happen("not enough values on stack");
    }
    if (s->size > v->size) {
        cant_happen("not enough space in target");
    }
#endif
    int base = s->size - num;
    copyValues(&v->entries[base], &s->entries[base], num);
}

void restoreNamespace(Stack *s, Vec *vl) {
#ifdef SAFETY_CHECKS
    if (vl->size > s->capacity) {
        cant_happen("copy too big %d/%d", vl->size, s->capacity);
    }
#endif
    copyValues(s->entries, vl->entries, vl->size);
    s->size = vl->size;
}

Vec *snapshotNamespace(Stack *s) {
    Vec *vl = newVec(s->size);
    copyValues(vl->entries, s->entries, s->size);
    return vl;
}

void patchClo(Clo *target, Stack *s) {
    copyStackEntries(target->env->stack, s);
}

void snapshotKont(Kont *target, Stack *s) {
    copyStackEntries(target->stack, s);
}

void snapshotFail(Fail *target, Stack *s) {
    copyStackEntries(target->stack, s);
}

void restoreKont(Stack *s, Kont *source) {
    copyStackEntries(s, source->stack);
}

void restoreFail(Stack *s, Fail *source) {
    copyStackEntries(s, source->stack);
}

CharArray *listToCharArray(Value list) {
    CharArray *chars = newCharArray();
    int save = PROTECT(chars);
    while (list.val.vec) {
#ifdef SAFETY_CHECKS
        if (list.type != VALUE_TYPE_VEC) {
            cant_happen("unexpected %s", valueTypeName(list.type));
        }
#endif
        Vec *v = list.val.vec;
#ifdef SAFETY_CHECKS
        if (v->size != 3 && v->size != 1) {
            cant_happen("unexpected %d", v->size);
        }
#endif
        if (v->size == 3) {
            Value character = v->entries[1];
#ifdef SAFETY_CHECKS
            if (character.type != VALUE_TYPE_CHARACTER) {
                cant_happen("unexpected %s", valueTypeName(character.type));
            }
#endif
            pushCharArray(chars, character.val.character);
            list = v->entries[2];
        } else {
            list.val.vec = NULL;
        }
    }
    pushCharArray(chars, (Character) 0); // null terminated
    UNPROTECT(save);
    return chars;
}

Value charArrayToList(CharArray *c) {
    Vec *nullByte = newVec(1);
    nullByte->entries[0] = value_Stdint(0); // tag=null
    Value v = value_Vec(nullByte);
    int save = protectValue(v);
    for (Index i = c->size; i > 0; --i) {
        Character cc = c->entries[i - 1];
        if (cc) {
            Vec *pair = newVec(3);
            pair->entries[0] = value_Stdint(1); // tag=pair
            pair->entries[1] = value_Character(cc);
            pair->entries[2] = v;
            v = value_Vec(pair);
            protectValue(v);
        }
    }
    UNPROTECT(save);
    return v;
}

Value makeNull(void) {
    Vec *vec = newVec(1);
    vec->entries[0] = value_Stdint(0);
    return value_Vec(vec);
}

Value makePair(Value car, Value cdr) {
    Vec *vec = newVec(3);
    vec->entries[0] = value_Stdint(1);
    vec->entries[1] = car;
    vec->entries[2] = cdr;
    return value_Vec(vec);
}
