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
#include "cekf.h"
#include "common.h"
#include "debug.h"
#include "hash.h"
#include "memory.h"
#ifdef DEBUG_STACK
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

#ifdef SAFETY_CHECKS
static int cont_pops = 0;
static int cont_restores = 0;
#endif

static CharacterArray *listToCharArray(Value list);
static Value charArrayToList(CharacterArray *c);

/*
 * constants and memory allocation functions for the CEKF machine
 */

Value vTrue = {.type = VALUE_TYPE_STDINT, .val = VALUE_VAL_STDINT(1)};

Value vFalse = {.type = VALUE_TYPE_STDINT, .val = VALUE_VAL_STDINT(0)};

Value vLt = {.type = VALUE_TYPE_STDINT, .val = VALUE_VAL_STDINT(0)};

Value vEq = {.type = VALUE_TYPE_STDINT, .val = VALUE_VAL_STDINT(1)};

Value vGt = {.type = VALUE_TYPE_STDINT, .val = VALUE_VAL_STDINT(2)};

Value vVoid = {.type = VALUE_TYPE_NONE, .val = VALUE_VAL_NONE()};

Env *makeEnv(Env *parent) {
    Frame *s = newFrame();
    int save = PROTECT(s);
    Env *new = newEnv(s, parent);
    UNPROTECT(save);
    return new;
}

Kont *makeKont(Control offset, Env *env, bool makeStack, Kont *next) {
    DEBUG("makeKont(%04lx, %p, %d, %p)", offset, env, makeStack, next);
    Stack *s = NULL;
    if (makeStack) {
        s = newStack();
    }
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

#ifdef DEBUG_STEP
void dumpStack(Stack *s) {
    for (Index i = 0; i < s->frames_index; i++) {
        for (Index j = 0; j < s->frames[i].offset; j++) {
            if (j == 0)
                eprintf("      [%03d] [%03d] %s\n", i, j,
                        valueTypeName(s->entries[s->frames[i].frame + j].type));
            else
                eprintf("            [%03d] %s\n", j,
                        valueTypeName(s->entries[s->frames[i].frame + j].type));
        }
    }
    for (Index i = 0; i < s->offset; i++) {
        if (i == 0)
            eprintf("      [%03d] [%03d] %s\n", s->frames_index, i,
                    valueTypeName(s->entries[s->frame + i].type));
        else
            eprintf("            [%03d] %s\n", i,
                    valueTypeName(s->entries[s->frame + i].type));
    }
}
#endif

#ifdef DEBUG_STEP
void dumpFrame(Frame *s) {
    eprintf("=================================\n");
    eprintf("FRAME DUMP sp = %d, capacity = %d\n", s->size, s->capacity);
    for (Index i = 0; i < s->size; i++) {
        eprintf("[%03d] %s\n", i, valueTypeName(s->entries[i].type));
    }
    eprintf("=================================\n");
}
#endif

void copyValues(Value *to, Value *from, int size) {
    COPY_ARRAY(Value, to, from, size);
}

void copyTosToVec(Vec *vec, Stack *s) {
#ifdef SAFETY_CHECKS
    if (vec->size > s->offset) {
        cant_happen("copy too big %u > %u", vec->size, s->offset);
    }
#endif
    copyValues(vec->entries, &(s->entries[s->frame + s->offset - vec->size]),
               vec->size);
}

void copyTosToEnv(Env *e, Stack *s, int n) {
    extendFrame(e->S, n);
    copyValues(e->S->entries, &s->entries[s->frame + s->offset - n], n);
    e->S->size = n;
}

void copyStackToFrame(Stack *src, Frame *dest, int n) {
    DEBUG("copyStackToFrame %p -> %p, %d-%d", src, dest, src->offset, n);
#ifdef SAFETY_CHECKS
    if ((int)src->offset - n < 0) {
        cant_happen("stack underflow %d / %u", n, src->offset);
    }
#endif
    if ((Index)n < src->offset) {
        extendFrame(dest, src->offset - n);
        copyValues(dest->entries, &src->entries[src->frame], src->offset - n);
    }
    dest->size = src->offset - n;
}

void snapshotClo(Clo *target, Stack *s, int n) {
    Env *env = makeEnv(target->E);
    target->E = env;
    copyStackToFrame(s, env->S, n);
}

void patchVec(Vec *v, Stack *s, int num) {
#ifdef SAFETY_CHECKS
    if (num < 0) {
        cant_happen("negative count");
    }
    if (num > (int)s->offset) {
        cant_happen("not enough values on stack");
    }
    if (s->offset > v->size) {
        cant_happen("not enough space in target");
    }
#endif
    int base = s->offset - num;
    copyValues(&v->entries[base], &s->entries[s->frame + base], num);
}

void restoreNameSpace(Stack *s, Vec *vl) {
    extendStackEntries(s, s->frame + vl->size);
#ifdef SAFETY_CHECKS
    if (vl->size > (s->entries_capacity - s->frame)) {
        cant_happen("copy too big %d/%d", vl->size,
                    s->entries_capacity - s->frame);
    }
#endif
    copyValues(&s->entries[s->frame], vl->entries, vl->size);
    s->offset = vl->size;
}

Vec *snapshotNameSpace(Stack *s) {
    Vec *vl = newVec(s->offset);
    copyValues(vl->entries, &s->entries[s->frame], s->offset);
    return vl;
}

void patchClo(Clo *target, Stack *s) { copyStackToFrame(s, target->E->S, 0); }

void snapshotKont(Kont *target, Stack *s) { copyAllStackEntries(target->S, s); }

void snapshotFail(Fail *target, Stack *s) { copyAllStackEntries(target->S, s); }

void restoreKont(Stack *s, Kont *source) {
    if (source->S == NULL) {
        popStackFrame(s);
#ifdef SAFETY_CHECKS
        cont_pops++;
#endif
    } else {
        copyAllStackEntries(s, source->S);
#ifdef SAFETY_CHECKS
        cont_restores++;
#endif
    }
}

#ifdef SAFETY_CHECKS
void reportKonts() {
    printf("stack pops %d\n", cont_pops);
    printf("stack restores %d\n", cont_restores);
}
#endif

void restoreFail(Stack *s, Fail *source) { copyAllStackEntries(s, source->S); }

static CharacterArray *listToCharArray(Value list) {
    CharacterArray *chars = newCharacterArray();
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
            pushCharacterArray(chars, character.val.character);
            list = v->entries[2];
        } else {
            list.val.vec = NULL;
        }
    }
    pushCharacterArray(chars, (Character)0); // null terminated
    UNPROTECT(save);
    return chars;
}

static Value charArrayToList(CharacterArray *c) {
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

// converts a list of char to a utf8 string.
CharVec *listToUtf8(Value v) {
#ifdef SAFETY_CHECKS
    if (v.type != VALUE_TYPE_VEC) {
        cant_happen("unexpected %s", valueTypeName(v.type));
    }
#endif
    CharacterArray *unicode = listToCharArray(v);
    int save = PROTECT(unicode);
    size_t size = wcstombs(NULL, unicode->entries, 0);
    CharVec *buf = newCharVec((int)(size + 1));
    PROTECT(buf);
    wcstombs(buf->entries, unicode->entries, size + 1);
    UNPROTECT(save);
    return buf;
}

// converts a utf8 string to a list of char (Value)
// returns the empty list if the string is invalid
Value utf8ToList(const char *utf8) {
    size_t size = mbstowcs(NULL, utf8, 0);
    CharacterArray *unicode = newCharacterArray();
    int save = PROTECT(unicode);
    if (size == (size_t)-1) {
        pushCharacterArray(unicode, (Character)0);
    } else {
        extendCharacterArray(unicode, (Index)(size + 1));
        mbstowcs(unicode->entries, utf8, size + 1);
        unicode->size = (Index)(size + 1);
    }
    Value v = charArrayToList(unicode);
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
