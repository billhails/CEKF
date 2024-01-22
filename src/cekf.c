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
#include "memory.h"
#include "hash.h"

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
    .type = VALUE_TYPE_VOID,
    .val = VALUE_VAL_NONE()
};

ValueList *newValueList(int count) {
    ValueList *x = NEW(ValueList, OBJTYPE_VALUELIST);
    int save = PROTECT(x);
    x->count = 0;
    x->values = NEW_ARRAY(Value, count);
    for (int i = 0; i < count; ++i) {
        x->values[i] = vVoid;
    }
    x->count = count;
    UNPROTECT(save);
    return x;
}

Clo *newClo(int nvar, Control c, Env *rho) {
    Clo *x = NEW(Clo, OBJTYPE_CLO);
    x->nvar = nvar;
    x->c = c;
    x->rho = rho;
    return x;
}

Env *newEnv(Env *next, int count) {
    Env *x = NEW(Env, OBJTYPE_ENV);
    int save = PROTECT(x);
    x->next = next;
    x->count = 0;
    x->values = NULL;
    if (count > 0) {
        x->values = NEW_ARRAY(Value, count);
        x->count = count;
        for (int i = 0; i < count; i++) {
            x->values[i] = vVoid;
        }
    }
    UNPROTECT(save);
    return x;
}

Kont *newKont(Control body, Env *rho, Kont *next) {
    Kont *x = NEW(Kont, OBJTYPE_KONT);
    x->body = body;
    x->rho = rho;
    x->next = next;
    x->snapshot = noSnapshot;
    return x;
}

Fail *newFail(Control exp, Env *rho, Kont *k, Fail *next) {
    Fail *x = NEW(Fail, OBJTYPE_FAIL);
    x->exp = exp;
    x->rho = rho;
    x->k = k;
    x->next = next;
    x->snapshot = noSnapshot;
    return x;
}

Vec *newVec(int size) {
    Vec *x = NEW_VEC(size);
    x->size = size;
    for (int i = 0; i < size; i++) {
        x->values[i] = vVoid;
    }
    return x;
}

void markValue(Value x) {
    switch (x.type) {
        case VALUE_TYPE_VOID:
        case VALUE_TYPE_STDINT:
        case VALUE_TYPE_CHARACTER:
            break;
        case VALUE_TYPE_PCLO:
        case VALUE_TYPE_CLO:
            markClo(x.val.clo);
            break;
        case VALUE_TYPE_CONT:
            markKont(x.val.k);
            break;
        case VALUE_TYPE_VEC:
            markVec(x.val.vec);
            break;
        case VALUE_TYPE_BIGINT:
            markBigInt(x.val.b);
            break;
        default:
            cant_happen("unrecognised type in markValue (%d)", x.type);
    }
}

void markValueList(ValueList *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->count; ++i) {
        markValue(x->values[i]);
    }
}

void markClo(Clo *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markEnv(x->rho);
}

void markEnv(Env *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markEnv(x->next);
    for (int i = 0; i < x->count; i++) {
        markValue(x->values[i]);
    }
}

static void markSnapshot(Snapshot s) {
    for (int i = 0; i < s.frameSize; i++) {
        markValue(s.frame[i]);
    }
}

void markKont(Kont *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markSnapshot(x->snapshot);
    markEnv(x->rho);
    markKont(x->next);
}

void markCons(Cons *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markValue(x->car);
    markValue(x->cdr);
}

void markVec(Vec *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    for (int i = 0; i < x->size; i++) {
        markValue(x->values[i]);
    }
}

void markFail(Fail *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markSnapshot(x->snapshot);
    markEnv(x->rho);
    markKont(x->k);
    markFail(x->next);
}

void markCekfObj(Header *h) {
    switch (h->type) {
        case OBJTYPE_CLO:
            markClo((Clo *)h);
            break;
        case OBJTYPE_ENV:
            markEnv((Env *)h);
            break;
        case OBJTYPE_FAIL:
            markFail((Fail *)h);
            break;
        case OBJTYPE_KONT:
            markKont((Kont *)h);
            break;
        case OBJTYPE_CONS:
            markCons((Cons *)h);
            break;
        case OBJTYPE_VALUELIST:
            markValueList((ValueList *)h);
            break;
        default:
            cant_happen("unrecognised header type in markCekfObj");
    }
}

void freeCekfObj(Header *h) {
    switch (h->type) {
        case OBJTYPE_CONS:
            reallocate((void *)h, sizeof(Cons), 0);
            break;
        case OBJTYPE_VEC:
            Vec *vec = (Vec *)h;
            FREE_VEC(vec);
            break;
        case OBJTYPE_CLO:
            reallocate((void *)h, sizeof(Clo), 0);
            break;
        case OBJTYPE_ENV: {
                Env *env = (Env *)h;
                FREE_ARRAY(Value, env->values, env->count);
                reallocate((void *)h, sizeof(Env), 0);
            }
            break;
        case OBJTYPE_FAIL: {
                Fail *f = (Fail *)h;
                FREE_ARRAY(Value, f->snapshot.frame, f->snapshot.frameSize);
                reallocate((void *)h, sizeof(Fail), 0);
            }
            break;
        case OBJTYPE_KONT: {
                Kont *k = (Kont *)h;
                FREE_ARRAY(Value, k->snapshot.frame, k->snapshot.frameSize);
                reallocate((void *)h, sizeof(Kont), 0);
            }
            break;
        case OBJTYPE_VALUELIST: {
                ValueList *vl = (ValueList *)h;
                if (vl->count > 0) {
                    FREE_ARRAY(Value, vl->values, vl->count);
                }
                reallocate((void *)h, sizeof(ValueList), 0);
            }
            break;
        default:
            cant_happen("unrecognised header type in freeCekfObj");
    }
}
