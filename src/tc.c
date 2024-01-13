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
 *
 * Structures to support type inference
 *
 * Generated from src/tc.yaml by tools/makeAST.py
 */

#include "tc.h"
#include <stdio.h>
#include <strings.h>
#include "common.h"
#ifdef DEBUG_ALLOC
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

/*
 * constructor functions
 */

struct TcEnv * newTcEnv(HashTable * table, struct TcEnv * next) {
    struct TcEnv * x = NEW(TcEnv, OBJTYPE_TCENV);
    DEBUG("new TcEnv %pn", x);
    x->table = table;
    x->next = next;
    return x;
}

struct TcNg * newTcNg(HashTable * table, struct TcNg * next) {
    struct TcNg * x = NEW(TcNg, OBJTYPE_TCNG);
    DEBUG("new TcNg %pn", x);
    x->table = table;
    x->next = next;
    return x;
}

struct TcFunction * newTcFunction(struct TcType * arg, struct TcType * result) {
    struct TcFunction * x = NEW(TcFunction, OBJTYPE_TCFUNCTION);
    DEBUG("new TcFunction %pn", x);
    x->arg = arg;
    x->result = result;
    return x;
}

struct TcPair * newTcPair(struct TcType * first, struct TcType * second) {
    struct TcPair * x = NEW(TcPair, OBJTYPE_TCPAIR);
    DEBUG("new TcPair %pn", x);
    x->first = first;
    x->second = second;
    return x;
}

struct TcTypeDef * newTcTypeDef(HashSymbol * name, struct TcTypeDefArgs * args) {
    struct TcTypeDef * x = NEW(TcTypeDef, OBJTYPE_TCTYPEDEF);
    DEBUG("new TcTypeDef %pn", x);
    x->name = name;
    x->args = args;
    return x;
}

struct TcTypeDefArgs * newTcTypeDefArgs(struct TcType * type, struct TcTypeDefArgs * next) {
    struct TcTypeDefArgs * x = NEW(TcTypeDefArgs, OBJTYPE_TCTYPEDEFARGS);
    DEBUG("new TcTypeDefArgs %pn", x);
    x->type = type;
    x->next = next;
    return x;
}

struct TcVar * newTcVar(HashSymbol * name, int id) {
    struct TcVar * x = NEW(TcVar, OBJTYPE_TCVAR);
    DEBUG("new TcVar %pn", x);
    x->name = name;
    x->id = id;
    x->instance = NULL;
    return x;
}

struct TcType * newTcType(enum TcTypeType  type, union TcTypeVal  val) {
    struct TcType * x = NEW(TcType, OBJTYPE_TCTYPE);
    DEBUG("new TcType %pn", x);
    x->type = type;
    x->val = val;
    return x;
}


/*
 * copy functions
 */

struct TcEnv * copyTcEnv(struct TcEnv * o) {
    if (o == NULL) return NULL;
    struct TcEnv * x = NEW(TcEnv, OBJTYPE_TCENV);
    DEBUG("copy TcEnv %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TcEnv));
    x->header = _h;
    int save = PROTECT(x);
    x->table = o->table;
    x->next = copyTcEnv(o->next);
    UNPROTECT(save);
    return x;
}

struct TcNg * copyTcNg(struct TcNg * o) {
    if (o == NULL) return NULL;
    struct TcNg * x = NEW(TcNg, OBJTYPE_TCNG);
    DEBUG("copy TcNg %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TcNg));
    x->header = _h;
    int save = PROTECT(x);
    x->table = o->table;
    x->next = copyTcNg(o->next);
    UNPROTECT(save);
    return x;
}

struct TcFunction * copyTcFunction(struct TcFunction * o) {
    if (o == NULL) return NULL;
    struct TcFunction * x = NEW(TcFunction, OBJTYPE_TCFUNCTION);
    DEBUG("copy TcFunction %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TcFunction));
    x->header = _h;
    int save = PROTECT(x);
    x->arg = copyTcType(o->arg);
    x->result = copyTcType(o->result);
    UNPROTECT(save);
    return x;
}

struct TcPair * copyTcPair(struct TcPair * o) {
    if (o == NULL) return NULL;
    struct TcPair * x = NEW(TcPair, OBJTYPE_TCPAIR);
    DEBUG("copy TcPair %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TcPair));
    x->header = _h;
    int save = PROTECT(x);
    x->first = copyTcType(o->first);
    x->second = copyTcType(o->second);
    UNPROTECT(save);
    return x;
}

struct TcTypeDef * copyTcTypeDef(struct TcTypeDef * o) {
    if (o == NULL) return NULL;
    struct TcTypeDef * x = NEW(TcTypeDef, OBJTYPE_TCTYPEDEF);
    DEBUG("copy TcTypeDef %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TcTypeDef));
    x->header = _h;
    int save = PROTECT(x);
    x->name = o->name;
    x->args = copyTcTypeDefArgs(o->args);
    UNPROTECT(save);
    return x;
}

struct TcTypeDefArgs * copyTcTypeDefArgs(struct TcTypeDefArgs * o) {
    if (o == NULL) return NULL;
    struct TcTypeDefArgs * x = NEW(TcTypeDefArgs, OBJTYPE_TCTYPEDEFARGS);
    DEBUG("copy TcTypeDefArgs %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TcTypeDefArgs));
    x->header = _h;
    int save = PROTECT(x);
    x->type = copyTcType(o->type);
    x->next = copyTcTypeDefArgs(o->next);
    UNPROTECT(save);
    return x;
}

struct TcVar * copyTcVar(struct TcVar * o) {
    if (o == NULL) return NULL;
    struct TcVar * x = NEW(TcVar, OBJTYPE_TCVAR);
    DEBUG("copy TcVar %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TcVar));
    x->header = _h;
    int save = PROTECT(x);
    x->name = o->name;
    x->id = o->id;
    x->instance = copyTcType(o->instance);
    UNPROTECT(save);
    return x;
}

struct TcType * copyTcType(struct TcType * o) {
    if (o == NULL) return NULL;
    struct TcType * x = NEW(TcType, OBJTYPE_TCTYPE);
    DEBUG("copy TcType %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct TcType));
    x->header = _h;
    int save = PROTECT(x);
    switch(o->type) {
        case TCTYPE_TYPE_FUNCTION:
            x->val.function = copyTcFunction(o->val.function);
            break;
        case TCTYPE_TYPE_PAIR:
            x->val.pair = copyTcPair(o->val.pair);
            break;
        case TCTYPE_TYPE_VAR:
            x->val.var = copyTcVar(o->val.var);
            break;
        case TCTYPE_TYPE_SMALLINTEGER:
            x->val.smallinteger = o->val.smallinteger;
            break;
        case TCTYPE_TYPE_BIGINTEGER:
            x->val.biginteger = o->val.biginteger;
            break;
        case TCTYPE_TYPE_CHARACTER:
            x->val.character = o->val.character;
            break;
        case TCTYPE_TYPE_TYPEDEF:
            x->val.typeDef = copyTcTypeDef(o->val.typeDef);
            break;
        default:
            cant_happen("unrecognised type %d in copyTcType", o->type);
    }
    x->type = o->type;
    UNPROTECT(save);
    return x;
}


/*
 * push functions
 */


/*
 * mark functions
 */

void markTcEnv(struct TcEnv * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashTable(x->table);
    markTcEnv(x->next);
}

void markTcNg(struct TcNg * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashTable(x->table);
    markTcNg(x->next);
}

void markTcFunction(struct TcFunction * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTcType(x->arg);
    markTcType(x->result);
}

void markTcPair(struct TcPair * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTcType(x->first);
    markTcType(x->second);
}

void markTcTypeDef(struct TcTypeDef * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTcTypeDefArgs(x->args);
}

void markTcTypeDefArgs(struct TcTypeDefArgs * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTcType(x->type);
    markTcTypeDefArgs(x->next);
}

void markTcVar(struct TcVar * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTcType(x->instance);
}

void markTcType(struct TcType * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case TCTYPE_TYPE_FUNCTION:
            markTcFunction(x->val.function);
            break;
        case TCTYPE_TYPE_PAIR:
            markTcPair(x->val.pair);
            break;
        case TCTYPE_TYPE_VAR:
            markTcVar(x->val.var);
            break;
        case TCTYPE_TYPE_SMALLINTEGER:
            break;
        case TCTYPE_TYPE_BIGINTEGER:
            break;
        case TCTYPE_TYPE_CHARACTER:
            break;
        case TCTYPE_TYPE_TYPEDEF:
            markTcTypeDef(x->val.typeDef);
            break;
        default:
            cant_happen("unrecognised type %d in markTcType", x->type);
    }
}


/*
 * generic mark function
 */

void markTcObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_TCENV:
            markTcEnv((TcEnv *)h);
            break;
        case OBJTYPE_TCNG:
            markTcNg((TcNg *)h);
            break;
        case OBJTYPE_TCFUNCTION:
            markTcFunction((TcFunction *)h);
            break;
        case OBJTYPE_TCPAIR:
            markTcPair((TcPair *)h);
            break;
        case OBJTYPE_TCTYPEDEF:
            markTcTypeDef((TcTypeDef *)h);
            break;
        case OBJTYPE_TCTYPEDEFARGS:
            markTcTypeDefArgs((TcTypeDefArgs *)h);
            break;
        case OBJTYPE_TCVAR:
            markTcVar((TcVar *)h);
            break;
        case OBJTYPE_TCTYPE:
            markTcType((TcType *)h);
            break;
        default:
            cant_happen("unrecognised type %d in markTcObj\n", h->type);
    }
}

/*
 * free functions
 */

void freeTcEnv(struct TcEnv * x) {
    FREE(x, TcEnv);
}

void freeTcNg(struct TcNg * x) {
    FREE(x, TcNg);
}

void freeTcFunction(struct TcFunction * x) {
    FREE(x, TcFunction);
}

void freeTcPair(struct TcPair * x) {
    FREE(x, TcPair);
}

void freeTcTypeDef(struct TcTypeDef * x) {
    FREE(x, TcTypeDef);
}

void freeTcTypeDefArgs(struct TcTypeDefArgs * x) {
    FREE(x, TcTypeDefArgs);
}

void freeTcVar(struct TcVar * x) {
    FREE(x, TcVar);
}

void freeTcType(struct TcType * x) {
    FREE(x, TcType);
}


/*
 * generic free function
 */

void freeTcObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_TCENV:
            freeTcEnv((TcEnv *)h);
            break;
        case OBJTYPE_TCNG:
            freeTcNg((TcNg *)h);
            break;
        case OBJTYPE_TCFUNCTION:
            freeTcFunction((TcFunction *)h);
            break;
        case OBJTYPE_TCPAIR:
            freeTcPair((TcPair *)h);
            break;
        case OBJTYPE_TCTYPEDEF:
            freeTcTypeDef((TcTypeDef *)h);
            break;
        case OBJTYPE_TCTYPEDEFARGS:
            freeTcTypeDefArgs((TcTypeDefArgs *)h);
            break;
        case OBJTYPE_TCVAR:
            freeTcVar((TcVar *)h);
            break;
        case OBJTYPE_TCTYPE:
            freeTcType((TcType *)h);
            break;
        default:
            cant_happen("unrecognised type %d in freeTcObj\n", h->type);
    }
}

/*
 * type identifier function
 */

char *typenameTcObj(int type) {
    switch(type) {
        case OBJTYPE_TCENV:
            return "TcEnv";
        case OBJTYPE_TCNG:
            return "TcNg";
        case OBJTYPE_TCFUNCTION:
            return "TcFunction";
        case OBJTYPE_TCPAIR:
            return "TcPair";
        case OBJTYPE_TCTYPEDEF:
            return "TcTypeDef";
        case OBJTYPE_TCTYPEDEFARGS:
            return "TcTypeDefArgs";
        case OBJTYPE_TCVAR:
            return "TcVar";
        case OBJTYPE_TCTYPE:
            return "TcType";
        default:
            cant_happen("unrecognised type %d in typenameTcObj\n", type);
    }
}
