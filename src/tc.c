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
 * generated from src/tc.yaml by makeAST.py
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

struct TcVar * newTcVar(HashSymbol * name) {
    struct TcVar * x = NEW(TcVar, OBJTYPE_TCVAR);
    DEBUG("new TcVar %pn", x);
    x->name = name;
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



/************************************/

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
        case TCTYPE_TYPE_INTEGER:
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

/************************************/

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

