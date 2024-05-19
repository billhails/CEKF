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

#include "tc_helper.h"
#include "tc_analyze.h"
#include "symbol.h"
#include "types.h"

void ppTcType(TcType *type) {
    if (type == NULL) {
        eprintf("<null type>");
        return;
    }
    switch (type->type) {
        case TCTYPE_TYPE_FUNCTION:
            ppTcFunction(type->val.function);
            break;
        case TCTYPE_TYPE_PAIR:
            ppTcPair(type->val.pair);
            break;
        case TCTYPE_TYPE_VAR:
            ppTcVar(type->val.var);
            break;
        case TCTYPE_TYPE_BIGINTEGER:
            eprintf("bigint");
            break;
        case TCTYPE_TYPE_SMALLINTEGER:
            eprintf("smallint");
            break;
        case TCTYPE_TYPE_CHARACTER:
            eprintf("char");
            break;
        case TCTYPE_TYPE_UNKNOWN:
            eprintf("unknown:%s", type->val.unknown->name);
            break;
        case TCTYPE_TYPE_USERTYPE:
            ppTcUserType(type->val.userType);
            break;
        case TCTYPE_TYPE_TUPLE:
            ppTcTuple(type->val.tuple);
            break;
        case TCTYPE_TYPE_ENV:
            eprintf("<env>");
            break;
        default:
            eprintf("unrecognized type %s", tcTypeTypeName(type->type));
    }
}

void ppTcFunction(TcFunction *function) {
    eprintf("(");
    ppTcType(function->arg);
    eprintf(") -> ");
    ppTcType(function->result);
}

void ppTcPair(TcPair *pair) {
    eprintf("#(");
    ppTcType(pair->first);
    eprintf(", ");
    ppTcType(pair->second);
    eprintf(")");
}

void ppTcVar(TcVar *var) {
    eprintf("%s", var->name->name);
    if (var->instance != NULL) {
        eprintf(" [");
        ppTcType(var->instance);
        eprintf("]");
    }
}

void ppTcTuple(TcTypeArray *tuple) {
    eprintf("#(");
    for (Index i = 0; i < tuple->size; i++) {
        ppTcType(tuple->entries[i]);
        if (i + 1 < tuple->size) {
            eprintf(", ");
        }
    }
    eprintf(")");
}

static void ppUserTypeArgs(TcUserTypeArgs *args) {
    while (args != NULL) {
        ppTcType(args->type);
        if (args->next)
            eprintf(", ");
        args = args->next;
    }
}

void ppTcUserType(TcUserType *userType) {
    eprintf("%s:%d(", userType->name->name, userType->ns);
    ppUserTypeArgs(userType->args);
    eprintf(")");
}

bool eqTcVar(struct TcVar *a, struct TcVar *b, HashTable *map) {
    if (a == b)
        return true;
    if (a->name == b->name)
        return true;
    HashSymbol *common = NULL;
    if (hashGet(map, a->name, &common)) {
        HashSymbol *other = NULL;
        if (hashGet(map, b->name, &other)) {
            return common == other;
        } else {
            return false;
        }
    } else if (hashGet(map, b->name, &common)) {
        return false;
    } else {
        // symmetric
        common = genSym("tt$");
        hashSet(map, a->name, &common);
        hashSet(map, b->name, &common);
    }
    return true;
}

static inline void pad(int depth) {
    eprintf("%*s", depth, "");
}

static void _ppTcEnv(TcEnv *env, int depth, bool done_namespaces);

static void _ppTcNamespaces(TcNamespaceArray *namespaces, int depth) {
    if (namespaces == NULL) return;
    for (Index i = 0; i < namespaces->size; i++) {
        pad(depth);
        eprintf("[%u]: ", i);
        if (namespaces->entries[i]->type == TCTYPE_TYPE_ENV) {
            _ppTcEnv(namespaces->entries[i]->val.env, depth + 1, true);
        } else {
            eprintf("%s\n", tcTypeTypeName(namespaces->entries[i]->type));
        }
    }
}

static void _ppTcEnv(TcEnv *env, int depth, bool done_namespaces) {
    if (env == NULL) {
        pad(depth);
        eprintf("<NULL> env\n");
        return;
    }
    pad(depth);
    eprintf("{\n");
    HashSymbol *name;
    Index i = 0;
    TcType *value;
    while ((name = iterateTcTypeTable(env->table, &i, &value)) != NULL) {
        pad(depth);
        if (value->type == TCTYPE_TYPE_NAMESPACE) {
            eprintf(" %s => %s [%d]\n", name->name, tcTypeTypeName(value->type), value->val.namespace);
        } else if (value->type == TCTYPE_TYPE_NAMESPACES) {
            if (done_namespaces) {
                eprintf(" %s => %s\n", name->name, tcTypeTypeName(value->type));
            } else {
                eprintf(" %s => %s [\n", name->name, tcTypeTypeName(value->type));
                _ppTcNamespaces(value->val.namespaces, depth + 1);
                pad(depth);
                eprintf(" ]\n");
            }
        } else {
            eprintf(" %s => %s\n", name->name, tcTypeTypeName(value->type));
        }
    }
    _ppTcEnv(env->next, depth + 1, done_namespaces);
    pad(depth);
    eprintf("}\n");
}

void ppTcEnv(TcEnv *env) {
    _ppTcEnv(env, 0, false);
}
