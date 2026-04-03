/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "tc_pp.h"

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
    case TCTYPE_TYPE_THUNK:
        ppTcThunk(type->val.thunk);
        break;
    case TCTYPE_TYPE_VAR:
        ppTcVar(type->val.var);
        break;
    case TCTYPE_TYPE_BIGINTEGER:
        eprintf("number");
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
    case TCTYPE_TYPE_TYPESIG:
        ppTcTypeSig(type->val.typeSig);
        break;
    case TCTYPE_TYPE_TUPLE:
        ppTcTuple(type->val.tuple);
        break;
    case TCTYPE_TYPE_OPAQUE:
        eprintf("opaque:%s", type->val.opaque->name);
        break;
    default:
        eprintf("unrecognized type %s", tcTypeTypeName(type->type));
    }
}

void ppTcFunction(TcFunction *function) {
    if (function->arg->type == TCTYPE_TYPE_FUNCTION) {
        eprintf("(");
        ppTcType(function->arg);
        eprintf(")");
    } else {
        ppTcType(function->arg);
    }
    eprintf(" -> ");
    ppTcType(function->result);
}

void ppTcPair(TcPair *pair) {
    eprintf("#(");
    ppTcType(pair->first);
    eprintf(", ");
    ppTcType(pair->second);
    eprintf(")");
}

void ppTcThunk(TcThunk *thunk) {
    eprintf("#() -> ");
    ppTcType(thunk->type);
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

static void ppTypeSigArgs(TcTypeSigArgs *args) {
    while (args != NULL) {
        ppTcType(args->type);
        if (args->next)
            eprintf(", ");
        args = args->next;
    }
}

void ppTcTypeSig(TcTypeSig *typeSig) {
    eprintf("%s", typeSig->name->name);
    if (typeSig->args != NULL) {
        eprintf("(");
        ppTypeSigArgs(typeSig->args);
        eprintf(")");
    }
}

static inline void pad(int depth) { eprintf("%*s", depth * 2, ""); }

static void _ppTcEnv(TcEnv *env, int depth) {
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
        eprintf("  %s => %s\n", name->name, tcTypeTypeName(value->type));
    }
    _ppTcEnv(env->next, depth + 1);
    pad(depth);
    eprintf("}\n");
}

void ppTcEnv(TcEnv *env) { _ppTcEnv(env, 0); }
