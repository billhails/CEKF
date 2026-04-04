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

void ppTcType(FILE *out, TcType *type) {
    if (type == NULL) {
        fprintf(out, "<null type>");
        return;
    }
    switch (type->type) {
    case TCTYPE_TYPE_FUNCTION:
        ppTcFunction(out, type->val.function);
        break;
    case TCTYPE_TYPE_PAIR:
        ppTcPair(out, type->val.pair);
        break;
    case TCTYPE_TYPE_THUNK:
        ppTcThunk(out, type->val.thunk);
        break;
    case TCTYPE_TYPE_VAR:
        ppTcVar(out, type->val.var);
        break;
    case TCTYPE_TYPE_BIGINTEGER:
        fprintf(out, "number");
        break;
    case TCTYPE_TYPE_SMALLINTEGER:
        fprintf(out, "smallint");
        break;
    case TCTYPE_TYPE_CHARACTER:
        fprintf(out, "char");
        break;
    case TCTYPE_TYPE_UNKNOWN:
        fprintf(out, "unknown:%s", type->val.unknown->name);
        break;
    case TCTYPE_TYPE_TYPESIG:
        ppTcTypeSig(out, type->val.typeSig);
        break;
    case TCTYPE_TYPE_TUPLE:
        ppTcTuple(out, type->val.tuple);
        break;
    case TCTYPE_TYPE_OPAQUE:
        fprintf(out, "opaque:%s", type->val.opaque->name);
        break;
    default:
        fprintf(out, "unrecognized type %s", tcTypeTypeName(type->type));
    }
}

void ppTcFunction(FILE *out, TcFunction *function) {
    if (function->arg->type == TCTYPE_TYPE_FUNCTION) {
        fprintf(out, "(");
        ppTcType(out, function->arg);
        fprintf(out, ")");
    } else {
        ppTcType(out, function->arg);
    }
    fprintf(out, " -> ");
    ppTcType(out, function->result);
}

void ppTcPair(FILE *out, TcPair *pair) {
    fprintf(out, "#(");
    ppTcType(out, pair->first);
    fprintf(out, ", ");
    ppTcType(out, pair->second);
    fprintf(out, ")");
}

void ppTcThunk(FILE *out, TcThunk *thunk) {
    fprintf(out, "#() -> ");
    ppTcType(out, thunk->type);
}

void ppTcVar(FILE *out, TcVar *var) {
    fprintf(out, "%s", var->name->name);
    if (var->instance != NULL) {
        fprintf(out, " [");
        ppTcType(out, var->instance);
        fprintf(out, "]");
    }
}

void ppTcTuple(FILE *out, TcTypeArray *tuple) {
    fprintf(out, "#(");
    for (Index i = 0; i < tuple->size; i++) {
        ppTcType(out, tuple->entries[i]);
        if (i + 1 < tuple->size) {
            fprintf(out, ", ");
        }
    }
    fprintf(out, ")");
}

static void ppTypeSigArgs(FILE *out, TcTypeSigArgs *args) {
    while (args != NULL) {
        ppTcType(out, args->type);
        if (args->next)
            fprintf(out, ", ");
        args = args->next;
    }
}

void ppTcTypeSig(FILE *out, TcTypeSig *typeSig) {
    fprintf(out, "%s", typeSig->name->name);
    if (typeSig->args != NULL) {
        fprintf(out, "(");
        ppTypeSigArgs(out, typeSig->args);
        fprintf(out, ")");
    }
}

static inline void pad(FILE *out, int depth) {
    fprintf(out, "%*s", depth * 2, "");
}

static void _ppTcEnv(FILE *out, TcEnv *env, int depth) {
    if (env == NULL) {
        pad(out, depth);
        fprintf(out, "<NULL> env\n");
        return;
    }
    pad(out, depth);
    fprintf(out, "{\n");
    HashSymbol *name;
    Index i = 0;
    TcType *value;
    while ((name = iterateTcTypeTable(env->table, &i, &value)) != NULL) {
        pad(out, depth);
        fprintf(out, "  %s => %s\n", name->name, tcTypeTypeName(value->type));
    }
    _ppTcEnv(out, env->next, depth + 1);
    pad(out, depth);
    fprintf(out, "}\n");
}

void ppTcEnv(FILE *out, TcEnv *env) { _ppTcEnv(out, env, 0); }
