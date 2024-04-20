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
#include "symbol.h"

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
    for (int i = 0; i < tuple->size; i++) {
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
    eprintf("%s(", userType->name->name);
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
