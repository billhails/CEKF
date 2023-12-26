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
        case TCTYPE_TYPE_INTEGER:
            eprintf("int");
            break;
        case TCTYPE_TYPE_CHARACTER:
            eprintf("char");
            break;
        case TCTYPE_TYPE_TYPEDEF:
            ppTcTypeDef(type->val.typeDef);
            break;
        default:
            cant_happen("unrecognized type %d in ppTcType", type->type);
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
    eprintf("<%s>", var->name->name);
    if (var->instance != NULL) {
        eprintf(" [");
        ppTcType(var->instance);
        eprintf("]");
    }
}

static void ppTypeDefArgs(TcTypeDefArgs *args) {
    while (args != NULL) {
        ppTcType(args->type);
        if (args->next) eprintf(", ");
        args = args->next;
    }
}

void ppTcTypeDef(TcTypeDef *typeDef) {
    eprintf("%s(", typeDef->name->name);
    ppTypeDefArgs(typeDef->args);
    eprintf(")");
}
