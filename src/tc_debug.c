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

#include <stdio.h>

#include "tc_debug.h"

static void pad(int depth) { eprintf("%*s", depth * 4, ""); }

void printTcEnv(struct TcEnv * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TcEnv (NULL)"); return; }
    eprintf("TcEnv[\n");
    printHashTable(x->table, depth + 1);
    eprintf("\n");
    printTcEnv(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTcNg(struct TcNg * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TcNg (NULL)"); return; }
    eprintf("TcNg[\n");
    printHashTable(x->table, depth + 1);
    eprintf("\n");
    printTcNg(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTcFunction(struct TcFunction * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TcFunction (NULL)"); return; }
    eprintf("TcFunction[\n");
    printTcType(x->arg, depth + 1);
    eprintf("\n");
    printTcType(x->result, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTcPair(struct TcPair * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TcPair (NULL)"); return; }
    eprintf("TcPair[\n");
    printTcType(x->first, depth + 1);
    eprintf("\n");
    printTcType(x->second, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTcTypeDef(struct TcTypeDef * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TcTypeDef (NULL)"); return; }
    eprintf("TcTypeDef[\n");
    printAstSymbol(x->name, depth + 1);
    eprintf("\n");
    printTcTypeDefArgs(x->args, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTcTypeDefArgs(struct TcTypeDefArgs * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TcTypeDefArgs (NULL)"); return; }
    eprintf("TcTypeDefArgs[\n");
    printTcType(x->type, depth + 1);
    eprintf("\n");
    printTcTypeDefArgs(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTcVar(struct TcVar * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TcVar (NULL)"); return; }
    eprintf("TcVar[\n");
    printAstSymbol(x->name, depth + 1);
    eprintf("\n");
    printTcType(x->instance, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTcType(struct TcType * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TcType (NULL)"); return; }
    eprintf("TcType[\n");
    switch(x->type) {
        case TCTYPE_TYPE_FUNCTION:
            pad(depth + 1);
            eprintf("TCTYPE_TYPE_FUNCTION\n");
            printTcFunction(x->val.function, depth + 1);
            break;
        case TCTYPE_TYPE_PAIR:
            pad(depth + 1);
            eprintf("TCTYPE_TYPE_PAIR\n");
            printTcPair(x->val.pair, depth + 1);
            break;
        case TCTYPE_TYPE_VAR:
            pad(depth + 1);
            eprintf("TCTYPE_TYPE_VAR\n");
            printTcVar(x->val.var, depth + 1);
            break;
        case TCTYPE_TYPE_INTEGER:
            pad(depth + 1);
            eprintf("TCTYPE_TYPE_INTEGER\n");
            pad(depth + 1);
eprintf("void * %p", x->val.integer);
            break;
        case TCTYPE_TYPE_CHARACTER:
            pad(depth + 1);
            eprintf("TCTYPE_TYPE_CHARACTER\n");
            pad(depth + 1);
eprintf("void * %p", x->val.character);
            break;
        case TCTYPE_TYPE_TYPEDEF:
            pad(depth + 1);
            eprintf("TCTYPE_TYPE_TYPEDEF\n");
            printTcTypeDef(x->val.typeDef, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTcType", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}


/***************************************/

bool eqTcEnv(struct TcEnv * a, struct TcEnv * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->table != b->table) return false;
    if (!eqTcEnv(a->next, b->next)) return false;
    return true;
}

bool eqTcNg(struct TcNg * a, struct TcNg * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->table != b->table) return false;
    if (!eqTcNg(a->next, b->next)) return false;
    return true;
}

bool eqTcFunction(struct TcFunction * a, struct TcFunction * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqTcType(a->arg, b->arg)) return false;
    if (!eqTcType(a->result, b->result)) return false;
    return true;
}

bool eqTcPair(struct TcPair * a, struct TcPair * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqTcType(a->first, b->first)) return false;
    if (!eqTcType(a->second, b->second)) return false;
    return true;
}

bool eqTcTypeDef(struct TcTypeDef * a, struct TcTypeDef * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->name != b->name) return false;
    if (!eqTcTypeDefArgs(a->args, b->args)) return false;
    return true;
}

bool eqTcTypeDefArgs(struct TcTypeDefArgs * a, struct TcTypeDefArgs * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqTcType(a->type, b->type)) return false;
    if (!eqTcTypeDefArgs(a->next, b->next)) return false;
    return true;
}

bool eqTcVar(struct TcVar * a, struct TcVar * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->name != b->name) return false;
    if (!eqTcType(a->instance, b->instance)) return false;
    return true;
}

bool eqTcType(struct TcType * a, struct TcType * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case TCTYPE_TYPE_FUNCTION:
            if (!eqTcFunction(a->val.function, b->val.function)) return false;
            break;
        case TCTYPE_TYPE_PAIR:
            if (!eqTcPair(a->val.pair, b->val.pair)) return false;
            break;
        case TCTYPE_TYPE_VAR:
            if (!eqTcVar(a->val.var, b->val.var)) return false;
            break;
        case TCTYPE_TYPE_INTEGER:
            if (a->val.integer != b->val.integer) return false;
            break;
        case TCTYPE_TYPE_CHARACTER:
            if (a->val.character != b->val.character) return false;
            break;
        case TCTYPE_TYPE_TYPEDEF:
            if (!eqTcTypeDef(a->val.typeDef, b->val.typeDef)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqTcType", a->type);
    }
    return true;
}

