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
 * Type inference structures used by Algorithm W.
 *
 * generated from src/tin.yaml by makeAST.py
 */

#include <stdio.h>

#include "debug_tin.h"

static void pad(int depth) { eprintf("%*s", depth * 4, ""); }

void printTinFunctionApplication(struct TinFunctionApplication * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinFunctionApplication (NULL)"); return; }
    eprintf("TinFunctionApplication[\n");
        printTinSymbol(x->name, depth + 1);
    eprintf("\n");
        pad(depth + 1);
eprintf("int %d", x->nargs);
    eprintf("\n");
    printTinMonoTypeList(x->args, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinMonoTypeList(struct TinMonoTypeList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinMonoTypeList (NULL)"); return; }
    eprintf("TinMonoTypeList[\n");
    printTinMonoType(x->monoType, depth + 1);
    eprintf("\n");
    printTinMonoTypeList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinTypeQuantifier(struct TinTypeQuantifier * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinTypeQuantifier (NULL)"); return; }
    eprintf("TinTypeQuantifier[\n");
        printTinSymbol(x->var, depth + 1);
    eprintf("\n");
    printTinPolyType(x->quantifiedType, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinContext(struct TinContext * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinContext (NULL)"); return; }
    eprintf("TinContext[\n");
        printHashTable(x->varFrame, depth + 1);
    eprintf("\n");
        printHashTable(x->tcFrame, depth + 1);
    eprintf("\n");
    printTinContext(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinSubstitution(struct TinSubstitution * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinSubstitution (NULL)"); return; }
    eprintf("TinSubstitution[\n");
        printHashTable(x->map, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinArgsResult(struct TinArgsResult * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinArgsResult (NULL)"); return; }
    eprintf("TinArgsResult[\n");
    printTinContext(x->context, depth + 1);
    eprintf("\n");
    printTinMonoTypeList(x->vec, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinVarResult(struct TinVarResult * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinVarResult (NULL)"); return; }
    eprintf("TinVarResult[\n");
    printTinSubstitution(x->substitution, depth + 1);
    eprintf("\n");
    printTinContext(x->context, depth + 1);
    eprintf("\n");
    printTinMonoType(x->monoType, depth + 1);
    eprintf("\n");
        printHashTable(x->set, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinVarsResult(struct TinVarsResult * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinVarsResult (NULL)"); return; }
    eprintf("TinVarsResult[\n");
    printTinContext(x->context, depth + 1);
    eprintf("\n");
        printHashTable(x->set, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinMonoType(struct TinMonoType * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinMonoType (NULL)"); return; }
    eprintf("TinMonoType[\n");
    switch(x->type) {
        case TINMONOTYPE_TYPE_VAR:
            pad(depth + 1);
            eprintf("TINMONOTYPE_TYPE_VAR\n");
                        printTinSymbol(x->val.var, depth + 1);
            break;
        case TINMONOTYPE_TYPE_FUN:
            pad(depth + 1);
            eprintf("TINMONOTYPE_TYPE_FUN\n");
            printTinFunctionApplication(x->val.fun, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTinMonoType", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printTinPolyType(struct TinPolyType * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("TinPolyType (NULL)"); return; }
    eprintf("TinPolyType[\n");
    switch(x->type) {
        case TINPOLYTYPE_TYPE_MONOTYPE:
            pad(depth + 1);
            eprintf("TINPOLYTYPE_TYPE_MONOTYPE\n");
            printTinMonoType(x->val.monoType, depth + 1);
            break;
        case TINPOLYTYPE_TYPE_QUANTIFIER:
            pad(depth + 1);
            eprintf("TINPOLYTYPE_TYPE_QUANTIFIER\n");
            printTinTypeQuantifier(x->val.quantifier, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTinPolyType", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

