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

static void pad(int depth) { fprintf(stderr, "%*s", depth * 4, ""); }

void printTinFunctionApplication(struct TinFunctionApplication * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinFunctionApplication (NULL)"); return; }
    fprintf(stderr, "TinFunctionApplication[\n");
        printTinSymbol(x->name, depth + 1);
    fprintf(stderr, "\n");
        pad(depth + 1);
fprintf(stderr, "int %d", x->nargs);
    fprintf(stderr, "\n");
    printTinMonoTypeList(x->args, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinMonoTypeList(struct TinMonoTypeList * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinMonoTypeList (NULL)"); return; }
    fprintf(stderr, "TinMonoTypeList[\n");
    printTinMonoType(x->monoType, depth + 1);
    fprintf(stderr, "\n");
    printTinMonoTypeList(x->next, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinTypeQuantifier(struct TinTypeQuantifier * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinTypeQuantifier (NULL)"); return; }
    fprintf(stderr, "TinTypeQuantifier[\n");
        printTinSymbol(x->var, depth + 1);
    fprintf(stderr, "\n");
    printTinPolyType(x->quantifiedType, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinContext(struct TinContext * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinContext (NULL)"); return; }
    fprintf(stderr, "TinContext[\n");
        printHashTable(x->varFrame, depth + 1);
    fprintf(stderr, "\n");
        printHashTable(x->tcFrame, depth + 1);
    fprintf(stderr, "\n");
    printTinContext(x->next, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinSubstitution(struct TinSubstitution * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinSubstitution (NULL)"); return; }
    fprintf(stderr, "TinSubstitution[\n");
        printHashTable(x->map, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinArgsResult(struct TinArgsResult * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinArgsResult (NULL)"); return; }
    fprintf(stderr, "TinArgsResult[\n");
    printTinContext(x->context, depth + 1);
    fprintf(stderr, "\n");
    printTinMonoTypeList(x->vec, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinVarResult(struct TinVarResult * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinVarResult (NULL)"); return; }
    fprintf(stderr, "TinVarResult[\n");
    printTinSubstitution(x->substitution, depth + 1);
    fprintf(stderr, "\n");
    printTinContext(x->context, depth + 1);
    fprintf(stderr, "\n");
    printTinMonoType(x->monoType, depth + 1);
    fprintf(stderr, "\n");
        printHashTable(x->set, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinVarsResult(struct TinVarsResult * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinVarsResult (NULL)"); return; }
    fprintf(stderr, "TinVarsResult[\n");
    printTinContext(x->context, depth + 1);
    fprintf(stderr, "\n");
        printHashTable(x->set, depth + 1);
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinMonoType(struct TinMonoType * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinMonoType (NULL)"); return; }
    fprintf(stderr, "TinMonoType[\n");
    switch(x->type) {
        case TINMONOTYPE_TYPE_VAR:
            pad(depth + 1);
            fprintf(stderr, "TINMONOTYPE_TYPE_VAR\n");
                        printTinSymbol(x->val.var, depth + 1);
            break;
        case TINMONOTYPE_TYPE_FUN:
            pad(depth + 1);
            fprintf(stderr, "TINMONOTYPE_TYPE_FUN\n");
            printTinFunctionApplication(x->val.fun, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTinMonoType", x->type);
    }
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

void printTinPolyType(struct TinPolyType * x, int depth) {
    pad(depth);
    if (x == NULL) { fprintf(stderr, "TinPolyType (NULL)"); return; }
    fprintf(stderr, "TinPolyType[\n");
    switch(x->type) {
        case TINPOLYTYPE_TYPE_MONOTYPE:
            pad(depth + 1);
            fprintf(stderr, "TINPOLYTYPE_TYPE_MONOTYPE\n");
            printTinMonoType(x->val.monoType, depth + 1);
            break;
        case TINPOLYTYPE_TYPE_QUANTIFIER:
            pad(depth + 1);
            fprintf(stderr, "TINPOLYTYPE_TYPE_QUANTIFIER\n");
            printTinTypeQuantifier(x->val.quantifier, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTinPolyType", x->type);
    }
    fprintf(stderr, "\n");
    pad(depth);
    fprintf(stderr, "]");
}

