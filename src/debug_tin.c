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

// generated from src/tin.yaml by makeAST.py



#include <stdio.h>

#include "debug_tin.h"

static void pad(int depth) { printf("%*s", depth * 4, ""); }

void printTinFunctionApplication(struct TinFunctionApplication * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinFunctionApplication (NULL)"); return; }
    printf("TinFunctionApplication[\n");
        printTinSymbol(x->name, depth + 1);
    printf("\n");
        pad(depth + 1);
printf("int %d", x->nargs);
    printf("\n");
    printTinMonoTypeList(x->args, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinMonoTypeList(struct TinMonoTypeList * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinMonoTypeList (NULL)"); return; }
    printf("TinMonoTypeList[\n");
    printTinMonoType(x->monoType, depth + 1);
    printf("\n");
    printTinMonoTypeList(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinTypeQuantifier(struct TinTypeQuantifier * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinTypeQuantifier (NULL)"); return; }
    printf("TinTypeQuantifier[\n");
        printTinSymbol(x->var, depth + 1);
    printf("\n");
    printTinPolyType(x->quantifiedType, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinContext(struct TinContext * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinContext (NULL)"); return; }
    printf("TinContext[\n");
        printHashTable(x->varFrame, depth + 1);
    printf("\n");
        printHashTable(x->tcFrame, depth + 1);
    printf("\n");
    printTinContext(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinSubstitution(struct TinSubstitution * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinSubstitution (NULL)"); return; }
    printf("TinSubstitution[\n");
        printHashTable(x->map, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinArgsResult(struct TinArgsResult * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinArgsResult (NULL)"); return; }
    printf("TinArgsResult[\n");
    printTinContext(x->context, depth + 1);
    printf("\n");
    printTinMonoTypeList(x->vec, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinVarResult(struct TinVarResult * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinVarResult (NULL)"); return; }
    printf("TinVarResult[\n");
    printTinSubstitution(x->substitution, depth + 1);
    printf("\n");
    printTinContext(x->context, depth + 1);
    printf("\n");
    printTinMonoType(x->monoType, depth + 1);
    printf("\n");
        printHashTable(x->set, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinVarsResult(struct TinVarsResult * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinVarsResult (NULL)"); return; }
    printf("TinVarsResult[\n");
    printTinContext(x->context, depth + 1);
    printf("\n");
        printHashTable(x->set, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinMonoType(struct TinMonoType * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinMonoType (NULL)"); return; }
    printf("TinMonoType[\n");
    switch(x->type) {
        case TINMONOTYPE_TYPE_VAR:
            pad(depth + 1);
            printf("TINMONOTYPE_TYPE_VAR\n");
                        printTinSymbol(x->val.var, depth + 1);
            break;
        case TINMONOTYPE_TYPE_FUN:
            pad(depth + 1);
            printf("TINMONOTYPE_TYPE_FUN\n");
            printTinFunctionApplication(x->val.fun, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTinMonoType", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

void printTinPolyType(struct TinPolyType * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("TinPolyType (NULL)"); return; }
    printf("TinPolyType[\n");
    switch(x->type) {
        case TINPOLYTYPE_TYPE_MONOTYPE:
            pad(depth + 1);
            printf("TINPOLYTYPE_TYPE_MONOTYPE\n");
            printTinMonoType(x->val.monoType, depth + 1);
            break;
        case TINPOLYTYPE_TYPE_QUANTIFIER:
            pad(depth + 1);
            printf("TINPOLYTYPE_TYPE_QUANTIFIER\n");
            printTinTypeQuantifier(x->val.quantifier, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printTinPolyType", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

