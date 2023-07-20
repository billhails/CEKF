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

// generated from src/lambda.yaml by makeAST.py



#include <stdio.h>

#include "debug_lambda.h"

static void pad(int depth) { printf("%*s", depth * 4, ""); }

void printLamLam(struct LamLam * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamLam (NULL)"); return; }
    printf("LamLam[\n");
        pad(depth + 1);
printf("int %d", x->nargs);
    printf("\n");
    printLamVarList(x->args, depth + 1);
    printf("\n");
    printLamExp(x->exp, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamVarList(struct LamVarList * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamVarList (NULL)"); return; }
    printf("LamVarList[\n");
        printLambdaSymbol(x->var, depth + 1);
    printf("\n");
    printLamVarList(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamPrimApp(struct LamPrimApp * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamPrimApp (NULL)"); return; }
    printf("LamPrimApp[\n");
    switch (x->type) {
        case LAMPRIMOP_TYPE_LAM_PRIM_ADD:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_ADD");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_SUB:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_SUB");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_MUL:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_MUL");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_DIV:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_DIV");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_EQ:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_EQ");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_NE:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_NE");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_GT:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_GT");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_LT:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_LT");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_GE:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_GE");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_LE:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_LE");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_CONS:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_CONS");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_VEC:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_VEC");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_XOR:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_XOR");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_AND:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_AND");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_OR:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_OR");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_AMB:
            pad(depth + 1);
            printf("LAMPRIMOP_TYPE_LAM_PRIM_AMB");
            break;
    }
    printf("\n");
    printLamExp(x->exp1, depth + 1);
    printf("\n");
    printLamExp(x->exp2, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamUnaryApp(struct LamUnaryApp * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamUnaryApp (NULL)"); return; }
    printf("LamUnaryApp[\n");
    switch (x->type) {
        case LAMUNARYOP_TYPE_LAM_UNARY_CAR:
            pad(depth + 1);
            printf("LAMUNARYOP_TYPE_LAM_UNARY_CAR");
            break;
        case LAMUNARYOP_TYPE_LAM_UNARY_CDR:
            pad(depth + 1);
            printf("LAMUNARYOP_TYPE_LAM_UNARY_CDR");
            break;
        case LAMUNARYOP_TYPE_LAM_UNARY_NOT:
            pad(depth + 1);
            printf("LAMUNARYOP_TYPE_LAM_UNARY_NOT");
            break;
        case LAMUNARYOP_TYPE_LAM_UNARY_PRINT:
            pad(depth + 1);
            printf("LAMUNARYOP_TYPE_LAM_UNARY_PRINT");
            break;
    }
    printf("\n");
    printLamExp(x->exp, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamList(struct LamList * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamList (NULL)"); return; }
    printf("LamList[\n");
    printLamExp(x->exp, depth + 1);
    printf("\n");
    printLamList(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamApply(struct LamApply * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamApply (NULL)"); return; }
    printf("LamApply[\n");
    printLamExp(x->function, depth + 1);
    printf("\n");
        pad(depth + 1);
printf("int %d", x->nargs);
    printf("\n");
    printLamList(x->args, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamMakeVec(struct LamMakeVec * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamMakeVec (NULL)"); return; }
    printf("LamMakeVec[\n");
        pad(depth + 1);
printf("int %d", x->nargs);
    printf("\n");
    printLamList(x->args, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamCond(struct LamCond * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamCond (NULL)"); return; }
    printf("LamCond[\n");
    printLamExp(x->condition, depth + 1);
    printf("\n");
    printLamExp(x->consequent, depth + 1);
    printf("\n");
    printLamExp(x->alternative, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamMatch(struct LamMatch * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamMatch (NULL)"); return; }
    printf("LamMatch[\n");
    printLamExp(x->index, depth + 1);
    printf("\n");
    printLamMatchList(x->cases, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamMatchList(struct LamMatchList * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamMatchList (NULL)"); return; }
    printf("LamMatchList[\n");
    printLamList(x->matches, depth + 1);
    printf("\n");
    printLamExp(x->body, depth + 1);
    printf("\n");
    printLamMatchList(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamLetRec(struct LamLetRec * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamLetRec (NULL)"); return; }
    printf("LamLetRec[\n");
        pad(depth + 1);
printf("int %d", x->nbindings);
    printf("\n");
    printLamLetRecBindings(x->bindings, depth + 1);
    printf("\n");
    printLamList(x->body, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamLetRecBindings(struct LamLetRecBindings * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamLetRecBindings (NULL)"); return; }
    printf("LamLetRecBindings[\n");
        printLambdaSymbol(x->var, depth + 1);
    printf("\n");
    printLamExp(x->val, depth + 1);
    printf("\n");
    printLamLetRecBindings(x->next, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamContext(struct LamContext * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamContext (NULL)"); return; }
    printf("LamContext[\n");
        printHashTable(x->frame, depth + 1);
    printf("\n");
    printLamContext(x->parent, depth + 1);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamTypeConstructorInfo(struct LamTypeConstructorInfo * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamTypeConstructorInfo (NULL)"); return; }
    printf("LamTypeConstructorInfo[\n");
        pad(depth + 1);
printf("bool %d", x->vec);
    printf("\n");
        pad(depth + 1);
printf("int %d", x->nargs);
    printf("\n");
        pad(depth + 1);
printf("int %d", x->index);
    printf("\n");
    pad(depth);
    printf("]");
}

void printLamExp(struct LamExp * x, int depth) {
    pad(depth);
    if (x == NULL) { printf("LamExp (NULL)"); return; }
    printf("LamExp[\n");
    switch(x->type) {
        case LAMEXP_TYPE_LAM:
            pad(depth + 1);
            printf("LAMEXP_TYPE_LAM\n");
            printLamLam(x->val.lam, depth + 1);
            break;
        case LAMEXP_TYPE_VAR:
            pad(depth + 1);
            printf("LAMEXP_TYPE_VAR\n");
                        printLambdaSymbol(x->val.var, depth + 1);
            break;
        case LAMEXP_TYPE_INTEGER:
            pad(depth + 1);
            printf("LAMEXP_TYPE_INTEGER\n");
                        pad(depth + 1);
printf("int %d", x->val.integer);
            break;
        case LAMEXP_TYPE_PRIM:
            pad(depth + 1);
            printf("LAMEXP_TYPE_PRIM\n");
            printLamPrimApp(x->val.prim, depth + 1);
            break;
        case LAMEXP_TYPE_UNARY:
            pad(depth + 1);
            printf("LAMEXP_TYPE_UNARY\n");
            printLamUnaryApp(x->val.unary, depth + 1);
            break;
        case LAMEXP_TYPE_LIST:
            pad(depth + 1);
            printf("LAMEXP_TYPE_LIST\n");
            printLamList(x->val.list, depth + 1);
            break;
        case LAMEXP_TYPE_MAKEVEC:
            pad(depth + 1);
            printf("LAMEXP_TYPE_MAKEVEC\n");
            printLamMakeVec(x->val.makeVec, depth + 1);
            break;
        case LAMEXP_TYPE_APPLY:
            pad(depth + 1);
            printf("LAMEXP_TYPE_APPLY\n");
            printLamApply(x->val.apply, depth + 1);
            break;
        case LAMEXP_TYPE_COND:
            pad(depth + 1);
            printf("LAMEXP_TYPE_COND\n");
            printLamCond(x->val.cond, depth + 1);
            break;
        case LAMEXP_TYPE_CALLCC:
            pad(depth + 1);
            printf("LAMEXP_TYPE_CALLCC\n");
            printLamExp(x->val.callcc, depth + 1);
            break;
        case LAMEXP_TYPE_LETREC:
            pad(depth + 1);
            printf("LAMEXP_TYPE_LETREC\n");
            printLamLetRec(x->val.letrec, depth + 1);
            break;
        case LAMEXP_TYPE_MATCH:
            pad(depth + 1);
            printf("LAMEXP_TYPE_MATCH\n");
            printLamMatch(x->val.match, depth + 1);
            break;
        case LAMEXP_TYPE_CHARACTER:
            pad(depth + 1);
            printf("LAMEXP_TYPE_CHARACTER\n");
                        pad(depth + 1);
printf("char %c", x->val.character);
            break;
        case LAMEXP_TYPE_STRING:
            pad(depth + 1);
            printf("LAMEXP_TYPE_STRING\n");
                        pad(depth + 1);
printf("char * %s", x->val.string);
            break;
        case LAMEXP_TYPE_BACK:
            pad(depth + 1);
            printf("LAMEXP_TYPE_BACK\n");
                        pad(depth + 1);
printf("void * %p", x->val.back);
            break;
        case LAMEXP_TYPE_T:
            pad(depth + 1);
            printf("LAMEXP_TYPE_T\n");
                        pad(depth + 1);
printf("void * %p", x->val.t);
            break;
        case LAMEXP_TYPE_F:
            pad(depth + 1);
            printf("LAMEXP_TYPE_F\n");
                        pad(depth + 1);
printf("void * %p", x->val.f);
            break;
        case LAMEXP_TYPE_NIL:
            pad(depth + 1);
            printf("LAMEXP_TYPE_NIL\n");
                        pad(depth + 1);
printf("void * %p", x->val.nil);
            break;
        default:
            cant_happen("unrecognised type %d in printLamExp", x->type);
    }
    printf("\n");
    pad(depth);
    printf("]");
}

