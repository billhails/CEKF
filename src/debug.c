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
#include <stdio.h>
#include <stdlib.h>

#include "debug.h"
#include "hash.h"

void printValue(Value x) {
    printf("Value[");
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printf("VOID");
            break;
        case VALUE_TYPE_INTEGER:
            printf("%d", x.val.z);
            break;
        case VALUE_TYPE_TRUE:
            printf("TRUE");
            break;
        case VALUE_TYPE_FALSE:
            printf("FALSE");
            break;
        case VALUE_TYPE_CLO:
            printClo(x.val.clo);
            break;
        case VALUE_TYPE_CONT:
            printKont(x.val.k);
            break;
    }
    printf("]");
}

void printValueList(ValueList *x) {
    printf("ValueList[");
    for (int i = 0; i < x->count; ++i) {
        printValue(x->values[i]);
        if (i < x->count - 1) {
            printf(", ");
        }
    }
    printf("]");
}

void printClo(Clo *x) {
    printf("Clo[");
    printAexpLam(x->lam);
    printf(", ");
    printElidedEnv(x->rho);
    printf("]");
}

void printCEKF(CEKF *x) {
    printf("[");
    printExp(x->C);
    printf(", ");
    printEnv(x->E);
    printf(", ");
    printKont(x->K);
    printf(", ");
    printFail(x->F);
    printf(", ");
    printValue(x->V);
    printf("]\n");
}

void printHashTable(HashTable *x) {
    int count = 0;
    printf("{");
    for (int i = 0; i < x->capacity; ++i) {
        if (x->entries[i].var != NULL) {
            printf("%s => ", x->entries[i].var->name);
            printValue(x->entries[i].value);
            count++;
            if (count < x->count) printf(", ");
        }
    }
    printf("}");
}

void printElidedHashTable(HashTable *x) {
    int count = 0;
    printf("{");
    for (int i = 0; i < x->capacity; ++i) {
        if (x->entries[i].var != NULL) {
            printf("%s => <...>", x->entries[i].var->name);
            count++;
            if (count < x->count) printf(", ");
        }
    }
    printf("}");
}

void printEnv(Env *x) {
    printf("Env[");
    while (x != NULL) {
        printHashTable(x->table);
        if (x->next != NULL) printf(", ");
        x = x->next;
    }
    printf("]");
}

void printElidedEnv(Env *x) {
    printf("Env[");
    while (x != NULL) {
        printElidedHashTable(x->table);
        if (x->next != NULL) printf(", ");
        x = x->next;
    }
    printf("]");
}

void printKont(Kont *x) {
    printf("Kont[");
    if (x != NULL) {
        printAexpVar(x->var);
        printf(", ");
        printExp(x->body);
        printf(", ");
        printEnv(x->rho);
        printf(", ");
        printKont(x->next);
    }
    printf("]");
}

void printFail(Fail *x) {
    printf("Fail[");
    if (x != NULL) {
        printExp(x->exp);
        printf(", ");
        printEnv(x->rho);
        printf(", ");
        printKont(x->k);
        printf(", ");
        printFail(x->next);
    }
    printf("]");
}

void printAexpLam(AexpLam *x) {
    printf("(lambda ");
    printAexpVarList(x->args);
    printf(" ");
    printExp(x->exp);
    printf(")");
}

void printAexpVarList(AexpVarList *x) {
    printf("(");
    while (x != NULL) {
        printAexpVar(x->var);
        if (x->next != NULL) {
            printf(" ");
        }
        x = x->next;
    }
    printf(")");
}

void printAexpVar(AexpVar *x) {
    printf("%s", x->name);
}

void printAexpPrimApp(AexpPrimApp *x) {
    printf("(");
    switch(x->op) {
        case AEXP_PRIM_ADD:
            printf("+ ");
            break;
        case AEXP_PRIM_SUB:
            printf("- ");
            break;
        case AEXP_PRIM_MUL:
            printf("* ");
            break;
        case AEXP_PRIM_DIV:
            printf("* ");
            break;
        case AEXP_PRIM_EQ:
            printf("= ");
            break;
        case AEXP_PRIM_NE:
            printf("!= ");
            break;
        case AEXP_PRIM_GT:
            printf("> ");
            break;
        case AEXP_PRIM_LT:
            printf("< ");
            break;
        case AEXP_PRIM_GE:
            printf(">= ");
            break;
        case AEXP_PRIM_LE:
            printf("<= ");
            break;
    }
    printBareAexpList(x->args);
    printf(")");
}

void printAexpList(AexpList *x) {
    printf("(");
    while (x != NULL) {
        printExp(x->exp);
        if (x->next) {
            printf(" ");
        }
        x = x->next;
    }
    printf(")");
}

void printBareAexpList(AexpList *x) {
    while (x != NULL) {
        printExp(x->exp);
        if (x->next) {
            printf(" ");
        }
        x = x->next;
    }
}

void printCexpApply(CexpApply *x) {
    printf("(");
    printExp(x->function);
    printf(" ");
    printBareAexpList(x->args);
    printf(")");
}

void printCexpCond(CexpCond *x) {
    printf("(if ");
    printExp(x->condition);
    printf(" ");
    printExp(x->consequent);
    printf(" ");
    printExp(x->alternative);
    printf(")");
}

void printCexpLetRec(CexpLetRec *x) {
    printf("(letrec ");
    printLetRecBindings(x->bindings);
    printf(" ");
    printExp(x->body);
    printf(")");
}

void printLetRecBindings(LetRecBindings *x) {
    printf("(");
    while (x != NULL) {
        printf("(");
        printAexpVar(x->var);
        printf(" ");
        printExp(x->val);
        printf(")");
        if (x->next != NULL) {
            printf(" ");
        }
        x = x->next;
    }
    printf(")");
}

void printCexpAmb(CexpAmb *x) {
    printf("(amb ");
    printExp(x->exp1);
    printf(" ");
    printExp(x->exp2);
    printf(")");
}

void printExp(Exp *x) {
    switch (x->type) {
        case AEXP_TYPE_LAM:
            printAexpLam(x->val.aexp.lam);
            break;
        case AEXP_TYPE_VAR:
            printAexpVar(x->val.aexp.var);
            break;
        case AEXP_TYPE_TRUE:
            printf("#t");
            break;
        case AEXP_TYPE_FALSE:
            printf("#f");
            break;
        case AEXP_TYPE_INT:
            printf("%d", x->val.aexp.integer);
            break;
        case AEXP_TYPE_PRIM:
            printAexpPrimApp(x->val.aexp.prim);
            break;
        case CEXP_TYPE_APPLY:
            printCexpApply(x->val.cexp.apply);
            break;
        case CEXP_TYPE_COND:
            printCexpCond(x->val.cexp.cond);
            break;
        case CEXP_TYPE_CALLCC:
            printf("(call/cc ");
            printExp(x->val.cexp.callCC);
            printf(")");
            break;
        case CEXP_TYPE_LETREC:
            printCexpLetRec(x->val.cexp.letRec);
            break;
        case CEXP_TYPE_AMB:
            printCexpAmb(x->val.cexp.amb);
            break;
        case CEXP_TYPE_BACK:
            printf("(back)");
            break;
        case EXP_TYPE_LET:
            printExpLet(x->val.let);
            break;
        case EXP_TYPE_DONE:
            printf("<DONE>");
            break;
        default:
            printf("<unrecognised exp %d>", x->type);
            exit(1);
    }
}

void printExpLet(ExpLet *x) {
    printf("(let (");
    printAexpVar(x->var);
    printf(" ");
    printExp(x->val);
    printf(") ");
    printExp(x->body);
    printf(")");
}
