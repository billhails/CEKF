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

#include "common.h"
#include "debug.h"
#include "hash.h"

static void printContainedValue(Value x) {
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printf("#V");
            break;
        case VALUE_TYPE_INTEGER:
            printf("%d", x.val.z);
            break;
        case VALUE_TYPE_TRUE:
            printf("#T");
            break;
        case VALUE_TYPE_FALSE:
            printf("#F");
            break;
        case VALUE_TYPE_CLO:
            printClo(x.val.clo);
            break;
        case VALUE_TYPE_CONT:
            printKont(x.val.k);
            break;
        case VALUE_TYPE_CONS:
            printCons(x.val.cons);
            break;
        default:
            cant_happen("unrecognised value type in printContainedValue");
    }
}

static void printSnapshot(Snapshot s) {
    if (s.frameSize == 0) {
        printf("S/");
        return;
    }
    printf("S[");
    for (int i = 0; i < s.frameSize; i++) {
        printContainedValue(s.frame[i]);
        if (i < s.frameSize - 1) {
            printf(", ");
        }
    }
    printf("]");
}

static void printElidedSnapshot(Snapshot s) {
    if (s.frameSize == 0) {
        printf("S/");
        return;
    }
    printf("S[<...>]");
}

void printValue(Value x) {
    if (x.type == VALUE_TYPE_VOID) {
        printf("V/");
        return;
    }
    printf("V[");
    printContainedValue(x);
    printf("]");
}

void printElidedClo(Clo *x) {
    printf("C[%d, %d, E[<...>], ", x->nvar, x->c);
    printf("]");
}

void printElidedKont(Kont *x) {
    if (x == NULL) {
        printf("K/");
        return;
    }
    printf("K[");
    printf("%d, E[<...>], ", x->body);
    printElidedSnapshot(x->snapshot);
    printElidedKont(x->next);
    printf("]");
}

void printCons(Cons *x) {
    printf("(");
    printContainedValue(x->car);
    printf(" . ");
    printContainedValue(x->cdr);
    printf(")");
}

void printElidedValue(Value x) {
    printf("V[");
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printf("#V");
            break;
        case VALUE_TYPE_INTEGER:
            printf("%d", x.val.z);
            break;
        case VALUE_TYPE_TRUE:
            printf("#T");
            break;
        case VALUE_TYPE_FALSE:
            printf("#F");
            break;
        case VALUE_TYPE_CONS:
            printCons(x.val.cons);
            break;
        case VALUE_TYPE_CLO:
            printElidedClo(x.val.clo);
            break;
        case VALUE_TYPE_CONT:
            printElidedKont(x.val.k);
            break;
        default:
            cant_happen("unrecognised value type in printElidedValue");
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
    printf("C[%d, %d, ", x->nvar, x->c);
    printElidedEnv(x->rho);
    printf("]");
}

void printCEKF(CEKF *x) {
    printf("( ");
    printf("%d", x->C);
    printf(" - ");
    printEnv(x->E);
    printf(" - ");
    printKont(x->K);
    printf(" - ");
    printFail(x->F);
    printf(" - ");
    printValue(x->V);
    printf(" - ");
    printStack(&x->S);
    printf(" )\n");
}

void printStack(Stack *x) {
    if (x == NULL || x->sp ==0) {
        printf("S/");
        return;
    }
    printf("S[");
    for (int i = x->sp; i > 0; --i) {
        printContainedValue(peekValue(x, i - 1));
        if (i > 1) {
            printf(", ");
        }
    }
    printf("]");
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

void printValues(Value *values, int count) {
    printf("{");
    for (int i = 0; i < count; ++i) {
        printValue(values[i]);
        if (i + 1 < count) {
            printf(", ");
        }
    }
    printf("}");
}

void printElidedValues(Value *values, int count) {
    printf("{");
    for (int i = 0; i < count; ++i) {
        printElidedValue(values[i]);
        if (i + 1 < count) {
            printf(", ");
        }
    }
    printf("}");
}

void printEnv(Env *x) {
    if (x == NULL) {
        printf("E/");
        return;
    }
    printf("E[");
    while (x != NULL) {
        printValues(x->values, x->count);
        if (x->next != NULL) printf(", ");
        x = x->next;
    }
    printf("]");
}

void printCTEnv(CTEnv *x) {
    printf("CTEnv[");
    while (x != NULL) {
        printHashTable(x->table);
        if (x->next != NULL) printf(", ");
        x = x->next;
    }
    printf("]");
}

void printElidedEnv(Env *x) {
    if (x == NULL) {
        printf("E/");
        return;
    }
    printf("E[");
    while (x != NULL) {
        printElidedValues(x->values, x->count);
        if (x->next != NULL) printf(", ");
        x = x->next;
    }
    printf("]");
}

void printKont(Kont *x) {
    if (x == NULL) {
        printf("K/");
        return;
    }
    printf("K[");
    if (x != NULL) {
        printf("%d, ", x->body);
        printEnv(x->rho);
        printf(", ");
        printSnapshot(x->snapshot);
        printf(", ");
        printKont(x->next);
    }
    printf("]");
}

void printFail(Fail *x) {
    if (x == NULL) {
        printf("F/");
        return;
    }
    printf("F[");
    if (x != NULL) {
        printf("%d", x->exp);
        printf(", ");
        printEnv(x->rho);
        printf(", ");
        printSnapshot(x->snapshot);
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

void printAexpAnnotatedVar(AexpAnnotatedVar *x) {
    printAexpVar(x->var);
    if (x->type == VAR_TYPE_STACK)
        printf("[%d]", x->offset);
    else
        printf("[%d:%d]", x->frame, x->offset);
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
        case AEXP_PRIM_CONS:
            printf("cons ");
            break;
        default:
            cant_happen("unrecognized op in printAexpPrimApp (%d)", x->op);
    }
    printAexp(x->exp1);
    if (x->exp2 != NULL) {
        printf(" ");
        printAexp(x->exp2);
    }
    printf(")");
}

void printAexpList(AexpList *x) {
    printf("(");
    while (x != NULL) {
        printAexp(x->exp);
        if (x->next) {
            printf(" ");
        }
        x = x->next;
    }
    printf(")");
}

void printBareAexpList(AexpList *x) {
    while (x != NULL) {
        printAexp(x->exp);
        if (x->next) {
            printf(" ");
        }
        x = x->next;
    }
}

void printCexpApply(CexpApply *x) {
    printf("(");
    printAexp(x->function);
    printf(" ");
    printBareAexpList(x->args);
    printf(")");
}

void printCexpCond(CexpCond *x) {
    printf("(if ");
    printAexp(x->condition);
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
        printAexp(x->val);
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

void printAexp(Aexp *x) {
    switch (x->type) {
        case AEXP_TYPE_LAM:
            printAexpLam(x->val.lam);
            break;
        case AEXP_TYPE_VAR:
            printAexpVar(x->val.var);
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            printAexpAnnotatedVar(x->val.annotatedVar);
            break;
        case AEXP_TYPE_TRUE:
            printf("#t");
            break;
        case AEXP_TYPE_FALSE:
            printf("#f");
            break;
        case AEXP_TYPE_VOID:
            printf("nil");
            break;
        case AEXP_TYPE_INT:
            printf("%d", x->val.integer);
            break;
        case AEXP_TYPE_PRIM:
            printAexpPrimApp(x->val.prim);
            break;
        default:
            cant_happen("unrecognised aexp %d in printAexp", x->type);
    }
}

void printCexp(Cexp *x) {
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            printCexpApply(x->val.apply);
            break;
        case CEXP_TYPE_COND:
            printCexpCond(x->val.cond);
            break;
        case CEXP_TYPE_CALLCC:
            printf("(call/cc ");
            printAexp(x->val.callCC);
            printf(")");
            break;
        case CEXP_TYPE_LETREC:
            printCexpLetRec(x->val.letRec);
            break;
        case CEXP_TYPE_AMB:
            printCexpAmb(x->val.amb);
            break;
        case CEXP_TYPE_BACK:
            printf("(back)");
            break;
        default:
            printf("<unrecognised cexp %d>", x->type);
            exit(1);
    }
}

void printExp(Exp *x) {
    switch (x->type) {
        case EXP_TYPE_AEXP:
            printAexp(x->val.aexp);
            break;
        case EXP_TYPE_CEXP:
            printCexp(x->val.cexp);
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

static int intAt(ByteCodeArray *b, int index) {
    return
        (b->entries[index] << 24) +
        (b->entries[index + 1] << 16) +
        (b->entries[index + 2] << 8) +
        b->entries[index + 3];
}

static int wordAt(ByteCodeArray *b, int index) {
    return (b->entries[index] << 8) + b->entries[index + 1];
}

static int offsetAt(ByteCodeArray *b, int index) {
    return index + wordAt(b, index);
}

void dumpByteCode(ByteCodeArray *b) {
    int i = 0;
    while (i < b->count) {
        switch (b->entries[i]) {
            case BYTECODE_NONE: {
                printf("%04d ### NONE\n", i);
                i++;
            }
            break;
            case BYTECODE_LAM: {
                printf("%04d ### LAM [%d] [%d]\n", i, b->entries[i + 1], offsetAt(b, i+2));
                i += 4;
            }
            break;
            case BYTECODE_VAR: {
                printf("%04d ### VAR [%d:%d]\n", i, b->entries[i + 1], b->entries[i + 2]);
                i += 3;
            }
            break;
            case BYTECODE_LVAR: {
                printf("%04d ### LVAR [%d]\n", i, b->entries[i + 1]);
                i += 2;
            }
            break;
            case BYTECODE_PRIM_ADD: {
                printf("%04d ### PRIM(+)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_SUB: {
                printf("%04d ### PRIM(-)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_MUL: {
                printf("%04d ### PRIM(*)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_DIV: {
                printf("%04d ### PRIM(/)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_EQ: {
                printf("%04d ### PRIM(==)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_NE: {
                printf("%04d ### PRIM(!=)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_GT: {
                printf("%04d ### PRIM(>)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_LT: {
                printf("%04d ### PRIM(<)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_GE: {
                printf("%04d ### PRIM(>=)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_LE: {
                printf("%04d ### PRIM(<=)\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_CONS: {
                printf("%04d ### PRIM(cons)\n", i);
                i++;
            }
            break;
            case BYTECODE_APPLY: {
                printf("%04d ### APPLY\n", i);
                i++;
            }
            break;
            case BYTECODE_IF: {
                printf("%04d ### IF [%d]\n", i, offsetAt(b, i + 1));
                i += 3;
            }
            break;
            case BYTECODE_LETREC: {
                printf("%04d ### LETREC [%d]\n", i, b->entries[i + 1]);
                i += 2;
            }
            break;
            case BYTECODE_AMB: {
                printf("%04d ### AMB [%d]\n", i, offsetAt(b, i + 1));
                i += 3;
            }
            break;
            case BYTECODE_BACK: {
                printf("%04d ### BACK\n", i);
                i++;
            }
            break;
            case BYTECODE_LET: {
                printf("%04d ### LET [%d]\n", i, offsetAt(b, i + 1));
                i += 3;
            }
            break;
            case BYTECODE_JMP: {
                printf("%04d ### JMP [%d]\n", i, offsetAt(b, i + 1));
                i += 3;
            }
            break;
            case BYTECODE_PUSHN: {
                printf("%04d ### PUSHN [%d]\n", i, b->entries[i + 1]);
                i += 2;
            }
            break;
            case BYTECODE_CALLCC: {
                printf("%04d ### CALLCC\n", i);
                i++;
            }
            break;
            case BYTECODE_TRUE: {
                printf("%04d ### TRUE\n", i);
                i++;
            }
            break;
            case BYTECODE_FALSE: {
                printf("%04d ### FALSE\n", i);
                i++;
            }
            break;
            case BYTECODE_VOID: {
                printf("%04d ### VOID\n", i);
                i++;
            }
            break;
            case BYTECODE_INT: {
                printf("%04d ### INT [%d]\n", i, intAt(b, i + 1));
                i += 5;
            }
            break;
            case BYTECODE_RETURN: {
                printf("%04d ### RETURN\n", i);
                i++;
            }
            break;
            case BYTECODE_DONE: {
                printf("%04d ### DONE\n", i);
                i++;
            }
            break;
            default:
                cant_happen("unrecognised bytecode in dumpByteCode");
        }
    }
}
