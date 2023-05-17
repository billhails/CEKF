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

static void printClo(Clo *x, int depth);
static void printElidedEnv(Env *x);
static void printEnv(Env *x, int depth);
static void printFail(Fail *x, int depth);
static void printKont(Kont *x, int depth);
static void printStack(Stack *x, int depth);
static void printCons(Cons *x);

static void printPad(int depth) {
    printf("%*s", depth * 4, "");
}

static void printContainedValue(Value x, int depth) {
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printPad(depth);
            printf("#V");
            break;
        case VALUE_TYPE_INTEGER:
            printPad(depth);
            printf("%d", x.val.z);
            break;
        case VALUE_TYPE_TRUE:
            printPad(depth);
            printf("#T");
            break;
        case VALUE_TYPE_FALSE:
            printPad(depth);
            printf("#F");
            break;
        case VALUE_TYPE_CLO:
            printClo(x.val.clo, depth);
            break;
        case VALUE_TYPE_CONT:
            printKont(x.val.k, depth);
            break;
        case VALUE_TYPE_CONS:
            printPad(depth);
            printCons(x.val.cons);
            break;
        default:
            cant_happen("unrecognised value type in printContainedValue");
    }
}

static void printSnapshot(Snapshot s, int depth) {
    printPad(depth);
    if (s.frameSize == 0) {
        printf("S/");
        return;
    }
    printf("SS[\n");
    for (int i = 0; i < s.frameSize; i++) {
        printContainedValue(s.frame[i], depth + 1);
        if (i < s.frameSize - 1) {
            printf(",");
        }
        printf("\n");
    }
    printPad(depth);
    printf("]");
}

static void printElidedSnapshot(Snapshot s) {
    if (s.frameSize == 0) {
        printf("S/");
        return;
    }
    printf("S[<...>]");
}

void printValue(Value x, int depth) {
    printPad(depth);
    if (x.type == VALUE_TYPE_VOID) {
        printf("V/");
        return;
    }
    printf("V[\n");
    printContainedValue(x, depth + 1);
    printf("\n");
    printPad(depth);
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
    printContainedValue(x->car, 0);
    printf(" . ");
    printContainedValue(x->cdr, 0);
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

static void printClo(Clo *x, int depth) {
    printPad(depth);
    printf("C[%d, %d, ", x->nvar, x->c);
    printElidedEnv(x->rho);
    printf("]");
}

void printCEKF(CEKF *x) {
    int depth = 1;
    printf("\nCEKF (\n");
    printPad(depth);
    printf("%d", x->C);
    printf(",\n");
    printEnv(x->E, depth);
    printf(",\n");
    printKont(x->K, depth);
    printf(",\n");
    printFail(x->F, depth);
    printf(",\n");
    printValue(x->V, depth);
    printf(",\n");
    printStack(&x->S, depth);
    printf("\n)\n\n");
}

static void printStack(Stack *x, int depth) {
    printPad(depth);
    if (x == NULL || x->sp ==0) {
        printf("S/");
        return;
    }
    printf("S[\n");
    for (int i = 0; i < x->sp; ++i) {
        printContainedValue(peekValue(x, i), depth + 1);
        if (i < x->sp - 1) {
            printf(",");
        }
        printf("\n");
    }
    printPad(depth);
    printf("]");
}

void printHashTable(HashTable *x) {
    int count = 0;
    printf("{");
    for (int i = 0; i < x->capacity; ++i) {
        if (x->entries[i].var != NULL) {
            printf("%s => ", x->entries[i].var->name);
            printValue(x->entries[i].value, 0);
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

static void printValues(Value *values, int count, int depth) {
    printPad(depth);
    printf("{\n");
    for (int i = 0; i < count; ++i) {
        printValue(values[i], depth + 1);
        if (i + 1 < count) {
            printf(",");
        }
        printf("\n");
    }
    printPad(depth);
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

void printEnv(Env *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        printf("E/");
        return;
    }
    printf("E[\n");
    while (x != NULL) {
        printValues(x->values, x->count, depth + 1);
        if (x->next != NULL) printf(",");
        printf("\n");
        x = x->next;
    }
    printPad(depth);
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

static void printKont(Kont *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        printf("K/");
        return;
    }
    printf("K[\n");
    if (x != NULL) {
        printPad(depth + 1);
        printf("%d,\n", x->body);
        printEnv(x->rho, depth + 1);
        printf(",\n");
        printSnapshot(x->snapshot, depth + 1);
        printf(",\n");
        printKont(x->next, depth + 1);
        printf("\n");
    }
    printPad(depth);
    printf("]");
}

static void printFail(Fail *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        printf("F/");
        return;
    }
    printf("F[\n");
    if (x != NULL) {
        printPad(depth + 1);
        printf("%d", x->exp);
        printf(",\n");
        printEnv(x->rho, depth + 1);
        printf(",\n");
        printSnapshot(x->snapshot, depth + 1);
        printf(",\n");
        printKont(x->k, depth + 1);
        printf(",\n");
        printFail(x->next, depth + 1);
        printf("\n");
    }
    printPad(depth);
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
        case AEXP_PRIM_XOR:
            printf("xor ");
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

void printAexpUnaryApp(AexpUnaryApp *x) {
    printf("(");
    switch(x->op) {
        case AEXP_UNARY_CAR:
            printf("car ");
            break;
        case AEXP_UNARY_CDR:
            printf("cdr ");
            break;
        case AEXP_UNARY_NOT:
            printf("not ");
            break;
        default:
            cant_happen("unrecognized op in printAexpUnaryApp (%d)", x->op);
    }
    printAexp(x->exp);
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

void printAexpMakeList(AexpList *x) {
    printf("(list ");
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

void printCexpBool(CexpBool *x) {
    printf("(");
    switch (x->type) {
        case BOOL_TYPE_AND:
            printf("and");
            break;
        case BOOL_TYPE_OR:
            printf("or");
            break;
        default:
            cant_happen("unrecognised type %d in printCexpBool", x->type);
    }
    printf(" ");
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
        case AEXP_TYPE_UNARY:
            printAexpUnaryApp(x->val.unary);
            break;
        case AEXP_TYPE_LIST:
            printAexpMakeList(x->val.list);
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
        case CEXP_TYPE_BOOL:
            printCexpBool(x->val.boolean);
            break;
        case CEXP_TYPE_BACK:
            printf("(back)");
            break;
        default:
            cant_happen("unrecognised cexp %d in printCexp", x->type);
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
                printf("%04d ### LAM [%d] [%d] [%d]\n", i, b->entries[i + 1], b->entries[i + 2], offsetAt(b, i + 3));
                i += 5;
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
                printf("%04d ### ADD\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_SUB: {
                printf("%04d ### SUB\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_MUL: {
                printf("%04d ### MUL\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_DIV: {
                printf("%04d ### DIV\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_EQ: {
                printf("%04d ### EQ\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_NE: {
                printf("%04d ### NE\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_GT: {
                printf("%04d ### GT\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_LT: {
                printf("%04d ### LT\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_GE: {
                printf("%04d ### GE\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_LE: {
                printf("%04d ### LE\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_XOR: {
                printf("%04d ### XOR\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_CONS: {
                printf("%04d ### CONS\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_CAR: {
                printf("%04d ### CAR\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_CDR: {
                printf("%04d ### CDR\n", i);
                i++;
            }
            break;
            case BYTECODE_PRIM_NOT: {
                printf("%04d ### NOT\n", i);
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
