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

// Debugging support for printing various structures.

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
static void printVec(Vec *x);

static void printPad(int depth) {
    fprintf(stderr, "%*s", depth * 4, "");
}

void printContainedValue(Value x, int depth) {
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printPad(depth);
            fprintf(stderr, "#V");
            break;
        case VALUE_TYPE_STDINT:
            printPad(depth);
            fprintf(stderr, "%d", x.val.z);
            break;
        case VALUE_TYPE_BIGINT:
            printPad(depth);
            fprintBigInt(stderr, x.val.b);
            break;
        case VALUE_TYPE_CHARACTER:
            printPad(depth);
            fprintf(stderr, "'%c'", x.val.c);
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
        case VALUE_TYPE_VEC:
            printPad(depth);
            printVec(x.val.vec);
            break;
        default:
            cant_happen("unrecognised value type in printContainedValue");
    }
}

static void printSnapshot(Snapshot s, int depth) {
    printPad(depth);
    if (s.frameSize == 0) {
        fprintf(stderr, "S/");
        return;
    }
    fprintf(stderr, "SS[\n");
    for (int i = 0; i < s.frameSize; i++) {
        printContainedValue(s.frame[i], depth + 1);
        if (i < s.frameSize - 1) {
            fprintf(stderr, ",");
        }
        fprintf(stderr, "\n");
    }
    printPad(depth);
    fprintf(stderr, "]");
}

static void printElidedSnapshot(Snapshot s) {
    if (s.frameSize == 0) {
        fprintf(stderr, "S/");
        return;
    }
    fprintf(stderr, "S[<...>]");
}

void printValue(Value x, int depth) {
    printPad(depth);
    if (x.type == VALUE_TYPE_VOID) {
        fprintf(stderr, "V/");
        return;
    }
    fprintf(stderr, "V[\n");
    printContainedValue(x, depth + 1);
    fprintf(stderr, "\n");
    printPad(depth);
    fprintf(stderr, "]");
}

void printElidedClo(Clo *x) {
    fprintf(stderr, "C[%d, %d, E[<...>], ", x->nvar, x->c);
    fprintf(stderr, "]");
}

void printElidedKont(Kont *x) {
    if (x == NULL) {
        fprintf(stderr, "K/");
        return;
    }
    fprintf(stderr, "K[");
    fprintf(stderr, "%d, E[<...>], ", x->body);
    printElidedSnapshot(x->snapshot);
    printElidedKont(x->next);
    fprintf(stderr, "]");
}

void printCons(Cons *x) {
    fprintf(stderr, "(");
    printContainedValue(x->car, 0);
    fprintf(stderr, " . ");
    printContainedValue(x->cdr, 0);
    fprintf(stderr, ")");
}

void printVec(Vec *x) {
    fprintf(stderr, "#[");
    for (int i = 0; i < x->size; i++) {
        printContainedValue(x->values[i], 0);
        if (i + 1 < x->size) {
            fprintf(stderr, " ");
        }
    }
    fprintf(stderr, "]");
}

void printElidedValue(Value x) {
    fprintf(stderr, "V[");
    switch (x.type) {
        case VALUE_TYPE_VOID:
            fprintf(stderr, "#V");
            break;
        case VALUE_TYPE_STDINT:
            fprintf(stderr, "%d", x.val.z);
            break;
        case VALUE_TYPE_CHARACTER:
            fprintf(stderr, "'%c'", x.val.c);
            break;
        case VALUE_TYPE_CONS:
            printCons(x.val.cons);
            break;
        case VALUE_TYPE_VEC:
            printVec(x.val.vec);
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
    fprintf(stderr, "]");
}

static void printClo(Clo *x, int depth) {
    printPad(depth);
    fprintf(stderr, "C[%d, %d, ", x->nvar, x->c);
    printElidedEnv(x->rho);
    fprintf(stderr, "]");
}

void printCEKF(CEKF *x) {
    int depth = 1;
    fprintf(stderr, "\nCEKF (\n");
    printPad(depth);
    fprintf(stderr, "%d", x->C);
    fprintf(stderr, ",\n");
    printEnv(x->E, depth);
    fprintf(stderr, ",\n");
    printKont(x->K, depth);
    fprintf(stderr, ",\n");
    printFail(x->F, depth);
    fprintf(stderr, ",\n");
    printValue(x->V, depth);
    fprintf(stderr, ",\n");
    printStack(&x->S, depth);
    fprintf(stderr, "\n)\n\n");
}

static void printStack(Stack *x, int depth) {
    printPad(depth);
    if (x == NULL || x->sp ==0) {
        fprintf(stderr, "S/");
        return;
    }
    fprintf(stderr, "S[\n");
    for (int i = 0; i < x->sp; ++i) {
        printContainedValue(peekValue(x, i), depth + 1);
        if (i < x->sp - 1) {
            fprintf(stderr, ",");
        }
        fprintf(stderr, "\n");
    }
    printPad(depth);
    fprintf(stderr, "]");
}

static void printValues(Value *values, int count, int depth) {
    printPad(depth);
    fprintf(stderr, "{\n");
    for (int i = 0; i < count; ++i) {
        printValue(values[i], depth + 1);
        if (i + 1 < count) {
            fprintf(stderr, ",");
        }
        fprintf(stderr, "\n");
    }
    printPad(depth);
    fprintf(stderr, "}");
}

void printElidedValues(Value *values, int count) {
    fprintf(stderr, "{");
    for (int i = 0; i < count; ++i) {
        printElidedValue(values[i]);
        if (i + 1 < count) {
            fprintf(stderr, ", ");
        }
    }
    fprintf(stderr, "}");
}

void printEnv(Env *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        fprintf(stderr, "E/");
        return;
    }
    fprintf(stderr, "E[\n");
    while (x != NULL) {
        printValues(x->values, x->count, depth + 1);
        if (x->next != NULL) fprintf(stderr, ",");
        fprintf(stderr, "\n");
        x = x->next;
    }
    printPad(depth);
    fprintf(stderr, "]");
}

void printCTEnv(CTEnv *x) {
    fprintf(stderr, "CTEnv[");
    while (x != NULL) {
        printHashTable(x->table, 0);
        if (x->next != NULL) fprintf(stderr, ", ");
        x = x->next;
    }
    fprintf(stderr, "]");
}

void printElidedEnv(Env *x) {
    if (x == NULL) {
        fprintf(stderr, "E/");
        return;
    }
    fprintf(stderr, "E[");
    while (x != NULL) {
        printElidedValues(x->values, x->count);
        if (x->next != NULL) fprintf(stderr, ", ");
        x = x->next;
    }
    fprintf(stderr, "]");
}

static void printKont(Kont *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        fprintf(stderr, "K/");
        return;
    }
    fprintf(stderr, "K[\n");
    if (x != NULL) {
        printPad(depth + 1);
        fprintf(stderr, "%d,\n", x->body);
        printEnv(x->rho, depth + 1);
        fprintf(stderr, ",\n");
        printSnapshot(x->snapshot, depth + 1);
        fprintf(stderr, ",\n");
        printKont(x->next, depth + 1);
        fprintf(stderr, "\n");
    }
    printPad(depth);
    fprintf(stderr, "]");
}

static void printFail(Fail *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        fprintf(stderr, "F/");
        return;
    }
    fprintf(stderr, "F[\n");
    if (x != NULL) {
        printPad(depth + 1);
        fprintf(stderr, "%d", x->exp);
        fprintf(stderr, ",\n");
        printEnv(x->rho, depth + 1);
        fprintf(stderr, ",\n");
        printSnapshot(x->snapshot, depth + 1);
        fprintf(stderr, ",\n");
        printKont(x->k, depth + 1);
        fprintf(stderr, ",\n");
        printFail(x->next, depth + 1);
        fprintf(stderr, "\n");
    }
    printPad(depth);
    fprintf(stderr, "]");
}

void printAexpLam(AexpLam *x) {
    fprintf(stderr, "(lambda ");
    printAexpVarList(x->args);
    fprintf(stderr, " ");
    printExp(x->exp);
    fprintf(stderr, ")");
}

void printAexpVarList(AexpVarList *x) {
    fprintf(stderr, "(");
    while (x != NULL) {
        printAexpVar(x->var);
        if (x->next != NULL) {
            fprintf(stderr, " ");
        }
        x = x->next;
    }
    fprintf(stderr, ")");
}

void printAexpVar(HashSymbol *x) {
    fprintf(stderr, "%s", x->name);
}

void printAexpAnnotatedVar(AexpAnnotatedVar *x) {
    printAexpVar(x->var);
    if (x->type == VAR_TYPE_STACK)
        fprintf(stderr, ":%d", x->offset);
    else
        fprintf(stderr, ":%d:%d", x->frame, x->offset);
}

void printAexpPrimApp(AexpPrimApp *x) {
    fprintf(stderr, "(");
    switch(x->op) {
        case AEXP_PRIM_ADD:
            fprintf(stderr, "add ");
            break;
        case AEXP_PRIM_SUB:
            fprintf(stderr, "sub ");
            break;
        case AEXP_PRIM_MUL:
            fprintf(stderr, "mul ");
            break;
        case AEXP_PRIM_DIV:
            fprintf(stderr, "div ");
            break;
        case AEXP_PRIM_EQ:
            fprintf(stderr, "eq ");
            break;
        case AEXP_PRIM_NE:
            fprintf(stderr, "ne ");
            break;
        case AEXP_PRIM_GT:
            fprintf(stderr, "gt ");
            break;
        case AEXP_PRIM_LT:
            fprintf(stderr, "lt ");
            break;
        case AEXP_PRIM_GE:
            fprintf(stderr, "ge ");
            break;
        case AEXP_PRIM_LE:
            fprintf(stderr, "le ");
            break;
        case AEXP_PRIM_XOR:
            fprintf(stderr, "xor ");
            break;
        case AEXP_PRIM_CONS:
            fprintf(stderr, "cons ");
            break;
        case AEXP_PRIM_VEC:
            fprintf(stderr, "vec ");
            break;
        case AEXP_PRIM_MOD:
            fprintf(stderr, "mod ");
            break;
        default:
            cant_happen("unrecognized op in printAexpPrimApp (%d)", x->op);
    }
    printAexp(x->exp1);
    if (x->exp2 != NULL) {
        fprintf(stderr, " ");
        printAexp(x->exp2);
    }
    fprintf(stderr, ")");
}

void printAexpUnaryApp(AexpUnaryApp *x) {
    fprintf(stderr, "(");
    switch(x->op) {
        case AEXP_UNARY_CAR:
            fprintf(stderr, "car ");
            break;
        case AEXP_UNARY_CDR:
            fprintf(stderr, "cdr ");
            break;
        case AEXP_UNARY_NOT:
            fprintf(stderr, "not ");
            break;
        case AEXP_UNARY_PRINT:
            fprintf(stderr, "print ");
            break;
        default:
            cant_happen("unrecognized op in printAexpUnaryApp (%d)", x->op);
    }
    printAexp(x->exp);
    fprintf(stderr, ")");
}

static void printAexpListContents(AexpList *x) {
    while (x != NULL) {
        printAexp(x->exp);
        if (x->next) {
            fprintf(stderr, " ");
        }
        x = x->next;
    }
}

void printAexpList(AexpList *x) {
    fprintf(stderr, "(");
    printAexpListContents(x);
    fprintf(stderr, ")");
}

static void printAexpIntListContents(AexpIntList *x) {
    while (x != NULL) {
        fprintf(stderr, "%d", x->integer);
        if (x->next) {
            fprintf(stderr, " ");
        }
        x = x->next;
    }
}

void printAexpIntList(AexpIntList *x) {
    fprintf(stderr, "(");
    printAexpIntListContents(x);
    fprintf(stderr, ")");
}

void printAexpMakeList(AexpList *x) {
    fprintf(stderr, "(list ");
    printAexpListContents(x);
    fprintf(stderr, ")");
}

void printAexpMakeVec(AexpMakeVec *x) {
    fprintf(stderr, "(make-vec ");
    printAexpListContents(x->args);
    fprintf(stderr, ")");
}

void printBareAexpList(AexpList *x) {
    while (x != NULL) {
        printAexp(x->exp);
        if (x->next) {
            fprintf(stderr, " ");
        }
        x = x->next;
    }
}

void printCexpApply(CexpApply *x) {
    fprintf(stderr, "(");
    printAexp(x->function);
    fprintf(stderr, " ");
    printBareAexpList(x->args);
    fprintf(stderr, ")");
}

void printCexpIf(CexpIf *x) {
    fprintf(stderr, "(if ");
    printAexp(x->condition);
    fprintf(stderr, " ");
    printExp(x->consequent);
    fprintf(stderr, " ");
    printExp(x->alternative);
    fprintf(stderr, ")");
}

void printCexpCond(CexpCond *x) {
    fprintf(stderr, "(cond ");
    printAexp(x->condition);
    fprintf(stderr, " ");
    printCexpCondCases(x->cases);
    fprintf(stderr, ")");
}

void printCexpIntCondCases(CexpIntCondCases *x) {
    while (x != NULL) {
        fprintf(stderr, "(");
        fprintBigInt(stderr, x->option);
        fprintf(stderr, " ");
        printExp(x->body);
        fprintf(stderr, ")");
        if (x->next) {
            fprintf(stderr, " ");
        }
        x = x->next;
    }
}

void printCexpCharCondCases(CexpCharCondCases *x) {
    while (x != NULL) {
        fprintf(stderr, "('%c' ", x->option);
        printExp(x->body);
        fprintf(stderr, ")");
        if (x->next) {
            fprintf(stderr, " ");
        }
        x = x->next;
    }
}

void printCexpCondCases(CexpCondCases *x) {
    switch (x->type) {
        case CONDCASE_TYPE_INT:
            printCexpIntCondCases(x->val.intCases);
            break;
        case CONDCASE_TYPE_CHAR:
            printCexpCharCondCases(x->val.charCases);
            break;
        default:
            cant_happen("unrecognised type %d in printCexpCondCases", x->type);
    }
}

void printCexpLetRec(CexpLetRec *x) {
    fprintf(stderr, "(letrec ");
    printLetRecBindings(x->bindings);
    fprintf(stderr, " ");
    printExp(x->body);
    fprintf(stderr, ")");
}

void printLetRecBindings(LetRecBindings *x) {
    fprintf(stderr, "(");
    while (x != NULL) {
        fprintf(stderr, "(");
        printAexpVar(x->var);
        fprintf(stderr, " ");
        printAexp(x->val);
        fprintf(stderr, ")");
        if (x->next != NULL) {
            fprintf(stderr, " ");
        }
        x = x->next;
    }
    fprintf(stderr, ")");
}

void printCexpAmb(CexpAmb *x) {
    fprintf(stderr, "(amb ");
    printExp(x->exp1);
    fprintf(stderr, " ");
    printExp(x->exp2);
    fprintf(stderr, ")");
}

void printCexpCut(CexpCut *x) {
    fprintf(stderr, "(cut ");
    printExp(x->exp);
    fprintf(stderr, ")");
}

void printCexpBool(CexpBool *x) {
    fprintf(stderr, "(");
    switch (x->type) {
        case BOOL_TYPE_AND:
            fprintf(stderr, "and");
            break;
        case BOOL_TYPE_OR:
            fprintf(stderr, "or");
            break;
        default:
            cant_happen("unrecognised type %d in printCexpBool", x->type);
    }
    fprintf(stderr, " ");
    printExp(x->exp1);
    fprintf(stderr, " ");
    printExp(x->exp2);
    fprintf(stderr, ")");
}

void printMatchList(MatchList *x) {
    if (x == NULL) return;
    fprintf(stderr, "(");
    printAexpIntList(x->matches);
    fprintf(stderr, " ");
    printExp(x->body);
    fprintf(stderr, ")");
    if (x->next != NULL) {
        fprintf(stderr, " ");
        printMatchList(x->next);
    }
}

void printCexpMatch(CexpMatch *x) {
    fprintf(stderr, "(match ");
    printAexp(x->condition);
    fprintf(stderr, " ");
    printMatchList(x->clauses);
    fprintf(stderr, ")");
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
            fprintf(stderr, "#t");
            break;
        case AEXP_TYPE_FALSE:
            fprintf(stderr, "#f");
            break;
        case AEXP_TYPE_VOID:
            fprintf(stderr, "nil");
            break;
        case AEXP_TYPE_BIGINT:
            fprintBigInt(stderr, x->val.biginteger);
            break;
        case AEXP_TYPE_LITTLEINT:
            fprintf(stderr, "i%d", x->val.littleinteger);
            break;
        case AEXP_TYPE_CHAR:
            fprintf(stderr, "'%c'", x->val.character);
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
        case AEXP_TYPE_MAKEVEC:
            printAexpMakeVec(x->val.makeVec);
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
        case CEXP_TYPE_IF:
            printCexpIf(x->val.iff);
            break;
        case CEXP_TYPE_COND:
            printCexpCond(x->val.cond);
            break;
        case CEXP_TYPE_CALLCC:
            fprintf(stderr, "(call/cc ");
            printAexp(x->val.callCC);
            fprintf(stderr, ")");
            break;
        case CEXP_TYPE_LETREC:
            printCexpLetRec(x->val.letRec);
            break;
        case CEXP_TYPE_AMB:
            printCexpAmb(x->val.amb);
            break;
        case CEXP_TYPE_CUT:
            printCexpCut(x->val.cut);
            break;
        case CEXP_TYPE_BOOL:
            printCexpBool(x->val.boolean);
            break;
        case CEXP_TYPE_MATCH:
            printCexpMatch(x->val.match);
            break;
        case CEXP_TYPE_BACK:
            fprintf(stderr, "(back)");
            break;
        case CEXP_TYPE_ERROR:
            fprintf(stderr, "(error)");
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
            fprintf(stderr, "<DONE>");
            break;
        default:
            fprintf(stderr, "<unrecognised exp %d>", x->type);
            exit(1);
    }
}

void printExpLet(ExpLet *x) {
    fprintf(stderr, "(let (");
    printAexpVar(x->var);
    fprintf(stderr, " ");
    printExp(x->val);
    fprintf(stderr, ") ");
    printExp(x->body);
    fprintf(stderr, ")");
}

void dumpByteCode(ByteCodeArray *b) {
    int i = 0;
    /*
    while (i < b->count) {
        fprintf(stderr, "[%04x] %02x\n", i, b->entries[i]);
        i++;
    }
    i = 0;
    */
    while (i < b->count) {
        fprintf(stderr, "%04x ### ", i);
        switch (readByte(b, &i)) {
            case BYTECODE_NONE: {
                fprintf(stderr, "NONE\n");
            }
            break;
            case BYTECODE_LAM: {
                int nargs = readByte(b, &i);
                int letRecOffset = readByte(b, &i);
                int offset = readOffset(b, &i);
                fprintf(stderr, "LAM [%d] [%d] [%04x]\n", nargs, letRecOffset, offset);
            }
            break;
            case BYTECODE_VAR: {
                int frame = readByte(b, &i);
                int offset = readByte(b, &i);
                fprintf(stderr, "VAR [%d:%d]\n", frame, offset);
            }
            break;
            case BYTECODE_LVAR: {
                int offset = readByte(b, &i);
                fprintf(stderr, "LVAR [%d]\n", offset);
            }
            break;
            case BYTECODE_PRIM_ADD: {
                fprintf(stderr, "ADD\n");
            }
            break;
            case BYTECODE_PRIM_SUB: {
                fprintf(stderr, "SUB\n");
            }
            break;
            case BYTECODE_PRIM_MUL: {
                fprintf(stderr, "MUL\n");
            }
            break;
            case BYTECODE_PRIM_DIV: {
                fprintf(stderr, "DIV\n");
            }
            break;
            case BYTECODE_PRIM_POW: {
                fprintf(stderr, "POW\n");
            }
            break;
            case BYTECODE_PRIM_MOD: {
                fprintf(stderr, "MOD\n");
            }
            break;
            case BYTECODE_PRIM_EQ: {
                fprintf(stderr, "EQ\n");
            }
            break;
            case BYTECODE_PRIM_NE: {
                fprintf(stderr, "NE\n");
            }
            break;
            case BYTECODE_PRIM_GT: {
                fprintf(stderr, "GT\n");
            }
            break;
            case BYTECODE_PRIM_LT: {
                fprintf(stderr, "LT\n");
            }
            break;
            case BYTECODE_PRIM_GE: {
                fprintf(stderr, "GE\n");
            }
            break;
            case BYTECODE_PRIM_LE: {
                fprintf(stderr, "LE\n");
            }
            break;
            case BYTECODE_PRIM_XOR: {
                fprintf(stderr, "XOR\n");
            }
            break;
            case BYTECODE_PRIM_CONS: {
                fprintf(stderr, "CONS\n");
            }
            break;
            case BYTECODE_PRIM_CAR: {
                fprintf(stderr, "CAR\n");
            }
            break;
            case BYTECODE_PRIM_CDR: {
                fprintf(stderr, "CDR\n");
            }
            break;
            case BYTECODE_PRIM_NOT: {
                fprintf(stderr, "NOT\n");
            }
            break;
            case BYTECODE_PRIM_PRINT: {
                fprintf(stderr, "PRINT\n");
            }
            break;
            case BYTECODE_PRIM_MAKEVEC: {
                int size = readByte(b, &i);
                fprintf(stderr, "MAKEVEC [%d]\n", size);
            }
            break;
            case BYTECODE_PRIM_VEC: {
                fprintf(stderr, "VEC\n");
            }
            break;
            case BYTECODE_APPLY: {
                int nargs = readByte(b, &i);
                fprintf(stderr, "APPLY [%d]\n", nargs);
            }
            break;
            case BYTECODE_IF: {
                int offset = readOffset(b, &i);
                fprintf(stderr, "IF [%04x]\n", offset);
            }
            break;
            case BYTECODE_MATCH: {
                int count = readByte(b, &i);
                fprintf(stderr, "MATCH [%d]", count);
                while (count > 0) {
                    int offset = readOffset(b, &i);
                    fprintf(stderr, "[%04x]", offset);
                    count--;
                }
                fprintf(stderr, "\n");
            }
            break;
            case BYTECODE_CHARCOND: {
                int count = readWord(b, &i);
                fprintf(stderr, "CHARCOND [%d]", count);
                while (count > 0) {
                    int val = readInt(b, &i);
                    int offset = readOffset(b, &i);
                    fprintf(stderr, " %d:[%04x]", val, offset);
                    count--;
                }
                fprintf(stderr, "\n");
            }
            break;
            case BYTECODE_INTCOND: {
                int count = readWord(b, &i);
                fprintf(stderr, "INTCOND [%d]", count);
                while (count > 0) {
                    if (bigint_flag) {
                        bigint bi = readBigint(b, &i);
                        fprintf(stderr, " ");
                        bigint_fprint(stderr, &bi);
                        bigint_free(&bi);
                    } else {
                        int li = readInt(b, &i);
                        fprintf(stderr, " %d", li);
                    }
                    int offset = readOffset(b, &i);
                    fprintf(stderr, ":[%04x]", offset);
                    count--;
                }
                fprintf(stderr, "\n");
            }
            break;
            case BYTECODE_LETREC: {
                int size = readByte(b, &i);
                fprintf(stderr, "LETREC [%d]\n", size);
            }
            break;
            case BYTECODE_AMB: {
                int offset = readOffset(b, &i);
                fprintf(stderr, "AMB [%04x]\n", offset);
            }
            break;
            case BYTECODE_CUT: {
                fprintf(stderr, "CUT\n");
            }
            break;
            case BYTECODE_BACK: {
                fprintf(stderr, "BACK\n");
            }
            break;
            case BYTECODE_LET: {
                int offset = readOffset(b, &i);
                fprintf(stderr, "LET [%04x]\n", offset);
            }
            break;
            case BYTECODE_JMP: {
                int offset = readOffset(b, &i);
                fprintf(stderr, "JMP [%04x]\n", offset);
            }
            break;
            case BYTECODE_PUSHN: {
                int size = readByte(b, &i);
                fprintf(stderr, "PUSHN [%d]\n", size);
            }
            break;
            case BYTECODE_CALLCC: {
                fprintf(stderr, "CALLCC\n");
            }
            break;
            case BYTECODE_TRUE: {
                fprintf(stderr, "TRUE\n");
            }
            break;
            case BYTECODE_FALSE: {
                fprintf(stderr, "FALSE\n");
            }
            break;
            case BYTECODE_VOID: {
                fprintf(stderr, "VOID\n");
            }
            break;
            case BYTECODE_STDINT: {
                int val = readInt(b, &i);
                fprintf(stderr, "STDINT [%d]\n", val);
            }
            break;
            case BYTECODE_BIGINT: {
                fprintf(stderr, "BIGINT [");
                bigint bi = readBigint(b, &i);
                bigint_fprint(stderr, &bi);
                fprintf(stderr, "]\n");
                bigint_free(&bi);
            }
            break;
            case BYTECODE_CHAR: {
                char c = readByte(b, &i);
                fprintf(stderr, "CHAR [%c]\n", c);
            }
            break;
            case BYTECODE_RETURN: {
                fprintf(stderr, "RETURN\n");
            }
            break;
            case BYTECODE_DONE: {
                fprintf(stderr, "DONE\n");
            }
            break;
            case BYTECODE_ERROR: {
                fprintf(stderr, "ERROR\n");
            }
            break;
            default:
                cant_happen("unrecognised bytecode %d in dumpByteCode", b->entries[i]);
        }
    }
}
