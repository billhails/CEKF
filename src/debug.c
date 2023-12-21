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
    eprintf("%*s", depth * 4, "");
}

void printContainedValue(Value x, int depth) {
    switch (x.type) {
        case VALUE_TYPE_VOID:
            printPad(depth);
            eprintf("#V");
            break;
        case VALUE_TYPE_STDINT:
            printPad(depth);
            eprintf("%d", x.val.z);
            break;
        case VALUE_TYPE_BIGINT:
            printPad(depth);
            fprintBigInt(stderr, x.val.b);
            break;
        case VALUE_TYPE_CHARACTER:
            printPad(depth);
            eprintf("'%c'", x.val.c);
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
        eprintf("S/");
        return;
    }
    eprintf("SS[\n");
    for (int i = 0; i < s.frameSize; i++) {
        printContainedValue(s.frame[i], depth + 1);
        if (i < s.frameSize - 1) {
            eprintf(",");
        }
        eprintf("\n");
    }
    printPad(depth);
    eprintf("]");
}

static void printElidedSnapshot(Snapshot s) {
    if (s.frameSize == 0) {
        eprintf("S/");
        return;
    }
    eprintf("S[<...>]");
}

void printValue(Value x, int depth) {
    printPad(depth);
    if (x.type == VALUE_TYPE_VOID) {
        eprintf("V/");
        return;
    }
    eprintf("V[\n");
    printContainedValue(x, depth + 1);
    eprintf("\n");
    printPad(depth);
    eprintf("]");
}

void printElidedClo(Clo *x) {
    eprintf("C[%d, %04lx, E[<...>], ", x->nvar, x->c);
    eprintf("]");
}

void printElidedKont(Kont *x) {
    if (x == NULL) {
        eprintf("K/");
        return;
    }
    eprintf("K[");
    eprintf("%04lx, E[<...>], ", x->body);
    printElidedSnapshot(x->snapshot);
    printElidedKont(x->next);
    eprintf("]");
}

void printCons(Cons *x) {
    eprintf("(");
    printContainedValue(x->car, 0);
    eprintf(" . ");
    printContainedValue(x->cdr, 0);
    eprintf(")");
}

void printVec(Vec *x) {
    eprintf("#[");
    for (int i = 0; i < x->size; i++) {
        printContainedValue(x->values[i], 0);
        if (i + 1 < x->size) {
            eprintf(" ");
        }
    }
    eprintf("]");
}

void printElidedValue(Value x) {
    eprintf("V[");
    switch (x.type) {
        case VALUE_TYPE_VOID:
            eprintf("#V");
            break;
        case VALUE_TYPE_STDINT:
            eprintf("%d", x.val.z);
            break;
        case VALUE_TYPE_CHARACTER:
            eprintf("'%c'", x.val.c);
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
    eprintf("]");
}

static void printClo(Clo *x, int depth) {
    printPad(depth);
    eprintf("C[%d, %04lx, ", x->nvar, x->c);
    printElidedEnv(x->rho);
    eprintf("]");
}

void printCEKF(CEKF *x) {
    int depth = 1;
    eprintf("\nCEKF (\n");
    printPad(depth);
    eprintf("%04lx", x->C);
    eprintf(",\n");
    printEnv(x->E, depth);
    eprintf(",\n");
    printKont(x->K, depth);
    eprintf(",\n");
    printFail(x->F, depth);
    eprintf(",\n");
    printValue(x->V, depth);
    eprintf(",\n");
    printStack(&x->S, depth);
    eprintf("\n)\n\n");
}

static void printStack(Stack *x, int depth) {
    printPad(depth);
    if (x == NULL || x->sp ==0) {
        eprintf("S/");
        return;
    }
    eprintf("S[\n");
    for (int i = 0; i < x->sp; ++i) {
        printContainedValue(peekValue(x, i), depth + 1);
        if (i < x->sp - 1) {
            eprintf(",");
        }
        eprintf("\n");
    }
    printPad(depth);
    eprintf("]");
}

static void printValues(Value *values, int count, int depth) {
    printPad(depth);
    eprintf("{\n");
    for (int i = 0; i < count; ++i) {
        printValue(values[i], depth + 1);
        if (i + 1 < count) {
            eprintf(",");
        }
        eprintf("\n");
    }
    printPad(depth);
    eprintf("}");
}

void printElidedValues(Value *values, int count) {
    eprintf("{");
    for (int i = 0; i < count; ++i) {
        printElidedValue(values[i]);
        if (i + 1 < count) {
            eprintf(", ");
        }
    }
    eprintf("}");
}

void printEnv(Env *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        eprintf("E/");
        return;
    }
    eprintf("E[\n");
    while (x != NULL) {
        printValues(x->values, x->count, depth + 1);
        if (x->next != NULL) eprintf(",");
        eprintf("\n");
        x = x->next;
    }
    printPad(depth);
    eprintf("]");
}

void printCTEnv(CTEnv *x) {
    eprintf("CTEnv[");
    while (x != NULL) {
        printHashTable(x->table, 0);
        if (x->next != NULL) eprintf(", ");
        x = x->next;
    }
    eprintf("]");
}

void printElidedEnv(Env *x) {
    if (x == NULL) {
        eprintf("E/");
        return;
    }
    eprintf("E[");
    while (x != NULL) {
        printElidedValues(x->values, x->count);
        if (x->next != NULL) eprintf(", ");
        x = x->next;
    }
    eprintf("]");
}

static void printKont(Kont *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        eprintf("K/");
        return;
    }
    eprintf("K[\n");
    if (x != NULL) {
        printPad(depth + 1);
        eprintf("%04lx,\n", x->body);
        printEnv(x->rho, depth + 1);
        eprintf(",\n");
        printSnapshot(x->snapshot, depth + 1);
        eprintf(",\n");
        printKont(x->next, depth + 1);
        eprintf("\n");
    }
    printPad(depth);
    eprintf("]");
}

static void printFail(Fail *x, int depth) {
    printPad(depth);
    if (x == NULL) {
        eprintf("F/");
        return;
    }
    eprintf("F[\n");
    if (x != NULL) {
        printPad(depth + 1);
        eprintf("%04lx", x->exp);
        eprintf(",\n");
        printEnv(x->rho, depth + 1);
        eprintf(",\n");
        printSnapshot(x->snapshot, depth + 1);
        eprintf(",\n");
        printKont(x->k, depth + 1);
        eprintf(",\n");
        printFail(x->next, depth + 1);
        eprintf("\n");
    }
    printPad(depth);
    eprintf("]");
}

void printAexpLam(AexpLam *x) {
    eprintf("(lambda ");
    printAexpVarList(x->args);
    eprintf(" ");
    printExp(x->exp);
    eprintf(")");
}

void printAexpVarList(AexpVarList *x) {
    eprintf("(");
    while (x != NULL) {
        printAexpVar(x->var);
        if (x->next != NULL) {
            eprintf(" ");
        }
        x = x->next;
    }
    eprintf(")");
}

void printAexpVar(HashSymbol *x) {
    eprintf("%s", x->name);
}

void printAexpAnnotatedVar(AexpAnnotatedVar *x) {
    printAexpVar(x->var);
    if (x->type == VAR_TYPE_STACK)
        eprintf(":%d", x->offset);
    else
        eprintf(":%d:%d", x->frame, x->offset);
}

void printAexpPrimApp(AexpPrimApp *x) {
    eprintf("(");
    switch(x->op) {
        case AEXP_PRIM_ADD:
            eprintf("add ");
            break;
        case AEXP_PRIM_SUB:
            eprintf("sub ");
            break;
        case AEXP_PRIM_MUL:
            eprintf("mul ");
            break;
        case AEXP_PRIM_DIV:
            eprintf("div ");
            break;
        case AEXP_PRIM_EQ:
            eprintf("eq ");
            break;
        case AEXP_PRIM_NE:
            eprintf("ne ");
            break;
        case AEXP_PRIM_GT:
            eprintf("gt ");
            break;
        case AEXP_PRIM_LT:
            eprintf("lt ");
            break;
        case AEXP_PRIM_GE:
            eprintf("ge ");
            break;
        case AEXP_PRIM_LE:
            eprintf("le ");
            break;
        case AEXP_PRIM_XOR:
            eprintf("xor ");
            break;
        case AEXP_PRIM_CONS:
            eprintf("cons ");
            break;
        case AEXP_PRIM_VEC:
            eprintf("vec ");
            break;
        case AEXP_PRIM_MOD:
            eprintf("mod ");
            break;
        default:
            cant_happen("unrecognized op in printAexpPrimApp (%d)", x->op);
    }
    printAexp(x->exp1);
    if (x->exp2 != NULL) {
        eprintf(" ");
        printAexp(x->exp2);
    }
    eprintf(")");
}

void printAexpUnaryApp(AexpUnaryApp *x) {
    eprintf("(");
    switch(x->op) {
        case AEXP_UNARY_CAR:
            eprintf("car ");
            break;
        case AEXP_UNARY_CDR:
            eprintf("cdr ");
            break;
        case AEXP_UNARY_NOT:
            eprintf("not ");
            break;
        case AEXP_UNARY_PRINT:
            eprintf("print ");
            break;
        default:
            cant_happen("unrecognized op in printAexpUnaryApp (%d)", x->op);
    }
    printAexp(x->exp);
    eprintf(")");
}

static void printAexpListContents(AexpList *x) {
    while (x != NULL) {
        printAexp(x->exp);
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void printAexpList(AexpList *x) {
    eprintf("(");
    printAexpListContents(x);
    eprintf(")");
}

static void printAexpIntListContents(AexpIntList *x) {
    while (x != NULL) {
        eprintf("%d", x->integer);
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void printAexpIntList(AexpIntList *x) {
    eprintf("(");
    printAexpIntListContents(x);
    eprintf(")");
}

void printAexpMakeList(AexpList *x) {
    eprintf("(list ");
    printAexpListContents(x);
    eprintf(")");
}

void printAexpMakeVec(AexpMakeVec *x) {
    eprintf("(make-vec ");
    printAexpListContents(x->args);
    eprintf(")");
}

void printBareAexpList(AexpList *x) {
    while (x != NULL) {
        printAexp(x->exp);
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void printCexpApply(CexpApply *x) {
    eprintf("(");
    printAexp(x->function);
    eprintf(" ");
    printBareAexpList(x->args);
    eprintf(")");
}

void printCexpIf(CexpIf *x) {
    eprintf("(if ");
    printAexp(x->condition);
    eprintf(" ");
    printExp(x->consequent);
    eprintf(" ");
    printExp(x->alternative);
    eprintf(")");
}

void printCexpCond(CexpCond *x) {
    eprintf("(cond ");
    printAexp(x->condition);
    eprintf(" ");
    printCexpCondCases(x->cases);
    eprintf(")");
}

void printCexpIntCondCases(CexpIntCondCases *x) {
    while (x != NULL) {
        eprintf("(");
        fprintBigInt(stderr, x->option);
        eprintf(" ");
        printExp(x->body);
        eprintf(")");
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void printCexpCharCondCases(CexpCharCondCases *x) {
    while (x != NULL) {
        eprintf("('%c' ", x->option);
        printExp(x->body);
        eprintf(")");
        if (x->next) {
            eprintf(" ");
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
    eprintf("(letrec ");
    printLetRecBindings(x->bindings);
    eprintf(" ");
    printExp(x->body);
    eprintf(")");
}

void printLetRecBindings(LetRecBindings *x) {
    eprintf("(");
    while (x != NULL) {
        eprintf("(");
        printAexpVar(x->var);
        eprintf(" ");
        printAexp(x->val);
        eprintf(")");
        if (x->next != NULL) {
            eprintf(" ");
        }
        x = x->next;
    }
    eprintf(")");
}

void printCexpAmb(CexpAmb *x) {
    eprintf("(amb ");
    printExp(x->exp1);
    eprintf(" ");
    printExp(x->exp2);
    eprintf(")");
}

void printCexpCut(CexpCut *x) {
    eprintf("(cut ");
    printExp(x->exp);
    eprintf(")");
}

void printCexpBool(CexpBool *x) {
    eprintf("(");
    switch (x->type) {
        case BOOL_TYPE_AND:
            eprintf("and");
            break;
        case BOOL_TYPE_OR:
            eprintf("or");
            break;
        default:
            cant_happen("unrecognised type %d in printCexpBool", x->type);
    }
    eprintf(" ");
    printExp(x->exp1);
    eprintf(" ");
    printExp(x->exp2);
    eprintf(")");
}

void printMatchList(MatchList *x) {
    if (x == NULL) return;
    eprintf("(");
    printAexpIntList(x->matches);
    eprintf(" ");
    printExp(x->body);
    eprintf(")");
    if (x->next != NULL) {
        eprintf(" ");
        printMatchList(x->next);
    }
}

void printCexpMatch(CexpMatch *x) {
    eprintf("(match ");
    printAexp(x->condition);
    eprintf(" ");
    printMatchList(x->clauses);
    eprintf(")");
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
            eprintf("#t");
            break;
        case AEXP_TYPE_FALSE:
            eprintf("#f");
            break;
        case AEXP_TYPE_VOID:
            eprintf("nil");
            break;
        case AEXP_TYPE_BIGINT:
            fprintBigInt(stderr, x->val.biginteger);
            break;
        case AEXP_TYPE_LITTLEINT:
            eprintf("%d", x->val.littleinteger);
            break;
        case AEXP_TYPE_CHAR:
            eprintf("'%c'", x->val.character);
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
            eprintf("(call/cc ");
            printAexp(x->val.callCC);
            eprintf(")");
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
            eprintf("(back)");
            break;
        case CEXP_TYPE_ERROR:
            eprintf("(error)");
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
            eprintf("<DONE>");
            break;
        default:
            eprintf("<unrecognised exp %d>", x->type);
            exit(1);
    }
}

void printExpLet(ExpLet *x) {
    eprintf("(let (");
    printAexpVar(x->var);
    eprintf(" ");
    printExp(x->val);
    eprintf(") ");
    printExp(x->body);
    eprintf(")");
}

void dumpByteCode(ByteCodeArray *b) {
    size_t i = 0;
    /*
    while (i < b->count) {
        eprintf("[%04x] %02x\n", i, b->entries[i]);
        i++;
    }
    i = 0;
    */
    while (i < b->count) {
        eprintf("%04lx ### ", i);
        switch (readByte(b, &i)) {
            case BYTECODE_NONE: {
                eprintf("NONE\n");
            }
            break;
            case BYTECODE_LAM: {
                int nargs = readByte(b, &i);
                int letRecOffset = readByte(b, &i);
                int offset = readOffset(b, &i);
                eprintf("LAM [%d] [%d] [%04x]\n", nargs, letRecOffset, offset);
            }
            break;
            case BYTECODE_VAR: {
                int frame = readByte(b, &i);
                int offset = readByte(b, &i);
                eprintf("VAR [%d:%d]\n", frame, offset);
            }
            break;
            case BYTECODE_LVAR: {
                int offset = readByte(b, &i);
                eprintf("LVAR [%d]\n", offset);
            }
            break;
            case BYTECODE_PRIM_ADD: {
                eprintf("ADD\n");
            }
            break;
            case BYTECODE_PRIM_SUB: {
                eprintf("SUB\n");
            }
            break;
            case BYTECODE_PRIM_MUL: {
                eprintf("MUL\n");
            }
            break;
            case BYTECODE_PRIM_DIV: {
                eprintf("DIV\n");
            }
            break;
            case BYTECODE_PRIM_POW: {
                eprintf("POW\n");
            }
            break;
            case BYTECODE_PRIM_MOD: {
                eprintf("MOD\n");
            }
            break;
            case BYTECODE_PRIM_EQ: {
                eprintf("EQ\n");
            }
            break;
            case BYTECODE_PRIM_NE: {
                eprintf("NE\n");
            }
            break;
            case BYTECODE_PRIM_GT: {
                eprintf("GT\n");
            }
            break;
            case BYTECODE_PRIM_LT: {
                eprintf("LT\n");
            }
            break;
            case BYTECODE_PRIM_GE: {
                eprintf("GE\n");
            }
            break;
            case BYTECODE_PRIM_LE: {
                eprintf("LE\n");
            }
            break;
            case BYTECODE_PRIM_XOR: {
                eprintf("XOR\n");
            }
            break;
            case BYTECODE_PRIM_CONS: {
                eprintf("CONS\n");
            }
            break;
            case BYTECODE_PRIM_CAR: {
                eprintf("CAR\n");
            }
            break;
            case BYTECODE_PRIM_CDR: {
                eprintf("CDR\n");
            }
            break;
            case BYTECODE_PRIM_NOT: {
                eprintf("NOT\n");
            }
            break;
            case BYTECODE_PRIM_PRINT: {
                eprintf("PRINT\n");
            }
            break;
            case BYTECODE_PRIM_MAKEVEC: {
                int size = readByte(b, &i);
                eprintf("MAKEVEC [%d]\n", size);
            }
            break;
            case BYTECODE_PRIM_VEC: {
                eprintf("VEC\n");
            }
            break;
            case BYTECODE_APPLY: {
                int nargs = readByte(b, &i);
                eprintf("APPLY [%d]\n", nargs);
            }
            break;
            case BYTECODE_IF: {
                int offset = readOffset(b, &i);
                eprintf("IF [%04x]\n", offset);
            }
            break;
            case BYTECODE_MATCH: {
                int count = readByte(b, &i);
                eprintf("MATCH [%d]", count);
                while (count > 0) {
                    int offset = readOffset(b, &i);
                    eprintf("[%04x]", offset);
                    count--;
                }
                eprintf("\n");
            }
            break;
            case BYTECODE_CHARCOND: {
                int count = readWord(b, &i);
                eprintf("CHARCOND [%d]", count);
                while (count > 0) {
                    int val = readInt(b, &i);
                    int offset = readOffset(b, &i);
                    eprintf(" %d:[%04x]", val, offset);
                    count--;
                }
                eprintf("\n");
            }
            break;
            case BYTECODE_INTCOND: {
                int count = readWord(b, &i);
                eprintf("INTCOND [%d]", count);
                while (count > 0) {
                    if (bigint_flag) {
                        bigint bi = readBigint(b, &i);
                        eprintf(" ");
                        bigint_fprint(stderr, &bi);
                        bigint_free(&bi);
                    } else {
                        int li = readInt(b, &i);
                        eprintf(" %d", li);
                    }
                    int offset = readOffset(b, &i);
                    eprintf(":[%04x]", offset);
                    count--;
                }
                eprintf("\n");
            }
            break;
            case BYTECODE_LETREC: {
                int size = readByte(b, &i);
                eprintf("LETREC [%d]\n", size);
            }
            break;
            case BYTECODE_AMB: {
                int offset = readOffset(b, &i);
                eprintf("AMB [%04x]\n", offset);
            }
            break;
            case BYTECODE_CUT: {
                eprintf("CUT\n");
            }
            break;
            case BYTECODE_BACK: {
                eprintf("BACK\n");
            }
            break;
            case BYTECODE_LET: {
                int offset = readOffset(b, &i);
                eprintf("LET [%04x]\n", offset);
            }
            break;
            case BYTECODE_JMP: {
                int offset = readOffset(b, &i);
                eprintf("JMP [%04x]\n", offset);
            }
            break;
            case BYTECODE_PUSHN: {
                int size = readByte(b, &i);
                eprintf("PUSHN [%d]\n", size);
            }
            break;
            case BYTECODE_CALLCC: {
                eprintf("CALLCC\n");
            }
            break;
            case BYTECODE_TRUE: {
                eprintf("TRUE\n");
            }
            break;
            case BYTECODE_FALSE: {
                eprintf("FALSE\n");
            }
            break;
            case BYTECODE_VOID: {
                eprintf("VOID\n");
            }
            break;
            case BYTECODE_STDINT: {
                int val = readInt(b, &i);
                eprintf("STDINT [%d]\n", val);
            }
            break;
            case BYTECODE_BIGINT: {
                eprintf("BIGINT [");
                bigint bi = readBigint(b, &i);
                bigint_fprint(stderr, &bi);
                eprintf("]\n");
                bigint_free(&bi);
            }
            break;
            case BYTECODE_CHAR: {
                char c = readByte(b, &i);
                eprintf("CHAR [%c]\n", c);
            }
            break;
            case BYTECODE_RETURN: {
                eprintf("RETURN\n");
            }
            break;
            case BYTECODE_DONE: {
                eprintf("DONE\n");
            }
            break;
            case BYTECODE_ERROR: {
                eprintf("ERROR\n");
            }
            break;
            default:
                cant_happen("unrecognised bytecode %d in dumpByteCode", b->entries[i]);
        }
    }
}
