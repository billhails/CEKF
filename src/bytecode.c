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

// This file contains code for maintaining the bytecode array as well
// as generating bytecode from ANF lambda expressions.

#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "bytecode.h"
#include "common.h"

#ifdef DEBUG_BYTECODE
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif


void initByteCodeArray(ByteCodeArray *b) {
    b->count = 0;
    b->capacity = 0;
    b->entries = NULL;
}

void resetByteCodeArray(ByteCodeArray *b) {
    b->count = 0;
    for (int i = 0; i < b->capacity; i++) {
        b->entries[i] = BYTECODE_NONE;
    }
}

static void growCapacity(ByteCodeArray *byteCodes, int newCapacity) {
    int oldCapacity = byteCodes->capacity;
    byte *entries = GROW_ARRAY(byte, byteCodes->entries, oldCapacity, newCapacity);

    for (int i = oldCapacity; i < newCapacity; i++) {
        entries[i] = BYTECODE_NONE;
    }

    byteCodes->entries = entries;
    byteCodes->capacity = newCapacity;
}

static inline void reserve(ByteCodeArray *b, size_t size) {
    while ((b->count + size) >= b->capacity) {
        growCapacity(b, b->capacity < 8 ? 8 : b->capacity * 2);
    }
}

static void addByte(ByteCodeArray *b, int code) {
    if (code > 255) {
        cant_happen("maximim byte size exceeded");
    }
    DEBUG("%04x addByte %02x", b->count, code);
    reserve(b, sizeof(byte));
    b->entries[b->count++] = code;
}

static void writeWordAt(int loc, ByteCodeArray *b, word word) {
    if (word > 65535) {
        cant_happen("maximum word size exceeded");
    }
    DEBUG("%04x writeWord %04x", loc, word);
    memcpy(&b->entries[loc], &word, sizeof(word));
}

static void writeIntAt(int loc, ByteCodeArray *b, int word) {
    DEBUG("%04x writeInt %d", loc, word);
    memcpy(&b->entries[loc], &word, sizeof(int));
}

static void writeCurrentAddressAt(int patch, ByteCodeArray *b) {
    word offset = b->count - patch;
    writeWordAt(patch, b, offset);
}

static void addWord(ByteCodeArray *b, word w) {
    if (w > 65535) {
        cant_happen("maximum word size exceeded");
    }
    reserve(b, sizeof(word));
    writeWordAt(b->count, b, w);
    b->count += sizeof(word);
}

static int reserveWord(ByteCodeArray *b) {
    int address = b->count;
    addWord(b, 0);
    return address;
}

static void addInt(ByteCodeArray *b, int word) {
    if (word > 4294967295) {
        cant_happen("maximum int size exceeded");
    }
    reserve(b, sizeof(int));
    writeIntAt(b->count, b, word);
    b->count += sizeof(int);
}

static int reserveInt(ByteCodeArray *b) {
    int address = b->count;
    addInt(b, 0);
    return address;
}

static void addBig(ByteCodeArray *b, bigint bi) {
    addInt(b, bi.size);
    addInt(b, bi.capacity);
    addByte(b, bi.neg);
    size_t nBytes = bi.capacity * sizeof(bigint_word);
    reserve(b, nBytes);
    DEBUG("%04x addBig nBytes %ld", b->count, nBytes);
    memcpy(&b->entries[b->count], &bi.words[0], nBytes);
    b->count += nBytes;
}

byte readByte(ByteCodeArray *b, int *i) {
    return b->entries[(*i)++];
}

static inline void _readWord(ByteCodeArray *b, int *i, word *a) {
    memcpy(a, &b->entries[*i], sizeof(word));
    (*i) += sizeof(word);
}

word readWord(ByteCodeArray *b, int *i) {
    word a;
    _readWord(b, i, &a);
    return a;
}

static inline void _readInt(ByteCodeArray *b, int *i, int *a) {
    memcpy(a, &b->entries[*i], sizeof(int));
    (*i) += sizeof(int);
}

int readInt(ByteCodeArray *b, int *i) {
    int a;
    _readInt(b, i, &a);
    return a;
}

int readOffset(ByteCodeArray *b, int *i) {
    int ii = *i;
    int offset = readWord(b, i);
    return ii + offset;
}

int readOffsetAt(ByteCodeArray *b, int i, int step) {
    int ii = i + step * sizeof(word);
    int offset = readWord(b, &ii);
    return i + offset + step * sizeof(word);
}

bigint readBigint(ByteCodeArray *b, int *i) {
    bigint a;
    bigint_init(&a);
    int size;
    int capacity;
    _readInt(b, i, &size);
    _readInt(b, i, &capacity);
    int neg = readByte(b, i);
    bigint_reserve(&a, capacity);
    int nbytes = capacity * sizeof(bigint_word);
    memcpy(a.words, &b->entries[*i], nbytes);
    (*i) += nbytes;
    a.size = size;
    a.neg = neg;
    return a;
}

////////////////////////////////////////////////////////////////////////////

void writeAexpLam(AexpLam *x, ByteCodeArray *b) {
    ENTER(writeAexpLam);
    if (x == NULL) return;
    addByte(b, BYTECODE_LAM);
    addByte(b, x->nargs);
    addByte(b, x->letRecOffset);
    int patch = reserveWord(b);
    writeExp(x->exp, b);
    addByte(b, BYTECODE_RETURN);
    writeCurrentAddressAt(patch, b);
    LEAVE(writeAexpLam);
}

void writeAexpVarList(AexpVarList *x, ByteCodeArray *b) {
    ENTER(writeAexpVarList);
    cant_happen("writeAexpVarList called");
    LEAVE(writeAexpVarList);
}

void writeAexpVar(HashSymbol *x, ByteCodeArray *b) {
    ENTER(writeAexpVar);
    cant_happen("writeAexpVar called");
    LEAVE(writeAexpVar);
}

void writeAexpAnnotatedVar(AexpAnnotatedVar *x, ByteCodeArray *b) {
    ENTER(writeAexpAnnotatedVar);
    if (x == NULL) return;
    switch (x->type) {
        case VAR_TYPE_ENV:
            addByte(b, BYTECODE_VAR);
            addByte(b, x->frame);
            addByte(b, x->offset);
            break;
        case VAR_TYPE_STACK:
            addByte(b, BYTECODE_LVAR);
            addByte(b, x->offset);
            break;
        default:
            cant_happen("unrecognised annotated var type");
    }
            
    LEAVE(writeAexpAnnotatedVar);
}

void writeAexpUnaryApp(AexpUnaryApp *x, ByteCodeArray *b) {
    ENTER(writeAexpUnaryApp);
    if (x == NULL) return;
    writeAexp(x->exp, b);
    byte prim;
    switch (x->op) {
        case AEXP_UNARY_CAR:
            prim = BYTECODE_PRIM_CAR;
            break;
        case AEXP_UNARY_CDR:
            prim = BYTECODE_PRIM_CDR;
            break;
        case AEXP_UNARY_NOT:
            prim = BYTECODE_PRIM_NOT;
            break;
        case AEXP_UNARY_PRINT:
            prim = BYTECODE_PRIM_PRINT;
            break;
        default:
            cant_happen("unrecognised AexpUnaryOp in writeAexpUnaryApp");
    }
    addByte(b, prim);
    LEAVE(writeAexpUnaryApp);
}

void writeAexpPrimApp(AexpPrimApp *x, ByteCodeArray *b) {
    ENTER(writeAexpPrimApp);
    if (x == NULL) return;
    writeAexp(x->exp1, b);
    writeAexp(x->exp2, b);
    byte prim;
    switch (x->op) {
        case AEXP_PRIM_ADD:
            prim = BYTECODE_PRIM_ADD;
            break;
        case AEXP_PRIM_SUB:
            prim = BYTECODE_PRIM_SUB;
            break;
        case AEXP_PRIM_MUL:
            prim = BYTECODE_PRIM_MUL;
            break;
        case AEXP_PRIM_DIV:
            prim = BYTECODE_PRIM_DIV;
            break;
        case AEXP_PRIM_POW:
            prim = BYTECODE_PRIM_POW;
            break;
        case AEXP_PRIM_MOD:
            prim = BYTECODE_PRIM_MOD;
            break;
        case AEXP_PRIM_EQ:
            prim = BYTECODE_PRIM_EQ;
            break;
        case AEXP_PRIM_NE:
            prim = BYTECODE_PRIM_NE;
            break;
        case AEXP_PRIM_GT:
            prim = BYTECODE_PRIM_GT;
            break;
        case AEXP_PRIM_LT:
            prim = BYTECODE_PRIM_LT;
            break;
        case AEXP_PRIM_GE:
            prim = BYTECODE_PRIM_GE;
            break;
        case AEXP_PRIM_LE:
            prim = BYTECODE_PRIM_LE;
            break;
        case AEXP_PRIM_XOR:
            prim = BYTECODE_PRIM_XOR;
            break;
        case AEXP_PRIM_CONS:
            prim = BYTECODE_PRIM_CONS;
            break;
        case AEXP_PRIM_VEC:
            prim = BYTECODE_PRIM_VEC;
            break;
        default:
            cant_happen("unrecognised AexpPrimOp in writeAexpPrimApp");
    }
    addByte(b, prim);
    LEAVE(writeAexpPrimApp);
}

void writeAexpList(AexpList *x, ByteCodeArray *b) {
    ENTER(writeAexpList);
    while (x != NULL) {
        writeAexp(x->exp, b);
        x = x->next;
    }
    LEAVE(writeAexpList);
}

void writeAexpMakeVec(AexpMakeVec *x, ByteCodeArray *b) {
    ENTER(writeAexpMakeVec);
    writeAexpList(x->args, b);
    addByte(b, BYTECODE_PRIM_MAKEVEC);
    addByte(b, x->nargs);
    LEAVE(writeAexpMakeVec);
}

void writeCexpApply(CexpApply *x, ByteCodeArray *b) {
    ENTER(writeCexpApply);
    writeAexpList(x->args, b);
    writeAexp(x->function, b);
    addByte(b, BYTECODE_APPLY);
    addByte(b, x->nargs);
    LEAVE(writeCexpApply);
}

void writeCexpIf(CexpIf *x, ByteCodeArray *b) {
    ENTER(writeCexpIf);
    writeAexp(x->condition, b);
    addByte(b, BYTECODE_IF);
    int patch = reserveWord(b);
    writeExp(x->consequent, b);
    addByte(b, BYTECODE_JMP);
    int patch2 = reserveWord(b);
    writeCurrentAddressAt(patch, b);
    writeExp(x->alternative, b);
    writeCurrentAddressAt(patch2, b);
    LEAVE(writeCexpIf);
}

static int countCexpCharCondCases(CexpCharCondCases *x) {
    int val = 0;
    while (x != NULL) {
        val++;
        x = x->next;
    }
    return val;
}

static int countCexpIntCondCases(CexpIntCondCases *x) {
    int val = 0;
    while (x != NULL) {
        val++;
        x = x->next;
    }
    return val;
}

void writeCexpCharCondCases(int depth, int *values, int *addresses, int *jumps, CexpCharCondCases *x, ByteCodeArray *b) {
    ENTER(writeCexpCharCondCases);
    if (x == NULL) {
        return;
    }
    writeCexpCharCondCases(depth + 1, values, addresses, jumps, x->next, b);
    if (x->next == NULL) { // default
        writeExp(x->body, b);
    } else {
        writeIntAt(values[depth], b, x->option);
        writeCurrentAddressAt(addresses[depth], b);
        writeExp(x->body, b);
    }
    if (depth > 0) {
        addByte(b, BYTECODE_JMP);
        jumps[depth - 1] = reserveWord(b);
    }
    LEAVE(writeCexpCharCondCases);
}

//                                                  +-----------------------------------------------------------------------------------------------------+
//                                                  |                                                                                                     |
// | ..value.. | CHARCOND | numCases | value_1 | jump_1 | ... | value_n | jump_n | ..default.. | JMP | addr(end) | ..action_n.. | JMP | addr(end) | ... | ..action_1.. | ..end
//                                                                           |                         |           |                    |                                |
//                                                                           +-------------------------|-----------+                    +--------------------------------+
//                                                                                                     +-----------------------------------------------------------------+
void writeCexpCharCond(CexpCharCondCases *x, ByteCodeArray *b) {
    ENTER(writeCexpCharCond);
    addByte(b, BYTECODE_CHARCOND);
    int numCases = countCexpCharCondCases(x);
    numCases--; // don't count the default case
    if (numCases <= 0) {
        cant_happen("zero cases in writeCexpCharCond");
    }
    addWord(b, numCases);
    int *values = NEW_ARRAY(int, numCases); // address in b for each index_i
    int *addresses = NEW_ARRAY(int, numCases); // address in b for each addr(exp_i)
    int *jumps = NEW_ARRAY(int, numCases); // address in b for the JMP patch address at the end of each expression
    for (int i = 0; i < numCases; i++) {
        values[i] = reserveInt(b); // TODO can change this to a char later, but then again, wchar_t...
        addresses[i] = reserveWord(b);
    }
    writeCexpCharCondCases(0, values, addresses, jumps, x, b);
    for (int i = 0; i < numCases; i++) {
        writeCurrentAddressAt(jumps[i], b);
    }
    FREE_ARRAY(int, values, numCases);
    FREE_ARRAY(int, addresses, numCases);
    FREE_ARRAY(int, jumps, numCases);
    LEAVE(writeCexpCharCond);
}

void writeCexpIntCondCases(CexpIntCondCases *x, ByteCodeArray *b, int *endJumps, int *dispatches, int index) {
    ENTER(writeCexpIntCondCases);
    if (x == NULL) return;
    writeCexpIntCondCases(x->next, b, endJumps, dispatches, index + 1);
    if (x->next != NULL) { // last case is default, first one written, no dispatch as it follows the jmp table
        writeCurrentAddressAt(dispatches[index + 1], b);
    }
    writeExp(x->body, b);
    if (index != -1) { // -1 is first case. last one written out, doesn't need a JMP to end as the end immediately follows
        addByte(b, BYTECODE_JMP);
        endJumps[index] = reserveWord(b);
    }
    LEAVE(writeCexpIntCondCases);
}

void writeCexpIntCond(CexpIntCondCases *x, ByteCodeArray *b) {
    ENTER(writeCexpIntCond);
    addByte(b, BYTECODE_INTCOND);
    int numCases = countCexpIntCondCases(x);
    numCases--; // don't count the default case
    if (numCases <= 0) {
        cant_happen("zero cases in writeCexpIntCond");
    }
    addWord(b, numCases);
    // we start out by writing each of the cases, reserving a slot in memory for each dispatch address after each (variable length) value.
    int *dispatches = NEW_ARRAY(int, numCases); // address of the slots for each dispatch address
    {
        int i = 0;
        for (CexpIntCondCases *xx = x; xx != NULL; xx = xx->next) {
            if (xx->next == NULL) break; // default case doesn't get a test
            addBig(b, xx->option->bi);
            dispatches[i++] = reserveWord(b);
        }
    }
    // next we right-recurse on the expressions (so the default directly follows the dispatch table)
    int *endJumps = NEW_ARRAY(int, numCases); // address in b for the JMP patch address at the end of each expression which jumps to the end
    writeCexpIntCondCases(x, b, endJumps, dispatches, -1);
    // lastly we patch the escape addresses of the clauses.
    for (int  i = 0; i < numCases; i++) {
        writeCurrentAddressAt(endJumps[i], b);
    }
    FREE_ARRAY(int, dispatches, numCases);
    FREE_ARRAY(int, endJumps, numCases);
    LEAVE(writeCexpIntCond);
}

void writeCexpCond(CexpCond *x, ByteCodeArray *b) {
    ENTER(writeCexpCond);
    writeAexp(x->condition, b);
    switch (x->cases->type) {
        case CONDCASE_TYPE_INT:
            writeCexpIntCond(x->cases->val.intCases, b);
            break;
        case CONDCASE_TYPE_CHAR:
            writeCexpCharCond(x->cases->val.charCases, b);
            break;
        default:
            cant_happen("unrecognised type %d in writeCexpCond", x->cases->type);
    }
    LEAVE(writeCexpCond);
}

void writeCexpLetRec(CexpLetRec *x, ByteCodeArray *b) {
    ENTER(writeCexpLetRec);
    writeLetRecBindings(x->bindings, b);
    addByte(b, BYTECODE_LETREC);
    addByte(b, x->nbindings);
    writeExp(x->body, b);
    LEAVE(writeCexpLetRec);
}

static int validateCexpMatch(CexpMatch *x) {
    bool seen[256];
    for (int i = 0; i < 256; ++i) {
        seen[i] = false;
    }
    for (MatchList *m = x->clauses; m != NULL; m = m->next) {
        for (AexpIntList *matches = m->matches; matches != NULL; matches = matches->next) {
            int index = matches->integer;
            if (seen[index]) {
                cant_happen("duplicate index %d in validateCexpMatch", index);
            }
            seen[index] = true;
        }
    }
    bool end = false;
    int count = 0;
    for (int i = 0; i < 256; ++i) {
        if (seen[i]) {
            if (end)
                cant_happen("non-contiguous match indices in validateCexpMatch");
            else
                count = i + 1;
        } else {
            end = true;
        }
    }
    if (count == 0)
        cant_happen("empty match indices in validateCexpMatch");
    return count;
}

void writeCexpMatch(CexpMatch *x, ByteCodeArray *b) {
    ENTER(writeCexpMatch);
    int count = validateCexpMatch(x);
    writeAexp(x->condition, b);
    addByte(b, BYTECODE_MATCH);
    // create a dispatch table
    addByte(b, count);
    int patches[256];
    int jumps[256];
    for (int i = 0; i < count; ++i) {
        patches[i] = reserveWord(b);
    }
    int jumpCounter = 0;
    for (MatchList *m = x->clauses; m != NULL; m = m->next) {
        for (AexpIntList *l = m->matches; l != NULL; l = l->next) {
            int i = l->integer;
            writeCurrentAddressAt(patches[i], b);
        }
        writeExp(m->body, b);
        addByte(b, BYTECODE_JMP);
        jumps[jumpCounter++] = reserveWord(b);
    }
    for (int i = 0; i < jumpCounter; i++)
        writeCurrentAddressAt(jumps[i], b);
    LEAVE(writeCexpMatch);
}

void writeLetRecBindings(LetRecBindings *x, ByteCodeArray *b) {
    ENTER(writeLetRecBindings);
    while (x != NULL) {
        writeAexp(x->val, b);
        x = x->next;
    }
    LEAVE(writeLetRecBindings);
}

void writeCexpAmb(CexpAmb *x, ByteCodeArray *b) {
    ENTER(writeCexpAmb);
    addByte(b, BYTECODE_AMB);
    int patch = reserveWord(b);
    writeExp(x->exp1, b);
    addByte(b, BYTECODE_JMP);
    int patch2 = reserveWord(b);
    writeCurrentAddressAt(patch, b);
    writeExp(x->exp2, b);
    writeCurrentAddressAt(patch2, b);
    LEAVE(writeCexpAmb);
}

void writeCexpCut(CexpCut *x, ByteCodeArray *b) {
    ENTER(writeCexpCut);
    addByte(b, BYTECODE_CUT);
    writeExp(x->exp, b);
    LEAVE(writeCexpCut);
}

void writeExpLet(ExpLet *x, ByteCodeArray *b) {
    ENTER(writeExpLet);
    addByte(b, BYTECODE_LET);
    int patch = reserveWord(b);
    writeExp(x->val, b);
    addByte(b, BYTECODE_RETURN);
    writeCurrentAddressAt(patch, b);
    writeExp(x->body, b);
    LEAVE(writeExpLet);
}

void writeAexp(Aexp *x, ByteCodeArray *b) {
    ENTER(writeAexp);
    switch (x->type) {
        case AEXP_TYPE_LAM: {
            writeAexpLam(x->val.lam, b);
        }
        break;
        case AEXP_TYPE_VAR: {
            cant_happen("un-annotated var in writeAexp");
        }
        break;
        case AEXP_TYPE_ANNOTATEDVAR: {
            writeAexpAnnotatedVar(x->val.annotatedVar, b);
        }
        break;
        case AEXP_TYPE_TRUE: {
            addByte(b, BYTECODE_TRUE);
        }
        break;
        case AEXP_TYPE_FALSE: {
            addByte(b, BYTECODE_FALSE);
        }
        break;
        case AEXP_TYPE_VOID: {
            addByte(b, BYTECODE_VOID);
        }
        break;
        case AEXP_TYPE_LITTLEINT: {
            addByte(b, BYTECODE_STDINT);
            addInt(b, x->val.littleinteger);
        }
        break;
        case AEXP_TYPE_BIGINT: {
            addByte(b, BYTECODE_BIGINT);
            addBig(b, x->val.biginteger->bi);
        }
        break;
        case AEXP_TYPE_CHAR: {
            addByte(b, BYTECODE_CHAR);
            addByte(b, x->val.character);
        }
        break;
        case AEXP_TYPE_PRIM: {
            writeAexpPrimApp(x->val.prim, b);
        }
        break;
        case AEXP_TYPE_UNARY: {
            writeAexpUnaryApp(x->val.unary, b);
        }
        break;
        case AEXP_TYPE_MAKEVEC: {
            writeAexpMakeVec(x->val.makeVec, b);
        }
        break;
        default:
            cant_happen("unrecognized Aexp type in writeAexp");
    }
    LEAVE(writeAexp);
}

void writeCexp(Cexp *x, ByteCodeArray *b) {
    ENTER(writeCexp);
    switch (x->type) {
        case CEXP_TYPE_APPLY: {
            writeCexpApply(x->val.apply, b);
        }
        break;
        case CEXP_TYPE_IF: {
            writeCexpIf(x->val.iff, b);
        }
        break;
        case CEXP_TYPE_COND: {
            writeCexpCond(x->val.cond, b);
        }
        break;
        case CEXP_TYPE_MATCH: {
            writeCexpMatch(x->val.match, b);
        }
        break;
        case CEXP_TYPE_CALLCC: {
            writeAexp(x->val.callCC, b);
            addByte(b, BYTECODE_CALLCC);
        }
        break;
        case CEXP_TYPE_LETREC: {
            writeCexpLetRec(x->val.letRec, b);
        }
        break;
        case CEXP_TYPE_AMB: {
            writeCexpAmb(x->val.amb, b);
        }
        break;
        case CEXP_TYPE_CUT: {
            writeCexpCut(x->val.cut, b);
        }
        break;
        case CEXP_TYPE_BACK: {
            addByte(b, BYTECODE_BACK);
        }
        break;
        case CEXP_TYPE_ERROR: {
            addByte(b, BYTECODE_ERROR);
        }
        break;
        default:
            cant_happen("unrecognized Cexp type %d in writeCexp", x->type);
    }
    LEAVE(writeCexp);
}

void writeExp(Exp *x, ByteCodeArray *b) {
    ENTER(writeExp);
    switch (x->type) {
        case EXP_TYPE_AEXP: {
            writeAexp(x->val.aexp, b);
        }
        break;
        case EXP_TYPE_CEXP: {
            writeCexp(x->val.cexp, b);
        }
        break;
        case EXP_TYPE_LET: {
            writeExpLet(x->val.let, b);
        }
        break;
        case EXP_TYPE_DONE: {
            addByte(b, BYTECODE_DONE);
        }
        break;
        default:
            cant_happen("unrecognized Exp type in writeExp");
    }
    LEAVE(writeExp);
}

void writeEnd(ByteCodeArray *b) {
    ENTER(writeEnd);
    addByte(b, BYTECODE_RETURN);
    LEAVE(writeEnd);
}
