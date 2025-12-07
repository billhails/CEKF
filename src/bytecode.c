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
#include "debug.h"
#include "common.h"
#include "utf8.h"

#ifdef DEBUG_BYTECODE
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif


void resetByteCodeArray(ByteCodeArray *b) {
    b->size = 0;
    for (size_t i = 0; i < b->capacity; i++) {
        b->entries[i] = BYTECODES_TYPE_NONE;
    }
}

static void growCapacity(ByteCodeArray *byteCodes, Control newCapacity) {
    int oldCapacity = byteCodes->capacity;
    Byte *entries =
        GROW_ARRAY(Byte, byteCodes->entries, oldCapacity, newCapacity);

    for (Control i = oldCapacity; i < newCapacity; i++) {
        entries[i] = BYTECODES_TYPE_NONE;
    }

    byteCodes->entries = entries;
    byteCodes->capacity = newCapacity;
}

static void reserve(ByteCodeArray *b, size_t size) {
    while ((b->size + size) >= b->capacity) {
        growCapacity(b, b->capacity < 8 ? 8 : b->capacity * 2);
    }
}

static void writeLocation(ParserInfo I, ByteCodeArray *b, LocationArray *L) {
    Location *loc = newLocation(b->size, I.lineno, I.filename);
    int save = PROTECT(loc);
    pushLocationArray(L, loc);
    UNPROTECT(save);
}

char *charRep(Character c) {
    switch (c) {
        case '\n':
            return "\\n";
        case '\t':
            return "\\t";
        case '\0':
            return "\\0";
        default: {
            static unsigned char buf[8];
            unsigned char *ptr = writeChar(buf, c);
            *ptr = 0;
            return (char *) buf;
        }
    }
}

static void addCharacter(ByteCodeArray *b, Character code) {
    DEBUG("%04lx addCharacter %02x", b->size, code);
    reserve(b, sizeof(Character));
    memcpy(&b->entries[b->size], &code, sizeof(Character));
    b->size += sizeof(Character);
}

static void addByte(ByteCodeArray *b, int code) {
    if (code > 255) {
        cant_happen("maximim byte size exceeded");
    }
    DEBUG("%04lx addByte %02x", b->size, code);
    reserve(b, sizeof(Byte));
    b->entries[b->size++] = code;
}

static void writeWordAt(Control loc, ByteCodeArray *b, Word word) {
    DEBUG("%04x writeWord %04x", loc, word);
    memcpy(&b->entries[loc], &word, sizeof(Word));
}

static void writeIntegerAt(Control loc, ByteCodeArray *b, Integer word) {
    DEBUG("%04x writeInt %d", loc, word);
    memcpy(&b->entries[loc], &word, sizeof(Integer));
}

static void writeDoubleAt(Control loc, ByteCodeArray *b, Double f) {
    DEBUG("%04x writeDouble %f", loc, f);
    memcpy(&b->entries[loc], &f, sizeof(Double));
}

static void writeCurrentAddressAt(int patch, ByteCodeArray *b) {
    Word offset = b->size - patch;
    writeWordAt(patch, b, offset);
}

static void addWord(ByteCodeArray *b, Word w) {
    reserve(b, sizeof(Word));
    writeWordAt(b->size, b, w);
    b->size += sizeof(Word);
}

static Control reserveWord(ByteCodeArray *b) {
    Control address = b->size;
    addWord(b, 0);
    return address;
}

static void addInteger(ByteCodeArray *b, Integer word) {
    reserve(b, sizeof(Integer));
    writeIntegerAt(b->size, b, word);
    b->size += sizeof(Integer);
}

static void addIrrational(ByteCodeArray *b, Double f) {
    reserve(b, sizeof(Double));
    writeDoubleAt(b->size, b, f);
    b->size += sizeof(Double);
}

static int reserveInteger(ByteCodeArray *b) {
    int address = b->size;
    addInteger(b, 0);
    return address;
}

static void addBig(ByteCodeArray *b, bigint bi) {
    addInteger(b, bi.size);
    addInteger(b, bi.capacity);
    addByte(b, bi.neg);
    size_t nBytes = bi.capacity * sizeof(bigint_word);
    reserve(b, nBytes);
    DEBUG("%04lx addBig nBytes %ld", b->size, nBytes);
    memcpy(&b->entries[b->size], &bi.words[0], nBytes);
    b->size += nBytes;
}

////////////////////////////////////////////////////////////////////////////

void writeAexpLam(AexpLam *x, ByteCodeArray *b , LocationArray *L) {
    ENTER(writeAexpLam);
    if (x == NULL)
        return;
    addByte(b, BYTECODES_TYPE_LAM);
    addByte(b, x->nargs);
    addByte(b, x->letRecOffset);
    Control patch = reserveWord(b);
    writeAnfExp(x->exp, b, L);
    addByte(b, BYTECODES_TYPE_RETURN);
    writeCurrentAddressAt(patch, b);
    LEAVE(writeAexpLam);
}

void writeAexpAnnotatedVar(AexpAnnotatedVar *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeAexpAnnotatedVar);
    if (x == NULL)
        return;
    writeLocation(CPI(x), b, L);
    switch (x->type) {
        case AEXPANNOTATEDVARTYPE_TYPE_ENV:
            addByte(b, BYTECODES_TYPE_VAR);
            addByte(b, x->frame);
            addByte(b, x->offset);
            break;
        case AEXPANNOTATEDVARTYPE_TYPE_STACK:
            addByte(b, BYTECODES_TYPE_LVAR);
            addByte(b, x->offset);
            break;
        default:
            cant_happen("unrecognised annotated var type");
    }

    LEAVE(writeAexpAnnotatedVar);
}

void writeAexpPrimApp(AexpPrimApp *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeAexpPrimApp);
    if (x == NULL)
        return;
    writeAexp(x->exp1, b, L);
    writeAexp(x->exp2, b, L);
    writeLocation(CPI(x), b, L);
    Byte prim;
    switch (x->type) {
        case AEXPPRIMOP_TYPE_ADD:
            prim = BYTECODES_TYPE_PRIM_ADD;
            break;
        case AEXPPRIMOP_TYPE_SUB:
            prim = BYTECODES_TYPE_PRIM_SUB;
            break;
        case AEXPPRIMOP_TYPE_MUL:
            prim = BYTECODES_TYPE_PRIM_MUL;
            break;
        case AEXPPRIMOP_TYPE_DIV:
            prim = BYTECODES_TYPE_PRIM_DIV;
            break;
        case AEXPPRIMOP_TYPE_POW:
            prim = BYTECODES_TYPE_PRIM_POW;
            break;
        case AEXPPRIMOP_TYPE_MOD:
            prim = BYTECODES_TYPE_PRIM_MOD;
            break;
        case AEXPPRIMOP_TYPE_EQ:
            prim = BYTECODES_TYPE_PRIM_EQ;
            break;
        case AEXPPRIMOP_TYPE_NE:
            prim = BYTECODES_TYPE_PRIM_NE;
            break;
        case AEXPPRIMOP_TYPE_GT:
            prim = BYTECODES_TYPE_PRIM_GT;
            break;
        case AEXPPRIMOP_TYPE_LT:
            prim = BYTECODES_TYPE_PRIM_LT;
            break;
        case AEXPPRIMOP_TYPE_GE:
            prim = BYTECODES_TYPE_PRIM_GE;
            break;
        case AEXPPRIMOP_TYPE_LE:
            prim = BYTECODES_TYPE_PRIM_LE;
            break;
        case AEXPPRIMOP_TYPE_VEC:
            prim = BYTECODES_TYPE_PRIM_VEC;
            break;
        case AEXPPRIMOP_TYPE_CMP:
            prim = BYTECODES_TYPE_PRIM_CMP;
            break;
        default:
            cant_happen("unrecognised AexpPrimOp in writeAexpPrimApp");
    }
    addByte(b, prim);
    LEAVE(writeAexpPrimApp);
}

void writeAexpList(AexpList *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeAexpList);
    while (x != NULL) {
        writeAexp(x->exp, b, L);
        x = x->next;
    }
    LEAVE(writeAexpList);
}

void writeAexpMakeVec(AexpMakeVec *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeAexpMakeVec);
    writeAexpList(x->args, b, L);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_PRIM_MAKEVEC);
    addByte(b, x->nargs);
    LEAVE(writeAexpMakeVec);
}

void writeAexpNamespaceArray(AexpNamespaceArray *x, ByteCodeArray *b, LocationArray *L) {
    if (x->size > 0) {
        writeLocation(CPI(x->entries[0]->body), b, L);
        addByte(b, BYTECODES_TYPE_NS_START);
        addWord(b, x->size);
        for (Index i = 0; i < x->size; i++) {
            writeAnfExp(x->entries[i]->body, b, L);
            writeLocation(CPI(x->entries[i]->body), b, L);
            addByte(b, BYTECODES_TYPE_NS_END);
            addWord(b, x->entries[i]->nbindings);
            addWord(b, x->size - i);
        }
        writeLocation(CPI(x->entries[x->size - 1]->body), b, L);
        addByte(b, BYTECODES_TYPE_NS_FINISH);
        addWord(b, x->size);
    }
}

void writeAexpNamespaces(AexpNamespaces *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeAexpNamespaces);
    writeAexpNamespaceArray(x->namespaces, b, L);
    writeAnfExp(x->body, b, L);
    LEAVE(writeAexpNamespaces);
}

void writeCexpApply(CexpApply *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpApply);
    // Preserve existing evaluation order: evaluate args left-to-right, then the function
    Index n = countAexpList(x->args);
    // We'll conditionally emit args in a custom order; defer function emission below.

    // Hybrid approach: if the callee is a direct lambda with known arity m
    // and there are extra arguments (n > m), emit an exact APPLY m followed
    // by (n - m) chained APPLY 1 instructions. This avoids VM over-application
    // while keeping the original evaluation order and stack discipline.
    bool directLam = (x->function != NULL && x->function->type == AEXP_TYPE_LAM);
    if (directLam) {
        int m = x->function->val.lam != NULL ? x->function->val.lam->nargs : 0;
        if ((int)n > m && m > 0) {
            // Collect args into an array to control emission order
            AexpList *cur = x->args;
            Aexp **argv = NEW_ARRAY(Aexp*, n);
            Index i = 0;
            while (cur != NULL && i < n) {
                argv[i++] = cur->exp;
                cur = cur->next;
            }
            // Emit extra args (a_{m+1}..a_n) in reverse so that a_{m+1} is nearest to TOS after APPLY m
            for (Index k = n; k > (Index)m; k--) {
                writeAexp(argv[k - 1], b, L);
            }
            // Emit the first m args in order a1..am
            for (Index k = 0; k < (Index)m; k++) {
                writeAexp(argv[k], b, L);
            }
            // Emit function last (callable on TOS)
            writeAexp(x->function, b, L);
            // Apply exactly m args
            writeLocation(CPI(x), b, L);
            addByte(b, BYTECODES_TYPE_APPLY);
            addByte(b, m);
            // Chain the remaining (n - m) arguments one-by-one; they are already on the stack
            for (Index r = 0; r < (Index)((int)n - m); r++) {
                writeLocation(CPI(x), b, L);
                addByte(b, BYTECODES_TYPE_APPLY);
                addByte(b, 1);
            }
            FREE_ARRAY(Aexp*, argv, n);
            LEAVE(writeCexpApply);
            return;
        }
    }

    // Default: single APPLY with all args
    // Evaluate args left-to-right, then function
    writeAexpList(x->args, b, L);
    writeAexp(x->function, b, L);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_APPLY);
    addByte(b, n);
    LEAVE(writeCexpApply);
}

void writeCexpIf(CexpIf *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpIf);
    writeAexp(x->condition, b, L);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_IF);
    Control patch = reserveWord(b);
    writeAnfExp(x->consequent, b, L);
    writeLocation(CPI(x->consequent), b, L);
    addByte(b, BYTECODES_TYPE_JMP);
    Control patch2 = reserveWord(b);
    writeCurrentAddressAt(patch, b);
    writeAnfExp(x->alternative, b, L);
    writeCurrentAddressAt(patch2, b);
    LEAVE(writeCexpIf);
}

void writeCexpCharCondCases(int depth, Control *values, Control *addresses,
                            Control *jumps, CexpCharCondCases *x,
                            ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpCharCondCases);
    if (x == NULL) {
        return;
    }
    writeCexpCharCondCases(depth + 1, values, addresses, jumps, x->next, b, L);
    if (x->next == NULL) {      // default
        writeAnfExp(x->body, b, L);
    } else {
        writeIntegerAt(values[depth], b, x->option);
        writeCurrentAddressAt(addresses[depth], b);
        writeAnfExp(x->body, b, L);
    }
    if (depth > 0) {
        writeLocation(CPI(x->body), b, L);
        addByte(b, BYTECODES_TYPE_JMP);
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
void writeCexpCharCond(CexpCharCondCases *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpCharCond);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_CHARCOND);
    Index numCases = countCexpCharCondCases(x);
    if (numCases <= 1) {
        cant_happen("zero cases in writeCexpCharCond");
    }
    numCases--;                 // don't count the default case
    addWord(b, numCases);
    Control *values = NEW_ARRAY(Control, numCases);     // address in b for each index_i
    Control *addresses = NEW_ARRAY(Control, numCases);  // address in b for each addr(exp_i)
    Control *jumps = NEW_ARRAY(Control, numCases);      // address in b for the JMP patch address at the end of each expression
    for (Index i = 0; i < numCases; i++) {
        values[i] = reserveInteger(b);      // TODO can change this to a char later, but then again, wchar_t...
        addresses[i] = reserveWord(b);
    }
    writeCexpCharCondCases(0, values, addresses, jumps, x, b, L);
    for (Index i = 0; i < numCases; i++) {
        writeCurrentAddressAt(jumps[i], b);
    }
    FREE_ARRAY(Control, values, numCases);
    FREE_ARRAY(Control, addresses, numCases);
    FREE_ARRAY(Control, jumps, numCases);
    LEAVE(writeCexpCharCond);
}

void writeCexpIntCondCases(CexpIntCondCases *x, ByteCodeArray *b,
                           Control *endJumps, Control *dispatches, int index, LocationArray *L) {
    ENTER(writeCexpIntCondCases);
    if (x == NULL)
        return;
    writeCexpIntCondCases(x->next, b, endJumps, dispatches, index + 1, L);
    if (x->next != NULL) {      // last case is default, first one written, no dispatch as it follows the jmp table
        writeCurrentAddressAt(dispatches[index + 1], b);
    }
    writeAnfExp(x->body, b, L);
    if (index != -1) {          // -1 is first case. last one written out, doesn't need a JMP to end as the end immediately follows
        writeLocation(CPI(x), b, L);
        addByte(b, BYTECODES_TYPE_JMP);
        endJumps[index] = reserveWord(b);
    }
    LEAVE(writeCexpIntCondCases);
}

void writeCexpIntCond(CexpIntCondCases *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpIntCond);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_INTCOND);
    Index numCases = countCexpIntCondCases(x);
    // eprintf("writeCexpIntCond size %d\n", numCases);
    // printCexpIntCondCases(x);
    if (numCases <= 1) {
        cant_happen("zero cases in writeCexpIntCond");
    }
    numCases--;                 // don't count the default case
    addWord(b, numCases);
    // we start out by writing each of the cases, reserving a slot in memory for each dispatch address after each (variable length) value.
    Control *dispatches = NEW_ARRAY(Control, numCases); // address of the slots for each dispatch address
    {
        int i = 0;
        for (CexpIntCondCases *xx = x; xx != NULL; xx = xx->next) {
            if (xx->next == NULL)
                break;          // default case doesn't get a test
            writeLocation(CPI(xx), b, L);
            switch (xx->option->type) {
                case BI_SMALL:
                    addByte(b, xx->option->imag ? BYTECODES_TYPE_STDINT_IMAG : BYTECODES_TYPE_STDINT);
                    addInteger(b, xx->option->small);
                    break;
                case BI_BIG:
                    addByte(b, xx->option->imag ? BYTECODES_TYPE_BIGINT_IMAG : BYTECODES_TYPE_BIGINT);
                    addBig(b, xx->option->big);
                    break;
                case BI_IRRATIONAL:
                    addByte(b, xx->option->imag ? BYTECODES_TYPE_IRRATIONAL_IMAG : BYTECODES_TYPE_IRRATIONAL);
                    addIrrational(b, xx->option->irrational);
                    break;
                default:
                    cant_happen("unsupported MaybeBigIntType %d", xx->option->type);
            }
            dispatches[i++] = reserveWord(b);
        }
    }
    // next we right-recurse on the expressions (so the default directly follows the dispatch table)
    Control *endJumps = NEW_ARRAY(Control, numCases);   // address in b for the JMP patch address at the end of each expression which jumps to the end
    writeCexpIntCondCases(x, b, endJumps, dispatches, -1, L);
    // lastly we patch the escape addresses of the clauses.
    for (Index i = 0; i < numCases; i++) {
        writeCurrentAddressAt(endJumps[i], b);
    }
    FREE_ARRAY(Control, dispatches, numCases);
    FREE_ARRAY(Control, endJumps, numCases);
    LEAVE(writeCexpIntCond);
}

void writeCexpCond(CexpCond *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpCond);
    writeAexp(x->condition, b, L);
    switch (x->cases->type) {
        case CEXPCONDCASES_TYPE_INTCASES:
            writeCexpIntCond(x->cases->val.intCases, b, L);
            break;
        case CEXPCONDCASES_TYPE_CHARCASES:
            writeCexpCharCond(x->cases->val.charCases, b, L);
            break;
        default:
            cant_happen("unrecognised type %d in writeCexpCond",
                        x->cases->type);
    }
    LEAVE(writeCexpCond);
}

void writeCexpLetRec(CexpLetRec *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpLetRec);
    writeLetRecBindings(x->bindings, b, L);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_LETREC);
    addByte(b, x->nbindings);
    writeAnfExp(x->body, b, L);
    LEAVE(writeCexpLetRec);
}

static int validateCexpMatch(CexpMatch *x) {
    bool seen[256];
    for (int i = 0; i < 256; ++i) {
        seen[i] = false;
    }
    for (AnfMatchList *m = x->clauses; m != NULL; m = m->next) {
        for (AexpIntList *matches = m->matches; matches != NULL;
             matches = matches->next) {
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
                cant_happen
                    ("non-contiguous match indices in validateCexpMatch");
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

void writeCexpMatch(CexpMatch *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpMatch);
    int count = validateCexpMatch(x);
    writeAexp(x->condition, b, L);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_MATCH);
    // create a dispatch table
    addByte(b, count);
    Control patches[256];
    Control jumps[256];
    for (int i = 0; i < count; ++i) {
        patches[i] = reserveWord(b);
    }
    int jumpCounter = 0;
    for (AnfMatchList *m = x->clauses; m != NULL; m = m->next) {
        for (AexpIntList *l = m->matches; l != NULL; l = l->next) {
            int i = l->integer;
            writeCurrentAddressAt(patches[i], b);
        }
        writeAnfExp(m->body, b, L);
        writeLocation(CPI(m->body), b, L);
        addByte(b, BYTECODES_TYPE_JMP);
        jumps[jumpCounter++] = reserveWord(b);
    }
    for (int i = 0; i < jumpCounter; i++)
        writeCurrentAddressAt(jumps[i], b);
    LEAVE(writeCexpMatch);
}

void writeLetRecBindings(AnfLetRecBindings *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeLetRecBindings);
    while (x != NULL) {
        writeAexp(x->val, b, L);
        x = x->next;
    }
    LEAVE(writeLetRecBindings);
}

void writeCexpAmb(CexpAmb *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpAmb);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_AMB);
    Control patch = reserveWord(b);
    writeLocation(CPI(x->exp1), b, L);
    writeAnfExp(x->exp1, b, L);
    addByte(b, BYTECODES_TYPE_JMP);
    Control patch2 = reserveWord(b);
    writeCurrentAddressAt(patch, b);
    writeAnfExp(x->exp2, b, L);
    writeCurrentAddressAt(patch2, b);
    LEAVE(writeCexpAmb);
}

void writeCexpCut(CexpCut *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexpCut);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_CUT);
    writeAnfExp(x->exp, b, L);
    LEAVE(writeCexpCut);
}

void writeAnfExpLet(AnfExpLet *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeAnfExpLet);
    writeLocation(CPI(x), b, L);
    addByte(b, BYTECODES_TYPE_LET);
    Control patch = reserveWord(b);
    writeAnfExp(x->val, b, L);
    writeLocation(CPI(x->val), b, L);
    addByte(b, BYTECODES_TYPE_RETURN);
    writeCurrentAddressAt(patch, b);
    writeAnfExp(x->body, b, L);
    LEAVE(writeAnfExpLet);
}

void writeLookup(AnfExpLookup *x, ByteCodeArray *b, LocationArray *L) {
#ifdef SAFETY_CHECKS
    if (x->annotatedVar == NULL) {
        cant_happen("annotated var missing from lookup");
    }
#endif
    writeLocation(CPI(x), b, L);
    switch(x->annotatedVar->type) {
        case AEXPANNOTATEDVARTYPE_TYPE_STACK:
            addByte(b, BYTECODES_TYPE_NS_PUSHSTACK);
            addWord(b, x->annotatedVar->offset);
            break;
        case AEXPANNOTATEDVARTYPE_TYPE_ENV:
            addByte(b, BYTECODES_TYPE_NS_PUSHENV);
            addWord(b, x->annotatedVar->frame);
            addWord(b, x->annotatedVar->offset);
            break;
        default:
            cant_happen("unrecognised annotation type %d", x->annotatedVar->type);
    }
    writeAnfExp(x->body, b, L);
    writeLocation(CPI(x->body), b, L);
    addByte(b, BYTECODES_TYPE_NS_POP);
}

void writeAexp(Aexp *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeAexp);
    writeLocation(CPI(x), b, L);

    switch (x->type) {
        case AEXP_TYPE_LAM:{
                writeAexpLam(x->val.lam, b, L);
            }
            break;
        case AEXP_TYPE_VAR:{
                cant_happen("un-annotated var in writeAexp");
            }
            break;
        case AEXP_TYPE_ANNOTATEDVAR:{
                writeAexpAnnotatedVar(x->val.annotatedVar, b, L);
            }
            break;
        case AEXP_TYPE_LITTLEINTEGER:{
                addByte(b, BYTECODES_TYPE_STDINT);
                addInteger(b, x->val.littleinteger);
            }
            break;
        case AEXP_TYPE_BIGINTEGER:{
                switch (x->val.biginteger->type) {
                    case BI_SMALL:
                        addByte(b, x->val.biginteger->imag ? BYTECODES_TYPE_STDINT_IMAG : BYTECODES_TYPE_STDINT);
                        addInteger(b, x->val.biginteger->small);
                        break;
                    case BI_BIG:
                        addByte(b, x->val.biginteger->imag ? BYTECODES_TYPE_BIGINT_IMAG : BYTECODES_TYPE_BIGINT);
                        addBig(b, x->val.biginteger->big);
                        break;
                    case BI_IRRATIONAL:
                        addByte(b, x->val.biginteger->imag ? BYTECODES_TYPE_IRRATIONAL_IMAG : BYTECODES_TYPE_IRRATIONAL);
                        addIrrational(b, x->val.biginteger->irrational);
                        break;
                    default:
                        cant_happen("unsupported MaybeBigInt type %d", x->val.biginteger->type);
                }
            }
            break;
        case AEXP_TYPE_CHARACTER:{
                addByte(b, BYTECODES_TYPE_CHAR);
                addCharacter(b, x->val.character);
            }
            break;
        case AEXP_TYPE_PRIM:{
                writeAexpPrimApp(x->val.prim, b, L);
            }
            break;
        case AEXP_TYPE_MAKEVEC:{
                writeAexpMakeVec(x->val.makeVec, b, L);
            }
            break;
        case AEXP_TYPE_NAMESPACES:{
                writeAexpNamespaces(x->val.namespaces, b, L);
            }
            break;
        default:
            cant_happen("unrecognized Aexp type %s", aexpTypeName(x->type));
    }
    LEAVE(writeAexp);
}

void writeCexp(Cexp *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeCexp);
    switch (x->type) {
        case CEXP_TYPE_APPLY:{
                writeCexpApply(x->val.apply, b, L);
            }
            break;
        case CEXP_TYPE_IFF:{
                writeCexpIf(x->val.iff, b, L);
            }
            break;
        case CEXP_TYPE_COND:{
                writeCexpCond(x->val.cond, b, L);
            }
            break;
        case CEXP_TYPE_MATCH:{
                writeCexpMatch(x->val.match, b, L);
            }
            break;
        case CEXP_TYPE_CALLCC:{
                writeAexp(x->val.callCC, b, L);
                writeLocation(CPI(x), b, L);
                addByte(b, BYTECODES_TYPE_CALLCC);
            }
            break;
        case CEXP_TYPE_LETREC:{
                writeCexpLetRec(x->val.letRec, b, L);
            }
            break;
        case CEXP_TYPE_AMB:{
                writeCexpAmb(x->val.amb, b, L);
            }
            break;
        case CEXP_TYPE_CUT:{
                writeCexpCut(x->val.cut, b, L);
            }
            break;
        case CEXP_TYPE_BACK:{
                writeLocation(CPI(x), b, L);
                addByte(b, BYTECODES_TYPE_BACK);
            }
            break;
        case CEXP_TYPE_ERROR:{
                writeLocation(CPI(x), b, L);
                addByte(b, BYTECODES_TYPE_ERROR);
            }
            break;
        default:
            cant_happen("unrecognized Cexp type %s", cexpTypeName(x->type));
    }
    LEAVE(writeCexp);
}

void writeAnfExp(AnfExp *x, ByteCodeArray *b, LocationArray *L) {
    ENTER(writeAnfExp);
    switch (x->type) {
        case ANFEXP_TYPE_AEXP:{
                writeAexp(x->val.aexp, b, L);
            }
            break;
        case ANFEXP_TYPE_CEXP:{
                writeCexp(x->val.cexp, b, L);
            }
            break;
        case ANFEXP_TYPE_LET:{
                writeAnfExpLet(x->val.let, b, L);
            }
            break;
        case ANFEXP_TYPE_DONE:{
                writeLocation(CPI(x), b, L);
                addByte(b, BYTECODES_TYPE_DONE);
            }
            break;
        case ANFEXP_TYPE_LOOKUP:{
                writeLookup(x->val.lookup, b, L);
            }
            break;
        case ANFEXP_TYPE_ENV:
            break;
        default:
            cant_happen("unrecognized Exp type %s", anfExpTypeName(x->type));
    }
    LEAVE(writeAnfExp);
}

void writeEnd(ByteCodeArray *b) {
    ENTER(writeEnd);
    addByte(b, BYTECODES_TYPE_RETURN);
    LEAVE(writeEnd);
}

enum ReadByteCodeStatus readBinaryInputFile(ByteCodeArray *b, char *filename) {
    FILE *fh = fopen(filename, "r");
    if (fh == NULL) {
        return BYTECODES_BADFILE; // errno will be set
    }
    if (fgetc(fh) != 'F') {
        return BYTECODES_BADHEADER;
    }
    if (fgetc(fh) != 'N') {
        return BYTECODES_BADHEADER;
    }
    int vh = fgetc(fh);
    if (vh == EOF) {
        return BYTECODES_BADHEADER;
    }
    int vl = fgetc(fh);
    if (vl == EOF) {
        return BYTECODES_BADHEADER;
    }
    if ((vl | (vh << 8)) != CEKF_BYTECODE_VERSION) {
        fprintf(stderr, "bad version %d.%d\n", vh, vl);
        return BYTECODES_BADVERSION;
    }
    int c;
    while ((c = fgetc(fh)) != EOF) {
        pushByteCodeArray(b, c);
    }
    return BYTECODES_OK;
}

bool writeBinaryOutputFile(ByteCodeArray *b, char *filename) {
    FILE *fh = fopen(filename, "w");
    if (fh == NULL) {
        return false; // errno will be set
    }
    fputc('F', fh);
    fputc('N', fh);
    fputc((CEKF_BYTECODE_VERSION & 0xFF00) >> 8, fh);
    fputc((CEKF_BYTECODE_VERSION & 0xFF), fh);
    for (Index i = 0; i < b->size; i++) {
        fputc((int) b->entries[i], fh);
    }
    fclose(fh);
    return true;
}
