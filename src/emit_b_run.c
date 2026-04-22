/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "emit_b_run.h"
#include "builtins_impl.h"
#include "cekfs.h"
#include "minlam_runtime.h"

#ifdef TRACE_BRUN
#define EPRINTF(...) eprintf(__VA_ARGS__)
#else
#define EPRINTF(...) ;
#endif

typedef struct {
    int code;
    int a1;
    int a2;
    int a3;
} Instruction;

static inline UInteger bReadWord(BLinkedImage *image, Index *IPP) {
    return getUIntArray(image->codes, (*IPP)++);
}

static inline Instruction readInstruction(BLinkedImage *image, Index *IPP) {
    Instruction res;
    UInteger code = bReadWord(image, IPP);
    if ((BBC)code == BBC_TYPE_EXT) {
        res.code = bReadWord(image, IPP);
        res.a1 = bReadWord(image, IPP);
        res.a2 = bReadWord(image, IPP);
        res.a3 = bReadWord(image, IPP);
    } else {
        res.code = code & 0xFF;
        res.a1 = (code >> 8) & 0xFF;
        res.a2 = (code >> 16) & 0xFF;
        res.a3 = (code >> 24) & 0xFF;
    }
    return res;
}

void brun(BLinkedImage *image, BuiltIns *builtins) {
    Index IP = image->entryPoint;
    Instruction inst;
    Vec *regVec = newVec(image->maxReg);
    Value *reg = regVec->entries;
    int save = PROTECT(regVec);

    for (;;) {
        EPRINTF("%04x ", IP);
        inst = readInstruction(image, &IP);
        switch (inst.code) {
        case BBC_TYPE_CALL_BUILTIN: { // builtin_id, dst, argv_reg
            EPRINTF("CALL_BUILTIN id=%u dst=reg[%d] arg=reg[%d]\n", inst.a1,
                    inst.a2, inst.a3);
            int builtInId = inst.a1;
            int dst = inst.a2;
            Vec *argv = getValue_Vec(reg[inst.a3]);
            BuiltIn *builtIn = getBuiltIns(builtins, builtInId);
            BuiltInFunction fn = (BuiltInFunction)builtIn->implementation;
            reg[dst] = fn(argv);
            break;
        }
        case BBC_TYPE_CHARCOND: { // test_reg, table_id
            Index t = bReadWord(image, &IP);
            EPRINTF("CHARCOND test=reg[%d] table=%u\n", inst.a1, t);
            Character c = getValue_Character(reg[inst.a1]);
            CharCondSwitch *table = getCharCondTable(image->charConds, t);
            // consider later sorting and binary search, or a tree
            for (Index i = 0; i < countCharCondCaseArray(table->cases); i++) {
                CharCondCase *candidate = getCharCondCaseArray(table->cases, i);
                if (c == candidate->codepoint) {
                    IP = candidate->target;
                    goto END_CHARCOND;
                }
            }
            IP = table->default_target;
        END_CHARCOND:
            break;
        }
        case BBC_TYPE_CLOSURE_NEW: { // dst, lambda_id
            Index lambda = bReadWord(image, &IP);
            EPRINTF("CLOSURE_NEW dst=reg[%d] lambda=%04x\n", inst.a1, lambda);
            Vec *closure = newVec(2);
            reg[inst.a1] = value_Vec(closure);
            setVec(closure, 0, value_Index(lambda));
            break;
        }
        case BBC_TYPE_CLOSURE_SET_ENV: { // clo_reg, env_reg
            EPRINTF("CLOSURE_SET_ENV clo=reg[%d] env=reg[%d]\n", inst.a1,
                    inst.a2);
            setVec(getValue_Vec(reg[inst.a1]), 1, reg[inst.a2]);
            break;
        }
        case BBC_TYPE_DONE: { // exit_status
            EPRINTF("DONE exit=%d\n", inst.a1);
            exit(inst.a1); // reconsider return instead
        }
        case BBC_TYPE_EXT: {    // unpacked modifier for the next instruction
            cant_happen("EXT"); // readInstruction should have handled this.
        }
        case BBC_TYPE_INTCOND: { // test_reg, table_id
            Index t = bReadWord(image, &IP);
            EPRINTF("INTCOND test=reg[%d] table=%u\n", inst.a1, t);
            Value v = reg[inst.a1];
            IntCondSwitch *table = getIntCondTable(image->intConds, t);
            for (Index i = 0; i < countIntCondCaseArray(table->cases); i++) {
                IntCondCase *candidate = getIntCondCaseArray(table->cases, i);
                Value w =
                    getBConstantArray(image->constants, candidate->const_index);
                if (minlam_runtime_cmp(w, v) == CMP_EQ) {
                    IP = candidate->target;
                    goto END_INTCOND;
                }
            }
            IP = table->default_target;
        END_INTCOND:
            break;
        }
        case BBC_TYPE_JMP_FALSE: { // test_reg, imm_target
            Index target = bReadWord(image, &IP);
            EPRINTF("JMP_FALSE test=reg[%d] target=%04x\n", inst.a1, target);
            if (!isTrue(reg[inst.a1])) {
                IP = target;
            }
            break;
        }
        case BBC_TYPE_JMP_REG: { // reg_target DUPLICATE of TAILCALL
            EPRINTF("JMP_REG target=reg[%d]\n", inst.a1);
            IP = getValue_Index(reg[inst.a1]);
            break;
        }
        case BBC_TYPE_JMP: { // imm_target
            IP = bReadWord(image, &IP);
            EPRINTF("JMP target=%04x\n", IP);
            break;
        }
        case BBC_TYPE_LOAD_ADDR: { // dst, imm32 # 32-bit addr
            Index addr = bReadWord(image, &IP);
            EPRINTF("LOAD_ADDR dst=reg[%d] addr=%04x\n", inst.a1, addr);
            reg[inst.a1] = value_Index(addr);
            break;
        }
        case BBC_TYPE_LOAD_CHAR: { // dst, codepoint
            Index c = bReadWord(image, &IP);
            EPRINTF("LOAD_CHAR target=reg[%d] codepoint=%u\n", inst.a1, c);
            reg[inst.a1] = value_Character(c);
            break;
        }
        case BBC_TYPE_LOAD_CONST: { // dst, const_index
            EPRINTF("LOAD_CONST target=reg[%d] id=%d\n", inst.a1, inst.a2);
            reg[inst.a1] = getBConstantArray(image->constants, inst.a2);
            break;
        }
        case BBC_TYPE_LOAD_I32: { // dst, imm32
            int i32 = bReadWord(image, &IP);
            EPRINTF("LOAD_I32 dst=reg[%d] i32=%d\n", inst.a1, i32);
            reg[inst.a1] = value_Stdint(i32);
            break;
        }
        case BBC_TYPE_LOAD_NONE: { // dst
            EPRINTF("LOAD_NONE\n");
            reg[inst.a1] = value_None();
            break;
        }
        case BBC_TYPE_MAKE_VEC: { // dst, count
            EPRINTF("MAKE_VEC dst=reg[%d] count=%d\n", inst.a1, inst.a2);
            reg[inst.a1] = value_Vec(newVec((int)inst.a2));
            break;
        }
        case BBC_TYPE_MATCH: { // test_reg, table_id
            Index t = bReadWord(image, &IP);
            EPRINTF("MATCH test=reg[%d] table=%d\n", inst.a1, t);
            int i = getValue_Stdint(reg[inst.a1]);
            IndexArray *table = getMatchTable(image->matches, t);
            IP = getIndexArray(table, i);
            break;
        }
        case BBC_TYPE_MOVE: // dst, src
            EPRINTF("MOVE dst=reg[%d] src=reg[%d]\n", inst.a1, inst.a2);
            reg[inst.a1] = reg[inst.a2];
            break;
        case BBC_TYPE_PRIM_ADD: { // dst, left, right
            EPRINTF("ADD dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = nadd(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_CANON: { // dst, left, right
            EPRINTF("CANON dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1,
                    inst.a2, inst.a3);
            reg[inst.a1] = ncanon(reg[inst.a2]);
            break;
        }
        case BBC_TYPE_PRIM_CMP: { // dst, left, right
            EPRINTF("CMP dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = cmp(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_DIV: { // dst, left, right
            EPRINTF("DIV dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = ndiv(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_EQ: { // dst, left, right
            EPRINTF("EQ dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = eq(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_GCD: { // dst, left, right
            EPRINTF("GCD dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = ngcd(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_GE: { // dst, left, right
            EPRINTF("GE dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = ge(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_GT: { // dst, left, right
            EPRINTF("LE dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = gt(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_LCM: { // dst, left, right
            EPRINTF("LCM dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = nlcm(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_LE: { // dst, left, right
            EPRINTF("LE dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = le(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_LT: { // dst, left, right
            EPRINTF("LT dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = lt(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_MOD: { // dst, left, right
            EPRINTF("MOD dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = nmod(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_MUL: { // dst, left, right
            EPRINTF("MUL dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = nmul(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_NE: { // dst, left, right
            EPRINTF("NE dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = ne(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_POW: { // dst, left, right
            EPRINTF("POW dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = npow(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_SUB: { // dst, left, right
            EPRINTF("SUB dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = nsub(reg[inst.a2], reg[inst.a3]);
            break;
        }
        case BBC_TYPE_PRIM_VEC: { // dst, index_reg, vec_reg
            EPRINTF("VEC dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            reg[inst.a1] = getVec(getValue_Vec(reg[inst.a3]),
                                  getValue_Stdint(reg[inst.a2]));
            break;
        }
        case BBC_TYPE_UNPROTECT: // if a bigint was protected by
                                 // minlam_runtime
            EPRINTF("UNPROTECT\n");
            break;
        case BBC_TYPE_TAILCALL: // clo_reg
            EPRINTF("TAILCALL clo=reg[%d]\n", inst.a1);
            IP = getValue_Index(reg[inst.a1]);
            break;
        case BBC_TYPE_VEC_GET_IMM: { // dst, index_imm, vec_reg
            EPRINTF("VEC_GET_IMM dst=reg[%d] index=%d vec=reg[%d]\n", inst.a1,
                    inst.a2, inst.a3);
            reg[inst.a1] = getVec(getValue_Vec(reg[inst.a3]), inst.a2);
            break;
        }
        case BBC_TYPE_VEC_SET: // vec_reg, index_imm, src_reg
            EPRINTF("VEC_SET vec=reg[%d] index=%d src=reg[%d]\n", inst.a1,
                    inst.a2, inst.a3);
            setVec(getValue_Vec(reg[inst.a1]), inst.a2, reg[inst.a3]);
            break;
        }
    }
    UNPROTECT(save);
}
