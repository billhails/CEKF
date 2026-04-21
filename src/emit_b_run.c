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
    Vec *reg = newVec(image->maxReg);
    int save = PROTECT(reg);

    for (;;) {
        eprintf("%08x ", IP);
        inst = readInstruction(image, &IP);
        switch (inst.code) {
        case BBC_TYPE_CALL_BUILTIN: { // builtin_id, dst, argv_reg
            eprintf("CALL_BUILTIN id=%u dst=reg[%d] arg=reg[%d]\n", inst.a1,
                    inst.a2, inst.a3);
            int builtInId = getValue_Stdint(getVec(reg, inst.a1));
            int dst = getValue_Stdint(getVec(reg, inst.a2));
            Vec *argv = getValue_Vec(getVec(reg, inst.a3));
            BuiltIn *builtIn = getBuiltIns(builtins, builtInId);
            BuiltInFunction fn = (BuiltInFunction)builtIn->implementation;
            setVec(reg, dst, fn(argv));
            break;
        }
        case BBC_TYPE_CHARCOND: { // test_reg, table_id
            Index t = bReadWord(image, &IP);
            eprintf("CHARCOND test=reg[%d] table=%u\n", inst.a1, t);
            Character c = getValue_Character(getVec(reg, inst.a1));
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
            eprintf("CLOSURE_NEW dst=reg[%d] lambda=%08x\n", inst.a1, lambda);
            Vec *closure = newVec(2);
            setVec(reg, inst.a1, value_Vec(closure));
            setVec(closure, 0, value_Index(lambda));
            break;
        }
        case BBC_TYPE_CLOSURE_SET_ENV: { // clo_reg, env_reg
            eprintf("CLOSURE_SET_ENV clo=reg[%d] env=reg[%d]\n", inst.a1,
                    inst.a2);
            setVec(getValue_Vec(getVec(reg, inst.a1)), 1, getVec(reg, inst.a2));
            break;
        }
        case BBC_TYPE_DONE: { // exit_status
            eprintf("DONE exit=%d\n", inst.a1);
            exit(inst.a1); // reconsider return instead
        }
        case BBC_TYPE_EXT: {    // unpacked modifier for the next instruction
            cant_happen("EXT"); // readInstruction should have handled this.
        }
        case BBC_TYPE_INTCOND: { // test_reg, table_id
            Index t = bReadWord(image, &IP);
            eprintf("INTCOND test=reg[%d] table=%u\n", inst.a1, t);
            Value v = getVec(reg, inst.a1);
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
            eprintf("JMP_FALSE test=reg[%d] target=%d\n", inst.a1, target);
            if (!isTrue(getVec(reg, inst.a1))) {
                IP = target;
            }
            break;
        }
        case BBC_TYPE_JMP_REG: { // reg_target DUPLICATE of TAILCALL
            eprintf("JMP_REG target=reg[%d]\n", inst.a1);
            IP = getValue_Index(getVec(reg, inst.a1));
            break;
        }
        case BBC_TYPE_JMP: { // imm_target
            IP = bReadWord(image, &IP);
            eprintf("JMP target=%08x\n", IP);
            break;
        }
        case BBC_TYPE_LOAD_ADDR: { // dst, imm32 # 32-bit addr
            Index addr = bReadWord(image, &IP);
            eprintf("LOAD_ADDR dst=reg[%d] addr=%08x\n", inst.a1, addr);
            setVec(reg, inst.a1, value_Index(addr));
            break;
        }
        case BBC_TYPE_LOAD_CHAR: { // dst, codepoint
            Index c = bReadWord(image, &IP);
            eprintf("LOAD_CHAR target=reg[%d] codepoint=%u\n", inst.a1, c);
            setVec(reg, inst.a1, value_Character(c));
            break;
        }
        case BBC_TYPE_LOAD_CONST: { // dst, const_index
            eprintf("LOAD_CONST target=reg[%d] id=%d\n", inst.a1, inst.a2);
            setVec(reg, inst.a1, getBConstantArray(image->constants, inst.a2));
            break;
        }
        case BBC_TYPE_LOAD_I32: { // dst, imm32 DUPLICATE OF LOAD_ADDR
            Index addr = bReadWord(image, &IP);
            eprintf("LOAD_I32 target=reg[%d] addr=%08x\n", inst.a1, addr);
            setVec(reg, inst.a1, value_Index(addr));
            break;
        }
        case BBC_TYPE_LOAD_NONE: { // dst
            eprintf("LOAD_NONE\n");
            setVec(reg, inst.a1, value_None());
            break;
        }
        case BBC_TYPE_MAKE_VEC: { // dst, count
            eprintf("MAKE_VEC dst=reg[%d] count=%d\n", inst.a1, inst.a2);
            setVec(reg, inst.a1, value_Vec(newVec((int)inst.a2)));
            break;
        }
        case BBC_TYPE_MATCH: { // test_reg, table_id
            Index t = bReadWord(image, &IP);
            eprintf("MATCH test=reg[%d] table=%d\n", inst.a1, t);
            Index i = getValue_Index(getVec(reg, inst.a1));
            IndexArray *table = getMatchTable(image->matches, t);
            IP = getIndexArray(table, i);
            break;
        }
        case BBC_TYPE_MOVE: // dst, src
            eprintf("MOVE dst=reg[%d] src=reg[%d]\n", inst.a1, inst.a2);
            setVec(reg, inst.a1, getVec(reg, inst.a2));
            break;
        case BBC_TYPE_PRIM_ADD: // dst, left, right
            eprintf("ADD dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   nadd(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_CANON: // dst, left, right
            eprintf("CANON dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1,
                    inst.a2, inst.a3);
            setVec(reg, inst.a1, ncanon(getVec(reg, inst.a2)));
            break;
        case BBC_TYPE_PRIM_CMP: // dst, left, right
            eprintf("CMP dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   cmp(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_DIV: // dst, left, right
            eprintf("DIV dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   ndiv(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_EQ: // dst, left, right
            eprintf("EQ dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   eq(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_GCD: // dst, left, right
            eprintf("GCD dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   ngcd(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_GE: // dst, left, right
            eprintf("GE dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   ge(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_GT: // dst, left, right
            eprintf("LE dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   gt(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_LCM: // dst, left, right
            eprintf("LCM dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   nlcm(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_LE: // dst, left, right
            eprintf("LE dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   le(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_LT: // dst, left, right
            eprintf("LT dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   lt(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_MOD: // dst, left, right
            eprintf("MOD dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   nmod(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_MUL: // dst, left, right
            eprintf("MUL dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   nmul(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_NE: // dst, left, right
            eprintf("NE dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   ne(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_POW: // dst, left, right
            eprintf("POW dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   npow(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_SUB: // dst, left, right
            eprintf("SUB dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   nsub(getVec(reg, inst.a2), getVec(reg, inst.a3)));
            break;
        case BBC_TYPE_PRIM_VEC: // dst, index_reg, vec_reg
            eprintf("VEC dst=[%d] lhs=reg[%d] rhs=reg[%d]\n", inst.a1, inst.a2,
                    inst.a3);
            setVec(reg, inst.a1,
                   getVec(getValue_Vec(getVec(reg, inst.a3)),
                          getValue_Index(getVec(reg, inst.a2))));
            break;
        case BBC_TYPE_UNPROTECT: // if a bigint was protected by
                                 // minlam_runtime
            eprintf("UNPROTECT\n");
            break;
        case BBC_TYPE_TAILCALL: // clo_reg
            eprintf("TAILCALL clo=reg[%d]\n", inst.a1);
            IP = getValue_Index(getVec(reg, inst.a1));
            break;
        case BBC_TYPE_VEC_GET_IMM: // dst, index_imm, vec_reg
            eprintf("VEC_GET_IMM dst=reg[%d] index=%d vec=reg[%d]\n", inst.a1,
                    inst.a2, inst.a3);
            setVec(reg, inst.a1,
                   getVec(getValue_Vec(getVec(reg, inst.a3)), inst.a2));
            break;
        case BBC_TYPE_VEC_SET: // vec_reg, index_imm, src_reg
            eprintf("VEC_SET vec=reg[%d] index=%d src=reg[%d]\n", inst.a1,
                    inst.a2, inst.a3);
            setVec(getValue_Vec(getVec(reg, inst.a1)), inst.a2,
                   getVec(reg, inst.a3));
            break;
        }
    }
    UNPROTECT(save);
}
