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

#include "minlam_run_b.h"
#include "builtins_impl.h"
#include "cekfs.h"

typedef struct {
    int code;
    int a1;
    int a2;
    int a3;
} Instruction;

static inline Instruction readInstruction(BLinkedImage *image, Index *IP) {
    Instruction res;
    UIntArray *codes = image->codes;
    UInteger code = getUIntArray(codes, *IP);
    if ((BBC)code == BBC_TYPE_EXT) {
        (*IP)++;
        res.code = getUIntArray(codes, *IP);
        (*IP)++;
        res.a1 = getUIntArray(codes, *IP);
        (*IP)++;
        res.a2 = getUIntArray(codes, *IP);
        (*IP)++;
        res.a3 = getUIntArray(codes, *IP);
    } else {
        res.code = code & 0xFF;
        res.a1 = (code >> 8) & 0xFF;
        res.a2 = (code >> 16) & 0xFF;
        res.a3 = (code >> 24) & 0xFF;
    }
    (*IP)++;
    return res;
}

void brun(BLinkedImage *image, BuiltIns *builtins) {
    Index IP = image->entryPoint;
    Instruction instruction;
    Vec *reg = newVec(image->maxReg);
    int save = PROTECT(reg);

    for (;;) {
        instruction = readInstruction(image, &IP);
        switch (instruction.code) {
        case BBC_TYPE_CALL_BUILTIN: { // builtin_id, dst, argv_reg
            int builtInId = getValue_Stdint(getVec(reg, instruction.a1));
            int dst = getValue_Stdint(getVec(reg, instruction.a2));
            Vec *argv = getValue_Vec(getVec(reg, instruction.a3));
            BuiltIn *builtIn = getBuiltIns(builtins, builtInId);
            BuiltInFunction fn = (BuiltInFunction)builtIn->implementation;
            setVec(reg, dst, fn(argv));
            break;
        }
        case BBC_TYPE_CHARCOND: { // test_reg, table_id
            Character c = getValue_Character(getVec(reg, instruction.a1));
            CharCondSwitch *table =
                getCharCondTable(image->charConds, instruction.a2);
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
            Vec *closure = newVec(2);
            setVec(reg, instruction.a1, value_Vec(closure));
            setVec(closure, 0, getVec(reg, instruction.a2));
            break;
        }
        case BBC_TYPE_CLOSURE_SET_ENV: { // clo_reg, env_reg
            setVec(getValue_Vec(getVec(reg, instruction.a1)), 2,
                   getVec(reg, instruction.a2));
            break;
        }
        case BBC_TYPE_DONE: {     // exit_status
            exit(instruction.a1); // reconsider return instead
        }
        case BBC_TYPE_EXT: {    // unpacked modifier for the next instruction
            cant_happen("EXT"); // readInstruction should have handled this.
        }
        case BBC_TYPE_INTCOND: // test_reg, table_id
        // need to go back and address the problem that maybeBigInt is not
        // meant to be a runtime type (and how on earth is the C runtime
        // working?)
        case BBC_TYPE_JMP_FALSE:   // test_reg, imm target
        case BBC_TYPE_JMP_REG:     // reg target
        case BBC_TYPE_JMP:         // imm target
        case BBC_TYPE_LOAD_ADDR:   // dst, imm32 # 32-bit addr
        case BBC_TYPE_LOAD_CHAR:   // dst, codepoint
        case BBC_TYPE_LOAD_CONST:  // dst, const_index
        case BBC_TYPE_LOAD_I32:    // dst, imm32
        case BBC_TYPE_LOAD_NONE:   // dst
        case BBC_TYPE_MAKE_VEC:    // dst, count
        case BBC_TYPE_MATCH:       // test_reg, table_id
        case BBC_TYPE_MOVE:        // dst, src
        case BBC_TYPE_PRIM_ADD:    // dst, left, right
        case BBC_TYPE_PRIM_CANON:  // dst, left, right
        case BBC_TYPE_PRIM_CMP:    // dst, left, right
        case BBC_TYPE_PRIM_DIV:    // dst, left, right
        case BBC_TYPE_PRIM_EQ:     // dst, left, right
        case BBC_TYPE_PRIM_GCD:    // dst, left, right
        case BBC_TYPE_PRIM_GE:     // dst, left, right
        case BBC_TYPE_PRIM_GT:     // dst, left, right
        case BBC_TYPE_PRIM_LCM:    // dst, left, right
        case BBC_TYPE_PRIM_LE:     // dst, left, right
        case BBC_TYPE_PRIM_LT:     // dst, left, right
        case BBC_TYPE_PRIM_MOD:    // dst, left, right
        case BBC_TYPE_PRIM_MUL:    // dst, left, right
        case BBC_TYPE_PRIM_NE:     // dst, left, right
        case BBC_TYPE_PRIM_POW:    // dst, left, right
        case BBC_TYPE_PRIM_SUB:    // dst, left, right
        case BBC_TYPE_PRIM_VEC:    // dst, index_reg, vec_reg
        case BBC_TYPE_UNPROTECT:   // if a bigint was protected by
                                   // minlam_runtime
        case BBC_TYPE_TAILCALL:    // clo_reg
        case BBC_TYPE_VEC_GET_IMM: // dst, index_imm, vec_reg
        case BBC_TYPE_VEC_SET:     // vec_reg, index_imm, src
            break;
        }
    }
    UNPROTECT(save);
}
