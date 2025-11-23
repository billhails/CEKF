/*
 * CEKF - LLVM Prototype
 * Copyright (C) 2025  Bill Hails
 *
 * Proof-of-concept: Compiling ANF to LLVM IR
 * 
 * This prototype demonstrates how to translate A-Normal Form expressions
 * to LLVM IR while calling existing CEKF runtime functions.
 *
 * Build with:
 *   clang -I../src -I../generated -I/usr/include/llvm-c-14 \
 *         -c llvm_prototype.c -o llvm_prototype.o
 *   clang llvm_prototype.o -lLLVM-14 -o llvm_prototype
 *
 * Note: Adjust LLVM version (14) to match your system.
 */

#include <stdio.h>
#include <stdlib.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>

/*
 * This prototype shows compilation of simple ANF expressions like:
 *
 * ANF: let x = 5 in let y = x + 3 in y * 2
 *
 * Which translates to calls to the CEKF runtime:
 *   Value x = value_Stdint(5);
 *   Value y = nadd(x, value_Stdint(3));
 *   Value result = nmul(y, value_Stdint(2));
 *   return result;
 */

// Forward declarations of CEKF runtime functions we'll reference
// In real implementation, these come from step.c, arithmetic.c, etc.
typedef struct {
    int type;
    union {
        int stdint;
        void *ptr;
    } val;
} Value;

// Mock runtime functions for demonstration
Value value_Stdint(int i) {
    Value v;
    v.type = 0; // VALUE_TYPE_STDINT
    v.val.stdint = i;
    return v;
}

Value nadd(Value left, Value right) {
    Value result;
    result.type = 0;
    result.val.stdint = left.val.stdint + right.val.stdint;
    return result;
}

Value nmul(Value left, Value right) {
    Value result;
    result.type = 0;
    result.val.stdint = left.val.stdint * right.val.stdint;
    return result;
}

/*
 * Compile ANF expression: let x = 5 in let y = x + 3 in y * 2
 * 
 * This function demonstrates:
 * 1. Creating LLVM module and function
 * 2. Declaring external C runtime functions
 * 3. Emitting LLVM IR that calls those functions
 * 4. Proper value passing between LLVM and C
 */
LLVMModuleRef compile_simple_anf_expression(void) {
    // Initialize LLVM
    LLVMModuleRef module = LLVMModuleCreateWithName("cekf_prototype");
    LLVMBuilderRef builder = LLVMCreateBuilder();
    
    // Define the Value type in LLVM (opaque struct for now)
    // In real implementation, would match cekfs.yaml definition
    LLVMTypeRef value_type = LLVMStructCreateNamed(
        LLVMGetGlobalContext(), 
        "struct.Value"
    );
    LLVMTypeRef value_fields[2] = {
        LLVMInt32Type(),           // type discriminant
        LLVMInt64Type()            // union payload (simplified)
    };
    LLVMStructSetBody(value_type, value_fields, 2, 0);
    
    // Declare external runtime functions
    
    // Value value_Stdint(int i)
    LLVMTypeRef value_stdint_param_types[] = { LLVMInt32Type() };
    LLVMTypeRef value_stdint_type = LLVMFunctionType(
        value_type,                  // return type
        value_stdint_param_types,    // params
        1,                           // param count
        0                            // not vararg
    );
    LLVMValueRef value_stdint_fn = LLVMAddFunction(
        module, 
        "value_Stdint", 
        value_stdint_type
    );
    
    // Value nadd(Value left, Value right)
    LLVMTypeRef nadd_param_types[] = { value_type, value_type };
    LLVMTypeRef nadd_type = LLVMFunctionType(
        value_type,
        nadd_param_types,
        2,
        0
    );
    LLVMValueRef nadd_fn = LLVMAddFunction(module, "nadd", nadd_type);
    
    // Value nmul(Value left, Value right)
    LLVMTypeRef nmul_param_types[] = { value_type, value_type };
    LLVMTypeRef nmul_type = LLVMFunctionType(
        value_type,
        nmul_param_types,
        2,
        0
    );
    LLVMValueRef nmul_fn = LLVMAddFunction(module, "nmul", nmul_type);
    
    // Create the compiled function: Value compute(void)
    LLVMTypeRef compute_type = LLVMFunctionType(
        value_type,    // returns Value
        NULL,          // no params
        0,
        0
    );
    LLVMValueRef compute_fn = LLVMAddFunction(
        module, 
        "compute", 
        compute_type
    );
    
    // Create entry basic block
    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(compute_fn, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);
    
    // Emit LLVM IR for: let x = 5
    // %x = call Value @value_Stdint(i32 5)
    LLVMValueRef five = LLVMConstInt(LLVMInt32Type(), 5, 0);
    LLVMValueRef args_x[] = { five };
    LLVMValueRef x = LLVMBuildCall2(
        builder,
        value_stdint_type,
        value_stdint_fn,
        args_x,
        1,
        "x"
    );
    
    // Emit LLVM IR for: let y = x + 3
    // %three = call Value @value_Stdint(i32 3)
    // %y = call Value @nadd(Value %x, Value %three)
    LLVMValueRef three = LLVMConstInt(LLVMInt32Type(), 3, 0);
    LLVMValueRef args_three[] = { three };
    LLVMValueRef three_val = LLVMBuildCall2(
        builder,
        value_stdint_type,
        value_stdint_fn,
        args_three,
        1,
        "three_val"
    );
    
    LLVMValueRef args_add[] = { x, three_val };
    LLVMValueRef y = LLVMBuildCall2(
        builder,
        nadd_type,
        nadd_fn,
        args_add,
        2,
        "y"
    );
    
    // Emit LLVM IR for: y * 2
    // %two = call Value @value_Stdint(i32 2)
    // %result = call Value @nmul(Value %y, Value %two)
    LLVMValueRef two = LLVMConstInt(LLVMInt32Type(), 2, 0);
    LLVMValueRef args_two[] = { two };
    LLVMValueRef two_val = LLVMBuildCall2(
        builder,
        value_stdint_type,
        value_stdint_fn,
        args_two,
        1,
        "two_val"
    );
    
    LLVMValueRef args_mul[] = { y, two_val };
    LLVMValueRef result = LLVMBuildCall2(
        builder,
        nmul_type,
        nmul_fn,
        args_mul,
        2,
        "result"
    );
    
    // Return result
    LLVMBuildRet(builder, result);
    
    // Cleanup builder
    LLVMDisposeBuilder(builder);
    
    // Verify module
    char *error = NULL;
    LLVMVerifyModule(module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    
    return module;
}

/*
 * Demonstration of compiling a conditional:
 * 
 * ANF: let x = 10 in if (x > 5) then x * 2 else x + 1
 *
 * Shows:
 * 1. Control flow (if/then/else)
 * 2. Phi nodes for SSA form
 * 3. Comparison operations
 */
LLVMModuleRef compile_conditional_anf(void) {
    LLVMModuleRef module = LLVMModuleCreateWithName("cekf_conditional");
    LLVMBuilderRef builder = LLVMCreateBuilder();
    
    // Value type (simplified)
    LLVMTypeRef value_type = LLVMStructCreateNamed(
        LLVMGetGlobalContext(),
        "struct.Value"
    );
    LLVMTypeRef value_fields[2] = {
        LLVMInt32Type(),
        LLVMInt64Type()
    };
    LLVMStructSetBody(value_type, value_fields, 2, 0);
    
    // Declare runtime functions
    LLVMTypeRef value_stdint_type = LLVMFunctionType(
        value_type,
        (LLVMTypeRef[]){ LLVMInt32Type() },
        1, 0
    );
    LLVMValueRef value_stdint_fn = LLVMAddFunction(
        module, "value_Stdint", value_stdint_type
    );
    
    // bool truthy(Value v) - returns i1
    LLVMTypeRef truthy_type = LLVMFunctionType(
        LLVMInt1Type(),
        (LLVMTypeRef[]){ value_type },
        1, 0
    );
    LLVMValueRef truthy_fn = LLVMAddFunction(module, "truthy", truthy_type);
    
    // Value gt(Value left, Value right)
    LLVMTypeRef gt_type = LLVMFunctionType(
        value_type,
        (LLVMTypeRef[]){ value_type, value_type },
        2, 0
    );
    LLVMValueRef gt_fn = LLVMAddFunction(module, "gt", gt_type);
    
    // Value nmul(Value, Value)
    LLVMTypeRef nmul_type = LLVMFunctionType(
        value_type,
        (LLVMTypeRef[]){ value_type, value_type },
        2, 0
    );
    LLVMValueRef nmul_fn = LLVMAddFunction(module, "nmul", nmul_type);
    
    // Value nadd(Value, Value)
    LLVMTypeRef nadd_type = LLVMFunctionType(
        value_type,
        (LLVMTypeRef[]){ value_type, value_type },
        2, 0
    );
    LLVMValueRef nadd_fn = LLVMAddFunction(module, "nadd", nadd_type);
    
    // Create compute function
    LLVMTypeRef compute_type = LLVMFunctionType(value_type, NULL, 0, 0);
    LLVMValueRef compute_fn = LLVMAddFunction(module, "compute_conditional", compute_type);
    
    // Create basic blocks
    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(compute_fn, "entry");
    LLVMBasicBlockRef then_block = LLVMAppendBasicBlock(compute_fn, "then");
    LLVMBasicBlockRef else_block = LLVMAppendBasicBlock(compute_fn, "else");
    LLVMBasicBlockRef merge = LLVMAppendBasicBlock(compute_fn, "merge");
    
    // Entry block: let x = 10
    LLVMPositionBuilderAtEnd(builder, entry);
    LLVMValueRef ten = LLVMConstInt(LLVMInt32Type(), 10, 0);
    LLVMValueRef x = LLVMBuildCall2(
        builder, value_stdint_type, value_stdint_fn,
        (LLVMValueRef[]){ ten }, 1, "x"
    );
    
    // Comparison: x > 5
    LLVMValueRef five = LLVMConstInt(LLVMInt32Type(), 5, 0);
    LLVMValueRef five_val = LLVMBuildCall2(
        builder, value_stdint_type, value_stdint_fn,
        (LLVMValueRef[]){ five }, 1, "five_val"
    );
    
    LLVMValueRef cmp_result = LLVMBuildCall2(
        builder, gt_type, gt_fn,
        (LLVMValueRef[]){ x, five_val }, 2, "cmp"
    );
    
    LLVMValueRef is_true = LLVMBuildCall2(
        builder, truthy_type, truthy_fn,
        (LLVMValueRef[]){ cmp_result }, 1, "is_true"
    );
    
    LLVMBuildCondBr(builder, is_true, then_block, else_block);
    
    // Then block: x * 2
    LLVMPositionBuilderAtEnd(builder, then_block);
    LLVMValueRef two = LLVMConstInt(LLVMInt32Type(), 2, 0);
    LLVMValueRef two_val = LLVMBuildCall2(
        builder, value_stdint_type, value_stdint_fn,
        (LLVMValueRef[]){ two }, 1, "two_val"
    );
    LLVMValueRef then_result = LLVMBuildCall2(
        builder, nmul_type, nmul_fn,
        (LLVMValueRef[]){ x, two_val }, 2, "then_result"
    );
    LLVMBuildBr(builder, merge);
    
    // Else block: x + 1
    LLVMPositionBuilderAtEnd(builder, else_block);
    LLVMValueRef one = LLVMConstInt(LLVMInt32Type(), 1, 0);
    LLVMValueRef one_val = LLVMBuildCall2(
        builder, value_stdint_type, value_stdint_fn,
        (LLVMValueRef[]){ one }, 1, "one_val"
    );
    LLVMValueRef else_result = LLVMBuildCall2(
        builder, nadd_type, nadd_fn,
        (LLVMValueRef[]){ x, one_val }, 2, "else_result"
    );
    LLVMBuildBr(builder, merge);
    
    // Merge block: phi node
    LLVMPositionBuilderAtEnd(builder, merge);
    LLVMValueRef phi = LLVMBuildPhi(builder, value_type, "result");
    LLVMValueRef phi_values[] = { then_result, else_result };
    LLVMBasicBlockRef phi_blocks[] = { then_block, else_block };
    LLVMAddIncoming(phi, phi_values, phi_blocks, 2);
    
    LLVMBuildRet(builder, phi);
    
    LLVMDisposeBuilder(builder);
    
    char *error = NULL;
    LLVMVerifyModule(module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    
    return module;
}

int main(void) {
    printf("CEKF LLVM Prototype\n");
    printf("===================\n\n");
    
    // Initialize LLVM
    LLVMLinkInMCJIT();
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
    
    // Example 1: Simple arithmetic
    printf("Example 1: Compiling ANF expression (5 + 3) * 2\n");
    printf("-----------------------------------------------\n");
    LLVMModuleRef module1 = compile_simple_anf_expression();
    
    // Dump LLVM IR to see what we generated
    char *ir = LLVMPrintModuleToString(module1);
    printf("%s\n", ir);
    LLVMDisposeMessage(ir);
    
    // We could execute this with LLVM JIT, but for prototype we just show IR
    LLVMDisposeModule(module1);
    
    // Example 2: Conditional
    printf("\nExample 2: Compiling conditional expression\n");
    printf("--------------------------------------------\n");
    LLVMModuleRef module2 = compile_conditional_anf();
    
    ir = LLVMPrintModuleToString(module2);
    printf("%s\n", ir);
    LLVMDisposeMessage(ir);
    
    LLVMDisposeModule(module2);
    
    printf("\nPrototype complete. Key observations:\n");
    printf("1. ANF maps naturally to LLVM SSA form\n");
    printf("2. All runtime calls are external functions\n");
    printf("3. Control flow uses LLVM's native br/phi\n");
    printf("4. Value passing works through struct-by-value\n");
    printf("\nNext steps:\n");
    printf("- Handle closures (LAM bytecode)\n");
    printf("- Implement APPLY with dispatch\n");
    printf("- Add GC PROTECT/UNPROTECT calls\n");
    printf("- Support continuations (challenging)\n");
    printf("- Implement backtracking (AMB/BACK)\n");
    
    return 0;
}
