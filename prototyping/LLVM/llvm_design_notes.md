# LLVM Backend Design Notes

## Overview

This document outlines the design for adding LLVM code generation to CEKF, targeting ANF (A-Normal Form) as the input representation.

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                    CEKF Compiler Pipeline                     │
└──────────────────────────────────────────────────────────────┘
    Parser → AST → Lambda → ANF → Type Check → Annotate
                                        │
                            ┌───────────┴────────────┐
                            ▼                        ▼
                      ┌──────────┐            ┌──────────┐
                      │ Bytecode │            │   LLVM   │
                      │ Compiler │            │ Compiler │
                      └─────┬────┘            └─────┬────┘
                            │                       │
                            ▼                       ▼
                      ┌──────────┐            ┌──────────┐
                      │  step()  │            │LLVM func │
                      │    VM    │            │ pointer  │
                      └─────┬────┘            └─────┬────┘
                            │                       │
                            └──────────┬────────────┘
                                       ▼
                            ┌───────────────────┐
                            │   Runtime Support │
                            │  (step.c, etc.)   │
                            └───────────────────┘
```

## ANF → LLVM Translation

### Basic Mapping

| ANF Construct | LLVM IR |
|---------------|---------|
| `let x = aexp in body` | `%x = call @compile_aexp(...)`<br>`compile_exp(body)` |
| `if aexp then e1 else e2` | `br i1 %cond, label %then, label %else`<br>phi nodes in merge block |
| Variable reference | Load from environment or use SSA value directly |
| Function application | `call @applyProc(...)` |
| Literal integer | `call @value_Stdint(i32 %val)` |

### Value Representation

All CEKF values are represented as LLVM struct type matching the C `Value` union:

```llvm
%struct.Value = type { i32, i64 }
; Field 0: type discriminant (VALUE_TYPE_STDINT, etc.)
; Field 1: union payload (simplified as i64, actual is union)
```

All operations on Values call external C functions from the runtime.

## Runtime Interface

### Functions Exported for LLVM

These functions from `step.c`, `arithmetic.c`, etc. must be callable from LLVM:

#### Value Construction
- `Value value_Stdint(int i)`
- `Value value_Bigint(BigInt *b)`
- `Value value_Character(character c)`
- `Value value_Irrational(double d)`
- `Value value_Clo(Clo *clo)`
- `Value value_Vec(Vec *vec)`

#### Arithmetic Operations
- `Value nadd(Value left, Value right)`
- `Value nsub(Value left, Value right)`
- `Value nmul(Value left, Value right)`
- `Value ndiv(Value left, Value right)`
- `Value npow(Value left, Value right)`
- `Value nmod(Value left, Value right)`

#### Comparison Operations
- `Value eq(Value left, Value right)`
- `Value ne(Value left, Value right)`
- `Value gt(Value left, Value right)`
- `Value lt(Value left, Value right)`
- `Value ge(Value left, Value right)`
- `Value le(Value left, Value right)`
- `Value vcmp(Value left, Value right)`

#### Control Flow
- `bool truthy(Value v)`

#### Stack Operations
- `void push(Value v)`
- `Value pop(void)`
- `Value peek(int index)`
- `void extend(int n)`
- `void popn(int n)`

#### Environment Operations
- `Value lookup(int frame, int offset)`

#### Closure Operations
- `Clo *newClo(int pending, control C, Env *E)`
- `void exactCallFromClo(Clo *clo)`
- `void exactCallFromPclo(Clo *clo, int naargs)`
- `void makePartialFromClo(Value *callable, Clo *clo, int naargs)`
- `void applyProc(int naargs)` - Main application dispatch

#### GC Operations
- `int PROTECT(void *ptr)` - Returns save point
- `void UNPROTECT(int save)` - Restore to save point
- `int protectValue(Value v)` - Protect a Value

#### Vector Operations
- `Vec *newVec(int size)`
- `Value vec(Value index, Value vector)` - Index into vector

## Compilation Strategy

### Phase 1: Direct Translation

Initially, compile ANF almost 1-to-1 to LLVM calls:

```c
// ANF: let x = 5 in x + 3
```

```llvm
define %struct.Value @compute() {
entry:
  ; let x = 5
  %x = call %struct.Value @value_Stdint(i32 5)
  
  ; x + 3
  %three = call %struct.Value @value_Stdint(i32 3)
  %result = call %struct.Value @nadd(%struct.Value %x, %struct.Value %three)
  
  ret %struct.Value %result
}
```

### Phase 2: Optimization Opportunities

Once basic translation works, LLVM can optimize:

1. **Constant folding**: LLVM won't fold `nadd(5, 3)` since it's opaque call, but could with inlining
2. **Dead code elimination**: Unused let-bindings removed
3. **Common subexpression elimination**: Reuse computation results
4. **Inlining**: Inline small runtime functions if marked with LLVM attribute
5. **Register allocation**: Better than interpreter

### Phase 3: Type Specialization (Future)

If static analysis proves a value is always `stdint`, generate specialized code:

```llvm
; Instead of:
%x = call %struct.Value @value_Stdint(i32 5)
%y = call %struct.Value @value_Stdint(i32 3)
%result = call %struct.Value @nadd(%struct.Value %x, %struct.Value %y)

; Generate:
%x = 5
%y = 3
%result_int = add i32 %x, %y
%result = call %struct.Value @value_Stdint(i32 %result_int)
```

This requires flow-sensitive type analysis.

## Handling Complex Features

### Closures (LAM)

ANF closure:
```
let f = lambda(x, y) { x + y } in ...
```

LLVM approach:

1. **Create Clo structure** via runtime call
2. **Generate separate LLVM function** for lambda body
3. **Store function pointer** in Clo (or bytecode offset initially)

```llvm
define %struct.Value @lambda_body_1(i32 %nargs) {
  ; Get args from stack via peek()
  %x = call %struct.Value @peek(i32 0)
  %y = call %struct.Value @peek(i32 1)
  
  ; Body: x + y
  %result = call %struct.Value @nadd(%struct.Value %x, %struct.Value %y)
  ret %struct.Value %result
}

define %struct.Value @compute() {
  ; Create closure
  %env = call %struct.Env* @get_current_env()
  %clo = call %struct.Clo* @newClo(i32 2, i64 ptrtoint(ptr @lambda_body_1 to i64), %struct.Env* %env)
  %f = call %struct.Value @value_Clo(%struct.Clo* %clo)
  ; ... use f
}
```

### Function Application (APPLY)

ANF application:
```
let result = f(a, b) in ...
```

LLVM approach - call the runtime dispatcher:

```llvm
; Push arguments
call void @push(%struct.Value %a)
call void @push(%struct.Value %b)
call void @push(%struct.Value %f)

; Dispatch via runtime (handles CLO/PCLO/BUILTIN/KONT)
call void @applyProc(i32 2)

; Result is on stack
%result = call %struct.Value @pop()
```

The `applyProc` function handles all the complexity:
- Type dispatch on callable
- Partial application
- Over-application
- Built-in functions
- Continuations

### Continuations (LET, CALLCC)

**Challenge**: First-class continuations don't map to LLVM naturally.

**Solution options**:

1. **Keep as runtime calls** (initial approach):
   ```llvm
   ; let x = e1 in e2
   call void @let_begin(ptr @continuation_addr)
   ; compile e1
   ; continuation will restore and run e2
   ```

2. **CPS transformation** (future):
   Transform all code to continuation-passing style before LLVM generation

3. **LLVM coroutines** (experimental):
   Use LLVM's coroutine intrinsics (`llvm.coro.*`)

### Backtracking (AMB, BACK)

**Challenge**: Non-deterministic execution requires saving/restoring state.

**Solution**: Keep as runtime calls initially:

```llvm
; amb e1 e2
call void @setup_amb_fail(ptr @alternative_addr)
; compile e1
; if backtrack triggered, runtime resumes at alternative_addr
```

The Fail register (`state.F`) is managed entirely in C runtime.

## Implementation Files

### New Files to Create

1. **src/llvm_compiler.c**: Main ANF → LLVM compiler
   - `LLVMModuleRef compileAnfToLLVM(Exp *anf)`
   - `void compileExp(LLVMBuilderRef builder, Exp *exp, ...)`
   - `void compileCexp(LLVMBuilderRef builder, Cexp *cexp, ...)`
   - `void compileAexp(LLVMBuilderRef builder, Aexp *aexp, ...)`

2. **src/llvm_compiler.h**: Public interface

3. **src/llvm_runtime.h**: Declarations of runtime functions callable from LLVM
   - Aggregates prototypes from step.c, arithmetic.c, etc.
   - Ensures consistent ABI

4. **src/llvm_runner.c**: Execute LLVM-compiled code
   - `void runLLVM(LLVMModuleRef module, BuiltIns *builtIns)`
   - JIT or AOT compilation

### Modified Files

**Minimal changes to existing code:**

1. **src/main.c**: Add `--use-llvm` flag
   ```c
   if (use_llvm) {
       LLVMModuleRef module = compileAnfToLLVM(anf);
       runLLVM(module, builtIns);
   } else {
       ByteCodeArray B = compileToByteCode(anf);
       run(B, L, builtIns);
   }
   ```

2. **Makefile**: Add LLVM flags
   ```makefile
   LLVM_CONFIG = llvm-config
   LLVM_CFLAGS = $(shell $(LLVM_CONFIG) --cflags)
   LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags --libs core executionengine mcjit native)
   ```

3. **src/step.c**: Export functions (or create wrapper header)
   - Most functions already suitable
   - May need to remove `static` from some helpers

## Testing Strategy

### Unit Tests

1. **Arithmetic**: `(5 + 3) * 2`
2. **Conditionals**: `if x > 5 then x else 0`
3. **Let bindings**: `let x = 1 in let y = 2 in x + y`
4. **Function calls**: `let f = fn(x) {x*2} in f(5)`

### Integration Tests

Run existing `tests/fn/test_*.fn` with both backends, compare results.

### Performance Tests

Benchmark suite comparing bytecode VM vs LLVM:
- Arithmetic-heavy: factorial, fibonacci
- Closure-heavy: higher-order functions
- Backtracking: amb searches

## Build Options

### Debug Build
```bash
make MODE=testing
./bin/fn --use-llvm --dump-llvm-ir program.fn
```

### Release Build
```bash
make MODE=production
./bin/fn --use-llvm program.fn
```

### Both Backends
```bash
# Run bytecode version
./bin/fn program.fn

# Run LLVM version
./bin/fn --use-llvm program.fn

# Compare output
diff <(./bin/fn program.fn) <(./bin/fn --use-llvm program.fn)
```

## Migration Path

### Milestone 1: Proof of Concept (2-3 weeks)
- [ ] Compile simple ANF expressions (literals, arithmetic)
- [ ] Integrate LLVM into build system
- [ ] Execute via LLVM JIT
- [ ] Verify output matches bytecode VM

### Milestone 2: Core Features (4-6 weeks)
- [ ] Closures (LAM)
- [ ] Function application (APPLY)
- [ ] Let bindings
- [ ] Conditionals (IF)
- [ ] Variables (VAR, LVAR)

### Milestone 3: Advanced Features (6-8 weeks)
- [ ] Continuations (LET, RETURN, CALLCC)
- [ ] Backtracking (AMB, BACK, CUT)
- [ ] Pattern matching (MATCH, INTCOND, CHARCOND)
- [ ] Namespaces (NS_*)
- [ ] LETREC

### Milestone 4: Optimization (ongoing)
- [ ] Profile-guided optimization
- [ ] Type specialization
- [ ] Inlining runtime functions
- [ ] Custom calling conventions

### Milestone 5: Production Ready (2-3 months)
- [ ] All tests passing
- [ ] Performance benchmarks
- [ ] Documentation
- [ ] Error handling
- [ ] Debugging support (DWARF metadata)

## Open Questions

1. **Should we generate one LLVM function per F♮ function, or one module per program?**
   - One module easier initially
   - One function per allows better caching

2. **JIT or AOT compilation?**
   - JIT: Easier development, runtime overhead
   - AOT: Better performance, separate compilation step

3. **How to handle recursive functions?**
   - Need forward declarations in LLVM
   - Or generate all functions first, then bodies

4. **What about tail calls?**
   - LLVM has `tail` and `musttail` attributes
   - Could optimize tail-recursive functions

5. **Debug info generation?**
   - LLVM supports DWARF debug info
   - Could map back to .fn source locations

## Resources

- [LLVM C API Documentation](https://llvm.org/doxygen/group__LLVMC.html)
- [Kaleidoscope Tutorial](https://llvm.org/docs/tutorial/) - Great LLVM intro
- [LLVM Language Reference](https://llvm.org/docs/LangRef.html)
- Matt Might's [CESK Machine](https://matt.might.net/articles/cesk-machines/)
- [LLVM Garbage Collection](https://llvm.org/docs/GarbageCollection.html)

## Notes

- Keep bytecode backend during development as reference
- Use LLVM's verifier extensively (`LLVMVerifyModule`)
- Start with LLVM JIT for rapid iteration
- Profile before optimizing - measure actual bottlenecks
- Consider generating readable LLVM IR for debugging (human-readable assembly)
