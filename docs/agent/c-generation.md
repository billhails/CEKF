# C Code Generation Backend

Alternative backend that compiles F-natural source to standalone C executables
instead of bytecode. Activated with the `--target-c` flag.

## Pipeline Overview

The C backend shares the front end with the bytecode path (scanning, parsing,
lambda conversion, type checking, desugaring) but diverges after desugaring.
Where the bytecode path continues to ANF and bytecode compilation, the C path
applies its own transforms and emits C directly.

```text
MinExp (from desugaring)
  ↓ runCpsTrampolineTc()   — CPS transform
  ↓ betaMinExp()            — beta reduction
  ↓ ambMinExp()             — amb (backtracking) transform
  ↓ betaMinExp()            — beta reduction (2nd pass)
  ↓ etaMinExp()             — eta reduction
  ↓ betaMinExp()            — beta reduction (3rd pass)
  ↓ flatClosureConvert()    — closure conversion (or sharedClosureConvert)
  ↓ indexMinExp()           — lexical addressing (De Bruijn indices)
  ↓ emitProgram()           — C code generation
  ↓
Standalone C source file
```

Pipeline code is in `src/main.c` (search for `targetCFlag`).

## Command-Line Flags

| Flag | Purpose |
|---|---|
| `--target-c` | Enable C code generation (output to stdout) |
| `--flat-closures` | Use flat closure representation (default: shared) |
| `--dump-ir` | Print final MinExp IR before emission |
| `--dump-cps` | Print IR after CPS transform |
| `--dump-amb` | Print IR after amb transform |
| `--dump-closure` | Print IR after closure conversion |

## Transform Stages

### CPS Transform (`src/minlam_cpsTc.c`, `src/minlam_cpsTk.c`)

Converts to continuation-passing style. Every function gains an explicit
continuation parameter; every call passes an explicit "what to do next."

- `cpsM()` — adds continuation parameter `k` to lambdas.
- `cpsTc()` — transforms expressions, threading continuation `c`.
- Uses a **trampoline** (`src/minlam_cpsTrampoline.c`) to convert deep
  recursion into an iterative work loop, preventing stack overflow on
  deeply nested expressions.

### Beta Reduction (`src/minlam_beta.c`)

Applies lambda abstractions to arguments at compile time when both the
lambda and argument are atomic expressions (literals, variables, lambdas).

### Eta Reduction (`src/minlam_eta.c`)

Simplifies `λx. f(x)` to `f` when `x` does not appear free in `f`.

### Amb Transform (`src/minlam_amb.c`)

Converts `amb` expressions into explicit failure-driven choice points
with dual-path control flow. Adds a failure continuation parameter `f`
to all functions.

### Closure Conversion (`src/minlam_closureConvert.c`)

Makes closures explicit by extracting free variables into environment
vectors.

Two strategies:

- **Flat closures** (`flatClosureConvert` / `bottomUpMinExp`): Each lambda
  captures all its free variables directly. Faster access (single vector
  index) but larger environments.
- **Shared closures** (`sharedClosureConvert` / `topDownMinExp`): Nested
  lambdas share parent environment vectors. Smaller environments but
  deeper chained lookups.

After conversion, a closure is a 2-element vector:
`(make-vec <lambda> <env>)` where `<env>` is a vector of captured values.
Free variable references become `vec(Stdint(N), env)` index lookups.

### Lexical Indexing (`src/minlam_annotate.c`)

Replaces variable names with integer positions (De Bruijn indices). After
this pass, variables map directly to `reg[N]` in the generated C code.

## Generated C Code Structure

The emitter (`src/minlam_emit.c`, function `emitProgram`) produces a
single self-contained C file:

```c
#include "minlam_runtime.h"

#define MAX_REG 146
static Value reg[MAX_REG];

int main(int argc, char *argv[]) {
    // Initialize all registers to value_None()
    // Initialize runtime
    goto ENTRY;

  LAMBDA_foo:
    // ... lambda body ...

  LAMBDA_bar:
    // ... lambda body ...

  ENTRY:
    // Create closures, backpatch environments, start execution
}
```

### Register Allocation

All values live in a flat `reg[]` array. The emitter tracks slot usage via
a `SlotPool` and `currentReg` counter. `MAX_REG` is the peak usage.

### Closure Representation

Closures are 2-element vectors:

```c
// entries[0] = code address,  entries[1] = environment
reg[42] = make_vec(2, value_Addr(&&LAMBDA_foo), value_None());
```

### Letrec Two-Phase Construction

Recursive bindings require two phases to allow cyclic references:

1. **Create skeleton**: `make_vec(2, value_Addr(&&LABEL), value_None())`
2. **Backpatch environment**: `getValue_Vec(reg[N])->entries[1] = envVec;`

This is implemented by `emitMakeClosure` and `emitBackpatchClosure`.

### Function Calls

Calls use computed gotos:

```c
// Extract code address and environment from closure
reg[4] = vec(value_Stdint(0), closure);   // code addr
reg[5] = vec(value_Stdint(1), closure);   // env

// Set up arguments: env, args..., continuation, fail-continuation
reg[0] = reg[5];        // env
reg[1] = arg;           // argument
reg[2] = continuation;  // k
reg[3] = fail;          // f

goto *getValue_Addr(reg[4]);
```

### Done Continuations

The top-level program receives success and failure continuations that
call `exit(0)` (emitted as `MINEXP_TYPE_DONE`).

## Runtime (`src/minlam_runtime.h`, `src/minlam_runtime.c`)

The generated C links against the runtime, which provides:

- **Value**: Tagged union (stdint, bigint, character, vec, none, etc.)
- `make_vec(count, ...)` — allocate and initialize a vector
- `vec(index, array)` — index into vector: `entries[getValue_Stdint(index)]`
- `getValue_Addr()`, `getValue_Vec()`, `getValue_Stdint()` — type-checked extractors
- `eq()`, `ne()`, `lt()`, `gt()`, `le()`, `ge()` — comparison operators
- `minlam_runtime_mark_reg()` — GC integration, marks all register values as live

## Building and Testing

```bash
# Most targets in junk/ can be built directly by giving them as a make argument:
make junk/test_foo        # Build binary (generates C, compiles, links)
make junk/test_foo.c      # Generate C only
make junk/test_foo.scm    # Dump final IR

# Run all binary tests
make test-binary

# Or invoke the compiler directly:
bin/fn --target-c --flat-closures --include=fn tests/fn/test_foo.fn > junk/test_foo.c
```

The Makefile variable `TARGET_CG` controls the default flags for test
binary generation: `--include=fn --target-c --flat-closures`.

## Key Source Files

| File | Role |
|---|---|
| `src/main.c` | Pipeline orchestration |
| `src/minlam_cpsTc.c` | CPS Tc transform |
| `src/minlam_cpsTk.c` | CPS Tk transform |
| `src/minlam_cpsTrampoline.c` | Trampoline driver loop |
| `src/minlam_beta.c` | Beta reduction |
| `src/minlam_eta.c` | Eta reduction |
| `src/minlam_amb.c` | Amb transform |
| `src/minlam_closureConvert.c` | Closure conversion (flat/shared) |
| `src/minlam_annotate.c` | Lexical indexing |
| `src/minlam_emit.c` | C code emitter |
| `src/minlam_runtime.c` | Runtime support |
| `src/minlam_runtime.h` | Runtime API |
| `src/minlam.yaml` | MinExp type definitions |

## Debugging

- Use `--dump-ir` to inspect the final IR before emission.
- Use `--dump-cps`, `--dump-amb`, `--dump-closure` to inspect intermediate stages.
- The generated C file has comments indicating source locations and
  which emitter function produced each block (e.g., `// emitApplyClosure +141 preamble`).
- Generated lambda labels include the original function name
  (e.g., `LAMBDA___error___3600`).
- Build with `TRACE_GOTO` defined to log every computed goto at runtime.
