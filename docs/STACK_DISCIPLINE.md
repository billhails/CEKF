# Stack Discipline and Execution Model

## Overview

The CEKF virtual machine implements a CEK machine (Control, Environment, Kontinuation) extended with **F** for failure continuation (supporting `amb` non-deterministic programming). This document describes how the stack, closures, continuations, and function application work together.

## Core Data Structures

### CEKF State Machine (`src/cekfs.yaml`, `src/step.c`)

```c
struct CEKF {
    Control C;        // Program counter (bytecode offset)
    Env *E;           // Environment chain (lexically captured variables)
    Kont *K;          // Continuation chain (where to return to)
    Fail *F;          // Failure continuation chain (backtracking points)
    Stack *S;         // Value stack (arguments, temporaries, locals)
    ByteCodeArray B;  // Bytecode to execute
    LocationArray *L; // Source locations for error reporting
};
```

The machine state is global (`static CEKF state` in `step.c`) and modified in place during execution.

### Stack Structure

The **Stack** is a multi-frame structure that grows upward. It's implemented as a single contiguous array with frame markers:

```c
struct Stack {
    Header header;           // GC header
    Value *entries;          // Flat array of all values
    Index entries_capacity;  // Total allocated capacity
    Index frame;             // Base offset of current frame
    Index offset;            // Number of values in current frame
    StackFrame *frames;      // Array of saved frames
    Index frames_index;      // Number of saved frames
    Index frames_capacity;   // Capacity of frames array
};

struct StackFrame {
    Index frame;   // Saved base offset
    Index offset;  // Saved frame size
};
```

**Key insight**: The stack is frame-oriented, not globally flat. Each frame is an independent slice:
- `entries[frame .. frame+offset-1]` = current frame's values
- Previous frames are saved in the `frames[]` array when entering nested scopes

### Environment Structure

The **Env** captures lexical bindings for closures:

```c
struct Env {
    Header header;
    Frame *S;      // Values captured by this scope
    Env *E;        // Parent environment (outer scope)
};

struct Frame {
    Header header;
    Value *entries;     // Captured values
    Index size;         // Number of captured values
    Index capacity;     // Allocated capacity
};
```

Environments form a **linked list** representing the lexical scope chain. Variables are looked up using de Bruijn indices: `(frame, offset)` where:
- `frame` = how many Env links to traverse up
- `offset` = index within that Env's Frame

### Value Types

The `Value` discriminated union represents all runtime values:

```c
struct Value {
    ValueType type;  // Discriminator
    union {
        int stdint;
        BigInt *bigint;
        Character character;
        Clo *clo;               // Direct closure (CLO)
        Clo *pclo;              // Partial closure (PCLO) - same struct, different type tag
        Kont *kont;             // Continuation
        Vec *vec;               // Vectors/tuples/constructors
        BuiltInImplementation *builtIn;
        Vec *namespace;
        // ... more numeric types, rationals, complex, etc.
    } val;
};
```

Key value types for execution:
- `VALUE_TYPE_CLO` - Direct closure (never been applied)
- `VALUE_TYPE_PCLO` - Partial closure (has some args captured)
- `VALUE_TYPE_KONT` - Continuation (reified control flow)
- `VALUE_TYPE_VEC` - Multi-value structure (tuples, constructors)
- `VALUE_TYPE_STDINT`, `VALUE_TYPE_CHARACTER`, etc. - Immediates

### Closures

```c
struct Clo {
    Header header;
    int pending;    // Number of arguments still needed
    Control C;      // Bytecode address of function body
    Env *E;         // Captured environment
};
```

**Two types of closures**:

1. **CLO** (`VALUE_TYPE_CLO`): A "fresh" closure that's never been partially applied
   - Created by the `LAM` bytecode
   - `pending` = total arity of the function
   - `E` = environment at definition site

2. **PCLO** (`VALUE_TYPE_PCLO`): A partially applied closure
   - Created when a closure is applied with fewer args than it needs
   - `pending` = remaining arity (original arity - captured args)
   - `E->S` contains the previously captured arguments
   - Same `Clo` struct as CLO, just different Value type tag

**Critical invariant**: PCLO's environment chain has an extra Frame at the head containing captured arguments.

### Continuations

```c
struct Kont {
    Header header;
    Control C;      // Bytecode address to resume at
    Env *E;         // Environment to restore
    Stack *S;       // Stack snapshot (or NULL for lightweight kontinuations)
    Kont *K;        // Next continuation in chain
};
```

Continuations represent "where to return to" after completing a sub-expression. They come in two flavors:

1. **Lightweight Kontinuation** (`S == NULL`):
   - Created by `LET` bytecode for simple let-bindings
   - Only saves C, E, K - assumes stack can be popped instead of fully restored
   - Optimization: avoids copying entire stack for common case

2. **Full Kontinuation** (`S != NULL`):
   - Created by `CALLCC` (call-with-current-continuation)
   - Saves complete stack snapshot for later restoration
   - Enables full continuation capture (can jump anywhere)

### Failure Continuations

```c
struct Fail {
    Header header;
    Control C;      // Bytecode address of alternative
    Env *E;         // Environment at choice point
    Kont *K;        // Continuation at choice point
    Stack *S;       // Stack snapshot at choice point
    Fail *F;        // Previous failure continuation
};
```

Failure continuations support non-deterministic programming via `amb` (spelled `then` in the language):
- `AMB` bytecode creates a choice point
- `BACK` restores the most recent choice point
- Stack, environment, and continuation are all restored on backtrack

## Stack Operations

### Basic Stack Primitives (inline functions in `step.c`)

```c
static inline void push(Value v)       { pushStackEntry(state.S, v); }
static inline Value pop()              { return popStackEntry(state.S); }
static inline Value peek(int index)   { return peeknStack(state.S, index); }
static inline void poke(int offset, Value v) { pokeStack(state.S, offset, v); }
static inline void extend(int n)      { pushnStack(state.S, n, vVoid); }
static inline void discard(int n)     { popnStack(state.S, n); }
static inline void popn(int n)        { popnStack(state.S, n); }
```

**Indexing semantics**:
- `peek(0)` = **bottom** of current frame (first element: `entries[frame + 0]`)
- `peek(1)` = second element from bottom (`entries[frame + 1]`)
- `peek(n)` where n ≥ 0 = element at position n from frame base
- `peek(-1)` = **top** of current frame (last element: `entries[frame + offset - 1]`)
- `peek(-2)` = second from top (`entries[frame + offset - 2]`)
- `peek(n)` where n < 0 = element at position `offset + n` (n < 0) from frame base

**Implementation** (from `generated/cekfs.c`):
```c
struct Value peeknStack(struct Stack *x, int offset) {
    if (offset < 0) offset = ((int) x->offset) + offset;
    return x->entries[x->frame + offset];
}
```

### Stack Frame Management

Stack frames are **not** automatically created on function calls. Instead, frames are explicitly managed:

1. **`letStackFrame(Stack *s)`** - Called by `LET` bytecode
   - Saves current `(frame, offset)` to the `frames[]` array
   - Resets offset to 0 for new frame
   - Does NOT allocate new memory - just bookkeeping

2. **`popStackFrame(Stack *s)`** - Called when restoring lightweight kontinuation
   - Restores previous `(frame, offset)` from `frames[]` array
   - Undoes `letStackFrame()`

3. **`copyAllStackEntries(Stack *dest, Stack *src)`** - Full stack copy
   - Used for continuation capture and failure continuation restoration
   - Copies ALL frames and current frame
   - Expensive operation - avoided when possible

**Key insight**: Most function calls do NOT create new stack frames. Stack frames are only for `let` bindings and continuation capture. Function arguments live in the **current** frame.

### Stack During Function Application

When `APPLY [n]` executes, the stack looks like:

```
TOS → | callable |
      | arg_n    |
      | arg_{n-1}|
      | ...      |
      | arg_1    |
      | ...      | (earlier values)
```

The `applyProc(int naargs)` function:
1. Pops the callable
2. Checks its type (CLO, PCLO, KONT, BUILTIN)
3. Dispatches appropriately
4. Arguments remain on stack for the callee

## Closure Application in Detail

### Exact Application (CLO)

When `clo->pending == naargs` (exact match):

```c
static inline void exactCallFromClo(Clo *clo) {
    state.C = clo->C;           // Jump to function body
    state.E = clo->E;           // Restore captured environment
    moveStack(state.S, 0, clo->pending);  // Move args to base of frame
    apply_exact_calls++;
}
```

**`moveStack(Stack *s, int base, int n)`** (defined in generated code):
- Moves top `n` values to position `base` within current frame
- Used to "slide" arguments down to frame base
- Pattern: `s->entries[s->frame + base .. base+n-1] = s->entries[s->frame + s->offset - n .. s->offset-1]`

After this:
- Control points to function body bytecode
- Environment is the closure's captured environment
- Stack frame base has the arguments in order
- Function body can use `LVAR [i]` to access arg `i`

### Exact Application (PCLO)

When `pclo->pending == naargs` for a partial closure:

```c
static inline void exactCallFromPclo(Clo *clo, int naargs) {
    int ncaptured = clo->E->S->size;  // Previously captured args
    
    // Move new args right, leaving space for captured args
    moveStack(state.S, ncaptured, naargs);
    
    // Copy captured args to base of frame
    copyValues(&state.S->entries[state.S->frame], 
               clo->E->S->entries, 
               ncaptured);
    
    // Set stack pointer to include all args
    state.S->offset = ncaptured + naargs;
    
    // Jump to function with full environment
    state.E = clo->E->E;  // Parent of PCLO's env (which holds captured args)
    state.C = clo->C;
    apply_exact_calls++;
}
```

**Key difference from CLO**: PCLO has captured arguments in `clo->E->S`, which must be spliced into the frame **before** the new arguments.

Result stack layout:
```
| captured_arg_1  |  offset 0
| captured_arg_2  |  offset 1
| ...             |
| new_arg_1       |  offset ncaptured
| new_arg_2       |  offset ncaptured+1
| ...             |
```

### Partial Application (Creating PCLO from CLO)

When `naargs < clo->pending`:

```c
static inline void makePartialFromClo(Value *callable, Clo *clo, int naargs) {
    // Create new environment as child of closure's environment
    Env *env = makeEnv(clo->E);
    int save = PROTECT(env);
    
    // Copy arguments from stack into environment's frame
    copyTosToEnv(env, state.S, naargs);
    
    // Create partial closure with reduced arity
    Clo *pclo = newClo(clo->pending - naargs, clo->C, env);
    PROTECT(pclo);
    
    // Replace arguments and callable with the PCLO
    callable->type = VALUE_TYPE_PCLO;
    callable->val.clo = pclo;
    popn(naargs);      // Remove arguments from stack
    push(*callable);   // Push PCLO as result
    
    UNPROTECT(save);
    apply_partial_creations++;
}
```

**Result**: A new PCLO where:
- `pending` = original pending - naargs
- `E` = new Env containing captured args, with parent = original closure's env
- `C` = same bytecode address (same function)

### Partial Application (Creating PCLO from PCLO)

When applying a PCLO with `naargs < pclo->pending`:

```c
static inline void makePartialFromPclo(Value *callable, Clo *clo, int naargs) {
    int ncaptured = clo->E->S->size;  // Already-captured args
    
    // Create new env as sibling of PCLO's env (shares parent)
    Env *env = makeEnv(clo->E->E);
    int save = PROTECT(env);
    
    // Allocate space for both old and new captured args
    extendFrame(env->S, ncaptured + naargs);
    
    // Copy previously captured args
    copyValues(env->S->entries, clo->E->S->entries, ncaptured);
    
    // Copy new args from stack after them
    copyValues(&(env->S->entries[clo->E->S->size]),
               &(state.S->entries[totalSizeStack(state.S) - naargs]), 
               naargs);
    
    env->S->size = ncaptured + naargs;
    
    // Create new PCLO with further reduced arity
    Clo *pclo = newClo(clo->pending - naargs, clo->C, env);
    PROTECT(pclo);
    callable->val.clo = pclo;
    
    // Replace args with new PCLO
    popn(naargs);
    push(*callable);
    
    UNPROTECT(save);
    apply_partial_creations++;
}
```

**Key insight**: The new PCLO's environment is a **sibling** of the old PCLO's environment (both share the same parent), with a **concatenation** of the old captured args and the new args.

### Over-Application (Staged Application)

When `naargs > clo->pending` (too many arguments):

The system uses **staged over-application** with the `OverApplyStack`:

```c
// For CLO over-application
int pending = clo->pending;
int extra = naargs - pending;

// Create vec to hold extra arguments
Vec *vec = newVec(extra);
int saveExtras = PROTECT(vec);

// Pop extra args in reverse order (entries[0] = first extra)
for (int i = 0; i < extra; i++) {
    Value v = pop();
    vec->entries[extra - 1 - i] = v;
}

// Push over-apply frame
pushOverApplyFrame(extra, vec);
UNPROTECT(saveExtras);

// Perform exact call with correct arity
exactCallFromClo(clo);
```

**`OverApplyFrame` structure**:

```c
struct OverApplyFrame {
    int count;      // Number of extra args
    int index;      // Current index (for resumption)
    Vec *extras;    // The extra arguments
    bool ready;     // True when function returned and we can resume
};
```

**The over-application protocol**:

1. **APPLY phase**: Too many args detected
   - Extra args saved to Vec
   - OverApplyFrame pushed to `overApplyStack` (global)
   - Exact call performed with correct arity
   - Frame marked `ready = false`

2. **RETURN phase**: Function returns
   ```c
   case BYTECODES_TYPE_RETURN:
       Value kont = value_Kont(state.K);
       push(kont);
       applyProc(1);
       
       // Check if we can resume over-application
       if (overApplyStack->size > 0) {
           Value top = peek(-1);
           if (top.type == VALUE_TYPE_CLO || top.type == VALUE_TYPE_PCLO) {
               peekOverApplyStack(overApplyStack)->ready = true;
           }
       }
   ```
   
   **Critical fix**: Only mark `ready = true` if TOS is a callable (this was the bug we just fixed!)

3. **Resumption phase**: After each bytecode, check for staged over-application
   ```c
   if (overApplyStack->size > 0) {
       OverApplyFrame *f = peekOverApplyStack(overApplyStack);
       while (f->ready && f->index < f->count) {
           Value top = peek(-1);
           if (top.type == VALUE_TYPE_CLO || top.type == VALUE_TYPE_PCLO) {
               Value callable = pop();
               int saveCallable = protectValue(callable);
               
               // Apply next extra arg
               Value arg = f->extras->entries[f->index];
               push(arg);
               push(callable);
               applyProc(1);
               
               f->index++;
               apply_staged_steps++;
               UNPROTECT(saveCallable);
               
               // Wait for next RETURN
               f->ready = false;
           } else if (top.type == VALUE_TYPE_KONT) {
               break;  // Still unwinding continuation
           } else {
               cant_happen("unexpected type on stack during over-application");
           }
       }
       
       // All extra args applied?
       if (f->index == f->count) {
           popOverApplyFrame();
           if (overApplyStack->size == 0) break;
           f = peekOverApplyStack(overApplyStack);
       }
   }
   ```

**Why staged over-application?**

Consider: `f(a, b)(c, d, e)` where `f` expects 2 args and returns a function expecting 2 args.

Without staging:
1. Call `f(a, b, c, d, e)` - detect over-application by 3
2. Call `f(a, b)` → returns closure `g` expecting 2 args
3. Now need to apply `g(c, d, e)` - another over-application!

Naive approach would require recursion or complex state. Instead:
1. `f(a, b, c, d, e)` pushes frame with `[c, d, e]`
2. Calls `f(a, b)` exactly
3. `f` returns `g`
4. System automatically applies `g(c)` → returns `h`
5. System automatically applies `h(d)` → returns `i`
6. System automatically applies `i(e)` → final result

**Each application waits for a RETURN** before applying the next arg. This handles nested over-applications naturally.

## Continuations in Detail

### Creating Continuations (LET bytecode)

```c
case BYTECODES_TYPE_LET:
    int offset = readCurrentOffset();  // Resume address
    DEBUG("LET [%04x]", offset);
    
    // Create new stack frame for let-binding
    letStackFrame(state.S);
    
    // Create lightweight continuation (S = NULL)
    state.K = makeKont(offset, state.E, false, state.K);
```

The `LET` bytecode:
1. Creates a new stack frame (saves current frame info)
2. Creates a continuation pointing to the "in" part of `let...in...`
3. Continues executing the binding expressions

### Returning (RETURN bytecode)

```c
case BYTECODES_TYPE_RETURN:
    DEBUG("RETURN");
    
    // Convert continuation to value and apply it
    Value kont = value_Kont(state.K);
    push(kont);
    applyProc(1);
    
    // Mark over-application as ready if result is callable
    if (overApplyStack->size > 0) {
        Value top = peek(-1);
        if (top.type == VALUE_TYPE_CLO || top.type == VALUE_TYPE_PCLO) {
            peekOverApplyStack(overApplyStack)->ready = true;
        }
    }
```

**Key insight**: RETURN doesn't pop the stack or restore state directly. Instead, it turns the continuation into a Value and **applies it as a function**, delegating to `applyProc()`.

### Applying Continuations

```c
case VALUE_TYPE_KONT:
    if (callable.val.kont == NULL) {
        // End of program
        state.C = END_CONTROL;
    } else {
        // Pop result from stack
        Value result = pop();
        protectValue(result);
        
        Kont *kont = callable.val.kont;
        
        // Restore continuation state
        state.C = kont->C;      // Resume address
        state.K = kont->K;      // Parent continuation
        state.E = kont->E;      // Captured environment
        
        // Restore stack (either pop frame or full restore)
        restoreKont(state.S, kont);
        
        // Push result as value for resumed code
        push(result);
    }
```

**`restoreKont()` logic**:

```c
void restoreKont(Stack *s, Kont *source) {
    if (source->S == NULL) {
        // Lightweight: just pop stack frame
        popStackFrame(s);
    } else {
        // Full continuation: restore entire stack snapshot
        copyAllStackEntries(s, source->S);
    }
}
```

### Call-with-Current-Continuation (CALLCC bytecode)

```c
case BYTECODES_TYPE_CALLCC:
    DEBUG("CALLCC");
    
    // Pop the function that will receive the continuation
    Value aexp = pop();
    int save = protectValue(aexp);
    
    // Capture current continuation (including stack)
    Value cc = captureKont();
    protectValue(cc);
    
    // Apply: aexp(cc)
    push(cc);      // Argument
    push(aexp);    // Callable
    UNPROTECT(save);
    applyProc(1);
```

**`captureKont()` creates a full continuation**:

```c
static Value captureKont(void) {
    Kont *K = state.K;
    Value cc;
    
    if (K == NULL) {
        cc = value_Kont(NULL);  // End of program
    } else if (K->S == NULL) {
        // Current continuation is lightweight - need to capture stack
        Stack *s = newStack();
        int save = PROTECT(s);
        copyStackContinuation(s, state.S);  // Copy current stack
        Kont *newK = newKont(K->C, K->E, s, K->K);
        cc = value_Kont(newK);
        UNPROTECT(save);
    } else {
        // Current continuation already has stack snapshot
        cc = value_Kont(state.K);
    }
    
    return cc;
}
```

**Using captured continuations**:

Once captured as a Value, a continuation can be:
- Stored in data structures
- Passed to functions
- Applied multiple times (jumping back to the same point)

When applied, it:
1. Restores the entire execution state (C, E, K, S)
2. Pushes the "return value" onto the restored stack
3. Resumes from the captured point

This enables control flow operators like exceptions, coroutines, and backtracking.

## Failure Continuations (Non-Deterministic Programming)

### Creating Choice Points (AMB bytecode)

```c
case BYTECODES_TYPE_AMB:
    int branch = readCurrentOffset();  // Alternative branch
    DEBUG("AMB [%04x]", branch);
    
    // Create failure continuation
    state.F = makeFail(branch, state.E, state.K, state.F);
    
    // Snapshot entire execution state
    snapshotFail(state.F, state.S);
```

**`makeFail()` creates a full snapshot**:

```c
Fail *makeFail(Control offset, Env *env, Kont *k, Fail *next) {
    Stack *s = newStack();
    int save = PROTECT(s);
    Fail *f = newFail(offset, env, k, s, next);
    UNPROTECT(save);
    return f;
}

void snapshotFail(Fail *target, Stack *s) {
    copyAllStackEntries(target->S, s);  // Full stack copy
}
```

**The choice point saves**:
- `C` = address of alternative branch
- `E` = current environment
- `K` = current continuation
- `S` = complete stack snapshot
- `F` = previous failure continuation (linked list)

### Backtracking (BACK bytecode)

```c
case BYTECODES_TYPE_BACK:
    DEBUG("BACK");
    
    if (state.F == NULL) {
        // No alternatives left - halt
        state.C = END_CONTROL;
    } else {
        // Restore from most recent failure continuation
        state.C = state.F->C;
        state.E = state.F->E;
        state.K = state.F->K;
        restoreFail(state.S, state.F);
        
        // Pop this failure continuation
        state.F = state.F->F;
    }
```

**Complete restoration**: Everything goes back to the choice point, as if the first alternative never happened.

### Cutting Alternatives (CUT bytecode)

```c
case BYTECODES_TYPE_CUT:
    DEBUG("CUT");
    
    #ifdef SAFETY_CHECKS
    if (state.F == NULL) {
        cant_happen("cut with no extant failure continuation");
    }
    #endif
    
    // Discard most recent choice point
    state.F = state.F->F;
```

Commits to the current alternative, preventing backtracking to the most recent `then`.

### Example: Non-Deterministic Search

```fn
fn one_of {
    ([]) { back }              // No choices left - backtrack
    (h @ t) { h then one_of(t) }  // Try h, or backtrack and try rest
}

let x = one_of([1, 2, 3]);
    y = one_of([10, 20]);
in
    x + y
```

Execution:
1. `one_of([1,2,3])` → tries 1, creates choice point with alternatives `[2,3]`
2. `one_of([10,20])` → tries 10, creates choice point with alternatives `[20]`
3. `x + y` → computes `1 + 10 = 11`
4. If result is rejected (implicit `back` at end), backtrack:
   - Restore to `one_of([20])` → tries 20 → computes `1 + 20 = 21`
   - If still rejected, backtrack to outer choice:
   - Restore to `one_of([2,3])` → tries 2 → enters inner `one_of([10,20])` again
   - ... and so on

## Bytecode Execution Flow

### Key Bytecodes

**LAM [nargs] [letrecOffset] [endOffset]** - Create closure
- Creates `Clo` with current environment
- `pending = nargs`
- `C = current bytecode offset` (pointing to function body)
- If `letrecOffset > 0`, captures `letrecOffset` values from stack into closure's environment
- Jumps to `endOffset` (skipping function body)
- Pushes closure value

**VAR [frame] [offset]** - Environment variable access
- Traverses `frame` links in environment chain
- Returns `env->S->entries[offset]`
- Pushes result

**LVAR [offset]** - Stack variable access
- Returns `peek(offset)` from current stack frame
- Used for function parameters and let-bound variables

**APPLY [nargs]** - Function application
- Expects stack: `[arg1, arg2, ..., argN, callable]`
- Calls `applyProc(nargs)` which handles all closure types
- May trigger over-application staging

**RETURN** - Return from function/let
- Converts current continuation to value
- Applies it (delegating to `applyProc`)
- Triggers over-application resumption if result is callable

**LET [offset]** - Let-binding
- Creates new stack frame
- Creates continuation pointing to `offset` (the "in" part)
- Continues with binding expressions

**LETREC [n]** - Mutual recursion
- Patches the environments of top `n` closures on stack
- Each closure's environment is extended to include the current stack frame
- Enables mutually recursive function definitions

**CALLCC** - Capture continuation
- Captures current continuation (including stack)
- Calls function with continuation as argument

**AMB [offset]** - Non-deterministic choice
- Creates failure continuation pointing to `offset` (alternative)
- Snapshots entire execution state
- Continues with first alternative

**BACK** - Backtrack
- Restores most recent failure continuation
- Or halts if no alternatives left

**CUT** - Commit to choice
- Discards most recent failure continuation
- Prevents backtracking past this point

### Instruction Loop Structure

```c
while (state.C != END_CONTROL) {
    // 1. Decode and execute bytecode
    switch (readCurrentByte()) {
        case BYTECODES_TYPE_LAM: ...
        case BYTECODES_TYPE_APPLY: ...
        case BYTECODES_TYPE_RETURN: ...
        // ... etc
    }
    
    // 2. After each instruction, check for over-application resumption
    if (overApplyStack->size > 0) {
        OverApplyFrame *f = peekOverApplyStack(overApplyStack);
        while (f->ready && f->index < f->count) {
            // Apply next extra argument
            Value top = peek(-1);
            if (top.type == VALUE_TYPE_CLO || top.type == VALUE_TYPE_PCLO) {
                Value callable = pop();
                Value arg = f->extras->entries[f->index];
                push(arg);
                push(callable);
                applyProc(1);
                f->index++;
                f->ready = false;  // Wait for next RETURN
            } else if (top.type == VALUE_TYPE_KONT) {
                break;  // Still unwinding
            } else {
                cant_happen("unexpected type during over-application");
            }
        }
        
        if (f->index == f->count) {
            popOverApplyFrame();
        }
    }
}
```

**Key insight**: Over-application resumption happens **between instructions**, not during `APPLY` or `RETURN`. This allows nested over-applications to naturally resolve.

## Memory Management and GC Protection

### The PROTECT/UNPROTECT Pattern

All allocations during execution must be protected from GC:

```c
Vec *vec = newVec(size);
int save = PROTECT(vec);

// ... code that might allocate (triggering GC) ...

UNPROTECT(save);
```

**How it works**:
- `PROTECT(obj)` pushes `obj` to a global protection stack, returns the stack index
- During GC, all objects in the protection stack are marked as reachable
- `UNPROTECT(save)` pops the protection stack back to the saved index

**Critical rule**: Any code between `new*()` and its use that might allocate must protect the new object.

### The `protectValue()` Helper

For Values on the stack that might contain pointers:

```c
int protectValue(Value v) {
    // If v contains a GC-managed pointer, protect it
    // Returns save point for UNPROTECT
}
```

### Common Patterns

**Allocating and protecting**:
```c
Clo *clo = newClo(nargs, state.C, state.E);
int save = PROTECT(clo);
// ... more allocations ...
UNPROTECT(save);
```

**Protecting multiple objects**:
```c
Vec *v1 = newVec(10);
int save = PROTECT(v1);
Clo *c = newClo(5, 0, env);
PROTECT(c);
Frame *f = newFrame();
PROTECT(f);
// ... use objects ...
UNPROTECT(save);  // Pops all three at once
```

**Protecting Values**:
```c
Value result = pop();
int save = protectValue(result);
// ... allocations ...
push(result);
UNPROTECT(save);
```

## Stack Discipline Summary

### When Stack Frames Are Created

1. **LET bytecode** - Entering a let-binding (via `letStackFrame()`)
2. **Namespace operations** - `NS_PUSHSTACK`, `NS_PUSHENV`

### When Stack Frames Are Destroyed

1. **RETURN** → continuation application → `restoreKont()` → `popStackFrame()` (if lightweight)
2. **Full continuation restoration** - `copyAllStackEntries()` restores entire stack state

### When Stack Values Move

1. **Function call (CLO)** - `moveStack()` slides arguments to frame base
2. **Function call (PCLO)** - `moveStack()` slides new args, then captured args copied before them
3. **Continuation restoration** - Entire stack replaced from snapshot

### Lifetime of Values on Stack

- **Function arguments**: Live from APPLY until function returns
- **Let-bound values**: Live from binding until end of `let...in` body
- **Closure captures**: Copied from stack to environment Frame, live as long as closure exists
- **Temporary values**: Live until consumed by next instruction

### Critical Invariants

1. **Stack frame base invariant**: `state.S->entries[state.S->frame .. frame+offset-1]` is the current frame
2. **Closure environment invariant**: PCLO's first env Frame contains captured args, parent env is definition site
3. **Continuation stack invariant**: Lightweight konts have `S==NULL`, full konts have complete snapshot
4. **Over-application staging invariant**: `ready==false` until RETURN produces callable result
5. **GC protection invariant**: All heap-allocated objects must be protected between allocation and use

## Performance Characteristics

### Fast Paths

- **Exact application of CLO**: Just update C, E, and move stack - very fast
- **Lightweight continuations**: Just pop frame - no stack copy
- **Stack-local operations**: push/pop/peek are array operations - O(1)

### Slow Paths

- **Full continuation capture**: Must copy entire stack - O(stack size)
- **Failure continuation (AMB)**: Must snapshot stack, env, kont - expensive
- **Over-application**: Requires staging, multiple apply cycles - overhead per extra arg
- **Partial application**: Allocates new Env and Clo, copies args to env

### Optimization Opportunities

1. **Tail call optimization**: Not currently implemented - recursive calls accumulate kontinuations
2. **Continuation frame sharing**: Multiple continuations could share stack snapshots via copy-on-write
3. **Inline caching for VAR**: Environment lookup traverses linked list - could cache frame offsets
4. **Stack frame pooling**: Reuse Frame allocations instead of allocating fresh each time

## Common Bugs and Debugging

### Stack Underflow

**Symptom**: Crash in `peek()` or `pop()`
**Cause**: Bytecode tried to access more values than on stack
**Debug**: Enable `DEBUG_STEP` to see stack state before each instruction

### GC Corruption

**Symptom**: Crash during or after GC, invalid pointers
**Cause**: Allocation without PROTECT, or UNPROTECT too early
**Debug**: Check all code paths between `new*()` and use for protection

### Over-Application Failure

**Symptom**: `cant_happen("expected CLO or PCLO, got ...")`
**Cause**: Over-application resumption when TOS is not callable
**Fix**: RETURN should only set `ready=true` if result is callable (see recent bugfix)

### Continuation Mismatch

**Symptom**: Wrong values on stack after return, or crash in `restoreKont`
**Cause**: Lightweight kont used where full kont needed, or vice versa
**Debug**: Add assertions in `restoreKont`, check `kont->S != NULL`

### Environment Lookup Failure

**Symptom**: Wrong values returned by VAR, or crash traversing Env chain
**Cause**: Incorrect de Bruijn indices, or closure environment not properly snapshotted
**Debug**: Enable `DEBUG_STEP`, verify VAR indices match actual environment depth

## Future Improvements

### Tail Call Elimination

Currently, recursive calls build up kontinuation chains. Could detect tail calls and reuse current frame:

```c
case BYTECODES_TYPE_TAILAPPLY:
    // Instead of creating continuation, directly jump
    // Reuse current stack frame for tail call
```

Would require bytecode compiler to distinguish tail vs. non-tail calls.

### Register-Based Execution

Current stack-based model requires push/pop overhead. Could allocate fixed "register" slots per frame:

```c
struct StackFrame {
    Value registers[MAX_LOCALS];  // Fixed-size instead of dynamic
    ...
};
```

Would reduce dynamic allocation, improve cache locality.

### Inline Caching

Environment lookups via `VAR [frame] [offset]` traverse linked lists. Could cache results:

```c
struct VarCache {
    Env *last_env;      // Last environment seen
    int cached_depth;   // Cached traversal depth
};
```

Would speed up repeated variable accesses in loops.

### Continuation Frame Compression

Multiple continuations often share common stack prefixes. Could use structural sharing:

```c
struct Kont {
    Stack *S;          // Full stack
    Stack *S_delta;    // Or: just the delta from parent
    Kont *S_base;      // Parent continuation to inherit base from
};
```

Would reduce memory usage for deep continuation chains.

## Conclusion

The CEKF machine implements a sophisticated execution model combining:

- **Stack-based evaluation** for efficiency
- **Environment-based lexical scoping** for closures
- **Continuation-based control flow** for flexibility
- **Failure continuations** for backtracking
- **Staged over-application** for curried functions

The stack discipline is **frame-oriented**, not flat - frames are explicitly managed for `let` bindings and continuations, while function calls operate within the current frame. This design balances simplicity (single stack structure) with flexibility (multiple frame management strategies).

The key to understanding execution flow is recognizing that **most control flow is delegated to continuation application** - RETURN doesn't directly restore state, it applies a continuation, which in turn restores state. This uniformity simplifies the implementation and enables powerful control flow operators like `callcc` and `back`.
