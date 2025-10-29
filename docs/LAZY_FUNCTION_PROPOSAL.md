# Lazy Function Type Tracking Proposal

## Status: Phase 1 & 2 COMPLETE ✅

**Completed (October 2024):**
- ✅ Phase 1: Type system extended with `isLazy` field
- ✅ Phase 2: Macros marked with `isMacro` flag, laziness propagated to types
- ✅ Type printing shows lazy arguments: `(#() -> Type) -> Result`
- ✅ Critical bug fixed: `freshFunction` now preserves `isLazy` during polymorphic instantiation

**In Progress:**
- ⧗ Phase 3: Automatic adapter generation (manual adapters work, automatic generation in progress)

**Future Work:**
- ☐ Phase 4: Type unification with laziness checking
- ☐ Optimization: Avoid unnecessary wrapping/unwrapping

## Problem Statement

Currently, macros in CEKF are implemented as lazy functions that expect thunked arguments. The system works as follows:

1. **At call site**: When calling a macro, arguments are wrapped in thunks (`fn() { arg }`) in `wrapMacro()` 
2. **In macro body**: Macro arguments are automatically invoked (`arg()`) in `performVarSubstitution()`

The problem: **Macros cannot be passed as first-class functions** because:
- Regular higher-order functions don't know to wrap arguments in thunks
- Type signatures don't distinguish lazy from strict functions
- Example failure case:
  ```fn
  macro lazy_or(a, b) { a or b }  // expects thunks
  
  fn apply_binary(f, x, y) { f(x, y) }  // doesn't know to thunk
  
  apply_binary(lazy_or, true, false)  // FAILS - passes strict values
  ```

## Current Architecture

### Macro Detection (`lambda_conversion.c`)
```c
static bool isMacro(HashSymbol *symbol, LamContext *env) {
    // Checks LamMacroSet in environment chain
}

static LamExp *makePrimApp(...) {
    if (isMacro(symbol, env)) {
        return wrapMacro(PI, symbol, args);  // Wraps args in thunks
    }
    // ... handle other operators
}
```

### Type System (`tc.yaml`)
```yaml
TcFunction:
    data:
        arg: TcType      # Just the argument type
        result: TcType   # Just the result type
```

**Key insight**: No information about laziness in the type system!

## Proposed Solution: Laziness Tracking in Types

### Phase 1: Extend Type System

#### 1.1 Add Laziness Flag to Function Types

Modify `src/tc.yaml`:

```yaml
TcFunction:
    meta:
        brief: Function type
        description: >-
            Represents a function type `a -> b` with an argument type,
            a result type, and a laziness flag indicating if the argument
            should be thunked.
    data:
        arg: TcType
        result: TcType
        isLazy: bool=false  # NEW: true if arg expects a thunk
```

This creates:
- Strict function: `Int -> Int` with `isLazy=false`
- Lazy function: `Int -> Int` with `isLazy=true` (expects `() -> Int`)

#### 1.2 Multi-Argument Consideration

For functions with multiple args where only some are lazy:

```fn
macro weird(strict_arg, lazy_arg, another_strict) { ... }
// Type: Int -> (() -> Bool) -> String -> Result
```

The current representation already handles this via currying:
```
TcFunction(arg=Int, isLazy=false,
  result=TcFunction(arg=Bool, isLazy=true,
    result=TcFunction(arg=String, isLazy=false,
      result=Result)))
```

Each arrow can have its own laziness flag!

### Phase 2: Macro Type Construction

#### 2.1 Detect Macro Definitions

In `lambda_conversion.c`, `convertAstMacro()`:

```c
static LamExp *convertAstMacro(AstDefMacro *astMacro, LamContext *env) {
    // ... existing code ...
    
    // NEW: Mark this function as lazy in the type system
    // We need to pass this information through to type checking
    
    LamLam *lam = newLamLam(CPI(astMacro), args, body);
    lam->isMacro = true;  // NEW FLAG - mark as macro
    
    // ... rest of existing code ...
}
```

Add `isMacro` flag to `LamLam` in `lambda.yaml`:

```yaml
LamLam:
    data:
        args: LamVarList
        exp: LamExp
        isMacro: bool=false  # NEW: true if this is a macro
```

#### 2.2 Type Check Macros

In `tc_analyze.c`, modify `analyzeLam()`:

```c
static TcType *analyzeLam(LamLam *lam, TcEnv *env, TcNg *ng) {
    // ... existing environment setup ...
    
    // Build function type with laziness information
    TcType *returnType = analyzeExp(lam->exp, env, ng);
    PROTECT(returnType);
    
    // NEW: Pass isMacro flag to create lazy function type
    TcType *functionType = makeFunctionType(lam->args, env, returnType, lam->isMacro);
    
    UNPROTECT(save);
    return functionType;
}
```

Modify `makeFunctionType()`:

```c
static TcType *makeFunctionType(LamVarList *args, TcEnv *env, 
                                TcType *returnType, bool isLazy) {
    if (args == NULL) {
        return returnType;
    }
    
    TcType *this = getFromEnv(env, args->var);
    int save = PROTECT(this);
    TcType *next = makeFunctionType(args->next, env, returnType, isLazy);
    PROTECT(next);
    
    // NEW: Create function with laziness flag
    TcType *ret = makeLazyFn(this, next, isLazy);
    
    UNPROTECT(save);
    return ret;
}
```

New helper:

```c
static TcType *makeLazyFn(TcType *arg, TcType *result, bool isLazy) {
    TcFunction *fun = newTcFunction(arg, result);
    fun->isLazy = isLazy;  // Set the flag
    int save = PROTECT(fun);
    TcType *res = newTcType(TCTYPE_TYPE_FUNCTION, TCTYPE_VAL_FUNCTION(fun));
    UNPROTECT(save);
    return res;
}
```

### Phase 3: Automatic Adapter Generation

#### 3.1 Detect When Lazy Function Used as Value

In `tc_analyze.c`, when analyzing function application:

```c
static TcType *analyzeApply(LamApply *apply, TcEnv *env, TcNg *ng) {
    TcType *funType = analyzeExp(apply->function, env, ng);
    // ... existing code ...
    
    // NEW: Check if we're passing a lazy function where strict expected
    if (isLazyFunction(funType) && !contextExpectsLazy(env)) {
        // Generate adapter wrapper
        apply->function = wrapLazyFunctionForStrictContext(apply->function, funType);
    }
    
    // ... rest of existing code ...
}
```

#### 3.2 Generate Wrapper

```c
static LamExp *wrapLazyFunctionForStrictContext(LamExp *lazyFun, TcType *lazyType) {
    // Given: macro f(x, y) { ... }  (expects thunks)
    // Generate: fn(x, y) { f(fn(){x}, fn(){y}) }
    
    int arity = countFunctionArgs(lazyType);
    LamVarList *wrapperArgs = generateFreshVars(arity);
    int save = PROTECT(wrapperArgs);
    
    // Build thunked arguments: fn(){x}, fn(){y}
    LamArgs *thunkedArgs = thunkArgs(wrapperArgs, lazyType);
    PROTECT(thunkedArgs);
    
    // Build application: f(fn(){x}, fn(){y})
    LamApply *innerApply = newLamApply(CPI(lazyFun), lazyFun, thunkedArgs);
    PROTECT(innerApply);
    LamExp *innerExp = newLamExp_Apply(CPI(innerApply), innerApply);
    PROTECT(innerExp);
    
    // Build wrapper: fn(x, y) { ... }
    LamLam *wrapper = newLamLam(CPI(lazyFun), wrapperArgs, innerExp);
    PROTECT(wrapper);
    LamExp *result = newLamExp_Lam(CPI(wrapper), wrapper);
    
    UNPROTECT(save);
    return result;
}

static LamArgs *thunkArgs(LamVarList *vars, TcType *funType) {
    if (vars == NULL) return NULL;
    
    // Check if THIS argument position is lazy
    bool thisArgIsLazy = isArgLazy(funType, 0);
    
    LamArgs *rest = thunkArgs(vars->next, getFunctionResult(funType));
    int save = PROTECT(rest);
    
    LamExp *argExp = newLamExp_Var(CPI(vars), vars->var);
    PROTECT(argExp);
    
    if (thisArgIsLazy) {
        // Wrap in thunk: fn() { arg }
        argExp = thunkMacroArg(argExp);
        PROTECT(argExp);
    }
    
    LamArgs *result = newLamArgs(CPI(argExp), argExp, rest);
    UNPROTECT(save);
    return result;
}
```

### Phase 4: Type Unification

Modify `unify()` in `tc_analyze.c`:

```c
static void unify(TcType *a, TcType *b, TcNg *ng) {
    // ... existing type matching ...
    
    if (a->type == TCTYPE_TYPE_FUNCTION && b->type == TCTYPE_TYPE_FUNCTION) {
        TcFunction *fa = a->val.function;
        TcFunction *fb = b->val.function;
        
        // NEW: Check laziness compatibility
        if (fa->isLazy != fb->isLazy) {
            reportError("Cannot unify lazy and strict functions without adapter");
            // OR: automatically insert adapter here
        }
        
        unify(fa->arg, fb->arg, ng);
        unify(fa->result, fb->result, ng);
        return;
    }
    
    // ... rest of existing code ...
}
```

## Implementation Plan

### Step 1: Type System Extension (Low Risk)
1. Add `isLazy` field to `TcFunction` in `tc.yaml`
2. Run `make` to regenerate type structures
3. Update `makeFn()` to accept and set laziness flag (default `false`)
4. Update type printing (`ppTcFunction`) to show laziness

### Step 2: Macro Marking (Low Risk)
1. Add `isMacro` field to `LamLam` in `lambda.yaml`
2. Set `isMacro=true` in `convertAstMacro()`
3. Pass flag through to `makeFunctionType()`

### Step 3: Test Infrastructure
1. Add test cases for lazy function typing
2. Test unification of lazy/strict functions
3. Test error messages

### Step 4: Adapter Generation (Medium Risk)
1. Implement detection of lazy-function-as-value
2. Implement wrapper generation
3. Test with `map`, `filter`, and other HOFs

### Step 5: Optimization (Optional)
1. Detect unnecessary wrapping/unwrapping
2. Optimize `fn(){x}` when `x` is already strict
3. Consider thunk memoization

## Example Transformations

### Before (Current System)
```fn
macro lazy_or(a, b) { a or b }

// Direct call - works
lazy_or(expensive(), cheap())
// Becomes: lazy_or(fn(){expensive()}, fn(){cheap()})

// HOF usage - FAILS
map(lazy_or, list_of_pairs)
// Tries to call: lazy_or(pair, ???) - expects thunks but gets values!
```

### After (Proposed System)
```fn
macro lazy_or(a, b) { a or b }
// Type: (() -> Bool) -> (() -> Bool) -> Bool
//       ^ lazy        ^ lazy

fn map(f, lst) { ... }
// Type: ((#a -> #b) -> list(#a) -> list(#b))
//        ^ strict function expected

// Automatic adapter insertion
map(lazy_or, list_of_pairs)
// Compiler detects type mismatch and generates:
map(fn(x, y) { lazy_or(fn(){x}, fn(){y}) }, list_of_pairs)
//  ^^^^^^ adapter wraps strict args in thunks ^^^^^^^
```

## Benefits

1. **Backward Compatible**: All existing macro calls continue to work
2. **Type Safe**: Compiler tracks and enforces laziness
3. **Zero Runtime Cost**: Adapters generated at compile time (when automatic generation complete)
4. **Explicit Where Needed**: Type signatures show laziness
5. **Minimal Code Changes**: Most changes in type checker and lambda converter

## Implementation Progress

### Phases 1 & 2: Complete ✅

The type system now fully tracks laziness:

```fn
macro lazy_or(a, b) { a or b }
// Type: (#() -> bool) -> (#() -> bool) -> bool

fn strict_function(x, y) { x or y }  
// Type: (bool) -> (bool) -> bool
```

Type printing clearly distinguishes lazy arguments with the `#() ->` prefix.

### Phase 3: Manual Adapters (Working) ✅

Manual adapter generation works as expected:

```fn
macro lazy_or(a, b) { a or b }

// Manual adapter: converts strict args to lazy
fn strict_lazy_or(x, y) {
    lazy_or(fn() { x }, fn() { y })
}

fn apply_binary(f, x, y) { f(x, y) }

// Now we can pass the adapter to HOFs
apply_binary(strict_lazy_or, true, false)  // Works!
```

**Type transformation:**
- `lazy_or`: `(#() -> bool) -> (#() -> bool) -> bool` (lazy)
- `strict_lazy_or`: `(bool) -> (bool) -> bool` (strict adapter)

The adapter has the type signature that unifies with what HOFs expect, while internally wrapping arguments in thunks.

**Test results:** See `tests/fn/test_manual_adapter.fn` and `tests/fn/test_adapter_types.fn`

### Phase 3: Automatic Adapters (Future Work) ☐

**Current state:** Manual adapters work perfectly and provide a viable solution. Automatic generation would be a nice-to-have enhancement.

**Design:** See `docs/PHASE3_NOTES.md` for detailed implementation options. Recommended approach:
1. Annotate Lambda AST with inferred types during type checking
2. Add post-type-checking pass to detect mismatches
3. Generate adapter wrappers automatically

**Challenges:**
- Need to preserve type information after type checking
- Complex variable name generation
- Handle partial application correctly
- Ensure no performance regression

**Priority:** Low - manual adapters provide sufficient functionality for now.

## Alternative Approaches Considered

### 1. Uniform Lazy Evaluation (Haskell-style)
- Make everything lazy by default
- **Rejected**: Too radical a change, performance implications

### 2. Explicit Thunk/Force Syntax
- Add `&expr` for thunk, `!expr` for force
- **Rejected**: Too much syntactic burden on users

### 3. Separate Macro Application Operator
- Use different syntax for macro calls (e.g., `@lazy_or(a, b)`)
- **Rejected**: Makes macros second-class, breaks composability

### 4. Proposed Approach: Type-Directed Laziness
- **Chosen**: Minimal syntax change, maximal composability, type-safe

## Open Questions

1. **Partial Application**: What's the type of `lazy_or(true)`?
   - Proposed: `() -> Bool` (single lazy argument remaining)

2. **Type Inference**: Can we infer laziness from usage?
   - Proposed: No, laziness must be declared (via `macro` keyword)

3. **Performance**: Is adapter generation too expensive?
   - Proposed: Profile after implementation, optimize if needed

4. **Error Messages**: How to explain lazy/strict mismatch?
   - Proposed: "Function expects lazy argument but received strict value. Consider wrapping in thunk: fn() { ... }"

## Next Steps

1. Review this proposal with the team
2. Create prototype implementation of Phase 1 (type system)
3. Add test cases
4. Implement remaining phases incrementally
5. Update documentation

## References

- `src/macro_substitution.c` - Current macro argument substitution
- `src/lambda_conversion.c` - Macro detection and wrapping
- `src/tc_analyze.c` - Type checking implementation
- `docs/MACROS.md` - Macro design documentation
