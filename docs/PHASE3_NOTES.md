# Phase 3 Implementation Notes

## Current Status

Manual adapters work perfectly. Users can write:

```fn
macro lazy_or(a, b) { a or b }

fn strict_lazy_or(x, y) {
    lazy_or(fn() { x }, fn() { y })
}

apply_binary(strict_lazy_or, true, false)  // Works!
```

## Automatic Adapter Generation: Design Options

### Option 1: Post-Type-Checking Pass

Add a new pass after type checking that:
1. Walks the Lambda AST
2. Re-analyzes types at each application point  
3. Detects lazy/strict mismatches
4. Generates and inserts adapter code

**Pros:** Clean separation of concerns
**Cons:** Need to re-run type analysis, complex

### Option 2: During Type Checking

Modify `tc_analyze.c` to:
1. Detect mismatches during unification
2. Mark locations needing adapters (add field to LamApply?)
3. Generate adapter code immediately

**Pros:** Type information already available
**Cons:** Mixes type checking with code generation

### Option 3: Annotated Lambda AST

1. During type checking, annotate Lambda AST nodes with their types
2. Add new pass that reads annotations and generates adapters
3. Similar to how ANF works

**Pros:** Clean, type info preserved
**Cons:** Requires extending Lambda AST with type annotations

## Recommended Approach: Option 3

1. Add optional `TcType *inferredType` field to Lambda expressions
2. During type checking, store inferred types
3. Add `generateAdapters(LamExp *)` pass after type checking
4. Walk AST, check for lazy/strict mismatches using stored types
5. Generate adapter wrappers where needed

### Implementation Steps

1. **Extend Lambda AST** (`lambda.yaml`):
   ```yaml
   LamExp:
     data:
       # ... existing fields ...
       inferredType: TcType*=NULL  # Type annotation from type checker
   ```

2. **Store types during checking** (`tc_analyze.c`):
   ```c
   static TcType *analyzeExp(LamExp *exp, TcEnv *env, TcNg *ng) {
       TcType *type = /* ... existing type inference ... */;
       exp->inferredType = type;  // Store for later passes
       return type;
   }
   ```

3. **Add adapter generation pass** (`lambda_adapter.c`):
   ```c
   LamExp *generateAdapters(LamExp *exp) {
       // Walk AST looking for applications
       // Check if function type is lazy but context expects strict
       // Generate wrapper: fn(x,y) { lazy_fn(fn(){x}, fn(){y}) }
       // Replace lazy_fn reference with wrapper
   }
   ```

4. **Integrate into pipeline** (`main.c`):
   ```c
   typeCheck(exp, builtIns);
   exp = generateAdapters(exp);  // NEW PASS
   exp = inlineExp(exp);
   ```

### Key Challenges

1. **Detecting context**: How do we know if context expects strict vs lazy?
   - Check the type of the parameter receiving the function
   - If parameter type is strict function but value is lazy, generate adapter

2. **Generating fresh variables**: Adapter needs fresh variable names
   - Use existing name generation utilities

3. **Preserving semantics**: Ensure adapter doesn't change behavior
   - Adapter just wraps/unwraps, no logic

4. **Partial application**: Handle curried functions correctly
   - Generate adapter for remaining arguments only

## Testing Strategy

1. Test lazy macro passed to strict HOF (map, filter, fold)
2. Test partial application of macros
3. Test macros with mixed lazy/strict arguments (future)
4. Test nested adapters (macro returning macro)
5. Performance testing (adapter overhead)

## Future Optimizations

1. **Avoid double-wrapping**: If argument is already a thunk, don't wrap again
2. **Inline trivial adapters**: Adapter that just forwards could be inlined
3. **Memoization**: Cache adapter generation for same type signatures

## Documentation Needed

1. User guide: When to write manual adapters
2. Type error messages: Explain lazy/strict mismatch clearly
3. Performance guide: Cost of adapters
4. Migration guide: Converting old code to use lazy types
