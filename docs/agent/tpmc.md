# TPMC (Term Pattern Matching Compiler)

Compiles pattern-matching function definitions into efficient decision trees.

## What TPMC Does

Converts multi-clause pattern-matching functions like:

```fn
fn map {
    (_, []) { [] }
    (f, h @ t) { f(h) @ map(f, t) }
}
```

Into optimized decision trees (DFA-like state machines) that efficiently dispatch based on argument structure.

## Architecture

**Four main files** (all in `src/`):

- `tpmc_logic.c` - Entry point, converts AST patterns to TPMC patterns, creates root variables
- `tpmc_match.c` - Core algorithm implementing the Variable Rule and Mixture Rule
- `tpmc_translate.c` - Generates lambda expressions from the compiled state machine
- `tpmc_compare.c` - Handles comparison patterns (e.g., `(x, x @ _)` where same variable appears twice)

**Supporting files**:

- `tpmc.yaml` - Defines TPMC data structures (states, arcs, patterns, matrices)
- `tpmc_pp.c` - Pretty-printing for debugging
- `tpmc_mermaid.c` - Generates Mermaid diagrams of state machines (use `--dump-tpmc=<function>`)

## The Algorithm (from Pettersson 1992 paper)

### Step 1: Pattern Matrix Construction

- Collects all function clauses into a matrix M where rows = clauses, columns = arguments
- Each pattern gets a unique "path" name (e.g., `p$0`, `p$1`, `p$1$0` for nested)
- Creates array S of final states (function bodies)

### Step 2: DFA Generation via `match(M, S)`

- **Variable Rule**: If top row is all wildcards, return first final state (trivial match)
- **Mixture Rule**: Find first column with constructors/comparisons:
  1. For each unique constructor K in that column:
     - Extract rows matching K, expand nested patterns
     - Recurse on sub-matrix
     - Create arc labeled with K
  2. Handle wildcards as fallback arc
  3. Add error arc if not exhaustive

### Step 3: Optimization

- Reference-count states
- States with refcount > 1 become local functions (letrec-bound)
- Remove duplicate states

### Step 4: Code Generation

- Test states → `switch` expressions (MATCH for constructors, COND for constants)
- Arcs → case arms
- Final states → function bodies with variable bindings in scope

## Key Data Structures

- `TpmcMatrix` - 2D array of patterns (width=args, height=clauses)
- `TpmcState` - Either test state (with arcs) or final state (with body)
- `TpmcArc` - Transition labeled with pattern (constructor/comparison/wildcard)
- `TpmcPattern` - Wildcard, Var, Constructor, Comparison, Character, BigInt, Assignment

## Known Issues & Improvement Areas

1. **Comparison Pattern Ordering**: When a pattern like `(x, x @ _)` appears, the algorithm must ensure `x` is bound before comparison. Current fix: prefer first column if any pattern exists in top row. May not be optimal.

2. **Nested Pattern Efficiency**: Deep nesting can produce many intermediate states. Consider flattening optimizations.

3. **Exhaustiveness Checking**: The language enforces exhaustive pattern matching via the `unsafe` qualifier. Functions with non-exhaustive patterns must be declared `unsafe fn ...`, and functions with exhaustive patterns cannot be declared unsafe. The compiler enforces both rules. See `fn/listutils.fn` for examples like `unsafe fn foldl1 (func, h @ t)` which only handles non-empty lists.

4. **Matrix Column Selection**: The "find first constructor column" heuristic is simple but may not produce minimal DFAs. Could benefit from cost-based selection.

5. **Path Naming**: Generated names like `p$1$2$0` work but are hard to debug. Better naming strategy?

## Debugging TPMC

```bash
# Generate Mermaid diagram of compiled pattern match
./bin/fn --dump-tpmc=functionName path/to/file.fn > diagram.md

# Enable debug output during compilation
# Uncomment DEBUG_TPMC_MATCH in src/common.h
```

**Watch for**:

- "unsafe function" errors (comparisons without proper binding)
- Non-exhaustive pattern matches (may indicate missing cases)
- Large state machines (>20 states suggests optimization opportunity)

## Testing

Pattern matching tests in `tests/fn/test_*.fn` - especially:

- `test_tc.fn` - Complex mutual recursion with patterns
- `fn/barrels.fn` - Non-deterministic search with patterns
- `fn/listutils.fn` - Standard list operations

## References

- `docs/lambda-conversion.md` - Extensive TPMC algorithm walkthrough with examples
- `docs/pettersson92.pdf` - The original Pettersson 1992 paper on pattern matching compilation

## Matrix Visualization Example

To visualize the transformation, consider a simplified `map` function:

```fn
fn map {
  (_, []) { [] }
  (f, h @ t) { f(h) @ map(f, t) }
}
```

**Initial Pattern Matrix (Rows=Clauses, Cols=Arguments):**
```text
Row 1: [ _ ,  []    ]  -> Body 1
Row 2: [ f ,  h @ t ]  -> Body 2
```

**Transformation:**
1. **Variable Rule**: Column 1 has a wildcard (`_`) and a variable (`f`). Since variables match anything, we *could* process this column, but the **Mixture Rule** prefers columns with constructors.
2. **Column Selection**: Column 2 has constructors `[]` (Nil) and `@` (Cons). TPMC selects Column 2.
3. **Splitting**: TPMC creates a switch on Argument 2:

```text
SWITCH(Arg2) {
  CASE Nil:
    // Match Row 1 (Arg2 matched [], Arg1 is wildcard)
    // Residual Matrix for Case Nil:
    // [ _ ] -> Body 1
    // (This eventually simplifies to just executing Body 1)

  CASE Cons(h, t):
    // Match Row 2 (Arg2 matched h@t, Arg1 is f)
    // Residual Matrix for Case Cons:
    // [ f ] -> Body 2 (with h, t bound)
}
```

This matrix reduction process repeats recursively until leaves (Bodies) are reached.
