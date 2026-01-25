# ANF (Administrative Normal Form) Conversion

Converts lambda expressions to A-Normal Form where all intermediate computations are named.

## What ANF Does

Transforms nested expressions into a flat sequence of let-bindings where:

- **Atomic expressions (aexp)**: Variables, constants, lambdas - always terminate, never error
- **Complex expressions (cexp)**: Function applications, conditionals - may not terminate or may error
- All complex subexpressions become let-bound temporary variables

Example transformation:

```scheme
(a (b c) (d e))
=>
(let (t$1 (d e))
  (let (t$2 (b c))
    (a t$2 t$1)))
```

## The Algorithm (from Matt Might's blog)

**Core Idea**: Walk expressions depth-first, replacing complex subexpressions with fresh variables, accumulating let-bindings on the way back out.

**Key functions** (all in `src/anf_normalize.c`):

- `normalize(LamExp, tail)` - Main entry point, dispatches on LamExp type
- `replaceLamExp(LamExp, replacements)` - Converts LamExp to Aexp, accumulating replacements
- `letBind(body, replacements)` - Wraps body in let-bindings from replacements table
- `wrapTail(exp, tail)` - Optionally wraps expression in additional let-binding

**The `tail` parameter**: Continuation-like - represents the "rest of the computation" to wrap the current expression in. NULL means this is the final result.

## Implementation Pattern

Most normalize functions follow this pattern:

1. Create a `LamExpTable` for tracking replacements (hash table mapping fresh symbols to LamExps)
2. Call `replaceLamExp()` on subexpressions, which:
   - If subexpr is atomic (var/constant), return it as Aexp
   - If subexpr is complex (application), generate fresh symbol, add to replacements, return symbol as Aexp
3. Build the ANF construct with replaced Aexps
4. Call `wrapTail(exp, tail)` to optionally wrap in outer binding
5. Call `letBind(exp, replacements)` to wrap in all accumulated let-bindings
6. Return the wrapped expression

## Critical Data Flow

```text
LamExp (lambda.yaml)
  ↓ normalize()
  → replaceLamExp() + LamExpTable
    → Aexp (atomic expressions)
  → Build ANF structure (Exp/Cexp)
  → wrapTail()
  → letBind() - wraps in let-bindings
  ↓
Exp (anf.yaml)
```

## Known Complexity Issues

1. **Deeply Nested Functions**: The normalize functions have 30+ dispatch cases, one per LamExp type. Each follows slightly different logic.

2. **GC Protection Overhead**: Extensive use of PROTECT/UNPROTECT macros throughout due to allocations during traversal. Easy to get wrong.

3. **Tail Threading**: The `tail` parameter threads through recursion but its purpose isn't always clear. Sometimes NULL, sometimes accumulated let-bindings.

4. **Dual Type System**: Must track both LamExp (input) and Aexp/Cexp/Exp (output) simultaneously. Easy to confuse which type is which.

5. **Replacements Table**: The `LamExpTable` accumulates symbol→LamExp mappings that become let-bindings, but lifetime and scope isn't always obvious.

## Debugging ANF

```bash
# Enable debug output
# Uncomment DEBUG_ANF in src/common.h

# Dump ANF for inspection
./bin/fn --dump-anf path/to/file.fn
```

**Watch for**:

- Incorrect nesting of let-bindings
- Fresh symbol collisions (shouldn't happen but indicates `freshSymbol()` issues)
- GC crashes (usually from missing PROTECT/UNPROTECT)
- Type mismatches between LamExp and ANF structures

## Potential Improvements

1. **Simplify normalize dispatch**: Could the 30+ cases share more common code?
2. **Clearer tail semantics**: Document when tail is NULL vs. non-NULL
3. **Reduce PROTECT overhead**: Could intermediate allocations be batched?
4. **Better error messages**: When ANF conversion fails, why?
5. **Refactor replacements**: The hash table approach works but is it the clearest?

## Key Files

- `src/anf_normalize.c` - The implementation (1100+ lines)
- `src/anf.yaml` - ANF data structures (Exp, Aexp, Cexp)
- `src/lambda.yaml` - Input lambda structures
- `docs/ANF.md` - Original algorithm notes

## References

- [Matt Might's ANF blog post](https://matt.might.net/articles/a-normalization/)

## Tail Recursion & Wrapping Pitfalls

**Correct tail wrapping is critical**. The `tail` parameter in `normalize` functions represents the "continuation" or "context" that the current expression should return to.

### Incorrect Wrapping (Breaks Tail Recursion)

Wraps the result `t$1` in a new `let` *after* the recursive call returns, forcing a stack frame.

```scheme
;; Source: tail_call(x)
;; Bad ANF:
(let (t$1 (tail_call x))
  t$1) 
;; This is NOT a tail call!
```

### Correct Wrapping (Preserves Tail Recursion)

If `tail` is passed down correctly, the recursive call becomes the body of the `let` chain.

```scheme
;; Source: tail_call(x)
;; Good ANF:
(tail_call x)
;; No wrapping, jumps directly
```

**Rule of Thumb**: When normalizing a function call, if it is in tail position (i.e., `tail` parameter is NULL or empty identity), **do not bind it to a variable** just to return that variable. Return the `AppExp` (or `Cexp`) directly.
