# Function Inlining Pass

## Problem

Beta reduction (`minlam_beta.c`) only fires when the function position of an
application is syntactically a lambda:

```scheme
((λ (x) body) arg)   ;; beta-redex, reduced
(f arg)              ;; variable in function position, not reduced
```

When a `letrec`-bound name like `OR$0$arity_2` is applied, beta reduction sees
a `MINEXP_TYPE_VAR` and does nothing. It has no mechanism to look up bindings.

## Solution: Inlining (Unfolding)

A separate pass replaces variable applications with direct lambda applications
when the variable is `letrec`-bound to a known lambda. This creates beta-redexes
that the existing beta reduction pass can then simplify.

### Before inlining

```scheme
(OR$0$arity_2 (λ ...) (λ ...) k f)
```

### After inlining

```scheme
((λ (a b k f) (a (λ (rv f') (if rv (k 1 f') (b k f'))) f))
  (λ ...) (λ ...) k f)
```

### After beta reduction

```scheme
((λ ...) (λ (rv f') (if rv (k 1 f') ((λ ...) k f'))) f)
```

Which further reduces to a direct `if`.

## Pipeline Placement

Current `--target-c` pipeline:

```
CPS → β → AMB → β → η → Shake → Closure Convert → ...
```

With inlining:

```
CPS → β → AMB → β → η → Shake → Inline → β → η → Shake → Closure Convert → ...
```

The second shake removes bindings that become dead after inlining. The sequence
(inline → β → η → shake) can be iterated to a fixpoint, or just run twice.

## Inlining Criteria

A `letrec` binding `f = λ ...` is eligible for inlining when:

1. **Non-recursive**: `f` does not participate in any recursion cycle (direct or
   mutual). See [Recursion Detection](#recursion-detection).
2. **Called once** (always inline, no code growth) OR **small body** (inline at
   all call sites, bounded code growth).
3. The binding's value is a lambda (not an arbitrary expression).

### Recursion Detection

A simple "is `f` free in its own body" check catches direct recursion but misses
mutual recursion:

```scheme
(letrec
  ((f (λ () ... (g) ...))
   (g (λ () ... (f) ...)))
  ...)
```

Here neither `f` nor `g` mentions itself directly, but inlining either one leads
to unbounded expansion.

#### Using the Existing Dependency Graph

`minlam_shake.c` already builds what is needed:

- `getAllKeys(bindings)` — the set of all bound names.
- `buildDependencyGraph(bindings, keys)` — a `SymbolSetMap` mapping each name to
  the set of sibling names that appear free in its body.
- `computeLiveBindings(deps, rootSet)` — transitive closure over the dependency
  graph from a seed set.

To detect whether binding `f` participates in any cycle:

```c
SymbolSet *fDeps = NULL;
bool isRecursive = false;
if (getSymbolSetMap(deps, f, &fDeps)) {
    // Seed with f's direct dependencies, NOT f itself
    SymbolSet *transitive = computeLiveBindings(deps, fDeps);
    isRecursive = getSymbolSet(transitive, f);
}
```

This seeds the worklist with `f`'s direct dependencies (not `f`), then checks if
the transitive closure includes `f`. It catches both direct and mutual recursion.

Complexity is $O(n^2)$ in the number of bindings per `letrec`, but `letrec`
groups are small in practice.

#### Alternative: Tarjan's SCC

For larger binding groups, compute strongly connected components of the
dependency graph in $O(n + e)$. Any SCC with more than one member is a mutual
recursion group; a singleton SCC where the name appears in its own dependency set
is directly recursive. Everything else is safe to inline. This is the approach
used in GHC's simplifier.

## Occurrence Analysis

An occurrence analysis pass counts how many times each `letrec`-bound variable is
referenced in the body and sibling bindings. This informs the inlining decision:

- **0 references**: Dead code (already handled by tree shaking).
- **1 reference**: Inline unconditionally. No code duplication. The binding
  becomes dead after inlining and is removed by the subsequent shake pass.
- **N references** (N > 1): Inline only if the body is "small" (below some
  size threshold). See [Body Size Estimation](#body-size-estimation).

The occurrence count must account for all references within the same `letrec`
scope: both the body expression and all sibling binding values.

### Body Size Estimation

A recursive walk over the `MinExp` body of a candidate binding, returning a
weighted node count.

#### Suggested weights

| Node type | Weight | Rationale |
|---|---|---|
| `Var`, `StdInt`, `BigInteger`, `Character` | 0 | Trivial to substitute, no runtime cost |
| `Lam` | 0 | At a known call site the lambda args get beta-reduced away |
| `Apply` | 1 | Real runtime cost (closure call or builtin dispatch) |
| `Prim` | 1 | Arithmetic operation |
| `Iff` | 1 | Branch |
| `Match`, `Cond` | 1 + per case | Each case arm is real work |
| `MakeVec` | 1 | Allocation |
| `LetRec` | 1 + recurse into bindings | Introduces closures |
| `Builtin` | 1 | Like Apply |

With these weights, `OR$0$arity_2`'s body scores about 3 (one apply of `a$3`,
inner lambda at 0, one if at 1, two leaf branches at ~1 each). A threshold of
10-15 comfortably admits it while excluding genuinely large functions like
`__error__$0$arity_3` (a long chain of string construction + I/O calls).

A simple unweighted node count with a threshold of ~20 also works as a starting
point. The threshold can be tuned later.

Note: size is irrelevant for the "called once" case. The size threshold only
gates multi-reference inlining.

## Existing Infrastructure

These existing facilities can be reused:

| Facility | Location | Use |
|---|---|---|
| Capture-avoiding substitution | `minlam_subst.h` / `substMinExp` | Substitute lambda body at call site |
| Free variable analysis | `minlam_freeVars.h` / `freeVarsMinExp` | Dependency graph, recursion check |
| Dependency graph | `minlam_shake.c` | Recursion detection |
| Beta reduction | `minlam_beta.c` | Clean up after inlining |
| Eta reduction | `minlam_eta.c` | Clean up after inlining |
| Tree shaking | `minlam_shake.c` | Remove dead bindings after inlining |

## Safety Considerations

- **Non-recursive only**: Never inline recursive or mutually recursive bindings.
- **Lambda values only**: Only inline bindings whose RHS is a lambda. Inlining
  arbitrary expressions could duplicate effects.
- **The existing beta reduction safety check still applies**: After inlining
  produces `((λ ...) args...)`, beta reduction only substitutes when all
  substituted arguments are A-expressions (`isAexp`). For the target use cases
  like `OR$0$arity_2`, the arguments are lambdas and variables, which are
  A-expressions, so this works out.
- **GC protection**: The inlining pass must follow CALLER PROTECTS and PROTECT
  results of allocating calls, like all other MinExp passes.

## Example: OR Inlining in pythagorean triples

The `require` function calls `OR$0$arity_2`:

```scheme
(#12324247#66306#require$0
  (λ (p$5$0 k$22 f$381)
    (OR$0$arity_2
      (λ (k$23 f$382) (k$23 p$5$0 f$382))
      (λ (k$24 f$383) (f$383))
      k$22 f$381)))
```

`OR$0$arity_2` is defined as:

```scheme
(OR$0$arity_2
  (λ (a$3 b$1 k$128 f$183)
    (a$3
      (λ (rv$115 f$184)
        (if rv$115
          (k$128 1 f$184)
          (b$1 k$128 f$184)))
      f$183)))
```

After inlining OR into require, the call becomes:

```scheme
((λ (a$3 b$1 k$128 f$183)
    (a$3
      (λ (rv$115 f$184)
        (if rv$115
          (k$128 1 f$184)
          (b$1 k$128 f$184)))
      f$183))
  (λ (k$23 f$382) (k$23 p$5$0 f$382))
  (λ (k$24 f$383) (f$383))
  k$22 f$381)
```

All four arguments are A-expressions (lambdas and variables), so beta reduction
fires and substitutes:

```scheme
((λ (k$23 f$382) (k$23 p$5$0 f$382))
  (λ (rv$115 f$184)
    (if rv$115
      (k$22 1 f$184)
      ((λ (k$24 f$383) (f$383)) k$22 f$184)))
  f$381)
```

A second beta pass reduces further, eliminating all the lambda overhead and
leaving a direct conditional test.
