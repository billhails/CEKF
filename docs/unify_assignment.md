# Pseudo-Unification with Alias Patterns

## Problem

Patterns combining pseudo-unification (repeated variable) with alias
(`x=pattern`) are currently rejected:

```fn
fn test {
    (x, x=1) { true }  // error: cannot compare assignment (var x)
    (_, _) { false }
}
```

The intent is: match when both args are equal **and** the value is 1.

## Where the Error Occurs

The error comes from `replaceComparisonPattern()` in `src/tpmc_logic.c`, which
runs a left-to-right scan over each rule's patterns to detect repeated
variables and convert them into comparison patterns.

For `(x, x=1)`:

1. Pattern 0 (`x`): VAR("x") stored in `seen` table.
2. Pattern 1 (`x=1`): ASSIGNMENT with `name="x"`, `value=bigint(1)`.
   `replaceAssignmentPattern()` looks up "x" in `seen`, finds the VAR, and
   emits the error.

The reverse `(x=1, x)` fails similarly: the assignment is stored first, then
`replaceVarPattern()` finds the assignment in `seen` and emits the same error.

## Two Distinct Cases

### Case A: `(x, x=1)` - VAR first, ASSIGNMENT second

The second occurrence's name matches a previously-seen VAR. This should create
a comparison (assert equality) between the two argument positions, **and** the
second argument should additionally be matched against the inner pattern (`1`).

### Case B: `(x=1, x)` - ASSIGNMENT first, VAR second

The assignment is seen first, storing the binding. The second `x` is a plain
repeated variable that should compare against the assigned position.

## Feasibility Assessment

### Case A: `(x, x=1)` - Moderate Complexity

In `replaceAssignmentPattern()`:

- Currently: error if `name` already in `seen`.
- Needed: if the previous is a VAR, create a COMPARISON between the assignment's
  position and the VAR's position, then continue processing the assignment's
  inner value pattern normally.

The tricky part is that the assignment pattern does two things simultaneously:

1. Binds `x` to the current position (substitution for the body).
2. Matches the inner pattern `1` against the value.

When a comparison is also needed, we need to generate **both** a comparison arc
(equality test) and the inner pattern match. The current TPMC representation
does not combine a comparison with an inner value pattern in a single pattern
node.

**Possible approach**: transform the assignment into its inner value pattern
(stripping the alias, since the binding is already recorded in `seen`), and
separately inject a comparison. The assignment's inner value `1` becomes the
actual pattern for that position, and the comparison is added as a guard.
However, the current architecture processes comparisons and value patterns
through different column selection paths in `mixture()` (comparisons go through
the "actionable" check, value patterns go through constructor arcs). Combining
both in one matrix cell would require either:

- A new composite pattern type (comparison + value), or
- Splitting the single pattern into two columns (comparison column + value
  column), which would change the matrix dimensions and require restructuring.

A simpler approach: keep the comparison as the matrix pattern (for the equality
guard) and let the inner literal `1` be handled by adding an additional row
constraint. This could work if the comparison arc's recursive `match()` call
includes the literal check, but currently comparisons are opaque guards without
sub-patterns.

### Case B: `(x=1, x)` - Lower Complexity

In `replaceVarPattern()`:

- Currently: error if `other` is an ASSIGNMENT.
- Needed: extract the assignment's **path** (which will be set during renaming)
  to create a comparison. The assignment pattern binds `x` to its position and
  matches `1`. The second occurrence of `x` just needs to compare its position
  against the first's position.

The complication is that at `replaceComparisonPattern` time, paths haven't been
assigned yet (that happens in `renamePattern`). However, comparisons already
handle this: `renameComparisonPattern()` sets `requiredPath = previous->path`
**after** renaming. So the fix would be:

```c
if (other->pattern->type == TPMCPATTERNVALUE_TYPE_ASSIGNMENT) {
    // The assignment binds 'x' at its position. We need to compare
    // our position against the assignment's position. Use the
    // assignment pattern itself as 'previous' - renaming will assign
    // it the correct path.
    TpmcPatternValue *val = makeTpmcPatternValue_Comparison(other, pattern);
    ...
}
```

But `renameComparisonPattern()` does `pattern->requiredPath =
pattern->previous->path`, and `previous` here would be the ASSIGNMENT pattern.
Since `renameAssignmentPattern()` delegates to `renamePattern(value, path)`,
the assignment pattern's own `path` field is set by `renamePattern()` before
descending. So `previous->path` would correctly be the assignment's position.

This case is feasible with minimal changes to `replaceVarPattern()`.

### Downstream Impact in `tpmc_match.c`

The `patternMatches()` function has `cant_happen("patternMatches encountered
assignment")`, and several other places similarly reject assignments. However,
by the time `mixture()` runs, assignments have been processed by
`collectAssignmentSubstitutions()` which strips them down to their inner value
pattern. So assignments should never reach `tpmc_match.c` - they are resolved
during substitution collection. This means no changes are needed in
`tpmc_match.c` for Case B.

For Case A, the question is whether the inner value pattern (e.g., literal `1`)
correctly survives through the comparison creation. If the assignment is
converted to a comparison, its inner value pattern must still be present in the
matrix for `mixture()` to match against.

## Recommendation

### Implement Case B first: `(x=pattern, x)`

This is the lower-risk change:

1. In `replaceVarPattern()`, when `other` is an ASSIGNMENT, create a
   comparison with `previous = other` (the assignment pattern).
2. The renaming phase will assign paths correctly since assignment patterns
   get their `path` set normally.
3. The substitution collection phase already handles assignments correctly.
4. No changes needed in `tpmc_match.c`.

Estimated effort: small (a few lines in `replaceVarPattern()`).

### Case A: `(x, x=pattern)` requires more thought

The core difficulty is that the current TPMC pattern model assumes each matrix
cell is a single pattern serving one purpose: either a constructor/literal match
**or** a comparison guard. An assignment-after-variable needs both.

Options:

1. **Rewrite to Case B**: a pre-processing pass that normalizes `(x, x=1)` to
   `(x=1, x)` by moving the assignment to the first occurrence. This reuses
   Case B's implementation but requires detecting the situation and swapping
   pattern structure before comparison replacement.

2. **Composite pattern**: introduce a pattern type that combines a comparison
   with an inner pattern. This is more invasive but architecturally cleaner.

3. **Desugaring**: treat `(x, x=1)` as syntactic sugar for `(x=1, x)` at the
   parser or lambda conversion level, before TPMC ever sees it.

**Option 1 (rewrite to Case B)** is likely the most pragmatic. During the
`replaceComparisonPattern` pass, when `replaceAssignmentPattern()` finds its
name already in `seen` as a VAR:

- Convert the current VAR (first occurrence) into the assignment (move the
  binding and inner pattern to the first position).
- Convert the current assignment position into a plain comparison against the
  first position.

This effectively transforms `(x, x=1)` into `(x=1, x)` at the TPMC pattern
level.

## Risk Assessment

- **Case B alone**: low risk, minimal code change, straightforward testing.
- **Case A via rewrite**: moderate risk, needs careful handling of the `seen`
  table state and pattern mutation, but avoids architectural changes.
- **Case A via composite pattern**: higher risk, touches `tpmc_match.c`
  pattern dispatching, exhaustiveness checking, and code generation.
