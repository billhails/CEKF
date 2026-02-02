# Multi-Occurrence Pseudo-Unification

## Overview

Currently, F♮ supports pseudo-unification where the same variable appearing twice in a pattern creates an equality constraint. For example:

```fn
fn same_pair {
    (a, a) { true }
    (_, _) { false }
}
```

This document proposes extending support to three or more occurrences of the same variable in a pattern.

This document is a consequence of significant improvements and bug fixes (Bugs 1-5) documented in [TPMC Comparison Column Ordering](bugs/tpmc-comparison-column-ordering.md). The fixes ensure that:

1. Comparisons are deferred until their required bindings are available (`comparisonIsReady()`)
2. Comparison paths are correctly excluded from free variables during substitution (Bug 2 fix)
3. Comparisons don't incorrectly group with constructor patterns during row partitioning (Bug 5 fix)

These fixes are **prerequisites** for multi-occurrence support.

## Current Limitation

The current implementation explicitly rejects patterns with more than two occurrences:

```c
// src/tpmc_logic.c, lines 419-423
if (other->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON) {
    can_happen(
        "variable '%s' appears more than twice in pattern at +%d %s",
        pattern->pattern->val.var->name, I.lineNo, I.fileName);
}
```

## Use Cases

Supporting 3+ occurrences would enable patterns like:

```fn
// All three elements equal
fn all_same {
    (a, a, a) { true }
    (_, _, _) { false }
}

// Nested structure with repeated variable
fn symmetric {
    (node(a, x, a)) { true }  // left and right children are equal
    (_) { false }
}

// Multiple equality constraints in algebraic simplification
fn simplify {
    (mul(a, mul(a, a))) { pow(a, 3) }  // a * a * a = a^3
    (x) { x }
}
```

## Proposed Implementation

### Design Choice: All Compare to First

When a variable appears N times, generate N-1 comparison patterns, each comparing to the **first** occurrence (the binding site).

For pattern `(a, a, a)`:

- 1st `a` at path `$0`: stored as binding site (VAR pattern)
- 2nd `a` at path `$1`: comparison `$1 == $0` (COMPARISON pattern with `previous = $0`, `requiredPath = $0`)
- 3rd `a` at path `$2`: comparison `$2 == $0` (COMPARISON pattern with `previous = $0`, `requiredPath = $0`)

Both comparisons have `requiredPath = $0`, so they become ready at the same time.

**Important**: All comparisons must point directly to the original VAR pattern, not to other COMPARISON patterns. This is critical because:

1. **Bug 2 fix requirement**: `collectComparisonSubstitutions()` only processes `previous` (the binding site) for variable name substitution into the body. If `previous` points to another comparison instead of the original VAR, the variable name will be lost.

2. **Consistency**: The `requiredPath` must always reference the actual binding location where the variable is bound, not an intermediate comparison.

### Changes Required

#### 1. Modify `replaceVarPattern()` in `src/tpmc_logic.c`

Current logic (lines 406-433):

```c
static TpmcPattern *replaceVarPattern(TpmcPattern *pattern,
                                      TpmcPatternTable *seen, ParserInfo I) {
    TpmcPattern *other = NULL;
    if (getTpmcPatternTable(seen, pattern->pattern->val.var, &other)) {
        if (other->pattern->type == TPMCPATTERNVALUE_TYPE_ASSIGNMENT) {
            can_happen("cannot compare assignment (var %s) at +%d %s",
                       pattern->pattern->val.var->name, I.lineNo, I.fileName);
        }
        if (other->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON) {
            // More than 2 occurrences of same variable not yet supported
            can_happen(
                "variable '%s' appears more than twice in pattern at +%d %s",
                pattern->pattern->val.var->name, I.lineNo, I.fileName);
        }
        TpmcPatternValue *val = makeTpmcPatternValue_Comparison(other, pattern);
        int save = PROTECT(val);
        // Note: requiredPath is set later in renameComparisonPattern when paths
        // are assigned
        TpmcPattern *result = newTpmcPattern(val);
        UNPROTECT(save);
        return result;
    } else {
        setTpmcPatternTable(seen, pattern->pattern->val.var, pattern);
        return pattern;
    }
}
```

**Key observation**: The `seen` table is NOT updated after the second occurrence. When the second occurrence is encountered, `other` is the first occurrence (a VAR pattern), and a COMPARISON is created pointing to it. However, the `seen` table still maps the variable name to the original VAR pattern, not to the new COMPARISON.

This means the current implementation **already has the correct structure** for multi-occurrence support - it just needs the error check removed.

Proposed change:

```c
if (other->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON) {
    // Extract the original binding site (first occurrence)
    // The 'other' here is itself a comparison, so we need to follow
    // 'previous' to get the original VAR pattern
    other = other->pattern->val.comparison->previous;
}
TpmcPatternValue *val = makeTpmcPatternValue_Comparison(other, pattern);
```

This ensures all comparisons point to the original VAR, not to an intermediate COMPARISON.

### Testing

Test cases to add:

```fn
// tests/fn/test_multi_unification.fn

// Three identical values
fn triple {
    (a, a, a) { 0 }
    (_, _, _) { 1 }
}

// Four identical values  
fn quad {
    (a, a, a, a) { 0 }
    (_, _, _, _) { 1 }
}

// Mixed: some unified, some not
fn partial {
    (a, a, b, b) { 0 }  // pairs must match
    (_, _, _, _) { 1 }
}

// Nested with triple occurrence
fn nested_triple {
    (pair(a, pair(a, a))) { 0 }
    (_) { 1 }
}

// In algebraic simplification context
typedef expr {
    num(number) |
    mul(expr, expr) |
    pow(expr, number)
}

fn simplify_cubes {
    (mul(a, mul(a, a))) { pow(a, 3) }
    (mul(mul(a, a), a)) { pow(a, 3) }
    (x) { x }
}
```

### Potential Complications

1. **Readiness ordering**: With multiple comparisons, they may become ready at different times if the binding site is deeply nested. The existing `comparisonIsReady()` check (with `pathIsPrefix()` from Bug 5 fix) handles this correctly since each comparison independently checks if its `requiredPath` is a column header or has been deconstructed.

2. **Free variable tracking**: With the Bug 2 fix, `collectComparisonSubstitutions()` only processes `previous` (the binding site) and explicitly does NOT process `current`. This is correct for multi-occurrence: only the binding site's path will be in free variables, not the comparison paths. Each comparison adds only the binding site to its dependencies.

3. **Code generation**: Multiple comparisons generate multiple `if (eq ...)` tests. The order should not matter since they are all equality checks. Example for `(a, a, a)` with paths `$0, $1, $2`:

   ```scheme
   (if (eq $1 $0)
     (if (eq $2 $0)
       <body with substitution a → $0>
       <alternative>)
     <alternative>)
   ```

4. **Row partitioning**: The Bug 5 fix ensures comparisons don't incorrectly group with constructor patterns. With multiple comparisons in a pattern, each comparison will be evaluated independently via `patternIsActionable()`, ensuring they all reach the wildcard partition until ready.

**Critical validation needed**: Test that when a third occurrence creates a comparison pointing to the original VAR (not to the second comparison), the `requiredPath` is correctly set to the VAR's path, not to the second comparison's path. This is essential because `renameComparisonPattern()` sets `requiredPath = previous->path` - we need `previous` to be the VAR.

## Implementation Steps

1. **Verify `seen` table behavior**: Confirmed that the `seen` table is NOT updated after the second occurrence - it always maps to the first (VAR) pattern.

2. **Modify `replaceVarPattern()`**:
   - Remove the error check for `TPMCPATTERNVALUE_TYPE_COMPARISON`
   - Add logic to extract `previous` from a comparison pattern to ensure all comparisons point to the original VAR

3. **Verify `renameComparisonPattern()` correctness**: Ensure that when `requiredPath = previous->path` is set, `previous` is always a VAR pattern with a valid path, not a COMPARISON pattern.

4. **Add comprehensive test cases** covering:
   - Three identical values in tuple
   - Four or more occurrences
   - Nested patterns with multiple occurrences
   - Mixed patterns with multiple independent equality constraints
   - Edge case: all occurrences in different nesting levels

5. **Run full test suite** to ensure no regressions from Bugs 1-5 fixes.

## Risk Assessment

**Low risk** given the bug fixes:

- Bug 1-4 fixes ensure comparison readiness, row partitioning, and free variable tracking work correctly
- Bug 5 fix ensures comparisons don't group with constructors, which is independent of occurrence count
- The `seen` table already naturally points all occurrences to the first VAR pattern
- Main change is removing an artificial restriction rather than adding new logic

**Validation focus**:

- Ensure `previous` pointer chain doesn't create comparisons pointing to comparisons
- Verify `requiredPath` is set correctly for all N-1 comparisons
- Test patterns with deep nesting where binding site is nested deeper than some comparison sites
