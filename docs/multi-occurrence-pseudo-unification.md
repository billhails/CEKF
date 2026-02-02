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

When a variable appears N times, generate N-1 comparison patterns, each comparing to the **first** occurrence (the binding site). This is simpler than chaining comparisons.

For pattern `(a, a, a)`:

- 1st `a` at path `$0`: stored as binding site
- 2nd `a` at path `$1`: comparison `$1 == $0`
- 3rd `a` at path `$2`: comparison `$2 == $0`

Both comparisons have `requiredPath = $0`, so they become ready at the same time.

### Changes Required

#### 1. Modify `replaceVarPattern()` in `src/tpmc_logic.c`

Current logic:

```c
if (getTpmcPatternTable(seen, varName, &other)) {
    if (other->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON) {
        can_happen("more than twice...");  // ERROR
    }
    // Create comparison, implicitly replaces in table
    ...
} else {
    setTpmcPatternTable(seen, varName, pattern);  // First occurrence
}
```

Proposed logic:

```c
if (getTpmcPatternTable(seen, varName, &other)) {
    // If 'other' is already a comparison, extract the original binding
    TpmcPattern *original = other;
    if (other->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON) {
        original = other->pattern->val.comparison->previous;
    }
    // Create comparison to the original binding site
    TpmcPatternValue *val = makeTpmcPatternValue_Comparison(original, pattern);
    // DON'T update 'seen' - keep pointing to original for future occurrences
    ...
} else {
    setTpmcPatternTable(seen, varName, pattern);  // First occurrence
}
```

Wait - there's a subtlety. The `seen` table currently stores the pattern, and when the second occurrence creates a comparison, the comparison is returned but `seen` isn't explicitly updated. Let me re-examine.

Actually, looking more carefully at the code, `setTpmcPatternTable` is only called for the first occurrence. The comparison is returned but doesn't replace the entry. So subsequent lookups would find the original pattern... but then we hit the error because `other` is not a comparison type.

Ah, I see the issue now. The check `if (other->pattern->type == TPMCPATTERNVALUE_TYPE_COMPARISON)` is checking the *stored* pattern, which should be the original VAR pattern, not a comparison. The error triggers if somehow the stored entry is already a comparison, which shouldn't happen with the current code.

Let me re-read more carefully...

#### Revised Understanding

Looking at the code flow:

1. First `a`: not in `seen`, so `setTpmcPatternTable(seen, "a", pattern1)` stores it
2. Second `a`: found in `seen`, `other = pattern1` (type VAR), creates comparison
3. Third `a`: found in `seen`, `other = pattern1` (type VAR)...

Wait, the original pattern is still in `seen`, so the third occurrence would also find the VAR pattern, not a comparison. The error check seems wrong or there's code I'm missing.

Let me look at whether the `seen` table gets updated after creating a comparison.

#### Re-examination Needed

Before implementing, we need to verify:

1. Does `seen` get updated after the second occurrence?
2. If not, why does the error check for COMPARISON exist?

If `seen` is NOT updated, then the fix might just be removing the error - multiple comparisons would all point to the original binding naturally.

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

1. **Readiness ordering**: With multiple comparisons, they may become ready at different times if the binding site is deeply nested. The existing `comparisonIsReady` check should handle this correctly since each comparison independently checks if its `requiredPath` is a column header.

2. **Free variable tracking**: Each comparison will add the binding site to its free variables. This should work correctly with the existing `collectComparisonSubstitutions` logic.

3. **Code generation**: Multiple comparisons generate multiple `if (eq ...)` tests. The order shouldn't matter since they're all equality checks.

## Implementation Steps

1. Examine the `seen` table behavior more closely to understand current state
2. Modify or remove the error check in `replaceVarPattern()`
3. Ensure comparisons always point to the original VAR pattern, not to other comparisons
4. Add test cases
5. Run full test suite

## Estimated Effort

Low to moderate - likely 2-4 hours including testing, assuming the existing TPMC machinery handles multiple comparisons correctly (which it should, since each comparison is independent).
