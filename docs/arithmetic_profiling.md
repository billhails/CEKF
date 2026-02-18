# Arithmetic profiling notes

Date: 2026-02-16

## Scope

This note records profiling work for staged arithmetic integration and immediate optimization passes. It focuses on arithmetic dispatch overhead and does not include GC redesign work.

## Method

- Built from clean states when comparing modes.
- Used callgrind and callgrind_annotate.
- Collected both targeted arithmetic harness data and a runtime mixed-types workload.

## Baseline findings

### Targeted harness profile

Workload: tests/test_arithmetic_next

- Clean testing baseline observed earlier: 1,984,064 Ir.
- Main staged hotspots in this phase were normalization/dispatch helpers:
  - requirePlan
  - applyCommonDomainBinary
  - getDomainHandler
  - applyCommonDomainCompare
- Memory/protection activity remained significant in the overall profile.

### Runtime profile

Workload: bin/fn --include=fn tests/fn/test_arithmetic_mixed_types.fn

- Program total: 140,490,365 Ir.
- Dominant costs were allocator/GC/protection/hash paths, not arithmetic kernels:
  - libc malloc/free/realloc family
  - reallocate, protect, unProtect, allocate
  - hashSet, hashGet, uniqueHashSymbol
- Arithmetic entry next was present but comparatively small in this runtime mix.

## Optimization work performed

### Change kept

File changed: src/arithmetic_next.c

- For non-pow operations, removed repeated normalization-plan construction from the hot common-domain path.
- Kept pow on the asymmetric plan route (requirePlan + getPowDomainHandler) to preserve semantics.

Result:

- Current best measured targeted profile: 1,883,709 Ir.
- Improvement vs clean baseline: 100,355 fewer instructions.
- Relative reduction: about 5.1 percent.

### Changes tested and reverted

- Tried a class-table rewrite of requireCommonDomain.
- Measured slight regression (1,886,589 Ir vs 1,883,709 Ir on the same targeted profile).
- Reverted to the faster branch-chain variant.

## Correctness checks

- tests/test_arithmetic_next passed after the kept optimization.
- tests/test_arithmetic_dispatch passed after the kept optimization.
- Comprehensive stress run re-check completed with no errors.

## Interpretation

- Dispatch simplification produced a measurable gain in arithmetic-targeted workload.
- Runtime-wide workloads are still dominated by allocation/protection/hash and pipeline work outside arithmetic dispatch.
- Given current scope, arithmetic-only optimization should target conversion churn and repeated helper calls before deeper system-level work.

## Suggested next steps for tomorrow

1. Profile and reduce conversion churn in src/arithmetic_next.c (for example nextToIrr and complex conversion paths).
2. Validate each micro-change with tests/test_arithmetic_next and callgrind deltas before stacking more edits.
3. Re-run one runtime mixed-types profile to confirm that arithmetic changes still move total runtime in meaningful cases.
4. Keep GC architecture untouched for now and avoid broad runtime refactors during arithmetic tuning.

## Tomorrow quick-start checklist (first 30 minutes)

1. Verify working tree and current arithmetic changes.

  ```bash
  git status --short
  git diff -- src/arithmetic_next.c
  ```

  Expected: only intended arithmetic/profile-note edits are present.

1. Reconfirm focused correctness baseline.

  ```bash
  make MODE=testing tests/test_arithmetic_next >/dev/null && ./tests/test_arithmetic_next
  make MODE=testing tests/test_arithmetic_dispatch >/dev/null && ./tests/test_arithmetic_dispatch
  ```

  Expected: both tests exit cleanly with no error output.

1. Recreate arithmetic-targeted profile baseline for comparison.

  ```bash
  valgrind --tool=callgrind \
    --callgrind-out-file=callgrind.out.test_arithmetic_next.current \
    ./tests/test_arithmetic_next >/dev/null 2>&1

  callgrind_annotate --auto=yes callgrind.out.test_arithmetic_next.current \
    | grep -E "PROGRAM TOTALS|requireCommonDomain|applyCommonDomainBinary|applyCommonDomainCompare|requirePlan"
  ```

  Expected: totals near current reference (~1.88M Ir), with the same dispatch helpers visible.

1. Make one small conversion-churn optimization in src/arithmetic_next.c, then re-run step 2 and step 3.

  Expected: no correctness regressions and a measurable or neutral callgrind delta.

1. If delta is positive, capture the exact before/after numbers in this file before moving to a second optimization.
