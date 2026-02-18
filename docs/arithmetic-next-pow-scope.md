# `n_pow` staged scope

Date: 2026-02-16.  
Status: scoped; implementation deferred.

This document defines the rollout boundary for `n_pow` in the staged arithmetic layer (`src/arithmetic_next.c`).

## Goals

- keep behavior parity with legacy `npow` while staged operators are still being built,
- avoid early regressions in the most asymmetric operator,
- make `n_pow` rollout additive and testable in small slices.

## Current behavior

`n_pow(left, right)` currently validates the normalization plan and delegates to legacy `npow(left, right)`.

This is intentional for now.

## Why `pow` is different

Unlike `add/sub/mul/div/mod`, `pow` is asymmetric by design:

- base and exponent are normalized differently,
- domain rules are not a single common-domain promotion,
- several combinations route to complex/exponential formulas.

Because of this, `pow` should not be staged as a symmetric operator table first.

## Staged rollout order

### Phase P0 (now)

- keep delegation to legacy `npow`,
- keep plan validation in `n_pow` so unsupported pairs still fail consistently.

### Phase P1 (safe scalar-only direct cases)

Implement direct handlers for these pairs only:

- `STDINT ^ STDINT`
- `BIGINT ^ BIGINT|STDINT`
- `IRRATIONAL ^ IRRATIONAL`

All other pairs continue delegating to legacy `npow`.

### Phase P2 (rational exponent/base paths)

Add staged paths for:

- real base with rational exponent (`realPowRat` behavior),
- rational base with integer/rational exponent where simplification is expected.

### Phase P3 (imag/complex)

Add staged routing and handlers for:

- imag base with int exponent,
- complex base with int/rational exponent,
- complex base with complex exponent.

Complex formulas should still reuse existing math primitives first; no formula rewrite is required in scope.

## Non-goals for staged `n_pow`

- no new numeric domains,
- no semantic changes to branch-cut/ordering behavior,
- no change to divide-by-zero or other legacy hard-fail behavior.

## Test strategy

For each phase:

1. add focused end-to-end cases for new pair(s),
2. run parity assertions against legacy `npow`,
3. keep previously delegated pairs covered by parity checks.

`pow` parity tests should avoid depending on a specific representation when numeric equality is sufficient.

## Exit criteria to start implementation

- `n_cmp` staged for scalar domains (so cross-checks remain stable),
- imag/complex rollout for add/sub/mul complete,
- `tests/test_arithmetic_next` stable in testing mode.
