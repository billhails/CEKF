# Arithmetic Implementation Review Notes

Date: 2026-02-15.
Scope: `src/arithmetic.c` internals, while preserving the public API in `src/arithmetic.h`.

## Interface boundary (stable)

The external interface is already compact and usable:

- Binary arithmetic: `nadd`, `nsub`, `nmul`, `ndiv`, `nmod`, `npow`
- Unary/parts: `nneg`, `nrand`, `real_part`, `imag_part`, `mag_part`, `theta_part`
- Comparison and lifecycle: `ncmp`, `init_arithmetic`, `markArithmetic`

Maintainability issues are almost entirely internal to `src/arithmetic.c`.

## High-level internal structure today

The file mixes several responsibilities:

1. Type classification and coercion (`coerce`, conversion helpers).
2. Primitive operations by domain (int, rational, irrational, complex).
3. Composite operator dispatch (`dispatch` for most operators; custom logic for `npow`).
4. GC protection choreography (`PROTECT`/`UNPROTECT` and `protectValue`).
5. Formula-specific implementations (rational algebra, complex trig/polar identities).

This concentration creates strong coupling between type-system rules, numerical semantics, and memory-safety mechanics.

## What is working well

- Public entry points are centralized and consistent.
- Safety checks exist (`ASSERT_*`, `cant_happen`) and catch invalid states aggressively.
- Numeric simplification helpers are present (`ratSimplify`, `comSimplify`, `irratSimplify`).
- Overflow-aware integer paths exist (`safe_add`, `safe_sub`, `safe_mul`).
- Most arithmetic operators (`+ - * / %`) share the same dispatch skeleton.

## Main maintainability pain points

### 1) Coercion logic is exhaustive but monolithic

`coerce(Value *left, Value *right, int *save)` is a large nested `switch` on `(left.type, right.type)`.

- It combines *type promotion policy* with *temporary value allocation/protection*.
- Any new numeric subtype or rule change forces edits across many branches.
- Repeated blocks appear across branches (especially real/imaginary-to-complex promotion).

This function is effectively a hand-written type relation matrix embedded in control flow.

### 2) Dispatch model is partly uniform, partly special-case

`dispatch(...)` is a good abstraction for `nadd/nsub/nmul/ndiv/nmod/ncmp`, but `npow` bypasses it and duplicates the cross-product handling manually.

- `npow` has a large nested `switch` with many repeated branch shapes.
- Special formulas are mixed with coercion decisions.
- The file therefore has two distinct dispatch paradigms that evolve independently.

### 3) Type taxonomy drives combinatorial branching

Concrete runtime types include:

- real-domain values: `STDINT`, `BIGINT`, `RATIONAL`, `IRRATIONAL`
- imaginary scalar encodings: `STDINT_IMAG`, `BIGINT_IMAG`, `RATIONAL_IMAG`, `IRRATIONAL_IMAG`
- pair form: `COMPLEX`

Note: `RATIONAL` and `COMPLEX` both use a two-entry `Vec` representation internally, but they are different semantic domains.

Several paths normalize these to complex before operation, but that normalization is repeated in many places instead of being represented once as data.

### 4) Recursion + GC protection increases cognitive load

Examples:

- `ratOp` recursively re-enters after ad-hoc conversions.
- `npow` calls `npow` recursively in selected branches.
- Protection scopes are often broad and nested around control-flow-heavy logic.

The code is careful, but hard to audit because numeric semantics and lifetime management are interleaved.

### 5) Formula-heavy code is distributed rather than encapsulated

Formula implementations are mathematically valid in isolation, but not grouped by a coherent policy layer.

- Rational identities (`ratAddSubOrMod`, `rat_ac_bd`, `ratDiv3`) are split among conversion logic.
- Complex formulas (`irrPowCom`, `comPowCom`, `comRoot`, polar conversions) are spread across paths selected by outer type branching.

This makes it difficult to reason about equivalence and performance of alternative evaluation routes.

### 5.1) Higher-order rational helper pattern (`IntegerBinOp` arguments)

One especially subtle pattern is the rational helper trio that takes function pointers:

- `ratAddSubOrMod(IntegerBinOp base_op, Value left, Value right)`
- `rat_ac_bd(IntegerBinOp base_op, Value left, Value right)`
- `ratDiv3(IntegerBinOp base_op, Value left, Value right)`

These are used through `ratOp(...)` to build multiple operators from shared scaffolding.

Behavior summary:

- `ratAddSubOrMod` computes

 $$
 \frac{a}{b} \;\text{op}\; \frac{c}{d}
 = \frac{(ad)\;\text{op}\;(bc)}{bd}
 $$

 where `base_op` is `intAdd`, `intSub`, or `intMod`.

- `rat_ac_bd` computes

 $$
 \frac{a}{b} \;\operatorname{op}\; \frac{c}{d}
 = \frac{a\;\operatorname{op}\;c}{b\;\text{op}\;d}
 $$

 and is used with `intMul` for rational multiplication.

- `ratDiv3` inverts the right rational and delegates to `rat_ac_bd`, so division becomes multiply-by-reciprocal.

Why they exist:

- Reduce duplicated arithmetic formulas across `ratAdd`, `ratSub`, `ratMul`, `ratDiv`, `ratMod`.
- Keep integer-level behavior centralized in existing integer operators (`intAdd`, `intSub`, `intMul`, `intMod`).
- Reuse rational simplification and protection handling in one path.

Why this feels intractable in practice:

- The abstraction is mathematically elegant but semantically leaky: the same function-pointer slot (`base_op`) is reused for operations with different algebraic meaning.
- `ratOp` adds recursive conversion/simplification control around these helpers, so behavior is split across higher-order callbacks and control-flow recursion.
- GC/protection concerns are interleaved with callback-driven math, making call chains hard to audit.
- `ratDiv3` is named structurally (internal transformation) rather than semantically (division intent), which increases cognitive load.

Net effect:

The function-pointer approach removed local duplication, but it also obscures operator intent and makes it harder to trace exact execution paths when debugging or extending behavior.

### 6) Error/safety style is consistent but verbose

There are many `cant_happen("unrecognised ...")` cases.

- Good for fail-fast behavior.
- In practice, this increases boilerplate and masks the underlying matrix shape that could otherwise be represented declaratively.

## Specific observations worth preserving

- The distinction between rationals containing only integer components and imaginary wrappers is explicit and important.
- `coerce` intentionally avoids some mathematically “maximal” coercions for practical behavior.
- `comSimplify` and rational simplification preserve user-visible canonicalization in many cases.
- `init_arithmetic` initializes `Zero`/`One` as `BigInt`, which affects equality/comparison and should remain stable.

## Risks if changes are made without a plan

- Semantic drift in corner cases (`npow` with rational/complex combinations).
- GC lifetime bugs introduced by moving logic without clear ownership/protection conventions.
- Behavior changes due to altered coercion precedence.
- Performance regressions from eager promotion to complex or irrational forms.

## Bottom line

The core problem is less about incorrect math and more about *representation of policy*.

Today, policy is encoded as deeply nested control flow. A maintainable redesign should make coercion and operator dispatch explicit data (tables/rules), then keep formula implementations as focused leaf functions.
