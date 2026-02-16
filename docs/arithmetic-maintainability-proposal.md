# Proposal: Make Arithmetic More Maintainable (Without API Changes)

Date: 2026-02-15.  
Status: design proposal.

See [arithmetic review notes](arithmetic-review-notes.md) for details of the existing implementation.

## Scope

Refactor `src/arithmetic.c` internals for maintainability while preserving the current external API in `src/arithmetic.h`.

Out of scope for this proposal:

- adding vectors, matrices, quaternions, or other new runtime types,
- changing language-visible arithmetic semantics,
- rewriting mathematical formula implementations.

Future domains are considered only as design constraints.

Future note: a soft-NaN style numeric value has been identified as a likely addition to replace hard abort behavior on undefined arithmetic (for example divide/mod by zero). That is out of scope here but should shape error-path design.

Integration of new dispatch into existing public entry points is explicitly deferred; this proposal targets parallel implementation and validation first.

## Core decisions

1. Use data-driven dispatch, not large nested switches.
2. Model coercion as compatibility + normalization rules, not a single “lower to higher” numeric ladder.
3. Keep scalar arithmetic as the only implemented family in this refactor.
4. Keep non-scalar families explicitly unsupported for now.

## Dispatch model

Two-stage execution:

1. **Normalize/coerce** operands using operator-specific rules.
2. **Dispatch** via operator × lhs-domain × rhs-domain table.

For commutative operators (`+`, `*`, often comparison), normalization may map both operands to a shared domain. For non-commutative/asymmetric operators (`pow`, and potentially others), normalization should preserve operand roles and may normalize only one side.

Suggested scalar domains:

- `DOM_INT_STD`, `DOM_INT_BIG`, `DOM_RAT`, `DOM_IRR`, `DOM_IMAG`, `DOM_COMPLEX`

These are semantic dispatch domains, not storage-layout classes; for example, `RATIONAL` and `COMPLEX` may both be represented as 2-entry vectors internally while remaining distinct domains.

Suggested operator set:

- `OP_ADD`, `OP_SUB`, `OP_MUL`, `OP_DIV`, `OP_MOD`, `OP_POW`, `OP_CMP`

Tables:

- `binaryTable[OP_COUNT][DOM_COUNT][DOM_COUNT]`
- `compareTable[DOM_COUNT][DOM_COUNT]`

Each cell is either a concrete handler or explicit `not_supported`.

## Coercion policy

For scalars, promotion to a common domain is often valid. For composite domains, it may not be.

Rules to preserve:

- scalar→vector (if vectors exist later) is not automatic type promotion,
- scalar broadcast is an operator rule, not a coercion rule,
- validity may depend on extra constraints (for example shape checks).

Represent coercion as:

1. compatibility status: `valid` / `invalid` / `needs extra check`,
2. normalization action: `none`, `promote`, `to complex`, etc.

Normalization actions are operator-specific and can be asymmetric (`normalize base`, `normalize exponent`, or both).

Current hard-fail arithmetic paths should be treated as temporary implementation behavior, not permanent API contracts, to allow later soft-NaN semantics.

## Domain-family boundary (future-aware, scalar-only now)

Define families for routing clarity:

- `F_SCALAR` (implemented in this proposal),
- `F_COMPOSITE` (placeholder),
- `F_EXT_SCALAR` (placeholder).

Routing rule:

1. classify operand families,
2. if both are `F_SCALAR`, use scalar tables,
3. otherwise return explicit `not_supported`.

This creates an internal seam for future growth without expanding current scope.

## `pow` strategy

`pow` remains special-case math, but should use the same routing framework:

1. normalize through `pow`-specific rules (not forced common-type coercion),
2. dispatch to dedicated handlers (`pow_int_int`, `pow_real_rat`, `pow_irr_complex`, `pow_complex_real`, `pow_complex_complex`),
3. avoid a separate giant cross-product switch.

In other words, `pow` should be table-driven for routing, but not forced into the same symmetric coercion behavior as `+`, `-`, `*`, `/`, `%`.

Detailed staged scope for `n_pow` is tracked in [arithmetic-next-pow-scope](arithmetic-next-pow-scope.md).

## Replacing higher-order rational helpers

Current pattern (`ratOp` + callback-based `ratAddSubOrMod` / `rat_ac_bd` / `ratDiv3`) reduces duplication but obscures intent.

### Option A: Explicit per-operator rational functions

- Keep `ratAdd`, `ratSub`, `ratMul`, `ratDiv`, `ratMod` as direct formula functions with no function-pointer parameters.
- Pros: easiest to read and debug; operator intent is obvious.
- Cons: some duplication returns; consistency must be enforced manually.

### Option B: Strategy struct (named behavior slots)

- Replace raw function pointers with a small struct describing semantics (for example `crossNumeratorOp`, `denominatorMode`, `requiresReciprocal`).
- Pros: keeps shared machinery while making behavior explicit and named.
- Cons: extra abstraction layer; still indirect at runtime.

### Option C: Table entries call named semantic handlers (recommended)

- Keep table-driven dispatch, but route to semantically named rational handlers:
  - `ratAddCross`, `ratSubCross`, `ratMulPairwise`, `ratDivReciprocalMul`, `ratModCross`.
- Shared internals can still exist, but only behind semantically named wrappers.
- Pros: aligns with operator×domain dispatch model, preserves readability, minimizes callback-driven ambiguity.
- Cons: slightly more wrapper functions.

Recommended direction for this refactor: **Option C**.

It keeps the new dispatch architecture coherent and removes the least maintainable part of the current rational callback pattern without forcing a large formula rewrite.

### Naming map and migration sketch

Proposed mapping from current internals to semantically named handlers:

| Current helper | Current role | Proposed wrapper name |
| --- | --- | --- |
| `ratAddSubOrMod(..., intAdd, ...)` | cross-add numerators over common denominator | `ratAddCross` |
| `ratAddSubOrMod(..., intSub, ...)` | cross-sub numerators over common denominator | `ratSubCross` |
| `ratAddSubOrMod(..., intMod, ...)` | cross-mod numerators over common denominator | `ratModCross` |
| `rat_ac_bd(..., intMul, ...)` | pairwise multiply numerator/denominator | `ratMulPairwise` |
| `ratDiv3(..., intMul, ...)` | reciprocal-right then pairwise multiply | `ratDivReciprocalMul` |

Suggested extraction order (low risk):

1. Introduce wrappers with new names that call existing helpers unchanged.
2. Point `ratAdd`/`ratSub`/`ratMul`/`ratDiv`/`ratMod` to new wrappers.
3. Remove direct callback plumbing from call sites (`ratOp` signatures can then simplify).
4. Optionally inline/retire old generic helpers once tests are stable.

This sequence improves readability first, then reduces abstraction complexity without changing behavior.

## Migration plan

### Phase 1

1. Extract normalization/classification into `src/arithmetic_dispatch.c` (+ header).
2. Keep current leaf math functions (`intAdd`, `ratMul`, `comPowCom`, etc.).
3. Add parallel entry points (`n_add`, `n_sub`, `n_mul`, `n_div`, `n_mod`, `n_pow`, `n_cmp`) outside current call paths.
4. Add unit tests for the parallel layer and normalization plans.

### Phase 2

1. Implement table-driven behavior behind the parallel entry points.
2. Keep existing formulas where possible; replace only dispatch plumbing.

### Phase 3

1. Expand parity/regression coverage across legacy and parallel entry points.
2. Keep legacy entry points as production path until confidence threshold is met.
3. Defer final name swap/integration to a separate proposal/change.

## Validation

- Run baseline `make test`.
- Add focused regressions for mixed-type coercion, rational simplification, complex/imag behavior, and `pow` combinations.
- Require no behavior drift in existing scalar arithmetic.
- Keep a dedicated test list of currently aborting arithmetic paths (for example divide/mod by zero cases) so they can be re-opened and converted to soft-NaN behavior in a later phase.

## Why this approach

- Centralizes policy as tables/rules.
- Reduces combinatorial branching in control flow.
- Keeps current behavior while making future domain additions additive rather than invasive.
