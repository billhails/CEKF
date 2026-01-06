# RFC-001: Function Over-Application

Status: Implemented (branch: `over-application-hybrid`)

Owner: billhails

Date: 2025-11-04

Target branch: `over-application-hybrid`

## Summary

Add first-class support for function over-application: applying a function to more
arguments than its declared arity. Semantics: treat over-application as
left-associative chaining of unary applications. If the intermediate result
is a function, keep applying remaining arguments; otherwise, raise a precise
arity error at the boundary where a non-function is applied.

This RFC proposes implementing the behavior primarily at compile time by
splitting known over-application sites into a sequence of calls the VM already
supports. No change to surface syntax, evaluation order, or existing
curried/partial application behavior. The VM can optionally improve error
messages in the over-application branches but does not need structural changes.

## Motivation

- The type checker already expresses multi-argument functions as curried arrows
  (e.g., `int -> int -> int -> int`), so over-application is semantically natural.
- Today the VM rejects over-application with a hard assertion
  (see `step.c` branches that say "over-application not supported yet").
- Enabling over-application improves ergonomics for higher-order code and aligns
  with user expectations from other curried functional languages.

## Goals and non-goals

Goals

- Preserve current evaluation order and calling conventions.
- Support over-application without VM control-flow surgery.
- Provide clear static or dynamic errors when an intermediate result is not a function.

Non-goals (for this RFC)

- Currying for builtins (currently unsupported; keep behavior unchanged).
- Global transformation to unary application for every call site (we’ll use a
  targeted compile-time split where it’s safe and beneficial).

## User-facing semantics

Given an application `f(a1, a2, ..., an)` where `f` has arity `m`:

- Under-application (n < m): return a partial closure awaiting `m - n` args
  (current behavior).
- Exact application (n = m): perform the call (current behavior).
- Over-application (n > m):
  - Apply the first `m` arguments to `f` to get result `r`.
  - If `r` is a function, apply the next argument to `r`, producing a new result,
    and continue left-to-right until all extra args are consumed.
  - If `r` is not a function at any step and extra args remain, raise an arity
    error at that boundary.

Clarification: The “one at a time” chaining of the extra arguments (i.e., emitting
`APPLY 1` steps for each remaining argument) is to maintain the VM’s invariant that
we never pass more arguments than a closure’s pending arity. After the first exact
application, the result’s arity is not represented in the stack layout, so batching
the extras into another multi-arg `APPLY k` could again exceed the next closure’s
pending arity. Chaining `APPLY 1` avoids hitting the VM’s over-application path and
pinpoints any non-function application exactly where it occurs.

On typing: In a well-typed HM program, over-application that would attempt to apply
an already non-function value is rejected at compile time. The runtime arity check
is a defensive measure for cases not fully resolved by types (e.g., intentionally
unsafe constructs or future extensions), but we do not expect it to trigger for
well-typed code.

Evaluation order

- Preserve existing order: arguments evaluated left-to-right, then the function
  expression, just as today.
- The chaining semantics do not change precedence or associativity; they are
  defined in terms of repeated applications that the backend emits.

Examples

- `let f = fn(x, y) { fn (z) { x + y + z } }` has type
  `int -> int -> int -> int`. Then `f(1, 2, 3)` evaluates to `6`.
  This is an example of valid over-application relative to the declared arity `m=2`
  because the fully-applied result (after 2 args) is itself a function.

Additional examples

- If `g: int -> int -> (int -> int)`, then `g(1, 2, 3)` is valid and returns an `int`.
- Calling `g(1, 2, 3, 4)` is a type error: after the third argument the result is an
  `int`, so a fourth argument cannot be applied.
- If `h: int -> (int -> (int -> int))`, then `h(1, 2, 3)` is valid; `h(1, 2, 3, 4)` is a
  type error for the same reason (the result after three arguments is `int`).

## Typing rules (HM)

- Functions are viewed as curried: `T1 -> T2 -> ... -> Tm -> R`.
- Application type rule is iterated per argument, unifying the result type at
  each step. Over-application corresponds to more iterations.
- If the type checker can prove that after `k` applications the result type is
  not a function but arguments remain, report a compile-time error at that
  boundary.
- If polymorphism/unknowns prevent a static decision, allow the program and
  defer to runtime; at runtime, attempting to apply a non-function raises an
  arity error at that application site.

Note: Application constrains the intermediate to a function type at each step.
Therefore, in a standard HM setting we do not expect runtime arity errors in
well-typed programs; they are retained as a defensive backstop.

This preserves principal types and requires no new core type features.

## Builtins and operators

- Builtins remain non-curried as today.
- Over-application of builtins is not supported in this RFC (unchanged);
  attempting to supply extra arguments to a builtin is still an error.
- User-defined operators desugar to function applications; they follow the same
  over-application semantics as functions.

## Design: where to implement

Initial design targeted compile-time only splitting. Final implementation is a hybrid of:

1. Compile-time splitting for syntactically direct lambdas with known arity (in `writeCexpApply`). If `n > m` we emit `APPLY m` followed by `n-m` chained `APPLY 1`.
2. Runtime staging for indirect / variable-held call sites: a single `APPLY n` may over-apply a closure whose pending arity is only known at runtime. The VM now catches `naargs > pending`, performs an exact application with the first `pending` args, then stages the remaining extras and applies them one at a time after each `RETURN`.

The VM invariants (never executing an `APPLY k` with `k > pending`) are preserved; compile-time splitting avoids triggering the runtime path at direct sites, while the staging mechanism extends support to all other sites.

Why this preserves evaluation order

- Current order is "evaluate args left-to-right, then evaluate function".
- We keep that exact evaluation schedule. The only change is how we break the
  application into VM steps, not the order expressions are evaluated.

## Stack discipline with APPLY

Contract (existing VM invariants):

- Before an `APPLY k`, the stack holds the `k` arguments (older at lower
  addresses) and then the callable on top (TOS). `applyProc(k)` pops the
  callable, inspects `k` and the closure’s pending arity, and either executes
  the body (exact), produces a partial closure (under), or errors (over).

Over-application compilation strategy:

- Ensure the first `APPLY` never has `k > pending`. That is achieved by choosing
  `k = m` (the callee’s arity) when `n > m`.
- After that first `APPLY m`, if the result is a function of arity at least 1,
  a subsequent `APPLY 1` will be valid. If it is not a function, the VM will
  signal a non-function application at that point.

## VM behavior (runtime staging mechanics)

`applyProc` now distinguishes:

- Under-application: create partial closure (unchanged).
- Exact application: execute body (unchanged, with counters).
- Over-application: push an `OverApplyFrame` (auto-generated from `cekf.yaml` as part of an `OverApplicationStack`) capturing extra arguments. Perform exact call immediately; mark frame `ready` on `RETURN`; a post-instruction loop issues `APPLY 1` for the next staged argument when `ready` and the result is callable. Supports arbitrary nesting (one frame per active over-application), removing the prior fixed depth constant.

Builtins remain exact-only; over-application still errors for them.

## Compiler changes (precise)

File: `src/bytecode.c` (`writeCexpApply`):

- Detect direct lambda (`AEXP_TYPE_LAM`). If `n>m`, emit `APPLY m` + `n-m` times `APPLY 1`.
- Else emit single `APPLY n`; VM staging handles any over-application.

Future extension: simple arity propagation to treat some let-bound lambdas as direct.

## Pipeline impact & invariants

Affected stages:

- Pratt / Parsing: No syntactic change; applications unchanged.
- Lambda Conversion / ANF: Unmodified; over-application handled after ANF (arity known for direct lambdas).
- Type Checking: Already curried; no new rules, just longer application chains.
- Bytecode Compiler: Adds compile-time split for direct lambdas (`APPLY m` then chained `APPLY 1`). Emits a single `APPLY n` for indirect sites.
- Runtime VM (`step.c`): Introduces staging path in `applyProc` plus post-instruction loop and generated `OverApplicationStack` for nested frames.
- Code Generation (`cekf.yaml`): Now owns the over-application frame/stack types; removes hard-coded max depth.

Core invariants preserved:

1. Never call `applyProc(k)` with `k > pending` for a closure (compile-time split or runtime staging ensures this).
2. Stack layout before each `APPLY`: arguments below, callable on top.
3. Staged extras applied only after a completed RETURN (body evaluated) ensuring result is a stable callable.
4. Builtins remain exact-only (no currying/over-application path).
5. Nested over-application frames pop in LIFO order; each frame’s vector is GC-protected until completion.

Risks / mitigations:

- Potential extra dispatch overhead from post-instruction loop: guarded by `overApplyDepth` check (cheap integer test).
- Memory growth from many nested frames: dynamic vectors are freed promptly on frame completion.
- Error clarity: non-callable staging boundary produces precise arity error with location; avoids silent failure.

No changes required in TPMC or macro expansion; operator definitions lower to ordinary applications and inherit semantics automatically.

## Error reporting & instrumentation

Static

- When type inference proves that after `k` applications the result cannot be a
  function while extra arguments remain, report a compile-time error at the
  application site of arg `k+1`.

Dynamic

- When an `APPLY 1` targets a non-function value (e.g., number), raise an arity
  error at that application with source location from the last `writeLocation`.
- Optional (nice-to-have): replace the `cant_happen` over-application branches
  in `step.c` with an explicit arity error; this helps end users if a non-split
  site still over-applies.

## Interaction with other features

- ANF: unchanged. ANF already reduces complex applications to Aexp/Cexp with
  arguments available for the bytecode writer.
- `amb`/failure continuations: no change. The chain of `APPLY`s composes with
  continuations as usual.
- Tail calls: no change in semantics. The first `APPLY m` remains as before; the
  subsequent `APPLY 1` steps may or may not be in tail position depending on the
  source program.
- Namespaces/operators/macros: unchanged; they lower to applications that follow
  the same rules.

## Backward compatibility

- Existing programs continue to work as before.
- Programs that previously failed with over-application of direct lambdas will
  now succeed (or raise a dynamic non-function application error at the precise
  boundary if the intermediate is not a function).
- Builtins continue to disallow currying/over-application.

## Testing plan

Tests:

- `tests/fn/test_over_application.fn` — direct lambda over-application; multiple extras; under-application; builtin rejection.
- `tests/fn/test_over_application_nested.fn` — nested over-application using stacked frames; partial then staged cases.
- Static typing error (typechecker) — where the intermediate is provably non-function, compile-time error at the boundary.
- Dynamic error — where runtime produces a non-function value mid-chain, precise arity error at the first invalid `APPLY`.

## Implementation steps

1) Compile-time split for direct lambdas.
   - Update `writeCexpApply` to detect direct-lambda callees and split `APPLY` as
     described.
   - Ensure source locations (`writeLocation`) precede each emitted `APPLY`
     involved in chaining for accurate error reporting.

2) Runtime staging stack (dynamic, generated) with nested frames.
   - In `step.c`, change the two `cant_happen("over-application not supported yet, ...")`
     branches to a user-facing arity error (keeping SAFETY_CHECKS semantics).
   - Improve the default-case message for `APPLY` on non-function values.

3) Tests (direct + nested + regressions).
   - Add language tests under `tests/fn/` to exercise the above cases.

4) (Optional) Let-bound lambda arity propagation.
   - Track arity across let-bound lambdas to split more call sites at compile
     time without VM changes.

## Implementation & test plan (retrospective)

Incremental rollout followed this order:

1. RFC drafting and semantic agreement (curried chaining, left-to-right).
2. Compile-time split for direct lambdas to eliminate immediate VM over-calls.
3. Runtime staging for indirect over-application (single-site transparent fallback).
4. Instrumentation (exact calls, partial creations, staged steps, nested frames) for profiling.
5. Nested support by replacing fixed array with generated dynamic stack.
6. Test suite expansion: direct, indirect, nested, error boundaries; regression confirmation for existing arithmetic and pattern matching tests.

No feature flag retained—behavior is strictly additive and preserves evaluation order.

Future test additions (optional): stress tests with deep nesting and mixed partial/over sequences; performance benchmarks comparing staged vs. direct split frequencies.

## _cmp mismatch investigation (deferred)

Rare earlier reports of `_cmp` mismatches under SAFETY_CHECKS were linked to incorrect stack state during experimental over-application attempts. Current staging preserves stack invariants; any recurrence should add temporary logging around the first `APPLY m` and subsequent staged `APPLY 1` to capture value types before comparison. (Out of scope for this RFC’s completed implementation.)

## Open questions

- Do we want a feature flag to enable the split initially (e.g., a compiler
  option) or ship it by default? Recommended: default on; behavior is a strict
  improvement and preserves evaluation order.
- Exact wording and category of runtime errors: keep consistent with existing
  error taxonomy (e.g., ArityError vs TypeError), and ensure they are included
  in `--assertions-accumulate` runs.
- Should we support currying or over-application for some builtins in the
  future? Out of scope here.

Note: While out of scope, adding lightweight wrappers around builtins would enable
currying/over-application for selected builtins without changing VM internals.

## Alternatives considered

- VM-level implementation of over-application by introducing a continuation
  form that carries remaining args. Rejected for complexity and GC/dispatch
  impact; compile-time split is simpler and sufficient.
- Global unary-application lowering. Rejected for performance and because it
  changes evaluation schedule unless carefully re-ordered; targeted split is
  adequate.

## Appendix: illustrative bytecode sketch (conceptual)

For `f(a,b,c)` where `f` is a direct lambda with `m = 2`:

- Before (today):
  - emit a; emit b; emit c; emit f; `APPLY 3` → VM asserts (over-application).

- After (this RFC):
  - emit a; emit b; emit c; emit f; `APPLY 2`; `APPLY 1`.
  - If `(f a b)` returns a function, the second `APPLY 1` applies `c`.
  - If not, we error exactly at the second `APPLY`.
