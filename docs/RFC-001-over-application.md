# RFC-001: Function Over-Application

Status: Draft

Owner: billhails

Date: 2025-11-04

Target branch: `over-application`

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

VM-only change is possible but invasive (would need a continuation shape or
runtime stub to carry "remaining args"). Instead, implement at compile time in
bytecode generation:

- In `writeCexpApply`, detect when the callee has a known arity at compile time
  (e.g., direct lambda). If number of supplied args `n` exceeds arity `m`,
  emit code that:
  1) Applies the first `m` arguments in one `APPLY m` (exact application), and
  2) For each of the remaining `n - m` arguments, emits a subsequent `APPLY 1`.

This sequence ensures that at no point do we pass `naargs > pending` into the VM.
If an intermediate result is not a function, the subsequent `APPLY 1` fails at
exactly the right boundary with a clear error.

For callees whose arity is unknown at compile time (e.g., variable of function
 type without local arity knowledge), continue to emit a single `APPLY n`.
Those sites retain current behavior; over-application there will still rely on a
later compiler pass to expose arity or remain a runtime error if unsupported.

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

## VM behavior (no structural changes required)

- The VM’s `applyProc` implementation already supports exact- and
  under-application for `CLO` and `PCLO`.
- The VM currently asserts on over-application for closures; with the compile-time
  split, those branches are not exercised at over-application sites we rewrite.
- Optional improvement (non-blocking): change those assertion messages to a
  user-facing arity error when they do trigger, and clarify the message for the
  non-function `APPLY` case. This RFC does not require changing VM behavior to
  land the feature.

## Compiler changes (precise)

File: `src/bytecode.c`
- Function: `writeCexpApply(CexpApply *x, ...)`

Algorithm:
- Let `n = countAexpList(x->args)`.
- If `x->function` is syntactically a lambda in ANF (`AEXP_TYPE_LAM`), its arity
  `m` is known (and emitted by `BYTECODES_TYPE_LAM`).
  - If `n <= m`: emit current code (single `APPLY n`).
  - If `n > m`: emit args (unchanged), emit function (unchanged), emit
    `APPLY m`, then for each of the remaining `n - m` args (which are already on
    the stack in order), emit `APPLY 1`.
- Else (callee not a direct lambda): emit current code (single `APPLY n`).

Instrumentation (optional, behind a debug flag): Count and report the proportion of
direct-lambda call sites (where we split) versus indirect call sites (single `APPLY n`).
This helps evaluate any performance impact of additional `APPLY 1` steps in practice.

Note: The stack layout produced by the current writer already matches the VM’s
`applyProc` expectations: arguments first (left-to-right), function last.
Splitting as above preserves that contract.

Future extension (optional):
- Propagate known arities for let-bound lambdas through simple analysis, so more
  indirect call sites can be split at compile time without changing ANF.

## Error reporting

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

Add `tests/fn/test_over_application.fn` with cases:
- Direct lambda over-application: `f(1,2,3)` returns 6 per example above.
- Multiple extras: `fn(a,b){ fn(c){ fn(d){ a+b+c+d }}}(1,2,3,4) == 10`.
- Static error: a function known to return non-function after saturation but
  called with an extra arg should be rejected by the type checker (assert on
  error message/position).
- Runtime error: a higher-order function returns a non-function at runtime;
  applying extra arg raises an arity error at that site.
- Under-application: unchanged behavior producing partial closures.
- Builtin: over-application remains an error.

## Implementation steps

1) Bytecode split (minimal):
   - Update `writeCexpApply` to detect direct-lambda callees and split `APPLY` as
     described.
   - Ensure source locations (`writeLocation`) precede each emitted `APPLY`
     involved in chaining for accurate error reporting.

2) Friendly runtime errors (optional):
   - In `step.c`, change the two `cant_happen("over-application not supported yet, ...")`
     branches to a user-facing arity error (keeping SAFETY_CHECKS semantics).
   - Improve the default-case message for `APPLY` on non-function values.

3) Tests:
   - Add language tests under `tests/fn/` to exercise the above cases.

4) (Optional) Small arity analysis:
   - Track arity across let-bound lambdas to split more call sites at compile
     time without VM changes.

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
