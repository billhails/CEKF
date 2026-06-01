# Plan for Typechecking Delimited Continuations

This document expands [delimited-types.md](delimited-types.md) into an implementation roadmap.

The goal is not to settle every typing detail up front. The goal is to define a sequence of steps that can be validated one at a time in the rewrite prototype, then translated into the C typechecker without redesigning the whole approach again.

The recommended order is driven by the real compiler pipeline:

1. Fix the source-level typing model at the shared early typechecking stage, before ANF or target-specific CPS.
2. Carry those guarantees through the rest of the pipeline instead of trying to recover them after lowering.
3. Use the rewrite CPS transform as a semantic cross-check, not as the main typing gate.
4. Keep the rewrite prototype and the C typechecker aligned around that same early model.

That keeps the guarantees available to earlier transforms, works for both the ANF-based CEKF path and the target-b path, and still leaves room to use the rewrite CPS note as an operational oracle.

## Current Starting Point

- The rewrite IR already contains `reset_expr` and `shift_expr` in [expr.fn](../expr.fn) and [minexpr.fn](../minexpr.fn).
- The rewrite inference engine in [infer_expr.fn](../infer_expr.fn) still assumes plain HM types and only has a special case for `callcc_expr`.
- The C typechecker in [src/tc.yaml](../../../src/tc.yaml) and [src/tc_analyze.c](../../../src/tc_analyze.c) likewise models ordinary function types plus a special-case `call/cc` rule.
- In the real compiler pipeline, typechecking is supposed to establish guarantees before later lowering steps such as ANF or any target-specific CPS split. Delimited-control typing therefore has to live at that shared earlier stage.
- More concretely, that early stage is the `expr.fn` / `src/lambda.yaml`-like layer. Desugaring to `minexpr.fn` / `src/minlam.yaml` happens shortly afterward, and most later transforms then run on that minimal core.
- [delimited-types.md](delimited-types.md) defines the direct-style semantic judgment

$$
\Gamma \vdash e : \tau [\alpha \mapsto \beta]
$$

  and the corresponding latent function type carrying answer-type change.

- The rewrite syntax uses `(shift f)` rather than the binder presentation `shift k. e`, so the implementation plan must state how those two views correspond.

## Desired End State

By the end of this work:

- source-level typing can express that an expression has both a value type and an answer-type effect
- named functions can carry latent delimited-control behavior in their function types
- `reset` discharges internal answer-type change at the delimiter boundary
- `shift` captures only the local continuation up to the nearest `reset`
- the implementation uses explicit type structures rather than syntax-local inspection at call sites
- both target-b and the older CEKF/ANF path rely on the same early typing guarantees
- the rewrite prototype and the C typechecker follow the same conceptual model

## Decisions To Freeze Before Coding

These choices should be fixed before implementing source-level typing.

### 1. Keep delimited typing on the shared early pipeline

Delimited-continuation typing cannot be deferred to post-CPS typing.

The reasons are structural:

- earlier transforms need the guarantees that typing provides
- the CEKF/ANF path never passes through target-b CPS
- later lowerings should consume already-typed source terms rather than becoming the place where well-typedness is rediscovered

The rewrite CPS transform is still useful, but only as a secondary semantic cross-check.

### 2. Keep the direct-style note as the semantic source of truth

[delimited-types.md](delimited-types.md) should remain the reference for the typing judgment and the intended meaning of `reset` and `shift`.

The implementation may use different helper functions or a different internal representation, but it should not invent a second semantic story.

### 3. Treat rewrite `shift` syntax as the implementation form of the binder rule

The note presents:

```text
shift k. e
```

The rewrite prototype currently represents:

```text
(shift f)
```

For the first typing pass, treat `(shift f)` as the implementation form of `shift k. e`, where `f` is a pure function that receives the captured continuation.

Recommended first constraint:

$$
f : (\tau \to \alpha) \to \beta
$$

and therefore:

$$
\Gamma \vdash \operatorname{shift}\ f : \tau [\alpha \mapsto \beta]
$$

This keeps the first implementation close to the rule in [delimited-types.md](delimited-types.md) without forcing a new binder form into the rewrite syntax.

### 4. Keep answer-type variables monomorphic at first

Do not attempt full answer-type polymorphism in the first implementation.

For the first cut:

- answer-type variables are non-generalizable by default
- bindings whose types mention answer-type variables remain monomorphic unless there is an explicit annotation story
- recursive controlful bindings should remain restricted until the basic rules are stable

This matches the caution already described in [delimited-types.md](delimited-types.md).

### 5. Use an explicit implementation representation for controlful functions

The theory can present all functions uniformly, but the implementation should make the control effect explicit.

Recommended incremental representation:

- in the rewrite inferencer, add a dedicated controlful function type constructor, or an equivalent `operType` encoding with explicit `answerIn` and `answerOut`
- in the C typechecker, add a corresponding explicit `TcType` variant or generated structure rather than relying on side channels

Pure functions can then be treated as the special case where `answerIn` and `answerOut` unify.

This is easier to inspect, easier to print in errors, and easier to port into generated C structures than an implicit effect discipline.

## Phase 1: Fix the Shared Typing Boundary

**Objective:** place delimited-continuation typing at the same shared early stage as the rest of source-level typechecking.

### Phase 1 Work

- Anchor delimited-continuation typing at the `expr.fn` / `src/lambda.yaml`-like layer, before `desugar.fn` / `src/lambda_desugar.c` lowers into `minexpr.fn` / `src/minlam.yaml`.
- Document which guarantees later passes are allowed to rely on once delimited typing succeeds.
- Decide whether those guarantees need explicit type annotations on the earlier lambda/expr nodes or whether the successful checker plus existing environments are sufficient.
- State explicitly which guarantees the later `minexpr` / `minlam` pipeline inherits after desugaring.
- Treat the rewrite CPS transform as out-of-band validation rather than as the source of truth for typing.

### Phase 1 Rationale

If typing moves later than the shared source pipeline, most earlier transforms lose its guarantees and one of the active backends never sees the check at all.

Putting delimited typing at the shared early stage keeps the architecture consistent with the rest of the compiler.

### Phase 1 Exit Criteria

- there is one documented early typing boundary at the `expr` / `lambda.yaml` layer that both target-b and the CEKF/ANF path share
- later passes can state which delimited-control guarantees they are allowed to assume
- the handoff from `expr` / `lambda.yaml` typing into the `minexpr` / `minlam` core is explicit
- no implementation step depends on post-CPS typing to make source programs well-typed

## Phase 2: Define the Source-Level Implementation Model

**Objective:** choose the internal representation and analyzer shape that will carry answer-type modification through the shared early checker.

Representation choices for this phase are expanded in [delimited-type-representation.md](delimited-type-representation.md).

### Phase 2 Work

- Change the mental model of expression analysis from “an expression returns one type” to “an expression is checked relative to an incoming and outgoing answer type”.
- Keep the expression rule conceptually in this form:

```text
analyzeExpr(expr, env, ng, subst, answerIn, answerOut) -> #(valueType, subst)
```

- Represent pure expressions by unifying `answerIn` with `answerOut`.
- Represent controlful functions explicitly, with fields for:
  - argument type
  - result type
  - pre-call answer type
  - post-call answer type
- Decide how plain functions embed into that representation.
- Decide whether later passes need the inferred control behavior attached to nodes, environments, or both.

### Phase 2 Recommended Approach

Use a distinct controlful function representation in the implementation, even if the semantic note continues to describe pure and controlful functions uniformly.

That preserves incremental adoption:

- existing pure function typing can remain readable
- application can lift pure functions into the identity-effect case when needed
- printed types can make controlful bindings obvious

### Phase 2 Exit Criteria

- there is one agreed implementation shape for controlful function types
- there is one agreed analyzer signature for threading ambient answer types
- the role of `call/cc` inside that model is documented, even if it remains a special case temporarily

## Phase 3: Implement the Direct-Style Rules in the Rewrite Inferencer

**Objective:** add answer-type-modifying typing to [infer_expr.fn](../infer_expr.fn) as a reference implementation of the early source-level rules.

Breadth-first rule skeletons for this phase are expanded in [delimited-typing-rules.md](delimited-typing-rules.md).

### Phase 3 Work

- Extend substitution, occurs checking, instantiation, rendering, and unification so the controlful function representation behaves like any other type constructor.
- Add source rules in evaluation order.

#### Phase 3 Pure Forms

- variables and constants preserve the ambient answer type
- arithmetic, tuples, constructors, and other transparent forms only thread answer-type change from their subexpressions
- sequencing forms preserve left-to-right answer-type threading

#### Phase 3 Lambda

- analyze the body under fresh answer-type variables
- store the latent answer-type change on the resulting function type
- keep lambda construction itself pure

#### Phase 3 Application

- thread intermediate answer types through:
  - evaluation of the function expression
  - evaluation of the argument expressions
  - the latent answer-type behavior of the callee body
- make pure application the special case where the callee preserves the answer type

#### Phase 3 Reset

- introduce a fresh local delimiter boundary
- analyze the body with a fresh local answer-type pair
- discharge the internal answer-type change before returning to the outer context

#### Phase 3 Shift

- implement the rule for rewrite `(shift f)`, not only the binder presentation from the note
- require the operand to type as a pure function from the captured continuation to the body result in the first cut
- only relax that restriction after the core machinery is stable

### Phase 3 Exit Criteria

- the small examples from [delimited-types.md](delimited-types.md) infer the expected result types
- a named helper containing `shift` visibly carries latent control behavior in its inferred type
- the checker rejects using such a helper without an enclosing `reset`

## Phase 4: Add a Safe Generalization Policy

**Objective:** prevent unsound answer-type polymorphism while keeping the first implementation usable.

### Phase 4 Work

- treat answer-type variables as non-generic in `let` and `letrec`
- keep bindings with latent answer-type change monomorphic unless annotations are added intentionally
- document this as a design restriction, not a temporary accident
- decide whether recursive controlful bindings require stricter rules than non-recursive ones

### Phase 4 Exit Criteria

- intended monomorphic control examples typecheck
- deliberately polymorphic answer-type examples are rejected for an explicit reason
- the restriction is documented next to the implementation rather than hidden in behavior

## Phase 5: Reconcile `shift/reset` with Existing `call/cc` Support

**Objective:** avoid ending with two unrelated control-typing mechanisms.

### Phase 5 Work

- compare the current special-case `callcc_expr` rule in [infer_expr.fn](../infer_expr.fn) with the new delimited-control representation
- decide whether `call/cc` should:
  - remain a small special case, or
  - be re-expressed using the same underlying type machinery where practical
- document the exact difference between whole-continuation capture and delimited capture in implementation terms

### Phase 5 Exit Criteria

- the relationship between `call/cc` and `shift/reset` is explicit
- any remaining special cases are small and intentional

## Phase 6: Port the Model to the C Typechecker

**Objective:** translate the validated rewrite design into the real compiler's existing early typechecking stage.

### Phase 6 Work

- extend [src/tc.yaml](../../../src/tc.yaml) with an explicit representation for controlful function types
- update [src/tc_analyze.c](../../../src/tc_analyze.c) so expression analysis threads answer-type information through the same control points as the rewrite prototype, while the program is still in the `src/lambda.yaml`-like layer and before later lowering to `src/minlam.yaml`, ANF, or any target-specific CPS path
- preserve the same implementation order used in the prototype:
  - transparent pure forms first
  - lambda and application threading
  - `reset`
  - `shift`
  - let-generalization restrictions after the core rules work
- keep this early checker authoritative rather than introducing a second later typing pass as the source of truth
- add helper constructors and helper analyzers for:
  - pure vs controlful function creation
  - application threading
  - delimiter discharge
  - answer-type mismatch diagnostics

### Phase 6 Practical Mapping

The current C implementation already has a useful comparison point in the `call/cc` path inside [src/tc_analyze.c](../../../src/tc_analyze.c).

Use that as a regression oracle, but do not let the new design depend on syntax-local inspection of lambda bodies at call sites. The control behavior must travel in the type representation.

### Phase 6 Exit Criteria

- the C typechecker has an explicit representation for latent delimited-control behavior
- the C analyzer no longer depends on local syntax to know whether a named function may alter the answer type
- rewrite examples have corresponding C-side tests once the front-end can construct the relevant AST

## Phase 7: Validation Matrix

**Objective:** make the typing discipline executable and regression-resistant.

### Phase 7 Positive Cases

- immediate resumption through the captured continuation
- discarding the captured continuation
- reusing the captured continuation multiple times
- named helper containing `shift` under `reset`
- nested `reset` cases where only the nearest delimiter is captured

### Phase 7 Negative Cases

- `shift` without an enclosing `reset`
- unsafe let-generalization of a controlful binding
- answer-type mismatch across branches or sequence points
- misuse of a controlful helper in a context that expects a pure function result

### Phase 7 Two Validation Layers

Run validation at both levels:

1. source-level terms should typecheck with the answer-type-modifying rules at the shared early checker
2. where the rewrite CPS transform exists, transformed terms may still be checked with ordinary HM as a secondary oracle

This separates source-level typing mistakes from operational rewrite mistakes without making CPS a prerequisite for sound typing.

## Non-Goals for the First Implementation

The first implementation should not try to solve these at the same time:

- full answer-type polymorphism
- prompt tags
- `control/prompt`
- one-shot continuation restrictions
- combining `amb` and delimited control in one typing change

## Recommended Order of Attack

1. Fix the shared early typing boundary before ANF or any target-specific CPS split.
2. Freeze the internal representation for controlful function types and ambient answer-type threading.
3. Implement the direct-style rules in [infer_expr.fn](../infer_expr.fn) as a reference model for the early checker.
4. Add safe monomorphic restrictions before attempting richer inference.
5. Reconcile the new model with the existing `call/cc` support.
6. Port the validated design into [src/tc.yaml](../../../src/tc.yaml) and [src/tc_analyze.c](../../../src/tc_analyze.c) at the existing early `lambda.yaml`-level typechecking stage, before desugaring to `minlam`.
7. Keep the rewrite CPS transform available as a secondary semantic oracle, not as the place where typing first becomes valid.

This order keeps the operational model and the typing model aligned while preserving the guarantees needed by the rest of the real pipeline.
