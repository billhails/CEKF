# Implementation Checklist for `infer_expr.fn`

This note turns [delimited-typing-rules.md](delimited-typing-rules.md) into a file-local checklist against [infer_expr.fn](../infer_expr.fn).

The goal is not to restate the full design. The goal is to make it obvious, from the structure of `infer_expr.fn` itself, what needs to change and in what order.

This checklist is intentionally scoped to one file. It assumes the representation choice from [delimited-type-representation.md](delimited-type-representation.md) and the breadth-first analyzer plan from [delimited-typing-rules.md](delimited-typing-rules.md).

## Current File Shape

`infer_expr.fn` currently has five broad regions:

1. `typeExp` plus small type constructors such as `tupleType`, `thunkType`, and `funType`
2. generic type operations such as substitution, occurs check, unification, instantiation, rendering, and canonicalization
3. support predicates and expression-list helpers
4. environment, letrec, constructor, and primitive-analysis helpers
5. the main `analyzeExpr` dispatcher plus `infer`, `infer_string`, and `infer_to_string`

The safest implementation order is to preserve that structure while making each region delimited-control aware.

## Working Rule for This Checklist

When this note says a function should be “threaded”, it means the function should be updated to carry:

```text
answerIn, answerOut, controlAllowed
```

either directly in its signature or indirectly via a narrower helper that preserves the same information.

## Section A: Type Representation Block

This section covers the top of the file through the basic type constructors.

### `typeExp`

- [ ] Replace the current callable encoding inside `operType` with explicit callable constructors.
- [ ] Keep `varType` unchanged.
- [ ] Keep `operType` for non-callable named constructors such as `Int`, `Bool`, `Null`, `list`, and `Tuple`.
- [ ] Add explicit constructors for pure and controlful thunks.
- [ ] Add explicit constructors for pure and controlful unary functions.

### Small type constructor helpers

- [ ] Keep `intType`, `boolType`, `charType`, `stringType`, `nullValueType`, and `tupleType` as ordinary named-type helpers.
- [ ] Rewrite `thunkType` and `funType` to construct explicit callable nodes rather than `operType("Thunk", ...)` and `operType("->", ...)`.
- [ ] Keep `funTypeList`, but make it build explicit nested callable nodes.
- [ ] Add `controlThunkType` helper.
- [ ] Add `controlFunType` helper.
- [ ] Add a helper that normalizes `controlThunkType(R, A, A)` to `thunkType(R)`.
- [ ] Add a helper that normalizes `controlFunType(X, R, A, A)` to `funType(X, R)`.
- [ ] Add a helper for building curried callables whose rightmost callable node is controlful.

### Fresh variables and names

- [ ] Keep `freshTypeVar` unchanged.
- [ ] Keep `typeVarName` unchanged.
- [ ] Do not introduce a second fresh-variable source specifically for answer types.

## Section B: Generic Type Operations

This section covers the functions that walk or compare `typeExp` structurally.

### Substitution

- [ ] Update `applySubst` to recurse through each new callable constructor explicitly.
- [ ] Keep `applySubstList` for `operType` argument lists.
- [ ] Add any callable-specific child helpers only if the pattern matching becomes too repetitive.

### Occurs check

- [ ] Update `occursInType` to recurse through explicit thunk and function constructors.
- [ ] Keep `occursInTypeList` for `operType` argument lists.

### Unification

- [ ] Extend `unifyType` with structural cases for:
  - pure thunk with pure thunk
  - pure function with pure function
  - controlful thunk with controlful thunk
  - controlful function with controlful function
- [ ] Keep `operType` unification for ordinary named constructors.
- [ ] Do not add implicit coercions between pure callables and controlful callables here.
- [ ] Keep purity normalization outside the unifier.
- [ ] Preserve the current occurs-check behavior for type variables.
- [ ] Keep `unifyArgs` for `operType` argument lists.

### Instantiation and freshness

- [ ] Update `instantiateApplied` to recurse structurally through the new callable constructors.
- [ ] Keep `instantiateArgs` for `operType` argument lists.
- [ ] Keep `freshType` as the environment-instantiation entrypoint.
- [ ] Make sure freshening preserves controlful callables instead of flattening them back into generic operators.

### Rendering and canonicalization

- [ ] Update `renderType` with explicit cases for pure and controlful thunks.
- [ ] Update `renderType` with explicit cases for pure and controlful functions.
- [ ] Update `renderDomType` so controlful callable domains print with parentheses when needed.
- [ ] Keep readable output for pure types unchanged where possible.
- [ ] Update `canonicalizeType` to recurse structurally through the new callable constructors.
- [ ] Keep `canonicalizeArgs` for `operType` argument lists.
- [ ] Verify that canonicalization plus rendering still produce stable letrec snapshots.

## Section C: Support Predicates and Front-Door Gating

This section covers the `supports*` family.

### `supports`

- [ ] Add explicit support handling for `reset_expr` once the branch is implemented.
- [ ] Add explicit support handling for `shift_expr` once the branch is implemented.
- [ ] Decide whether unsupported delimited-control expressions should return `false` here before their typing branches land, rather than falling through implicitly.
- [ ] Keep `make_vec` unsupported.
- [ ] Keep the current primitive allowlist until delimited typing is stable.

### `supportsList`, `supportsNonEmptyList`, `supportsBindings`, `supportsCondCases`, `supportsCondCasesTail`, `supportsMatchCases`, `supportsMatchCasesTail`

- [ ] Update only as needed for `reset_expr` and `shift_expr` propagation.
- [ ] Do not redesign these helpers unless the analyzer rollout exposes a concrete gap.

## Section D: New Analyzer-Level Helper Layer

This section is new work rather than migration of existing helpers.

### Analyzer entry helpers to add

- [ ] Add an effect-aware `analyzeExpr` signature carrying `answerIn`, `answerOut`, and `controlAllowed`.
- [ ] Add a small wrapper for pure ambient contexts, if that keeps branches readable.
- [ ] Add a helper that creates fresh local answer-type pairs for lambda bodies and reset bodies.

### Callable builder helpers to add

- [ ] Add `makeLambdaCallable` for zero-argument lambdas.
- [ ] Add `makeLambdaCallableChain` for curried lambdas.
- [ ] Make these helpers responsible for purity normalization after substitution has been applied.

### Callable consumer helpers to add

- [ ] Add `consumeThunkCallable`.
- [ ] Add `consumeCurriedCallable`.
- [ ] Keep callable-consumption logic out of `unifyType`.
- [ ] Let these helpers enforce `controlAllowed` when a controlful callable is actually invoked.
- [ ] Let these helpers instantiate type variables to controlful callable skeletons when needed.

### Argument threading helpers to add or replace

- [ ] Replace the current `analyzeArgs` with an effect-threaded version.
- [ ] Thread answer types left-to-right through argument evaluation.
- [ ] Preserve current source evaluation order.

## Section E: Existing Mid-Level Helper Migration

This section covers the helpers below the support predicates and above the `analyzeExpr` dispatcher.

### `analyzeArgs`

- [ ] Expand the signature to thread `answerIn`, `answerOut`, and `controlAllowed`.
- [ ] Preserve left-to-right evaluation.
- [ ] Return the argument type list plus the updated substitution as it does now.

### `analyzeCondCases` and `analyzeMatchCases`

- [ ] Expand both helpers to thread answer types explicitly.
- [ ] Ensure each branch starts from the same post-test answer state.
- [ ] Keep branch result-type unification logic unchanged in spirit.

### `bindParams`

- [ ] Keep parameter binding structurally unchanged.
- [ ] Do not add answer-type variables here; those belong to lambda-body analysis, not parameter binding.

### `analyzeLetBindings`

- [ ] Expand the helper to thread answer types left-to-right through binding expression evaluation.
- [ ] Keep the environment-extension result unchanged in shape.

### `extendTypeEnv`

- [ ] No semantic change expected.
- [ ] Only touch if explicit callable constructors require a small representation update.

### `analyzeSequence`

- [ ] Expand the helper to thread answer types left-to-right.
- [ ] Preserve “last expression determines result type”.

### Letrec helpers

This group includes:

- `bindRecBindings`
- `finalizeRecBindings`
- `snapshotRecBindings`
- `snapshotRecBindingsAcc`
- `processRecBinding`
- `checkRecBindings`
- `processFirstRecBindings`
- `checkRecLambdaBindings`
- `iterateRecBindings`

Checklist:

- [ ] Keep the broad letrec strategy unchanged for the first pass.
- [ ] Update recursive binder snapshots so controlful callables print deterministically.
- [ ] Expand `processRecBinding` so lambda bodies are analyzed through the new effect-aware analyzer.
- [ ] Keep answer-type generalization policy out of this first migration.
- [ ] Defer any letrec soundness redesign beyond what the new representation and analyzer interface force.

### Primitive and constructor helpers

This group includes:

- `analyzePrimitive`
- `analyzeConstruct`
- `ctorTypeToTypeExp`
- `ctorTypeArgList`
- `spineArg`
- `spineResult`

Checklist:

- [ ] Expand `analyzePrimitive` to thread answer types through left and right operands.
- [ ] Keep the primitive result-type rules unchanged.
- [ ] Expand `analyzeConstruct` only through the new application path.
- [ ] Update `ctorTypeToTypeExp` so constructor arrows become explicit pure `funType` nodes.
- [ ] Update `spineArg` to match explicit `funType` instead of `operType("->", ...)`.
- [ ] Update `spineResult` to walk explicit `funType` nodes.
- [ ] Decide explicitly whether constructor types should remain always pure in the first cut.

## Section F: `analyzeExpr` Branch Checklist

This section maps each current branch family to the intended migration work.

## Atomic and wrapper branches

This group includes:

- `E.var`
- `E.bigint`
- `E.stdint`
- `E.character`
- constant branches
- `E.cut_expr`
- `E.typeof_expr`
- `E.print_exp`
- `E.tag`

Checklist:

- [ ] Make atomic value branches unify `answerIn` with `answerOut`.
- [ ] Keep their direct result types unchanged.
- [ ] Make wrapper branches preserve their child expression's answer threading rather than silently forcing purity.

## Lambda branches

This group includes:

- `E.lambda([], body)`
- `E.lambda(params, body)`

Checklist:

- [ ] Analyze lambda bodies under fresh local answer types.
- [ ] Analyze lambda bodies with `controlAllowed = true`.
- [ ] Keep lambda construction itself pure in the surrounding context.
- [ ] Route the result through the new callable-builder helpers.
- [ ] Normalize pure latent effects back to pure callable types when possible.

## Application branches

This group includes:

- `E.apply(fun, [])`
- `E.apply(fun, args)`

Checklist:

- [ ] Decompose application into function evaluation, argument evaluation, and callable consumption.
- [ ] Thread answer types through function evaluation.
- [ ] Thread answer types through argument evaluation in source order.
- [ ] Route zero-argument calls through `consumeThunkCallable`.
- [ ] Route non-empty applications through `consumeCurriedCallable`.
- [ ] Reject controlful callable invocation when `controlAllowed = false`.

## Control-related existing branch

This group includes:

- `E.callcc_expr`

Checklist:

- [ ] Keep this branch special-cased for the first pass.
- [ ] Update it only enough to use explicit pure `funType` construction.
- [ ] Do not force `call/cc` into the new delimited-control rule set yet.

## Branching expressions

This group includes:

- `E.if_expr`
- `E.cond_expr`
- `E.match_cases`

Checklist:

- [ ] Evaluate the test or scrutinee first and thread the answer state through it.
- [ ] Start each branch from the same post-test answer state.
- [ ] Make both branches or all cases end at the same final `answerOut`.
- [ ] Keep existing branch result-type unification logic.

## Binding expressions

This group includes:

- `E.let_expr`
- `E.letrec_expr`
- `E.letstar_expr`

Checklist:

- [ ] Thread answer types through binding evaluation before entering the body.
- [ ] Keep environment extension logic structurally unchanged.
- [ ] Keep the letrec fixpoint loop intact unless the new analyzer interface forces a small local rewrite.

## Data and constructor expressions

This group includes:

- `E.construct`
- `E.deconstruct`
- `E.make_tuple`
- `E.make_vec`
- `E.primapp`
- `E.tuple_index`
- `E.typedefs`

Checklist:

- [ ] Thread answer types through any evaluated subexpressions.
- [ ] Keep the final constructor or tuple shape itself pure.
- [ ] Keep `make_vec` unsupported.
- [ ] Keep typedef environment extension behavior unchanged.

## Sequence and nondeterminism-adjacent expressions

This group includes:

- `E.sequence`
- `E.amb_expr`
- `E.back_expr`

Checklist:

- [ ] Expand `sequence` to explicit left-to-right answer threading.
- [ ] Decide whether `amb_expr` should simply thread both sides through the new analyzer without deeper redesign in this pass.
- [ ] Decide whether `back_expr` remains a placeholder returning a fresh type variable for now.
- [ ] Keep these decisions explicit rather than letting them drift as accidental behavior.

## New branches to add

This group includes:

- `E.reset_expr`
- `E.shift_expr`

Checklist:

- [ ] Add `E.reset_expr` only after the general analyzer threading is in place.
- [ ] Analyze `reset` bodies under fresh local answer types and `controlAllowed = true`.
- [ ] Make `reset` pure to the outside by unifying the surrounding `answerIn` with `answerOut`.
- [ ] Return the body's outgoing answer type as the direct result type of `reset`.
- [ ] Add `E.shift_expr` only after callable builders and consumers are stable.
- [ ] Require `controlAllowed = true` in `shift`.
- [ ] Check `shift` operands against the rewrite function-argument form described in the rules note.
- [ ] Make `shift` return the captured hole type, not the shift-body result type.
- [ ] Use the ambient answer-type change of the `shift` expression itself to carry the captured answer to shift-body result transition.

## Section G: Entry Points and Public Surface

This section covers the bottom of the file.

### `infer`

- [ ] Make `infer` create one fresh top-level answer variable.
- [ ] Call the effect-aware `analyzeExpr` with identical `answerIn` and `answerOut` at top level.
- [ ] Set top-level `controlAllowed = false`.
- [ ] Normalize the final type after substitution so pure callables do not print as controlful callables with identical answer types.

### `infer_string` and `infer_to_string`

- [ ] Keep both entrypoints structurally unchanged.
- [ ] Let them inherit the new normalized output through `infer` and `renderType`.

## Section H: Suggested Implementation Order

Use this order to keep diffs reviewable and failures localized.

1. [ ] Change `typeExp` plus the small callable constructor helpers.
2. [ ] Update substitution, occurs check, unification, instantiation, rendering, and canonicalization.
3. [ ] Update constructor spine helpers and any pure arrow assumptions.
4. [ ] Add the new analyzer signature and thread it through existing helpers without changing branch meaning yet.
5. [ ] Add callable builders, callable consumers, and effect-threaded `analyzeArgs`.
6. [ ] Migrate lambda and apply branches.
7. [ ] Migrate ordered pure branches, branching forms, and binding forms.
8. [ ] Add `reset_expr`.
9. [ ] Add `shift_expr`.
10. [ ] Revisit `supports` and any temporary placeholders such as `amb_expr` and `back_expr`.

## Section I: Minimal Validation Checklist

This note is file-local, but a few checks should be tied to the implementation order.

1. [ ] After explicit callable constructors land, verify that pure programs still render pure arrow and thunk types unchanged.
2. [ ] After the analyzer signature is threaded everywhere, verify that the file still supports the existing non-delimited expression subset.
3. [ ] After lambda and apply migration, verify that partial application still produces the expected nested callable types.
4. [ ] After `reset_expr` lands, verify that internal answer-type change is hidden from the surrounding context.
5. [ ] After `shift_expr` lands, verify that named helpers infer latent controlful callable types and that invoking them outside a delimiter is rejected.

## Final Intent

The implementation target for [infer_expr.fn](../infer_expr.fn) should now be explicit enough to track line-by-line work without having to reinterpret the broader planning notes each time.
