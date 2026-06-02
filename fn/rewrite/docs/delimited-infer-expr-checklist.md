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

- [x] Replace the current callable encoding inside `operType` with explicit callable constructors.
- [x] Keep `varType` unchanged.
- [x] Keep `operType` for non-callable named constructors such as `Int`, `Bool`, `Null`, `list`, and `Tuple`.
- [x] Add explicit constructors for pure and controlful thunks.
- [x] Add explicit constructors for pure and controlful unary functions.

### Small type constructor helpers

- [x] Keep `intType`, `boolType`, `charType`, `stringType`, `nullValueType`, and `tupleType` as ordinary named-type helpers.
- [x] Rewrite `thunkType` and `funType` to construct explicit callable nodes rather than `operType("Thunk", ...)` and `operType("->", ...)`.
- [x] Keep `funTypeList`, but make it build explicit nested callable nodes.
- [x] Add `controlThunkType` helper.
- [x] Add `controlFunType` helper.
- [x] Add a helper that normalizes `controlThunkType(R, A, A)` to `thunkType(R)`.
- [x] Add a helper that normalizes `controlFunType(X, R, A, A)` to `funType(X, R)`.
- [x] Add a helper for building curried callables whose rightmost callable node is controlful.

### Fresh variables and names

- [x] Keep `freshTypeVar` unchanged.
- [x] Keep `typeVarName` unchanged.
- [x] Do not introduce a second fresh-variable source specifically for answer types.

## Section B: Generic Type Operations

This section covers the functions that walk or compare `typeExp` structurally.

### Substitution

- [x] Update `applySubst` to recurse through each new callable constructor explicitly.
- [x] Keep `applySubstList` for `operType` argument lists.
- [ ] Add any callable-specific child helpers only if the pattern matching becomes too repetitive.

### Occurs check

- [x] Update `occursInType` to recurse through explicit thunk and function constructors.
- [x] Keep `occursInTypeList` for `operType` argument lists.

### Unification

- [x] Extend `unifyType` with structural cases for:
  - pure thunk with pure thunk
  - pure function with pure function
  - controlful thunk with controlful thunk
  - controlful function with controlful function
- [x] Keep `operType` unification for ordinary named constructors.
- [ ] Do not add implicit coercions between pure callables and controlful callables here.
- [ ] Keep purity normalization outside the unifier.
- [x] Preserve the current occurs-check behavior for type variables.
- [x] Keep `unifyArgs` for `operType` argument lists.

### Instantiation and freshness

- [x] Update `instantiateApplied` to recurse structurally through the new callable constructors.
- [x] Keep `instantiateArgs` for `operType` argument lists.
- [x] Keep `freshType` as the environment-instantiation entrypoint.
- [x] Make sure freshening preserves controlful callables instead of flattening them back into generic operators.

### Rendering and canonicalization

- [x] Update `renderType` with explicit cases for pure and controlful thunks.
- [x] Update `renderType` with explicit cases for pure and controlful functions.
- [x] Update `renderDomType` so controlful callable domains print with parentheses when needed.
- [x] Keep readable output for pure types unchanged where possible.
- [x] Update `canonicalizeType` to recurse structurally through the new callable constructors.
- [x] Keep `canonicalizeArgs` for `operType` argument lists.
- [ ] Verify that canonicalization plus rendering still produce stable letrec snapshots.

## Section C: Support Predicates and Front-Door Gating

This section covers the `supports*` family.

### `supports`

- [x] Add explicit support handling for `reset_expr` once the branch is implemented.
- [x] Add explicit support handling for `shift_expr` once the branch is implemented.
- [ ] Decide whether unsupported delimited-control expressions should return `false` here before their typing branches land, rather than falling through implicitly.
- [x] Keep `make_vec` unsupported.
- [x] Keep the current primitive allowlist until delimited typing is stable.

### `supportsList`, `supportsNonEmptyList`, `supportsBindings`, `supportsCondCases`, `supportsCondCasesTail`, `supportsMatchCases`, `supportsMatchCasesTail`

- [ ] Update only as needed for `reset_expr` and `shift_expr` propagation.
- [ ] Do not redesign these helpers unless the analyzer rollout exposes a concrete gap.

## Section D: New Analyzer-Level Helper Layer

This section is new work rather than migration of existing helpers.

### Analyzer entry helpers to add

- [x] Add an effect-aware `analyzeExpr` signature carrying `answerIn`, `answerOut`, and `controlAllowed`.
- [ ] Add a small wrapper for pure ambient contexts, if that keeps branches readable.
- [x] Add a helper that creates fresh local answer-type pairs for lambda bodies and reset bodies.

### Callable builder helpers to add

- [x] Add `makeLambdaCallable` for zero-argument lambdas.
- [x] Add `makeLambdaCallableChain` for curried lambdas.
- [ ] Make these helpers responsible for purity normalization after substitution has been applied.

### Callable consumer helpers to add

- [x] Add `consumeThunkCallable`.
- [x] Add `consumeCurriedCallable`.
- [x] Keep callable-consumption logic out of `unifyType`.
- [x] Let these helpers enforce `controlAllowed` when a controlful callable is actually invoked.
- [x] Let these helpers instantiate type variables to controlful callable skeletons when needed.

### Argument threading helpers to add or replace

- [x] Replace the current `analyzeArgs` with an effect-threaded version.
- [x] Thread answer types left-to-right through argument evaluation.
- [x] Preserve current source evaluation order.

## Section E: Existing Mid-Level Helper Migration

This section covers the helpers below the support predicates and above the `analyzeExpr` dispatcher.

### `analyzeArgs`

- [x] Expand the signature to thread `answerIn`, `answerOut`, and `controlAllowed`.
- [x] Preserve left-to-right evaluation.
- [x] Return the argument type list plus the updated substitution as it does now.

### `analyzeCondCases` and `analyzeMatchCases`

- [x] Expand both helpers to thread answer types explicitly.
- [x] Ensure each branch starts from the same post-test answer state.
- [ ] Keep branch result-type unification logic unchanged in spirit.

### `bindParams`

- [x] Keep parameter binding structurally unchanged.
- [x] Do not add answer-type variables here; those belong to lambda-body analysis, not parameter binding.

### `analyzeLetBindings`

- [x] Expand the helper to thread answer types left-to-right through binding expression evaluation.
- [ ] Keep the environment-extension result unchanged in shape.

### `extendTypeEnv`

- [x] No semantic change expected.
- [ ] Only touch if explicit callable constructors require a small representation update.

### `analyzeSequence`

- [x] Expand the helper to thread answer types left-to-right.
- [x] Preserve “last expression determines result type”.

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

- [x] Keep the broad letrec strategy unchanged for the first pass.
- [x] Update recursive binder snapshots so controlful callables print deterministically.
- [x] Expand `processRecBinding` so lambda bodies are analyzed through the new effect-aware analyzer.
- [x] Keep answer-type generalization policy out of this first migration.
- [x] Defer any letrec soundness redesign beyond what the new representation and analyzer interface force.

### Primitive and constructor helpers

This group includes:

- `analyzePrimitive`
- `analyzeConstruct`
- `ctorTypeToTypeExp`
- `ctorTypeArgList`
- `spineArg`
- `spineResult`

Checklist:

- [x] Expand `analyzePrimitive` to thread answer types through left and right operands.
- [x] Keep the primitive result-type rules unchanged.
- [x] Expand `analyzeConstruct` only through the new application path.
- [x] Update `ctorTypeToTypeExp` so constructor arrows become explicit pure `funType` nodes.
- [x] Update `spineArg` to match explicit `funType` instead of `operType("->", ...)`.
- [x] Update `spineResult` to walk explicit `funType` nodes.
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

- [x] Make atomic value branches unify `answerIn` with `answerOut`.
- [x] Keep their direct result types unchanged.
- [x] Make wrapper branches preserve their child expression's answer threading rather than silently forcing purity.

## Lambda branches

This group includes:

- `E.lambda([], body)`
- `E.lambda(params, body)`

Checklist:

- [x] Analyze lambda bodies under fresh local answer types.
- [x] Analyze lambda bodies with `controlAllowed = true`.
- [x] Keep lambda construction itself pure in the surrounding context.
- [x] Route the result through the new callable-builder helpers.
- [ ] Normalize pure latent effects back to pure callable types when possible.

## Application branches

This group includes:

- `E.apply(fun, [])`
- `E.apply(fun, args)`

Checklist:

- [x] Decompose application into function evaluation, argument evaluation, and callable consumption.
- [x] Thread answer types through function evaluation.
- [x] Thread answer types through argument evaluation in source order.
- [x] Route zero-argument calls through `consumeThunkCallable`.
- [x] Route non-empty applications through `consumeCurriedCallable`.
- [x] Reject controlful callable invocation when `controlAllowed = false`.

## Control-related existing branch

This group includes:

- `E.callcc_expr`

Checklist:

- [x] Keep this branch special-cased for the first pass.
- [x] Update it only enough to use explicit pure `funType` construction.
- [x] Do not force `call/cc` into the new delimited-control rule set yet.
- [x] Treat `call/cc` as a whole-continuation operator, not as evidence that a lambda should be classified as a reset-gated controlful callable.
- [ ] Revisit `call/cc` only in the later reconciliation phase once `answerIn` / `answerOut` threading is stable across the analyzer.

## Branching expressions

This group includes:

- `E.if_expr`
- `E.cond_expr`
- `E.match_cases`

Checklist:

- [x] Evaluate the test or scrutinee first and thread the answer state through it.
- [x] Start each branch from the same post-test answer state.
- [x] Make both branches or all cases end at the same final `answerOut`.
- [x] Keep existing branch result-type unification logic.

## Binding expressions

This group includes:

- `E.let_expr`
- `E.letrec_expr`
- `E.letstar_expr`

Checklist:

- [x] Thread answer types through binding evaluation before entering the body.
- [x] Keep environment extension logic structurally unchanged.
- [x] Keep the letrec fixpoint loop intact unless the new analyzer interface forces a small local rewrite.

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

- [x] Thread answer types through any evaluated subexpressions.
- [x] Keep the final constructor or tuple shape itself pure.
- [x] Keep `make_vec` unsupported.
- [x] Keep typedef environment extension behavior unchanged.

## Sequence and nondeterminism-adjacent expressions

This group includes:

- `E.sequence`
- `E.amb_expr`
- `E.back_expr`

Checklist:

- [x] Expand `sequence` to explicit left-to-right answer threading.
- [x] Decide whether `amb_expr` should simply thread both sides through the new analyzer without deeper redesign in this pass.
- [x] Decide whether `back_expr` remains a placeholder returning a fresh type variable for now.
- [x] Keep these decisions explicit rather than letting them drift as accidental behavior.

## New branches to add

This group includes:

- `E.reset_expr`
- `E.shift_expr`

Checklist:

- [x] Add `E.reset_expr` only after the general analyzer threading is in place.
- [x] Analyze `reset` bodies under fresh local answer types and `controlAllowed = true`.
- [x] Make `reset` pure to the outside by unifying the surrounding `answerIn` with `answerOut`.
- [x] Return the body's outgoing answer type as the direct result type of `reset`.
- [ ] Add `E.shift_expr` only after callable builders and consumers are stable.
- [x] Require `controlAllowed = true` in `shift`.
- [x] Check `shift` operands against the rewrite function-argument form described in the rules note.
- [x] Make `shift` return the captured hole type, not the shift-body result type.
- [ ] Use the ambient answer-type change of the `shift` expression itself to carry the captured answer to shift-body result transition.

## Section G: Entry Points and Public Surface

This section covers the bottom of the file.

### `infer`

- [x] Make `infer` create one fresh top-level answer variable.
- [x] Call the effect-aware `analyzeExpr` with identical `answerIn` and `answerOut` at top level.
- [x] Set top-level `controlAllowed = false`.
- [ ] Normalize the final type after substitution so pure callables do not print as controlful callables with identical answer types.

### `infer_string` and `infer_to_string`

- [x] Keep both entrypoints structurally unchanged.
- [ ] Let them inherit the new normalized output through `infer` and `renderType`.

## Section H: Suggested Implementation Order

Use this order to keep diffs reviewable and failures localized.

1. [x] Change `typeExp` plus the small callable constructor helpers.
2. [x] Update substitution, occurs check, unification, instantiation, rendering, and canonicalization.
3. [x] Update constructor spine helpers and any pure arrow assumptions.
4. [ ] Add the new analyzer signature and thread it through existing helpers without changing branch meaning yet.
5. [x] Add callable builders, callable consumers, and effect-threaded `analyzeArgs`.
6. [x] Migrate lambda and apply branches.
7. [x] Migrate ordered pure branches, branching forms, and binding forms.
8. [x] Add `reset_expr`.
9. [x] Add `shift_expr`.
10. [ ] Revisit `supports` and any temporary placeholders such as `amb_expr` and `back_expr`.

## Section I: Minimal Validation Checklist

This note is file-local, but a few checks should be tied to the implementation order.

1. [x] After explicit callable constructors land, verify that pure programs still render pure arrow and thunk types unchanged.
2. [x] After the analyzer signature is threaded everywhere, verify that the file still supports the existing non-delimited expression subset.
3. [x] After lambda and apply migration, verify that partial application still produces the expected nested callable types.
4. [x] After `reset_expr` lands, verify that internal answer-type change is hidden from the surrounding context.
5. [x] After `shift_expr` lands, verify that named helpers infer latent controlful callable types and that invoking them outside a delimiter is rejected.

## Final Intent

The implementation target for [infer_expr.fn](../infer_expr.fn) should now be explicit enough to track line-by-line work without having to reinterpret the broader planning notes each time.
