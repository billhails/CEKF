# Plan for Delimited Typing Rules in `fn/rewrite`

This note expands the representation choice in [delimited-type-representation.md](delimited-type-representation.md) into analyzer-level rule skeletons.

The file-local implementation breakdown for [infer_expr.fn](../infer_expr.fn) is in [delimited-infer-expr-checklist.md](delimited-infer-expr-checklist.md).

The goal here is breadth first, not depth first. This note should answer:

- what the new analyzer interface needs to look like
- how the existing `infer_expr.fn` cases roughly map onto that interface
- where `lambda`, `apply`, `reset`, and `shift` need distinct handling
- which cross-cutting helpers should exist before rule-by-rule coding starts

This note is still scoped to `fn/rewrite`. It intentionally stops short of a C port.

## Current Anchor in `infer_expr.fn`

The current inferencer revolves around:

```text
analyzeExpr(expr, envt, ng, subst) -> #(valueType, subst)
```

The current callable-related cases are structurally simple:

- `lambda([], body)` analyzes `body` and returns `thunkType(bodyType)`
- `lambda(params, body)` analyzes `body` and returns a nested pure arrow chain
- `apply(fun, [])` unifies the function with `thunkType(resultType)`
- `apply(fun, args)` unifies the function with `funTypeList(argTypes, resultType)`

That is exactly the surface we want to preserve conceptually, but it is not enough once answer-type change becomes part of function types.

## Main Refinement to the Earlier Sketch

The earlier planning note used this conceptual form:

```text
analyzeExpr(expr, envt, ng, subst, answerIn, answerOut) -> #(valueType, subst)
```

That is necessary, but not quite sufficient.

It does not by itself prevent these invalid situations:

- a bare `shift` at top level
- applying a latent controlful function in a context with no enclosing `reset`

Answer-type equality alone is not enough to rule those out. A controlful callable may have `answerIn == answerOut` and still require a delimiter.

So the analyzer needs one more piece of context.

## Recommended Analyzer Interface

Use this conceptual form:

```text
analyzeExpr(expr, envt, ng, subst, answerIn, answerOut, controlAllowed) -> #(valueType, subst)
```

where:

- `valueType` is the direct-style value type of the expression
- `answerIn` is the answer type expected before the expression runs in the current local control context
- `answerOut` is the answer type produced after the expression runs in that same local control context
- `controlAllowed` says whether this expression position may consume or introduce delimited control

Recommended initial meaning of `controlAllowed`:

- `false` at the top-level program entrypoint
- `true` inside the body of `reset`
- `true` inside lambda bodies, because lambdas may carry latent control behavior even when defined outside `reset`
- inherited by ordinary subexpressions

This keeps the answer-type machinery focused on typing, while the extra flag enforces the separate syntactic fact that delimited control must have a delimiter available somewhere in the relevant control context.

## Top-Level Entry Point

The public `infer(expr)` entrypoint should continue to return an ordinary type.

Conceptually:

```text
let topAnswer = freshTypeVar()
in analyzeExpr(expr, nullTcEnv, [], nullSubstEnv, topAnswer, topAnswer, false)
```

This does two things:

- top-level expressions must be pure in the ambient context
- a top-level `shift` or top-level application of a controlful callable is rejected because `controlAllowed` is `false`

## Cross-Cutting Helper Layer

Before changing individual expression rules, the inferencer should gain a small helper layer.

## Helper 1: Pure-effect wrapper

Many existing rules are semantically pure in the ambient context.

Recommended helper shape:

```text
analyzeExprPure(expr, envt, ng, subst, answer) -> #(valueType, subst)
```

which simply calls:

```text
analyzeExpr(expr, envt, ng, subst, answer, answer, false-or-inherited)
```

The exact wrapper signature can vary, but the point is to make the “this position is pure in the ambient context” intent explicit.

## Helper 2: Callable normalization

The representation note chose explicit pure and controlful callable constructors:

- `thunkType(result)`
- `funType(arg, result)`
- `controlThunkType(result, answerIn, answerOut)`
- `controlFunType(arg, result, answerIn, answerOut)`

For inference, it is useful to have a normalization helper that collapses controlful callables whose answer types have unified:

```text
controlThunkType(R, A, A)  => thunkType(R)
controlFunType(X, R, A, A) => funType(X, R)
```

This should happen in analyzer-side helper logic or final type normalization, not inside generic unification.

That keeps inference workable for unknown callables while preserving a clean surface form for pure results.

## Helper 3: Callable builders

The lambda rules should not manually decide between pure and controlful constructors inline.

Recommended helpers:

- `makeLambdaCallable(bodyType, answerIn, answerOut, subst)` for zero-argument lambdas
- `makeLambdaCallableChain(paramTypes, bodyType, answerIn, answerOut, subst)` for curried lambdas

These helpers should:

- place the latent answer-type behavior at the rightmost callable node
- normalize the final callable shape when `answerIn` and `answerOut` unify

## Helper 4: Callable consumers

Application should not inline the distinction between:

- pure thunk
- controlful thunk
- pure function
- controlful function
- unknown type variable later forced into one of those callable shapes

Recommended helpers:

- `consumeThunkCallable(type, resultType, callAnswerIn, callAnswerOut, controlAllowed, subst)`
- `consumeCurriedCallable(type, argTypes, resultType, callAnswerIn, callAnswerOut, controlAllowed, subst)`

These helpers are where application-specific coercion logic belongs.

In particular, if a type variable is used in callable position, these helpers may instantiate it to a controlful callable shape first and rely on later normalization to collapse the pure case.

That is analyzer logic, not unifier magic.

## Helper 5: Effect-threaded argument analysis

Current `analyzeArgs` only accumulates argument types.

Delimited typing needs the evaluation order to stay explicit.

Recommended conceptual shape:

```text
analyzeArgs(args, envt, ng, subst, answerIn, answerOut, controlAllowed) -> #(argTypes, subst)
```

The helper should evaluate argument expressions left-to-right, threading answer types through them exactly the same way the direct application rule in [delimited-types.md](delimited-types.md) does.

## Breadth-First Rule Skeletons

The sections below do not try to finish every edge case. They define the intended skeleton and where each existing `infer_expr.fn` branch should land.

## Rule Group 1: Atomic Pure Forms

These forms remain pure in the ambient context:

- variables
- integer and character literals
- boolean and null constants
- constructor names already stored in the type environment

Skeleton:

```text
analyze atomic form under (answerIn, answerOut, controlAllowed)
unify answerIn with answerOut
return ordinary value type
```

This group is the direct counterpart of the `VAR` and constant rules in [delimited-types.md](delimited-types.md).

## Rule Group 2: Transparent Wrappers

These forms do not add control behavior of their own but simply forward the result of a subexpression:

- `print_exp`
- `tag`
- `typeof_expr` if it remains supported in this path

Skeleton:

```text
analyze child under the same (answerIn, answerOut, controlAllowed)
return the child result type or the wrapper-specific result type
```

The key point is that these wrappers do not force purity. They preserve whatever ambient answer-type threading their child already induces.

## Rule Group 3: Lambda Introduction

## Lambda Zero-Argument Skeleton

For:

```text
lambda([], body)
```

use fresh body answer variables:

```text
bodyAnswerIn  = freshTypeVar()
bodyAnswerOut = freshTypeVar()
```

Then:

1. analyze `body` under `(bodyAnswerIn, bodyAnswerOut, true)`
2. build either `thunkType(bodyType)` or `controlThunkType(bodyType, bodyAnswerIn, bodyAnswerOut)` via the callable builder
3. unify the ambient `answerIn` with `answerOut`, because constructing the thunk is pure
4. return the callable value type

## Lambda Parameterized Skeleton

For:

```text
lambda(params, body)
```

use the existing parameter-binding machinery, but analyze the body under fresh answer variables and `controlAllowed = true`.

Then build the callable chain so that only the rightmost callable node carries the latent answer-type change.

Conceptually:

```text
paramTypes = [A, B, C]
bodyType = R
body effect = [X => Y]
```

becomes:

```text
funType(A, funType(B, controlFunType(C, R, X, Y)))
```

followed by normalization if `X` and `Y` unify.

As with the zero-argument case, lambda construction itself is pure in the surrounding ambient context.

## Rule Group 4: Application

Application is where the direct-style latent effect becomes active.

It should be implemented as a three-stage decomposition.

## Application Stage 1: Evaluate the function expression

For either `apply(fun, [])` or `apply(fun, args)`:

1. create a fresh intermediate answer variable `afterFun`
2. analyze `fun` under `(answerIn, afterFun, controlAllowed)`

This captures the fact that evaluating the function position may itself contain control behavior.

## Application Stage 2: Evaluate the arguments

For `apply(fun, args)` with arguments present:

1. create a fresh intermediate answer variable `beforeCall`
2. analyze the argument list under `(afterFun, beforeCall, controlAllowed)`
3. obtain the list of argument types in source evaluation order

For `apply(fun, [])`, this stage is skipped and `afterFun` is used directly as the call-entry answer type.

## Application Stage 3: Consume the callable type

After function and argument evaluation are complete:

- `apply(fun, [])` uses `consumeThunkCallable`
- `apply(fun, args)` uses `consumeCurriedCallable`

These helpers must distinguish four semantic cases.

### Pure thunk call

If the callable is `thunkType(result)`:

- unify `callAnswerIn` with `callAnswerOut`
- return `result`

### Controlful thunk call

If the callable is `controlThunkType(result, thunkIn, thunkOut)`:

- require `controlAllowed = true`
- unify `callAnswerIn` with `thunkIn`
- unify `callAnswerOut` with `thunkOut`
- return `result`

### Pure curried function call

If the callable is a pure arrow chain whose final node is pure:

- unify arguments against the pure chain as usual
- unify `callAnswerIn` with `callAnswerOut`
- return the remaining result type

### Controlful curried function call

If the callable is a chain whose rightmost callable node is controlful:

- require `controlAllowed = true`
- unify arguments against the chain
- use the rightmost callable node's `answerIn` and `answerOut` as the latent body effect
- return the resulting value type after the consumed arguments

## Unknown callable variables in application

This is the main inference wrinkle.

If a type variable appears in callable position, application cannot wait for some other rule to decide whether it is pure or controlful.

Recommended approach:

- instantiate unknown callables to a controlful callable skeleton during callable consumption
- keep the fresh answer variables explicit
- normalize later if those answer variables unify

For example, a zero-argument unknown callable can be forced to:

```text
controlThunkType(result, freshIn, freshOut)
```

and a unary unknown callable can be forced to:

```text
controlFunType(arg, result, freshIn, freshOut)
```

This keeps application deterministic without adding special disjunctive logic to generic unification.

## Rule Group 5: Sequence and Other Ordered Pure Forms

These forms remain structurally transparent to answer-type change but must preserve evaluation order.

This group includes:

- `sequence`
- tuple construction
- primitive arithmetic and comparisons
- constructor argument lists
- deconstruction scrutinees
- tuple index scrutinees

Skeleton:

1. evaluate subexpressions left-to-right using threaded answer variables
2. once all subexpressions are available, build the ordinary result type
3. do not introduce new latent effect at the final constructor itself

That means the resulting answer-type behavior comes entirely from the subexpressions already analyzed.

## Rule Group 6: Branching Forms

This group includes:

- `if_expr`
- `cond_expr`
- `match_cases`

## Branching test position

The test or scrutinee expression is evaluated first and may itself change the current answer type.

So each branching rule should introduce an intermediate answer variable after the test position.

## Branching branch positions

Each branch should then be checked from the same post-test answer type to the same final `answerOut`.

Conceptually for `if_expr`:

```text
answerIn --test--> afterTest
afterTest --consequent--> answerOut
afterTest --alternative--> answerOut
```

The branch result types must unify as usual.

## Rule Group 7: Binding Forms

This group includes:

- `let_expr`
- `letstar_expr`
- `letrec_expr`

## Let and let*

For a simple binding sequence, keep the current environment logic but make answer threading explicit.

For `let`:

1. analyze the binding expressions left-to-right from `answerIn` to `afterBindings`
2. extend the environment with the inferred binding types
3. analyze the body from `afterBindings` to `answerOut`

For `let*`, the same structure applies, but the environment extends after each binding as it does now.

## Letrec

The recursive-binding fixpoint logic remains, but the stored binder types may now be controlful callable types.

Breadth-first consequences only:

- recursive binder snapshots must print normalized controlful callables deterministically
- recursive lambda bodies must be analyzed under `controlAllowed = true`
- generalization policy is still deferred to the later dedicated phase

No deeper letrec policy should be decided in this note.

## Rule Group 8: `reset`

`reset` creates a fresh local delimiter and is pure to its surrounding context.

Recommended skeleton:

1. create fresh `bodyAnswerIn`
2. create fresh `bodyAnswerOut`
3. analyze the body under `(bodyAnswerIn, bodyAnswerOut, true)`
4. unify the body value type with `bodyAnswerIn`
5. unify the surrounding `answerIn` with `answerOut`
6. return `bodyAnswerOut`

This is the direct analyzer counterpart of the rule from [delimited-types.md](delimited-types.md):

$$
\frac{\Gamma \vdash e : \tau [\tau\mapsto\sigma]}
     {\Gamma \vdash \operatorname{reset}\ e : \sigma [\alpha\mapsto\alpha]}
$$

Two consequences matter.

First, the body of `reset` gets a fresh local answer-type context, independent of the ambient outer one.

Second, `reset` itself is pure to the outside world even when its body is not.

## Rule Group 9: `shift`

Rewrite syntax uses:

```text
(shift f)
```

not the binder notation from the semantic note.

For the first cut, implement `shift` through the function-argument reading already described in [delimited-types.md](delimited-types.md) and [delimited-type-representation.md](delimited-type-representation.md).

Recommended skeleton:

1. require `controlAllowed = true`
2. create fresh `holeType`
3. create fresh `capturedAnswer`
4. analyze `f` under a pure ambient context for the shift body result
5. require `f` to have type:

```text
funType(funType(holeType, capturedAnswer), shiftBodyResult)
```

or its zero-argument equivalent only if the surface syntax later allows it

1. return `holeType` as the value type of the whole `shift`
2. treat the ambient answer-type change of the `shift` expression itself as:

```text
[capturedAnswer => shiftBodyResult]
```

This is the rewrite equivalent of the binder rule:

$$
\frac{\Gamma, k : \tau \to \alpha \vdash e : \beta [\beta\mapsto\beta]}
     {\Gamma \vdash \operatorname{shift}\ k.e : \tau [\alpha\mapsto\beta]}
$$

The important implementation point is that `shift` does not itself return the body result type. It returns the hole type expected by the captured continuation.

## Rule Group 10: `call/cc`

`call/cc` can remain a temporary special case while `shift/reset` work lands.

Breadth-first recommendation:

- keep the existing `callcc_expr` branch separate initially
- update it only as much as needed to work with explicit pure `funType` rather than the old stringly arrow encoding
- do not force it into the delimited-control framework until `shift/reset` typing is stable

That keeps the scope of the first pass bounded.

## Suggested Breadth-First Rollout

Implement these rules in four passes.

## Rollout Pass 1: Thread the new context everywhere

- change `analyzeExpr` to accept `answerIn`, `answerOut`, and `controlAllowed`
- update all existing branches to thread those values, initially preserving current pure behavior
- keep `reset` and `shift` unsupported for the moment

The goal of this pass is structural: make the entire analyzer effect-aware without changing its meaning yet.

## Rollout Pass 2: Switch callable representation and helpers

- land explicit callable constructors
- land callable builders, consumers, and normalization
- update current lambda and apply rules to use the new helpers
- keep the behavior pure except where helper logic must reserve space for controlful callables

The goal of this pass is to make lambda and apply ready for latent effects before adding the control operators themselves.

## Rollout Pass 3: Add `reset`

- implement the delimiter boundary rule
- verify that ordinary pure programs still infer unchanged visible types
- confirm that `reset` can hide internal answer-type change and return an ordinary value type outwardly

This pass establishes the boundary before adding continuation capture.

## Rollout Pass 4: Add `shift`

- implement the rewrite `(shift f)` rule
- reject `shift` when `controlAllowed = false`
- verify that named helpers now carry latent control behavior in their inferred callable types

This is the point where the representation and the analyzer interface finally meet the source feature.

## Final Recommendation

For `fn/rewrite`, the rule plan should be:

- refine the analyzer interface to thread answer types and delimiter availability explicitly
- keep lambda construction pure but analyze lambda bodies in a control-enabled latent context
- decompose application into function evaluation, argument evaluation, and callable consumption
- treat `reset` as the boundary that reinitializes local answer types and returns pure to the outside
- treat `shift` as requiring explicit delimiter availability, not merely answer-type compatibility
- keep `call/cc` separate until the delimited rules are stable

That gives `infer_expr.fn` a breadth-first target that matches the chosen type representation and is specific enough to implement incrementally.
