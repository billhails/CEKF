# Plan for Delimited Type Representation in `fn/rewrite`

This note narrows Phase 2 of [delimited-typechecking-plan.md](delimited-typechecking-plan.md).

Breadth-first analyzer rule skeletons using this representation are expanded in [delimited-typing-rules.md](delimited-typing-rules.md).

The goal here is not to finish the typing rules. The goal is to choose a concrete representation for delimited-control types in [infer_expr.fn](../infer_expr.fn) so the later rule work has a stable target.

This note is intentionally scoped to `fn/rewrite`. Porting choices are out of scope for now, but when two representations are otherwise comparable this note prefers the one that will be easier to translate into explicit C structures later.

## Current Anchor

The current rewrite inferencer uses this shape:

```text
typedef typeExp {
      varType(string)
    | operType(string, list(typeExp))
}
```

That means all structured types are currently encoded through `operType`, including:

- ordinary arrows via `operType("->", [dom, cod])`
- thunks via `operType("Thunk", [type])`
- tuples via `operType("Tuple", parts)`

This works for ordinary HM because the inferencer only needs generic traversal plus a few printer and matcher special cases.

Delimited control changes the pressure on this design for three reasons.

First, a named function must carry latent answer-type behavior in its own type.

Second, zero-argument lambdas are represented distinctly as thunks in [infer_expr.fn](../infer_expr.fn), so the representation has to account for controlful thunks as well as controlful arrows.

Third, recursive-binding convergence currently depends on `canonicalizeType` and `renderType` producing stable strings, so any new type shape must remain easy to canonicalize and print deterministically.

## Representation Requirements

The chosen representation should satisfy all of these.

### Requirement 1: Latent control must live in the type

The control behavior of a named function cannot be reconstructed later from syntax.

That rules out plans that keep answer-type change only in expression-analysis side state or only on `shift` syntax nodes.

### Requirement 2: Answer types stay ordinary types

For the first cut, the answer types in

$$
[\alpha \mapsto \beta]
$$

should remain ordinary `typeExp` values.

Do not introduce a second class of answer-only variables or a second substitution engine. The same unifier should continue to handle:

- value result types
- function argument types
- pre-shift answer types
- post-shift answer types

The restriction on answer-type polymorphism should be enforced by generalization policy, not by inventing a separate representation for answer variables.

### Requirement 3: Partial application must preserve the latent effect

If a multi-argument function body contains `shift`, then partially applying the first arguments should produce another function value that still carries the latent answer-type change.

For example, if the source-level reading is:

```text
lambda(x, y). body
```

with body result type `R` and latent answer-type change `[A => B]`, then after applying the first argument the resulting value must still know that applying the remaining argument may change the answer type from `A` to `B`.

### Requirement 4: Zero-argument lambdas must be handled explicitly

Current rewrite inference treats `lambda([], body)` as a thunk, not as an ordinary unary function over unit.

That means a representation that only adds a controlful arrow and leaves thunks pure is incomplete.

### Requirement 5: The representation should remain first-order and explicit

The inferencer already performs direct recursive traversals in:

- `applySubst`
- `occursInType`
- `unifyType`
- `instantiateApplied`
- `canonicalizeType`
- `renderType`

The delimited-control representation should fit that style cleanly. It should not require higher-order encodings, hidden side tables, or effect-specific unification logic scattered across the analyzer.

### Requirement 6: Debug output must stay readable

Recursive binding iteration uses canonicalized string snapshots, so `renderType` must remain easy to read and easy to stabilize.

If the representation is difficult to print clearly, it will be difficult to debug letrec behavior later.

## Options Considered

## Option A: Keep everything inside `operType`

One possibility is to keep `typeExp` unchanged and encode delimited-control callables with new operator names such as:

```text
operType("Ctl->", [arg, result, answerIn, answerOut])
operType("CtlThunk", [result, answerIn, answerOut])
```

### Option A Advantages

- smallest immediate change to the `typeExp` definition
- generic substitution, unification, and instantiation continue to work automatically
- easy to prototype quickly

### Option A Disadvantages

- callable shapes remain stringly typed
- printer logic has to keep recognizing special operator names manually
- the representation gives up static structure exactly where the typechecker most needs clarity
- later C porting would have to recover explicit structure that rewrite chose not to model directly

### Option A Verdict

This is acceptable as a quick spike but not as the representation to plan around.

## Option B: Represent delimited control as a wrapped computation result

Another possibility is to keep ordinary arrows pure and wrap only the codomain of controlful functions, conceptually along these lines:

```text
funType(arg, controlResultType(result, answerIn, answerOut))
```

Under this approach, application would produce a special computation-result type, and expression analysis would interpret that wrapper as the latent answer-type change.

### Option B Advantages

- partial application works naturally because the effect lives at the end of the arrow chain
- ordinary arrows stay syntactically ordinary

### Option B Disadvantages

- the distinction between value types and computation types becomes more implicit
- zero-argument thunks still need special treatment
- variables and environment entries now carry a wrapper that is not itself a normal value type
- this drifts away from the direct-style function reading in [delimited-types.md](delimited-types.md)

### Option B Verdict

This is conceptually possible, but it makes the representation less direct than necessary for the first cut.

## Option C: Add explicit controlful callable constructors

This option keeps ordinary named types in `operType` but makes callable shapes explicit.

Recommended shape:

```text
typedef typeExp {
      varType(string)
    | operType(string, list(typeExp))
    | thunkType(typeExp)
    | funType(typeExp, typeExp)
    | controlThunkType(typeExp, typeExp, typeExp)
    | controlFunType(typeExp, typeExp, typeExp, typeExp)
}
```

with the following intended meanings:

- `thunkType(result)` means a pure zero-argument computation returning `result`
- `funType(arg, result)` means a pure unary function
- `controlThunkType(result, answerIn, answerOut)` means a zero-argument computation with latent answer-type change
- `controlFunType(arg, result, answerIn, answerOut)` means a unary function whose body may change the answer type when called under `reset`

### Option C Advantages

- latent control is explicit in the type tree
- pure and controlful callables are easy to distinguish in application rules
- zero-argument lambdas are handled directly instead of as a footnote
- later C porting will be easier because callable forms are already explicit structures
- printer support can remain readable and deterministic

### Option C Disadvantages

- more initial rewrite work than Option A
- the generic type walkers need new cases for callable constructors

### Option C Verdict

This is the recommended representation.

## Recommended Representation

Use Option C.

More specifically:

1. Keep ordinary algebraic and named types in `operType`.
2. Pull callable forms out into explicit constructors.
3. Represent answer input and answer output as ordinary `typeExp` children.
4. Keep pure callables and controlful callables distinct in the representation.
5. Let generalization policy, not the type representation, decide which answer variables are non-generic.

This gives the rewrite inferencer an explicit internal model without forcing it to redesign all non-callable types at the same time.

## How Multi-Argument Functions Should Be Represented

Current rewrite inference already represents multi-argument functions as nested unary arrows.

That should remain true.

The latent answer-type change should live at the rightmost callable node, because that is the point where the function body actually runs.

For a pure two-argument function:

```text
A -> B -> R
```

keep the current nested shape:

```text
funType(A, funType(B, R))
```

For a two-argument function whose body has latent answer-type change `[X => Y]`, use:

```text
funType(A, controlFunType(B, R, X, Y))
```

That means partial application to the first argument returns a value whose type still records the remaining control behavior.

For a zero-argument function with latent answer-type change `[X => Y]`, use:

```text
controlThunkType(R, X, Y)
```

This is the main reason the representation needs both controlful arrows and controlful thunks.

## How `shift` Should See the Captured Continuation

For the first cut, the captured continuation passed into `shift` should still use ordinary pure callable types.

Under the binder presentation:

$$
k : \tau \to \alpha
$$

the continuation is just an ordinary function from the hole type to the pre-shift answer type.

That means the representation does not need a special continuation constructor.

For rewrite `(shift f)`, the first implementation target remains:

$$
f : (\tau \to \alpha) \to \beta
$$

with the ambient answer-type change carried by the surrounding expression analysis, not by a separate continuation-specific type family.

## Suggested Printer Shape

The exact printer syntax can still change, but the representation plan should reserve readable forms for debugging.

Suggested forms:

- pure thunk: `#() -> R`
- controlful thunk: `#() -[X=>Y]-> R`
- pure function: `A -> R`
- controlful function: `A -[X=>Y]-> R`

Examples:

- `funType(A, controlFunType(B, R, X, Y))` prints as `A -> (B -[X=>Y]-> R)`
- `controlThunkType(R, X, Y)` prints as `#() -[X=>Y]-> R`

This keeps snapshots readable while making latent answer-type behavior hard to miss.

## Consequences for `infer_expr.fn`

Even before adding `reset` and `shift` rules, the representation change will require coordinated updates in a small number of places.

### Callable constructors and builders

- replace helper functions that currently encode arrows and thunks via `operType`
- keep a helper for pure arrow chains
- add a helper for building arrow chains whose rightmost callable node is controlful

### Recursive type walkers

- `applySubst`
- `occursInType`
- `instantiateApplied`
- `canonicalizeType`

Each of these should recurse structurally through the new callable constructors.

### Unification

`unifyType` should treat:

- pure thunk with pure thunk
- pure function with pure function
- controlful thunk with controlful thunk
- controlful function with controlful function

as ordinary structural unification cases.

Do not add magical coercions from pure callable types to controlful callable types inside `unifyType`. If a rule wants to treat purity as the special case where `answerIn == answerOut`, it should do so explicitly in the analyzer logic.

### Rendering and snapshot stability

- `renderType`
- `renderDomType`
- `canonicalizeType`
- recursive binding snapshots

These must all gain explicit cases for controlful callables so the letrec fixpoint logic remains stable and debuggable.

## Recommended Staging Inside `fn/rewrite`

Implement the representation in three small steps.

### Stage 1: Make callable forms explicit without changing behavior

- introduce explicit pure `thunkType` and `funType` constructors in `typeExp`
- keep all existing inference behavior otherwise unchanged
- preserve the current printed surface for pure types

### Stage 2: Add controlful callable constructors

- add `controlThunkType` and `controlFunType`
- update substitution, unification, canonicalization, and rendering
- do not yet add `reset` or `shift` typing rules

### Stage 3: Build rules on top of the new representation

- lambda typing chooses between pure and controlful callable constructors
- application threads answer types and consumes the new callable shapes
- `reset` and `shift` rules are added only after the representation is stable

This keeps the representation decision isolated from the later rule work.

## Final Recommendation

For `fn/rewrite`, the representation plan should be:

- keep `operType` for ordinary named type constructors
- make pure callables explicit
- add explicit `controlFunType` and `controlThunkType`
- keep answer types as ordinary `typeExp` children
- store latent answer-type change at the rightmost callable node in a curried chain
- avoid stringly `operType` encodings for controlful callables

That is the best balance between rewrite-local clarity, incremental implementation, and later portability into explicit C data structures.
