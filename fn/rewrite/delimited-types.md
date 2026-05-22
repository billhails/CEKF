# Delimited control typing sketch for `fn/rewrite`

This note complements [delimited-cps.md](delimited-cps.md).

That note fixes a candidate operational account for `reset` and `shift` in the rewrite CPS prototype. This note asks a narrower question: what static typing discipline can describe that behavior safely, especially when `shift` appears inside a named function and the matching `reset` is only present at some later call site.

The short answer is that plain Hindley-Milner types over ordinary arrows are not enough. A `shift` occurrence does not just compute a result of some type `τ`. It also changes the answer type of the surrounding delimited computation. Once `shift` can sit inside a named function, that control behavior becomes latent in the function type and cannot be reconstructed only from the local syntax at the eventual call site.

## Goal

The goal of this sketch is not to commit to a full inference algorithm yet. The goal is to pin down a type judgment precise enough that:

- it explains why `shift` and `reset` are co-dependent
- it handles named functions whose bodies contain `shift`
- it matches the intended two-layer CPS reading in [delimited-cps.md](delimited-cps.md)
- it makes clear which part is easy to prototype and which part is likely to need annotations or a value restriction

## Why ordinary HM is not enough

The current rewrite inference prototype in [infer.fn](infer.fn) only infers ordinary expression types and ordinary arrow types.

That works for `call/cc`, because `call/cc` can be given a self-contained type schema:

$$
((a \to b) \to a) \to a
$$

The direct-style type of `shift` is different. Its meaning depends on the nearest enclosing `reset`, because `shift` captures only the continuation up to that delimiter. That means the source typing judgment needs to track not just the type of the local subexpression, but also how that subexpression changes the answer type of the enclosing delimited computation.

The named-function case makes this unavoidable:

```text
let f = λx. shift k. e
in reset (C[f v])
```

Operationally, the `shift` in `f` captures the continuation from the call site `C[□]`, not from the definition site of `f`.

Statically, that means `f` cannot be assigned a plain type like `σ -> τ`. The fact that calling `f` requires a delimiter and may modify the answer type has to appear in the type of `f` itself.

## Candidate judgment

Use an answer-type-modifying judgment:

$$
\Gamma \vdash e : \tau ; \alpha, \beta
$$

Read this as:

- `e` produces a value of type `τ`
- when `e` is evaluated under the nearest enclosing `reset`, it transforms the ambient answer type from `α` to `β`

Intuitively:

- pure expressions leave the answer type unchanged, so they have the form `Γ ⊢ e : τ ; α, α`
- `shift` is the construct that can change `α` to a different `β`
- `reset` hides that internal answer-type change and restores an ordinary outer judgment

This is the standard answer-type-modification view of `shift/reset`, adapted to the rewrite prototype.

## Function types

Ordinary arrows are no longer sufficient for functions whose bodies may use `shift`. Use an effect-indexed arrow:

$$
\sigma / \alpha \to \tau / \beta
$$

Read this as:

- the function accepts an argument of type `σ`
- it returns a result of type `τ`
- when applied inside a delimiter, its body transforms the answer type from `α` to `β`

Pure functions are the special case where `α = β`.

This is the key point for named functions: a function that contains `shift` carries the answer-type change in its own type, so a later caller under `reset` can instantiate that latent control behavior.

## Core rules

The full language would need rules for all forms, but the core picture can be fixed with just values, lambdas, application, `reset`, and `shift`.

### Variables and constants

Pure expressions preserve the ambient answer type:

$$
\frac{x : \tau \in \Gamma}{\Gamma \vdash x : \tau ; \alpha, \alpha}
$$

and similarly for constants.

### Lambda

Building a function value is pure, even when calling that function later may not be:

$$
\frac{\Gamma, x : \sigma \vdash e : \tau ; \alpha, \beta}
     {\Gamma \vdash \lambda x.e : \sigma / \alpha \to \tau / \beta ; \gamma, \gamma}
$$

The body records the latent answer-type change on the arrow. The act of constructing the lambda itself does not immediately affect the current delimiter, so the outer judgment stays pure.

### Application

Application composes the latent answer-type change from the function body with the current context:

$$
\frac{\Gamma \vdash f : \sigma / \alpha \to \tau / \beta ; \gamma, \delta \qquad
      \Gamma \vdash a : \sigma ; \delta, \alpha}
     {\Gamma \vdash f\ a : \tau ; \gamma, \beta}
$$

This is the point where a named function with latent control effect becomes active.

If `f` is pure, this collapses to the ordinary rule because `α = β`.

### Reset

`reset` delimits answer-type change and returns an ordinary expression result to its surrounding context:

$$
\frac{\Gamma \vdash e : \tau ; \tau, \sigma}
     {\Gamma \vdash \operatorname{reset}\ e : \sigma ; \alpha, \alpha}
$$

Read this as:

- inside the reset, the body `e` starts in a context expecting a `τ`
- the body may change the answer type from `τ` to `σ`
- the reset itself discharges that internal change and is pure to the outside world

This matches the operational idea from [delimited-cps.md](delimited-cps.md): inside a fresh `reset`, the local continuation starts at `idk`, and once the body completes, control returns to the outer meta continuation as an ordinary result.

### Shift

`shift` captures the local continuation up to the nearest `reset`.

The continuation variable gets the ordinary function type from the hole type to the pre-shift answer type:

$$
\frac{\Gamma, k : \tau \to \alpha \vdash e : \beta ; \beta, \beta}
     {\Gamma \vdash \operatorname{shift}\ k.e : \tau ; \alpha, \beta}
$$

Read this as:

- the surrounding delimited context is waiting for a value of type `τ`
- if resumed normally, that context would produce answer type `α`
- inside the body of `shift`, the captured continuation is available as `k : τ -> α`
- the body itself runs after the current local continuation has been removed, so it is checked as an ordinary expression producing `β`
- the whole `shift` expression therefore changes the answer type from `α` to `β`

This rule is the source-level counterpart of the CPS rule where `shift` passes a reified local continuation to its operand and resumes with `idk` inside the body.

## Named function case

Now consider the case that motivated this note:

```text
let f = λx. shift k. e
in reset (C[f v])
```

The body of `f` is checked under some answer-type pair `α, β`, so the lambda gets a type of the form:

$$
f : \sigma / \alpha \to \tau / \beta
$$

That is the static record of the latent control behavior.

At the call site inside `reset`, the ambient delimited context `C[□]` determines the specific instantiation of `α` and `β`. The caller does not discover the control effect by re-inspecting the syntax of `f`. It learns it from the type of `f`.

This is the essential reason a syntax-local rule for `shift` is not enough once `shift` can appear behind a variable reference.

## Small examples

### Pure use of the captured continuation

```text
reset (1 + shift k. k 2)
```

The hole type is `Int`, and resuming the captured continuation also yields `Int`, so the answer type does not change. This is the easy case where the internal pair is `Int, Int`.

### Discarding the local continuation

```text
reset (1 + shift k. 2)
```

The surrounding local continuation still expects an `Int`, so the left side of the pair is `Int`. But the shift body returns `2` directly from the reset body, so the right side is also `Int` in this particular example.

Operationally the local continuation is discarded. Typing alone does not force `k` to be used.

### Named helper

```text
let f = λx. shift k. k x
in reset (1 + f 2)
```

The important part is not the final result type. The important part is that `f` cannot be typed as only `Int -> Int`. Its type must include the answer-type behavior of its body, even though the actual delimiter is only introduced around `f 2`.

## Generalization and safety

This is where the design gets subtle.

Once function types mention answer-type variables, unrestricted HM-style let-generalization becomes questionable. A binding like:

```text
let f = λx. shift k. e
```

may appear to want a polymorphic type scheme over `α` and `β`. In the literature, unrestricted answer-type polymorphism interacts badly with ordinary let-polymorphism unless the language adopts additional restrictions.

For a first prototype, the safe options are deliberately narrower:

1. Require explicit type annotations on any binding whose type mentions answer-type variables.
2. Treat answer-type variables as non-generalizable by default, analogous to a value restriction.
3. Prototype the typing story after CPS first, where the continuation structure is explicit and ordinary function typing can validate the transform.

Option 1 is the cleanest if the immediate goal is clarity rather than maximal inference power.

## Relationship to the two-layer CPS note

The proposed judgment matches the two-layer CPS story directly.

In [delimited-cps.md](delimited-cps.md):

- `k` is the local continuation up to the nearest `reset`
- `m` is the meta continuation outside that `reset`
- `shift` captures `k` but not `m`

In this typing note:

- `τ` is the type expected by the captured local continuation hole
- `α` is the answer type produced if that local continuation runs normally
- `β` is the answer type produced by the `shift` body after the local continuation has been removed

So the answer-type pair `α, β` is the type-level shadow of the operational split between local continuation and meta continuation.

## Recommended first implementation target

For the rewrite prototype, the most pragmatic staged plan is:

1. Keep this note as the direct-style semantic reference.
2. Validate the delimited CPS transform first.
3. Add a post-CPS type check before attempting source-level inference for `shift/reset`.
4. If source-level typing is still desired, start with explicit annotations on controlful named functions and on `reset` bodies.

That order keeps the hard part isolated. It also avoids overcommitting to a complex inference algorithm before the operational rewrite is stable.
