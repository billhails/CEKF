# Delimited control typing sketch for `fn/rewrite`

This note complements [delimited-cps.md](delimited-cps.md).

That note fixes a candidate operational account for `reset` and `shift` in the rewrite CPS prototype. This note asks a narrower question: what static typing discipline can describe that behavior safely, especially when `shift` appears inside a named function and the matching `reset` is only present at some later call site.

The short answer is that plain Hindley-Milner types over ordinary arrows are not enough. A `shift` occurrence does not just compute a result of some type $\tau$. It also changes the answer type of the surrounding delimited computation. Once `shift` can sit inside a named function, that control behavior becomes latent in the function type and cannot be reconstructed only from the local syntax at the eventual call site.

For the real compiler pipeline, this judgment belongs on the earlier `expr.fn` / `src/lambda.yaml`-like structures. Desugaring to `minexpr.fn` / `src/minlam.yaml` happens shortly afterward, and the majority of later transforms then operate on that reduced core, so the delimited-control guarantees need to exist before that handoff.

## Goal

The goal of this sketch is not to commit to a full inference algorithm yet. The goal is to pin down a type judgment precise enough that:

- it explains why `shift` and `reset` are co-dependent
- it handles named functions whose bodies contain `shift`
- it matches the intended two-layer CPS reading in [delimited-cps.md](delimited-cps.md)
- it makes clear which part is easy to prototype and which part is likely to need annotations or a value restriction

## Why ordinary HM is not enough

The current rewrite inference prototype in [infer.fn](../infer.fn) only infers ordinary expression types and ordinary arrow types.

That works for `call/cc`, because `call/cc` can be given a self-contained type schema:

$$
((a \to b) \to a) \to a
$$

The direct-style type of `shift` is different. Its meaning depends on the nearest enclosing `reset`, because `shift` captures only the continuation up to that `reset`. That means the source typing judgment needs to track not just the type of the local subexpression, but also how that subexpression changes the answer type of the enclosing `reset` body.

The named-function case makes this unavoidable:

```text
let f = λx. shift k. e
in reset (C[f v])
```

Operationally, the `shift` in `f` captures the continuation from the call site `C[□]`, not from the definition site of `f`.

Statically, that means `f` cannot be assigned a plain type like $\sigma \to \tau$. The fact that calling `f` requires an enclosing `reset` and may modify the answer type has to appear in the type of `f` itself.

## Candidate judgment

Use an answer-type-modifying judgment:

$$
\Gamma \vdash e : \tau [\alpha\mapsto\beta]
$$

The conceptual point is simple: an expression has both a value type and a control behavior.
The value type is $\tau$. The control behavior says what answer type the nearest enclosing `reset` body starts with and what answer type it ends with.

So:

- pure expressions have the form $\Gamma \vdash e : \tau [\alpha\mapsto\alpha]$
- `shift` is the construct that can change $\alpha$ to a different $\beta$
- `reset` hides that internal answer-type change and restores an ordinary outer judgment

This is the standard answer-type-modification view of `shift/reset`, adapted to the rewrite prototype.

## Function types

Functions whose bodies may use `shift` need types that record latent
control behavior.  A function type has to describe not just the value
it returns, but also the answer-type change its body may perform when
called under `reset`.

$$
\sigma \xrightarrow[\lbrack {\alpha\mapsto\beta} \rbrack]{} \tau
$$

Read this as a function type $\sigma \to \tau$ that carries an
answer-type change $[\alpha\mapsto\beta]$.

This is important for named functions: a function that contains `shift` carries the answer-type change in its own type, so a later caller under `reset` can instantiate that latent control behavior.

## Core rules

The full language would need rules for all forms, but the core picture can be fixed with just values, lambdas, application, `reset`, and `shift`.

### Variables and constants

Pure expressions preserve the ambient answer type:

$$
\frac{x : \tau \in \Gamma}{\Gamma \vdash x : \tau [\alpha\mapsto\alpha]}\texttt{[VAR]}
$$

and similarly for constants.

### Lambda

Building a function value is pure, even when calling that function later may not be:

$$
\frac{\Gamma, x : \sigma \vdash e : \tau [\alpha\mapsto\beta]}
     {\Gamma \vdash \lambda x.e : \sigma \xrightarrow[\lbrack \alpha\mapsto\beta \rbrack]{} \tau [\gamma\mapsto\gamma]}\texttt{[LAMBDA]}
$$

The important distinction is between defining a function and calling it. Defining the lambda is pure. The possible answer-type change is stored on the function type and only becomes relevant when the function is later applied.

### Application

Application composes the latent answer-type change from the function body with the current context:

$$
\frac{\Gamma \vdash f : \sigma \xrightarrow[\lbrack \alpha\mapsto\beta \rbrack]{} \tau [\gamma\mapsto\delta] \qquad
     \Gamma \vdash a : \sigma [\delta\mapsto\alpha]}
     {\Gamma \vdash f\ a : \tau [\gamma\mapsto\beta]}\texttt{[APPLY]}
$$

The easiest way to read this rule is as a left-to-right call-by-value decomposition.

First evaluate `f`, which may already change the ambient answer type from $\gamma$ to $\delta$. Then evaluate the argument `a`, which takes that intermediate answer type from $\delta$ to $\alpha$. Only after both are available do we enter the function body, whose latent behavior changes the answer type from $\alpha$ to $\beta$. The whole application therefore changes the answer type from $\gamma$ to $\beta$.

This is the point where a named function with latent control effect becomes active. The effect does not come from re-reading the syntax of the lambda at the call site. It comes from the latent $[\alpha\mapsto\beta]$ annotation on the function value.

If `f` is pure, this collapses to the ordinary rule because $\alpha = \beta$.

### Reset

`reset` delimits answer-type change and returns an ordinary expression result to its surrounding context:

$$
\frac{\Gamma \vdash e : \tau [\tau\mapsto\sigma]}
     {\Gamma \vdash \operatorname{reset}\ e : \sigma [\alpha\mapsto\alpha]}\texttt{[RESET]}
$$

Inside the `reset`, the body starts in a context expecting a $\tau$ and may change the answer type from $\tau$ to $\sigma$. The `reset` itself discharges that internal change and is pure to the outside world.

The repeated $\tau$ on the left is intentional. This rule is about typing the whole body of a fresh `reset`, where the initial local continuation is the top-level hole of that body. Subexpressions inside the body can still have other value types. The point is only that, at the boundary introduced by `reset`, the pre-`shift` answer type is the type that the body would deliver if no answer-type change occurred.

This matches the operational idea from [delimited-cps.md](delimited-cps.md): inside a fresh `reset`, the local continuation starts at `idk`, and once the body completes, control returns to the outer meta continuation as an ordinary result.

### Shift

`shift` captures the local continuation up to the nearest `reset`.

The continuation variable gets the ordinary function type from the hole type to the pre-shift answer type:

$$
\frac{\Gamma, k : \tau \to \alpha \vdash e : \beta [\beta\mapsto\beta]}
     {\Gamma \vdash \operatorname{shift}\ k.e : \tau [\alpha\mapsto\beta]}\texttt{[SHIFT]}
$$

Conceptually, the surrounding context is waiting for a value of type $\tau$, and if that context were resumed normally it would produce answer type $\alpha$. Inside the body of `shift`, that suspended context is available as an ordinary function $k : \tau \to \alpha$. Because the local continuation has been removed, the `shift` body is checked as an ordinary computation producing $\beta$. The whole `shift` therefore changes the answer type from $\alpha$ to $\beta$.

This rule is the source-level counterpart of the CPS rule where `shift` passes a reified local continuation to its operand and resumes with `idk` inside the body.

### Other Rules

Most of the omitted rules are structurally transparent to answer-type change. They do not introduce new control behavior; they only preserve or thread the ambient $[\alpha\mapsto\beta]$ information according to evaluation order. For example, arithmetic and tuple construction are pure once their subexpressions are evaluated, so they just pass through whatever answer-type change those subexpressions already induce. A `let x = e_1 in e_2` rule would similarly thread the answer type from `e_1` into `e_2`, much like sequencing. The non-transparent cases are the control operators themselves, especially `reset` and `shift`, because they are the forms that delimit or reify continuation structure.

## Named function case

Now consider the case that motivated this note:

```text
let f = λx. shift k. e
in reset (C[f v])
```

The body of `f` is checked under some answer-type change $[\alpha\mapsto\beta]$, so the lambda gets a type of the form:

$$
f : \sigma \xrightarrow[\lbrack \alpha\mapsto\beta \rbrack]{} \tau
$$

That is the static record of the latent control behavior.

At the call site inside `reset`, the ambient context `C[□]` inside that `reset` determines the specific instantiation of $\alpha$ and $\beta$. The caller does not discover the control effect by re-inspecting the syntax of `f`. It learns it from the type of `f`.

This is the essential reason a syntax-local rule for `shift` is not enough once `shift` can appear behind a variable reference.

## Small examples

### Pure use of the captured continuation

```text
reset (1 + shift k. k 2)
```

Operationally, the body captures the pending `1 + □` continuation and immediately reinstalls it with `2`, so the whole reset behaves like `reset (1 + 2)`.

The hole type is `Int`, and resuming the captured continuation also yields `Int`, so the answer type does not change. This is the easy case where the internal answer-type change is
$[\operatorname{Int}\mapsto\operatorname{Int}]$.

### Discarding the local continuation

```text
reset (1 + shift k. 2)
```

Operationally, the `1 + □` continuation is captured and then discarded. The body returns directly from the enclosing `reset`, so the pending addition never resumes.

The surrounding local continuation still expects an `Int`, so the left side of the answer-type change is `Int`. But the shift body returns `2` directly from the reset body, so the right side is also `Int` in this particular example.

Operationally the local continuation is discarded. Typing alone does not force `k` to be used.

### Named helper

```text
let f = λx. shift k. k x
in reset (1 + f 2)
```

Operationally, `f` captures the caller's pending `1 + □` continuation, not anything from its definition site. That is exactly why the effect has to travel with the function value.

The important part is not the final result type. The important part is that `f` cannot be typed as only `Int -> Int`. Its type must include the answer-type behavior of its body, even though the actual delimiter is only introduced around `f 2`.

## Generalization and safety

This is where the design gets subtle.

Once function types mention answer-type variables, unrestricted HM-style let-generalization becomes questionable. A binding like:

```text
let f = λx. shift k. e
```

may appear to want a polymorphic type scheme over $\alpha$ and $\beta$. In the literature, unrestricted answer-type polymorphism interacts badly with ordinary let-polymorphism unless the language adopts additional restrictions.

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

- $\tau$ is the type expected by the captured local continuation hole
- $\alpha$ is the answer type produced if that local continuation runs normally
- $\beta$ is the answer type produced by the `shift` body after the local continuation has been removed

So the answer-type change $[\alpha\mapsto\beta]$ is the type-level shadow of the operational split between local continuation and meta continuation.

The application rule above mirrors the ordinary left-to-right threading of `k` and `m` in the CPS note. The `reset` rule mirrors the installation of `idk` plus a fresh meta continuation. The `shift` rule mirrors the point where the local continuation is reified as a value and the current local continuation is replaced by `idk`.

## Recommended first implementation target

For the rewrite prototype and the real compiler, the pragmatic staged plan is:

1. Keep this note as the direct-style semantic reference.
2. Implement delimited typing at the existing early source-level typechecking stage over the `expr.fn` / `src/lambda.yaml`-like structures, before desugaring to `minexpr.fn` / `src/minlam.yaml`, ANF, or any target-specific CPS split.
3. Use the delimited CPS transform as a secondary validation oracle in the rewrite prototype, not as the place where typing first becomes valid.
4. If inference is too ambitious at first, start with explicit annotations on controlful named functions and on `reset` bodies, together with monomorphic answer-type variables.

That order preserves guarantees for earlier transforms, works for both the ANF-based CEKF path and the target-b path, and still lets the CPS note act as an operational cross-check.

## Worked example

This example makes the difference between the local hole type and the enclosing answer type explicit:

```text
reset (tostring (1 + shift k. true))
```

The captured evaluation context around `shift` is:

```text
tostring (1 + □)
```

So the hole expects an `Int`, and if resumed normally the surrounding `reset` body would produce a `String`. That means the captured continuation has type:

$$
k : \operatorname{Int} \to \operatorname{String}
$$

The body of the `shift` is just `true`, so under that continuation binding it is a pure `Bool` computation:

$$
\Gamma, k : \operatorname{Int} \to \operatorname{String}
\vdash
\operatorname{true}
:
\operatorname{Bool}
[\operatorname{Bool}\mapsto\operatorname{Bool}]
$$

Applying the `shift` rule gives:

$$
\Gamma \vdash
\operatorname{shift}\ k.\operatorname{true}
:
\operatorname{Int}
[\operatorname{String}\mapsto\operatorname{Bool}]
$$

This is the key judgment. The `shift` expression sits in a place expecting an `Int`, but evaluating it changes the enclosing `reset` answer type from `String` to `Bool`.

The surrounding pure contexts preserve that same answer-type modification, so:

$$
\Gamma \vdash
1 + \operatorname{shift}\ k.\operatorname{true}
:
\operatorname{Int}
[\operatorname{String}\mapsto\operatorname{Bool}]
$$

and then:

$$
\Gamma \vdash
\operatorname{tostring}(1 + \operatorname{shift}\ k.\operatorname{true})
:
\operatorname{String}
[\operatorname{String}\mapsto\operatorname{Bool}]
$$

Finally, `reset` discharges the internal answer-type change:

$$
\Gamma \vdash
\operatorname{reset}(\operatorname{tostring}(1 + \operatorname{shift}\ k.\operatorname{true}))
:
\operatorname{Bool}
[\alpha\mapsto\alpha]
$$

So the whole expression has type `Bool`.

This example is useful because the three roles stay visibly distinct:

- the local hole type at the `shift` site is `Int`
- the pre-`shift` answer type inside the `reset` is `String`
- the post-`shift` answer type is `Bool`

Operationally, the pending continuation $\lambda n.\,\operatorname{tostring}(1+n)$ is captured and discarded, so the body `true` returns directly from the nearest `reset`.
