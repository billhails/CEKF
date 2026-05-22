# Delimited control CPS sketch for `fn/rewrite`

This note defines a prototype rewrite target for `reset` and `shift` on the `fn/rewrite` CPS path.

The goal is not to commit to final surface syntax or runtime representation yet. The goal is to pin down rewrite rules precise enough that:

- they can be prototyped in `fn/rewrite/cps.fn`
- they can be validated against small examples in `fn/rewrite/samples.fn`
- they can later inform the C implementation on the `--target-b` / `--target-c` path

## Scope

This sketch assumes:

- `shift/reset`, not `control/prompt`
- captured continuations behave as ordinary reusable values in the prototype
- delimited control only affects the ordinary success continuation
- `amb` remains a later, separate pass exactly as it is today

That last point matches the current rewrite and C pipelines: first CPS, then `amb`/failure continuation plumbing.

The rewrite prototype should model the standard multi-shot semantics by default. That keeps the laws and worked examples simple, and it matches the ordinary reading of a captured continuation as a function value that may be called more than once. If a later runtime wants one-shot continuations for performance or representation reasons, that should be introduced as a separate implementation restriction rather than baked into the prototype semantics.

## IR extension

The rewrite prototype will need two new forms in both `expr.fn` and `minexpr.fn`:

```text
reset_expr(expr)
shift_expr(expr)
```

Suggested surface syntax for the prototype parser:

```text
(reset e)
(shift f)
```

The operand of `shift` should evaluate to a function that receives the captured continuation as its first ordinary argument.

## Why the current CPS transform is not enough

`fn/rewrite/cps.fn` currently mirrors the C CPS pass closely:

- `M` rewrites lambdas to accept one continuation argument
- `T_k` converts into higher-order CPS
- `T_c` converts against an explicit continuation expression

That works for `call/cc`, because `call/cc` captures the whole current continuation already represented by the existing CPS pass.

It is not enough for delimited control, because `reset` introduces a boundary that the transform itself must preserve. If we wait until after ordinary CPS, the boundary is no longer explicit in the right way.

## Delimited CPS judgment

Replace the one-continuation CPS judgment with a two-layer judgment:

```text
T^d_k(e, k, m)
T^d_c(e, c, m)
M^d(e)
```

where:

- `k` is the current continuation up to the nearest enclosing `reset`
- `m` is the meta continuation outside that `reset`
- `c` is the first-order representation of `k`, as in the current `T_c`

Intuitively:

- ordinary computation returns to `k`
- completing a delimited region returns through `m`
- `shift` captures `k`, not `m`

## Calling convention

The prototype should generalize the current CPS calling convention from:

```text
lambda(args, body)  =>  lambda(args @@ [k], ...)
```

to:

```text
lambda(args, body)  =>  lambda(args @@ [k, m], ...)
```

The generated continuation `k` itself becomes a two-argument function:

```text
k(v, m)
```

This is the most direct generalization of the existing rewrite code, because applications can still append continuation parameters at the end.

## Atomic translation

For atomic expressions, the current computation just calls the local continuation:

```text
T^d_c(a, c, m) = c(M^d(a), m)
```

The meta continuation is threaded through unchanged.

## Lambda translation

The atomic translation of lambdas becomes:

```text
M^d(λ(args). body) = λ(args, k, m). T^d_c(body, k, m)
M^d(x)             = x
```

This is the direct delimited analogue of the current `M` in `fn/rewrite/cps.fn`.

## Helper continuations

Two helper continuations are useful in the prototype.

The first is the ordinary bridge from higher-order `T^d_k` to first-order `T^d_c`:

```text
kToC^d(k) = λ(rv, m). k(rv, m)
```

The second is the empty continuation for the inside of a fresh `reset`:

```text
idk = λ(v, m). m(v)
```

`idk` means: there is no more local work inside this reset; hand the value to the meta continuation for the reset.

## Ordinary forms

Every existing rule in `cps.fn` lifts by threading `m` through unchanged.

Examples:

```text
T^d_c((f e1 ... en), c, m)
  = T^d_k(f, λ(sf, m1).
      T^d_{s,k}(e1 ... en,
        λ(sargs, m2). sf(sargs @@ [c, m2]),
        m1),
      m)
```

```text
T^d_c((if e1 e2 e3), c, m)
  = T^d_k(e1,
      λ(a1, m1). if a1 then T^d_c(e2, c, m1) else T^d_c(e3, c, m1),
      m)
```

```text
T^d_c((begin e1 ... en), c, m)
  = T^d_k(e1,
      λ(_, m1). T^d_c((begin e2 ... en), c, m1),
      m)
```

The important invariant is simple: unless a rule is specifically about `reset` or `shift`, it preserves the current delimiter structure and only threads `m` onward.

## Reset rule

`reset` installs a fresh local continuation boundary.

The body of the reset runs with:

- a fresh empty local continuation `idk`
- a meta continuation that resumes the surrounding local continuation

Formally:

```text
T^d_c(reset e, c, m)
  = T^d_c(e,
      idk,
      λ(v). c(v, m))
```

Read this as:

- inside the reset, the current local continuation starts empty
- when the reset body finishes, its result is handed back to the continuation outside the reset

This rule is the delimiter. It is the main reason this transform must be built into CPS itself rather than bolted on later.

## Shift rule

`shift` captures the current local continuation `c`, packages it as a function value, and removes that local continuation from the remainder of the body.

The captured continuation must re-enter the saved context under a fresh delimiter. In this two-layer CPS formulation, that means the reified continuation receives its own caller continuation and meta continuation.

Define the captured continuation as:

```text
capture(c) = λ(x, k2, m2). c(x, λ(v). k2(v, m2))
```

Intuition:

- `c` is the saved local continuation up to the nearest reset
- calling the captured continuation with `x` plugs `x` into that saved context
- once that reinstated context reaches the reset boundary, control continues with the caller's current continuation `k2`

The `shift` rule is then:

```text
T^d_c(shift f, c, m)
  = T^d_k(f,
      λ(sf, m1). sf(capture(c), idk, m1),
      m)
```

Read this as:

- evaluate `f`
- pass it the captured continuation
- run the body of `f` with the current local continuation removed (`idk`)
- if the shift body returns normally, it exits to the enclosing reset via `m`

This is the key distinction from `call/cc`: only the current delimited continuation is captured.

## Call/cc in the unified transform

Once the prototype is unified into a single two-layer CPS transform, `call/cc` should remain expressible directly rather than through a separate legacy path.

In the unified transform, `call/cc` captures the whole current continuation, which in this representation is the pair `(c, m)`.

The captured continuation can therefore be represented as:

```text
captureWhole(c, m) = λ(x, k2, m2). c(x, m)
```

It ignores the continuation at the call site because invoking a `call/cc` continuation aborts the current continuation and reinstalls the captured one.

The corresponding rule is:

```text
T^d_c(call/cc f, c, m)
  = T^d_k(f,
      λ(sf, m1). sf(captureWhole(c, m1), c, m1),
      m)
```

This keeps `call/cc` and `shift/reset` in a single reference transform while preserving their semantic difference:

- `call/cc` captures the whole continuation `(c, m1)`
- `shift` captures only the local continuation `c`

## Validation laws

The rewrite prototype should validate against these laws first.

These laws assume the captured continuation can be invoked multiple times. That is intentional: the rewrite prototype is the semantic reference point. A later one-shot runtime would need either a dynamic restriction on repeated invocation or a separate user-facing contract.

### Basic reset laws

```text
reset(v)  ==>  v
reset(reset(e))  ==>  reset(e)
```

### Shift/reset law

For an evaluation context `E` that does not cross `reset`:

```text
reset(E[shift k. e])
  ==> reset(e[(λx. reset(E[x])) / k])
```

This is the main substitution law the prototype should preserve.

### Worked examples

These are good first samples for `fn/rewrite/samples.fn` once parsing exists:

```text
(reset 1)                                   ==> 1
(reset (+ 1 (shift (λ (k) 2))))             ==> 2
(reset (+ 1 (shift (λ (k) (k 2)))))         ==> 3
(reset (+ 1 (shift (λ (k) (+ (k 2) (k 3)))))) ==> 7
(+ 1 (reset (shift (λ (k) 2))))             ==> 3
```

Those examples are small enough to inspect through the prototype pipeline stage-by-stage.

## Prototype plan in `fn/rewrite`

The minimal prototype sequence should be:

1. Extend `fn/rewrite/expr.fn` with `reset_expr` and `shift_expr`, printer support, and parser support.
2. Extend `fn/rewrite/minexpr.fn` with the same two forms and printer support.
3. Extend `fn/rewrite/desugar.fn` to lower the new `expr` forms directly to `minexpr`.
4. Unify `fn/rewrite/cps.fn` around a single two-layer CPS transform.
5. Reuse the existing passes after CPS without changing their role.
6. Add small samples in `fn/rewrite/samples.fn` and inspect output through `fn/rewrite/test_harness.fn`.

The recommended implementation strategy in `cps.fn` is:

- generalize `T_k` / `T_c` to thread the extra meta continuation
- generalize lambda translation so all CPS-transformed lambdas accept `k` and `m`
- express `call/cc`, `reset`, and `shift` inside that single transform

The temporary forked prototype can be useful while the equations are still in flux, but the reference implementation should converge back to one transform before informing the C implementation.

## Interaction with `amb`

Do not fold `amb` into the delimited-control design initially.

The current architecture already treats success and failure control separately. The prototype should keep that split:

- delimited CPS first, producing success-side control plumbing
- `amb.fn` afterwards, appending failure continuation structure

If later work wants delimited failure control too, that should be a separate design step.

## What to validate in the prototype

There are two distinct validation targets.

### Structural validation

Inspect the generated CPS for these invariants:

- transformed lambdas now accept `k` and `m`
- ordinary applications append both continuation parameters
- `reset` introduces `idk`
- `shift` packages the current continuation with `capture(c)`

### Behavioral validation

Use small samples and compare them against the laws above.

For the first round, it is enough to confirm:

- `shift` can discard the surrounding local context
- invoking the captured continuation re-enters the saved context
- that re-entry is delimited and returns to the call site of the captured continuation

## Non-goals for the first prototype

These should stay out of the first cut:

- prompt tags
- `control`/`prompt` semantics
- runtime enforcement of one-shot use
- CEKF runtime support
- user-facing generator syntax

If the prototype validates, the next step is to decide whether to expose raw `shift/reset` surface syntax or to build `yield` as the user-facing feature on top of the same machinery.
