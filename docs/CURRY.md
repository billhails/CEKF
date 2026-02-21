# Currying as a Transform

To avoid building support for currying into the VM, it is possible to mechanically translate an IR into a curried form, and in fact it's not even difficult, the question is whether the result is acceptable from an efficiency point of view. Anyway

$$
\begin{align*}
\text{curry}\lgroup\lambda.x\;a\rgroup &=
    \lambda.x\;\text{curry}\lgroup a\rgroup
\\
\text{curry}\lgroup\lambda.x.y\dots\;a\rgroup &=
    \lambda.x\;\text{curry}\lgroup\lambda.y\dots\;a\rgroup
\\
& \text{and}
\\
\text{curry}\lgroup(x\;a)\rgroup &=
    (\text{curry}\lgroup x\rgroup\;\text{curry}\lgroup a\rgroup)
\\
\text{curry}\lgroup(x\;a\;b\dots)\rgroup &=
    \text{curry}\lgroup\big((x\;\text{curry}{\small(a)})\;b\dots\big)\rgroup
\end{align*}
$$

That translates quite naturally into a pattern-matching language like F♮

```fn
(lambda([x], a))    { lambda([x], curry(a)) }
(lambda(x @ xs, a)) { lambda([x], curry(lambda(xs, a))) }
// and
(apply(x, [a]))     { apply(curry(x), [curry(a)]) }
(apply(x, a @ as))  { curry(apply(apply(x, [curry(a)]), as)) }
```

See [curry.fn](../fn/rewrite/curry.fn) for a full reference implementation.

The upshot of this is that

$$
\begin{align*}
\mathtt{(\lambda\;(a\;b\;c)\; x)} &\mapsto \mathtt{(\lambda\;(a)\;(\lambda\;(b)\;(\lambda\;(c)\;x)))}
\\
&\text{and}
\\
\mathtt{(a\;b\;c\;d)} &\mapsto \mathtt{(((a\;b)\;c)\;d)}
\end{align*}
$$

This also allows for the possibility of currying primitives, but that would mess up constant folding, so in the reference implementation application of primitives is handled separately.

Built-in application cannot be curried either. There is a trick in the parser that wraps each built-in in a normal, curry-able function. Remember that the CEKF VM recognizes curried applications, but if we want to target LLVM we are probably better off not leaving it until run-time. Anyway that will probably require a new `minBuiltInApp` alongside `minPrimApp` in `minlam.yaml` to distinguish built-in applications.

Asked cursor for opinions and suggestions about optimizing:

## Uncurrying Optimization in Practice

This can be done on the S-expression IR, and it is often a good place to do a first pass.
The pass needs two facts:

1. A stable function identity for direct calls (for example a lambda-binding symbol).
2. An arity table for known functions.

Given those, treat curried application chains as a recoverable structure and rewrite them when safe.

### Pass position

A practical ordering is:

1. curry normalization
2. CPS conversion
3. closure conversion / free-variable analysis
4. uncurrying + worker/wrapper specialization
5. LLVM lowering

If you prefer, a lighter version can run on S-expr IR before CPS. In practice, keeping a stronger pass after CPS usually gives better call-graph facts.

### Core rewrite rules

Assume:

- `arity(f) = n` for known `f`
- `flattenApp(e)` converts `(((f a) b) c)` into `(f, [a, b, c])`

Then for each application chain:

```text
(h, args) = flattenApp(expr)
```

#### Saturated direct call

If `h` is known and `len(args) == arity(h)`, rewrite to a direct worker call:

```text
(((f a1) ... ) an)  =>  call$arity_n(f$arity_n, [a1, ..., an])
```

No PAP allocation is needed.

#### Undersaturated call (partial application)

If `h` is known and `len(args) < arity(h)`, build a PAP (or closure) that stores supplied args:

```text
((f a1) ... ak)  =>  pap(f$arity_n, [a1, ..., ak], missing=n-k)
```

This preserves language semantics while avoiding nested lambda wrappers at runtime.

#### Oversaturated call

If `h` is known and `len(args) > arity(h)`, split into head call plus tail apply:

```text
args = prefix ++ suffix
len(prefix) = arity(h)

((((f a1) ... ) an) b1 ... bm)
    => applyMany(call$arity_n(f$arity_n, prefix), suffix)
```

If the head result is statically known to be callable with fixed arity, repeat specialization on `applyMany`.

### Worker/wrapper shape

For source-level `f` with arity `n`:

- keep a curried wrapper `f` for semantic compatibility
- generate a worker `f$arity_n(env, a1, ..., an)` for direct calls

Wrapper sketch:

```text
f = \a1 -> \a2 -> ... -> \an -> f$arity_n(env, a1, ..., an)
```

Optimized call sites use `f$arity_n` directly whenever saturated.

### What to keep out of the pass

- `minPrimApp` should stay separate so constant folding remains straightforward.
- Proposed `minBuiltInApp` should stay separate for direct runtime/LLVM lowering.

Treat both as non-curried, fixed-arity call forms.

### A concrete S-expr style matcher

Pseudo-rules in the style of this document:

```fn
// gather all args from left-associated apply nodes
(applyChain(e)) {
    // returns (head, [a1, ... ak]) for (((head a1) ...) ak)
}

(optApply(e)) {
    let (h, args) = applyChain(e)
    match h {
        (knownFun(name, n)) if len(args) == n -> directCall(name, args)
        (knownFun(name, n)) if len(args) <  n -> makePap(name, args, n - len(args))
        (knownFun(name, n)) if len(args) >  n -> {
            let prefix = take(args, n)
            let suffix = drop(args, n)
            applyMany(directCall(name, prefix), suffix)
        }
        _ -> rebuildCurried(h, args)
    }
}
```

That gives you a direct path from curried source semantics to efficient LLVM call sites without changing front-end behavior.

## Separate Compilation and ABI

For precompiled libraries, uncurrying should use a fixed calling convention rather than exporting many arity variants.

For each exported source function `f` with semantic arity `n`:

1. Export a curried wrapper `f` (language-level entry).
2. Export one canonical worker `f$arity_n` (compiler-level direct call target).
3. Use a standard PAP/runtime apply protocol for undersaturated and oversaturated calls.

### ABI sketch

At module boundaries, treat the following as the stable contract:

- `f` is the exported curried wrapper symbol.
- `f$arity_n` is the exported canonical worker symbol for semantic arity `n`.
- PAP/runtime apply entry points are exported runtime symbols, not per-function ABI variants.

Pseudotype signatures:

```text
f            : Env -> Value -> Value
f$arity_n    : Env -> Value -> ... -> Value   // n value arguments
papMake      : WorkerSym -> CapturedArgs -> MissingCount -> Value
applyMany    : Value -> [Value] -> Value
```

Arity-3 example (`f` has semantic arity 3):

```text
f            : Env -> Value -> Value
f$arity_3    : Env -> Value -> Value -> Value -> Value

// saturated
(((f a) b) c)      => call f$arity_3(env, a, b, c)

// undersaturated
((f a) b)          => papMake(f$arity_3, [a, b], 1)

// oversaturated
((((f a) b) c) d)  => applyMany(call f$arity_3(env, a, b, c), [d])
```

Additional specialized workers are optimization artifacts and should remain internal symbols.

IR mapping note:

- `minApply` represents normal curried function application and remains the generic fallback form.
- Saturated direct-worker rewrites replace eligible `minApply` chains with a direct-call IR form during lowering.
- `minPrimApp` remains a separate fixed-arity form so primitive constant folding is unaffected.
- `minBuiltInApp` (proposed) should remain a separate fixed-arity form for built-in runtime calls.
- PAP creation and `applyMany` behavior can be represented as explicit runtime-call nodes in lowering, not as new source-level surface syntax.

This avoids code-size blowup, keeps ABI stable across modules, and still allows later in-module specialization when profitable.
