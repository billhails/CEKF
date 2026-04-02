# Types and Type Constructors Analysis

I'm not really happy with how the language handles algebraic data types
and their constructors. This document attempts to describe how it **should**
work, and maybe what the mis-alignments are.

Let's take a simple ADT as an example.

```fn
typedef Maybe(#t) { nothing | just(#t) };
```

This defines the type `Maybe(#t)` and two constructors `nothing`
and `just()`. Since `nothing` takes no arguments it behaves as
a constant.

The trouble with type constructors is that they mess with normal
function definition, If the arguments to a function are type
constructor names, they do not get shadowed like normal variables,
instead they act like constants and containers to be matched
and expanded.

If they appear in the body of a function, they behave like normal
functions and constants. So given the typedef above, there really
should be a letrec-bound function `just` like:

```scheme
(letrec (...
         (just (λ (t) (make-vec 1 t)))
         ...))
```

and a (let-bound?) constant

```scheme
(let (...
      (nothing (make-vec 0))
      ...))
```

or maybe simpler as a letrec bound thunk

```scheme
(letrec (...
         (nothing (λ () (make-vec 1)))
         ...))
```

For efficiency, we really need those `make-vec` to be inlined,
currently that is handled by a specific pass: `inline.c`, but
could it be done as part of β-reduction? I don't think that is
quite the same thing.

We also require that constructors can be curried like any other
function.

## Constructor Inlining as Beta Reduction

It turns out that `inline.c` **is** performing beta reduction,
if we model constructors as the lambdas they logically are.

A constructor like `pair` with tag 0 and arity 2 is semantically:

```scheme
(letrec ((pair (λ (a b) (make-vec 0 a b)))) ...)
```

So `pair(x, y)` is the beta-redex `(λ (a b) (make-vec 0 a b)) x y`,
which beta-reduces to `(make-vec 0 x y)`. The `inline.c` pass does
exactly this in one fused step: it recognizes that the function position
is a constructor (a known function with a trivial body) and substitutes
directly, skipping the intermediate lambda representation.

This is how GHC's simplifier handles it: constructors are ordinary
known functions eligible for inlining, and once inlined the resulting
beta-redex is reduced. The two-phase approach (inline definition then
beta-reduce) yields the same result as our single specialized pass.

Related framings from the literature:

- **Known-call optimization**: when the function in call position is
  statically known, the generic call mechanism can be replaced with a
  direct operation. Constructor application is the simplest case since
  the "body" is just allocation.
- **Delta reduction**: if constructors are treated as primitives rather
  than lambdas, then replacing `Apply(Cons, x, y)` with
  `(make-vec tag x y)` is delta reduction — reducing a known built-in
  to its definition.
- **Partial evaluation / static dispatch**: the constructor identity is
  statically known, so the dispatch to "what does this function do?" can
  be resolved at compile time.

The specialized pass is worth keeping rather than replacing with a
general inliner + beta-reducer: a general beta-reducer on the lambda IR
would need substitution machinery, capture avoidance, and would create
intermediate terms. The `inline.c` pass avoids all of that because the
"body" of a constructor is always the same trivial shape (`make-vec`),
so it can emit the result directly without ever materializing the lambda
or performing substitution. Same semantic result as beta reduction, none
of the machinery.

## Currying

To make curry-able versions of constructors, we emit the eta-expanded
nested lambda form. For a constructor of arity N with tag T:

```scheme
(letrec ((cons (λ (a) (λ (b) (make-vec T a b))))) ...)
```

Applying `cons(x)` yields the closure `(λ (b) (make-vec T x b))`,
and applying that to `y` yields `(make-vec T x y)`.

The existing `inline.c` pass still works for the fully saturated case:
when all N arguments are present at a single call site, it recognizes
the complete application and emits `make-vec` directly, beta-reducing
the entire chain in one step. Partial applications (fewer than N args)
naturally fall through to the normal function application path and
produce closures, which is exactly what currying requires.
