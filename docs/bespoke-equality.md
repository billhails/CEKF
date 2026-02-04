# Bespoke Equality Functons for User Types

Example type that prompted the idea:

```fn
typedef term {
    num(number) |
    var(string) |
    add(term, term) |
    sub(term, term) |
    mul(term, term) |
    div(term, term) |
    pow(term, term)
}
```

The `add` and `mul` operations are commutative, so as far as comparing them for semantic equality, `add(a, b)` is equal to `add(b, a)` etc. We may want this behavior when for example doing constant folding:

```fn
    ...
    add(a, a) { mul(num(2), a) }
    ...
```

Where the two arguments to `add` should match if they are semantically equal, without needing to create cases for every possible arrangement of their components.

It's trivial to write a function to express this for a top-level comparison:

```fn
fn eq_term {
    (num(a), num(a)) |
    (var(a), var(a)) |
    (add(a, b), add(a, b)) |
    (add(b, a), add(a, b)) |
    (sub(a, b), sub(a, b)) |
    (mul(a, b), mul(a, b)) |
    (mul(b, a), mul(a, b)) |
    (div(a, b), div(a, b)) |
    (pow(a, b), pow(a, b)) { true }
    (_, _) { false }
}
```

but to be correct the function must also use its own definition: the unifying `a` and `b` variables need to be compared in the same way.

What makes this useful is that since the function has to override the built-in comparison operations to work in the first place, it should naturally do so for all comparisons of that type.

Assuming a similar definition strategy to `print`, like: `eq type { ...body... }` the type of the function would be constrained to `type -> type -> bool`. When an `eq` comparison is reached in the type checker while the type of the operands is known, the compile-time environment is checked for a bespoke comparator and a call to that is substituted in place of the `eq`.

It would be simple enough to do a to-string and compare strings for structural equality if we needed it occasionally.

## Implementation

Looking at how the parser currently handles analogous `print` tokens in a `let`, it just calls the `defun` parser, passing it a flag `isPrinter = true` which causes the parser to generate a `print$type` name for the function being defined.

We could extend this by replacing the boolean with an enum, values `FUNCTION`, `PRINTER`, `EQUALITY` for now. Then the `defun` would generate an `eq$type` name if given an `EQUALITY` type. We just need to decide what the initiating token is.

On encountering an `eq$type` function, the typechecker would immediately unify its type with `type -> type -> bool`.  On encountering an `eq`, the type checker would inspect its `TcEnv` for an `eq$type` and if so replace the `eq` with a call to that function.  If the type encountered is namespace qualified then the type checker nust inspect that namespace for the equivalent `eq$type` function.

The surface level `==` is a user defined operator that expands to a call to a macro: `x == y` becomes `(op$macro$1 (λ () 1) (λ () 1))` where `(opMacro$1 (λ (x1 x2) (eq (x1) (x2))))`. There is no way currently to get `x == y` to `(eq x y)` - though it would be great if we could, likewise for the other primitives if we plan to do constant folding in a later compiler pass. That's for another day.
