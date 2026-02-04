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

The `add` and `mul` operations are commutative, so as far as comparing them for semantic equality, `add(a, b)` is equal to `add(b, a)` etc. It's trivial to write a function to express this for a top-level comparison:

```fn
fn eq_term {
    (num(a), num(a)) |
    (var(a), var(a)) |
    (add(a, b), add(a, b)) |
    (add(a, b), add(b, a)) |
    (sub(a, b), sub(a, b)) |
    (mul(a, b), mul(a, b)) |
    (mul(a, b), mul(b, a)) |
    (div(a, b), div(a, b)) |
    (pow(a, b), pow(a, b)) { true }
    (_, _) { false }
}
```

but to be correct the function must also use its own definition: the unifying `a` and `b` variables need to be compared in the same way.

What makes this useful is that since the function has to override the built-in comparison operations to work in the first place, it should naturally do so for all comparisons of that type.

Assuming a similar definition strategy to `print`, like: `eq type { ...body... }` the type of the function would be constrained to `type -> type -> bool`. When an `eq` comparison is reached in the TPMC while the type of the operands is known, the compile-time environment is checked for a bespoke comparator and a call to that is substituted in place of the `eq`.

It would be simple enough to do a to-string and compare strings for structural equality if we needed it occasionally.
