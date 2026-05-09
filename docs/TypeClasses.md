# Type Classes

Taking inspiration from [How to make ad-hoc polymorphism less ad hoc](How_to_make_ad_hoc_polymorphism_less_ad_hoc.pdf),
We'll start by inventing some new syntax then show how to translate
that back to existing F♮ syntax which implements type classes.

CAVEAT - probably won't work.

```fn
typeclass EQ(#t) {
    eq: #t -> #t -> bool;
    ne: #t -> #t -> bool;
}

instance EQ(_) { // default behaviour
    fn eq {
        (a, a) { true } // need to be careful not to recurse
        (_, _) { false }
    }
    fn ne(a, b) { not eq(a, b) }
}

typedef exp(#t) { val(#t) | app(exp(#t), exp(#t)) }
instance EQ(exp) {
    fn eq {
        (val(a), val(a)) { true }
        (app(a, b), app(a, b)) { true }
        (app(a, b), app(b, a)) { true } // this is why we need them
        (_, _) { false }
    }
}
```

Translation

```fn
fn eq (#(e, n)) { e }
fn ne (#(e, n)) { n }
EqExp = #(
    fn {
        (val(a), val(a)) { true }
        (app(a, b), app(a, b)) { true }
        (app(a, b), app(b, a)) { true }
        (_, _) { false }
    },
    fn (a, b) { not eq(EqExp, a, b) } // is this legal?
);
```

What about `map`?

```fn
typeclass FUNCTOR(t) { // N.B. not #t
    fmap: (#u -> #v) -> t(#u) -> t(#v)
}

instance FUNCTOR(list) {
    fmap: map
}

typedef maybe(#t) { nothing | just(#t) }
instance FUNCTOR(maybe) {
    fn fmap {
        (_, nothing) { nothing }
        (f, maybe(v)) { maybe(f(v)) }
    }
}

typedef tree(#t) { leaf | branch(tree(#t), #t, tree(#t)) }
instance FUNCTOR(tree) {
    fn fmap {
        (_, leaf) { leaf }
        (f, branch(a, b, c)) { branch(fmap(f, a), f(b), fmap(f, c)) }
    }
}
```
