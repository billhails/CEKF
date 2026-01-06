# Syntax Extension

## Language Level

Thinking to model this on the scheme `define-syntax`/`syntax-rules` system, a scheme
example is

```scheme
(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ e) e)
    ((_ e1 e2 e3 ...)
     (let ((t e1)) (if t #t (or e2 e3 ...))))))
```

I'd like to at least be able to transform

```fn
switch (x) {
    (1) { "one" }
    ...
}
```

into

```fn
fn { (1) { "one" } ...} (x)
```

and

```fn
if (x) { false } else { true }
```

into

```fn
case (x) {
    (true) { false }
    (false) { true }
}
```

and thence to

```fn
fn { (true) { false } (false) { true }} (x);
```

We don't need to maintain the separation of `define-syntax` and `syntax-rules`
that scheme has. so a single `syntax` keyword may suffice:

```fn
syntax (switch) {
    switch (a1, ...)
}
```

That's not really working, maybe explicit BNF approach

```fn
syntax (switch) {
    SWITCH ::= switch exprs body ::== fn body exprs ;;
    exprs ::= (expr, ...)
    body ::= 
}
```
