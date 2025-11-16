# Syntax Extension

# Language Level
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

```
switch (x) {
    (1) { "one" }
    ...
}
```

into

```
fn { (1) { "one" } ...} (x)
```

and

```
if (x) { false } else { true }
```

into

```
case (x) {
    (true) { false }
    (false) { true }
}
```

and thence to

```
fn { (true) { false } (false) { true }} (x);
```

We don't need to maintain the separation of `define-syntax` and `syntax-rules`
that scheme has. so a single `syntax` keyword may suffice:

```
syntax (switch) {
    switch (a1, ...)
}
```

That's not really working, maybe explicit BNF approach

```
syntax (switch) {
    SWITCH ::= switch exprs body ::== fn body exprs ;;
    exprs ::= (expr, ...)
    body ::= 
}