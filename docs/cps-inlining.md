# Inlining CPS

An attempt to thrash out some issues with inlining functions after
CPS.

## Non-CPS Basis

To start with let's describe the simpler non-CPS situation as a basis.

Consider 2 functions

```scheme
f = (λ (x) (+ x 1))
g = (λ (y) (* 2 (f y)))
```

`f` is a candidate for inlining into `g`. The process starts with
simple substitution - replace `f` in `g` with the body of `f`:

```scheme
g = (λ (y) (* 2 ((λ (x) (+ x 1)) y)))
```

Followed by β-reduction

```scheme
g = (λ (y) (* 2 ((+ y 1))))
```

No η-reduction required.

## CPS Version

Start with the same two functions, but CPS converted:

```scheme
f = (λ (x kf) (kf (+ x 1)))
g = (λ (y kg) (f y (λ (r) (kg (* 2 r)))))
```

Now we substitute `f` into `g`:

```scheme
g = (λ (y kg) ((λ (x kf) (kf (+ x 1))) y (λ (r) (kg (* 2 r)))))
```

and β-reduce that:

```scheme
g = (λ (y kg) ((λ (r) (kg (* 2 r))) (+ y 1)))
```

and β-reduce again:

```scheme
g = (λ (y kg) (kg (* 2 (+ y 1))))
```

No problems!
