# ANF Conversion

Another [blog post](https://matt.might.net/articles/a-normalization/)
from Matt Might describes a "simple" algorithm for doing A-Normalization,
but that algorithm uses continuations which aren't available in C, and so
I'll have to reverse-engineer a solution.

Probably best to do specific cases.

## 1
```scheme
(a (b c)) => (let (t$1 (b c))
                  (a t$1))
```
So on encountering an application, descend into the arguments.
If you find an inner function application, generate a symbol, replace the
inner application with the symbol, and wrap the outer application with a let
binding of that symbol to the inner application.

## 2
```scheme
(a (b c) (d e)) => (let (t$1 (d e))
                        (let (t$2 (a b))
                             (a t$2 t$1)))
```
The same logic applies here, the outer application is wrapped in let bindings.
Worth noting the recursion is like `foldr`, the let bindings are constructed
right to left on the way out of the recursion.

## 3
```scheme
(a (b (c d))) => (let (t$2 (c d))
                      (let (t$1 (b t$1))
                           (a t$1)))
```
Ok in this case we need to be a bit more careful, the let bindings being
extended remain the bindings for the `a` application, we can't just treat
`b` as an outer application or we'd end up with a `let` inside the arguments
to `a`.

It might be possible to perform that transformation in two stages?
```scheme
(a (b (c d))) => (let (t$1 (b (c d))) => (let (t$2 (c d))
                      (a t$1))                (let (t$1 (b t$2))
                                                   (a t$1)))
```

regardless of the details, the plan is to recurse into the leaves of the arguments, replacing
and constructing the let bindings on the way back out. so in processing `(a (b (c d)))` the
`t$2` binding for `(c d)` must be in scope when `(b t$2)` is replaced.

| Recursion | Let Bindings |
| --------- | ------------ |
| `(a (b (c d)))` | `...` |
| `(b (c d))`     | `...` |
| `(c d)`         | `...` |
| `t$1`           | `(let (t$1 (c d)) ... )` |
| `(b t$1)`       | `(let (t$1 (c d)) ... )` |
| `t$2`           | `(let (t$1 (c d)) (let (t$2 (b t$1)) ... ))` |
| `(a t$2)`       | `(let (t$1 (c d)) (let (t$2 (b t$1)) (a t$2)))` |

or the other way, where we recurse on the bindings

| Expression | Let Bindings |
| --------- | ------------ |
| `(a (b (c d)))` | `...` |
| `(a t$1)`       | `(let (t$1 (b (c d))) (a t$1))` |
| `(a t$1)`       | `(let (t$2 (c d)) (let (t$1 (b t$2)) (a t$1)))` |

The second approach seems a little more intuitive as we're prepending to the let bindins and the
body of the call is fully substituted in step 1

so informally:
1. Walk the application, replacing any cexp with a fresh
   symbol and binding that symbol to the cexp.
2. Iteratively walk each application bound in step 1.
3. Stop when the iteration binds no new variables.

Not sure that's quite right though, as nests of primitive applications are fine as aexps, but then
again the recursion is not limited to just the top level of an application.

In terms of types, the cexp starts out as a LamExp which is being transformed into an Exp.
The result of walking a LamExp application should be an Exp, with variables substituted,
but the substitutions temporarily bound before being assigned to let bindings are still LamExps.
Let bindings are only constructed from sub-applications once the sub-application is translated
and the next iteration of bindings prepared for transformation.
