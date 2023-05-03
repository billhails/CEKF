# Lexical Addressing

**Static analysis** is a phase prior to evaluation that replaces variables with their location in the environment.

We don't know what the *values* of variables will be at run-time, but we can work out their *locations*.

We do this with the help of a compile-time environment (`ct-env`) that just stores variables, not values.

For example consider this `let` expression

```scheme
(let ((x 10)) x)
```

If we were evaluating that naively we would be doing something like

```scheme
(define (eval-let expr env)
        (eval (let-body expr)
              (extend env
                      (eval-bindings (let-bindings expr)
                                     env))
)
```

Where `eval-bindings` evaluates the value part of each binding and returns tuples of `(var evaluated-val)` 

Static analysis proceeds analogously, but we don't need to (and can't) evaluate anything,

```scheme
(define (analyze-let expr ct-env)
        (analyze (let-body expr)
                 (ct-extend ct-env (vars (let-bindings expr)))
)
```

where `(vars bindings)` is just `(map car bindings)`

and similarily for other language constructs.

Continuing with our naive run-time evaluation analogy, variable are looked up in the current environment:

```scheme
(define (eval-var var env)
        (lookup var env)
)
```

The only transformations we perform during lexical addressing is to annotate the variables with their locations in the compile-time environment:

```scheme
(define (analyze-var var ct-env)
        (annotate-var var (ct-lookup var env))
)
```

That depends on `ct-extend` and `ct-lookup`

```scheme
(define (ct-extend ct-env vars)
        (cons vars ct-env)
)
```

```scheme
(define (ct-lookup var ct-env)
        (define (lookup-in-env ct-env frame-counter)
                (define (lookup-in-frame frame offset)
                        (cond ((null? frame) null)
                              ((eq (car frame) var) offset)
                              (else (lookup-in-frame (cdr frame)
                                                     (+ offset 1))))
                )
                (if (null? ct-env)
                    (error "no binding for" var)
                    (let ((offset (lookup-in-frame (car ct-env) 0)))
                         (if (null? offset)
                             (lookup-in-env (cdr ct-env) (+ frame-counter 1))
                             (list frame-counter offset)
                         )
                    )
                )
        )
        (lookup-in-env ct-env 0)
)
```

All it's doing is searching the environment for the location of the variable, and returning a tuple of `(frame offset)`.

For example

```scheme
(ct-lookup 'y '((a b) (c d) (x y z)))
```

should return `(2 1)`

The other thing we need to handle is when we see a lambda. It's simpler than you might think,
and completely analogous to let:

```scheme
(define (analyze-lambda expr ct-env)
        (analyze (lambda-body expr)
                 (ct-extend ct-env (lambda-args expr))
        )
)
```

No need to create any compile-time equivalent to closures or anything like that, we just analyse the body in the environment that the lambda is created in, which is exactly what lexical variables require.

The only thing we need to ensure to make all this work is that the run-time environments will be constructed with identical frames and offsets. They no longer need variables, or hash tables to find them.

A run time environment is then just a linked list of arrays of values, and a run-time lookup is just:

```scheme
(define (lookup frame offset env)
        (if (eq frame 0)
            (index offset (car env))
            (lookup (- frame 1) offset (cdr env))
        )
)
```
And since we've already done the analysis we know the environment must be there and we don't even need to check for `null`!
