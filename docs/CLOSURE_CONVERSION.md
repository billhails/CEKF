# Closure Conversion

> Notes on [How to Compile Lambda](https://matt.might.net/articles/closure-conversion/) before any initial prototype.

So we understand the nature of closure and the problem with closures in C, in fact we've already created closures in C in order to implement the ANF rewrite and CPS transform while sticking close to the reference implementations in Racket and F♮, see
[anf_continuations.yaml](../tools/anf_continuations.yaml) (generator support),
[anf_normalize_2.c](../src/anf_normalize_2.c) (actual implementation)
and [normalize.fn](../fn/rewrite/normalize.fn) (reference implementation) for examples.

The meat of the Closure Conversion algorithm remains to investigate.

## Problem

The core problem: we need to hoist all functions to the top level. Consider

```
(define adder
  (lambda (x)
    (lambda (y) (+ x y))))

((adder 5) 6)
```

That inner lambda cannot stand. We need to produce something like:

```
(define f$42
  (lambda (env y)
    (+ (env-ref env x) y)))

(define adder
  (lambda (x)
    (make-closure f$42 (make-env (x x)))))

(apply-closure (adder 5) 6)
```

## Algorithm

The blog starts with plain lambda calculus and extends it to add explicit closure support:

```bnf
<exp> ::= (lambda (<var> ...) <exp>)
      |   (<exp> <exp> ...)
      |   <var>

      |   (lambda* (<var> <var> ...) <exp>) ;; already closure-converted
      |   (make-closure <exp> <exp>)
      |   (make-env (<var> <exp>) ...)
      |   (env-ref <exp> <var>)
      |   (apply-closure <exp> <exp> ...)   ;; invoking a closure instead of a lambda
```

The blog gives `closure-convert` in Racket, so let's try it in F♮. first we'll need to extend `expr`

```fn
    lambdac(list(string), expr) |
    make_closure(expr, expr) |
    make_env(list(#(string, expr))) |
    env_ref(expr, string) |
    apply_closure(expr, list(expr)) |
```

Then we can attack `closure_convert`:

```fn
unsafe fn closure_convert {
    (exp=E.lambda(params, body)) {
        let
            senv = gensym("env");
            sparams = senv @ params;
            fv = free(exp);
            venv = list.map(fn (v) { #(v, var(v)) }, fv);
            sub = DICT.make(fv, list.map(fn (v) { env_ref(senv, v) }, fv));
            vbody = substitute(sub, body);
        in
            make_closure(lambdac(sparams, vbody), make_env(venv))
    }
}
```

Helper functions `free`:

```fn
// expr -> list(string)
fn free {
    // lambda[c](list(string), expr)
    (E.lambda(params, body)) | (E.lambdac(params, body)) {
        list.exclude(params, free(body))
    }

    // var(string)
    (E.var(v)) { [v] }

    // make_closure(expr, expr)
    (E.make_closure(fun, env)) {
        list.unique(free(fun) @@ free(env));
    }

    // make_env(list(#(string, expr)))
    (E.make_env(bindings)) {
        let #(vs, es) = list.unzip(bindings);
        in list.unique(list.concat(list.map(free, es)))
    }

    // env_ref(expr, string)
    (E.env_ref(e, s)) {
        free(env)
    }

    // apply[_closure](expr, list(expr))
    (E.apply_closure(f, args)) | (E.apply(f, args)) {
        list.unique(list.concat(free(f) @ list.map(free, args)))
    }
}
```

and `substitute`:

```fn
// substitute :: DICT.Dict(string, E.expr) -> E.expr -> E.expr
fn substitute (sub, exp) {
    switch(exp) {
        // lambda(list(string), expr)
        (E.lambdac(params, body)) {
            let sub2 = DICT.delete_list(params);
            in E.lambdac(params, substitute(sub2, body))
        }

        (E.lambda(params, body)) {
            let sub2 = DICT.delete_list(params);
            in E.lambda(params, substitute(sub2, body))
        }

        (E.var(v)) {
            switch (DICT.lookup(v, sub)) {
                (some(e)) { e }
                (nothing) { exp }
            }
        }

        // make_closure(expr, expr)
        (E.make_closure(lam, env)) {
            E.make_closure(substitute(sub, lam), substitute(sub, env))
        }

        // make_env(list(#(string, expr)))
        (E.make_env(bindings)) {
            E.make_env(list.map fn (#(v, e)) { #(v, subst(sub, e)) })
        }

        // env_ref(expr, string)
        (E.env_ref(e, s)) {
            E.env_ref(substitute(sub, e), s)
        }

        // apply_closure(expr, list(expr))
        (E.apply_closure(f, args)) {
            E.apply_closure(substitute(sub, f), list.map(substitute(sub), args))
        }

        // apply(expr, list(expr))
        (E.apply(f, args)) {
            E.apply(substitute(sub, f), list.map(substitute(sub), args))
        }
    }
}
```

## Bottom Up Transform

Pros: single lookup to find the variable
Cons: expensive in terms of space (multiple environments with the same data)

```fn
fn transform_bottom_up(f, exp) {
    f(transform(transform_bottom_up(f), exp))
}

fn flat_closure_convert(exp) { transform_bottom_up(closure_convert, exp) }
```

## Top Down Transform

Pros: space efficient
Cons: chains of environments (slower)

```fn
fn transform_top_down(f, exp) {
    transform(transform_top_down(f), f(exp))
}

fn shared_closure_convert(exp) { transform_top_down(closure_convert, exp) }
```

## Helper

```fn
fn transform(t, exp) {
    switch (exp) {
        // lambda(list(string), expr)
        (E.lambda(params, body)) {
            E.lambda(params, t(body))
        }

        // lambdac(list(string), expr)
        (E.lambdac(params, body)) {
            E.lambdac(params, t(body))
        }

        (E.var(_)) { exp }

        // make_closure(expr, expr)
        (E.make_closure(lam, env)) {
            E.make_closure(t(lam), t(env))
        }

        // make_env(list(#(string, expr)))
        (E.make_env(bindings)) {
            E.make_env(list.map fn (#(v, e)) { #(v, t(e)) })
        }

        // env_ref(expr, string)
        (E.env_ref(e, s)) {
            E.env_ref(t(e), s)
        }

        // apply_closure(expr, list(expr))
        (E.apply_closure(fun, args)) {
            E.apply_closure(t(fun), list.map(t, args))
        }

        // apply(expr, list(expr))
        (E.apply(fun, args)) {
            E.apply(t(fun), list.map(t, args))
        }
    };
}
```