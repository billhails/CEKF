# Environments

Various approaches to having namespaced environments.

## Environments as Namespaces

By elevating environments to first-class data types we can re-use existing
code to achieve encapsulation and namespaces.

However this proves very difficult to reconcile with fast lexical
adressing, two environments with the same apparent prototype can still
differ in the order of their bindings and hence the lexical addresses of
their components, a variable access could not then be correctly annotated
for multiple use-cases.

## Namespaces Only

As an alternative to fully first-class environments we could look at
`env` declarations purely as namespaces, that is the names of envs are
not variables but constant names to be used literally. This is certainly
simpler, but the semantics are a bit wooly, so how would it work?

The `.` operator would evaluate its rhs in the context of the namespace
on the lhs, like `myns.doSomething(arg)`. The dot operator would have to
have higher precedence than even function calls, so that would parse as
`(myns.doSomething)(arg)`. Bytecode might then be something like

```text
| ..arg.. | SAVENV | myns | SETENV | VAR[n][n] | SWAP | RESTORENV | APPLY |
```

I'm not sure I like the save and restore because we don't currently have
an environment stack, and we'd need Forth-level stack twiddling to use
the existing stack (i.e. the `VAR` lookup would leave its result top of
stack hence the need for a SWAP bytecode or similar).

> Future self: using the existing stack turns out to be fine.

The new `getenv` internal construct pushes the current environment on
the top of the stack, where it can be bound to a variable `myns` in the
normal way. `myns` in the bytecode above just becomes a var lookup.

What about type checking? Again if `myns` is bound to a `TcEnv` during
type checking, that env can be used to validate the rhs of the dot
operator.

Any other holes in this idea? ANF conversion is purely local transforms
that shouldn't be affected, desugaring likewise.

### Printing Again

The `print` construct fails in this scenario though. If an env declares a
`typedef` and return data of that type, then for example

```fn
print(a.returnThing())
```

will compole to

```scheme
(print$Thing (dot a (returnThing)))
```

and `print$thing` is not in scope. Error. What we want print to compile
to instead is

```scheme
((dot a print$Thing) (dot a (returnThing)))
```

This can be made to work if types are annotated with their scopes. which
leads to...

### Scoped Types

Scoped types are necessary in any case, otherwise two environments could
both declare the same type name for different types and those types
would unify incorrectly. So we'll need types to unify on their scopes
too. For that reason scopes need to be canonical.

It works to treat canonical scopes equivalently to a file system with
a root `/`, the difference being that we don't allow relative paths,
only absolute ones.

It will also pay to keep the syntax distinct in discussions, using
`/a/b/c` to mean a canonical scope and restrict `a.b.c` to mean lookup in
the surface language, which has very different semantics. To illustrate,
after:

```text
env a {
    env b {
    }
}
env c {
    d
}
```

...the expression `a.b.c.d` is valid at the top level: `a` is found
at top, `b` is found in `a`, but when `c` is not found in `b` search
proceeds back to `a` then to the root where `c` is located, thence `d`,
so in this example `a.b.c.d` resolves to `/c/d`.

Before proceeding, there's a problem here:

```text
env a {
    env b {
        env c {
        }
    }
}
env c {
    d
}
```

in this case `a.b.c.d` would not resolve because `c` is found in `b`
and will not be searched for a second time.

we could solve this with backtracking (compile time so probably ok)
but the semantics are still not great, maybe better to leave as is and
fail to resolve, after all given the above `a.b.c.d` is a pretty silly
thing to do.

Anyway assuming we can always resolve valid type scopes to a canonical
form, the print compiler will need the type, as before, plus the current
canonical scope. For example

```text
env a {
    env b {
        typedef T1 ...
        fn getT1() {...}
    }
    ...
        print(b.getT1())
}
```

The print compiler will get a type `/a/b/T1` and a current scope `/a`
so will resolve to:

```scheme
((dot b print$T1) (dot b (getT1)))
```

by removing the common prefix.

This should work fine with `extends` whaen that's implemented:

```fn
env complex extends math { ... }
```

provides an initial `/math` scope for processing `complex`.

Another problem with this though, typedefs from within function bodies
won't be resolvable in this way, or even at all since the `print$thing`
functions won't even exist after the function exits. Maybe we only allow
typedefs and envs within other envs or at the top level?

Since that seems overly restrictive, let's try another approach.

### Named vs. Anonymous Scopes

Suppose we make a distinction between `env`s being "named scopes" and
other types of anonymous nests (function bodies etc.) as being "anonymous
scopes", then we can simply impose the restiction that user-defined types
cannot escape anonymous scopes. That way we can continue to use typedefs
within function bodies, but functions defining such types can only use
them internally and not return values of those types. types defined within
envs are accessible outside of those envs if they are qualified by the
path to that env (which cannot cross the boundary of an anonymous scope).

All types can remain fully qualified by a scope, but canonical scopes
can make use of some internally constructed pseudo-root to indicate
they are not top-level. A possible nomenclature might be `$/a/b/c` meaning
`a` is an env whose parent is the closest enclosing anonymous scope.

Trial:

```fn
env a {
    typedef T1 ... // /a/T1
    env b {
        typedef T2 ... // /a/b/T2
    }
    fn f1 () { // begin anonymous scope
        typedef T3 ... // $/T3
        T3 // ok $/T3
        b.T2 // ok /a/b/T2
        T1 // ok /a/T1
    }
    T1 // ok /a/T1
    b.T2 // ok /a/b/T2
    T3 // not ok
}
```

This looks like it could be made to work, but will need some formalizing.
In particular the error case, if `T3` is allowed to leak out of
`f1` then its scope cannot just be re-interpreted as a new nearest
enclosing anonymous scope, so maybe each anonymous scope needs a unique
identifier? or we trap the leak and raise an error. Trapping the leak
is too restrictive: if we declare a type inside a function and then
map over a list of that type within the function that should be fine,
but the type must be leaked to `map` it's just that map doesn't care as
long as the types of its arguments match `(a -> b) -> list(a) -> list(b)`.

Since the type checker we've already decided will be passing around a
current canonical scope, we might just use the machine address of the
nest as the current scope, or generate a symbol on the fly with a qualifying
`$` to say it's anonymous. Then it's unifiable within the current scope
but not outside of it.

so assume each anonymous nest generates a new id, and then all canonical
paths are root based. We've identified two "leaking" scenarios so far:

1. Returning a value from a function of a type defined in that function.
2. Passing a value from a function, of a type defined in that function,
   to another polymorphic function.

The first should cause an error if that type is used in a non-polymorphic
way, the second should be ok.

```fn
env a {
    fn b() {
        let
            typedef T1 ...       // /a/$nnn/T1
            fn c (t1) { ... }    // /a/$nnn/T1 -> int
        in
            map(c [t1, t1, t1])  // returns list of int
    }
}
```

`map` should get `(/a/$nnn/T1 -> int) -> /list(/a/$nnn/T1) -> /list(int)`
which will unify fine even outside of the body of `fn b`.

Because the type checker is passing around the current scope, `print` can
compare that with the scope of a type `T1` to determine if the `print$T1`
function is in scope or not. If the type is in scope it will use the
generated printer, otherwise fallback to a generic default `putv`.`

`print` needs the scope in any case to "trim" the canonical scope to an
env path, and if there are no anonymous ids left in this trimmed path then
`print` knows the `print$T1` function is accessible.

So to nail the semantics, scopes are linked lists, the root of the
scope is the tail of the list, so in `env a { env b { env c { x } } }`
the scope of `x` is `c=>b=>a=>/`. Having got that out of the way lets
tabulate some examples using the following structure:

```fn
T1
env a {
    T2
    fn f1 {
        T3
    }
    env b {
        T4
        fn f2 {
            T5
        }
        env c {
            T6
            fn f3 {
                T7
            }
        }
    }
}
env d {
    T8
    fn f4 {
        T9
    }
}

```

| current scope | type scope          | relative scope | accessible | notes                             |
| ------------- | ------------------- | -------------- | ---------- | --------------------------------- |
| `/`           | `T1 /`              | `T1`           | Y          | both global                       |
| `/`           | `T2 a=>/`           | `a.T2`         | Y          |                                   |
| `/`           | `T3 f1=>a=>/`       | `a.f1.T3`      | N          | relative contains anonymous scope |
| `/`           | `T4 b=>a=>/`        | `a.b.T4`       | Y          |                                   |
| `/`           | `T5 f2=>b=>a=>/`    | `a.b.f2.T5`    | N          |                                   |
| `/`           | `T6 c=>b=>a=>/`     | `a.b.c.T6`     | Y          |                                   |
| `/`           | `T7 f3=>c=>b=>a=>/` | `a.b.c.f3.T7`  | N          |                                   |
| `/`           | `T8 d=>/`           | `d.T8`         | Y          | same as T2                        |
| `/`           | `T9 f4=>d=>/`       | `d.f4.T9`      | N          | same as T3                        |
| `a=>/`        | `T1 /`              | `T1`           | Y          | parent scope                      |
| `a=>/`        | `T2 a=>/`           | `T2`           | Y          | same scope                        |
| `a=>/`        | `T3 f1=>a=>/`       | `f1.T3`        | N          | relative contains anonymous scope |
| `a=>/`        | `T4 b=>a=>/`        | `b.T4`         | Y          |                                   |
| `a=>/`        | `T5 f2=>b=>a=>/`    | `b.f2.T5`      | N          |                                   |
| `a=>/`        | `T6 c=>b=>a=>/`     | `b.c.T6`       | Y          |                                   |
| `a=>/`        | `T7 f3=>c=>b=>a=>/` | `b.c.f3.T7`    | N          |                                   |
| `a=>/`        | `T8 d=>/`           | `d.T8`         | Y          | parent scope                      |
| `a=>/`        | `T9 f4=>d=>/`       | `d.f4.T9`      | N          | parent scope                      |
| `f1=>a=>/`    | `T1 /`              | `T1`           | Y          | parent scope                      |
| `f1=>a=>/`    | `T2 a=>/`           | `T2`           | Y          | same scope                        |
| `f1=>a=>/`    | `T3 f1=>a=>/`       | `T3`           | Y          | anonymous scope pruned            |
| `f1=>a=>/`    | `T4 b=>a=>/`        | `b.T4`         | Y          |                                   |
| `f1=>a=>/`    | `T5 f2=>b=>a=>/`    | `b.f2.T5`      | N          | different functions               |
| `f1=>a=>/`    | `T6 c=>b=>a=>/`     | `b.c.T6`       | Y          |                                   |
| `f1=>a=>/`    | `T7 f3=>c=>b=>a=>/` | `b.c.f3.T7`    | N          |                                   |
| `f1=>a=>/`    | `T8 d=>/`           | `d.T8`         | Y          | parent scope                      |
| `f1=>a=>/`    | `T9 f4=>d=>/`       | `d.f4.T9`      | N          | parent scope                      |
| `b=>a=>/`     | `T1 /`              | `T1`           | Y          | parent scope                      |
| `b=>a=>/`     | `T2 a=>/`           | `T2`           | Y          | same scope                        |
| `b=>a=>/`     | `T3 f1=>a=>/`       | `f2.T3`        | N          | anonymous scope pruned            |
| `b=>a=>/`     | `T4 b=>a=>/`        | `T4`           | Y          |                                   |

etc.

Obviously the relative scope is generated by comparing the current
scope with the type scope and removing any common roots. Then if the
result contains any anonymous components the type is not accessible
from the current scope, as far as printing is concerned (the generated
`print$type` functions are not accessible to the print compiler).  If the
types and therefore their generated print functions are acccessible, the
print compiler need only prefix the print function with the calculated
relative scope. Otherwise it uses the generic `print$` function.

### Algorithm to Calculate Relative Scope

Easiest is just to reverse the scopes then walk them from tail to head.
We'll have tuples at some point so let's just use them here, a scope
element is a tuple of a string and a bool (anonymous flag). A scope is
a list of elements.

```fn
fn relativeScope {
    ([], []) { [] }
    (#(v, _) @ sc, #(v, _) @ st) { relativeScope(sc, st) }
    (_, st) { st }
}
```

Try it on a few examples

|  sc |  st | result |
| --- | --- | -------|
| /   | /   | /      |
| /   | a   | a      |
| /   | a.b | a.b    |
| /   | b   | b      |
| a   | /   | /      |
| a   | a   | /      |
| a   | a.b | b      |
| a   | b   | b      |
| a.b | /   | /      |
| a.b | a   | /      |
| a.b | a.b | /      |

## More Problems

Consider

```fn
env a {
    env b {
        T1
    }
}
env b {
    T1
}
```

`a.b.T1` resolves to `b.T1` in `a`, but so does `b.T1`, so the whole
relative paths idea may be dead in the water. But only `print` uses
these relative paths, can it use absolute ones? or, for efficiency,
have a global root env that is hidden (illegal var name) but can be
pruned to a relative path as with any other? None of the above changes,
except that the root is an explicit component rather than just nil.

We might even support it in the language, use a leading '`.`' to signify
an absolute path, that way in the above, code inside `.a` could explicitly
refer to code in `.b`, distinct from `.a.b` which it could continue to
refer to as just `b`.

If we use `$` to signify the root then `$.b` in context `$.a.b` would
still get pruned to `b` so that doesn't work either.

Of course we don't have to prune, `print` would still work if all
of its paths were absolute, but we'd loose the ability to check for
inaccessible scopes, and actually `print` relies on relative paths
to avoid impossible lookups on those anonymous scopes so it wouldn't
work.

Another option, this might just work, rename environments to all have
unique names, or otherwise tag them with some unique id. Some sort
of scoping rules would be needed to rewrite explicit environment
references, but the print compiler would just directly use the unique
names. In fact the unique names could just be their absolute paths
or some concatenation of them. That then removes the need for a global
env to qualify them, top-level `.b` is just `b`, but `.a.b` becomes
something like `.a.a/b` and within `.a`, `b` is relatively `a/b`.

### TODO, then

1. rename environments to reflect their scope and make them globally
   unique.
2. rewrite explicit environment lookups to use these qualified names.
3. type-checker maintains current environment context and passes it to
   the print compiler, as well as using it to qualify the context of
   each type (constructor).

## Environments as Dispatch Functions

Another possibility is to translate an environment into a dispatch
function returning the components of the environment.

This promises less effort because the generated lambda calculus will not
need to be extended to support environments explicitly, and therefore
nothing downstream of it will need those extensions either. There is
the adiitional overhead of a lookup and return to access an environment
component, but that might be worth the decrease in complexity.

The problem is that such a dispatch function, which must return arbitrary
types, can not be naively type-checked. We have to either type-check it
specially, or propagate the environment concept into the lambda form,
rewriting it to a dispatch after type checking.  This might still be
worthwhile.

Anyway the transformations are quite straightforward, for example

```fn
let
    env a {
        typedef color { red | green | blue }
        fn add1(x) { 1 + x }
    }
in
    a.add1(2)
```

might generate something like:

```scheme
(letrec ((a
    (letrec ((print$color ...)
             (add1 (lambda (x) (+ 1 x))))
         (lambda (selector) ;; returned dispatch function 'a'
             (match selector
                    ((0) print$color)
                    ((1) add1))))))
   ((a 1) 2))
```

Note that any generated `print$` functions are also exported, and we
make use of the existing `match` construct for fast O(1) lookup, safe
because we know the number of elements in the environment.

Chained lookup also works, so `a.b.c(x)` becomes something like `(((a 0)

1) x)`

It would be preferable to typecheck the dispatcher specially but that
might be just as difficult or more so than typechecking the environment
then transforming it. It would be less likely to be wrong though.

Also it's not yet clear how this might work if we plan to implement the
`extends` attribute of environments, which is likely a requirement for
any really useful language.

Anyway, food for thought, and maybe there's a hybrid approach.

Returning to this, actually there is a way to get a dispatch function
past the type-checker, if we declare a type that contains all possible
arguments and another type that contains all popssible result types,
for example:

```fn
let
    env a {
        fn map {
            (_, []) { [] }
            (f, h @ t) { f(h) @ map(f, t) }
        }
        fn fact {
          (0) { 1 }
          (n) { n * fact(n - 1) }
        }
    }
in
    a.factorial(5)
```

becomes something like

```fn
let
    typedef a$args(#f, #u) { a$map$args(#f, list(#t)) | a$fact$args(int) }
    typedef a$results(#u) { a$map$result(list(#u)) | a$fact$result(int) }
    fn a$dispatch(args) {
        let
            fn map {
                (_, []) { [] }
                (f, h @ t) { f(h) @ map(f, t) }
            }
            fn fact {
              (0) { 1 }
              (n) { n * fact(n - 1) }
            }
        in
            switch(args) {
                (a_map_args(f, u)) { a_map_result(map(f, u)) }
                (a_fact_args(n) { a_fact_result(fact(n)) }
            }
    }
in
    switch (a$dispatch(a$fact$args(5))) {
        (a$fact$result(n)) { n }
    }
```

That might just work but I'm not sure I like it.
