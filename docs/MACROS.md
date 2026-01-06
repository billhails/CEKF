# Macros

Just thinking out loud. Basic intent is just something like lisp
`defmacro`, no attempt to do `syntax-rules` or anything fancy, but we
can provide some support for hygene.

```fn
macro time(expression) {
    let $start = now()
    in
        let $result = expression;
        in
            print(now() - $start);
            $result
}
```

The arguments are substituted literally. Variables starting with a `$`
are only allowed in macros and are replaced consistently by a generated
symbol, e.g. all occurences of a `$`-prefixed variable are replaced by
the same generated symbol.

Macros can nest without conflict provided `$` prefixes are used to
resolve conflicts, i.e.

```fn
time(time(sort(hugeList)))
```

would get expanded to:

```fn
{
    let start$1 = now()
    in
        let result$1 = {
            let start$2 = now()
            in
                let result$2 = sort(hugeList)
                in
                    print(now() - start$2);
                    result$2
        }
        in
            print(now() - start$1);
            result$1
}
```

Note the result of a macro expansion is always a nest, which is a valid
expression.  Macro expansion can be performed entirely by the parser,
so nothing downstream should need to change.

## Use Cases

`time` as shown above, also

```fn
macro trace(expression) {
    print("entering " @@ `expression`);
    {
        let $result = expression
        in
            print("leaving " @@ `expression`);
            $result
    }
}
```

The backticks or something equivalent could prevent evaluation of the
`expression`, instead returning a string representation.

Something a bit more ambitious...

```fn
macro for(name, init, test, modifier, expression) {
    let fn $loop (name) {
        if (condition) {
            expression;
            $loop(modifier)
        } else {
            name
        }
    }
    in $loop(init)
}

for(n, 10, n > 0, n - 1, print(n))
```

is simple but ugly.

Maybe we can extend the parser just a bit to acommodate them:

```fn
macro for(name = init, test, modifier): expression {
    ... same as before
}
for(n = 10, n > 0, n - 1): { print(n) }
```

Not totally convinced.

## Implementation

Macros will also need to be namespaced.

Dodging parser issues for now, if macros look just like fuctions then
they can be invoked as functions but with their arguments unevaluated,
then the result is re-evaluated just like for `defmacro`.  However I'm not
sure we can delay invocation of macros until after bytecode generation,
in fact that's undesirable as there should be no run-time performance
penalty for using macros. Possibly the place to handle them is immediately
before type-checking.  we'll need an environment with namespaces installed
in order to look them up, but we're doing that already for constructors.
