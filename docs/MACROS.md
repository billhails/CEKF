# Macros

Just thinking out loud.

```
macro time(expression) {
    let $start = now()
    in
        let $result = expression
        in
            print(now() - $start);
            $result
}
```

The arguments are substituted literally. Variables starting with a `$` are only allowed
in macros and are replaced consistently by a generated symbol, e.g. all occurences of
a `$`-prefixed variable are replaced by the same generated symbol.

Macros can nest without conflict provided `$` prefixes are used to resolve conflicts, i.e.

```
time(time(sort(hugeList)))
```

would get expanded to:

```
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

Note the result of a macro expansion is always a nest, which is a valid expression.
Macro expansion can be performed entirely by the parser, so nothing downstream should need to change.

## Use Cases

`time` as shown above, also

```
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

The backticks or something equivalent could prevent evaluation of the `expression`, instead returning
a string representation.
