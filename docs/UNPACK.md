# Unpacking Tuples from Function Results

It would be very useful to be able to immediately unpack the values
from a tuple returned by a function, for example:

```fn
let
   #(a, b) = myfunc(c);
in
   ...
```

This would not be as sophisticated as the term pattern matching of function calls
and switch statements, and would be restricted to simple variables
bound in a let expression. I'd imagine the AST would be another variant of
AstDefine, but with a list of symbols from the target tuple instead of just
a single symbol, so like:

```yaml
AstMultiDefine:
    ...
    data:
        symbols: AstSymbolList
        expression: AstExpression
AstSymbolList:
    ...
    data:
        symbol: HashSymbol
        next: AstSymbolList
```

and the intermediate lambda code would be like:

```scheme
(letrec ((tmp$123 (myfunc c))
         (a (unpack tmp$123 0))
         (b (unpack tmp$123 1))
         ...
) ... )
```

See [LETREC.md](LETREC.md) for why ANF will convert this into a `let`.

Of course the type checking will validate that `tmp$123` is a tuple
etc. in the normal way provided the above intermediate lambda code is generated.

The changes should only be needed in the parser/AST and in the lambda conversion,
there should be no need for changes to the actual lambda.yaml or and ANF conversion.
