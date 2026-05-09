# Extending Imports

We can currently link namespaces into a file with the link directive:

```fn
link "listutils.fn" as list;
```

And then we can import individual exported operators from a linked namespace:

```fn
import list operator "_|>_";
```

or all exported operators:

```fn
import list operators;
```

Ordinary strict functions can also be imported directly into the local scope:

```fn
import list.map;
import list.filter as keep;
```

These forms behave like parser-synthesized wrappers around the qualified call,
so `import list.map;` is equivalent to writing a local `fn map(...) {
list.map(...) }` with the correct arity.

At present this direct member import supports ordinary `fn` definitions only.
`lazy fn` imports are intentionally rejected for now.

Since namespaces are desugared to normal declarations with prefixes, it should be
relatively easy now to allow the export and import of normal (and lazy) functions
as well as typedefs.

The syntax should be something like:

```fn
export function map { ... } // within a namespace
```

and

```fn
import list fn map;
```

or

```fn
import list fn;
```

Likewise for typedefs.

The export of a typedef must (also?) export its constructors.

The parser merely parses these new import and export directives and
either creates AST nodes for them (import) or notes then in the
`AstNameSpaceImpl` (export). It will probably need to distinguish
type constructors from normal functions.

A new visitor on the AST immediately
after parsing and before the namespace desugaring should do the rest. We'd need new
import declarations in the AST but they could be removed by this step at
the same time that it actions them so no polluting downstream. It would
need to know what symbols a namespace is exporting, a new `exports: SymbolSet`
field on `AstNameSpaceImpl` could provide that as well as allowing checking:

1. That an import is legal.
2. Whether two imports conflict.
3. What symbols are imported by an unqualified import.

The visitor can track bound variables and exclude symbols that are
shadowed from its set, on encountering a `LookUpOrSymbol_Symbol` it
would check if the symbol is exported from a namespace and replace with
`LookUpOrSymbol_LookUp` if so. Probably it will have translated the
`SymbolSet` from `AstNameSpaceImpl` into a `SymbolMap` from symbol to
exporting namespace. It might also need to check bare symbols in the
same way.

Actually, on syntax, we can probably improve the import syntax:

```fn
import list "_|>_", "_|?_"; // import the map and filter operators
import maybe _; // import all exports (_ is wildcard)
import list map, filter; // import the map and filter functions
import interpreter eval, expr, "_<<_"; // import the eval function, the expr
                                       // type and whatever that operator is.
```

I have a feeling that while this sounds very do-able there are a
lot of wrinkles especially around constructor inlining that might
scupper this.
