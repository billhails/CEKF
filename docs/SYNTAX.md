# Syntax Extension

## List Comprehensions Example

```fn
// list comprehensions
syntax lco ::= "lco" "["
    exp: Expr
    "for" x: Name "in" xs: Expr
    filters: Syntax(where(x, xs)) // new "Syntax(id)" refers to other syntax
                                  // Syntax(id(arg,...)) allows passing data
"]" {
    quote { map(fn (unquote x) { unquote exp }, unquote filters) }
};

syntax where(x, xs) ::= { xs }; // empty
                       | "where" cond: Expr rest: Syntax(filter(x, xs)) {
                           quote { filter(fn (unquote x) { unquote cond }, unquote rest) }
                       }

syntax filter(x, xs) ::= { xs }
                       |  "," cond: Expr rest: Syntax(filter(x, xs)) {
                           quote { filter(fn (unquote x) { unquote cond }, unquote rest) }
                       }
```

How would that interact with the existing Pratt Parser?

1. Each "syntax" declaration creates a parselet. If the syntax is initiating (declaration tbd) then
   its token is added to the trie and the parselet is registered as a prefix operator. otherwise
   the syntax will not activate by itself, which is correct for "inner" syntax.
2. Each such parselet is configured with a DAG of its components.
3. The parselet must also somehow arrange to accept arguments from a containing parselet.
4. Most components (Expression, Name, Nest etc.) do not take arguments and can be handed out
   to existing parser routines, though we may need "maybe" versions of those that don't error.
5. Only syntax parselets need to capture name to parse-fragment mappings, and forward those mappings
   to sub-syntax-parselets.
6. bindings are captured with `let*` semantics, no use as argument before capture.
7. Only syntax parselets need to parse the terminal nest and handle quote and unquote.
8. We may be constrained to syntactically valid (ignoring quote/unquote) terminal nests. i.e. existing
   parser logic must handle them.
9. We may need additional parser context (i.e. a map of names that can be unquoted or a more general
   handler function)
10. Hygene is not addressed.
