# Simple Namespaces

The current namespace implementation is a constant cause of complexity
and of bugs.

The simplest possible implementation of namespaces is to make them part
of the names of the entities defined in them. So `list.length` (almost)
literally becomes `list_length`.

Not quite that simple as the names we give to namespaces via the `link`
expression can differ, but we can use that name to look up the agnostic
file id of the namespace file, and use that file id to generate a
canonical namespace prefix: `#<dev>#<ino>#`.

So what needs to happen? The hope is that if we can get rid of namespaces
immediately, while parsing, Then any and all code that refers to them is
merely redundant and can be removed in a later refactoring.

Currently namespaces are parsed into standalone `AstNamespaceImpl` lists
of `AstDefinition`s.
These are stored as an entry in an array of namespaces and the index of that entry
is associated with the name of the namespace. If the namespace has already
been parsed, that is short-circuited and the nsid is returned directly.
The value of a namespace expression is an `AstDefinition_Blank`, which is
later discarded.

That can remain unchanged, as does the rest of the parsing.

What we need is a transform between the parse and the lambda conversion that
applies all the prefixes, both within the namespaces and to the lookups
(`namespace.value`), then inserts all of the namespaces into the top-level
list of definitions.

The namespace array is emptied, and all lookup operations have been replaced
with simple variable access to the prefixed namespace variables.

The substitution on namespaces should be trivial, only the top-level names
being defined by the namespace need be captured by an environment mapping
them to their translations. Normal logic of discarding bound variables when
descending into lets and lambdas works here, and namespace lookups within
namespaces are no different from those at the top-level.

Is this better done after lambda conversion? - No, lambda conversion does
a lot to namespaces, so painful or not this transform has to be done on
the AST.

But. once done everything should still work. After all there is no requirement
for a program to link any namespaces, so code already works without them.

## More Detailed Plan

Visitor `ast_ns` is generated.

Top-level entry point is `AstProg`. This structure only occurs once at the
top, it is not recursive. It has three components:

* a `preamble` of `AstDefinitions`.
* an `AstNamespaceArray` of `AstNamespaceImpl`, where each `AstNamespaceImpl` has two components:
  * a `file_id`
  * an `AstDefinitions`
* a `body` of `AstExpressions`

`nsAstProg` does not take a `VisitorContext`, rather it constructs one.

The `VisitorContext` needs:

* An array mapping namespace id to file id. It can just use the array from `AstTop`.
  * This is used to resolve lookups to flat symbols, wherever they are encountered.
  * It is static and can be attached once by `nsAstTop`.
* A map from bare symbol to namespace-qualified symbol.
  * This is constructed from the set of definitions within a namespace and is only populated while visiting a namespace (probably by `nsAstNamespaceImpl`).
  * It is used to replace uqualified names with their namespace-qualified replacements.

When control returns to `nsAstTop`, its last job is to merge the definitions from
each `AstNamespaceImpl` into the `preamble`, then empty out the `AstNamespaceArray`.
