# F♮ Language Syntax Reference

## Core Syntax

### Functions

```fn
fn name { (args) { body } }
// or
let name = fn(args) { body }
```

### Let/In Blocks

- `let` introduces mutually recursive definitions (letrec semantics)
- `in` begins the body that uses those definitions
- Can appear at file top-level or inside any `{ }` block
- Without `let`/`in`, blocks are just sequences of expressions
- Nested `let/in` blocks must be enclosed in curly braces: `let x = 1; in { let y = 2; in y + x }`

### Pattern Matching

Functions can have multiple cases, switch on arguments:

- Wildcards: `_` matches anything without binding
- Named structures: `x = h @ t` binds both components and whole
- Pseudo-unification: `(x, x @ _)` requires matching values
- Switch expressions: `switch (expr) { (pattern) { result } }`

### Typedefs

```fn
typedef typename(#generic) { constructor1(types) | constructor2 }
```

- Generic types use `#` prefix: `#t`, `#a`, `#b`
- Built-in types: `int`, `char`, `bool`, `string` (alias for `list(char)`)
- Named fields: `constructor{ fieldName: type }`
- Typedefs at `namespace` level are global; typedefs inside `let` blocks are scoped to that block

### Namespaces

Files start with `namespace` keyword (like `let` without `in` - mutually recursive declarations).

- `link` directives must appear inside a `let` or `namespace` block, not at top level
- Import via `link "<path>.fn" as <name>` (inside `let`)
- Reference imported components as `name.component`

Example:

```fn
let
    link "listutils.fn" as lst;
    result = lst.length([1, 2, 3]);
in
    result
```

## Common Syntax Gotchas

- **Link placement**: `link` statements must be inside a `let` or `namespace` block, not standalone
- **Nested blocks**: Nested `let/in` require `{ }` around the `let/in` expression
- **Semicolons in let**: Multiple bindings in `let` are separated by semicolons: `let a = 1; b = 2; in a + b`
- **Variable naming**: Identifiers starting with `fn` followed by a digit (like `fn3`, `fn10`) are rejected by the parser - use different names like `func3`, `f3`
- **Division operator**: `/` on integers will produce rationals, not integer quotient
- **Print declarations**: in a `let`, `print typename { ... }` is a declaration, not an invocation - defines custom printer for a type
- **Exhaustive patterns**: Pattern matches must be exhaustive unless function or switch is declared `unsafe`

## Non-Deterministic Programming (amb)

- **`then`**: Right-associative binary operator - evaluates LHS, but if backtracked returns RHS
- **`back`**: Triggers backtracking to most recent `then`
- Not just try/catch - can backtrack to any chronologically previous `then` in the history of the process

Example:

```fn
fn one_of { ([]) { back } (h @ t) { h then one_of(t) } }
```

See `fn/barrels.fn` for a canonical example.

## Print System

- **Auto-generated**: Print functions created automatically for typedefs (only if no user-defined printer is specified for that type)
- **User-defined**: `print typename(pt, pu, obj) {...}` - takes printer functions for generic types
- **Built-ins**: `puts(string)`, `putc(char)`, `putn(number)`, `putv(value)`
- **Implementation**: Uses currying to specialize generic printers at call sites

## User-Defined Operators

**Syntax**: `operator "pattern" [optional <associativity>] <precedence> <implementation>`

**Patterns**: Use `_` as placeholder for operands in the pattern string:

- Prefix: `"-_"` (underscore after operator)
- Infix: `"_+_"` (underscores on both sides)
- Postfix: `"_!"` (underscore before operator)

**Examples**:

```fn
operator "_@_" right 90 cons;
operator "_!" 120 factorial;
```

**Restrictions**: Can't redefine with same fixity, can't have same op as both infix and postfix.

Defined in `src/preamble.fn` or user code. For details, see [pratt-parser.md](pratt-parser.md).

## Wiki

There is an associated [wiki](https://github.com/billhails/CEKF/wiki) that may contain additional useful information.
