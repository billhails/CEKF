# Operators (and macros)

Some issues with the initial implementation.

I'd thought I could get away with a pure parser-only implementation of
infix operators and it basically works, but there are some issues which
make that approach quite clunky.  One specific scenario is where I'm
declaring an infix addition operator in the preamble as:

```
infix left 100 "+" addition;
```

Where `addition` is the symbol for the two-argument built-in addition
operator. Trouble is in another file I'd redefined `addition` as a type
of expression for unrelated purposes, and because i.e. `2 + 2` gets
re-written to plain old `addition(2, 2)` unconditionally, in that context
the interpreter finds the `addition` type from the current environment
rather than the one where the infix operator was declared.

This is clearly unacceptable.

[Hygenic Macros in Wikipedia](https://en.wikipedia.org/wiki/Hygienic_macro) states:

> The basic strategy is to identify bindings in the macro definition and
> replace those names with gensyms, and to identify free variables in the
> macro definition and make sure those names are looked up in the scope
> of the macro definition instead of the scope where the macro was used.

This offers hope, if we can re-work the macro system to be hygenic by
default, then the parser instead of generating `addition(a, b)` for `a +
b` could instead generate:

```
macro gensym$1(a, b) { addition(a, b) }
```

at the point of the operator declaration, and generate `gensym$1(a, b)`
when `a + b` is subsequently encountered.

Firstly I now think the use of a `$` prefix to indicate a gensym in a
macro is not the best idea. Instead the lambda conversion should identify
bound `let` variables and replace them automatically. That also frees up
`$` as a potentially useful user-defined prefix operator.

The bigger problem is that we can't continue to do naiive macro expansion
during the lambda conversion step, or we'd be back where we started with
`addition(a, b)` referring to whatever `addition` happens to be the
current definition.

We may have to revert to the scheme definition of a macro: pass the
arguments unevaluated to the macro, evaluate the macro body, then
re-evaluate the result.

But we really don't want to have the macro evaluated like that,
because F♮ is not homoiconic, "evaluating the macro body" can only
mean substitution.

What if the arguments to macros were wrapped in a closure?

```
macro AND(a, b) { if (a) { b } else { false } } => fn AND(a, b) { if (a()) { b() } else { false } }

AND(a, b) => AND(fn () { a }, fn () { b })
```

That would definately work, though it won't be quite as efficient. It
solves both local scoping rules, since `AND` is now a normal function then
free variables in the body are evaluated in the context of the function
definition, and variables in the argument expressions are evaluated in
the calling context.

> Got that working, and we're also handling local shadowing of arguments
so they don't get wrapped in an invocation unless they are the same
lexical variable.

One little unnecssary inefficiency needs to be addressed. If one macro
calls another, for example

```
macro NAND(a, b) { NOT(AND(a, b)) }
```

This first gets rewritten, by `lambda_conversion.c` to

```
fn NAND(a, b) { NOT(AND(fn () {a}, fn () {b})) }
```

and then subsequently by `macro_substitution.c` to

```
fn NAND(a, b) { NOT(AND(fn () {a()}, fn () {b()})) }
```

While correct, the expression `fn () {a()}` is just `a` so we'll need
a pass to optimise away this unnecessary wrapping and unwrapping,
essentially restoring

```
fn NAND(a, b) { NOT(AND(a, b)) }
```

Two approaches:

1. Macro specific, have a special type for macro argument application
   and another for macro argument wrapping, and detect the explicit
   combination of the two.
2. Generic pass that would detect this wherever it occurs and optimize it.

In either case we need to be a little bit careful that we allow the
pattern if the argument is being modified, for example if a macro
called another with it's argument modified in some way then the pattern
i.e. `fn() { a() + 1 }` would be necessary.

> Got option 1 working, but no need for extra types, just inspect the
thunk during macro conversion, if it has no arguments and just contains
a symbol that would otherwise be invoked then return the symbol.


## Operator scoping and hygiene

F♮ supports user-defined operators in prefix, infix and postfix positions. To make
them predictable and composable, operator definitions now follow lexical scoping
rules and are hygienic by construction.

### Key rules

- let/in creates a new operator environment
   - A `let … in …` block is parsed with a child Pratt parser that inherits the
      parent rules table and trie. Operators defined inside the block are visible
      only inside that block and shadow outer definitions with the same symbol.
   - Redefinition checks are local: attempting to define the same operator twice
      in the same scope is an error; redefining an operator in an inner scope is
      shadowing and is allowed.

- Namespaces do not export operators (yet)
   - Operators defined inside a `namespace` remain local to that namespace body
      at parse time. We keep namespace parsing in the current global parser so
      preamble/file-level operators (for example, postfix `!`) remain globally
      available. A design for explicit export/import is sketched below.

- Hygiene is automatic
   - When you define an operator, the parser generates a hygienic wrapper
      function name (a gensym). The operator application expands to call this
      wrapper, which internally references the original implementation captured in
      the definition’s scope. This ensures free variables in the operator’s
      implementation resolve to the definition site, not the use site.

### Examples

Shadowing and restoration across nested blocks:

```
let
      prefix 13 "neg" fn(x) { 0 - x };
      a = neg 5;        // -5
in {
      let
            prefix 13 "neg" fn(x) { 0 - x - 1 };
            b = neg 5;    // -6, inner shadowing
      in
            assert(a == -5 and b == -6)
};
// after inner block, outer binding is in force again
assert(neg 5 == -5)
```

Shadowing inside a function body:

```
fn run() {
   {
      let
         prefix 13 "inc" fn(x) { x + 1 };
         a = inc 5;   // 6
      in {
         let
            prefix 13 "inc" fn(x) { x + 2 };
            b = inc 5; // 7
         in
            assert(a == 6 and b == 7)
      };
      // restored to function-local definition
      assert(inc 5 == 6)
   }
}
```

### Implementation notes

- The scanner consults the current parser’s operator trie, then falls back to
   parent parsers via `parser->next`, so shadowing works naturally.
- `addOperator()` inserts new operators into the current parser only and checks
   for redefinition locally. This prevents false positives when shadowing.
- The precedence scale remains the same (`PRECEDENCE_SCALE = 3`).
- Constraint unchanged: the same symbol cannot be both infix and postfix.

## Exporting and importing operators from namespaces

Operator export/import is implemented to make namespace-defined operators reusable,
while keeping parsing single-pass and preserving hygiene.

### Export in the defining namespace

- Syntax:
  - `export operators;`                // export all operators defined in this namespace
  - `export prefix "~";`
  - `export infix "plus";`
  - `export postfix "squared";`
- Rules:
  - Only operators defined in the current namespace scope may be exported.
  - Export marks fixities on the local operator records; parents are not affected.

### Import in the client (after link)

- Link the file in the usual way to get a namespace handle:
  - `link "path/to/file.fn" as ops;`
- Import exported operators by namespace:
  - `import ops operators;`            // import all exported operators
  - `import ops prefix "~";`          // import a specific fixity
  - `import ops infix "plus";`
  - `import ops postfix "squared";`
- Conflicts and shadowing:
  - Redefining the same symbol+fixity in the same scope is an error.
  - Importing a different fixity for an existing symbol is an error.
  - Shadowing is allowed by importing (or redefining) in an inner `let`.

### Hygiene when importing

Imported operators expand to call their hygienic wrapper function in the defining
namespace. Internally, the call is namespace-qualified, so free variables in the
operator implementation resolve to the definition site, not the use site. Local
bindings in the importing scope will not capture imported operator bodies.

### Examples

In `ops.fn`:

```
namespace

fn negate(x) { 0 - x }
prefix 13 "~" negate;

fn plus(a, b) { a + b }
infix left 100 "plus" plus;

fn squared(x) { x * x }
postfix 120 "squared" squared;

export operators;
```

In a client file:

```
let
  link "ops.fn" as ops;
  import ops operators;
  fn negate(x) { 999 };  // local function with same name as ops.negate
in
  assert(~5 == -5);        // prefix imported
  assert(3 plus 4 == 7);   // infix imported
  assert(5 squared == 25); // postfix imported
  assert(~1 == -1);        // hygiene: uses ops.negate, not local negate
```

Selective import:

```
let
  link "ops.fn" as ops;
  import ops prefix "~";
  import ops infix "plus";

  assert(~5 == -5);
  assert(3 plus 4 == 7)
```
