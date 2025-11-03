# End-to-End: User-Defined Operators, Export/Import, and Hygiene

This guide shows how to define operators, export them from a namespace, import them elsewhere with lexical scoping, and avoid hygiene pitfalls. It mirrors docs/OPERATORS.md and provides runnable examples.

## Quick start

- Define operators inside a namespace and export them:

```fn
namespace

fn negate(x) { 0 - x }
prefix 13 "~" negate;

fn plus(a, b) { a + b }
infix left 100 "plus" plus;

fn squared(x) { x * x }
postfix 120 "squared" squared;

export operators;
```

- Import into another file and use them:

```fn
let
    link "import_ops_lib.fn" as ops;
    import ops operators;

in
    assert(~5 == -5);
    assert(2 plus 3 == 5);
    assert(6 squared == 36);
```

## Selective import

You can import specific fixities only:

```fn
let
    link "import_ops_selective_lib.fn" as ops;
    import ops prefix "~";
    import ops infix "plus";
    // postfix "squared" is not imported here
in
    assert(~5 == -5);
    assert(3 plus 4 == 7);
```

## Nested scopes and shadowing

Imports are lexical. Inner scopes can shadow imported operators without affecting outer scopes.

```fn
let
    link "import_ops_lib.fn" as ops;
    import ops infix "plus";

in
    assert(2 plus 3 == 5);   // imported

    {
        let
            fn times(a, b) { a * b };
            infix left 100 "plus" times; // shadows imported plus
        in
            assert(2 plus 3 == 6);       // local shadow
    }

    assert(2 plus 3 == 5);   // outer import still in effect here
```

## Hygiene and name collisions

Imported operators are hygienic: they call a hygienic wrapper that captures the implementation at definition site, so local functions can’t capture them.

```fn
let
    link "import_ops_lib.fn" as ops;
    import ops prefix "~";
    import ops infix "plus";

    // Local function colliding with the library's prefix callee name
    fn negate(x) { 999 };

in
    assert(2 plus 3 == 5);   // uses imported infix plus
    assert(~5 == -5);        // still uses imported negate
```

Note: once an operator symbol (like `plus`) is imported as an operator, it’s tokenized as an operator, not an identifier. Defining a function named `plus` in the same scope will be rejected as a syntax error. The hygiene guarantees don’t require such a definition; the above `negate` collision is sufficient to demonstrate correctness.

## Common error messages

- Redefinition conflict (same fixity):
  - `import redefines infix operator plus`
- Fixity mismatch conflict (different fixities already present):
  - `import defines infix operator plus over existing postfix operator`
- Not exported:
  - `namespace ops did not export postfix 'squared'`

These errors are reported with file and line information when run via `./bin/fn`.

## Limitations and notes

- A symbol cannot be both infix and postfix.
- Redefinition is local-only: inner scopes may shadow, but you can’t redefine the same fixity within one scope.
- Precedence is scaled internally (×3) to keep room for ±1 adjustments during parsing.
- Macro-based operators are lazily evaluated via thunks; hygiene is implemented via namespace-qualified wrappers.

## Try it locally

- Run the full suite (passing and negative tests) using the Makefile target:

```
make test
```

- Or run one file manually:

```
./bin/fn --include=fn --assertions-accumulate tests/fn/test_import_operators_selective.fn
```

If you hit an error importing a symbol, check whether:
- The source namespace exported the requested fixity.
- You’re not redefining the same fixity in the same scope.
- You’re not mixing fixities for the same symbol in one scope.
