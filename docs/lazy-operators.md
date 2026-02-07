# Lazy Operators Proposal

The old `macro` syntax in the parser has now been replaced by `lazy fn` declarations which are more true to what the actual implementation is. It also frees up the `macro` keyword for a potential later attempt at true syntactic extensibility, but for now I'd like to return to a problem remaining after the [bespoke comparators](bespoke-equality.md) was completed.

As mentioned, operators are bound to automatically generated lazy functions declared in the output AST at the same point that the operator declaration was encountered in the input. Those lazy functions have generated names that would not pass the parser (they contain a dollar symbol) so cannot be shadowed by later definitions. For example the user-defined (preamble.yaml) `==` is bound to `opLazy$1` so `1 == 1` gets expaded to `(opLazy$1 (λ () 1) (λ () 1))` where `opLazy$1` is letrec-bound as `(opLazy$1 (λ (x1 x2) (eq (x1) (x2))))` (`eq` is the primitive).

I'd like to extend the use of the `lazy` keyword to also qualify operators. Any `lazy operator ...` declaration should retain the current behaviour, which is needed for short-circuiting operators like `and` and `or`, but operators without the `lazy` qualifier should bind directly to their implementation.

As a concrete example: `operator "_==_" left 5 EQUALTO` without the `lazy` qualifier would directly wrap that AST `EQUALTO` symbol and `1 == 1` would expand to `(eq 1 1)`.

## Things to watch out for

1. The use of operators to wrap constructors like `@` required special handling by the parser, we need to preserve that behaviour.
2. Namespace qualification is also something to preserve.

## Initial Assessment

Pros:

1. The type checker can recognize the types of those arguments to `eq` and any bespoke comparator can be invoked as it should be.
2. The lack of a named wrapper function means that beta reductions can be applied, opening the door to later constant-folding simplifications.

Cons:

1. non-lazy operators are not hygenic. That `EQUALTO` symbol could be shadowed by user declarations.

However most operators can be declared lazy, it's only equality and arithmetic where having them non-lazy has benefit (other than efficiency). And we can make those AST symbols like `EQUALTO` as obscure as we like: `INTERNAL_EQUALTO` or similar, where the chance of accidental shadowing is negligeable.

## Implementation Strategy

### Summary

The default behaviour of `operator` declarations changes from lazy (thunked)
to non-lazy (direct). A new `lazy operator` syntax retains the current
lazy/hygienic wrapping for operators that need short-circuit evaluation.

For non-lazy operators the key design decision — motivated by bespoke
comparators — is to **not generate a wrapper function at all**. Instead,
lambda conversion resolves the original implementation symbol directly at
each call site. This puts `LamPrimApp` nodes where the type checker can see
them with concrete, monomorphic argument types, enabling bespoke comparator
lookup to succeed.

### Why the wrapper must be eliminated (not just made non-lazy)

The bespoke comparator system injects replacements on `LamPrimApp(EQ, ...)`
nodes during type checking. It calls `lookupComparator(type)` which requires
a concrete `TcTypeSig` — a bare type variable is not sufficient.

If a wrapper function exists (lazy or otherwise), the `LamPrimApp` lives
inside the wrapper body where the argument types are non-generic type
variables (`α`). The type checker processes the wrapper definition once and
generalizes it to `∀α. α → α → bool`. By the time the call site
`opDirect$N(myStruct1, myStruct2)` is analyzed with concrete types, the
`LamPrimApp` has already been analyzed with `replacement = NULL` and is
never revisited.

Eliminating the wrapper for non-lazy operators moves the `LamPrimApp` to
the call site where the argument types are concrete. For example
`1 == 1` would produce `LamPrimApp(EQ, 1, 1)` directly, and the type
checker would see `int` arguments, enabling `lookupComparator` to find the
appropriate bespoke comparator (or fall back to primitive equality).

### Phase 1: YAML Schema Changes

#### 1a. Add `isLazy` to `PrattFixityConfig` (`src/pratt.yaml`)

Add a new field to the inline struct:

```yaml
isLazy: bool=false
```

This is needed for export/import propagation and for the parselets to know
which code path to take when emitting AST nodes.

#### 1b. Add `isLazy` to `AstAnnotatedSymbol` (`src/ast.yaml`)

Add a field so that the laziness decision is carried through the AST to
lambda conversion:

```yaml
AstAnnotatedSymbol:
    data:
        symbol: HashSymbol
        originalImpl: AstExpression
        isLazy: bool=false
```

This is the bridge between parser and lambda conversion: `convertAnnotatedSymbol`
will dispatch on this flag.

#### 1c. Regenerate

Run `make` to regenerate all code from the updated YAML schemas.

#### 1d. Update all initializer sites

All `PrattFixityConfig` literal initializers (e.g. `emptyConfig` in
`addOperator()`) must be extended with the new `isLazy` field. Similarly
any code that constructs `AstAnnotatedSymbol` needs the new argument.

### Phase 2: Parser Changes (`src/pratt_parser.c`)

#### 2a. Parse `lazy operator` syntax

In the `definition()` function, the `TOK_LAZY` branch currently requires
`TOK_FN` to follow. Extend it to also accept `TOK_OPERATOR`:

```c
} else if (match(parser, TOK_LAZY())) {
    if (check(parser, TOK_OPERATOR())) {
        match(parser, TOK_OPERATOR());
        res = operator(parser, /*isLazy=*/true);
    } else {
        consume(parser, TOK_FN());
        res = defLazy(parser);
    }
```

This must be done in all three places where `definition()` appears
(top-level, namespace, export contexts). Note that `export lazy operator ...`
will also need handling in the export path.

The plain `operator` path becomes:

```c
} else if (match(parser, TOK_OPERATOR())) {
    res = operator(parser, /*isLazy=*/false);
```

#### 2b. Thread `isLazy` through the operator functions

Add a `bool isLazy` parameter to:

- `operator()` (the static function that parses the pattern string)
- `operatorWithPattern()` (parses associativity, precedence, implementation)
- `addOperator()` (builds the record and wrapper definition)
- `addMixfixOperator()` (handles mixfix patterns)

#### 2c. Conditional wrapper generation in `addOperator()`

Currently `addOperator()` always calls `makeHygienicNaryOperatorDef()` which
calls `makeHygenicOperatorBody()` which emits `makeAstDefinition_Lazy(...)`.
Split this:

- **If `isLazy`**: Keep current behaviour. Call `makeHygienicNaryOperatorDef()`
  to emit `makeAstDefinition_Lazy(...)`. The wrapper name continues to be
  generated by `makeLazyName()` (e.g. `opLazy$N`).
- **If not `isLazy`**: Do not generate a wrapper function. Return
  `newAstDefinition_Blank(...)`. A `hygienicFunc` name is still generated
  (for consistency in the `PrattFixityConfig` and for the lazy fallback
  path if the operator is later re-declared lazy in an inner scope), but
  no corresponding definition is emitted.

#### 2d. Parselet AST emission

The parselets (`userPrefix`, `userInfixCommon`, `userPostfix`, mixfix
variants) currently emit:

```c
AstExpression *func = makeAstExpression_AnnotatedSymbol(
    TOKPI(tok), fixityConfig->hygienicFunc, fixityConfig->originalImpl);
```

Update to pass the `isLazy` flag:

```c
AstExpression *func = makeAstExpression_AnnotatedSymbol(
    TOKPI(tok), fixityConfig->hygienicFunc,
    fixityConfig->originalImpl, fixityConfig->isLazy);
```

The namespace `LookUp` wrapping (`if (fixityConfig->importNsRef >= 0)`)
remains unchanged — it ensures the inner expression resolves in the
defining namespace regardless of laziness.

#### 2e. Store laziness flag in `PrattFixityConfig`

Set `fixityConfig->isLazy = isLazy` in `addOperator()` alongside the other
fixity config fields.

#### 2f. Export/import paths

In `exportOp()`, the export-with-definition path (`export operator ...` and
`export lazy operator ...`) needs to accept the `lazy` qualifier:

```c
} else if (match(parser, TOK_LAZY())) {
    consume(parser, TOK_OPERATOR());
    // parse as lazy operator, then mark exported
```

In `mergeFixity()`, copy `source->isLazy` → `target->isLazy`.

The import path (`importOp()`) requires no changes beyond the `mergeFixity`
copy since it doesn't re-parse the operator definition.

### Phase 3: Lambda Conversion (`src/lambda_conversion.c`)

#### 3a. `convertAnnotatedSymbol` — three-way dispatch

Currently this function has two paths: constructor inlining and hygienic
wrapper fallback. Add a third path for non-lazy operators:

```c
static LamExp *convertAnnotatedSymbol(AstAnnotatedSymbol *annotated,
                                      LamContext *env) {
    // Path 1: Constructor inlining (unchanged)
    if (annotated->originalImpl->type == AST_EXPRESSION_TYPE_SYMBOL) {
        HashSymbol *originalSym =
            getAstExpression_Symbol(annotated->originalImpl);
        LamExp *constructor = makeConstructor(originalSym, env);
        if (constructor != NULL) {
            return constructor;
        }
        // Path 2: Non-lazy — resolve original symbol directly
        if (!annotated->isLazy) {
            return newLamExp_Var(CPI(annotated), originalSym);
        }
    }
    // Path 3: Lazy — use hygienic wrapper (existing behaviour)
    return newLamExp_Var(CPI(annotated), annotated->symbol);
}
```

Path 2 is the key change. By returning `LamExp_Var(EQUALTO)` instead of
`LamExp_Var(opLazy$N)`, the symbol flows into `convertFunCall` →
`makePrimApp`, which recognizes `EQUALTO` as `eqSymbol()` and produces
`LamPrimApp(EQ, arg1, arg2)` directly at the call site.

#### 3b. `convertFunCall` / `makePrimApp` — no changes needed

`makePrimApp` already recognizes built-in symbols (`addSymbol()`,
`eqSymbol()`, etc.) and produces `LamPrimApp` nodes. With Path 2 above,
these symbols now arrive at the call site rather than being buried inside a
wrapper body.

For non-built-in implementations (e.g. `factorial` for `_!`, `append` for
`_@@_`), `makePrimApp` returns NULL and `convertFunCall` falls through to
`makeApplication()`, producing a normal `LamApply(factorial, [arg])`. This
is correct — these are ordinary function calls, just without thunking.

#### 3c. `convertAstLazy` — unchanged

When the parser emits `AstDefinition_Lazy` (for lazy operators), lambda
conversion calls `convertAstLazy()` which registers the symbol in
`env->macros` and sets `isLazy=true` on the `LamLam`. Call sites for these
operators continue to thunk arguments. No changes needed.

### Phase 4: Type Checker — Bespoke Comparators

With the wrapper eliminated for non-lazy operators, `1 == 1` now produces
`LamPrimApp(EQ, 1, 1)` at the call site. The type checker's `analyzePrim()`
processes this directly:

1. `analyzeComparison()` unifies both operands — gets concrete type (e.g. `int`)
2. `lookupComparator(int)` checks for `eq$int` — finds it or falls back to
   primitive EQ
3. If a bespoke comparator exists, sets `prim->replacement`

For user-defined types: `myStruct1 == myStruct2` produces
`LamPrimApp(EQ, myStruct1, myStruct2)`. The type checker sees the concrete
`TcTypeSig` for the struct type, `lookupComparator` finds `eq$myStruct`,
and the replacement is injected.

No changes are needed in the type checker. The existing `analyzePrim` /
`analyzeComparison` / `lookupComparator` machinery works correctly once
the `LamPrimApp` appears at the call site with concrete types.

### Phase 5: Preamble Updates (`src/preamble.fn`)

Update operator declarations to use the new syntax. Operators that genuinely
need laziness get `lazy operator`; all others become plain `operator`:

**Stay as `lazy operator`** (need short-circuit / thunking):

- `_and_`, `_or_`, `_nand_`, `_nor_`, `_xnor_` — short-circuit boolean logic
- `_then_` — amb/backtracking (arguments must not be eagerly evaluated)
- `&_` (THUNK) — the whole point is to delay evaluation
- `here_` (callcc) — continuation capture semantics

**Become plain `operator`** (strict, both arguments always evaluated):

- `_==_`, `_!=_`, `_≠_`, `_>_`, `_<_`, `_<=_`, `_>=_`, `_<=>_` — comparisons
- `_+_`, `_-_`, `_*_`, `_/_`, `_%_`, `_**_` — arithmetic
- `-_`, `+_` — unary arithmetic
- `_@_` — cons (already bypassed via constructor inlining, but consistency)
- `not_` — boolean negation (strict)
- `_xor_` — strict boolean (all args evaluated)
- `_!` — factorial (strict)
- `_of_` — function composition (strict)
- `*_` (FORCE) — strict
- `_@@_` — append (strict)
- `<_`, `>_` — car/cdr (strict)

Note: `NUMERICIDENTITY` (unary `+`) is currently declared as `lazy fn`. Since
the unary `+` operator would become non-lazy, `NUMERICIDENTITY` can be
changed from `lazy fn` to a regular `fn`.

### Phase 6: Testing

#### 6a. Syntax tests

Add test cases for:

- `operator "..." prec impl;` — non-lazy operator (new default)
- `lazy operator "..." prec impl;` — lazy operator (explicit)
- `export operator ...` and `export lazy operator ...`
- `import ns operators;` — importing a mix of lazy and non-lazy operators

#### 6b. Semantic tests

- Verify that `and`/`or` still short-circuit (lazy operators)
- Verify that arithmetic operators produce correct results (non-lazy operators)
- Verify that bespoke comparators (`eq$T`) trigger for `==` on user types
- Verify that `cons` (`@`) still works for pattern matching
- Verify namespace import/export preserves laziness flag

#### 6c. Bespoke comparator tests

Specific tests that would have failed with a wrapper approach:

- Define a type with `eq$T`, use `==` on it, verify custom comparator is
  called
- Use `==` on a type without `eq$T`, verify primitive equality is used
- Use `==` on a polymorphic function argument — verify it still works
  (falls back to primitive equality since the type is a variable)

#### 6d. Regression tests

Run the full test suite (`make test`) after each phase to catch regressions.

### Risk Assessment

**Low risk:**

- Parser changes are localized to `addOperator` and its callers.
- Type checker requires no changes.
- Preamble changes are mechanical (add/remove `lazy` keyword).

**Medium risk:**

- Lambda conversion change to `convertAnnotatedSymbol` is small (adding one
  `if` branch) but sits on a critical path. Constructor inlining must
  continue to take precedence over the non-lazy path.
- The `PrattFixityConfig` and `AstAnnotatedSymbol` schema changes will
  regenerate code. All construction sites must be updated.
- Export/import paths need testing with the new `isLazy` flag to ensure
  it propagates correctly across namespace boundaries.

**Low concern (documented cons):**

- Non-lazy operators lose hygiene. The `originalImpl` symbol (e.g.
  `EQUALTO`) is resolved in the call-site scope (or the imported namespace
  scope for imported operators). Mitigated by using internal names that
  users are unlikely to shadow.

**Namespace qualification note:** For imported non-lazy operators, the
`AstLookUp` wrapper ensures the `originalImpl` symbol resolves in the
defining namespace. `convertLookUp` evaluates the inner `AnnotatedSymbol`
in the imported namespace's `LamContext`, so `EQUALTO` resolves correctly
there.

### Implementation Order

1. Add `isLazy` field to `PrattFixityConfig` in `pratt.yaml` and to
   `AstAnnotatedSymbol` in `ast.yaml`. Run `make` to regenerate.
2. Update all `PrattFixityConfig` initializer sites and
   `AstAnnotatedSymbol` construction sites for the new field.
3. Thread `isLazy` parameter through `operator()` →
   `operatorWithPattern()` → `addOperator()`.
4. In `addOperator()`, skip wrapper generation for non-lazy operators.
5. Update parselets to pass `isLazy` to `makeAstExpression_AnnotatedSymbol`.
6. Update `definition()` in all three contexts to parse `lazy operator`.
7. Update `exportOp()` to handle `export lazy operator`.
8. Update `mergeFixity()` to copy `isLazy`.
9. Update `convertAnnotatedSymbol` in `lambda_conversion.c` with the
   three-way dispatch.
10. Update `preamble.fn` to use `lazy operator` where needed, plain
    `operator` elsewhere.
11. Run `make test` and fix any failures.
