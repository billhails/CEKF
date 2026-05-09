# Hygienic Macro Design Kickoff

## Status

This document is superseded by [SYNTAX.md](SYNTAX.md), which should be treated
as the clearer statement of the intended end state for this feature.

Within this document, the term `macro` should now be read as meaning `syntax`.
The two terms may be used interchangeably when discussing compile-time syntax
transformation, but the preferred current term is `syntax`.

The parser work already done under the `macro` name is still intended to be
reused. The current direction is to remove the dedicated `macro` keyword from
the parser surface while keeping and repurposing the underlying parser support.

## Goal

Start a design-first pass for real compile-time macros in F♮.

The immediate objective is to define a small but useful macro system that
helps with compiler, interpreter, macro-expander, and static-analysis work.
This should stay distinct from the current thunk-based lazy evaluation
machinery.

## Working Position

- `macro` should mean a real compile-time syntax transformer.
- `lazy fn` and `lazy operator` should remain the existing value-level
  laziness features.
- The first milestone should use syntax objects as macro inputs and
  hygienic templates as macro output.
- The first milestone should prefer keyword forms such as `quote`,
  `unquote`, and `splice` over punctuation-heavy syntax.

This keeps the first step small enough to be practical while still
unlocking useful compiler-oriented rewrites.

## Recommended First Milestone

### Scope

The first milestone should be deliberately narrow.

1. Real macros expand at compile time.
2. Macro arguments are syntax objects, not evaluated values.
3. Generated binders are hygienic by default.
4. Free identifiers in macro definitions resolve at the definition site.
5. Explicit hygiene escape hatches should wait until after the hygienic
   core works.

### Phase 1 Guardrails

Phase 1 should optimize for the smallest macro system that can clearly handle
`time`, the corrected `unless`, `for`, simple list comprehensions, and
eventually extraction of the existing `switch` desugaring from the parser.

That means phase 1 should explicitly avoid taking on:

- type classes or overloaded macro expansion
- monadic `do` notation
- grammar-definition macros
- parser-level extensibility beyond the chosen macro parameter classes
- statement forms whose expansion model is meaningfully different from normal
    expression rewriting

At the same time, phase 1 should avoid painting later work into a corner.
The MVP should leave room for:

- structured clause nests beyond plain expressions
- macro forms that need dictionary-like compile-time parameters
- later typeclass-driven surface sugar such as `do`
- richer source spans and diagnostics on expanded code

### Expansion Phase

Expansion should happen after parsing, and after namespace rewriting has
done enough work to preserve definition-site resolution, but before type
checking.

That keeps macros in the compile-time pipeline while leaving room for
later source-span preservation work.

### Recommended Surface Syntax

Use macro patterns with double-quoted literal terminals, plus keyword-based
quotation forms.

Example:

```fn
macro "unless" "(" cond: Expr ")" consequent: Nest "else" alternative: Nest {
    quote {
        if (unquote cond) {
            unquote alternative
        } else {
            unquote consequent
        }
    }
}
```

In this notation, the leading macro head is a quoted symbol, and any remaining
fixed token in the pattern is written as a quoted terminal, including
punctuation such as `"("` and `")"`.

The quoted head symbol should be registered in the parser trie and dispatched
through a generic macro parselet, so macro use sites are scanner-recognized
rather than falling through generic identifier parsing.

This fits the existing parser style better than speculative backtick or
quasiquote punctuation, which is mentioned in older notes but does not appear
to exist as a live parser feature.

### Suggested Initial Syntax Classes

To avoid solving arbitrary grammar extension on day one, start with a
small fixed set of syntax classes.

- `Expr`
- `Name`
- `Nest`
- `String`
- `Type`

These are enough to support useful call-shaped macros, header-plus-body
macros, and template-oriented rewrites without taking on clause-oriented or
binding-shaped parser entry points too early.

### Phase-1 Template Quotation

For phase 1, make the quoted template surface explicit.

- `quote { ... }` builds template syntax from a brace-delimited nest.
- `unquote name` inserts one captured syntax object.
- `splice name` inserts captured sequence syntax in sequence positions.
- `unquote` and `splice` are only valid within `quote`.

This keeps declaration patterns and template construction separate: quoted
terminals describe how a macro is recognized, while `quote`, `unquote`, and
`splice` describe how its expansion is built.

## Concrete Motivating Examples

The design should be driven by examples that are useful when writing
language tooling, not just generic metaprogramming demos.

### 1. Time Or Trace Instrumentation

Use-site:

```fn
time(parseModule(path))
trace(expr)
```

Definition shape:

```fn
macro "time" "(" expr: Expr ")" {
    quote {
        let start = now()
        in
            let result = unquote expr
            in {
                print(now() - start);
                result
            }
    }
}
```

Why this matters:

- Parser, typechecker, and optimizer work often needs lightweight
  instrumentation.
- Hygiene is required for generated `start` and `result` bindings.
- This is a small example that still exercises real macro behavior.

Minimum capability:

- Template expansion.
- Hygienic generated bindings.
- Expression parameters.

### 2. Unless

Use-site:

```fn
unless(hasBinding(name)): {
    defineMissing(name)
} else: {
    lookUpBinding(name)
}
```

Definition shape:

```fn
macro "unless" "(" cond: Expr ")" consequent: Nest "else" alternative: Nest {
    quote {
        if (unquote cond) {
            unquote alternative
        } else {
            unquote consequent
        }
    }
}
```

Why this matters:

- Front-end and analysis code often wants an inverted conditional without
  forcing the reader through `if not ...`.
- It proves the usefulness of header-plus-body syntax.
- It stays simple enough to be part of the MVP.
- In F♮ this needs an explicit `else` arm because `if` is an expression and
  both branches must have the same type.

Minimum capability:

- Expression parameters.
- Nest parameters.
- Quote and unquote.

### 3. For Loop As Expression Sugar

Use-site:

```fn
for(i = 0, i < len(xs), i + 1): {
    emit(tokenAt(xs, i))
}
```

Definition shape:

```fn
macro "for" "(" name: Name "=" init: Expr "," test: Expr "," step: Expr ")" body: Nest {
    quote {
        let fn loop(unquote name) {
            if (unquote test) {
                unquote body;
                loop(unquote step)
            } else {
                unquote name
            }
        }
        in loop(unquote init)
    }
}
```

Why this matters:

- Compiler pipelines often need compact iteration sugar.
- This example proves the need for hygienic fresh binders.
- It also shows that macro parameters are not all just expressions.

Minimum capability:

- Name parameters.
- Expression parameters.
- Nest parameters.
- Hygienic generated bindings.

### 4. List Comprehensions As Pipeline Sugar

Use-sites:

```fn
lco [x + 1 for x in xs]
lco [x + 1 for x in xs where x > 4]
lco [x + 1 for x in xs where x > 4, isOdd(x)]
```

Likely expansion shapes:

```fn
lco [x + 1 for x in xs]
=> xs |> fn (x) { x + 1 }

lco [x + 1 for x in xs where x > 4]
=> xs |? fn (x) { x > 4 } |> fn (x) { x + 1 }

lco [x + 1 for x in xs where x > 4, isOdd(x)]
=> xs
        |? fn (x) { x > 4 }
        |? fn (x) { isOdd(x) }
        |> fn (x) { x + 1 }
```

Likely definition shape:

- One `lco` macro, not separate macros for with-`where` and without-`where`
    forms.
- A required introductory keyword plus bracketed comprehension body.
- An optional `where` tail represented internally as part of one grouped macro
    pattern rather than as a separate macro form.
- A repeated comma-separated predicate sequence within that tail.
- Expansion to zero or more `|?` stages followed by one `|>` stage.

Why this matters:

- It is a compact, readable example of expression-only macro rewriting.
- It opens up the question of variant macro forms: optional and repeated
    pattern segments should still belong to one macro definition rather than a
    family of near-duplicate declarations.
- It shows how existing library operators such as `|?` and `|>` can become
    the target of surface sugar rather than requiring new core machinery.
- It exercises definition-site resolution for helper operators while still
    using captured names inside generated lambdas.
- It is a realistic convenience form for tree walks, analysis passes, and
    collection-oriented compiler code.

Minimum capability:

- Expression parameters.
- Name parameters.
- Quote and unquote.
- Definition-site resolution for expansion helpers.
- Some parser-owned way to represent optional and repeated pattern segments,
    with normalization remaining an implementation aid rather than the long-term
    model.

This should likely start with a required introductory keyword such as `lco`
rather than trying to reuse Python-style bare bracket syntax.

### 5. Switch As A Macro-Extraction Candidate

Use-site:

```fn
switch(tok.kind) {
    (TOK_IF) { parseIf() }
    (TOK_FN) { parseFn() }
    (_) { parseError(tok) }
}
```

Current status:

- This already exists today as hard-coded parser sugar.
- In [../src/pratt_parser.c](../src/pratt_parser.c), `switchFC()` rewrites
    `switch (...) { ... }` into immediate anonymous function application by
    building `fn { ... }` and applying it to the parenthesized arguments.

Likely macro definition shape:

- A clause-oriented macro rather than a single fixed template.
- Expansion to the same anonymous-function application shape the parser uses
    today.

Why this matters:

- Token-kind and AST-tag dispatch is common in parsers and interpreters.
- This is not a speculative new feature. It is a concrete existing syntax form
    that would be good to move out of the parser once the macro system is ready.

Minimum capability:

- Clause-list parameters.
- Splicing.
- Quoting or constructing composite-function clauses.

This is probably just beyond the smallest MVP, but it is an especially good
early migration target.

### 6. Rewrite-Rule Helper

Use-site:

```fn
rewrite(expr) {
    (if(true, t, f)) => t;
    (if(false, t, f)) => f
}
```

Likely definition shape:

- A pattern-dispatch macro that expands to a composite function,
  matcher, or decision tree.

Why this matters:

- Compiler passes are full of local tree rewrites.
- This targets exactly the workflow described in the roadmap.

Minimum capability:

- Syntax-pattern matching.
- Repetition.
- Structured expansion.

This is a strong second-wave target rather than a first-milestone one.

### 7. Definition-Position Boilerplate Generators

Use-site:

```fn
define_ast Expr {
    Int(value);
    Add(lhs, rhs);
    Let(name, value, body)
}
```

Likely definition shape:

- A definition-position macro that expands to typedefs,
  constructors, pretty printers, and visitors.

Why this matters:

- This directly attacks AST boilerplate.
- It aligns closely with the roadmap goal of making tree
  transformation code shorter and safer.

Minimum capability:

- Definition-position expansion.
- List splicing.
- Possibly derived helper generation.

This is probably second wave, but it is an excellent north-star
example.

### 8. Grammar Or DSL Macros

Use-site:

```fn
grammar Expr {
    Term (("+") Term)*
}
```

Likely definition shape:

- A separate later form for parser-facing syntax or macro rules,
  not necessarily the same function-shaped macro syntax.

Why this matters:

- This is the clearest route toward self-hosted front-end tooling.
- It also clearly exceeds what the MVP should try to solve.

Minimum capability:

- Rich parser-facing syntax handling.
- Likely grammar-level repetition and binding.
- Possibly a different expansion model.

This should stay out of the first milestone.

### 9. Haskell-Like `do` Notation For Monads

This is explicitly not phase 1, but it is still a good motivating stretch
goal because it would make parser combinators, state threading, error
handling, and resource-oriented workflows much easier to read.

For language implementation work, this is one of the clearest examples of a
macro that would remove a lot of plumbing without changing core semantics.

#### Why It Is Appealing

Compiler and interpreter code often has sequences like:

- parse this token stream, then parse the next form
- read state, update state, continue
- run a step that may fail, then keep going on success
- open or allocate a resource, do work, then wrap the result

Those patterns are all readable in direct style with `do` notation, but much
less pleasant when written as nested `bind` applications.

#### Likely First Surface

Because F♮ does not yet have type classes, the first plausible version would
probably need an explicit monad dictionary or namespace parameter rather than
fully overloaded Haskell syntax.

Something like:

```fn
do(Parse): {
    lhs <- parseAtom(stream);
    _ <- expect(stream, TOK_ARROW);
    rhs <- parseType(stream);
    Parse.pure(makeArrowType(lhs, rhs))
}
```

or equivalently, if the chosen macro style prefers a plain name after the
keyword:

```fn
do Parse {
    lhs <- parseAtom(stream);
    _ <- expect(stream, TOK_ARROW);
    rhs <- parseType(stream);
    Parse.pure(makeArrowType(lhs, rhs))
}
```

The exact surface does not matter yet. The important point is that before
type classes exist, the macro probably needs an explicit way to know which
`bind` and `pure` operations to use.

If a later `Monad` class or equivalent ad hoc polymorphism exists, that could
collapse to a more Haskell-like form:

```fn
do {
    lhs <- parseAtom(stream);
    _ <- expect(stream, TOK_ARROW);
    rhs <- parseType(stream);
    pure(makeArrowType(lhs, rhs))
}
```

#### Core Desugaring Shape

The natural expansion is recursive over a semicolon-separated nest of
clauses.

Roughly:

```fn
do(M): { x <- mx; rest }
=> M.bind(mx, fn (x) { do(M): { rest } })

do(M): { action; rest }
=> M.bind(action, fn (_do$1) { do(M): { rest } })

do(M): { let x = e; rest }
=> let x = e in do(M): { rest }

do(M): { finalExpr }
=> finalExpr
```

The last clause follows Haskell's shape: it must already be a monadic
expression. If the user wants to return a plain value, they must write
`M.pure(value)` or `pure(value)` depending on the final syntax chosen.

#### Why This Fits Macros Well

This is a macro, not a new runtime feature.

The expansion only needs to:

- parse a structured nest of clauses
- generate nested lambdas hygienically
- preserve definition-site resolution for `bind` and `pure`
- recursively rewrite the nest into ordinary F♮ expressions

That is exactly the kind of surface-to-core translation a real macro system
should eventually handle.

#### Useful First Restrictions

The first workable version should probably be conservative.

- Only allow simple name binders in `x <- mx`.
- Require semicolon-separated clauses.
- Require the final clause to be an explicit monadic expression.
- Leave pattern binds and `fail` behavior for later.

That would already be enough to make parser, state, and `try`-style code much
clearer.

#### Later Extension: Pattern Binds

Once the macro system can comfortably handle clause-oriented rewrites, a later
version could allow full patterns on the left of `<-`.

For example:

```fn
do(Maybe): {
    just(x) <- lookup(env, name);
    Maybe.pure(x)
}
```

That could expand through an intermediate anonymous function with pattern
matching, reusing the same machinery that already underlies `switch` and
composite functions.

Roughly:

```fn
Maybe.bind(lookup(env, name), fn {
    (just(x)) { Maybe.pure(x) }
    (_) { Maybe.fail("pattern match in do") }
})
```

This is powerful, but it clearly depends on an agreed failure story and is
therefore later than the simple-name version.

#### Why It Matters For CEKF Specifically

This would connect several roadmap threads:

- real hygienic macros
- better effect and state handling
- parser and lexer libraries written in F♮
- eventual type classes or similar ad hoc polymorphism

It is not the right first macro to implement, but it is a very good target to
keep in mind when deciding whether the macro system is expressive enough.

## MVP Versus Stretch

### Strong MVP Candidates

If a narrower first target is needed, the strongest trio is:

1. `time` or `trace`
2. `unless`
3. `for`

Together they force the design to cover:

- Hygienic generated bindings.
- Expression and Nest parameters.
- Name parameters.
- Header-plus-body forms.

List comprehensions are also a strong early follow-on example because they
mostly reuse the same machinery while additionally stressing definition-site
resolution of expansion helpers such as `|?` and `|>`, plus optional and
repeated pattern segments within a single macro form.

### Second-Wave Targets

- `do` notation for explicit monad dictionaries or later typeclass-based monads
- `rewrite`
- `define_ast`
- Grammar or DSL macros

### Early Migration Target After MVP

- `switch`

Unlike the items above, `switch` already exists and already has a known
expansion shape. The main missing piece is expressing its clause-oriented
rewrite as an ordinary macro rather than as parser-only sugar.

These are valuable, but they depend on capabilities that the first
milestone does not need to solve immediately.

## Design Questions To Answer Next

1. What exact runtime-independent representation should syntax objects use
   in the AST?
2. What is the smallest useful container-aware `splice` surface once
    `quote` and `unquote` are fixed?
3. What phase ordering is safest relative to namespace flattening and
   later pipeline stages?
4. What source metadata should macro expansions preserve now, and what
   richer span model should be reserved for later?
5. What is the smallest useful surface for syntax-class parameters?
6. What is the smallest useful way to express optional and repeated pattern
    segments within one macro definition, so forms like list comprehensions can
    omit `where` or chain comma-separated predicates without separate macros?
7. Should some later macro forms accept structured statement nests rather
    than only expression and Nest parameters?
8. If `do` notation arrives before type classes, should it require an
   explicit monad dictionary or namespace argument?
9. If pattern binds are allowed in `do`, what is the failure semantics?
10. When should explicit hygiene escapes be introduced, if at all?
11. Which existing hard-coded parser sugars should be migrated first?
    `switch` is the clearest candidate.

## Relevant Files

- [PRATT_MACRO_PHASE1_ARCHITECTURE.md](PRATT_MACRO_PHASE1_ARCHITECTURE.md)
- [LANGUAGE_IMPLEMENTATION_ROADMAP.md](LANGUAGE_IMPLEMENTATION_ROADMAP.md)
- [MACROS.md](MACROS.md)
- [OPERATORS.md](OPERATORS.md)
- [LAZY_FUNCTION_PROPOSAL.md](LAZY_FUNCTION_PROPOSAL.md)
- [SYNTAX.md](SYNTAX.md)
- [NAMESPACES.md](NAMESPACES.md)
- [Import.md](Import.md)
- [src/pratt_parser.c](../src/pratt_parser.c)
- [src/parser_info.h](../src/parser_info.h)
- [src/lambda_conversion.c](../src/lambda_conversion.c)
- [src/lazy_substitution.c](../src/lazy_substitution.c)
- [src/ast_ns.c](../src/ast_ns.c)

## Decisions For This Draft

- Include real hygienic compile-time macros over syntax objects.
- Keep current lazy-function behavior separate.
- Keep phase 1 tightly scoped to syntax-object expansion, hygiene, and a
    small fixed family of macro parameter classes.
- Bias examples toward compiler, interpreter, analyzer, and DSL work.
- Use quoted macro heads registered in the parser trie and dispatched through
    a generic macro parselet.
- Use `Nest` as the phase-1 body syntax class.
- Prefer keyword-based quote forms in the first draft.
- Delay arbitrary grammar extension and deliberate hygiene escapes.
- Treat typeclass-backed `do` notation as a useful later stress test, not as
    a phase-1 requirement.

## Next Draft Should Probably Do Two Things

1. Work through `quote`, `unquote`, and `splice` against the current AST
    container shapes, especially which positions should admit splicing in phase
    1.
2. Work through the `time`, `unless`, `for`, and list-comprehension examples
    in more detail, including their exact expansion shapes and the smallest
    useful treatment of optional and repeated macro-pattern segments.
