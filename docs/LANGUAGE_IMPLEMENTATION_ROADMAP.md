# Language Implementation Roadmap

## Goal

Make F♮ better suited to implementing compilers, interpreters, macro
expanders, static analyzers, and domain-specific languages.

F♮ already has several strong foundations for this kind of work:

- Algebraic data types and pattern matching.
- Hindley-Milner type inference.
- User-defined operators and a scoped Pratt parser.
- A self-hosting rewrite prototype in `fn/rewrite`.

The main gap is that it is currently easier to express computations than to
build full language tooling around syntax, trees, modules, diagnostics, and
compiler-oriented data structures.

## What To Optimize For

When prioritizing future work, prefer features that:

1. Remove hard-coded parser and compiler cases.
2. Make AST transformation code shorter and more reliable.
3. Preserve source information across rewrites.
4. Make self-hosted front-end code practical.
5. Improve diagnostics and testability.

## Highest-Value Features

### 1. Real Hygienic Macros

The current `lazy fn` and `lazy operator` support is useful for short-circuiting
and lightweight operator abstraction, but it is not a full macro system.

Desirable properties:

- Reclaim `macro` for true syntactic extension.
- Syntax objects instead of raw values.
- `quote`, quasiquote, and unquote.
- Pattern-based syntax destructuring.
- Hygiene by default, with explicit escape hatches.
- A clear phase distinction between compile-time and run-time.
- Expansion results tagged with source spans.

Why this matters:

A language that can define new surface forms without editing the parser for
each one becomes much more suitable for hosting other languages and
domain-specific notations.

### 2. Source Spans And Diagnostics As First-Class Data

Desirable properties:

- Every parsed node carries a source span.
- Rewritten nodes preserve or compose spans.
- A standard diagnostic type with severity, primary span, related spans, and
  notes.
- Parser error recovery so a single syntax error does not terminate the whole
  analysis.
- Golden-test helpers for diagnostics.

Why this matters:

Compilers are judged heavily by diagnostics. Poor spans and weak error
reporting turn language work into debugging by `printf`.

### 3. Compiler-Oriented Collections And Text Handling

Desirable properties:

- Efficient maps and sets keyed by symbols, strings, and integers.
- Mutable or transient arrays and vectors.
- Byte strings, string builders, and token buffers.
- A stable symbol interning API.
- Fast iteration primitives.

Why this matters:

Parsers, type checkers, symbol tables, and code generators spend much of their
time manipulating collections and text. Without these, self-hosted tooling
stays slower and more awkward than it needs to be.

### 4. Better Modules And Namespace Control

Desirable properties:

- Explicit export and import of functions, types, operators, and aliases.
- Re-export and selective import.
- Aliases accessible across namespaces.
- Predictable shadowing rules.
- Eventual support for separate compilation.

Why this matters:

Compiler implementations are usually split across lexer, parser, AST,
typechecker, simplifier, and backend modules. Those boundaries need better
support than ad hoc file linking.

### 5. Tree Manipulation Ergonomics

Desirable properties:

- Generated visitors, folds, and rewrites for user-defined ADTs.
- Derived equality, ordering, and pretty printing where sensible.
- Record update syntax.
- Fresh-name and substitution utilities.
- Library support for free-variable, binder, and alpha-equivalence work.

Why this matters:

Most compiler code is tree plumbing. Small ergonomic wins compound quickly.

## Important But Probably Second Wave

### 6. Parser And Lexer Libraries In F♮

This could build on the current scanner and parser ideas, but exposed as
libraries or DSLs that F♮ programs can use directly.

Desirable properties:

- Token and span abstractions.
- Simple parser combinators or a grammar DSL.
- Pratt or precedence-climbing support as a library.
- Unicode-aware lexing helpers.

Why this matters:

If writing a parser in F♮ still means dropping to C or hand-rolling too much
machinery, the language is not yet good for writing languages.

### 7. Type Classes Or Similar Ad Hoc Polymorphism

Desirable properties:

- `Eq`, `Ord`, `Show`, `Parse`-like abstractions.
- Cleaner generic collection and compiler utility APIs.
- Less boilerplate around comparisons and printing.

Why this matters:

Useful, but not a blocker. A macro system, spans, and data structures will buy
more leverage sooner.

### 8. Better Effect And State Story

Desirable properties:

- A cleaner I/O story.
- Local mutation or disciplined state threading for algorithms like union-find
  and parser state.
- Performance-oriented escape hatches where they are clearly justified.

Why this matters:

Real compilers are stateful. Pure style is pleasant for some passes, but too
much ceremony around state becomes friction.

## Lower Priority For This Specific Goal

The following can still be valuable, but they do not move F♮ toward "good for
writing programming languages" as directly as the items above:

- More numeric tower work.
- Replacing the GC on its own.
- General application-level libraries unrelated to tooling.
- Surface syntax additions that do not improve extensibility or diagnostics.

## Suggested Order

If the goal is to increase language-implementation usefulness as quickly as
possible, a reasonable order is:

1. Real hygienic macros with syntax objects and spans.
2. Better diagnostics and span propagation.
3. Compiler-oriented collections: maps, sets, vectors, builders, and bytes.
4. Explicit module and import or export improvements.
5. Tree-manipulation and binder utilities.
6. Self-hosted lexer and parser libraries.
7. Type classes or similar generic abstraction support.
8. Faster native code generation or LLVM as the payoff stage for self-hosting.

## Practical Near-Term Milestones

A near-term roadmap that fits the current CEKF direction could be:

1. Reclaim `macro` for true syntactic extension, while keeping `lazy fn` for
   call-by-name style operators.
2. Make source spans survive parsing, macro expansion, desugaring, and later
   rewrites.
3. Strengthen namespace imports and exports, including aliases and operators.
4. Add bytes, builders, maps, and sets to the standard toolbelt.
5. Grow `fn/rewrite` into a reference front-end toolkit, not just a pass
   playground.
6. Use that toolkit to implement a non-trivial parser or DSL front end in F♮.

## Decision Rule

When choosing between two future features, prefer the one that makes one of the
following shorter, safer, or more self-hostable:

- Writing a parser.
- Representing an AST.
- Transforming syntax trees.
- Producing diagnostics.
- Organizing a multi-pass compiler.
- Bootstrapping the next piece of the toolchain.

This is the main test for whether the language is becoming better at writing
languages.

## Related Notes

- [MACROS](MACROS.md)
- [NAMESPACES](NAMESPACES.md)
- [OPERATORS](OPERATORS.md)
- [LAZY FUNCTION PROPOSAL](LAZY_FUNCTION_PROPOSAL.md)
- [TypeClasses](TypeClasses.md)
- [TODO](TODO.md)