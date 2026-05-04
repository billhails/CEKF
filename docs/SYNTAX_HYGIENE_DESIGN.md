# Syntax Hygiene Design

## Purpose

This document records the implemented phase-1 syntax hygiene architecture.

The shipped design replaces the old parser-resident substitution path with an
AST-carried, post-namespace, pre-lambda lowering pipeline that:

- preserves declaration-site resolution for literal template identifiers
- preserves use-site identity for captured fragments
- freshens template-introduced binders
- keeps parser-time syntax recognition separate from hygienic lowering
- ensures `lambda_conversion.c` only sees ordinary AST

## Implemented Pipeline

The syntax pipeline now sits here:

```text
Parse
  -> nsAstProg
  -> prepareAst
  -> lowerAst
  -> lamConvertProg
  -> tc_analyze
  -> ... existing later stages
```

Why this placement remains correct:

- parser-time substitution lost too much provenance for hygiene
- namespace rewriting must already have run before declaration-site literal
  references are finalized
- lambda conversion and later stages should stay unaware of syntax carriers

## File Boundaries

### `src/pratt_parser.c`

Owns:

- syntax entry hooks from expression and definition parsing
- scanner-trie registration for initiating heads
- parser-core utilities used by syntax code
- parser-scoped quoted-template behavior such as `quotedAtomSymbols`

It no longer owns hygienic expansion.

### `src/syntax_parse.h` and `src/syntax_parse.c`

Own parser-adjacent syntax support:

- syntax declaration parsing after the leading `syntax` keyword
- rollback-aware syntax matching
- helper-rule invocation through `Syntax(rule(args))`
- carrier construction for matched syntax uses
- duplicate and obvious shadowing checks for helper alternatives

### `src/syntax_template.h` and `src/syntax_template.c`

Own template parsing and durable template IR:

- `quote { ... }` parsing
- bare-parameter shorthand parsing
- `unquote` handling in expression and binder positions
- quoted-template metadata
- conversion from parser-side template bodies into durable syntax-template IR

The old parser-only quote wrapper sentinel is gone. Quoted templates are now
represented explicitly, and parser-side token forcing is centralized in the
parser core instead of temporary rule-table mutation.

### `src/ast_prepare.h` and `src/ast_prepare.c`

Own syntax preparation after namespace processing:

- declaration-site literal resolution for syntax templates
- preparation of syntax declarations and syntax-use captures in lexical order
- maintenance of the symbol world needed by later hygienic lowering

### `src/ast_lower.h` and `src/ast_lower.c`

Own hygienic instantiation and erasure:

- syntax-use lookup by declaration id
- template instantiation
- introduced-binder freshening
- use-site capture splicing
- declaration-site literal preservation
- result-kind enforcement and lowering diagnostics
- carrier erasure before lambda conversion

### `src/lambda_conversion.c`

Contains only defensive checks now. If a syntax carrier leaks past lowering, it
reports a targeted error instead of silently relying on undefined behavior.

## AST Carriers And Template IR

The implemented carrier model uses:

- `AstDefinition_SyntaxDecl`
- `AstExpression_SyntaxUse`
- `AstDefinition_SyntaxUse`

These survive parsing and namespace processing, but must not survive `lowerAst`.

Each syntax declaration stores:

- rule name
- entry kind
- result kind
- inherited parameters
- ordered alternatives
- durable syntax-template IR
- declaration id

Each syntax use stores:

- referenced declaration id
- selected alternative index
- captured or inherited bindings
- parser info for diagnostics

The template IR distinguishes at least these semantic categories:

- declaration-site literal references
- template-introduced binders
- references to those introduced binders
- unquoted captured fragments

That distinction is what the old `substituteSyntax*` path could not preserve.

## Hygiene Guarantees

The current implementation enforces three core hygiene guarantees.

### Declaration-Site Literal Resolution

Identifiers written literally in a template keep the declaration-site meaning
recorded during `prepareAst`, even when the use site shadows the same surface
name.

### Use-Site Capture Preservation

Captured fragments inserted through `unquote` preserve the use-site binding
identity prepared before lowering. Lowering does not rebind them to the syntax
declaration site.

### Introduced Binder Freshness

Binders introduced by the template receive fresh symbols during `lowerAst`.
References to those binders are rewritten consistently to the same freshened
symbols.

## Removed Paths

The following transitional mechanisms are now gone.

- parser-time recursive `substituteSyntax*` expansion
- quote-wrapper sentinels such as `makeSyntaxQuotedTemplate` /
  `unwrapSyntaxQuotedTemplate`
- temporary parser rule-table mutation used only to force quoted atoms

The parser still recognizes and matches syntax rules, but successful expansion
now always flows through AST carriers plus `prepareAst` / `lowerAst`.

## Diagnostics And Validation

Lowering now reports explicit errors for:

- unresolved syntax declaration ids
- result-kind mismatches between a declaration and its use site
- leaked syntax carriers reaching lambda conversion

The core validation coverage is:

- runtime and dump-ast hygiene tests for declaration-site shadowing,
  use-site unquote shadowing, and introduced-binder freshness
- focused carrier-leak and mismatch coverage in
  `tests/src/test_ast_lower.c`
- the broader syntax and macro regression set under `make test`

## Remaining Deferred Work

The remaining deferred items are beyond the completed phase-1 hygiene work.

- broader fragment kinds beyond `Expr` and `Def`
- multi-definition syntax results beyond the supported single-definition shape
- imported or exported syntax rules
- hoisting later syntax declarations to the start of a `let` block
- general `splice`, repetition-oriented templates, and clause-sequence
  fragments
- explicit hygiene escape hatches
- broadening initiating rules beyond the current single-alternative entry path

## Recommendation For Follow-On Work

Future syntax work should preserve the current separation of concerns.

- Keep parsing and rollback in the parser-side syntax modules.
- Keep hygienic provenance and binder freshness in the AST-side passes.
- Do not reintroduce parser-time direct expansion shortcuts for new features.

That separation is now proven by the current test set and should remain the
baseline for any later stage-6-plus syntax extensions.
