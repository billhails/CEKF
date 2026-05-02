# Syntax Phase 1 Implementation Plan

## Goal

Implement the phase-1 syntax system described in [SYNTAX.md](SYNTAX.md),
reusing the existing parser-side macro substrate where it helps, while moving
the source surface from `macro` to `syntax`.

The plan is intentionally narrower than the long-term syntax design.
It aims to deliver the strongest phase-1 examples:

- `time`
- `unless`
- `for`
- list comprehensions
- one simple definition-position example such as `annotate`

## Source Documents

This plan assumes the design decisions recorded in:

- [SYNTAX.md](SYNTAX.md)
- [PRATT_MACRO_PHASE1_ARCHITECTURE.md](PRATT_MACRO_PHASE1_ARCHITECTURE.md)

## Phase-1 Assumptions

This implementation plan treats the following as fixed inputs.

### Surface Forms

- `syntax name: Expr ::= ...` means an expression-initiating rule.
- `syntax name: Def ::= ...` means a definition-initiating rule.
- `syntax name ::= ...` means a helper-only rule.
- Helper-only rules default to an `Expr` result in phase 1.
- The `macro` keyword is removed from the intended long-term surface.

### Phase-1 Declaration Grammar

The parser work should follow a concrete phase-1 grammar for syntax
declarations.

```bnf
<syntax-declaration> ::= "syntax" <syntax-name> <syntax-formals-opt>
                         <initiating-annotation-opt> "::="
                         <syntax-alternative-list> ";"

<syntax-name> ::= <identifier>

<syntax-formals-opt> ::= ε
                       | "(" <identifier-list> ")"

<identifier-list> ::= <identifier>
                    | <identifier> "," <identifier-list>

<initiating-annotation-opt> ::= ε
                              | ":" <initiating-kind>

<initiating-kind> ::= "Expr"
                    | "Def"

<syntax-alternative-list> ::= <syntax-alternative>
                            | <syntax-alternative> "|"
                              <syntax-alternative-list>

<syntax-alternative> ::= <syntax-pattern> <syntax-template-block>

<syntax-pattern> ::= ε
                   | <syntax-component> <syntax-pattern>

<syntax-component> ::= <quoted-terminal>
                     | <bound-component>

<quoted-terminal> ::= <string-literal>

<bound-component> ::= <identifier> ":" <syntax-class>

<syntax-class> ::= "Expr"
                 | "Name"
                 | "Nest"
                 | "String"
                 | "Type"
                 | "Syntax" "(" <syntax-call> ")"

<syntax-call> ::= <syntax-target>
                | <syntax-target> "(" <syntax-actual-args> ")"

<syntax-target> ::= <identifier>

<syntax-actual-args> ::= <syntax-actual-arg>
                       | <syntax-actual-arg> "," <syntax-actual-args>

<syntax-actual-arg> ::= <identifier>

<syntax-template-block> ::= "{" <template-body> "}"
```

Phase-1 notes:

- An initiating annotation is what marks a rule as directly triggerable in
  phase 1. `: Expr` means expression-initiating and `: Def` means
  definition-initiating.
- No initiating annotation means helper-only.
- No parentheses means no arguments. Parentheses imply a non-empty argument
  list in phase 1, so empty `()` is not part of the planned surface syntax.
- The empty alternative is represented by `ε` in `<syntax-pattern>`, which is
  how a rule such as `syntax where(x, xs) ::= { xs } | ... ;` is modeled.
- Phase-1 `Syntax(...)` actual arguments are identifiers naming already-bound
  inherited or captured values, not arbitrary new expressions.
- `<template-body>` is intentionally left abstract here. It belongs to the
  phase-1 template grammar built around `quote` and `unquote`.

### Template Parsing Contract

Phase 1 does not need a full standalone BNF for template bodies, but it does
need an explicit parsing contract.

#### Template Entry

- The phase-1 template entry forms are `quote { ... }` and a single bound
  parameter name.
- A bare bound parameter name is semantically equivalent to
  `quote { unquote <parameter> }`.
- The braces after `quote` are template delimiters, not automatically an
  ordinary runtime `Nest`.
- Entering `quote` switches the parser into template mode.
- This shorthand only applies when the whole template body is a single already-
  bound parameter. If a literal identifier is wanted as template content, it
  should be written inside an explicit `quote { ... }`.

#### Expected Result Kind

The enclosing syntax rule determines what the template is expected to produce.

- An `Expr` rule must produce exactly one expression template.
- A `Def` rule must produce exactly one definition template.

The parser should reject obvious mismatches early rather than relying on a much
later lowering stage to discover them.

#### Use Of Existing Parser Machinery

Template mode should reuse existing parser routines where possible, but it
cannot be treated as ordinary parsing plus token substitution.

In particular, the parser needs to preserve the distinction between:

- literal identifiers written directly in the template
- unquoted identifiers or fragments coming from captured syntax
- binders introduced by the template itself

That is why template mode is an explicit parser state rather than just a normal
AST parse.

#### Unquote

- `unquote` is only valid inside the nearest enclosing `quote`.
- In phase 1, `unquote` takes a single identifier naming an already-bound
  capture or inherited parameter.
- `unquote` of arbitrary expressions is out of scope for phase 1.
- Unknown `unquote` names should be rejected as early as practical while
  parsing the syntax declaration.

The bare-parameter shorthand described above should obey the same binding and
kind checks as an explicit `unquote`.

#### Position-Sensitive Splicing

The parser should determine the splice node kind from the syntactic position in
which `unquote` appears.

Examples:

- In expression position, `unquote exp` produces an expression splice.
- In binder position, `unquote x` produces a name splice.
- In other positions, only the syntax categories explicitly supported by the
  phase-1 template machinery are legal.

This is the minimum needed to support examples such as `for` and `lco`, where
the same quoted body contains both ordinary expressions and binder positions.

#### Deferred Template Features

The following remain out of scope for phase 1:

- general `splice`
- nested quotation semantics beyond the simplest direct case
- arbitrary `unquote` expressions
- reflection of syntax objects to strings

Those should not be smuggled into the first implementation under the heading of
template parsing.

### Scope And Binding

- Syntax declarations live in the surrounding `let` or nest scope.
- Syntax declarations in one scope are recursively bound as declarations.
- Expansion availability is still textual in phase 1.
- Earlier ordinary definitions do not expand later syntax declarations.

### Phase Ordering

The implementation needs two different moments in the pipeline.

1. Parser-time registration and matching.
   Syntax declarations affect token recognition and use-site parsing while the
   parser is running.
2. Pre-typecheck hygienic lowering.
   Template instantiation should preserve enough provenance that the final
   hygienic lowering can happen after parsing, after enough namespace work to
   preserve definition-site resolution, and before type checking.

The plan assumes the parser does not immediately erase syntax provenance into a
plain ordinary AST.

### Phase-1 Template Surface

- `quote` is required.
- `unquote` is required.
- General-purpose `splice` is deferred.

This keeps the first implementation aligned with the strongest MVP examples.

### Explicit Non-Goals For Phase 1

- No clause-list fragments.
- No multi-definition results.
- No syntax-object pattern matching.
- No export or import of syntax rules.
- No general hygiene escape hatch.

## Phase-1 Exit Criteria

Phase 1 is complete when all of the following are true.

- The parser accepts `syntax` declarations in the three phase-1 surface forms.
- Expression-initiating rules work at expression use sites.
- Definition-initiating rules work in definition position.
- Helper-only recursive rules work through `Syntax(...)` calls.
- Quote and unquote templates lower hygienically.
- Definition-site resolution for literal template identifiers is preserved.
- The `time`, `unless`, `for`, `lco`, and `annotate` examples are covered by
  tests.
- The old `macro` keyword is no longer required for the feature.

## Recommended Delivery Strategy

The safest path is a sequence of vertical slices rather than one large parser
rewrite.

### Slice 1

Get `syntax` declarations parsed and registered, but keep the first slice as
simple as possible.

Target:

- rename the declaration surface from `macro` to `syntax`
- support one simple expression-initiating rule
- keep helper-only recursion and hygienic lowering out of the first slice

This is mainly a parser registration and dispatch slice.

### Slice 2

Add helper-only rules, recursive `Syntax(...)` calls, and rollback-aware
matching.

Target:

- enough machinery for the `lco` example shape
- declaration-level recursive binding
- phase-1 textual expansion availability

### Slice 3

Add template representation and hygienic lowering.

Target:

- enough provenance to distinguish introduced, literal, and unquoted names
- enough lowering support for `time`, `for`, and `lco`

### Slice 4

Add definition-position initiation.

Target:

- a simple `annotate`-style definition rule
- reuse of the same syntax matcher through a definition-entry dispatch path

## Work Plan

## Stage 0: Freeze The Phase-1 Contract

Record the assumptions above as implementation constraints.

Deliverables:

- this plan document
- [SYNTAX.md](SYNTAX.md) as the design contract

The point of this stage is to avoid reopening scope during coding.

## Stage 1: Reuse And Rename The Existing Macro Substrate

The parser already has macro-oriented structures and code paths. Phase 1 should
reuse them where doing so reduces risk.

Likely touch points:

- [src/pratt.yaml](../src/pratt.yaml)
- [src/pratt_parser.c](../src/pratt_parser.c)
- existing generated files that follow from `pratt.yaml`

Likely tasks:

- replace the parser surface keyword `macro` with `syntax`
- decide which existing `PrattMacro*` structures can be renamed in place and
  which should be generalized with minimal churn
- keep internal names temporarily if that is cheaper than a flag-day rename

Success condition:

- the source surface says `syntax`, even if some internal implementation names
  still say `macro` for one iteration

## Stage 2: Add Phase-1 Syntax Metadata

The existing phase-1 macro metadata is not enough for recursive helper rules
and initiating versus helper-only distinction.

Add or repurpose data structures for:

- syntax rule name
- result kind (`Expr` or `Def`)
- entry kind (expression, definition, helper-only)
- inherited parameters
- ordered alternatives
- ordered components
- syntax-call metadata
- template representation or a placeholder for deferred lowering

Likely touch points:

- [src/pratt.yaml](../src/pratt.yaml)

Success condition:

- generated parser metadata can describe `lco`, `where`, and `filter` without
  abusing the old one-pattern macro shape

## Stage 3: Declaration Parsing And Scope Registration

Teach the definition parser to accept the phase-1 syntax declaration forms and
install them in scope.

Likely tasks:

- parse `syntax name: Expr ::= ...`
- parse `syntax name: Def ::= ...`
- parse helper-only `syntax name ::= ...`
- register initiating head tokens in the trie only for initiating rules
- keep helper-only rules out of direct entry dispatch
- implement recursive declaration binding with placeholders or incomplete rule
  entries
- keep expansion availability textual by only enabling use after full
  declaration installation

Likely touch points:

- [src/pratt_parser.c](../src/pratt_parser.c)
- [src/pratt_scanner.c](../src/pratt_scanner.c) if trie behavior needs a small
  helper

Success condition:

- syntax declarations parse in `let` blocks
- helper-only rules can be declared in the same scope
- initiating rules become visible only after their declaration completes

## Stage 4: Generic Syntax Matching Engine

Implement the actual matcher used at syntax use sites.

Likely tasks:

- add a generic expression-entry syntax parselet
- add a generic definition-entry syntax dispatcher
- implement ordered alternative matching
- implement local rollback on syntax-level mismatch
- distinguish mismatch from real nested parser errors
- implement component parsing for:
  - Terminal
  - Expr
  - Name
  - Nest
  - String
  - Type
  - SyntaxCall
  - Empty
- enforce duplicate-binding errors within one alternative

Likely touch points:

- [src/pratt_parser.c](../src/pratt_parser.c)
- possibly a new helper file such as `src/pratt_syntax_parse.c`

Success condition:

- `time`, `unless`, and a non-hygienic first cut of `lco` can be matched and
  turned into syntax-use results

## Stage 5: Template Representation And Hygienic Lowering

This is the most important semantic stage.

The parser should not immediately lose the provenance needed for hygiene.
The implementation should preserve enough information to distinguish:

- template-introduced identifiers
- literal identifiers written in the template
- unquoted identifiers and fragments coming from the use site

Likely tasks:

- parse `quote { ... }` into a syntax template representation
- represent `unquote` as typed splice nodes
- support binder-position unquote
- introduce a dedicated syntax lowering pass before type checking
- freshen template-introduced binders during lowering
- preserve use-site identity for unquoted fragments
- preserve definition-site resolution for literal template identifiers

Likely touch points:

- [src/pratt_parser.c](../src/pratt_parser.c)
- a new syntax lowering file or files
- whichever AST or intermediate representation carries syntax-use nodes until
  lowering

Success condition:

- `time` and `for` are hygienic
- `lco` expands with the intended distinction between captured names and helper
  identifiers

## Stage 6: Definition-Position Initiation

Once expression rules are stable, add definition-entry support.

Likely tasks:

- hook syntax dispatch into the existing definition parser
- require `: Def` on initiating definition rules
- lower `Def` templates to one `AstDefinition`
- reject result-kind mismatches clearly

Likely touch points:

- [src/pratt_parser.c](../src/pratt_parser.c)
- syntax lowering code from stage 5

Success condition:

- a simple `annotate`-style example works in definition position

## Stage 7: Migration And Cleanup

At this point the implementation exists. Then clean up the old naming and test
surface.

Likely tasks:

- update tests that still say `macro`
- rename or replace test files such as
  [tests/fn/test_macro_definitions.fn](../tests/fn/test_macro_definitions.fn)
- update older macro-oriented docs to point at the new syntax implementation
- decide whether remaining internal `Macro` names are worth renaming now or can
  wait for a later cleanup pass

Success condition:

- the parser surface is consistently `syntax`
- tests no longer rely on `macro`

## Validation Plan

Each stage should have a narrow validation target.

### Declaration Parsing

- parse a simple `syntax time: Expr ::= ...`
- parse helper-only syntax declarations
- parse a recursive helper group like `lco` / `where` / `filter`

### Matching

- `time(expr)`-style use-site matching
- `unless(...) ... else ...` header-plus-body matching
- recursive helper matching for `lco`

### Hygiene

- generated binder does not capture use-site names
- literal helper identifiers resolve at definition site
- unquoted identifiers preserve use-site identity

### Definition Position

- one `annotate`-style example lowers to a single definition
- result-kind mismatch produces a clear error

## Test Targets

The first implementation should at least add or update tests for:

- simple expression syntax declarations
- helper-only recursive syntax declarations
- duplicate binding rejection inside one rule alternative
- textual expansion availability inside one `let`
- hygienic binder generation
- definition-position initiation

The existing macro tests are a good migration starting point, but they should be
updated to the `syntax` surface rather than treated as a permanent compatibility
requirement.

## Deferred Work

This plan deliberately leaves the following for later.

- clause-oriented fragments for `switch` and `do`
- multi-definition results for `define_ast`
- syntax-object pattern matching for `rewrite`
- import or export of syntax rules
- hoisting later syntax declarations to the start of a `let` block
- general `splice`
- hygiene escape hatches

## Main Risks

### Rollback Semantics

The matcher needs a precise distinction between recoverable mismatch and real
parser error. If this boundary is not designed carefully, debugging syntax
rules will be unpleasant.

### Deferred Lowering Carrier

If hygienic lowering is deferred until after parsing, the implementation needs a
clean place to store syntax-use results without immediately flattening them to
ordinary AST.

### Rename Churn

The codebase already has `Macro` names in parser metadata and tests. A total
rename may be desirable, but it should not be allowed to block the first
working syntax slice.

## Recommended First Coding Slice

If only one slice is attempted first, it should be:

1. parse `syntax name: Expr ::= ...`
2. register the initiating head token in the trie
3. match a single-alternative expression rule
4. lower a trivial quoted template without hygiene sophistication yet
5. migrate [tests/fn/test_macro_definitions.fn](../tests/fn/test_macro_definitions.fn)
   to the `syntax` keyword

That slice is small enough to prove the end-to-end integration path before the
recursive helper and hygiene-heavy examples are attempted.
