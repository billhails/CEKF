# Syntax Phase 1 Implementation Plan

## Goal

Phase 1 of the local syntax system is complete.

This document now serves two purposes:

1. record the delivered phase-1 model described in [SYNTAX.md](SYNTAX.md)
2. record the next follow-on work, beginning with syntax import and export

There is also one intentionally narrow parser-surface adjustment under
consideration: restoring `macro` as the public entry-rule declaration while
retaining `syntax` for helper rules. That follow-on is meant to be a focused
parser-only change, not a new syntax-engine design.

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

- `macro name: Expr helper;` means an expression-initiating rule.
- `macro name: Def helper;` means a definition-initiating rule.
- `syntax name ::= ...` means a helper-only rule.
- Helper-only rules default to an `Expr` result in phase 1.
- A `macro` declaration is only a public entry point. It does not own a
  pattern or template directly.
- The helper named by a `macro` declaration must resolve to a helper-only
  `syntax` declaration with zero parameters.

Current implementation note:

- Initiating rules still use a dedicated registered head token and currently
  accept only one alternative.
- Helper-only rules own their full patterns and may use `|` alternatives.

### Phase-1 Declaration Grammar

The parser work should follow a concrete phase-1 grammar for syntax
declarations.

```bnf
<syntax-declaration> ::= <macro-declaration>
                       | <helper-syntax-declaration>

<macro-declaration> ::= "macro" <syntax-name> ":" <initiating-kind>
                        <helper-syntax-name> ";"

<helper-syntax-declaration> ::= "syntax" <syntax-name>
                                <syntax-formals-opt> "::="
                                <syntax-alternative-list> ";"

<syntax-name> ::= <identifier>

<helper-syntax-name> ::= <identifier>

<syntax-formals-opt> ::= ε
                       | "(" <identifier-list> ")"

<identifier-list> ::= <identifier>
                    | <identifier> "," <identifier-list>

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
                     | "empty"

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

- A `macro` declaration is what marks a rule as directly triggerable in
  phase 1. `: Expr` means expression-initiating and `: Def` means
  definition-initiating.
- A `syntax` declaration is helper-only.
- No parentheses means no arguments. Parentheses imply a non-empty argument
  list in phase 1, so empty `()` is not part of the planned surface syntax.
- A `macro` declaration does not take formals. Its helper target must be a
  zero-arity helper rule.
- The empty alternative is represented by `ε` in `<syntax-pattern>`, which is
  how a rule such as `syntax where(x, xs) ::= { xs } | ... ;` is modeled.
- A bare unquoted `empty` token is also accepted anywhere a positional
  component could appear and is ignored completely. This is only a readability
  marker for action-only branches; it does not contribute a terminal or a
  binding.
- In the current implementation, initiating rules are a narrower subset than
  the grammar above: they accept one alternative, while helper-only rules use
  `<syntax-alternative-list>`.
- Helper-only matching now tries consuming alternatives before empty ones,
  while preserving declaration order within each group.
- Helper-only declarations reject exact duplicate alternatives and obvious
  strict-prefix shadowing at parse time.
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
- No phase-1 implementation of export or import of syntax rules.
- No general hygiene escape hatch.

## Follow-On Plan: Importing And Exporting Syntax

The next syntax milestone after the phase-1 local system should be namespace
transport of syntax declarations.

The main surface constraint for this feature is intentionally narrow:

- Only initiating syntax rules need explicit export or import.
- Helper-only syntax rules should come along automatically as the transitive
  dependency closure of an exported initiating rule.

That keeps the user model close to operator import/export: users choose the
entry points they want to make public, and the implementation takes care of the
private helper machinery needed to make those entry points expand.

### Why Syntax Import Cannot Be Parser-Only

Imported operators are currently handled by a parser-side namespace cache.
That is sufficient for operators because the parser consumes them immediately
and later stages do not need durable operator declarations in the AST.

Imported syntax is different.

- Parser-time syntax use produces `AstExprSyntaxUse` or `AstDefSyntaxUse`
  carriers that refer to a durable declaration by `declarationId`.
- Hygienic lowering later resolves those carriers by walking the visible
  `AstSyntaxDecl` definitions in lexical scope.
- The current lowerer only sees syntax declarations that are present in the
  AST definition lists.

So a parser-only copy of imported syntax metadata would be insufficient. It
would let parsing succeed, but lowering would fail because the imported
declaration would not exist in the importing module's durable syntax scope.

That gives one hard architectural rule for the design:

- importing syntax must materialize durable syntax declarations, or a durable
  AST wrapper equivalent to syntax declarations, in the importing module.

### Recommended Surface Forms

The existing namespace forms should be reused rather than introducing a second
module system.

Recommended exports:

- `export syntax name;`
- `export syntaxes;`

Recommended imports:

- `import ns syntax name;`
- `import ns syntaxes;`

Phase-2 notes:

- `name` must name an initiating rule, not a helper-only rule.
- Exporting a helper-only rule directly should be a parser error.
- `export syntaxes;` should export all locally-defined initiating rules in the
  current parser scope.
- `import ns syntaxes;` should import all exported initiating rules from the
  namespace and bring their helper closure with them.

This mirrors the existing `operator` or `operators` split and keeps the surface
predictable.

### Representation Strategy

The existing operator path already has the right high-level shape:

1. parse a linked namespace once
2. capture exported parser metadata into a cache indexed by `nsRef`
3. import from that cached snapshot into another parser scope

Syntax import should reuse that shape, but the snapshot must be richer than the
operator snapshot.

The cache entry for a namespace should grow from:

- exported operators only

to:

- exported operators
- exported initiating syntax declarations
- helper declarations needed by those exported initiating declarations

The helper part should not be stored as a second public export table. Instead,
it should be stored as private support data attached to each exported
initiating rule or to the namespace syntax snapshot as a whole.

### Namespace Syntax Snapshot

The current `PrattExportedOps` cache is too narrow for syntax. A follow-on
snapshot structure should carry both the parser-time syntax metadata and the
durable AST declaration form.

The minimum useful structure looks like:

- exported initiating rule names keyed by rule name
- a durable cloned `AstSyntaxDecl` for each exported initiating rule
- a durable cloned `AstSyntaxDecl` for each helper in the transitive closure
- parser-side `PrattMacroSpec` data for those same rules
- a mapping from original declaration ids to cloned declaration ids

That last mapping matters because imported syntax uses must point at
declaration ids that are valid in the importing module's durable syntax scope,
not at ids from the original namespace parser.

### Computing The Helper Closure

The closure rule should be simple and structural.

For each exported initiating declaration:

1. inspect every pattern item in every alternative
2. whenever a `Syntax(...)` hole references another rule, record that target
3. recursively visit the referenced rule
4. stop once the reachable set is closed

Constraints:

- The exported root must be initiating.
- Reachable helper rules may be helper-only or initiating, but any initiating
  rule reached as a dependency should be treated as private support unless it is
  also explicitly exported by name.
- Duplicate visits should be ignored.
- Missing helper references should remain declaration-time errors in the source
  namespace; import should not attempt to repair broken syntax graphs.

This is deliberately a declaration-graph walk, not a template-body analysis.
Helper reachability is determined by `Syntax(...)` calls in patterns.

### Import Semantics

On `import ns syntax name;`, the importing parser should:

1. load the namespace snapshot by `nsRef`
2. locate the exported initiating rule named `name`
3. copy that rule and its helper closure into the current local syntax scope
4. assign fresh local `declarationId` values to every copied declaration
5. rewrite every copied `AstSyntaxDecl` and `PrattMacroSpec` so internal helper
   references and declaration ids point at the fresh local copies
6. register the imported initiating head token in the local parser trie
7. install the copied durable syntax declarations into the importing module's
   AST-visible definition stream

The final step is the key difference from operator import. Imported syntax must
not disappear after parsing.

### AST Materialization Strategy

There are two plausible ways to make imported syntax visible to lowering.

1. Rewrite the `import ... syntax ...` definition into concrete local
   `AstSyntaxDecl` definitions at parse time.
2. Introduce a dedicated AST import node for syntax, then elaborate that node
   into concrete `AstSyntaxDecl` definitions during `prepareAst` before
   lowering.

The first option is the smaller change and matches the current phase-1
architecture better.

Reasons:

- `ast_lower.c` already knows how to discover visible syntax declarations by
  walking ordinary definition lists.
- imported syntax should behave like a local lexical declaration once imported
- avoiding a new durable AST form keeps lowering simpler

So the recommended first implementation is:

- parse `import ns syntax ...` as a definition
- immediately elaborate it into one or more ordinary `AstSyntaxDecl`
  definitions in the current scope
- keep a blank or lightweight import definition only if source-preserving AST
  printing is important

### Parser Storage Changes

The existing namespace cache and parser tables need only incremental changes.

Recommended parser-side work:

- extend the namespace export snapshot to include syntax exports
- add a helper to capture exported initiating syntax plus reachable helpers
- add a helper to clone a syntax snapshot into a fresh parser scope
- keep imported syntax marked with source namespace metadata for diagnostics,
  paralleling imported operators

The imported `PrattMacroSpec` clones should preserve:

- entry kind
- result kind
- head symbol
- alternatives and template metadata
- import namespace metadata

But they must not preserve original declaration ids unchanged.

### Name And Conflict Rules

The initial conflict policy should stay conservative.

- Importing syntax may not silently replace a local rule of the same name.
- Importing syntax may not silently replace an already-imported rule of the
  same name from another namespace.
- Importing an initiating head token that is already registered for another
  initiating syntax rule in the same scope should be a parser error.
- Helper names imported as support data should also participate in conflict
  checks, because they become ordinary lexical syntax declarations after
  import.

This is stricter than an eventual shadowing design, but much easier to reason
about while the feature is new.

### Export Validation Rules

Export should validate at declaration scope, not at use sites.

- `export syntax name;` requires `name` to resolve to a local initiating
  syntax rule.
- exporting a non-local imported syntax rule should initially be rejected,
  matching the current operator policy for non-local exports.
- exporting a helper-only rule should be rejected with a targeted error.
- `export syntaxes;` should mark only local initiating rules for export.

This keeps namespace interfaces explicit and avoids accidentally re-exporting a
large helper graph from imports.

### Scope And Lowering Expectations

Once imported syntax has been materialized into ordinary `AstSyntaxDecl`
definitions, the rest of the pipeline should not need a fundamentally new
model.

- `prepareAst` still resolves declaration-site literal names inside the copied
  declarations in the importing module's lexical context.
- `lowerAst` still finds visible declarations by ordinary lexical traversal.
- syntax uses produced after import point at the imported local declaration ids.

This gives the desired semantics:

- the imported rule's helper machinery comes along automatically
- the imported rule behaves like syntax declared in the importing scope
- hygiene still uses the imported declaration as the definition site

### Suggested Delivery Order

The work should be staged to keep validation narrow.

1. Add export flags and snapshot capture for local initiating syntax only,
   without import yet.
2. Add helper-closure computation and tests for exported initiating rules.
3. Add import of one named initiating syntax rule from a namespace.
4. Materialize imported rules as local `AstSyntaxDecl` definitions with fresh
   declaration ids.
5. Add bulk forms `export syntaxes;` and `import ns syntaxes;`.
6. Add conflict and diagnostic polish.

### Test Plan

The minimum useful tests for this feature are:

- export one initiating rule with one helper and import it into another file
- export one initiating rule with a recursive helper family and verify the full
  closure imports
- export multiple initiating rules that share a helper and verify the helper is
  imported once
- reject `export syntax helperName;` when the target is helper-only
- reject import when the target namespace did not export that initiating rule
- reject import when the imported helper graph conflicts with an existing local
  syntax rule
- verify that imported syntax still lowers hygienically by using a binder-
  introducing template across the namespace boundary

### Questions To Leave Open Initially

The first cut does not need to solve every namespace-interface question.

- Whether imported syntax can later be selectively renamed.
- Whether re-export of imported syntax should be allowed.
- Whether helper names should remain invisible to explicit source lookup after
  import, even though they exist as local support declarations.
- Whether syntax exports should eventually support manifest-style curation
  beyond individual names and `syntaxes`.

Those are useful later refinements, but they are not prerequisites for the
basic initiating-rule export/import design.

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

- parse `macro name: Expr helper;`
- parse `macro name: Def helper;`
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

## Implemented Outcome

The planned stage-5 through stage-10 work is now complete.

The implemented architecture is:

```text
Parse
  -> nsAstProg
  -> prepareAst
  -> lowerAst
  -> lamConvertProg
  -> tc_analyze
  -> ... existing later stages
```

The important file boundaries are now stable.

- [src/pratt_parser.c](../src/pratt_parser.c) owns syntax entry hooks,
  scanner-trie registration, and parser-core behavior such as the
  `quotedAtomSymbols` hook used while parsing quoted templates.
- [src/syntax_parse.c](../src/syntax_parse.c) owns syntax declaration parsing,
  rollback-aware matching, helper-rule invocation, and carrier construction.
- [src/syntax_template.c](../src/syntax_template.c) owns template parsing and
  the durable syntax-template IR.
- [src/ast_prepare.c](../src/ast_prepare.c) resolves declaration-site literal
  references and prepares syntax carriers after namespace processing.
- [src/ast_lower.c](../src/ast_lower.c) instantiates syntax-use carriers,
  freshens introduced binders, preserves use-site captures, erases syntax
  carriers, and enforces syntax result kinds.
- [src/lambda_conversion.c](../src/lambda_conversion.c) now only contains
  defensive error reporting if a syntax carrier leaks past lowering.

What shipped:

- hygienic `quote` / `unquote` lowering through explicit AST carriers and a
  dedicated template IR
- helper-only recursive rules through `Syntax(rule(args))`
- expression-position initiating rules
- definition-position initiating rules for the supported single-definition
  `name = expr` shape
- parser-side direct substitution removal
- parser-side quote-wrapper sentinel removal in favor of explicit template
  metadata
- focused diagnostics for lowering mismatches and unresolved syntax
  declarations
- test migration and expansion for phase-1 hygiene cases

The current implementation still has two intentional phase-1 limits.

- Initiating rules are currently single-alternative even though helper-only
  rules support ordered alternatives.
- Definition-position syntax is intentionally limited to existing core
  definition forms rather than introducing new typed-definition surface syntax.

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

The remaining deferred items are now follow-on work rather than blockers for
phase-1 syntax.

- clause-oriented fragments for `switch`, `do`, or other non-Expr / non-Def
  surfaces
- multi-definition syntax results beyond the existing single-definition support
- syntax-object pattern matching for `rewrite`
- import or export of syntax rules across namespace boundaries
- hoisting later syntax declarations to the start of a `let` block so earlier
  ordinary definitions may use them
- general `splice` and repetition-oriented template features
- explicit hygiene escape hatches
- broadening initiating rules beyond the current single-alternative entry path

## Remaining Risks

The largest remaining risks are now incremental rather than architectural.

- Broadening the entry path from single-alternative initiating rules to full
  ordered alternatives without regressing existing error reporting.
- Deciding how imported or exported syntax rules should interact with current
  declaration-site hygiene semantics.
- Extending result kinds or template splicing without reintroducing parser-time
  expansion shortcuts.

## Next Follow-On Slice

The next coherent slice is documentation and surface cleanup for work beyond
phase 1.

1. document the implemented file boundaries and pipeline
2. describe the removal of parser-time substitution explicitly
3. record the deferred work list above as the starting point for any later
   phase-6-plus syntax expansion
