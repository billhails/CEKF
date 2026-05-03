# Syntax Hygiene Design

## Purpose

This document specifies stage 5 of the phase-1 syntax plan: hygienic template
representation and lowering.

It is intentionally narrower than a long-term syntax-object system. The goal is
to replace the current parser-resident substitution path with a design that:

- preserves definition-site resolution for literal template identifiers
- preserves use-site identity for captured fragments
- freshens template-introduced binders
- keeps `pratt_parser.c` from growing further by moving most new work into
  fresh source files
- lowers all syntax carriers before `lambda_conversion.c`

This document supersedes the vague stage-5 notes in
`SYNTAX_IMPLEMENTATION_PLAN.md`. It is written against the implementation that
already exists in `src/pratt_parser.c`.

## Current State

The parser already does four useful stage-4 things.

1. It parses syntax declarations into `PrattMacroSpec` and
   `PrattMacroAlternative`.
2. It supports helper-only rules and `Syntax(rule(args))` recursive calls.
3. It parses `quote { ... }` as a special template entry form.
4. It matches syntax uses with rollback-aware alternative selection.

The current stage-5 placeholder is the direct substitution path in
`src/pratt_parser.c`.

Relevant functions today:

- `parseSyntaxAlternative`
- `makeSyntaxQuotedTemplate`
- `unwrapSyntaxQuotedTemplate`
- `parseSyntaxHoleExpression`
- `expandSyntaxAlternative`
- `expandSyntaxExprWithBindings`
- the `substituteSyntax*` family

That code is good enough for early proving slices, but it is not a viable
hygiene design for phase 1.

## Why The Current Approach Is Not Sufficient

The current parser implementation stores templates as ordinary `AstExpression`
trees and lowers them by copying those trees and destructively substituting
captured bindings.

That loses information needed for hygiene.

It cannot reliably distinguish:

- literal identifiers written directly in the template
- identifiers introduced as new binders by the template
- references to those introduced binders
- `unquote` splices from the use site

It also places all of the following in `pratt_parser.c`:

- syntax template parsing
- syntax use matching
- binding lookup
- recursive AST substitution
- quote/unquote validity checks

That is already too much parser responsibility.

## Design Decision Summary

The recommended stage-5 design is:

1. Keep parser-time syntax declaration parsing and syntax-use matching.
2. Stop expanding syntax directly to ordinary AST in the parser.
3. Store syntax declarations and matched syntax uses as explicit AST carriers.
4. Parse `quote` templates into a provenance-carrying template IR, not plain
   `AstExpression`.
5. Run a new AST-side syntax preparation and lowering step after `nsAstProg()`
   and before `lamConvertProg()`.
6. Ensure `lambda_conversion.c` never sees syntax carriers or template IR.

The key point is that syntax remains parser-driven for recognition, but hygiene
becomes an AST-side lowering concern in new source files.

## Recommended Pipeline Position

The new syntax hygiene work should sit here:

```text
Parse
  -> nsAstProg
  -> syntaxPrepareAst
  -> syntaxLowerAst
  -> lamConvertProg
  -> tc_analyze
  -> ... existing later stages
```

This placement is deliberate.

Why not lower in the parser:

- The parser does not have a stable representation of definition-site binding
  identity.
- Parser-time substitution collapses literal, introduced, and unquoted names
  too early.

Why after `nsAstProg()`:

- namespace qualification should already have happened before syntax literals
  are resolved to their definition-site meaning
- syntax expansion should see the same namespace flattening rules as the rest
  of the AST

Why before `lamConvertProg()`:

- lambda conversion assumes ordinary AST only
- type checking should not know anything about syntax carriers
- later min-lambda alpha conversion is too late to reconstruct hygiene intent

## New Source Files

To keep `pratt_parser.c` from absorbing stage 5, the implementation should be
split into fresh files.

### `src/syntax_parse.h` and `src/syntax_parse.c`

Parser-adjacent syntax support that still runs during parsing.

Responsibilities:

- parse syntax declarations after the leading `syntax` keyword
- parse syntax alternatives
- parse `Syntax(...)` metadata in pattern holes
- perform rollback-aware syntax matching
- build syntax-use carrier nodes instead of expanded AST

Code expected to move out of `pratt_parser.c`:

- `parseOptionalSyntaxParameters`
- `parseSyntaxAlternative`
- `validateSyntaxAlternatives`
- `parseSyntaxHoleExpression` split into matcher-specific helpers
- `expandSyntaxAlternative`
- `trySyntaxAlternative`
- `expandSyntaxExprWithBindings`
- `expandSyntaxExpr`

What stays in `pratt_parser.c`:

- the entry hooks in `definition()` and `userSyntaxExpr()`
- record and trie registration
- generic parser utilities such as `expressionPrecedence`, `nest`, `symbol`,
  and scanner interaction

### `src/syntax_template.h` and `src/syntax_template.c`

Template parsing and template IR utilities.

Responsibilities:

- parse `quote { ... }`
- parse the bare-parameter shorthand template form
- recognize `unquote` in expression and binder positions
- produce a syntax-template IR with explicit provenance
- provide copy and traversal helpers for that IR

Code expected to move out of `pratt_parser.c`:

- `syntaxQuoteSymbol`
- `syntaxUnquoteSymbol`
- `isSyntaxQuoteToken`
- `makeSyntaxQuotedTemplate`
- `unwrapSyntaxQuotedTemplate`
- the temporary parser-rule override used while parsing quoted bodies

### `src/syntax_prepare.h` and `src/syntax_prepare.c`

AST-side preparation pass run after namespace processing.

Responsibilities:

- walk the AST in textual order
- maintain the current ordinary binding environment
- maintain the current syntax declaration environment
- resolve definition-site literal references inside syntax declarations
- assign stable temporary binding identities for later hygienic lowering

This is the pass that makes stage-5 hygiene concrete instead of name-based.

### `src/syntax_lower.h` and `src/syntax_lower.c`

AST-side hygienic lowering pass.

Responsibilities:

- instantiate syntax-use carriers
- freshen template-introduced binders
- rewrite template-introduced references to the freshened binders
- splice already-captured use-site fragments without changing their identity
- rewrite definition-site literal references to the resolved declaration-site
  bindings recorded by `syntaxPrepareAst`
- erase syntax declarations and syntax-use carriers from the AST

## Required AST Changes

Yes, stage 5 needs AST changes.

The parser cannot keep returning only ordinary `AstExpression` if lowering is
deferred until after parsing. There must be explicit carrier nodes that survive
until `syntaxLowerAst`.

### Minimum Required Carriers

The smallest practical AST extension is:

1. `AstDefinition_SyntaxDecl`
   Stores the syntax declaration in the AST instead of discarding it as
   `AstDefinition_Blank`.

2. `AstExpression_SyntaxUse`
   Stores a matched expression-position syntax use together with its captures
   and the selected rule alternative.

3. `AstDefinition_SyntaxUse`
   Stores a matched definition-position syntax use for the later stage-6 entry
   path. Adding it now avoids redesigning the carrier model later.

These are temporary nodes. They must not survive past `syntaxLowerAst`.

### Data Stored In `AstDefinition_SyntaxDecl`

Each syntax declaration node should contain:

- rule name
- entry kind: `Expr`, `Def`, or helper-only
- result kind: `Expr` or `Def`
- inherited parameter list
- ordered alternatives
- the parsed syntax template IR for each alternative
- a unique declaration id used by later preparation and lowering

The parser should still register the same declaration in its parser-local syntax
table for use-site parsing. The AST declaration node exists for later lowering,
not instead of parser registration.

### Data Stored In `AstExpression_SyntaxUse` And `AstDefinition_SyntaxUse`

Each use-site carrier should contain:

- a reference to the matched syntax declaration id
- the selected alternative index
- captured bindings produced by pattern matching
- inherited bindings passed into helper rules
- parser info for diagnostics

The use-site node should not contain already-expanded ordinary AST.

## Template IR Requirements

The template representation must preserve provenance that ordinary AST does not.

At minimum it needs these name categories.

### `TemplateLiteralRef`

An identifier written directly in the template and not introduced as a binder.

Semantics:

- resolves at the syntax declaration site
- must not be captured by use-site bindings

### `TemplateIntroducedBinder`

A binder introduced by the template itself.

Semantics:

- receives a fresh symbol during `syntaxLowerAst`
- is visible only within the template region that binds it

### `TemplateIntroducedRef`

A reference to a `TemplateIntroducedBinder`.

Semantics:

- rewrites to the same fresh symbol chosen for the matching introduced binder

### `TemplateUnquote`

An unquoted capture or inherited value.

Semantics:

- preserves use-site identity
- is inserted without declaration-site rebinding

The current direct substitution path collapses all four into ordinary AST names.
The new template IR must not.

## Binding Identity Strategy

Stage 5 needs more than fresh names for introduced binders. It also needs a way
to preserve definition-site references for literal identifiers.

Name equality is not enough.

Example problem:

```fn
let helper = 1
in {
    syntax useHelper: Expr ::= "useHelper" quote { helper };
    let helper = 2
    in useHelper
}
```

The expansion must refer to the outer `helper`, not the inner one.

That requires a stable notion of binding identity before `lambda_conversion.c`.

### Recommended Approach

Add an AST preparation pass that assigns temporary unique symbols to ordinary
binder sites and their references before syntax lowering.

This is an AST-local alpha-like preparation step, not a replacement for the
later min-lambda alpha conversion.

`syntaxPrepareAst` should:

- walk the AST in lexical order
- replace each ordinary binder symbol with a fresh internal `HashSymbol`
- rewrite corresponding ordinary references to that same fresh symbol
- record those resolved symbols as the meaning of `TemplateLiteralRef` inside
  syntax declarations
- descend into syntax-use captures in the use-site lexical environment so they
  preserve use-site identity
- avoid lowering syntax uses yet

This gives `syntaxLowerAst` a concrete symbol world in which:

- declaration-site literals already point at the correct bindings
- use-site captures already point at the correct bindings
- introduced binders can be freshly generated with `genSymDollar`

Without this preparation step, stage 5 would still be forced to guess by name.

## Namespace Interaction

`nsAstProg()` should continue to run before syntax lowering.

Required rule:

- namespace rewriting must descend into syntax declaration templates and syntax
  use captures using declaration-site semantics for declarations and use-site
  semantics for captures

This does not require `nsAstProg()` to expand syntax. It only requires it not
to treat syntax templates as ordinary use-site AST.

In practice that means `ast_ns.c` will need small new cases for the syntax
carrier nodes:

- visit `AstDefinition_SyntaxDecl` and rewrite literal namespace references in
  its template IR under the declaration-site context
- visit `AstExpression_SyntaxUse` and `AstDefinition_SyntaxUse` by rewriting the
  captured fragments under the current use-site context

## Parser-Time Behavior After This Change

After stage 5, parser-time matching should stop at carrier construction.

For expression entry:

1. Pratt dispatches on the registered syntax head.
2. Syntax matching selects an alternative and captures the fragments.
3. The parser returns `AstExpression_SyntaxUse`.

For helper calls:

1. The matcher resolves the helper rule as it does now.
2. The helper call returns a nested syntax-use carrier or a captured fragment,
   depending on the matched rule result.

For definition entry:

1. The definition parser dispatches to the syntax matcher.
2. The matcher returns `AstDefinition_SyntaxUse`.

The parser should no longer call a recursive `substituteSyntax*` tree walk.

## Lowering Algorithm

`syntaxLowerAst` should lower one syntax use as follows.

1. Look up the referenced syntax declaration and selected alternative.
2. Copy the alternative's template IR.
3. Allocate fresh symbols for every `TemplateIntroducedBinder` in that copied
   template.
4. Rewrite all `TemplateIntroducedRef` nodes to those fresh symbols.
5. Replace each `TemplateUnquote` with the already-prepared captured fragment.
6. Replace each `TemplateLiteralRef` with the declaration-site resolved symbol
   recorded by `syntaxPrepareAst`.
7. Convert the lowered template IR to ordinary AST.
8. Recursively lower any nested syntax-use carriers produced by helper syntax.

The result must be plain `AstExpression` or plain `AstDefinition` according to
the syntax rule result kind.

## Result-Kind Enforcement

The lowering pass must enforce the rule's declared result kind.

- `Expr` syntax must lower to exactly one `AstExpression`.
- `Def` syntax must lower to exactly one `AstDefinition`.

That check belongs in `syntaxLowerAst`, not in `lambda_conversion.c`.

## What Does Not Need To Change

The following subsystems should remain structurally unchanged.

- `lambda_conversion.c`
- `tc_analyze.c`
- the later lambda, min-lambda, and ANF passes

Their only contract change is that syntax carriers must be gone before they run.

## Implementation Order

The recommended implementation order is:

### Step 1: Move Parser-Side Syntax Helpers Out Of `pratt_parser.c`

Create `syntax_parse.c` and `syntax_template.c`. Move existing syntax parsing
and matching helpers there with minimal behavior change.

Success condition:

- no semantic change yet
- `pratt_parser.c` becomes a thin caller for syntax entry points

### Step 2: Replace Direct Substitution With Carrier Construction

Change expression-entry syntax matching to return `AstExpression_SyntaxUse`
instead of calling `substituteSyntaxExpression`.

Do the same for helper results.

Success condition:

- parser still recognizes existing expression syntax tests
- AST dump shows syntax-use carrier nodes instead of already-expanded ordinary
  expressions

### Step 3: Keep Syntax Declarations In The AST

Change `syntaxDefinition()` to emit `AstDefinition_SyntaxDecl` instead of
`AstDefinition_Blank`.

Success condition:

- parser registration still works
- AST retains syntax declarations for later passes

### Step 4: Add Namespace Traversal Support For Syntax Carriers

Teach `ast_ns.c` to visit syntax declarations and syntax-use captures without
lowering them.

Success condition:

- namespace processing completes with syntax carriers still present

### Step 5: Implement `syntaxPrepareAst`

Add the AST preparation pass that resolves declaration-site literals and gives
ordinary binders stable temporary unique symbols.

Success condition:

- a shadowing example proves that template literal refs resolve to the
  declaration site, not the use site

### Step 6: Implement `syntaxLowerAst`

Lower carriers to ordinary AST and erase syntax declarations from the final AST.

Success condition:

- `--dump-ast` after lowering shows no syntax carriers
- `lambda_conversion.c` runs unchanged

### Step 7: Add Definition-Position Entry On Top Of The Same Carriers

Once expression-position lowering is stable, definition-position initiation can
reuse the same declaration and lowering infrastructure.

## Validation Targets

The minimum targeted tests for this design are:

1. Introduced-binder freshness.

   `time` or `for` should not capture a use-site variable named like the
   generated binder.

2. Definition-site literal resolution.

   A literal helper name written in the template should resolve to the syntax
   declaration site under shadowing.

3. Use-site capture preservation.

   `unquote` of an identifier should continue to refer to the use-site binding
   under shadowing.

4. Helper recursion.

   `lco` / `where` / `filter` should expand through nested syntax-use carriers
   and still end as plain AST.

5. Namespace interaction.

   A syntax declared in one namespace and used after import should preserve the
   declaration-site meaning of literal template identifiers.

6. No syntax leakage past lowering.

   A focused assertion or debug dump should confirm that `lamConvertProg()` is
   never given `AstDefinition_SyntaxDecl`, `AstExpression_SyntaxUse`, or
   `AstDefinition_SyntaxUse`.

## Explicit Non-Goals

This design does not attempt to add:

- general-purpose syntax objects as runtime values
- escape hatches such as intentional unhygienic insertion
- multi-definition syntax results
- repetition or clause-sequence splicing
- changes to lambda or min-lambda hygiene

## Recommendation

Implement stage 5 as an AST-carried, post-namespace, pre-lambda lowering step.

The parser should keep doing what it is already good at:

- declaration parsing
- recursive syntax matching
- rollback
- entry dispatch

But the parser should stop doing hygienic expansion.

That separation is the smallest design that can satisfy the phase-1 hygiene
requirements without further inflating `src/pratt_parser.c`.
