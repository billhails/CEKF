# Syntax Extension

## List Comprehensions Example

```fn
// list comprehensions
syntax lco: Expr ::= "lco" "["
    exp: Expr
    "for" x: Name "in" xs: Expr
    filters: Syntax(where(x, xs)) // new "Syntax(id)" refers to other syntax
                                  // Syntax(id(arg,...)) allows passing data
"]" {
    quote { map(fn (unquote x) { unquote exp }, unquote filters) }
};

syntax where(x, xs) ::= "where" cond: Expr rest: Syntax(filter(x, xs)) {
                           quote { filter(fn (unquote x) { unquote cond }, unquote rest) }
                       }
             | { xs };

syntax filter(x, xs) ::= "," cond: Expr rest: Syntax(filter(x, xs)) {
                           quote { filter(fn (unquote x) { unquote cond }, unquote rest) }
                       }
             | { xs }
```

In phase 1, the explicit `: Expr` or `: Def` annotation is a good surface
marker for initiating rules.

- `syntax lco: Expr ::= ...` means an expression-initiating rule.
- `syntax annotate: Def ::= ...` means a definition-initiating rule.
- `syntax where(...) ::= ...` means a helper-only rule.

This avoids introducing another keyword just to mark triggers. Internally the
implementation still needs both a result kind and an entry kind, but the first
surface syntax can reuse the optional annotation to mark the common initiating
cases.

Current phase-1 implementation note:

- Helper-only rules own their full patterns and may use ordered alternatives.
- Initiating rules still register a dedicated head token and are currently
  limited to one alternative each.

## Implementation Sketch

The existing Pratt parser is still a good fit for expression parsing, precedence,
and operator dispatch, but richer syntax extensions should not be modelled as
ordinary user operators.

The clean split is:

1. Pratt remains responsible for parsing expressions and for deciding when an
  expression-oriented syntax rule may begin.
2. The existing definition parser remains responsible for deciding when a
  definition-oriented syntax rule may begin.
3. A separate syntax-rule engine handles ordered components, alternatives,
   recursive helper rules, inherited arguments, and template expansion.

In other words, an initiating syntax declaration would be entered from either
Pratt or the definition parser, but most of its body would not be parsed by the
ordinary prefix or definition routines directly.

## Parser Boundary

Each syntax declaration should compile to a SyntaxSpec rather than directly to a
PrattRecord.

- Expression-initiating syntax rules register their head token in the scanner
  trie and add a prefix parselet that delegates to the syntax-rule engine.
- Definition-initiating syntax rules register their head token in the scanner
  trie and add a definition-dispatch entry that delegates to the same syntax
  engine from the existing definition parser.
- Non-initiating syntax rules are stored in a syntax table only. They are not
  reachable from the top-level Pratt expression loop and can only be called from
  another syntax rule through Syntax(...).
- Helper-only alternatives own their full patterns. They do not share an
  implicit entry head token with each other.
- Adding a token to the trie and allowing it to start an expression are distinct
  decisions. Secondary keywords still need to be recognized lexically even when
  they are not valid entry points by themselves.

That split matters for backtracking. A helper rule such as `chooseValue ::= "with"
"skip" ... | "with" replacement: Expr ...` must be able to roll back before the
first `"with"`, whereas an initiating rule such as `syntax time: Expr ::= "time"
...` has already committed to its registered head token before rule matching
starts.

This keeps Pratt in charge of expression entry and the existing definition
parser in charge of definition entry, while allowing a syntax rule to run its
own ordered matching logic once selected.

## SyntaxSpec Shape

The syntax-rule engine needs something closer to a small grammar than to a Pratt
operator record.

Each rule should contain:

- A rule name.
- Zero or more inherited parameters.
- One or more alternatives for helper-only rules.
- A resolved result kind such as `Expr` or `Def`.
- An entry kind: expression, definition, or helper-only.
- A template to build when an alternative matches.

The result kind remains required internal metadata. In phase 1, the source
surface can reuse the optional `: Expr` or `: Def` annotation as the marker for
initiating rules rather than as a general-purpose annotation on every rule.

- `syntax name: Expr ::= ...` means an expression-initiating rule.
- `syntax name: Def ::= ...` means a definition-initiating rule.
- `syntax name ::= ...` means a helper-only rule, defaulting to an `Expr`
  result unless a later extension adds explicit helper result annotations.
- Result kind and entry kind remain separate properties internally even if the
  initial surface syntax couples them in the common cases.
- The current phase-1 implementation keeps initiating rules single-alternative
  until the entry path is generalized.

Each alternative should be an ordered list of components rather than a DAG.
That is enough for the examples here and makes backtracking rules simpler.

For helper-only rules, alternatives that consume input are tried before empty
alternatives. Within those two groups, declaration order is preserved. That
makes an empty recursive base case readable in either first or last position
without changing the match result.

The current implementation also rejects helper alternatives that are obviously
unreachable at declaration time. Exact duplicates are rejected, and a later
consuming alternative is rejected if an earlier consuming alternative is a
strict pattern prefix of it modulo binder renaming.

Useful component kinds are:

- Terminal: a fixed quoted token such as "for" or ",".
- Expr: parse one expression through the existing Pratt expression parser.
- Name: parse one identifier or binder.
- Nest: parse one nest through the existing nest parser.
- String: parse a string literal.
- Type: parse a type.
- SyntaxCall: invoke another syntax rule, optionally with inherited arguments.
- Empty: match nothing, useful for explicit base cases.

This is enough to express the lco, where, and filter example without forcing
the problem back into prefix or infix operator machinery.

## Parse Environment

Each syntax invocation needs an environment separate from the ordinary Pratt
parser state.

That environment should hold:

- Inherited arguments passed in by the caller.
- Captured bindings produced by components in the current alternative.
- The current parser and lexer position.
- The active syntax table for the current scope.

Bindings should use let-star semantics, but with explicit alternative-local
rollback semantics:

- Each alternative starts with a fresh local environment seeded by the rule's
  inherited arguments.
- Components match left to right.
- A captured name becomes visible only after the component that binds it has
  matched successfully.
- Attempting to bind the same name twice within one alternative is an error.
- If a later component fails, the engine discards every binding created while
  attempting that alternative and restores the parser position to the
  alternative's checkpoint.
- A SyntaxCall should run in a child environment seeded from its explicit
  arguments. Its local captures should not mutate the caller's environment.

This is close to let-star on the successful path, but more precise for an
implementation that needs backtracking and alternative selection.

For the running example:

- lco binds exp, x, xs, and filters.
- where receives x and xs as inherited arguments.
- filter receives x and xs as inherited arguments.
- filter additionally binds cond and rest during recursive matching.

That makes Syntax(where(x, xs)) a recursive rule invocation with explicit
environment passing, not a plain Pratt parselet call.

## Declaration-Level Binding

The let-star-with-rollback rules above apply only to bindings captured within a
single syntax rule invocation. They are not the binding rule for syntax
declarations themselves.

Syntax declarations need letrec-like binding semantics.

- A syntax rule should be able to refer to itself.
- A syntax rule should be able to refer to peer syntax rules in the same
  binding group.
- In phase 1, a syntax rule should only become available for actual expansion
  after its declaration has been fully parsed and installed.
- Nested scopes should still shadow outer syntax rules lexically.

This is the declaration-level analogue of ordinary recursive binding, and it is
separate from the left-to-right capture rules that apply inside one matched
alternative.

For the running example, this means lco, where, and filter should be treated as
part of one recursive syntax binding group within the surrounding let scope,
even though the components captured inside each rule still follow let-star
semantics.

This gives a simple phase-1 rule:

- Syntax declarations share the lexical scope of the surrounding let or nest.
- Syntax declarations in that scope may be mutually recursive as declarations.
- Earlier ordinary definitions in the same scope do not get to expand later
  syntax declarations yet.

That keeps the initial implementation textual in terms of expansion
availability, while still allowing recursive relationships among syntax
declarations themselves.

### Does This Require A Parser Pre-Pass?

Hopefully not a whole-file or whole-parser pre-pass.

The semantic requirement is only that recursive syntax names be available as a
binding group. That can likely be achieved with a local two-stage strategy:

1. Reserve syntax rule names or placeholders in the current scope.
2. Parse and attach their alternatives, templates, and metadata.
3. Resolve SyntaxCall targets against that local syntax table.

That is closer to letrec elaboration than to a separate parser pre-pass.

One caveat does matter: the current parser is still definition-by-definition.
For phase 1, that is acceptable because syntax expansion availability remains
textual even though syntax declaration binding is recursive.

A later enhancement could hoist syntax declarations to the start of the
surrounding let block for parser purposes, so that earlier ordinary definitions
could use later syntax declarations. That should be treated as a compatible
nice-to-have rather than a requirement for the initial design.

## Interaction With Existing Pratt Routines

Most component kinds should reuse existing parser routines.

- Expr delegates to the ordinary expression parser.
- Name delegates to identifier parsing.
- Nest delegates to the nest parser.
- String and Type delegate to their existing parsers.
- Definition-oriented entry rules should reuse the existing definition parser's
  dispatch points rather than trying to pass definitions through the expression
  parser.

What is new is the control layer around them:

- Syntax matching needs non-committing probes so one alternative can fail
  without poisoning the whole parse.
- The engine needs a way to distinguish "this component did not match" from
  "this component matched but the nested parser raised a real syntax error".
- Entering a syntax rule should therefore use checkpoint and rollback support
  around token consumption.

That is the main reason this should be a syntax engine sitting beside Pratt and
the existing definition parser, not a direct extension of the current
PrattRecord model.

## Template Expansion

The terminal body should be treated as a template, not as an ordinary AST nest
with ad hoc substitution.

A practical model is:

1. Parse quote { ... } into a template AST.
2. Represent unquote name as a typed splice node.
3. During expansion, resolve each splice from either the captured bindings or
   the inherited arguments.
4. Lower the expanded template to the ordinary AST used by later compiler
  stages, according to the rule's declared result kind.

The splice nodes need syntax-category awareness.

- unquote x in binder position should splice a Name.
- unquote exp should splice an Expr.
- unquote filters should splice whatever expression fragment the recursive rule
  returned.

The template itself also needs a declared result kind.

- An `Expr` rule should lower to an AstExpression.
- A `Def` rule should lower to an AstDefinition.

That keeps the first cut simple while still allowing syntax extensions to
participate in let bodies, namespace bodies, and other definition contexts.

This is also where hygiene will eventually need to live. For now it is enough
to say that template expansion must preserve parser info and distinguish between
introduced names and spliced names.

## Parse Flow For The Example

The lco example would run roughly as follows:

1. Pratt sees the initiating token lco and dispatches to the syntax-rule entry
   parselet.
2. The syntax engine selects the lco rule and begins matching its single
   alternative.
3. Expr parses the body expression.
4. The fixed terminals for, in, and ] are matched directly.
5. Name and Expr capture x and xs.
6. Syntax(where(x, xs)) invokes the where rule with those inherited arguments.
7. where either matches Empty and returns xs, or matches where cond ... and
   delegates to filter.
8. filter either matches Empty and returns xs, or consumes a comma, captures a
   condition, and recurses.
9. Once the recursive calls return, the corresponding templates are expanded
   outward to produce the final expression AST.

This gives the desired grammar-like structure while still reusing the existing
expression parser where it matters.

## Scope And Storage

Syntax rules should follow the same broad scoping model as operators.

- A parser scope owns a syntax table alongside its operator table.
- Child scopes inherit visibility from parent scopes.
- New local syntax rules shadow outer rules of the same name.
- Export and import can be added later once the rule representation is stable.

This suggests adding syntax storage to the Pratt parser state, but not forcing
syntax rules into PrattRecord.

## Deferred Issues

The sketch above intentionally leaves several issues open.

- Hygiene is still unresolved.
- Alternative selection needs a precise policy for error reporting versus
  backtracking.
- Template expansion needs a concrete representation for quoted binders and
  quoted nests.
- The first cut should support Expr and Def result kinds. Broader
  fragment kinds such as Definitions, Statements, or type-level fragments can
  remain deferred.

Those are substantial design questions, but they are local to the syntax-rule
engine. They do not undermine the core approach of entering through Pratt and
delegating the rest to a separate rule matcher.

## Minimum Concrete First Cut

Without changing code yet, the smallest concrete design that seems worth
implementing is:

- Support two initiating entry kinds: `Expr` and `Def`.
- Support two result kinds: Expr and Def.
- Keep helper-only rules non-initiating.
- Reuse existing parser routines wherever a component can be expressed in terms
  of Expr, Name, Nest, String, or Type.

That gives enough surface area to support expression sugar such as list
comprehensions and also definition sugar inside let and namespace bodies,
without committing yet to broader fragment kinds such as statement lists or
type-level rewrites.

### Draft Schema

At the documentation level, the minimum useful structures look like this:

- SyntaxTable
  A scope-local table keyed by rule name.
- SyntaxRule
  Holds the rule name, entry kind, result kind, inherited parameter list,
  alternatives, and template metadata.
- SyntaxAlternative
  Holds one ordered sequence of components plus the template to expand when the
  alternative succeeds.
- SyntaxComponent
  A tagged union with variants such as Terminal, Expr, Name, Nest, String,
  Type, SyntaxCall, and Empty.
- SyntaxCall
  Names a target rule and carries the explicit arguments passed into that child
  invocation.
- SyntaxTemplate
  A quoted template plus typed splice nodes for unquote sites.
- SyntaxEnv
  Holds inherited arguments, locally captured bindings, and the checkpoints
  needed for rollback.

The first cut does not need every possible abstraction up front. In particular,
it does not need generic fragment polymorphism if the rule itself already
declares whether it produces an Expr or a Def.

### Parser Storage

The parser state likely needs one new scope-aware table beside the existing
operator and macro tables:

- PrattParser gains a SyntaxTable.
- Child parser scopes inherit syntax visibility the same way they inherit other
  scoped parser state.
- Initiating rules additionally arrange for their head token to be recognized by
  the scanner trie.

That keeps syntax lookup a parser concern without forcing syntax rules into the
existing PrattRecord shape.

### Minimum Dispatch Hooks

The first cut seems to need only two direct entry hooks.

1. Expression hook.
   The ordinary Pratt expression path sees a registered initiating token and
   dispatches to a syntax-rule entry parselet. That parselet invokes the syntax
   engine and expects an Expr result.
2. Definition hook.
   The ordinary definition parser sees a registered initiating token and
   dispatches to a syntax-rule definition entry. That entry invokes the same
   syntax engine and expects a Definition result.

Helper-only rules have no direct dispatch hook. They are reachable only through
SyntaxCall from another rule.

This is important because it avoids a false unification. Expression syntax and
definition syntax can share the same rule engine without pretending they are
entered through the same parser routine.

### Matching Contract

For a first implementation, each entry hook can obey a simple contract:

- Resolve the named initiating rule.
- Create a fresh SyntaxEnv seeded with inherited arguments, if any.
- Try alternatives in declaration order.
- On success, expand the template and verify that its result kind matches the
  entry hook's expected kind.
- On alternative failure, roll back both token position and local bindings.
- On total failure, raise a normal parser error at the initiating token.

This is conservative, but it gives a well-defined behavior model before any
later optimization or ambiguity handling.

### What This Deliberately Excludes

The first cut should avoid taking on too much.

- No generic fragment result kinds beyond Expr and Def.
- No export or import design yet.
- No fully general hygiene system or hygiene escape-hatch design yet.
- No attempt to merge syntax rules into the existing operator representation.
- No attempt to make every parser surface syntax-extensible at once.

That should keep the implementation small enough to validate the overall model
before broadening the feature.

## Hygiene Position

The existing alpha-conversion pass is too late in the pipeline to serve as the
primary hygiene mechanism for syntax expansion.

By the time a generic later alpha-conversion pass runs, expansion has already
flattened syntax templates into ordinary AST. At that point the compiler can no
longer reliably reconstruct the distinction between:

- names introduced by the syntax template itself
- names coming from unquote or splice
- literal free names written in the template that should resolve at the
  definition site

A later alpha-conversion pass is still useful as a normalization and safety
step, because it can freshen binders in the lowered representation and prevent
accidental capture there. But it cannot decide the core hygiene policy after
the fact.

For a first implementation, hygiene should therefore be handled by the syntax
expander itself.

The minimum useful policy is:

- Template-introduced binders are freshened during expansion.
- References introduced by the same template are rewritten to those freshened
  binders.
- Literal identifiers written directly in the template resolve at the
  definition site.
- Unquoted or spliced syntax preserves use-site identity.
- A later generic alpha-conversion pass may still run, but only as cleanup,
  not as the mechanism that decides hygiene.

This does not require a fully general syntax-rules-style mark system on day
one, but it does require the template representation to preserve identifier
provenance long enough for the expander to make these distinctions.

That makes a few early design constraints important:

- The template AST must distinguish literal identifiers from unquoted
  identifiers.
- Binding positions must be explicit so the expander knows which names to
  freshen.
- Expansion should not erase provenance too early by lowering immediately to an
  unannotated ordinary AST.

If those constraints are preserved, later work can refine hygiene without
needing to redesign the whole expansion model.

## Stress Test Against Motivating Examples

This section compares the current plan against the examples in
[HYGIENIC_MACRO_DESIGN_KICKOFF.md](HYGIENIC_MACRO_DESIGN_KICKOFF.md).
The verdicts here are about the phase-1 design described in this document, not
the eventual long-term goal.

### Fits The Phase-1 Plan

#### Time

The `time` example fits the current plan cleanly.

- It only needs an Expr parameter.
- It expands to an Expr result.
- It needs hygienic generated binders.
- It needs definition-site resolution for helpers such as `now` and `print`.

That is already aligned with the current SyntaxSpec shape and hygiene position.

#### Trace

`trace` is only partly covered.

If `trace` just wraps an expression with extra evaluation-time behavior, then it
fits the same way as `time`. If it also wants to recover a user-facing string
representation of the captured syntax object, then the current plan does not yet
say how syntax objects become strings. That reflection surface should therefore
be treated as later than the core phase-1 design.

#### Unless

`unless` fits the current plan cleanly.

- Fixed terminals are enough to represent the header and trailing `else` form.
- Expr and Nest parameters are already part of the phase-1 component set.
- The expansion is a straightforward quoted Expr template.

This is still a strong MVP candidate.

#### For Loop As Expression Sugar

The `for` example also fits the phase-1 plan.

- It needs Name, Expr, and Nest parameters.
- It needs a hygienically introduced binder such as `loop`.
- It needs unquote to work correctly in binder position.

Those requirements are already visible in the current template and hygiene
sections, so this remains a good stress test rather than a blocker.

#### List Comprehensions

List comprehensions are the main example this document is already designed
around, and they remain in scope.

- Recursive helper rules cover the `where` and comma-separated predicate tail.
- One grouped syntax declaration can cover both the empty and non-empty `where`
  cases.
- The expansion relies on definition-site resolution for helper operators while
  still using captured names in generated lambdas.

This is a demanding first-wave example, but it still fits the current model.

### Early Next-Wave Targets

#### Switch

`switch` is close, but not quite phase 1.

The current plan does not yet have a first-class notion of clause-list
parameters or clause splicing. That means it cannot yet represent the body of a
`switch` as a structured macro parameter the way the hard-coded parser support
does today.

Even so, `switch` remains the clearest early migration target after the MVP,
because its expansion shape is already known.

#### Definition-Position Boilerplate Generators

`define_ast` partially fits the plan and partially exceeds it.

The current design already allows syntax rules in definition position, which is
an important step. But the example as written wants to expand to multiple
definitions rather than one AstDefinition. That exceeds the current phase-1
result kinds.

So the blocker here is not definition position itself, but the lack of a
Definitions result kind or definition-list splicing.

### Beyond The Phase-1 Plan

#### Rewrite-Rule Helper

`rewrite` is beyond the current phase-1 design.

It needs syntax-pattern matching over structured syntax objects, repetition, and
structured expansion into a matcher or decision tree. The current plan is about
parser-side recognition and template expansion, not pattern matching on already
captured syntax trees.

#### Grammar Or DSL Macros

Grammar-oriented macros are also beyond the current phase-1 design.

They want parser-facing grammar handling, richer repetition and binding, and
possibly a different expansion model entirely. That is a reasonable long-term
goal, but it is not a small extension of the current first cut.

#### Do Notation

`do` notation is also beyond the current phase-1 design.

The hard part is not the final desugaring shape. The hard part is that the body
is a semicolon-separated sequence of structured clauses, not just a single Expr
or Nest parameter with no internal macro-specific structure.

To support `do` well, the syntax system will likely need at least one of:

- clause-list parameters
- structured inspection of captured Nest syntax objects
- richer fragment kinds than the current Expr and Def result model

That keeps `do` as a compatible future target, but not a first-wave one.

### Overall Verdict

The current plan comfortably covers the strongest phase-1 examples:

- `time`
- `unless`
- `for`
- list comprehensions

It starts to break down at the first examples that need one of three things the
current design does not yet include:

- clause-oriented fragments such as `switch` and `do`
- syntax-pattern matching over structured syntax objects such as `rewrite`
- multi-definition results such as `define_ast`

That suggests the current plan is strong enough for an MVP, and also makes the
most natural second-wave extensions fairly clear.

## Ordered Backlog After MVP

The stress test suggests a fairly clear order for what should come next once the
phase-1 design is working.

### 1. Clause-Oriented Fragments

The first extension should likely be some way to represent structured clause
lists or other parser-owned sequence fragments.

This is the smallest step that would unlock examples such as:

- `switch`
- a conservative first version of `do`

It is also a natural extension of the current design, because the main missing
piece is not hygiene or recursive binding. It is the inability to capture and
re-expand bodies whose internal structure matters to the syntax rule.

### 2. Multi-Definition Results

After clause-oriented fragments, the next likely extension is allowing a syntax
rule to expand to more than one definition.

This is what examples such as `define_ast` really want. Definition-position
entry by itself is not enough if the expansion target is a whole generated block
of typedefs, constructors, visitors, or printers.

The likely shape is a new result kind such as Definitions or some equivalent
definition-list container.

### 3. Syntax-Object Pattern Matching

After that, the next major step is pattern matching over captured syntax
objects.

That is what examples such as `rewrite` need, and it is qualitatively a bigger
step than the first two backlog items. It goes beyond parser-side recognition
and template expansion into structured deconstruction of captured syntax.

This likely wants its own design pass rather than being smuggled into the first
implementation as a small extension.

### Implication For Scope

This ordering suggests that the current MVP boundary is in a good place.

- Phase 1 should focus on expression and definition syntax with hygienic
  template expansion.
- The first second-wave target should be clause-oriented bodies.
- Multi-definition generation and syntax-pattern matching can remain clearly
  later.

That keeps the initial implementation small enough to finish while still making
the next pressure points explicit.
