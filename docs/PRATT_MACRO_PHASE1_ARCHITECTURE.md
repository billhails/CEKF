# Pratt Parser Macro Phase 1 Architecture

## Goal

Record a concrete parser and scanner analysis for phase-1 hygienic macros,
and sketch a minimal implementation shape that fits the current Pratt parser
architecture.

This note is intentionally narrower than a full macro RFC. It focuses on the
actual seams in `pratt_parser` and `pratt_scanner`, and on how a first macro
implementation could fit into those seams without trying to solve later work
such as `do` notation, grammar-level macros, or user-defined parser
callbacks.

## Summary

The current parser already has three properties that make phase-1 macros
plausible.

1. Token recognition is parser-owned, not scanner-owned. The scanner consults
   a trie that lives on the current parser chain.
2. Parsing behavior is already driven by runtime tables of parselets and
   associated metadata.
3. There is already a real parser-to-scanner feedback loop for user-defined
   operators and mixfix syntax.

The main conclusion is that phase-1 syntax-object parameter classes should be
modeled as a small fixed set of parser entry points, not as arbitrary new
parselets supplied by users.

That means classes such as `Expr`, `Name`, `Nest`, `String`, and perhaps
`Type` are realistic for phase 1, while things like `SwitchClauseList` or
`TypeBody` should wait until the simpler entry points are proven out.

## Findings

### 1. The Scanner Is Driven By Parser State

The scanner does not own a fixed grammar table. It looks up symbols in a trie
attached to the current parser and its parents.

Relevant structures:

- [src/pratt.yaml](../src/pratt.yaml#L201) defines `PrattFixityConfig`.
- [src/pratt.yaml](../src/pratt.yaml#L71) defines `PrattToken`.
- [src/pratt.yaml](../src/pratt.yaml#L96) defines `PrattParser`.

Relevant control points:

- [src/pratt_scanner.c](../src/pratt_scanner.c#L314) `_lookUpTrieSymbol()`
  walks the current parser and its parent chain.
- [src/pratt_scanner.c](../src/pratt_scanner.c#L940) `next()` calls trie
  lookup before falling back to normal identifier parsing.
- [src/pratt_scanner.c](../src/pratt_scanner.c#L1096) `insertPrattTrie()`
  inserts user-visible symbols into the trie.

Consequence:

- Macro head keywords and literal subkeywords can reuse the same scanner
  recognition path that operators use today.
- A separate scanner architecture is probably unnecessary for phase 1.

### 2. The Core Extension Seam Is The Pratt Parselet Table

The parser already dispatches through runtime parselet function pointers.

- [src/pratt_functions.h](../src/pratt_functions.h#L27) defines
  `PrattParselet`.
- [src/pratt_parser.c](../src/pratt_parser.c#L4344)
  `expressionPrecedence()` is the central Pratt loop.

Consequence:

- Phase-1 macros should plug into the same dispatch model rather than trying
  to create a separate parser path.
- A generic macro parselet is plausible.

### 3. Operator Registration Is The Closest Existing Model

Operators already flow through a declaration-time registration path that:

1. parses user syntax
2. stores metadata in parser-owned structures
3. updates the trie so the scanner recognizes the new tokens
4. reuses that metadata at expression parse time

Relevant functions:

- [src/pratt_parser.c](../src/pratt_parser.c#L1358) `addOperator()`
- [src/pratt_parser.c](../src/pratt_parser.c#L1594) `addMixfixOperator()`
- [src/pratt_parser.c](../src/pratt_parser.c#L2039) `definition()`

Consequence:

- Real macros should probably reuse this broad architecture.
- Phase 1 should try to look like “new declaration kind with parser metadata”
  rather than “special token rewriting trick”.

### 4. Parser Entry Points Are More Important Than Raw Parselets

Your idea that syntax-object parameter classes should correspond to existing
parser entry points is the right one. The refinement is that not all of those
entry points are plain Pratt parselets.

Examples:

- [src/pratt_parser.c](../src/pratt_parser.c#L1055) `childNest()`
- [src/pratt_parser.c](../src/pratt_parser.c#L1074) `nestBody()`
- [src/pratt_parser.c](../src/pratt_parser.c#L2579) `altFunction()`
- [src/pratt_parser.c](../src/pratt_parser.c#L2646) `altArgs()`
- [src/pratt_parser.c](../src/pratt_parser.c#L3321) `typeBody()`
- [src/pratt_parser.c](../src/pratt_parser.c#L2435) `typeType()`
- [src/pratt_parser.c](../src/pratt_parser.c#L3782) `switchFC()`

Consequence:

- Phase-1 syntax classes should map to existing parser helper functions.
- They should not be modeled as “user-supplied parselet callbacks”.

### 5. There Is Already A Scanner Feedback Loop Analogous To What Macros Need

User-defined operators are effectively passed back to the scanner from the
parser by trie insertion.

That is the existing analogue to “macro declarations influence later token
recognition”.

Consequence:

- If a macro introduces a head keyword or fixed literal separators, those can
  be registered in the trie just like operators and mixfix keywords.
- This is a recognition mechanism, not an expansion mechanism.

### 6. Namespace Export Or Import Is The Closest Existing Cross-File Transport

There is already infrastructure for exporting parser-visible operator metadata
from one scope and importing it into another.

Relevant functions:

- [src/pratt_parser.c](../src/pratt_parser.c#L756)
  `captureNameSpaceOperatorExports()`
- [src/pratt_parser.c](../src/pratt_parser.c#L785) `ensureTargetRecord()`
- [src/pratt_parser.c](../src/pratt_parser.c#L828) `mergeFixityImport()`
- [src/pratt_parser.c](../src/pratt_parser.c#L2150) `exportOp()`
- [src/pratt_parser.c](../src/pratt_parser.c#L2303) `importOp()`

Consequence:

- If phase-1 macros need import or export later, the operator transport path
  is the first place to look.
- This argues for storing macro parser metadata in something parallel to the
  existing fixity metadata, not in an ad hoc side structure hidden elsewhere.

## Risks And Constraints

### Parselet-Aligned Classes Are Not Free Today

The current mixfix machinery stores keyword patterns and precedence, but not a
per-hole syntax class. Every hole is parsed as an expression today.

That means phase 1 needs an explicit new dispatch layer for hole parsing.

### `Nest` Is A Real Parser Entry Point, But Not A Generic Statement Language

`childNest()` and `nestBody()` are good anchors for a `Nest` or `Block`
syntax class. But they bring along the existing semantics of brace nests,
including `let`, `namespace`, and statement parsing.

That is probably good for phase 1, but it means `Nest` is not a tiny
sub-expression class. It is a fairly opinionated grammar entry point.

### Clause-Oriented Forms Are Specialized

`switchFC()`, `altFunction()`, `altArgs()`, and `typeBody()` are all useful,
but each carries specific delimiter and AST assumptions.

That makes them poor candidates for the initial fixed syntax-class set.

### Scanner Trie Growth Changes Expression Termination Behavior

The Pratt loop allows tokens with no local record to terminate expressions.
That behavior is convenient for mixfix secondary keywords, but it also means
that adding more literal keywords through the trie may affect how expressions
stop.

That is manageable, but it is a real design pressure toward a conservative
phase-1 keyword set.

### Full Hygiene Still Does Not Belong In The Scanner

The scanner knows about token recognition and line numbers. It does not know
about syntax-object scope, marks, or hygienic rewriting.

So even if macro heads are scanner-recognized, actual macro expansion should
remain parser-side or post-parser-side.

## Practical Extraction Seams

If the goal is to keep new code out of `pratt_parser.c` where possible, some
parts are much easier to separate than others.

### Low-Risk Candidates For Separate Files

The operator or mixfix subsystem is the best template.

Candidate cluster:

- operator or mixfix pattern parsing
- macro declaration registration
- macro metadata creation
- generic syntax-class hole parsing
- generic macro parselet implementation

Practical file split:

- `src/pratt_macro_defs.c`
- `src/pratt_macro_parse.c`
- possibly `src/pratt_macro.h`

Those files can be called from the existing parser dispatcher without moving
the dispatcher itself.

### Risky Candidates To Extract

These are too central and shared to be good first extraction targets.

- [src/pratt_parser.c](../src/pratt_parser.c#L2039) `definition()`
- [src/pratt_parser.c](../src/pratt_parser.c#L4344)
  `expressionPrecedence()`
- [src/pratt_parser.c](../src/pratt_parser.c#L1022) `makeChildParser()`
- [src/pratt_parser.c](../src/pratt_parser.c#L1074) `nestBody()`

These should remain as thin callers into macro-specific helpers.

## Recommended Phase-1 Architecture

### Design Goal

Keep phase 1 expression-oriented, hygienic, and parser-integrated.

Do not attempt arbitrary grammar extension. Do not attempt token-stream
rewriting. Do not attempt user-defined parser callbacks.

### Proposed Fixed Syntax Classes

For phase 1, use a small fixed enum of parser-side syntax classes.

Recommended starting set:

- `Expr`
- `Name`
- `Nest`
- `String`
- `Type` (optional, but plausible)

Possible mapping onto current parser helpers:

- `Expr` -> `expressionPrecedence(parser, precedence)`
- `Name` -> a symbol or identifier parser path
- `Nest` -> `childNest(parser, terminal)` after consuming braces
- `String` -> existing string token or string helper path
- `Type` -> `typeType(parser)`

### Suggested New Metadata Shape

The current operator metadata is centered on `PrattFixityConfig`. A phase-1
macro system should probably use a parallel struct rather than trying to force
everything into fixity metadata.

In `pratt.yaml` terms, one plausible sketch is:

```yaml
structs:
  PrattMacroHole:
    meta:
      brief: A single typed hole in a macro pattern.
      description: >-
        Describes one macro parameter, including its parser-owned
        syntax class and the symbol it binds in the macro template.
    data:
      syntaxClass: PrattSyntaxClass
      name: HashSymbol

  PrattMacroSpec:
    meta:
      brief: Parser metadata for a phase-1 macro definition.
      description: >-
        Stores the head symbol, literal subkeywords, typed holes,
        and template expression needed for parser-time macro
        expansion in the current scope.
    data:
      headSymbol: HashSymbol
      literalKeywords: PrattStrings
      holes: PrattMacroHoles
      template: AstExpression=NULL
      export: bool=false
      importNsRef: int=-1
      importNsSymbol: HashSymbol=NULL

enums:
  PrattSyntaxClass:
    meta:
      brief: The fixed parser-side syntax classes for phase-1 macros.
    data:
      - EXPR
      - NAME
      - NEST
      - STRING
      - TYPE

arrays:
  PrattMacroHoles:
    meta:
      brief: An ordered list of macro holes.
      description: >-
        Holds the typed holes for a macro pattern in declaration
        order so parse-time expansion can bind arguments
        consistently.
    data:
      entries: PrattMacroHole
```

This is intentionally small.

Important point:

- The syntax class is parser-owned metadata.
- It is not a function pointer supplied by user code.

### Suggested Registration Flow

The registration path should mirror operators as closely as possible.

1. `definition()` recognizes a new `macro` declaration form.
2. A macro-definition helper parses the head pattern and parameter classes.
3. The helper builds a `PrattMacroSpec`.
4. The head keyword and literal subkeywords are inserted into the parser trie.
5. The current parser scope stores the macro metadata in a local table.
6. The expression parser later consults that metadata when the head token is
   encountered.

This is the cleanest analogue to the operator pipeline.

### Suggested Parse-Time Flow

At parse time, a generic macro parselet should:

1. recognize the macro head token
2. consult the stored macro spec
3. parse each hole according to its fixed syntax class
4. build a syntax-object environment
5. expand to normal AST using the stored template or expansion form

The key phase-1 choice is that hole parsing is dispatched by a fixed parser
helper table, not by user-supplied parser logic.

### Where The New Code Should Live

This seems practical:

- keep `definition()` and `expressionPrecedence()` in `pratt_parser.c`
- move macro-specific registration and parsing helpers into separate files
- keep scanner changes minimal, ideally limited to trie insertion reuse

Plausible split:

- `src/pratt_macro_defs.c`
  - parse macro declaration header
  - build `PrattMacroSpec`
  - register trie keywords
  - store local parser metadata
- `src/pratt_macro_parse.c`
  - generic macro parselet
  - syntax-class dispatch helpers
  - syntax-object construction glue

This would keep the existing parser entry points stable while giving macro
logic its own implementation area.

## What Phase 1 Should Not Try To Do

Phase 1 should not attempt:

- arbitrary parser callbacks in macro definitions
- grammar-definition macros
- scanner-level token rewriting as the expansion mechanism
- pattern-clause macro classes such as `SwitchClauseList`
- full typeclass-aware macro overload resolution

Those are all later requirements. The phase-1 design should remain compatible
with them, but should not be blocked by them.

## Recommended First Implementation Slice

If this were implemented incrementally, the smallest defensible slice would
be:

1. add a fixed `PrattSyntaxClass` enum
2. add a small parser-owned macro metadata struct
3. add a `macro` declaration path in `definition()`
4. add trie registration for macro head and literal keywords
5. add a generic macro parselet that supports only `Expr`, `Name`, and `Nest`
6. expand only into ordinary AST, not token streams

That would be enough to test whether the architecture works without taking on
the specialized clause grammars too early.

## Relationship To Other Notes

This note is intended to complement, not replace:

- [HYGIENIC_MACRO_DESIGN_KICKOFF.md](HYGIENIC_MACRO_DESIGN_KICKOFF.md)
- [agent/pratt-parser.md](agent/pratt-parser.md)
- [OPERATORS.md](OPERATORS.md)
- [SYNTAX.md](SYNTAX.md)

The kickoff note says what the macro system is for.
This note says where it fits in the current parser.
