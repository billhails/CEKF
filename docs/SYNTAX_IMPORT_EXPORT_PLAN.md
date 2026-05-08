# Syntax Import And Export Plan

This document is the working plan for namespace import and export of macros,
with `syntax` reserved for helper-only rules in the current surface syntax.

It is intentionally narrower than the broader syntax design documents. The goal
here is to decide how imported syntax should cross a namespace boundary,
materialize in the importing module, and remain visible to later hygienic
lowering.

See also:

- [SYNTAX.md](SYNTAX.md)
- [SYNTAX_IMPLEMENTATION_PLAN.md](SYNTAX_IMPLEMENTATION_PLAN.md)
- [Import.md](Import.md)

## Scope

This plan is for the follow-on step after phase-1 local syntax.

Initial scope:

- export initiating syntax rules from a namespace
- import initiating syntax rules from a linked namespace
- bring helper-only rules along automatically as private support data
- preserve hygienic lowering across the namespace boundary

Initial non-goals:

- direct export of helper-only rules
- helper-only imports written explicitly in source
- renaming imported syntax on arrival
- re-export of imported syntax
- manifest-style export curation beyond named and bulk forms

## Hard Constraints

Two architectural constraints already look fixed.

### Imported Syntax Cannot Be Parser-Only

Imported operators can live in parser-side namespace cache because later stages
do not need durable operator declarations.

Imported syntax is different:

- parser-time syntax use produces `AstExprSyntaxUse` or `AstDefSyntaxUse`
  carriers
- those carriers refer to a durable declaration by `declarationId`
- hygienic lowering resolves that id by walking visible `AstSyntaxDecl`
  definitions in lexical AST scope

So a parser-only copy of imported syntax metadata is not enough. Parsing might
succeed, but `ast_lower.c` would not be able to resolve the imported rule.

Design rule:

- importing syntax must materialize durable local `AstSyntaxDecl` definitions,
  or an equivalent AST form elaborated into them before lowering

### Public Surface Should Stay Narrow

The explicit import/export surface should expose only initiating rules.

- users export `macro` entry points, not helper machinery
- helper-only rules come along as the transitive `Syntax(...)` dependency
  closure of an exported initiating rule
- helper rules remain an implementation detail of the imported entry rule

This keeps the user model close to operator import/export while matching how
syntax is actually structured.

## Recommended Surface Forms

Reuse the existing namespace surface rather than introducing a second module
system.

Recommended exports:

- prefix an individual initiating declaration with `export`, as in
  `export macro unless: Expr unlessBody;`
- `export macros;`

Recommended imports:

- `import ns macro name;`
- `import ns macros;`

Initial validation rules:

- `name` must resolve to an initiating rule, not a helper-only rule
- individual export applies only to `macro` declarations, not helper `syntax`
  declarations
- `export macros;` exports all locally-defined initiating rules in the
  current parser scope
- `import ns macros;` imports all exported initiating rules from `ns` and the
  helper closure needed by each of them
- exporting an imported rule should initially be rejected

## Representation Strategy

The operator path already provides the right high-level shape:

1. Parse a linked namespace once.
2. Capture exported metadata in a cache keyed by `nsRef`.
3. Import from that cached snapshot into another parser scope.

Syntax import should reuse that shape, but the snapshot must carry both
parser-time and durable AST forms.

Minimum useful namespace syntax snapshot:

- exported initiating rule names keyed by rule name
- a durable cloned `AstSyntaxDecl` for each exported initiating rule
- a durable cloned `AstSyntaxDecl` for each helper in the transitive closure
- parser-side `PrattMacroSpec` data for those same rules
- a mapping from original declaration ids to cloned declaration ids

That declaration-id map matters because imported syntax uses in the importing
module must point at local declaration ids, not ids from the exporting parse.

## Computing The Helper Closure

Closure should be computed structurally from rule patterns.

For each exported initiating rule:

1. Inspect every pattern item in every alternative.
2. Record every referenced target in a `Syntax(...)` hole.
3. Recursively visit the referenced rule.
4. Stop when the reachable set is closed.

Constraints:

- the exported item is a `macro` declaration, but the closure walk starts from
  the helper rule named by that macro
- every rule reached during the closure walk should be a helper-only `syntax`
  declaration reached through `Syntax(...)` references
- duplicate visits should be ignored
- broken helper references, or references that resolve to a `macro` rather than
  a helper `syntax` rule, remain declaration-time errors in the source
  namespace

This should be a declaration-graph walk, not template-body analysis.

Note:

- this helper-closure walk is only about `Syntax(...)` references in rule
  patterns
- a template body may also contain declaration-site literal references to
  other local macros
- if exported macros are allowed to invoke non-exported local macros from
  `quote { ... }`, import will need a second dependency analysis over template
  IR in addition to helper-closure analysis

## Recommended Import Semantics

For `import ns macro name;`, the importing parser should:

1. Load the namespace snapshot for `ns`.
2. Locate the exported initiating rule `name`.
3. Copy that rule and its helper closure into the current local syntax scope.
4. Assign fresh local `declarationId` values to every copied declaration.
5. Rewrite copied `AstSyntaxDecl` and `PrattMacroSpec` data so internal helper
   references and declaration ids point at the fresh local copies.
6. Register the imported initiating head token in the local parser tables.
7. Materialize the copied declarations into the importing module's
   AST-visible definition stream.

The last step is the critical difference from operator import. Imported syntax
must survive parsing as ordinary local syntax declarations.

## AST Materialization Strategy

The first implementation will use parse-time elaboration.

### Option A: Elaborate During Parsing

- parse `import ns macro ...` or `import ns macros;` as a definition
- immediately elaborate it into one or more ordinary local `AstSyntaxDecl`
  definitions

Advantages:

- matches the current phase-1 architecture
- keeps `ast_lower.c` working over ordinary definition lists
- treats imported syntax like local lexical declarations immediately

Decision:

- option A is the chosen strategy for the first implementation
- imported `import ns macro ...` and `import ns macros;` forms should
  elaborate immediately into ordinary local `AstSyntaxDecl` definitions

### Option B: Add A Durable Syntax-Import AST Node

- keep a dedicated import node in the AST
- elaborate that node into local `AstSyntaxDecl` definitions during
  `prepareAst`

Advantages:

- preserves source shape more explicitly

Costs:

- adds a new durable AST form
- pushes the materialization step later for little semantic gain in the first
  cut

Status:

- option B is deferred unless later implementation pressure makes parse-time
  elaboration unworkable

Additional note:

- if imported macros are allowed to depend on non-exported local macros named
  literally inside `quote { ... }`, option A remains viable but needs that
  template-IR dependency information available during import elaboration
- that makes option B somewhat cleaner conceptually, because `prepareAst`
  already participates in declaration-site template resolution
- this weakens the case for A slightly, but does not by itself require B

## Name And Conflict Rules

The first implementation should keep conflicts conservative.

- importing syntax may not silently replace a local rule of the same name
- importing syntax may not silently replace an already-imported rule of the
  same name from another namespace
- importing an initiating head token that is already registered for another
  initiating rule in the same scope is a parser error
- imported helper names also participate in conflict checks because they become
  ordinary local support declarations after import

This is stricter than an eventual shadowing design, but easier to reason about
while the feature is new.

## Pipeline Expectations

Once imported syntax has been materialized into ordinary local
`AstSyntaxDecl` definitions, downstream stages should not need a new model.

- `prepareAst` still resolves declaration-site literal names inside the copied
  declarations in the importing module's lexical context
- `ast_lower.c` still discovers visible syntax declarations by ordinary lexical
  traversal
- syntax uses produced after import point at imported local declaration ids

Desired semantics:

- the imported entry rule brings its helper machinery automatically
- the imported rule behaves like syntax declared in the importing scope
- hygiene still uses the imported declaration as the relevant definition site

## Suggested Delivery Order

1. Add export markers and namespace snapshot capture for local initiating
   syntax only.
2. Add helper-closure computation and tests for exported initiating rules.
3. Add import of one named initiating syntax rule from a namespace.
4. Materialize imported rules as local `AstSyntaxDecl` definitions with fresh
   declaration ids.
5. Add bulk forms `export macros;` and `import ns macros;`.
6. Add conflict handling and diagnostic polish.

## Concrete Implementation Checklist

The safest first slice is still the narrow one:

- support `export macro ...` on local initiating rules only
- support `import ns macro name;` only
- elaborate imports immediately into ordinary local `AstSyntaxDecl`
  definitions
- carry helper closure only through `Syntax(...)` references
- reject template-body references to non-exported local macros for now

Likely implementation surfaces:

### Parser Snapshot And Import Path

- [src/pratt.yaml](../src/pratt.yaml)
  - extend `PrattExportedOps` so the namespace cache can carry exported macro
    data in addition to exported operators
  - reuse or extend `PrattMacroSpec` export/import metadata rather than adding
    a new AST import form
- [src/pratt_parser.c](../src/pratt_parser.c)
  - extend `parseLink()` so namespace parsing captures syntax exports alongside
    operator exports
  - split or generalize `captureNameSpaceOperatorExports()` into a syntax-aware
    namespace snapshot helper
  - add a helper-closure walk that starts from a macro's helper rule and
    gathers reachable helper `syntax` declarations
  - add cloning/remapping helpers for imported `PrattMacroSpec` and
    `AstSyntaxDecl` data with fresh local `declarationId` values
  - add an import path parallel to `importOp()` for `import ns macro name;`
  - register imported initiating heads in the local parser tables during that
    elaboration step

### Export Marking And Validation

- [src/pratt_parser.c](../src/pratt_parser.c)
  - update macro declaration parsing so `export macro ...` marks the local
    initiating rule for namespace export
  - reject export on helper-only `syntax` declarations
  - reject export of non-local imported macros in the first cut
  - keep the existing macro/helper fixup flow intact:
    `validateMacroHelperTarget()`, `resolveMacroFixup()`,
    `materializeHelperSyntaxDecl()`, and `finalizePendingMacroFixups()` are the
    main places where imported clones must still satisfy the local macro/helper
    invariants

### AST And Lowering Expectations

- [src/ast_lower.c](../src/ast_lower.c)
  - no new import node should be needed for option A
  - imported syntax should work once imported clones appear as ordinary
    `AST_DEFINITION_TYPE_SYNTAXDECL` entries with fresh ids
- [src/ast_prepare.c](../src/ast_prepare.c)
  - likely place for the temporary first-cut rejection of declaration-site
    `quote { ... }` references to non-exported local macros, because template
    literal references are already resolved here
- [src/ast_pp.c](../src/ast_pp.c)
  - only touch this if better debugging output is needed while validating
    imported syntax declarations
- [src/ast.yaml](../src/ast.yaml)
  - likely no schema change is needed for option A unless implementation
    pressure shows some export metadata must survive outside parser snapshots

### Suggested First Commit

1. Extend the namespace cache in [src/pratt.yaml](../src/pratt.yaml) and
   [src/pratt_parser.c](../src/pratt_parser.c) to capture exported local macros
   and helper closure, but do not import them yet.
2. Add `export macro ...` parsing and validation in
   [src/pratt_parser.c](../src/pratt_parser.c).
3. Add one named-import path, `import ns macro name;`, in
   [src/pratt_parser.c](../src/pratt_parser.c), including fresh
   `declarationId` remapping and local `AstSyntaxDecl` materialization.
4. Add the temporary template-body rejection in
   [src/ast_prepare.c](../src/ast_prepare.c) if that edge case is not being
   supported yet.

### Suggested Test Files

- new library-style files under [tests/fn](../tests/fn), analogous to the
  operator import fixtures such as
  [tests/fn/import_ops_lib.fn](../tests/fn/import_ops_lib.fn)
- new macro import tests alongside existing syntax tests such as
  [tests/fn/test_macro_recursive_helpers.fn](../tests/fn/test_macro_recursive_helpers.fn)
  and import tests such as
  [tests/fn/test_import_operators_all.fn](../tests/fn/test_import_operators_all.fn)

Good first additions would be:

- `tests/fn/import_macro_lib.fn`
- `tests/fn/test_import_macro_named.fn`
- `tests/fn/test_import_macro_helper_closure.fn`
- `tests/fn/test_import_macro_conflict.fn`
- `tests/fn/test_import_macro_template_dependency_error.fn`

## Test Plan

Minimum useful coverage:

- export one initiating rule with one helper and import it into another file
- export one initiating rule with a recursive helper family and verify the full
  closure imports
- export multiple initiating rules that share a helper and verify the helper is
  imported once
- reject export on helper-only `syntax` declarations
- reject import when the target namespace did not export that initiating rule
- reject import when the imported helper graph conflicts with an existing local
  syntax rule
- verify that imported syntax still lowers hygienically when a binder-
  introducing template crosses the namespace boundary

## Open Questions

These can stay open for the first implementation unless they block the core
design.

- whether imported syntax should later support renaming
- whether re-export of imported syntax should be allowed
- whether declaration-site macro references inside `quote { ... }` should pull
  in a second private dependency closure of non-exported macros and their
  helper rules
- whether helper names should remain invisible to explicit source lookup after
  import, even though they exist locally as support declarations
- whether syntax exports should later support richer manifest-style curation
- whether preserving a dedicated AST import node is worth the extra machinery
  after the first cut ships
