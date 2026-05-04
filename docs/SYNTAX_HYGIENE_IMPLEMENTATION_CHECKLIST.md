# Syntax Hygiene Implementation Checklist

## Scope

This checklist turns the stage-5 design into executable engineering steps.

Primary references:

- [docs/SYNTAX_HYGIENE_DESIGN.md](docs/SYNTAX_HYGIENE_DESIGN.md)
- [docs/SYNTAX_HYGIENE_YAML_DRAFT.md](docs/SYNTAX_HYGIENE_YAML_DRAFT.md)

Current code anchors:

- parser syntax entry and expansion in [src/pratt_parser.c](src/pratt_parser.c)
- pipeline position between namespace and lambda conversion in
  [src/main.c](src/main.c#L529)
- namespace rewrite pass in [src/ast_ns.c](src/ast_ns.c#L391)

## Exit Criteria

- Syntax declarations are represented in AST as durable declaration carriers.
- Syntax uses are represented as AST carriers and not expanded in the parser.
- A syntax preparation pass runs after namespace rewriting and before lowering.
- A syntax lowering pass removes all syntax carriers before lambda conversion.
- Lambda conversion code remains unchanged except for defensive assertions.
- Existing syntax and macro tests pass after migration.

## Phase 0: Branch Safety And Baseline

### Tasks (Phase 0)

- [ ] Record baseline test status before refactor.
- [ ] Confirm no unrelated working-tree conflicts in target files.
- [ ] Capture baseline output for a representative syntax test.

### Validation (Phase 0)

- [ ] Run `make test` and capture failures if any.
- [ ] Run one focused syntax test file manually through `bin/fn` if useful.

## Phase 1: Introduce AST Schema Carriers And Template IR

### Files (Phase 1)

- [ ] [src/ast.yaml](src/ast.yaml)

### Tasks (Phase 1)

- [ ] Add AST syntax declaration carrier type.
- [ ] Add AST expression syntax-use carrier type.
- [ ] Add AST definition syntax-use carrier type.
- [ ] Add syntax binding array/struct type for captured and inherited bindings.
- [ ] Add syntax enums for entry kind, result kind, and syntax class.
- [ ] Add template IR node family with explicit provenance nodes:
  - literal reference
  - introduced binder
  - introduced reference
  - unquote site
- [ ] Extend `AstDefinition` union with syntax-related variants.
- [ ] Extend `AstExpression` union with expression syntax-use variant.

### Notes (Phase 1)

- Ensure parserInfo behavior remains consistent with existing AST generation.

### Validation (Phase 1)

- [ ] Build generated artifacts with `make`.
- [ ] Confirm generated AST headers/sources compile.

## Phase 2: Slim Pratt Schema To Parser-Only Matcher Metadata

### Files (Phase 2)

- [ ] [src/pratt.yaml](src/pratt.yaml)

### Tasks (Phase 2)

- [ ] Add `declarationId` to parser syntax spec metadata.
- [ ] Keep parser-side pattern matching data only.
- [ ] Remove durable template ownership from Pratt alternative/spec types.
- [ ] Keep parser-side binding helper type only if needed during match.
- [ ] Preserve existing compatibility naming (`PrattMacro*`) for this phase.

### Validation (Phase 2)

- [ ] Rebuild generated Pratt artifacts.
- [ ] Compile parser with no functional behavior change yet.

## Phase 3: Extract Parser Syntax Code Into New Modules

### Files (Phase 3)

- [ ] [src/syntax_parse.h](src/syntax_parse.h)
- [ ] [src/syntax_parse.c](src/syntax_parse.c)
- [ ] [src/syntax_template.h](src/syntax_template.h)
- [ ] [src/syntax_template.c](src/syntax_template.c)
- [ ] [src/pratt_parser.c](src/pratt_parser.c)
- [ ] [src/pratt_parser.h](src/pratt_parser.h)
- [ ] [Makefile](Makefile) if new units must be listed explicitly

### Tasks (Phase 3)

- [ ] Move parser-side syntax declaration parsing helpers from
  [src/pratt_parser.c](src/pratt_parser.c):
  - optional parameters parsing
  - alternative parsing
  - alternative validation
- [ ] Move quote/unquote token/template parsing helpers to `syntax_template`.
- [ ] Move syntax matching engine helpers to `syntax_parse`.
- [ ] Keep `definition()` and `userSyntaxExpr()` in parser as thin dispatchers.
- [ ] Expose minimal non-static parser utility APIs needed by new modules.
- [ ] Avoid behavior changes in this extraction phase.

### Validation (Phase 3)

- [ ] Parser still builds.
- [ ] Existing syntax-related tests still pass unchanged.

## Phase 4: Emit AST Syntax Carriers Instead Of Parser-Time Expansion

### Files (Phase 4)

- [ ] [src/syntax_parse.c](src/syntax_parse.c)
- [ ] [src/pratt_parser.c](src/pratt_parser.c)

### Tasks (Phase 4)

- [ ] Replace parser-time substitution result path with carrier construction.
- [ ] Update syntax declaration parsing to emit AST declaration node instead of
      blank definition.
- [ ] Update expression entry path to emit AST expression syntax-use node.
- [ ] Keep helper matching recursive behavior but store captures in carrier.
- [x] Remove direct dependency on `substituteSyntax*` for successful paths.

### Validation (Phase 4)

- [ ] `--dump-ast` shows syntax declaration and syntax-use nodes.
- [ ] Parser accepts same syntax test files as before.

## Phase 5: Add Syntax Awareness To Namespace Rewrite Pass

### Files (Phase 5)

- [ ] [src/ast_ns.c](src/ast_ns.c)

### Tasks (Phase 5)

- [ ] Add visitor cases for syntax declaration carriers.
- [ ] Add visitor cases for syntax-use carriers.
- [ ] Ensure declaration-site literal template refs are rewritten in declaration
      context.
- [ ] Ensure captured use-site fragments are rewritten in use-site context.
- [ ] Do not lower syntax in namespace pass.

### Validation (Phase 5)

- [ ] Namespace tests still pass.
- [ ] Syntax tests with namespace-qualified references pass or fail only for
      expected unimplemented stage-6 behavior.

## Phase 6: Implement Syntax Preparation Pass

### Files (Phase 6)

- [ ] [src/syntax_prepare.h](src/syntax_prepare.h)
- [ ] [src/syntax_prepare.c](src/syntax_prepare.c)
- [ ] [src/main.c](src/main.c)

### Tasks (Phase 6)

- [ ] Implement lexical walk of AST before syntax lowering.
- [ ] Introduce temporary unique-symbol assignment for ordinary binders and refs
      in this pass scope.
- [ ] Resolve template literal refs to declaration-site bindings.
- [ ] Preserve use-site identity for captured bindings.
- [ ] Keep syntax carriers intact for lowering.
- [ ] Insert call into pipeline after `nsAstProg` and before `syntaxLowerAst`.

### Validation (Phase 6)

- [ ] Shadowing-focused test proves declaration-site literal resolution.
- [ ] Shadowing-focused test proves use-site unquote preservation.

## Phase 7: Implement Syntax Lowering Pass

### Files (Phase 7)

- [ ] [src/syntax_lower.h](src/syntax_lower.h)
- [ ] [src/syntax_lower.c](src/syntax_lower.c)
- [ ] [src/main.c](src/main.c)

### Tasks (Phase 7)

- [ ] Lower expression syntax-use carriers to ordinary `AstExpression`.
- [ ] Lower definition syntax-use carriers to ordinary `AstDefinition`.
- [ ] Freshen introduced binders with stable mapping per expansion instance.
- [ ] Rewrite introduced refs to freshened names.
- [ ] Splice unquotes from prepared captures.
- [ ] Apply declaration-site resolved literal references.
- [ ] Remove syntax declaration nodes from final AST.
- [ ] Insert lowering call in pipeline after `syntaxPrepareAst` and before
      `lamConvertProg`.

### Validation (Phase 7)

- [ ] `--dump-ast` after lowering has no syntax carriers.
- [ ] Pipeline reaches lambda conversion without syntax node crashes.

## Phase 8: Remove Old Parser Substitution Path

### Files (Phase 8)

- [ ] [src/pratt_parser.c](src/pratt_parser.c)
- [ ] [src/syntax_parse.c](src/syntax_parse.c)

### Tasks (Phase 8)

- [x] Delete unused `substituteSyntax*` function family.
- [ ] Delete old quote-wrapper hack helpers no longer needed.
- [ ] Remove obsolete temporary parser overrides if now centralized.
- [ ] Keep rollback and mismatch/error distinction behavior intact.

### Notes (Phase 8)

- The `substituteSyntax*` family was removed once carrier emission made it dead
  code. Remaining Phase 8 work is limited to any residual parser-only cleanup.

### Validation (Phase 8)

- [ ] Build clean with no dead code warnings related to syntax substitution.

## Phase 9: Add Defensive Checks And Diagnostics

### Files (Phase 9)

- [ ] [src/lambda_conversion.c](src/lambda_conversion.c)
- [ ] [src/main.c](src/main.c)
- [ ] [src/syntax_lower.c](src/syntax_lower.c)

### Tasks (Phase 9)

- [ ] Add defensive assert or error if syntax carriers leak past lowering.
- [ ] Add clear diagnostics for result-kind mismatch in lowering.
- [ ] Add clear diagnostics for unresolved declarationId at lowering time.

### Validation (Phase 9)

- [ ] Negative tests for mismatched kinds produce explicit errors.

## Phase 10: Test Migration And Expansion

### Files (Phase 10)

- [ ] Existing syntax and macro tests under [tests/fn](tests/fn)
- [ ] New stage-5 hygiene tests under [tests/fn](tests/fn)

### Tasks (Phase 10)

- [ ] Keep these existing tests green during migration:
  - [tests/fn/test_macro_definitions.fn](tests/fn/test_macro_definitions.fn)
  - [tests/fn/test_macro_list_comprehension.fn](tests/fn/test_macro_list_comprehension.fn)
  - [tests/fn/test_macro_recursive_helpers.fn](tests/fn/test_macro_recursive_helpers.fn)
  - [tests/fn/test_macro_quote_unquote.fn](tests/fn/test_macro_quote_unquote.fn)
  - [tests/fn/test_macro_for_loop.fn](tests/fn/test_macro_for_loop.fn)
- [ ] Keep operator-collision syntax tests green:
  - [tests/fn/test_syntax_quote_operator_collision.fn](tests/fn/test_syntax_quote_operator_collision.fn)
  - [tests/fn/test_syntax_unquote_operator_collision.fn](tests/fn/test_syntax_unquote_operator_collision.fn)
- [ ] Add new tests:
  - declaration-site literal ref under shadowing
  - use-site unquote under shadowing
  - introduced binder freshness under shadowing
  - no-carrier-leak smoke test (if supported via debug/assert path)

### Validation (Phase 10)

- [ ] Run `make test` and verify full pass.

## Phase 11: Cleanup And Follow-On

### Files (Phase 11)

- [ ] [docs/SYNTAX_IMPLEMENTATION_PLAN.md](docs/SYNTAX_IMPLEMENTATION_PLAN.md)
- [ ] [docs/SYNTAX.md](docs/SYNTAX.md)
- [ ] [docs/SYNTAX_HYGIENE_DESIGN.md](docs/SYNTAX_HYGIENE_DESIGN.md)

### Tasks (Phase 11)

- [ ] Update docs to reflect implemented pipeline and file boundaries.
- [ ] Mark parser-time substitution path as removed.
- [ ] Record remaining deferred items for stage 6 and beyond.

## Suggested Delivery Slices

Use these slices to reduce risk.

### Slice A: Schema + Extraction (No Behavior Change)

- Phase 1
- Phase 2
- Phase 3

### Slice B: Carriers End-To-End (Parser to AST)

- Phase 4
- Phase 5

### Slice C: Hygiene Semantics

- Phase 6
- Phase 7
- Phase 8

### Slice D: Hardening

- Phase 9
- Phase 10
- Phase 11

## Minimum Stop Points

If work must pause, stop only at these coherent boundaries.

1. End of Slice A with all existing tests green.
2. End of Slice B with `--dump-ast` showing carriers and parser green.
3. End of Slice C with no carriers past lowering and lambda conversion green.
4. End of Slice D with full test suite green and docs updated.
