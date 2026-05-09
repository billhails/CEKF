# Proposal: Namespace Function Imports

## Summary

Add a parser-side convenience import for namespace functions:

```fn
link "listutils.fn" as list;
import list.map;
```

The imported name should behave as if the parser had synthesized:

```fn
fn map(a$0, a$1) { list.map(a$0, a$1) }
```

This fits the existing pipeline well because namespace references are already
resolved and flattened in `ast_ns.c`, and later inlining plus eta-reduction can
remove the wrapper.

The key design question is arity discovery. The good news is that the parser
already has enough information to recover arity from the parsed namespace AST,
so a new type-checking or runtime mechanism is not required.

## What Exists Today

### Namespace parsing and storage

- `link "file.fn" as ns;` parses the target file immediately in
  `src/pratt_parser.c:1396` via `parseLink()`.
- The parsed namespace definitions are stored in the global `nameSpaces` array
  as `AstNameSpaceImpl` entries.
- The parser also caches exported operators and macros per namespace in
  `nsOpsCache`, but this cache does not contain ordinary function metadata.

### Current import support

- `import` is handled entirely in `src/pratt_parser.c:3322` by `importOp()`.
- Today it only supports:
  - `import ns operators;`
  - `import ns operator "...";`
  - `import ns macro name;`
- Ordinary namespace members are not imported into the local scope.

### Qualified namespace calls already work

- `list.map(...)` already parses today.
- `scopedSymbol()` in `src/pratt_parser.c:3813` resolves `list.map` into a
  namespace lookup.
- `ast_ns.c` later rewrites namespace references to qualified global symbols and
  empties `AstProg.nameSpaces` entirely.

This means `import list.map;` is not a new visibility mechanism. It is only a
local wrapper around an already legal qualified call.

## Recommended Scope

### V1: strict `fn` imports only

Support:

```fn
import list.map;
```

Do not support in the first pass:

- `lazy fn` imports
- typedef imports
- constructor imports
- bulk imports such as `import list functions;`
- aliasing syntax such as `import list.map as map2;`

This keeps the feature aligned with the proposed semantics: synthesize an
ordinary wrapper function and let existing optimization passes remove it.

## Why `lazy fn` Should Not Use The Same Rewrite

The obvious rewrite:

```fn
lazy fn map(a$0, a$1) { list.map(a$0, a$1) }
```

is not obviously correct under the current lazy pipeline.

Relevant behavior:

- Lazy definitions are converted by `convertAstLazy()` in
  `src/lambda_conversion.c:892`.
- Lazy arguments are represented as thunks, and occurrences of lazy parameters
  inside the body are forced by `lamPerformLazySubstitutions()`.
- Lazy calls are recognized by symbol name and re-thunked in `makePrimApp()` in
  `src/lambda_conversion.c:1311`.

If a lazy wrapper forces its parameters and then forwards them to another lazy
function, the callee will receive thunks of already-forced values, not the
original caller thunks. That is a different protocol from direct lazy calling.

So there are two safe options:

1. Exclude `lazy fn` imports from V1.
2. Introduce a dedicated non-wrapper import form downstream of parsing.

For this proposal I recommend option 1.

## Arity Investigation

### Ordinary functions

Arity is already present in the namespace AST.

- Top-level `fn` declarations parse to `AstDefinition_Define` whose expression is
  an `AstExpression_Fun`.
- Composite functions are expanded into `AstCompositeFunction` chains by
  `makeAstCompositeFunction()` in `src/pratt_parser.c:460`.
- Lambda conversion already treats the first clause as authoritative for arity:
  `countAstFargList(fun->function->argList)` in
  `src/lambda_conversion.c:1961`.
- The generated AST helpers already expose `countAstFargList()` in
  `generated/ast.h`.

So for a namespace definition like:

```fn
fn map(f, lst) { ... }
```

the parser can recover arity by:

1. Finding the matching `AstDefinition` in the namespace.
2. Confirming it is `AST_DEFINITION_TYPE_DEFINE`.
3. Confirming the expression is `AST_EXPRESSION_TYPE_FUN`.
4. Counting the first function's formal arguments with `countAstFargList()`.

### Lazy functions

Lazy definitions also expose an arity:

- They parse as `AST_DEFINITION_TYPE_LAZY`.
- The underlying parameters are available at
  `lazy->definition->altArgs->argList`.

But, as above, the forwarding semantics are not the same as strict forwarding,
so knowing the arity is not enough to make the wrapper correct.

### Non-functions

Namespace members that should be rejected for V1:

- `AST_DEFINITION_TYPE_DEFINE` where the expression is not `AST_EXPRESSION_TYPE_FUN`
- `AST_DEFINITION_TYPE_LAZY`
- `AST_DEFINITION_TYPE_TYPEDEF`
- `AST_DEFINITION_TYPE_ALIAS`
- `AST_DEFINITION_TYPE_MULTI`

The parser should report these as not importable by `import ns.name`.

### No type-signature fallback exists

There is no separate function-signature AST to consult for arity.

- `AstTypeSig` in `src/ast.yaml` is for typedefs, not function declarations.
- The signature comments in library files are documentation only.

So the definition AST is the right source of truth.

## Proposed Parsing Strategy

Extend `importOp()` in `src/pratt_parser.c` with a new early branch:

1. Parse the first atom after `import` as the namespace symbol.
2. If the next token is `.`:
   - parse the member name
   - synthesize a local wrapper definition
   - return that wrapper definition instead of a blank import marker
3. Otherwise keep the current operator/macro import behavior unchanged.

That gives two disjoint surfaces:

- `import list.map;` for function import
- `import list operators;` and `import list macro ...;` for existing behavior

This is a minimal grammar change because `.` already distinguishes the new form
from the existing `import ns ...` syntax.

## Proposed AST Rewrite

For `import list.map;`, synthesize an ordinary top-level definition:

```fn
fn map(a$0, a$1) { list.map(a$0, a$1) }
```

Implementation shape:

1. Build a formal `AstFargList` and matching actual `AstExpressions` using the
   recovered arity.
2. Build the callee as the same AST shape the parser would produce for
   `list.map`.
3. Wrap the call in an `AstNest`, `AstFunction`, and `AstExpression_Fun`.
4. Return `makeAstDefinition_Define(...)` for the imported local name.

The existing builtin wrapper generator in `src/wrapper_synthesis.c` is a good
pattern to copy:

- deterministic generated argument names
- parallel formal and actual argument lists
- plain function body containing a single call

I would keep this helper local to `pratt_parser.c` first. If more wrapper
generation features appear later, it can be shared then.

## Recommended Helper API

Inside `src/pratt_parser.c`, add a small internal resolver, something like:

```c
typedef struct {
    HashSymbol *name;
    int arity;
    bool isLazy;
    AstDefinitionType definitionType;
} ImportedFunctionSpec;
```

And helpers equivalent to:

- `resolveImportedFunctionSpec(int nsRef, HashSymbol *member, ImportedFunctionSpec *out)`
- `makeImportedFunctionWrapper(ParserInfo PI, int nsRef, HashSymbol *nsSymbol, HashSymbol *member, int arity)`

Resolution should walk `nameSpaces->entries[nsRef]->definitions` and return the
first top-level matching definition.

## Error Handling

The parser should reject these cases with direct errors:

- unknown namespace
- unknown namespace member
- member exists but is not an ordinary `fn`
- member is a `lazy fn` and lazy imports are not supported yet
- imported local name conflicts with an existing binding in the same scope

The last item is worth checking explicitly in the parser so the failure happens
at the import site, not later during preparation.

## Why This Fits The Existing Pipeline

This approach works with the pipeline as it already exists:

1. Parser synthesizes a normal local function definition.
2. `ast_ns.c` rewrites the qualified call to the namespace-qualified global
   symbol.
3. Namespace definitions are merged into the preamble.
4. Inlining can inline the qualified target.
5. Eta reduction can eliminate the imported wrapper.

No new downstream IR is required.

## Alternative Designs

### Alternative A: extend `nsOpsCache` with function metadata

Possible, but not necessary for V1.

Pros:

- fast import lookup
- clear separation between parser metadata and AST traversal

Cons:

- duplicates information already present in `AstNameSpaceImpl`
- forces a new cache design before the feature is proven

I would only do this if V2 adds bulk imports.

### Alternative B: add an AST import node and lower it later

This is the more general long-term design.

Pros:

- could support lazy imports, typedef imports, constructor imports, and aliasing
- could centralize conflict checks in one lowering pass

Cons:

- much more invasive than needed for strict function wrappers
- adds new AST forms only to erase them immediately

This looks better as a follow-up if function imports grow beyond simple wrapper
generation.

## Suggested V1 Implementation Plan

1. Extend `importOp()` to detect `import ns.name;`.
2. Add a namespace-definition scan helper that resolves ordinary function
   arity from `AstNameSpaceImpl`.
3. Synthesize a strict wrapper `AstDefinition_Define`.
4. Reject `lazy fn` imports with a targeted parser error.
5. Add tests for:
   - successful import of a normal namespace function
   - import of unknown function
   - import of non-function definition
   - import name collision
   - rejection of lazy function import

## Recommendation

Implement `import ns.name;` as a parser-synthesized wrapper for ordinary `fn`
definitions only.

Arity should be derived directly from the parsed namespace AST by inspecting the
matched top-level definition and counting its formal arguments. That is the
smallest change that matches the current architecture and requires no new
downstream machinery.

Do not include `lazy fn` in the first implementation. The simple wrapper rewrite
does not preserve the current lazy calling protocol.
