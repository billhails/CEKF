# CEKF Project - AI Coding Assistant Instructions

## Project Overview

CEKF, a.k.a.  F♮ is a **bytecode-based functional programming language VM** Written in C with Python code generation tools.

## Architecture Pipeline

Source flows through these stages:

1. **Scanner** (`src/pratt_scanner.c`) (text → [`PrattToken`](../src/pratt.yaml)) — *see [pratt-parser.md](../docs/agent/pratt-parser.md)*
2. **Pratt Parser** (`src/pratt_parser.c`) → AST  (`PrattToken*` → [`AstNode*`](../src/ast.yaml)) — *see [pratt-parser.md](../docs/agent/pratt-parser.md)*
3. **Lambda Conversion** (`src/lambda_conversion.c`) → Plain Lambda Form  (`AstNode*` → [`LamExp*`](../src/lambda.yaml))
   - Includes **TPMC** (Term Pattern Matching Compiler, `src/tpmc_*.c`) — *see [tpmc.md](../docs/agent/tpmc.md)*
   - **Lazy Function Expansion** (`src/macro_substitution.c`)
   - **Print Generator** (`src/print_generator.c`)
4. **Type Checking** (`src/tc_analyze.c`) - Hindley-Milner Algorithm W using Prolog-style logical variables  (`LamExp*` → [`TcType*`](../src/tc.yaml))
5. **Constructor Inlining** (`src/inline.c`) (`LamExp*` → `LamExp*`)
6. **Desugaring** (`src/lambda_desugar.c`) - Simplifies syntactic sugar (`LamExp*` → [`MinExp*`](../src/minlam.yaml))
7. **Alpha Conversion** (`src/lambda_alphaconvert.c`) - Renames variables to avoid capture (`MinExp*` → `MinExp*`)
8. **ANF Conversion** (`src/anf_normalize.c`) → A-Normal Form (`MinExp*` → [`AnfExp*`](../src/anf.yaml)) — *see [anf.md](../docs/agent/anf.md)*
9. **Lexical Analysis** (`src/annotate.c`) - De Bruijn indexing for fast variable lookup (`AnfExp*` → `AnfExp*`)
10. **Bytecode Compiler** (`src/bytecode.c`) → Bytecode (`AnfExp*` → [`ByteCodeArray`](../src/cekfs.yaml))
11. **CEKF Runtime** (`src/step.c`) - The virtual machine

## Build System

The system uses GNU make, and targets CLang or GCC.

### Code Generation

The build depends heavily on code generation. Do not manually edit files in `generated/`. See [code-generation.md](../docs/agent/code-generation.md) for details.

### Build Modes

(via `MODE=` variable):

```bash
make                    # default MODE=debug: -g, enables `--stress-gc` flag which will force GC on every malloc
make MODE=testing       # -g without aggressive GC option
make MODE=production    # -O2, all safety checks disabled
```

### Key Make Targets

```bash
make test              # Builds then runs C unit tests + all tests/fn/test_*.fn files
make profile           # Builds then runs callgrind profiling
make leak-check        # Runs valgrind memory leak detection
make docs              # Generates Mermaid diagrams from YAML schemas
```

## Memory Management

### Mark-and-sweep GC with global protection stack

- Use `PROTECT(obj)` macro to shield objects during construction
- `PROTECT(obj)` pushes `obj` onto protection stack and returns the previous stack pointer
- `UNPROTECT(save)` restores the stack pointer to `save` (previous result of `PROTECT()`)
- Pattern: `int save = PROTECT(obj); /* allocating code */ UNPROTECT(save);`
- Never use literal numbers with UNPROTECT - only values returned by `PROTECT()`

### HashSymbol objects must never be PROTECT'ed

- `HashSymbol` objects are not GC-managed (no `Header`)
- Symbols are automatically interned in a global symbol table and never freed
- Calling `PROTECT(hashSymbol)` will corrupt the protection stack

## C Coding Conventions

### Generated union constructor functions

- `new<Union>_<Variant>(parserInfo, variant)` - Wraps an existing variant in a union
- `make<Union>_<Variant>(parserInfo, field1, field2, ...)` - Constructs variant then wraps in union
- Visitor patterns should use `new*` functions since they already have visited variants

### Type-safe union accessors

- `get<Union>_<Variant>(union*)` - Safely extracts variant from union with type checking
- Prefer this over direct field access (`node->val.amb`) for safety

### ParserInfo access

- `CPI(node)` - Macro to access node's parser info (expands to `node->_yy_parser_info`)
- Always use this macro instead of direct field access

### Pointer comparisons

- Always use explicit NULL comparisons: `if (ptr != NULL)` or `if (ptr == NULL)`

### Naming Conventions

- Types: `MixedCase` (e.g., `LamExp`, `AstExpression`)
- Functions and Variables: `camelCase` (e.g., `cpsTcLamExp`, `newLamExp_Amb`)
- Avoid snake case; underscores reserved for union discriminators i.e. `newLamExp_Amb`.

## Documentation Style

- Use simple periods instead of exclamation points
- Avoid hyperbole: use "significant", "notable" instead of "HUGE", "Amazing"
- Avoid emphatic modifiers in headings and verdict-style declarations
- No emoji
- Minimize bold emphasis on routine statements
- Follow markdownlint rules

## Debugging

### Conditional compilation flags

(define in source to enable):

- `DEBUG_STEP`, `DEBUG_BYTECODE`, `DEBUG_TC`, `DEBUG_LAMBDA_CONVERT`, etc.
- Defined in `src/common.h`
- Each stage has `#include "debugging_on.h"` or `"debugging_off.h"` pattern

### Command-line flags

- `--dump-ast`, `--dump-lambda`, `--dump-anf`, `--dump-bytecode` etc.
- `--exec="<snippet>"` to run code snippet directly
- `--stress-gc` forces GC on every allocation
- `--help` shows all.

## Common Patterns

### Adding a new bytecode instruction

1. Add to `src/cekfs.yaml` enum
2. Implement in `src/step.c` switch statement
3. Add compiler case in `src/bytecode.c`
4. Update `docs/bytecodes.md` documentation

### Adding a new AST node type

1. Define in appropriate `.yaml` file (ast/lambda/anf/etc)
2. Run `make` to regenerate code
3. Add handling in conversion functions for next stage
4. Add pretty-printing in `*_pp.c` if needed

## File Organization

- `src/` - C source and YAML schemas
- `generated/` - Auto-generated C code (don't edit!)
- `fn/` - Standard library and example programs
- `tests/fn/` - Language test suite
- `docs/` - Architecture documentation (read `MATH.md`, `V2.md`, `TYPES.md`)
- `docs/agent/` - Detailed stage-specific documentation for AI assistants
- `tools/` - Python code generators and utilities

## Key Invariants

  1. Intermediate representations are typically (not always) immutable - transformations produce new structures
  2. Type checking must succeed before proceeding to ANF
  3. Namespaces are compile-time only - resolved during lambda conversion
  4. The preamble (`src/preamble.fn`) defines required operators and types - compiler depends on these
  5. Variables lose names after lexical addressing - only frame/offset used at runtime

## Testing

- **C Unit Tests**: `tests/src/*.c` - alternative `main()` functions.
- **Language Tests**: `tests/fn/test_*.fn` run with `--assertions-accumulate`
- Tests automatically run via `make test`

## Stage-Specific Documentation

For detailed information on specific compiler stages, see:

- [code-generation.md](../docs/agent/code-generation.md) - YAML schemas and generated code
- [pratt-parser.md](../docs/agent/pratt-parser.md) - Parser, operators, syntactic extension
- [tpmc.md](../docs/agent/tpmc.md) - Pattern matching compilation
- [anf.md](../docs/agent/anf.md) - A-Normal Form conversion
- [language-syntax.md](../docs/agent/language-syntax.md) - F♮ language reference

## When Reading Code

- Start at `src/main.c` for overall flow
- Each stage has `*_helper.h` with utility functions
- Generated `*_debug.c` files contain basic pretty-printers for data structures
- `docs/lambda-conversion.md` has extensive TPMC algorithm walkthrough

## Workflows and Cross-Cutting Concerns

See [workflows.md](../docs/agent/workflows.md) for detailed guides on:
- Error Handling (User vs Internal errors)
- Adding Built-in Functions
