# CEKF Project - AI Coding Assistant Instructions

> **Note**: This file is duplicated in two locations:
> - `.github/copilot-instructions.md` (this file - used by GitHub Copilot)
> - `docs/AI-ASSISTANT-GUIDE.md` (for human readers browsing the repository)
> 
> When updating, please keep both files in sync.

## Project Overview

CEKF is a **bytecode-based functional programming language VM** implementing a CEK machine (Control, Environment, Kontinuation) plus "F" for failure continuation supporting `amb` non-deterministic programming. Written in C with Python code generation tools.

**Core Innovation**: Backtracking via failure continuations. The `amb` operator (spelled `then` in the language) creates decision points; `back` backtracks to try alternatives. See `fn/barrels.fn` for a canonical example.

## Architecture Pipeline

Source flows through these stages (see README.md flowchart):

1. **Scanner** (`src/pratt_scanner.c`) → Tokens
2. **Pratt Parser** (`src/pratt_parser.c`) → AST (`src/ast.yaml`)
3. **Lambda Conversion** (`src/lambda_conversion.c`) → Plain Lambda Form (`src/lambda.yaml`)
   - Includes **TPMC** (Term Pattern Matching Compiler, `src/tpmc_*.c`) - see detailed section below
   - **Macro Expansion** (`src/macro_substitution.c`)
   - **Print Generator** (`src/print_generator.c`) - auto-generates print functions for typedefs
4. **Type Checking** (`src/tc_analyze.c`) - Hindley-Milner Algorithm W using Prolog-style logical variables
5. **Constructor Inlining** (`src/inline.c`)
6. **ANF Conversion** (`src/anf_normalize.c`) → A-Normal Form (`src/anf.yaml`) - see detailed section below
7. **Lexical Analysis** (`src/annotate.c`) - De Bruijn indexing for fast variable lookup
8. **Bytecode Compiler** (`src/bytecode.c`) → Bytecode (`src/cekfs.yaml`)
9. **CEKF Runtime** (`src/step.c`) - The virtual machine

## Critical Build System Knowledge

### Code Generation (`tools/makeAST.py`)

**The build depends heavily on Python code generation**. DO NOT manually edit files in `generated/`!

#### Overview
The `tools/makeAST.py` script (4500+ lines) reads YAML schema files and generates complete C implementations including memory management, debugging, and data structure operations. Originally for AST generation, now handles all compiler stage data structures.

#### YAML Schema Structure

Each `.yaml` file in `src/` defines structures for a compiler stage:

```yaml
config:
    name: <stage_name>              # e.g., "ast", "lambda", "anf", "cekfs"
    description: "..."              # Purpose of this stage
    parserInfo: true                # Include ParserInfo in structs
    limited_includes:               # Additional headers needed
    - bigint.h

structs:
    StructName:
        meta:
            brief: "Short description"
            description: "Detailed description"
        data:
            fieldName: fieldType
            optionalField: type=defaultValue
            
unions:
    UnionName:
        data:
            variantName: StructName

arrays:
    ArrayName:
        data: ElementType

hashes:
    HashName:
        data:
            key: KeyType
            value: ValueType
```

#### Primitives (`src/primitives.yaml`)

Common types shared across all stages - referenced via `!include primitives.yaml`:
- `HashSymbol` - Symbol table entries (always a pointer)
- `int`, `bool`, `char`, `character` - Basic types
- `BigInt`, `MaybeBigInt` - Arbitrary precision integers
- `file_id` - Source file tracking
- Each has `cname`, `printf`/`printFn`, optionally `markFn`, `compareFn`

#### Generated Functions

For each struct/union, `makeAST.py` generates:

**Memory Management:**
- `new<Type>()` - Allocator with GC header, takes all fields as args
- `copy<Type>()` - Deep copy
- `mark<Type>()` - Recursive GC marking
- `free<Type>()` - Cleanup (called by GC)

**Arrays/Stacks/Vectors:**
- `new<Array>()` - Create with initial capacity
- `push<Array>()` - Append element
- `pop<Array>()` - Remove last element
- `peek<Array>()` - Access element without removing
- `poke<Array>()` - Set element at index

**Hash Tables:**
- `new<Hash>()` - Create hash table
- `get<Hash>()` - Retrieve value by key
- `set<Hash>()` - Store value by key
- Iterator functions for traversal

**Debugging (in `*_debug.{c,h}`):**
- `print<Type>()` - Pretty-print for debugging
- `eq<Type>()` - Deep equality for testing
- `typename<Type>Obj()` - String name of type

**Object Types (in `*_objtypes.h`):**
- Enum of all object types for GC
- Switch case macros for dispatch
- Generic `mark*Obj()`, `free*Obj()`, `typename*Obj()` dispatchers

#### Key Classes in `makeAST.py`

- `Catalog` - Manages all entities in a YAML file, orchestrates generation
- `SimpleStruct` - Regular C struct
- `DiscriminatedUnion` - Tagged union (type field + union of alternatives)
- `SimpleArray` - Dynamic array with push/pop
- `SimpleStack` - Stack operations (extends SimpleArray)
- `SimpleHash` - Hash table for symbol→value mappings
- `SimpleVector` - Fixed-size arrays
- `SimpleEnum` - C enum
- `Primitive` - Built-in types from `primitives.yaml`

#### Usage Pattern

1. **Define structures** in `src/<stage>.yaml`
2. **Run make** - triggers `makeAST.py` for each YAML
3. **Generated files** appear in `generated/`
4. **Include headers** in your C code: `#include "<stage>.h"`
5. **Use generated functions** - no manual memory management code needed

#### Important Notes

- **ParserInfo**: If `parserInfo: true`, all structs get `ParserInfo I` field for error reporting
- **Default values**: Use `field: type=value` syntax in YAML
- **GC Integration**: All generated `new*()` functions automatically register with GC
- **Type safety**: Generated code includes type checking in mark/free dispatchers
- **Documentation**: YAML `meta` blocks generate doxygen-style comments

#### When Adding New Structures

1. Add to appropriate `src/*.yaml` file
2. Define in `structs:`, `unions:`, `arrays:`, or `hashes:` section
3. Add `meta` documentation (optional but recommended)
4. Run `make` - regeneration is automatic
5. Include generated header in C files that use the new types

#### Debugging Generation

If generated code looks wrong:
- Check YAML syntax (especially indentation)
- Verify types are defined (either in same YAML or `primitives.yaml`)
- Look at similar existing definitions as templates
- Run `python3 tools/makeAST.py src/<file>.yaml h` manually to see errors

### Build Modes (via `MODE=` variable)

```bash
make                    # debugging mode: -g, forces GC on every malloc
make MODE=testing       # -g without aggressive GC
make MODE=unit          # enables UNIT_TESTS
make MODE=production    # -O2, all safety checks disabled
```

### Key Make Targets

```bash
make test              # Runs C unit tests + all tests/fn/test_*.fn files
make profile           # Builds then runs callgrind profiling
make leak-check        # Runs valgrind memory leak detection
make docs              # Generates Mermaid diagrams from YAML schemas
make indent            # Formats code with GNU indent
```

## Language Syntax (`.fn` files)

### Core Syntax
- **Functions**: `fn name { (args) { body } }` or `let name = fn(args) { body }`
- **Pattern Matching**: Functions can have multiple cases, switch on arguments
  - Wildcards: `_` matches anything without binding
  - Named structures: `x = h @ t` binds both components and whole
  - Pseudo-unification: `(x, x @ _)` requires matching values
  - Switch expressions: `switch (expr) { (pattern) { result } }`
- **Typedefs**: `typedef typename(#generic) { constructor1(types) | constructor2 }`
  - Generic types use `#` prefix: `#t`, `#a`, `#b`
  - Built-in types: `int`, `char`, `bool`, `string` (alias for `list(char)`)
  - Named fields: `constructor{ fieldName: type }`
- **Operators**: Defined in `src/preamble.fn` (e.g., `@` = cons, `@@` = append)
- **Namespaces**: Files start with `namespace` keyword; import via `"path.fn"`

### Non-Deterministic Programming (amb)
- **`then`**: Right-associative binary operator - evaluates LHS, but if backtracked returns RHS
- **`back`**: Triggers backtracking to most recent `then`
- Not just try/catch - can backtrack to any chronological `then` in the call stack
- Example: `fn one_of { ([]) { back } (h @ t) { h then one_of(t) } }`

### Print System
- **Auto-generated**: Print functions created automatically for typedefs
- **User-defined**: `print typename(pt, pu, obj)` - takes printer functions for generic types
- **Built-ins**: `puts(string)`, `putc(char)`, `putn(number)`, `putv(value)`
- **Implementation**: Uses currying to specialize generic printers at call sites

### User-Defined Operators
- **Syntax**: `infix|prefix|postfix precedence "op" function`
- **Example**: `infix right 90 "@" cons` or `postfix 120 "!" factorial`
- **Restrictions**: Can't redefine with same fixity, can't have same op as both infix and postfix
- Defined in `src/preamble.fn` or user code

## Memory Management

**Mark-and-sweep GC with protection stack**:

- Use `PROTECT(var)` macro to shield objects during construction
- Must `UNPROTECT(save)` to avoid leaks
- Pattern: `int save = PROTECT(obj); /* allocating code */ UNPROTECT(save);`
- All allocated structures have `Header` with GC metadata
- Generated `mark*()` functions handle recursive marking

## Debugging

**Conditional compilation flags** (define in source to enable):
- `DEBUG_STEP`, `DEBUG_BYTECODE`, `DEBUG_TC`, `DEBUG_LAMBDA_CONVERT`, etc.
- Each stage has `#include "debugging_on.h"` or `"debugging_off.h"` pattern
- Command-line: `--dump-ast`, `--dump-lambda`, `--dump-anf`, `--dump-bytecode`

## Type System

- **Hindley-Milner** type inference with parametric polymorphism
- Implementation in `src/tc_analyze.c` uses logical variables (not substitutions)
- See `docs/TYPES.md` for the journey (final approach differs from notes!)
- Type checking happens **between** lambda conversion and ANF

## Testing

- **C Unit Tests**: `tests/src/*.c` (enabled with `MODE=unit`)
- **Language Tests**: `tests/fn/test_*.fn` run with `--assertions-accumulate`
- Tests automatically run via `make test`

## Common Patterns

**Adding a new bytecode instruction**:
1. Add to `src/cekfs.yaml` enum
2. Implement in `src/step.c` switch statement
3. Add compiler case in `src/bytecode.c`
4. Update `docs/bytecodes.md` documentation

**Adding a new AST node type**:
1. Define in appropriate `.yaml` file (ast/lambda/anf/etc)
2. Run `make` to regenerate code
3. Add handling in conversion functions for next stage
4. Add pretty-printing in `*_pp.c` if needed

**Lexical addressing**: Variables stored as `(frame, offset)` pairs after static analysis. Never use variable names at runtime.

## File Organization

- `src/` - C source and YAML schemas
- `generated/` - Auto-generated C code (don't edit!)
- `fn/` - Standard library and example programs
- `tests/fn/` - Language test suite
- `docs/` - Architecture documentation (read `MATH.md`, `V2.md`, `TYPES.md`)
- `tools/` - Python code generators and utilities
- `unicode/` - Unicode data tables (downloaded on first build)

## Key Invariants

1. **All intermediate representations are immutable** - transformations produce new structures
2. **Type checking must succeed** before proceeding to ANF
3. **Namespaces are compile-time only** - resolved during lambda conversion
4. **The preamble** (`src/preamble.fn`) defines required operators and types - compiler depends on these existing
5. **Variables lose names** after lexical addressing - only frame/offset used at runtime

## TPMC (Term Pattern Matching Compiler)

**Critical component that may need improvement** - Compiles pattern-matching function definitions into efficient decision trees.

### What TPMC Does

Converts multi-clause pattern-matching functions like:
```fn
fn map {
    (_, []) { [] }
    (f, h @ t) { f(h) @ map(f, t) }
}
```

Into optimized decision trees (DFA-like state machines) that efficiently dispatch based on argument structure.

### Architecture

**Four main files** (all in `src/`):
- `tpmc_logic.c` - Entry point, converts AST patterns to TPMC patterns, creates root variables
- `tpmc_match.c` - Core algorithm implementing the Variable Rule and Mixture Rule
- `tpmc_translate.c` - Generates lambda expressions from the compiled state machine
- `tpmc_compare.c` - Handles comparison patterns (e.g., `(x, x @ _)` where same variable appears twice)

**Supporting files**:
- `tpmc.yaml` - Defines TPMC data structures (states, arcs, patterns, matrices)
- `tpmc_pp.c` - Pretty-printing for debugging
- `tpmc_mermaid.c` - Generates Mermaid diagrams of state machines (use `--dump-tpmc=<function>`)

### The Algorithm (from Pettersson 1992 paper)

**Step 1: Pattern Matrix Construction**
- Collects all function clauses into a matrix M where rows = clauses, columns = arguments
- Each pattern gets a unique "path" name (e.g., `p$0`, `p$1`, `p$1$0` for nested)
- Creates array S of final states (function bodies)

**Step 2: DFA Generation via `match(M, S)`**
- **Variable Rule**: If top row is all wildcards, return first final state (trivial match)
- **Mixture Rule**: Find first column with constructors/comparisons:
  1. For each unique constructor K in that column:
     - Extract rows matching K, expand nested patterns
     - Recurse on sub-matrix
     - Create arc labeled with K
  2. Handle wildcards as fallback arc
  3. Add error arc if not exhaustive

**Step 3: Optimization**
- Reference-count states
- States with refcount > 1 become local functions (letrec-bound)
- Remove duplicate states

**Step 4: Code Generation**
- Test states → `switch` expressions (MATCH for constructors, COND for constants)
- Arcs → case arms
- Final states → function bodies with variable bindings in scope

### Known Issues & Improvement Areas

1. **Comparison Pattern Ordering**: When a pattern like `(x, x @ _)` appears, the algorithm must ensure `x` is bound before comparison. Current fix: prefer first column if any pattern exists in top row. May not be optimal.

2. **Nested Pattern Efficiency**: Deep nesting can produce many intermediate states. Consider flattening optimizations.

3. **Exhaustiveness Checking**: The language enforces exhaustive pattern matching via the `unsafe` qualifier. Functions with non-exhaustive patterns MUST be declared `unsafe fn ...`, and functions with exhaustive patterns CANNOT be declared unsafe. The compiler enforces both rules. See `fn/listutils.fn` for examples like `unsafe fn foldl1 (func, h @ t)` which only handles non-empty lists.

4. **Matrix Column Selection**: The "find first constructor column" heuristic is simple but may not produce minimal DFAs. Could benefit from cost-based selection.

5. **Path Naming**: Generated names like `p$1$2$0` work but are hard to debug. Better naming strategy?

### Debugging TPMC

```bash
# Generate Mermaid diagram of compiled pattern match
./bin/fn --dump-tpmc=functionName path/to/file.fn > diagram.md

# Enable debug output during compilation
# Edit src/tpmc_match.c and define DEBUG_TPMC_MATCH
```

**Watch for**:
- "unsafe function" warnings (comparisons without proper binding)
- Non-exhaustive pattern matches (may indicate missing cases)
- Large state machines (>20 states suggests optimization opportunity)

### Key Data Structures

- `TpmcMatrix` - 2D array of patterns (width=args, height=clauses)
- `TpmcState` - Either test state (with arcs) or final state (with body)
- `TpmcArc` - Transition labeled with pattern (constructor/comparison/wildcard)
- `TpmcPattern` - Wildcard, Var, Constructor, Comparison, Character, BigInt, Assignment

### Testing

Pattern matching tests in `tests/fn/test_*.fn` - especially:
- `test_tc.fn` - Complex mutual recursion with patterns
- `fn/barrels.fn` - Non-deterministic search with patterns
- `fn/listutils.fn` - Standard list operations

## ANF (A-Normal Form) Conversion

**May be overly complex and error-prone** - Converts lambda expressions to A-Normal Form where all intermediate computations are named.

### What ANF Does

Transforms nested expressions into a flat sequence of let-bindings where:
- **Atomic expressions (aexp)**: Variables, constants, lambdas - always terminate, never error
- **Complex expressions (cexp)**: Function applications, conditionals - may not terminate or may error
- All complex subexpressions become let-bound temporary variables

Example transformation:
```scheme
(a (b c) (d e))
=>
(let (t$1 (d e))
  (let (t$2 (b c))
    (a t$2 t$1)))
```

### The Algorithm (from Matt Might's blog)

**Core Idea**: Walk expressions depth-first, replacing complex subexpressions with fresh variables, accumulating let-bindings on the way back out.

**Key functions** (all in `src/anf_normalize.c`):
- `normalize(LamExp, tail)` - Main entry point, dispatches on LamExp type
- `replaceLamExp(LamExp, replacements)` - Converts LamExp to Aexp, accumulating replacements
- `letBind(body, replacements)` - Wraps body in let-bindings from replacements table
- `wrapTail(exp, tail)` - Optionally wraps expression in additional let-binding

**The `tail` parameter**: Continuation-like - represents the "rest of the computation" to wrap the current expression in. NULL means this is the final result.

### Implementation Pattern

Most normalize functions follow this pattern:
1. Create a `LamExpTable` for tracking replacements (hash table mapping fresh symbols to LamExps)
2. Call `replaceLamExp()` on subexpressions, which:
   - If subexpr is atomic (var/constant), return it as Aexp
   - If subexpr is complex (application), generate fresh symbol, add to replacements, return symbol as Aexp
3. Build the ANF construct with replaced Aexps
4. Call `wrapTail(exp, tail)` to optionally wrap in outer binding
5. Call `letBind(exp, replacements)` to wrap in all accumulated let-bindings
6. Return the wrapped expression

### Critical Data Flow

```
LamExp (lambda.yaml)
  ↓ normalize()
  → replaceLamExp() + LamExpTable
    → Aexp (atomic expressions)
  → Build ANF structure (Exp/Cexp)
  → wrapTail()
  → letBind() - wraps in let-bindings
  ↓
Exp (anf.yaml)
```

### Known Complexity Issues

1. **Deeply Nested Functions**: The normalize functions have 30+ dispatch cases, one per LamExp type. Each follows slightly different logic.

2. **GC Protection Overhead**: Extensive use of PROTECT/UNPROTECT macros throughout due to allocations during traversal. Easy to get wrong.

3. **Tail Threading**: The `tail` parameter threads through recursion but its purpose isn't always clear. Sometimes NULL, sometimes accumulated let-bindings.

4. **Dual Type System**: Must track both LamExp (input) and Aexp/Cexp/Exp (output) simultaneously. Easy to confuse which type is which.

5. **Replacements Table**: The `LamExpTable` accumulates symbol→LamExp mappings that become let-bindings, but lifetime and scope isn't always obvious.

### Debugging ANF

```bash
# Enable debug output
# Edit src/anf_normalize.c and define DEBUG_ANF

# Dump ANF for inspection
./bin/fn --dump-anf path/to/file.fn
```

**Watch for**:
- Incorrect nesting of let-bindings
- Fresh symbol collisions (shouldn't happen but indicates `freshSymbol()` issues)
- GC crashes (usually from missing PROTECT/UNPROTECT)
- Type mismatches between LamExp and ANF structures

### Potential Improvements

1. **Simplify normalize dispatch**: Could the 30+ cases share more common code?
2. **Clearer tail semantics**: Document when tail is NULL vs. non-NULL
3. **Reduce PROTECT overhead**: Could intermediate allocations be batched?
4. **Better error messages**: When ANF conversion fails, why?
5. **Refactor replacements**: The hash table approach works but is it the clearest?

### Key Files

- `src/anf_normalize.c` - The implementation (1100+ lines)
- `src/anf.yaml` - ANF data structures (Exp, Aexp, Cexp)
- `src/lambda.yaml` - Input lambda structures
- `docs/ANF.md` - Original algorithm notes
- Reference: [Matt Might's ANF blog post](https://matt.might.net/articles/a-normalization/)

## When Reading Code

- Start at `src/main.c` for overall flow
- Each stage has `*_helper.h` with utility functions
- Generated `*_debug.c` files are essential for understanding structure relationships
- `docs/MATH.md` formalizes the CEKF machine mathematics
- `docs/lambda-conversion.md` has extensive TPMC algorithm walkthrough with examples
- Pratt parsing is unusual - see `src/pratt_parser.c` for infix operator handling
