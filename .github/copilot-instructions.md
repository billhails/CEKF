# CEKF Project - AI Coding Assistant Instructions

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

### Code Generation (`tools/generate.py` and `tools/generate/`)

**The build depends heavily on Python code generation**. DO NOT manually edit files in `generated/`!

#### Overview
The code generator is now modular: the main entry point is `tools/generate.py`, which orchestrates the `generate` Python package (in `tools/generate/`). This package contains all logic for parsing YAML schemas and generating C code for all compiler stages. The old monolithic `makeAST.py` has been fully refactored into modules such as `catalog.py`, `primitives.py`, `fields.py`, `structs.py`, `unions.py`, `arrays.py`, `vectors.py`, `hashes.py`, and more. All code generation is now managed through this modular structure.

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
            autoInitField: type=initValue    # Constructor auto-initializes, not a parameter
            
unions:
    UnionName:
        data:
            variantName: StructName

arrays:
    ArrayName:
        entries: ElementType

hashes:
    HashName:
        data:
            entries: ValueType
```

#### Primitives (`src/primitives.yaml`)

Common types shared across all stages - referenced via `!include primitives.yaml`:
- `HashSymbol` - Symbol table entries (always a pointer)
- `int`, `bool`, `char`, `character` - Basic types
- `BigInt`, `MaybeBigInt` - Arbitrary precision integers
- `file_id` - Source file tracking
- Each has `cname`, `printf`/`printFn`, optionally `markFn`, `compareFn`

#### Generated Functions

For each struct/union, the code generator (via `tools/generate.py` and the `generate` package) generates:

**Memory Management:**
- `new<Type>()` - Allocator with GC header, takes all fields as args
- `new<Union>_<Component>()` - Creates discriminated union variant in type-safe way
- `make<Union>_<Component>()` - Creates the Component and wraps in union
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

#### Extended Features for Generated Unions

- `new<union>_<variant>(variant)` creates a union from an existing variant with the correct discriminating tag.
- `make<union>_<variant>(fields...)` creates the variant and wraps it in the union in one step.
- `get<Union>_<variant>(unionPtr)` extracts the variant from the union, throwing an error if the type does not match.

#### Key Classes in `makeAST.py`

The main classes are now in the `generate` package:
- `Catalog` - Manages all entities in a YAML file, orchestrates generation
- `SimpleStruct`, `DiscriminatedUnion`, `SimpleArray`, `SimpleStack`, `SimpleHash`, `SimpleVector`, `SimpleEnum`, `Primitive`, and more, each in their own module.

#### Usage Pattern

1. **Define structures** in `src/<stage>.yaml`
2. **Run make** - triggers `generate.py` for each YAML (via Makefile)
3. **Generated files** appear in `generated/`
4. **Include headers** in your C code: `#include "<stage>.h"`
5. **Use generated functions** - no manual memory management code needed

#### Important Notes

- **ParserInfo**: If `parserInfo: true`, all structs get `ParserInfo I` field for error reporting
- **Auto-initialized fields**: Use `field: type=value` syntax in YAML to have constructor initialize the field automatically rather than requiring it as a parameter
- **GC Integration**: All generated `new*()` functions automatically register with GC
- **Type safety**: Generated code includes type checking in mark/free dispatchers
- **Documentation**: YAML `meta` blocks generate doxygen-style comments

#### When Adding New Structures

1. Add to appropriate `src/*.yaml` file
2. Define in `structs:`, `unions:`, `arrays:`, or `hashes:` section
3. Add `meta` documentation (optional but recommended)
4. Run `make` - regeneration is automatic
5. Include generated header in C files that use the new types
6. **For GC-managed types**: If the YAML generates `*_objtypes.h`:
   - Include the generated `*_objtypes.h` file in `src/memory.h`
   - Add the `*_OBJTYPES()` macro to the `ObjType` enum at the bottom of `memory.h`
   - This registers the types with the garbage collector

#### Debugging Generation

If generated code looks wrong:
- Check YAML syntax (especially indentation)
- Verify types are defined (either in same YAML or `primitives.yaml`)
- Look at similar existing definitions as templates
- Run `python3 tools/generate.py src/<file>.yaml h` manually to see errors

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
- **Let/In blocks**: `let` declarations `in` expressions
  - `let` introduces mutually recursive definitions (letrec semantics)
  - `in` begins the body that uses those definitions
  - Can appear at file top-level or inside any `{ }` block
  - Without `let`/`in`, blocks are just sequences of expressions
  - **IMPORTANT**: Nested `let/in` blocks must be enclosed in curly braces: `let x = 1; in { let y = 2; in y + x }`
- **Pattern Matching**: Functions can have multiple cases, switch on arguments
  - Wildcards: `_` matches anything without binding
  - Named structures: `x = h @ t` binds both components and whole
  - Pseudo-unification: `(x, x @ _)` requires matching values
  - Switch expressions: `switch (expr) { (pattern) { result } }`
- **Typedefs**: `typedef typename(#generic) { constructor1(types) | constructor2 }`
  - Generic types use `#` prefix: `#t`, `#a`, `#b`
  - Built-in types: `int`, `char`, `bool`, `string` (alias for `list(char)`)
  - Named fields: `constructor{ fieldName: type }`
  - **IMPORTANT**: Typedefs at file level are global; typedefs inside `let` blocks are scoped to that block
- **Operators**: Defined in `src/preamble.fn` (e.g., `@` = cons, `@@` = append)
- **Namespaces**: Files start with `namespace` keyword (like `let` without `in` - mutually recursive declarations)
  - **IMPORTANT**: `link` directives must appear inside a `let` block, not at top level
  - Import via `link "<path>.fn" as <name>` (inside `let`)
  - Reference imported components as `name.component`
  - Example:
    ```fn
    let
        link "listutils.fn" as lst;
        result = lst.length([1, 2, 3]);
    in
        result
    ```

### Common Syntax Gotchas
- **Link placement**: `link` statements MUST be inside a `let` block, not standalone
- **Nested blocks**: Nested `let/in` require `{ }` around the `let/in` expression
- **Semicolons in let**: Multiple bindings in `let` are separated by semicolons: `let a = 1; b = 2; in a + b`
- **Variable naming**: Identifiers starting with `fn` followed by a digit (like `fn3`, `fn10`) are rejected by the parser - use different names like `func3`, `f3`
- **Division operator**: `/` will produce rationals, not integer quotient.
- **Print declarations**: `print typename { ... }` is a declaration, not a function call - defines custom printer for a type
- **Exhaustive patterns**: Pattern matches must be exhaustive unless function or switch is declared `unsafe`

### Non-Deterministic Programming (amb)
- **`then`**: Right-associative binary operator - evaluates LHS, but if backtracked returns RHS
- **`back`**: Triggers backtracking to most recent `then`
- Not just try/catch - can backtrack to any chronologically previous `then` in the history of the process
- Example: `fn one_of { ([]) { back } (h @ t) { h then one_of(t) } }`

### Print System
- **Auto-generated**: Print functions created automatically for typedefs (only if no user-defined printer is specified for that type)
- **User-defined**: `print typename(pt, pu, obj)` - takes printer functions for generic types
- **Built-ins**: `puts(string)`, `putc(char)`, `putn(number)`, `putv(value)`
- **Implementation**: Uses currying to specialize generic printers at call sites

### User-Defined Operators
- **Syntax**: `operator "pattern" [optional <associativity>] <precedence> <implementation>`
- **Example**: `operator "_@_" right 90 cons` or `operator "_!" 120 factorial`
- **Patterns**: Use `_` as placeholder for operands in the pattern string
  - Prefix: `"-_"` (underscore after operator)
  - Infix: `"_+_"` (underscores on both sides)
  - Postfix: `"_!"` (underscore before operator)
- **Restrictions**: Can't redefine with same fixity, can't have same op as both infix and postfix
- Defined in `src/preamble.fn` or user code

## Memory Management

**Mark-and-sweep GC with protection stack**:

- Use `PROTECT(var)` macro to shield objects during construction
- `UNPROTECT(save)` restores the protection stack to a previously saved position
- Pattern: `int save = PROTECT(obj); /* allocating code */ UNPROTECT(save);`
- All allocated structures have `Header` with GC metadata
- Generated `mark*()` functions handle recursive marking

**CRITICAL: HashSymbol objects must NEVER be PROTECT'ed**:
- `HashSymbol` objects are **not** GC-managed (no `Header`)
- Symbols are automatically interned in a global symbol table and never freed
- Calling `PROTECT(hashSymbol)` will corrupt the protection stack and cause GC errors
- This includes symbols returned by: `newSymbol()`, `genSym()`, `genSymDollar()`, token types, etc.
- Only protect structures with a `Header` (AST nodes, arrays, hashes, etc.)

## C Coding Conventions

**Generated union constructor functions**:
- `new<Union>_<Variant>(parserInfo, variant)` - Wraps an existing variant in a union
  - Use when you already have the variant object and just need to create the union wrapper
  - Example: `newLamExp_Amb(parserInfo, ambNode)`
- `make<Union>_<Variant>(parserInfo, field1, field2, ...)` - Constructs variant then wraps in union
  - Use when you have the raw field values and need to construct both variant and union
  - Example: `makeLamExp_Amb(parserInfo, exp1, exp2)`
- **Important**: Visitor patterns should use `new*` functions since they already have visited variants

**Type-safe union accessors**:
- `get<Union>_<Variant>(union*)` - Safely extracts variant from union with type checking
  - Example: `LamAmb *amb = getLamExp_Amb(node)`
  - In production builds: compiles to direct field access `node->val.amb`
  - In debug builds: validates the union type matches expected variant
  - **Prefer this over direct field access** (`node->val.amb`) for safety

**ParserInfo access**:
- `CPI(node)` - Macro to access node's parser info
  - Expands to `node->_yy_parser_info`
  - **Always use this macro** instead of direct field access
  - Provides indirection in case the field name or access pattern changes

**Pointer comparisons**:
- **Always use explicit NULL comparisons**: `if (ptr != NULL)` or `if (ptr == NULL)`
- **Never test pointer "truthiness"**: Don't use `if (ptr)` or `if (!ptr)`
- Makes intent clearer and avoids potential confusion with boolean expressions
- Example: Write `if (node->next != NULL)` not `if (node->next)`

**Naming Conventions**:
- Types: `MixedCase` (e.g., `LamExp`, `AstExpression`)
- Functions and Variables: `camelCase` (e.g., `cpsTcLamExp`, `newLamExp_Amb`)
- Avoid snake case, underscores are reseved for special uses (e.g. union discriminators)

## Documentation Style

**Avoid hyperbole and excessive emphasis**:
- Use simple periods instead of exclamation points in technical documentation
- Avoid phrases like "HUGE WIN", "Amazing", "Incredible" - use "significant", "notable", "substantial"
- Avoid emphatic modifiers like "Critical", "Key", "Major" in headings - use plain descriptive headings
- Avoid verdict-style declarations like "Verdict:", "Key insight:", "Key advantage:" - state facts directly
- Remove emoji decorations from section headings (🎉, 💡, ✅, etc.)
- Avoid unnecessary all-caps emphasis (IMPORTANT, MUST, NEVER) except in actual code constants or where technically required
- Minimize bold emphasis on routine statements - reserve for truly important concepts
- Keep tone professional and measured
- State facts directly without emphatic language
- Example: Write "This improves performance." not "This improves performance!"
- Example: Write "Significant code reduction." not "HUGE code reduction!"
- Example: Write "## Generated Visitor Pattern" not "## Critical Discovery: Generated Visitor Pattern"
- Example: Write "Problem/Mitigation" not "Risk/Mitigation" for straightforward issue discussion

## Debugging

**Conditional compilation flags** (define in source to enable):
- `DEBUG_STEP`, `DEBUG_BYTECODE`, `DEBUG_TC`, `DEBUG_LAMBDA_CONVERT`, etc.
- Defined in `src/common.h`
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

## Pratt Parser & Syntactic Extension

**Table-driven parser enabling runtime operator definitions** - The Pratt parser allows F♮ to be syntactically extensible, supporting user-defined operators with custom precedence and associativity. Operators are defined using a unified `operator` keyword with mixfix patterns (e.g., `"_+_"` for infix, `"-_"` for prefix, `"_!"` for postfix) rather than separate `prefix`/`infix`/`postfix` keywords.

**Note**: The Pratt parser implementation (and much of the early memory management and hash table code) is based on Bob Nystrom's excellent book [Crafting Interpreters](https://craftinginterpreters.com/).

### Why Pratt Parsing?

Traditional parser generators (Flex/Bison) compile to fixed runtime parsers, making syntactic extension impossible. F♮ uses a **hand-written Pratt parser** that is:
- **Table-driven**: All precedence/associativity handled by runtime tables (`PrattRecordTable`)
- **Re-entrant**: Can pause parsing, load linked files, resume (required for `link` directive)
- **Scoped**: Operator definitions are scoped to `let/in` blocks and `{ }` nests
- **UTF-8 aware**: Token scanner uses tree structures called "tries" to efficiently recognize Unicode operators (a trie is a tree where each path from root to leaf represents a token, indexed by successive characters)

### Architecture Overview

**Three main components**:

1. **Scanner** (`src/pratt_scanner.c`):
   - Trie-based token recognition (`PrattTrie`)
   - Handles keywords, operators, strings, numbers, characters
   - Maintains input stack (`PrattBufList`) for nested file parsing
   - Produces token stream (`PrattToken`)

2. **Parser** (`src/pratt_parser.c`):
   - Core function: `expressionPrecedence(parser, precedence)`
   - Looks up operators in `PrattRecordTable` by symbol
   - Each `PrattRecord` contains prefix/infix/postfix implementations
   - Parselets are function pointers: `typedef AstExpression *(*PrattParselet)(PrattRecord *, PrattParser *, AstExpression *, PrattToken *)`

3. **Parser State** (`PrattParser`):
   - `rules`: Hash table of operator parsing rules
   - `trie`: Token recognition trie
   - `lexer`: Scanner state
   - `next`: Pointer to parent parser (for scope nesting)

### Precedence & Associativity

**Precedence scaling** (`PRECEDENCE_SCALE = 3`):
- User declares precedence (e.g., `100`)
- Internal precedence: `user_prec * 3`
- Allows +1/-1 adjustments without overlapping: `(1*3+1) < (2*3-1)`

**Associativity** (stored in `PrattRecord`):
- **Left**: RHS parsed with `prec + 1` (tighter binding right)
- **Right**: RHS parsed with `prec - 1` (allows right recursion)
- **None**: RHS parsed with `prec` (disallows chaining)

Example from `src/preamble.fn`:
```fn
operator "_+_" left 10 ADDITION;      // left associative: a + b + c = (a + b) + c
operator "_@_" right 90 cons;         // right associative: a @ b @ c = a @ (b @ c)
operator "_<=>_" none 5 COMPARISON;   // non-associative: can't chain a <=> b <=> c
```

### Operator Definition Syntax

**Syntax**: `operator "pattern" [associativity] precedence implementation`

**Examples**:
```fn
operator "-_" 13 negate;                  // prefix unary
operator "_+_" left 100 addition;         // infix binary left-assoc
operator "_then_" right 2 amb;            // infix binary right-assoc
operator "_?" 120 optional;               // postfix unary (hypothetical)
```

**Macros as operators**:
```fn
macro AND(a, b) { if (a) { b } else { false } }
operator "_and_" left 3 AND;
```
Macro arguments are wrapped in thunks (`fn() { arg }`) and lazily evaluated, providing proper scoping.

### Parser Table (`PrattRecord`)

Each operator symbol maps to a `PrattRecord` containing:

```c
struct PrattRecord {
    HashSymbol *symbol;           // Operator symbol
    PrattParselet prefixOp;      // Prefix parser function (or NULL)
    int prefixPrec;               // Prefix precedence
    AstExpression *prefixImpl;    // User implementation (or NULL for built-ins)
    PrattParselet infixOp;       // Infix parser function (or NULL)
    int infixPrec;                // Infix precedence
    AstExpression *infixImpl;     // User implementation
    PrattParselet postfixOp;     // Postfix parser function (or NULL)
    int postfixPrec;              // Postfix precedence
    AstExpression *postfixImpl;   // User implementation
    PrattAssoc associativity;     // LEFT, RIGHT, or NONE
};
```

**Key parselets**:
- `userPrefix()` - Handles user-defined prefix operators
- `userInfixLeft()`, `userInfixRight()`, `userInfixNone()` - Handle infix with associativity
- `userPostfix()` - Handles user-defined postfix operators
- Built-in parselets: `grouping()`, `list()`, `fn()`, `iff()`, `switchExp()`, etc.

### Scoping Mechanism

**Parser nesting**: Each `PrattParser` has a `next` pointer to parent parser. When entering a new scope:
1. Create new `PrattParser` with `next` pointing to parent
2. Copy parent's `rules` table (copy-on-write semantics)
3. Parse scope body
4. Discard child parser when exiting scope

**Scope entry points**:
- `let ... in ...` blocks
- `{ }` nests
- File boundaries (via `link` directive)
- Namespace imports

**Operator shadowing**: Inner scopes can redefine operators; definitions are forgotten when scope exits.

### Exporting and importing operators

F♮ supports exporting operators from a namespace and importing them into other scopes, while preserving hygiene and lexical scoping.

- Export operators in a namespace:
   - `export operators;` to export all operators defined in that namespace
   - Or export during definition by prepending the keyword `export` to the operator definition:
     - `export operator "_+_" left 100 addition;`
- Import exported operators (after `link "file.fn" as ns;`):
   - `import ns operators;` to import all exported operators
   - Or per-operator: `import ns operator "~_";`, `import ns operator "_plus_";`, `import ns operator "_squared";`

Conflicts: redefining the same symbol+fixity in the same scope is an error; importing a different fixity for an existing symbol is an error. Shadowing is allowed by redefining/importing in an inner `let`.

Hygiene: imported operator applications call a hygienic wrapper qualified to the defining namespace, so free variables resolve to the definition site and cannot be captured by local bindings in the importing scope.

### Known Limitations & Improvement Areas

1. **Restriction: Operator can't be both infix and postfix**
   - Comment in code: "operators can't be both infix and postfix"
   - Reason: `PrattRecord` allows all three fixities but parser may have conflicts
   - Could be relaxed with better disambiguation

2. **Fixity conflicts**: Same symbol with multiple fixities must be disambiguated
   - Current: Simply disallowed
   - Improvement: Context-sensitive parsing based on preceding token

3. (Addressed) Operator export/import from namespaces is implemented as described above. Future improvements: richer conflict diagnostics, and optional re-export controls in aggregate modules.

4. **Precedence granularity**: `PRECEDENCE_SCALE = 3` limits how many operators can fit between declared levels
   - Could be increased if needed

5. **Macro hygiene**: Currently solved by wrapping args in thunks
   - See `docs/OPERATORS.md` for evolution of approach
   - Works but adds runtime overhead (though optimized in simple cases)

6. **Copy-on-write for rules tables**: Not currently implemented
   - Each scope creates full copy of parent's rules
   - Could be optimized with COW semantics

### Integration with Lambda Conversion

After parsing, operator applications are transformed:
- Infix: `a + b` → `addition(a, b)` (function application)
- Prefix: `!a` → `NOT(a)`
- Postfix: `a!` → `factorial(a)`

Macro operators are handled specially:
1. Arguments wrapped in thunks during lambda conversion: `AND(a, b)` → `AND(fn(){a}, fn(){b})`
2. Macro substitution unwraps if thunk just invokes arg: `fn(){a()}` → `a`
3. Free variables in macro body resolved in definition scope (lexical scoping)

See `src/macro_substitution.c` for implementation details.

### Debugging Parser Issues

```bash
# Enable parser debug output
# Uncomment DEBUG_PRATT_PARSER in src/common.h

# Dump AST to see how operators parsed
./bin/fn --dump-ast path/to/file.fn
```

**Watch for**:
- Precedence conflicts (operators binding wrong way)
- Associativity bugs (wrong grouping in chains)
- Scope leakage (operators visible outside their scope)
- Macro expansion issues (arguments evaluated too early/late)

### Key Files

- `src/pratt_parser.c` - Main parser implementation (4300+ lines)
- `src/pratt_scanner.c` - Token scanner with trie-based recognition
- `src/pratt.yaml` - Parser data structures
- `src/preamble.fn` - Required operator definitions (compiler depends on these)
- `docs/PARSING.md` - Original design notes and requirements
- `docs/OPERATORS.md` - Evolution of operator and macro system

### Testing

Operator tests in `tests/fn/`:
- Precedence and associativity combinations
- Scope behavior (shadowing, export)
- Macro hygiene and lazy evaluation

### Future Work

Potential improvements for operator system:
- Allow infix/postfix ambiguity (context-sensitive parsing)
- Copy-on-write for parser rule tables (performance)
- Better precedence conflict detection at definition time

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
# Uncomment DEBUG_TPMC_MATCH in src/common.h
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
# Uncomment DEBUG_ANF in src/common.h

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
- `docs/pettersson92.pdf` - The original Pettersson 1992 paper on pattern matching compilation
- Pratt parsing is unusual - see `src/pratt_parser.c` for infix operator handling
