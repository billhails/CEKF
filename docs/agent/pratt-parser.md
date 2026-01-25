# Pratt Parser & Syntactic Extension

Table-driven parser enabling runtime operator definitions. The Pratt parser allows F♮ to be syntactically extensible, supporting user-defined operators with custom precedence and associativity.

Based on Bob Nystrom's [Crafting Interpreters](https://craftinginterpreters.com/).

## Why Pratt Parsing?

Traditional parser generators (Flex/Bison) compile to fixed runtime parsers, making syntactic extension impossible. F♮ uses a hand-written Pratt parser that is:

- **Table-driven**: All precedence/associativity handled by runtime tables (`PrattRecordTable`)
- **Re-entrant**: Can pause parsing, load linked files, resume (required for `link` directive)
- **Scoped**: Operator definitions are scoped to `let/in` blocks and `{ }` nests
- **UTF-8 aware**: Token scanner uses tries to efficiently recognize Unicode operators

## Architecture Overview

### Scanner (`src/pratt_scanner.c`)

- Trie-based token recognition (`PrattTrie`)
- Handles keywords, operators, strings, numbers, characters
- Maintains input stack (`PrattBufList`) for nested file parsing
- Produces token stream (`PrattToken`)
- Operates on a `wchar_t` array of Unicode code points (UTF-32)

### Parser (`src/pratt_parser.c`)

- Core function: `expressionPrecedence(parser, precedence)`
- Looks up operators in `PrattRecordTable` by symbol
- Each `PrattRecord` contains prefix/infix/postfix implementations
- Parselets are function pointers: `typedef AstExpression *(*PrattParselet)(PrattRecord *, PrattParser *, AstExpression *, PrattToken *)`

### Parser State (`PrattParser`)

- `rules`: Hash table of operator parsing rules
- `trie`: Token recognition trie
- `lexer`: Scanner state
- `next`: Pointer to parent parser (for scope nesting)

## Precedence & Associativity

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

## Operator Definition Syntax

**Syntax**: `operator "pattern" [associativity] precedence implementation`

**Patterns**: Use `_` as placeholder for operands:

- Prefix: `"-_"` (underscore after operator)
- Infix: `"_+_"` (underscores on both sides)
- Postfix: `"_!"` (underscore before operator)

**Examples**:

```fn
operator "-_" 13 negate;                  // prefix unary
operator "_+_" left 100 addition;         // infix binary left-assoc
operator "_then_" right 2 amb;            // infix binary right-assoc
operator "_?" 120 optional;               // postfix unary
```

**Macros as operators**:

```fn
macro AND(a, b) { if (a) { b } else { false } }
operator "_and_" left 3 AND;
```

Macro arguments are wrapped in thunks (`fn() { arg }`) and lazily evaluated, providing proper scoping.

## Parser Table (`PrattRecord`)

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

## Scoping Mechanism

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

## Exporting and Importing Operators

F♮ supports exporting operators from a namespace and importing them into other scopes, while preserving hygiene and lexical scoping.

**Export operators in a namespace**:

- `export operators;` to export all operators defined in that namespace
- Or export during definition: `export operator "_+_" left 100 addition;`

**Import exported operators** (after `link "file.fn" as ns;`):

- `import ns operators;` to import all exported operators
- Or per-operator: `import ns operator "~_";`, `import ns operator "_plus_";`

**Conflicts**: Redefining the same symbol+fixity in the same scope is an error; importing a different fixity for an existing symbol is an error. Shadowing is allowed by redefining/importing in an inner `let`.

**Hygiene**: Imported operator applications call a hygienic wrapper qualified to the defining namespace, so free variables resolve to the definition site.

## Integration with Lambda Conversion

After parsing, operator applications are transformed:

- Infix: `a + b` → `addition(a, b)` (function application)
- Prefix: `!a` → `NOT(a)`
- Postfix: `a!` → `factorial(a)`

Macro operators are handled specially:

1. Arguments wrapped in thunks during lambda conversion: `AND(a, b)` → `AND(fn(){a}, fn(){b})`
2. Macro substitution unwraps if thunk just invokes arg: `fn(){a()}` → `a`
3. Free variables in macro body resolved in definition scope (lexical scoping)

See `src/macro_substitution.c` for implementation details.

## Known Limitations

1. **Operator can't be both infix and postfix** - Could be relaxed with better disambiguation
2. **Fixity conflicts** - Same symbol with multiple fixities simply disallowed
3. **Precedence granularity** - `PRECEDENCE_SCALE = 3` limits operators between declared levels
4. **Macro hygiene** - Solved by wrapping args in thunks; works but adds runtime overhead
5. **Copy-on-write for rules tables** - Not currently implemented; each scope creates full copy

## Debugging Parser Issues

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

## Key Files

- `src/pratt_parser.c` - Main parser implementation (4300+ lines)
- `src/pratt_scanner.c` - Token scanner with trie-based recognition
- `src/pratt.yaml` - Parser data structures
- `src/preamble.fn` - Required operator definitions (compiler depends on these)
- `docs/PARSING.md` - Original design notes and requirements
- `docs/OPERATORS.md` - Evolution of operator and macro system
