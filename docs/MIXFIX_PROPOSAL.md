# Mixfix Operators Proposal for F♮

## Overview

Mixfix operators allow multi-part syntax with "holes" for arguments, enabling more natural and domain-specific expression syntax. This proposal extends F♮'s existing operator system to support mixfix notation while maintaining hygiene, scoping, and the table-driven Pratt parsing architecture.

## Examples

### Array/List Indexing

```fn
mixfix 110 "_[_]" index;

// Usage:
let arr = [10, 20, 30, 40] in
    arr[2]  // => 30
```

### If-Then-Else Expression

```fn
mixfix 1 "if_then_else_" ternary;

// Usage:
let x = if true then 42 else 0 in
    assert(x == 42)
```

### For-In-Do Loop

```fn
mixfix 2 "for_in_do_" forEach;

// Usage:
for x in [1, 2, 3] do print(x)
// Desugars to: forEach(fn(x) { print(x) }, [1, 2, 3])
```

### Custom Control Flow

```fn
mixfix 3 "when_do_" conditional;
mixfix 5 "unless_do_" negatedConditional;
mixfix 4 "repeat_times_" repeatN;

when someCondition() do sideEffect();
unless finished do continue();
repeat 10 times expensiveComputation();
```

### Mathematical Notation

```fn
mixfix 100 "_choose_" binomialCoefficient;
mixfix 95 "_mod_" modulo;

// Usage:
n choose k
x mod 7
```

### Pattern Matching Extension

```fn
mixfix 8 "match_with_" patternMatch;

match value with [
    (0) { "zero" }
    (n) { "nonzero" }
]
```

## Syntax Alternatives

### Alternative 1: Explicit Argument Positions (Recommended)

Use underscores `_` to mark argument positions:

```fn
mixfix precedence "_keyword1_keyword2_" implementation;
mixfix 110 "_[_]" index;
mixfix 1 "if_then_else_" ternary;
```

**Pros:**

- Clear visual separation of keywords and argument positions
- Familiar to Agda/Idris users
- Unambiguous parsing
- Natural ASCII representation

**Cons:**

- Requires escaping if `_` is wanted as an operator character
- Slightly verbose

### Alternative 2: Positional Markers with Sigils

Use `$1`, `$2`, etc. for argument positions:

```fn
mixfix 110 "$1[$2]" index;
mixfix 1 "if $1 then $2 else $3" ternary;
```

**Pros:**

- Numbered arguments make arity explicit
- Could support non-linear patterns (e.g., `$1 + $1` for doubling)
- Clear distinction from regular underscores

**Cons:**

- More verbose
- Less familiar syntax
- Numbered arguments might not map cleanly to function parameters

### Alternative 3: Keyword-Only Pattern (No Markers)

Infer argument positions from keyword structure:

```fn
mixfix 110 "[" "]" index;          // binary: arg [ arg ]
mixfix 1 "if" "then" "else" ternary;  // ternary: if arg then arg else arg
```

**Pros:**

- Cleaner syntax, no underscores
- Keywords alone define the structure
- Natural reading

**Cons:**

- Ambiguous: where do arguments go?
- Hard to parse: is `if x then y else z` three keywords or five?
- Doesn't work for operators like `_!` (postfix factorial)

### Alternative 4: Template Syntax with Holes

Use a template string with explicit `{expr}` holes:

```fn
mixfix 110 "{expr}[{expr}]" index;
mixfix 1 "if {expr} then {expr} else {expr}" ternary;
```

**Pros:**

- Very explicit about argument positions
- Could support named arguments later
- Familiar to template users

**Cons:**

- Verbose
- Mixing string-like syntax with code
- Requires escaping braces

### Alternative 5: Prolog-Style with Variables

Use capital letters or variables in the pattern:

```fn
mixfix 110 "X[Y]" index;
mixfix 1 "if X then Y else Z" ternary;
```

**Pros:**

- Declarative style
- Could bind to actual parameter names
- Familiar to logic programming users

**Cons:**

- Conflates pattern and binding
- May confuse with actual variables
- Parsing complexity

### Alternative 6: Split Declaration

Separate the pattern from the implementation signature:

```fn
mixfix 110 "[" "]" fn(array, index) { ... };
mixfix 1 "if" "then" "else" fn(test, consequent, alternative) { ... };
```

**Pros:**

- Parameter names are explicit
- Clear mapping from pattern to parameters
- No special markers needed

**Cons:**

- Redundant information (arity appears twice)
- Harder to see the complete operator at a glance
- More complex parsing

## Recommended Syntax: Alternative 1 (Underscore Markers)

**Rationale:**

- Simple, clear, and unambiguous
- Consistent with existing operator conventions (infix operators are conceptually `_ op _`)
- Easy to parse: underscores delimit keyword segments
- Familiar to functional programming community
- Works for all cases: prefix, infix, postfix, and mixfix

**Full syntax:**

```fn
mixfix [associativity] precedence "pattern_with_underscores_" implementation;
```

Where:

- `associativity` is optional: `left`, `right`, or `none` (defaults to `none`)
- `precedence` is an integer (scaled by `PRECEDENCE_SCALE` internally)
- `pattern_with_underscores_` contains keywords and `_` for argument positions
- `implementation` is a function expression (can be a symbol, anonymous function, or complex expression)

**Examples:**

```fn
// Unary (like postfix)
mixfix 120 "_!" factorial;

// Binary (like infix)
mixfix left 100 "_choose_" binomialCoefficient;

// Ternary
mixfix 1 "if_then_else_" fn(test, conseq, alt) {
    switch (test) {
        (true) { conseq }
        (false) { alt }
    }
};

// Quaternary
mixfix 50 "_[_:_]" fn(seq, start, end) { slice(seq, start, end) };

// Multiple keywords
mixfix 3 "for_in_do_" fn(body, list) {
    map(body, list)
};
```

## Semantics

### Argument Order

Arguments appear **left-to-right** as they occur in the pattern:

```fn
mixfix 100 "_[_:_]" fn(seq, start, end) { ... };
// list[0:5]
// => fn(list, 0, 5)
```

The first `_` becomes the first parameter, second `_` becomes second parameter, etc.

### Precedence and Associativity

Mixfix operators participate in the same precedence hierarchy as prefix/infix/postfix:

```fn
mixfix 110 "_[_]" index;      // Higher precedence
mixfix 10 "_+_" add;           // Lower precedence

arr[i] + arr[j]  // Parsed as: (arr[i]) + (arr[j])
```

#### Associativity for Mixfix Operators

**Key insight:** Associativity is only meaningful for **binary** (arity 2) mixfix operators.

**Syntax:**

```fn
mixfix [associativity] precedence "pattern" implementation;
```

Where `associativity` is optional and can be:

- `left` - left associative
- `right` - right associative  
- `none` - non-associative (default if omitted)

**Associativity behavior:**

- `left`: `a op b op c` → `(a op b) op c`
- `right`: `a op b op c` → `a op (b op c)`
- `none`: `a op b op c` → error (must use explicit parens)

**Examples:**

```fn
// Binary mixfix with left associativity (like +)
mixfix left 100 "_plus_" addition;
// Usage: a plus b plus c  →  (a plus b) plus c

// Binary mixfix with right associativity (like cons)
mixfix right 90 "_cons_" cons;
// Usage: a cons b cons c  →  a cons (b cons c)

// Binary mixfix with no associativity (like comparison)
mixfix none 50 "_cmp_" compare;
// Usage: a cmp b cmp c  →  ERROR: must use parens

// Default is 'none' if not specified
mixfix 110 "_[_]" index;
// Usage: a[i][j]  →  (a[i])[j]  // This actually works!
// Note: This chains because each application returns a value,
// not because of associativity - the second '[' sees result of first
```

#### Associativity Semantics

**For binary operators (arity 2):**

The associativity affects how the parser calls `expressionPrecedence` for the right-hand side:

```c
// Left associative: parse RHS with higher precedence (prec + 1)
rhs = expressionPrecedence(parser, record->mixfixPrec + 1);
// This prevents right recursion: a + b + c
//   First: parses 'a + b' (prec 100)
//   Then: sees second '+' at same prec 100, but RHS needs 101
//   So: second '+' waits, returns (a + b)
//   Finally: parses '(a + b) + c'

// Right associative: parse RHS with same precedence (prec)
rhs = expressionPrecedence(parser, record->mixfixPrec);
// This allows right recursion: a @ b @ c
//   First: parses 'a', sees '@'
//   Then: parses RHS at same prec, which includes 'b @ c'
//   So: gets 'a @ (b @ c)'

// Non-associative: parse RHS with higher precedence (prec + 1)
// BUT also check if next operator is the same and error if so
rhs = expressionPrecedence(parser, record->mixfixPrec + 1);
if (next_op == current_op && same_precedence) {
    error("non-associative operator cannot be chained");
}
```

**For non-binary operators (arity > 2):**

Associativity is **not meaningful** and should be ignored or cause an error:

```fn
// This doesn't make sense - what would it mean to chain?
mixfix left 1 "if_then_else_" ternary;

// Usage: if a then b else c if d then e else f
// Left: (if a then b else c) if d then e else f  ← syntax error!
// Right: if a then b else (c if d then e else f) ← maybe? but weird
```

**Recommendation:** Require associativity to be `none` (or omitted) for operators with arity > 2.

#### Default Associativity

**Question:** What should the default be if associativity is omitted?

**Answer:** `none` (non-associative)

**Rationale:**

1. **Safety:** Prevents accidental mis-parsing of chained expressions
2. **Explicitness:** Forces users to think about whether chaining makes sense
3. **Consistency:** Most mixfix operators won't be chainable anyway
4. **Backward compatibility:** If we later change defaults, `none` is safest starting point

**Examples:**

```fn
// Without associativity specified - defaults to 'none'
mixfix 100 "_mod_" modulo;
// a mod b mod c  →  ERROR: must parenthesize

// With explicit left associativity
mixfix left 100 "_mod_" modulo;  
// a mod b mod c  →  (a mod b) mod c  ✓
```

#### Special Case: Array/List Indexing

Array indexing is an interesting case:

```fn
mixfix 110 "_[_]" index;

// This works even without explicit associativity:
matrix[i][j]

// Why? Because it's not actually chaining the same operator!
// It's: (matrix[i])[j]
// First [i] is called on matrix
// Then [j] is called on the result
// Each is a separate infix operation
```

This is **not** associativity - it's just normal left-to-right parsing where each `[` is an infix operator that can operate on any expression (including the result of a previous indexing).

The precedence is high enough that:

```fn
matrix[i] + matrix[j]  →  (matrix[i]) + (matrix[j])
```

But:

```fn
matrix[i + 1]  →  matrix[(i + 1)]  // + has lower precedence
```

#### Comparison: Mixfix vs. Infix

For binary operators, mixfix with pattern `"_op_"` is **equivalent** to infix `op`:

```fn
// These are functionally identical:
infix left 100 "+" addition;
mixfix left 100 "_+_" addition;

// Both parse: a + b + c  →  (a + b) + c
```

**So why have mixfix for binary operators at all?**

1. **Consistency:** Single syntax for all arities
2. **Multi-character operators:** `"_choose_"` is clearer than infix with a word
3. **Future extensibility:** Could add features specific to mixfix
4. **User choice:** Some users may prefer the explicit `_` markers

**Recommendation:** Keep both. Infix/prefix/postfix are convenient special cases, mixfix is the general mechanism.

### Pattern Recognition and Parselet Triggering

**The critical question: What triggers a mixfix parselet?**

The answer depends on whether the pattern starts with `_` (hole) or a keyword.

#### Case 1: Pattern Starts with Keyword (Prefix-style)

Pattern: `"if_then_else_"`

**Triggering:**

- The scanner sees token `if` in prefix position
- Parser looks up `if` in the `PrattRecord` table
- Finds a mixfix parselet registered for prefix position
- Calls `record->prefixOp(record, parser, NULL, tok)` where `tok` is the `if` token
- The `lhs` argument is `NULL` because there's no left-hand side expression

**Example:**

```fn
mixfix 1 "if_then_else_" ternary;

// In expression:
if x > 5 then "big" else "small"

// Parsing sequence:
// 1. Scanner returns token 'if'
// 2. expressionPrecedence() sees 'if' in prefix position
// 3. Looks up record for 'if', finds mixfix parselet
// 4. Calls userMixfix(record, parser, NULL, 'if')
// 5. Parselet parses: expr(x > 5), keyword 'then', expr("big"), keyword 'else', expr("small")
```

#### Case 2: Pattern Starts with Hole (Infix-style)

Pattern: `"_[_]"` or `"_choose_"`

**Triggering:**

- The scanner sees token `[` (or `choose`) in infix position (after an expression)
- Parser has already parsed the left-hand side expression
- Parser looks up `[` in the `PrattRecord` table
- Finds a mixfix parselet registered for infix position
- Calls `record->infixOp(record, parser, lhs, tok)` where `lhs` is the left expression and `tok` is the `[` token

**Example:**

```fn
mixfix 110 "_[_]" index;

// In expression:
arr[5]

// Parsing sequence:
// 1. Parser sees 'arr', creates AstExpression for it
// 2. Parser checks if next token has infix operator
// 3. Scanner returns token '['
// 4. Looks up record for '[', finds mixfix parselet in infix position
// 5. Calls userMixfix(record, parser, arr_expr, '[')
// 6. Parselet parses: expr(5), then expects ']' (stored in pattern keywords)
```

#### Case 3: Pattern Starts with Hole (Postfix-style)

Pattern: `"_!"` (factorial)

**Note:** While this looks like it could work, this is actually **redundant** with the existing postfix operator system. The recommendation is to use regular `postfix` declarations for this case:

```fn
postfix 120 "!" factorial;  // Preferred
// vs
mixfix 120 "_!" factorial;  // Works but unnecessary
```

#### Registration Strategy

When a mixfix operator is defined, the parser must register it in the appropriate position(s):

```c
// Pattern: "if_then_else_" (starts with keyword)
// Register 'if' as PREFIX operator
record->prefixOp = userMixfix;
record->prefixPrec = precedence * PRECEDENCE_SCALE;

// Pattern: "_[_]" (starts with hole)
// Register '[' as INFIX operator (first keyword after first hole)
record->infixOp = userMixfix;
record->infixPrec = precedence * PRECEDENCE_SCALE;

// Pattern: "_choose_" (starts with hole)
// Register 'choose' as INFIX operator
record->infixOp = userMixfix;
record->infixPrec = precedence * PRECEDENCE_SCALE;
```

#### Pattern Analysis Algorithm

When processing a mixfix definition, the parser must determine:

1. **Does it start with `_`?** → Register first keyword as INFIX
2. **Does it start with a keyword?** → Register that keyword as PREFIX
3. **Store the full pattern** in the record for the parselet to use

```c
void registerMixfixOperator(PrattParser *parser,
                           PrattMixfixPattern *pattern,
                           int precedence,
                           AstExpression *impl)
{
    HashSymbol *triggerKeyword;
    bool isInfix;
    
    if (pattern->starts_with_hole) {
        // Pattern like "_[_]" or "_choose_"
        // First keyword triggers as infix
        triggerKeyword = pattern->keywords[0];
        isInfix = true;
    } else {
        // Pattern like "if_then_else_"
        // First keyword triggers as prefix
        triggerKeyword = pattern->keywords[0];
        isInfix = false;
    }
    
    PrattRecord *record = ensureRecord(parser, triggerKeyword);
    
    if (isInfix) {
        if (record->infixOp != NULL) {
            error("operator already defined as infix");
        }
        record->infixOp = userMixfix;
        record->infixPrec = precedence * PRECEDENCE_SCALE;
    } else {
        if (record->prefixOp != NULL) {
            error("operator already defined as prefix");
        }
        record->prefixOp = userMixfix;
        record->prefixPrec = precedence * PRECEDENCE_SCALE;
    }
    
    // Store pattern and implementation in both cases
    record->mixfixPattern = pattern;
    record->mixfixHygienicFunc = makeMacroName();
    record->mixfixOriginalImpl = impl;
}
```

#### Multi-Keyword Lookup Challenge

**Problem:** How does the parser know that `if` is part of `if_then_else_` and not just a standalone keyword?

**Solution:** The `PrattRecord` stores the full pattern:

```c
struct PrattRecord {
    HashSymbol *symbol;           // 'if' (the trigger)
    // ... other fields ...
    PrattMixfixPattern *mixfixPattern;  // Complete pattern: ["if", "then", "else"]
    // ...
};
```

When `userMixfix()` is called, it has access to the full pattern and knows what keywords to expect next.

#### Example: Full Trigger Sequence

**Code:**

```fn
mixfix 1 "if_then_else_" ternary;

let x = if a > b then a else b in ...
```

**What happens:**

1. **Scanner** produces tokens: `if`, `a`, `>`, `b`, `then`, `a`, `else`, `b`
2. **Parser** in `expressionPrecedence()`:
   - Sees `if` token
   - Looks up `if` in `PrattRecord` table
   - Finds `record->prefixOp = userMixfix`
   - Calls `userMixfix(record, parser, NULL, if_token)`
3. **userMixfix parselet**:
   - Examines `record->mixfixPattern`:
     - `keywords = ["if", "then", "else"]`
     - `arity = 3`
     - `starts_with_hole = false`
   - Already consumed `if` (the trigger keyword)
   - Parses expression 1: `a > b`
   - Expects keyword `then`, consumes it
   - Parses expression 2: `a`
   - Expects keyword `else`, consumes it
   - Parses expression 3: `b`
   - Builds function call: `$mixfix$42(a > b, a, b)`

4. **Result:** `AstFunCall` with three arguments

### Hygiene

Like existing operators, mixfix operators are hygienic:

```fn
let
    fn myIndex(arr, i) { ... }
    mixfix 110 "_[_]" myIndex;
in {
    let
        fn myIndex(x, y) { 999 }  // Shadow the function name
    in
        arr[5]  // Still calls the original myIndex via hygienic wrapper
}
```

The parser generates a unique hygienic function name (e.g., `$mixfix$42`) that captures the implementation at definition time.

## Implementation Strategy

### 1. Scanner Changes

**Keyword Recognition:**

- The scanner needs to recognize mixfix keyword sequences
- First keyword triggers a mixfix parselet lookup
- Subsequent keywords are consumed during parsing

**Trie Extension:**

- Current trie structure can handle this with minimal changes
- Store mixfix patterns in the `PrattRecord` structure
- First keyword is the lookup key

### 2. AST Changes (`ast.yaml`)

Add a new definition type for mixfix operators:

```yaml
AstMixfixPattern:
    meta:
        brief: Mixfix operator pattern
        description: Represents the keyword structure of a mixfix operator
    data:
        keywords: AstStringArray      # ["if", "then", "else"]
        arity: int                     # Number of argument positions (3)

AstDefinition:
    # Existing variants...
    mixfixop: AstMixfixOp

AstMixfixOp:
    meta:
        brief: Mixfix operator definition
    data:
        pattern: AstMixfixPattern
        precedence: int
        associativity: PrattAssoc
        implementation: AstExpression
```

### 3. PrattRecord Extension (`pratt.yaml`)

Extend `PrattRecord` to support mixfix:

```yaml
PrattRecord:
    data:
        symbol: HashSymbol                  # First keyword
        # ... existing prefix/infix/postfix fields ...
        mixfixOp: PrattParselet            # Mixfix parselet function
        mixfixPrec: int                     # Precedence
        mixfixPattern: PrattMixfixPattern  # Keyword sequence
        mixfixHygienicFunc: HashSymbol     # Generated function name
        mixfixExport: bool=false

PrattMixfixPattern:
    meta:
        brief: Pattern structure for mixfix operators
    data:
        keywords: PrattStringArray    # e.g., ["if", "then", "else"]
        arity: int                     # Number of holes (3)
```

### 4. Parser Implementation

**New parselet function:**

```c
static AstExpression *userMixfix(PrattRecord *record,
                                 PrattParser *parser,
                                 AstExpression *first_arg,
                                 PrattToken *tok)
{
    // first_arg is the expression before the first keyword (or NULL for prefix-style)
    // tok is the first keyword token
    
    int arity = record->mixfixPattern->arity;
    AstExpression **args = malloc(arity * sizeof(AstExpression *));
    int arg_index = 0;
    
    // If the pattern starts with _, we already have first_arg
    if (first_arg != NULL) {
        args[arg_index++] = first_arg;
    }
    
    // Parse remaining arguments and keywords
    for (int i = 1; i < record->mixfixPattern->keywords->size; i++) {
        // Parse next argument expression
        args[arg_index++] = expressionPrecedence(parser, record->mixfixPrec + 1);
        
        // Consume next keyword
        HashSymbol *expected = record->mixfixPattern->keywords->entries[i];
        if (!match(parser, expected)) {
            parserError(parser, "expected '%s' in mixfix operator", expected->name);
        }
    }
    
    // Parse final argument (if pattern ends with _)
    if (arg_index < arity) {
        args[arg_index++] = expressionPrecedence(parser, record->mixfixPrec + 1);
    }
    
    // Build function call: hygienicFunc(arg1, arg2, ..., argN)
    return makeHygienicMixfixCall(record->mixfixHygienicFunc, args, arity);
}
```

**Pattern parsing:**

```c
// In the parser for mixfix definitions
static PrattMixfixPattern *parseMixfixPattern(PrattParser *parser, char *pattern_str) {
    // Parse "_[_:_]" into:
    // keywords: ["[", ":"]
    // arity: 3
    // positions: [0, 1, 2] (before "[", between "[" and ":", after ":")
    
    PrattStringArray *keywords = newPrattStringArray();
    int arity = 0;
    bool last_was_hole = false;
    
    // Split on '_' and collect keywords
    // Count holes (underscores)
    
    return newPrattMixfixPattern(keywords, arity);
}
```

### 5. Hygiene and Definition Generation

Similar to existing operators, generate a wrapper function:

```c
static AstDefinition *makeHygienicMixfixOperatorDef(ParserInfo I,
                                                     HashSymbol *hygienicName,
                                                     AstExpression *impl,
                                                     int arity)
{
    // Generate: fn hygienicName(arg1, arg2, ..., argN) { impl(arg1, arg2, ..., argN) }
    // Or if impl is a bare symbol: just create alias
    
    // Build parameter list
    AstFargList *params = makeParameterList(arity);  // p$1, p$2, ...
    
    // Build call to impl
    AstExpression *call = makeImplementationCall(impl, params);
    
    // Wrap in function definition
    return makeFunctionDef(I, hygienicName, params, call);
}
```

### 6. Export/Import

Mixfix operators can be exported and imported like other operators:

```fn
// In library.fn
namespace
    mixfix 110 "_[_]" index;
    export mixfix "_[_]";
in ...

// In client.fn
let
    link "library.fn" as lib;
    import lib mixfix "_[_]";
in
    arr[5]  // Uses imported operator
```

## Edge Cases and Constraints

### 1. Pattern Restrictions

**Cannot start and end with keyword:**

```fn
mixfix 100 "foo_bar" fn(x) { ... }  // ERROR: no leading hole
```

Must have at least one hole. Use prefix/postfix/infix for single-hole operators.

**Minimum arity:**

- Unary: use prefix or postfix operators
- Binary+: use mixfix

### 2. Ambiguity Resolution

**Overlapping patterns:**

```fn
mixfix 100 "if_then_else_" ternary;
mixfix 100 "if_then_" binary;  // Conflict!
```

The parser should reject patterns where one is a prefix of another with the same first keyword.

**Keywords vs. operators:**

```fn
infix 100 "then" thenOp;
mixfix 1 "if_then_else_" ternary;  // "then" is both keyword and operator!
```

This is allowed but may be confusing. The context determines which is used.

### 3. Precedence Interactions

Mixfix operators follow standard precedence rules:

```fn
mixfix 50 "_[_]" index;
infix left 100 "+" add;

a + b[i]  // => a + (b[i])   (index binds tighter)
a[i] + b  // => (a[i]) + b
```

### 4. Whitespace Sensitivity

**Question:** Should whitespace matter?

```fn
mixfix 100 "_mod_" modulo;

x mod y   // clearly mixfix
xmody     // identifer? or parse error?
x mody    // identifier 'x', identifier 'mody'?
```

**Recommendation:** Require whitespace or clear token boundaries around keywords (same as current behavior for operators).

### 5. Partial Application

**Can you partially apply a mixfix operator?**

```fn
mixfix 110 "_[_]" index;

let getFirst = index(_, 0);  // Partial application?
```

This is tricky. Initial implementation should **not** support partial application of mixfix operators. They always require all arguments.

## Migration Path

### Phase 1: Core Implementation

- Basic mixfix pattern parsing
- Hygiene and definition generation
- Simple precedence handling
- No associativity (all `none`)

### Phase 2: Full Features

- Left/right associativity for mixfix
- Export/import support
- Better error messages for pattern conflicts

### Phase 3: Advanced Features (Future)

- Partial application
- Non-linear patterns (same argument appears multiple times)
- Custom delimiter pairs (related but separate feature)

## Examples from Other Languages

### Agda

```agda
if_then_else_ : Bool → A → A → A
if true  then x else y = x
if false then x else y = y
```

### Idris

```idris
infix 5 `div`
mixfix [_] : Vect n a → Fin n → a
```

### Fortress (defunct but interesting)

```
opr ⟦ i ⟧(self, i:ℤ): E = ...
```

## Open Questions

1. **Should we allow zero-argument mixfix?** E.g., `mixfix 10 "begin_end" block`
   - Probably not - use a regular function call

2. **Should keywords be reserved within a scope?**
   - No - keywords are only special in context of the mixfix pattern

3. **How do we handle Unicode in patterns?**
   - Same as existing operators - full UTF-8 support

4. **Should we support mixfix with variadic arguments?**
   - Not in initial implementation
   - Could consider later: `mixfix 5 "sum[_,_,...,_]" sumAll`

5. **Pretty-printing?**
   - Should the AST printer show mixfix in original syntax or desugared form?
   - Recommendation: Show original syntax for readability

## Summary

**Recommended approach:**

- Use underscore-based pattern syntax: `mixfix precedence "_pattern_with_holes_" impl`
- Implement as an extension to the existing operator system
- Maintain hygiene via generated wrapper functions
- Support export/import like other operators
- Start with basic implementation (no associativity, no partial application)
- Extend with advanced features in later phases

This design:

- ✓ Fits naturally into existing Pratt parser architecture
- ✓ Maintains hygiene and scoping properties
- ✓ Provides powerful syntactic abstraction
- ✓ Stays true to F♮'s functional programming philosophy
- ✓ Can be implemented incrementally
