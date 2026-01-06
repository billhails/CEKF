# Mixfix Operators: Implementation Examples

This document provides concrete code examples showing how mixfix operators would work in F♮, both from the user's perspective and implementation details.

## User-Facing Examples

### Example 1: Array Indexing

**Definition:**

```fn
namespace

fn index {
    ([], _) { error("index out of bounds") }
    (h @ _, 0) { h }
    (_ @ t, n) { index(t, n - 1) }
}

mixfix 110 "_[_]" index;

export mixfix "_[_]";
```

**Usage:**

```fn
let
    link "array_lib.fn" as arr;
    import arr mixfix "_[_]";
    
    myList = [10, 20, 30, 40, 50];
in {
    assert(myList[0] == 10);
    assert(myList[3] == 40);
    // myList[10]  // runtime error: index out of bounds
}
```

**Desugared AST (internal):**

```fn
AstFunCall(
    func: AstAnnotatedSymbol($mixfix$42, index),
    args: [
        AstSymbol(myList),
        AstNumber(0)
    ]
)
```

### Example 2: Ternary Conditional

**Definition:**

```fn
fn ternary {
    (true, conseq, _) { conseq }
    (false, _, alt) { alt }
}

mixfix 1 "if_then_else_" ternary;
```

**Usage:**

```fn
let
    x = 5;
    result = if x > 3 then "big" else "small";
in
    assert(result == "big")
```

**Comparison with built-in if:**

```fn
// Mixfix version (expression)
let x = if test then a else b in ...

// Built-in if (statement-like, but still expression)
let x = if (test) { a } else { b } in ...
```

The mixfix version could coexist with the built-in `if` since they have different syntax.

### Example 3: List Slicing

**Definition:**

```fn
fn slice {
    (lst, start, end) {
        lst |> drop(start) |> take(end - start)
    }
}

mixfix 50 "_[_:_]" slice;
```

**Usage:**

```fn
let
    numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
in {
    assert(numbers[2:5] == [2, 3, 4]);
    assert(numbers[0:3] == [0, 1, 2]);
}
```

### Example 4: For-Each Loop (Monadic Style)

**Definition:**

```fn
fn forEach(body, list) {
    map(body, list)
}

mixfix 3 "for_in_do_" forEach;
```

**Usage:**

```fn
let
    sum = here(fn(cc) {
        let total = 0 in {
            for (fn(x) { cc(total + x) }) in [1, 2, 3, 4, 5] do total;
            cc(total)
        }
    });
in
    assert(sum == 15)
```

Actually, this shows the limitation - without mutable state or better control flow, `for_in_do_` is just sugar for `map`. A better use case:

```fn
// Side-effect version
for print in [1, 2, 3, 4, 5] do ();
// Desugars to: map(print, [1, 2, 3, 4, 5])
// Prints: 1\n2\n3\n4\n5\n
```

### Example 5: Mathematical Notation

**Definition:**

```fn
fn factorial {
    (0) { 1 }
    (n) { n * factorial(n - 1) }
}

fn choose(n, k) {
    factorial(n) / (factorial(k) * factorial(n - k))
}

mixfix 100 "_choose_" choose;
```

**Usage:**

```fn
let
    ways = 10 choose 3;
in
    assert(ways == 120)
```

### Example 6: Conditional Execution (When/Unless)

**Definition:**

```fn
fn when(cond, action) {
    if (cond) { action } else { () }
}

fn unless(cond, action) {
    if (cond) { () } else { action }
}

mixfix 3 "when_do_" when;
mixfix 3 "unless_do_" unless;
```

**Usage:**

```fn
let
    debug = true;
    count = 0;
in {
    when debug do puts("Debug mode enabled");
    unless (count > 0) do puts("No items");
}
```

### Example 7: Repeat Construct

**Definition:**

```fn
fn repeatN {
    (0, _, acc) { acc }
    (n, action, acc) { repeatN(n - 1, action, action(acc)) }
}

fn repeat(action, n) {
    repeatN(n, action, ())
}

mixfix 4 "repeat_times_" repeat;
```

**Usage:**

```fn
let
    counter = here(fn(cc) {
        let i = 0 in {
            repeat (fn(_) { cc(i + 1) }) times 10;
            cc(i)
        }
    });
in
    assert(counter == 10)
```

## Implementation Details

### How Mixfix Parselets Are Triggered

Understanding the trigger mechanism is crucial to implementing mixfix operators correctly.

#### Trigger Mechanism Overview

The Pratt parser uses **position-based lookup**: when it sees a token, it checks what parselets are registered for that token in the current parsing position (prefix, infix, or postfix).

For mixfix operators, the **first keyword in the pattern** acts as the trigger, and the pattern structure determines whether it's registered as a prefix or infix operator.

#### Pattern Analysis and Registration

When you write:

```fn
mixfix 110 "_[_]" index;
```

The parser analyzes the pattern `"_[_]"`:

1. **Starts with `_`** (hole) → This is an **infix-style** operator
2. **First keyword** after the leading hole is `[`
3. **Register** `[` as an **INFIX** operator in the `PrattRecord` table
4. **Store** the full pattern `["["]` with arity 2

When you write:

```fn
mixfix 1 "if_then_else_" ternary;
```

The parser analyzes the pattern `"if_then_else_"`:

1. **Starts with keyword** `if` → This is a **prefix-style** operator
2. **First keyword** is `if`
3. **Register** `if` as a **PREFIX** operator in the `PrattRecord` table
4. **Store** the full pattern `["if", "then", "else"]` with arity 3

#### Detailed Trigger Examples

##### Example 1: Array Indexing `"_[_]"`

**Pattern structure:**

- Starts with: `_` (hole)
- Keywords: `["["]`
- Arity: 2
- Trigger keyword: `[` (registered as INFIX)

**What happens during parsing of `arr[5]`:**

Step 1: Parser in expressionPrecedence(parser, 0)

- Calls next token → gets 'arr'
- Looks up 'arr' → no prefix operator, must be identifier
- Creates: AstExpression(Symbol(arr))
- Stores as lhs

Step 2: Parser checks for infix operator

- Peeks at next token → gets '['
- Looks up '[' in PrattRecord table
- Finds: record->infixOp = userMixfix
- Precedence check: 110 * 3 = 330 > current minimum
- Calls: userMixfix(record, parser, lhs, '[' token)

Step 3: Inside userMixfix parselet

- record->mixfixPattern->starts_with_hole = true
- record->mixfixPattern->keywords = ["["]
- record->mixfixPattern->arity = 2
- Already have first argument in lhs (arr expression)
- Parse next argument: expressionPrecedence(parser, 331)
    → gets 5
- No more keywords in pattern (only one '[')
- But pattern ends with hole, so expect ']'
- Consume ']' token
- Build call: $mixfix$42(arr, 5)

There's an issue here - where does the closing `]` come from?

**Answer:** The pattern `"_[_]"` should probably be stored as:

- Keywords: `["[", "]"]`
- Holes: positions [0, 1]

Or we need a different representation. Let me reconsider...

Actually, the pattern `"_[_]"` means:

- Hole 1, then `[`, then hole 2
- But there's no closing delimiter specified!

This suggests we need to handle **paired delimiters** specially. Alternative approaches:

###### Approach A: Require explicit closing in pattern

```fn
mixfix 110 "_[_]" index;  // Pattern literally includes ']'
```

Pattern stored as: `["[", "]"]` with holes at positions [0, 1]

###### Approach B: Paired delimiters are inferred

```fn
mixfix 110 "_[_" index;    // Just opening bracket
```

Parser automatically looks for matching `]`

**Recommendation:** Use Approach A - be explicit about all keywords.

So `"_[_]"` is actually parsed as:

- Character `_` → hole position 0
- Character `[` → keyword 1
- Character `_` → hole position 1  
- Character `]` → keyword 2

Keywords array: `["[", "]"]`
Arity: 2

##### Example 2: If-Then-Else `"if_then_else_"`

###### Pattern structure

- Starts with: `if` (keyword)
- Keywords: `["if", "then", "else"]`
- Arity: 3
- Trigger keyword: `if` (registered as PREFIX)

###### What happens during parsing of `if x > 5 then "big" else "small"`

- Step 1: Parser in expressionPrecedence(parser, 0)
  - Calls next token → gets 'if'
  - Looks up 'if' in PrattRecord table
  - Finds: record->prefixOp = userMixfix
  - Calls: userMixfix(record, parser, NULL, 'if' token)
                                     ^^^^
                                     No lhs for prefix!

- Step 2: Inside userMixfix parselet
  - record->mixfixPattern->starts_with_hole = false
  - record->mixfixPattern->keywords = ["if", "then", "else"]
  - record->mixfixPattern->arity = 3
  - lhs is NULL (prefix position)
  - Already consumed trigger keyword 'if'
  
  - Parse argument 1: expressionPrecedence(parser, 4)  // (prec 1 * 3) + 1
    → gets: x > 5
  
  - Expect keyword 'then' (keywords[1])
  - Consume 'then' token
  
  - Parse argument 2: expressionPrecedence(parser, 4)
    → gets: "big"
  
  - Expect keyword 'else' (keywords[2])
  - Consume 'else' token
  
  - Parse argument 3: expressionPrecedence(parser, 4)
    → gets: "small"
  
  - Build call: $mixfix$42(x > 5, "big", "small")

##### Example 3: Choose Operator `"_choose_"`

###### Pattern structure

- Starts with: `_` (hole)
- Keywords: `["choose"]`
- Arity: 2
- Trigger keyword: `choose` (registered as INFIX)

###### What happens during parsing of `10 choose 3`

- Step 1: Parser parses '10'
  - Creates: AstExpression(Number(10))
  - Stores as lhs

- Step 2: Parser checks for infix
  - Peeks at next token → gets 'choose'
  - Looks up 'choose' in PrattRecord table
  - Finds: record->infixOp = userMixfix
  - Calls: userMixfix(record, parser, lhs, 'choose' token)

- Step 3: Inside userMixfix parselet
  - record->mixfixPattern->starts_with_hole = true
  - record->mixfixPattern->keywords = ["choose"]
  - record->mixfixPattern->arity = 2
  - Already have first argument in lhs (10)
  - Already consumed 'choose' keyword
  - Pattern ends with '_', so parse final argument
  - Parse argument 2: expressionPrecedence(parser, 301)
    → gets: 3
  - Build call: $mixfix$42(10, 3)

#### Pattern Representations in Code

The pattern string needs to be parsed into a structured representation:

```c
typedef struct PrattMixfixPattern {
    char **keywords;           // Array of keyword strings
    int num_keywords;          // Number of keywords
    int arity;                 // Number of arguments (holes)
    bool starts_with_hole;     // true if pattern starts with _
    bool ends_with_hole;       // true if pattern ends with _
} PrattMixfixPattern;
```

**Examples:**

Pattern `"_[_]"`:

```c
{
    keywords: ["[", "]"],
    num_keywords: 2,
    arity: 2,
    starts_with_hole: true,
    ends_with_hole: false  // ends with ']' keyword
}
```

Pattern `"if_then_else_"`:

```c
{
    keywords: ["if", "then", "else"],
    num_keywords: 3,
    arity: 3,
    starts_with_hole: false,
    ends_with_hole: true
}
```

Pattern `"_choose_"`:

```c
{
    keywords: ["choose"],
    num_keywords: 1,
    arity: 2,
    starts_with_hole: true,
    ends_with_hole: true
}
```

#### Token Stream Processing

The mixfix parselet must carefully manage the token stream:

```c
static AstExpression *userMixfix(PrattRecord *record,
                                 PrattParser *parser,
                                 AstExpression *lhs,
                                 PrattToken *tok)
{
    PrattMixfixPattern *pattern = record->mixfixPattern;
    AstExpression **args = malloc(pattern->arity * sizeof(AstExpression *));
    int arg_idx = 0;
    int kw_idx = 0;
    
    // The trigger keyword (tok) is already consumed by the parser
    // It's the first keyword in the pattern
    
    if (pattern->starts_with_hole) {
        // Pattern like "_[_]" or "_choose_"
        // lhs contains the first argument
        args[arg_idx++] = lhs;
        // tok is keywords[0], already consumed
        kw_idx = 1;  // Start from second keyword
    } else {
        // Pattern like "if_then_else_"
        // tok is keywords[0] (the 'if'), already consumed
        kw_idx = 1;  // Start from second keyword
    }
    
    // Now iterate through remaining keywords and arguments
    while (kw_idx < pattern->num_keywords || arg_idx < pattern->arity) {
        // Parse next argument if we need one
        if (arg_idx < pattern->arity) {
            if (kw_idx >= pattern->num_keywords || 
                arg_idx == 0 ||  // Always parse after first keyword
                !pattern->ends_with_hole) {
                
                args[arg_idx++] = expressionPrecedence(parser, 
                                                      record->mixfixPrec + 1);
            }
        }
        
        // Consume next keyword if there is one
        if (kw_idx < pattern->num_keywords) {
            HashSymbol *expected_kw = symbolFromString(pattern->keywords[kw_idx]);
            if (!match(parser, expected_kw)) {
                parserError(parser, 
                           "expected '%s' in mixfix operator", 
                           pattern->keywords[kw_idx]);
                return errorExpression(tok->I);
            }
            kw_idx++;
        }
    }
    
    // Build the function call
    return buildMixfixCall(record, args, pattern->arity, tok->I);
}
```

### Pattern Parsing

**Input string:** `"_[_:_]"`

**Parsing algorithm:**

```c
typedef struct {
    char **keywords;      // ["[", ":"]
    int arity;            // 3
    bool starts_with_hole; // true
    bool ends_with_hole;   // true
} MixfixPattern;

MixfixPattern *parse_mixfix_pattern(const char *pattern) {
    // State machine:
    // - Split on '_'
    // - Collect keywords between underscores
    // - Count underscores for arity
    // - Check first/last character
    
    MixfixPattern *result = malloc(sizeof(MixfixPattern));
    result->keywords = NULL;
    result->arity = 0;
    int keyword_count = 0;
    
    const char *p = pattern;
    bool in_keyword = false;
    char keyword_buf[256];
    int keyword_len = 0;
    
    result->starts_with_hole = (p[0] == '_');
    
    while (*p) {
        if (*p == '_') {
            result->arity++;
            if (in_keyword && keyword_len > 0) {
                // Save collected keyword
                keyword_buf[keyword_len] = '\0';
                result->keywords = realloc(result->keywords, 
                                          (keyword_count + 1) * sizeof(char *));
                result->keywords[keyword_count++] = strdup(keyword_buf);
                keyword_len = 0;
            }
            in_keyword = false;
        } else {
            in_keyword = true;
            keyword_buf[keyword_len++] = *p;
        }
        p++;
    }
    
    result->ends_with_hole = (*(p-1) == '_');
    
    return result;
}
```

**Example outputs:**

- `"_[_]"` → keywords: `["["]`, arity: 2, starts: true, ends: true
- `"if_then_else_"` → keywords: `["if", "then", "else"]`, arity: 3, starts: false, ends: true
- `"_!"` → keywords: `["!"]`, arity: 1, starts: true, ends: false

### Parselet Implementation

```c
static AstExpression *userMixfix(PrattRecord *record,
                                 PrattParser *parser,
                                 AstExpression *lhs,
                                 PrattToken *tok)
{
    ENTER(userMixfix);
    
    PrattMixfixPattern *pattern = record->mixfixPattern;
    int arity = pattern->arity;
    
    AstExpression **args = malloc(arity * sizeof(AstExpression *));
    int save = PROTECT(args);  // Custom protection for array
    int arg_idx = 0;
    
    // If pattern starts with a keyword, lhs is NULL
    // If pattern starts with _, lhs is the first argument
    if (pattern->starts_with_hole) {
        if (lhs == NULL) {
            parserError(parser, "mixfix operator %s requires leading argument",
                       record->symbol->name);
            UNPROTECT(save);
            return errorExpression(TOKPI(tok));
        }
        args[arg_idx++] = lhs;
    }
    
    // Iterate through keywords and arguments
    for (int kw_idx = 0; kw_idx < pattern->num_keywords; kw_idx++) {
        // Parse next argument (before or after keyword depending on position)
        if (arg_idx < arity) {
            args[arg_idx++] = expressionPrecedence(parser, record->mixfixPrec + 1);
            PROTECT(args[arg_idx - 1]);
        }
        
        // If not the last keyword, consume it
        if (kw_idx < pattern->num_keywords - 1) {
            HashSymbol *expected_kw = pattern->keywords[kw_idx + 1];
            if (!match(parser, expected_kw)) {
                parserError(parser, "expected '%s' in mixfix operator %s",
                           expected_kw->name, record->symbol->name);
                UNPROTECT(save);
                return errorExpression(TOKPI(tok));
            }
        }
    }
    
    // Parse final argument if pattern ends with _
    if (pattern->ends_with_hole && arg_idx < arity) {
        args[arg_idx++] = expressionPrecedence(parser, record->mixfixPrec + 1);
        PROTECT(args[arg_idx - 1]);
    }
    
    // Build argument list in reverse (for cons list)
    AstExpressions *arg_list = NULL;
    for (int i = arity - 1; i >= 0; i--) {
        AstExpressions *cell = newAstExpressions(CPI(args[i]), args[i], arg_list);
        PROTECT(cell);
        arg_list = cell;
    }
    
    // Create annotated symbol with hygienic wrapper
    AstAnnotatedSymbol *annotated = newAstAnnotatedSymbol(
        TOKPI(tok),
        record->mixfixHygienicFunc,
        record->mixfixOriginalImpl
    );
    PROTECT(annotated);
    
    AstExpression *func = newAstExpression_AnnotatedSymbol(TOKPI(tok), annotated);
    PROTECT(func);
    
    // Handle namespace imports
    if (record->importNsRef >= 0) {
        AstLookup *lup = newAstLookup(TOKPI(tok), 
                                      record->importNsRef,
                                      record->importNsSymbol, 
                                      func);
        PROTECT(lup);
        func = newAstExpression_Lookup(TOKPI(tok), lup);
        PROTECT(func);
    }
    
    // Create function call
    AstFunCall *call = newAstFunCall(TOKPI(tok), func, arg_list);
    PROTECT(call);
    
    AstExpression *result = newAstExpression_FunCall(CPI(call), call);
    
    LEAVE(userMixfix);
    UNPROTECT(save);
    return result;
}
```

### Hygienic Wrapper Generation

For a mixfix operator with arity N:

```c
static AstDefinition *makeHygienicMixfixDef(ParserInfo I,
                                            HashSymbol *hygienicName,
                                            AstExpression *impl,
                                            int arity)
{
    // Generate parameter symbols: p$1, p$2, ..., p$N
    AstFargList *params = NULL;
    for (int i = arity - 1; i >= 0; i--) {
        char param_name[32];
        sprintf(param_name, "p$%d", i + 1);
        HashSymbol *param_sym = newSymbol(param_name);
        
        AstFarg *farg = newAstFarg_Symbol(I, param_sym);
        int save = PROTECT(farg);
        params = newAstFargList(I, farg, params);
        UNPROTECT(save);
    }
    
    // If impl is a bare symbol, optimize to direct call
    if (impl->type == AST_EXPRESSION_TYPE_SYMBOL) {
        HashSymbol *impl_sym = impl->val.symbol;
        
        // Build argument expressions
        AstExpressions *args = NULL;
        for (int i = arity - 1; i >= 0; i--) {
            char param_name[32];
            sprintf(param_name, "p$%d", i + 1);
            HashSymbol *param_sym = newSymbol(param_name);
            
            AstExpression *arg = newAstExpression_Symbol(I, param_sym);
            int save = PROTECT(arg);
            args = newAstExpressions(I, arg, args);
            UNPROTECT(save);
        }
        
        // Build call: impl_sym(p$1, p$2, ..., p$N)
        AstExpression *impl_func = newAstExpression_Symbol(I, impl_sym);
        int save = PROTECT(impl_func);
        AstFunCall *call = newAstFunCall(I, impl_func, args);
        PROTECT(call);
        AstExpression *body = newAstExpression_FunCall(I, call);
        PROTECT(body);
        
        // Wrap in function
        AstAltArgs *alt_args = newAstAltArgs(I, params);
        PROTECT(alt_args);
        AstNest *nest = newAstNest(I, NULL, newAstExpressions(I, body, NULL));
        PROTECT(nest);
        AstAltFunction *alt = newAstAltFunction(I, alt_args, nest);
        PROTECT(alt);
        AstCompositeFunction *func = newAstCompositeFunction(I, alt, NULL);
        PROTECT(func);
        func->unsafe = false;
        
        AstExpression *func_expr = newAstExpression_Fun(I, func);
        PROTECT(func_expr);
        
        AstDefine *def = newAstDefine(I, hygienicName, func_expr);
        PROTECT(def);
        
        AstDefinition *result = newAstDefinition_Define(I, def);
        UNPROTECT(save);
        return result;
    }
    
    // Otherwise, wrap the complex implementation
    // fn hygienicName(p$1, ..., p$N) { impl(p$1, ..., p$N) }
    // ... similar to above but calling impl
}
```

## Associativity Deep Dive

### When Does Associativity Matter?

Associativity is **only meaningful for binary (arity 2) mixfix operators** that can be chained.

### Binary Mixfix Operators

#### Example 1: Left-Associative Binary Mixfix

```fn
fn plus(a, b) { a + b }
mixfix left 100 "_plus_" plus;

// Expression:
let x = 5 plus 3 plus 2 in ...

// Parsing with LEFT associativity:
// First pass: sees '5', then 'plus' (infix trigger)
//   - lhs = 5
//   - Parse RHS with precedence 101 (prec + 1)
//   - RHS parsing sees '3', then 'plus' again
//   - But 'plus' needs precedence 100, we're at 101
//   - So RHS stops at '3', returns 3
//   - First call builds: plus(5, 3)
//
// Second pass: sees 'plus' again  
//   - lhs = plus(5, 3)
//   - Parse RHS with precedence 101
//   - Gets '2'
//   - Builds: plus(plus(5, 3), 2)
//
// Result: (5 plus 3) plus 2 = (8) plus 2 = 10
```

#### Example 2: Right-Associative Binary Mixfix

```fn
fn cons_op(a, b) { a @ b }
mixfix right 90 "_cons_" cons_op;

// Expression:
let x = 1 cons 2 cons 3 cons [] in ...

// Parsing with RIGHT associativity:
// First pass: sees '1', then 'cons' (infix trigger)
//   - lhs = 1  
//   - Parse RHS with precedence 90 (same as current - allows recursion)
//   - RHS parsing sees '2', then 'cons'
//   - 'cons' needs precedence 90, we're at 90, so it continues
//   - RHS recursively parses: 2 cons (3 cons [])
//   - Returns: cons(2, cons(3, []))
//   - First call builds: cons(1, cons(2, cons(3, [])))
//
// Result: 1 cons (2 cons (3 cons [])) = [1, 2, 3]
```

#### Example 3: Non-Associative Binary Mixfix (Default)

```fn
fn mod_op(a, b) { a % b }
mixfix none 100 "_mod_" mod_op;
// Or equivalently (none is default):
mixfix 100 "_mod_" mod_op;

// Expression:
let x = 10 mod 3 mod 2 in ...

// Parsing with NONE associativity:
// First pass: sees '10', then 'mod'
//   - lhs = 10
//   - Parse RHS with precedence 101 (prec + 1, like left)
//   - Gets '3'
//   - Builds: mod(10, 3)
//   - After returning, parser checks next token
//   - Sees 'mod' again at same precedence
//   - Checks: is it the same operator and non-associative?
//   - YES → ERROR: "non-associative operator cannot be chained"
//
// Error: Must write: (10 mod 3) mod 2  OR  10 mod (3 mod 2)
```

### Non-Binary Mixfix Operators

For operators with arity > 2, associativity doesn't make sense:

```fn
// Ternary operator
mixfix 1 "if_then_else_" ternary;

// What would this even mean?
if a then b else c if d then e else f

// Possible interpretations:
// 1. Left: (if a then b else c) if d then e else f
//    - But the first part is a value, not a boolean
//    - And 'if' expects it as first arg... syntax error!
//
// 2. Right: if a then b else (c if d then e else f)
//    - This could work: the else branch is itself an if expression
//    - But is this what user meant? Unclear!
//
// Better: Require explicit nesting
if a then b else (if d then e else f)  // Clear!
```

**Recommendation:** For arity > 2, associativity should be:

- Rejected during parsing (error if user specifies `left` or `right`)
- Or ignored (treated as `none`)
- Or only `none` is allowed

### Associativity Implementation

The implementation mirrors current infix operators:

```c
static AstExpression *userMixfix(PrattRecord *record,
                                 PrattParser *parser,
                                 AstExpression *lhs,
                                 PrattToken *tok)
{
    // ... pattern analysis ...
    
    // For binary operators, associativity affects RHS precedence
    int rhs_prec;
    if (pattern->arity == 2) {
        switch (record->associativity) {
        case PRATTASSOC_TYPE_LEFT:
            rhs_prec = record->mixfixPrec + 1;  // Higher prec = stops recursion
            break;
        case PRATTASSOC_TYPE_RIGHT:
            rhs_prec = record->mixfixPrec;      // Same prec = allows recursion
            break;
        case PRATTASSOC_TYPE_NONE:
            rhs_prec = record->mixfixPrec + 1;  // Like left, but check after
            break;
        }
    } else {
        // For non-binary, always use prec + 1
        rhs_prec = record->mixfixPrec + 1;
    }
    
    // Parse arguments with appropriate precedence
    // ... parsing logic ...
    
    // For non-associative binary operators, check for chaining
    if (pattern->arity == 2 && record->associativity == PRATTASSOC_TYPE_NONE) {
        PrattToken *next = peek(parser);
        if (next != NULL) {
            PrattRecord *next_record = fetchRecord(parser, next->type, false);
            if (next_record != NULL &&
                next_record->symbol == record->symbol &&
                next_record->mixfixPrec == record->mixfixPrec) {
                parserError(parser, 
                           "non-associative operator '%s' cannot be chained",
                           record->symbol->name);
            }
        }
    }
    
    // ... build function call ...
}
```

### Chaining vs. Composition

**Important distinction:** Some operators appear to chain but actually don't need associativity:

```fn
mixfix 110 "_[_]" index;

// This works:
matrix[i][j][k]

// Why? It's not associativity, it's composition:
// (((matrix[i])[j])[k])
//
// Each [_] operates on the result of the previous expression:
// 1. matrix[i] returns a row (which is also indexable)
// 2. row[j] returns an element (which might also be indexable)
// 3. element[k] returns a value
//
// This is just normal left-to-right parsing of infix operators
// No associativity needed!
```

Similarly:

```fn
fn apply(f, x) { f(x) }
mixfix 120 "_$_" apply;

// Function application composition:
f $ g $ x
// Parses as: (f $ g) $ x
// But if $ is right-associative:
mixfix right 120 "_$_" apply;
// Then: f $ (g $ x)  which might be more useful
```

### Testing Associativity

#### Test 1: Left Associativity

```fn
fn test_left_assoc() {
    let
        fn minus(a, b) { a - b }
        mixfix left 100 "_minus_" minus;
    in {
        // Left: (10 - 3) - 2 = 7 - 2 = 5
        assert((10 minus 3 minus 2) == 5);
        
        // Right would be: 10 - (3 - 2) = 10 - 1 = 9
        assert((10 minus (3 minus 2)) == 9);
    }
}
```

#### Test 2: Right Associativity

```fn
fn test_right_assoc() {
    let
        fn power(a, b) { a ** b }
        mixfix right 110 "_pow_" power;
    in {
        // Right: 2 ** (3 ** 2) = 2 ** 9 = 512
        assert((2 pow 3 pow 2) == 512);
        
        // Left would be: (2 ** 3) ** 2 = 8 ** 2 = 64
        assert(((2 pow 3) pow 2) == 64);
    }
}
```

#### Test 3: Non-Associative (Error Case)

```fn
fn test_non_assoc() {
    let
        fn cmp(a, b) {
            if (a < b) { lt }
            else if (a == b) { eq }
            else { gt }
        }
        mixfix none 50 "_cmp_" cmp;
    in {
        // This should work:
        assert((5 cmp 3) == gt);
        
        // This should be a PARSE ERROR:
        // let x = 5 cmp 3 cmp 2 in ...
        // Error: non-associative operator 'cmp' cannot be chained
        
        // Must use explicit parens:
        assert(((5 cmp 3) cmp (3 cmp 2)) == gt);
    }
}
```

#### Test 4: Default is None

```fn
fn test_default_none() {
    let
        fn combine(a, b) { [a, b] }
        mixfix 100 "_with_" combine;  // No associativity specified
    in {
        // This should work:
        assert((1 with 2) == [1, 2]);
        
        // This should ERROR (default is none):
        // let x = 1 with 2 with 3 in ...
        // Error: non-associative operator 'with' cannot be chained
    }
}
```

### Summary Table

| Pattern | Arity | Assoc | Expression `a op b op c` | Result |
|---------|-------|-------|--------------------------|--------|
| `"_+_"` | 2 | left | `(a + b) + c` | Left-to-right |
| `"_@_"` | 2 | right | `a @ (b @ c)` | Right-to-left |
| `"_<=>_"` | 2 | none | ERROR | Must parenthesize |
| `"_[_]"` | 2 | (default=none) | `(a[b])[c]` | Works due to composition, not assoc |
| `"if_then_else_"` | 3 | N/A | N/A | Doesn't chain |

**Key Takeaway:**

- For **binary mixfix** `"_op_"`: associativity works exactly like infix `op`
- For **non-binary mixfix**: associativity is meaningless
- **Default**: `none` (safest, most explicit)
- **Special case**: Some operators appear to chain but are actually composing (like array indexing)

## Testing Examples

### Test 1: Basic Binary Mixfix

```fn
fn test_binary_mixfix() {
    let
        fn index { (h @ _, 0) { h } (_ @ t, n) { index(t, n - 1) } }
        mixfix 110 "_[_]" index;
        list = [10, 20, 30];
    in {
        assert(list[0] == 10);
        assert(list[2] == 30);
    }
}
```

### Test 2: Ternary Mixfix

```fn
fn test_ternary_mixfix() {
    let
        fn tern { (true, c, _) { c } (false, _, a) { a } }
        mixfix 1 "if_then_else_" tern;
    in {
        assert((if true then 42 else 0) == 42);
        assert((if false then 0 else 99) == 99);
    }
}
```

### Test 3: Precedence

```fn
fn test_mixfix_precedence() {
    let
        fn index { (h @ _, 0) { h } (_ @ t, n) { index(t, n - 1) } }
        mixfix 110 "_[_]" index;
        list = [10, 20, 30];
    in {
        assert(list[1] + list[2] == 50);  // (list[1]) + (list[2])
        assert(list[0] * 2 == 20);        // (list[0]) * 2
    }
}
```

### Test 4: Hygiene

```fn
fn test_mixfix_hygiene() {
    let
        fn myFunc(x, y) { x + y }
        mixfix 100 "_plus_" myFunc;
    in {
        let
            fn myFunc(a, b) { a * b }  // Shadow the function
        in {
            assert(5 plus 3 == 8);     // Still uses original myFunc (addition)
            assert(myFunc(5, 3) == 15) // But direct call uses shadowed (multiplication)
        }
    }
}
```

### Test 5: Scoping

```fn
fn test_mixfix_scoping() {
    let
        mixfix 100 "_foo_" fn(x, y) { x + y };
        a = 5 foo 3;
    in {
        let
            mixfix 100 "_foo_" fn(x, y) { x * y };  // Shadow
            b = 5 foo 3;
        in {
            assert(a == 8);   // Outer scope: addition
            assert(b == 15)   // Inner scope: multiplication
        };
        assert(5 foo 2 == 7)  // Back to outer scope: addition
    }
}
```

## Error Cases

### Error 1: Missing Keyword

```fn
let
    mixfix 100 "_[_:_]" slice;
    list = [1, 2, 3, 4, 5];
in
    list[1 2]  // ERROR: expected ':' in mixfix operator _[_:_]
```

### Error 2: Wrong Arity

```fn
let
    mixfix 100 "_[_]" index;
    list = [1, 2, 3];
in
    list[1, 2]  // ERROR: mixfix operator _[_]_ expects 2 arguments, got 3
```

### Error 3: Conflicting Patterns

```fn
let
    mixfix 100 "if_then_else_" ternary;
    mixfix 100 "if_then_" binary;  // ERROR: pattern conflicts with existing mixfix operator
in
    ...
```

### Error 4: No Leading Hole

```fn
mixfix 100 "foo_bar_" fn(x, y) { x + y }
// ERROR: mixfix pattern must start with _ or a keyword followed by _
```

## Comparison with Existing Operators

### Infix Operator (Current)

```fn
infix left 100 "+" addition;
// Usage: a + b
// Desugars to: $infix$42(a, b)  where $infix$42 = addition
```

### Mixfix Operator (Proposed)

```fn
mixfix 100 "_+_" addition;
// Usage: a + b  (identical!)
// Desugars to: $mixfix$42(a, b)  where $mixfix$42 = addition
```

**Key insight:** Infix operators are just a special case of mixfix with pattern `"_op_"`. We could unify them, but keeping them separate is clearer for users.

### Postfix Operator (Current)

```fn
postfix 120 "!" factorial;
// Usage: n!
// Desugars to: $postfix$42(n)
```

### Mixfix Equivalent

```fn
mixfix 120 "_!" factorial;
// Usage: n!
// Desugars to: $mixfix$42(n)
```

Again, infix/prefix/postfix are special cases, but keeping them distinct improves ergonomics and error messages.
