# Mixfix Secondary Keywords and Precedence Interaction

## Question

Should we allow mixfix secondary keywords to coexist with standalone operators of the same symbol? Currently there's a conflict check that prevents this (line 804-811 in `mergeFixity()`).

## Analysis

### Current Behavior (With Conflict Check)

If you try to import a mixfix operator whose secondary keyword matches an existing operator:
```fn
operator "_??_" left 10 BINARY_OP;
import ns operator "_?_??_";  // ERROR: conflict with existing operator
```

### Proposed Behavior (Without Conflict Check)

Allow both to coexist, with precedence determining which "wins":

**Example**:
- Existing: `_??_` with precedence 10
- Mixfix: `_?_??_` with precedence 5
- Expression: `a ? b ?? c`

**Execution Flow**:

1. Parser recognizes `?` as mixfix primary operator (prec 5)
2. Calls `userMixfix()` which needs to parse arguments interleaved with `??`
3. Calculates argument-parsing precedence (line 1455):
   - Left-assoc: `prec + 1 = 6`
   - Right-assoc: `prec - 1 = 4`
4. Calls `expressionPrecedence(parser, 6)` to parse first argument
5. Parses `b`, looks ahead, sees `??`
6. Finds PrattRecord for standalone `??` (prec 10)
7. Checks: `10 < 6` → false
8. **Consumes `??` as standalone operator**, parses as `b ?? c`
9. Returns to `userMixfix()`
10. `userMixfix()` expects keyword `??` but it's already consumed
11. **Error**: "expected mixfix operator keyword `??` got `...`"

### Key Insight: Precedence-Driven Disambiguation

The behavior depends on relative precedences:

| Standalone `??` Prec | Mixfix `_?_??_` Prec | Arg Parse Prec | Result |
|---|---|---|---|
| 10 | 5 (left) | 6 | Standalone wins, parse ERROR |
| 10 | 5 (right) | 4 | Standalone wins, parse ERROR |
| 10 | 15 (left) | 16 | Mixfix wins, parses correctly |
| 10 | 15 (right) | 14 | Mixfix wins, parses correctly |
| 5 | 10 (left) | 11 | Mixfix wins, parses correctly |

**Rule**: If `standalone_prec < mixfix_arg_parsing_prec`, the mixfix works. Otherwise, parse error.

### Advantages of Allowing Coexistence

1. **Flexibility**: Users can define both operators if they're careful about precedences
2. **Use-site errors**: Problems manifest at use-site with clear error messages, not at import-site
3. **Precedence control**: Users have fine-grained control via precedence levels
4. **Consistency**: Matches how normal operator precedence works

### Disadvantages of Allowing Coexistence

1. **Subtle bugs**: User might not realize secondary keyword conflicts with standalone operator
2. **Precedence confusion**: Behavior depends on precedence arithmetic that might be non-obvious
3. **Error at use-site vs import-site**: Some prefer fail-fast at import time

### Comparison to Normal Operators

This is analogous to how nested operators work:
```fn
a + b * c
```
- `*` has higher precedence, so binds tighter
- Parse as `a + (b * c)` not `(a + b) * c`
- Precedence determines structure

Similarly:
```fn
a ? b ?? c
```
- If standalone `??` has higher precedence than mixfix argument-parsing precedence
- It "binds tighter" and steals the keyword
- Parse fails, but failure is at use-site with clear message

## Test Cases

### Case 1: Lower Precedence Standalone (Should Work)

```fn
operator "_??_" left 5 lower_op;
operator "_?_??_" left 10 mixfix_op;

// Parses as: mixfix_op(a, b, c)
// Because standalone ?? (prec 5) < arg parse prec (11)
a ? b ?? c
```

### Case 2: Higher Precedence Standalone (Should Error)

```fn
operator "_??_" left 15 higher_op;
operator "_?_??_" left 10 mixfix_op;

// Tries to parse as: mixfix_op(a, higher_op(b, c), ???)
// But mixfix expects ?? keyword, finds EOF or next token
// ERROR: "expected mixfix operator keyword ?? got ..."
a ? b ?? c
```

### Case 3: No Conflict (Different Symbols)

```fn
operator "_??_" left 10 standalone;
operator "_?_!!_" left 10 mixfix;

// No ambiguity, works fine
a ? b !! c
```

## Recommendation

**Remove the conflict check** (lines 804-811 in `mergeFixity()`).

**Rationale**:
1. Precedence-based disambiguation is consistent with how Pratt parsing works
2. Use-site errors are more informative than blanket import-time rejection
3. Users have control via precedence settings
4. Reduces artificial restrictions

**Alternative**: Keep conflict check but make it a **warning** instead of error, with flag to suppress.

## Implementation

Simply remove or comment out the conflict check in `mergeFixity()`:

```c
// Check for conflicts with existing keywords, except the first
if (source->pattern != NULL) {
    for (Index i = 1; i < source->pattern->keywords->size; ++i) {
        HashSymbol *inner =
            utf8ToSymbol(source->pattern->keywords->entries[i]);
        
        // REMOVED: Conflict check
        // if (getPrattRecordTable(parser->rules, inner, NULL)) {
        //     parserError(parser, "import operator conflicts...");
        // }
        
        // Add secondary keyword to importing parser's trie
        parser->trie = insertPrattTrie(parser->trie, inner);
    }
    target->pattern = source->pattern;
}
```

The same change should be made in `addMixfixOperator()` at line 1548-1559 for consistency.

## Edge Cases to Consider

1. **What if secondary keyword matches a built-in operator?**
   - Same precedence logic applies
   - Might be confusing but not broken

2. **What if multiple mixfix operators share secondary keywords?**
   - Each mixfix has its own pattern reference
   - When parsing, whichever mixfix is being parsed expects its keywords
   - Shouldn't interfere if precedences are set correctly

3. **What about scoping?**
   - Secondary keywords are scoped with their mixfix operator
   - When mixfix goes out of scope, secondary keywords are no longer relevant
   - Standalone operator continues to work

## Files Involved

- `src/pratt_parser.c` - Lines 800-811 (`mergeFixity()`) and 1548-1559 (`addMixfixOperator()`)
