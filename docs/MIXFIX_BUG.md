# Mixfix Operator Secondary Keyword Bug

## Problem Summary

Mixfix operators with multiple keywords (e.g., `"_||>_|||>_||||>"`) fail to parse. The secondary and tertiary keywords (`|||>`, `||||>`) are not being properly registered as tokens, causing "unrecognised token" errors when the operator is used.

## Test Case

```fn
let
    operator "_||>_|||>_||||>_" right 10 fn (a, b, c, d) {
        a + b + c + d
    }
in
    1 ||> 2 |||> 3 ||||> 4;
```

**Error**: `unrecognised token: |||> at +7 tests/fn/test_multivalue_operator.fn`

## Root Cause Analysis

### How Mixfix Operators Currently Work

1. **Pattern Parsing** (`parseMixfixPattern()` at line 1791):
   - Parses operator pattern string (e.g., `"_||>_|||>_||||>"`)
   - Extracts keywords into `PrattMixfixPattern.keywords` array
   - Keywords: `["||>", "|||>", "||||>"]` (index 0, 1, 2)
   - Calculates arity (number of `_` holes = 4 in this case)

2. **Operator Registration** (`addMixfixOperator()` at line 1545):
   ```c
   for (Index i = 1; i < pattern->keywords->size; ++i) {
       HashSymbol *inner = utf8ToSymbol(pattern->keywords->entries[i]);
       if (getPrattRecordTable(parser->rules, inner, NULL)) {
           parserError(parser, "mixfix operator keyword %s conflicts...");
       }
       parser->trie = insertPrattTrie(parser->trie, inner);
   }
   ```
   - Adds secondary keywords (`|||>`, `||||>`) to the **scanner trie**
   - Checks for conflicts with existing operators
   - **DOES NOT** add PrattRecords for secondary keywords to `parser->rules`

3. **Token Lookup** (`lookUpTrieSymbol()` at line 297 in `pratt_scanner.c`):
   - Scanner looks up tokens in `parser->trie`
   - If found, returns token with the symbol as its type
   - The secondary keywords ARE in the trie, so they should be recognized

4. **Parsing Usage** (`userMixfix()` at line 1427):
   ```c
   while (arity > 0) {
       lhs = expressionPrecedence(parser, precedence);
       pushAstExpressionArray(args, lhs);
       arity--;
       if (arity > 0 || !(pattern->endsWithHole)) {
           PrattUTF8 *kw = keywords->entries[kwIndex++];
           PrattToken *nextTok = peek(parser);
           HashSymbol *nextSym = utf8ToSymbol(kw);
           if (!isAtomSymbol(nextTok, nextSym)) {
               parserErrorAt(..., "expected mixfix operator keyword...");
           }
           next(parser);
       }
   }
   ```
   - Expects secondary keywords as ATOM tokens
   - Uses `isAtomSymbol(nextTok, nextSym)` which checks:
     ```c
     return (token->value->type == PRATTVALUE_TYPE_ATOM && 
             token->value->val.atom == symbol);
     ```

### The Problem - ROOT CAUSE IDENTIFIED

**The execution flow reveals the real issue**:

1. When parsing `1 ||> 2 |||> 3 ||||> 4`:
   - Parser recognizes `||>` as an infix operator with `userInfixMix` parselet
   - Calls `userMixfix()` at line 1427 to handle the mixfix operator
   
2. Inside `userMixfix()` at line 1457:
   - Calls `expressionPrecedence(parser, precedence)` to parse the first argument
   - This **recursively** parses `2`
   - After parsing `2`, `expressionPrecedence()` **looks ahead** for infix/postfix operators
   - Sees `|||>` token and tries to extend the expression
   
3. At line 4253 in `expressionPrecedence()`:
   ```c
   PrattRecord *record = fetchRecord(parser, op->type);
   if (record == NULL) {
       parserError(parser, "unrecognised token: %s", op->type->name);
   ```
   - `fetchRecord()` returns NULL because `|||>` has no PrattRecord
   - Parser errors before `userMixfix()` can consume `|||>` at line 1467

4. The control **never returns** to `userMixfix()` to consume the secondary keyword

### Why Secondary Keywords Should NOT Have PrattRecords

Secondary keywords like `|||>` are NOT standalone operators - they're part of the mixfix syntax. They should:
- Be **tokenized** (require trie entry) so `userMixfix()` can peek/consume them at line 1467
- **Stop expression parsing** (require NO PrattRecord) so `expressionPrecedence()` returns to `userMixfix()`

The current code incorrectly treats "no PrattRecord" as an error. It should treat it as "end of expression".

## Current Implementation Details

### Data Structures

**PrattMixfixPattern** (src/pratt.yaml:134):
```yaml
PrattMixfixPattern:
    data:
        keywords: PrattStrings       # Array of keyword strings
        arity: int                   # Number of arguments
        associativity: PrattAssoc
        startsWithHole: bool
        endsWithHole: bool
```

**PrattFixityConfig** (src/pratt.yaml:226):
```yaml
pattern: PrattMixfixPattern=NULL     # Stored on primary operator only
```

### Scanner Token Recognition

The scanner uses a trie-based approach for recognizing operator tokens:
- `insertPrattTrie()` adds symbols to the trie
- `lookUpTrieSymbol()` searches the trie for matching operator sequences
- Returns a token with the matched symbol as its type

### Key Functions

- `parseMixfixPattern()` - line 1791: Parses operator pattern string
- `addMixfixOperator()` - line 1545: Registers operator with parser
- `userMixfix()` - line 1427: Parses usage of mixfix operator
- `lookUpTrieSymbol()` - pratt_scanner.c:297: Scanner trie lookup
- `isAtomSymbol()` - pratt_scanner.h:53: Token type checker

## Potential Solutions (Not Yet Implemented)

### Option 1: Make NULL PrattRecord Stop Expression Parsing (RECOMMENDED)

Modify `expressionPrecedence()` at line 4253-4256 to treat missing PrattRecord as "end of expression":

```c
PrattRecord *record = fetchRecord(parser, op->type);
if (record == NULL) {
    // Token exists (in trie) but has no PrattRecord
    // This is valid for mixfix secondary keywords
    // Stop expression parsing and return to caller
    break;
}
```

**Advantages**:
- Minimal change (3 lines)
- Correct semantics: unknown tokens stop expression parsing
- Secondary keywords work automatically
- No special tracking or data structures needed

**Rationale**:
- If a token is in the trie but not in rules, it means:
  - Scanner recognizes it (can be used as keyword)
  - Parser doesn't treat it as operator (can't extend expression)
- This is exactly what mixfix secondary keywords need

### Option 2: Add Dummy PrattRecords with NULL Parselets

Create minimal PrattRecords for secondary keywords:
```c
// In addMixfixOperator() after line 1556:
PrattFixityConfig empty = {NULL, 0, NULL, NULL, false, false, NULL, -1, NULL};
PrattRecord *keywordRecord = newPrattRecord(inner, empty, empty, empty);
setPrattRecordTable(parser->rules, inner, keywordRecord);
```

Then ensure `expressionPrecedence()` handles NULL parselets:
```c
} else if (record->infix.op != NULL) {
    // ... existing infix handling
} else {
    // Record exists but no infix/postfix parselet
    break;
}
```

**Disadvantages**:
- More complex: changes in two places
- Creates unnecessary PrattRecord objects
- Doesn't match semantics (secondary keywords aren't operators)

### Option 3: Track Secondary Keywords Explicitly

Maintain a set of secondary keyword symbols:
```c
// In addMixfixOperator():
parser->mixfixKeywords = insertHashSet(parser->mixfixKeywords, inner);
```

Then check in `expressionPrecedence()`:
```c
if (record == NULL) {
    if (isInHashSet(parser->mixfixKeywords, op->type)) {
        break;  // Secondary keyword, stop parsing
    }
    parserError(parser, "unrecognised token: %s", op->type->name);
}
```

**Disadvantages**:
- Requires new data structure
- More bookkeeping
- Less elegant than Option 1

## Recommended Approach

**Option 1 is the correct fix**: Change line 4254-4256 in `expressionPrecedence()` from:
```c
parserError(parser, "unrecognised token: %s", op->type->name);
break;
```

To simply:

```c
break;  // Unknown token - stop expression parsing
```

The Fix (One Line)

In `expressionPrecedence()` at line 4253-4256, change:

```c
PrattRecord *record = fetchRecord(parser, op->type);
if (record == NULL) {
    parserError(parser, "unrecognised token: %s", op->type->name);
    break;
}
```

To:

```c
PrattRecord *record = fetchRecord(parser, op->type);
if (record == NULL) {
    // Token exists but has no operator record - stop expression parsing
    break;
}
```

### Why This Works

1. **During operator definition**: `addMixfixOperator()` adds secondary keywords to `parser->trie` (line 1556)
2. **During scanning**: Scanner finds `|||>` in trie, creates token
3. **During parsing**: 
   - `expressionPrecedence()` recursively parses argument inside `userMixfix()`
   - After parsing argument, looks ahead for infix/postfix operators
   - Sees `|||>` token, calls `fetchRecord()` → returns NULL
   - **NEW**: Breaks out of loop, returns to `userMixfix()`
   - `userMixfix()` consumes `|||>` at line 1467-1475
4. **Result**: Mixfix operator parses correctly

### Testing

1. **Basic mixfix**: `1 ||> 2 |||> 3 ||||> 4` should evaluate to `10`
2. **Error case**: Bare `|||>` without context should error in statement context (not in `expressionPrecedence()` but when statement parser doesn't recognize it)
3. **Conflict detection**: Already handled at line 1549 - error if secondary keyword conflicts with existing operator
4. **Scoping**: Secondary keywords scoped with their mixfix operator (via parser nesting)

### Additional Considerations

**Question**: Should truly invalid tokens error in `expressionPrecedence()` or earlier?

**Answer**: Invalid tokens won't be in the trie, so scanner will error with "unrecognised token" before `expressionPrecedence()` is reached. Only tokens that are:
- In the trie (intentionally registered)
- But not in rules (not operators)

Will reach this code path. These are exactly the mixfix secondary keywords.

**Edge case**: What if someone does `x = |||> + 5`?
- `|||>` is parsed as a symbol/identifier (if used in prefix position, might hit different code path)
- Actually, `|||>` is in trie, so it becomes a token
- In prefix position, `expressionPrecedence()` would call prefix parselet
- No prefix parselet exists → would need NULL check there too
- OR: prefix position might fail earlier in scanning logic
2. Test error cases: bare keyword `|||>` without context
3. Test conflicts: defining `|||>` as both secondary keyword and standalone operator
4. Test scoping: secondary keywords only valid where mixfix operator is in scope

### Alternative: Error Parselets

Instead of NULL parselets, could create error parselets:
```c
static AstExpression *mixfixKeywordError(PrattRecord *record, ...) {
    parserError(parser, "keyword %s can only appear within mixfix operator",
                record->symbol->name);
    return errorExpression(...);
}
```

This provides better error messages but requires more infrastructure.
