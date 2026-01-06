# Mixfix Operator Import Bug - Secondary Keywords Not Added to Trie

## Problem Summary

When importing a mixfix operator from another namespace, the secondary keywords are not added to the importing parser's trie, causing them to be mis-tokenized.

## Test Case

**Export file** (`import_operator_multi.fn`):

```fn
namespace
export operator "_||>_|||>_||||>_" 10 fn (a, b, c, d) { a + b + c + d }
```

**Import file** (`test_multivalue_operator_import.fn`):

```fn
let
    link "import_operator_multi.fn" as op;
    import op operator "_||>_|||>_||||>_";
in
    1 ||> 2 |||> 3 ||||> 4;
```

**Error**: `expected mixfix operator keyword "|||>" got "|"`

## Root Cause Analysis

### The Import Flow

1. **During Export** (in exporting namespace):
   - `addMixfixOperator()` at line 1545 adds secondary keywords to exporter's trie:

     ```c
     for (Index i = 1; i < pattern->keywords->size; ++i) {
         HashSymbol *inner = utf8ToSymbol(pattern->keywords->entries[i]);
         parser->trie = insertPrattTrie(parser->trie, inner);
     }
     ```

   - Pattern is stored in `PrattRecord.prefix/infix/postfix.pattern`
   - `PrattRecord` is added to `exportedRules` table

2. **During Import** (in importing namespace at line 2269):
   - `ensureTargetRecord()` creates/gets target record at line 770:

     ```c
     target = newPrattRecord(op, empty, empty, empty);
     setPrattRecordTable(parser->rules, op, target);
     parser->trie = insertPrattTrie(parser->trie, op);  // Only primary operator!
     ```

   - `mergeFixityImport()` called at line 2270
   - `mergeFixity()` called at line 817

3. **In `mergeFixity()` at line 785**:
   - Copies implementation, precedence, parselets from source
   - At line 800: Checks for conflicts with secondary keywords
   - At line 809: Copies pattern reference: `target->pattern = source->pattern`
   - **BUT**: Never adds secondary keywords to importing parser's trie!

4. **Result**:
   - Primary operator `||>` is in importing parser's trie
   - Secondary keywords `|||>`, `||||>` are NOT in importing parser's trie
   - When parsing `1 ||> 2 |||> 3`, scanner can't tokenize `|||>`
   - Scanner falls back to single-character tokens: `|`, `|`, `|`, `>`
   - `userMixfix()` expects `|||>` but gets `|`

### Why Secondary Keywords Must Be In Trie

The scanner needs trie entries to recognize multi-character operators:

- Without trie entry for `|||>`, scanner tokenizes it as four separate characters
- With trie entry, scanner recognizes it as a single token
- The trie is the scanner's longest-match token recognizer

## Current Code Paths

### Export: How Secondary Keywords ARE Added

In `addMixfixOperator()` at line 1548-1559:

```c
for (Index i = 1; i < pattern->keywords->size; ++i) {
    HashSymbol *inner = utf8ToSymbol(pattern->keywords->entries[i]);
    if (getPrattRecordTable(parser->rules, inner, NULL)) {
        parserError(parser, "mixfix operator keyword %s conflicts...");
    }
    parser->trie = insertPrattTrie(parser->trie, inner);
}
```

Secondary keywords added to trie in defining namespace.

### Import: Where Secondary Keywords Should Be Added

In `mergeFixity()` at line 785, after line 809 where pattern is copied:

```c
target->pattern = source->pattern;

// MISSING: Add secondary keywords to importing parser's trie
for (Index i = 1; i < source->pattern->keywords->size; ++i) {
    HashSymbol *inner = utf8ToSymbol(source->pattern->keywords->entries[i]);
    parser->trie = insertPrattTrie(parser->trie, inner);
}
```

### Where Pattern Pointer Is Valid

The pattern is checked at line 800:

```c
for (Index i = 1; i < source->pattern->keywords->size; ++i) {
    // ... conflict check
}
```

This proves `source->pattern` is valid and accessible. The same loop structure should be used to add keywords to trie.

## Solution

### Option 1: Add Keywords in `mergeFixity()` (RECOMMENDED)

Modify `mergeFixity()` at line 800-809 to also add secondary keywords to trie:

```c
// Check for conflicts with existing keywords, except the first
for (Index i = 1; i < source->pattern->keywords->size; ++i) {
    HashSymbol *inner =
        utf8ToSymbol(source->pattern->keywords->entries[i]);
    if (getPrattRecordTable(parser->rules, inner, NULL)) {
        parserError(
            parser,
            "import operator conflicts with existing operator");
    }
    // NEW: Add secondary keyword to importing parser's trie
    parser->trie = insertPrattTrie(parser->trie, inner);
}
target->pattern = source->pattern;
```

**Advantages**:

- Minimal change: one line added
- Mirrors the logic in `addMixfixOperator()`
- Happens in same place as conflict checking
- Both conflict check and trie insertion in same loop

**Considerations**:

- Pattern might be NULL for non-mixfix operators
- Need to check `source->pattern != NULL` before loop

### Option 2: Add Keywords in `mergeFixityImport()`

Add trie updates after calling `mergeFixity()` at line 817-840:

```c
if (importPrefix) {
    mergeFixity(parser, &target->prefix, &source->prefix, nsRef, nsSymbol);
    if (source->prefix.pattern != NULL) {
        for (Index i = 1; i < source->prefix.pattern->keywords->size; ++i) {
            HashSymbol *inner = utf8ToSymbol(source->prefix.pattern->keywords->entries[i]);
            parser->trie = insertPrattTrie(parser->trie, inner);
        }
    }
}
// Similar for infix and postfix
```

**Disadvantages**:

- Duplicates code three times (prefix, infix, postfix)
- Less clean than Option 1

### Option 3: Separate Function for Secondary Keyword Registration

Create helper function:

```c
static void addSecondaryKeywordsToTrie(PrattParser *parser, PrattMixfixPattern *pattern) {
    if (pattern == NULL) return;
    for (Index i = 1; i < pattern->keywords->size; ++i) {
        HashSymbol *inner = utf8ToSymbol(pattern->keywords->entries[i]);
        parser->trie = insertPrattTrie(parser->trie, inner);
    }
}
```

Call from both `addMixfixOperator()` and `mergeFixity()`.

**Advantages**:

- Reduces duplication
- Clear separation of concerns

**Disadvantages**:

- More code churn
- Might be overkill for current needs

## Recommended Implementation

**Option 1 with NULL check**:

In `mergeFixity()` at line 800:

```c
// Check for conflicts with existing keywords, except the first
if (source->pattern != NULL) {
    for (Index i = 1; i < source->pattern->keywords->size; ++i) {
        HashSymbol *inner =
            utf8ToSymbol(source->pattern->keywords->entries[i]);
        if (getPrattRecordTable(parser->rules, inner, NULL)) {
            parserError(
                parser,
                "import operator conflicts with existing operator");
        }
        // Add secondary keyword to importing parser's trie
        parser->trie = insertPrattTrie(parser->trie, inner);
    }
    target->pattern = source->pattern;
}
```

Changes:

1. Add `if (source->pattern != NULL)` wrapper around existing conflict check loop
2. Add `parser->trie = insertPrattTrie(parser->trie, inner);` inside loop
3. Move `target->pattern = source->pattern;` inside the NULL check

This ensures:

- Simple operators (no pattern) work unchanged
- Mixfix operators get secondary keywords in trie
- Pattern is only copied when it exists

## Testing

After fix, verify:

1. **Basic import**: `import op operator "_||>_|||>_||||>_"` works ✅
2. **Bulk import**: `import op operators` includes mixfix operators
3. **Conflict detection**: Importing when secondary keyword exists as operator errors appropriately
4. **Scoping**: Secondary keywords only valid in importing scope
5. **Non-mixfix operators**: Still import correctly (pattern = NULL case)

## Future Work / TODO

**Review Conflict Check Necessity**: The conflict check at line 804-811 (in the loop over secondary keywords) checks if any secondary keyword conflicts with existing operators in the importing parser's rules table. This may be overly conservative or unnecessary given that:

- Secondary keywords are not standalone operators
- They can't be used outside their mixfix operator context
- The conflict might be valid if scoped appropriately

Consider whether this check should:

1. Be removed entirely (allow secondary keywords to shadow existing operators)
2. Be relaxed to only check for conflicts within the same scope
3. Be kept as-is for safety

Test cases needed: What happens if importing namespace already has `|||>` as a standalone operator, then tries to import a mixfix operator using `|||>` as a secondary keyword?

## Files Involved

- `src/pratt_parser.c` - Line 785-809 (`mergeFixity()` function)
- `tests/fn/import_operator_multi.fn` - Export namespace with mixfix operator
- `tests/fn/test_multivalue_operator_import.fn` - Import test case
