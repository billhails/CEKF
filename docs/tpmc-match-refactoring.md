# Refactoring Recommendations for tpmc_match.c

This document outlines proposed refactorings to improve maintainability of [src/tpmc_match.c](../../src/tpmc_match.c).

## Overview

The file is currently ~1035 lines implementing the TPMC match algorithm based on the Pettersson 1992 paper. While functional, several areas could benefit from restructuring.

---

## 1. Extract Duplicate Arc Detection (High Priority)

**Problem:** The duplicate-arc detection in `mixture()` (lines 862-914) is ~50 lines of deeply nested conditional code checking if an arc already exists for a given constructor.

**Current code pattern:**

```c
bool alreadyProcessed = false;
for (Index i = 0; i < testState->state->val.test->arcs->size; i++) {
    TpmcArc *existingArc = testState->state->val.test->arcs->entries[i];
    if (c->pattern->type == existingArc->test->pattern->type) {
        if (c->pattern->type == TPMCPATTERNVALUE_TYPE_CONSTRUCTOR) {
            if (c->pattern->val.constructor->tag ==
                existingArc->test->pattern->val.constructor->tag) {
                alreadyProcessed = true;
                break;
            }
        } else if (c->pattern->type == TPMCPATTERNVALUE_TYPE_CHARACTER) {
            // ... more nesting ...
        }
        // ... 6 more pattern types ...
    }
}
```

**Proposed solution:** Extract to a helper function:

```c
static bool arcExistsForPattern(TpmcArcArray *arcs, TpmcPattern *pattern) {
    for (Index i = 0; i < arcs->size; i++) {
        TpmcArc *existing = arcs->entries[i];
        if (patternsAreEquivalentForArc(pattern, existing->test)) {
            return true;
        }
    }
    return false;
}

static bool patternsAreEquivalentForArc(TpmcPattern *a, TpmcPattern *b) {
    if (a->pattern->type != b->pattern->type) return false;
    
    switch (a->pattern->type) {
    case TPMCPATTERNVALUE_TYPE_CONSTRUCTOR:
        return a->pattern->val.constructor->tag == b->pattern->val.constructor->tag;
    case TPMCPATTERNVALUE_TYPE_CHARACTER:
        return a->pattern->val.character == b->pattern->val.character;
    case TPMCPATTERNVALUE_TYPE_BIGINTEGER:
        return cmpMaybeBigInt(a->pattern->val.bigInteger, 
                             b->pattern->val.bigInteger) == 0;
    case TPMCPATTERNVALUE_TYPE_TUPLE:
        return true;  // All tuples of same arity are equivalent
    case TPMCPATTERNVALUE_TYPE_COMPARISON:
        return tpmcPatternEq(a, b);
    default:
        return false;
    }
}
```

**Benefits:**

- Reduces `mixture()` complexity
- Makes the arc-existence logic testable in isolation
- Clarifies intent with descriptive function name

---

## 2. Introduce a Context Struct (Medium Priority)

**Problem:** Several functions pass the same 6-8 parameters repeatedly:

- `errorState`
- `knownStates`
- `stateTable`
- `unsafe`
- `testedPaths`
- `I` (ParserInfo)

**Current signatures:**

```c
static TpmcState *match(TpmcMatrix *matrix, TpmcStateArray *finalStates,
                        TpmcState *errorState, TpmcStateArray *knownStates,
                        TpmcStateTable *stateTable, bool *unsafe,
                        SymbolSet *testedPaths, ParserInfo I);

static TpmcState *mixture(TpmcMatrix *M, TpmcStateArray *finalStates,
                          TpmcState *errorState, TpmcStateArray *knownStates,
                          TpmcStateTable *stateTable, bool *unsafe,
                          SymbolSet *testedPaths, ParserInfo I);
```

**Proposed solution:** Bundle into a context struct:

```c
typedef struct TpmcMatchContext {
    TpmcState *errorState;
    TpmcStateArray *knownStates;
    TpmcStateTable *stateTable;
    bool *unsafe;
    SymbolSet *testedPaths;
    ParserInfo parserInfo;
} TpmcMatchContext;
```

**New signatures:**

```c
static TpmcState *match(TpmcMatrix *matrix, TpmcStateArray *finalStates,
                        TpmcMatchContext *ctx);

static TpmcState *mixture(TpmcMatrix *M, TpmcStateArray *finalStates,
                          TpmcMatchContext *ctx);
```

**Benefits:**

- Shorter function signatures
- Easier to add new context fields later
- Groups related state together
- Reduces chance of parameter ordering mistakes

**Considerations:**

- Requires updating all internal functions
- Context struct would be stack-allocated in `tpmcMatch()` entry point

---

## 3. Split mixture() into Smaller Functions (High Priority)

**Problem:** `mixture()` is ~130 lines doing multiple distinct tasks:

1. Column selection and extraction
2. Building constructor arcs (main loop)
3. Exhaustiveness checking
4. Building default/error arc

**Proposed solution:** Extract into focused functions:

```c
static TpmcState *mixture(TpmcMatrix *M, TpmcStateArray *finalStates,
                          TpmcMatchContext *ctx) {
    int col = findFirstActionableColumn(M, ctx->testedPaths);
    TpmcPatternArray *selectedColumn = extractMatrixColumn(col, M);
    TpmcMatrix *remainingColumns = discardMatrixColumn(col, M);
    
    TpmcState *testState = makeEmptyTestState(selectedColumn->entries[0]->path);
    setSymbolSet(ctx->testedPaths, selectedColumn->entries[0]->path);
    
    createConstructorArcs(testState, selectedColumn, remainingColumns, 
                          finalStates, ctx);
    
    createDefaultArcIfNeeded(testState, selectedColumn, remainingColumns,
                             finalStates, ctx);
    
    return deduplicateState(testState, ctx->knownStates, ctx->stateTable);
}

static void createConstructorArcs(TpmcState *testState,
                                  TpmcPatternArray *selectedColumn,
                                  TpmcMatrix *remainingColumns,
                                  TpmcStateArray *finalStates,
                                  TpmcMatchContext *ctx);

static void createDefaultArcIfNeeded(TpmcState *testState,
                                     TpmcPatternArray *selectedColumn,
                                     TpmcMatrix *remainingColumns,
                                     TpmcStateArray *finalStates,
                                     TpmcMatchContext *ctx);
```

**Benefits:**

- Each function has a single responsibility
- Easier to understand control flow
- Better for debugging (can set breakpoints on specific phases)

---

## 4. Rename Single-Letter Variables (Easy Win)

**Problem:** Variables named `N`, `M`, `MN` come from the Pettersson paper but are cryptic without that context.

**Current names:**

- `M` - the pattern matrix
- `N` - the selected column
- `MN` - the matrix minus the selected column

**Proposed names:**

- `patternMatrix` or just `matrix` (already used in some places)
- `selectedColumn`
- `remainingColumns`

**Also consider:**

- `c` → `candidatePattern` or `currentPattern`
- `I` → `parserInfo` (already partially done with context struct)

**Benefits:**

- Self-documenting code
- Reduces need to reference the paper for understanding

---

## 5. Add Section Comments (Low Effort, Medium Value)

**Problem:** The file has distinct logical sections but no visual separation.

**Proposed solution:** Add banner comments:

```c
// ============================================================================
// Matrix Operations
// ============================================================================

static TpmcPatternArray *extractMatrixColumn(int x, TpmcMatrix *matrix) { ... }
static TpmcMatrix *discardMatrixColumn(Index column, TpmcMatrix *matrix) { ... }
static TpmcMatrix *extractMatrixRows(IntArray *indices, TpmcMatrix *matrix) { ... }
static TpmcMatrix *appendMatrices(TpmcMatrix *prefix, TpmcMatrix *suffix) { ... }

// ============================================================================
// Pattern Operations  
// ============================================================================

static bool patternIsWildCard(TpmcPattern *pattern) { ... }
static bool patternIsComparison(TpmcPattern *pattern) { ... }
static bool patternMatches(TpmcPattern *constructor, TpmcPattern *pattern) { ... }
// ...

// ============================================================================
// State and Arc Operations
// ============================================================================

static TpmcState *makeEmptyTestState(HashSymbol *path) { ... }
static TpmcArc *makeTpmcArc(TpmcPattern *pattern, TpmcState *state) { ... }
// ...

// ============================================================================
// Core Algorithm (Pettersson 1992)
// ============================================================================

static TpmcState *match(...) { ... }
static TpmcState *mixture(...) { ... }
```

**Alternative:** Split into multiple files (`tpmc_matrix.c`, `tpmc_pattern.c`, etc.) - but this may be overkill for ~1000 lines.

---

## 6. Pattern Operation Dispatch Table (Low Priority)

**Problem:** Multiple `switch (pattern->pattern->type)` blocks repeat similar logic for each pattern type.

**Locations:**

- `patternMatches()` - lines 203-266
- `makeSubPatternMatrix()` - lines 453-490
- `arityOf()` - lines 378-390
- `replacePatternComponentsWithWildCards()` - lines 509-551

**Proposed solution:** Create a dispatch table:

```c
typedef struct PatternTypeOps {
    int (*getArity)(TpmcPattern *);
    bool (*matchesPattern)(TpmcPattern *constructor, TpmcPattern *pattern);
    void (*populateMatrixRow)(TpmcMatrix *, int y, int arity, 
                              TpmcPattern *, ParserInfo);
    TpmcPattern *(*replaceComponents)(TpmcPattern *);
} PatternTypeOps;

static PatternTypeOps patternOps[] = {
    [TPMCPATTERNVALUE_TYPE_CONSTRUCTOR] = {
        .getArity = constructorArity,
        .matchesPattern = constructorMatches,
        .populateMatrixRow = populateConstructorRow,
        .replaceComponents = replaceConstructorComponents,
    },
    [TPMCPATTERNVALUE_TYPE_TUPLE] = { ... },
    // ...
};
```

**Benefits:**

- Centralizes pattern-type-specific behavior
- Adding new pattern types only requires updating one place
- More extensible design

**Considerations:**

- Adds indirection
- May be over-engineering for current needs
- Some operations don't apply to all types (need NULL checks or no-op defaults)

---

## 7. Simplify PROTECT/UNPROTECT Patterns (Low Priority)

**Problem:** Many functions have 5-10 PROTECT calls creating visual noise.

**Example from `mixture()`:**

```c
int save = PROTECT(N);
PROTECT(MN);
PROTECT(testState);
// ... later in loop ...
int save2 = PROTECT(indicesMatchingC);
PROTECT(patternsMatchingC);
PROTECT(subPatternsMatchingC);
PROTECT(prefixMatrix);
PROTECT(newMatrix);
PROTECT(newFinalStates);
PROTECT(cPrime);
PROTECT(newState);
PROTECT(arc);
// ...
UNPROTECT(save2);
// ...
UNPROTECT(save);
```

**Possible solutions:**

1. **Scoped protection helper** (if C allows):

```c
#define PROTECTED_SCOPE(save) \
    for (int save = protectionStackTop(), _loop = 1; _loop; UNPROTECT(save), _loop = 0)
```

1. **Batch protection function**:

```c
int protectMany(int count, ...) {
    va_list args;
    va_start(args, count);
    int save = -1;
    for (int i = 0; i < count; i++) {
        void *obj = va_arg(args, void *);
        if (i == 0) save = PROTECT(obj);
        else PROTECT(obj);
    }
    va_end(args);
    return save;
}
```

1. **Accept current pattern** - it's explicit and matches the rest of the codebase.

**Recommendation:** This is low priority. The current pattern is verbose but clear and consistent with the rest of the project.

---

## Implementation Order

Suggested order based on effort/value ratio:

1. **Rename variables** (Easy, immediate clarity improvement)
2. **Extract duplicate arc detection** (Medium effort, high value)
3. **Add section comments** (Easy, helps navigation)
4. **Split mixture()** (Medium effort, high value)
5. **Introduce context struct** (Medium effort, simplifies signatures)
6. **Dispatch table** (Higher effort, consider if adding new pattern types)
7. **PROTECT simplification** (Low priority, consider deferring)

---

## Notes

- All refactorings should maintain the existing test suite passing
- Consider doing one refactoring at a time with a commit after each
- The Pettersson paper reference in comments should be preserved for algorithm understanding
