# CEKF End-to-End Test Expansion Plan

## Executive Summary

This document proposes a comprehensive expansion of the F♮ language test suite based on systematic analysis of existing tests against wiki documentation. The analysis identified **23 feature areas** with varying levels of coverage, revealing significant gaps in error handling, builtin function testing, edge cases, and complex feature interactions.

## Current State

- **56 existing test files** in `tests/fn/`
- Strong coverage: Pattern matching, type checking, operators, imports, closures, amb/backtracking
- Weak coverage: Error conditions, untested builtins, string edge cases, namespace complexity
- Missing coverage: Several documented builtins, library functions, and feature interactions

## Coverage Gaps by Severity

### Critical Gaps (Tier 1)

Features that are documented, widely used, but have insufficient or missing tests:

1. **Error Handling & Unsafe Operations**
   - Division by zero (rational vs float vs int)
   - Empty list operations (`<[]`, `>[]`)
   - Non-exhaustive pattern matching enforcement
   - File I/O failure cases
   - Type mismatch error messages

2. **Untested Builtins** (documented but no test coverage)
   - `rand()` - random number generation
   - `argv()` - command-line argument access
   - `getenv()` - environment variable access  
   - `ftype()` - file type detection
   - `opendir()`, `readdir()`, `closedir()` - directory operations

3. **Undertested Library Functions**
   - `amb.some_of()` - subset generation
   - `amb.integers_from()`, `amb.integers_between()` - infinite/range generators
   - `list.unique()` - duplicate removal
   - `list.sortBy()` with custom comparator - only default sort tested
   - Library exported operators (`_except_`, `_|>_`, `$_`, etc.)

### Important Gaps (Tier 2)

Features with basic tests but missing edge cases and comprehensive scenarios:

1. **Arithmetic Edge Cases**
   - Float arithmetic and precision
   - Mixed-type coercion (rational+float, complex+float)
   - Overflow/underflow boundaries
   - Rational simplification verification
   - Extreme value handling (BigInt limits, float infinity)

2. **String & Character Completeness**
   - All escape sequences (`\t`, `\r`, `\\`, `\"`, `\'`, `\0`)
   - Multi-byte UTF-8 edge cases (surrogate pairs, invalid sequences)
   - Unicode normalization (multiple representations of same character)
   - String indexing with multi-byte characters
   - Character literal edge cases

3. **Type System Depth**
   - Named field constructors (`person{ name: "x", age: 30 }`)
   - Named field pattern matching (`person{ name: n, age: _ }`)
   - Mutual recursive types (A references B references A)
   - Complex generic types (multiple parameters, deep nesting)
   - Type constructor namespacing
   - `typeof` with complex expressions beyond simple variables

4. **Control Flow Completeness**
   - `else if` chain testing (not just binary if/else)
   - Type agreement enforcement in conditional branches
   - Deep nested conditionals
   - Switch statement with complex patterns and guards

5. **Pattern Matching Depth**
   - Deep nested patterns (`[[[x]]]`, `#(#(a, b), #(c, d))`)
   - Tuple destructuring in function parameters
   - Complex pseudo-unification (multiple repeated variables)
   - Pattern matching in let bindings beyond simple cases
   - Named field patterns in user typedefs

6. **Comparison & Ordering**
   - User-defined type ordering (constructor precedence)
   - Function comparison (bytecode address ordering)
   - Mixed type comparisons
   - Complex number tie-breaking edge cases
   - Nested structure comparison

### Enhancement Gaps (Tier 3)

Advanced scenarios and feature interactions for robustness:

1. **Closure & Higher-Order Function Patterns**
    - Deep closure nesting (3+ levels)
    - Closures that "modify" captured state via shadowing
    - Function composition, piping, partial application patterns
    - Function equality testing

2. **Currying & Over-Application Edge Cases**
    - Functions with 0, 1, 5, 10+ arguments
    - Mixed partial application and over-application
    - Over-applying to non-function error handling
    - Type inference with complex currying chains

3. **Continuation (here) Advanced Usage**
    - Multiple continuation capture
    - Continuations as first-class values (stored, passed)
    - Calling continuations multiple times
    - Nested `here` expressions
    - Error handling patterns with continuations
    - Interaction between continuations and backtracking

4. **Backtracking Complexity**
    - Nested backtracking (3+ levels of `then`)
    - Backtracking exhaustion behavior
    - Large search spaces (performance testing)
    - `then` with different result types
    - Side effects during backtracking (if possible with immutability)

5. **Operator System Completeness**
    - Complex operator patterns (`_[_]` for indexing)
    - Unicode operators (`_⊕_`, `_∘_`)
    - Multi-character word operators (`sqrt_`, `_squared`)
    - Precedence conflict scenarios (same precedence, different associativity)
    - Operator precedence in complex expressions
    - Operators in nested namespace imports

6. **Macro Edge Cases**
    - Multiple lazy arguments
    - Macro hygiene stress tests (variable capture scenarios)
    - Macro recursion (self-calling macros)
    - Macro with side effects and evaluation order
    - Thunk optimization verification (`fn(){a()}` → `a`)

7. **Namespace & Import Complexity**
    - Nested imports (A imports B which imports C)
    - Cyclic import detection
    - Namespace shadowing (local vs imported definitions)
    - Type constructor access across namespaces
    - Print function namespacing
    - Relative vs absolute path resolution

8. **Print System Completeness**
    - Polymorphic print with different type instantiations
    - Generic representation fallback verification
    - Function printing format
    - Complex/rational number format verification
    - Nested user-defined prints (type A's print uses type B's print)

9. **File I/O Robustness**
    - Error handling for all file operations
    - File handle lifecycle and GC interaction
    - Binary I/O testing
    - Large file operations (performance)
    - Nested file operations (multiple open handles)
    - Buffer operations with `openmem`

10. **SQLite Edge Cases**
    - Transaction handling
    - Error conditions (malformed SQL, constraint violations)
    - Complex queries (joins, subqueries)
    - NULL handling
    - Multiple concurrent connections

11. **Tuple Edge Cases**
    - Large tuples (10+ elements)
    - Deep nesting (`#(1, #(2, #(3, 4)))`)
    - Tuple unpacking with wrong arity errors
    - Partial unpacking patterns

12. **List Edge Cases**
    - Very large lists (10000+ elements)
    - Deep nesting (lists of lists of lists)
    - List equality with complex elements
    - Operator precedence (`@` vs `@@` in complex expressions)
    - All listutils functions with empty, single-element, and large lists

13. **Variable & Scoping Edge Cases**
    - Type change through shadowing verification
    - Scope lifetime guarantees
    - Shadowing in deeply nested scopes
    - Mutually recursive definitions in let blocks

14. **Boolean & Logic Edge Cases**
    - All boolean operators with all input combinations
    - Short-circuit with side effects (multiple scenarios)
    - Comparison operator chaining errors
    - XOR/XNOR truth tables

## Proposed Test Files

### Tier 1: Critical (Immediate Priority)

#### `test_error_handling.fn`

- Division by zero (int, rational, float)
- Empty list car/cdr operations
- Type mismatch scenarios
- File operations on non-existent files
- Invalid UTF-8 sequences

#### `test_builtins_rand.fn`

- Basic random number generation
- Range validation (0.0 to 1.0)
- Distribution properties (multiple calls)

#### `test_builtins_argv.fn`

- Access command-line arguments
- Handle missing arguments
- Argument count verification

#### `test_builtins_env.fn`

- Read environment variables
- Handle missing variables
- Common variables (HOME, PATH, etc.)

#### `test_builtins_ftype.fn`

- Detect file types (regular, directory, symlink, etc.)
- Handle non-existent paths
- Error conditions

#### `test_builtins_directory.fn`

- Open, read, close directory operations
- List directory contents
- Error handling (permissions, non-existent)

#### `test_ambutils_complete.fn`

- `some_of()` subset generation
- `integers_from()` infinite sequences with backtracking
- `integers_between()` range generation
- Exported operator testing

#### `test_listutils_complete.fn`

- `unique()` duplicate removal
- `for_each()` iteration
- `sortBy()` with custom comparators
- Exported operators (`_except_`, `_|>_`)
- Edge cases for all functions (empty, single, large)

### Tier 2: Important (High Priority)

#### `test_arithmetic_comprehensive.fn`

- Float arithmetic and precision
- Mixed-type coercion chains
- Rational simplification
- Overflow/underflow boundaries
- BigInt edge cases

#### `test_strings_escapes.fn`

- All escape sequences
- Multi-byte UTF-8 characters
- Unicode normalization
- String indexing with multi-byte chars
- Invalid sequences

#### `test_typedef_named_fields.fn`

- Named field constructor syntax
- Named field pattern matching
- Field access patterns
- Type errors with named fields

#### `test_typedef_advanced.fn`

- Mutual recursive types
- Complex generics (multiple parameters)
- Type constructor currying
- `typeof` with complex expressions

#### `test_control_flow_complete.fn`

- `else if` chains (3+ levels)
- Deep nested conditionals
- Type agreement enforcement
- Switch with complex patterns

#### `test_pattern_matching_deep.fn`

- Deep nested patterns
- Tuple patterns in functions
- Complex pseudo-unification
- Pattern matching in let bindings

#### `test_comparisons_complete.fn`

- User-defined type ordering
- Function comparison
- Mixed type comparisons
- Nested structure comparison
- Complex number tie-breaking edge cases

### Tier 3: Enhancement (Medium Priority)

#### `test_closures_advanced.fn`

- Deep closure nesting (3+ levels)
- Closures with shadowing updates
- Function composition patterns
- Partial application chains

#### `test_currying_complete.fn`

- Large arity functions (5, 10 params)
- Mixed partial and over-application
- Error cases
- Type inference verification

#### `test_continuations_advanced.fn`

- Multiple continuation capture
- Continuation storage and passing
- Calling continuations multiple times
- Nested `here` expressions
- Error handling patterns
- Continuations + backtracking interaction

#### `test_backtracking_advanced.fn`

- Deep nested backtracking
- Exhaustion behavior
- Large search spaces
- Type variations in `then` branches

#### `test_operators_advanced.fn`

- Complex patterns (`_[_]`)
- Unicode operators
- Multi-character operators
- Precedence conflicts
- Complex expression parsing

#### `test_macros_advanced.fn`

- Multiple lazy arguments
- Hygiene edge cases
- Macro recursion
- Side effect ordering
- Thunk optimization verification

#### `test_namespaces_advanced.fn`

- Nested imports (3+ levels)
- Cyclic import detection
- Shadowing scenarios
- Type constructor namespacing
- Print function namespacing

#### `test_print_complete.fn`

- Polymorphic printing
- Generic representation fallback
- Function printing
- Number format verification
- Nested user-defined prints

#### `test_file_io_complete.fn`

- All error conditions
- GC and file handle lifecycle
- Binary I/O
- Large files
- Nested operations

#### `test_sqlite_advanced.fn`

- Transactions
- Error handling
- Complex queries
- NULL values
- Concurrent connections

#### `test_tuples_complete.fn`

- Large tuples (10+ elements)
- Deep nesting
- Arity mismatch errors
- Pattern matching edge cases

#### `test_lists_complete.fn`

- Very large lists (10000+ elements)
- Deep nesting
- Complex equality
- Operator precedence chains

#### `test_scoping_complete.fn`

- Type change via shadowing
- Scope lifetime
- Deep nesting
- Mutual recursion in let

#### `test_logic_complete.fn`

- All boolean operators (truth tables)
- Short-circuit with side effects
- Operator combinations

## Implementation Strategy

### Phase 1: Foundation (Weeks 1-2)

- Implement all Tier 1 tests (critical gaps)
- Focus on error handling and untested builtins
- Establish patterns for test organization

### Phase 2: Completeness (Weeks 3-4)

- Implement Tier 2 tests (important gaps)
- Focus on edge cases for existing features
- Expand arithmetic, string, and type system coverage

### Phase 3: Robustness (Weeks 5-6)

- Implement Tier 3 tests (enhancement gaps)
- Focus on advanced scenarios and interactions
- Performance and stress testing

### Phase 4: Integration (Week 7)

- Review all test results
- Document discovered bugs
- Prioritize bug fixes
- Update wiki documentation where gaps found

## Success Metrics

1. **Coverage**: Every wiki-documented feature has at least one test
2. **Error Handling**: All unsafe operations have error case tests
3. **Builtins**: All documented builtins tested
4. **Libraries**: All exported functions and operators tested
5. **Edge Cases**: Common edge cases (empty, single, large) covered for collections
6. **Interactions**: Major feature combinations tested

## Expected Outcomes

- **40-50 new test files** (from current 56 to ~100-110 total)
- **300-500 new test assertions** (estimated)
- **Bug discovery**: Expect to find 10-20 bugs in edge cases and error handling
- **Documentation gaps**: Identify features that exist but aren't documented
- **Specification clarification**: Areas where behavior is undefined or ambiguous

## Notes

- Tests follow existing conventions (`--assertions-accumulate` mode)
- Each test file focuses on one feature area
- Test names should be descriptive: `assert(condition)` with meaningful conditions
- Unsafe operations should be wrapped in appropriate patterns
- Complex tests should include comments explaining intent

## Appendix: Test Naming Conventions

- `test_<feature>_<aspect>.fn` - General pattern
- `test_<feature>_complete.fn` - Comprehensive coverage of a feature
- `test_<feature>_advanced.fn` - Complex scenarios and edge cases
- `test_<library>_<function>.fn` - Specific library function testing
