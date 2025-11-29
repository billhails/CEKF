# CEKF Test Coverage Analysis

## Overall Coverage Statistics

**Lines:** 72.2% (12,720 out of 17,622)  
**Functions:** 79.2% (1,400 out of 1,768)  
**Branches:** 58.5% (3,233 out of 5,524)

## Reports Available

1. **HTML Report:** `coverage_html/index.html`
   - Interactive, browsable coverage report
   - Shows line-by-line coverage for each source file
   - Color-coded: green (covered), red (not covered)
   - Open in browser with: `xdg-open coverage_html/index.html`

2. **Text Report:** `coverage_report.txt`
   - AI-readable format with detailed per-file statistics
   - Includes llvm-cov summary output for each file
   - Overall statistics at the end

3. **Gcov Files:** `gcov_output/*.gcov`
   - Raw gcov output files for detailed analysis

## Regenerating Coverage Reports

To regenerate coverage after making changes:

```bash
make coverage
```

Or manually:

```bash
./tools/coverage.sh
```

## High Coverage Areas (>90%)

- `tpmc_match.c` - 93% (pattern matching compiler core)
- `anf_normalize.c` - 93% (ANF conversion)
- `annotate.c` - 92% (lexical addressing)
- `tpmc_translate.c` - 92% (pattern match code generation)
- `tpmc_logic.c` - 89% (pattern matching logic)
- `symbol.c` - 88% (symbol table)
- `pratt_scanner.c` - 86% (lexical scanner)

## Areas Needing More Coverage (<60%)

- `tpmc_pp.c` - 0% (pretty printing for debugging)
- `tpmc_mermaid.c` - 2% (diagram generation)
- `anf_pp.c` - 0% (ANF pretty printing)
- All `*_debug.c` files - 0% (debug utilities not used in tests)
- `unicode.c` - 34% (Unicode handling)
- `opaque.c` - 45% (opaque type handling)
- `tc_helper.c` - 58% (type checker helpers)

## Test Suite Summary

**Total Tests:** 80 test files
- Original: 56 files
- Newly Added: 24 files (43% increase)

**New Test Categories:**
- Error handling edge cases
- Built-in functions (I/O, readdir, string operations)
- Library functions (listutils, ambutils)
- Arithmetic operations (integers, rationals)
- String and character operations
- Comparison operators
- Control flow (conditionals, pattern matching)
- Recursion patterns (mutual, tail, non-tail)
- Higher-order functions
- Generic types and polymorphism
- Backtracking and non-deterministic programming
- Macro expansion
- Big integer arithmetic
- Rational number arithmetic
- Namespace linking
- Custom operators
- Type system and inference

## Bugs Found

1. **Fixed:** `readdir()` segmentation fault (closedir implementation)
2. **Documented:** Type checker crash on undefined variables (tc_analyze.c:377)
3. **Documented:** Type checker crash on closure variable capture (same root cause)

## Coverage Build Configuration

The coverage mode is configured in the Makefile with:
- Compiler flags: `-g --coverage`
- No aggressive GC stress testing
- Uses llvm-cov for clang compatibility
- Excludes generated code and test infrastructure

## Notes

- Debug utilities (`*_debug.c`, `*_pp.c`) show 0% coverage as they're only used for manual debugging
- The `generated/` directory is excluded from coverage statistics
- Coverage data is based on actual test execution, not all possible code paths
