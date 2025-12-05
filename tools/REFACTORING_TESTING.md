# Code Generation Refactoring Testing

## Overview

This directory contains testing infrastructure for the code generation refactoring project. The goal is to ensure that refactored code generators produce **identical output** to the original generators.

## Quick Start

### 1. Establish Baseline (Before Any Changes)

```bash
# Generate baseline snapshot of current code generation
make establish-baseline
```

This will:
- Clean and regenerate all code
- Copy all generated files to `test_baseline/generated/`
- Create checksums for verification
- Record the current git commit

### 2. Make Your Changes

Edit files in `tools/generate/` as needed.

### 3. Test Your Changes

```bash
# Run the refactoring test
make test-refactoring
```

This will:
- Regenerate all code with your changes
- Compare byte-for-byte with baseline
- Run the full test suite
- Report any differences

### 4. Iterate

If tests fail:
- Review the differences: `diff -u test_baseline/generated/ast.h generated/ast.h`
- Fix your code
- Re-run `make test-refactoring`

If differences are **intentional** (e.g., you improved comments):
- Document the change
- Update baseline: `make update-baseline`

## Testing Workflow

```
┌─────────────────────────────────────┐
│  make establish-baseline            │  ← Do this ONCE before starting
│  (creates test_baseline/)           │
└────────────────┬────────────────────┘
                 │
                 v
┌─────────────────────────────────────┐
│  Edit tools/generate/*.py           │
└────────────────┬────────────────────┘
                 │
                 v
┌─────────────────────────────────────┐
│  make test-refactoring              │
└────────────────┬────────────────────┘
                 │
         ┌───────┴───────┐
         │               │
         v               v
    ✓ PASS          ✗ FAIL
         │               │
         │               v
         │      ┌─────────────────┐
         │      │ Investigate:    │
         │      │ - View diffs    │
         │      │ - Fix code      │
         │      │ - Retry         │
         │      └─────────────────┘
         │               │
         └───────┬───────┘
                 │
                 v
           Done! Commit changes
```

## Files in test_baseline/

After running `make establish-baseline`:

```
test_baseline/
├── generated/          # Snapshot of all generated C files
│   ├── ast.h
│   ├── ast.c
│   ├── ast_debug.h
│   ├── ast_debug.c
│   ├── lambda.h
│   └── ...
├── src/               # Copy of YAML schemas
│   ├── ast.yaml
│   ├── lambda.yaml
│   └── ...
├── checksums.txt      # MD5 sums for verification
├── commit.txt         # Git commit when baseline was created
└── timestamp.txt      # When baseline was created
```

## The test_refactoring.sh Script

Located in `tools/test_refactoring.sh`, this script:

1. **Cleans and regenerates** all code
2. **Compares** each generated file with baseline using `diff -w -B` (ignoring whitespace)
3. **Runs** the full test suite to ensure functionality
4. **Reports** success or failure with details

Usage:
```bash
# Run with default settings
./tools/test_refactoring.sh

# Use custom baseline directory
BASELINE_DIR=my_baseline ./tools/test_refactoring.sh
```

## Handling Differences

### Unintentional Differences (Bugs)

If the test reports differences you didn't expect:

```bash
# View the diff
diff -u test_baseline/generated/ast.h generated/ast.h

# Or use your favorite diff tool
meld test_baseline/generated/ast.h generated/ast.h

# Fix the bug in your code
vim tools/generate/structs.py

# Re-test
make test-refactoring
```

### Intentional Differences (Improvements)

If you **intentionally** changed the output (e.g., better formatting, improved comments):

1. **Document** what changed and why
2. **Review** the diff carefully to ensure it's correct
3. **Update** the baseline:

```bash
# Update baseline with current output
make update-baseline
```

**Important**: Only update the baseline if you're certain the new output is correct!

### Whitespace-Only Differences

The test script uses `diff -w -B` which ignores:
- Whitespace differences (`-w`)
- Blank line differences (`-B`)

If you have legitimate whitespace changes, they won't fail the test.

## Testing Strategy by Phase

### Phase 1: Infrastructure (No Changes Expected)

When adding new modules (`codegen.py`, `templates.py`, etc.) that aren't used yet:

```bash
make test-refactoring  # Should pass - no changes to output
```

### Phase 2: Migrating One Entity Type

When refactoring `Primitive`:

```bash
# 1. Test before changes
make test-refactoring  # Baseline check

# 2. Make changes to Primitive

# 3. Test just primitives.yaml output
python3 tools/generate.py src/primitives.yaml h | diff - test_baseline/generated/primitives.h

# 4. If good, test everything
make test-refactoring
```

### Phase 3: Migrating All Entities

Repeat the Phase 2 process for each entity type, testing after each migration.

## Debugging Failed Tests

When `make test-refactoring` fails:

### 1. Identify Which Files Differ

The script will show you which files have differences.

### 2. Examine the Diff

```bash
# Unified diff format
diff -u test_baseline/generated/ast.h generated/ast.h

# Side-by-side comparison
diff -y test_baseline/generated/ast.h generated/ast.h | less

# Visual diff tool
meld test_baseline/generated/ast.h generated/ast.h
```

### 3. Determine Root Cause

Common causes:
- **Logic error** in refactored code
- **Missing case** in generator
- **Wrong template** being used
- **Field ordering** changed unintentionally

### 4. Test in Isolation

```bash
# Test just one YAML file
python3 tools/generate.py src/ast.yaml h > /tmp/ast.h
diff -u test_baseline/generated/ast.h /tmp/ast.h

# Add debug output
DEBUG=1 python3 tools/generate.py src/ast.yaml h
```

### 5. Bisect the Change

If you made multiple changes, bisect to find which one caused the issue:

```bash
git stash                    # Save current changes
make test-refactoring        # Should pass

git stash pop                # Restore changes
# Manually revert half your changes
make test-refactoring        # Pass or fail?
# Binary search to find the problematic change
```

## Continuous Integration

The testing should be integrated into CI:

```yaml
# .github/workflows/test-refactoring.yml
name: Test Code Generation

on: [push, pull_request]

jobs:
  test-codegen:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Establish baseline
        run: make establish-baseline
      
      - name: Test refactoring
        run: make test-refactoring
```

## FAQ

### Q: Do I commit test_baseline/ to git?

**A**: That depends:
- **Yes** if you want to track the "golden master" in version control
- **No** if it's just a local testing artifact

If you commit it, other developers can verify their changes against the same baseline.

### Q: What if the Makefile changes break the test?

**A**: Test before and after Makefile changes:
```bash
make establish-baseline     # Before Makefile changes
# Edit Makefile
make test-refactoring       # Should still work
```

### Q: Can I have multiple baselines?

**A**: Yes:
```bash
BASELINE_DIR=baseline_v1 make establish-baseline
BASELINE_DIR=baseline_v2 make establish-baseline

# Test against specific baseline
BASELINE_DIR=baseline_v1 make test-refactoring
```

### Q: What if I want to test intermediate states?

**A**: Create checkpoints:
```bash
# After Phase 1
make establish-baseline
mv test_baseline test_baseline_phase1

# After Phase 2
make establish-baseline
mv test_baseline test_baseline_phase2

# Compare phases
diff -r test_baseline_phase1/generated test_baseline_phase2/generated
```

### Q: The test passes but `make test` fails. Why?

**A**: The generated code might be **syntactically identical** but **semantically different**. This suggests a bug in the refactored generators. Investigate:
- Runtime behavior changes
- Memory layout changes
- GC interaction changes

## Rollback Procedure

If you need to abort the refactoring:

```bash
# 1. Verify baseline is still valid
BASELINE_DIR=test_baseline make test-refactoring

# 2. Revert all code generation changes
git checkout HEAD -- tools/generate/

# 3. Verify everything works
make clean
make test
make test-refactoring  # Should pass

# 4. Remove baseline if desired
make clean-baseline
```

## Best Practices

1. **Establish baseline immediately** before making any changes
2. **Test frequently** - after each logical change
3. **Keep changes small** - easier to debug failures
4. **Document intentional differences** before updating baseline
5. **Never update baseline** without understanding why output changed
6. **Commit working states** - easier to bisect problems
7. **Run full test suite** - not just the refactoring test

## Advanced: Comparing Semantics, Not Syntax

If you want to allow formatting changes but ensure semantic equivalence:

```python
# tools/semantic_compare.py
import re
import sys

def normalize(code):
    """Normalize C code for semantic comparison"""
    # Remove all comments
    code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)
    code = re.sub(r'//.*$', '', code, flags=re.MULTILINE)
    # Remove all whitespace
    code = re.sub(r'\s+', ' ', code)
    # Normalize braces
    code = code.replace(' {', '{').replace('{ ', '{')
    return code.strip()

# Usage: python3 tools/semantic_compare.py file1.h file2.h
if normalize(open(sys.argv[1]).read()) == normalize(open(sys.argv[2]).read()):
    sys.exit(0)
else:
    sys.exit(1)
```

## Summary

The testing infrastructure provides:
- ✓ **Confidence** that refactoring doesn't break anything
- ✓ **Fast feedback** when something does break
- ✓ **Clear process** for handling intentional changes
- ✓ **Rollback capability** if needed
- ✓ **Documentation** of what changed and when

Follow the workflow, test frequently, and the refactoring will be safe and successful!
