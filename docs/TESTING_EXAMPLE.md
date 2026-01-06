# Example: Testing Your First Refactoring Change

This document walks through a concrete example of using the testing infrastructure for the first small refactoring change.

## Scenario: Extract Common Documentation Generator

Let's say we want to extract the documentation comment generation that's duplicated across entity types.

## Step-by-Step Process

### Step 1: Establish Baseline (If Not Done)

```bash
$ cd /home/bill/src/CEKF
$ make establish-baseline

Establishing baseline for code generation testing...
WARNING: This will regenerate all code and create a snapshot.
Press Ctrl-C to cancel, or Enter to continue...
[Press Enter]

Cleaning...
Regenerating code...
Baseline established in test_baseline/
Commit: 1a2b3c4d
Files: 84

$ ls test_baseline/
checksums.txt  commit.txt  generated/  src/  timestamp.txt
```

**What happened:**

- All code was regenerated from scratch
- Copied to `test_baseline/generated/`
- Checksums computed for verification
- Current git commit recorded

### Step 2: Verify Baseline is Good

```bash
$ make test-refactoring

=== Testing Code Generation Refactoring ===
Baseline: test_baseline
Current:  generated/

Cleaning...
Regenerating code...
Comparing generated code...
✓ ast.h
✓ ast.c
✓ ast_debug.h
✓ ast_debug.c
✓ lambda.h
... (80 more files)

Checked 84 files
✓ All generated files match baseline!

Running test suite...
✓ All tests pass!

=== Refactoring test PASSED ===
```

**Good!** Baseline test passes. We can proceed.

### Step 3: Make a Small Change

Let's extract the documentation generator to `tools/generate/docgen.py`:

```bash
vim tools/generate/docgen.py
```

```python
# tools/generate/docgen.py - NEW FILE

"""
Documentation generation utilities for code generation.
"""

class DocGenerator:
    """Generates C-style documentation comments"""
    
    @staticmethod
    def function_doc(entity, purpose):
        """Generate documentation for a function"""
        lines = ["/**"]
        if hasattr(entity, 'brief') and entity.brief:
            lines.append(f" * @brief {entity.brief}")
        if hasattr(entity, 'description') and entity.description:
            lines.append(" *")
            for line in DocGenerator._wrap_text(entity.description, 70):
                lines.append(f" * {line}")
        if purpose:
            lines.append(" *")
            lines.append(f" * {purpose}")
        lines.append(" */")
        return '\n'.join(lines)
    
    @staticmethod
    def _wrap_text(text, width):
        """Wrap text to specified width"""
        words = text.split()
        lines = []
        current = ""
        for word in words:
            if len(current) + len(word) + 1 > width:
                lines.append(current.strip())
                current = word + " "
            else:
                current += word + " "
        if current:
            lines.append(current.strip())
        return lines
```

Save the file.

### Step 4: Test Immediately (Before Using It)

```bash
$ make test-refactoring

=== Testing Code Generation Refactoring ===
...
✓ All generated files match baseline!
✓ All tests pass!
=== Refactoring test PASSED ===
```

**Good!** Adding a new file that's not used yet doesn't change output.

### Step 5: Use the New Module in One Place

Let's refactor just the `SimpleStruct` class to use it:

```bash
vim tools/generate/structs.py
```

```python
# At top of file, add import:
from .docgen import DocGenerator

# Find the printNewFunction method (around line 201)
# Change from:
def printNewFunction(self, catalog):
    c = self.comment('printNewFunction')
    decl = self.getNewSignature(catalog)
    print("/**")
    print(f" * Creates a new {self.getName()} struct with the given arguments.")
    print(" */")
    print(f"{decl} {{ {c}")
    # ... rest of function

# To:
def printNewFunction(self, catalog):
    c = self.comment('printNewFunction')
    decl = self.getNewSignature(catalog)
    doc = DocGenerator.function_doc(
        self, 
        f"Creates a new {self.getName()} struct with the given arguments."
    )
    print(doc)
    print(f"{decl} {{ {c}")
    # ... rest of function
```

Save the file.

### Step 6: Test the Change

```bash
$ make test-refactoring

=== Testing Code Generation Refactoring ===
Cleaning...
Regenerating code...
Comparing generated code...
✓ ast.h
✓ ast.c
✓ ast_debug.h
✓ ast_debug.c
... (all files)

✓ All generated files match baseline!
✓ All tests pass!
=== Refactoring test PASSED ===
```

**Success!** The refactored code generates identical output.

### Step 7: Commit the Change

```bash
$ git add tools/generate/docgen.py tools/generate/structs.py
$ git commit -m "Refactor: extract documentation generation to docgen.py

- Create DocGenerator class for C-style doc comments
- Use in SimpleStruct.printNewFunction()
- Generated code is identical (verified with test-refactoring)"
```

## What If The Test Failed?

Let's simulate a bug. Say we accidentally changed the doc format:

```python
# In docgen.py, change:
lines.append(f" * {purpose}")
# To:
lines.append(f" * Purpose: {purpose}")  # BUG: added "Purpose:"
```

### Testing Catches It

```bash
$ make test-refactoring

=== Testing Code Generation Refactoring ===
Cleaning...
Regenerating code...
Comparing generated code...
✓ ast.h
✗ Difference found in ast.c
  First 20 lines of diff:
  --- test_baseline/generated/ast.c
  +++ generated/ast.c
  @@ -100,7 +100,7 @@
   /**
    * Creates a new AstExpression struct with the given arguments.
  - */
  + * Purpose: Creates a new AstExpression struct with the given arguments.
   AstExpression *newAstExpression(...) {
  ...

=== FAILED: 1 file(s) differ from baseline ===
```

### Debug and Fix

```bash
# View the full diff
$ diff -u test_baseline/generated/ast.c generated/ast.c | less

# Ah! We added "Purpose:" which duplicates the text. Remove it.
$ vim tools/generate/docgen.py
# Fix: remove "Purpose:" prefix

# Re-test
$ make test-refactoring
✓ All generated files match baseline!
=== Refactoring test PASSED ===
```

## Example: Intentional Improvement

Now let's say we WANT to improve the documentation format:

```python
# In docgen.py
def function_doc(entity, purpose):
    """Generate documentation for a function"""
    lines = ["/**"]
    lines.append(f" * @brief {purpose}")  # Always include brief
    if hasattr(entity, 'description') and entity.description:
        lines.append(" *")
        for line in DocGenerator._wrap_text(entity.description, 70):
            lines.append(f" * {line}")
    lines.append(" *")
    lines.append(f" * @returns Pointer to newly allocated {entity.getName()}")  # NEW!
    lines.append(" */")
    return '\n'.join(lines)
```

### Test Shows Intentional Change

```bash
$ make test-refactoring

✗ Difference found in ast.c
  --- test_baseline/generated/ast.c
  +++ generated/ast.c
  @@ -99,6 +99,7 @@
   /**
    * @brief Creates a new AstExpression struct with the given arguments.
    *
  + * @returns Pointer to newly allocated AstExpression
    */
   AstExpression *newAstExpression(...) {

=== FAILED: Multiple files differ from baseline ===
```

### Review and Update Baseline

```bash
# Review all diffs to ensure they're correct
$ diff -u test_baseline/generated/ast.c generated/ast.c | less
$ diff -u test_baseline/generated/lambda.c generated/lambda.c | less
# ... review all changes

# Looks good! Update the baseline
$ make update-baseline

Updating baseline with current generated code...
WARNING: This will replace the baseline. Only do this if you've
verified the new output is correct!
Press Ctrl-C to cancel, or Enter to continue...
[Press Enter]

Baseline updated.

# Verify
$ make test-refactoring
✓ All generated files match baseline!
=== Refactoring test PASSED ===

# Commit both code and baseline
$ git add tools/generate/docgen.py test_baseline/
$ git commit -m "Improve: add @returns to all constructor documentation

- Modified DocGenerator to include return type documentation
- Updated baseline to reflect improved documentation
- All tests pass"
```

## Summary of Workflow

```text
1. make establish-baseline     ← Do once at start
2. Make small change
3. make test-refactoring       ← Do after EVERY change
4. If pass: commit
5. If fail: debug and go to step 3
6. If intentional change: make update-baseline, then commit
7. Repeat from step 2
```

## Tips

- **Start small**: One function, one class, one file at a time
- **Test frequently**: After every change, not at the end
- **Understand diffs**: Don't blindly update baseline
- **Document intentions**: Commit messages should explain changes
- **Keep baseline clean**: Only update when you're certain

## Next Example

After successfully refactoring documentation generation, you could:

- Extract signature generation (similar process)
- Refactor mark function generation
- Create template classes for common patterns

Each time, follow the same workflow:

1. Make change
2. Test
3. Debug or commit
4. Repeat

The testing infrastructure ensures you never break anything!
