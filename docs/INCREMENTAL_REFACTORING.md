# Incremental Refactoring Strategy

## Overview

Yes! Small incremental steps with testing at each stage is the **safest and most recommended** approach. The testing infrastructure is designed specifically for this workflow.

## Why Incremental Works Better

- ✓ **Easier to debug** - if test fails, you know exactly what caused it
- ✓ **Lower risk** - can roll back one small change instead of hours of work
- ✓ **Faster feedback** - test runs take seconds
- ✓ **Natural checkpoints** - commit after each passing test
- ✓ **Less stressful** - steady progress vs. big-bang rewrite

## Recommended Incremental Path

### Phase 0: Infrastructure (No Output Changes)

These steps add new code WITHOUT using it yet, so tests should always pass:

#### Step 0.1: Create utils module for common C code patterns

```bash
# Create tools/generate/c_types.py
vim tools/generate/c_types.py
```

Add simple utilities for C type manipulation:

```python
class CType:
    """Utilities for C type declarations"""
    
    @staticmethod
    def pointer(typename):
        """Return pointer type declaration"""
        return f"{typename} *"
    
    @staticmethod
    def struct_pointer(typename):
        """Return struct pointer type"""
        return f"struct {typename} *"
    
    @staticmethod
    def is_pointer(type_decl):
        """Check if type declaration is a pointer"""
        return '*' in type_decl
```

Test:

```bash
make test-refactoring  # Should PASS - nothing uses this yet
git add tools/generate/c_types.py
git commit -m "Add CType utility class (unused)"
```

#### Step 0.2: Create code generation building blocks

```bash
vim tools/generate/codegen.py
```

Add basic code generation structures (see proposal for full code):

```python
class Statement:
    """Represents a C statement"""
    def __init__(self, code):
        self.code = code
    
    def render(self, indent=0):
        return "    " * indent + self.code

class FunctionBody:
    """Represents a function body"""
    def __init__(self, statements):
        self.statements = statements
    
    def render(self, indent=1):
        for stmt in self.statements:
            if isinstance(stmt, str):
                print("    " * indent + stmt)
            else:
                stmt.render(indent)
```

Test:

```bash
make test-refactoring  # Should PASS - still unused
git add tools/generate/codegen.py
git commit -m "Add basic code generation classes (unused)"
```

#### Step 0.3: Create template utilities

```bash
vim tools/generate/templates.py
```

Add comment/documentation generators:

```python
class DocComment:
    """Generate C documentation comments"""
    
    @staticmethod
    def brief(text):
        return f"/** @brief {text} */"
    
    @staticmethod
    def function(brief=None, description=None, params=None, returns=None):
        lines = ["/**"]
        if brief:
            lines.append(f" * @brief {brief}")
        if description:
            lines.append(" *")
            for line in description.split('\n'):
                lines.append(f" * {line}")
        if params:
            lines.append(" *")
            for name, desc in params.items():
                lines.append(f" * @param {name} {desc}")
        if returns:
            lines.append(" *")
            lines.append(f" * @returns {returns}")
        lines.append(" */")
        return '\n'.join(lines)
```

Test:

```bash
make test-refactoring  # Should PASS
git add tools/generate/templates.py
git commit -m "Add documentation template generators (unused)"
```

**After Phase 0**: You have infrastructure in place, all tests pass, no risk.

---

### Phase 1: Refactor One Method in One Class

Pick the **simplest** entity and the **simplest** method. Start with `Primitive`.

#### Step 1.1: Refactor Primitive.printNewDeclaration()

```bash
vim tools/generate/primitives.py
```

Find this method (around line 50):

```python
def printNewDeclaration(self, catalog):
    # Current implementation
    pass
```

Change to use new templates (MINIMAL change):

```python
def printNewDeclaration(self, catalog):
    from .templates import DocComment
    # Rest stays the same for now
    pass
```

Test:

```bash
make test-refactoring  # Should PASS - just added import
```

Now actually use it:

```python
def printNewDeclaration(self, catalog):
    from .templates import DocComment
    # Use DocComment.brief() instead of raw print
    # Keep everything else identical
```

Test:

```bash
make test-refactoring  # Should PASS - output identical
git add tools/generate/primitives.py
git commit -m "Refactor Primitive.printNewDeclaration to use DocComment"
```

#### Step 1.2: Refactor Primitive.printNewFunction()

Same pattern - change one method, test, commit.

```bash
vim tools/generate/primitives.py
# Refactor printNewFunction
make test-refactoring
git commit -m "Refactor Primitive.printNewFunction"
```

#### Step 1.3: Continue with other Primitive methods

One method at a time:

- `printMarkDeclaration()` → test → commit
- `printMarkFunction()` → test → commit
- `printFreeDeclaration()` → test → commit
- etc.

**Each step takes 2-5 minutes. Each commit is safe.**

---

### Phase 2: Refactor Same Method Across All Classes

Once one method is refactored in one class, apply the pattern to other classes.

#### Step 2.1: Extract common pattern from printNewDeclaration

After refactoring `Primitive.printNewDeclaration()`, you'll see a pattern.
Extract it:

```bash
vim tools/generate/common.py
```

```python
class CommonGenerators:
    """Common generation patterns shared across entity types"""
    
    @staticmethod
    def new_declaration(entity, catalog):
        """Generate new*() declaration for any entity"""
        sig = entity.getNewSignature(catalog)
        return f"{sig};"
```

#### Step 2.2: Use in SimpleStruct

```bash
vim tools/generate/structs.py
```

```python
def printNewDeclaration(self, catalog):
    from .common import CommonGenerators
    print(CommonGenerators.new_declaration(self, catalog))
```

Test:

```bash
make test-refactoring  # Should PASS
git commit -m "Refactor SimpleStruct.printNewDeclaration to use common pattern"
```

#### Step 2.3: Repeat for other entity types

One class at a time:

- SimpleArray → test → commit
- SimpleHash → test → commit  
- SimpleVector → test → commit
- etc.

---

### Phase 3: Introduce Capabilities (Carefully)

This is bigger change, but still incremental.

#### Step 3.1: Create capability interface

```bash
vim tools/generate/capabilities.py
```

```python
class Constructible:
    """Entity that can be constructed with new()"""
    
    def get_new_signature(self, catalog):
        """Return signature for new*() function"""
        raise NotImplementedError
    
    def get_initialization_statements(self, catalog):
        """Return list of initialization statements"""
        raise NotImplementedError
```

Test:

```bash
make test-refactoring  # Should PASS - not used yet
git commit -m "Add Constructible capability interface (unused)"
```

#### Step 3.2: Make ONE class implement it

```bash
vim tools/generate/primitives.py
```

```python
from .capabilities import Constructible

class Primitive(Base, Constructible):  # Add Constructible
    # Implement required methods
    def get_new_signature(self, catalog):
        # Move logic from printNewDeclaration here
        pass
    
    def get_initialization_statements(self, catalog):
        # Move logic from printNewFunction here
        pass
```

Test:

```bash
make test-refactoring  # Should PASS
git commit -m "Make Primitive implement Constructible"
```

#### Step 3.3: Gradually migrate other classes

One at a time:

- SimpleEnum → test → commit
- SimpleStruct → test → commit
- etc.

---

## Practical Example: Complete First Step

Here's exactly what to do RIGHT NOW for your first refactoring step:

### Create First New Module

```bash
cd /home/bill/src/CEKF
vim tools/generate/comment_gen.py
```

Paste this:

```python
"""
Simple comment and documentation utilities.
This is a minimal first refactoring - just extract comment generation.
"""

class CommentGen:
    """Generate C-style comments and documentation"""
    
    @staticmethod
    def line_comment(text):
        """Generate a single-line // comment"""
        return f"// {text}"
    
    @staticmethod
    def class_comment(entity_type, method_name):
        """Generate standard class.method comment"""
        return f"// {entity_type}.{method_name}"
```

Test it:

```bash
make test-refactoring
```

Expected output:

```text
=== Testing Code Generation Refactoring ===
...
✓ All generated files match baseline!
✓ All tests pass!
=== Refactoring test PASSED ===
```

**Why it passes**: You created a new file but didn't use it yet.

Commit it:

```bash
git add tools/generate/comment_gen.py
git commit -m "Add CommentGen utility class (first refactoring step, unused)"
```

### Use It In One Place

```bash
vim tools/generate/primitives.py
```

Find the `comment()` method (around line 100):

```python
def comment(self, method):
    return f'// Primitive.{method}'
```

Change to:

```python
def comment(self, method):
    from .comment_gen import CommentGen
    return CommentGen.class_comment('Primitive', method)
```

Test:

```bash
make test-refactoring
```

Expected output:

```text
✓ All generated files match baseline!
✓ All tests pass!
=== Refactoring test PASSED ===
```

**Why it passes**: The output is identical, just generated differently.

Commit:

```bash
git add tools/generate/primitives.py
git commit -m "Use CommentGen in Primitive.comment() - output identical"
```

### Expand to Other Classes

```bash
# Edit each class one at a time
vim tools/generate/structs.py    # Change SimpleStruct.comment()
make test-refactoring             # Test
git commit -m "..."               # Commit

vim tools/generate/arrays.py     # Change SimpleArray.comment()
make test-refactoring
git commit -m "..."

# etc.
```

---

## Testing at Each Step

### The Rhythm

Every change follows this rhythm:

1. Edit one file
2. make test-refactoring (15-30 seconds)
3. If ✓ pass: git commit
4. If ✗ fail: debug (diff shows exactly what changed)
5. Repeat

### What "One Step" Means

A single step could be:

- ✓ Add a new utility class (not used yet)
- ✓ Add a new method to existing class
- ✓ Change one method in one class to use new utility
- ✓ Extract common code from 2-3 methods into helper
- ✗ Refactor 5 classes at once (too big!)
- ✗ Change multiple methods simultaneously (hard to debug)

### Sizing Your Steps

**Too small** (inefficient):

- Change one line of code
- Rename a variable

**Too big** (risky):

- Refactor an entire class
- Change multiple files at once
- Introduce new architecture without testing

**Just right** (recommended):

- One method in one class
- Extract one helper function and use it in one place
- Add capability interface and make ONE class implement it

---

## Quick Reference Card

Print this and keep it next to you while refactoring:

```text
┌─────────────────────────────────────────────────────────┐
│  INCREMENTAL REFACTORING CHECKLIST                      │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  Before ANY change:                                     │
│    ☐ Working directory is clean (git status)            │
│    ☐ Baseline exists (ls test_baseline/)                │
│    ☐ Tests pass (make test-refactoring)                 │
│                                                         │
│  For EACH change:                                       │
│    ☐ Edit ONE file (or create ONE new file)             │
│    ☐ Make ONE logical change                            │
│    ☐ Run: make test-refactoring                         │
│    ☐ If pass: git commit -m "..."                       │
│    ☐ If fail: debug with diff, fix, retry               │
│                                                         │
│  When stuck:                                            │
│    ☐ Check diff: diff -u test_baseline/... generated/   │
│    ☐ Revert: git checkout -- tools/generate/file.py     │
│    ☐ Smaller steps: break change into smaller pieces    │
│                                                         │
│  End of session:                                        │
│    ☐ All tests pass                                     │
│    ☐ All changes committed                              │
│    ☐ git push (optional)                                │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

---

## Example Session (10 Steps in 30 Minutes)

Real-world example of incremental refactoring:

```bash
# Step 1: Create utility (2 min)
vim tools/generate/comment_gen.py
make test-refactoring  # ✓ PASS
git commit -m "Add CommentGen"

# Step 2: Use in Primitive (3 min)
vim tools/generate/primitives.py
make test-refactoring  # ✓ PASS
git commit -m "Use CommentGen in Primitive"

# Step 3: Use in SimpleStruct (3 min)
vim tools/generate/structs.py
make test-refactoring  # ✓ PASS
git commit -m "Use CommentGen in SimpleStruct"

# Step 4: Use in SimpleArray (3 min)
vim tools/generate/arrays.py
make test-refactoring  # ✗ FAIL - oops!
diff -u test_baseline/generated/ast.h generated/ast.h  # See problem
vim tools/generate/arrays.py  # Fix typo
make test-refactoring  # ✓ PASS
git commit -m "Use CommentGen in SimpleArray"

# Steps 5-10: Repeat pattern (15 min)
# Each step: edit, test, commit
# After 30 minutes: 10 commits, all tests passing
```

---

## Summary

**Yes, incremental refactoring works perfectly with this testing setup!**

**Start with**:

1. Create one small utility class (test → commit)
2. Use it in ONE method in ONE class (test → commit)
3. Expand to other classes one at a time (test → commit each)
4. Repeat for other utilities

**Key principles**:

- One change at a time
- Test after EVERY change (15-30 seconds)
- Commit immediately after passing test
- If test fails, diff shows exactly what changed
- Can always rollback to last commit

**First practical step** (do this now):

```bash
# Create comment_gen.py as shown above
vim tools/generate/comment_gen.py
make test-refactoring  # Will pass
git commit
```

You're all set to start! Begin with the smallest possible change and build from there.
