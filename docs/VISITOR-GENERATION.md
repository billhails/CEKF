# Visitor Generation Design

## Overview

Extend the code generator to produce **editable boilerplate** for tree traversals. This is a development tool, not a build step - generates starter code that users then customize for specific transformations.

## Usage

```bash
python3 tools/generate.py src/lambda.yaml visitor > src/flatten.c
```

Generates `src/flatten.c` - a self-contained file with:
- Context struct skeleton
- Visitor function for each struct/union in the YAML
- Dispatcher functions for unions
- PROTECT/UNPROTECT scaffolding

User then edits the generated code to implement the actual transformation logic.

## Design Principles

1. **Self-contained output** - Single C file to stdout, no separate header
2. **Distributed implementation** - Each class (SimpleStruct, DiscriminatedUnion, etc.) implements its own `generateVisitor()` method
3. **Follows existing patterns** - Like `generateMarkFunction()`, `generateHeader()`, etc.
4. **Immutable by default** - Returns new structure if changed, original if unchanged
5. **Editable** - Generated code is the starting point, not the final product

## Implementation Plan

### 1. Add to `structs.py` - `SimpleStruct` class

```python
def generateVisitorDecl(self):
    """Generate forward declaration for visitor function"""
    # Generate:
    #   static StructName *visitStructName(StructName *node, VisitorContext *context);
    pass

def generateVisitor(self):
    """Generate visitor function for this struct"""
    # For struct FooBar with fields: field1, field2, field3
    # Generate:
    #   static StructName *visitStructName(StructName *node, ContextName *context) {
    #       int save = PROTECT((Header *)node);
    #       Type1 *newField1 = visitType1(node->field1, context);
    #       PROTECT((Header *)newField1);
    #       Type2 *newField2 = visitType2(node->field2, context);
    #       PROTECT((Header *)newField2);
    #       // ... for all fields
    #       
    #       StructName *result;
    #       if (newField1 != node->field1 || newField2 != node->field2 || ...) {
    #           result = newStructName(newField1, newField2, ...);
    #       } else {
    #           result = node;
    #       }
    #       UNPROTECT(save);
    #       return result;
    #   }
    pass
```

### 2. Add to `unions.py` - `DiscriminatedUnion` class

```python
def generateVisitorDecl(self):
    """Generate forward declarations for dispatcher + variant visitor functions"""
    # Generate declarations for:
    #   - Main dispatcher: static UnionName *visitUnionName(UnionName *node, VisitorContext *context);
    #   - Each variant: static UnionName_Variant *visitUnionName_Variant(UnionName_Variant *node, VisitorContext *context);
    pass

def generateVisitor(self):
    """Generate dispatcher + variant visitor functions"""
    # Generate dispatcher:
    #   UnionName *visitUnionName(UnionName *node, ContextName *context) {
    #       if (!node) return NULL;
    #       switch(node->tag) {
    #           case UNION_VARIANT1: return visitUnionName_Variant1(...);
    #           case UNION_VARIANT2: return visitUnionName_Variant2(...);
    #           ...
    #       }
    #   }
    # Plus visitor for each component (delegate to struct's generateVisitor)
    pass
```

### 3. Add to `catalog.py` - `Catalog` class

```python
def generateVisitor(self):
    """Generate complete visitor boilerplate"""
    output = []
    
    # GPL header (use existing method)
    output.append(self.generateGPLHeader())
    output.append("\n")
    
    # Includes
    output.append(f'#include "{self.config.name}.h"\n')
    output.append('#include "memory.h"\n\n')
    
    # Context struct skeleton
    output.append("typedef struct VisitorContext {\n")
    output.append("    // Add your context fields here\n")
    output.append("} VisitorContext;\n\n")
    
    # Forward declarations
    output.append("// Forward declarations\n")
    for entity in self.entities.values():
        output.append(entity.generateVisitorDecl())
    output.append("\n")
    
    # Implementations
    output.append("// Visitor implementations\n")
    for entity in self.entities.values():
        output.append(entity.generateVisitor())
    
    return ''.join(output)
```

### 4. Update `generate.py`

Add to `generate_output()` function:
```python
def generate_output(args, catalog, document, typeName, description, includes, limited_includes, parserInfo):
    """Generate the appropriate output based on args.type"""
    
    if args.type == "h":
        generate_header(args, catalog, document, typeName, includes, limited_includes, parserInfo)
    # ... existing cases ...
    elif args.type == 'visitor':
        print(catalog.generateVisitor())
    # ... rest of cases ...
```

## Field Type Handling

Each field type needs appropriate visitor call:

- **Structs**: `visitStructName(field, context)`
- **Unions**: `visitUnionName(field, context)` (uses dispatcher)
- **Arrays**: Create replacement array up front, loop over elements visiting each and populating the replacement, only return replacement if any element changed
- **Hashes**: Same as arrays, begerate the replacement up front, Iterate entries, visit values, return replacement if different.
- **Primitives** (int, char, bool): Pass through (but generate call for customization)
- **HashSymbol**: Pass through unchanged (never transform symbols)
- **AutoInit fields**: Visit but don't include in constructor parameters, will need to manually assign to a replacement if changed.

## Special Cases

1. **Protection stack**: All visitor functions wrap work in PROTECT/UNPROTECT
2. **Null checks**: Generated code checks for NULL before visiting
3. **HashSymbol**: Never call visitor on symbols, always pass through unchanged
4. **Optimization**: Return original pointer if no fields changed (avoid unnecessary allocation)

## Next Steps

1. Add `generateVisitor()` and `generateVisitorDecl()` methods to base `Entity` class (or all candidate classes) with `pass` implementation, allowing children to override
2. Implement `generateVisitorDecl()` and `generateVisitor()` in `SimpleStruct` (structs.py)
3. Implement `generateVisitorDecl()` and `generateVisitor()` in `DiscriminatedUnion` (unions.py)
4. Add orchestration to `Catalog` (catalog.py) - include GPL header generation
5. Update `generate.py` to handle new mode
6. Test with `src/lambda.yaml` to generate a sample visitor (can run at any point to see progress)
7. Extend to arrays/vectors/hashes as needed

## Example Generated Output

For a simple struct in `src/lambda.yaml`:
```c
/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2025  Bill Hails
 *
 * This program is free software: you can redistribute it and/or modify
 * ...
 */

#include "lambda.h"
#include "memory.h"

typedef struct VisitorContext {
    // Add your context fields here
} VisitorContext;

// Forward declarations
static LamExp *visitLamExp(LamExp *node, VisitorContext *context);
static LamVar *visitLamVar(LamVar *node, VisitorContext *context);
// ...

// Visitor implementations
static LamExp *visitLamExp(LamExp *node, FlattenContext *context) {
    if (!node) return NULL;
    switch(node->tag) {
        case LAM_EXP_LAMBDA: 
            return (LamExp *)visitLamExp_Lambda(node->val.lambda, context);
        // ... other cases
    }
}

static LamExp_Lambda *visitLamExp_Lambda(LamExp_Lambda *node, FlattenContext *context) {
    int save = PROTECT((Header *)node);
    
    LamVar *newVar = visitLamVar(node->var, context);
    PROTECT((Header *)newVar);
    LamExp *newExp = visitLamExp(node->exp, context);
    PROTECT((Header *)newExp);
    
    LamExp_Lambda *result;
    if (newVar != node->var || newExp != node->exp) {
        result = newLamExp_Lambda(newVar, newExp, node->I);
    } else {
        result = node;
    }
    
    UNPROTECT(save);
    return result;
}
```

User then customizes the body of visitor functions to implement their specific transformation.
