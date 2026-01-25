# Code Generation System

The build depends heavily on Python code generation. **Do not manually edit files in `generated/`**.

## Overview

The code generator is modular: the main entry point is `tools/generate.py`, which orchestrates the `generate` Python package (in `tools/generate/`). This package contains all logic for parsing YAML schemas and generating C code for all compiler stages. Contains modules for each type of generated structure (structs, discriminated unions, hashes, arrays etc.)

## YAML Schema Structure

Each `.yaml` file in `src/` defines structures for a compiler stage:

```yaml
config:
    name: <stage_name>     # e.g., "ast", "lambda", "anf", "cekfs"
    description: "..."     # Purpose of this stage
    parserInfo: true       # Include ParserInfo (file name and line number) in structs and unions (only)
    limited_includes:      # Additional headers needed
    - bigint.h

structs:
    StructName:
        meta:
            brief: "Short description"
            description: "Detailed description"
            parserInfo: false # optionally disable ParserInfo for this struct
        data:
            fieldName: fieldType
            autoInitField: type=initValue    # Constructor auto-initializes, not a parameter
            
unions:
    UnionName:
        meta:
        data:
            variantName: StructName
            ...

enums:      # simple C enums
    EnumName:
        meta:
        data:
            - type
            - type

arrays:                # support basic push/pop/peek etc.
    ArrayName:
        meta:
        data:
            dimensions: 2  # default 1, can be 2
            entries: ElementType

vectors:               # simple lightweight fixed-length memory-managed arrays
    VectorName:
        meta:
        data:
            entries: ElementType

stacks:                 # derivative of arrays, supports frames
    StackName:
        meta:
        data:
            entries: ElementType

hashes:
    HashName:
        meta:
        data:
            entries: ValueType
```

The yaml may also contain an `inline` section which in turn can contain arrays, unions and structs. These inline variants are not separately memory managed (no GC header), are often passed by value, and may be used as components of structs without the extra pointer indirection.

## Primitives (`src/primitives.yaml`)

Common types shared across all stages - referenced via `!include primitives.yaml`:

- `HashSymbol` - Symbol table entries (always a pointer)
- `int`, `bool`, `char`, `character` - Basic types
- `BigInt`, `MaybeBigInt` - Arbitrary precision integers
- `file_id` - Source file tracking
- Each has `cname`, `printf`/`printFn`, optionally `markFn`, `eqFn`

## Generated Functions

For each struct/union, the code generator produces:

### Memory Management

- `new<Type>()` - Allocator with GC header, takes all fields as args
- `set<Type>()` - Field setter with type check
- `copy<Type>()` - Deep copy
- `mark<Type>()` - Recursive GC marking
- `free<Type>()` - Cleanup (called by GC)
- `eq<Type>()` - Deep equality

### Arrays/Stacks/Vectors

- `new<Array>()` - Create with initial capacity
- `push<Array>()` - Append element
- `pop<Array>()` - Remove last element
- `peek<Array>()` - Access element without removing
- `poke<Array>()` - Set element at index

### Hash Tables

- `new<Hash>()` - Create hash table
- `get<Hash>()` - Retrieve value by key
- `set<Hash>()` - Store value by key
- Iterator functions for traversal

### Debugging (in `*_debug.{c,h}`)

- `print<Type>()` - Pretty-print for debugging
- `typename<Type>Obj()` - String name of type

### Object Types (in `*_objtypes.h`)

- Enum of all object types for GC
- Switch case macros for dispatch
- Generic `mark*Obj()`, `free*Obj()`, `typename*Obj()` dispatchers

## Extended Features for Generated Unions

- `new<union>_<variant>(variant)` creates a union from an existing variant with the correct discriminating tag.
- `make<union>_<variant>(fields...)` creates the variant and wraps it in the union in one step.
- `get<Union>_<variant>(unionPtr)` extracts the variant from the union, throwing an error if the type does not match.
- `set<Union>_<variant>(unionPtr, variantPtr)` safely sets the union to a new variant, with type checking.

## Key Classes in `tools/generate/`

- `Catalog` - Manages all entities in a YAML file, orchestrates generation
- `SimpleStruct`, `DiscriminatedUnion`, `SimpleArray`, `SimpleStack`, `SimpleHash`, `SimpleVector`, `SimpleEnum`, `Primitive` - each in their own module

## Usage Pattern

1. **Define structures** in `src/<stage>.yaml`
2. **Run make** - triggers `generate.py` for each YAML (via Makefile)
3. **Generated files** appear in `generated/`
4. **Include headers** in your C code: `#include "<stage>.h"`
5. **Use generated functions** - no manual memory management code needed

## Important Notes

- **ParserInfo**: If `parserInfo: true`, all structs get `ParserInfo I` field for error reporting source file and line number.
- **Auto-initialized fields**: Use `field: type=value` syntax in YAML to have constructor initialize the field automatically rather than requiring it as a parameter
- **GC Integration**: All generated `new*()` functions automatically register with GC
- **Type safety**: Generated code includes type checking in mark/free dispatchers
- **Documentation**: YAML `meta` blocks generate doxygen-style comments

## Adding New Structures

1. Add to appropriate `src/*.yaml` file
2. Define in `structs:`, `unions:`, `arrays:`, or `hashes:` section
3. Add `meta` documentation (optional but recommended)
4. Run `make` - regeneration is automatic
5. Include generated header in C files that use the new types
6. **For GC-managed types**: If the YAML generates `*_objtypes.h`:
   - Include the generated `*_objtypes.h` file in `src/memory.h`
   - Add the `*_OBJTYPES()` macro to the `ObjType` enum at the bottom of `memory.h`
   - This registers the types with the garbage collector
   - Add the `*_OBJTYPE_CASES()` macro to the switch statements in `markObj()`, `freeObj()`, and `typenameObj()` in `src/memory.c` to complete the integration.

## Debugging Generation

If generated code looks wrong:

- Check YAML syntax (especially indentation)
- Verify types are defined (either in same YAML or `primitives.yaml`)
- Look at similar existing definitions as templates
- Run `python3 tools/generate.py src/<file>.yaml h` manually to see errors
