# ANF Continuation Scaffolding Generator

**Status**: Specification Draft  
**Goal**: Generate boilerplate for continuation-passing style ANF normalization  
**Scope**: Environment structs, wrapper functions, and construction macros only - continuation bodies remain manual

## Problem Statement

The ANF rewrite (see ANF-REWRITE.md) requires ~70 continuation functions, each with ~15 lines of boilerplate:

```c
// Manual boilerplate (repeated 70+ times)
static LamData *normalizeLetKont(LamData *anfval, LamMap *map) {
    // Extract free variables from hash table
    HashSymbol *x = getLamMap_Symbol(map, TOK_X());
    LamData *body = getLamMap_Data(map, TOK_BODY());
    LamKont *k = getLamMap_Kont(map, TOK_K());
    
    // Actual continuation logic
    LamExp *anfbody = normalize(body, k);
    int save = PROTECT(anfbody);
    LamData *let = makeLamData_Let(x, getLamData_Exp(anfval), getLamData_Exp(anfbody));
    UNPROTECT(save);
    return let;
}

static LamData *normalizeLet(LamLet *let, LamKont *k) {
    // Setup continuation environment
    LamMap *map = newLamMap();
    int save = PROTECT(map);
    setLamMap_Symbol(map, TOK_X(), let->var);
    setLamMap_Data(map, TOK_BODY(), let->body);
    setLamMap_Kont(map, TOK_K(), k);
    LamKont *k2 = newLamKont(normalizeLetKont, map);
    PROTECT(k2);
    
    // Actual normalize logic
    LamData *val = newLamData_Exp(let->val);
    PROTECT(val);
    LamData *result = normalize(val, k2);
    UNPROTECT(save);
    return result;
}
```

**Boilerplate per continuation**: ~10 lines  
**Total savings**: ~700 lines across all continuations

**Note**: This proposal completely replaces the `LamMap` hash table approach from ANF-REWRITE.md with typed environment structs. Benefits:
- Type safety: Compiler catches field mismatches
- Performance: Direct field access vs hash lookup
- Clarity: Struct definition shows exactly what continuation needs
- Size: Fixed-size structs vs dynamic hash tables

## Proposed Solution

### 1. YAML Specification File

Create `src/anf_continuations.yaml`:

```yaml
config:
  name: anf_kont
  description: Continuation specifications for ANF normalization

continuations:
  normalizeLetKont:
    key: let
    brief: "Continuation for let-expression normalization"
    context: |
      (`(let ((,x ,val)) ,body)
          (normalize val
              [λ (anfval)
                  `(let ((,x ,anfval))
                        ,(normalize body k))]))
    free_vars:
      x: HashSymbol*
      body: LamData*
      k: LamKont*
    param:
      anfval: LamData*
      
  normalizeIfKont:
    key: iff
    brief: "Continuation for if-expression normalization"
    context: |
      (`(if ,e0 ,e1 ,e2)
          (normalize-name e0
              [λ (anfE0)
                  (k `(if ,anfE0
                          ,(normalize-term e1)
                          ,(normalize-term e2)))]))
    free_vars:
      k: LamKont*
      e1: LamData*
      e2: LamData*
    param:
      anfE0: LamData*
      
  normalizeCallInnerKont:
    key: callInner
    brief: "Inner continuation for function application"
    context: |
      (`(,Fn . ,Ms)
          (normalize-name Fn
              [λ (t)
                  (normalize-names Ms
                      [λ (ts) (k `(,t . ,ts))])]))
    free_vars:
      t: LamExp*
      k: LamKont*
    param:
      ts: LamData*
      
  # ... ~70 total continuations
```

### 2. Generated Code Structure

#### A. Environment Structs (generated/anf_kont.h)

Environment structs are generated using the **existing code generation system** by programmatically adding them to the Catalog:

```python
# In tools/generate/kontinuations.py
def populate_catalog(catalog, kont_specs):
    """Add continuation environment structs to catalog"""
    
    # Validate keys are unique
    keys = [spec['key'] for spec in kont_specs.values()]
    if len(keys) != len(set(keys)):
        raise Exception("Duplicate keys found in continuation specs")
    
    # Add each env struct as a SimpleStruct
    for kont_name, spec in kont_specs.items():
        struct_name = f"{kont_name}Env"
        struct = SimpleStruct(
            name=struct_name,
            brief=spec['brief'],
            data=spec['free_vars']  # x: HashSymbol*, body: LamData*, etc.
        )
        catalog.add(struct)
    
    # Add discriminated union of all envs using short keys
    # Maps: key -> struct_name (e.g., "iff" -> "NormalizeIfKontEnv")
    union_data = {spec['key']: f"{kont_name}Env" 
                  for kont_name, spec in kont_specs.items()}
    union = DiscriminatedUnion(
        name="KontEnv",
        brief="Continuation environments for ANF conversion",
        data=union_data
    )
    catalog.add(union)
```

Generated output:
```c
// Individual environment structs (with Header for GC)
typedef struct NormalizeLetKontEnv {
    Header header;
    HashSymbol *x;
    LamData *body;
    LamKont *k;
} NormalizeLetKontEnv;

typedef struct NormalizeIfKontEnv {
    Header header;
    LamKont *k;
    LamData *e1;
    LamData *e2;
} NormalizeIfKontEnv;

// Discriminated union of all envs (using short keys)
typedef struct KontEnv {
    KontEnvType type;
    union {
        NormalizeLetKontEnv *let;
        NormalizeIfKontEnv *iff;
        NormalizeCallInnerKontEnv *callInner;
        // ... one variant per continuation (using key names)
    } val;
} KontEnv;
```

**Benefits**: 
- Leverages existing code generation (new, copy, mark, free all automatic)
- Type-safe field access
- GC integration automatic via Header
- Public accessor functions (newKontEnv_Let, getKontEnv_Let, etc.) generated automatically

**Note**: These are generated to `generated/anf_kont.h` and `generated/anf_kont.c` using the **standard code generation pipeline** - no special handling needed.

#### B. Static Implementation Code (generated/anf_kont_impl.inc)

**Design Decision**: All continuation-specific code (declarations, wrappers, constructors) goes in a separate `.inc` file that is included only by `anf_normalize.c`. Everything in this file is declared `static` for maximum encapsulation.

```c
// generated/anf_kont_impl.inc
// Include this ONLY in anf_normalize.c
// All declarations and implementations are static

// User implementation declarations (to be implemented in anf_normalize.c)
static LamData* normalizeLetKont(LamData *anfval, NormalizeLetKontEnv *env);
static LamData* normalizeIfKont(LamData *anfE0, NormalizeIfKontEnv *env);
static LamData* normalizeCallInnerKontEnv(LamData *ts, NormalizeCallInnerKontEnv *env);
// ... one static declaration per continuation

// Wrapper implementations (bridge LamKontProc to typed env)
static LamData* normalizeLetKont_wrapper(LamData *anfval, LamKont *kont) {
    KontEnv *kontEnv = (KontEnv*)kont->env;
    NormalizeLetKontEnv *env = getKontEnv_Let(kontEnv);
    return normalizeLetKont(anfval, env);
}
// ... one wrapper per continuation

// Constructor implementations (build continuation closures)
static LamKont* makeKont_normalizeLet(HashSymbol *x, LamData *body, LamKont *k) {
    NormalizeLetKontEnv *env = newNormalizeLetKontEnv(x, body, k);
    int save = PROTECT(env);
    KontEnv *kontEnv = newKontEnv_Let(env);
    PROTECT(kontEnv);
    LamKont *kont = newLamKont(normalizeLetKont_wrapper, kontEnv);
    REPLACE_PROTECT(save, kont);
    return kont;
}
// ... one constructor per continuation

// Entry point - converts LamExp to LamData and starts normalization
static LamData* normalize_term(LamExp *e) {
    return normalize(newLamData_Exp(e), identity_continuation);
}
```

Usage in `anf_normalize.c`:
```c
#include "anf_kont.h"           // Public API: env structs, KontEnv union
#include "anf_kont_impl.inc"    // Static: declarations, wrappers, constructors

// User implements the continuation bodies (static)
static LamData* normalizeLetKont(LamData *anfval, NormalizeLetKontEnv *env) {
    // ... implementation matches declaration in .inc
}

// Single public interface function (manually written)
LamExp* anfNormalize(LamExp *exp) {
    LamData *result = normalize_term(exp);
    return getLamData_Exp(result);
}
```

**Benefits**: 
- ✅ **Complete encapsulation**: All ~70 continuation functions are static
- ✅ **Type safety**: Compiler enforces signatures via static declarations
- ✅ **Single public interface**: Only `anfNormalize()` is public
- ✅ **Clean separation**: Standard codegen in .h/.c, special codegen in .inc
- ✅ **No namespace pollution**: Zero continuation functions visible externally

#### C. Complete Structure Example

Here's how all the pieces fit together in `anf_normalize.c`:

```c
#include "anf_kont.h"           // Generated: env structs, KontEnv union, accessors
#include "anf_kont_impl.inc"    // Generated: static decls, wrappers, constructors

// User implements continuation bodies (static)
static LamData* normalizeLetKont(LamData *anfval, NormalizeLetKontEnv *env) {
    // Direct field access - no hash lookup!
    LamExp *anfbody = normalize(env->body, env->k);
    int save = PROTECT(anfbody);
    LamData *let = makeLamData_Let(env->x, getLamData_Exp(anfval), 
                                    getLamData_Exp(anfbody));
    UNPROTECT(save);
    return let;
}
// ... implement all ~70 continuation functions

// Use constructors to build continuations
static LamData* normalizeLet(LamLet *let, LamKont *k) {
    LamKont *k2 = makeKont_normalizeLet(let->var, let->body, k);  // Static constructor
    int save = PROTECT(k2);
    LamData *val = newLamData_Exp(let->val);
    PROTECT(val);
    LamData *result = normalize(val, k2);
    UNPROTECT(save);
    return result;
}

// Single public interface function (manually written)
LamExp* anfNormalize(LamExp *exp) {
    LamData *result = normalize_term(exp);  // Static entry point from .inc
    return getLamData_Exp(result);
}
```

**Benefits**: 
- No manual getLamMap_* extraction
- Clear what each continuation needs (typed structs)
- All continuation details are private
- Single clean public interface

#### D. File Organization Summary

**Design Decision**: Wrappers are included in `anf_normalize.c` via the `.inc` file:

```c
// generated/anf_kont_impl.inc (included by anf_normalize.c)

// Static declarations for user implementations
static LamData* normalizeLetKont(LamData *anfval, NormalizeLetKontEnv *env);

// Static wrapper - bridges LamKontProc signature to typed env
static LamData* normalizeLetKont_wrapper(LamData *anfval, LamKont *kont) {
    KontEnv *kontEnv = (KontEnv*)kont->env;
    // Use auto-generated safe extractor (throws error if wrong type)
    NormalizeLetKontEnv *env = getKontEnv_Let(kontEnv);
    return normalizeLetKont(anfval, env);  // Can call - same translation unit
}

// More declarations and wrappers...
```

**Benefits**:
- ✅ Both user impl and wrapper are static in same file
- ✅ Wrapper can call user impl (same translation unit)
- ✅ Only constructor functions need to be extern
- ✅ Maximum encapsulation

**Note**: This means `anf_kont_impl.inc` contains both declarations AND wrapper implementations

#### E. File Generation Summary

**Three files generated**:

1. **`generated/anf_kont.h`** + **`generated/anf_kont.c`** (standard codegen)
   - Environment struct definitions (NormalizeLetKontEnv, etc.)
   - KontEnv discriminated union  
   - Public accessors: `newKontEnv_Let()`, `getKontEnv_Let()`, `newNormalizeLetKontEnv()`, etc.
   - mark/free/copy implementations
   - Generated by **existing Catalog system** - no changes needed
   - Public scope, normal header/implementation split

2. **`generated/anf_kont_impl.inc`** (new special codegen)
   - Static user function declarations
   - Static wrapper implementations
   - Static constructor implementations  
   - Static normalize_term entry point
   - Generated by **new KontinuationGenerator**
   - All `static` - private to anf_normalize.c
   - Included only by anf_normalize.c

**Revised Design**: Constructors are also static in the `.inc` file:

```c
// generated/anf_kont_impl.inc

// User implementation declaration
static LamData* normalizeLetKont(LamData *anfval, NormalizeLetKontEnv *env);

// Wrapper implementation (static, calls user impl)
static LamData* normalizeLetKont_wrapper(LamData *anfval, LamKont *kont) {
    KontEnv *kontEnv = (KontEnv*)kont->env;
    // Use auto-generated safe extractor (throws error if wrong type)
    NormalizeLetKontEnv *env = getKontEnv_Let(kontEnv);
    return normalizeLetKont(anfval, env);
}

// Constructor implementation (ALSO static - all private)
static LamKont* makeKont_normalizeLet(HashSymbol *x, LamData *body, LamKont *k) {
    NormalizeLetKontEnv *env = newNormalizeLetKontEnv(x, body, k);
    int save = PROTECT(env);
    KontEnv *kontEnv = newKontEnv_Let(env);
    PROTECT(kontEnv);
    LamKont *kont = newLamKont(normalizeLetKont_wrapper, kontEnv);
    REPLACE_PROTECT(save, kont);
    return kont;
}

// Entry point (also static)
static LamData* normalize_term(LamExp *e) {
    return normalize(newLamData_Exp(e), identity_continuation);
}
```

**Final Architecture**:
- `anf_kont.h` + `anf_kont.c`: Public env structs and accessors (standard codegen)
- `anf_kont_impl.inc`: All static implementation details (new special codegen)
- `anf_normalize.c`: Includes both, implements continuation bodies, exposes single public `anfNormalize()` function

**Benefits**:
- ✅ **Zero public continuation functions** - all ~70 are static
- ✅ **Single public interface**: Only `anfNormalize(LamExp*) → LamExp*`
- ✅ **Standard codegen unchanged**: Env structs use existing Catalog system
- ✅ **Clean separation**: Types are public, implementation is private
- ✅ **Maximum encapsulation**: All continuation details hidden

#### F. User Implementation Pattern in anf_normalize.c
```c
#include "anf_kont.h"           // Public API: env structs, constructor decls
#include "anf_kont_impl.inc"    // Private: user decls, wrappers, constructor impls

// User implements the continuation bodies (static)
static LamData* normalizeLetKont(LamData *anfval, NormalizeLetKontEnv *env) {
    LamExp *anfbody = normalize(env->body, env->k);
    int save = PROTECT(anfbody);
    LamData *let = makeLamData_Let(env->x, getLamData_Exp(anfval), 
                                    getLamData_Exp(anfbody));
    UNPROTECT(save);
    return let;
}

// ... rest of normalization code uses makeKont_* constructors
```

**Note**: Constructors are public API - declared in header, implemented in .inc, used throughout anf_normalize.c

### 3. User Code Transformation

**Before (manual)**:
```c
static LamData *normalizeLet(LamLet *let, LamKont *k) {
    LamMap *map = newLamMap();
    int save = PROTECT(map);
    setLamMap_Symbol(map, TOK_X(), let->var);
    setLamMap_Data(map, TOK_BODY(), let->body);
    setLamMap_Kont(map, TOK_K(), k);
    LamKont *k2 = newLamKont(normalizeLetKont, map);
    PROTECT(k2);
    LamData *val = newLamData_Exp(let->val);
    PROTECT(val);
    LamData *result = normalize(val, k2);
    UNPROTECT(save);
    return result;
}
```

**After (generated scaffolding)**:
```c
static LamData *normalizeLet(LamLet *let, LamKont *k) {
    LamKont *k2 = MAKE_KONT_normalizeLet(let->var, let->body, k);
    int save = PROTECT(k2);
    LamData *val = newLamData_Exp(let->val);
    PROTECT(val);
    LamData *result = normalize(val, k2);
    UNPROTECT(save);
    return result;
}
```

**Savings**: 15 lines → 8 lines (7 lines eliminated per call site)

## Implementation Questions

### Q1: LamKont Structure and GC Integration

**RESOLVED**: Using existing code generation system solves this automatically.

- Each `*Env` struct added to catalog as `SimpleStruct` with Header
- Auto-generates `mark*Env()`, `free*Env()` functions
- `KontEnv` discriminated union gets `markKontEnv()`, `freeKontEnv()` dispatchers
- LamKont stores `KontEnv*` (which has Header and type tag)
- No special handling needed - GC integration automatic

### Q2: Type Registration

**RESOLVED**: Leveraging catalog system handles this.

- Each `*Env` struct gets its own object type enum value
- `KontEnv` union gets its own type with variant dispatch
- Object type generation follows existing pattern (see catalog.py)
- All mark/free dispatchers auto-generated

### Q3: Naming Convention

**RESOLVED**: Spec describes current approach.

- User writes `normalizeLetKont(LamData *arg, NormalizeLetKontEnv *env)` 
- Wrapper is `normalizeLetKont_wrapper` (static, internal)
- Constructor is `makeKont_normalizeLet` (public API)
- Clear, consistent naming with no suffixes on user code

### Q4: Constructor Implementation

**RESOLVED**: Using regular functions (not macros).

- Declared in generated header: `LamKont* makeKont_normalizeLet(...)`
- Implemented in generated `.inc` file (included by anf_normalize.c)
- Uses auto-generated `new*Env()` and `newKontEnv_*()` functions
- Fully debuggable, type-checked
- No cast needed (wrapper has correct `LamKontProc` signature)

### Q5: Where Do Continuation Specs Live?

**RESOLVED**: Separate file `src/anf_continuations.yaml`

- Keeps transformation specs separate from data structure definitions
- Follows pattern of other specialized YAML files in src/
- Allows independent versioning/maintenance

### Q6: INVOKE and Wrapper Signatures

**RESOLVED**: Wrapper signature matches `LamKontProc`.

```c
// LamKontProc signature
typedef LamData *(*LamKontProc)(LamData *arg, LamKont *kont);

// Wrapper matches exactly - no cast needed
static LamData* normalizeLetKont_wrapper(LamData *anfval, LamKont *kont) {
    KontEnv *kontEnv = (KontEnv*)kont->env;
    NormalizeLetKontEnv *env = kontEnv->val.let;
    return normalizeLetKont(anfval, env);  // User impl
}

// INVOKE works as expected
static inline LamData *INVOKE(LamKont *k, LamData *arg) {
    return k->proc(arg, k);  // Calls wrapper, which extracts env
}
```

## Open Questions

None - all design decisions resolved. Ready to implement.

## Build Integration

### Makefile Changes

```makefile
# Generate continuation environment structs (standard codegen)
generated/anf_kont.h: src/anf_continuations.yaml tools/generate.py
	$(PYTHON) tools/generate.py $< h > $@

generated/anf_kont.c: src/anf_continuations.yaml tools/generate.py
	$(PYTHON) tools/generate.py $< c > $@

generated/anf_kont_objtypes.h: src/anf_continuations.yaml tools/generate.py
	$(PYTHON) tools/generate.py $< objtypes_h > $@

# Generate continuation implementation scaffolding (new codegen)
generated/anf_kont_impl.inc: src/anf_continuations.yaml tools/generate.py
	$(PYTHON) tools/generate.py $< kont_impl_inc > $@

# Dependencies
obj/anf_kont.o: generated/anf_kont.h generated/anf_kont_objtypes.h
obj/anf_normalize.o: generated/anf_kont.h generated/anf_kont_impl.inc
```

**Note**: `anf_kont.c` is generated and compiled normally. `anf_kont_impl.inc` is included by `anf_normalize.c`.

### Generator Extension

Add `tools/generate/kontinuations.py`:
```python
class KontinuationGenerator:
    def populate_catalog(self, catalog, kont_specs):
        """Add env structs and KontEnv union to existing catalog
        
        Uses SimpleStruct and DiscriminatedUnion to add types to catalog.
        The catalog will then generate standard h/c/objtypes files.
        """
        
    def generate_kont_impl_inc(self, kont_specs):
        """Generate implementation include file with all static code
        
        Returns string containing:
        - Static user function declarations
        - Static wrapper implementations  
        - Static constructor implementations
        - Static normalize_term entry point
        
        Everything is static - no public functions exposed.
        """
```

**Key Design Points**:
- Standard types use existing Catalog → generates h/c/objtypes normally
- Special implementation uses new generator → generates .inc with all static
- `.inc` file keeps ~70 continuation functions completely private
- Single public `anfNormalize()` function is manually written

## Success Criteria

1. **Code reduction**: 15 lines → 8 lines per continuation call site (~50% reduction)
2. **Type safety**: Compiler catches env field mismatches
3. **Debuggability**: Can step through continuation construction and invocation
4. **Maintainability**: Adding new continuation = adding YAML entry
5. **Performance**: No measurable overhead vs manual hash table approach

## External Type Dependencies

### Investigation: Referencing Lambda-Generated Types

**Question**: Continuation environment structs contain fields of types generated from `lambda.yaml` (specifically `LamKont*` and `LamData*`). How does the code generator know these are GC-managed types that need marking?

**Answer**: Use the `external` section, already supported by the YAML code generation system.

**Example from `anf.yaml`**:
```yaml
external:
    TcType:
        meta:
            brief: external type from the type-checker
            description: A type-checker type referenced by the ANF code.
        data:
            cname: "struct TcType *"
            printFn: printTcType
            markFn: markTcType
            valued: true
```

**For continuations, add to `anf_continuations.yaml`**:
```yaml
external:
    LamKont:
        meta:
            brief: Continuation from lambda stage
            description: A lambda continuation closure used in ANF normalization.
        data:
            cname: "struct LamKont *"
            printFn: printLamKont
            markFn: markLamKont
            valued: true
    
    LamData:
        meta:
            brief: Data from lambda stage
            description: Lambda data passed through continuation maps.
        data:
            cname: "struct LamData *"
            printFn: printLamData
            markFn: markLamData
            valued: true
```

**Result**: When generating `markNormalizeLetKontEnv()`, the generator will:
1. See field `k: LamKont*` in free_vars
2. Look up `LamKont` in external types
3. Find `markFn: markLamKont`
4. Generate: `if (env->k) markLamKont(env->k);`

Same for `LamData*` fields: `if (env->body) markLamData(env->body);`

**Note on Union Accessors**: The spec uses the auto-generated `getKontEnv_Let()` accessor function instead of direct field access (`kontEnv->val.let`). This provides type safety - the getter will throw an error at runtime if the wrong variant is accessed, catching bugs earlier.

**Verdict**: ✅ **Easy and already supported**. The external type system in the YAML generator handles exactly this use case. Just need to add the external declarations.

**Implementation Note**: The `cname` field allows the generator to emit proper forward declarations and casts if needed. The `printFn` enables debug pretty-printing. The `valued` flag indicates these are pointer types that can be NULL-checked before marking.

## Next Steps

1. ~~Resolve open questions (Q1-Q6 above)~~ ✅ **All resolved**
2. Add `external` declarations for `LamKont` and `LamData` to `anf_continuations.yaml`
3. Create minimal `anf_continuations.yaml` with 3-4 continuations (already done in tools/)
4. Implement `KontinuationGenerator` class
5. Generate code and validate compilation
6. Port 3-4 manual continuations to use scaffolding
7. Measure code reduction and validate correctness
8. Extend to all ~70 continuations
