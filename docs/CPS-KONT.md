# CPS Continuation Scaffolding: Generalizing ANF-KONT

**Status**: Proposal Draft  
**Date**: December 2025  
**Goal**: Generalize the ANF continuation scaffolding generator to support CPS transform and future continuation-based algorithms

## Executive Summary

The ANF continuation scaffolding generator (`tools/generate/kontinuations.py`) successfully reduces boilerplate by generating environment structs and wrapper code from YAML specifications. This proposal evaluates how much of that infrastructure can be reused for the CPS transform, identifies differences, and proposes a generalized framework.

**TL;DR**: ~80% of the ANF-KONT infrastructure can be directly reused. The main differences are:
1. CPS has **two** transformation functions (T_k and T_c) vs ANF's single `normalize`
2. CPS continuations have **different signatures** (take expression + return expression)
3. CPS has **value-vs-computation** distinction requiring different treatment

Generalization is achievable with minimal changes to the YAML specification and generator.

## Generated Visitor Pattern

The CEKF build system has a visitor pattern generator that reduces the work needed for CPS transform.

Files like [src/lambda_cpsTk.c](../src/lambda_cpsTk.c) and [src/lambda_cpsTc.c](../src/lambda_cpsTc.c) are automatically generated from `lambda.yaml` using:

```bash
python3 tools/generate.py src/lambda.yaml visitor --target=cpsTk
```

This generates ~1877 lines of boilerplate including:
- Forward declarations for all visitor functions
- Complete visitor implementation for every LamExp type
- Automatic NULL checking and ENTER/LEAVE debugging
- Proper GC protection with PROTECT/UNPROTECT for all memory-managed fields
- Immutability pattern: creates new nodes only when children change
- Pass-through optimization: non-managed types (HashSymbol, int) passed unchanged
- Context structure for passing state through the visitor

The manual tree-walking and GC protection code is already generated. The user only needs to add transformation logic within the visitor functions.

### Example: Generated Visitor Skeleton

```c
// Generated automatically by tools/generate.py
static LamIff *cpsTkLamIff(LamIff *node, VisitorContext *context) {
    ENTER(cpsTkLamIff);
    if (node == NULL) {
        LEAVE(cpsTkLamIff);
        return NULL;
    }

    bool changed = false;
    LamExp *new_condition = cpsTkLamExp(node->condition, context);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    LamExp *new_consequent = cpsTkLamExp(node->consequent, context);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    LamExp *new_alternative = cpsTkLamExp(node->alternative, context);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);

    if (changed) {
        LamIff *result = newLamIff(new_condition, new_consequent, new_alternative);
        UNPROTECT(save);
        LEAVE(cpsTkLamIff);
        return result;
    }

    UNPROTECT(save);
    LEAVE(cpsTkLamIff);
    return node;
}
```

**User adds transformation logic here** - everything else is automated.

## Background: ANF vs CPS Continuations

### ANF Pattern

```scheme
;; ANF: normalize expression, pass result to continuation
(define (normalize e k)
  (match e
    (`(if ,e0 ,e1 ,e2)
        (normalize-name e0
            [λ (anfE0)                           ; continuation captures k, e1, e2
                (k `(if ,anfE0
                        ,(normalize-term e1)
                        ,(normalize-term e2)))]))))
```

**Characteristics**:
- Single transformation function: `normalize(LamExp, AnfKont) -> LamExp`
- Continuations take normalized result, return final result
- All subexpressions normalized recursively
- **Visitor pattern NOT used** (manually written tree walking)

### CPS Pattern (from [cps5.fn](fn/rewrite/cps5.fn))

```fn
;; CPS: Two transformations - T_k (returns value) and T_c (passes to continuation)
fn T_k(e, k) {
    if (isAexpr(e)) {
        k(M(e))                                  // Apply continuation to value
    } else {
        switch (e) {
            (E.if_expr(test, e1, e2)) {
                let
                    rv = gensym("$rv");
                    cont = E.lambda([rv], k(rv));   // Create continuation lambda
                in
                    T_k(test, fn (atest) {          // Transform test with continuation
                        E.if_expr(atest, T_c(e1, cont), T_c(e2, cont))
                    })
            }
        }
    }
}

fn T_c(expr, c) {
    if (isAexpr(expr)) {
        E.apply(c, [M(expr)])                    // Apply continuation to value
    } else {
        switch(expr) {
            (E.if_expr(test, e1, e2)) {
                let
                    rv = gensym("$rv");
                    cont = E.lambda([rv], E.apply(c, [rv]));
                in
                    T_k(test, fn (atest) {
                        E.if_expr(atest, T_c(e1, cont), T_c(e2, cont))
                    })
            }
        }
    }
}
```

**Characteristics**:
- **Two** transformation functions:
  - `T_k(expr, k) -> expr`: Transform with continuation `k` (**needs scaffolding**)
  - `T_c(expr, c) -> expr`: Transform with lambda expression `c` (**just data, no scaffolding**)
- **Only T_k uses CpsKont structures** - T_c passes LamExp* as regular function arguments
- `k` is a structured continuation (C function pointer + environment)
- `c` is just a generated lambda expression (LamExp*) - no continuation wrapper needed
- Generated fresh continuation variables (`$rv`, `$k`) are part of output
- Value expressions (Aexpr) vs computation expressions (Cexpr) have different handling
- **Can leverage visitor pattern**: Generated visitors handle tree walking, user adds CPS logic

## Comparison: ANF vs CPS Continuation Needs

| Aspect | ANF | CPS | Generalizable? |
|--------|-----|-----|----------------|
| **Environment Structs** | ✅ Per-continuation struct holding free vars | ✅ Same pattern | ✅ **Yes - identical** |
| **Discriminated Union** | ✅ `KontEnv` union of all env structs | ✅ Same pattern | ✅ **Yes - identical** |
| **Type Safety** | ✅ Typed accessors (`getKontEnv_Let`) | ✅ Same benefit | ✅ **Yes - identical** |
| **External Type Refs** | ✅ References `LamExp*` via external | ✅ References `LamExp*` via external | ✅ **Yes - identical** |
| **Wrapper Functions** | ✅ Bridge generic to specific env | ✅ Same pattern (T_k only) | ✅ **Yes - identical** |
| **Constructor Functions** | ✅ `makeKont_normalize*()` | ✅ `makeKont_T_k*()` (T_c doesn't need) | ✅ **Yes - T_k only** |
| **Number of Transformers** | 1 (`normalize`) | 2 (`T_k` with konts, `T_c` with data) | ✅ **Only T_k needs scaffolding** |
| **Continuation Signature** | `LamExp* -> LamExp*` | `LamExp* -> LamExp*` (T_k only) | ✅ **Yes - identical** |
| **🆕 Visitor Pattern** | ❌ Manual tree walking | ✅ **Generated visitors** | ✅ **Significant improvement** |
| **🆕 GC Protection** | ⚠️ Manual PROTECT/UNPROTECT | ✅ **Auto-generated** | ✅ **Significant improvement** |
| **🆕 Immutability** | ⚠️ Manual node creation | ✅ **Auto-generated** | ✅ **Significant improvement** |

CPS can use the generated visitor pattern which provides:
- ✅ Complete tree traversal boilerplate (~1800 lines generated)
- ✅Hybrid Approach: Visitors + Continuation Scaffolding

The optimal CPS implementation combines **two** code generation systems:

### 1. Generated Visitors (Already Exists)

**Command**: `python3 tools/generate.py src/lambda.yaml visitor --target=cpsTk`

**Generates**: [src/lambda_cpsTk.c](../src/lambda_cpsTk.c) with complete tree traversal

**User modifies**: Add CPS transformation logic to generated visitor functions

**Example**:
```c
// Generated visitor - user adds transformation logic between the boilerplate
static LamIff *cpsTkLamIff(LamIff *node, VisitorContext *context) {
    ENTER(cpsTkLamIff);
    if (node == NULL) { LEAVE(cpsTkLamIff); return NULL; }

    // USER ADDS: Check if this is a value (Aexpr) - use pass-through
    if (isAexpr(node)) {
        LEAVE(cpsTkLamIff);
        return node;  // Pass through unchanged
    }

    // Generated GC-protected traversal
    bool changed = false;
    LamExp *new_condition = cpsTkLamExp(node->condition, context);
    int save = PROTECT(new_condition);
    // ... more generated code ...

    // USER ADDS: CPS transformation logic here
    // - Generate fresh continuation variable
    // - Create continuation lambda
    // - Transform branches with continuation
    // ... CPS-specific logic ...

    UNPROTECT(save);
    LEAVE(cpsTkLamIff);
    return result;
}
```

**Benefits**:
- ✅ No manual tree walking
- ✅ Automatic GC protection
- ✅ Immutability handled
- ✅ ~1800 lines of boilerplate generated

### 2. Continuation Scaffolding (Proposed)

**Purpose**: Generate continuation environment structs and construction helpers

**Still needed because**: Visitors handle tree walking, but CPS needs continuation closures

**Example CPS pattern**:
```c
// CPS needs to create continuation closures with captured free variables
CpsKont *k2 = makeKont_T_k_if(k, iff->consequent, iff->alternative, cont);
```

This is where the continuation scaffolding (from ANF-KONT.md) helps:
- Auto-generate environment structs for each continuation
- Auto-generate `makeKont_*` constructor functions
- Auto-generate wrapper functions to bridge generic to specific

##  Automatic GC protection for all fields
- ✅ Immutability pattern (only create new nodes when changed)
- ✅ NULL checking and debugging hooks
- ✅ Context threading through transformation

Only **T_k needs continuation scaffolding**:
- T_k uses CpsKont structures (function pointer + environment) - needs scaffolding
- T_c uses plain LamExp* expressions - just regular function parameters, no scaffolding
- This makes CPS closer to ANF than initially thought (one set of continuations, not two)

CPS implementation is more straightforward than initially expected:
1. Core scaffolding generation is reusable (continuation structs for T_k only)
2. Visitor pattern eliminates most manual tree-walking code
3. T_c doesn't need continuation infrastructure at all
4. User only writes the transformation logic, not the traversal infrastructure

## Proposed Generalized YAML Schema

### Extended Config Section

```yaml
config:
  name: cps_kont                          # Base name for generated files
  description: "Continuation scaffolding for CPS transform"
  parserInfo: false
  includes:
    - lambda.h
  limited_includes:
    - lambda_debug.h
  
  # NEW: Continuation framework configuration
  continuation_config:
    transformers:                         # List of transformer function names
      - T_k
      - T_c
    kont_struct: CpsKont                  # Name of top-level continuation struct
    kont_proc_type: CpsKontProcWrapper    # Type of continuation function pointer
    param_type: Expr                      # Type passed to/returned from continuations
    return_type: Expr                     # Type returned from continuations
```

### Continuation Specifications (Unchanged)

```yaml
continuations:
  T_k_if:                                 # Continuation name includes transformer
    transformer: T_k                      # NEW: Which transformer this belongs to
    key: k_if                             # Short key for union variant
    brief: "Continuation for if-expression (value context)"
    context: |
      (E.if_expr(test, e1, e2))
          T_k(test, [fn (atest) {
              E.if_expr(atest, T_c(e1, cont), T_c(e2, cont))
          }])
    free_vars:
      k: CpsKont                          # Outer continuation
      e1: Expr                            # Then branch
      e2: Expr                            # Else branch
      cont: Expr                          # Fresh continuation variable
    param:
      atest: Expr                         # Normalized test expression
  
  T_c_if:
    transformer: T_c                      # Belongs to T_c transformer
    key: c_if
    brief: "Continuation for if-expression (computation context)"
    context: |
      (E.if_expr(test, e1, e2))
          T_k(test, [fn (atest) {
              E.if_expr(atest, T_c(e1, cont), T_c(e2, cont))
          }])
    free_vars:
      c: Expr                             # Outer continuation (expression, not CpsKont!)
      e1: Expr
      e2: Expr
      cont: Expr
    param:
      atest: Expr
  
  T_k_apply:
    transformer: T_k
    key: k_apply
    brief: "Continuation for function application"
    context: |
      (E.apply(fn, args))
          Ts_k(args, [fn (sargs) {
              T_k(fn, fn (sfn) {
                  k(E.apply(sfn, sargs))
              })
          }])
    free_vars:
      fn: Expr
      k: CpsKont
    param:
      sargs: Expr                         # List of normalized arguments

external:
  Expr:
    meta:
      brief: Lambda expressions for CPS transform
    data:
      cname: "struct LamExp *"            # May be same underlying type
      printFn: printLamExp
      markFn: markLamExp
      valued: true
```

## Generated Code Structure (CPS Example)

### Public Header (generated/cps_kont.h)

```c
// Environment structs (one per continuation)
typedef struct T_k_ifKontEnv {
    Header header;
    CpsKont *k;
    Expr *e1;
    Expr *e2;
    Expr *cont;
} T_k_ifKontEnv;

typedef struct T_c_ifKontEnv {
    Header header;
    Expr *c;              // Note: not CpsKont*, raw expression
    Expr *e1;
    Expr *e2;
    Expr *cont;Tk.c - Generated + Modified)

**Step 1**: Generate visitor skeleton:
```bash
python3 tools/generate.py src/lambda.yaml visitor --target=cpsTk
```

**Step 2**: Modify generated visitor to add CPS logic:

```c
// Generated visitor skeleton - user adds CPS transformation logic
#include "cps_kont.h"              // Continuation structs (also generated)
#include "cps_kont_impl.inc"       // Continuation constructors (also generated)

// VisitorContext now holds the continuation
typedef struct VisitorContext {
    CpsKont *k;                     // Continuation for T_k
    // Or: LamExp *c;                  // Continuation expression for T_c
    bool isValue;                   // Track if we're in value/computation context
} VisitorContext;

// User modifies generated visitor function
static LamIff *cpsTkLamIff(LamIff *node, VisitorContext *context) {
    ENTER(cpsTkLamIff);
    if (node == NULL) { LEAVE(cpsTkLamIff); return NULL; }

    // USER ADDS: Value check
    if (isAexpr(makeLamExp_Iff(node))) {
        LEAVE(cpsTkLamIff);
        return node;  // Pass through values unchanged
    }

    // USER ADDS: CPS transformation logic
    // Generate fresh continuation variable
    LamExp *rv = gensym("$rv");
    int save = PROTECT(rv);
    
    // Create continuation lambda: (λ (rv) (k rv))
    LamArgs *rvArgs = newLamArgs(rv, NULL);
    PROTECT(rvArgs);
    LamExp *applyK = makeLamExp_Apply(context->k, rvArgs);
    PROTECT(applyK);
    LamVarList *lamVars = newLamVarList(getSymbol(rv), NULL);
    PROTECT(lamVars);
    LamLam *contLam = newLamLam(lamVars, applyK, false);
    PROTECT(contLam);
    LamExp *cont = makeLamExp_Lam(contLam);
    PROTECT(cont);
    
    // Create continuation for test transformation
    CpsKont *k2 = makeKont_T_k_if(context->k, 
                                   makeLamExp_Consequent(node), 
                                   makeLamExp_Alternative(node), 
                                   cont);
    PROTECT(k2);
    
    // Transform test with new continuation
    VisitorContext newContext = { .k = k2, .isValue = false };
    LamExp *testNode = makeLamExp_Condition(node);
    PROTECT(testNode);
    LamExp *cpsTest = cpsTkLamExp(testNode, &newContext);
    PROTECT(cpsTest);
    
    // ... rest of transformation ...
    
    UNPROTECT(save);
    LEAVE(cpsTkLamIff);
    return result;
}

// Entry point
LamExp *cpsTransform_T_k(LamExp *exp) {
    // Create initial continuation (halt)
    CpsKont *halt = makeHaltContinuation();
    VisitorContext context = { .k = halt, .isValue = false };
    return cpsTkLamExp(exp, &context);
}
```

Summary:
- ✅ Tree traversal is generated (entire visitor structure)
- ✅ GC protection is generated (PROTECT/UNPROTECT scaffolding)
- ✅ Continuation structs are generated (from cps_continuations.yaml)
- ✅ Continuation constructors are generated (`makeKont_T_k_if`)
- ⚠️ User adds: CPS-specific transformation logic (gensym, continuation creation, context threading)
- ⚠️ User adds: Value/computation distinction (isAexpr checks)

Work estimate:
- Generated automatically: ~1800 lines (visitor) + ~500 lines (continuation scaffolding) = ~2300 lines
- User writes: ~50-100 lines per case * ~40 cases = ~2000-4000 lines
- Total: From ~5000-7000 lines manual to ~2000-4000 lines with scaffolding return kont;
}

```c
static CpsKont *makeKont_T_c_if(Expr *c, Expr *e1, Expr *e2, Expr *cont) {
    KontEnv *env = makeKontEnv_C_if(c, e1, e2, cont);
    int save = PROTECT(env);
    CpsKont *kont = newCpsKont(T_c_ifKontWrapper, env);
    UNPROTECT(save);
    return kont;
}
```

### User Implementation (src/lambda_cps_2.c)

```c
#include "cps_kont.h"
#include "cps_kont_impl.inc"

// User implements continuation bodies
static Expr *T_k_ifKont(Expr *atest, T_k_ifKontEnv *env) {
    Expr *cps_e1 = T_c(env->e1, env->cont);
    int save = PROTECT(cps_e1);
    Expr *cps_e2 = T_c(env->e2, env->cont);
    PROTECT(cps_e2);
    Expr *iff = makeExpr_If(atest, cps_e1, cps_e2);
    PROTECT(iff);
    Expr *result = INVOKE(env->k, iff);
    UNPROTECT(save);
    return result;
}

static Expr *T_c_ifKont(Expr *atest, T_c_ifKontEnv *env) {
    // Similar but applies c (an expression) instead of k (a CpsKont)
    Expr *cps_e1 = T_c(env->e1, env->cont);
    int save = PROTECT(cps_e1);
    Expr *cps_e2 = T_c(env->e2, env->cont);
    PROTECT(cps_e2);
    Expr *iff = makeExpr_If(atest, cps_e1, cps_e2);
    PROTECT(iff);
    Expr *result = makeExpr_Apply(env->c, makeExpr_List1(iff));
    UNPROTECT(save);
    return result;
}

// Main transformation functions
static Expr *T_k(Expr *e, CpsKont *k) {
    if (isAexpr(e)) {
        return INVOKE(k, M(e));
    }
    switch (e->type) {
        case EXPR_TYPE_IF:
            LamIff *iff = getExpr_If(e);
            // Generate fresh continuation variable
            Expr *rv = gensym("$rv");
            Expr *cont = makeExpr_Lambda(makeList1(rv), 
                                         makeExpr_Apply(k, makeList1(rv)));
            int save = PROTECT(cont);
            // Create continuation for test transformation
            CpsKont *k2 = makeKont_T_k_if(k, iff->e1, iff->e2, cont);
            PROTECT(k2);
            Expr *result = T_k(iff->test, k2);
            UNPROTECT(save);
            return result;
        // ... other cases
    }
}

static Expr *T_c(Expr *e, Expr *c) {
    if (isAexpr(e)) {
        return makeExpr_Apply(c, makeList1(M(e)));
    }
    switch (e->type) {
        case EXPR_TYPE_IF:
            // Similar pattern but continuation is expression c, not CpsKont k
            // ...
    }
}
```

## Differences from ANF-KONT

### 1. Multiple Transformer Functions

**ANF**: Single `normalize()` function, all continuations belong to it  
**CPS**: Two functions (`T_k`, `T_c`), continuations namespaced by transformer

Solution: Add `transformer` field to continuation specs:
```yaml
continuations:
  T_k_if:
    transformer: T_k      # NEW field
    key: k_if
    # ...
```

Generator uses this to:
- Namespace continuation names: `T_k_ifKont`, `T_c_ifKont`
- Group constructors by transformer
- Generate separate forward declarations per transformer

### 2. Shared Continuation Functions Across Multiple Files

**ANF**: Single `.inc` file with static implementations included in one C file  
**CPS**: Two separate visitor files (`lambda_cpsTk.c` and `lambda_cpsTc.c`) both need the same continuation functions

**Problem**: Static functions in `.inc` file can't be shared across multiple compilation units.

**Two possible solutions**:

**Option A: Public API with header file**
- Generate `cps_continuations.c` (implementation) and `cps_continuations.h` (declarations)
- Make all continuation functions public (no `static`)
- Both `lambda_cpsTk.c` and `lambda_cpsTc.c` include the header and link against the `.c` file
- Pros: Maintains separation of concerns, standard C pattern
- Cons: Slightly more complexity in generator (must produce both `.h` and `.c`)

**Option B: Combine visitor files**
- Merge `lambda_cpsTk.c` and `lambda_cpsTc.c` into a single `lambda_cps.c`
- Include the generated `.inc` file once in the combined file
- Pros: Simpler, matches ANF pattern exactly
- Cons: Loses the separation between T_k and T_c implementations

**Recommendation**: Option A (public API with header) is more scalable. If we add more continuation-based transforms in the future, they can all share the same continuation infrastructure.

Generator command-line arguments control output format:
```bash
# For ANF-style static .inc file (single consumer)
python3 tools/generate.py src/anf_continuations.yaml kont_impl_inc > generated/anf_kont.inc

# For CPS-style public API (multiple consumers)
python3 tools/generate.py src/cps_continuations.yaml kont_impl_h > generated/cps_continuations.h
python3 tools/generate.py src/cps_continuations.yaml kont_impl_c > generated/cps_continuations.c
```

No YAML configuration needed - the choice is made at generation time.

### 3. Continuation Types Vary

**ANF**: All continuations are `AnfKont*` (C function pointer wrapper)  
**CPS**: Some are `CpsKont*`, others are `Expr*` (generated lambda expressions)

**Solution**: Allow free variables to have different types:
```yaml
T_k_if:
  free_vars:
    k: CpsKont          # C continuation structure

T_c_if:
  free_vars:
    c: Expr             # Generated expression, not CpsKont
```

This already works! The type system handles both.

### 3. Fresh Variable Generation

**ANF**: Minimal fresh variables (just for intermediate results)  
**CPS**: Extensive use of `gensym()` for continuation variables (`$rv`, `$k`)

**Impact**: No impact on scaffolding generation. Fresh variable generation happens in user-written continuation bodies, not in generated code.

### 4. Value vs Computation Context

**ANF**: No distinction, all expressions normalized uniformly  
**CPS**: Aexpr (values) handled differently from Cexpr (computations)

**Solution**: Add optional `value_predicate` to config:
```yaml
continuation_config:
  value_predicate: isAexpr    # Function to test if expression is a value
```

Generator can emit helper checks, but main logic stays in user code.

## Proposed Generator Enhancements

### 1. Parameterized KontinuationGenerator

```python
class KontinuationGenerator:
    def __init__(self, yaml_data: Dict[str, Any]):
        self.config = yaml_data.get('config', {})
        self.kont_config = self.config.get('continuation_config', {
            'transformers': ['normalize'],
            'kont_struct': 'AnfKont',
            'kont_proc_type': 'AnfKontProcWrapper',
            'param_type': 'LamExp',
            'return_type': 'LamExp'
        })
        self.continuations = yaml_data.get('continuations', {})
    
    def populate_catalog(self, catalog: Catalog) -> None:
        # Group continuations by transformer
        by_transformer = {}
        for name, spec in self.continuations.items():
            transformer = spec.get('transformer', self.kont_config['transformers'][0])
            by_transformer.setdefault(transformer, []).append((name, spec))
        
        # Generate env structs (same as before)
        union_data = {}
        for name, spec in self.continuations.items():
            struct_name = ucFirst(name) + "KontEnv"
            # ... existing struct generation logic
            union_data[spec['key']] = struct_name
        
        # Generate union (same as before)
        catalog.add(DiscriminatedUnion('KontEnv', {'data': union_data}))
        
        # Generate top-level kont struct (parameterized)
        catalog.add(SimpleStruct(self.kont_config['kont_struct'], {
            'meta': {'brief': 'Continuation structure'},
            'data': {
                'wrapper': self.kont_config['kont_proc_type'],
                'env': 'KontEnv'
            }
        }))
```

### 2. Namespace Wrappers and Constructors

```python
def _write_wrappers(self, output: TextIO) -> None:
    output.write("// Wrapper functions\n\n")
    
    for name, spec in self.continuations.items():
        transformer = spec.get('transformer', self.kont_config['transformers'][0])
        param_type = self.kont_config['param_type']
        return_type = self.kont_config['return_type']
        
        # Generate wrapper: bridges generic to specific env
        output.write(f"static {return_type} *{name}Wrapper(")
        output.write(f"{param_type} *param, KontEnv *env")
        output.write(") {\n")
        output.write(f"    return {name}Kont(param, ")
        output.write(f"getKontEnv_{ucFirst(spec['key'])}(env));\n")
        output.write("}\n\n")

def _write_constructors(self, output: TextIO, catalog: Catalog) -> None:
    output.write("// Constructor functions\n\n")
    
    for name, spec in self.continuations.items():
        kont_struct = self.kont_config['kont_struct']
        free_vars = spec.get('free_vars', {})
        
        output.write(f"static {kont_struct} *makeKont_{name}(")
        # ... generate parameter list from free_vars
        output.write(") {\n")
        output.write(f"    KontEnv *__env = makeKontEnv_{ucFirst(spec['key'])}(")
        output.write(", ".join(free_vars.keys()))
        output.Hybrid Approach
```

### 1. Massive Code Reuse
- **~1800 lines** of visitor boilerplate generated automatically
- **~500 lines** of continuation scaffolding generated automatically  
- ~80% of generator code shared between ANF and CPS
- Same patterns for environment structs, unions, wrappers
- **Total boilerplate savings: ~2300 lines per algorithm**

### 2. Separation of Concerns
- **Visitor generator**: Handles tree traversal, GC, immutability
- **Continuation generator**: Handles closure creation, type safety
- **User code**: Only algorithm-specific transformation logic
- Each layer is independently testable and maintainable

### 3. Type Safety at Multiple Levels
- **Visitor**: Compiler verifies field types during traversal
- **Continuation**: Compiler catches mismatches in environment structs
- **Runtime**: Type-safe accessors (`getKontEnv_*`) validate discriminated unions
- No runtime hash table lookups (ANF-REWRITE.md's original approach)

### 4. Performance
- **No hash table overhead**: Direct struct field access throughout
- **Compiler optimization**: Static inline-able wrapper functions
- **Lazy evaluation**: Only create new nodes when children change (visitor pattern)
- **Memory efficiency**: Fixed-size continuation structs vs dynamic hash tables

### 5. Debuggability
- **Typed structs visible**: Both visitor state and continuation environments in debugger
- **Stack traces clear**: Named functions for each case (`cpsTkLamIff`, `T_k_ifKont`)
- **Breakpoint precision**: Can break at exact transformation point
- **No opaque structures**: No hash tables or generic pointers to decode

### 6. Maintainability
- **YAML drives generation**: Change spec, regenerate, minimal manual updates
- **Consistent patterns**: Both ANF and CPS follow same architecture
- **Future algorithms easy**: Third algorithm just needs YAML spec + transformation logic
- **Generated code stays fresh**: Rebuild from YAML as schema evolvesntation

### Phase 3: Full CPS Implementation

1. Port all CPS transformations from F♮ to C
2. Integrate with lambda conversion pipeline
3. Compare performance: manual vs scaffolded
4. Document lessons learned

### Phase 4: Generalization Review

1. Identify any CPS-specific hacks needed
2. Abstract into configuration parameters
3. Update ANF-KONT and CPS-KONT documentation
4. Create generalized "Continuation Scaffolding Guide"

## CPS-Specific Challenges

### 1. Expression-Level Continuations

CPS has two kinds of continuations:
- **C continuations** (`CpsKont*`): Structured continuation objects (what we generate)
- **Expression continuations** (`Expr*`): Generated lambda expressions

The scaffolding handles both because they're just different field types. Example:
- ❌ **Does NOT use visitor pattern** (manual tree walking)

### For CPS-KONT (Proposed - With Visitor Pattern)
- [ ] **Generate visitor skeleton**: `tools/generate.py lambda.yaml visitor --target=cpsTk` (~1800 lines)
- [ ] **Generate continuation scaffolding**: From `cps_continuations.yaml` (~500 lines)
- [ ] **Support dual-transformer pattern**: T_k and T_c with separate visitor contexts
- [ ] **Handle mixed continuation types**: Both `CpsKont*` and `LamExp*` in same system
- [ ] **Port 3-4 CPS cases**: From F♮ to C, validate against [cps5.fn](../fn/rewrite/cps5.fn) output
- [ ] **No performance regression**: Benchmark performance vs hypothetical hand-written
- [ ] **Measure LOC savings**: Compare generated+user vs pure manual implementation

### For Generalization (Long-Term)
- [ ] **Visitor + Continuation pattern**: Document hybrid approach as standard practice
- [ ] **Third algorithm**: Test framework with different continuation-based transform
- [ ] **Unified documentation**: Cover ANF (no visitors), CPS (with visitors), patterns
- [ ] **Generator simplicity**: <5 config parameters for continuation scaffolding
- [ ] **Tooling integration**: Make visitor + continuation generation a single command

### Metrics to Track

| Metric | Manual (Estimated) | Generated (Hybrid) | Savings |
|--------|-------------------|--------------------|---------|
| Tree traversal boilerplate | ~2000 lines | ~1800 (generated) | **90%** |
| GC protection code | ~800 lines | ~0 (in visitor) | **100%** |
| Continuation scaffolding | ~700 lines | ~500 (generated) | **71%** |
| Transformation logic | ~2500 lines | ~2500 (manual) | 0% |
| **TOTAL** | **~6000 lines** | **~4800 lines** | **20%** |
| **User writes** | **~6000 lines** | **~2500 lines** | **58%** |

Note: User writes only transformation logic, not infrastructure.
### 2. Fresh Variable Allocation

CPS extensively uses `gensym()` to create fresh variables:

```fn
let
    rv = gensym("$rv");
    cont = E.lambda([rv], E.apply(c, [rv]));
in
    T_k(test, fn (atest) { /* uses cont */ })
```

**Not a scaffolding concern**: Fresh variable generation happens in user-written continuation bodies, not in generated scaffolding. The scaffolding just needs to capture and pass the generated `cont` variable as a free variable.

### 3. Mutual Recursion Between T_k and T_c

T_k calls T_c, T_c calls T_k. Both must be forward-declared.

**Solution**: Generated `.inc` file includes forward declarations for all user functions:
combination of **visitor pattern generation** and **continuation scaffolding** makes CPS transform implementation dramatically easier than initially anticipated:

### Findings

1. **Visitor Pattern Already Exists**
   - Command: `python3 tools/generate.py src/lambda.yaml visitor --target=cpsTk`
   - Generates: ~1800 lines of tree traversal, GC protection, immutability handling
   - **User doesn't write**: Tree walking, PROTECT/UNPROTECT, node creation boilerplate

2. **Continuation Scaffolding Generalizes**
   - 80% code reuse between ANF and CPS
   - Only need: parameterization for transformer names and types
   - **User doesn't write**: Environment structs, unions, wrappers, constructors

3. **Hybrid Approach is Optimal**
   - Visitor handles: Tree traversal infrastructure
   - Continuation scaffolding handles: Closure creation infrastructure  
   - User writes: Only transformation logic (~40% of total code)

### Code Savings Summary

| Component | Lines | Who Writes |
|-----------|-------|------------|
| T_k visitor boilerplate | ~1800 | **Generated** |
| T_c visitor boilerplate | ~1800 | **Generated** |
| T_k continuation scaffolding | ~250 | **Generated** |
| T_c continuation scaffolding | 0 | **Not needed** |
| Transformation logic | ~2500 | User |
| **Total** | **~6350** | **61% generated** |

Compare to pure manual: ~8000 lines, 100% manual

User writes 69% less code, and what they write is the interesting algorithm logic, not infrastructure.

Advantage: T_c just passes LamExp* as data - no continuation infrastructure needed.

### Recommendations

#### Immediate (Week 1)
1. ✅ **Validate approach**: Generate visitors for T_k and T_c
2. ✅ **Review proposal**: Gather feedback on hybrid approach
3. ⚠️ **Create CPS YAML**: Specify 5-10 continuations as proof-of-concept

#### Short-Term (Weeks 2-3)
4. **Implement continuation generator**: Parameterize for CPS config
5. **Port 3 cases**: Implement T_k_if, T_c_if, T_k_apply using hybrid approach
6. **Validate output**: Compare against [cps5.fn](../fn/rewrite/cps5.fn) results

#### Medium-Term (Month 2)
7. **Full CPS port**: Complete all ~40 transformation cases
8. **Integration**: Wire into lambda conversion pipeline
9. **Benchmarking**: Measure performance vs manual approach
10. **Documentation**: Update with lessons learned

#### Long-Term Benefit
- **Pattern established**: Visitor + Continuation = standard for transformation algorithms
- **Future algorithms**: Can reuse both generation systems
- **Maintenance burden**: Reduced by 50%+ through code generation
- **Type safety**: Compiler catches errors in both layers

**Final recommendation**: Proceed with hybrid approach. Generate visitors first (already works), then add parameterized continuation scaffolding. This is significantly easier than implementing either system from scratch.

### 1. Code Reuse
- ~80% of generator code shared between ANF and CPS
- Same patterns for environment structs, unions, wrappers
- Unified approach reduces maintenance burden

### 2. Consistency
- Both transformations use identical scaffolding patterns
- Easy to understand one after learning the other
- Future continuation-based algorithms can follow same pattern

### 3. Type Safety
- Compiler catches field mismatches in continuation environments
- No runtime hash table lookups (ANF-REWRITE.md's original approach)
- Direct field access with type checking

### 4. Performance
- No hash table overhead
- Direct struct field access
- Compiler can inline wrapper functions

### 5. Debuggability
- Typed environment structs visible in debugger
- Can inspect continuation closures at breakpoints
- No opaque `LamMap` hash tables to decode

## Risks and Mitigation

### Risk 1: Over-Generalization

Problem: Adding too many configuration parameters makes generator complex  
Mitigation: Start with minimal parameterization, add only when needed for second algorithm (CPS)

### Risk 2: CPS-Specific Quirks

Problem: CPS may have unique needs not covered by ANF patterns  
Mitigation: F♮ implementation in [cps5.fn](fn/rewrite/cps5.fn) already works, so patterns are validated. Port incrementally to identify issues early.

### Risk 3: Performance Regression

Problem: Generated scaffolding might be slower than hand-written code  
Mitigation: 
- Wrappers are static inline-able
- Struct field access vs hash lookup is faster
- Benchmark performance

### Risk 4: Debugging Difficulty

Problem: Generated code harder to debug than manual code  
Mitigation:
- Generated code is readable C with comments
- `.inc` file can be inspected directly
- User-written continuation bodies are normal C functions

## Success Criteria

### For ANF-KONT (Already Achieved)
- ✅ Reduces boilerplate from ~15 to ~8 lines per continuation
- ✅ Type-safe environment access
- ✅ Automatic GC integration
- ✅ Maintainable YAML specifications

### For CPS-KONT (Proposed)
- [ ] Generate scaffolding from YAML with minimal config changes
- [ ] Support dual-transformer pattern (T_k and T_c)
- [ ] Handle expression-level and structured continuations
- [ ] Port 3-4 CPS cases from F♮ to C and validate correctness
- [ ] No performance regression vs hand-written code
- [ ] Documentation shows clear generalization patterns

### For Generalization (Long-Term)
- [ ] Third continuation-based algorithm can reuse framework with <100 LOC changes
- [ ] Unified documentation covers ANF, CPS, and generalization patterns
- [ ] Generator has <5 configuration parameters (avoid complexity explosion)

## Recommended Next Steps

### Immediate (Week 1-2)
1. **Review Proposal**: Gather feedback on generalization approach
2. **Update ANF-KONT**: Add `continuation_config` to ANF YAML (backwards compatible)
3. **Refactor Generator**: Extract parameterized methods in `KontinuationGenerator`

### Short-Term (Week 3-4)
4. **Create CPS YAML**: Specify first 10 CPS continuations from [cps5.fn](fn/rewrite/cps5.fn)
5. **Generate CPS Scaffolding**: Test generation with CPS config
6. **Port 3 CPS Cases**: Implement `T_k_if`, `T_c_if`, `T_k_apply` in C

### Medium-Term (Month 2)
7. **Full CPS Port**: Complete all ~40 CPS continuations
8. **Integration**: Wire CPS transform into lambda conversion pipeline
9. **Validation**: Compare output against F♮ CPS implementation
10. **Documentation**: Update ANF-KONT.md with generalization lessons

### Long-Term (Month 3+)
11. **Performance Analysis**: Benchmark ANF vs CPS scaffolded code
12. **Generalization Guide**: Document patterns for future algorithms
13. **Consider**: Other continuation-based algorithms that could benefit

## Conclusion

The ANF continuation scaffolding generator is generalizable to CPS transform with minimal changes:

- 80% code reuse: Core scaffolding generation (structs, unions, wrappers) is identical
- 20% parameterization: Add transformer names, type names, and count to config
- Zero breaking changes: ANF-KONT continues to work with default config values
- Validated patterns: F♮ CPS implementation proves the continuation patterns work

The main work is:
1. Adding `continuation_config` to YAML schema
2. Updating generator to use config parameters instead of hard-coded names
3. Specifying 30-40 CPS continuations in YAML
4. Writing user continuation bodies in C (the actual algorithm logic)

This investment will provide benefits:
- Future continuation-based algorithms can reuse framework
- Consistent patterns across codebase
- Less boilerplate to maintain
- Type-safe, debuggable generated code

Recommendation: Proceed with generalization. Start by making ANF-KONT parameterized (non-breaking), then implement CPS-KONT to validate the approach.
