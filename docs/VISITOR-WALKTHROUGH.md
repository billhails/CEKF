# Visitor Pattern Walkthrough: Pretty-Printing Example

Status: EXPLORATION COMPLETED - NOT PROCEEDING WITH IMPLEMENTATION

**Conclusion**: The visitor pattern does not provide sufficient benefit for CEKF. The fundamental issue is that CEKF transformations require interleaved control (operation before/between/after child visits), which classic visitor pattern doesn't support. What we'd actually build is just "typed dispatch table generation" - a modest improvement that eliminates switch statements but doesn't reduce the complexity of visitor functions themselves. The cost/benefit ratio doesn't justify the implementation effort.

**Key Learnings**:

1. Pretty-printing needs control before, between, and after visiting children (e.g., printing delimiters)
2. Classic visitor pattern assumes framework controls traversal, but CEKF needs manual traversal control
3. Switch elimination saves ~200 lines but visitor functions remain unchanged
4. Continuation scaffolding (from VISITOR.md Phase 2) would save ~700 lines but can be done independently
5. Better to focus on direct ANF rewrite (see ANF-REWRITE.md) than build visitor infrastructure

**Decision**: Abandon visitor pattern approach. Return to manual ANF continuation-passing style implementation.

---

## Goal

Walk through implementing a visitor pattern for `lambda_pp.c` to identify challenges and validate the approach before applying to more complex transformations.

## Current Implementation Analysis

### The Switch Statement (lambda_pp.c:82-173)

```c
void ppLamExp(LamExp *exp) {
    if (exp == NULL) {
        eprintf("<NULL exp>");
        return;
    }
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            ppLamLam(exp->val.lam);
            break;
        case LAMEXP_TYPE_VAR:
            ppHashSymbol(exp->val.var);
            break;
        case LAMEXP_TYPE_BIGINTEGER:
            fprintMaybeBigInt(errout, exp->val.biginteger);
            break;
        case LAMEXP_TYPE_STDINT:
            eprintf("%d", exp->val.stdint);
            break;
        // ... 27 more cases
        default:
            cant_happen("unrecognized type %s", lamExpTypeName(exp->type));
    }
}
```

**31 total variants in LamExp union** (from lambda.yaml:512-545)

### Key Observations

1. **No context needed**: Pretty-printing doesn't pass state between calls
2. **No return value**: Uses side-effect (eprintf) instead of returning strings
3. **Mixed complexity**: Some cases inline (stdint), others call helpers (lam, apply)
4. **Primitive type handling**: HashSymbol, character, void_ptr variants need special treatment
5. **NULL handling**: Defensive checks at top of function
6. **⚠️ CONTROL FLOW AROUND RECURSION**: Pretty-printers need control before, between, and after visiting children!

## Challenge 0: The Fundamental Pretty-Printing Problem (CRITICAL)

### The Problem

**Classic visitor pattern assumes post-order traversal** - visit children first, then operate on parent. This works fine for transformations that build new structures from the bottom up.

**Pretty-printing requires interleaved control** - operation on parent is split across multiple points in the traversal:

```c
void ppLamLam(LamLam *lam) {
    eprintf("(lambda ");      // ← BEFORE child 1
    ppLamVarList(lam->args);  // ← VISIT child 1
    eprintf(" ");             // ← BETWEEN child 1 and child 2
    ppLamExp(lam->exp);       // ← VISIT child 2
    eprintf(")");             // ← AFTER child 2
}

void ppLamIff(LamIff *iff) {
    eprintf("(if ");             // ← BEFORE child 1
    ppLamExp(iff->condition);    // ← VISIT child 1
    eprintf(" ");                // ← BETWEEN child 1 and child 2
    ppLamExp(iff->consequent);   // ← VISIT child 2
    eprintf(" ");                // ← BETWEEN child 2 and child 3
    ppLamExp(iff->alternative);  // ← VISIT child 3
    eprintf(")");                // ← AFTER child 3
}

void ppLamApply(LamApply *apply) {
    eprintf("(");                 // ← BEFORE children
    ppLamExp(apply->function);    // ← VISIT child 1
    _ppLamArgs(apply->args);      // ← VISIT child 2 (which visits N children with spaces between)
    eprintf(")");                 // ← AFTER children
}
```

**Visitor pattern as typically implemented doesn't support this!**

Classic visitor separates traversal strategy from operation:

- **Traversal code** (generated): Visits children in fixed order
- **Visitor function** (user-written): Operates on already-visited children

But pretty-printing needs to **control the traversal itself** - printing delimiters at specific points.

### Why This Matters for Other Transformations

This isn't just a pretty-printing problem! Consider:

**ANF normalization** also needs interleaved control:

```c
// Need to normalize arg1, THEN build continuation with result, THEN normalize arg2
normalize(apply(f, arg1, arg2)) =
    normalize(arg1, λx1. normalize(arg2, λx2. k(apply(f, x1, x2))))
    ^              ^                    ^
    Child 1        Between              Child 2
```

**Type checking** needs control flow:

```c
// Check condition first, unify result with bool, THEN check branches with updated context
typecheck(if(cond, cons, alt)) = 
    let τ_cond = infer(cond) in
    unify(τ_cond, bool);          // ← BETWEEN child 1 and children 2,3
    let τ_cons = infer(cons) in
    let τ_alt = infer(alt) in
    unify(τ_cons, τ_alt)          // ← AFTER all children
```

### Solution Options

#### Option A: Visitor Controls Traversal (Manual Recursion)

**Don't** generate automatic child traversal. Visitor function manually recurses:

```c
// Visitor has full control - manually calls recursion
void visitLam_pp(LamLam *lam) {
    eprintf("(lambda ");
    visitLamVarList(lam->args, &ppVisitorTable);  // Manual recursion
    eprintf(" ");
    visitLamExp(lam->exp, &ppVisitorTable);       // Manual recursion
    eprintf(")");
}

// Visitor table just dispatches, doesn't traverse
void visitLamExp(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    if (exp == NULL) { /* ... */ return; }
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            vtable->visitLam(exp->val.lam);  // Visitor controls recursion
            break;
        // ...
    }
}
```

**Pros**:

- Full control over traversal order
- Can print before/between/after children
- Natural for pretty-printing and complex transformations

**Cons**:

- Not really "visitor pattern" anymore - just switch elimination
- No benefit from generated traversal helpers
- Visitor functions have more boilerplate (manual recursion calls)

#### Option B: Multi-Phase Visitors (Pre/In/Post)

Provide hooks at different traversal phases:

```c
typedef struct {
    void (*visitLam_pre)(LamLam *lam);   // Called BEFORE children
    void (*visitLam_in)(LamLam *lam);    // Called BETWEEN children (with index)
    void (*visitLam_post)(LamLam *lam);  // Called AFTER children
} LamExpPrettyPrinterTable;

// Generated traversal
void visitLamExp(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            if (vtable->visitLam_pre)
                vtable->visitLam_pre(exp->val.lam);  // "(lambda "
            
            visitLamVarList(exp->val.lam->args, vtable);  // args
            
            if (vtable->visitLam_in)
                vtable->visitLam_in(exp->val.lam);   // " "
            
            visitLamExp(exp->val.lam->exp, vtable);  // body
            
            if (vtable->visitLam_post)
                vtable->visitLam_post(exp->val.lam); // ")"
            break;
    }
}
```

**Pros**:

- Automated traversal (generated from schema)
- Hooks at precise moments
- Could work for pretty-printing

**Cons**:

- Complex to generate (need to know child structure from YAML)
- Visitor table bloats (3+ entries per variant)
- Hard to handle variable-length children (lists, args)
- Doesn't help with conditional logic (e.g., "print space only if not last element")

#### Option C: Callback-Based Traversal

Visitor receives callbacks to visit children:

```c
typedef struct {
    void (*visitChild)(LamExp *child);  // Callback to visit any child
    void (*visitVarList)(LamVarList *list);
} VisitorContext;

void visitLam_pp(LamLam *lam, VisitorContext *ctx) {
    eprintf("(lambda ");
    ctx->visitVarList(lam->args);  // Use callback
    eprintf(" ");
    ctx->visitChild(lam->exp);     // Use callback
    eprintf(")");
}
```

**Pros**:

- Visitor has full control over when children are visited
- Callbacks can be specialized (different printing contexts)

**Cons**:

- Complex to set up (need to create context with right callbacks)
- Not much simpler than manual recursion
- Indirection overhead (callback through context)

#### Option D: Abandon Visitor for Pretty-Printing, Keep for Transformations

**Accept that pretty-printing is special** and doesn't benefit from visitor pattern.

- Keep manual pretty-printers as-is (or just eliminate the switch with simple dispatch)
- Use visitor pattern for **transformations** that build new structures (ANF, bytecode)

**Pros**:

- Right tool for right job
- Pretty-printers stay simple and readable
- Visitor pattern focuses on cases where it actually helps

**Cons**:

- Still have switch statements in pretty-printers
- Two different patterns in codebase

### Analysis: What Do We Actually Gain?

Let's be honest about what the visitor pattern buys us:

**For pretty-printing**:

- ❌ Can't eliminate traversal logic (visitor needs to control it)
- ✓ Can eliminate type dispatch switch (replace with table lookup)
- ✓ Can auto-generate NULL handling
- ✓ Slight improvement in clarity (visitor table shows structure)

**Net benefit**: ~30 lines saved (switch → table), but visitor functions unchanged

**For transformations (ANF, bytecode, type checking)**:

- ❌ Still can't auto-generate traversal (complex control flow needed)
- ✓ Can eliminate type dispatch switch
- ✓ Can generate continuation scaffolding (bigger win!)
- ✓ Type-safe dispatch

**Net benefit**: Switch elimination + continuation scaffolding = significant

### Recommendation: Hybrid Approach

**Option A (visitor controls traversal) is the pragmatic choice**:

1. **Generate simple dispatch tables** - eliminate switch statements
2. **Visitor functions control traversal** - manually recurse to children
3. **For transformations, also generate continuation scaffolding** - the real win

This is honest about what we're doing:

- Not a "pure" visitor pattern (no automatic traversal)
- But still eliminates switch boilerplate
- And enables continuation scaffolding for complex transformations

**Updated YAML config**:

```yaml
visitors:
  - name: PrettyPrinter
    union: LamExp
    return_type: void
    context_type: null
    auto_traverse: false  # Visitor controls traversal
    inline_primitives:
      stdint: 'eprintf("%d", val)'
```

**Generated code** (revised):

```c
// No automatic traversal helpers - visitor calls recursion manually
void visitLamExp(LamExp *exp, LamExpPrettyPrinterTable *vtable);

// Visitor has full control
void visitLam_pp(LamLam *lam) {
    eprintf("(lambda ");
    visitLamVarList(lam->args, &ppVisitorTable);  // Manual
    eprintf(" ");
    visitLamExp(lam->exp, &ppVisitorTable);       // Manual
    eprintf(")");
}
```

This is essentially **switch elimination with typed dispatch**, not a classic visitor pattern.

**But that's okay!** The switch elimination alone is valuable, and the continuation scaffolding (Phase 2 of VISITOR.md) is the real prize.

## Challenge 1: Return Type and Context

### Problem

The visitor pattern assumes:

```c
typedef void* (*LamExpVisitFn)(void *node, void *context);
```

But pretty-printing has signature:

```c
void ppLamLam(LamLam *lam);  // No context, no return
```

### Solution Options

#### Option A: Use NULL for unused parameters

```c
// Wrapper functions adapt existing pretty-printers
static void* visitLam_pp(void *node, void *context) {
    ppLamLam((LamLam*)node);
    return NULL;  // Unused
}

static LamExpVisitorTable ppVisitorTable = {
    .visitLam = visitLam_pp,
    .visitVar = visitVar_pp,
    // ...
};
```

**Pros**:

- Minimal changes to existing code
- Clear separation between visitor infrastructure and implementation

**Cons**:

- Wrapper boilerplate for every variant (31 wrappers!)
- Extra function call overhead

#### Option B: Generate typed visitors per use-case

Instead of generic `void*`, generate specific signatures:

```yaml
# lambda.yaml
config:
  visitors:
    - name: LamExpPrettyPrinter
      union: LamExp
      return_type: void
      context_type: void  # or omit entirely
      
    - name: LamExpNormalizer
      union: LamExp  
      return_type: AnfExp*
      context_type: AnfExp*  # the "tail"
```

Generated:

```c
// lambda_pp_visitor.h
typedef void (*LamExpPrettyPrintFn)(void *node);  // No context!

typedef struct LamExpPrettyPrinterTable {
    LamExpPrettyPrintFn visitLam;
    LamExpPrettyPrintFn visitVar;
    // ...
} LamExpPrettyPrinterTable;

void visitLamExpPrettyPrint(LamExp *exp, LamExpPrettyPrinterTable *vtable);
```

**Pros**:

- Type-safe, no wrappers needed
- Can directly use existing functions

**Cons**:

- More complex code generation
- Need to specify visitor config for each use case

**Decision**: **Option B** - Generate use-case-specific visitors. The type safety and zero-overhead are worth the extra generator complexity.

## Challenge 2: Primitive Type Variants

### Problem

Not all union variants are structs:

```yaml
# From lambda.yaml LamExp union
data:
  lam: LamLam           # Pointer to struct ✓
  var: HashSymbol       # Pointer to struct ✓
  stdint: int           # Primitive value! ✗
  character: character  # Primitive value! ✗
  biginteger: MaybeBigInt  # Pointer to struct ✓
  env: void_ptr         # void* ✗
  back: void_ptr        # void* ✗
  error: void_ptr       # void* ✗
```

Current switch can access these directly:

```c
case LAMEXP_TYPE_STDINT:
    eprintf("%d", exp->val.stdint);  // Direct access to int
    break;
```

Visitor receives `void *node` - can't pass int by value!

### Solution Options

#### Option A: Pass pointer to union variant slot

```c
void visitLamExpPrettyPrint(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    switch (exp->type) {
        case LAMEXP_TYPE_STDINT:
            if (vtable->visitStdint)
                vtable->visitStdint(&exp->val.stdint);  // Pass &int
            break;
        // ...
    }
}

// Visitor function signature
void ppStdInt(int *val) {
    eprintf("%d", *val);  // Dereference
}
```

**Pros**: Uniform interface (everything is pointer)

**Cons**: Awkward API (why pass pointer to stack int?)

#### Option B: Special-case primitives in generator

Generate different signatures for primitive variants:

```c
// Generated dispatch knows which are primitives
void visitLamExpPrettyPrint(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            vtable->visitLam(exp->val.lam);  // Pointer
            break;
        case LAMEXP_TYPE_STDINT:
            vtable->visitStdint(exp->val.stdint);  // By value
            break;
        case LAMEXP_TYPE_VAR:
            vtable->visitVar(exp->val.var);  // Pointer (HashSymbol*)
            break;
    }
}
```

But then visitor table has mixed signatures:

```c
typedef struct {
    void (*visitLam)(LamLam *lam);
    void (*visitStdint)(int val);  // Different signature!
    void (*visitVar)(HashSymbol *var);
} LamExpPrettyPrinterTable;
```

**Pros**: Natural API, matches existing code

**Cons**: Type-heterogeneous function pointer table (is this even legal C?)

#### Option C: Inline primitives in generated dispatch

Don't even call visitor for trivial primitives:

```c
void visitLamExpPrettyPrint(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    switch (exp->type) {
        case LAMEXP_TYPE_STDINT:
            eprintf("%d", exp->val.stdint);  // Inline, no visitor call
            break;
        case LAMEXP_TYPE_CHARACTER:
            eprintf("\"%c\"", exp->val.character);  // Inline
            break;
        case LAMEXP_TYPE_LAM:
            if (vtable->visitLam)
                vtable->visitLam(exp->val.lam);
            break;
        // ...
    }
}
```

Visitor table only has entries for complex types:

```c
typedef struct {
    void (*visitLam)(LamLam *lam);
    void (*visitVar)(HashSymbol *var);
    void (*visitApply)(LamApply *apply);
    // No visitStdint, visitCharacter
} LamExpPrettyPrinterTable;
```

**Pros**:

- Simplifies visitor table
- Matches current implementation pattern
- No overhead for trivial cases

**Cons**:

- Hard-codes primitive handling (what if user wants custom int printing?)
- Less flexible

**Decision**: **Option B with variant function pointers** - Use C11 `_Generic` or union of function pointers to handle type variance properly. See detailed solution below.

## Challenge 3: NULL Handling

### Problem

Current code checks NULL at top of function:

```c
void ppLamExp(LamExp *exp) {
    if (exp == NULL) {
        eprintf("<NULL exp>");
        return;
    }
    // ... switch
}
```

With visitor pattern, where does this check live?

### Solution Options

#### Option A: In generated dispatch

```c
void visitLamExpPrettyPrint(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    if (exp == NULL) {
        if (vtable->visitNull)
            vtable->visitNull();
        return;
    }
    // ... switch
}
```

#### Option B: Caller responsibility

```c
void ppLamExp(LamExp *exp) {
    if (exp == NULL) {
        eprintf("<NULL exp>");
        return;
    }
    visitLamExpPrettyPrint(exp, &ppVisitorTable);
}
```

**Decision**: **Option A** - Generated dispatch handles NULL with optional `visitNull` callback. Cleaner and more consistent.

## Challenge 4: Helper Function Consistency

### Problem

Some cases are trivial:

```c
case LAMEXP_TYPE_BACK:
    eprintf("(back)");
    break;
```

Others call complex helpers:

```c
case LAMEXP_TYPE_APPLY:
    ppLamApply(exp->val.apply);  // Separate function
    break;
```

Should we force all variants to use separate functions?

### Solution

**Allow both patterns** - visitor table entries can be NULL:

```c
void visitLamExpPrettyPrint(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    switch (exp->type) {
        case LAMEXP_TYPE_BACK:
            eprintf("(back)");  // Inline trivial case
            break;
        case LAMEXP_TYPE_APPLY:
            if (vtable->visitApply)
                vtable->visitApply(exp->val.apply);
            break;
    }
}

static LamExpPrettyPrinterTable ppVisitorTable = {
    .visitApply = ppLamApply,  // Set for complex cases
    // .visitBack remains NULL - handled inline
};
```

**Benefit**: Gradual migration path - can move to visitor pattern incrementally.

## Proposed Generated Code

### Configuration in lambda.yaml

```yaml
config:
  name: lambda
  # ... existing config
  visitors:
    - name: PrettyPrinter
      union: LamExp
      return_type: void
      context_type: null  # No context needed
      null_handler: true  # Generate NULL check
      inline_primitives:  # These are printed inline, not via visitor
        - stdint: 'eprintf("%d", val)'
        - character: 'eprintf("\"%%c\"", val)'
        - back: 'eprintf("(back)")'
        - error: 'eprintf("(error)")'
        - env: 'eprintf("env")'
```

### Generated Header (generated/lambda_pp_visitor.h)

```c
#ifndef lambda_pp_visitor_h
#define lambda_pp_visitor_h

#include "lambda.h"

/**
 * Visitor function types for LamExp pretty-printing.
 * Each variant has a specific signature matching its type.
 */

// Visitor table - only complex types need entries
typedef struct LamExpPrettyPrinterTable {
    void (*visitNull)(void);  // Optional: called for NULL exp
    void (*visitLam)(LamLam *lam);
    void (*visitVar)(HashSymbol *var);
    void (*visitBiginteger)(MaybeBigInt bigint);
    void (*visitPrim)(LamPrimApp *prim);
    void (*visitSequence)(LamSequence *seq);
    void (*visitMakeVec)(LamMakeVec *makeVec);
    void (*visitDeconstruct)(LamDeconstruct *deconstruct);
    void (*visitConstruct)(LamConstruct *construct);
    void (*visitTag)(LamExp *tag);
    void (*visitConstant)(LamConstant *constant);
    void (*visitApply)(LamApply *apply);
    void (*visitIff)(LamIff *iff);
    void (*visitCallcc)(LamExp *callcc);
    void (*visitPrint)(LamPrint *print);
    void (*visitLetrec)(LamLetRec *letrec);
    void (*visitTypedefs)(LamTypeDefs *typedefs);
    void (*visitLet)(LamLet *let);
    void (*visitMatch)(LamMatch *match);
    void (*visitCond)(LamCond *cond);
    void (*visitAmb)(LamAmb *amb);
    void (*visitTupleIndex)(LamTupleIndex *tupleIndex);
    void (*visitMakeTuple)(LamArgs *args);
    void (*visitNamespaces)(LamNamespaceArray *namespaces);
    void (*visitConstructor)(LamTypeConstructorInfo *constructor);
    void (*visitLookup)(LamLookup *lookup);
    void (*visitTypeof)(LamTypeof *typeOf);
} LamExpPrettyPrinterTable;

/**
 * Main dispatch function for pretty-printing LamExp.
 * Handles NULL check and primitives automatically.
 * Calls visitor table functions for complex types.
 */
void visitLamExpPrettyPrint(LamExp *exp, LamExpPrettyPrinterTable *vtable);

#endif
```

### Generated Implementation (generated/lambda_pp_visitor.c)

```c
#include "lambda_pp_visitor.h"
#include "common.h"

void visitLamExpPrettyPrint(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    // NULL check
    if (exp == NULL) {
        if (vtable && vtable->visitNull) {
            vtable->visitNull();
        } else {
            eprintf("<NULL exp>");  // Default NULL handling
        }
        return;
    }
    
    // Dispatch based on type
    switch (exp->type) {
        // Inline primitive cases (from yaml config)
        case LAMEXP_TYPE_STDINT:
            eprintf("%d", exp->val.stdint);
            break;
            
        case LAMEXP_TYPE_CHARACTER:
            eprintf("\"%c\"", exp->val.character);
            break;
            
        case LAMEXP_TYPE_BACK:
            eprintf("(back)");
            break;
            
        case LAMEXP_TYPE_ERROR:
            eprintf("(error)");
            break;
            
        case LAMEXP_TYPE_ENV:
            eprintf("env");
            break;
        
        // Complex types - call visitor functions
        case LAMEXP_TYPE_LAM:
            if (vtable && vtable->visitLam)
                vtable->visitLam(exp->val.lam);
            break;
            
        case LAMEXP_TYPE_VAR:
            if (vtable && vtable->visitVar)
                vtable->visitVar(exp->val.var);
            break;
            
        case LAMEXP_TYPE_BIGINTEGER:
            if (vtable && vtable->visitBiginteger)
                vtable->visitBiginteger(exp->val.biginteger);
            break;
            
        case LAMEXP_TYPE_PRIM:
            if (vtable && vtable->visitPrim)
                vtable->visitPrim(exp->val.prim);
            break;
            
        case LAMEXP_TYPE_SEQUENCE:
            if (vtable && vtable->visitSequence)
                vtable->visitSequence(exp->val.sequence);
            break;
            
        case LAMEXP_TYPE_MAKEVEC:
            if (vtable && vtable->visitMakeVec)
                vtable->visitMakeVec(exp->val.makeVec);
            break;
            
        case LAMEXP_TYPE_DECONSTRUCT:
            if (vtable && vtable->visitDeconstruct)
                vtable->visitDeconstruct(exp->val.deconstruct);
            break;
            
        case LAMEXP_TYPE_CONSTRUCT:
            if (vtable && vtable->visitConstruct)
                vtable->visitConstruct(exp->val.construct);
            break;
            
        case LAMEXP_TYPE_TAG:
            if (vtable && vtable->visitTag)
                vtable->visitTag(exp->val.tag);
            break;
            
        case LAMEXP_TYPE_CONSTANT:
            if (vtable && vtable->visitConstant)
                vtable->visitConstant(exp->val.constant);
            break;
            
        case LAMEXP_TYPE_APPLY:
            if (vtable && vtable->visitApply)
                vtable->visitApply(exp->val.apply);
            break;
            
        case LAMEXP_TYPE_IFF:
            if (vtable && vtable->visitIff)
                vtable->visitIff(exp->val.iff);
            break;
            
        case LAMEXP_TYPE_CALLCC:
            if (vtable && vtable->visitCallcc)
                vtable->visitCallcc(exp->val.callcc);
            break;
            
        case LAMEXP_TYPE_PRINT:
            if (vtable && vtable->visitPrint)
                vtable->visitPrint(exp->val.print);
            break;
            
        case LAMEXP_TYPE_LETREC:
            if (vtable && vtable->visitLetrec)
                vtable->visitLetrec(exp->val.letrec);
            break;
            
        case LAMEXP_TYPE_TYPEDEFS:
            if (vtable && vtable->visitTypedefs)
                vtable->visitTypedefs(exp->val.typedefs);
            break;
            
        case LAMEXP_TYPE_LET:
            if (vtable && vtable->visitLet)
                vtable->visitLet(exp->val.let);
            break;
            
        case LAMEXP_TYPE_MATCH:
            if (vtable && vtable->visitMatch)
                vtable->visitMatch(exp->val.match);
            break;
            
        case LAMEXP_TYPE_COND:
            if (vtable && vtable->visitCond)
                vtable->visitCond(exp->val.cond);
            break;
            
        case LAMEXP_TYPE_AMB:
            if (vtable && vtable->visitAmb)
                vtable->visitAmb(exp->val.amb);
            break;
            
        case LAMEXP_TYPE_TUPLE_INDEX:
            if (vtable && vtable->visitTupleIndex)
                vtable->visitTupleIndex(exp->val.tuple_index);
            break;
            
        case LAMEXP_TYPE_MAKE_TUPLE:
            if (vtable && vtable->visitMakeTuple)
                vtable->visitMakeTuple(exp->val.make_tuple);
            break;
            
        case LAMEXP_TYPE_NAMESPACES:
            if (vtable && vtable->visitNamespaces)
                vtable->visitNamespaces(exp->val.namespaces);
            break;
            
        case LAMEXP_TYPE_CONSTRUCTOR:
            if (vtable && vtable->visitConstructor)
                vtable->visitConstructor(exp->val.constructor);
            break;
            
        case LAMEXP_TYPE_LOOKUP:
            if (vtable && vtable->visitLookup)
                vtable->visitLookup(exp->val.lookup);
            break;
            
        case LAMEXP_TYPE_TYPEOF:
            if (vtable && vtable->visitTypeof)
                vtable->visitTypeof(exp->val.typeOf);
            break;
            
        default:
            cant_happen("unrecognized LamExp type %s", lamExpTypeName(exp->type));
    }
}
```

### Refactored lambda_pp.c

```c
#include "lambda_pp_visitor.h"

// Existing helper functions unchanged
void ppLamLam(LamLam *lam) { /* ... existing code ... */ }
void ppLamAmb(LamAmb *amb) { /* ... existing code ... */ }
void ppLamApply(LamApply *apply) { /* ... existing code ... */ }
// ... all other pp* functions

// Special handler for biginteger (inline in current code)
static void ppBigInteger(MaybeBigInt bigint) {
    fprintMaybeBigInt(errout, bigint);
}

// Special handler for constructor (inline in current code)  
static void ppConstructor(LamTypeConstructorInfo *constructor) {
    eprintf("constructor:%s", constructor->name->name);
}

// Visitor table - maps variants to existing functions
static LamExpPrettyPrinterTable ppVisitorTable = {
    .visitLam = ppLamLam,
    .visitVar = ppHashSymbol,
    .visitBiginteger = ppBigInteger,
    .visitPrim = ppLamPrimApp,
    .visitSequence = ppLamSequence,
    .visitMakeVec = ppLamMakeVec,
    .visitDeconstruct = ppLamDeconstruct,
    .visitConstruct = ppLamConstruct,
    .visitTag = ppLamTag,
    .visitConstant = ppLamConstant,
    .visitApply = ppLamApply,
    .visitIff = ppLamIff,
    .visitCallcc = ppLamExp,  // Recursive!
    .visitPrint = ppLamPrint,
    .visitLetrec = ppLamLetRec,
    .visitTypedefs = ppLamTypeDefs,
    .visitLet = ppLamLet,
    .visitMatch = ppLamMatch,
    .visitCond = ppLamCond,
    .visitAmb = ppLamAmb,
    .visitTupleIndex = ppLamTupleIndex,
    .visitMakeTuple = ppLamMakeTuple,
    .visitNamespaces = ppLamNamespaces,
    .visitConstructor = ppConstructor,
    .visitLookup = ppLamLookup,
    .visitTypeof = ppLamTypeof,  // Need to add this function
};

// Main entry point - now trivial!
void ppLamExp(LamExp *exp) {
    visitLamExpPrettyPrint(exp, &ppVisitorTable);
}

void ppLamExpD(LamExp *exp, int depth) {
    while (depth > 0) {
        depth--;
        eprintf("  ");
    }
    ppLamExp(exp);
}
```

**Code reduction**:

- **Before**: ~92 lines in switch statement
- **After**: ~30 lines in visitor table initialization
- **Savings**: ~62 lines, plus the switch logic is now generated

## Challenge 5: Recursive Visitors

### Problem

Some variants are recursive:

```c
case LAMEXP_TYPE_CALLCC:
    ppLamExp(exp->val.callcc);  // Calls ppLamExp recursively!
    break;
    
case LAMEXP_TYPE_TAG:
    ppLamTag(exp->val.tag);  // ppLamTag also calls ppLamExp
    break;
```

With visitor table, we need:

```c
.visitCallcc = ppLamExp,  // Points back to main dispatcher!
```

But `ppLamExp` now just calls the visitor - circular dependency!

### Solution

The visitor table needs a reference to the dispatch function:

```c
// Main dispatcher
void ppLamExp(LamExp *exp) {
    visitLamExpPrettyPrint(exp, &ppVisitorTable);
}

// Recursive variant handler  
static void ppLamCallcc(LamExp *callcc) {
    ppLamExp(callcc);  // OK - calls dispatcher
}

static LamExpPrettyPrinterTable ppVisitorTable = {
    .visitCallcc = ppLamCallcc,  // Wrapper that calls ppLamExp
    // ...
};
```

**Alternative**: Generate recursive helper:

```c
// Generated in lambda_pp_visitor.c
void visitLamExpRecursive(LamExp *exp, LamExpPrettyPrinterTable *vtable) {
    visitLamExpPrettyPrint(exp, vtable);
}

// User code
static void ppLamCallcc(LamExp *callcc) {
    visitLamExpRecursive(callcc, &ppVisitorTable);  // Use helper
}
```

**Decision**: **Generated recursive helper** - Makes pattern explicit and reusable.

## Challenge 6: Missing Functions

Current code has:

```c
case LAMEXP_TYPE_CONSTRUCTOR:
    eprintf("constructor:%s", exp->val.constructor->name->name);
    break;
```

But no `ppLamTypeConstructorInfo()` function exists. Should we:

1. **Force creation** of all helper functions?
2. **Allow inline** handling in visitor table setup?
3. **Generate stubs** for missing functions?

### Solution

**Allow inline lambdas in C (C99 compound literals)**:

```c
static void ppConstructor(LamTypeConstructorInfo *constructor) {
    eprintf("constructor:%s", constructor->name->name);
}

static LamExpPrettyPrinterTable ppVisitorTable = {
    .visitConstructor = ppConstructor,
    // ...
};
```

Or use macro to define inline:

```c
#define INLINE_VISITOR(name, type, body) \
    static void name(type *arg) body

INLINE_VISITOR(ppConstructor, LamTypeConstructorInfo, {
    eprintf("constructor:%s", arg->name->name);
})

static LamExpPrettyPrinterTable ppVisitorTable = {
    .visitConstructor = ppConstructor,
};
```

**Decision**: **Add small helper functions** - keeps visitor table clean and functions testable.

## Benefits Realized

### 1. Eliminates Switch Statement Boilerplate

**Before**: 92 lines of switch cases
**After**: 30 lines of visitor table initialization

### 2. Type-Safe Dispatch

Each visitor function has the correct signature for its type:

```c
void (*visitLam)(LamLam *lam);           // Not void*
void (*visitApply)(LamApply *apply);     // Not void*
```

Compiler catches type mismatches at compile time.

### 3. Incremental Migration

Can move to visitor pattern gradually:

```c
static LamExpPrettyPrinterTable ppVisitorTable = {
    .visitLam = ppLamLam,     // Migrated
    .visitApply = ppLamApply, // Migrated
    // NULL entries for not-yet-migrated variants fall back to inline code
};
```

### 4. Clear API Surface

Visitor table shows at-a-glance which variants have complex handling:

```c
// These variants have dedicated functions:
.visitApply = ppLamApply,
.visitIff = ppLamIff,
.visitMatch = ppLamMatch,

// These are handled inline (NULL entries):
// .visitBack - prints "(back)"
// .visitError - prints "(error)"
// .visitEnv - prints "env"
```

### 5. Easier Testing

Can test individual visitor functions without full AST:

```c
void test_ppLamApply() {
    LamApply *apply = /* construct test case */;
    ppLamApply(apply);
    // Verify output
}
```

## Challenges Identified

### 1. Primitive Type Handling

**Issue**: Union variants can be primitives (int, char) or pointers (struct*)

**Solution**:

- Inline primitives in generated dispatch (configured via YAML)
- Only generate visitor table entries for complex types
- Allow custom inline code via YAML config

### 2. Type-Specific Function Signatures

**Issue**: Generic `void*` approach loses type information

**Solution**:

- Generate use-case-specific visitor types
- Each visitor table has typed function pointers
- Configuration in YAML specifies return/context types per use-case

### 3. NULL Handling

**Issue**: Where to check for NULL pointers?

**Solution**:

- Generated dispatch includes NULL check
- Optional `visitNull` callback in visitor table
- Default NULL behavior if callback not provided

### 4. Recursive Cases

**Issue**: Some visitors need to recurse back to dispatcher

**Solution**:

- Generate `visitRecursive` helper function
- Visitor functions call helper to recurse
- Clearer than circular function pointers

### 5. Inline vs Helper Consistency

**Issue**: Some cases are trivial, others complex

**Solution**:

- Allow NULL entries in visitor table
- Generated dispatch handles NULL entries with inline code
- Configuration specifies inline handling per variant

### 6. Configuration Complexity

**Issue**: YAML config getting complex with inline code

**Solution**:

- Keep inline code simple (single eprintf statements)
- Complex cases must use helper functions
- Limit inline to true primitives and constants

## Recommendations for Full Implementation

### 1. Start Simple

**Phase 1**: Generate basic visitor infrastructure without inline config

- Every variant requires a visitor function
- No inline primitive handling
- Validates core visitor pattern

**Phase 2**: Add inline primitive support

- YAML config for inline cases
- Reduces visitor table size
- Handles trivial cases efficiently

**Phase 3**: Add use-case-specific typing

- Multiple visitor configs per union
- Type-safe return values and context
- Supports diverse use cases (pretty-print, normalize, compile, etc.)

### 2. Configuration Schema

```yaml
# lambda.yaml
config:
  name: lambda
  # ... existing
  visitors:
    # Pretty-printing visitor
    - name: PrettyPrinter
      union: LamExp
      return_type: void
      context_type: null
      inline_variants:
        stdint: 'eprintf("%d", val)'
        character: 'eprintf("\"%%c\"", val)'
        back: 'eprintf("(back)")'
        error: 'eprintf("(error)")'
        env: 'eprintf("env")'
    
    # ANF normalization visitor (future)
    - name: Normalizer
      union: LamExp
      return_type: AnfExp*
      context_type: AnfExp*
      null_handler: true
      # No inline variants - all need normalization
```

### 3. Generator Architecture

```python
# tools/generate/visitors.py

class VisitorGenerator:
    def __init__(self, catalog, visitor_config):
        self.catalog = catalog
        self.config = visitor_config
        self.union = catalog.get(visitor_config['union'])
        
    def generate_header(self):
        """Generate visitor table typedef and dispatch declaration"""
        return f"""
// Visitor table for {self.config['name']}
typedef struct {self.visitor_table_name()} {{
    {self.generate_null_handler()}
    {self.generate_visitor_fields()}
}} {self.visitor_table_name()};

// Main dispatch function
{self.return_type()} {self.dispatch_function_name()}(
    {self.union.cname()} *obj,
    {self.visitor_table_name()} *vtable
    {self.context_parameter()}
);
"""
    
    def generate_implementation(self):
        """Generate switch-based dispatch with inline handling"""
        return f"""
{self.return_type()} {self.dispatch_function_name()}(
    {self.union.cname()} *obj,
    {self.visitor_table_name()} *vtable
    {self.context_parameter()}
) {{
    {self.generate_null_check()}
    
    switch (obj->type) {{
        {self.generate_inline_cases()}
        {self.generate_visitor_cases()}
        default:
            cant_happen("unrecognized type");
    }}
}}
"""
```

### 4. Incremental Migration Strategy

1. **Generate visitor for LamExp pretty-printing**
2. **Keep existing `lambda_pp.c` as-is**
3. **Create `lambda_pp_visitor_test.c`** - parallel implementation using visitor
4. **Compare outputs** on test suite
5. **Once validated**, replace original with visitor version
6. **Repeat** for other use cases (normalize, compile, etc.)

### 5. Testing Strategy

**Unit tests** for generated code:

```c
void test_visitor_dispatch_null() {
    visitLamExpPrettyPrint(NULL, &ppVisitorTable);
    // Verify "<NULL exp>" printed
}

void test_visitor_dispatch_stdint() {
    LamExp *exp = makeLamExp_Stdint(42);
    visitLamExpPrettyPrint(exp, &ppVisitorTable);
    // Verify "42" printed
}

void test_visitor_dispatch_complex() {
    LamExp *exp = /* construct apply node */;
    visitLamExpPrettyPrint(exp, &ppVisitorTable);
    // Verify complex output
}
```

**Integration tests**: Run full test suite with visitor-based pretty-printer

**Performance tests**: Benchmark visitor dispatch vs manual switch

## Conclusion (Revised After Challenge 0)

The visitor pattern **requires significant revision** based on the traversal control issue:

### Critical Realization

**Classic visitor pattern doesn't fit CEKF's needs.** The fundamental assumption of visitor pattern is:

- Framework controls traversal (automatic, generated)
- User code operates on nodes (before or after traversal)

**CEKF transformations need interleaved control**:

- Print delimiters before/between/after children
- Build continuations between child normalizations
- Thread type constraints through inference

This isn't a visitor pattern problem - it's a **visitor pattern limitation**.

### What We're Actually Building

Not a visitor pattern, but a **typed dispatch table generator**:

```c
// Generated: eliminates switch, provides type-safe dispatch
void visitLamExp(LamExp *exp, LamExpPrettyPrinterTable *vtable);

// User-written: full control over traversal
void visitLam_pp(LamLam *lam) {
    eprintf("(lambda ");
    visitLamVarList(lam->args, &ppVisitorTable);  // Manual recursion
    eprintf(" ");
    visitLamExp(lam->exp, &ppVisitorTable);       // Manual recursion  
    eprintf(")");
}
```

This is **switch elimination**, not visitor pattern. But that's still valuable!

### Revised Benefits Assessment

#### For Pretty-Printing

**Before (manual switch)**:

```c
void ppLamExp(LamExp *exp) {
    switch (exp->type) {
        case LAMEXP_TYPE_LAM: ppLamLam(exp->val.lam); break;
        case LAMEXP_TYPE_VAR: ppHashSymbol(exp->val.var); break;
        // ... 29 more cases
    }
}
```

**After (generated dispatch)**:

```c
void ppLamExp(LamExp *exp) {
    visitLamExp(exp, &ppVisitorTable);
}

static LamExpPrettyPrinterTable ppVisitorTable = {
    .visitLam = ppLamLam,
    .visitVar = ppHashSymbol,
    // ... 29 more entries
};
```

**Net benefit**:

- Switch eliminated: ~60 lines → ~30 lines
- Type safety: Compiler catches wrong function signatures
- Clarity: Table shows all variants at-a-glance
- **But visitor functions unchanged** - still manually control traversal

**Value**: Modest. Mainly organizational/clarity benefit.

#### For Transformations (ANF, Bytecode)

**Switch elimination**: Same benefit as pretty-printing (~60 lines → ~30)

**Continuation scaffolding**: THIS is the real win!

```yaml
# anf_continuations.yaml
continuations:
  normalizeLetKont:
    free_vars:
      x: HashSymbol*
      body: LamData*
      k: LamKont*
    param:
      name: anfval
      type: LamData*
```

Generates env struct, wrapper functions, PROTECT boilerplate, etc.
**Saves ~10 lines per continuation × 70 continuations = ~700 lines**

**Value**: High. Eliminates error-prone boilerplate.

### Honest Assessment: Is This Worth It?

**For switch elimination alone**: Marginal value

- ~200 lines saved across all pretty-printers
- Modest clarity improvement
- Still need to write all the visitor functions (manually controlled traversal)

**For switch + continuation scaffolding**: Worthwhile

- ~200 lines (switch) + ~700 lines (continuations) = ~900 lines saved
- Reduces error-prone continuation boilerplate
- Type-safe env structs instead of hash tables
- Foundation for future transformations

### Revised Challenges

✓ Primitive types - inline in generated dispatch
✓ Type safety - use-case-specific visitor tables with typed function pointers
✓ NULL handling - generated NULL check with optional callback
✗ ~~Recursion - generated recursive helper function~~ - **Manual recursion in visitor**
✓ Trivial cases - allow NULL table entries, inline in dispatch
✓ Configuration - YAML schema extension for visitor specs
✓ **Traversal control** - visitor controls when children are visited (NOT automatic)

### Revised Recommendation

**Proceed with implementation, but adjust expectations**:

1. **This is dispatch table generation, not classic visitor pattern**
   - No automatic traversal
   - Visitor functions manually recurse
   - Generated code just eliminates switch statements

2. **Primary value is continuation scaffolding, not switch elimination**
   - Pretty-printing gains are modest
   - ANF/bytecode gains are substantial (continuation boilerplate)
   - Worth doing for Phase 2, marginal for Phase 1 alone

3. **Phased approach still makes sense**:
   - **Phase 1**: Generate dispatch tables, validate with pretty-printing
   - **Phase 2**: Generate continuation scaffolding (THE REAL WIN)
   - **Phase 3**: (Optional) Generate traversal helpers for simple cases

4. **Alternative to consider**:
   - Skip Phase 1 (dispatch tables for pretty-printing)
   - Jump straight to Phase 2 (continuation scaffolding for ANF)
   - That's where the real value is

### Final Verdict

**The visitor pattern walkthrough revealed**:

- Classic visitor pattern doesn't fit (traversal control issue)
- Generated dispatch tables are still useful (switch elimination)
- Continuation scaffolding is the real prize (~700 lines saved)
- Pretty-printing was the wrong test case (minimal benefit)
- **Should have started with ANF normalization** (where benefits are clear)

**Updated recommendation**:

- Focus on **continuation scaffolding** (Phase 2 from VISITOR.md)
- Treat dispatch table generation as a side benefit, not primary goal
- Pretty-printing gains are too modest to justify Phase 1 alone
- Build continuation generator first, add dispatch tables if time permits
