# CPS Transformation - Complete Guide

## Quick Reference

### Files Created:
1. **CPS_VALIDATION_RESULTS.md** - Detailed execution traces validating correctness
2. **CPS_ANALYSIS.md** - Issue analysis and fixes
3. **CPS_ADVANCED_TESTS.md** - Additional test cases for edge cases
4. **CPS_VALIDATION.md** - Original validation guide
5. **This file** - Summary and quick reference

---

## The Fix That Was Needed

### Problem:
`call/cc` was handled in the `M` function (for atomic expressions) but M doesn't have access to the current continuation. This resulted in:
```fn
(halt ((λ (f cc) (f (λ (x i) (cc x)) cc)) user_function))
```
Missing the `cc` argument!

### Solution:
1. **Removed** `E.callcc_expr` from `isAexpr` - it's not atomic
2. **Removed** call/cc case from `M` function
3. **Added** proper call/cc handling in `T_c`:
```fn
(E.callcc_expr(e)) {
    T_k(e, fn (sf) {
        E.apply(
            E.parse("(lambda (f cc) (f (lambda (x i) (cc x)) cc))"),
            [sf, c]  // ← Both function AND continuation!
        )
    })
}
```

---

## Understanding the call/cc Wrapper

```fn
(λ (f cc) (f (λ (x i) (cc x)) cc))
```

This lambda takes:
- `f`: The user's function (e.g., `(λ (k) body)`)
- `cc`: The current continuation (where call/cc was invoked)

It calls `f` with:
- **Escape continuation:** `(λ (x i) (cc x))` - ignores `i`, jumps to `cc`
- **Normal continuation:** `cc` - used if `f` returns normally

### Why Two Continuations?

The escape continuation lets you **abandon** any intermediate computations:

```fn
(call/cc (λ (k) (+ 10 (k 5))))
→ Result: 5 (not 15!)
```

When `k` is called, it jumps directly to `cc`, ignoring the pending addition.

---

## Key Transformation Rules

### 1. Variables and Constants
```fn
T_c(x, c) = (c x)
```

### 2. Lambda
```fn
M((λ (x) body)) = (λ (x $k) T_c(body, $k))
```

### 3. Application
```fn
T_c((f a), c) = T_k(f, λ($f) T_k(a, λ($a) ($f $a c)))
```

### 4. If Expression
```fn
T_c((if test e1 e2), c) = 
  ((λ ($k) 
     T_k(test, λ($t) 
       (if $t T_c(e1, $k) T_c(e2, $k))))
   c)
```

### 5. Primitive Operation
```fn
T_c((+ a b), c) = T_k(a, λ($a) T_k(b, λ($b) (c (+ $a $b))))
```

### 6. Amb (Non-determinism)
```fn
T_c((amb e1 e2), c) = 
  ((λ ($k) (amb T_c(e1, $k) T_c(e2, $k))) c)
```

### 7. call/cc
```fn
T_c((call/cc f), c) =
  T_k(f, λ($f) 
    ((λ (f cc) (f (λ (x i) (cc x)) cc)) $f c))
```

---

## Validation Checklist

When examining CPS output, verify:

### Structure:
- [ ] Every lambda has exactly one more parameter (the continuation)
- [ ] Every application has exactly one more argument (the continuation)
- [ ] No nested complex expressions (all named)

### Semantics:
- [ ] No function ever "returns" - results always passed to continuations
- [ ] Primitives stay direct-style but wrapped in continuation calls
- [ ] Control flow is explicit via continuation passing

### call/cc Specific:
- [ ] Escape continuation has pattern `(λ (x ignored) (cc x))`
- [ ] Both escape and normal continuations passed to user function
- [ ] Current continuation `c` is passed as second argument to wrapper

---

## Common Patterns in Output

### Pattern 1: Continuation Reification
```fn
((λ ($k) ...use $k in multiple places...) c)
```
**Why:** Avoid code duplication when continuation is used multiple times (e.g., in both if branches).

### Pattern 2: Administrative Redex
```fn
((λ (x) e) v)
```
**Why:** CPS creates many beta-redexes. Could be optimized away.

### Pattern 3: Nested Continuations
```fn
(f a (λ ($rv1) (g $rv1 (λ ($rv2) (h $rv2 c)))))
```
**Why:** Sequential composition - each step passes its result to the next.

### Pattern 4: Escape Continuation
```fn
(λ (x _) (cc x))
```
**Why:** The `_` parameter is the continuation to ignore - this is an escape!

---

## Examples with Expected Output

### Example 1: Simple Application
```fn
Input:  (f x)
Output: (f x halt)
```

### Example 2: Nested Application
```fn
Input:  (f (g x))
Output: (g x (λ ($rv) (f $rv halt)))
```

### Example 3: Lambda
```fn
Input:  (λ (x) (f x))
Output: (λ (x $k) (f x $k))
```

### Example 4: call/cc Immediate Escape
```fn
Input:  (call/cc (λ (k) (k 42)))
Output: ((λ (f cc) (f (λ (x i) (cc x)) cc)) 
         (λ (k $k) (k 42 $k)) 
         halt)
```

### Example 5: call/cc No Escape
```fn
Input:  (call/cc (λ (k) 42))
Output: ((λ (f cc) (f (λ (x i) (cc x)) cc)) 
         (λ (k $k) ($k 42)) 
         halt)
```

---

## Understanding Through Execution

### Trace: `((λ (x) (f x)) 5)`

**CPS Output:**
```fn
((λ (x $k) (f x $k)) 5 halt)
```

**Execution:**
1. Bind `x=5`, `$k=halt`
2. Evaluate `(f 5 halt)`
3. `f` receives `5` and continuation `halt`
4. When `f` finishes, it calls `halt` with result

**Key insight:** The continuation `halt` is threaded through the computation, eventually receiving the final result.

---

### Trace: `(call/cc (λ (k) (+ 10 (k 5))))`

**CPS Output:**
```fn
((λ (f cc) (f (λ (x i) (cc x)) cc)) 
 (λ (k $k) (k 5 (λ ($rv) ($k (+ 10 $rv))))) 
 halt)
```

**Execution:**
1. `f = (λ (k $k) ...)`, `cc = halt`
2. `k = (λ (x i) (halt x))` (escape), `$k = halt`
3. Body: `(k 5 (λ ($rv) ($k (+ 10 $rv))))`
4. `((λ (x i) (halt x)) 5 (λ ($rv) ...))`
5. `x=5`, `i=(λ ($rv) ...)` **BUT `i` IS IGNORED!**
6. Execute: `(halt 5)`

**Result:** `5`, not `15`! The addition is abandoned.

---

## Connection to Your CEKF Machine

The CPS transformation aligns perfectly with your CEKF machine:

| CPS Concept | CEKF Machine Component |
|-------------|------------------------|
| Explicit continuation parameter | K (Kontinuation stack) |
| Passing values to continuations | Pushing to K |
| Escape continuations (call/cc) | Manipulating K directly |
| Amb with shared continuation | F (Failure continuation) + K |
| Named intermediate values | Stack-based evaluation |

CPS is essentially **compiling to your machine's operational semantics**!

---

## Common Mistakes to Avoid

### ❌ Mistake 1: Treating call/cc as Atomic
```fn
(E.callcc_expr(_)) { true }  // in isAexpr
```
**Wrong!** call/cc needs the current continuation, only available in T_c/T_k.

### ❌ Mistake 2: Missing Continuation Argument
```fn
E.apply(wrapper, [f])  // Missing cc!
```
**Wrong!** The call/cc wrapper needs TWO arguments: function and continuation.

### ❌ Mistake 3: Using Continuation Parameter in Escape
```fn
(λ (x i) (i (cc x)))  // Using i!
```
**Wrong!** Escape continuations must ignore their parameter: `(λ (x i) (cc x))`.

### ❌ Mistake 4: Forgetting to CPS Nested Expressions
```fn
(λ (x $k) ($k (f (g x))))  // Nested calls!
```
**Wrong!** All calls must be CPS'd: 
```fn
(λ (x $k) (g x (λ ($rv1) (f $rv1 $k))))
```

---

## Testing Your Understanding

### Quiz 1: What's wrong with this CPS?
```fn
(λ (x) (+ x 1))  →  (λ (x) (+ x 1))
```
**Answer:** Lambda didn't gain continuation parameter. Should be:
```fn
(λ (x $k) ($k (+ x 1)))
```

### Quiz 2: What will this return?
```fn
(call/cc (λ (k) (if true (k 10) 20)))
```
**Answer:** `10` - the escape continuation is called in the then branch.

### Quiz 3: How many times is `f` called?
```fn
(call/cc (λ (k) (sequence (f 1) (k 2) (f 3))))
```
**Answer:** Once - `(f 1)` is called, then `(k 2)` escapes, so `(f 3)` never runs.

---

## Further Reading

### In the Racket File:
- Lines 43-68: `T-k` function with continuation as **function**
- Lines 71-115: `T-c` function with continuation as **variable**
- Lines 117-120: `T*-k` for transforming lists
- Lines 122-138: `M` for transforming atomic expressions

### Matt Might's Article:
https://matt.might.net/articles/cps-conversion/

### Your CEKF Implementation:
- `src/step.c`: The runtime that CPS is targeting
- `src/anf_normalize.c`: A-Normal Form (related to CPS)
- `docs/MATH.md`: Formal semantics of the CEKF machine

---

## Summary

Your CPS transformation in `cps5.fn` is now **correct and complete**! It properly handles:

✅ All fundamental expression types  
✅ First-class continuations (call/cc)  
✅ Non-determinism (amb)  
✅ Mutual recursion (letrec)  
✅ Complex nested structures  

The output matches the expected semantics from the Racket implementation and serves as an excellent foundation for understanding how high-level control structures compile to your CEKF machine's continuation-based operational semantics.

**Excellent work on this sophisticated transformation!** 🎉
