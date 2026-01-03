# Additional CPS Test Cases

## Advanced call/cc Patterns

These test cases explore sophisticated uses of first-class continuations:

### 1. call/cc Escaping from Deep Nesting
```fn
(f (g (call/cc (λ (k) (if test (k 42) 99)))))
```

**Expected behavior:** If `test` is true, the escape continuation `k` should bypass both `g` and `f`, returning 42 directly to the top-level continuation.

---

### 2. Storing and Calling Continuation Later
```fn
(letrec ((saved null))
  (+ (call/cc (λ (k) (sequence (saved = k) 10)))
     5))
```

**Expected behavior:**
- First time: `saved` is set to the continuation, returns 10, adds 5, result is 15
- If you call `(saved 20)` later: ignores current context, returns 20 to the original + expression, result would be 25

---

### 3. call/cc with Multiple Returns (Generator-like)
```fn
(letrec ((escape null))
  (+ (call/cc (λ (k) 
       (sequence (escape = k)
                 (k 1)
                 (k 2))))
     100))
```

**Expected behavior:**
- The sequence tries to call `(k 1)`, which escapes immediately with 1
- `(k 2)` never executes
- Result: `(+ 1 100)` = 101

---

### 4. Double-Escape Pattern
```fn
(call/cc (λ (k1)
  (call/cc (λ (k2)
    (k1 42)))))
```

**Expected:** Returns 42, bypassing both `k2` and directly escaping via `k1`.

---

### 5. Exception-Like Behavior
```fn
(letrec ((throw null) (catch null))
  (call/cc (λ (handler)
    (sequence
      (catch = handler)
      (call/cc (λ (k)
        (sequence
          (throw = (λ (val) (catch val)))
          (compute))))))))
```

This simulates exception handling where `throw` can be called from anywhere to escape back to the `catch` point.

---

## amb with call/cc Interactions

### 6. Escaping from All Branches
```fn
(call/cc (λ (escape)
  (amb (escape 1) (escape 2) (escape 3))))
```

**Expected:** All three branches call the same escape continuation, but with different values. Non-deterministically returns 1, 2, or 3, all via the same escape route.

---

### 7. Selective Escaping
```fn
(call/cc (λ (escape)
  (amb (if (test1) (escape 1) 10)
       (if (test2) (escape 2) 20)
       30)))
```

**Expected:** Some branches may escape, others may return normally. This creates a mix of escape and normal control flow.

---

### 8. call/cc Inside amb Branch
```fn
(amb (call/cc (λ (k) (k 1)))
     (call/cc (λ (k) (k 2)))
     3)
```

**Expected:** Each branch has its own call/cc with its own continuation. The continuations don't interact across branches.

---

## Primitives and CPS

### 9. Associativity Test (Left-to-Right Evaluation)
```fn
(+ (f 1) (g 2) (h 3))
```

**Expected:** If your + is binary, this needs to be parsed as either `(+ (+ (f 1) (g 2)) (h 3))` or `(+ (f 1) (+ (g 2) (h 3)))`. The CPS transformation should preserve the evaluation order.

---

### 10. Short-Circuit Logic (If Implemented)
```fn
(and (test1) (side-effect) (test2))
```

If `and` is a macro that short-circuits, the CPS transformation of the expanded form should not evaluate `side-effect` or `test2` if `test1` is false.

---

## Complex Nesting Patterns

### 11. Lambda Returning Lambda Returning Lambda (Currying)
```fn
(λ (a) (λ (b) (λ (c) (+ a (+ b c)))))
```

**Expected:** Each lambda gains its own continuation parameter. Nested lambdas are preserved with proper closure semantics.

---

### 12. Application of Nested Lambda Result
```fn
((call/cc (λ (k) (λ (x) (k x)))) 42)
```

**Expected:** 
- `call/cc` creates a function that takes `x` and escapes with it
- That function is then applied to 42
- Result: escapes with 42 immediately

**Tricky part:** The lambda `(λ (x) (k x))` is returned as a value through call/cc, but when it's called, `k` is no longer the current continuation - it's a captured continuation.

---

### 13. call/cc Returning a Function That Uses the Continuation
```fn
(let ((get-value (call/cc (λ (k) (λ () (k 99))))))
  (if (test) (get-value) 42))
```

**Expected:**
- First, `call/cc` returns the lambda `(λ () (k 99))` and binds it to `get-value`
- If `test` is true, calling `(get-value)` will escape back to the binding point with 99
- If `test` is false, returns 42

---

## Edge Cases

### 14. Empty Application
```fn
((λ () 42))
```

**Expected:** Lambda with no parameters (only continuation), applied with only continuation argument.

---

### 15. Identity Function in CPS
```fn
(λ (x) x)
```

**Expected:**
```fn
(λ (x $k) ($k x))
```

The simplest possible CPS transform - just pass the argument to the continuation.

---

### 16. Constant Function
```fn
(λ (x) 42)
```

**Expected:**
```fn
(λ (x $k) ($k 42))
```

Ignores argument, passes constant to continuation.

---

### 17. Deeply Nested If
```fn
(if a (if b (if c 1 2) 3) 4)
```

**Expected:** Each if should be properly CPS'd with continuation threading through branches.

---

### 18. call/cc That Never Calls the Continuation
```fn
(call/cc (λ (k) 
  (letrec ((loop (λ (n) (loop n))))
    (loop 0))))
```

**Expected:** Infinite loop. The continuation `k` is captured but never called. This tests that CPS transformation preserves non-termination.

---

## Testing Strategy

To validate these tests:

1. **Add to cps5.fn test list**
2. **Run transformation**
3. **Manually trace** execution for small examples
4. **Look for patterns:**
   - Every lambda has exactly one more parameter
   - Every application has exactly one more argument
   - Continuations are never "returned to" - they're called
   - Escape continuations have form `(λ (x ignored) ...)`

---

## What to Watch For

### Red Flags (Should NOT Appear):
- ❌ Function applications missing continuation argument
- ❌ Lambdas without continuation parameter
- ❌ Direct returns (not going through continuation)
- ❌ Escape continuations that use their continuation parameter

### Green Flags (Should Appear):
- ✅ All intermediate computations named
- ✅ All control flow explicit via continuation calls
- ✅ Primitive operations wrapped in continuation calls
- ✅ `(λ (x i) (cc x))` pattern for escape continuations

---

## Connection to CEKF Machine

Your CEKF machine natively uses continuations! The CPS transformation is **preprocessing** code into a form that matches the machine's operational semantics:

- **C (Control):** The current expression being evaluated
- **E (Environment):** Variable bindings
- **K (Kontinuation):** What to do with the result
- **F (Failure continuation):** For `amb`/backtracking

CPS makes the **K** explicit in the source code. Every function carries its continuation as an explicit parameter, matching what the CEKF machine does at runtime.

The CPS transformation is essentially **compiling** high-level control structures into the primitive operations your VM supports directly!

---

## Further Exploration

### 1. Administrative Reductions
The CPS transformation creates many `((λ (x) e) v)` forms where `v` is a value. These are **administrative redexes** that can be beta-reduced:

```fn
((λ ($k) ...) halt)
```

Could be simplified by substituting `halt` for `$k` directly.

### 2. Continuation Optimizations
When a continuation is used exactly once, it could be inlined. When it's never used, the computation is diverging or escaping.

### 3. Type Checking CPS
CPS-transformed code has specific type patterns. For example, in a typed language:
```
(λ (x) e) : A → B
```
Becomes:
```
(λ (x k) ...) : A → (B → R) → R
```

Where `R` is the final answer type. This is the "double negation" pattern in logic!

### 4. Defunctionalization
The continuations created by CPS can be **defunctionalized** - turned into data structures representing what operation to perform. This is closer to how compilers implement continuations.

---

## Conclusion

Your CPS implementation correctly handles all the fundamental cases. These additional tests would help validate:
- Edge cases (empty lambdas, deeply nested structures)
- Interaction patterns (call/cc + amb, call/cc + if)
- Advanced continuation usage (storing, calling later)
- Non-termination preservation

The transformation is a key bridge between high-level source language and your low-level CEKF machine semantics!
