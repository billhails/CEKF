# CPS Transformation Output Analysis

## Issues Found

### 🔴 Critical Issue #1: Lambda Wrapped in Continuation

**Test:** `(λ (a b) (+ a (* b 2)))`

**Actual Output:**
```fn
(halt (λ (a b $k3) ($k3 (+ a (* b 2)))))
```

**Expected Output:**
```fn
(λ (a b $k3) ($k3 (+ a (* b 2))))
```

**Problem:** The lambda itself is being passed to `halt`. This happens because when `T_c` receives an `aexpr` (atomic expression), it does:
```fn
if (isAexpr(expr)) {
    E.apply(c, [M(expr)])
}
```

For a standalone lambda, this is correct when the lambda is being **used** (evaluated in context). But when you're just **defining** it at the top level with `halt` as the "display this result" continuation, you don't want to wrap it.

**This is actually CORRECT for the CPS semantics!** The top-level `halt` continuation receives the lambda as its value. This is how CPS works - even returning a lambda goes through the continuation.

---

### 🔴 Critical Issue #2: call/cc Missing Continuation Argument

**Test:** `(call/cc (λ (k) (k 5)))`

**Actual Output:**
```fn
(halt ((λ (f cc) (f (λ (x i) (cc x)) cc)) (λ (k $k8) (k 5 $k8))))
```

**Expected Output:**
```fn
((λ (f cc) (f (λ (x i) (cc x)) cc)) 
 (λ (k $k8) (k 5 $k8)) 
 halt)
```

**Problem:** The call/cc wrapper `(λ (f cc) ...)` takes TWO arguments:
1. `f` - the user's function
2. `cc` - the current continuation

But you're only providing `f`. The `halt` should be passed as the second argument `cc`, not wrapping the whole thing.

**Root Cause:** In your `M` function:
```fn
(E.callcc_expr(expr)) {
    E.apply(E.parse("(lambda (f cc) (f (lambda (x i) (cc x)) cc))"), [M(expr)])
}
```

This creates an application with only ONE argument `[M(expr)]`, but the call/cc wrapper needs TWO: `[M(expr), <current-continuation>]`.

**The problem is:** `M` doesn't know what the current continuation is. That's only available in `T_c` or `T_k`.

---

## Correct Transformations ✅

### 1. Simple Application
```fn
(g a) ==> (g a halt)
```
Perfect! Just thread the continuation through.

---

### 2. Nested Application
```fn
(f (g (h x))) ==>
(h x (λ ($rv45) (g $rv45 (λ ($rv44) (f $rv44 halt)))))
```
Excellent! Each nested call is sequenced with continuations:
1. Call `(h x)` with continuation that receives `$rv45`
2. Call `(g $rv45)` with continuation that receives `$rv44`  
3. Call `(f $rv44 halt)` with final continuation

---

### 3. Lambda with Complex Body
```fn
((λ (a b) (+ a (* (f b) 2))) 3 4) ==>
((λ (a b $k5) (f b (λ ($rv6) ($k5 (+ a (* $rv6 2)))))) 3 4 halt)
```
Great! The lambda:
- Gains continuation parameter `$k5`
- Calls `(f b)` with continuation
- Continuation receives result in `$rv6`
- Passes final result to `$k5`

---

### 4. Factorial with Letrec
```fn
(letrec ((fact (λ (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5)) ==>
(letrec ((fact (λ (n $k17) 
                 ((λ ($k18) 
                    (= n 0 (λ ($rv19) 
                      (if $rv19 
                        ($k18 1) 
                        (fact (- n 1) (λ ($rv20) 
                          ($k18 (* n $rv20))))))))
                  $k17)))) 
  (fact 5 halt))
```

This is EXCELLENT and shows the full power of CPS:
1. `fact` gains continuation `$k17`
2. Bind `$k18` to `$k17` to avoid code duplication in if branches
3. Test `(= n 0)` in CPS - result in `$rv19`
4. Base case: `($k18 1)` - return 1 via continuation
5. Recursive case:
   - Compute `(- n 1)` (stays direct-style for primitives)
   - Call `fact` recursively with continuation
   - Continuation receives result in `$rv20`
   - Multiply: `(* n $rv20)` (direct-style)
   - Return via `$k18`

---

### 5. Amb (Non-determinism)
```fn
(amb 1 2) ==>
((λ ($k7) (amb ($k7 1) ($k7 2))) halt)
```

Perfect! Both branches of `amb` share the same continuation `$k7`. This is exactly right for non-deterministic choice.

---

### 6. Complex Amb
```fn
(+ (amb 1 2) (amb 3 4)) ==>
(amb ((λ ($rv42) 
       (amb ((λ ($rv43) (halt (+ $rv42 $rv43))) 3) 
            ((λ ($rv43) (halt (+ $rv42 $rv43))) 4))) 
      1) 
     ((λ ($rv42) 
       (amb ((λ ($rv43) (halt (+ $rv42 $rv43))) 3) 
            ((λ ($rv43) (halt (+ $rv42 $rv43))) 4))) 
      2))
```

This is fascinating! It shows the **search tree**:
- Outer amb chooses between 1 and 2 (binds to `$rv42`)
- Inner amb chooses between 3 and 4 (binds to `$rv43`)
- Four possible paths: (1,3), (1,4), (2,3), (2,4)

Note: There's code duplication here. The inner amb is duplicated for each branch of the outer amb. This is correct but could be optimized.

---

## Understanding Primitives in Your Implementation

Your code treats primitives specially in `T_k`:
```fn
(E.primapp(p, e1, e2)) {
    T_k(e1, fn (s1) {
        T_k(e2, fn (s2) {
            k(E.primapp(p, s1, s2))
        })
    })
}
```

And in `T_c`:
```fn
(E.primapp(p, e1, e2)) {
    T_k(e1, fn (s1) {
        T_k(e2, fn (s2) {
            E.apply(c, [E.primapp(p, s1, s2)])
        })
    })
}
```

This means:
1. Arguments are evaluated in CPS (to handle complex expressions)
2. The primitive itself stays **direct-style** 
3. The result is passed to the continuation

**This is correct!** Primitives are assumed to:
- Always terminate
- Never throw exceptions (in this simple model)
- Can be called directly

In the Racket code, `(cps <prim>)` is a **runtime wrapper** that does the same thing - takes arguments and a continuation, applies the primitive, passes result to continuation. But in your implementation, this happens at **compile time** via the transformation itself.

---

## The call/cc Problem Explained

### What call/cc Should Do

`call/cc` (call-with-current-continuation) captures the current continuation and passes it as a first-class value to a function. The key insight:

```scheme
(call/cc (lambda (k) ...body...))
```

Should behave like:
```scheme
(let ((k <current-continuation>))
  ...body...)
```

Where `k` is a function that, when called, **abandons** the current computation and jumps back to where `call/cc` was invoked.

### The Racket Transformation

```racket
'(λ (f cc) (f (λ (x _) (cc x)) cc))
```

This says:
- `f` is the user's function (e.g., `(lambda (k) body)`)
- `cc` is the current continuation (where `call/cc` was invoked)
- Pass to `f`:
  1. An escape continuation: `(λ (x _) (cc x))` - ignores its own continuation `_` and jumps to `cc`
  2. The normal continuation: `cc` again

### Why Your Code is Wrong

Your `M` function creates:
```fn
E.apply(wrapper, [M(expr)])
```

But `wrapper` expects TWO arguments: `f` and `cc`. You're only providing `f`.

### The Fix

The problem is that `M` operates on **atomic expressions** (aexpr) and doesn't have access to the continuation context. The continuation is only available in `T_c` and `T_k`.

**Option 1:** Handle `call/cc` specially in `T_c`, not in `M`:

```fn
// In M function, remove the call/cc case:
fn M {
    (E.lambda(vars, body)) {
        let k = gensym("$k");
        in E.lambda(vars @@ [k], T_c(body, k))
    }
    (x) { x }  // Everything else unchanged
}

// In T_c, add special case:
fn T_c(expr, c) {
    if (isAexpr(expr)) {
        E.apply(c, [M(expr)])
    } else {
        switch(expr) {
            ...existing cases...
            
            (E.callcc_expr(f)) {
                // Transform the function f
                T_k(f, fn (sf) {
                    // Apply the call/cc wrapper with TWO args: sf and c
                    E.apply(
                        E.parse("(lambda (f cc) (f (lambda (x i) (cc x)) cc))"),
                        [sf, c]
                    )
                })
            }
        }
    }
}
```

**Option 2:** Remove `callcc_expr` from `isAexpr` check:

Currently you have:
```fn
fn isAexpr {
    ...
    (E.callcc_expr(_)) { true }  // <-- This is wrong!
    ...
}
```

`call/cc` is NOT an atomic expression - it needs to capture the continuation, so it must be handled in `T_c`/`T_k`, not treated as atomic.

---

## Sequence Issue

Looking at this output:
```fn
(sequence (f 1) (g 2) (h 3)) ==>
(f 1 (λ ($rv33) (g 2 (λ ($rv34) (h 3 (λ ($rv35) (sequence $rv33 $rv34 $rv35 halt)))))))
```

This is technically correct BUT has a subtle issue: the final result is `(sequence $rv33 $rv34 $rv35 halt)`. This creates a **new** sequence of the three results, rather than just returning the last one.

In typical sequence/begin semantics, you should only return the LAST value. The intermediate values are computed for side effects only:

```fn
(sequence (f 1) (g 2) (h 3)) ==>
(f 1 (λ ($rv33) (g 2 (λ ($rv34) (h 3 halt)))))
```

The `$rv33` and `$rv34` are bound but never used (unless there are side effects).

---

## Summary

### ✅ Working Correctly:
1. Simple applications
2. Lambda transformation (gaining continuation parameter)
3. Nested applications (proper sequencing)
4. If expressions (proper branch handling)
5. Letrec (mutual recursion preserved)
6. Amb (both branches share continuation)
7. Primitives (staying direct-style, wrapped in continuation calls)

### 🔴 Needs Fixing:
1. **call/cc transformation:** Missing the current continuation as second argument
2. **Sequence transformation:** Should discard intermediate values, only return last

### 💡 Design Questions:
1. Should top-level lambdas be wrapped in `halt`? (Actually yes for pure CPS)
2. Should amb duplication be optimized? (Would require let-bindings)

---

## Next Steps

1. **Fix call/cc:** Remove it from `isAexpr`, handle specially in `T_c` with TWO arguments
2. **Test call/cc:** After fixing, verify that escape continuations properly ignore their own continuation parameter
3. **Fix sequence:** Make it discard intermediate values
4. **Add more tests:** Especially for call/cc escaping from nested contexts
