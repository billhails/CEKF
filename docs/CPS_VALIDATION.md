# CPS Transformation Validation Guide

## Understanding CPS Transformation

CPS (Continuation-Passing Style) makes **control flow explicit** by:
1. Every function takes an extra parameter: the continuation (what to do next)
2. No function ever "returns" - instead it calls its continuation with the result
3. All intermediate computations are named

## Key Transformation Rules

### Rule 1: Simple Application `(f a)`
```
(f a) with continuation halt
=>
(f a halt)  ; Pass continuation as extra argument
```

### Rule 2: Lambda `(lambda (x) body)`
```
(lambda (x) body)
=>
(lambda (x $k) (T_c body $k))  ; Add continuation parameter
```

### Rule 3: Nested Application `(f (g x))`
```
(f (g x)) with continuation c
=>
(g x (lambda ($rv) (f $rv c)))  ; Name intermediate result
```

### Rule 4: Primitive Operations `(+ a b)`
Primitives are **direct style**, so wrap them:
```
(+ a b) with continuation c
=>
(c (+ a b))  ; Call continuation with primitive result
```

### Rule 5: If Expression
Branches must be wrapped to avoid code duplication:
```
(if test e1 e2) with continuation c
=>
((lambda ($k)
   (test_in_cps (lambda ($test)
     (if $test (T_c e1 $k) (T_c e2 $k)))))
 c)
```

### Rule 6: Letrec
Transform bound lambdas, thread continuation through body:
```
(letrec ((f (lambda (x) body))) expr)
=>
(letrec ((f (lambda (x $k) body_cps))) expr_cps)
```

### Rule 7: call/cc
The tricky one! `call/cc` receives the current continuation as a first-class value:
```
(call/cc (lambda (k) body))
=>
((lambda (f cc) 
   (f (lambda (x _) (cc x))  ; Escape continuation ignores its own k
      cc))                    ; Also pass cc as normal continuation
 (lambda (k $k1) body_cps)
 halt)
```

The key insight: when you call the escape continuation `k`, it should jump to where `call/cc` was invoked, ignoring any intermediate continuations.

## Expected Outputs for Test Cases

### Test 1: `(g a)`
**Expected:**
```
(g a halt)
```
Simple - just pass the continuation.

---

### Test 2: `((lambda (x) (h x)) (g 4))`
**Expected:**
```
(g 4 (lambda ($rv1) 
       ((lambda (x $k2) (h x $k2)) 
        $rv1 
        halt)))
```
**Explanation:**
1. Evaluate `(g 4)` with continuation that receives result in `$rv1`
2. Apply transformed lambda (which now takes `x` and `$k2`)
3. Pass `halt` as final continuation

---

### Test 3: `(lambda (a b) (+ a (* b 2)))`
**Expected:**
```
(lambda (a b $k) 
  (halt (+ a (* b 2))))  ; Primitives stay direct-style, wrapped in continuation call
```
Wait, this is wrong. Let me reconsider...

**Actually Expected:**
```
(lambda (a b $k) 
  ($k (+ a (* b 2))))
```
The lambda's continuation parameter `$k` is used, not `halt`.

---

### Test 4: `((lambda (a b) (+ a (* b 2))) 3 4)`
**Expected:**
```
((lambda (a b $k1) 
   ($k1 (+ a (* b 2)))) 
 3 
 4 
 halt)
```
Direct application with all arguments plus final continuation.

---

### Test 5: `(call/cc (lambda (k) (k 5)))`
**Expected:**
```
((lambda (f cc) 
   (f (lambda (x _) (cc x)) cc))
 (lambda (k $k1) 
   (k 5 $k1))
 halt)
```
**What happens at runtime:**
1. `f` is bound to `(lambda (k $k1) (k 5 $k1))`
2. `cc` is bound to `halt`
3. Call `f` with escape continuation `(lambda (x _) (cc x))` and normal continuation `cc`
4. Inside `f`: `k` is `(lambda (x _) (halt x))`
5. `(k 5 $k1)` becomes `((lambda (x _) (halt x)) 5 $k1)`
6. This ignores `$k1` and calls `(halt 5)` directly!

---

### Test 6: `(call/cc (lambda (k) 42))`
**Expected:**
```
((lambda (f cc) 
   (f (lambda (x _) (cc x)) cc))
 (lambda (k $k1) 
   ($k1 42))
 halt)
```
**What happens:**
- The escape continuation `k` is never called
- Just returns 42 via normal continuation `$k1`
- Eventually reaches `halt`

---

### Test 7: Factorial `(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))`
**Expected (simplified structure):**
```
(letrec ((fact (lambda (n $k) 
                 ((lambda ($k2)
                    ((= n 0) (lambda ($test)
                      (if $test 
                        ($k2 1)
                        ((- n 1) (lambda ($rv)
                          (fact $rv (lambda ($rv2)
                            ($k2 (* n $rv2))))))))))
                  $k))))
  (fact 5 halt))
```
This is complex because:
1. `fact` gets continuation parameter `$k`
2. `(= n 0)` becomes a CPS call
3. Each branch is wrapped in the same continuation `$k2`
4. Recursive call to `fact` must pass a continuation
5. That continuation multiplies `n` by the result

---

### Test 8: Nested Primitives `(+ 1 (+ 2 (+ 3 (+ 4 5))))`
**Expected:**
```
(halt (+ 1 (+ 2 (+ 3 (+ 4 5)))))
```
All primitives stay as-is, just wrapped in final continuation.

---

### Test 9: `(+ (amb 1 2) (amb 3 4))`
**Expected:**
```
((lambda ($k1)
   (amb ((lambda ($k2) 
           (amb ($k2 1) ($k2 2))) 
         (lambda ($rv1)
           ((lambda ($k3)
              (amb ($k3 3) ($k3 4)))
            (lambda ($rv2)
              ($k1 (+ $rv1 $rv2))))))
        ((lambda ($k2) 
           (amb ($k2 1) ($k2 2))) 
         (lambda ($rv1)
           ((lambda ($k3)
              (amb ($k3 3) ($k3 4)))
            (lambda ($rv2)
              ($k1 (+ $rv1 $rv2))))))))
 halt)
```
Wait, that's not right either. Let me reconsider amb...

Actually, looking at your T_c for amb:
```fn
(E.amb_expr(expr1, expr2)) {
    let sk = gensym("$k");
    in E.apply(E.lambda([sk], E.amb_expr(T_c(expr1, sk), T_c(expr2, sk))), [c])
}
```

So `(amb e1 e2)` becomes:
```
((lambda ($k) (amb (T_c e1 $k) (T_c e2 $k))) c)
```

This wraps both branches in the same continuation, which is correct for amb!

## Common Patterns to Validate

### Pattern 1: Administrative Redexes
CPS often creates `((lambda (x) e) v)` where `v` is a value. These are **administrative redexes** that can be optimized away via beta-reduction, but should be present in the basic transformation.

### Pattern 2: Continuation Shadowing
Watch for variables named `$k1`, `$k2`, etc. These should **not** shadow each other - each should be in a separate scope.

### Pattern 3: call/cc Escape Behavior
The escape continuation `(lambda (x _) (cc x))` must **ignore** its continuation parameter. This is how it "escapes" from nested contexts.

## Running the Tests

```bash
cd /home/bill/git/CEKF
./bin/fn fn/rewrite/cps5.fn > cps_test_output.txt
```

Then manually inspect the output to verify:
1. Every lambda has one more parameter than the source
2. Every application has one more argument (the continuation)
3. Primitives are wrapped in continuation calls
4. No continuation is ever "returned" - they're always called
5. call/cc properly captures and can ignore continuations

## Debugging Tips

1. **Track continuation flow:** Follow the `$k` variables through transformations
2. **Count parameters:** Transformed lambdas should have N+1 parameters (original N plus continuation)
3. **Find returns:** If you see a "return" happening implicitly, you've missed a CPS transform
4. **Check call/cc:** The escape continuation should have pattern `(lambda (x _) ...)`
