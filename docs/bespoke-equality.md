# Bespoke Equality Functions for User Types

Example type that prompted the idea:

```fn
typedef term {
    num(number) |
    var(string) |
    add(term, term) |
    sub(term, term) |
    mul(term, term) |
    div(term, term) |
    pow(term, term)
}
```

The `add` and `mul` operations are commutative, so as far as comparing them for semantic equality, `add(a, b)` is equal to `add(b, a)` etc. We may want this behavior when for example doing constant folding:

```fn
    ...
    sub(a, a) { num(0) }
    ...
```

Where the two arguments to `sub` should match if they are semantically equal, without needing to create cases for every possible arrangement of their components.

It's trivial to write a function to express this for a top-level comparison:

```fn
fn eq_term {
    (num(a), num(a)) |
    (var(a), var(a)) |
    (add(a, b), add(a, b)) |
    (add(b, a), add(a, b)) |
    (sub(a, b), sub(a, b)) |
    (mul(a, b), mul(a, b)) |
    (mul(b, a), mul(a, b)) |
    (div(a, b), div(a, b)) |
    (pow(a, b), pow(a, b)) { true }
    (_, _) { false }
}
```

but to be correct the function must also use its own definition: the unifying `a` and `b` variables need to be compared in the same way.

What makes this useful is that since the function has to override the built-in comparison operations to work in the first place, it should naturally do so for all comparisons of that type.

## How Recursion Works

The bespoke comparator must be invoked automatically during pattern matching. To achieve this, when TPMC compiles a pattern like `(add(a, b), add(a, b))`, it generates normal `eq` operations for the pattern variables, but the type checker later recognizes that `a` and `b` have type `term` and substitutes those `eq` calls with calls to `eq$term`.

Similarly, in the constant folding example:

```fn
    sub(a, a) { num(0) }
```

The pattern `sub(a, a)` requires the two arguments to match. TPMC generates an `eq` comparison; since both arguments are type `term`, the type checker substitutes `eq$term`, enabling semantic equality (so `sub(x+1, 1+x)` matches, as does `sub(y*2, 2*y)`).

Assuming a similar definition strategy to `print`, like: `eq type { ...body... }` the type of the function would be constrained to `type -> type -> bool`. When an `eq` comparison is reached in the type checker while the type of the operands is known, the compile-time environment is checked for a bespoke comparator and a call to that is substituted in place of the `eq`.

It would be simple enough to do a to-string and compare strings for structural equality if we needed it occasionally.

## Implementation

Looking at how the parser currently handles analogous `print` tokens in a `let`, it just calls the `defun` parser, passing it a flag `isPrinter = true` which causes the parser to generate a `print$type` name for the function being defined.

We could extend this by replacing the boolean with an enum, values `FUNCTION`, `PRINTER`, `EQUALITY` for now. Then the `defun` would generate an `eq$type` name if given an `EQUALITY` type. We just need to decide what the initiating token is.

On encountering an `eq$type` function, the typechecker would immediately create the type signature `type -> type -> bool` and unify the function against it before type checking the function body. Any type mismatches will produce normal type checking errors. On encountering an `eq` primitive operation, the type checker would inspect the current `TcEnv` for an `eq$type` function and if found would replace the `eq` with a call to that function. If the type encountered is namespace qualified then the type checker must inspect that namespace for the equivalent `eq$type` function and namespace-qualify the invocation.

TPMC requires no changes—it continues to generate `eq` operations for pattern variable comparisons as it always has. The type checker then substitutes these with bespoke comparator calls when appropriate.

### Limitation

The surface level `==` is a user defined operator that expands to a call to a macro: `x == y` becomes `(op$macro$1 (λ () x) (λ () y))` where `opMacro$1` is bound to `(λ (x1 x2) (eq (x1) (x2)))`. There is no way currently to get from `x == y` to `(eq x y)` - though it would be great if we could, likewise for the other primitives. Also if we plan to do constant folding in a later compiler pass we'll need to fix this. That's for another day.

```mermaid
---
title: eq$term
---
flowchart LR
T208("p$107<br/>[]<br/>(arcs 7)")
T209("p$108<br/>[p$108 p$107$0]<br/>(arcs 2)")
T210("p$108$0<br/>[p$107$0 p$108$0]<br/>(arcs 2)")
F197("(begin constructor:true)<br/>[p$107$0]")
T210 --"p$108$0:p$107$0:_==p$108$0:var a<br/>[p$107$0]"--> F197
F206("(begin constructor:false)<br/>[]")
T210 --"p$108$0:_<br/>[]"--> F206
T209 --"p$108:var(p$108$0:_)<br/>[p$107$0]"--> T210
T209 --"p$108:_<br/>[]"--> F206
T208 --"p$107:var(p$107$0:_)<br/>[p$108]"--> T209
T211("p$108<br/>[p$108 p$107$0]<br/>(arcs 2)")
T212("p$108$0<br/>[p$107$0 p$108$0]<br/>(arcs 2)")
F198("(begin constructor:true)<br/>[p$107$0]")
T212 --"p$108$0:p$107$0:_==p$108$0:var a<br/>[p$107$0]"--> F198
T212 --"p$108$0:_<br/>[]"--> F206
T211 --"p$108:num(p$108$0:_)<br/>[p$107$0]"--> T212
T211 --"p$108:_<br/>[]"--> F206
T208 --"p$107:num(p$107$0:_)<br/>[p$108]"--> T211
T213("p$108<br/>[p$108 p$107$0 p$107$1]<br/>(arcs 2)")
T214("p$108$0<br/>[p$107$0 p$108$0 p$107$1 p$108$1]<br/>(arcs 2)")
T215("p$108$1<br/>[p$107$0 p$107$1 p$108$1]<br/>(arcs 2)")
F199("(begin constructor:true)<br/>[p$107$0 p$107$1]")
T215 --"p$108$1:p$107$1:_==p$108$1:var b<br/>[p$107$0 p$107$1]"--> F199
T215 --"p$108$1:_<br/>[]"--> F206
T214 --"p$108$0:p$107$0:_==p$108$0:var a<br/>[p$107$0 p$107$1 p$108$1]"--> T215
T214 --"p$108$0:_<br/>[]"--> F206
T213 --"p$108:pow(p$108$0:_, p$108$1:_)<br/>[p$107$0 p$107$1]"--> T214
T213 --"p$108:_<br/>[]"--> F206
T208 --"p$107:pow(p$107$0:_, p$107$1:_)<br/>[p$108]"--> T213
T216("p$108<br/>[p$108 p$107$0 p$107$1]<br/>(arcs 2)")
T217("p$108$0<br/>[p$107$0 p$108$0 p$107$1 p$108$1]<br/>(arcs 2)")
T218("p$108$1<br/>[p$107$0 p$107$1 p$108$1]<br/>(arcs 2)")
F200("(begin constructor:true)<br/>[p$107$0 p$107$1]")
T218 --"p$108$1:p$107$1:_==p$108$1:var b<br/>[p$107$0 p$107$1]"--> F200
T218 --"p$108$1:_<br/>[]"--> F206
T217 --"p$108$0:p$107$0:_==p$108$0:var a<br/>[p$107$0 p$107$1 p$108$1]"--> T218
T217 --"p$108$0:_<br/>[]"--> F206
T216 --"p$108:div(p$108$0:_, p$108$1:_)<br/>[p$107$0 p$107$1]"--> T217
T216 --"p$108:_<br/>[]"--> F206
T208 --"p$107:div(p$107$0:_, p$107$1:_)<br/>[p$108]"--> T216
T219("p$108<br/>[p$108 p$107$0 p$107$1]<br/>(arcs 2)")
T220("p$108$0<br/>[p$107$0 p$108$0 p$107$1 p$108$1]<br/>(arcs 3)")
T221("p$108$1<br/>[p$107$0 p$107$1 p$108$1]<br/>(arcs 2)")
F201("(begin constructor:true)<br/>[p$107$0 p$107$1]")
T221 --"p$108$1:p$107$0:_==p$108$1:var b<br/>[p$107$0 p$107$1]"--> F201
T221 --"p$108$1:_<br/>[]"--> F206
T220 --"p$108$0:p$107$1:_==p$108$0:var a<br/>[p$107$0 p$107$1 p$108$1]"--> T221
T222("p$108$1<br/>[p$107$0 p$107$1 p$108$1]<br/>(arcs 2)")
F202("(begin constructor:true)<br/>[p$107$0 p$107$1]")
T222 --"p$108$1:p$107$1:_==p$108$1:var b<br/>[p$107$0 p$107$1]"--> F202
T222 --"p$108$1:_<br/>[]"--> F206
T220 --"p$108$0:p$107$0:_==p$108$0:var a<br/>[p$107$0 p$107$1 p$108$1]"--> T222
T220 --"p$108$0:_<br/>[]"--> F206
T219 --"p$108:mul(p$108$0:_, p$108$1:_)<br/>[p$107$0 p$107$1]"--> T220
T219 --"p$108:_<br/>[]"--> F206
T208 --"p$107:mul(p$107$0:_, p$107$1:_)<br/>[p$108]"--> T219
T223("p$108<br/>[p$108 p$107$0 p$107$1]<br/>(arcs 2)")
T224("p$108$0<br/>[p$107$0 p$108$0 p$107$1 p$108$1]<br/>(arcs 2)")
T225("p$108$1<br/>[p$107$0 p$107$1 p$108$1]<br/>(arcs 2)")
F203("(begin constructor:true)<br/>[p$107$0 p$107$1]")
T225 --"p$108$1:p$107$1:_==p$108$1:var b<br/>[p$107$0 p$107$1]"--> F203
T225 --"p$108$1:_<br/>[]"--> F206
T224 --"p$108$0:p$107$0:_==p$108$0:var a<br/>[p$107$0 p$107$1 p$108$1]"--> T225
T224 --"p$108$0:_<br/>[]"--> F206
T223 --"p$108:sub(p$108$0:_, p$108$1:_)<br/>[p$107$0 p$107$1]"--> T224
T223 --"p$108:_<br/>[]"--> F206
T208 --"p$107:sub(p$107$0:_, p$107$1:_)<br/>[p$108]"--> T223
T226("p$108<br/>[p$108 p$107$0 p$107$1]<br/>(arcs 2)")
T227("p$108$0<br/>[p$107$0 p$108$0 p$107$1 p$108$1]<br/>(arcs 3)")
T228("p$108$1<br/>[p$107$0 p$107$1 p$108$1]<br/>(arcs 2)")
F204("(begin constructor:true)<br/>[p$107$0 p$107$1]")
T228 --"p$108$1:p$107$0:_==p$108$1:var b<br/>[p$107$0 p$107$1]"--> F204
T228 --"p$108$1:_<br/>[]"--> F206
T227 --"p$108$0:p$107$1:_==p$108$0:var a<br/>[p$107$0 p$107$1 p$108$1]"--> T228
T229("p$108$1<br/>[p$107$0 p$107$1 p$108$1]<br/>(arcs 2)")
F205("(begin constructor:true)<br/>[p$107$0 p$107$1]")
T229 --"p$108$1:p$107$1:_==p$108$1:var b<br/>[p$107$0 p$107$1]"--> F205
T229 --"p$108$1:_<br/>[]"--> F206
T227 --"p$108$0:p$107$0:_==p$108$0:var a<br/>[p$107$0 p$107$1 p$108$1]"--> T229
T227 --"p$108$0:_<br/>[]"--> F206
T226 --"p$108:add(p$108$0:_, p$108$1:_)<br/>[p$107$0 p$107$1]"--> T227
T226 --"p$108:_<br/>[]"--> F206
T208 --"p$107:add(p$107$0:_, p$107$1:_)<br/>[p$108]"--> T226
```
