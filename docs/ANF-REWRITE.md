# ANF Rewrite Proposal

The current ANF conversion stage in `anf_normalize.c` is inelegant, verbose and probably incorrect. The main reason for that was the decision that since C does not have native support for closures, the continuation passing style of the classic ANF-conversion algorithm was not an option. Instead the code must jump through hoops passing pre-computed "tail" structures to represent continuation results.

However it's not really that difficult to imitate closures in C. All we need is a pair of a C procedure and a data structure containing the proceedure's free variables. This pair can then get passed as the `k` argument to those C procedures that require it. We can imagine a very simple `INVOKE(k, arg)` C macro or inline procedure to call the contained procedure in `k` passing it `arg` and the free variables needed by `k`.

## Other Pain Points

Another cause of the complexity of the ANF conversion is that the input (domain) of the function is different from the output (range). It would be cleaner to perform the ANF conversion entirely in the `lambda.yaml` domain, then a very naïve transformation to the domain of `anf.yaml` could follow as a separate step. This is based on the observation that the classic ANF algorithm's domain and range are the same.

## The Algorithm

The entire algorithm is given here without comment. Later discussions will elaborate.

```scheme
(define (normalize-term e) (normalize e (λ (x) x)))

(define (normalize-name e k)
  (normalize e (λ (x)
    (if (value? x)
        (k x)
        (let ((y (gensym)))
             `(let (,y ,x) ,(k y)))))))

(define (normalize e k)
    (match e
        (`(λ ,params ,body)
            (k `(λ ,params ,(normalize-term body))))

        ((? value?)
            (k e))

        (`(let ((,x ,val)) ,body)
            (normalize val
                (λ (anfval)
                    `(let ((,x ,anfval))
                          ,(normalize body k)))))

        (`(if ,e0 ,e1 ,e2)
            (normalize-name e0
                (λ (test)
                    (k `(if ,test
                            ,(normalize-term e1)
                            ,(normalize-term e2))))))

        (`(,Fn . ,Ms)
            (normalize-name Fn
                (λ (t)
                    (normalize-names Ms
                        (λ (ts) (k `(,t . ,ts)))))))))

(define (normalize-names Ms k)
    (if (null? Ms)
        (k '())
        (normalize-name (car Ms) (λ (t)
            (normalize-names (cdr Ms) (λ (ts)
                (k `(,t . ,ts))))))))
```

## Implementation Steps

Please note that the following C code is untested and almost certainly contains bugs. It is just to provide concrete examples to make the ideas clear.

### Typedef

We need a typedef for the closure procedure that we can use as a primitive in `lambda.yaml` when declaring the `LamKont`:

```C
// lambda_functions.h

struct LamMap; // forward declaration
struct LamData; // forward declaration

typedef struct LamData *(*LamContProc)(struct LamData *, struct LamMap *);
```

There is precedence for this in the `PrattParselet` type used by the parser.

### New `LamKont` struct, `LamData` union and `LamMap` Hash

```yaml
structs:
    LamKont:
        meta:
            brief: structure representing a C continuation
        data:
            proc: LamContProc
            map: LamMap
unions:
    LamExp:
        data: &lamexp-types
            ...
    LamData:
        meta:
            brief: anything we need to pass in a LamMap
        data:
            <<: *lamexp-types
            kont: LamKont
            args: LamArgs
hashes:
    LamMap:
        meta:
            brief: Free variables for a LamKont
        data:
            entries: LamData
```

Note the use of yaml anchors and references to copy all of the `LamExp` types to `LamData`.

### Inline Function to Invoke a Continuation

```C
static inline LamData *INVOKE(LamKont *k, LamData *arg) {
    return k->proc(arg, k->map);
}
```

### Translate the Existing Algorithm to C

This is the payoff. Let's break down the algorithm to get to the details. I'm particularily interested in identifying where all the continuations are created and invoked, so I'm delimiting the continuations themselves in square brackets within the following scheme examples.

#### Utilities

Collected here as the algorithm translation gets filled out.

```C
static bool        IS_VALUE(LamExp *);
static LamData    *genExpSymDollar(char *);
static void        setLamMap_Symbol(LamMap *, HashSymbol *, HashSymbol *);
static void        setLamMap_Kont(LamMap *, HashSymbol *, LamKont *);
static void        setLamMap_Args(LamArgs *, HashSymbol *, LamKont *);
static HashSymbol *getLamMap_Symbol(LamMap *, HashSymbol *);
static LamKont    *getLamMap_Kont(LamMap *, HashSymbol *);
static LamArgs    *getLamMap_Args(LamMap *, HashSymbol *);
```

#### 1. `noramalize-term`

```scheme
(define (normalize-term e) (normalize e [λ (x) x]))
```

In this case the translation is trivial, the continuation is the identity function:

```C
static LamData *identity(LamData *x,
                         LamMap *map __attribute__((unused))) {
    return x;
}

static LamData *normalizeTerm(LamData *e) {
    LamKont *k = newLamKont(identity, NULL);
    int save = PROTECT(k);
    LamExp *result = normalize(e, k);
    UNPROTECT(save);
    return result;
}
```

#### 2. `normalize-name`

```scheme
(define (normalize-name e k)
  (normalize e [λ (x)
    (if (value? x)
        (k x)
        (let ((y (gensym)))
             `(let (,y ,x) ,(k y))))]))
```

Not quite so trivial, but manageable. The constructed continuation here has a free variable `k`. This means our `LamData` will need `LamKont` as a member, so we can pass it via the map.


```C
static LamData *normalizeNameKont(LamData *x, LamMap *map) {
    LamKont *k = getLamMap_Kont(map, TOK_K());
    if (IS_VALUE(x)) { // constant or symbol
        return INVOKE(k, x);
    }
    LamData *y = genExpSymDollar("y");
    int save = PROTECT(y);
    LamData *body = INVOKE(k, y);
    PROTECT(body);
    LamData *let = makeLamData_Let(y, x, getLamData_Exp(body));
    UNPROTECT(save);
    return let;
}

static LamData *normalizeName(LamData *e, LamKont *k) {
    LamMap *map = newLamMap();
    int save = PROTECT(map);
    setLamMap_Kont(map, TOK_K(), k);
    LamKont *k2 = newLamKont(normalizeNameKont, map);
    PROTECT(k2);
    LamData *result = normalize(exp, k2);
    UNPROTECT(save);
    return result;
}
```

#### 3. `normalize`


```scheme
(define (normalize e k)
    (match e
        (`(λ ,params ,body)
            (k `(λ ,params ,(normalize-term body))))

        ((? value?)
            (k e))

        (`(let ((,x ,val)) ,body)
            (normalize val
                [λ (anfval)
                    `(let ((,x ,anfval))
                          ,(normalize body k))]))

        (`(if ,e0 ,e1 ,e2)
            (normalize-name e0
                [λ (test)
                    (k `(if ,test
                            ,(normalize-term e1)
                            ,(normalize-term e2)))]))

        (`(,Fn . ,Ms)
            (normalize-name Fn
                [λ (t)
                    (normalize-names Ms
                        [λ (ts) (k `(,t . ,ts))])]))))
```

Decidedly non-trivial, but let's do it one case at a time, and lets assume all of the dispatches look like this:

```C
case LAMDATA_TYPE_THING:
    return normalizeThing(e->val.thing, k);
```

---

First case is a lambda expression:

```scheme
    (`(λ ,params ,body)
        (k `(λ ,params ,(normalize-term body))))
```

There's no new continuation being created here.

```C
static LamData *normalizeLam(LamLam *lam, LamKont *k) {
    LamData *body = normalizeTerm(newLamData_Exp(lam->exp));
    int save = PROTECT(body);
    LamData *exp = makeLamData_Lam(lam->args, getLamData_Exp(body));
    PROTECT(exp);
    LamData *result = INVOKE(k, exp);
    UNPROTECT(save);
    return result;
}
```

---

Next simple atomic values (symbols and constants):

```scheme
    ((? value?)
        (k e))
```

In this case the dispatch will have multiple matching cases so it will pass the entire `LamData`.

```C
static LamData *normalizeValue(LamData *e, LamKont *k) {
    return INVOKE(k, e);
}
```

---

Next let expressions:

```scheme
    (`(let ((,x ,val)) ,body)
        (normalize val
            [λ (anfval)
                `(let ((,x ,anfval))
                    ,(normalize body k))]))
```

The new continuation has free variables `x`, `body` and `k`.

```C
static LamData *normalizeLetKont(LamData *anfval, LamMap *map) {
    HashSymbol *x = getLamMap_Symbol(map, TOK_X());
    LamData *body = getLamMap_Data(map, TOK_BODY());
    LamKont *k = getLamMap_Kont(map, TOK_K());
    LamExp *anfbody = normalize(body, k);
    int save = PROTECT(anfbody);
    LamData *let = makeLamData_Let(x, getLamData_Exp(anfval), getLamData_Exp(anfbody));
    UNPROTECT(save);
    return let;
}

static LamData *normalizeLet(LamLet *let, LamKont *k) {
    LamMap *map = newLamMap();
    int save = PROTECT(map);
    setLamMap_Symbol(map, TOK_X(), let->var);
    setLamMap_Exp(map, TOK_NBODY(), let->body);
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

---

Next if-expressions:

```scheme
    (`(if ,e0 ,e1 ,e2)
        (normalize-name e0
            [λ (anfE0)
                (k `(if ,anfE0
                        ,(normalize-term e1)
                        ,(normalize-term e2)))]))
```

The continuation has free variables `k`, `e1` and `e2`.

```C
static lamData *normalizeIfKont(lamData *anfE0, LamMap *map) {
    LamKont *k = getLamMap_Kont(map, TOK_K());
    LamData *e1 = getLamMap_Data(map, TOK_E1());
    LamData *e2 = getLamMap_Data(map, TOK_E2());
    LamData *anfE1 = normalizeTerm(e1);
    int save = PROTECT(anfE1);
    LamData *anfE2 = normalizeTerm(e2);
    PROTECT(anfE2);
    LamData *iff = makeLamData_Iff(getLamData_Exp(anfE0), getLamData_Exp(anfE1), getLamData_Exp(anfE2));
    PROTECT(iff);
    LamData *result = INVOKE(k, iff);
    UNPROTECT(save);
    return result;
}

static LamData *normalizeIff(LamIff *iff, LamKont *k) {
    LamMap *map = newLamMap();
    int save = PROTECT(map);
    setLamMap_Kont(map, TOK_K(), k);
    setLamMap_Exp(map, TOK_E1(), iff->consequent);
    setLamMap_Exp(map, TOK_E2(), iff->alternative);
    LamKont *k2 = newLamKont(normalizeIfKont, map);
    PROTECT(k2);
    LamData *result = normalizeName(newLamData_Exp(iff->condition), k2);
    UNPROTECT(save);
    return result;
}
```

---

Finally the function call branch:

```scheme
    (`(,Fn . ,Ms)
        (normalize-name Fn
            [λ (t)
                (normalize-names Ms
                    [λ (ts) (k `(,t . ,ts))])]))
```

Note this is function application, which in scheme is syntactically just consing up a list, wheras our LamExp has a specific LamApply type for this.

This is more interesting because there are two nested continuations. However we can simply pass the map from one to the other so it's not too bad. Starting with the inner continuation, it has free variables `t` and `k`.

```C
static LamData *normalizeCallInnerKont(LamData *ts, LamMap *map) {
    LamExp *t = getLamMap_Exp(map, TOK_T());
    LamKont *k = getLamMap_Kont(map, TOK_K());
    LamData *apply = makeLamData_Apply(getLamData_Exp(t), getLamData_Args(ts));
    int save = PROTECT(apply);
    LamData *result = INVOKE(k, apply);
    UNPROTECT(save);
    return result;
}

static LamData *normalizeCallOuterKont(LamData *t, LamMap *map) {
    LamData *Ms = getLamMap_Data(map, TOK_MS());
    setLamMap_Data(map, TOK_T(), t);
    LamKont *k = newLamKont(normalizeCallInnerKont, map);
    int save = PROTECT(k);
    LamData *result = normalizeNames(Ms, k);
    UNPROTECT(save);
    return result;
}

static LamData *normalizeCall(LamApply *apply, LamKont *k) {
    LamMap *map = newLamMap();
    int save = PROTECT(map);
    setLamMap_Args(map, TOK_MS(), apply->args);
    setLamMap_Kont(map, TOK_K(), k);
    LamKont *k2 = newLamKont(normalizeCallOuterKont, map);
    PROTECT(k2);
    LamData *f = newLamData_Exp(apply->function);
    PROTECT(f);
    LamData *result = normalizeName(f, k2);
    UNPROTECT(save);
    return result;
}
```

#### 4. `normalize-names`

```scheme
(define (normalize-names Ms k)
    (if (null? Ms)
        (k '())
        (normalize-name (car Ms) [λ (t)
            (normalize-names (cdr Ms) [λ (ts)
                (k `(,t . ,ts))])])))
```

In this case it is not function application, it is just normalizing the arguments to a function.

Again two nested continuations. The inner one has free variables `k` and `t`.

```C
static LamData *normalizeNamesInnerKont(LamData *ts, LamMap *map) {
    LamKont *k = getLamMap_Kont(map, TOK_K());
    LamExp *t = getLamMap_Exp(map, TOK_T());
    LamData *args = newLamData_Args(t, getLamData_Args(ts));
    int save = PROTECT(args);
    LamData *result = INVOKE(k, args);
    UNPROTECT(save);
    return result;
}
```

The outer continuation uses a free variable `Ms` but must also provide the value `t` to the free variables of the inner continuation.

```C
static LamData *normalizeNamesOuterKont(LamData *t, LamMap *map) {
    LamArgs *Ms = getLamMap_Args(map, TOK_MS());
    setLamMap_Data(map, TOK_T(), t);
    LamKont *k2 = newLamKont(normalizeNamesInnerKont, map);
    int save = PROTECT(k2);
    LamData *result = normalizeNames(newLamData_Args(Ms->next), k2);
    UNPROTECT(save);
    return result;
}
```

Lastly the procedure itself:

```C
static LamData *normalizeNames(LamData *Ms, LamKont *k) {
    if (getLamData_Args(Ms) == NULL) {
        return INVOKE(k, Ms);
    } else {
        LamMap *map = newLamMap();
        int save = PROTECT(map);
        setLamMap_Kont(map, TOK_K(), k);
        setLamMap_Data(map, TOK_MS(), Ms);
        LamKont *k2 = newLamKont(normalizeNamesOuterKont, map);
        PROTECT(k2);
        LamData *value = newLamData_Exp(getLamData_Args(Ms)->value);
        PROTECT(value);
        LamData *result = normalizeName(value, k2);
        UNPROTECT(save);
        return result;
    }
}
```

That's certainly a lot less code. Of course there are many more cases in the full set of lambda expressions, and I'm not pretending that any of the above is just going to work.

However the practice of specifying the algorithm in scheme before writing it in C feels very natural and productive.

## `letrec`

One of the extensions we're going to need is `letrec` where there are multiple parallel bindings before the body. It's syntactically equivalent to a normal `let` so we can start with the existing `let` and extend it to have two bindings just to see what it looks like:

```scheme
(`(letrec ((,x1 ,val1)
           (,x2 ,val2)) ,body)
    (let ((anfbody (normalize body k)))
        (normalize val1
            [λ (anfval1)
                (normalize val2
                    [λ (anfval2)
                        `(letrec ((,x1 ,anfval1)
                                  (,x2 ,anfval2)) ,anfbody)])])))
```

That was easier than I expected, and we can see how this pattern would expand for three, four bindings etc.

All well and good, but we need the general case for any number of bindings.

```scheme
(`(letrec ,bindings ,body)
    (normalize-bindings bindings
        [λ (anfbindings)
            `(letrec ,anfbindings, ,(normalize body k))]))
```

So the hard work is left to `normalize-bindings`.

```scheme
(define (normalize-bindings bindings k)
  (match bindings
    ('()
       (k '()))
    (`((,x ,val) . ,rest)
        (normalize val
            [λ (anfval)
              (normalize-bindings rest
                [λ (anfrest)
                    (k `((,x ,anfval) . ,anfrest))])]))))
```
♮
What's the chances that'll work? Rather than going straight to C, why not write it in F♮?

```fn
typedef expr {
    ...
    letrec(list(#(string, expr)), expr)
    ...
}

    // one clause of normalize:
    (letrec(bindings, body), k) {
        let
            anfBody = normalize(body, k);
        in
            normalizeBindings(bindings,
                fn (anfBindings) {
                    letrec(anfbindings, anfBody)
                }
            )
    }

fn normalizeBindings {
    ([], k) { k([]) }
    (#(x, val) @ rest) {
        normalize(val,
            fn (anfVal) {
                normalizeBindings(rest,
                    fn (anfRest) {
                        k(#(x, anfVal) @ anfRest)
                    }
                )
            }
        )
    }
}
```

[normalize.fn](../fn/rewrite/normalize.fn) demonstrates that the normalization of letrec is working.

I think I still prefer to translate from scheme to C, but having working code in F♮ as a PoC is a real plus.
So with this core subset working let's extend to cover the rest of the implementation.

## The Rest

The current list of `LamExp` in `lambda.yaml` is:

```yaml
data:
    amb: LamAmb
    apply: LamApply
    back: void_ptr
    biginteger: MaybeBigInt
    callcc: LamExp
    character: character
    cond: LamCond
    constant: LamConstant
    construct: LamConstruct
    constructor: LamTypeConstructorInfo
    deconstruct: LamDeconstruct
    env: void_ptr
    error: void_ptr
    iff: LamIff
    lam: LamLam
    let: LamLet
    letrec: LamLetRec
    lookup: LamLookup
    make_tuple: LamArgs
    makeVec: LamMakeVec
    match: LamMatch
    namespaces: LamNamespaceArray
    prim: LamPrimApp
    print: LamPrint
    sequence: LamSequence
    stdint: int
    tag: LamExp
    tuple_index: LamTupleIndex
    typedefs: LamTypeDefs
    typeOf: LamTypeof
    var: HashSymbol
```

This is not nearly as bad as it looks, many if not most of these are eather constants or AExpr or applications, and can be lumped in to extra clauses of existing cases in `normalize`. But let's be systematic and cover them one at a time.

### amb: LamAmb

This does need special handling, both the lhs and rhs should not be evaluated until needed. However we can treat it much like an `Iff` with no test:

```scheme
   (`(amb ,e0 ,e1)
     (k `(amb ,(normalize-term e0)
              ,(normalize-term e1))))
```

### apply: LamApply

Done, already.

### back: void_ptr

No substructure so can be rolled in with `value`.

### biginteger: MaybeBigInt

Value

### callcc: LamExp

Done already.

### character: character

Value

### cond: LamCond

Ok, this is a bit more involved,

```yaml
structs:
    LamCond:
        data:
            value: LamExp
            cases: LamCondCases
    LamIntCondCases:
        data:
            constant: MaybeBigInt
            body: LamExp
            next: LamIntCondCases
    LamCharCondCases:
        data:
            constant: character
            body: LamExp
            next: LamCharCondCases
unions:
    LamCondCases:
        data:
            integers: LamIntCondCases
            characters: LamCharCondCases
```

In scheme we can just blur the char/int distinction, and it effectively becomes an `if` with multiple cases and a default. Let's review `if` and see.

```scheme
    (`(if ,e0 ,e1 ,e2)
        (normalize-name e0
            [λ (anfE0)
                (k `(if ,anfE0
                        ,(normalize-term e1)
                        ,(normalize-term e2)))]))
```

So yeah, it's going to be mostly the same but we need to break out a map over the cases.

```scheme
    (`(cond ,e0 ,cases)
        (normalize-name e0
            [λ (anfE0)
                (k `(cond ,anfE0
                          ,(normalize-cases cases)))]))
```

which leaves

```scheme
(define (normalize-cases cases)
    (map (λ (case)
            (match case `(,val ,expr) `(,val ,(normalize-term expr))))
         cases))
```

### constant: LamConstant

A "constant" in this sense is a type constructor with no arguments, so it can be treated as a Value.

### construct: LamConstruct

The application of a type constructor to its arguments, can be treated like apply.

```scheme
    (`(construct ,name ,Ms))
        (normalize-names Ms
            (λ (ts) (k `(construct ,name . ,ts))))
```

### constructor: LamTypeConstructorInfo

Container for type constructor information, but all the contained values are constants so it can be treated as a constant value.

### deconstruct: LamDeconstruct

An accessor for a construct, vec index basically. The entity being deconstructed is an expression, so this can be treated as an apply with one argument.

```scheme
    (`(deconstruct ,name ,index ,e0)
        (normalize-name e0 (λ (t)
            (k `(deconstruct ,name ,index ,t)))))
```

### env: void_ptr

Constant.

### error: void_ptr

Constant.

### iff: LamIff

Done already.

### lam: LamLam

Done already.

### let: LamLet

Done already.

### letrec: LamLetRec

Done already.

### lookup: LamLookup

This will probably need revisiting.

```yaml
    LamLookup:
        data:
            nsid: int
            nsSymbol: HashSymbol
            exp: LamExp
```

The langage actually allows constructs like:

```
print ns.(a() + b())
```

So for now we treat the expression as a Cexp

```scheme
    (`(lookup ,name ,id ,expr)
      (k `(lookup ,name ,id ,(normalize-term expr))))
```

and assume later conversion will supply it with the namespace context.

### make_tuple: LamArgs

Basically the same as construct

```scheme
    (`(make-tuple . ,Ms)
        (normalize-names Ms
            (λ (ts) (k `(make-tuple . ,ts)))))
```

### makeVec: LamMakeVec

```scheme
    (`(make-vec ,nargs . ,Ms)
        (normalize-names Ms
            (λ (ts) (k `(make-vec ,nargs . ,ts)))))
```
### match: LamMatch

```yaml
    LamMatch:
        data:
            index: LamExp
            cases: LamMatchList
    LamMatchList:
        data:
            matches: LamIntList
            body: LamExp
```

Very similar to `cond`

```scheme
    (`(match-expr ,e0 ,cases)
        (normalize-name e0
            [λ (anfE0)
                (k `(match-expr ,anfE0
                                ,(normalize-cases cases)))]))
```

We can even re-use `match-cases`.

### namespaces: LamNamespaceArray

Just an array of exp, but each will need to be treated as a Cexp

```scheme
    (`(namespaces , ,Ms)
     (k `(namespaces . ,(normalize-terms Ms))))
```

```scheme
(define (normalize-terms terms)
  (map (λ (term) (normalize-term term)) terms))
```

### prim: LamPrimApp

Treated like any other apply.

```scheme
    (`(primitive-apply ,op ,e0 ,e1)
        (normalize-name e0
            [λ (anfE0)
                (noramalize-name e1
                    [λ (anfE1)
                        (k `(primitive-apply ,op ,anfE0 ,anfE1))])]))
```

### print: LamPrint

Just an apply with a single argument.

```scheme
    (`(print ,e0)
        (normalize-name e0
            [λ (anfE0) (k `(print ,anfE0))]))
```

### sequence: LamSequence

```scheme
    (`(begin . ,Ms)
     (k `(begin . (normalize-terms Ms))))
```

### stdint: int

Constant value.

### tag: LamExp

Used to retrieve the type constructor index from a type instance, for pattern matching.

```scheme
    (`(tag ,e0)
      (normalize-name e0
        [λ (t0) (k `(tag ,t0))]))
```

### tuple_index: LamTupleIndex

Same sort of thing, used to retrieve a value from a tuple.

```scheme
    (`(tuple-index ,size ,index ,e0)
      (normalize-name e0
        [λ (t0) (k `(tuple-index ,size ,index ,t0))]))
```

### typedefs: LamTypeDefs

Analogous to a `letrec`, the `LamTypeDefs` expression contains a list of typedefs (which contain only static definitions) and a `LamExp` body (typically a `letrec`) to execute with the typedefs defined. For that reason we don't want to hoist temporary variables above the letrecs so we call `normalize-term` on the body.

In fact, looking at the current/old C implementation, ANF conversion simply discards the typedefs entirely,
but we can leave that to the simple domain mapping phase after this.

```scheme
    (`(typedefs ,defs ,body)
      (k `(typedefs ,defs ,(normalize-term body))))
```

### typeOf: LamTypeof

Same as print etc.

```scheme
    (`(typeof ,e0)
        (normalize-name e0
            [λ (anfE0) (k `(typeof ,anfE0))]))
```

### var: HashSymbol

Variable, done already.

## Working Code
[normalize.fn](../fn/rewrite/normalize.fn) now has working examples of all of the above, written in F♮.