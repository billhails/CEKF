# ANF Rewrite Proposal

The current ANF conversion stage in `anf_normalize.c` is inelegant, verbose and probably incorrect. The main reason for that was the decision that since C does not have native support for closures, the continuation passing style of the classic ANF-conversion algorithm was not an option. Instead the code must jump through hoops passing pre-computed "tail" structures to represent continuation results.

However it's not really that difficult to imitate closures in C. All we need is a pair of a C procedure and a data structure containing the proceedure's free variables. This pair can then get passed as the `k` argument to those C procedures that require it. We can imagine a very simple `INVOKE(k, arg)` C macro or inline procedure to call the contained procedure in `k` passing it `arg` and the free variables needed by `k`.

It remains to be decided whether we use a hash table to store the free variables, necessitating wrapping everything in `LamExp`, or just have a slot in `LamKont` for every free we need. There will not be that many as types can be re-used.

## Other Pain Points

Another cause of the complexity of the ANF conversion is that the input (domain) of the function is different fron the output (range). It might be cleaner to perform the ANF conversion entirely in the `lambda.yaml` domain, then a very naïve transformation to the domain of `anf.yaml` could follow as a separate step. This is based on the observation that the classic ANF algorithm's domain and range are the same.

## Implementation Steps

### Typedef

We need a typedef for the closure procedure that we can use as a primitive in `lambda.yaml` when declaring the `LambdaKont`:

```C
struct LamMap; // forward declaration
struct LamExp; // forward declaration

typedef struct LamExp *(*LamContProc)(struct LamExp *, struct LamMap *);
```

There is precedence for this in the `PrattParselet` type used by the parser.

### New `LamKont` struct and `LamMap` Hash

```yaml
structs:
    LamKont:
        meta:
            brief: structure representing a C continuation
        data:
            proc: LamContProc
            map: LamMap
hashes:
    AnfMap:
        meta:
            brief: Free variables for a LamKont
        data:
            entries: LamExp
```

### Inline Function to Invoke a Continuation

```C
static inline LamExp *INVOKE(LamKont *k, LamExp *arg) {
    return k->proc(arg, k->map);
}
```

### Translate the Existing Algorithm to C

This is the payoff. Let's break down the algorithm to get to the details. I'm particularily interested in identifying where all the continuations are created and invoked, so I'm delimiting the continuations themselves in square brackets.

#### 1. `noramalize-term`

```scheme
(define (normalize-term e) (normalize e [λ (x) x]))
```

In this case the translation is trivial, the continuation is the identity function:

```C
static LamExp *identity(LamExp *x,
                        LamMap *map __attribute__((unused))) {
    return x;
}

static LamExp *normalizeTerm(LamExp *lam) {
    LamKont *k = newLamKont(identity, NULL);
    int save = PROTECT(k);
    LamExp *result = normalize(exp, k);
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

Not quite so trivial, but manageable. The constructed continuation here has a free variable `k`.

OK, so the map may need to be more flexible than just holding `LamExp`, but maybe it only ever needs `LamKonts`, we'll see.

```C
LamExp normalizeNameKont(LamExp *x, LamMap *map) {
    // extractKont unwraps k
    LamKont *k = extractKont(map, TOK_K());
    if (IS_VALUE(x)) {
        return INVOKE(k, x);
    }
    // genExpSymDollar wraps the HashSymbol
    LamExp *y = genExpSymDollar("y");
    int save = PROTECT(y);
    LamExp *body = INVOKE(k, y);
    PROTECT(body);
    LamExp *anfLet = makeLamExp_Let(y, x, body); // more pseudocode
    UNPROTECT(save);
    return anfLet;
}

LamExp *normalizeName(LamExp *e, LamKont *k) {
    LamMap *map = newLamMap();
    int save = PROTECT(map);
    // setLamMapKont wraps k as a LamExp
    setLamMapKont(map, TOK_K(), k);
    LamKont *k = newLamKont(identity, map);
    PROTECT(k);
    LamExp *result = normalize(exp, k);
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

Decidedly non-trivial, but let's do it one case at a time,
and lets assume all of the dispatches look like this:

```C
case LAMEXP_TYPE_THING:
    return normalizeThing(lamExp->val.thing, k);
```

---

First a lambda expression:

```scheme
(`(λ ,params ,body)
    (k `(λ ,params ,(normalize-term body))))
```

There's no continuation being created here.

```C
static LamExp *normalizeLam(LamLam *lam, LamKont *k) {
    LamExp *body = normalizeTerm(lam->exp);
    int save = PROTECT(body);
    LamExp *exp = makeLamExp_Lam(lam->args, body);
    PROTECT(exp);
    AnfExp *result = INVOKE(k, exp);
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

In this case the dispatch will have multiple matching cases so it will pass the entire `LamExp`.

```C
static LamExp *normalizeValue(LamExp *val, LamKont *k) {
    return INVOKE(k, val);
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
static LamExp *normalizeLetKont(LamExp *anfval, LamMap *map) {
    HashSymbol *x = extractHashStmbol(map, TOK_X());
    LamExp *body = extractLamExp(map, TOK_BODY());
    LamKont *k = extractKont(map, TOK_K());
    LamExp *anfbody = normalize(body, k);
    int save = PROTECT(anfbody);
    LamExp *anfLet = makLamLet(x, anfval, anfbody);
    UNPROTECT(save);
    return anfLet;
}

static LamExp normalizeLet(LamLet *let, AnfKont *k) {
    LamMap *map = newLamMap();
    PROTECT(map);
    setLamMapSymbol(map, TOK_X(), let->var);
    setLamMapExp(map, TOK_NBODY(), anfbody);
    setLamMapKont(map, TOK_K(), k);
    LamKont *k2 = newLamKont(normalizeLetKont, map);
    PROTECT(k2);
    LamExp *result = normalize(let->val, k2);
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
static lamExp *normalizeIfKont(lamExp *anfE0, LamMap *map) {
    LamKont *k = extractKont(map, TOK_K());
    LamExp *e1 = extractExp(map, TOK_E1());
    LamExp *e2 = extractExp(map, TOK_E2());
    LamExp *anfE1 = normalizeTerm(e1);
    int save = PROTECT(anfE1);
    LamExp *anfE2 = normalizeTerm(e2);
    PROTECT(anfE2);
    LamExp *iff = makeLamExp_Iff(anfE0, anfE1, anfE2);
    PROTECT(iff);
    AnfExp *result = INVOKE(k, iff);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeIff(LamIff *iff, LamKont *k) {
    AnfMap *map = newAnfMap();
    int save = PROTECT(map);
    setLamMap_Kont(map, TOK_K(), k);
    setLamMap_Exp(map, TOK_E1(), iff->consequent);
    setLamMap_Exp(map, TOK_E2(), iff->alternative);
    AnfKont *k2 = newAnfKont(normalizeIfKont, map);
    PROTECT(k2);
    AnfExp *result = normalizeName(iff->condition, k2);
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

This is more interesting because there are two nested continuations.
However we can simply pass the map from one to the other so it's not too bad. Starting with the inner continuation, it has free variables `t` and `k`.

```C
static LamExp *normalizeCallInnerKont(LamExp *ts, LamMap *map) {
    LamExp *t = extractExp(map, TOK_T());
    LamKont *k = extractKont(map, TOK_K());
    LamExp *apply = makeLamExp_Apply(t, ts);
    int save = PROTECT(apply);
    LamExp *result = INVOKE(k, apply);
    UNPROTECT(save);
    return result;
}

static LamExp *normalizeCallOuterKont(LamExp *t, LamMap *map) {
    LamExp *Ms = NULL;
    getAnfMap(map, TOK_MS(), &Ms);
    setAnfMap(map, TOK_T(), t);
    AnfKont *k = newAnfKont(normalizeCallInnerKont, map);
    int save = PROTECT(k);
    AnfExp *result = normalizeNames(Ms, k);
    UNPROTECT(save);
    return result;
}

static AnfExp *normalizeCall(LamApply *apply, AnfKont *k) {
    AnfMap *map = newAnfMap();
    int save = PROTECT(map);
    setAnfMap(map, TOK_MS(), <convert>(apply->args));
    setAnfMap(map, TOK_K(), k);
    AnfKont *k2 = newAnfKont(normalizeCAllOuterKont, map);
    PROTECT(k2);
    AnfExp *result = normalizeName(apply->function, k2);
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

Again two nested continuations. The inner one has free variables `k` and `t`.

```C
static AnfExp *normalizeNamesInnerKont(AnfExp *ts, AnfMap *map) {
    AnfKont *k = NULL;
    getAnfMap(map, TOK_K(), &k);
    AnfExp *t = NULL;
    getAnfMap(map, TOK_T(), &t);
    AnfCexpApply *apply = newAnfApply(t, ts);
    int save = PROTECT(apply);
    AnfExp *result = INVOKE(k, apply);
    UNPROTECT(save);
    return result;
}
```

The outer continuation uses a free variable `Ms` but must also provide the value `t` to the free variables of the inner continuation.

```C
static AnfExp *normalizeNamesOuterKont(AnfExp *t, AnfMap *map) {
    AnfExp *Ms = NULL;
    getAnfMap(map, TOK_MS(), &Ms);
    setAnfMap(map, TOK_T(), t);
    AnfKont *k2 = newAnfKont(normalizeNamesInnerKont, map);
    int save = PROTECT(k2);
    AnfExp *result = normalizeNames(Ms->next, k2);
    UNPROTECT(save);
    return result;
}
```

Lastly the procedure itself:

```C
static AnfExp *normalizeNames(LamList *Ms, AnfKont *k) {
    if (Ms == NULL) {
        return INVOKE(k, NULL);
    } else {
        AnfMap *map = newAnfMap();
        int save = PROTECT(map);
        setAnfMap(map, TOK_K(), k);
        setAnfMap(map, TOK_MS(), Ms);
        AnfKont *k2 = newAnfKont(normalizeNamesOuterKont, map);
        PROTECT(k2);
        AnfExp *result = normalizeName(Ms->value, k2);
        UNPROTECT(save);
        return result;
    }
}
```

That's certainly a lot less code. Of course there are many more cases in the full set of lambda expressions, and I'm not pretending that any of the above is just going to work, the `AnfKont` will need to be included in `AnfExp` before the `AnfMap` will accept them, and the Argument to normalizeNames is still in the lambda domain when it is passed to the continuations.

However the practice of specifying the algorithm in scheme before writing it in C feels very natural and productive.

## `letrec`

One of the extensions we're going to need is `letrec` where there are multiple parallel bindings before the body. It's syntactically equivalent to a normal `let` so we can start with the existing `let` and extend it to have two bindings just to see what happens:

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

That was easier than I expected.
All well and good, but now we need the general case for any number of bindings.

```scheme
(`(letrec ,bindings ,body)
    (let ((anfbody (normalize body k)))
        (normalize-bindings bindings
            [λ (anfbindings)
                `(letrec ,anfbindings, ,anfbody)])))
```

So the hard work is left to `normalize-bindings`.

```scheme
(define normalize-bindings (bindings k)
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

What's the chances that'll work? Rather than going straight to C, why not write it in F♮?

```
typedef Lambda {
    letrec(list(#(string, expr)), expr)
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

We should then be able to test that.