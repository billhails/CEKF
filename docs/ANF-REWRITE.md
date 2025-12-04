# ANF Rewrite Proposal

The current ANF conversion stage in `anf_normalize.c` and `anf.yaml` is clumsy, inelegant and verbose. The main reason for that was the decision that since C does not have native support for closures, the continuation-passing of the classic ANF-conversion algorithm was not available and instead the code must jump through hoops passing pre-computed "tail" structures to represent continuation results.

However it's not really difficult to imitate closures and continuations in C. All we need is a pair of a C procedure and a map  containing its free variables. This can be a new structure that gets passed as the `k` argument to those C procedures that require it. We can imagine a very simple `INVOKE(k, arg)` C macro or inline procedure to call the contained procedure in `k` passing it `arg` and the map from `k`. At the point the continuation (closure) is constructed, the required free variables are added to a new map and the map plus the static procedure are bound in a new `AnfKont` struct.

## Implementation Steps

### Renaming

As a preliminary I'd like to rename all of the entities in `anf.yaml` to have an `Anf` prefix (`AnfExp`, `AnfCexp` etc.) This just brings the code in line with other stages that have their own prefix namespace and makes the pairing of i.e. `LamExp` with `AnfExp` easier to follow.

### Typedef

We need a typedef for the closure procedure that we can use as a primitive in `anf.yaml` when declaring the `AnfKont`:

```C
struct AnfMap; // forward declaration
struct AnfExp; // forward declaration

typedef struct AnfExp *(*AnfContProc)(struct AnfExp *, struct AnfMap *);
```

### New AnfKont struct and AnfMap map

```yaml
structs:
    AnfKont:
        meta:
            brief: structure representing a C continuation
        data:
            proc: AnfContProc
            map: AnfMap
hashes:
    AnfMap:
        meta:
            brief: Free variables for an AnfKont
        data:
            entries: AnfExp
```

### Inline Function to Invoke a Continuation

```C
static inline AnfExp *invoke(AnfKont *k, AnfExp *arg) {
    return k->proc(arg, k->map);
}
```

### Translate the Existing Algorithm to C

This is the payoff. Let's break down the algorithm to get to the details. I'm particularily interested in identifying where all the closures are created and invoked.

#### noramalize-term

```scheme
(define (normalize-term e) (normalize e [λ (x) x]))
```

For clarity, the continuation is delimited by square brackets. In this case it is trivial, the continuation is the identity function:

```C
static AnfExp *identity(AnfExp *x,
                        AnfMap *map __attribute__((unused))) {
    return x;
}

static AnfExp *normalizeTerm(LamExp *lam) {
    AnfKont *k = newAnfKont(identity, NULL);
    int save = PROTECT(k);
    AnfExp *result = normalize(exp, k);
    UNPROTECT(save);
    return result;
}
```

#### normalize-name

```scheme
(define (normalize-name e k)
  (normalize e [λ (x)
    (if (value? x)
        (k x)
        (let ((y (gensym)))
             `(let (,y ,x) ,(k y))))]))
```

Not quite so trivial, but manageable. The constructed continuation here has a free variable `k`.

OK, so the map may need to be more flexible than just holding AnfExp, but maybe it only ever needs AnfKonts, we'll see.

```C
AnfExp normalizeNameKont(AnfExp *x, AnfMap *map) {
    AnfKont *k;
    getAnfMap(map, TOK_K(), &k);
    if (IS_VALUE(x)) {
        return invoke(k, x);
    }
    HashSymbol *y = genSymDollar("y");
    AnfExp *body = invoke(k, y); // pseudocode, need to make y an AnfExp
    int save = PROTECT(body);
    AnfExp *anfLet = makAnfLet(y, x, body); // more pseudocode
    UNPROTECT(save);
    return anfLet;
}

AnfExp *normalizeName(LamExp *e, AnfKont *k) {
    AnfMap *map = newAnfMap;
    int save = PROTECT(map);
    setAnfMap(map, TOK_K(), k);
    AnfKont *k = newAnfKont(identity, map);
    PROTECT(k);
    AnfExp *result = normalize(exp, k);
    UNPROTECT(save);
    return result;
}
```

#### normalize

Big dispatch function

```scheme
(define (normalize e k)
    (match e
        (`(λ ,params ,body)
            (k `(λ ,params ,(normalize-term body))))

        ((? value?)
            (k e))

        (`(let ((,x ,eb)) ,e-body)
            (normalize eb
                [λ (e-r)
                    `(let ((,x ,e-r))
                    ,(normalize e-body k))]))

        (`(if ,e0 ,e1 ,e2)
            (normalize-name e0
                [λ (e-r)
                    (k `(if ,e-r ,(normalize-term e1)
                    ,(normalize-term e2)))]))

        (`(,Fn . ,M*)
            (normalize-name Fn
                [λ (t)
                    (normalize-name* M*
                        [λ (t*) (k `(,t . ,t*))])]))))
```

Decidedly non-trivial, but let's do it one case at a time.

First a lambda expression:

```scheme
(`(λ ,params ,body)
    (k `(λ ,params ,(normalize-term body))))
```

There's no continuation being created here, so lets assume all of the dispatches look like this:

```C
case LAMEXP_TYPE_LAM:
    return normalizeLam(lamExp->val.lam, k);
```

and then for lambdas it's:

```C
static AnfExp *normalizeLam(LamLam *lam, AnfKont *k) {
    AnfExp *body = normalizeTerm(lam->body);
    int save = PROTECT(body);
    AnfAexpVarList *varlist = <convert>(lam->varList);
    PROTECT(varlist);
    AnfAexpLam *anfLam = newAnfAexpLam(varlist, body);
    PROTECT(body);
    AnfExp *result = invoke(k, anfLam);
    UNPROTECT(save);
    return result;
}
```

Next simple atomic values (symbols and constants):

```scheme
((? value?)
    (k e))
```

```C
static AnfExp *normalizeValue(LamValue *val, AnfKont *k) {
    AnfAexpValue *v = <convert>(val);
    int save = PROTECT(v);
    AnfExp *result = invoke(k, v);
    UNPROTECT(save);
    return result;
}
```

Next let expressions:

```scheme
(`(let ((,x ,eb)) ,e-body)
    (normalize eb
        [λ (e-r)
            `(let ((,x ,e-r))
            ,(normalize e-body k))]))
```

The new continuation has free variables `x`, `e-body` and `k`.
However that requires the map to also deal with `LamExp` types, which might result in having to do unsafe pointer casts. We can instead rewrite the above a little, to move the `(normalize e-body k)` expression out of the continuation:

```scheme
(`(let ((,x ,eb)) ,e-body)
    (let ((neb (normalize e-body k)))
        (normalize eb
            [λ (e-r)
                `(let ((,x ,e-r)) ,neb)]))
```

Now the continuation only has free variables `x` and `neb`.

```C
static AnfExp *normalizeLetKont(AnfExp *letArg, AnfMap *map) {
    AnfExp *x = NULL;
    getAnfMap(map, TOK_X(), &x);
    int save = PROTECT(x);
    AnfExp *neb = NULL;
    getAnfMap(map, TOK_NEB(), &neb);
    AnfExp *anfLet = makAnfLet(x, letArg, neb); // more pseudocode
    UNPROTECT(save);
    return anfLet;
}

static AnfExp normalizeLet(LamLet *let, AnfKont *k) {
    AnfExp *neb = normalize(let->exp, k);
    int save = PROTECT(neb);
    AnfMap *map = newAnfMap();
    PROTECT(map);
    setAnfMap(map, TOK_X(), <convert>(let->var));
    setAnfMap(map, TOK_NEB(), neb);
    AnfKont *k2 = newAnfKont(normalizeLetKont, map);
    PROTECT(k2);
    AnfExp *result = normalize(let->val, k2);
    UNPROTECT(save);
    return result;
}
```

Next if-expressions:

```scheme
(`(if ,e0 ,e1 ,e2)
    (normalize-name e0
        [λ (e-r)
            (k `(if ,e-r
                    ,(normalize-term e1)
                    ,(normalize-term e2)))]))
```

The continuation has free variables `k`, `e1` and `e2`. Again we can avoid passing LamExp values in the map by normalizing `e1` and `e2` ahead of the continuation:

```scheme
(`(if ,e0 ,e1 ,e2)
    (let ((consequent (normalize-term e1))
          (alternative (normalize-term e2)))
         (normalize-name e0
            [λ (e-r)
                (k `(if ,e-r ,consequent ,alternative))])))
```

Now the continuation has free variables `k`, `consequent` and `alternative` which are all in the `Anf` domain.

```C
static AnfExp *normalizeIfKont(AnfAexp *cond, AnfMap *map) {
    AnfKont *k = NULL;
    getAnfMap(map, TOK_K(), &k);
    AnfExp *cons = NULL;
    getAnfMap(map, TOK_CONS(), &cons);
    AnfExp *alt = NULL;
    getAnfMap(map, TOK_ALT(), &alt);
    AnfCexpIf *iff = newAnfCexpIf(cond, cons, alt);
    PROTECT(iff);
    AnfExp *result = invoke(k, iff);
    UNPROTECT(save);
    return result;
}

static AnfExp *normalizeIff(LamIff *iff, AnfKont *k) {
    AnfExp cons = normalizeTerm(iff->consequent);
    int save = PROTECT(cons);
    AnfExp *alt = normalizeTerm(iff->alternative);
    PROTECT(alt);
    AnfMap *map = newAnfMap();
    PROTECT(map);
    setAnfMap(map, TOK_K(), k);
    setAnfMap(map, TOK_CONS(), cons);
    setAnfMap(map, TOK_ALT(), alt);
    AnfKont *k2 = newAnfKont(normalizeIfKont, map);
    PROTECT(k2);
    AnfExp *result = normalizeName(iff->condition, k2);
    UNPROTECT(save);
    return result;
}
```

Finally the function call branch:

```scheme
(`(,Fn . ,M*)
    (normalize-name Fn
        [λ (t)
            (normalize-name* M*
                [λ (t*) (k `(,t . ,t*))])]))
```

This is more interesting because there are two nested continuations.
However we can simply pass the map from one to the other so it's not too bad. Starting with the inner continuation, it has free variables `t` and `k`.

```C
static AnfExp *normalizeCallInnerKont(AnfExpr *ts, AnfMap *map) {
    AnfExpr *t = NULL;
    getAnfMap(map, TOK_T(), &t);
    AnfKont *k = NULL;
    getAnfMap(map, TOK_K(), &k);
    AnfExp *apply = newAnfApply(t, ts);
    int save = PROTECT(apply);
    AnfExp *result = invoke(k, apply);
    UNPROTECT(save);
    return result;
}

static AnfExp *normalizeCallOuterKont(AnfExpr *t, AnfMap *map) {
    AnfExpr *Ms = NULL;
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

#### normalize-names

```scheme
(define (normalize-name* M* k)
    (if (null? M*)
        (k '())
        (normalize-name (car M*) [λ (t)
            (normalize-name* (cdr M*) [λ (t*)
                (k `(,t . ,t*))])])))
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
    AnfExp *result = invoke(k, apply);
    UNPROTECT(save);
    return result;
}
```

The outer continuation uses a free variable `M*` but must also provide the value `t` to the free variables of the inner continuation.

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
        return invoke(k, NULL);
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