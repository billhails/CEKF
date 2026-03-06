# Trampoline Infrastructure Proposal

## Problem

Recursive compiler passes (CPS transform, ANF normalization, etc.) overflow the C stack on moderately large programs. The CPS transform processes `samples.fn` (90 strings × ~30 chars each) creating 2700+ nested frames.

Building with `-O2` masks the issue via tail-call optimization, but this is fragile.

## Solution: Type-Safe Trampolined Execution

Convert recursive functions to return "work items" instead of calling directly. A loop processes work items until done.

Scope will be limited to `minlam_cps*` source files and the `tools/cps_continuations.yaml`.

This implementation requres that all existing functions and konts return a new
`CpsWork` type defined below, which is then processed by the trampoline.

### Yaml config

```yaml
# in cps_continuations.yaml
structs:
    CpsTkThunk:
        meta:
            brief: thunk that invokes cpsTk
        data:
            exp: MinExp
            kont: CpsKont

    # ...

    CpsInvokeThunk:
        meta:
            brief: thunk that invokes a kont
        data:
            exp: MinExp
            kont: CpsKont

unions:
    CpsWork:
        meta:
            brief: a thunk or a result
        data:
            tk: CpsTkThunk
            tc: CpsTcThunk
            tsk: CpsTskThunk
            # ...
            invoke: CpsInvokeThunk
            result: MinExp
```

### Specific CPS Trampoline (hand-written)

```c
static MinExp *trampoline(CpsWork *work) {
    int save = PROTECT(work);
    while (true) {
        switch (work->type) {
            case CPSWORK_TYPE_RESULT:
                UNPROTECT(save);
                return getCpsWork_Result(work);
            case CPSWORK_TYPE_TK:
                work = cpsTk(getCpsWork_Tk(work)->exp,
                             getCpsWork_Tk(work)->kont);
                break;
            // ...
            case CPSWORK_TYPE_INVOKE:
                work = INVOKE(getCpsWork_Invoke(work)->kont,
                              getCpsWork_Invoke(work)->exp);
                break;
        }
        REPLACE_PROTECT(save, work);
    }
}
```

### Example Rewrites

```c
static CpsWork *INVOKE(CpsKont *k, MinExp *arg) {
    return k->wrapper(arg, k->env);
}

CpsWork *cpsTs_k(MinExp *exp, CpsKont *k) {
    if (getMinExp_Args(exp) == NULL) {
        return makeCpsWork_Invoke(k, exp);
    }
    CpsKont *k1 = makeKont_TkS1(getMinExp_Args(exp)->next, k);
    int save = PROTECT(k1);
    CpsWork *result = makeCpsWork_Tk(getMinExp_Args(exp)->exp, k1);
    UNPROTECT(save);
    return result;
}

CpsWork *TkS1Kont(MinExp *hd, TkS1KontEnv *env) {
    CpsKont *k2 = makeKont_TkS2(env->k, hd);
    int save = PROTECT(k2);
    MinExp *args = newMinExp_Args(CPI(hd), env->t);
    PROTECT(args);
    CpsWork *result = makeCpsWork_Tsk(args, k2);
    UNPROTECT(save);
    return result;
}

CpsWork *TkS2Kont(MinExp *tl, TkS2KontEnv *env) {
    MinExp *args = makeMinExp_Args(CPI(env->hd), env->hd, getMinExp_Args(tl));
    int save = PROTECT(args);
    CpsWork *result = makeCpsWork_Invoke(env->k, args);
    UNPROTECT(save);
    return result;
}
```

## Non-tail Call Sites Need Extra Work Variants

Some kont functions currently call `cpsTc`/`cpsTk` and then do additional work, so the call is not in tail position. Those sites cannot be converted by just changing return types; they need an extra split phase represented as additional `CpsWork` variants.

Example pattern:

```c
// current shape (non-tail)
MinExp *consequent = cpsTc(env->exprt, env->c);
MinExp *alternative = cpsTc(env->exprf, env->c);
return makeMinExp_Iff(CPI(aexp), aexp, consequent, alternative);
```

Trampolined shape (split):

1. Schedule work for first sub-call.
2. Resume in a new work variant carrying partial state.
3. Schedule work for second sub-call.
4. Resume in a second work variant to build final node.

Minimum known sites to split:

- `TkIffKont` in `minlam_cpsTk.c` (two `cpsTc` calls then `makeMinExp_Iff`).
- `TcIffKont` in `minlam_cpsTc.c` (same pattern).
- `cpsTkMinAmb` in `minlam_cpsTk.c` (two `cpsTc` calls then `makeMinExp_Amb`).
- `cpsTcMinAmb` in `minlam_cpsTc.c` (same pattern).
- `TkCondKont` / `TcCondKont` map helpers where recursive body transforms are followed by node reconstruction.
- `TkMatchKont` / `TcMatchKont` map helpers with the same map-then-rebuild pattern.

Rule of thumb: every statement of the form `tmp = cpsT*(...);` followed by more computation in the same function requires a split into at least one additional work variant.

## Termination Contract (HALT Root Continuation)

Use a distinguished CPS continuation variant `HALT` as the root continuation for the transform.

- The transform entrypoint creates `halt` and starts with `cpsTc(rootExp, halt)`.
- `INVOKE` dispatches all continuations normally except `HALT`.
- `INVOKE(HALT, value)` must return `newCpsWork_Result(value)`.
- No step is allowed to return `NULL`; each step returns exactly one `CpsWork` value.

This gives a precise loop rule:

- Continue while work is any task variant (`TK`, `TC`, `TSK`, `INVOKE`, ...).
- Stop only when work is `CPSWORK_TYPE_RESULT`.

This keeps completion criteria at the meta-transform layer, independent of object-language `Done` nodes.

```c
// Pseudocode only
static CpsWork *INVOKE(CpsKont *k, MinExp *arg) {
    if (k->type == CPSKONT_TYPE_HALT) {
        return newCpsWork_Result(arg);
    }
    return k->wrapper(arg, k->env);
}

static MinExp *trampoline(CpsWork *work) {
    int save = PROTECT(work);
    while (true) {
        switch (work->type) {
            case CPSWORK_TYPE_RESULT:
                UNPROTECT(save);
                return getCpsWork_Result(work);

            case CPSWORK_TYPE_TK:
                work = cpsTk(getCpsWork_Tk(work)->exp,
                             getCpsWork_Tk(work)->kont);
                break;

            // same pattern for TC, TSK, and any other task variants

            case CPSWORK_TYPE_INVOKE:
                work = INVOKE(getCpsWork_Invoke(work)->kont,
                              getCpsWork_Invoke(work)->exp);
                break;
        }
        REPLACE_PROTECT(save, work);
    }
}
```
