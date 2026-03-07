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

### Concrete Tag Taxonomy (single shared trampoline)

Use one `CpsWork` union that includes both Tc and Tk operations.

Core task tags:

- `tcRoot`: run root `cpsTc(exp, cont0)` with terminal semantics
- `tk`: run `cpsTk(exp, kont)`
- `tc`: run `cpsTc(exp, cont)`
- `tsk`: run `cpsTs_k(args, kont)`
- `invoke`: run `INVOKE(kont, arg)`
- `result`: terminal transformed expression

Resume tags for non-tail splits (reviewed against `minlam_cpsTc.c` + `minlam_cpsTk.c`):

- Iff
  - `tkIffAfterCondition`
  - `tkIffAfterConsequent`
  - `tkIffBuild`
  - `tcIffAfterCondition`
  - `tcIffAfterThen`
  - `tcIffBuild`
- Amb
  - `tkAmbAfterLeft`
  - `tkAmbBuild`
  - `tcAmbAfterLeft`
  - `tcAmbBuild`
- LetRec / Lambda-body completion
  - `tkLetRecAfterBody`
  - `tcLetRecAfterBody`
  - `mLamAfterBody`
- Cond case mapping
  - `tkMapIntCondCases`
  - `tkMapIntCondCasesAfterBody`
  - `tkMapCharCondCases`
  - `tkMapCharCondCasesAfterBody`
  - `tcMapIntCondCases`
  - `tcMapIntCondCasesAfterBody`
  - `tcMapCharCondCases`
  - `tcMapCharCondCasesAfterBody`
- Match case mapping
  - `tkMapMatchCases`
  - `tkMapMatchCasesAfterBody`
  - `tcMapMatchCases`
  - `tcMapMatchCasesAfterBody`
- Binding mapping helpers
  - `cpsMapBindings`
  - `cpsMapBindingsAfterVal`

These are the minimum practical families to avoid leaving hidden recursive islands in helper paths.

Naming rule for future additions:

- Use camelCase tags with `tk`/`tc` prefix (for example `tkMatchAfterIndex`).
- For phase-neutral helper passes, use `cps` prefix (for example `cpsMapBindingsAfterVal`).
- Keep each tag single-purpose: it should represent one suspended point in one function.

This keeps dispatch explicit and lets Tc↔Tk handovers remain transitions in one loop.

### Full YAML Example (`CpsThunk` + `CpsWork`)

```yaml
# Example declaration set for tools/cps_continuations.yaml

structs:
    # Core entry/task thunks
    CpsTcRootThunk:
        meta:
            brief: Root Tc entrypoint task
        data:
            exp: MinExp
            cont0: MinExp

    CpsTkThunk:
        meta:
            brief: Task that invokes cpsTk
        data:
            exp: MinExp
            kont: CpsKont

    CpsTcThunk:
        meta:
            brief: Task that invokes cpsTc
        data:
            exp: MinExp
            cont: MinExp

    CpsTskThunk:
        meta:
            brief: Task that invokes cpsTs_k
        data:
            args: MinExp
            kont: CpsKont

    CpsInvokeThunk:
        meta:
            brief: Task that invokes a kont
        data:
            kont: CpsKont
            arg: MinExp

    # Iff split thunks
    TkIffAfterConditionThunk:
        meta:
            brief: TkIff after condition, schedule consequent
        data:
            aexp: MinExp
            exprt: MinExp
            exprf: MinExp
            c: MinExp

    TkIffAfterConsequentThunk:
        meta:
            brief: TkIff after consequent, schedule alternative
        data:
            aexp: MinExp
            exprf: MinExp
            c: MinExp
            consequent: MinExp

    TkIffBuildThunk:
        meta:
            brief: TkIff build final if-expression
        data:
            aexp: MinExp
            consequent: MinExp

    TcIffAfterConditionThunk:
        meta:
            brief: TcIff after condition, schedule then branch
        data:
            aexp: MinExp
            exprt: MinExp
            exprf: MinExp
            sk: MinExp

    TcIffAfterThenThunk:
        meta:
            brief: TcIff after then, schedule else branch
        data:
            aexp: MinExp
            exprf: MinExp
            sk: MinExp
            thenExp: MinExp

    TcIffBuildThunk:
        meta:
            brief: TcIff build final if-expression
        data:
            aexp: MinExp
            thenExp: MinExp

    # Amb split thunks
    TkAmbAfterLeftThunk:
        meta:
            brief: TkAmb after left, schedule right
        data:
            left: MinExp
            right: MinExp
            c: MinExp

    TkAmbBuildThunk:
        meta:
            brief: TkAmb build amb expression
        data:
            leftVal: MinExp

    TcAmbAfterLeftThunk:
        meta:
            brief: TcAmb after left, schedule right
        data:
            left: MinExp
            right: MinExp
            k: MinExp

    TcAmbBuildThunk:
        meta:
            brief: TcAmb build amb expression
        data:
            k: MinExp
            leftVal: MinExp

    # LetRec/lambda body completion thunks
    TkLetRecAfterBodyThunk:
        meta:
            brief: Tk letrec body complete, build letrec
        data:
            bindings: MinBindings

    TcLetRecAfterBodyThunk:
        meta:
            brief: Tc letrec body complete, build letrec
        data:
            bindings: MinBindings

    MLamAfterBodyThunk:
        meta:
            brief: cpsM lambda body complete, build lambda
        data:
            args: SymbolList

    # Cond case mapping thunks
    TkMapIntCondCasesThunk:
        meta:
            brief: Map Tk int cond cases
        data:
            cases: MinIntCondCases
            c: MinExp

    TkMapIntCondCasesAfterBodyThunk:
        meta:
            brief: Build Tk int cond case node after body
        data:
            constant: MaybeBigInt
            next: MinIntCondCases

    TkMapCharCondCasesThunk:
        meta:
            brief: Map Tk char cond cases
        data:
            cases: MinCharCondCases
            c: MinExp

    TkMapCharCondCasesAfterBodyThunk:
        meta:
            brief: Build Tk char cond case node after body
        data:
            constant: character
            next: MinCharCondCases

    TcMapIntCondCasesThunk:
        meta:
            brief: Map Tc int cond cases
        data:
            cases: MinIntCondCases
            sk: MinExp

    TcMapIntCondCasesAfterBodyThunk:
        meta:
            brief: Build Tc int cond case node after body
        data:
            constant: MaybeBigInt
            next: MinIntCondCases

    TcMapCharCondCasesThunk:
        meta:
            brief: Map Tc char cond cases
        data:
            cases: MinCharCondCases
            sk: MinExp

    TcMapCharCondCasesAfterBodyThunk:
        meta:
            brief: Build Tc char cond case node after body
        data:
            constant: character
            next: MinCharCondCases

    # Match case mapping thunks
    TkMapMatchCasesThunk:
        meta:
            brief: Map Tk match cases
        data:
            cases: MinMatchList
            c: MinExp

    TkMapMatchCasesAfterBodyThunk:
        meta:
            brief: Build Tk match case node after body
        data:
            matches: MinIntList
            next: MinMatchList

    TcMapMatchCasesThunk:
        meta:
            brief: Map Tc match cases
        data:
            cases: MinMatchList
            sk: MinExp

    TcMapMatchCasesAfterBodyThunk:
        meta:
            brief: Build Tc match case node after body
        data:
            matches: MinIntList
            next: MinMatchList

    # Binding map helper thunks
    CpsMapBindingsThunk:
        meta:
            brief: Map cpsM over letrec bindings
        data:
            bindings: MinBindings

    CpsMapBindingsAfterValThunk:
        meta:
            brief: Build mapped binding after transformed value
        data:
            var: HashSymbol
            arity: int
            next: MinBindings

unions:
    CpsWork:
        meta:
            brief: CPS trampoline work item or final result
        data:
            tcRoot: CpsTcRootThunk
            tk: CpsTkThunk
            tc: CpsTcThunk
            tsk: CpsTskThunk
            invoke: CpsInvokeThunk

            tkIffAfterCondition: TkIffAfterConditionThunk
            tkIffAfterConsequent: TkIffAfterConsequentThunk
            tkIffBuild: TkIffBuildThunk
            tcIffAfterCondition: TcIffAfterConditionThunk
            tcIffAfterThen: TcIffAfterThenThunk
            tcIffBuild: TcIffBuildThunk

            tkAmbAfterLeft: TkAmbAfterLeftThunk
            tkAmbBuild: TkAmbBuildThunk
            tcAmbAfterLeft: TcAmbAfterLeftThunk
            tcAmbBuild: TcAmbBuildThunk

            tkLetRecAfterBody: TkLetRecAfterBodyThunk
            tcLetRecAfterBody: TcLetRecAfterBodyThunk
            mLamAfterBody: MLamAfterBodyThunk

            tkMapIntCondCases: TkMapIntCondCasesThunk
            tkMapIntCondCasesAfterBody: TkMapIntCondCasesAfterBodyThunk
            tkMapCharCondCases: TkMapCharCondCasesThunk
            tkMapCharCondCasesAfterBody: TkMapCharCondCasesAfterBodyThunk
            tcMapIntCondCases: TcMapIntCondCasesThunk
            tcMapIntCondCasesAfterBody: TcMapIntCondCasesAfterBodyThunk
            tcMapCharCondCases: TcMapCharCondCasesThunk
            tcMapCharCondCasesAfterBody: TcMapCharCondCasesAfterBodyThunk

            tkMapMatchCases: TkMapMatchCasesThunk
            tkMapMatchCasesAfterBody: TkMapMatchCasesAfterBodyThunk
            tcMapMatchCases: TcMapMatchCasesThunk
            tcMapMatchCasesAfterBody: TcMapMatchCasesAfterBodyThunk

            cpsMapBindings: CpsMapBindingsThunk
            cpsMapBindingsAfterVal: CpsMapBindingsAfterValThunk

            result: MinExp
```

This block is intentionally explicit so each non-tail or recursive helper path has a corresponding resumable work shape.

### Generator rollout with `input`/`output`

To avoid a long broken build window, stage the migration:

1. Add/keep `input: MinExp` and `output: MinExp` while introducing `CpsWork` tags and constructors.
2. Convert call sites and signatures (`cpsTk`, `cpsTc`, `cpsTs_k`, kont wrappers) to `CpsWork`.
3. Flip `output: CpsWork` once all generated wrappers and callers are migrated.

`currency` can be left as legacy/compat during transition, but `input`/`output` should be treated as authoritative.

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

## Architecture Decision: One Trampoline, Two Modules

The CPS transform remains split by concern:

- `minlam_cpsTc.c` handles MinExp continuations.
- `minlam_cpsTk.c` handles CpsKont konts.

Do not merge these files. Instead, use a single shared `CpsWork` loop that schedules tasks for both halves.

### Why

- Two independent trampolines risk reintroducing stack growth at C/K handovers.
- Trampolining only one half leaves the other half recursive.
- File merge adds churn without solving a technical requirement.

### Execution Model

- `CpsWork` contains task variants for both Tc and Tk operations.
- Each step returns the next `CpsWork` item rather than making a C call.
- Tc↔Tk handovers become variant transitions in the same loop.
- Termination is still only through `INVOKE(HALT, value) -> RESULT(value)`.

### Module Boundaries

- Keep case handlers `static` inside each file where possible.
- Add one exported step-dispatch function per module if needed.
- Shared trampoline dispatches by `CpsWork` tag to Tc/Tk handlers.

### Dispatcher + Trampoline Code Sketches

The following sketches show one-loop orchestration without merging files.

```c
// src/minlam_cpsTk.c
// exported
CpsWork *cpsStepTk(CpsWork *work) {
    switch (work->type) {
        case CPSWORK_TYPE_TK:
            return cpsTk(getCpsWork_Tk(work)->exp,
                         getCpsWork_Tk(work)->kont);

        case CPSWORK_TYPE_TSK:
            return cpsTs_k(getCpsWork_Tsk(work)->exp,
                           getCpsWork_Tsk(work)->kont);

        case CPSWORK_TYPE_TK_IFF_AFTER_CONDITION:
            return tkIffAfterConditionStep(getCpsWork_TkIffAfterCondition(work));

        case CPSWORK_TYPE_TK_IFF_AFTER_CONSEQUENT:
            return tkIffAfterConsequentStep(getCpsWork_TkIffAfterConsequent(work));

        case CPSWORK_TYPE_TK_AMB_AFTER_LEFT:
            return tkAmbAfterLeftStep(getCpsWork_TkAmbAfterLeft(work));

        default:
            cant_happen("cpsStepTk got non-Tk tag %s", cPsWorkTypeName(work->type));
    }
}
```

```c
// src/minlam_cpsTc.c
// exported
CpsWork *cpsStepTc(CpsWork *work) {
    switch (work->type) {
        case CPSWORK_TYPE_TC:
            return cpsTc(getCpsWork_Tc(work)->exp,
                         getCpsWork_Tc(work)->cont);

        case CPSWORK_TYPE_TC_IFF_AFTER_CONDITION:
            return tcIffAfterConditionStep(getCpsWork_TcIffAfterCondition(work));

        case CPSWORK_TYPE_TC_IFF_AFTER_THEN:
            return tcIffAfterThenStep(getCpsWork_TcIffAfterThen(work));

        case CPSWORK_TYPE_TC_AMB_AFTER_LEFT:
            return tcAmbAfterLeftStep(getCpsWork_TcAmbAfterLeft(work));

        default:
            cant_happen("cpsStepTc got non-Tc tag %s", cPsWorkTypeName(work->type));
    }
}
```

```c
// src/minlam_cps_trampoline.c
static CpsWork *INVOKE(CpsKont *k, MinExp *arg) {
    if (k->type == CPSKONT_TYPE_HALT) {
        return newCpsWork_Result(arg);
    }
    return k->wrapper(arg, k->env);
}

static CpsWork *cpsStep(CpsWork *work) {
    switch (work->type) {
        case CPSWORK_TYPE_INVOKE:
            return INVOKE(getCpsWork_Invoke(work)->kont,
                          getCpsWork_Invoke(work)->exp);

        case CPSWORK_TYPE_RESULT:
            return work;

        // Tk-owned tags
        case CPSWORK_TYPE_TK:
        case CPSWORK_TYPE_TSK:
        case CPSWORK_TYPE_TK_IFF_AFTER_CONDITION:
        case CPSWORK_TYPE_TK_IFF_AFTER_CONSEQUENT:
        case CPSWORK_TYPE_TK_AMB_AFTER_LEFT:
            return cpsStepTk(work);

        // Tc-owned tags
        case CPSWORK_TYPE_TC:
        case CPSWORK_TYPE_TC_IFF_AFTER_CONDITION:
        case CPSWORK_TYPE_TC_IFF_AFTER_THEN:
        case CPSWORK_TYPE_TC_AMB_AFTER_LEFT:
            return cpsStepTc(work);

        default:
            cant_happen("unhandled CpsWork tag %s", cPsWorkTypeName(work->type));
    }
}

MinExp *runCpsTrampolineTc(MinExp *rootExp, MinExp *cont0) {
    CpsWork *work = makeCpsWork_TcRoot(rootExp, cont0); // explicit root marker
    int save = PROTECT(work);

    while (work->type != CPSWORK_TYPE_RESULT) {
        work = cpsStep(work);
        REPLACE_PROTECT(save, work);
    }

    MinExp *result = getCpsWork_Result(work);
    UNPROTECT(save);
    return result;
}
```

This mirrors the current `main.c` call shape (`cpsTc(rootExp, doneCont)`), where `cont0` is a target-language continuation (`MinExp`).
`cont0` itself is not used as a termination marker; root/final status is carried by the `tcRoot` work tag.

```c
// non-tail split example (TkIffKont)
// step 1: after condition, schedule consequent
static CpsWork *tkIffAfterConditionStep(TkIffAfterConditionThunk *t) {
    CpsWork *resume = makeCpsWork_TkIffAfterConsequent(t->aexp, t->exprf, t->c);
    int save = PROTECT(resume);
    CpsWork *work = makeCpsWork_TcWithResume(t->exprt, t->c, resume);
    UNPROTECT(save);
    return work;
}

// step 2: after consequent, schedule alternative
static CpsWork *tkIffAfterConsequentStep(TkIffAfterConsequentThunk *t) {
    CpsWork *resume = makeCpsWork_TkIffBuild(t->aexp, t->consequent);
    int save = PROTECT(resume);
    CpsWork *work = makeCpsWork_TcWithResume(t->exprf, t->c, resume);
    UNPROTECT(save);
    return work;
}

// step 3: build final node
static CpsWork *tkIffBuildStep(TkIffBuildThunk *t, MinExp *alternative) {
    MinExp *iff = makeMinExp_Iff(CPI(t->aexp), t->aexp, t->consequent, alternative);
    int save = PROTECT(iff);
    CpsWork *work = makeCpsWork_Invoke(t->k, iff);
    UNPROTECT(save);
    return work;
}
```

The exact field names and helper constructor names can follow generator output, but the control-flow split above is the target shape.

### Migration Order

1. Introduce shared work tags spanning both Tc and Tk operations.
2. Convert direct Tc↔Tk calls to scheduled work transitions.
3. Split non-tail sites into additional resume variants.
4. Convert remaining recursive calls.
5. Validate with `fn/rewrite/test_harness.fn` in debug mode.

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

Minimum known sites to split (from direct review of both files):

- `TkIffKont` / `TcIffKont` (sequential `cpsTc` calls, then `makeMinExp_Iff`).
- `cpsTkMinAmb` / `cpsTcMinAmb` (sequential `cpsTc` calls, then `makeMinExp_Amb`).
- `cpsTkMinLetRec` / `cpsTcMinLetRec` (transform body, then construct `LetRec`).
- `cpsM` lambda case in `minlam_cpsTc.c` (transform body, then construct lambda).
- `mapIntCondCases` and `mapCharCondCases` in both files (recursive map + transformed body + list reconstruction).
- `mapTcOverMatchCases` (recursive map + transformed body + list reconstruction).
- `mapMOverBindings` (recursive map + transformed value + list reconstruction).

If any of these helpers remain recursive while top-level `cpsTk`/`cpsTc` are trampolined, stack pressure can persist in large case/binding lists.

Rule of thumb: every statement of the form `tmp = cpsT*(...);` followed by more computation in the same function requires a split into at least one additional work variant.

## Termination Contract

Primary entry mode (matches `main.c`):

- Entrypoint starts with `makeCpsWork_TcRoot(rootExp, cont0)`.
- `cont0` is a target-language continuation (`MinExp`), typically the same done continuation currently passed to `cpsTc`.
- Internal `tc` tasks are not terminal.
- When the `tcRoot` computation completes, it must produce `newCpsWork_Result(transformedExp)`.

Optional Tk-root/testing mode:

- Use a distinguished kont sentinel `HALT`.
- `INVOKE(HALT, value)` must return `newCpsWork_Result(value)`.

In all modes:

- No step may return `NULL`; each step returns exactly one `CpsWork` value.

### Root/final marker rule

Use a dedicated root tag (`tcRoot`) or equivalent meta-marker. Do not infer finality from `cont0` pointer identity.

- `cont0` belongs to target-language semantics.
- Finality belongs to trampoline/meta semantics.
- Only root-marked work or `INVOKE(HALT, ...)` may emit `RESULT`.

Tag names are camelCase (`tcRoot`, `tkIffAfterCondition`, ...). Generated C enum constants may still be uppercase per generator conventions.

This gives a precise loop rule:

- Continue while work is any task variant (`TK`, `TC`, `TSK`, `INVOKE`, ...).
- Stop only when work is `CPSWORK_TYPE_RESULT`.

This keeps completion criteria at the meta-transform layer, while preserving the existing `cpsTc(..., MinExp continuation)` startup shape.

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
    while (work->type != CPSWORK_TYPE_RESULT) {
        work = cpsStep(work); // delegates to cpsStepTk/cpsStepTc/INVOKE routing
        REPLACE_PROTECT(save, work);
    }
    MinExp *result = getCpsWork_Result(work);
    UNPROTECT(save);
    return result;
}
```
