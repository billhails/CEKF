# Cannot import non-lambdas from a namespace

This bug was fixed.

The test file `tests/fn/test_import_data.fn` and the supporting `tests/fn/import_data.fn` demonstrate
the problem.

If `import_data.fn` is written as

```fn
namespace
    fn data() { 1 }
```

and `test_import_data.fn` as

```fn
let
    link "import_data.fn" as Data;
in
    Data.data()
```

Then the file compiles and runs with no problems.

If instead `import_data.fn` is written as:

```fn
namespace
    data = 1
```

and `test_import_data.fn` as

```fn
let
    link "import_data.fn" as Data;
in
    Data.data
```

Then there is an error:

```text
no binding for var 'data$0' in annotateAexpVar [tests/fn/test_import_data.fn +4] at +109 src/annotate.c
```

(`data$0` is due to a prior alpha-conversion step, the original variable is just `data`)

The primary difference is in how namespaces treat functions as opposed to data. Functions are hoisted into a `letrec`, while plain data is pushed down into a `let*`, then a later simplification step rewrites `let*` to a nest of `let` then further rewrites `let` to an anonymous lambda application.

If I run the test with `--dump-alpha` to dump the IR after alpha conversion, the relevant section for the plain `data = 1` version looks like this:

```scheme
(begin (nameSpaces [((λ (data$0) env) 1)]) (lookUp 0 data$0))
```

While the version with `fn data() { 1 }` looks like this:

```scheme
(begin (nameSpaces [(letrec ((data$0 (λ () 1))) env)]) ((lookUp 0 data$0)))
```

I should also explain what that `env` is. It's a special token representing the "body" of the namespace, injected by the parser, which tells downstream processing steps that they have reached the heart of the namespace and the result is effectively the environment current at that point.

I need to stress that **this used to work** and recent changes have broken it.
`annotate.c` is the step that gives each variable a "De Bruijn index".

## Investigation Findings

### The commit that likely introduced this bug

Commit `dd3dd1a` ("separate let-star construct") changed `lambda_conversion.c` to use
`makeLamExp_LetStar` instead of `makeLamExp_Let` for non-lambda bindings in namespaces:

```diff
-        letRecBody = makeLamExp_Let(CPI(varDefsList), varDefsList, letRecBody);
+        letRecBody = makeLamExp_LetStar(CPI(varDefsList), varDefsList, letRecBody);
```

### Root cause analysis

The problem occurs in `annotate.c` in the function `annotateAexpNameSpaceArray`.

**Working case (`letrec`)**: When the namespace body is `(letrec ((data$0 (λ () 1))) env)`:

1. `annotateAexpNameSpaceArray` creates `env2` with `isNameSpace = true`
2. `annotateCexpLetRec` creates a child environment, adds `data$0` to it
3. The `env` token is reached, `annotateExpEnv` returns the environment chain including `data$0`
4. This environment (with `data$0`) is pushed to `nsEnvs`
5. Later lookup succeeds because `data$0` is in the recorded namespace environment

**Broken case (lambda application)**: When the namespace body is `((λ (data$0) env) 1)`:

1. `annotateAexpNameSpaceArray` creates `env2` with `isNameSpace = true`
2. The body is a `CexpApply`, so `annotateCexpApply` is called
3. `annotateAexp(x->function, env)` annotates the lambda:
   - `annotateAexpLam` creates a child environment, adds `data$0` to it
   - The `env` token inside the lambda triggers `annotateExpEnv`, which returns this environment
   - **But this return value is discarded** - `annotateCexpApply` ignores it
4. `annotateCexpApply` returns `env2` (the original, with **no bindings**)
5. `env2` (empty) is pushed to `nsEnvs`
6. Later, `lookUp 0 data$0` fails because `data$0` is not in `nsEnvs[0]`

### The specific code issue

In [annotate.c](../../src/annotate.c#L141-L152):

```c
static AnfEnv *annotateCexpApply(CexpApply *x, AnfEnv *env) {
    annotateAexp(x->function, env);  // Return value ignored!
    annotateAexpList(x->args, env);
    return env;  // Returns original env, not the one from the lambda
}
```

The return value from `annotateAexp(x->function, ...)` is ignored. When the function is a
lambda containing the `env` token, the environment captured inside that lambda (which includes
the lambda's parameters) is lost.

### Why this worked before

Before the `let*` separation, non-lambda bindings in namespaces were likely processed
differently - possibly they stayed in the `letrec` or were handled in a way that didn't
rely on lambda application semantics for environment capture.

### Potential fixes (not implemented)

1. **Special-case lambda applications in namespaces**: Detect when a `CexpApply` has a lambda
   as its function and the lambda body contains the `env` token, then capture that environment.

2. **Don't desugar `let*` to lambda applications for namespaces**: Keep a `Let` construct in
   ANF that preserves the binding semantics.

3. **Change how namespace environments are captured**: Instead of relying on the return value
   from `annotateExp`, explicitly traverse the namespace body to find all bindings.
