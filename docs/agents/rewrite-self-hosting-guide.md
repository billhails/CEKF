# Rewrite self-hosting guide for agents

This document explains the `fn/rewrite` area so agents can reason about it as a prototype pipeline for compiler stages.

## Why this folder matters

`fn/rewrite` is not just experimental code. It is a staging area where algorithms are prototyped in the language itself before or alongside C implementations.

Recent and relevant examples:

- β-reduction is implemented in `fn/rewrite/beta_reduce.fn`.
- η-reduction is implemented in `fn/rewrite/eta_reduce.fn`.
- Constant/operator folding is implemented in `fn/rewrite/constant_folding.fn`.
- Closure conversion variants are in `fn/rewrite/closure-convert.fn`.

Treat this directory as a design/prototyping reference when working on equivalent C stages.

## Primary entrypoint

Start with `fn/rewrite/test_harness.fn`.

It loads:

- `samples.fn` input corpus.
- Front-end representation and parser from `expr.fn`.
- Lower-level representation from `minexpr.fn`.
- Transform passes: `desugar`, `curry`, `eta_reduce`, `cps`, `beta_reduce`, `constant_folding`, `closure-convert`.

### Execution

From repo root:

```bash
./bin/fn fn/rewrite/test_harness.fn
```

The harness prints each sample and a transformed result. By default, intermediate pretty-prints are commented out in the file, but can be re-enabled for debugging pass-by-pass output.

## Data flow in the harness

For each non-comment entry in `samples.fn`:

1. Parse source string with `E.parse` (`expr` IR).
2. `DS.desugar` lowers to `minexpr` and removes/normalizes syntax forms.
3. `C.curry` enforces unary application/lambda shape.
4. `η.reduce` performs eta reduction.
5. `CPS.T_c(..., M.var("□"))` runs CPS conversion with a hole continuation.
6. `β.reduce` contracts lambda applications and handles arity mismatch cases.
7. `OF.fold` applies algebraic and constant simplification.
8. `CC.shared_closure_convert` computes shared closure-converted output.

Important current behavior: the harness computes step 8 but currently prints step 7 (`g`) by default.

## The sample corpus

`fn/rewrite/samples.fn` provides:

- General language constructs (`let`, `letrec`, lambdas, conditionals, `call/cc`, `amb`, constructors, vectors, namespaces, etc.).
- A large arithmetic/optimization section focused on constant-folding behavior.
- Several explicit η-reduction-focused samples near the end.
- Inline comments as strings beginning with `";"` that the harness prints as section labels.

Use this corpus first when validating behavioral changes in rewrite passes.

## Representation split to keep in mind

- `expr.fn` is intended to broadly mirror `src/lambda.yaml` (LamExp-like shapes in the rewrite prototype).
- `desugar.fn` lowers from `expr.fn` to `minexpr.fn`.
- `minexpr.fn` is intended to broadly mirror `src/minlam.yaml` (MinExp-like reduced forms).
- All subsequent transforms in the main rewrite pipeline operate on `minexpr.fn` values.

This mirrors the C compiler flow where desugaring lowers `LamExp` to `MinExp` before later optimization/normalization stages.

Many transform bugs come from accidentally assuming an `expr`/LamExp-like form still exists after desugaring, or from printing/parsing assumptions between these two representations.

## Key transform files and purpose

- `desugar.fn`: lowers `expr` to `minexpr`, rewrites `let*`, transforms some constructs to primitive forms, and handles partial/over-application shaping via arity context.
- `curry.fn`: rewrites lambdas and applications into curried form.
- `eta_reduce.fn`: removes `λx.(f x)`-style wrappers when safe (`occurs_in` guard).
- `cps.fn`: CPS transform (`T_k`, `T_c`, and list helper `Ts_k`).
- `beta_reduce.fn`: β-reduction with explicit handling for exact/under/over application.
- `constant_folding.fn`: algebraic simplifier plus recursive fold over `minexpr`.
- `closure-convert.fn`: closure conversion with `flat_closure_convert` (bottom-up) and `shared_closure_convert` (top-down) using `transform.fn` traversal helpers.

## Relationship to the C compiler pipeline

Use this rewrite pipeline as a conceptual mirror for C stages, not as an exact 1:1 implementation.

IR correspondence first:

- `fn/rewrite/expr.fn` ≈ `src/lambda.yaml`
- `fn/rewrite/minexpr.fn` ≈ `src/minlam.yaml`
- `fn/rewrite/desugar.fn` performs the same broad lowering role as `src/lambda_desugar.c` (`LamExp`-like to `MinExp`-like)

Useful rough correspondences:

- `cps.fn` ↔ CPS/continuation strategy used in `src/lambda_cpsTk.c` and `src/lambda_cpsTc.c`
- `beta_reduce.fn` / `eta_reduce.fn` ↔ min-lambda simplification ideas (`src/minlam_*.c`)
- `constant_folding.fn` ↔ arithmetic simplification ideas (see `src/arithmetic.c` and related optimization logic)
- `closure-convert.fn` ↔ closure conversion concepts in lambda conversion/runtime lowering

When adding or changing a C-stage algorithm, check whether `fn/rewrite` already has a compact version that clarifies intent or edge-case behavior.

## Agent workflow recommendations

When asked to modify a rewrite pass:

1. Start from `test_harness.fn` and identify where in the sequence the pass runs.
2. Add or adjust cases in `samples.fn` close to the affected feature area.
3. Toggle intermediate prints in `test_harness.fn` to isolate first divergent stage.
4. Keep changes constrained to one IR layer (`expr` or `minexpr`) per edit when possible.
5. If a rewrite behavior is intended to inform C code, document the invariant in both places.

This keeps prototype and production-stage behavior aligned as self-hosting efforts expand.
