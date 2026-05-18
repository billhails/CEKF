# Re-instating the `cut` directive

Note on continuations: old docs say "fail" instead of "back", "fail" and "back" continuations are the same thing but I want to move towards using "back" for these continuations in future.

## What is it?

`cut` is a unary special form `cut expr`, somewhat equivalent to the Prolog
"green cut" `!` operation. It commits to the current decision branch during
backtracking. If backtracked through, no additional choices at that
particular decision point will be attempted. The expression has the same type
and value as `expr`, but its control effect happens before `expr` is
evaluated.

## How it works

Conceptually `cut` prunes the current "back" continuation, so that
backtracking proceeds immediately to the previous "back" continuation,
skipping the one that was cut. Because it is a special form, this pruning
happens before evaluation of the argument expression.

## What already exists

There was an end-to-end implementation of `cut` on the CEKF path but this was somehow dropped. There is still unused code in the ANF transform `anf_normalize.c`, `anf.yaml` etc. and the CEKF machine `step.c`, `cekfs.yaml` etc. but it is missing from the AST, lambda conversion, type-checking and `minlam.yaml`.

## How is it implemented?

On the default CEKF path, `cut expr` lowers to the existing ANF form `(cut
expr)`. The `CUT` instruction merely replaces the back continuation register
with the parent: `F->F`, then execution continues with `expr`. The emitted
bytecode performs `CUT` before the code for `expr`, so the new back
continuation is already installed while `expr` is being evaluated.

If `cut` is executed when the current back continuation is `NULL`, that is a
run-time error.

Hopefully the CEKF path still works.

On the CPS path, each `back` continuation should take a boolean `skip`
argument. If `skip = true`, it should immediately call its parent `back`
continuation; otherwise it should perform its normal work. `cut` should then
install a new failure continuation that will call its parent with
`skip = true`, and it should do that before evaluating the argument
expression. All ordinary `back` calls should pass `skip = false`.

If the final back continuation is invoked with `skip = true`, that is a
run-time error.

## Resolved design decisions

- Keep the surviving expression-wrapper shape `(cut <expr>)` through the
  front-end IRs rather than treating `cut` as a bare directive.
- Treat `cut` as a special form rather than an ordinary function application:
  it installs its back-continuation effect before evaluating its argument.
- Give `cut expr` the type of `expr`.
- Treat `cut` with no current back continuation as a CEKF run-time error.
- Treat invocation of the final CPS back continuation with `skip = true` as a
  run-time error.
- Use the pass order in `main()` as the source of truth for the pipeline
  split, not the README architecture sketch.
- Treat the work as a shared path from parsing through uncurrying, followed
  by separate default-CEKF and target-b/target-c tails.
- Expand the front-end work list to include the full parser and AST surface,
  not just `ast.yaml`.
- The target-b/target-c tail in `main()` is not just `minlam_amb`; after the
  split it runs `runCpsTrampolineTc`, a repeated `betaEtaFixedPoint`,
  `ambMinExp`, a repeated `shakeMinExp`, the inline/beta/eta/shake/fold fixed
  point, `checkMinExp`, closure conversion, `indexMinExp`, and finally target
  emission.
- Some of those post-split minExp passes are repeats of shared-path passes,
  while a few are unique to the target-b/target-c branch and may need small
  additions or explicit validation.
- The CPS transform itself is the tricky branch-specific part.
- For the CPS path, the change should stay in `minlam_amb` unless later
  validation shows another pass is making a hidden assumption. The current
  target-b/target-c closure conversion and emitters count lambda arguments
  generically and do not appear to hard-code failure-continuation arity.

## Work to be done

### Shared work through uncurrying

This is the common path in `main()`: parse, `prepareAst`, `lowerAst`,
`lamConvertProg`, lambda simplification, type checking, constructor inlining,
`desugarLamExp`, `shakeMinExp`, `alphaConvertMinExp`, `curryMinExp`,
`betaEtaFixedPoint`, `foldMinExp`, and `uncurry`.

1. Add `cut` to the scanner and Pratt parser as a prefix form `cut expr`.
2. Add it to `ast.yaml` and to the analogous syntax-template AST surface.
3. Update the surrounding AST pipeline pieces that currently mirror `back`:
 parser validity checks, AST preparation, AST lowering, namespace handling,
 and pretty-printing.
4. Add it to `lambda.yaml`, `lambda_conversion.c`, lambda simplification, and
 any lambda visitors or printers that need to preserve it.
5. Pass it through type checking with `typeof(cut expr) == typeof(expr)`.
6. Add it to `minlam.yaml`, `lambda_desugar.c`, and the shared minlam support
 code that needs to preserve or inspect the new form.
7. Update the shared pre-split minlam passes so `cut` survives through to the
 post-uncurry split point in `main()`: shake, alpha conversion, currying,
 beta/eta simplification, folding, pretty-printing, checking, and uncurrying.

### Default CEKF path

1. Carry `cut` through ANF as `CexpCut`.
2. Reconnect `anf_normalize.c` to lower `MinCut` to the already surviving
 ANF `cut` form.
3. Confirm the CEKF runtime treats `cut` with `F == NULL` as a run-time error.

### Target-b and target-c CPS path

This tail in `main()` is: `runCpsTrampolineTc`, a repeated
`betaEtaFixedPoint`, `ambMinExp`, a repeated `shakeMinExp`, the post-split
inline fixed point (`inlineMinExp` plus repeated `betaEtaFixedPoint` and
`shakeMinExp`, plus `foldVecMinExp`, `foldIffMinExp`, and `foldCmpMinExp`),
`checkMinExp`, closure conversion, `indexMinExp`, and then target-b/target-c
emission.

1. Thread `cut` through `runCpsTrampolineTc`; this is likely the trickiest
 branch-specific change.
2. Confirm that the repeated post-split `betaEtaFixedPoint` and
 `shakeMinExp` passes still behave correctly on the CPS-transformed IR once
 the shared-path support is in place.
3. Rewrite `cut` away during `minlam_amb.c`.
4. Change the CPS back-continuation protocol so back continuations accept a
 `skip` boolean.
5. Make ordinary `back` calls pass `skip = false`.
6. Make `cut` install a failure continuation that invokes its parent with
 `skip = true`.
7. Audit the branch-only post-split transforms so they either never see
 `cut` after `minlam_amb.c` or accept small plumbing additions as needed:
 `inlineMinExp`, `foldVecMinExp`, `foldIffMinExp`, `foldCmpMinExp`,
 `checkMinExp`, closure conversion, and `indexMinExp`.
8. Make the final back continuation report a run-time error when called with
 `skip = true`.

### Tests

1. Basic commit behaviour.
2. Nested `amb` and `cut`.
3. `cut` with no enclosing choice point.
4. Interaction with `here` and escaped continuations.
