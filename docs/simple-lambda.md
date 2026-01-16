# Extreme Desugaring done ASAP

The idea on this `simple-lambda` branch is to perform maximum desugaring
of the `lambda.yaml` constructs after type-checking.

The reason we can't do it earlier than that is that the type checker requires
anonymous lambda application to be rewritten into a `let` for polymorphic type
checking, while desugaring does the opposite. There is also an "inline"
step after typechecking that resolves type constructors, we can't desugar before that
(but a later refactoring might unify inlining and desugaring).

Anyway here is a list of the desugaring operations that are desired:

1. `let*` becomes a nest of `let`.
1. `let` in turn becomes an anonymous lambda.
1. `LamPrimApp` becomes `apply` of a primOp.
1. All typedefs are discarded.
1. `LamPrint` becomes an `apply` of the compiled printer to its argument (already done in the type-checker but could be moved out to this additional step).
1. `typeof` is replaced by a string (already done in the type-checker).
1. There should only be `LamLookUp`, `LamLookUpSymbol` etc are redundant.
1. `Construct` and `Deconstruct` replaced with simple vector operations.
   (`constructToMakeVec` and `deconstructToPrimApp` in `anf_normalize.c`).
1. `LamConstant` to integer (see the treatment of `LAMEXP_TYPE_CONSTANT` in
   `anf_normalize.c`)
1. `LamTypeConstructorInfo` reduced to `Construct` or `Constant` by `inline.c`.
1. `MakeTuple` to `makeVec` (see `tupleToMakeVec` in `anf_normalize.c`)
1. `tag` becomes `primApp` (see `tagToPrimApp` in `anf_normalize.c`)
1. `tupleIndex` becomes `primApp` (see `tupleIndexToPrimApp` in `anf_normalize.c`)
1. `typeDefs` stepped over and discarded (see treatment of `LAMEXP_TYPE_TYPEDEFS` in `anf_normalize.c`)
1. ...

We really want to push towards a pure minimal lambda representation.
The result should be represented as a new stage, `minlam.yaml` or similar, presenting a much smaller surface area for subsequent transformations and greatly
reducing the amount of code downstream.

Later desugaring that can't be done here:

1. During CPS conversion, `call/cc` can be replaced with simple function application: `(call/cc f)` becomes `((lambda (f k) (f k k)) f k)`.
1. During CPS conversion, `sequence` becomes a chain of continuations(?)

## Approach

1. Create a verbatim copy of `lambda.yaml` called `minlam.yaml` **done**
2. Rename all prefixes in `minlam.yaml` from `Lam` to `Min` **done**
3. Generate a visitor on `lambda.yaml` called `lambda_desugar.c` **done**
4. Modify the generator to perform a plain translation from `Lam*` types to the new `Min*` types. **done**
5. Update the pipeline in main to perform this transform, after the constructor inlining step, before ANF conversion and to pass the new tree downstream to `lambda_alphaconvert.c`. **done**
6. Update `lambda_alphaconvert.c` and `anf_normalize.c` to consume `Min*` types instead of `Lam*` types. **done**
7. Test everything still works. We will also need to update the so-far unused `anf_normalize_2.c` and `lambda_CpsT[ck].c` to use `Min*` instead of `Lam*`. **done**
8. Iterate on the individual desugaring steps outlined in the previous section using the new visitor to translate, removing the now unused type from minlam.yaml and updating all affected files to no longer perform unnecessary translations.

This may seem slightly pointless since `anf_normalize.c` immediately transforms that tree and discards it in favour of the new structs in `anf.yaml`, but this work is actually more geared towards the alternative lambda transforms "branch" of the pipeline, the alpha conversion, cps conversion, closure conversion, beta-reduction etc. that will all benefit from a much simpler and smaller dataset.
