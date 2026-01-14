# Extreme Desugaring done ASAP

The idea on this `simple-lambda` branch is to perform maximum desugaring
of the `lambda.yaml` constructs immediately after type-checking.

The reason we can't do it earlier than that is that the type checker requires
anonymous lambda application to be rewritted into a let for polymorphic type
checking, while desugaring does the opposite.

Anyway here is a list of the desugaring operations that are desired:

1. `let*` becomes a nest of `let`.
1. `let` in turn becomes an anonymous lambda.
1. `LamPrimApp` becomes `apply` of a primOp.
1. All typedefs are discarded.
1. `LamPrint` becomes an `apply` of the compiled printer to its argument (already done in the type-checker but could be moved out to this additional step).
1. `typeof` is replaced by a string (already done in the type-checker).
1. ...

We really want to push towards a pure minimal lambda representation.
The result should be represented as a new stage, `SimpleLambda.yaml` or similar, presenting a much smaller surface area for subsequent transformations and greatly
reducing the amount of code downstream.

Later desugaring that can't be done here:

1. After CPS conversion, `call/cc` can be replaced with simple function application: `(call/cc f)` becomes `((lambda (f k) (f k k)) f k)`.
