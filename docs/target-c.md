# Target C as a Compilation Step

As a stopgap before targetting LLVM, but also as a useful exercise that
might inform the LLVM work, I'd like to attempt to generate a standalone
`main` function from the MinExp (`minlam.yaml`) structures after CPS
transformation and closure conversion.

This seems reasonable as CPS allows all function calls to be replaced
with `goto`, and the closure conversion means that all variable access
is local.

Also the recent AMB transform has "un-magic'd" the `amb` operator
to a plain failure continuation passed along with, and to, the CPS
continuation.

It is envisaged that lambdas will be stored in `Value`s as computed gotos:
`void *` pointers created with the C `&&LABEL` syntax.

Before we can proceed, we need to decide how to represent, and where
to store, local variables. The "env" structures that are generated as
a result of closure-conversion have been desugared to simple `make-vec`
calls, and the dereferencing of their contents to primitive `VEC` indexed
O(1) lookups. Those envs in turn are either contained by other envs or
directly by local variables so it all comes down to how local variables
are handled.

I *think* that in turn devolves to the choice between a register machine
and a stack.

Once that decision is made, and the details are thrashed out, the next
step is likely to be a variable-annotation pass to convert variables
to locations. After that the C code generation should be fairly
straightforward.

## Update

After consultation, the decision is some sort of register machine, as
a stack is overkill for a known set of variables. The best solution
for performance is a set of named C variables allowing direct access,
but probably an array is more pragmatic as it can be memory-managed
more easily.

As a first attempt I have a prototype in
[fn/rewrite/annotate.fn](../fn/rewrite/annotate.fn) which is hooked
in to the [test harness](../fn/rewrite/test_harness.fn) and producing
output, but it's difficult to be sure if it is correct as the previous
[closure conversion](../fn/rewrite/closure-convert.fn) step introduces
a lot of noise, somewhat mitigated by subsequent
[clean-up](../fn/rewrite/unconvert.fn).
