# First Class Environments

By elevating environments to first-class data types we can re-use existing
code to achieve encapsulation and namespaces.

Environments are already explicit in the CEKF machine as the `E` register,
so making them in to data types should just be a matter of extending
the definition of values to include some $\mathbf{env}(\rho)$
type which is a constant.

Syntactically, these environments are just the `let` part of a nest,
without the `in` component, so are effectively `letrec` bindings where
the implicit letrec body just returns the environment as its value.

So in lambda conversion, we can forgoe an explicit `LamEnv` or similar
in favour of a normal nest, where the body makes use of a new `getenv`
operation that captures the current environment and returns it as a value.
Of course the top of the current environment at that point is just the
current stack frame so we would need to snapshot that, Semantically that
could be achieved by `((lambda () (getenv))` to force the snapshotting
then `getenv` really does just need to capture the current `E` register.

In fact we could get rid of all of the surface syntax around environments
and just expose `getenv` in the language itself, worth thinking about.

Having thought about it I like this idea, it's far less effort for the
same capabilities, and is more flexible as being able to return the
environment from the body of a function means that environments can be
parameterised by function arguments.

In any case the `.` operator then takes an expression on the
lhs that evaluates to an environment and an expression on the
rhs to evaluate in that environment.  So the `.` operator will be
left-associative.  We should think about making this work for `print` too,
`myenv.print(myenv.value)` should have access to typedefs in `myenv`.

Fast Lexical Addressing will have problems, that analysis is static
so has no example environment to search, plus there is no guarantee
that the same variables are in the same locations in different
environments. Perhaps the type-checker can assist, and/or there
will have to be an additional level of indirection (each env knows
the lexical address of its contents?)

Type checking all this is where the fun starts, but there is an insight
that should help: the type of an environment is precisely the type
environment that was constructed while type checking it.
