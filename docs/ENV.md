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

## Namespaces Only

As an alternative to fully first-class environments, which may not be
possible with fast lexical addressing, we could look at `env` declarations
purely as namespaces, that is the names of envs are not variables but
constant names to be used literally. This is certainly simpler, but the
semantics are a bit wooly, so how would it work?

The `.` operator would evaluate its rhs in the context of the namespace
on the lhs, like `myns.doSomething(arg)`. The dot operator would have to
have higher precedence than even function calls, so that would parse as
`(myns.doSomething)(arg)`. Bytecode might then be something like

```
| ..arg.. | SAVENV | myns | SETENV | VAR[n][n] | SWAP | RESTORENV | APPLY |
```

I'm not sure I like the save and restore because we don't currently have
an environment stack, and we'd need Forth-level stack twiddling to use
the existing stack (i.e. the `VAR` lookup would leave its result top of
stack hence the need for a SWAP bytecode or similar).

Also what's the representation of `myns` in bytecode if it's not a var?
It has to refer to an environment in some way.

Also how would static analysis find `myns`? We'll likely need a second
environment mapping namespaces to envs, and those envs in turn would need
to be paired with namespace envs, unless we have the one env do double-duty
and store both the frame offset of the var and optionally the env it
refers to if it's an env. Not impossible but starts to feel messy. On the
other hand making envs vars again but requiring them to be instantiated
statically would solve the bytecode issue too. `myns` just becomes another
`VAR` lookup.

What about type checking? Again if `myns` is bound to a `TcEnv` during
type checking, that env can be used to validate the rhs of the dot
operator.

Any other holes in this idea? ANF conversion is purely local transforms
that shouldn't be affected, desugaring likewise.
