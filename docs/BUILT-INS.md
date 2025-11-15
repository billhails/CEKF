# Built In Functions

Already implemented, the primary data structures are all defined in
`src/builtins.yaml` and generated automatically.

The individual implementations are mostly just in `src/builtins_impl.c` if they are small, while larger inplementations have their own source files (`src/builtin_sqlite,c` etc.)

`src/builtin_helpr.c` has the registration support, and the routine `registerBuiltins` is called from `main` in `src/main.c`, before any parsing
starts. The registered builtins are passed to the type-checker along with
the LamExp resulting from parsing, and they are later passed to `run`
in `src/step.c` for run-time execution.

## Currying Builtins

Currently builtins cannot be curried, but there is an easy solution which
is to wrap every builtin in a normal function which can be curried.
We will have to arrange that the builtins internal names are changed, the
wrapping functions should get the names of the builtins they wrap. For a concrete example, the builtin `rand` would be given the name `builtin$rand`
or similar, the dollar in the name is not normally valid syntax for an
identifier so cannot be accidentally overridden by the user (same idea as the
macro implementation for operators.) The generated wrapper function would 
then be called `rand`. It would take the
same arguments as the builtin and call it with those arguments. The function
will need to be generated because it also needs a dollar in the name of the
builtin it is calling.

One incremental approach to implementing this is to extend the definition of
the `BuiltIn` struct in `src/builtins.yaml` to contain both the internal (dollar-containing) and external plain name. the `registerBuiltins` routine
can initialize it with both, and subsequent stages have access to both.