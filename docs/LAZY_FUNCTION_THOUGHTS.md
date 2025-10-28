# Thoughts about lazy functions and typing

Review [LAZY_FUNCTION_PROPOSAL.md](LAZY FUNCTION PROPOSAL) for background.

I'm not totally happy with the `isLazy` flag on the `TcFunction` type, I think it might be better to have an explicit thunk type `thunk(#t)` with a deconstructor `force:: thunk(#t) -> #t` which is really just invoking the thunk as a function with no arguments. Then the `isLazy` flag would go and instead the macro arguments would be explicitly thunks. The main reason I'm not happy with it is that there can be run-time type errors and that really shouldn't be possible in a strictly typed language. By keeping the types real rather than special-casing lazy functions this should be feasible?

I think the first step would be to create that new thunk type, it can display as `(#() => a)` then in `tc_analyze.c/analyzeLam()`, rather than just calling the recursive `makeFunctionType` it would check for null arguments (an explicit thunk) and instead create a thunk of the return type.

I'm also considering some additional syntax for thunks, a prefix `&` operator on the formal arguments of a function would declare that it expects a thunk, a prefix `&` operator on an actual argument would thunk it (so `&x` produces `fn () { x }`) and a prefix `*` would force the thunk (so `*x` produces `x()`).