# Operators (and macros)

Some issues with the initial implementation.

I'd thought I could get away with a pure parser-only implementation of
infix operators and it basically works, but there are some issues which
make that approach quite clunky.  One specific scenario is where I'm
declaring an infix addition operator in the preamble as:

```
infix left 100 "+" addition;
```

Where `addition` is the symbol for the two-argument built-in addition
operator. Trouble is in another file I'd redefined `addition` as a type
of expression for unrelated purposes, and because i.e. `2 + 2` gets
re-written to plain old `addition(2, 2)` unconditionally, in that context
the interpreter finds the `addition` type from the current environment
rather than the one where the infix operator was declared.

This is clearly unacceptable.

[Hygenic Macros in Wikipedia](https://en.wikipedia.org/wiki/Hygienic_macro) states:

> The basic strategy is to identify bindings in the macro definition and
> replace those names with gensyms, and to identify free variables in the
> macro definition and make sure those names are looked up in the scope
> of the macro definition instead of the scope where the macro was used.

This offers hope, if we can re-work the macro system to be hygenic by default,
then the parser instead of generating
`addition(a, b)` for `a + b` could instead generate:

```
macro gensym$1(a, b) { addition(a, b) }
```

at the point of the operator declaration, and generate `gensym$1(a, b)`
when `a + b` is subsequently encountered.

Firstly I now think the use of a `$` prefix to indicate a gensym in a macro
is not the best idea. Instead the lambda conversion should identify bound
`let` variables and replace them automatically. That also frees up `$` as a
potentially useful user-defined prefix operator.

The bigger problem is that we can't continue to do naiive macro expansion during
the lambda conversion step, or we'd be back where we started with
`addition(a, b)` referring to whatever `addition` happens to be the current
definition.

We may have to revert to the scheme definition of a macro: pass the arguments
unevaluated to the macro, evaluate the macro body, then re-evaluate the result.

But we really don't want to have the macro evaluated like that, because F♮ is not
homoiconic, "evaluating the macro body" can only mean substitution.

What if the arguments to macros were wrapped in a closure?

```
macro AND(a, b) { if (a) { b } else { false } } => fn AND(a, b) { if (a()) { b() } else { false } }

AND(a, b) => AND(fn () { a }, fn () { b })
```

That would definately work, though it won't be quite as efficient. It solves both
local scoping rules, since `AND` is now a normal function then free variables in the
body are evaluated in the context of the function definition, and variables in the
argument expressions are evaluated in the calling context.

