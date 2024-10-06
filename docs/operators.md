# Runtime Syntactic Extensions

Now we have a Pratt Parser with tries and an operator lookup table, it
superficially seems easy to start to replace the hard-coded rewrites of
`a @ b` to `cons(a, b)` etc. with simple syntax extensions within the
language, for example

```
infix left 90 "@" cons;
```

The `infix` token is an operator with its own keyword, but `left`
is just an atom that is matched by token equivalence - we can have a
`TOK_LEFT()` but just not in the lookup table.

The quoted operator is checked for validity (i.e. no whitespace) and
that it does not already exist with that fixity.

The current trie is extended with the new operator, and the lookup table
is extended with a new or replacement record, with, in this example,
an infix function and precedence.

The structure of the Record will need to be extended to accomodate the
replacement expression.

The generic infix function parses its operands with the supplied
precedence and uses the replacement expression from the record to build
a function (or macro) application of the replacement expression to its
two arguments.

## The Problem

The problem is that `@` and `cons` are parsed in two different ways
depending on context. In the context of an expression all works fine
as described above, but in the context of a formal argument the parsed
structures are entirely different.

There are a few potential solutions:
1. Perform a purely textual substitution prior to parsing. This has
   all the drawbacks of C macros and is not really a good option.
2. Parse all formal arguments as expressions then perform an
   `exprToFarg()` transformation on them individually to re-build
   the existing grammar. This transform could also invalidate illegal
   expressions i.e. `a + b`.
3. Parse all fargs as expressions and modify the TPMC to cope. This
   could get ugly, as the TPMC has enough to deal with as it is.

So all things considered option 2 is the current front-runner.

One complication, there are a couple of constructs that are only legal as
formal arguments, specifically wildcards `_` and argument aliasing (`x =
a @ b`) The expression parser would need to be extended to cope. Perhaps
an additional flag could be passed to it to say whether it is parsing
fargs or expressions, but that seems unwieldy as it would proliferate
everywhere. Another option is to have a wrapper around the expression
parser that validates for normal expressions and a separate wrapper that
does the `exprToFarg` transform.

## For Later

Looking ahead, in case this has any bearing on the previous discussion,
we'll eventually want to remove all special case behavior for lists,
which are defined in the preamble but required in the core. Some of
that is relatively trivial:

```
prefix 50 "<" car;
prefix 50 ">" cdr;
```

but lists declared with `[]` need a little more thought.

```
macro __id__ (x) { x }
prefix 2 "[" __id__
infix right 1 "," cons
postfix 0 "]" nil
```

Might work, but it's starting to look like vimscript. Anyway it probably
won't work, once we allow ',' as an infix operator, all calls to the
`expression` parser will start to return conses when they encounter
commas. There may be a way to selectively "turn on" operators within
the context of another operator which would partially solve that,
but the operator would need to be only valid in the top-level context
of that containing operator, otherwise it would inappropriately apply
to subexpressions.

Strings as lists of chars are even more problematic,

We might consider writing macros as parsers in the language, but then
they would need to be separately pre-compiled all the way to bytecode
and run on the remaining input.

What about a BNF style approach to macros?

```
prefix 0 "[" grammar {
    conslist: "[" consargs "]"        { $2 }
            ;
    consargs:                         { nil }
            | expression              { cons($1, nil) }
            | expression "," consargs { cons($1, $3) }
            ;
}
```

Looks like a lot of work, re-implementing a parser generator after
we just got rid of one, and parser generators are distinctly
non-trivial. It would solve the problem though.

Back to basics, the lisp definition of a macro is a special form
that is passed its arguments unevaluated, but then the structure
it returns is subsequently evaluated. This is fine in lisp which
is homoiconic, but macros in f♮ will need to be more careful.

The earlier document [MACROS](MACROS.md) suggests using a dollar
sign as prefix for atoms that are to be replaced with generated
symbols, allowing some hygene. The limitation of these functional
style macros is they cannot readily extend the actual syntax of
the language. They would be fine for the booleans though:

```
macro __and__(a, b) { if (a) { b } else { false } }
infix left 30 "and" __and__;
```

etc.

So assuming we go with that, how do we make macros work? They will
need to operate on the AST, or maybe they could be simply recorded
in the definitions and dealt with during lambda conversion, either
way they don't want to get as far as the type-checker as that is
overly complicated already.

If operators are handled directly by the parser and macros passed
to lambda conversion then that would be a clean separation, also
lambda conversion is already using a context (environment) to track
type constructors for inlining, namespaces etc. so it should be
relatively easy to add macro inlining there.

Also, because macros are expanded in a later pass, there is no need
to declare them lexically before use in an operator.
