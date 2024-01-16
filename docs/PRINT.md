# Printing is Hard!

Thinking out loud again.

If we print the result of `a == b` we would like to see `true` or `false`,
i.e. the type constructor names. Instead we see the integer internal
representations 0 and 1.

If we have a typedef like

```
typedef Tree(#t) { leaf | tree(Tree(#t), #t, Tree(#t)) }
```

and we print the result of `tree(leaf, 1, tree(leaf, 2, leaf))` we would
like to see `tree(leaf, 1, tree(leaf, 2, leaf))`. Instead we see the
almost unreadable internal vector representation, like

```
#[1, #[0] 1, #[1, #[0], 2, #[0]]]
```

Likewise if we print the result of `[1, 2, 3]` we would like to see
`[1, 2, 3]` i.e. the same shorthand notation for lists that the parser
accepts. Instead we get `#[1, 1, #[1, 2, #[1, 3, #[0]]]]`

Finally if we print the result of `['a', 'b', 'c']` we would like to see
`"abc"` because that's the short form for lists of chars. Instead we get
`#[1, 'a', #[1, 'b', #[1, 'c', #[0]]]]`

There is existing code in the type-checker that adds a type-annotation
to print statements, in the hope that it could later on be used to guide
printing. This was a mistake and needs to be rolled back.

There are two problems here:

1. in a polymorphic context the type-checker can only provide partial
   information to the print statement
2. the PRINT bytecode op has no access to that type information.

I can't think of any way to solve the first problem without passing
around run-time type information, which is a bad case of the tail
wagging the dog. However we may be able to live with that since
most functions are not polymorphic.

There are potential solutions to the second problem,
use the type information to compile a function that would print the
argument and call that instead.

We can try to generate the functions either early during lambda conversion,
or last-minute if they are required by `print` though it'll still be
better done before ANF conversion to avoid that complexity. Maybe we
can just generate them one-to-one with the typedefs as part of lambda
conversion? Another plus here, we just substitute print for the chosen
function during type checking rather than annotating it with type
information.

Implementation detail, as usual we can use "illegal" characters like
`$` and `#` in the function names to avoid potential clashes with user
defined functions, so our `typedef bool ...` results in
`(letrec ((print$bool ...` etc.


We might start by looking at what these generated functions might look like:

## special case list of char

reminder:

```
typedef List(#t) { nil | pair(#t, List(#t)) }
```

no need to generate this, we can just inject it.

```scheme
(print$string (lambda (l)
  (putc '"')
  (letrec ((p (lambda (l)
                (match (vec 0 lst)
                       ((0) true)
                       ((1) (putc (vec 1 l))
                            (p (vec 2 l)))))))
          (p l))
  (putc '"')
  l))
```

`putc` takes a single char (`Value`) and does what the C macro does.
bytecode would be like

```
CHAR '"' | PUTC
```

and should leave the printed char on the stack (stack cost 1 invariant).

Finally all these print functions should behave as the identity function
and return their argument.

## semi-special case list of other type

Remember we want i.e. `[1, 2, 3]` not `pair(1, pair(2, pair(3, nil)))`.
Again we can just inject this

```scheme
(print$list (lambda (list, print#t)
  (putc '[')
  (letrec ((p (lambda (l)
                (match (vec 0 l)
                       ((0) 0)
                       ((1) (displayOther (vec 1 l))
                            (q (vec 2 l))))))
           (q (lambda (l)
                (match (vec 0 l)
                       ((0) 0)
                       ((1) (puts ", ")
                            (print#t (vec 1 l))
                            (q (vec 2 l))))))
                            )
          (p l))
  (putc ']')
  list))
```

`print#t` is the pre-compiled function to display the components
of the list, whatever they are.

`puts` needs a bit of thought, I might just expand it to something like
`(putc* ',' ' ')`. where `putc*` bytecode is followed by a count and prints
and pops that many chars from the stack (minus 1).

## simple general case, booleans etc.

This, and everything else, will be fully generated.

```scheme
(print$bool (lambda (bool)
  (match bool ((0) (puts "false"))
              ((1) (puts "true")))
  bool))
```

## complex general case

```
typedef Dict(#k, #v) { leaf | tree(Dict(#k, #v), #k, #v, Dict(#k, #v)) }
```

The function takes a dictionary to print and a helper for
each component `#k` and `#v`:

```scheme
(print$Dict (lambda (d, print#k, print#v)
  (match (vec 0 d)
         ((0) (puts "leaf"))
         ((1) (puts "tree("))
              (print$Dict (vec 1 d) print#k, print#v)
              (puts ", ")
              (print#k (vec 2 d))
              (puts ", ")
              (print#v (vec 3 d))
              (puts ", ")
              (print$Dict (vec 4 d) print#k, print#v)
              (putc ')'))
  d))
```

## Compiling printers

Notice a common pattern, best evidenced by the Dict example above,
that each `print$x` takes a thing to print, then zero or more helpers
for each component, in the order declared by the typedef. Note also
that the helpers can only take a single argument, the thing to print,
and so in general we should only be using functions of one argument,
not the multi-argument generated functions like `print$Dict`.

Thankfully we can use functional composition to do this:

```scheme
(make-printer (lambda (fn . args)
                (lambda (thing)
                  (fn thing . args))))
```

This is quite nice, we pre-generate our `print$x` functions exactly
as above while
processing the typedefs during lambda conversion, then at type-checking
time, on encountering `(print d)` and determining that `d` is of type i.e.

```
Dict(list(char), list(int))
```

we replace the `(print d)` with:

```scheme
((make-printer print$Dict
               print$string
               (make-printer print$list print$int)) d)
```

We don't yet have dot-notation to support variable numbers of args and it
might be better to collect the extra args in a list/vec because varargs
are kind of at odds with strict type checking, but this is code generated
during type checking so does not *need* to be subject to it.

It might be worth inventing some terminology to distinguish the two
kinds of "printers" here, we can call the raw `print$Dict` etc. printers
"generated" printers, they can take more than one argument, and we can
call the printer functions produced by `make-printer` "compiled" printers,
they only ever take one argument and are safe as additional arguments
to the "generated" printers. Note that "generated" printers with only
one argument can be directly used as if they were compiled, i.e. on
encountering

```scheme
(print 12)
```

The compiler should just produce

```scheme
(print$int 12)
```

although

```scheme
((make-printer print$int) 12)
```

would also work, if slightly less efficiently.

## last issue, type unavailable

In a polymorphic context, where type information is missing, we just need
a generic `print$` function that does exactly what `print` does currently.

So if we'd introduced debugging to print a list inside say `length` which
can take a list of anything, we'd just replace it with
`((make-printer print$list print$) l)` because we'd determined that
`l` has type `list(#t)` in this context (`#t` is not bound).

## other last issue

Implementing `puts`.

Probably easy, mostly,
it expects strings to have been pushed on to the stack for it, followed
by a count, so

```scheme
(puts "Hello")
```

produces

```
| CHAR 'o' | CHAR 'l' | CHAR 'l' | CHAR 'e' | CHAR 'H' | DISPLAY | 5 |
```

we should remember to leave a value on the stack as all functions
return values, maybe just `vTrue`.

## Future work

Compiling these printer functions at run time is sub-optimal, though
still a good approach imo. Maybe they could be hoisted out to globals
so they don't re-compile for every print statement.
