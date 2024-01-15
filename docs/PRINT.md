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
to print statements, in the hope that it can later on use it to guide
printing when it runs. There are two problems here:

1. in a polymorphic context the type-checker can only provide partial
   information to the print statement
2. the PRINT bytecode op has no access to that type information.

I can't think of any way to solve the first problem without passing
around run-time type information, which is a bad case of the tail
wagging the dog. However we may be able to live with that since
most functions are not polymorphic.

There are potential solutions to the second problem.

1. put the type information somewhere and add an extra arg to the PRINT
bytecode telling it where to look.
2. use the type information to compile a function that would print the
argument and call that instead.

I don't like the first option.

The second might have some legs, and we might be able to cache them keyed
on the type, so there would be only one "print list of chars" funsction.

We can try to generate the functions either early during lambda conversion,
or last-minute if they are required by `print` though it'll still be
better done before ANF conversion to avoid that complexity. Maybe we
can just generate them one-to-one with the typedefs as part of lambda
conversion? Another plus here, we just substitute print for the chosen
function during type checking rather than annotating it with type
information.

Implementation detail, as usual we can use "illegal" characters like
`$` in the function names to avoid potential clashes with use defined
functions, so our `typedef bool ...` results in
`(letrec ((display$bool ...` etc.


We might start by looking at what these compiled functions might look like:

## special case list of char

no need to generate this, we can just inject it.

```scheme
(lambda (listOfChar)
  (display '"')
  (letrec ((p (lambda (lst)
                (match (vec 0 lst)
                       ((0) 0)
                       ((1) (display (vec 1 lst))
                            (p (vec 2 lst)))))))
          (p listOfChar))
  (display '"')
  listOfChar)
```

## semi-special case list of other type

Again we can just inject this

```scheme
(lambda (list, displayOther)
  (display "[")
  (letrec ((p (lambda (lst)
                (match (vec 0 lst)
                       ((0) 0)
                       ((1) (displayOther (vec 1 lst))
                            (q (vec 2 lst))))))
           (q (lambda (lst)
                (match (vec 0 lst)
                       ((0) 0)
                       ((1) (display ", ")
                            (displayOther (vec 1 lst))
                            (q (vec 2 lst))))))
                            )
          (p listOfChar))
  (display "]")
  list)
```

where `displayOther` is the pre-compiled function to display the components
of the list.

## simple general case, booleans etc.

```scheme
(lambda (bool)
  (match bool ((0) (display "false"))
              ((1) (display "true")))
  bool)
```

## complex general case

```
typedef Dict(#k, #v) { empty | tree(Dict(#k, #v), #k, #v, Dict(#k, #v)) }
```

```
(letrec ((displayDict (lambda (dict, displayK, displayV)
  (match (vec 0 dict)
         ((0) (display "empty"))
         ((1) (display "tree("))
              (displayDict (vec 1 dict) displayK, displayV)
              (display ", ")
              (displayK (vec 2 dict))
              (display ", ")
              (displayV (vec 3 dict))
              (display ", ")
              (displayDict (vec 4 dict) displayK, displayV)
              (display ")"))
  dict)))
  ...
```

## last issue, type unavailable

In a polymorphic context, where type information is missing, we just need
a generic `displayAny` function that does exactly what `print` does currently.

So if we'd introduced debugging to print a list inside say `length` which
can take a list of anything, we'd just call it like
`(displayList list display)`

## other last issue

Implementing `display`.

Probably easy, mostly, it can defer to PRINT for bignums, otherwise
it expects strings to have been pushed on to the stack for it, followed
by a count, so

```scheme
(display "Hello")
```

produces

```
| CHAR 'o' | CHAR 'l' | CHAR 'l' | CHAR 'e' | CHAR 'H' | DISPLAY | 5 |
```

Of course this requires an alternative implementation of strings,
these are static compile time `const char *` entities.
