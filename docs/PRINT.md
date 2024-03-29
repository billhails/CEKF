# Printing is Hard!

Thinking out loud again.

If we print the result of `a == b` we would like to see `true` or
`false`, i.e. the type constructor names. Instead we see the integer
internal representations 0 and 1.

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

There are two problems here:

1. in a polymorphic context the type-checker can only provide partial
   information to the print statement.
2. the PRINT bytecode op has no access to that type information in any
   case.

I can't think of any way to solve the first problem without passing around
run-time type information, which is a bad case of the tail wagging the
dog. However we may be able to live with that since most functions are
not polymorphic.

There are potential solutions to the second problem, use the type
information to compile a function that would print the argument and call
that instead.

We can try to generate the functions either early during lambda
conversion, or last-minute if they are required by `print` though it'll
still be better done before ANF conversion to avoid that complexity. Maybe
we can just generate them one-to-one with the typedefs as part of
lambda conversion? Another plus here, we just substitute print for the
chosen function during type checking rather than annotating it with
type information.

Implementation detail, as usual we can use "illegal" characters like
`$` and `#` in the function names to avoid potential clashes with
user defined functions, so our `typedef bool ...` results in `(letrec
((print$bool ...` etc.


We might start by looking at what these generated functions could
look like:

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
(print$list (lambda (l print#t)
  (putc '[')
  (letrec ((p (lambda (l)
                (match (vec 0 l)
                       ((0) 0)
                       ((1) (print#t (vec 1 l))
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
  l))
```

`print#t` is the pre-compiled function to display the components of the
list, whatever they are.

`puts` needs a bit of thought, I might just expand it to something like
`(putc* ',' ' ')`. where `putc*` bytecode is followed by a count and
prints and pops that many chars from the stack (minus 1 to preserve the
stack cost invariant).

## simple general case, booleans etc.

This, and everything else, will be fully generated.

```scheme
(print$bool (lambda (bool)
  (match bool ((0) (puts "false"))
              ((1) (puts "true")))
  bool))
```

## complex general case 1 - dictionary/tree

The type is fully parameterised.

```
typedef Dict(#k, #v) { leaf | tree(Dict(#k, #v), #k, #v, Dict(#k, #v)) }
```

The function takes a dictionary to print and a helper for each component
`#k` and `#v`:

```scheme
(print$Dict (lambda (d print#k print#v)
  (match (vec 0 d)
         ((0) (puts "leaf"))
         ((1) (puts "tree("))
              (print$Dict (vec 1 d) print#k print#v)
              (puts ", ")
              (print#k (vec 2 d))
              (puts ", ")
              (print#v (vec 3 d))
              (puts ", ")
              (print$Dict (vec 4 d) print#k print#v)
              (puts ")"))
  d))
```

## complex general case 2 - named result

The type is partially parameterized, and contains some explicit types.

```
typedef NamedResult(#r) { result(list(char), #r) }
```

The function takes a NamedResult to print and a helper for the
parameterised type.

```scheme
(print$NamedResult (lambda (a print#r)
  (puts "result(")
  (print$string (vec 1 a))
  (puts ", ")
  (print#r (vec 2 a))
  (puts ")")
  a))
```

The code can directly infer the correct printer function for `list(char)`.

## Compiling printers

Notice a common pattern, best evidenced by the Dict example above,
that each `print$x` takes a thing to print, then zero or more helpers
for each component, in the same order as declared by the typedef (`#k,
#v`). Note also that the helpers can only take a single argument, the
thing to print, and so in general we should only be using functions of one
argument, not the multi-argument generated functions like `print$Dict`.

Thankfully we can use functional composition to create those single
argument functions "pre-parameterised":

```scheme
(make-printer (lambda (fn . args)
                (lambda (thing)
                  (fn thing . args))))
```

This is quite nice, we pre-generate our `print$x` functions exactly as
above while processing the typedefs during lambda conversion, then at
type-checking time, on encountering `(print d)` and determining that
`d` is of type i.e.

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

It might be worth inventing some terminology to distinguish the
two kinds of "printers" here, we can call the raw `print$Dict`
etc. printers "generated" printers, they can take helper functions as
additional arguments, and we can call the printer functions produced by
`make-printer` "compiled" printers, they only ever take one argument and
are safe as additional arguments to the "generated" printers. Note that
"generated" printers with only one argument are also safe and can be
directly used as if they were compiled, i.e. on encountering

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

would still work, if slightly less efficiently.

## Better Idea - currying

Instead of a special-purpose `make-printer` function, we can just
rearrange the arguments to the generated printers so the thing to print
comes last, then compose them in the same way, but directly. Additionally
we can write the bespoke printers for list and string directly in F natural.

For example assume the typedef for Tree as given above, and a `printTree`
function taking a key printer, a value printer and a Tree, written in
F natural, along with a printInt function. We can then print a Tree of
string to list of int simply as

```
printTree(printString, printList(printInt)) (tree)
```

This poses no problems for type-checking either, and the fact we can write
printers for user-defined types allows the possibility of overriding the
generated ones. That is useful because in a map or set we don't really
want to expose the implementation, it's not useful to the user. An
example set, implemented as a red/black tree, say

```
typedef Set(#t) {
  leaf |
  red(Set(#t), #t, Set(#t)) |
  black(Set(#t), #t, Set(#t))
}
```

could be given a printer like:

```
fn printSet(helper, s) {
    let
        fn h {
            (leaf) { true }
            (red(l, v, r)) |
            (black(l, v, r) {
                helper(v);
                puts(", ");
                true
            }
        }
    in
        puts("{");
        h(s);
        puts("}")
        s;
}
```

With a bit more effort I'm sure we could avoid printing a trailing comma.

## Last issue, type unavailable

In a polymorphic context, where type information is missing, we just need
a generic `print$` function that does exactly what `print` does currently.

So if we'd introduced debugging to print a list inside say `length`
which can take a list of anything, we'd just replace it with
`print$list(print$)(l)` because we'd determined that `l` has type
`list(#t)` in this context (`#t` is still a variable).

## Other last issue

Implementing `puts`.

Probably easy, mostly, it expects strings to have been pushed on to the
stack for it, followed by a count, so

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
so they don't re-compile for every print statement execution.
