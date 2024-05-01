# Namespaces

From an early stage I'd planned to support environments as first-class
entities. This is a very tempting idea because of all of the behaviour
you would get for free, almost bootstrapping ourselves an oop system out
of nothing. However there would be some difficult problems to solve with
that feature, off the top of my head I can think of two:

1. Typechecking - The type of an environment would be the type environment
   produced while checking, not a problem, but any function using such an
   environment would have to somehow explicitly declare its type.
2. Lexical addressing - Two environments with the same type would almost
   certainly not have their values at the same locations, so lexical
   addressing of access outside of one would fail when applied to another.

Given those, and probably other obstacles, I'm going to try for the much
more limited but hopefully practical approach of namespaces.

## Syntax

```
import "path/to/myname.fn" as myname;
```

Will import and declare a namespace called `myname`.  Repeated imports
of the same file should not create repeated compilation units.  The path
will be relative to the file doing the import.

```
export typedef foo ...
export fn bar ...
export baz = ...
```

Within the namespace file, marks certain entities as being part of the
namespace's interface. any entities not so marked will not be visible
outside of the namespace.

```
myname.foo
```

Outside of the namespace allows access to exported entities.

## Semantics

* Namespaces are not first class. They are static labels referring to
  compilation units.
* Namespaces can nest.
* (later) Namespaces should be able to export other namespaces, or at
  least export values imported from other namespaces.
* All namespaces are constructed at compile-time.
* The structure of namespace files is costrained to a set of letrec
  bindings.
* Recusive nesting of namespaces will be an error at least initially,
  there may be solutions with backfilling later.

## Implementation

In general we'll need a new concept of namespace, distinct from types
and variables, so a "namespace for namespaces" used for lookup on the
lhs of the `.` operator.

### Version Control

This is quite a big piece of work, so I plan to create sub-branches off
of a co-ordinating `namespaces` branch to keep some sort of control, and
(this time) using tags to allow easier recovery.

```mermaid
gitGraph TB:
	commit
	branch namespaces
	checkout namespaces
	commit tag: "namespaces-v1"
	branch parsing
	checkout parsing
	commit
	checkout namespaces
	branch lambda
	checkout lambda
	commit
	checkout namespaces
	merge parsing tag: "parsing-v1"
	merge lambda tag: "lambda-v1"
```

### Parsing

Some initial clean-up required, I'll remove all reference to `env` syntax.

The parser is already re-entrant, it should therefore be possible to
create a new parser for the file (if not already loaded), parse it in
a namespace context, and return a resulting structure to the parent
parser. However any subsequent import of the same file should return a
reference to the previous import, distinguished from the import itself
to avoid re-compilation. I'll need to start the re-entrant parser in a
different state though.

We'll also need to separate out the parsing of the prelude. We can treat it
as a namespace for this purpose, but not install it as one.

### Lambda Conversion

Mostly straightforward, will require a new lambda construct or two,
tagging of exported values etc. Typedefs exported by a namespace
should be available to pattern matching:

```
import "dict.fn" as dict;

fn foo {
    (dict.leaf) { ... }
}
```

and to the print compiler, ideally the print compiler would provide
namespace prefixes to the output, but that may prove impractical.

### Type Chacking

As mentioned, the type of a namespace is the type-environment constructed
while checking it, however that type environment needs to be pruned of
any private entities before it is used outside of the namespace.

### ANF Conversion

Hopefully no issues here, the ANF conversion currently needs an overhaul,
so if there are issues this may be the ime to do it.

### Desugaring

There shouldn't be any new constructs to desugar specifically.

### Bytecode Generation

Thinking ahead, it might be better to plan the bytecode first and work
backwards, so here's a first attempt.

![bytecode](Namespaces.png)

#### Bytecode Generation

* First off we'll need to parse the standard prelude separately, resulting
  in the first purple block of bytecodes.
* Next a new bytecode `BYTECODE_NS` introduces the namespaces. it is
  followed by an integer holding the number of namespaces to be expected.
* Then each namespace is compiled to a sequence of lambdas etc.,
  and each compiled namespace is terminated by another new bytecode
  `BYTECODE_NS_END`.
* After each `BYTECODE_NS_END` we write the number of stack slots the
  namespace will consume, and the number of the namespace (zero indexed)
  added to the number of slots consumed by the prelude, resulting in the
  stack position of the namespace.
* After the last namespace, we write a new `BYTECODE_NS_FINISH` followed
  by the number of namespaces.

#### Bytecode Execution

* The prelude execution is unchanged: skip over each lambda storing and
  pushing a closure with it's entry point address.
* On seeing `BYTECODE_NS`. read the following number and allocate that
  many stack slots.
* Subsequent lambdas etc, are processed normally, each getting
  pushed onto the stack, after the namespace slots.
* When a `BYTECODE_NS_END` is encountered:
  * Read the number of slots to pop and the namespace stack position.
  * Before popping anything, create a new Value type NAMESPACE with a
    copy of the current stack.
  * Then put that into the stack at the designated location.
  * Lastly pop the namespace internals off of the stack.
* When `BYTECODE_NS_FINISH` is encountered, read the number of namespaces.
  * For each namespace slot:
    * copy the entire block of completed namespace slots over the same
      locations in the namespace's snapshot.
* The body is then executed in the normal way.

#### Bytecode for Namespace Access

We can't treat the `.` operator like a normal postfix, because the rhs
will be an expression that expects to be evaluated in the context of
the lhs, an'd we haven't run in to the `.` bytecode yet. Consider a
normal apply:

```
| ..argn.. | ... | ..arg1.. | ..fn-expr.. | APPLY |
```

If that `fn-expr` is namespace-prefixed, any postfix operation will only
be encountered after it, just before the `APPLY`. So if the namespace
itself is treated as a normal VAR or LVAR then we'd need to follow
it with an op that installs the namespace as the new current context,
and after the `fn-expr` another op that restores the previous context:

```
| ..argn.. | ... | ..arg1.. | (L)VAR | NS_INSTALL | ..fn-expr.. | NS_UNINSTALL | APPLY |
                            |                                                  |
                            |------written by the code generating the dot------|
```

We should figure out if this `NS_UNINSTALL` can be accomplished with an
existing continuation operation like `RETURN` rather than adding yet
another bytecode. `NS_INSTALL` will probably need the address of the
instruction following the `RETURN` if we do that, it may be more
efficient to have the additional `NS_RETURN` instruction instead.

So how would that be executed?

![access](NamespaceAccess.png)

* `args` first the args are evaluated and pushed (1) - (3)
* `(L)VAR` then the namespace is evaluated (looked up) and pushed (4)
* `NS_INSTALL` then the namespace at TOS is installed (5):
  * pop the namespace
  * snapshot the stack to a new continuation (K).
  * replace the stack with the one in the namespace.
* `fn-expr` then the `fn-expr` is evaluated, leaving the result on the top of the stack (6)
* `NS_UNINSTALL` then the previous continuation is restored (7):
  * the value at TOS is popped and stashed
  * the stack is restored
  * the popped result is pushed back
* `APPLY` finally the closure at the TOS is applied to its arguments (8).
