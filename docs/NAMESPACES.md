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
import "path/to/myname.fn";
```

Will import and declare a namespace called `myname` using the basename of
the file.  Repeated imports of the same file should not create repeated
compilation units. By default the path will be relative to the file doing
the import.

```
import "path/to/myname.fn" as anothername;
```

Overrides the default name for the namespace.

```
export fn foo ...
export id = ...
```

Within the namespace file, marks certain entities as being part of the
namespace's interface. any entities not so marked are private.

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
of a co-ordinating `namespaces` branch to keep some sort of control.

### Parsing

Some initial clean-up required, I'll remove all reference to `env` syntax.

The parser is already re-entrant, it should therefore be possible to
create a new parser for the file (if not already loaded), parse it in
a namespace context, and return a resulting structure to the parent
parser. However any subsequent import of the same file should return a
reference to the previous import, distinguished from the import itself
to avoid re-compilation. I'll need to start the re-entrant parser in a
different state though.

### Lambda Conversion

Mostly straightforward, will require a new lambda construct or two,
tagging of exported values etc. Typedefs exported by a namespace
should be available to pattern matching:

```
import "dict.fn";

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

We will need to think about the environment available to the namespace. It
will need the standard prelude but probably shouldn't inherit any other
random context, maybe a separate bytecode array per namespace? That
would require a re-entrant VM, but may still be the best solution.
