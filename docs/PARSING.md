# Parsing

While using Flex and Bison has benefits of better grammar verification
and simpler code, it also imposes a number of restrictions which are
extremely difficult to overcome. Such static parser generators are fine
for languages like C and Java which have a completely static syntax,
but I'd like to make F♮ syntactically extensible.

A "simple" recursive-descent parser in C is a bit of a challenge, but
Nystrom is very enthusiastic about Pratt parsers and I'd like to give
them a shot.

The advantage of Pratt parsers over the more traditional recursive descent
is that they are table-driven, and all issues of precedence are handled
by this table, meaning you need only have a single routine for prefix
operators, a single routine for infix operators, etc.

For our purposes this table is even more appropriate as it is possible
to manipulate it at run-time.

The reasons to replace Flex are similar, it compiles to a fixed run-time.
It's not too hard to write a scanner by hand though, and again if we
use table-driven parsing (using tries to store tokens and keywords)
we can manipulate those structures at run-time. Supporting UTF8 should
only cause minor headaches, since we already have code to parse and
generate it.

## Requirements

We've already put a good deal of effort into making the parser re-entrant
so that it can stop, parse a `link` instruction (which reads another
file) then resume parsing the current file. We will need to retain
that capability. The existing parser also allows a nesting of inputs,
Which allows the prefixing of the standard preamble to each parse run,
and to inject the `__namespace__` hidden token, so we'll need to be able
to do that too. Also we'll want to retain the filename and line-number
"ParserInfo" header on the AST and Lambda structures.

Beyond the existing requirements, to be properly extensible any syntactic
extensions made by the language should be scoped in some way, and we'd
also like them to be exportable (without namespace qualifiers.)

I haven't thought too hard about what these syntactic extensions would
look like, but at the minimum the ability to define prefix, infix,
mixfix and postfix operators with precedence and associativity, and the
ability to attach them to anonymous functions.

Beyond that the ability to define loops of various kinds; things like
Haskell's `do` notation is also a dream, but I suspect F♮'s type system
isn't up to it.

Another desire is to remove some desugaring from the lambda conversion
and put that in the preamble, certainly `and` and `or` going to `if`,
but maybe even `if` going to `switch` and `switch` going to `fn`

## Implementation

Making a hand-written parser re-entrant is no big deal, just make sure
there is no global state and pass a parser state object around. Likewise
an input stack is just that, a stack of file handles or other sources of
tokens consumed LIFO within a single parser state.  If we're smart about
it we may even be able to use that approch when processing syntactic
extensions, but maybe not.

Making the parser extensible within a scope is maybe a bit more tricky,
the state of the parser would need to include a stack of driver tables
for the Pratt Parser, copying, pushing, modifying and dropping on
demand. Likewise the scanner would need stacks of tries to be able
to forget operators when they go out of scope. Maybe some kind of
copy-on-write mechanism? If each stack entry has a record of it's original
position on the stack, and subsequent stack entries just hold a pointer
to that previous entry, then a request to modify the top stack entry
would compare the current stack position with the position that the entry
"thinks" it is at and make a copy if the two values don't agree.  Entries
would then be pushed when entering a scope and popped when leaving.

There are only two scopes we care about, the scope of a `let/in` and
the scope of a nest (between curlies).

## Approach

Do we write a standalone parser at first, keeping an eye on the
integration points, then wire it up, or just go with replacing the
existing one? There's no transitional bit-at-a-time solution, so maybe
a standalone PoC with a target of reproducing the identical parse tree
for a couple of files, but then wire it up before it grows too many legs.

There's a lot of support code we'd like to keep from `parser.y` and
there's the "agnostic file id" thing for de-duplication of `link`
directives.
