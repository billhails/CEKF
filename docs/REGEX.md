# Regular Expression Support

The standalone implementation is done. It is unicode-centric (native
support for Unicode Category Classes) and purely wchar_t (Unicode
raw code points, no need to support char or UTF-8). Nothing is using
it yet. This document attempts to pin down the integration requirements.

The implementation is in:

* [regex.yaml](../src/regex.yaml)
* [regex_helper.c](../src/regex_helper.c)
* [regex_helper.h](../src/regex_helper.h)

There is reasonable unicode support already:

* [unicode.c](../src/unicode.c)
* [unicode.h](../src/unicode.h)

And simple mechanisms for translation between representations:

* [cekf.c](../src/cekf.c)
* [cekf.h](../src/cekf.h)

## Requirements

* Primarily a language-level feature, the existing Pratt scanner seems fine without it though if the scanner can be ported to use it later, that would be a bonus.
* Focus on its use in writing parsers. Specifically left-prefix anchored expressions will be the norm, no requirement for unanchored search.
* They should play nice with Parser Combinators.
* The ability to handle both internal strings (lists of `wchar_t`) and external files (streams of UTF-8) via a common translation to arrays (or streams?) of `wchar_t`.
* Being able to deal with infinite streams is a definite plus.
* Regexes should be their own language-level type, distinct from strings,
  with literal syntax using the perl/javascript `/.../` body form.

## Scanner and Literal Syntax

The scanner/parser split matters here.

The current Pratt parser only sees tokens after the scanner has already
decided what kind of token it is looking at. That means a regex literal
cannot be introduced purely as a prefix parselet if the scanner still
treats `/` only as comment or operator territory.

As a first integration step, `#/.../` looks like the simplest compromise.

* `#` already has dedicated prefix-token support in the Pratt parser.
* It avoids contextual lexing for bare `/.../`.
* It avoids fighting user-defined slash operators.
* It keeps the door open for bare `/.../` later if expression-start aware
  lexing ever becomes worth the cost.

The intended implementation shape is therefore:

* the scanner recognizes `#/.../` as a dedicated regex literal token.
* the token payload should preserve the raw regex body rather than trying to
  reuse string escape decoding.
* the parser attaches a dedicated prefix parselet for that token, parallel to
  existing string, char, and number literal handling.
* the parselet expands the literal to an ordinary constructor application,
  e.g. `regex("...")`, rather than introducing a dedicated AST node.

## Language-level Representation

The simplest representation is to make regex a normal typedef in the preamble,
in the same spirit as `list` and `maybe`:

```fn
typedef regex { regex(string) }
```

That has a few advantages:

* the type-checker sees a distinct regex type without any special-case type
  rules.
* the parser can translate `#/.../` directly to a normal constructor
  application using existing AST forms.
* regex values can also be constructed explicitly in code with `regex(...)`,
  not only through literal syntax.
* there is no need for a dedicated runtime regex value type in the first
  implementation.

Under this scheme, `#/.../` is syntax sugar for constructing a `regex` value
from a `string` payload.

## Current Grammar

The currently implemented grammar is intentionally small and parser-oriented.
It is aimed at left-prefix recognition rather than at PCRE-style feature
parity.

In rough EBNF form:

```text
expression   := alternation
alternation  := sequence ("|" sequence)*
sequence     := quantified*
quantified   := primary ["*" | "+" | "?"]
primary      := literal
              | "."
              | "^"
              | "$"
              | "(" expression ")"
              | char_class
              | named_category
              | escaped_atom

char_class   := "[" "^"? class_item+ "]"
class_item   := literal
              | literal "-" literal
              | named_category
              | escaped_class_item

named_category := "[[" category_name "]]"
category_name  := "L" | "M" | "N" | "P" | "S" | "Z" | "C"
                | "Ll" | "Lm" | "Lo" | "Lt" | "Lu"
                | "Mc" | "Me" | "Mn"
                | "Nd" | "Nl" | "No"
                | "Pc" | "Pd" | "Pe" | "Pf" | "Pi" | "Po" | "Ps"
                | "Sc" | "Sk" | "Sm" | "So"
                | "Zl" | "Zp" | "Zs"
                | "Cc" | "Cf" | "Cn" | "Co" | "Cs"
```

The supported escapes are currently:

* `\uHEX...;` and `\UHEX...;` for Unicode code points, matching the string and
  char escape syntax
* `\n`, `\r`, `\t`
* `\d`, `\D`
* `\s`, `\S`
* `\w`, `\W`
* `\x` for any other character `x`, meaning "treat `x` literally"

Some important current semantics are worth stating explicitly:

* `.` matches any code point except newline and carriage return.
* `^` and `$` are start/end anchors for the whole input slice being matched.
* `[[Lu]]`, `[[Nd]]`, `[[L]]`, etc. can appear both as standalone atoms and
  inside bracket character classes.
* Character-class ranges are only literal-to-literal ranges such as `[a-z]`.
  Endpoints are not currently parsed from escapes or named categories.
* Inside regex literals, escapes are regex escapes, not string escapes. For
  example `#/\n/` means newline, not a backslash followed by `n`.
* `\w` is Unicode alphanumeric plus underscore.

The following are not part of the current grammar:

* counted repetition such as `{m}` or `{m,n}`
* non-greedy quantifiers such as `*?`
* capturing groups or backreferences
* inline flags and mode modifiers
* lookaround assertions

Those may still be reasonable extensions later, but they are not needed for
the initial parser-facing use case.

## Parser Integration

This section is about regex use by parsers written in F♮, not about more
integration into CEKF's own Pratt scanner/parser.

The Pratt-side literal work is already conceptually separate:

* the scanner recognizes a regex literal token.
* the Pratt parser lowers that literal to the ordinary constructor form
  `regex("...")`.
* after that point the regex is just a normal typed value flowing through the
  existing pipeline.

So phase 3 is really about the first runtime consumer API for regex values.
That probably does mean adding some basic operation at the language level,
whether as a builtin or as a small primitive exposed for parser combinators to
build on.

The main use case is parser construction, so prefix consumption should be the
primary semantic model.

A phase-3 operation over plain strings does not need any rollback machinery.
The language is pure, and matching against an immutable `string` does not
mutate parser state. It can simply return either failure or a decomposition of
the input into matched prefix and remaining suffix.

That suggests a first interface along the lines of:

```fn
regex_match: regex -> string -> maybe(#(string, string))
```

with the result interpreted as:

* `nothing` if the regex does not match the left prefix of the input string.
* `just(#(prefix, rest))` if it does, where `prefix ++ rest == input`.

That is already enough to support a parser-combinator style interface in pure
code. Unanchored search can still exist later as a separate helper, but it
should not be the default parser-facing operation.

The exact surface name should be chosen carefully. Bare `match` is probably not
ideal because the language already uses `match` as syntax, so something more
specific such as `regex_match` or `regex_prefix` is less confusing.

Regex-based parsers only need rollback once they are coupled to some external
or stateful input cursor.

That breakdown happens in the next phase, for example when parsing files,
streams, or other cursor-backed inputs where progress is represented by mutable
position rather than by returning an explicit suffix value.

In that later setting, a regex-backed parser cannot just consume input and
return a boolean. It needs to install back continuations that restore parser
state when control backtracks. This serves two purposes:

* it prevents failed alternatives from leaving the parser in a corrupted input
  state.
* it allows `amb` to explore regex-driven alternatives while the parser state
  is restored correctly between choices.

This makes regexes a reasonable substrate for parser combinators rather than a
side channel around them.

So for phase 3 the runtime contract can stay simple:

* regex value in.
* immutable input string in.
* either `nothing`, or `just(#(matched_prefix, remaining_suffix))` out.

Only after that, when regexes are attached to stateful parser inputs, does the
contract need to widen toward:

* current input state in.
* either failure, or a new input state plus matched value out.
* a back continuation installed whenever choice needs to restore the prior
  parser state.

That restoration mechanism becomes even more important if regex matching later
gains richer internal choice points or starts working over stream-backed input.

## Streaming

The existing implementation is array-oriented and already fits finite strings
well.

That is enough for the first language integration. Infinite streams remain a
useful goal, but they should be treated as a later extension with an explicit
cursor or resumable input abstraction rather than folded into the initial
literal and parser work.

## Near-term Implementation Checklist

### 1. Scanner token for `#/.../`

* Add a dedicated regex literal token in the Pratt token surface rather than
  trying to parse bare `/` contextually on the first pass.
* Update the scanner in [pratt_scanner.c](../src/pratt_scanner.c) to recognize
  `#/.../` directly, preserving the raw regex body.
* Do not reuse string escape semantics for the regex body. The regex compiler
  should continue to own regex escapes and character-class syntax.
* Report unterminated regex literals at scan time with parser context.
* If the token payload needs a new kind, update [pratt.yaml](../src/pratt.yaml)
  and regenerate rather than editing anything under `generated/` manually.

### 2. Parser and AST literal support

* Add a dedicated prefix parselet in [pratt_parser.c](../src/pratt_parser.c),
  parallel to `makeString`, `makeChar`, and `makeNumber`.
* Register the new token in the default Pratt table in
  [pratt_parser.c](../src/pratt_parser.c).
* Have the parselet synthesize an ordinary constructor call equivalent to
  `regex("...")` using existing AST nodes.
* Add `typedef regex { regex(string) }` to [preamble.fn](../src/preamble.fn)
  so the constructor is present during type checking.
* No dedicated regex AST expression arm should be necessary under this design.

### 3. Parser-facing runtime API

This is the first phase that is no longer about literal syntax.

By the end of phase 2, the compiler can already read `#/.../` and lower it to
`regex("...")`. What is still missing is an operation that does something with
that regex value at runtime.

So the real question for phase 3 is not "how does the Pratt parser recognise a
regex literal?" but rather "what is the first useful language-level operation
over values of type `regex`?"

The narrowest plausible answer is therefore a basic prefix-match builtin over
immutable strings, for example something in the spirit of `regex_match` or
`regex_prefix`. Parser combinators could then layer on top of that.

That is different from baking regex matching into the existing Pratt parser.
The existing Pratt machinery is only relevant here as a model for rollback and
state restoration if regex-backed parsers later participate in alternative
parsing and backtracking.

* Keep the existing helper API for general regex matching, but add a parser
  facing builtin whose first contract is prefix-only consumption over plain
  strings.
* A good first surface is `regex -> string -> maybe(#(string, string))`.
* That entry point should succeed only when the regex matches the current left
  prefix of the input string, returning matched prefix and remainder.
* Do not make unanchored search the default parser interface.
* Do not pull rollback machinery into this first string-only interface.
* When regexes are later applied to stateful parser inputs, reuse the existing
  lexer rollback pattern from [syntax_parse.c](../src/syntax_parse.c) rather
  than inventing a second, unrelated restoration mechanism.
* The existing `SyntaxLexerCheckpoint` shape in [pratt.yaml](../src/pratt.yaml)
  is a good model for that later phase: queued tokens, buffer cursor state,
  and panic mode all need to be restored together.

### 4. Deferred: `amb` and backtracking integration

This phase can be deferred for now.

It is still needed later if regex matching is applied to streams, files, or
other stateful cursor-backed parser inputs. It is just not part of the current
string-only phase-3 surface.

This phase is also about user-level parsing machinery rather than about the
existing Pratt expression parser itself.

It is separate from the simpler phase-3 string interface. If regex matching is
just `regex -> string -> maybe(#(string, string))`, there is no hidden state to
restore and therefore no special backtracking problem yet.

* Any regex-backed parser that can participate in choice must install back
  continuations that restore parser state before control resumes elsewhere.
* Restoration must include both consumed input position and any queued Pratt
  lexer lookahead.
* A failed regex branch must not leave partially consumed parser state behind.
* A successful regex branch that later backtracks through `amb` must restore
  the parser state captured before that branch consumed input.
* If the regex engine later exposes internal choice points, those should plug
  into the same restoration story rather than bypassing parser rollback.

### 5. Runtime representation and operations

The special lowering step is no longer necessary.

* Regex literals and explicit `regex(...)` constructor calls can flow through
  the normal pipeline as ordinary typed values.
* The runtime representation can just be the existing typedef/constructor
  encoding used for other algebraic data types.
* Operations that consume regexes can destructure that value, recover the
  underlying string payload, and compile it on demand.
* Compiled regex objects remain an implementation detail of the consuming
  operations, with caching added later only if needed.

This also removes the earlier type-bridge problem:

* builtin-facing operations can simply declare regex arguments at the language
  level.
* implementation code can unpack the constructor payload instead of accepting a
  separately lowered representation.

### 6. Validation

* Keep [tests/src/test_regex.c](../tests/src/test_regex.c) as the focused C
  validation surface for the standalone engine.
* Add parser-level tests for literal tokenization and AST construction.
* Add parser-combinator tests that prove prefix-only semantics.
* Defer `amb`/rollback regression tests until the stateful-input phase begins.
* A useful first focused check remains:

```bash
make tests/test_regex && tests/test_regex
```

* Once literal integration begins, follow that with the narrower affected test
  slice before falling back to full `make test`.

* Add at least one language test that proves `#/.../` and `regex("...")`
  produce interchangeable typed values.

### 7. Later work

* Bare `/.../` can be revisited later if expression-start aware lexing becomes
  worth the extra scanner complexity.
* Stream-backed input and infinite streams should wait until the finite-input
  parser contract and rollback story are stable.
* The deferred phase-4 rollback work will still be needed before regex is used
  directly against stateful parser cursors or external streams.
