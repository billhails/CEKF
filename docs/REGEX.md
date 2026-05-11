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

## Parser Integration

The main use case is parser construction, so prefix consumption should be the
primary semantic model.

A parser-facing regex operation should attempt to match at the current parser
position only. It should either fail immediately or succeed with a consumed
prefix length. Unanchored search can still exist as a separate helper, but it
should not be the default parser interface.

Regex-based parsers must also cooperate correctly with `amb`.

In practice that means the regex parser cannot just consume input and return a
boolean. It needs to install back continuations that restore parser state when
control backtracks. This serves two purposes:

* it prevents failed alternatives from leaving the parser in a corrupted input
  state.
* it allows `amb` to explore regex-driven alternatives while the parser state
  is restored correctly between choices.

This makes regexes a reasonable substrate for parser combinators rather than a
side channel around them.

The runtime contract should therefore be closer to:

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

* Keep the existing helper API for general regex matching, but add a parser
  facing entry point whose contract is prefix-only consumption.
* That entry point should succeed only when the regex matches at the current
  parser position, returning consumed length or an updated input state.
* Do not make unanchored search the default parser interface.
* Reuse the existing lexer rollback pattern from
  [syntax_parse.c](../src/syntax_parse.c) rather than inventing a second,
  unrelated restoration mechanism.
* The existing `SyntaxLexerCheckpoint` shape in [pratt.yaml](../src/pratt.yaml)
  is a good model: queued tokens, buffer cursor state, and panic mode all need
  to be restored together.

### 4. `amb` and backtracking integration

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
* Add `amb` regression tests that show parser state is restored correctly after
  regex-driven backtracking.
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
