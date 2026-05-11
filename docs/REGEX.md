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
* Regexes should be their own expression type in the language, distinct from strings, and using the perl/javascript `/.../` delimiters.

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
* the parselet produces a regex expression node rather than lowering to a
  string.

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
