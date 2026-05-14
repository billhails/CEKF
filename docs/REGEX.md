# Regular Expression Support

Regex support is no longer just a standalone engine.

The current system already includes:

* a unicode-centric regex compiler and matcher over `wchar_t`-style
  `Character` arrays.
* a language-level `regex` type in the preamble.
* regex literal syntax using `#/.../`.
* a parser-facing builtin, `regex_match`, for left-prefix matching on
  immutable strings.

That is enough to use regexes directly from F♮ code and to start building
parser combinators on top of them.

The main implementation surface is:

* [regex.yaml](../src/regex.yaml)
* [regex_helper.c](../src/regex_helper.c)
* [regex_helper.h](../src/regex_helper.h)
* [builtin_regex.c](../src/builtin_regex.c)
* [builtin_regex.h](../src/builtin_regex.h)
* [pratt_scanner.c](../src/pratt_scanner.c)
* [pratt_parser.c](../src/pratt_parser.c)
* [preamble.fn](../src/preamble.fn)

Supporting Unicode and representation-translation code lives in:

* [unicode.c](../src/unicode.c)
* [unicode.h](../src/unicode.h)
* [cekf.c](../src/cekf.c)
* [cekf.h](../src/cekf.h)

## Current Status

The following pieces are implemented now:

* Regexes are a distinct language-level type:

```fn
typedef regex { regex(string) }
```

* The Pratt scanner recognizes `#/.../` as a dedicated regex literal.
* The Pratt parser lowers that literal to the ordinary constructor form
  `regex("...")`.
* Explicit constructor use and literal use are interchangeable typed values.
* The language exposes `regex_match` with the current type:

```fn
regex_match: regex -> string -> maybe(#(string, string))
```

* `regex_match` is left-prefix only. It returns either:
  * `nothing` if the regex does not match the current left prefix.
  * `just(#(matched_prefix, remaining_suffix))` if it does.

This is the intended parser-facing first step: pure, prefix-oriented, and free
of hidden parser-state mutation.

## Scanner and Literal Syntax

The current literal surface is `#/.../` rather than bare `/.../`.

That choice remains deliberate:

* it avoids contextual lexing for bare slash.
* it avoids conflict with comment and operator syntax.
* it keeps regex literals distinct without introducing a dedicated regex AST
  expression form.

The current implementation shape is:

* the scanner recognizes `#/.../` directly.
* the token payload preserves regex escapes rather than reusing ordinary string
  escape decoding.
* `\/` is treated specially only so `/` can appear inside the literal body.
* the parser lowers the token to `regex("...")` using ordinary AST
  constructor application.

So regex literals are syntax sugar over the ordinary regex constructor, not a
second representation.

## Language-level Representation

The language-level representation is still intentionally simple:

```fn
typedef regex { regex(string) }
```

That means:

* the type-checker sees a real distinct `regex` type.
* there is no dedicated regex expression node in the front-end.
* runtime consumers can unpack the constructor payload and compile on demand.

At present, `regex_match` is the main consumer. It destructures the regex
value, converts both pattern and input string to `CharacterArray`, compiles the
pattern, and returns the matched prefix plus remainder on success.

Compiled regex objects are still an internal implementation detail of the
consumer API. There is no persistent compiled-regex language value yet.

## Current Grammar

The current grammar is intentionally small and parser-oriented. It is aimed at
left-prefix recognition rather than PCRE-style feature parity.

In rough EBNF form:

```text
pattern        := leading_flags? expression
leading_flags  := "(?" flag+ ")"
flag           := "i"

expression     := alternation
alternation    := sequence ("|" sequence)*
sequence       := quantified*
quantified     := primary ["*" | "+" | "?"]
primary        := literal
                | "."
                | "^"
                | "$"
                | "(" expression ")"
                | char_class
                | named_category
                | escaped_atom

char_class     := "[" "^"? class_item+ "]"
class_item     := literal
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
  char escape syntax.
* `\n`, `\r`, `\t`.
* `\d`, `\D`.
* `\s`, `\S`.
* `\w`, `\W`.
* `\x` for any other character `x`, meaning "treat `x` literally".

The supported leading inline modifier syntax is currently:

* `(?i)` for whole-pattern case-insensitive matching.

Important details of the current modifier behavior:

* `(?i)` must appear at the beginning of the pattern.
* it applies to the whole pattern, not to a nested subexpression.
* only `i` is currently accepted.
* unsupported flags such as `(?s)` are rejected with
  `REGEX_STATUS_INVALID_INLINE_FLAG`.

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
* `(?i)` affects literal comparison and literal ranges by simple Unicode
  folding, but it does not reinterpret named Unicode categories. For example
  `(?i)[[Lu]]` still means uppercase-letter category membership, not
  "any letter regardless of case".

The following are not part of the current grammar:

* counted repetition such as `{m}` or `{m,n}`.
* non-greedy quantifiers such as `*?`.
* capturing groups or backreferences.
* scoped or negated inline modifiers such as `(?i:...)` or `(?-i)`.
* inline flags other than `i`.
* lookaround assertions.

Those may still be reasonable extensions later, but they are not required for
the current parser-facing use case.

## Parser-facing Integration

Regex support is already integrated far enough to be useful in F♮ parser code.

The current flow is:

* source code contains a literal such as `#/\d+/`.
* the Pratt scanner emits a regex token.
* the Pratt parser lowers it to `regex("\\d+")`.
* the typed program carries that value through the normal pipeline.
* runtime code calls `regex_match` to consume a left prefix of a string.

That means regex is already a language-level feature rather than just an
internal helper library.

For parser combinators over immutable strings, this is already enough.
`regex_match` gives a pure decomposition of the input into matched prefix and
remainder, which composes naturally with success/failure-based parser code.

That exploration is already underway in:

* [parser-playground.fn](../fn/parser-playground.fn), which sketches a more
  deterministic `try`-style parser-combinator surface.
* [parser-playground-amb.fn](../fn/parser-playground-amb.fn), which explores a
  nondeterministic `amb`-backed parser style.

Those files are not yet a settled library surface, but they are already a good
source of concrete design pressure for what the eventual parser API should look
like.

## Streaming and Stateful Parsing

The current integration is intentionally string-oriented.

That is sufficient for the first parser-facing phase because immutable strings
do not require any rollback or checkpoint machinery. Regex matching just
returns a suffix instead of mutating parser state.

That also means some larger goals are still deferred:

* stream-backed input.
* infinite or resumable input sources.
* regex parsers that consume stateful cursors.
* `amb` integration and rollback across regex-consuming parser branches.

Those problems remain real, but they belong to a later input-abstraction phase
instead of the current string-only surface.

## Validation

The current focused validation surface is:

* [test_regex.c](../tests/src/test_regex.c) for the standalone engine.
* [test_regex_literal.fn](../tests/fn/test_regex_literal.fn) for literal and
  constructor interchangeability.
* [test_regex_match.fn](../tests/fn/test_regex_match.fn) for language-level
  prefix matching.

Useful focused checks are:

```bash
make tests/test_regex && tests/test_regex
make bin/fn && ./bin/fn --include=fn --assertions-accumulate tests/fn/test_regex_literal.fn
./bin/fn --include=fn --assertions-accumulate tests/fn/test_regex_match.fn
```

These currently cover:

* Unicode category classes.
* grouping, alternation, and quantifiers.
* regex literal transport.
* `regex_match` prefix semantics.
* `\n`, `\r`, `\t`, `\u...;`, and `\U...;` escapes.
* `(?i)` case-insensitive matching, including Unicode literals.
* rejection of unsupported inline flags such as `(?s)`.

## Next Steps

The next practical steps are now less about regex existence and more about
building on the current language surface.

The most natural near-term directions are:

1. Consolidate the current parser-combinator experiments in
  [parser-playground.fn](../fn/parser-playground.fn) and
  [parser-playground-amb.fn](../fn/parser-playground-amb.fn) into a smaller,
  reusable library surface.
2. Decide whether parser-facing ergonomics need one or two more regex helpers,
   for example explicit whole-string matching or explicit unanchored search as
   separate operations.
3. Design a cached indexed input abstraction for strings and files before
   attempting rollback-aware regex integration with `amb`.
4. Only then revisit broader regex surface extensions such as additional inline
   flags, counted repetition, or stream-aware matching.

Step 3 is the most interesting architectural next move.

The current `regex_match` path is eager: it converts the entire input
`list(char)` to a `CharacterArray` before matching. That is simple, but it is
also the main reason the current string-only surface does not yet generalize to
files or streams cleanly.

A plausible refactoring direction is:

* move `regex_helper` away from raw `const Character *` input and toward an
  abstract indexed character source.
* give that source a growable `CharacterArray` cache plus a forward-only source
  cursor.
* satisfy character requests on demand: if index `i` is already cached, read it
  from the buffer; otherwise extend the buffer until `i` is available or the
  source is exhausted.
* treat end of source as `L'\0'` at and beyond the exhaustion point, matching
  the current engine's null-terminated contract.

For the string case, that source could contain:

* the unconsumed `list(char)` tail.
* the cached `CharacterArray` built so far.

For a file-backed variant, the same surface could be implemented over `FILE *`
using `getwchar` to extend the cache.

There is one important constraint from the current matcher design: the engine
does revisit earlier buffered positions during alternation, concatenation, and
repetition. Today those positions are represented as raw pointers into a fully
materialized input buffer. So although the source cursor itself can move only
forward, the cache should remain grow-only for the duration of a single match.
Discarding earlier buffered characters mid-match is not yet safe without extra
liveness tracking of the earliest still-referenced position.

That suggests a good first refactor boundary:

* replace pointer-based `RegexPosition` values with stable logical offsets.
* route character access through a source interface such as "get character at
  offset".
* keep the cache append-only for one match.
* postpone prefix eviction and bounded-memory optimization until after the
  abstraction is working.

That abstraction should stay type-safe.

In particular, it would be better to avoid a generic `void *` cookie plus
untyped callback table if possible. A more CEKF-shaped design would be:

* one abstract `RegexSource` surface used by the matcher.
* an explicit source-kind tag such as `REGEX_SOURCE_TYPE_STRING` or
  `REGEX_SOURCE_TYPE_FILE`.
* a tagged union of concrete source payloads, for example one arm for the
  string-backed cache state and one arm for the file-backed cache state.
* helper functions that switch on the source kind and then operate on the
  correctly typed concrete payload.

Since this repo already uses schema-driven generated structs, unions, and
constructors, the concrete source state should probably live in `regex.yaml`
rather than being handwritten in C. In rough YAML terms, the intended
direction is closer to:

```yaml
structs:
  RegexStringSource:
    data:
      tail: Vec # from cekfs.yaml
      cache: CharacterArray
      exhausted: bool=false

  RegexFileSource:
    data:
      handle: file
      cache: CharacterArray
      exhausted: bool=false

unions:
  RegexSource:
    data:
      string: RegexStringSource
      file: RegexFileSource

external:
  - !include cekfs.yaml
```

That would give the implementation the usual generated utilities for free:

* GC-aware storage for the source state itself.
* generated constructors and accessors.
* explicit tagged-union switching without untyped callback cookies.

The required matcher-facing operation is narrower than a full slice API.

At the engine boundary, the essential operation is just something like
`regexSourceGet(source, offset)`, together with end-of-source behavior
equivalent to `L'\0'`.

The exhaustion rule can stay concrete and source-specific:

* for `RegexStringSource`, `tail` is the ordinary CEKF list encoding carried in
  a `Vec`: either a 3-vector for pair/cons cells or a 1-vector for the null
  constructor. Reaching the null case means no further characters are available
  and should set `exhausted = true`.
* for `RegexFileSource`, `getwchar` advances the backing file handle; if it
  returns `WEOF`, that should set `exhausted = true`.

So the safer contract is simply:

* keep the core source interface focused on indexed character access.
* have each concrete source update `exhausted` when its underlying input is
  truly exhausted.
* leave range extraction or other convenience operations out of the interface
  unless they become necessary later.

In C terms, the minimal matcher-facing surface is probably closer to:

```c
typedef Index RegexPosition;

Character regexSourceGet(RegexSource *source, RegexPosition offset);
```

If a function-pointer typedef is wanted for documentation purposes, it should
still carry the logical offset:

```c
typedef Character (*RegexGetter)(RegexSource *source,
                                 RegexPosition offset);
```

Even so, the ordinary helper function form is probably the better fit here than
storing a callback inside the source itself: the current design already prefers
an explicit tagged `RegexSource` plus a type-safe switch over source kind.

Once that input abstraction exists, separate builtins still make sense:

* keep `regex_match` as the string-facing prefix matcher.
* add a separate `regex_match_file` builtin for file-backed sources.

That keeps the regex engine itself focused on the abstract source interface,
while file-position management such as `fgetpos` and `fsetpos` stays in the
file-facing layer rather than leaking into `regex_helper`.

### Making `amb` work with file-based regex matching

The most promising near-term route is to keep `regex_match_file` itself
deterministic and layer `amb` behavior on top in F♮ code.

That is a better fit for the current architecture:

* `regex_match_file` can stay a simple prefix matcher over a file-backed
  source.
* ordinary match failure can remain `nothing` rather than becoming an implicit
  `back`.
* rollback policy can live at the parser layer, alongside other `amb`-driven
  control flow.

In that shape, the helper should save the file position before attempting the
match, translate immediate failure into rewind-plus-`back`, and also rewind if
later backtracking revisits a successful match.

In rough F♮ terms:

```fn
fn regex_match_file_amb(re, file) {
    let pos = fgetpos(file);
    in switch (regex_match_file(re, file)) {
        (just(matched)) {
            matched then {
                fsetpos(file, pos);
                back
            }
        }
        (nothing) {
            fsetpos(file, pos);
            back
        }
    }
}
```

This is intentionally an `amb` adapter, not a replacement for the base builtin.
The builtin itself should stay useful in deterministic code, while the helper
provides the parser-facing rollback semantics.

Two practical notes follow from that:

* this approach needs language-visible `fgetpos` and `fsetpos` operations,
  together with a `filepos` value surface.
* if this is later spelled as a macro, the expansion should evaluate the file
  expression exactly once.

That gives file-backed regex parsing a viable `amb` story without forcing the
regex engine itself to understand backtracking continuations.
