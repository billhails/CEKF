# Regex Status

This note summarizes the current regex implementation as it exists in the
codebase now. It is intended as a status update to complement
`docs/REGEX.md`, which still contains older design discussion and now-outdated
future work.

## Summary

Regex support is already integrated at three levels:

* the engine compiles to a regex AST and matches against an abstract
  `RegexSource`.
* the runtime exposes both string-backed and file-backed regex matching.
* the F♮ library already contains reusable parser combinator modules for both
  deterministic and `amb`-based parsing, including regex-driven parsers.

For parser combinators, the important point is that this is no longer just a
prototype direction. There is already a small working library in `fn/` and it
is covered by focused tests.

## Engine and Source Model

The current matcher no longer depends on fully materialized null-terminated
input buffers as its only operating mode.

The key implementation split is:

* `src/regex_helper.c` and `src/regex_helper.h` implement regex compile and
  match logic.
* `src/regex_source.c` and `src/regex_source.h` implement the input-source
  abstraction used by the matcher.
* `src/regex.yaml` defines the generated data structures that back the source
  layer.

The source layer is cursor-based and offset-based:

* matching proceeds through `regexMatchSourcep(...)`.
* source access is routed through `regexSourceGet(source, position)`.
* file-backed cursor updates are routed through
  `regexSourceSetPosition(source, position)`.
* string-backed splitting and file-backed prefix extraction are routed through
  `regexSourceSplitAt(...)`.

The generated source types are:

* `RegexStringSource`: a grow-only cache backed by a CEKF string list tail.
* `RegexFileSource`: a grow-only cache backed by a file handle plus cached
  `fpos_t` positions.
* `RegexSource`: a tagged union over those two concrete source kinds.

Operationally, the model is:

* string input is consumed lazily from the list representation into a cached
  `CharacterArray`.
* file input is read incrementally from the `FILE *`, decoded into wide
  characters, and each cached character records the corresponding file
  position after that read.
* the matcher works in logical offsets rather than raw input pointers.
* the file source can restore the underlying file handle to a cached logical
  position.

That means the earlier design idea in `docs/REGEX.md` about adding a source
abstraction has already happened. Both string and file sources are now part of
the implementation surface.

## Language and Builtin Surface

The language-level `regex` type and `#/.../` literal syntax remain in place.
The current runtime entry points are registered in `src/builtin_regex.c`:

```fn
regex_match: regex -> list(char) -> maybe(#(list(char), list(char)))
regex_match_file: regex -> opaque:file -> maybe(list(char))
```

Their current behavior is:

* `regex_match` matches against a string source and returns the matched prefix
  plus the remaining suffix.
* `regex_match_file` matches against a file source and returns the matched
  prefix while advancing the file cursor on success.
* regex compilation is cached by pattern string in the builtin layer unless
  caching is explicitly disabled.

The matcher-facing entry point inside the engine is
`regexMatchSourcep(const Regex *pattern, RegexSource *source, Index
*matchLength)`.

## Current Grammar Support

The implemented grammar is still a deliberately small parser-oriented one.
In rough form, the supported surface is:

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
```

In practical terms, the current grammar includes:

* alternation, concatenation, grouping, and `*`, `+`, `?` quantifiers.
* `.`, `^`, and `$`.
* bracket character classes, including negated classes such as
  `[^[[Lu]]]`.
* named Unicode categories as both standalone atoms and class items.
* escapes including `\n`, `\r`, `\t`, `\d`, `\D`, `\s`, `\S`, `\w`,
  `\W`, `\u...;`, and `\U...;`.
* a leading whole-pattern `(?i)` case-insensitive flag.

Some current semantics are worth stating explicitly:

* `.` matches any code point except newline and carriage return.
* `^` and `$` are anchors for the whole input slice being matched.
* character-class ranges are literal-to-literal ranges such as `[a-z]`.
* escapes inside `#/.../` literals are regex escapes, not ordinary string
  escapes.
* only a leading whole-pattern `(?i)` flag is supported; unsupported flags
  such as `(?s)` are rejected with `REGEX_STATUS_INVALID_INLINE_FLAG`.

The current implementation still does not include counted repetition,
non-greedy quantifiers, capture groups, backreferences, lookaround, or scoped
inline modifiers.

## Unicode General Category Classes

Unicode general category support is a real part of the current grammar, not a
placeholder.

Named categories use the `[[...]]` form and are parsed into `RegexCategory`
nodes. The matcher then checks each candidate character with
`unicode_category(...)` from the Unicode support layer.

Two category shapes are supported:

* major categories such as `[[L]]`, `[[N]]`, `[[P]]`, and `[[Z]]`.
* exact subcategories such as `[[Lu]]`, `[[Ll]]`, `[[Nd]]`, `[[Pc]]`, and
  the other standard Unicode general-category codes listed in
  `docs/REGEX.md`.

The distinction matters in matching:

* major categories match by category family. For example, `[[L]]` matches any
  letter category, not just uppercase or lowercase letters.
* exact categories match only that specific category. For example, `[[Lu]]`
  matches uppercase letters and `[[Nd]]` matches decimal digits.

Current behavior confirmed by the implementation and tests includes:

* category atoms work directly in sequences, for example `^[[Lu]][[Ll]]+$`.
* category items work inside bracket classes.
* negated classes can contain category items.
* invalid category names such as `[[Qx]]` are rejected with
  `REGEX_STATUS_UNKNOWN_CATEGORY`.
* `(?i)` affects literal and range comparisons, but it does not reinterpret
  category membership. `(?i)^[[Lu]]+$` still means uppercase-letter category
  membership, so it matches `ABC` but not `abc`.

Compile-time status reporting is represented by `RegexStatus` in
`src/regex.yaml` and exported through the `REGEX_STATUS_*` macros in
`src/regex_helper.h`.

## Parser Combinator Status

Regex support for parser combinators is already present and usable.

The current reusable parser surfaces are:

* `fn/parsercore.fn`: shared higher-order helpers such as `map_with`,
  `pair_with`, `apply_with`, and `sequence_with`.
* `fn/parserdet.fn`: deterministic parser combinators built around
  `success(...)` and `failure(...)`.
* `fn/parseramb.fn`: nondeterministic combinators built around `amb`, `back`,
  and `cut`.
* `fn/parserdo.fn`: a small `pdo[...]` macro surface for parser do-notation.
* `fn/regexutils.fn`: regex/file helpers for `amb`-style rollback.

Regex integration is already part of those parser libraries rather than living
only in playground examples:

* `parserdet.match_regex` wraps `regex_match`.
* `parserdet.match_regex_file` wraps `regex_match_file`.
* `parseramb.match_regex` wraps `regex_match`.
* `parseramb.match_regex_file` saves and restores file position around
  backtracking.
* `regexutils.regex_match_file_amb` exposes the same file rollback behavior as
  a standalone helper.

So the status here is stronger than “there are experiments”. The old
`parser-playground.fn` and `parser-playground-amb.fn` files still exist, but
they now sit alongside a factored parser library that is already used in
tests.

## File Parsing and Backtracking

File-backed parsing is a first-class part of the current surface.

For deterministic parsing:

* `parserdet.match_regex_file` returns `success(#(matched, file))` or
  `failure(...)`.
* `parserdet.parse_complete_file` composes a parser with `file_eof`.

For `amb`-based parsing:

* `parseramb.match_regex_file` saves the current file position with `fgetpos`.
* on success it yields the match and arranges to restore the file position if
  backtracking later revisits that branch.
* on failure it restores the original file position before `back`.

That means the parser-facing file story is not just hypothetical. There is
already a concrete rollback-aware adapter layer for regex over files.

## Focused Validation in the Tree

The current codebase already has focused tests covering the main surfaces:

* `tests/src/test_regex.c` covers the regex engine directly.
* `tests/fn/test_regex_literal.fn` covers literal transport.
* `tests/fn/test_regex_match.fn` covers string-backed language-level matching.
* `tests/fn/test_regex_match_file.fn` covers file-backed matching.
* `tests/fn/test_regex_match_file_amb.fn` covers rollback-aware file matching.
* `tests/fn/test_parserdet.fn` covers deterministic parser combinators.
* `tests/fn/test_parserdet_file.fn` covers deterministic file parsing with
  regex parsers.
* `tests/fn/test_parseramb.fn` covers `amb`-based parser combinators.
* `tests/fn/test_parseramb_file.fn` covers `amb`-based file parsing with
  regex parsers.

Taken together, those tests support the current practical status:

* regexes are a language feature.
* the matcher works over both string and file sources.
* parser-combinator support is already implemented for both deterministic and
  nondeterministic styles.
* file-backed regex parsing already has a working cursor/rollback story.

## Practical Status

The current situation is best described like this:

* the regex engine has already crossed the boundary from string-only matching
  to a shared source abstraction.
* file-backed regex matching is already implemented, not planned.
* parser combinator support is already present as library code in `fn/`, not
  just as design sketches.
* the parser library is still small and likely not the final long-term API,
  but it is already real, reusable, and tested.

In short: regex support for parser combinators is complete enough to use now,
and the codebase already contains the beginnings of a proper parser-combinator
library built on top of it.
