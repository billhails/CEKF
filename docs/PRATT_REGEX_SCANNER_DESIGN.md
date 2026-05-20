# Pratt Scanner Regex Integration

This note investigates using the existing regex engine inside the core Pratt
scanner to replace the hand-written state-driven number and string recognizers
in `src/pratt_scanner.c`.

The short version is:

* number scanning is a good fit for regex-based recognition.
* string and char scanning are only a partial fit, because the current scanner
  also performs escape decoding and detailed error recovery.
* a third `RegexSource` leg is not required for a first experiment, but a
  zero-copy source path is required for any sensible final implementation.

## Current Scanner Surface

The relevant scanner entry points are:

* `parseNumeric(...)` for numeric lexemes.
* `parseString(...)` for string and char literals.
* `parseRegex(...)` for `#/.../` literals.
* `next(...)` for top-level scanner dispatch.

The current Pratt buffer model is already close to what the regex engine wants:

* `PrattBuffer` stores a contiguous `WCharVec`.
* `buffer->start` is a moving `Character *` into that contiguous data.
* the remaining scanner input is null-terminated because the backing
  `WCharVec` is null-terminated.

That means scanner input is already available as a contiguous wide-character
slice.

## Current Regex Interface Fit

The regex engine already has three relevant entry surfaces:

* `regexMatchp(const Regex *pattern, const Character *text, ...)`
* `regexMatchCharArrayp(const Regex *pattern, CharacterArray *text, ...)`
* `regexMatchSourcep(const Regex *pattern, RegexSource *source, ...)`

For a quick prototype, the scanner could call `regexMatchp(...)` directly on
`buffer->start`.

That does work functionally, but it is not zero-copy. `regexMatchp(...)`
currently copies the entire remaining null-terminated text into a
`CharacterArray` before matching.

So the current situation is:

* the regex engine can already be applied to Pratt buffers immediately.
* the current direct path copies the remaining scanner tail.
* the existing no-copy source path cannot be used directly because it accepts
  `CharacterArray`, while Pratt buffers are backed by `WCharVec`.

## Prefix Matching Versus Search

The scanner needs left-prefix recognition.

The current matcher entry point `regexMatchSourcep(...)` is a search routine:
it tries the pattern at offset `0`, then `1`, then `2`, and so on until it
finds a match or reaches end-of-input.

For scanner use, that means:

* anchoring a pattern with `^` is functionally correct.
* even with `^`, the current search loop still retries later offsets after a
  failed prefix attempt.

That is unnecessary work for tokenization. If regex becomes part of the Pratt
scanner, a dedicated prefix-only matcher would be a better fit than routing
scanner code through the generic search API.

A minimal helper would look like:

```c
bool regexMatchPrefixSource(const Regex *pattern, RegexSource *source,
                            Index *matchLength);
```

or, if the first step stays on contiguous text rather than `RegexSource`:

```c
bool regexMatchPrefixText(const Regex *pattern, const Character *text,
                          Index *matchLength);
```

## Numeric Literals

### Feasibility

Numeric scanning is the best target for the first integration step.

The current numeric scanner is mostly doing two jobs:

* recognizing the token boundary.
* converting the matched slice into `MaybeBigInt` or irrational form.

That split already exists in the code. `parseNumeric(...)` identifies the
lexeme, and the conversion logic lives in `makeMaybeBigInt(...)` and
`makeIrrational(...)`.

Regex can replace the boundary-recognition part while leaving the existing
numeric conversion code in place.

### Important Compatibility Detail

The current state machine is more permissive than a cleaned-up numeric regex
would naturally be.

Notable current behavior includes:

* `0` receives special treatment that other Unicode decimal digits do not.
* hexadecimal literals are ASCII-only after `0x` or `0X`.
* floats such as `1.` and `0.` are accepted.
* underscores are accepted in places that may not all be intentional.
* `i` is handled as an optional suffix for imaginary literals.

Because of that, the first design decision is not technical but semantic:

* either preserve current scanner behavior exactly.
* or use the regex migration to tighten the intended numeric grammar.

The safer path is to decide this explicitly before changing the scanner.

### Recommended Scanner Regex Split

Because only ASCII `0` gets the special zero/hex prefix behavior, the easiest
way to preserve the current shape is to choose the regex based on the first
character rather than forcing everything through one pattern.

Recommended split:

* if the first character is ASCII `0`, use a zero-prefixed pattern.
* otherwise, use a decimal pattern.

Candidate patterns, written in the current regex surface rather than C string
escaping, are:

```text
zeroOrHex := ^0((x|X)[0-9A-Fa-f_]*i?|([.]\d*)?i?)?
decimal   := ^\d[\d_]*([.]\d*)?i?
```

This is still only a proposal. The exact pattern should be chosen after
deciding whether to preserve the current permissive edge cases or intentionally
trim them.

### Recommended Numeric Integration

The cleanest first step is:

1. Compile the scanner numeric regexes once.
2. Match against `buffer->start`.
3. Use only the matched length from the regex.
4. Keep `makeMaybeBigInt(...)` and `makeIrrational(...)` unchanged.

That minimizes behavioral change by keeping token interpretation in one place.

## Strings and Chars

### Why Strings Are Harder

`parseString(...)` is not only identifying token boundaries. It is also:

* decoding escapes into a `WCharArray`.
* handling `\u...;` and `\U...;` escapes.
* reporting specific scanner errors.
* tracking line numbers on malformed multi-line input.
* handling char-specific conditions such as empty chars and missing
  terminators.

So string handling is not just a recognizer. It is part recognizer, part
decoder, and part error-recovery routine.

### Where Regex Still Helps

Regex can still help as a valid-lexeme fast path.

A practical split would be:

* regex determines whether the next token is a lexically valid complete string
  or char literal, and returns its length.
* existing or slightly refactored C code performs escape decoding over the
  matched slice.
* malformed literals continue to use the current hand-written path so that the
  scanner keeps its existing diagnostics.

That avoids forcing detailed error reporting into the regex layer.

### Candidate Fast-Path Patterns

Valid double-quoted string, using current string semantics:

```text
^"([^"\\\n]|\\([^uUntr\n]|[ntr]|[uU][0-9A-Fa-f]+;))*"
```

Valid single-quoted char literal:

```text
^'([^'\\\n]|\\([^uUntr\n]|[ntr]|[uU][0-9A-Fa-f]+;))'
```

These are intended only for the successful fast path.

They do not replace the need for the current state machine on malformed input,
because today malformed unicode escapes and unterminated literals still
produce specific scanner errors and partial recovery behavior.

## Regex Literals

`parseRegex(...)` is already small and specialized.

It preserves regex-literal transport semantics rather than string-literal
semantics, especially around `\/` handling and preservation of regex escape
content.

There is little benefit in rewriting that path via regex. It is reasonable to
leave regex-literal scanning hand-written even if numbers and strings move
toward regex-assisted recognition.

## Source-Layer Options

### Option A: No New Source Type Yet

For an experiment, call the regex engine directly on `buffer->start` via
`regexMatchp(...)`.

Advantages:

* smallest change.
* proves whether scanner regexes improve clarity and maintainability.
* does not require touching `regex.yaml` or the source abstraction.

Disadvantage:

* copies the entire remaining Pratt buffer tail on every match attempt.

This is acceptable only as a proof-of-concept technique. It is not an
acceptable steady-state scanner architecture, because a final implementation
cannot afford to copy the remaining Pratt buffer tail on each match attempt.

### Option B: Add a Third Regex Source Leg

If the scanner is going to rely on regex matching long-term, a third source
variant or equivalent zero-copy path is required.

The important recommendation is to keep it generic rather than Pratt-specific.
The source should represent an existing contiguous character buffer, not a
Pratt-only concept.

In shape, that wants something closer to:

```yaml
structs:
  RegexTextSource:
    data:
      data: WCharVec
      start: index
      exhausted: bool=true

unions:
  RegexSource:
    data:
      string: RegexStringSource
      file: RegexFileSource
      text: RegexTextSource

external:
  - !include cekfs.yaml
  - !include utils.yaml
```

The implementation would then be thin:

* `regexSourceGet(...)` returns `data->entries[start + position]` until it
  reaches `L'\0'`.
* `regexSourceSetPosition(...)` is a no-op for this source kind.
* scanner code constructs the source from the existing `PrattBuffer` without
  copying characters.

### Why This Should Not Store Only a Raw Pointer

A raw `Character *` is enough to read characters, but it is not an ideal
stored representation for a GC-managed source object.

Holding the owning `WCharVec` plus a start offset is safer because:

* the owner object remains visible to the GC.
* the source remains generic over any existing contiguous wide-character
  buffer.
* the source does not have to know about Pratt-specific structs.

## Compiled Regex Lifetime

If the scanner compiles regexes once and reuses them, those compiled regexes
must be rooted.

The builtin regex cache already does this via `markRegexCache()` during GC.
Scanner-owned compiled regexes would need a similar arrangement.

That means a production implementation should include either:

* a dedicated scanner regex cache with a mark hook.
* or compile-on-demand plus immediate use for the first iteration, then add
  caching once the scanner patterns have settled.

## Recommended Phasing

### Phase 1

Replace numeric recognition only.

* keep `parseNumeric(...)` as the entry point.
* replace its hand-written boundary-recognition loop with regex matching.
* continue to use the current conversion helpers.
* use the existing raw-text regex path first, even though it copies.

This is the highest-value, lowest-risk step, but it should be treated
explicitly as a temporary proof of concept rather than the intended end state.

### Phase 2

Add a prefix-only regex matcher entry point.

That avoids routing scanner tokenization through the generic search API.

### Phase 3

Add a generic contiguous-text `RegexSource` variant.

That removes the Pratt-buffer copy while keeping the source abstraction clean.
This phase is not an optional optimization pass. It is the point where the
scanner integration becomes architecturally viable as a final implementation.

### Phase 4

Optionally add a string fast path.

At this stage the recommended approach is still:

* regex for valid-lexeme recognition.
* hand-written decode and error handling for semantics and diagnostics.

## Risks

The main risks are semantic, not mechanical.

* numeric-literal compatibility may change if the regex surface is cleaner
  than the current state machine.
* string diagnostics may regress if regex tries to replace error-oriented
  scanning logic instead of only supplementing it.
* any scanner path that keeps the copy inside `regexMatchp(...)` is suitable
  only for experiments, not for the final implementation.
* a scanner-side compiled-regex cache needs GC rooting.

## Recommendation

The proposal is sound, but it should be staged.

Recommended final position:

* use regex to replace numeric lexeme recognition first.
* do not try to replace string decoding and error handling wholesale.
* keep `parseRegex(...)` hand-written.
* do not require a third `RegexSource` leg for the first experiment.
* require a generic contiguous-text source or equivalent zero-copy path before
  treating the scanner integration as complete.
* pair that zero-copy path with a prefix-only matcher so the scanner can use
  the regex engine without copying or search-loop overhead.

That path gets most of the maintainability benefit while keeping scanner
behavior understandable and testable.
