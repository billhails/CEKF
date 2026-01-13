# Proposal to Simplify the Parser's Unicode Handling

In a nutshell the parser makes extensive use of bespoke UTF8/Unicode transforms in [utf8.c](../src/utf8.c).
This was done simply because I was unaware that there is good support for Unicode in modern libc, and that support is not restricted to UTF8.

The general plan is to have the parser read multibyte encoded text but store `wchar_t` strings - the PrattBuffer will contain a `wchar_t` array instead of the current `ustring`. All of those complex state table-driven routines in the scanner can be greatly simplified or removed, and all the bespoke UTF8 support can later be removed.

There are some caveats however. I don't want the `wchar_t` to leak out into HashSymbols, they should continue to be multibyte (though the precise encoding can be left up to libc, in my case on Linux it should remain UTF8 I would hope.) A later effort might extend `wchar_t` throughout, but then what about all the error messages etc.

I'd like to attempt a "maximum benefit for minimum effort" approach by containing the changes entirely within the parser subsystem, then evaluating the benefits and seeing if it's worth going further.

So PrattTries would also need to become `wchar_t` while hash symbols remain UTF8. Hopefully the C compiler will catch any mismatches.

Overall the approach will be to push UTF8 to the edges of the parser:
textfile input and HashSymbol output. Everything else within the parser should be unicode.

## Detailed Implementation Steps

### Phase 1: Data Structure Changes

1. **Update primitives.yaml**
   - Add a new primitive type `wstring` for wide character strings: `wchar_t *`
   - Define appropriate printf format (likely `%ls` for wide strings)
1. **Update pratt.yaml**
   - Change `PrattBuffer.data` from `ustring` (`unsigned char *`) to `wstring` (`wchar_t *`)
   - Change `PrattBuffer.start` from `ustring` to `wstring`
   - Change `PrattTrie.character` from `byte` to `wchar_t`
   - Replace `PrattUTF8` array with `PrattUnicode` throughout
     - `PrattValue.string` changes from `PrattUTF8` to `PrattUnicode`
     - `PrattValue.character` changes from `PrattUTF8` to `character` (`wchar_t`)
     - Remove `PrattUTF8` array definition entirely
   - Regenerate code with `make` to get updated structures

### Phase 2: Buffer Initialization and File Reading

1. **Modify buffer loading in pratt_scanner.c**
   - Update `tokenFromString()` and string parsing functions to use `PrattUnicode`
   - Update `readFile()` which currently uses `fread()` to read entire file into byte buffer
   - After reading with `fread()`, convert the multibyte buffer to `wchar_t` array using `mbstowcs()`
   - Allocate wide character buffer: size is `mbstowcs(NULL, byteBuf, 0)` plus one for null terminator
   - Set locale at initialization (in `main()` or parser init): `setlocale(LC_ALL, "C.UTF-8")` to mandate UTF-8 encoding
   - Add encoding error detection: if `mbstowcs()` returns `(size_t)-1`, fail with clear message indicating encoding mismatch
   - Update `prattBufferFromFileName()` to return buffer containing `wchar_t *` instead of `unsigned char *`
   - Store encoding choice in parser state for use in error messages
   - Remove `PrattUTF8ToUnicode()` conversion function from pratt_parser.c once it is no longer used.
   - **Special case**: The preamble is hard-coded as an internalized C string and always parsed as UTF-8 regardless of `--encoding` flag (consider restricting preamble to 7-bit ASCII for maximum compatibility)
1. **Handle buffer capacity**
   - Review all buffer allocation/reallocation code
   - Ensure size calculations multiply by `sizeof(wchar_t)` where appropriate
   - Check buffer growth logic in scanner

### Phase 3: Character Processing Functions

1. **Simplify character navigation in pratt_scanner.c**
   - Replace `nextCharacter()` which currently uses `utf8Sgetc()` with simple pointer increment
   - Since `wchar_t` arrays are fixed-width, `buffer->start++` moves one character
   - Remove complex UTF-8 state machine logic from character reading
1. **Update character classification functions**
   - Replace custom `utf8_isalpha()`, `utf8_isspace()`, `utf8_isdigit()`, etc. with standard `iswalpha()`, `iswspace()`, `iswdigit()` from `<wctype.h>`
   - These functions already handle Unicode categories correctly
   - Remove dependencies on `unicode.h` and Unicode property tables where used by scanner
1. **Update trie operations**
   - Modify `lookUpTrie()` and `insertTrie()` functions to work with `wchar_t` keys instead of `byte`
   - Comparison operations become simpler: direct `wchar_t` comparison instead of UTF-8 byte sequence comparison
   - Update ordering/sorting logic in trie siblings for `wchar_t`

### Phase 4: String Building and Token Generation

1. **Update token value creation**
   - In `parseIdentifier()`, `parseString()`, `parseNumeric()`, etc., tokens are built from buffer ranges
   - Need to convert `wchar_t` substrings to multibyte strings when creating `HashSymbol` entries
   - Add conversion helper: `wchar_t *` to `char *` using `wcstombs()`
   - Update `newSymbolFromLength()` calls or create `newSymbolFromWideLength()` wrapper
1. **Handle string literals**
   - `parseString()` currently builds strings character by character
   - Update to work with `wchar_t` source but produce multibyte output
   - Escape sequence handling should work similarly but with wide chars
1. **Handle character literals**
    - Character tokens (parsed by `parseCharacter()`) currently return `Character` type (typedef'd to `wchar_t`)
    - This should work naturally with new `wchar_t` buffer
    - Ensure the value extraction logic handles wide chars properly

### Phase 5: Integration Points

1. **Symbol table interface**
    - The symbol table (`HashSymbol`) stores `char *` (multibyte strings)
    - All conversions from `wchar_t` to `char *` should happen at symbol creation time
    - Verify `newSymbol()` and `newSymbolFromLength()` receive proper multibyte input
    - Create wrapper functions if needed: `newSymbolFromWide()`, `newSymbolFromWideLength()`
1. **Token printing and debugging**
    - Update `printPrattToken()` and related debug functions
    - Ensure `wchar_t` buffers print correctly in debug output
    - May need `%ls` format specifiers or explicit conversion
1. **Error reporting**
    - Parser error messages include source snippets from buffers
    - Ensure error display converts `wchar_t` buffer contents to multibyte for output
    - Check `errorPrattParser()` and related functions
    - Add specific error message for encoding failures:

```text
      Error: File 'example.fn' contains invalid UTF-8 sequences at line X
      Hint: Ensure source files are UTF-8 encoded, or use --encoding flag
```

### Phase 6: Cleanup and Testing

1. **Remove UTF-8 dependencies from scanner**
    - Once scanner works with `wchar_t`, remove includes of `utf8.h` from `pratt_scanner.c`
    - Remove calls to custom UTF-8 functions: `utf8_isalpha()`, `utf8Sgetc()`, etc.
    - Keep `utf8.c` for now as it's used elsewhere (runtime string operations, etc.)
1. **Update header files**
    - Add `#include <wchar.h>` and `#include <wctype.h>` to `pratt_scanner.c`
    - Add `#include <locale.h>` for `setlocale()`
    - Remove unnecessary UTF-8 related includes
1. **Testing**
    - Run existing test suite: `make test`
    - Test with Unicode identifiers, operators, and string literals
    - Test with files in different locales (if applicable)
    - Verify error messages display correctly
    - Check all operator definitions still parse correctly (especially Unicode operators)

### Phase 7: Command-Line Options and Configuration

1. **Add encoding command-line option**
    - Add `--encoding=<name>` flag to override default UTF-8
    - Supported values: `UTF-8` (default), `ISO-8859-1`, `CP1252`, etc.
    - Implementation: translate flag to appropriate `setlocale()` call
    - Example: `./bin/fn --encoding=ISO-8859-1 legacy.fn`
    - Document in `command-line.md` wiki page

### Phase 8: Documentation and Evaluation

1. **Update documentation**
    - Document that source files must be UTF-8 encoded (matching existing practice with Unicode operators in preamble.fn)
    - Document `--encoding` flag for legacy file support
    - Note the boundary: parser uses `wchar_t`, symbol table uses UTF-8 multibyte
    - Update any developer documentation about the scanner
1. **Performance evaluation**
    - Measure parsing time before and after
    - Check memory usage (wide chars use more space)
    - Evaluate code complexity reduction
1. **Decide on further expansion**
    - If benefits are clear, consider extending `wchar_t` to more subsystems
    - If problems arise, document lessons learned
    - Consider whether to keep multibyte in symbol table or convert entire system

## Risks and Considerations

1. **UTF-8 mandate**: Source files must be UTF-8 encoded. Legacy files in other encodings require explicit `--encoding` flag. This matches modern practice (most languages mandate UTF-8) but may require user education.
2. **Memory usage**: `wchar_t` is typically 4 bytes vs 1 byte for UTF-8. Parser buffers will use 4x memory. However, this is temporary (during parsing only) and modern systems have plenty of RAM.
3. **Symbol table duplication**: If same identifier appears in both `wchar_t` buffer and multibyte symbol table, conversions happen repeatedly. This might add overhead.
4. **Platform differences**: `wchar_t` size and encoding varies by platform (UTF-32 on Linux, UTF-16 on Windows). Code must handle this portability concern.
5. **Conversion overhead**: Every token requires `wchar_t` to multibyte conversion at symbol creation time. This is likely negligible compared to parsing complexity but should be measured.
6. **Testing coverage**: Existing tests primarily use ASCII. Need to ensure Unicode test coverage is adequate to catch edge cases in the conversion.
7. **Preamble encoding**: The preamble is hard-coded as a C string and always treated as UTF-8. If it contains non-ASCII Unicode operators, this creates an inconsistency when `--encoding` specifies a different encoding. Safest approach is to restrict preamble to 7-bit ASCII.

## Expected Benefits

1. **Code simplification**: Remove complex UTF-8 state machine code from scanner. Character iteration becomes simple pointer arithmetic. Eliminate `PrattUTF8` array type and conversion functions.
2. **Standard library usage**: Replace custom Unicode functions with standard `wctype.h` functions that are well-tested and optimized.
3. **Correctness**: libc Unicode support is more likely to be correct and complete than hand-rolled UTF-8 code.
4. **Maintainability**: Less custom code to maintain. Standard library functions are documented and familiar to other developers.
5. **Predictable behavior**: Mandating UTF-8 ensures consistent behavior across systems. No surprises from locale environment variables.
6. **Better error detection**: Explicit encoding validation provides clear error messages instead of silent corruption or mysterious failures.

## Open Questions

1. Should `--encoding` support be implemented in phase 1, or deferred until after UTF-8 implementation is working?
2. What specific locale string to use: `"C.UTF-8"`, `"en_US.UTF-8"`, or something else? (Platform differences may matter)
3. Is it worth creating a benchmark suite to measure parser performance before/after?
4. Should the symbol table eventually move to `wchar_t` as well, or is the boundary at the parser the right long-term design?
5. Should we validate that output (error messages, etc.) can be correctly converted back to UTF-8, or assume it always works?
