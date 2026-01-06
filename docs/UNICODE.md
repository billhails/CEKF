# Plan for Unicode Support

Keep it simple, UTF8 only

The Value type has a type Character but this can be amended to a
`wchar_t`.
Although we stipulate UTF8 we should store characters in Values
as Unicode code points.
However the C strings used in i.e. symbol names by the compiler
will remain UTF8 and so do not require a `wchar_t` modification.

Any future character input / reader implementation must be able
to decode UTF8.

Strings and characters should support a `\Uxxxx;` escape allowing
unicode (hex) code points, as well as directly embedded UTF8.

We should not try too hard to categorize unicode characters, though
perhaps it might be possible to segregate punctuation from other
classes.

## Notes on UTF-8

| First   | Last    | Byte 1   | Byte 2   | Byte 3   | Byte 4   | Bits |
|---------|---------|----------|----------|----------|----------|------|
| 0x00    | 0x7F    | 0xxxxxxx |          |          |          | 7    |
| 0x80    | 0x7FF   | 110xxxxx | 10xxxxxx |          |          | 13   |
| 0x800   | 0xFFFF  | 1110xxxx | 10xxxxxx | 10xxxxxx |          | 16   |
| 0x10000 | 0x10FFF | 11110xxx | 10xxxxxx | 10xxxxxx | 10xxxxxx | 21   |

## UTF-8 Regexes

Generated using [Unicode.hs](https://lists.gnu.org/archive/html/help-flex/2005-01/msg00043.html)

```text
0x80 - 0x9F Control characters
\xC2[\x80-\x9F]

0xA0 - 0xD7FF Normal characters
\xC2[\xA0-\xBF]|[\xC3-\xDF][\x80-\xBF]|(\xE0[\xA0-\xBF]|\xED[\x80-\x9F]|[\xE1-\xEC][\x80-\xBF])[\x80-\xBF]

0xD800 - 0xDFFF Reserved
\xED[\xA0-\xBF][\x80-\xBF]

0xE000 - 0xFFFD Normal Characters
\xEF\xBF[\x80-\xBD]|(\xEF[\x80-\xBE]|\xEE[\x80-\xBF])[\x80-\xBF]

0xFFFE 0xFFFF Reserved
\xEF\xBF[\xBE-\xBF]

0x10000 - 0x10FFFF Normal Characters
(\xF0[\x90-\xBF]|\xF4[\x80-\x8F]|[\xF1-\xF3][\x80-\xBF])[\x80-\xBF][\x80-\xBF]

0x80 - 0x10FFFF All high-bit Unicode
[\xC2-\xDF][\x80-\xBF]|(\xE0[\xA0-\xBF]|[\xE1-\xEF][\x80-\xBF])[\x80-\xBF]|(\xF0[\x90-\xBF]|\xF4[\x80-\x8F]|[\xF1-\xF3][\x80-\xBF])[\x80-\xBF][\x80-\xBF]
```
