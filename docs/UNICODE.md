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

