# Support for SQLite3

The immediate use case for SQLite is to be able to efficiently query the
unicode dataset to categorise characters. The unicode consortium hosts a
CSV containing all of the known characters and their attributes  available
[on their website](https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt)
and there's a description of how to read it on
[Wikipedia](https://en.wikipedia.org/wiki/Unicode_character_property).

The second use case is to thrash out a general approach for linking
third party libraries. We already have some very basic support for adding
built-in functions (`rand` and `assert` currently) but they are limited
to accepting and returning types already part of the language. For SQLite
we'll need new opaque types for at least the database connection object
and the statement object.

We won't want to make those directly known to the type checker,
they should behave like instances of user-defined types. They'll
need a printable representation but that can just be something like
`<sqlite3:nnnnn>` or similar for now.  They'll need to be contained
by Value wrappers, but again there need only be a single value type
(`VALUE_TYPE_OPAQUE`?) and we can trust the type system to prevent
mistakes.  In fact the majority of the work is going to be fitting them
in to the type system.

Other considerations are around memory management, when such an opaque
object is garbage-collected it will need its resources properly cleaned
up, so best the opaque object is a common struct with pointers to various
implementation-specific functions.

That'll work well as that can also contain type information etc.

## Update

Builtin declarations now directly use the types declared in `tc.yaml`,
that turned out to be surprisingly easy to do, and there's a new
Opaque TcType and Opaque Value so `sqlite_open` and `sqlite_close`
are working, but nothing else.

Before going much deeper it's worth designing the interface I'd like,
rather than ending up with something clunky.

1. `sqlite_open` should return a `maybe` opaque value, which means
`maybe` will have to go in to the preamble. I was planning that
anyway.
2. Arguments to `sqlite3_bind` should ideally be an array, so we'll
need a new type for `sqlite` data, something along the lines of

```fn
typedef sqlite_data {
    sqlite_int(int) | sqlite_string(string) | ...
}
```

again in the prelude.

Going forward, having to add types directly to the preamble to support
extensions is not ideal, better if they were at least namespaced, and
in fact the namespace concept might just possibly be extensible to
cover such extensions, hmmm...

Dynamic linking is covered by `dlopen` and `dlsym` in linux/unix(/mac?),
and (apparently) `LoadLibrary` and `GetProcAddress` on Windows.
