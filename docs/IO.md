# Thoughts about IO

We're not going for the world's best file handling system, especially
since strings are quite bulky things in F♮ but we need basic I/O.

Siplest, just have an opaque file type, open a file read, write or append,
have all io operations take an optional file (or have versions of them
that do) and close the filehandle when done.

But there are complications.

1. Serializing and deserializing data structures.
2. File hygene, a better approach might be something like `with open
"file" mode expression` or maybe `from "file" <expression>` and `to
"file" <expression>`.

With `from` and `to` we'd only need to implement `gets` in the first
instance, `puts`, `print` etc. could be modified to use the current file
handle, which could be kept in the environment. The problem there is that
it would be lexically scoped and we'd probably want dynamic scoping for
file handles in a separate stack. Dealing with `here`, `then` and `back`
could be especially troublesome if we introduce dynamic scoping.

Probably better just go with file objects then, and type them as readable
or writable. We can get a monadic-style `with` purely in F♮ in the same
way as with database handles, probably built-in `read` `write` and
`append` functions return a `try` of a filehandle object in the same way
too. Actually thinking about it those existing functions in
`sqliteutils.fn` aren't robust in the face of continuations either.

We could arrange for `back` to close them as shown in SICP but `here`
is still difficult.

Other variations:

```fn
read "filename" fn (filehandle) { ... }
```

doesn't really give us anything and doesn't handle errors.

`write` and `append` both return a `writable` opaque type, there's
no relevant distinction between the two modes to a type system that
I can think of.

## Wish-list

* Read whole file in one gulp.
* Read line by line.
* Read character by character.
* Read/write binary data.
* Read/write serialized objects.
  * one at a time.
  * as a list.
* Read/write different encodings, at least UTF-16.

For serialization let's not make the mistake of inventing our own
format (like PHP did) instead just use JSON, or maybe offer a set
of options (YAML, XML etc.)

We should't need to worry about recursive loops in purely functional
data structures but we can't rule them out as input, at the minimum
we'd want to recognise and reject them (actually can they even be
represented in JSON? maybe, with some sort of `"$ref:..."` extension.)
