# TODO

In no particular order.

* over-application i.e. `fn (a) { fn (b) { a + b } }(2, 3)`
* user defined print functions.
* tuples - can use vec type to implement.
   * BUT - fn args are not tuples, that might interfere with currying.
   * The type checker has support for pairs and we might leverage those
     to type-check tuples, but flat vecs and/or the stack will be more
     efficient at run-time.
* unpacking function return values (tuples only).
* `now()` expression returns current time in milliseconds.
* macro support (see [MACROS](./MACROS.md) for initial thoughts).
* more numbers:
   * rationals: 1/3.
   * irrationals: sqrt(2).
   * complex numbers.
   * matrices.
   * quaternions?
* UTF8 and `wchar_t`.
* first class environments.
* libraries.
   * probably use file system layout.
   * env var to specify the root location(s).
   * better preable/postamble handling.
   * first class environments are likely a prerequisite.
* propagate file and line numbers into all error reporting.
   * much better error reporting.
* error recovery.
* command-line arguments for libraries etc.
* fail on non-exhaustive pattern match (optional).
* error function.
* file handles / streams.
* POSIX or similar library support.
