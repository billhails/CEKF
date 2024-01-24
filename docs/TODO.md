# TODO

* over-application i.e. `fn (a) { fn (b) { a + b } }(2, 3)`
* allow user overrides of print functions
* tuples - can use vec type to implement.
   * BUT - fn args are not tuples, that might interfere with currying.
   * tc has support for pairs and we might leverage those as a start, but flat vecs will be more efficient.
* unpacking function return values (tuples only)
* `now()` expression returns current time in milliseconds.
* macro support (see [MACROS](./MACROS.md) for initial thoughts).
* more numbers:
   * rationals: 1/3
   * irrationals: sqrt(2)
   * complex numbers
* UTF8 and `wchar_t`
* first class environments
* libraries
   * probably use file system layout
   * env var to specify the root location(s)
   * better preable/postamble handling
* propagate file and line numbers into all error reporting.
   * much better error reporting.
* error recovery.
* command-line arguments for libraries etc.
* fail on non-exhaustive pattern match (optional).
* error function
