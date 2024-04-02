# TODO

* Over-application i.e. `fn (a) { fn (b) { a + b } }(2, 3)`.
* Allow user overrides of print functions.
* Unpacking function return values (tuples only).
* `now()` expression returns current time in milliseconds.
* Macro support (see [MACROS](./MACROS.md) for initial thoughts).
* More numbers:
   * Rationals: 1/3.
   * Irrationals: sqrt(2).
   * Complex numbers.
* UTF8 and `wchar_t`.
* Namespaces.
* Libraries.
   * Probably use file system layout.
   * Env var to specify the root location(s).
   * Better preable/postamble handling.
* Propagate file and line numbers into all error reporting.
   * Much better error reporting.
* Error recovery.
* Command-line arguments for libraries etc.
* Fail on non-exhaustive pattern match (optional).
* Error function.
* User definable infix operators.
   * With precedence and associativity.
* Curried binary operators `(2+)` etc.
