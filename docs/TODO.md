# TODO

* Over-application i.e. `fn (a) { fn (b) { a + b } }(2, 3)`.
* Allow user overrides of print functions.
* Unpacking function return values (tuples only).
* `now()` expression returns current time in milliseconds.
* Macro support (see [MACROS](./MACROS.md) for initial thoughts).
* More numbers:
   * NaN.
* Improve arithmetic.
   * General overhaul.
   * move to [libgmp](https://gmplib.org/)
   * Pre-compute constant values at compile time.
   * allow numeric (not symbolic) arithmetic expressions in patterns.
   * allow unpacking of numerator and denominator variables in patterns.
      * special case because there is a canonical form
      * can't allow arbitrary ops and combinations because i.e. `(a + 2) * b` can't determine `a` and `b`, and `a ** 2` would require `a` to be bound to the square root of the actual argument.
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
