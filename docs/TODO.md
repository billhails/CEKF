# TODO

More of a wish-list than a hard and fast plan.

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
      * special case: `a / b` because there is a canonical form (via gcd).
      * match would fail if the actual argument is not integer or rational.
      * `b` would be bound to `1` if the argument was an integer.
      * can't allow arbitrary ops and combinations because i.e. `(a + 2) * b`
        can't uniquely determine `a` and `b`, and `a ** 2` would require `a`
        to be bound to the square root of the
        actual argument.
   * allow unpacking of the real and imaginary parts of a complex number in patterns.
      * another special case: `a + b`, `b` would be bound to `0i` if the actual
        argument is not complex.
* UTF8 and `wchar_t`.
   * Would play nicely with strings being lists of char.
   * Store the Unicode values internally.
   * Combining characters need special handling.
* Built-ins.
   * Supply an extensible mechanism to link C libraries and make the functions available.
* Namespaces.
* Libraries.
   * Closely linked to namespaces.
   * Probably use file system layout.
   * Env var to specify the root locations.
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
