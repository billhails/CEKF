# TODO

More of a wish-list than a hard and fast plan.

* Over-application i.e. `fn (a) { fn (b) { a + b } }(2, 3)`.
* Unpacking function return values (tuples only).
* `now()` builtin returns current time in milliseconds.
* Error function.
* Macro support (see [MACROS](./MACROS.md) for initial thoughts).
* More numbers:
   * NaN.
* Improve arithmetic.
   * General overhaul.
   * move to [libgmp](https://gmplib.org/)?
   * trig functions.
   * Pre-compute constant values at compile time.
   * allow numeric (not symbolic) arithmetic expressions in patterns.
   * allow unpacking of numerator and denominator variables in patterns.
      * special case: `a / b` because there is a canonical form (via gcd).
      * match would fail if the actual argument is not integer or rational.
      * `b` would be bound to `1` if the argument was an integer.
      * can't allow arbitrary ops and combinations because i.e. `(a + 2) * b`
        can't uniquely determine `a` and `b`, and `a ** 2` would require `a`
        to be bound to the square root of the actual argument.
   * allow unpacking of the real and imaginary parts of a complex number in patterns.
      * another special case: `a + b`, `b` would be bound to `0i` if the actual
        argument is not complex.
* UTF8 and `wchar_t`.
   * Would play nicely with strings being lists of char.
   * Store the Unicode code points internally.
   * Combining characters need special handling.
* Much better error reporting.
* Error recovery.
* User definable operators.
   * With precedence and associativity.
   * `infix 55 left >>= fn(l, r) { ... }`
   * `prefix 45 $ fn(a) { ... }`
   * `suffix 60 ! factorial`
* Curried binary operators `(2+)` etc.
* Aliases
   * `alias some = maybe.some;`
   * `alias string = list(char);`
* (internal) have a NEWZ variant of NEW that bzero's its result.
