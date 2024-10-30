# TODO

More of a wish-list than a hard and fast plan.

* Target LLVM
* Proper namespace import support including user-defined operators.
* Over-application i.e. `fn (a) { fn (b) { a + b } }(2, 3)`.
* Unpacking function return values (tuples only).
* More numbers:
   * NaN.
* Improve arithmetic.
   * General overhaul.
   * move to [libgmp](https://gmplib.org/)?
   * trig and log functions.
   * Pre-compute constant values at compile time.
   * allow numeric (not symbolic) arithmetic expressions in patterns.
   * allow unpacking of numerator and denominator variables in patterns.
      * special case: `a / b` because there is a canonical form (via gcd).
         * but does `a / 6` match `2 / 3` and bind `a` to `4`?
      * match would fail if the actual argument is not integer or rational.
      * `b` would be bound to `1` if the argument was an integer.
      * can't allow arbitrary ops and combinations because i.e. `(a + 2) * b`
        can't uniquely determine `a` and `b`, and `a ** 2` would require `a`
        to be bound to the square root of the actual argument.
   * allow unpacking of the real and imaginary parts of a complex number in patterns.
      * another special case: `a + b`, `b` would be bound to `0i` if the actual
        argument is not complex.
* Curried binary operators `(2+)` etc.
* (internal) have a NEWZ variant of NEW that bzero's its result.
* Builtins
   * `now()` builtin returns current time in milliseconds.
