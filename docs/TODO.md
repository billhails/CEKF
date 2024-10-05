# TODO

More of a wish-list than a hard and fast plan.

* Over-application i.e. `fn (a) { fn (b) { a + b } }(2, 3)`.
* Unpacking function return values (tuples only).
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
         * but does `a / 6` match `2 / 3` and bind `a` to `4`?
      * match would fail if the actual argument is not integer or rational.
      * `b` would be bound to `1` if the argument was an integer.
      * can't allow arbitrary ops and combinations because i.e. `(a + 2) * b`
        can't uniquely determine `a` and `b`, and `a ** 2` would require `a`
        to be bound to the square root of the actual argument.
   * allow unpacking of the real and imaginary parts of a complex number in patterns.
      * another special case: `a + b`, `b` would be bound to `0i` if the actual
        argument is not complex.
* Much better error reporting.
* `--expression="..."` to execute a snippet from the command line.
* Error recovery.
* User definable operators.
   * With precedence and associativity.
   * `infix 55 left ">>=" fn(l, r) { ... }`
   * `prefix 45 "$" fn(a) { ... }`
   * `suffix 60 "!" factorial`
   * `infix 40 "and" and` - where `and` would have to be a macro.
   * precedence of prefix and postfix operators matters, i.e. `-2**2` is `-(2**2)` not `(-2)**2`.
   * looks like this is a biggie and would involve replacing the existing parser (and probably lexer)
     with a
     [Pratt Parser](https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/). 
     However this may be the right thing to do as it also opens up a much better approach to
     [macros](https://journal.stuffwithstuff.com/2011/02/13/extending-syntax-from-within-a-language/).
* Curried binary operators `(2+)` etc.
* Aliases
   * `alias some = maybe.some;`
   * `alias string = list(char);`
* (internal) have a NEWZ variant of NEW that bzero's its result.
* Builtins
   * `now()` builtin returns current time in milliseconds.
