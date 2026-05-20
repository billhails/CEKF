# TODO

More of a wish-list than a hard and fast plan.

* More folding opportunities.
  * fold boolean expressions `true and false => false`.
    * tricky because `and`, `or` etc. are not primitive, they are lazy operators defined in terms of `if` in the preamble.
  * fold comparisons `a == a => true`, `a >= a => true` etc. DONE
  * fold constant conditions `(if true a b) => a`. DONE
    * This solves the boolean expression folding problem, after β/η-reduction:
      * `true and false => (if true false false) => false`
  * fold duplicate condition branches `(if x a a) => a`.
* Continuations.
  * Reinstate `cut` (prunes current back continuation). DONE
  * Implement delimited continuations.
* Regular Expressions.
  * Enlist the new regex engine to support the core Pratt scanner.
* Types.
  * Consider type classes as a general solution to `EQ <type>`, `map` etc.
  * Records should create accessor functions for each tag.
    * if there is only one type variant.
  * extend the `typedef` keyword.
    * `typedef container(#t);` is shorthand for `typedef container(#t) { container(#t) };`
    * `typedef container(char);` is shorthand for `typedef container { container(char) };`
    * no additional AST should be required, or if it is it gets immediately desugared so doesn't leak downstream.
* Namespaces.
  * We want `import <ns> function <x>` and `import <ns> functions`. DONE mostly.
  * And `import <ns> typedef <x>` and `import <ns> typedefs`.
* Parser.
  * re-elist the now-available `macro` keyword for proper syntactic extensibility. DONE
    * if/then/else => `(fn { (true) {then} (false) {else} }(if))` (we already do this but hard-coded in the parser).
    * `do` notation for monads.
* Memory Management.
  * Replace mark and sweep GC with a generational stop and copy.
* Pipeline.
  * Re-order Type Checking before TPMC.
  * Target LLVM.
* Generate.
  * Move all signatures into `signature_helper.py`, not just the shared ones.
* More numbers.
  * Vectors.
  * Matrices.
  * Quaternions.
* Improve arithmetic.
  * ceil, floor and round operations.
  * move to [libgmp](https://gmplib.org/)?
  * trig and log functions.
  * allow numeric (not symbolic) arithmetic expressions in patterns.
  * allow unpacking of numerator and denominator variables in patterns.
    * special case: `a / b` because there is a canonical form (via gcd).
    * does `a / 6` match `2 / 3` and bind `a` to `4`.
    * match would fail if the actual argument is not integer or rational.
    * `b` would be bound to `1` if the argument was an integer.
    * can't allow arbitrary ops and combinations because i.e. `a * b` can't uniquely determine `a` and `b`, and `a ** 2` would require `a` to be bound to the square root of the actual argument.
  * allow unpacking of the real and imaginary parts of a complex number in patterns.
    * another special case: `a + b`, `b` would be bound to `0i` if the actual argument  is not complex.
  * `mathutils.fn` for `factorial`, `gcd`, `lcm`, `fib`, `pi` etc.
* Curried binary operators `(2+)` etc.
* (internal) have a NEWZ variant of NEW that bzero's its result.
* Builtins.
  * `now()` builtin returns current time in milliseconds.
* I/O.
  * Make i/o pleasant to use.
  * Any ideas welcome, currently it's a mess.
  * Specifically think about ways to support parsing.
  * Generated printers should take a file handle, wrapper printer supplies stdout.
  * Generate parsers alongside printers.
