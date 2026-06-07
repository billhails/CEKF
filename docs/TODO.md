# TODO

More of a wish-list than a hard and fast plan.

* Fn/Rewrite.
  * Add an interpreter for the final IR.
  * Add a parser and AST.
  * Type Checking.
  * etc.
* More folding opportunities.
  * Fold duplicate condition branches `(if x a a) => a`.
* Continuations.
  * Implement delimited continuations.
    * delimit/capture instead of reset/shift?
* Types.
  * Consider type classes as a general solution to `EQ <type>`, `map` etc.
  * Records should create accessor functions for each tag.
    * if there is only one type variant.
* Namespaces.
  * `import <ns> typedef <x>` and `import <ns> typedefs`.
* Parser.
  * if/then/else => `(fn { (true) {then} (false) {else} }(if))` (we already do this but hard-coded in the parser).
  * `do` notation for monads.
* Memory Management.
  * Replace mark and sweep GC with a generational stop and copy.
* Pipeline.
  * Re-order Type Checking before TPMC.
  * Target LLVM.
* Generate.
  * Add a generator for equivalent F♮ `typedef`s to facilitate `fn/rewrite/`.
    * Will need F♮ support for hashes, symbols arrays etc.
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
