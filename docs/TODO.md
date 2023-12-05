# TODO

* over-application i.e. `fn (a) { fn (b) { a + b } }(2, 3)`
* tuples - can use vec type to implement.
   * BUT - fn args are not tuples, that might interfere with currying.
* unpacking function return values (tuples only)
* print function
   * type awareness
   * needs thought,
   * would be nice to be able to describe how types should print.
* don't convert `if` to `lambda` - it's too inefficient.
* remove old `cons` from ANF and bytecodes.
* numbers:
   * arbitrary size integers
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
   * verbose output for gc statistics at end of run etc.
* fail on non-exhaustive pattern match (optional).
   * some sort of `//@-allow-non-exhaustive` inline directive?
* error function
* remove hard-coded assumptions about type sizes in bytecodes
