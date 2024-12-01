# Rewrite

An exploratory sub-project to investigate re-writing F♯ in F♯. For
this to be practical we would need to target LLVM rather than a bytecode
interpreter.

* [ceskf.fn](ceskf.fn) - The core CESKF machine.
* [infer.fn](infer.fn) - Type inference.
* [interpreter.fn](interpreter.fn) - A naïve lambda interpreter demo.
* [normalize.fn](normalize.fn) - ANF conversion.
* [petterson92.fn](petterson92.fn) - Pettersson's Term Pattern Matching Compiler algorithm.
* [pratt.fn](pratt.fn) - Pratt Parser.
   * [pratt_lexer.fn](pratt_lexer.fn) - Lexer Support for the Parser.
   * [pratt_sexpr.fn](pratt_sexpr.fn) - Target Symbolic Expressions for the parser.
