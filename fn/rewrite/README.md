# Rewrite

An exploratory sub-project to investigate re-writing F♯ in F♯. For
this to be practical we would need to target LLVM rather than a bytecode
interpreter.

* [ceskf.fn](ceskf.fn) - The core CESKF machine.
* [interpreter.fn](interpreter.fn) - A naïve lambda interpreter demo.
* [petterson92.fn](petterson92.fn) - Pettersson's Term Pattern Matching Compiler.
* [pratt.fn](pratt.fn) - Pratt Parser.
   * [pratt_lexer.fn](pratt_lexer.fn) - Lexer Support for the Parser.
   * [pratt_sexpr.fn](pratt_sexpr.fn) - Target Symbolic Expressions for the parser.
