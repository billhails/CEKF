# Rewrite

An exploratory sub-project to investigate re-writing F♯ in F♯. For this to be practical we would need to target LLVM rather than a bytecode interpreter.

Even if it doesn't pan out, this is still useful as it provides concise reference implementations of some of the core algorithms which can inform the C implementation.

* [`alphaconvert.fn`](alphaconvert.fn) - ɑ-conversion algorithm over `expr`.
* [`expr.fn`](expr.fn) - A set of types intended to replicate the LamExp structures in [`lambda.yaml`](../../src/lambda.yaml). Only used by a subset of the other packages, would be nice to expand its usage.
* [`ceskf.fn`](ceskf.fn) - The core CESKF machine.
* [`infer.fn`](infer.fn) - Type inference.
* [`interpreter.fn`](interpreter.fn) - A naïve lambda interpreter demo.
* [`normalize.fn`](normalize.fn) - ANF conversion.
* [`petterson92.fn`](petterson92.fn) - Pettersson's Term Pattern Matching Compiler algorithm.
* [`pratt.fn`](pratt.fn) - Pratt Parser.
  * [`pratt_lexer.fn`](pratt_lexer.fn) - Lexer Support for the Parser.
  * [`pratt_sexpr.fn`](pratt_sexpr.fn) - Target Symbolic Expressions for the parser.
