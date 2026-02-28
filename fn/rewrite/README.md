# Rewrite

An exploratory sub-project to investigate re-writing F♯ in F♯. For this to be practical we would need to target LLVM rather than a bytecode interpreter.

Even if it doesn't pan out, this is still useful as it provides concise reference implementations of some of the core algorithms which can inform the C implementation.

For agent-oriented guidance on the rewrite prototyping/self-hosting workflow (`test_harness.fn`, pass ordering, and `samples.fn` usage), see [`docs/agents/rewrite-self-hosting-guide.md`](../../docs/agents/rewrite-self-hosting-guide.md).

* [`alphaconvert.fn`](alphaconvert.fn) - ɑ-conversion algorithm over `expr`.
* [`appel.fn`](appel.fn) - The continuation type from Appel's book (unused).
* [`beta_reduce.fn`](beta_reduce.fn) - β-reduction algorithm.
* [`ceskf.fn`](ceskf.fn) - The core CESKF machine.
* [`expr.fn`](expr.fn) - A set of types intended to replicate the LamExp structures in [`lambda.yaml`](../../src/lambda.yaml). Only used as initial input parser and output to desugar.
* [`closure_convert.fn`](closure_convert.fn) - Closure lifting.
* [`constant_folding.fn`](constant_folding.fn) - Constant folding and algebraic simplification.
* [`cps.fn`](cps.fn) - The CPS transform.
* [`curry.fn`](curry.fn) - Replacing multi-valued functions and applications with nests.
* [`desugar.fn`](desugar.fn) - Reduction from `expr` to `minexp` minimal IR.
* [`env.fn`](env.fn) - Simple environment utility.
* [`eta_reduce.fn`](eta_reduce.fn) - The η-reduction algorithm.
* [`freevars.fn`](freevars.fn) - Free variable function.
* [`gensym.fn`](gensym.fn) - Symbol and string generator.
* [`infer.fn`](infer.fn) - Type inference.
* [`interpreter.fn`](interpreter.fn) - A naïve lambda interpreter demo.
* [`minexpr.fn`](minexpr.fn) - Minimal IR used by most of the rest of the transforms.
* [`occurs_in.fn`](occurs_in.fn) - Test if variables occur free in exprs.
* [`normalize.fn`](normalize.fn) - ANF conversion.
* [`petterson92.fn`](petterson92.fn) - Pettersson's Term Pattern Matching Compiler algorithm.
* [`pratt.fn`](pratt.fn) - Pratt Parser.
  * [`pratt_lexer.fn`](pratt_lexer.fn) - Lexer Support for the Parser.
  * [`pratt_sexpr.fn`](pratt_sexpr.fn) - Target Symbolic Expressions for the parser.
* [`samples-curry.fn`](samples-curry.fn) - Dataset for curry testing.
* [`samples.fn`](samples.fn) - General purpose dataset.
* [`subst.fn`](subst.fn) - General substitution utility.
* [`test_harness.fn`](test_harness.fn) - main pipeline execution.
* [`transform.fn`](transform.fn) - ANF Transform.
* [`uncurry.fn`](uncurry.fn) - Uncurry optimiser.
