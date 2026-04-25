# CEKF(s)

![logo](./docs/CEKF2.png)

> "I do remember one thing, it took hours and hours and by the
  time I was done with it, I was so involved I didn't know
  what to think. I carried it around with me for days and days,
  playing little games. Like not looking at it for a whole day,
  and then looking at it, to see if I still liked it. I DID!"

Low level, hopefully fast C implementation of a CEK machine with an
additional "F" failure continuation supporting amb (and a stack).

CEKF is the rather unfortunate name of the repo and the implementation.
The language itself I'm calling F♮ (F-Natural).

This is heavily based on a blog post by Matt Might [Writing an
interpreter, CESK-style](https://matt.might.net/articles/cesk-machines/),
but using a bytecode interpreter rather than a tree-walking interpreter,
and utilising a hybrid stack/closure/continuation implementation where
variables local to a function are directly accessible on the stack, and
closures and continuations are snapshots of stack frames. It additionally
makes use of fast lexical addressing (technically
[De Bruijn Indexing](https://en.wikipedia.org/wiki/De_Bruijn_index)) for
added efficiency gains and an implementation of Hindley-Milner
Algorithm W for strict implicit type checking.

I'm hoping that I can reproduce [the F♮ language I once implemented
in Python](https://github.com/billhails/PyScheme), but as a standalone
binary with reasonable performance.

If you want to stick around, maybe start by reading
[the wiki]([wiki](https://github.com/billhails/CEKF/wiki)),
then maybe [the math](docs/MATH.md)
and comparing that with its implementation in [`step.c`](src/step.c), or
start at [`main.c`](src/main.c) and work your way through.

For AI coding assistants or developers wanting a comprehensive overview,
see the [AI Assistant Guide](docs/AI-ASSISTANT-GUIDE.md) which covers
architecture, build system, known complexity areas (TPMC, ANF), and the
code generation system.

### An aside on `amb`

I should probably give at least a brief explaination of `amb` here, for
those who don't know what it is, since it's somewhat the point of this
little project. `amb` is short for "ambivalent" in the sense of "having
more than one value", and is a way of doing non-deterministic programming.

If you have a continuation passing style interpreter, then all control
flow, both call and return, is always "forwards" by calling a function
(call) or calling a continuation (return). It then becomes possible to
thread an additional "failure" continuation as a sort of hidden argument
through all those calls.

Mostly that additional continuation goes completely unnoticed, except
in two specific cases:

1. When `amb` is invoked with two (unevaluated) arguments, it arranges
   to have it's first argument evaluated, and additionally installs a new
   failure continuation that will, if invoked, restore the state of the
   machine to the point just after `amb` was invoked, but with the second
   argument to `amb` ready to be evaluated instead.
2. When `back` is invoked, it restores the most recent state installed by
   `amb`, "backtracking" to the decision point and allowing the alternative
   to be produced.

To see `amb` in action, look at the sample [fn/barrels.fn](fn/barrels.fn).
Note that in this language `amb` is an infix operator called `then`.

For a good description of `amb` see [SICP pp.
412-437](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-28.html#%_sec_4.3).

What makes a CEK machine such an easy way to implement `amb` is that
the failure continuation is just an additional register, nothing else
in the CEK machine needs to change, apart from two additional cases in
the $step$ function: one to deal with `amb` and one to deal with `back`.

## Progress/Architecture

```mermaid
flowchart TD
classDef process fill:#aef,color:#123;
classDef new fill:#8bf,color:#000;
classDef minlam fill:#036,color:#fea;
classDef ast fill:#aa0,color:#000;
classDef lambda fill:#4a8,color:#000;
classDef anf fill:#a84,color:#000;

subgraph Parse
   source(Source) ==>
   scanner([Scanner]):::process ==>
   tokens(Tokens) ==>
   parser([Parser]):::process ==>
   oi([Operator Inlining]):::process --> scanner
   parser ==>
   ast(AST):::ast
end

ast ==> namespace_removal([Namespace Desugaring]):::process ==>
denamespace(Simplified AST):::ast

denamespace -.-> tcast

subgraph Proposed
   tcast([Typecheck AST]):::new
   tcast -.-> tcdast(Typechecked AST):::ast
   tcdast -.-> lconv([Lambda Conversion]):::new
end

lconv -.-> lambda_ds

denamespace ==> lc

subgraph Lambda Conversion
   lc([Lambda Conversion]):::process ===> tpmc([Pattern Matching Compiler]):::process
   lc <==> pg([Print Function Generator]):::process
   lc <==> me([Lazy Function Expansion]):::process
   tpmc ==> vs([Variable Substitution]):::process
   vs ==> lc
   lc <==> des([Desugaring]):::process
end

lc ==> lambda0(Plain Lambda Form):::lambda
lambda0 ==> sim([Simplification]):::process
sim ==> lambda1(Plain Lambda Form):::lambda
lambda1 ==> tc([Type Checking]):::process
tc <==> pc([Print Compiler]):::process
tc ==> lambda2(Plain Lambda Form):::lambda
lambda2 ==> ci([Constructor Inlining]):::process
ci ==> lambda3(Inlined Lambda):::lambda
desugaring(["Desugaring"]):::process
desugaring ==> lambda_ds("minimal lambda (MinLam)"):::minlam

lambda_ds ==> alpha(["ɑ-Conversion"]):::process
alpha ==> lambda_a(alphatized lambda):::minlam

lambda_a ==> curry(["Currying"]):::process
curry ==> curried(curried lambda):::minlam
curried ==> beta(["β-conversion"]):::process
beta ==> eta(["η-conversion"]):::process
eta ==> folding([Operator Folding]):::process
folding ==> folded(optimized operators):::minlam
folded ==> uncurry(["Un-Currying"]):::process
uncurry ==> uncurried(uncurried lambda):::minlam

uncurried ==> anfr([ANF Rewrite]):::process
anfr ==> lambda_b(New ANF):::anf

uncurried ==> cps

subgraph Target C
   cps([CPS Transform]):::process ==>
   beta2([Additional β-conversion]):::process ==>
   lambda_e("CPS λ"):::minlam ==>
   amb_conversion([AMB Conversion]):::process ==>
   amb(CPS λ with failure continuation):::minlam ==>
   beta3([Additional β-conversion]):::process ==>
   eta3(["Additional η-conversion (TCO)"]):::process ==>
   tree_shaking(["Dead Binding Elimination (Tree Shaking)"]):::process ==>
   small(Only Necessary Code):::minlam ==>
   closure_conversion([Closure Lifting]):::process ==>
   closures(Explicit Closures):::minlam ==>
   de_bruijn([DeBruijn Indexing]):::process ==>
   indexed("Annotated Variables (Final IR)"):::minlam
   indexed ==>
   c_emitter([Emit C]):::process ==>
   target_c(Pure C code)
   indexed ==>
   b_emitter([Emit B]):::process ==>
   target_b(Byte Code)
   target_b ==> brun([B-Runner]):::process
   target_b <==> bbcf(Bytecode Files)
end
   
lambda3 ==> desugaring
uncurried ==> anfc

subgraph Target Bytecode
   anfc([A-Normal Form Conversion]):::process ==>
   anf(ANF):::anf ==>
   lexa([Lexical Analysis]):::process ==>
   ann(Annotated ANF):::anf ==>
   bcc([Bytecode Compiler]):::process ==>
   bc(Byte Code) ==>
   cekf([CEKF Runtime VM]):::process
   bc <==> bcf(Bytecode Files)
end
```

A big driver for the progress made has been arriving at a minimalist
lambda (`MinLam`) set of structures for the IR. In the diagram above
the stages making use of this `MinLam` representation are coloured
dark-blue/grey.

The "ANF Rewrite" stage was a successful attempt to re-implement the
clunky existing ANF transform, but in the process it became apparent that
the CEK machine itself was blocking optimizations and so the trajectory
pivoted towards targeting a more "traditional" register machine via CPS,
initially in C but with an eye towards LLVM in the longer term.

The various components named in the diagram above are linked to their
implementation entry point here:

* Scanner [pratt_scanner.c](src/pratt_scanner.c)
* Parser [pratt_parser.c](src/pratt_parser.c)
* AST [ast.yaml](src/ast.yaml)
* Lambda Conversion [lambda_conversion.c](src/lambda_conversion.c)
* Tpmc [tpmc_logic.c](src/tpmc_logic.c)
* Print Function Generator [print_generator.c](src/print_generator.c)
* Variable Substitution [lambda_substitution.c](src/lambda_substitution.c)
* Lazy Function Expansion [lazy_substitution.c](src/lazy_substitution.c)
* Plain Lambda Form [lambda.yaml](src/lambda.yaml)
* Simplification [lambda_simplify.c](src/lambda_simplify.c)
* Type Checking [tc_analyze.c](src/tc_analyze.c)
* Print Compiler [print_compiler.c](src/print_compiler.c)
* Constructor Inlining [inline.c](src/inline.c)
* A-Normal Form Conversion [anf_normalize.c](src/anf_normalize.c)
* ANF [anf.yaml](src/anf.yaml)
* Lexical Analysis [annotate.c](src/annotate.c)
* Byte Code Compiler [bytecode.c](src/bytecode.c)
* Byte Code [cekfs.yaml](src/cekfs.yaml)
* CEKF Runtime [step.c](src/step.c)

The desugaring step is now rolled into thae lambda conversion phase as eary
as possible to simplify downstream processing.

### Function Over-Application

Implemented per RFC-001 (hybrid compile-time split + runtime staging). For details see:
`docs/RFC-001-over-application.md`.

## CEKF Formal Description

A formal mathematical description of the CEKF machine can be found [here](docs/MATH.md).

## Bytecode

The description of the machine linked above assumes it is evaluating a tree of
lambda expressions. That makes the concepts somewhat clearer so I've left
it as originally written. However the actual implementation uses a bytecode
interpreter instead. Translation from lambda expressions to bytecode turns
out to be not that difficult, see [docs/V2](docs/V2.md) for details of that.

## Lexical Addressing

A lexical analysis stage annotates variables with their locations for faster
run-time lookup. See [docs/LEXICAL_ADDRESSING](docs/LEXICAL_ADDRESSING.md).

## Type Inferencing

My previous attempt at implicit type-checking borrowed a pre-built
implementation of Algorithm W written in Python. This time around I've
gone with my own implementation, which required quite a lot of research.
I've made notes on that process in [docs/TYPES](docs/TYPES.md).

## Arbitrary size integers

Rather than forcing a requirement on an external library like
[libgmp](https://gmplib.org/) in the early stages, I've instead
opted to incorporate a much smaller, public domain implementation
from [983](https://github.com/983/bigint), only slightly amended to
play nice with the CEKF memory management and garbage collection.

## Unicode

There is basic support for Unicode, both in strings and in
identifiers. The base `Character` type is now a `wchar_t` which stores
raw Unicode code points (UTF-32). The only supported input/output encoding is UTF8, which is
used internally for C strings in the interpreter, to read source files
and and to output characters to the terminal.

## SQLite

The implementation bundles SQLite3 and provides an interface to it, see
[here](docs/SQLITE.md). The primary reason being to allow convenient
and fast lookup of Unicode data for individual characters, should the
need arise.

## Code Generation

While not properly part of the language description, I'm using some
hand-written code generators to make development easier and they're
documented [here](docs/CODEGEN.md).
