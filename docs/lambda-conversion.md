# Lambda Conversion

This is the stage after type checking where we desugar and rewrite the
input into something close to the lambda calculus, which then becomes
input to the final A-Normal form conversion to generate the structures
that are used to generate the bytecode which is input to the CEKF machine
for execution.

```mermaid
flowchart TD
source -->
AST[Parser] --abstract syntax-->
check[Type Checking] --abstract syntax-->
lambda[Lambda Conversion] --lambda calculus-->
desugaring1[Desugaring] -- simplified lambda calculus-->
anf[A-Normal Form Conversion] --ANF-->
static[Static Analysis] --annotated ANF-->
Bytecode[Bytecode Generation] --bytecode--> VM
subgraph WIP
   lambda
end
```

Mostly lambda conversion is a fairly straightforward process, a one-to-one
mapping between high-level and lower-level. However dealing with the pattern
matching of composite functions is much less so.

My current thinking is to convert the arguments to an NFA, then convert that
to a DFA, then compile that to a "parser" for the actual arguments received.
The advantages of this approach seem to be:

 1. Reqular expressions and the NFA to DFA conversion are well understood.
 2. An NFA with multiple end states is perfectly feasible (Lex/Flex for example).
 3. The resulting code should be fast.
 4. Using regular expression techniques gives us actual regular expressions in formal arguments almost for free if we want to implement them later.

So lets work through some examples to make sure it'll work.

```
fn xor {
	(false, false) { false }
	(false, true)  { true }
	(true, false)  { true }
	(true, true)   { false }
}
```

Produces an NFA:

```mermaid
flowchart LR

S(((S))) --false--> S1((S1))
S --false--> S2((S2))
S --true--> S3((s3))
S --true--> S4((S4))
S1 --false--> A(((A)))
S2 --true--> B(((B)))
S3 --false--> C(((C)))
S4 --true--> D(((D)))
```

Which in this case we can trivially convert to a DFA:

```mermaid
flowchart LR

S(((S))) --false--> S1((S1))
S --false--> S2((S2))
S --true--> S3((s3))
S --true--> S4((S4))
S1 --false--> A(((A)))
S2 --true--> B(((B)))
S3 --false--> C(((C)))
S4 --true--> D(((D)))
```

Which in turn can be trivially compiled to a nested case statement

```
(lambda ($a $b)
  (match $a (0 (match $b (0 0)
                         (1 1))
             1 (match $b (0 1)
                         (1 0)))))
```

let's try something a bit more ambitious.

```
fn map {
    (_, []) { [] }
    (f, h @ t) { f(h) @ map(f, t) }
}
```

NFA:

```mermaid
flowchart LR
S(((S))) --"_"--> S1((S1)) --"nil"--> A(((A)))
S --"f"--> S2((S2)) --cons--> S3((S3)) --h--> S4((S4)) --t--> B(((B)))
```

I think it's neccessary to include walking the structs as part of the automaton, especially
when things get more complex later.

DFA:

```mermaid
flowchart LR
S(((S))) --var--> S1((S1))
S1 --nil-->A(((A)))
S1 --cons--> S2((S2))--h--> S3((S3)) --t--> B(((B)))
```

compiling this is much less trivial. the end result should be something like

```
(lambda ($a $b)
  (match (vec $b 0) (0 (make-vec 1 0))
                    (1 (let (f $a)
                            (let (h (vec $b 1))
                                 (let t (vec $b 2)
                                        (cons (f h) (map f t)))))))
```

The main problem is the `let` bindings. in the conversion from an NFA to a DFA we seem to have lost some information. We might have to work backwards from the final state to the NFA transition that bound the variable. In which case we might have to additionally create an annotated inversion of the NFA:

```mermaid
flowchart TD
A(((A))) --> S(((S)))
B(((B))) --"(t (vec $b 2))"--> S3((S3))
--"(h (vec $b 1))"--> S2((S2))
--> S1((S1))
--"(f $a)"--> S
```

To track the variable bindings that have to be performed for each final state.

Thankfully, if this is done on the DFA, it can be done without having to cross-reference the component functions in the composite function, and doesn't have any implications for the NFA to DFA conversion.

Another example

```
fn member {
  (x, x @ _) { true }
  (x, []) { false }
  (y, h @ t) { member(y, t) }
}
```

first the DFA

```mermaid
flowchart LR

S(((S))) --"x"--> S1((S1))
--"cons"--> S2((S2))
--"x"--> S3((S3))
--"_"-->
A(((A)))
S --"x"--> S4((S4)) --"nil"--> B(((B)))
S --"y"--> S5((S5))
--"cons"--> S6((S6))
--"h"--> S7((S7))
--"t"--> 
C(((C)))
```

Now the annotation/inversion

```mermaid
flowchart TD
A(((A)))
--> S3((S3))
--"(x (vec $b 1))"--> S2((S2))
--> S1((S1))
--"(x $a)"--> S(((S)))
B(((B)))
--> S4((S4))
--"(x $a)"--> S
C(((C)))
--"(t (vec $b 2))"--> S7((S7))
--"(h (vec $b 1))"--> S6((S6))
--> S5((S5))
--"(y $a)"--> S
```

The NFA

```mermaid
flowchart LR
S(((S)))
--"var"--> S1((S1))
S1 --"cons"--> S2((S2))
S1 --"nil"--> B(((B)))
S2 --"(eq $a (vec $b 1))"--> A(((A)))
S2 --"else"--> C(((C)))
```

The resulting lambda

```
(lambda ($a $b)
  (match (vec $a 0)
         (0 0)
         (1 (match (eq $a (vec $b 1))
                   (0 (let (x $a)
                           (let (h (vec $b 1))
                                (let (t (vec $b 2))
                                     (member x t)))))
                   (1 1)))))
```

Note the equality check has to be part of the DFA, because we don't yet
know which branch we're on so can't start binding variables that will
differ between branches.


