# Byte Code Documentation

* `ADD` - Binop.
* `AMB [jmp]` - Creates failure that resumes at `jmp`.
* `APPLY [nargs]` - Expects `nargs` arguments on the stack with a callable on top, invokes the callable with those args, leaving the result on the stack.
* `BACK` - Pops current failure continuation and resumes it.
* `BIGINT [bigint]` - Pushes the literal bigint.
* `BIGINT_IMAG [bigint]` - Pushes the literal bigint as imaginary.
* `CALLCC` - Pop the callable, push the current continuation, push the callable and apply.
* `CHAR [character]` - Pushes the literal character (unicode).
* `CHARCOND [size] [val][jmp] [val][jmp] ...` Pop value, search table and jump.
* `CMP` - Binop.
* `CUT` - Pops current failure continuation and discards it.
* `DIV` - Binop.
* `DONE` - Halt.
* `EQ` - Binop.
* `ERROR` - Error.
* `GE` - Binop.
* `GT` - Binop.
* `IF [jmp]` - Pop value and jump if false.
* `INTCOND [size] [inttype][val][jmp] [inttype][val][jmp] ...` Pop value, search table and jump.
* `IRRATIONAL [float]` - Pushes the literal float.
* `IRRATIONAL_IMAG [float]` - Pushes the literal float as imaginary.
* `JMP [jmp]` - Unconditional jump.
* `LAM [nargs] [letrec offset] [end]` - Create a closure and push it.
* `LE` - Binop.
* `LET [jmp]` - Duplicates the top stack frame, create a new continuation to resume the body (at `jmp`) then lets control pass to the expression.
* `LETREC [nargs]` - Expects `nargs` closures at ToS, patches each with current S (i.e. themselves).
* `LT` - binop.
* `LVAR [offset]` - Look up a stack (local) variable and push it.
* `MAKEVEC [size]` - Pop size values create a vec of them and push it.
* `MATCH [size] [jmp] [jmp] ...` Pop index and jump (size is just for safety check).
* `MOD` - Binop.
* `MUL` - Binop.
* `NE` - Binop.
* `NONE` - Error.
* `NS_END [numLambdas] [stackOffset]` - Move the new lambdas at ToS into the namespace.
* `NS_FINISH [num]` - Patch each of the namespaces with the final block of populated namespaces, size `num`, at ToS.
* `NS_POP` - Pop value, restore previous stack frame, push value.
* `NS_PUSHENV [frame] [offset]` - Find the namespace in env at frame/offset, create new empty top stack frame, restores namespace to stack.
* `NS_PUSHSTACK [offset]` - Find the namespace on stack at offset, create new empty top stack frame, restore namespace to stack.
* `NS_START [num]` - Pushes num voids on to stack.
* `POW` - Binop.
* `PUSHN [n]` - Allocate `n` bytes of stack.
* `RETURN` - Push the current continuation and apply.
* `STDINT_IMAG [int]` - Pushes the literal int as imaginary.
* `STDINT [int]` - Pushes the literal int.
* `SUB` - Binop.
* `VAR [frame] [offset]` - Look up an env variable and push it.
* `VEC` - Pops a vector and an index into that vector and pushes the value at that index.
