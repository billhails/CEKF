# Byte Code Documentation

* `ADD` - binop
* `AMB [jmp]` - creates failure that resumes at jmp
* `APPLY [nargs]` - `[arg] [arg] .. [callable] => result`
* `BACK` - pops current failure continuation and resumes it
* `BIGINT [bigint]` - pushes the literal bigint
* `BIGINT_IMAG [bigint]` - pushes the literal bigint as imaginary
* `CALLCC` - pop the callable, push the current continuation, push the callable and apply
* `CHAR [character]` - pushes the literal character (unicode)
* `CHARCOND [size] [val][jmp] [val][jmp] ...` pop value, search table and jump
* `CMP` - binop
* `CUT` - pops current failure continuation and discards it
* `DIV` - binop
* `DONE` - halt
* `EQ` - binop
* `ERROR` - error
* `GE` - binop
* `GT` - binop
* `IF [jmp]` - pop value and jump if false
* `INTCOND [size] [inttype][val][jmp] [inttype][val][jmp] ...` pop value, search table and jump
* `IRRATIONAL [float]` - pushes the literal float
* `IRRATIONAL_IMAG [float]` - pushes the literal float as imaginary
* `JMP [jmp]` - unconditional jump
* `LAM [nargs] [letrec offset] [end]` - create a closure and push it
* `LE` - binop
* `LET [jmp]` - duplicates the top stack frame, create a new continuation to resume the body (at `jmp`) then lets control pass to the expression.
* `LETREC [nargs]` - expects `nargs` closures at ToS, patches each with current S (i.e. themselves)
* `LT` - binop
* `LVAR [offset]` - look up a stack (local) variable and push it
* `MAKEVEC [size]` - pop size values create a vec of them and push it
* `MATCH [size] [jmp] [jmp] ...` pop index and jump (size is just for safety check)
* `MOD` - binop
* `MUL` - binop
* `NE` - binop
* `NONE` - error
* `NS_END [numLambdas] [stackOffset]` - move the new lambdas at ToS into the namespace
* `NS_FINISH [num]` - patch each of the namespaces with the final block of populated namespaces, size `num`, at ToS
* `NS_POP` - pop value, restore previous stack frame, push value
* `NS_PUSHENV [frame] [offset]` - find the namespace in env at frams/offset, duplicate the top stack frame, restores namespace to stack
* `NS_PUSHSTACK [offset]` - find the namespace on stack at offset, duplicates the top stack frame, restore namespace to stack
* `NS_START [num]` - pushes num voids on to stack
* `POW` - binop
* `PUSHN [n]` - allocate n bytes of stack
* `RETURN` - push the current continuation and apply
* `STDINT_IMAG [int]` - pushes the literal int as imaginary
* `STDINT [int]` - pushes the literal int
* `SUB` - binop
* `VAR [frame] [offset]` - look up an env variable and push it
* `VEC` - binop: index, vector => value at index
