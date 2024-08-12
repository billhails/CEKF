# Optimising Continuations

Profiling has demonstrated that at least in certain scenarios `step` spends
more time (27% of runtime) in `makeKont` than anywhere else. If we can
relegate continuations to be mostly just stack positions rather than
entire structures with their own stacks then we should be able to gain
a significant performance boost.

For the most part continuations are linear, and only invoked once. The only
exceptions to this are continuations captured by `call/cc`, and the Failure
Continuation.

Firstly we will need to allow the stack to grow unbounded, and secondly
we will need to keep a second stack of (integer) stack frame offsets and
a stack frame pointer along with a stack pointer.

Something like:

```
 S
| |<----+SP
| |     |
| |     |
| |<----+SF
| |     |   S F
| |<-+  |   P P
| |  |  +--| | |<--SFP
| |<-+-----| | |
```

Do we need to remember the actual stack pointer for a continuation?
Is the frame enough?

Anyway we'd distinguish "first class continuations" as being objects with
a snapshot of the entire stack that has to be restored when they are invoked,
from "simple continuations" which are merely a stack pointer and a frame
pointer.

For the fast lexical addressing to continue to work, the stack pointer
will have to be relative to the frame pointer rather than absolute, which
is adding an overhead, but then actual lexical access is just current frame
pointer + location.

Restoring a simple continuation might be as simple as popping the top frame
pointer, wheras restoring a first-class continuation would involve
overwriting the entire stack and frame stack. Since most of the time we're
only dealing with simple continuations this should be an improvement
overall.

## After Investigation

And a lot of head scratching, I have a plan.

First some observations:

1. LET, NS_PUSHSTACK and NS_PUSHENV all create a new continuation.
2. LET does **not** do anything to the current stack frame.
3. APPLY tears down the current stack frame and re-uses it, regardless of Kont ot Closure (TCO).
4. RETURN just pushes the current continuation and calls APPLY.
5. CALL/CC pushes the cc as an argument to the function.

And proposed changes:

1. LET, NS_PUSHENV and NS_PUSHSTACK shoud create a new frame which is a copy of the previous, and push a continuation without a stack.
2. APPLY of a continuation with a stack should replace the entire stack.
3. APPLY of a continuation without a stack (the normal case) should just pop a stack frame.
4. CALL/CC can check if the cc has a stack and if so use that cc, otherwise create a copy and snapshot the stack in to it.
5. New Stack type (done) has a secondary frames array, which stores previous frames and offsets, the current frame and offset are immediate.
6. Env,stack can remain an array (single frame), Cont.stack, Fail.stack and CEKFs.stack will be proper Stacks.
