# Alternative Bytecode Generator

Given the progress of the `--target-c` pipeline, it seems obvious
to also generate bytecode as a sibling to the final `minlam_emit_c`
step. That new emitter `minlam_emit_b` or whatever would target
a new runtime, hopefully much simpler than the existing CEKF `step`
runtime, with a view to deprecationg that old `step` function
at a later date.

## Reasons

1. The entire ANF path is quite divorced from the CPS path and there
   seems to me to be a real possibility of semantic differences between
   the two.
2. The ANF pipeline is relatively old and is not as amenable to optimization
   because of the more complex support structures.
3. The old bytecode is not memory-aligned and does a lot of memcpy operations
   to retrieve mis-aligned data, which has to be less efficient.
4. The old CEKF machine is overly-complicated, especially in its stack-handling
   and closure/continuation application.

## Requirements and Improvements

* bytecode will by default be immediately executed as in the current interpreter.
* bytecode will at least be word-aligned.
* bytecode can be read from and written to files.

## Implementation

The implementation can closely follow the existing `minlam_emit_c` model.
Where that model emits to memstreams, the bytecode implementation can
emit to some `BytecodeArray` type. The model of lambdas emitting to
private streams can be maintained with a pool of `BytecodeArray`. When
the lambda bytecodes are catenated into a final array their starting
positions (equivalent to C labels) can be recorded and backpatched into
the array slots that require them (this needs more thought). Of course
we don't have the luxury of emitting complex nested C expressions, but
that would be just as true if we were emitting LLVM.  We also need to
emit primitive equivalents to conditionals. We should be able to re-use
the existing minlam_runtime implementation, extending if necessary.

## Advantages of CPS for the Runtime

Since everything is CPS-transformed before emission, every call is a
tail call. This eliminates the need for a call stack, return instructions,
or continuation management in the VM. The runtime becomes essentially a
dispatch loop, dramatically simpler than the current `step` function.

## Register-based Instructions

The C path already uses a flat `reg[N]` array with `claimSlot()`/`releaseSlot()`.
Bytecode instructions can directly encode register indices in a 3-address
style (e.g. `PRIM_ADD dst, src1, src2`). Word-aligned register-based
instructions are straightforward to decode and naturally aligned.

Consider 32-bit fixed-width instructions with opcode and register fields
packed into a single word. Simple to decode, naturally word-aligned,
easy to index.

## Label Resolution

In `minlam_emit_c`, lambda addresses are C label identifiers resolved
by the C compiler. For bytecode, actual offsets are needed. Two approaches:

1. Pool of `BytecodeArray` per lambda, concatenate, then backpatch
   starting positions into slots that reference them.
2. Emit placeholder offsets and record them in a fixup list, then
   patch after all lambdas are serialized.

Either way, closures become `[bytecode_offset, env_vec]` instead of
`[&&LABEL, env_vec]`.

## Suggested Instruction Set

* `LOAD_IMM reg, value` -- immediates (int, char)
* `MAKE_VEC reg, n` -- build vector from n preceding regs
* `CLOSURE reg, offset, env_reg` -- `[offset, env]`
* `GOTO offset` -- unconditional jump (for `if` branches)
* `IF reg, offset_true, offset_false` -- conditional
* `CALL reg_closure, nargs` -- extract code+env, set regs, jump (all calls are tail in CPS)
* `PRIM_* dst, a, b` -- arithmetic/comparison
* `DONE reg` / `BACK` -- termination/backtracking

## Practical Notes

1. Start with immediate execution, not file I/O. Get the dispatch
   loop working first; serialization is independent and can come later.
2. Reuse `minlam_runtime.c` -- the `Value` type, vector operations,
   and GC integration are already there.
3. Structure `minlam_emit_b.c` as a direct parallel to `minlam_emit_c.c`
   -- same walker, same lambda collection via `BufferBag`, but emitting
   opcodes into `BytecodeArray` instead of `fprintf` to memstreams.
4. The pipeline is identical up to emission -- CPS, beta, amb, eta,
   closure convert, index, then emit. Gate on a `--target-b` flag
   initially, switching to default later.

## Lexical Indexing

The current `minlam_annotate.c` assigns `(frame, offset)` pairs for
nested environment lookup in the C path's register array. For bytecode
a simpler flat register index per lambda frame may suffice, which is
effectively what the C path already uses after closure conversion
flattens environments.
