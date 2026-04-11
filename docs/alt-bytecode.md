# Alternative Bytecode Generator

This note proposes a new bytecode target as a sibling to `minlam_emit_c`.
It should be treated as a fresh start built around the current CPS-based
`--target-c` path.

That distinction matters. The current C emitter already gives us a clear
calling convention, closure protocol, and register discipline. The new
bytecode target should preserve those semantics closely so that `--target-c`
and `--target-b` stay obviously equivalent.

## Why a New Bytecode Target Still Makes Sense

1. The current `--target-c` path has diverged far enough from the old bytecode
   emitter that semantic drift is a real concern unless there is a bytecode
   target built directly around the current pipeline.
2. The current `--target-c` path is structurally simple enough to support a
   correspondingly simple runtime.
3. A word-aligned encoding avoids byte-stream payload decoding and misaligned
   payloads via `memcpy`, which is not attractive as a long-term format.
4. A new bytecode target gives us file I/O, inspection, and testing options
   without having to commit to generated C as the only executable artifact.

## Review of the Original Sketch

The original sketch had the right overall direction, but it left a few crucial
points undecided.

1. It did not tie the design tightly enough to the actual behavior of
   `minlam_emit_c.c`.
2. It did not clearly separate the core execution model from incidental
   implementation choices.
3. It suggested closures as `[offset, env_vec]` but did not decide whether that
   is only a logical model or the physical runtime representation.
4. It mentioned dispatch tables without specifying whether they should be
   inline in the code stream or kept as side tables.
5. It mentioned big integers without deciding whether they are instruction
   payloads, pool entries, or runtime objects.

The rest of this document makes those decisions explicit.

## Ground Truth from `minlam_emit_c.c`

The bytecode target should preserve these facts from the existing C emitter.

### Calls are tail transfers

`emitGoto()` copies outgoing arguments into `reg[0..N-1]` and then jumps to the
callee entry via computed goto. There is no ordinary call/return discipline in
the generated C path.

This strongly suggests a register VM with tail-call transfer, not a stack VM
with `CALL` and `RETURN`.

### The register file is flat

After closure conversion and indexing, variables are emitted as direct register
references such as `reg[pos]`. The bytecode target should preserve that direct
register discipline.

### Closures are created in two phases

`emitMinLetRec()` emits closure skeletons first and patches their environments
afterwards. Semantically the protocol is:

1. create closure skeleton with entry and empty environment.
2. build the environment value.
3. backpatch the closure environment.

Whatever physical representation the bytecode runtime chooses, it must preserve
that protocol.

### Builtins are a separate call path

`emitApplyBuiltin()` does not treat builtins as ordinary closures. It computes
the builtin result and then tail-calls the continuation closure `k` with
`(env, result, f)`.

That suggests a dedicated builtin-call opcode or helper path, not an attempt to
pretend that builtins are ordinary bytecode lambdas.

### Conditionals are structured control flow

`emitMinIff()`, `emitMinCond()`, and `emitMinMatch()` lower to ordinary C
branching and dispatch. The new bytecode target should preserve those semantics
directly.

### Big integer literals are heap values

`emitMaybeBigInt()` lowers big integer literals through `minlam_runtime_BigInt`,
which allocates a GC-managed `BigInt`. These literals are not simple immediates.

## Scope

This proposal is specifically for the current CPS-based minlam pipeline.

That means the design should assume:

1. exact-arity tail calls are the normal case.
2. explicit register addressing is available after indexing.

There is also an explicit persistence requirement:

1. the emitted program must be representable in a file and executable later in
   a different process.

That rules out any serialized representation that depends on process-local
addresses. Code pointers, heap pointers, builtin function pointers, and pointer-
shaped dispatch tables are acceptable only as in-memory runtime structures after
loading. They are not acceptable as the on-disk or assembly-level format.

## High-Level Structure

The cleanest split is into four layers.

1. `minlam_emit_b.c`.
   This mirrors `minlam_emit_c.c` and lowers minlam to a bytecode builder API.
2. A bytecode assembler.
   This owns code buffers, lambda labels, relocations, constant-pool entries,
   and dispatch-table entries.
3. A runtime interpreter.
   This executes the word-aligned instruction stream.
4. Optional serializer and loader.
   This can come later without changing the tree walk or the runtime semantics.

That separation keeps the emitter logic close to the current implementation and
isolates bytecode-format choices in one place.

## Execution Model

The new VM should be register-based.

### Register convention

Keep the same conceptual convention used by `minlam_emit_c.c`.

* `reg[0]` is the captured environment of the callee.
* `reg[1..n]` are explicit arguments.
* temporaries live in higher registers.
* a tail call repopulates low registers and jumps.

That gives a direct correspondence between emitted C and emitted bytecode.

### Runtime state

The minimal runtime state is:

* `pc` as a word offset into code.
* `reg[]` as a flat `Value` array.
* a failure stack only if the CPS/amb path still emits explicit backtracking.
* a final result slot for `DONE`.

It should not need a general call stack, return-oriented control flow, or
application machinery beyond what the current minlam target actually uses.

## Code Representation

Use a word-aligned instruction stream.

This instruction stream may be emitted directly as a binary format or emitted as
an assembly language which is then assembled into that binary format. The
important constraint is that the representation crossing the file boundary must
be fully symbolic or index-based, never pointer-based.

### Code Representation Recommendation

Use `uint32_t` code words.

Not every instruction has to be exactly one word. The important property is
that each instruction begins on a word boundary and any trailing payload is a
whole number of words.

That gives:

1. simple decoding.
2. straightforward relocation.
3. no misaligned payload reads.
4. clean file serialization.

### Suggested first-word layout

The first word can be split into:

* 8 bits opcode.
* 8 bits destination register.
* 8 bits operand A.
* 8 bits operand B or small immediate.

Instructions that need larger operands can consume extra words.

This is a good balance between fixed-width simplicity and practical operand
range.

### Persistence invariant

Every cross-reference in the stored format must be one of:

* a code offset.
* a section-relative offset.
* a table index.
* a constant-pool index.
* a symbolic label in assembly form.

It must never be:

* a raw C function pointer.
* a raw heap pointer.
* a pointer to a `BigInt`, `Vec`, closure, or dispatch table.

This is the central constraint that keeps later execution and serialization
honest.

## Closures and Lambdas

There are two separate questions here.

1. What closure semantics must be preserved.
2. What physical representation should the runtime use.

### Semantics to preserve

From `minlam_emit_c.c` we know that:

* every closure has an entry.
* every closure has an environment.
* recursive closures are created before their environment is known.
* recursive closures are patched afterwards.

### Recommended physical representation

Use the existing `Vec` representation already used by the C emitter: a 2-element
vector where `vec[0]` holds the entry and `vec[1]` holds the environment. This
is already the encoding produced by `emitMakeClosure()`:

```c
make_vec(2, value_Addr(&&LABEL), value_None())
```

For the bytecode runtime the only difference is that `vec[0]` holds a word
offset (`VALUE_TYPE_STDINT`) instead of a native code pointer. This is already
the natural entry encoding for a portable bytecode image.

This avoids adding a new variant to the `Value` union in `cekfs.yaml`, which is
shared with the old CEKF interpreter. Introducing a `BClo` type would require
touching that shared definition and could have unintended consequences for the
existing machine. Reusing `Vec` costs nothing: `VEC_GET_IMM` already serves as
the entry and environment accessor, and the two-phase letrec protocol maps
directly onto `CLOSURE_NEW` (allocate vec, write offset into slot 0, write
`None` into slot 1) and `CLOSURE_SET_ENV` (overwrite slot 1).

### Letrec support

The instruction set should support the logical two-phase protocol directly.

Suggested operations:

* `CLOSURE_NEW dst, lambda_id`
* `CLOSURE_SET_ENV clo_reg, env_reg`

That makes the recursive-closure protocol explicit in the instruction set.

The important detail for persistence is that `lambda_id` must resolve via the
loaded code image, not via an embedded native address.

## Constants and Values

The new target should distinguish three classes of literals.

### Small immediates

These can be encoded directly in code.

* small signed integers.
* characters.
* `None`.

Suggested instructions:

* `LOAD_I32 dst, imm32`
* `LOAD_CHAR dst, codepoint`
* `LOAD_NONE dst`

### Heap literals

These should live in a constant pool.

* big integers.
* irrationals.
* complex and rational literals.
* future larger constants.

Suggested instruction:

* `LOAD_CONST dst, const_index`

### Builtin references

Builtins should also be referenced through metadata rather than embedded as raw
C pointers in the instruction stream.

That means the stored format should use stable builtin identifiers such as:

* builtin table indices.
* builtin linker names in assembly form.

The loader can map those identifiers to actual function pointers when the code
image is instantiated in memory.

## Big Integer Representation

The original sketch needed more precision here.

### Big Integer Recommendation

Do not inline arbitrary-precision digits into executable code.
Store big integer literals in a constant pool and load them by index.

This is preferable because:

1. the instruction stream stays compact.
2. serialization becomes canonical.
3. constants can be allocated and rooted once.
4. repeated literals are cheap to reuse.

### Serialized form

Serialize only meaningful digits, not spare capacity.

A bigint constant record should contain:

* tag = bigint.
* sign.
* used word count.
* words.

For immutable literals there is little value in serializing unused capacity.

### Runtime loading

At load time, turn constant-pool bigint records into GC-managed `BigInt`
objects and store them in a `Value consts[]` array.

That array must be marked as a GC root alongside the live register file.

This fits naturally with the existing `minlam_runtime.c` value helpers.

The same rule applies if an assembly format is used instead of direct binary:
the assembly may spell out bigint words textually, but the assembled artifact
must still materialize them as constant-pool records or equivalent relocatable
data, not as host pointers.

## Dispatch Tables

There are two different dispatch-table questions.

### VM opcode dispatch

For the interpreter itself, start with a plain `switch(opcode)` loop.

That is the right initial implementation because it is easy to debug and keeps
instruction semantics obvious. A computed-goto dispatch table can be added
later behind an `#ifdef` if it is worth it.

### Source-level branch dispatch

For `match`, integer conditionals, and character conditionals, use side tables
rather than embedding ad hoc tables inline in the code stream.

This is simpler to inspect, relocate, and serialize.

It also satisfies the persistence requirement cleanly, because dispatch tables
can be serialized entirely in terms of constant indices and code offsets.

### Dense match table

This corresponds to `emitMinMatch()`, where the selector is already an integer
index.

```c
struct MatchTable {
    uint32_t count;
    uint32_t targets[count];
};
```

We can use the `UIntArray` from `utils.yaml` for this.

Suggested instruction:

* `MATCH test_reg, table_id`

Semantics:

* require the register to hold an integer in `[0, count)`.
* branch to `targets[index]`.

Each `target` is a code offset or relocatable label, never a native label
address.

### Integer conditional table

```c
struct IntCondCase {
    uint32_t const_index;
    uint32_t target;
};

struct IntCondSwitch {
    uint32_t count;
    uint32_t default_target;
    IntCondCase cases[count];
};
```

These translate to

```yaml
structs:
    IntCondSwitch:
        meta:
            brief: integer switch
        data:
            default_target: index
            cases: IntCondCaseArray

    IntCondCase:
        meta:
            brief: individual integer case
        data:
            const_index: index # index into const table
            target: index # index into bytecode array

arrays:
    IntCondCaseArray:
        data:
            entries: IntCondCase

    IntCondTable:
        data:
            entries: IntCondSwitch
```

Suggested instruction:

* `INTCOND test_reg, table_id`

Semantics:

* compare `test_reg` against pool values via the same comparison logic used by
  `minlam_runtime_cmp()`.
* branch to the first matching target, else to `default_target`.

Both `const_index` and branch targets are serializable identifiers. Neither may
degenerate into a pointer in the stored representation.

### Character conditional table

```c
struct CharCondCase {
    uint32_t codepoint;
    uint32_t target;
};

struct CharCondTable {
    uint32_t count;
    uint32_t default_target;
    CharCondCase cases[count];
};
```

These translate to

```yaml
structs:
    CharCondSwitch:
        meta:
            brief: character switch
        data:
            default_target: index
            cases: CharCondCaseArray

    CharCondCase:
        meta:
            brief: individual character case
        data:
            codepoint: character
            target: index

arrays:
    CharCondCaseArray:
        data:
            entries: CharCondCase

    CharCondTable:
        data:
            entries: CharCondSwitch
```

Suggested instruction:

* `CHARCOND test_reg, table_id`

Again, table entries must hold literal codepoints and code offsets, not native
addresses.

### Why side tables are preferable

For the new target, side tables are cleaner because:

1. code relocation stays localized.
2. dumps are easier to read.
3. file format evolution is simpler.
4. the interpreter loop stays small.

## Recommended Instruction Set

The instruction set should mirror the current minlam emitter. Where the choice
is neutral with respect to the existing design, prefer LLVM-style conventions.

### Basic block discipline

Organize each lambda body as a sequence of labeled basic blocks. Every basic
block ends with exactly one terminator instruction. No instruction may appear
after a terminator within a block. There is no implicit fall-through.

Valid terminators are:

* `TAILCALL` — the normal tail-call transfer.
* `DONE` — terminates the whole program.
* `JMP` — unconditional branch.
* `JMP_FALSE` — conditional branch.
* `MATCH` — dispatch-table branch.
* `INTCOND` — integer conditional branch.
* `CHARCOND` — character conditional branch.

This matches LLVM's terminator discipline exactly and is a natural fit for
CPS-lowered code: every tail call is already a terminator and every branch
already ends a basic block. The discipline costs nothing and makes the bytecode
easier to analyse, dump, and verify.

In a textual assembly form, each basic block begins with a label and ends before
the next label or the end of the lambda definition. This is the same convention
used in LLVM IR.

### Data movement and literals

* `MOVE dst, src`
* `LOAD_NONE dst`
* `LOAD_I32 dst, imm32`
* `LOAD_CHAR dst, codepoint`
* `LOAD_CONST dst, const_index`

### Aggregate values

* `MAKE_VEC dst, count`
* `VEC_SET vec_reg, index_imm, src`
* `VEC_GET_IMM dst, vec_reg, index`

`MAKE_VEC` allocates a vector of the given size with all slots initially
`None`. Individual slots are then filled with `VEC_SET`. This matches the
actual usage in `minlam_emit_c.c`: `emitMakeVec()` collects source values via
`emitRL()`, which returns references at arbitrary register positions rather than
a guaranteed contiguous run. A contiguous-range shorthand would be unsound for
the closure environment case, where free variables land wherever closure
conversion and De Bruijn indexing placed them.

### Closures

* `CLOSURE_NEW dst, lambda_id`
* `CLOSURE_SET_ENV clo_reg, env_reg`

### Primitives

For the primops already used by the minlam emitter:

* `PRIM_ADD dst, left, right`
* `PRIM_SUB dst, left, right`
* `PRIM_MUL dst, left, right`
* `PRIM_DIV dst, left, right`
* `PRIM_GCD dst, left, right`
* `PRIM_LCM dst, left, right`
* `PRIM_CANON dst, left, right`
* `PRIM_POW dst, left, right`
* `PRIM_MOD dst, left, right`
* `PRIM_EQ dst, left, right`
* `PRIM_NE dst, left, right`
* `PRIM_GT dst, left, right`
* `PRIM_LT dst, left, right`
* `PRIM_GE dst, left, right`
* `PRIM_LE dst, left, right`
* `PRIM_CMP dst, left, right`
* `PRIM_VEC dst, index_reg, vec_reg`

These names are convenient shorthand for the operations already present in the
current minlam lowering.

### Builtins

Use a dedicated builtin-call path.

Suggested instruction:

* `CALL_BUILTIN builtin_id, dst, argv_reg`

The surrounding minlam code can then explicitly tail-call the continuation with
that result, just as `emitApplyBuiltin()` does today.

`builtin_id` must be stable across processes. It should resolve through a
loader-maintained builtin table, not by embedding a compiled function pointer in
the bytecode image.

### Control flow

* `JMP target` (2 words)
* `JMP_FALSE test_reg, target` (2 words, `test_reg` is packed)
* `MATCH test_reg, table_id`
* `INTCOND test_reg, table_id`
* `CHARCOND test_reg, table_id`

### Calls and termination

* `TAILCALL clo_reg`
* `DONE exit_status`

`TAILCALL` should:

1. read the closure entry from `clo_reg`.
2. jump to the closure entry.

The caller is responsible for pre-loading `reg[0..N-1]` with explicit `MOVE`
instructions before issuing `TAILCALL`. This directly mirrors `emitGoto()`,
which emits individual `reg[i] = sourceSlots[i]` assignments — where the source
slots are at arbitrary register positions — before the jump. A contiguous source
range cannot be assumed because the slot allocator does not guarantee it.

The env slot (`reg[0]`) is no different from any other argument in this respect.
It is extracted from the closure with `VEC_GET_IMM` and then moved into place
with `MOVE`, just like any other outgoing argument.

This is the bytecode equivalent of `emitGoto()`.

The stored form of the closure entry is therefore an offset or relocatable code
label, never a process-local code pointer.

### Operator Extension

A single operator `EXT` should signal that the next operator is completely unpacked
into separate words, for example when accessing a register, builtin index or table
index greater than $2^8$.

## Design Exclusions

Several categories of mechanism should stay out of the initial design unless
they become necessary.

### Stack-machine operand discipline

The current minlam emitter already assumes a flat register machine. A stack VM
would add translation noise with no clear benefit.

### Partial and over-application machinery

The CPS minlam target should aim for exact-arity tail calls as a compile-time
invariant.

### Explicit return instructions

The CPS target does not need ordinary call-return control flow. `TAILCALL` and
`DONE` should be enough.

### Environment-walking opcodes

If the indexed minlam IR already gives direct register positions, the new VM
should not introduce frame-and-offset lookup opcodes.

## Emission Strategy

`minlam_emit_b.c` should be structurally identical to `minlam_emit_c.c` in almost
every respect. The same tree-walking functions, the same slot-allocation
discipline, the same lambda pool management, the same `EmitterContext` extension
protocol, and the same two-phase letrec protocol all carry over directly.

The only difference is what the leaf emission operations produce. In
`minlam_emit_c.c` they write C text to a `FILE *` memstream via `fprintf`. In
`minlam_emit_b.c` they append instruction words to a code buffer. Everything
above that leaf level can be shared verbatim or duplicated with minimal
adaptation.

This equivalence is the central maintenance argument for keeping the two emitters
in sync. If a pass adds handling for a new minlam form in `minlam_emit_c.c`, the
same change has a direct and obvious counterpart in `minlam_emit_b.c`.

### Recommended correspondence

* `emitSimpleExp()` writes a value into a target register.
* `emitMakeVec()` lowers to `MAKE_VEC` followed by one `VEC_SET` per element.
* `emitAnonymousLambda()` creates a lambda-table entry plus `CLOSURE_NEW`.
* `emitApplyClosure()` lowers to `TAILCALL`.
* `emitApplyBuiltin()` lowers to `CALL_BUILTIN` plus a continuation tail call.
* `emitMinIff()`, `emitMinCond()`, and `emitMinMatch()` lower to branch or
  dispatch-table instructions.
* `emitMinLetRec()` lowers to closure creation plus environment patching.

The emitter should still maintain a lambda pool and patch symbolic references
after layout, just as the C compiler currently resolves labels for the C target.

## Relocation and Layout

Use symbolic labels during emission and resolve them after all lambdas are laid
out.

### Metadata needed per lambda

Keep at least:

* symbolic name.
* arity.
* entry word offset.
* maximum temporary register used by that lambda.

### Fixups you will need

* `CLOSURE_NEW` lambda references.
* `JMP` and `JMP_FALSE` targets.
* dispatch-table targets.
* entry-point reference.

If the format supports external builtin references in the stored image, then
those also need a relocation or name-resolution path at load time.

This is the same problem that `minlam_emit_c.c` solves with C labels, except
that the assembler now performs the final resolution.

## Runtime Services Worth Reusing

The current `minlam_runtime.c` is still useful.

It already provides:

* `Value` construction.
* vector construction.
* numeric comparison.
* arithmetic compatibility across numeric types.
* GC integration points.

The new runtime should reuse those value and arithmetic helpers where practical.

## Missing Infrastructure Worth Adding Early

Several pieces are easy to postpone too long, but they should be part of the
design from the start.

### Text dump

A disassembler that can render the bytecode image as human-readable text should
be treated as required infrastructure, not an optional extra. It is the primary
debugging tool during development of both the emitter and the interpreter, and
it is the natural output format for a `--dump-b` flag parallel to the
existing `--dump-anf` and `--dump-lambda` flags (`--dump-bytecode` is already in
use on the ANF branch).

The text format should be self-contained: given only the text output, a reader
should be able to understand the full structure of the program.

### Comment side table

The bytecode emitter should maintain a side table mapping word offsets to
arbitrary comment strings, populated as emission proceeds. This is the bytecode
counterpart of the inline `// emitGoto`, `EMITLOC`, and similar diagnostic
comments that `minlam_emit_c.c` writes into the C output via `fprintf`.

The comment table is not part of the execution semantics and need not be present
in a stripped binary. It should, however, be included when the text dump is
produced, printed alongside the instruction it annotates. That makes the dump
readable at the same level of detail as the generated C.

A natural population strategy: every point in `minlam_emit_b.c` that would have
called `fprintf(FH(ctx), "// ...", ...)` in the C emitter instead records an
entry in the comment table.

### Source-location side table

The format should support source locations as a side table keyed by word
offset rather than ad hoc inline markers.

### Versioned file header

Even before file I/O is implemented, the format should reserve room for:

* magic number.
* version.
* endianness marker.
* section directory.

The section directory should be sufficient to describe at least:

* code section.
* constant pool.
* dispatch-table section.
* lambda metadata.
* builtin-reference table if needed.
* comment table (optional, may be stripped).

### Lambda debug names

Keep the generated lambda name or source binding name in metadata. That will
help enormously when dumping bytecode or diagnosing runtime failures.

### GC roots for constants

The constant pool must be marked alongside the live register file.

### Loader contract

The loader should be responsible for only two kinds of runtime rebinding:

* turning serialized constants into in-memory `Value` objects.
* resolving builtin IDs or names into callable runtime entries.

Everything else should remain representable by offsets, indices, and symbolic
labels so that the bytecode image stays portable and reproducible.

## Recommended First Cut

The first implementation should be deliberately narrow.

1. register VM.
2. word-aligned code stream.
3. exact-arity `TAILCALL` only.
4. dedicated closure object with backpatchable environment.
5. constant pool for bigints and other nontrivial literals.
6. side tables for `MATCH`, `INTCOND`, and `CHARCOND`.
7. `switch`-based interpreter loop.
8. comment side table populated during emission.
9. text dump covering all sections including comments.

That is enough to validate semantic parity with `minlam_emit_c.c` without
bringing in extra runtime complexity prematurely.

## Summary

The main design decision is this: the new bytecode target should be a bytecode
rendering of the current minlam C emitter.

If that discipline is maintained, then:

* the instruction set stays small.
* dispatch tables stay structured.
* bigint handling stays explicit and correct.
* letrec patching stays faithful to the current emitter.
* semantic drift between `--target-c` and `--target-b` is minimized.

## Implementation Notes and Progress

* Re-usable defintions have been moved out of `emit_c.yaml` into a new `emit.yaml`.
* `emit.yaml` contains the common definitions for slot handling, while `emit_c.yaml` retains the `EmitBuffer` opaque memstream definitions.
* A new sibling `emit_b.yaml` also uses structures from `emit.yaml` and adds alternative bytecode structures discussed in the document above.
* `emit.yaml` also defines a common inline `EmitterContext` for common context between implementations, and `emit_b.yaml` and `emit_c.yaml` include that in their respective `BEmitterContext` and `CEmitterContext` visitor contexts.
* Common slot pool and heap manipulation now lives in `emit_helper.c`.
* Where possible pointers to the common `EmitterContext` struct are used in preferance to the broader emitter-specific context, and those functions that only require that context have also been moved out of `minlam_emit_c.c` into `emit_helper.c` since they are demonstrably emitter agnostic.
* No attempt has been made to create any visitor for bytecode emission yet.
