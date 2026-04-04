# Plan: Generator `FILE*` Threading for `eprintf` Cleanup

## TL;DR

Thread `FILE *fp` through all generated `print*` functions in `*_debug.c` by
updating 9 Python generator modules. Three prerequisite changes first: update
`PrintHashValueFunction` and add `fprintHashTable` alongside `printHashTable` in
`hash.{c,h}`, and create `fprint`-prefixed alternatives for the 4 `printFn`
functions referenced from `primitives.yaml`.

---

## Phase 1 — Prerequisite: `fprint*` alternatives for `primitives.yaml`

Rather than modifying existing `print*` functions (which would break current
callers), create new `fprint`-prefixed alternatives and update
`src/primitives.yaml` to reference them. The old `print*` functions are left
untouched.

1. `src/bigint.c` + `src/bigint.h`: rename the existing 2-arg
   `fprintBigInt(FILE *, BigInt *)` → `fprintBigInt2` and
   `fprintMaybeBigInt(FILE *, MaybeBigInt *)` → `fprintMaybeBigInt2`, updating
   all callers (`src/step.c` ×3, `src/builtin_io.c` ×1, `src/bigint.c` itself
   ×2 in the `printBigInt`/`printMaybeBigInt` bodies). Then add the new 3-arg
   `fprintBigInt(FILE *fp, BigInt *x, int depth)` and
   `fprintMaybeBigInt(FILE *fp, MaybeBigInt *x, int depth)` — each delegating
   to the renamed `fprintBigInt2`/`fprintMaybeBigInt2`.
2. `src/ast_helper.c` + `src/ast_helper.h`: add `fprintAstSymbol(FILE *fp,
   struct HashSymbol *x, int depth)` mirroring `printAstSymbol` but using
   `fprintf(fp, ...)` instead of `eprintf(...)`.
3. `src/opaque.c` + its header: add `fprintOpaque(FILE *fp, Opaque *x, int depth)`
   mirroring `printOpaque` but using `fprintf(fp, ...)`.
4. `src/primitives.yaml`: update `printFn` entries from `printBigInt` →
   `fprintBigInt`, `printMaybeBigInt` → `fprintMaybeBigInt`,
   `printAstSymbol` → `fprintAstSymbol`, `printOpaque` → `fprintOpaque`.

## Phase 2 — Prerequisite: `fprintHashTable` infrastructure

Add `fprintHashTable` alongside the existing `printHashTable` rather than
replacing it. `printHashTable` becomes a thin shim delegating to
`fprintHashTable(stdout, ...)`, leaving all existing callers untouched.

1. `src/hash.h`: change typedef from `void (*PrintHashValueFunction)(void *,
   int)` → `void (*PrintHashValueFunction)(FILE *, void *, int)`; add
   `void fprintHashTable(FILE *fp, HashTable *, int)` declaration.
2. `src/hash.c`: add `fprintHashTable(FILE *fp, HashTable *table, int depth)`
   with the full implementation (all `eprintf(...)` → `fprintf(fp, ...)`,
   callback called as `table->printfunction(fp, valuePtr(...), depth+2)`).
   Update `printHashTable` to be a one-liner shim:
   `void printHashTable(HashTable *table, int depth) { fprintHashTable(stdout, table, depth); }`

Note: `printHashSymbol` is called inside the implementation — rename to
`fprintHashSymbol` and pass `fp` through, keeping `printHashSymbol` as a
`stdout` shim for the same reason.

## Phase 3 — Python generator

All steps are parallel once Phase 1/2 decisions are settled.

| Step | File | What to change |
|---|---|---|
| 6 | `tools/generate/signature_helper.py` | `print_signature()`: `f"void print{name}({type_decl} _x, int depth)"` → `f"void print{name}(FILE *fp, {type_decl} _x, int depth)"` |
| 7 | `tools/generate/generate.py` | `generate_debug_implementation()`: change `pad()` body to `static void pad(FILE *fp, int depth) { fprintf(fp, "%*s", depth * PAD_WIDTH, ""); }` |
| 8 | `tools/generate/simple_struct.py` | All `eprintf(...)` → `fprintf(fp, ...)`; all recursive `print{Name}(_x..., depth+1)` → `print{Name}(fp, _x..., depth+1)` in `printPrintFunction`, `printPrintFunctionBody`, `printPrintField`, `printPrintHashField` |
| 9 | `tools/generate/discriminated_union.py` | `printPrintFunctionBody()`: `eprintf("\\n")` → `fprintf(fp, "\\n")`; pass `fp` to field's `printPrintField` |
| 10 | `tools/generate/discriminated_union_field.py` | `printPrintCase()`: `eprintf("{typeName}\\n")` → `fprintf(fp, ...)`; pass `fp` through |
| 11 | `tools/generate/simple_array.py` | `getPrintSignature()` hardcoded → add `FILE *fp`; all `eprintf` → `fprintf(fp, ...)`; pass `fp` to subcalls |
| 12 | `tools/generate/hashes.py` | `getPrintSignature()` hardcoded → add `FILE *fp`; `printPrintFunction()` calls `fprintHashTable(fp, ...)`. Inline callback: `static void _print{Name}(void *ptr, int depth)` → `static void _print{Name}(FILE *fp, void *ptr, int depth)` (conforms to updated `PrintHashValueFunction`). `printPrintField`, `printPrintHashField`: pass `fp` to `fprintHashTable(fp, ...)` |
| 13 | `tools/generate/enum_field.py` | `printPrintCase()`: `eprintf("{typeName}")` → `fprintf(fp, "{typeName}")` |
| 14 | `tools/generate/simple_field.py` | `printPrintLine()` and `printPrintHashLine()`: thread `fp` as first arg through `obj.printPrintField` and `obj.printPrintHashField` — requires updating all target method signatures to accept `fp` |
| 15 | `tools/generate/primitives.py` | `printPrintField()`: `eprintf(fmt, val)` → `fprintf(fp, fmt, val)`; `{printFn}(val, depth+1)` → `{printFn}(fp, val, depth+1)`. Same in `printPrintHashField()`. |

**Special case — `simple_field.py` threading**: `printPrintLine` and
`printPrintHashLine` delegate to `obj.printPrintField` /
`obj.printPrintHashField`. Adding `fp` there requires all implementing classes
(`simple_struct.py`, `hashes.py`, `primitives.py`, `discriminated_union.py`)
to accept `fp` in their `printPrintField` and `printPrintHashField` methods.

## Phase 4 — Regenerate and verify

1. Run `make` (or the generator script directly) to regenerate
   `generated/*_debug.{c,h}`.
2. Search for any hand-written callers of generated `print*` functions and
   add `fp` argument. Use `grep -r 'print[A-Z]' src/` to find candidates.
3. `make` — clean build.
4. `make test` — all tests pass.
5. `grep -r 'eprintf' generated/` — zero matches.

---

## Decisions

- `PrintHashValueFunction` typedef gets `FILE *fp` — cleanest option; updating
  the `hashes.py` callback to match avoids any need for thread-local state or
  other workarounds.
- New 3-arg `fprintBigInt`/`fprintMaybeBigInt` take the clean names; the
  existing 2-arg variants are renamed to `fprintBigInt2`/`fprintMaybeBigInt2`
  and their callers updated first to keep the build clean throughout.
- `fprintHashTable` naming follows the C `fprintf`/`printf` convention.
- Scope excludes: `eprintf` in non-pp/non-debug code (error reporting,
  `tc_analyze.c` diagnostic output) — separate pass.
