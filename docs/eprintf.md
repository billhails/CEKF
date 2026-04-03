# Sort out `eprintf` &c

There are a number of inadequacies in the internal handling of i/o in F♮.

Firstly `eprintf`, defined in `src/errors.c` was originally intended
to write to `stderr`. It is even defined to write to `errout`, which
originally was `#define`'d as `stderr` but because of the abuse of
`eprintf` for normal output, it has been lately re-`#define`'d as
`stdout`.

Secondly the various pretty-printers would benefit from being able to
write to file handles rather than just using `eprintf`. Historically
they were only intended for debugging but they would be very useful
for testing if the could write to memstreams.

So:

1. Restore `eprintf` to it's original purpose of writing to stderr.
   * drop `errout` and just refer to `stderr` directly.
2. Continue to use `eprintf` to report errors and other out-of-band text.
3. Replace usage of `eprintf` for other purposes with plain `printf` or `fprintf`.
4. Update the pretty-printers, starting with `ppMinExp`, to use `fprintf` and to
   take an explicit file handle as first argument.

I'm wondering if we shouldn't have our own `oprintf` for `stdout`, I sense
the value of being able to control where the output goes, but I can't actually
justify it.

Regardless, the main problem is the enormous ubiquity of `eprintf` in almost every
file in the project.

## Scope Analysis

### Hand-written source files (`src/`) — ~740 calls across 32 files

| Category | Files | Approx. calls | Notes |
| --- | --- | --- | --- |
| Pretty-printers | `lambda_pp.c`, `minlam_pp.c`, `anf_pp.c`, `tpmc_pp.c` | ~390 | Mechanical — all become `fprintf(fp, ...)` once `FILE*` is added |
| Debuggers/annotators | `annotate.c`, `debug.c`, `tc_helper.c`, `tc_analyze.c` | ~210 | Needs intent analysis — mix of debug and error reporting |
| Infrastructure | `memory.c`, `hash.c`, `cekf.c` | ~46 | Mostly debug traces, all guarded |
| Error reporting | `step.c`, `main.c`, `builtin_sqlite.c`, `errors.c` | ~30 | Correct home for `eprintf` — leave as-is |
| Misc small files | ~15 files | ~60 | Trivial, mostly one-liners |

### Generated files (`generated/`) — ~2121 calls across 17 files

All in `*_debug.c` files. Fix once in the Python generator templates rather than
file-by-file.

The top offenders: `cps_kont_debug.c` (470), `ast_debug.c` (301),
`lambda_debug.c` (293), `anf_debug.c` (196), `minlam_debug.c` (159).

### Test files (`tests/`) — ~8 calls, trivial

### Suggested order of attack

1. **Pretty-printers** — highest value, most mechanical. Starting with `ppMinExp`
   as noted above is sensible; the pattern propagates to the other `*_pp.c` files.
2. **Generator templates** — fixes the bulk of raw call count in one place.
3. **Error/debug triage** — `tc_analyze.c` and friends: the error-context
   `eprintf` calls are genuine stderr output and correctly placed, but need the
   `errout` → `stderr` cleanup. Debug-guarded traces can be left as `eprintf`
   or converted as encountered.
