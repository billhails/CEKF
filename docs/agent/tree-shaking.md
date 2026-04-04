# Tree Shaking (Dead Binding Elimination)

Dead binding elimination pass for the `--target-c` backend. Removes
unused `MinLetRec` bindings, primarily preamble/standard library
functions that were included but never referenced.

## Source

- `src/minlam_shake.c` — the pass implementation
- `src/minlam_freeVars.c` — free variable computation (reused from
  closure conversion)

## Pipeline Position

Runs after all beta/eta reductions and before closure conversion:

```text
  ↓ betaMinExp()           — final beta reduction
  ↓ etaMinExp()            — eta reduction
  ↓ shakeMinExp()          ← tree shaking
  ↓ flatClosureConvert()   — closure conversion
```

This placement matters: reductions have already simplified the IR (so
dead bindings are truly dead), and variable references are still symbolic
names (making free-variable computation straightforward). Closure
conversion would obscure the reference structure.

## Algorithm

The pass is a bottom-up tree traversal. It recurses into all MinExp
nodes, and at each `MinLetRec` it:

1. **Recurses first** into bindings and body (post-order), so inner
   letrecs are cleaned before outer ones.
2. **Collects keys** — the set of all binding names in this letrec
   (`getAllKeys`).
3. **Builds a dependency graph** — for each binding, computes free
   variables of its value expression intersected with the key set
   (`buildDependencyGraph`). This gives the set of sibling bindings each
   binding depends on.
4. **Computes roots** — free variables of the body intersected with keys
   (`computeRoots`). These are bindings directly used by the body.
5. **Marks live bindings** — worklist traversal from roots through the
   dependency graph (`computeLiveBindings`). A binding is live if it is
   reachable from a root.
6. **Retains only live bindings** — rebuilds the `MinBindings` list,
   dropping dead entries (`retainOnlyLive`). If all bindings are dead the
   letrec is replaced by its body.

The visitor functions for all other MinExp node types simply recurse and
rebuild only when a child changes (structural sharing).

## Impact

Many test binaries drop from 200+ registers to 20-30 registers, since
the bulk of unused preamble definitions are eliminated.

## Debugging

Enable `DEBUG_MINLAM_SHAKE` in `src/common.h` for trace output.
Use `--dump-shake` to inspect the IR immediately after the shake pass.
