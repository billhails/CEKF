# Dead Binding Elimination for MinExp

Tree-shaking pass that removes unused bindings from `MinLetRec` nodes,
reducing the size of generated C output from `--target-c`.

## Placement in the Pipeline

After the final `betaMinExp()` pass but before closure conversion:

```text
MinExp (from desugaring)
  ↓ runCpsTrampolineTc()
  ↓ betaMinExp()
  ↓ ambMinExp()
  ↓ betaMinExp()
  ↓ etaMinExp()
  ↓ betaMinExp()
  ↓ shakeMinExp()            ← here
  ↓ flatClosureConvert()
  ↓ indexMinExp()
  ↓ emitProgram()
```

At this point all reductions have simplified the IR, variable references
are still symbolic names, and closure conversion has not yet obscured the
reference structure.

## Existing Building Block

`freeVarsMinExp()` in `src/minlam_freeVars.c` computes the free variables
of a MinExp subtree into a `SymbolSet`. The algorithm relies on this
throughout.

## Algorithm

The pass operates on `MinLetRec` nodes (the only place bindings are
introduced). Given a letrec with bindings `b1, b2, ..., bn` and a body:

### Step 1: Build a Dependency Graph

For each binding `bi` (where `bi.var = name_i`, `bi.val = expr_i`):

- Compute `freeVarsMinExp(expr_i, ...)` to get a `SymbolSet` of symbols
  that `expr_i` references.
- Intersect with the set of all binding names `{name_1, ..., name_n}`.
  Only references to sibling bindings matter, not outer scope.
- This gives `deps(name_i)`: the set of other bindings in this letrec
  that `name_i` depends on.

### Step 2: Compute Roots

Compute `freeVarsMinExp(body, ...)` and intersect with
`{name_1, ..., name_n}`. These are the root set: bindings directly
referenced by the body.

### Step 3: Mark (Transitive Closure)

Starting from the root set, do a worklist traversal:

```text
worklist = root set
live = {}
while worklist is not empty:
    pick name from worklist
    if name not in live:
        add name to live
        for each d in deps(name):
            add d to worklist
```

This is graph reachability. It terminates because the binding set is
finite.

### Step 4: Sweep

Walk the `MinBindings` linked list and rebuild it, keeping only bindings
whose `var` is in `live`. If all bindings are dead, replace the entire
`MinLetRec` with just its `body`.

### Step 5: Recurse

Apply the same transformation recursively to:

- The `val` of each surviving binding (which may contain nested letrecs).
- The `body` of the letrec.

A bottom-up traversal (post-order) is natural: clean inner letrecs first,
then outer ones. If eliminating inner dead code makes an outer binding
trivial or dead, the outer pass catches it.

## Iteration

Removing a binding could expose new dead bindings at a different nesting
level. A single bottom-up pass handles most cases. For completeness,
repeat until a fixed point (no bindings removed), but in practice one
pass is usually enough.

## Complexity

For a letrec with `n` bindings, the free-variable computation is
O(size of each binding's subtree), and the graph traversal is
O(n + edges). The overall pass is linear in the size of the IR.

## Limitations

This eliminates unused named bindings in letrecs. It does not eliminate
dead code inside a live function (e.g. an unreachable branch in an
`iff`). That would require constant propagation or similar analysis.
