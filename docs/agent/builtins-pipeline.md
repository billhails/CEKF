# How Builtins Work in the Current Pipeline

This document describes how built-in functions flow through the existing
bytecode pipeline, from registration to runtime invocation. It was
written to inform the design of the new CPS/closure-conversion/C-target
pipeline.

## Overview

Built-in functions have a dual identity: an **external name** visible to
user code (e.g. `rand`, `puts`, `assertion`) and an **internal name**
used after wrapper expansion (e.g. `builtin$rand`, `builtin$puts`).
The internal name is mechanically derived by prepending `builtin$` to
the external name.

The key design principle is that **user code never calls builtins
directly**. Instead, the parser synthesises thin wrapper functions
that forward to the internal name. This means the rest of the
pipeline (type checking, lambda conversion, optimisation, ANF, bytecode)
only sees ordinary function calls, with one exception: the `isBuiltin`
flag on apply nodes.

## Stage-by-Stage Detail

### 1. Registration (`builtins_helper.c`)

`registerBuiltIns()` creates a `BuiltIns` array. Each entry is a
`BuiltIn` struct containing:

- `externalName` — the user-visible `HashSymbol` (e.g. `rand`)
- `internalName` — the `HashSymbol` with `builtin$` prefix
  (e.g. `builtin$rand`)
- `result` — the return `TcType` (for type checking)
- `args` — a `BuiltInArgs` array of argument `TcType`s
- `implementation` — a `void *` C function pointer

### 2. Wrapper Synthesis (`wrapper_synthesis.c`)

`generateBuiltinWrappers()` runs once, before parsing. For each
registered builtin it creates an AST definition equivalent to:

```
fn rand(a$0) { builtin$rand(a$0) }
```

The call to `builtin$rand` has `isBuiltin = true` set on the
`AstFunCall` node. This is the *only* place the flag originates.

### 3. Injection into the Parse Tree (`pratt_parser.c`)

The preamble (`src/preamble.fn`) contains a `__builtins__` directive.
When the parser encounters it, `prependBuiltinWrappers()` splices the
generated wrapper definitions into the definition list at that point.
From the parser's perspective these are ordinary function definitions.

### 4. Lambda Conversion (`lambda_conversion.c`)

The `isBuiltin` flag is copied from `AstFunCall` to `LamApply`:

```c
getLamExp_Apply(result)->isBuiltin = funCall->isBuiltin;
```

### 5. Desugaring (`lambda_desugar.c`)

Copied from `LamApply` to `MinApply`:

```c
result->isBuiltin = node->isBuiltin;
```

### 6. MinExp Optimisation Passes

The `isBuiltin` flag is **propagated** through every MinExp
transformation pass (beta, eta, curry, uncurry, fold, subst, transform,
alpha-convert, amb). Every pass that reconstructs a `MinApply` copies
the flag.

The flag has two concrete effects during optimisation:

- **Currying** (`minlam_curry.c`): builtin applies are not
  restructured into a chain of single-argument applications. They
  remain as multi-argument calls.
- **Closure conversion** (`minlam_closureConvert.c`): builtin applies
  are **skipped entirely** — they are not decomposed into
  `(vec 0 closure)(vec 1 closure, args...)` form.

The `minlam_transform.c` generic visitor also short-circuits on
`isBuiltin`, returning the node unchanged.

The net effect is that after closure conversion, a builtin call like
`builtin$rand(x)` remains as a plain `MinApply` with
`isBuiltin = true`, while all user-defined function calls have been
decomposed into closure vector operations.

### 7. ANF normalisation (`anf_normalize.c`)

The `isBuiltin` flag does **not** appear in `anf.yaml` at all. ANF
normalisation converts `MinApply` to `CexpApply` without carrying the
flag forward. At this point it is no longer needed because:

- The wrapper function `rand` is an ordinary lambda that has been
  closure-converted.
- The body of that wrapper contains a call to `builtin$rand` which
  is just a variable reference — it resolves through the environment
  like any other variable.

### 8. ANF Annotation (`annotate.c`)

This is where builtins re-enter the picture. `annotateAnf()` creates
the root `AnfEnv` and calls `addBuiltInsToAnfEnv()`:

```c
void annotateAnf(AnfExp *x, BuiltIns *b) {
    AnfEnv *env = newAnfEnv(CPI(x), false, NULL);
    env->nsEnvs = newAnfEnvArray();
    addBuiltInsToAnfEnv(env, b);
    env = newAnfEnv(CPI(x), false, env);
    annotateExp(x, env);
}
```

This pre-populates the **outermost** environment frame with the
**internal** names (`builtin$rand`, etc.). These are the names that
appear in the wrapper function bodies. The wrappers themselves bind
the external names (`rand`) as ordinary let/letrec definitions, so
they get their own environment entries through normal annotation.

The result is a two-level environment:

- Frame 0 (innermost as annotation proceeds): normal program bindings
- Frame N (outermost): `builtin$rand`, `builtin$puts`, etc.

### 9. Bytecode (`bytecode.c`)

The bytecode compiler sees no `isBuiltin` flag. It simply compiles
`CexpApply` nodes into `BYTECODES_TYPE_APPLY` instructions. The
function being applied is a variable that resolves to an environment
location via the frame/offset annotation.

### 10. Runtime (`step.c`)

At startup, `inject()` calls `builtInsToEnv()` which creates the
runtime `Env` and pushes a `Value` for each builtin onto the stack
frame. Each value has type `VALUE_TYPE_BUILTIN` and contains a
`BuiltInImplementation` (name, C function pointer, arity).

When the `APPLY` instruction executes, it pops the callable value
and dispatches on its type:

- `VALUE_TYPE_CLO` / `VALUE_TYPE_PCLO` → user-defined function, push
  frame, jump to body bytecode
- `VALUE_TYPE_BUILTIN` → call the C function pointer directly:
  copies arguments from the stack into a `Vec`, calls the function,
  pushes the result
- `VALUE_TYPE_KONT` → continuation restore

Builtins currently do **not** support partial application at runtime
(the code has a `cant_happen("curried built-ins not supported yet")`).
This is fine because the wrapper lambda handles currying in user space.

## Summary of the Dual-Name Architecture

```
User writes:           rand(42)
                         │
Parser + wrappers:     rand(42)  ──where rand is──▶  fn rand(a$0) { builtin$rand(a$0) }
                         │
Lambda/desugar:        apply(rand, [42])  ──rand body──▶  apply(builtin$rand, [a$0], isBuiltin=true)
                         │
Curry/beta/eta:        isBuiltin prevents restructuring of the inner call
                         │
Closure convert:       rand gets closure-converted normally
                       builtin$rand call is SKIPPED (isBuiltin → no closure decomposition)
                         │
ANF annotation:        builtin$rand resolves to outermost env frame (pre-populated)
                       rand resolves normally through the environment chain
                         │
Bytecode APPLY:        pops callable, dispatches on VALUE_TYPE_BUILTIN vs CLO
```

## Implications for the New CPS/C-Target Pipeline

The new `minlam_annotate.c` crashed on `builtin$rand` because it
starts with an empty `IntMap` context — there is no equivalent of
`addBuiltInsToAnfEnv()`.

Options:

1. **Pre-populate the context** with internal builtin names, mirroring
   what `annotate.c` does. This requires passing `BuiltIns *` to
   `indexMinExp()`.

2. **Treat unresolved variables as globals** rather than calling
   `cant_happen()`. In the C target, builtin calls could be emitted
   as direct C function calls (since we have the function pointer in
   the `BuiltIn` struct) rather than going through the register file.

3. **Something different** — the CPS/closure-conversion pipeline
   already handles builtins specially via `isBuiltin`. It might be
   cleaner to recognise builtin applies and emit them as direct C
   calls during codegen (skipping annotation entirely for those
   nodes), since they do not participate in closure conversion and
   their implementations are known C functions.

## Decision

Option 3 was selected. free variable detection during closure conversion
ignores function names protected by `isBuiltin` applications, as does
annotation. There were a few problems to solve in that the CPS transform
was not propagating the `isBuiltin` flag.

It may be necessary or advisable to build a map from internal builtin name
to implementation for the code generator.
