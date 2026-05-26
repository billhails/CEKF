# Raylib Integration: Remaining Major Pieces

This document captures the remaining graphics integration work after the first
functional shader/model pipeline milestone.

It is intentionally scoped as a planning scaffold: each section is detailed
enough to be expanded into a full implementation plan when work starts.

## Status Update (2026-05-26)

- Sections 1-4 are now functionally delivered in code and demos.
- Section 5 started: postprocess shader now includes Reinhard tonemap and gamma
  correction controls (`uExposure`, `uGamma`) and the postprocess demo/smoke
  path uploads those uniforms.
- Section 6 started: shader uniform location lookups now use an implicit cache
  tied to shader lifetime in `src/builtin_graphics.c`.
- Section 7 started: graphics smoke coverage is now integrated into normal
  `test-a` discovery via new `tests/fn/test_gfx_*` files that gate execution
  through `gfx.enabled()` and pass silently when graphics is unavailable.

## Current Baseline

Delivered and working now:

- Window/frame/camera lifecycle wrappers.
- 2D and 3D primitive drawing.
- Texture, render texture, font, audio, and model load/draw paths.
- Shader lifecycle (`load`, `unload`, `begin/end`) and uniform setters
  (`int`, `float`, `vec2`, `vec3`, `vec4`, `mat4`).
- Model shader binding (`set_model_shader`) and a lit teapot demo.
- Shader + render-to-texture composition demos.

## 1. Material Control Surface

### Why it is major

Model shading is currently mostly model-level. Realistic rendering needs
per-material controls (especially for multi-material meshes and map-based
shading).

### Minimum deliverable

- Query material count for a model.
- Bind shader per material index (not only all materials at once).
- Set common material maps per material index (albedo, normal, specular/rough).
- Keep behavior safe when material index is out of range (boolean false, no
  crash).

### Primary implementation surfaces

- `src/builtin_graphics.c` builtin additions.
- `fn/graphics.fn` ergonomic wrappers.
- New focused demo showing two materials with different parameters.

### Dependencies

- Existing model and shader ownership registries.
- Existing texture and model wrappers.

### Validation hooks

- Targeted parse/run demo.
- Stress-gc run over load/bind/unload loop.

## 2. Lighting Abstraction Layer

### Why it is major

Lighting is currently hardcoded in demos. Reusable lights are needed to scale
from one scene to multiple scenes and effects.

### Minimum deliverable

- FN-level light record conventions (position/direction, color, intensity,
  type).
- Helper functions to upload one or more lights to shader uniform slots.
- Optional debug draw for light positions/directions.

### Primary implementation surfaces

- `fn/graphics.fn` helper layer first.
- `src/builtin_graphics.c` only if additional uniform upload primitives are
  required.

### Dependencies

- Stable shader uniform API.
- One lit scene to consume helpers.

### Validation hooks

- Demo with at least two moving lights.
- Manual visual check for light contribution changes.

## 3. Texture/Sampler Binding API

### Why it is major

Custom shaders commonly require explicit sampler bindings beyond default model
texture usage.

### Minimum deliverable

- Bind texture to named sampler uniform and texture unit.
- Support at least one custom map in a shader demo (for example normal map or
  emissive map).
- Define clear lifetime/ownership semantics: texture remains owned by existing
  texture wrapper; sampler binding is transient state.

### Primary implementation surfaces

- `src/builtin_graphics.c` new builtin for sampler binding.
- `fn/graphics.fn` wrapper with simple call shape.

### Dependencies

- Shader and texture wrappers.

### Validation hooks

- Parse-only and runtime sampler demo.
- Stress-gc pass while rebinding textures each frame.

## 4. Shadow Mapping Slice

### Why it is major

Shadows are the largest quality jump after basic direct lighting.

### Minimum deliverable

- Single directional-light shadow map pass.
- Render depth to shadow texture in a first pass.
- Sample shadow map in lit pass with basic bias to reduce acne.

### Primary implementation surfaces

- Shader assets for depth pass and lit pass.
- Render texture workflow expansion in FN demo.
- Potential builtin additions only if depth-texture specifics are blocked.

### Dependencies

- Render-to-texture path.
- Model shader assignment and matrix uniforms.

### Validation hooks

- Visual checks: moving camera and light while shadow remains coherent.
- Performance sanity check with one model + ground plane.

## 5. Color Pipeline and Tonemapping

### Why it is major

Without explicit color workflow, scenes can look flat or clipped even when
lighting math is correct.

### Minimum deliverable

- Define and document working color assumptions (linear workflow expectations).
- Add one tonemapping/gamma correction post-process pass.
- Verify text/UI overlays still look correct with post-processing enabled.

### Primary implementation surfaces

- Post-process shader assets.
- Existing render-texture composition demos.

### Dependencies

- Shader + render texture pipeline.

### Validation hooks

- Side-by-side scene comparison (before/after tonemap).
- Histogram-like debug readout is optional, not required for first pass.

## 6. Performance and State Management Pass

### Why it is major

Current APIs prioritize correctness and simplicity. Before broadening features,
state churn and per-frame overhead should be measured.

### Minimum deliverable

- Measure impact of repeated `GetShaderLocation` calls in hot paths.
- Decide keep-as-is vs caching uniform locations.
- If caching is adopted, keep cache ownership tied to shader lifetime.

### Primary implementation surfaces

- `src/builtin_graphics.c` shader uniform helper path.
- Optional new opaque helper for cached uniform locations.

### Dependencies

- Stable lit demo(s) with per-frame uniform updates.

### Validation hooks

- Benchmark loop comparing uncached and cached paths.
- Confirm no stale-location crashes after shader unload/reload.

## 7. Graphics Regression Coverage

### Why it is major

Most confidence currently comes from demos and broad test runs. Dedicated
graphics regressions reduce breakage risk as API surface expands.

### Minimum deliverable

- Add a small set of non-interactive smoke scripts under `fn/` for:
  - shader lifecycle,
  - model shader binding,
  - render-texture + shader composition,
  - stress-gc resource churn.
- Wire those smoke scripts into a repeatable validation command sequence.

### Primary implementation surfaces

- `fn/gfx_*_smoke.fn` scripts.
- Optional `Makefile` target for grouped graphics checks.

### Dependencies

- Existing demos and wrappers.

### Validation hooks

- Local run: parse-only + stress-gc + standard test suite.

## Suggested Execution Order

1. Material control surface.
2. Lighting abstraction layer.
3. Sampler binding API.
4. Shadow mapping slice.
5. Color pipeline and tonemapping.
6. Performance/state management pass.
7. Graphics regression coverage hardening.

## Explicitly Deferred for Later

- Full PBR material system.
- Skeletal animation pipeline.
- Advanced post-processing chains (multi-pass bloom/SSR).
- Scene graph/instancing framework.
