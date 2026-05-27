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
- Section 6 progressed: shader uniform location lookups use an implicit cache
  tied to shader lifetime in `src/builtin_graphics.c`; a benchmark harness
  (`fn/gfx_shader_cache_benchmark.fn`) and explicit unload/reload smoke test
  (`tests/fn/test_gfx_shader_reload_smoke.fn`) are now in place.
- Section 7 progressed: graphics smoke coverage is owned by
  `tests/fn/test_gfx_*` files that gate execution through `gfx.enabled()` and
  pass silently when graphics is unavailable; duplicate `fn/gfx_*smoke.fn`
  scripts have been retired and grouped Make targets now provide repeatable
  graphics-only validation (`make test-gfx`, `make test-gfx-stress`).

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

### 1.1 Why is it major

Model shading is currently mostly model-level. Realistic rendering needs
per-material controls (especially for multi-material meshes and map-based
shading).

### 1.2 Minimum deliverable

- Query material count for a model.
- Bind shader per material index (not only all materials at once).
- Set common material maps per material index (albedo, normal, specular/rough).
- Keep behavior safe when material index is out of range (boolean false, no
  crash).

### 1.3 Primary implementation surfaces

- `src/builtin_graphics.c` builtin additions.
- `fn/gfxutils.fn` ergonomic wrappers.
- New focused demo showing two materials with different parameters.

### 1.4 Dependencies

- Existing model and shader ownership registries.
- Existing texture and model wrappers.

### 1.5 Validation hooks

- Targeted parse/run demo.
- Stress-gc run over load/bind/unload loop.

## 2. Lighting Abstraction Layer

### 2.1 Why is it major

Lighting is currently hardcoded in demos. Reusable lights are needed to scale
from one scene to multiple scenes and effects.

### 2.2 Minimum deliverable

- FN-level light record conventions (position/direction, color, intensity,
  type).
- Helper functions to upload one or more lights to shader uniform slots.
- Optional debug draw for light positions/directions.

### 2.3 Primary implementation surfaces

- `fn/gfxutils.fn` helper layer first.
- `src/builtin_graphics.c` only if additional uniform upload primitives are
  required.

### 2.4 Dependencies

- Stable shader uniform API.
- One lit scene to consume helpers.

### 2.5 Validation hooks

- Demo with at least two moving lights.
- Manual visual check for light contribution changes.

## 3. Texture/Sampler Binding API

### 3.1 Why is it major

Custom shaders commonly require explicit sampler bindings beyond default model
texture usage.

### 3.2 Minimum deliverable

- Bind texture to named sampler uniform and texture unit.
- Support at least one custom map in a shader demo (for example normal map or
  emissive map).
- Define clear lifetime/ownership semantics: texture remains owned by existing
  texture wrapper; sampler binding is transient state.

### 3.3 Primary implementation surfaces

- `src/builtin_graphics.c` new builtin for sampler binding.
- `fn/gfxutils.fn` wrapper with simple call shape.

### 3.4 Dependencies

- Shader and texture wrappers.

### 3.5 Validation hooks

- Parse-only and runtime sampler demo.
- Stress-gc pass while rebinding textures each frame.

## 4. Shadow Mapping Slice

### 4.1 Why is it major

Shadows are the largest quality jump after basic direct lighting.

### 4.2 Minimum deliverable

- Single directional-light shadow map pass.
- Render depth to shadow texture in a first pass.
- Sample shadow map in lit pass with basic bias to reduce acne.

### 4.3 Primary implementation surfaces

- Shader assets for depth pass and lit pass.
- Render texture workflow expansion in FN demo.
- Potential builtin additions only if depth-texture specifics are blocked.

### 4.4 Dependencies

- Render-to-texture path.
- Model shader assignment and matrix uniforms.

### 4.5 Validation hooks

- Visual checks: moving camera and light while shadow remains coherent.
- Performance sanity check with one model + ground plane.

## 5. Color Pipeline and Tonemapping

### 5.1 Why is it major

Without explicit color workflow, scenes can look flat or clipped even when
lighting math is correct.

### 5.2 Minimum deliverable

- Define and document working color assumptions (linear workflow expectations).
- Add one tonemapping/gamma correction post-process pass.
- Verify text/UI overlays still look correct with post-processing enabled.

### 5.3 Primary implementation surfaces

- Post-process shader assets.
- Existing render-texture composition demos.

### 5.4 Dependencies

- Shader + render texture pipeline.

### 5.5 Validation hooks

- Side-by-side scene comparison (before/after tonemap).
- Histogram-like debug readout is optional, not required for first pass.

## 6. Performance and State Management Pass

### 6.1 Why is it major

Current APIs prioritize correctness and simplicity. Before broadening features,
state churn and per-frame overhead should be measured.

### 6.2 Minimum deliverable

- Measure impact of repeated `GetShaderLocation` calls in hot paths.
- Decide keep-as-is vs caching uniform locations.
- If caching is adopted, keep cache ownership tied to shader lifetime.

### 6.3 Primary implementation surfaces

- `src/builtin_graphics.c` shader uniform helper path.
- Optional new opaque helper for cached uniform locations.

### 6.4 Dependencies

- Stable lit demo(s) with per-frame uniform updates.

### 6.5 Validation hooks

- Benchmark loop comparing uncached and cached paths.
- Confirm no stale-location crashes after shader unload/reload.
- Runtime benchmark harness: `./bin/fn --include=fn fn/gfx_shader_cache_benchmark.fn`.
- Reload regression smoke: `./bin/fn --include=fn tests/fn/test_gfx_shader_reload_smoke.fn`.

## 7. Graphics Regression Coverage

### 7.1 Why is it major

Most confidence currently comes from demos and broad test runs. Dedicated
graphics regressions reduce breakage risk as API surface expands.

### 7.2 Minimum deliverable

- Add a small set of non-interactive gated smoke scripts under `tests/fn/` for:
  - shader lifecycle,
  - model shader binding,
  - render-texture + shader composition,
  - stress-gc resource churn.
- Wire those smoke scripts into a repeatable validation command sequence.

### 7.3 Primary implementation surfaces

- `tests/fn/test_gfx_*smoke.fn` scripts.
- `Makefile` grouped graphics checks (`test-gfx`, `test-gfx-stress`).

### 7.4 Dependencies

- Existing demos and wrappers.

### 7.5 Validation hooks

- Local run: `make test-gfx` + standard `make test`.
- Targeted diagnostics only: `make test-gfx-stress` (can be very slow; avoid routine use).

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

## Macro/Syntax helpers

There are a lot of repeated patterns like

```fn
dz = if (gfx.key_held(gfx.key_w())) { 0 - speed }
      else if (gfx.key_held(gfx.key_s())) { speed }
      else { 0 };
```

that could become:

```fn
dz = gfx.key_held_action[w => { 0 - speed };
                         s => { speed };
                         default => { 0 }];
```

likewise for

```fn
_cursor_vis = if (gfx.key_pressed(gfx.key_h())) { gfx.hide_cursor() }
              else if (gfx.key_pressed(gfx.key_j())) { gfx.show_cursor() }
              else { true };
```

could become:

```fn
_cursor_vis = gfx.key_pressed_action[h => { gfx.hide_cursor() };
                                     j => { gfx.show_cursor() };
                                     default => { true }];
```
