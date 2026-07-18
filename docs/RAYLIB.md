# Raylib Integration

F♮ includes an optional raylib binding that exposes 2D and 3D graphics, audio,
textures, shaders, fonts, and models through a functional API. All user-facing
functions live in `fn/gfxutils.fn`, which wraps the lower-level `gfx_*` built-ins.

## Building with Raylib

Raylib support is disabled by default.

```bash
# Install the system package (Debian/Ubuntu)
make install-raylib

# Build with raylib enabled
make ENABLE_RAYLIB=1

# Convenience target that cleans and rebuilds with raylib
make rebuild-raylib
```

When compiled without raylib, calling `gfx.enabled()` returns `false` and all
other graphics functions are no-ops. Programs can check `gfx.enabled()` to
provide a non-graphical fallback.

## Running the Demos

All demo programs are in `fn/`.

```bash
./bin/fn fn/gfx_demo.fn
./bin/fn fn/gfx_2d_primitives_demo.fn
./bin/fn fn/gfx_3d_demo.fn
./bin/fn fn/gfx_3d_mouse_look_demo.fn
./bin/fn fn/gfx_camera2d_demo.fn
./bin/fn fn/gfx_transform_demo.fn
./bin/fn fn/gfx_texture_demo.fn
./bin/fn fn/gfx_font_demo.fn
./bin/fn fn/gfx_render_texture_demo.fn
./bin/fn fn/gfx_shader_demo.fn
./bin/fn fn/gfx_shader_postprocess_demo.fn
./bin/fn fn/gfx_sampler_demo.fn
./bin/fn fn/gfx_model_demo.fn
./bin/fn fn/gfx_teapot_lit_demo.fn
./bin/fn fn/gfx_teapot_two_lights_demo.fn
./bin/fn fn/gfx_material_teaspoon_demo.fn
./bin/fn fn/gfx_shadow_demo.fn
./bin/fn fn/gfx_audio_demo.fn
./bin/fn fn/gfx_music_demo.fn
```

---

## Importing gfxutils

Link `gfxutils.fn` as a namespace alias:

```fn
link "gfxutils.fn" as gfx;
```

You can then call `gfx.run(...)`, `gfx.frame(...)`, etc. To bring specific
symbols into scope without the `gfx.` prefix, use `import`:

```fn
import gfx.rgb;
import gfx.white;
import gfx macro key_held_action;
```

---

## Core Types

`gfxutils.fn` defines several types that are used throughout the API:

| Type | Constructor | Fields |
| ---- | ----------- | ------ |
| `rgba` | `rgba(r, g, b, a)` | 0–255 integers |
| `point` | `point(x, y)` | screen or world coordinates |
| `vec3` | `vec3(x, y, z)` | 3D coordinates or directions |
| `vec4` | `vec4(x, y, z, w)` | 4-component vectors |
| `extent` | `extent(w, h)` | width and height |
| `rect` | `rect(x, y, w, h)` | origin and size |
| `Light` | `point_light(pos, color, intensity)` or `directional_light(dir, color, intensity)` | lighting model |
| `lighting_config` | `lighting_config(ambient, spec_strength, shininess)` | shader lighting parameters |

`rgb(r, g, b)` is a convenience wrapper that returns `rgba(r, g, b, 255)`.
Over 140 named colour constants are provided (`red()`, `blue()`, `cornflower_blue()`, etc.).

---

## Window and Frame Loop

### `run` — high-level game loop

```fn
gfx.run(width, height, title, fps, background, draw)
```

Opens a window, sets the frame rate, and loops calling `draw()` each frame until
the window is closed or `should_close()` returns true. `background` is the clear
colour. Returns `nothing` on window failure, otherwise whatever the loop
eventually returns.

### `with_window` and `with_window_size` — manual loop

```fn
gfx.with_window(width, height, title, handler)
gfx.with_window_size(extent(w, h), title, handler)
```

Opens a window, calls `handler()`, then closes. Use these when you need full
control of the loop (e.g. stateful recursion).

### `frame` — single frame rendering

```fn
gfx.frame(background, draw_fn)
```

Begins a frame, clears to `background`, calls `draw_fn()`, and ends the frame.
Returns the value of `draw_fn()`. All drawing calls must happen inside a frame.

### `loop` — simple frame-recursive loop

```fn
gfx.loop(background, draw)
```

Calls `frame(background, draw)` repeatedly until `should_close()` returns true.

### Other window functions

| Function | Description |
| -------- | ----------- |
| `should_close()` | True when the OS signals the window should close |
| `set_target_fps(fps)` | Set the target frame rate |
| `screen_width()` | Current window width in pixels |
| `screen_height()` | Current window height in pixels |
| `frame_time_ms()` | Time in milliseconds since the last frame |
| `take_screenshot(path)` | Save the current frame to a file |
| `enabled()` | True when the binary was compiled with raylib |

---

## Input

### Keyboard

| Function | Description |
| -------- | ----------- |
| `key_held(k)` | True while key `k` is held down |
| `key_pressed(k)` | True on the first frame the key is pressed |
| `key_released(k)` | True on the frame the key is released |

Key constants: `key_space()`, `key_escape()`, `key_enter()`, `key_tab()`,
`key_backspace()`, `arrow_right()`, `arrow_left()`, `arrow_down()`, `arrow_up()`,
`key_left_shift()`, `key_left_ctrl()`, `key_left_alt()`, and `key_a()` through
`key_z()`, `key_zero()` through `key_nine()`.

#### `key_held_action` and `key_pressed_action` macros

These macros provide a concise pattern-matching syntax for key dispatch:

```fn
import gfx macro key_held_action;
import gfx macro key_pressed_action;

let dx = key_held_action[
    left  => { x - speed };
    right => { x + speed };
    default => { x }
];
```

Available key names in the macro: `space`, `escape`, `enter`, `tab`,
`backspace`, `right`, `left`, `down`, `up`, `shift`, `ctrl`, `alt`,
and `a` through `z`.

### Mouse

| Function | Description |
| -------- | ----------- |
| `mouse_x()` | Cursor X position in screen coordinates |
| `mouse_y()` | Cursor Y position in screen coordinates |
| `mouse_delta_x()` | Cursor X movement since the last frame |
| `mouse_delta_y()` | Cursor Y movement since the last frame |
| `mouse_wheel()` | Wheel scroll delta this frame |
| `mouse_held(btn)` | True while mouse button is held |
| `mouse_pressed(btn)` | True on the first frame the button is pressed |
| `mouse_released(btn)` | True on the frame the button is released |
| `disable_cursor()` | Lock and hide the cursor (for mouse-look) |
| `enable_cursor()` | Restore the cursor |
| `hide_cursor()` | Hide cursor without locking |
| `show_cursor()` | Show the cursor |

Mouse button constants: `btn_left()` (0), `btn_right()` (1), `btn_middle()` (2).

---

## 2D Drawing

All 2D drawing functions must be called inside `gfx.frame(...)` or between
`gfx_begin_frame()` / `gfx_end_frame()`.

### Primitives

| Function | Description |
| -------- | ----------- |
| `clear(color)` | Fill the frame with `color` |
| `fill_rect(x, y, w, h, color)` | Filled rectangle |
| `fill_rect_from(rect(x,y,w,h), color)` | Filled rectangle from a `rect` |
| `draw_rect(x, y, w, h, color)` | Outlined rectangle |
| `draw_rect_at(point, extent, color)` | Outlined rectangle from point and extent |
| `draw_rect_from(rect(x,y,w,h), color)` | Outlined rectangle from a `rect` |
| `draw_rect_rounded(rect, roundness, segments, thickness, color)` | Rounded-corner rectangle |
| `fill_circle(x, y, radius, color)` | Filled circle |
| `fill_circle_at(point, radius, color)` | Filled circle at a `point` |
| `draw_circle(x, y, radius, color)` | Outlined circle |
| `draw_circle_at(point, radius, color)` | Outlined circle at a `point` |
| `draw_line(x1, y1, x2, y2, color)` | Line segment |
| `draw_line_between(point, point, color)` | Line between two `point` values |
| `fill_triangle(p1, p2, p3, color)` | Filled triangle |
| `draw_triangle(p1, p2, p3, color)` | Outlined triangle |
| `fill_polygon(center, sides, radius, rotation, color)` | Filled regular polygon |
| `draw_polygon(center, sides, radius, rotation, color)` | Outlined regular polygon |
| `draw_arc(center, radius, start_angle, end_angle, segments, color)` | Arc |

### Text

| Function | Description |
| -------- | ----------- |
| `draw_text(text, x, y, size, color)` | Draw text using the built-in font |
| `draw_text_at(text, point, size, color)` | Draw text at a `point` |
| `draw_text_font(font, text, x, y, size, spacing, color)` | Draw text with a custom font |
| `draw_text_font_at(font, text, point, size, spacing, color)` | Custom font at a `point` |
| `measure_text_width(font, text, size, spacing)` | Returns `success(width)` or `failure(msg)` |

### 2D Camera

```fn
gfx.with_camera2d(target, offset, rotation, zoom, draw_fn)
```

Renders `draw_fn()` through a 2D camera. `target` is the world `point` the camera
looks at; `offset` is the screen `point` that maps to `target`; `rotation` is in
degrees; `zoom` scales the world.

### 2D Transform Matrices

A `mat2d` opaque type represents a 2×3 affine transform matrix.

| Function | Description |
| -------- | ----------- |
| `mat2d_identity()` | Returns the identity matrix |
| `mat2d_translate(mat, dx, dy)` | Returns a new matrix with translation applied |
| `mat2d_rotate(mat, degrees)` | Returns a new matrix with rotation applied |
| `mat2d_scale(mat, sx, sy)` | Returns a new matrix with scale applied |
| `mat2d_compose(left, right)` | Multiplies two matrices |
| `mat2d_apply_x(mat, x, y)` | Transforms X component of a point |
| `mat2d_apply_y(mat, x, y)` | Transforms Y component of a point |
| `mat2d_apply_point(mat, point)` | Transforms a `point` through the matrix |

---

## Fonts

Fonts are loaded as opaque handles. The preferred usage is `with_font` which
automatically unloads the font when the handler returns.

```fn
gfx.with_font("/path/to/font.ttf", base_size, fn (font) {
    gfx.draw_text_font_at(font, "Hello", gfx.point(40, 40), 32, 1, gfx.white())
})
```

| Function | Description |
| -------- | ----------- |
| `load_font(path, base_size)` | Returns `success(font)` or `failure(msg)` |
| `unload_font(font)` | Frees the font |
| `with_font(path, base_size, handler)` | Load, use, and auto-unload |

---

## Textures

Textures are loaded from image files (PNG, JPG, etc.) as opaque handles.

```fn
gfx.with_texture("assets/image.png", fn (tex) {
    gfx.draw_texture_at(tex, gfx.point(0, 0), gfx.white())
})
```

| Function | Description |
| -------- | ----------- |
| `load_texture(path)` | Returns `success(texture)` or `failure(msg)` |
| `unload_texture(tex)` | Frees the texture |
| `with_texture(path, handler)` | Load, use, and auto-unload |
| `texture_width(tex)` | Width in pixels |
| `texture_height(tex)` | Height in pixels |
| `draw_texture(tex, x, y, color)` | Draw at (x, y) with a tint |
| `draw_texture_at(tex, point, color)` | Draw at a `point` with a tint |
| `draw_sprite(tex, src_rect, dest_point, color)` | Draw a sub-region |
| `draw_sprite_pro(tex, src_rect, dest_rect, origin, rotation, color)` | Scaled, rotated sub-region |
| `draw_sprite_pro_at(tex, src_rect, dest_point, dest_extent, origin, rotation, color)` | As above but separate position and size |

`src_rect` uses a `rect(x, y, w, h)`. A negative `w` or `h` flips the sprite.

---

## Render Textures

Render textures (off-screen framebuffers) allow multi-pass rendering.

```fn
gfx.with_render_texture_size(gfx.extent(400, 300), fn (rt) {
    gfx.render_to_texture(rt, gfx.black(), fn () {
        // draw here
    });
    gfx.frame(gfx.black(), fn () {
        gfx.draw_render_texture_at(rt, gfx.point(0, 0), gfx.white())
    })
})
```

| Function | Description |
| -------- | ----------- |
| `load_render_texture(w, h)` | Returns `success(rt)` or `failure(msg)` |
| `unload_render_texture(rt)` | Frees the render texture |
| `with_render_texture(w, h, handler)` | Load, use, and auto-unload |
| `with_render_texture_size(extent, handler)` | Same but takes an `extent` |
| `render_to_texture(rt, background, draw_fn)` | Draw into `rt` instead of the screen |
| `draw_render_texture(rt, x, y, color)` | Draw the render texture to the screen |
| `draw_render_texture_at(rt, point, color)` | Same but takes a `point` |

---

## Shaders

Vertex and fragment shaders are GLSL files loaded from disk.

```fn
gfx.with_shader("assets/shaders/basic.vs", "assets/shaders/tint.fs", fn (shader) {
    gfx.frame(gfx.black(), fn () {
        gfx.with_shader_mode(shader, fn () {
            // drawing here is shaded
        })
    })
})
```

| Function | Description |
| -------- | ----------- |
| `load_shader(vs_path, fs_path)` | Returns `success(shader)` or `failure(msg)` |
| `unload_shader(shader)` | Frees the shader |
| `with_shader(vs_path, fs_path, handler)` | Load, use, and auto-unload |
| `with_shader_mode(shader, draw_fn)` | Apply shader to all drawing in `draw_fn` |

### Setting Shader Uniforms

All `set_shader_*` functions return `true` on success or `false` if the uniform
name is not found in the shader. Use `assert(...)` to treat a missing uniform as
an error.

| Function | Uniform type |
| -------- | ------------ |
| `set_shader_int(shader, name, value)` | `int` |
| `set_shader_float(shader, name, value)` | `float` |
| `set_shader_vec2(shader, name, point)` | `vec2` |
| `set_shader_vec3(shader, name, vec3)` | `vec3` |
| `set_shader_vec4(shader, name, vec4)` | `vec4` |
| `set_shader_mat4(shader, name, m00..m33)` | `mat4` (row-major, 16 values) |
| `set_shader_sampler(shader, name, texture, unit)` | texture sampler (`int unit` 0–3) |
| `set_shader_render_texture_sampler(shader, name, rt, unit, attachment)` | render texture sampler |

For shadow mapping, three helpers compute a light-space view-projection matrix
and upload it directly:

- `set_shader_light_vp_ortho(shader, name, light_pos, target, up, left, right, bottom, top, near, far)` — combined VP matrix
- `set_shader_light_vp_ortho_vp(...)` — view matrix only
- `set_shader_light_vp_ortho_pv(...)` — projection matrix only

### Sampler Units

| Constant | Value |
| -------- | ----- |
| `sampler_unit0()` | 0 |
| `sampler_unit1()` | 1 |
| `sampler_unit2()` | 2 |
| `sampler_unit3()` | 3 |
| `sampler_attachment_color()` | 0 (color attachment of a render texture) |
| `sampler_attachment_depth()` | 1 (depth attachment of a render texture) |

The bundled shaders in `assets/shaders/` include:

| File | Purpose |
| ---- | ------- |
| `basic.vs` | Pass-through vertex shader |
| `tint.fs` | Color tint with `uAmount`, `uTint` (vec3) uniforms |
| `sampler_blend.fs` | Blends `uTex0` and `uTex1` via `uBlend` |
| `postprocess.fs / postprocess.vs` | Post-processing with exposure, gamma, tint, and a matrix |
| `lit_model.vs / lit_model.fs` | Single-light Blinn-Phong model shader |
| `lit_model_2l.vs / lit_model_2l.fs` | Two-light Blinn-Phong model shader |
| `shadow_depth.vs / shadow_depth.fs` | Depth-only pass for shadow map generation |
| `lit_shadow.vs / lit_shadow.fs` | Lit pass that reads the shadow map |

---

## 3D Drawing

3D drawing must happen inside `with_camera3d(...)`.

### Camera

```fn
gfx.with_camera3d(position, target, up, fovy, projection, draw_fn)
```

- `position`, `target`, `up`: `vec3` values.
- `fovy`: vertical field of view in degrees.
- `projection`: `projection_perspective()` (0) or `projection_orthographic()` (1).

### 3D Primitives

| Function | Description |
| -------- | ----------- |
| `draw_cube(center, size, color)` | Filled cube (`vec3` center, `vec3` size) |
| `draw_cube_wires(center, size, color)` | Wireframe cube |
| `draw_sphere(center, radius, color)` | Filled sphere |
| `draw_sphere_wires(center, radius, rings, slices, color)` | Wireframe sphere |
| `draw_cylinder(pos, r_top, r_bottom, height, slices, color)` | Filled cylinder |
| `draw_cylinder_wires(pos, r_top, r_bottom, height, slices, color)` | Wireframe cylinder |
| `draw_plane(center, size, color)` | Horizontal plane (`vec3` size; Y component ignored) |
| `draw_grid(slices, spacing)` | XZ ground grid |
| `draw_line_3d(start, end, color)` | 3D line segment |

### 3D Transform Helpers

`transform3d` is an opaque handle holding position, rotation (degrees), and
scale `vec3` values.

```fn
let t = gfx.make_transform3d(position, rotation, scale);
gfx.draw_cube(gfx.transform3d_position(t), gfx.transform3d_scale(t), gfx.white())
```

| Function | Description |
| -------- | ----------- |
| `make_transform3d(pos, rot, scale)` | Creates a transform |
| `transform3d_position(t)` | Extracts position as `vec3` |
| `transform3d_rotation(t)` | Extracts rotation as `vec3` |
| `transform3d_scale(t)` | Extracts scale as `vec3` |

---

## Models

OBJ (and other raylib-supported) model files can be loaded and drawn.

```fn
gfx.with_model("assets/TeaPot.obj", fn (model) {
    gfx.with_shader("assets/shaders/lit_model.vs", "assets/shaders/lit_model.fs", fn (shader) {
        gfx.set_model_shader(model, shader);
        // ... draw loop ...
        gfx.draw_model_at(model, gfx.vec3(0, 0, 0), 1, gfx.white())
    })
})
```

| Function | Description |
| -------- | ----------- |
| `load_model(path)` | Returns `success(model)` or `failure(msg)` |
| `unload_model(model)` | Frees the model |
| `with_model(path, handler)` | Load, use, and auto-unload |
| `set_model_shader(model, shader)` | Assign shader to all materials |
| `material_count(model)` | Number of materials in the model |
| `set_material_shader(model, index, shader)` | Assign shader to a specific material |
| `set_material_map(model, index, map_kind, texture)` | Assign a texture to a material map slot |
| `draw_model_at(model, pos, scale, color)` | Draw at `vec3` position with uniform scale |
| `draw_model_wires_at(model, pos, scale, color)` | Wireframe overlay |

Material map constants: `material_map_albedo()` (0), `material_map_normal()` (1),
`material_map_specular()` / `material_map_metalness()` (2), `material_map_roughness()` (3).

---

## Lighting Helpers

`gfxutils.fn` provides high-level helpers for the bundled lit shaders.

```fn
let light = gfx.point_light(
    gfx.vec3(4, 8, 4),          // world position
    gfx.vec3(1, 0.95, 0.88),    // colour (normalized 0–1 per channel)
    1.0                          // intensity
);
let lighting = gfx.default_lighting();   // reasonable defaults
gfx.upload_lighting(shader, camera_pos, light, lighting)
```

| Function | Description |
| -------- | ----------- |
| `default_lighting()` | Returns a `lighting_config` with reasonable defaults |
| `upload_lighting(shader, view_pos, light, config)` | Uploads single-light uniforms |
| `upload_lighting_two(shader, view_pos, light_a, light_b, config)` | Uploads two-light uniforms |
| `draw_light_debug(light, radius, color)` | Draws a sphere at the light position |

For directional lights, `light_position` converts the direction vector to a
distant world position automatically.

---

## Audio

Audio requires a separate device open/close around all sound operations.

```fn
gfx.with_audio(fn () {
    gfx.with_sound("sounds/click.wav", fn (sound) {
        gfx.play_sound(sound)
    })
})
```

### Sound (one-shot)

| Function | Description |
| -------- | ----------- |
| `with_audio(handler)` | Open audio device, run handler, close |
| `load_sound(path)` | Returns `success(sound)` or `failure(msg)` |
| `unload_sound(sound)` | Frees the sound |
| `with_sound(path, handler)` | Load, use, and auto-unload |
| `play_sound(sound)` | Play from the beginning |
| `stop_sound(sound)` | Stop playback |
| `is_sound_playing(sound)` | True if currently playing |
| `set_sound_volume(sound, percent)` | Volume 0–100 |

### Music (streaming)

Music is streamed from disk and must be updated every frame.

```fn
gfx.with_audio(fn () {
    gfx.with_music("music/track.mp3", fn (music) {
        gfx.play_music(music);
        gfx.loop(gfx.black(), fn () {
            gfx.update_music_stream(music);
            // draw ...
        })
    })
})
```

| Function | Description |
| -------- | ----------- |
| `load_music(path)` | Returns `success(music)` or `failure(msg)` |
| `unload_music(music)` | Frees the music stream |
| `with_music(path, handler)` | Load, use, and auto-unload |
| `play_music(music)` | Start or restart playback |
| `pause_music(music)` | Pause |
| `resume_music(music)` | Resume after pause |
| `stop_music(music)` | Stop and rewind |
| `is_music_playing(music)` | True if currently playing |
| `update_music_stream(music)` | Must be called once per frame |
| `seek_music_ms(music, position_ms)` | Seek to a position |
| `music_time_played_ms(music)` | Current playback position in ms |
| `music_time_length_ms(music)` | Total duration in ms |
| `music_time_progress_width(music, max_width)` | Convenience: pixels of a progress bar |
| `set_music_volume(music, percent)` | Volume 0–100 |

---

## Minimal Example

```fn
let
    link "gfxutils.fn" as gfx;
    import gfx macro key_pressed_action;
in
    gfx.run(800, 600, "Hello", 60, gfx.black(), fn () {
        gfx.draw_text_at("Hello, world!", gfx.point(300, 280), 24, gfx.white());
        key_pressed_action[
            escape => { gfx.should_close() };
            default => { false }
        ]
    })
```

---

## Resource Lifecycle

All opaque handles (textures, fonts, shaders, models, sounds, music, render
textures) are managed outside the GC. The `with_*` helpers are the preferred
pattern: they load a resource, pass it to a handler, and unload it when the
handler returns, even if it returns `nothing`. Manual `load_*` / `unload_*` pairs
are available for cases that need more control.

There is no automatic reference counting; passing a handle to `unload_*` after
it has already been unloaded is undefined behaviour.
