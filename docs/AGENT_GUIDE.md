# IronSmith for AI Agents

IronSmith models are plain-text `.irsm` scripts, so an AI agent (Claude Code,
Codex, or any tool that can run shell commands) can build a 3D model from a
description, look at the result, and refine it. This guide is written for
that agent.

## Setup

You need the `ironsmith` binary with `ironsmith-viewer` in the same folder
(the release package layout) or on `PATH`. Rendering opens the viewer
window, so it needs a desktop session with a GPU (Vulkan, Metal or DX12).
`check` and `export` are pure CPU and work anywhere.

## The loop

1. **Write** the model to a `.irsm` file.
2. **Check** it: `ironsmith check model.irsm`
3. **Render** it: `ironsmith render model.irsm -o model.png`, then look at
   the PNG. Render from more than one side (`--view`) before trusting it.
4. **Adjust** the script and repeat from step 2.
5. **Export** a mesh if one is wanted: `ironsmith export model.irsm -o model.obj`
   (geometry only), or `-o model.glb` to keep `paint()` colours and
   `material()`s, which is the right choice for web (three.js) or Blender.

## Commands

| Command | Does | Prints on success |
|---|---|---|
| `ironsmith check FILE` | Parse and validate | `OK: FILE (N statements, M bytecode instructions)` |
| `ironsmith render FILE [-o OUT.png] [--view YAW,PITCH,DIST[,X,Y,Z]]` | Show the model in the viewer and save a still | `OK: wrote /abs/OUT.png` |
| `ironsmith export FILE [-o OUT.obj\|OUT.glb] [--format obj\|glb] [--res N]` | Write a mesh: OBJ (geometry) or GLB (glTF with colours + materials). `--res` is the number of cells across the model's longest side (default 128) | `OK: wrote /abs/OUT.ext (resolution N)` |
| `ironsmith help` | Usage | |

- Outputs default to the input path with a `.png` / `.obj` / `.glb` extension.
  The export format comes from `--format`, else the `-o` extension, else OBJ.
- Errors go to stderr as `error: FILE:LINE: message` (parse errors also
  show the line with a caret). Exit codes: `0` ok, `1` script error,
  `2` bad usage, `3` viewer or I/O failure.
- `render` starts the viewer if none is running and **leaves it open**,
  so later renders take well under a second. If the user has the
  IronSmith editor open, `render` shows your model in their viewer window.
- Without `--view`, `render` frames the whole model automatically from the
  front-right. The image is the size of the viewer window.
- A scene with hundreds of shapes can take the GPU driver several seconds
  to compile the first time; `render` waits for it.

### Camera (`--view`)

`YAW,PITCH,DIST` in degrees and world units, optionally followed by the
`X,Y,Z` point to look at (default: the centre of the model).

- `yaw 0` looks from +Z towards the origin, `90` from +X, `-90` from -X,
  `180` from -Z.
- `pitch` is height above the horizon: `0` level, `89` almost straight
  down (avoid exactly `90`: the camera's up direction becomes undefined).
- Useful set for a model whose front faces +X: front-right `35,25,D`,
  rear-left `-145,25,D`, side `0,10,D`, top `0,89,D`.

## Language reference

```
// comments start with //
size = 2                         // number variable; + - * / and unary minus
part = sphere(size)              // shape variable
part = union(part, cube(1,1,1))  // reassigning may use the previous value

// Primitives (centred on the origin)
cube(width, height, depth)       // FULL sizes, not half sizes
sphere(radius)
cylinder(radius, height)         // axis along Y
cone(radius, height)             // axis along Y, point at +Y
cone(radius, top_radius, height)
torus(radius, tube_radius)       // ring lies flat in the XZ plane

// Transforms (the shape is the LAST argument)
move(x, y, z, shape)
rotateX(degrees, shape)          // also rotateY, rotateZ
scale(factor, shape)
scale(sx, sy, sz, shape)
repeat(spacing_x, spacing_y, spacing_z, shape)  // infinite tiling; 0 = don't repeat on that axis

// Combining
union(a, b)
intersection(a, b)
difference(a, b)                 // a with b cut out of it
group(a, b, c, ...)              // union of any number of shapes

// Appearance (wrap any number of shapes)
paint(#RRGGBB, shape, ...)       // or white, black, red, green, blue
material(metal, shape, ...)      // matte, plastic, neon, metal
light(x, y, z)                   // optional; light(x, y, z, intensity)
```

A line that is just a shape (`cube(1,1,1)`, `move(...)`, or a shape
variable name) draws it. Assignments draw nothing on their own.

There are **no loops, functions or conditionals**: write repeated parts
out, using variables for shared dimensions, or use `repeat(...)` for
regular tiling.

## Filters

A filter is a post-processing pass over the finished image. It is a `.glsl`
file in the viewer's filters folder, and it affects renders as well as the
live window:

- Windows: `%APPDATA%\ironsmith\filters`
- Linux / macOS: `~/.config/ironsmith/filters`

The viewer installs `passthrough`, `edge_detection`, `heatmap` and `ascii`
there. These are the user's files: add new ones, but don't overwrite existing
ones without asking.

### Writing one

A filter defines one function, and the file name (letters, digits, `_`, `-`)
is its name:

```glsl
vec4 apply(vec2 uv) {
    // uv            0..1 screen position
    // scene(uv)     vec4, the finished image at uv (already gamma-corrected)
    // luma(rgb)     float, perceptual brightness
    // u_resolution  vec2, window size in pixels
    // u_time        float, seconds since the viewer launched
    return scene(uv);
}
```

The filter only sees colour. There is no depth or normal buffer, so an edge
exists only where brightness changes.

### Rendering with a filter

`ironsmith render` has no filter option. There are two ways to choose one:

- `filter = NAME` under `[viewer]` in `ironsmith.toml`. `render` applies this
  only when it starts a new viewer. A viewer that is already running keeps its
  current filter.
- Send `CMD:SET_FILTER:NAME` to the viewer on `127.0.0.1:7878`. This changes
  the running viewer and leaves the user's config alone. Use `none` to turn
  filters off.

```python
import socket, time
s = socket.create_connection(("127.0.0.1", 7878))
s.sendall(b"CMD:SET_FILTER:heatmap")
s.close()
time.sleep(2.5)  # the filter compiles in the background
```

Then run `ironsmith render` as usual. The viewer must already be running, so
do one render first if it isn't.

- **Check that it compiled.** A filter that fails, or a name that doesn't
  exist, is not reported to the client: the previous filter stays active and
  `render` still succeeds. Look for `Filter shader rejected` or
  `Unknown filter` in `forge.log`, next to the `filters` folder, and look at
  the PNG.
- **Turn it off afterwards.** The filter stays active for every later render
  and in the user's window until it is changed, so send `CMD:SET_FILTER:none`.
- **Window size matters.** Filters measured in pixels, such as dot, character
  or cell sizes, look coarser in a smaller window.

### Building a model for a filter

Design the model around what the filter reads:

- Rendered colour is `pow(colour, 0.4545)`, after lighting. `matte` is
  `paint * (diffuse + 0.1)`. `neon` is `paint * 1.8` and ignores lighting, so
  it is how to get an exact, flat value, but its hex values look much darker
  than they render. `#8F2E06` comes out bright orange.
- To reach a target brightness `v` with neon, paint `pow(v, 2.2) / 1.8`.
- The background is not black by default. For a true black, surround the scene
  with a `neon` `#000000` shell and cut a hole on the light's side so it casts
  no shadow.
- Brightness-driven filters (`heatmap`, `ascii`): use `neon` steps to place
  each part on the ramp.
- Edge filters: use hard shapes, and give touching parts different tones.
- Cell filters (`ascii`, pixel or mosaic effects): small detail disappears.
  Use one bold silhouette and large areas of colour, and avoid specular
  highlights, which read as stray bright cells.
- Black parts vanish under filters that draw dark outlines. Use a very dark
  colour instead.

## Conventions and tips

- **Y is up.** Put the ground at `y = 0` and give models a front that
  faces +X, so the default render view shows the front-right.
- `cube`, `cylinder` and `cone` are centred on the origin, so a part of
  height `h` resting on the ground is `move(0, h / 2, 0, ...)`.
- Lay a cylinder along Z (wheels, axles): `rotateX(90, cylinder(r, w))`.
  Along X: `rotateZ(90, cylinder(r, w))`.
- Define dimensions as variables at the top (`wheelRadius = 0.8`) and
  derive positions from them; adjusting proportions is then a one-line
  change.
- Cut openings with `difference`: `difference(body, move(..., hole))`.
  The cutter should poke slightly past the surface it cuts, or the cut
  leaves a paper-thin skin.
- The walls of a cut take the colour of the shape *being cut*, never the
  cutter's: `difference(paint(red, body), paint(black, hole))` gives a red
  hole. For a dark recess, cut the hole, then fill its floor with a dark
  shape that sits just below the surface:
  `group(difference(body, hole), paint(#111111, intersection(smallerBody, hole)))`,
  where `smallerBody` is the same body built about 0.5 units smaller.
- Fake detail with thin dark slabs pushed slightly through a surface
  (windows, panel lines) instead of cutting holes.
- Use `material(neon, ...)` for things that glow (lights, screens) and
  `material(metal, ...)` sparingly: metal reflects the background.
- Overlapping shapes in a `group` simply merge; no need to make them meet
  exactly.
- Rotation directions: `rotateZ(90, ...)` turns +Y towards +X (a cone's tip
  then points +X; `rotateZ(-90, ...)` points it at -X), and `rotateX(a, ...)`
  with positive `a` swings a downward-hanging part towards +Z.
- `metal` is mostly reflection, so a coloured metal (brass, copper) looks
  like the sky. Use glossy `plastic` with the metal's colour instead.
- Very thin features on a *non-uniformly scaled* sphere (e.g. a 0.04-thick
  rib on `scale(7, 2.3, 2.3, sphere(1))`) can be stepped over by the ray
  marcher and vanish. Use exact shapes for thin details (`torus` rings,
  thin `cube`s) or make them thicker.
- Keep everything within about 30 units of the origin: the viewer only
  marches rays inside a bounding sphere of radius 35 (larger when the
  camera is further away).
- See [`examples/`](../examples) for complete, commented scenes.

## Example

```
// A simple car: front faces +X, ground at y = 0
wr = 0.8
wx = 2.6
wz = 1.75
bodyY = 1.3

arch = rotateX(90, cylinder(wr + 0.15, 4.6))
body = difference(move(0, bodyY, 0, cube(8, 1.4, 3.6)),
                  group(move(wx, wr, 0, arch), move(-wx, wr, 0, arch)))
cabin = move(-0.4, bodyY + 1.3, 0, cube(4.2, 1.2, 3.2))
material(plastic, paint(#C81E1E, body, cabin))

wheel = paint(#1A1A1A, rotateX(90, cylinder(wr, 0.6)))
move(wx, wr, wz, wheel)
move(wx, wr, -wz, wheel)
move(-wx, wr, wz, wheel)
move(-wx, wr, -wz, wheel)

material(neon, paint(#FFF2A0, move(4, bodyY + 0.2, 1.2, sphere(0.28)), move(4, bodyY + 0.2, -1.2, sphere(0.28))))
```
