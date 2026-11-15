# Changelog

All notable changes to IronSmith. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and IronSmith uses
[Semantic Versioning](https://semver.org/): from 1.0.0 on, a script that works
in 1.x keeps working in every later 1.x release.

## [Unreleased] — 1.0.0

### ⚠️ Breaking changes

- **`difference(a, b)` now means "a minus b"** (the first shape with the
  second cut out of it), like other CSG tools. In 0.1 it meant "b minus a".
  Run `python scripts/migrate_difference.py model.irsm [...]` to update old
  models; migrated models render exactly as before, and the originals are
  kept as `.bak`.
- **Bad values are now errors** instead of silently rendering nothing or
  garbage: division by zero (and any other non-finite number), negative sizes
  (`sphere(-1)`), and `scale` factors of zero or below. The error points at
  the line that uses the value.
- **Config folder on Linux and macOS** moved to match the viewer:
  `~/.config/ironsmith` (Linux) and `~/Library/Application Support/ironsmith`
  (macOS), instead of `~/.ironsmith`. Settings and recent files are copied
  over automatically on first run. Windows is unchanged
  (`%APPDATA%\ironsmith`).
- The unused `default_material` and `editor_line_wrap` settings were removed
  (they never had any effect; old config files with them still load).

### Added

- **Post-processing filters**: user-editable GLSL filters in the config
  folder, with built-in `edge_detection`, `heatmap`, `ascii` and
  `passthrough` examples (Ctrl+F cycles them).
- **Camera animation**: `camera(t, yaw, pitch, dist [, x, y, z])` keyframes,
  played as a loop in the viewer (Ctrl+A).
- **Video export** (Ctrl+Shift+V): `.mp4` via ffmpeg if installed, otherwise
  an animated `.gif`.
- **Coloured glTF export** (`.glb`, Ctrl+Shift+G) keeping `paint(...)`
  colours and `material(...)` as PBR materials, alongside `.obj`.
- **Lighting**: `light(x, y, z [, intensity])` and an `exposure` setting.
- **Headless CLI** for scripts and AI agents: `ironsmith check`, `render`
  and `export`, with line-oriented output and exit codes
  (see `docs/AGENT_GUIDE.md`).
- `ironsmith model.irsm` opens the editor on a file (creating it on first
  save if it doesn't exist); `ironsmith --version` and
  `ironsmith-viewer --version`.
- **Relaunch Viewer** (Ctrl+L) brings the viewer window back after it was
  closed or failed to start.
- `launch_viewer_on_start` and `restore_last_file` settings now work.
- Editor: undo/redo, text selection with Shift+arrows and the mouse,
  copy/cut/paste (also to the OS clipboard), bracketed paste, clickable
  scroll bars, and the mouse wheel.
- Unary minus (`-size`, `-(a + b)`) and quoted colour/material names.
- Release archives for Linux and macOS as well as Windows, with SHA-256
  checksums. Archive names now include the CPU architecture.

### Changed

- **Much faster edits.** Numbers live in a GPU buffer instead of the shader,
  so changing a size, position, colour or light is instant. Recently used
  scene structures are cached, so undo/redo doesn't recompile either, and
  new structures compile about 2x faster.
- Mesh export fits its grid to the model, so small parts get full detail and
  large scenes (up to 100 units from the origin) export whole. Meshes are
  indexed, so files are much smaller.
- The editor reuses a viewer that is already open (e.g. one left by
  `ironsmith render`) instead of starting a second one.
- Camera orbit and easing run at the same speed on any display refresh rate
  (they used to be 2.4x faster at 144 Hz than at 60 Hz).
- Script files are read and written as UTF-8 on every platform.

### Fixed

- **No more lost work:** Ctrl+Q and opening another file now ask before
  discarding unsaved changes.
- Saving to a folder that doesn't exist or isn't writable shows an error
  instead of crashing the editor (and the buffer stays unsaved).
- A missing or broken viewer no longer crashes the editor at startup; the
  error is shown and Ctrl+L retries.
- Errors inside assignments are reported where they are. `body = cube(1, 2)`
  says "too few arguments" instead of "unexpected '='".
- Calling a function that doesn't exist (`spere(2)`) names it.
- Redefining a number as a shape (or the other way round) is checked
  properly, and the error points at the line that misuses it.
- `export` with nothing to export now fails with a message instead of
  writing an empty file; `--res` must be between 8 and 1024.
- Starting a second viewer while one is running exits with a message
  instead of crashing.
- Trackpad scrolling zooms the viewer (macOS trackpads report pixels, not
  lines).
- The viewer no longer crashes on windows larger than the GPU's maximum
  texture size, or on malformed scenes sent while typing.
- The editor's mode indicator matches the viewer when `auto_orbit = false`.

## [0.1.0] — 2026-06-12

First public release (Windows): the `.irsm` language with primitives,
transformations, CSG, `paint` and `material`; the TUI editor with live
preview; the GPU raymarching viewer; and `.obj` export.

[Unreleased]: https://github.com/Fraser-Levack/IronSmith/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/Fraser-Levack/IronSmith/releases/tag/v0.1.0
