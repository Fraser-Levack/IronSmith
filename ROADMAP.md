# IronSmith Roadmap

Where IronSmith is going, from 0.1 to 1.0 and beyond. 1.0 is a compatibility
promise: from then on, a script that works in 1.x keeps working in every later
1.x release. So 1.0 has to be stable and safe with your work, and the
language it ships has to be one we're happy to keep.

See [CHANGELOG.md](CHANGELOG.md) for what has already changed since 0.1.0.

---

## 1.0.0

### Phase 1 — Blockers ✅

Bugs that lose work, crash, or silently do the wrong thing.

- [x] Ctrl+Q and Open File ask before discarding unsaved changes
- [x] Save/open/settings errors are shown, never crash the editor
- [x] A missing or broken viewer is reported, not a startup crash; Ctrl+L relaunches it
- [x] Errors inside assignments point at the real problem (`body = cube(1, 2)`)
- [x] Unknown function names are reported by name
- [x] Value checks: division by zero, negative sizes, zero/negative scale
- [x] Number/shape redefinition is validated, with the right line
- [x] `export` refuses to write empty files; `--res` is range-checked
- [x] Linux/macOS: the editor and the viewer use the same config folder (with migration)
- [x] Second viewer on a busy port exits cleanly; the editor reuses a running viewer
- [x] Camera speed independent of refresh rate; trackpad zoom
- [x] `--version`, `ironsmith model.irsm`, working `launch_viewer_on_start` / `restore_last_file`
- [x] Release workflow runs tests, publishes checksums, marks `-rc` tags as pre-releases
- [x] README/cabal metadata fixes, dead files removed, CHANGELOG started

### Phase 2 — Language surface for 1.0

Decide what 1.0 promises *before* tagging it; additions later are fine,
changes are not.

- [ ] **Smooth CSG**: `smooth_union(k, a, b)` / `smooth_difference` / `smooth_intersection`.
  The signature SDF feature, and only one new opcode family
  (update `generate_program`, `evalSDF` **and** `evalHit`).
- [ ] **Mouse control in the viewer window**: drag to orbit, right/middle-drag to pan.
  Today only the wheel works there; everything else goes through the editor.
- [ ] **STL export** (`--format stl`): what 3D-printing users expect; reuses the mesher.
- [ ] Accept `.5` and `5.` as numbers (currently a parse error).
- [ ] Freeze the grammar: write down reserved words (`light`, `camera`, primitive
  names) so future keywords can't break existing variable names.

### Phase 3 — Should-fix polish

Not blocking, but worth doing before or soon after 1.0.

- [ ] **Ctrl+Shift shortcuts on Linux/macOS terminals.** Many terminals send
  Ctrl+Shift+E as Ctrl+E, so the export shortcuts trigger the wrong command.
  Test on common terminals; consider Alt-based defaults off Windows.
  (The command palette, Ctrl+P, always works.)
- [ ] Undo/redo restores the cursor position instead of jumping to the top.
- [ ] **Parallel mesh export.** Sampling is single-threaded; `castle.irsm` at
  the default resolution takes ~27s. The runtime is already `-threaded`.
- [ ] Viewer: render only when something changes (orbiting, animating, new
  scene) instead of redrawing continuously when idle.
- [ ] Viewer: request the high-performance GPU on laptops with two GPUs.
- [ ] Editor/viewer version handshake, so a stale `ironsmith-viewer` on PATH
  is reported instead of failing in confusing ways.
- [ ] Run `hlint` in CI (it isn't installed on the dev machine yet).

### Phase 4 — Release candidate

- [ ] Bump versions: `ironsmith.cabal` (`1.0.0`), `IronSmith-Viewer/Cargo.toml`
  (`1.0.0`), README badges.
- [ ] Tag `v1.0.0-rc.1`; check the workflow produces Windows, Linux and macOS
  archives plus `SHA256SUMS.txt`.
- [ ] Smoke-test each archive on a real machine (see checklist below).
- [ ] Linux: build on an older Ubuntu runner (e.g. `ubuntu-22.04`) so the
  binary runs on older glibc.
- [ ] macOS: decide on Intel support (an x86_64 runner, or document arm64 only).
- [ ] Docs site: version numbers, install page per platform, CHANGELOG link.
- [ ] Tag `v1.0.0`, review the draft release notes (paste the CHANGELOG
  section), publish.

**Smoke test, per platform**

1. `ironsmith --version` and `ironsmith-viewer --version` print 1.0.0.
2. `ironsmith` opens; the viewer window appears; the demo renders.
3. Open `examples/robot.irsm`, edit a number (instant update) and add a shape (recompiles).
4. Ctrl+F cycles filters (proves the config folder is shared with the viewer).
5. Close the viewer window, press Ctrl+L, and it comes back.
6. Make an edit, press Ctrl+Q: the editor asks to save first.
7. Export `.obj` and `.glb`; open the `.glb` in a glTF viewer.
8. `ironsmith render examples/robot.irsm -o robot.png` writes an image.
9. Ctrl+A plays the `launch_pad.irsm` animation; Ctrl+Shift+V exports a video.

---

## Installing from the command line

Nice-to-have, in order of effort. All of these need a **published** (not
draft) GitHub release with stable asset URLs and the `SHA256SUMS.txt` the
release workflow now produces.

1. **Scoop bucket** (Windows). Create `Fraser-Levack/scoop-ironsmith` with an
   `ironsmith.json` manifest (`url`, `hash`, `bin: ["ironsmith.exe",
   "ironsmith-viewer.exe"]`, `checkver`/`autoupdate` on GitHub releases).
   Users: `scoop bucket add ironsmith https://github.com/Fraser-Levack/scoop-ironsmith`
   then `scoop install ironsmith`. Live the day of release; no review.
2. **Homebrew tap** (macOS, Linux). Create `Fraser-Levack/homebrew-ironsmith`
   with a formula that installs both binaries into `bin`. Users:
   `brew install fraser-levack/ironsmith/ironsmith`. Homebrew downloads skip
   the Gatekeeper quarantine that unsigned browser downloads hit on macOS.
3. **winget** (Windows). Submit a manifest to `microsoft/winget-pkgs`
   (e.g. with `wingetcreate new <zip url>`): installer type `zip` with nested
   `portable` installers for both `.exe` files. The PR is reviewed and
   validated by Microsoft (usually a few days). Check that `ironsmith.exe`
   still finds the viewer through winget's portable-app links; the PATH
   fallback in `viewerPath` should cover it.
4. **Install script** for Linux/macOS (`curl -fsSL .../install.sh | sh`)
   that downloads the right archive and runs the bundled `install.sh`.
5. Later: automate manifest bumps from the release workflow.

---

## After 1.0

Ideas for 1.x, roughly by value. All of these are additions, so they fit the
1.x compatibility promise.

- **1.1: Language power.** `for` loops (`for i in 0..8 { ... }`), user-defined
  functions, math functions (`sin`, `cos`, `sqrt`, `min`, `max`, `pi`),
  `mirror(x/y/z, shape)`.
- **1.1: Object animation.** Animate `move` / `rotate` / `scale` over time,
  alongside the camera keyframes (on the README roadmap).
- **1.2: Primitives.** Ellipsoid, capsule, plane, rounded box, extrusions.
- **1.2: Output.** `render --size WxH` independent of the window; transparent
  PNG backgrounds; turntable shortcut for video export.
- **Editor.** Autocomplete for shape names, inline argument hints, a file
  browser in the Open dialog, line wrapping.
- **Viewer.** Anti-aliasing, ambient occlusion, an environment map for metal.
- **Fallback CPU renderer** for machines without Vulkan/Metal/DX12.
