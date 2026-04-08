# 🏰 IronSmith

![Status: Alpha](https://img.shields.io/badge/Status-Alpha-orange)
![Haskell](https://img.shields.io/badge/Compiler-Haskell-5e5086?logo=haskell)
![Rust](https://img.shields.io/badge/Renderer-Rust-ce412b?logo=rust)

**IronSmith** is a Terminal-based 3D modeling scripting language. It allows you to build complex 3D shapes using code (Constructive Solid Geometry) and view the results instantly via a custom GPU-accelerated raymarching engine.

## 🚀 Features

* **Custom Scripting Language:** Write `.irsm` files using a clean, math-focused syntax.
* **TUI Editor:** A terminal user interface built in Haskell (`brick`) with syntax highlighting, a file manager, and error reporting.
* **Instant Hot-Reloading:** The moment you stop typing, the 3D viewer updates. No manual compilation required.
* **GPU Raymarching:** The renderer evaluates Signed Distance Fields (SDFs) natively on the GPU for perfectly smooth, mathematically accurate shapes.
* **Graceful Error Handling:** If you type a syntax error, the renderer catches the panic and holds the last working shape until you fix your code.

## 🏗️ Architecture

IronSmith is a monorepo utilizing two incredibly strict, high-performance languages communicating in real-time:

1. **The Forge (Haskell):** Acts as the orchestrator. It parses the custom `.irsm` language into an Abstract Syntax Tree (AST), evaluates variables, and compiles the result into optimized GLSL (OpenGL Shading Language) math. It also manages the lifecycle of the viewer process.
2. **The Viewer (Rust & wgpu):** A highly concurrent, cross-platform graphics window. It uses file-watching (`notify`) to detect incoming GLSL math from the Haskell compiler. It then wraps that math in a template, compiles the shader on the fly, and uses raymarching to render the SDFs to the screen.

## ⚙️ Prerequisites

To run IronSmith from source, you will need:
* **Haskell Toolchain:** GHC and Cabal
* **Rust Toolchain:** Cargo and `rustc`
* A GPU capable of Vulkan, Metal, or DX12.

## 🛠️ Quick Start

**1. Clone the repository:**
```bash
git clone [https://github.com/yourusername/IronSmith.git](https://github.com/yourusername/IronSmith.git)
cd IronSmith
```

**2. Build the Rust Viewer (Must be done first so Haskell can launch it):**



```bash
cd IronSmith-Viewer
cargo build --release
cd ..
```

**3. Run the Haskell TUI:**

```bash
cd IronSmith
cabal run ironsmith
```

## 📖 Language Syntax Overview
IronSmith supports basic primitives, transformations, and CSG (Constructive Solid Geometry) operations.

```Plaintext
// Variables
size = 5

// Primitives
cube(10, 10, 10)
sphere(size, 16)
cylinder(radius, definition, height)
cone(radius, top_radius, definition, height)
torus(radius, tube_radius, definition)

// Transformations
move(x, y, z, shape)
rotateX(degrees, shape)
rotateY(degrees, shape)
rotateZ(degrees, shape)

// CSG Operations (Union, Difference, Intersection)
difference(
    cube(10, 10, 10), 
    sphere(6, 16)
)
```

## 🗺️ Roadmap / Future Work
[x] Basic primitive parsing and rendering

[x] Cross-process hot-reloading

[x] Panic-free GPU fallback shaders

[ ] Material and color parsing

[ ] Exporting generated SDFs to .obj or .stl meshes

[ ] Free-flight camera controls

## 🤝 Contributing

IronSmith is an open-source forge, and contributions of any size are highly appreciated! Whether you're fixing a typo, optimizing the raymarching engine, or extending the Haskell parser, we'd love your help.

### How to Contribute

1. **Fork the repository**
2. **Create a new branch:** `git checkout -b feature/your-awesome-feature`
3. **Make your changes** (Be sure to test both the Haskell TUI and the Rust Viewer if your feature bridges both!)
4. **Commit your changes:** `git commit -m "Add some awesome feature"`
5. **Push to the branch:** `git push origin feature/your-awesome-feature`
6. **Open a Pull Request**

### Areas We Need Help With
* **Graphics (Rust):** Implementing material and color parsing in the GLSL generation.
* **Language (Haskell):** Adding variables, looping constructs, or new primitives to the `.irsm` language.
* **Tooling:** Exporting evaluated SDFs to standard 3D formats (`.obj` or `.stl`).
* **Quality of Life:** Expanding the Rust viewer to include free-flight camera controls.

### Development Guidelines
* **Haskell:** Please ensure your code compiles cleanly. Running your code through standard linters like `hlint` is highly encouraged.
* **Rust:** Keep the viewer blazing fast and safe. Please run `cargo fmt` and `cargo clippy` before submitting a PR to maintain code quality.

## ✨ Contributors

A massive thank you to everyone who has helped build the Forge!

* **Fraser W Levack** - *Creator & Lead Developer* - [@Fraser-Levack](https://github.com/Fraser-Levack)

*(Want to see your name here? Check out the section and open a PR!)*

## 📄 License
This project is licensed under the MIT License - see the [LICENSE](LICENCE) file for details.
