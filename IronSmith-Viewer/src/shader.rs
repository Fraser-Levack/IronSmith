use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};

/// Append a line to the forge log; shared by the async compile threads,
/// which have no other way to surface errors.
pub fn log_line(path: &Path, msg: &str) {
    if let Ok(mut file) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
    {
        let _ = writeln!(file, "{}", msg);
    }
}

const SHADER_TEMPLATE: &str = r#"
#version 450
layout(set = 0, binding = 0) uniform Uniforms {
    vec2 u_resolution;
    float u_time;
    float u_camera_dist;
    vec2 u_rotation;
    float u_shadow_enabled;
    float u_march_steps;
    vec4 u_target_pos;
    vec4 u_bg_color;
    vec4 u_exposure;
};

// Every number in the scene (sizes, offsets, angles, colours, materials,
// light) lives here instead of being baked into the GLSL, so the shader
// source depends only on the scene's *structure*. Editing a number is a
// buffer write; only adding/removing operations needs a new pipeline.
// Layout: see `SceneProgram` in shader.rs.
// Fixed-size (not runtime-sized) so every constant-index read is provably in
// bounds: naga then emits no per-read bounds check, which otherwise made the
// driver compile of big scenes several times slower.
layout(set = 0, binding = 1, std430) readonly buffer SceneParams { float prm[{PARAM_COUNT}]; };

layout(location = 0) out vec4 out_color;

struct Hit { float d; vec3 col; int mat; };
Hit opU(Hit h1, Hit h2) { return (h1.d < h2.d) ? h1 : h2; }
Hit opS(Hit h1, Hit h2) { return (h1.d > -h2.d) ? h1 : Hit(-h2.d, h1.col, h1.mat); }
Hit opI(Hit h1, Hit h2) { return (h1.d > h2.d) ? h1 : h2; }

float sdSphere(vec3 p, float s) { return length(p)-s; }
float sdBox(vec3 p, vec3 b) { vec3 q = abs(p) - b; return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0); }
float sdCylinder(vec3 p, float h, float r) { vec2 d = abs(vec2(length(p.xz),p.y)) - vec2(r,h); return min(max(d.x,d.y),0.0) + length(max(d,0.0)); }
float sdTorus(vec3 p, vec2 t) { vec2 q = vec2(length(p.xz)-t.x,p.y); return length(q)-t.y; }
float sdCappedCone(vec3 p, float h, float r1, float r2) {
    vec2 q = vec2(length(p.xz), p.y); vec2 k1 = vec2(r2,h); vec2 k2 = vec2(r2-r1,2.0*h);
    vec2 ca = vec2(q.x-min(q.x,(q.y<0.0)?r1:r2), abs(q.y)-h);
    vec2 cb = q - k1 + k2*clamp(dot(k1-q,k2)/dot(k2,k2), 0.0, 1.0);
    float s = (cb.x<0.0 && ca.y<0.0) ? -1.0 : 1.0;
    return s*sqrt(min(dot(ca,ca),dot(cb,cb)));
}
vec3 opRep(vec3 p, vec3 c) {
    vec3 q = p;
    if (c.x > 0.0) q.x = mod(p.x + 0.5*c.x, c.x) - 0.5*c.x;
    if (c.y > 0.0) q.y = mod(p.y + 0.5*c.y, c.y) - 0.5*c.y;
    if (c.z > 0.0) q.z = mod(p.z + 0.5*c.z, c.z) - 0.5*c.z;
    return q;
}

Hit map(vec3 p) {
    vec3 current_p = p; float current_scale = 1.0; vec3 current_col = vec3(0.8, 0.4, 0.1);

    {GENERATED_BODY}
}

// Tetrahedral normal-sampling direction for tap i (0..3).
vec3 tapDir(int i) {
    return 0.5773 * (2.0 * vec3(float(((i + 3) >> 1) & 1), float((i >> 1) & 1), float(i & 1)) - 1.0);
}

// Phases of the single raymarching loop in main().
const int PH_PRIMARY = 0;    // march the camera ray
const int PH_NORMAL = 1;     // 4 taps around the hit point
const int PH_SHADOW = 2;     // soft-shadow march toward the light
const int PH_REFLECT = 3;    // metal: march the reflected ray
const int PH_REF_NORMAL = 4; // metal: 4 taps around the reflected hit
const int PH_DONE = 5;

// Why one loop: GPU drivers inline map() (the whole generated scene) at
// every call site. Separate march / calcNormal / shadow / reflection code
// called it from 5 places, so the driver compiled the scene 5 times over.
// Here map() is called exactly once, and each iteration feeds its result to
// whichever phase is active. Same steps, thresholds and maths as before.
void main() {
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    vec2 st = (2.0 * uv - 1.0) * vec2(u_resolution.x / u_resolution.y, -1.0);
    vec3 ro = u_target_pos.xyz + vec3(u_camera_dist * cos(u_rotation.x) * sin(u_rotation.y), u_camera_dist * sin(u_rotation.x), u_camera_dist * cos(u_rotation.x) * cos(u_rotation.y));
    vec3 ww = normalize(u_target_pos.xyz - ro);
    vec3 uu = normalize(cross(ww, vec3(0.0, 1.0, 0.0)));
    vec3 vv = normalize(cross(uu, ww));
    vec3 rd = normalize(st.x * uu + st.y * vv + 1.5 * ww);

    // light(...) from the script (prm[0] = 1), else a camera-relative default.
    vec3 light_dir = normalize(-ww + uu * 0.4 + vv * 0.6); float light_intensity = 1.0;
    if (prm[0] > 0.5) { light_dir = normalize(vec3(prm[1], prm[2], prm[3])); light_intensity = prm[4]; }

    // Only march inside a bounding sphere around the origin.
    float scene_radius = max(35.0, u_camera_dist * 1.5);
    float b = dot(ro, rd);
    float h = b * b - (dot(ro, ro) - scene_radius * scene_radius);
    float t = 0.0; float t_out = 0.0;
    int phase = PH_DONE;
    if (h >= 0.0) { t_out = -b + sqrt(h); t = max(0.0, -b - sqrt(h)); phase = PH_PRIMARY; }

    int max_steps = int(u_march_steps);
    bool shadows_on = u_shadow_enabled > 0.5;

    bool hit = false; vec3 material_col = vec3(0.0); int material_id = 0;
    vec3 pos = vec3(0.0); vec3 normal = vec3(0.0, 1.0, 0.0);
    float shadow = 1.0; float s_t = 0.0; float s_res = 1.0;
    vec3 ref_ro = vec3(0.0); vec3 ref_rd = vec3(0.0); float ref_t = 0.0;
    bool ref_hit = false; vec3 ref_col = vec3(0.0); vec3 ref_pos = vec3(0.0); vec3 ref_normal = vec3(0.0, 1.0, 0.0);
    vec3 n_acc = vec3(0.0); int tap = 0; int steps = 0;
    vec3 sample_p = ro + rd * t;

    // Upper bound on samples across all phases; the loop always ends via
    // PH_DONE first. Runtime-sized, so the driver can't unroll it.
    int budget = max_steps + 4 + 30 + 30 + 4;
    for (int it = 0; it < budget; it++) {
        if (phase == PH_DONE) break;
        Hit res = map(sample_p);

        if (phase == PH_PRIMARY) {
            if (res.d < max(0.001, 0.0002 * t)) {
                hit = true; material_col = res.col; material_id = res.mat;
                pos = ro + rd * t;
                phase = PH_NORMAL; tap = 0; n_acc = vec3(0.0);
                sample_p = pos + 0.0005 * tapDir(0);
            } else {
                t += res.d; steps++;
                if (t > t_out || steps >= max_steps) phase = PH_DONE;
                sample_p = ro + rd * t;
            }
        } else if (phase == PH_NORMAL) {
            n_acc += tapDir(tap) * res.d; tap++;
            if (tap < 4) {
                sample_p = pos + 0.0005 * tapDir(tap);
            } else {
                normal = normalize(n_acc);
                if (shadows_on) {
                    phase = PH_SHADOW; s_t = 0.05; s_res = 1.0; steps = 0;
                    sample_p = pos + light_dir * s_t;
                } else if (material_id == 3) {
                    phase = PH_REFLECT; ref_rd = reflect(rd, normal); ref_ro = pos + normal * 0.01; ref_t = 0.0; steps = 0;
                    sample_p = ref_ro;
                } else {
                    phase = PH_DONE;
                }
            }
        } else if (phase == PH_SHADOW) {
            bool shadow_done = false;
            if (res.d < 0.001) {
                shadow = 0.1; shadow_done = true;
            } else {
                s_res = min(s_res, 12.0 * res.d / s_t);
                s_t += res.d; steps++;
                if (s_t > 20.0 || steps >= 30) { shadow = clamp(s_res, 0.1, 1.0); shadow_done = true; }
            }
            if (!shadow_done) {
                sample_p = pos + light_dir * s_t;
            } else if (material_id == 3) {
                phase = PH_REFLECT; ref_rd = reflect(rd, normal); ref_ro = pos + normal * 0.01; ref_t = 0.0; steps = 0;
                sample_p = ref_ro;
            } else {
                phase = PH_DONE;
            }
        } else if (phase == PH_REFLECT) {
            if (res.d < max(0.005, 0.001 * ref_t)) {
                ref_hit = true; ref_col = res.col; ref_pos = ref_ro + ref_rd * ref_t;
                phase = PH_REF_NORMAL; tap = 0; n_acc = vec3(0.0);
                sample_p = ref_pos + 0.0005 * tapDir(0);
            } else if (ref_t > 20.0) {
                phase = PH_DONE;
            } else {
                ref_t += res.d; steps++;
                if (steps >= 30) phase = PH_DONE;
                sample_p = ref_ro + ref_rd * ref_t;
            }
        } else if (phase == PH_REF_NORMAL) {
            n_acc += tapDir(tap) * res.d; tap++;
            if (tap < 4) {
                sample_p = ref_pos + 0.0005 * tapDir(tap);
            } else {
                ref_normal = normalize(n_acc); phase = PH_DONE;
            }
        }
    }

    vec3 bg_color = u_bg_color.rgb; vec3 col = bg_color;

    if (hit) {
        vec3 view_dir = normalize(ro - pos);
        float lit = shadow * light_intensity;

        if (material_id == 0) {
            col = material_col * max(dot(normal, light_dir), 0.0) * lit + material_col * 0.1;
        }
        else if (material_id == 1) {
            vec3 half_dir = normalize(light_dir + view_dir);
            float diff = max(dot(normal, light_dir), 0.0) * lit;
            float spec = pow(max(dot(normal, half_dir), 0.0), 64.0) * lit;
            col = (material_col * diff) + (material_col * 0.1) + vec3(1.0) * spec;
        }
        else if (material_id == 2) {
            col = material_col * 1.8;
        }
        else if (material_id == 3) {
            vec3 shaded_ref = ref_hit ? (ref_col * max(dot(ref_normal, light_dir), 0.0) + ref_col * 0.1) : (bg_color + max(ref_rd.y, 0.0) * 0.3);
            float spec = pow(max(dot(normal, normalize(light_dir + view_dir)), 0.0), 128.0) * lit;
            col = mix(material_col * 0.2 * lit, shaded_ref, 0.8) + vec3(1.0) * spec;
        }
    }
    out_color = vec4(pow(col * u_exposure.x, vec3(0.4545)), 1.0);
}
"#;

/// Number of leading `prm[]` slots reserved for scene-wide values, before
/// the per-instruction parameters:
///   prm[0] = 1.0 if the script has a light(...) statement, else 0.0
///   prm[1..4] = light direction, prm[4] = light intensity
///   prm[5..8] = reserved (keeps geometry params 16-byte aligned)
pub const HEADER_PARAMS: usize = 8;

/// A scene compiled for the GPU: GLSL that depends only on the scene's
/// structure, plus the numbers it reads from the `prm[]` storage buffer.
///
/// Two scenes that differ only in numbers (sizes, positions, angles,
/// colours, materials, light) produce the same `body`, so `body` is the
/// pipeline cache key and such edits never recompile anything.
#[derive(Debug, Clone, PartialEq)]
pub struct SceneProgram {
    pub body: String,
    pub params: Vec<f32>,
}

/// Walk the bytecode for lighting opcodes (40 = light direction, 41 = light
/// intensity) and fill the header slots described at `HEADER_PARAMS`. Uses
/// the same instruction strides as `generate_program`, so argument floats
/// are never misread as opcodes. A zero direction is ignored (it would
/// normalize to NaN), falling back to the default light.
pub fn extract_light(bytecode: &[f32]) -> [f32; HEADER_PARAMS] {
    let mut dir: Option<[f32; 3]> = None;
    let mut intensity: Option<f32> = None;
    let mut pc = 0;

    while pc < bytecode.len() {
        let width = if matches!(bytecode[pc] as i32, 2 | 4 | 23) {
            8
        } else {
            4
        };
        if pc + width > bytecode.len() {
            break; // truncated; generate_program reports it
        }
        match bytecode[pc] as i32 {
            0 => break,
            40 => {
                let d = [bytecode[pc + 1], bytecode[pc + 2], bytecode[pc + 3]];
                if d.iter().any(|v| *v != 0.0) {
                    dir = Some(d);
                }
            }
            41 => intensity = Some(bytecode[pc + 1].max(0.0)),
            _ => {}
        }
        pc += width;
    }

    let mut header = [0.0; HEADER_PARAMS];
    if let Some([x, y, z]) = dir {
        header[..5].copy_from_slice(&[1.0, x, y, z, intensity.unwrap_or(1.0)]);
    }
    header
}

/// Appends parameter values and hands back the GLSL that reads them.
struct Params(Vec<f32>);

impl Params {
    /// Store `v` and return `prm[k]`.
    fn f(&mut self, v: f32) -> String {
        self.0.push(v);
        format!("prm[{}]", self.0.len() - 1)
    }
    /// Store three values and return a `vec3(...)` reading them.
    fn v3(&mut self, x: f32, y: f32, z: f32) -> String {
        format!("vec3({}, {}, {})", self.f(x), self.f(y), self.f(z))
    }
    /// A material id: stored as a float, read back as an int.
    fn mat(&mut self, v: f32) -> String {
        format!("int({})", self.f(v))
    }
}

/// Translates the bytecode emitted by the Haskell compiler into the body of
/// the GLSL `map()` function plus its parameter buffer, walking the same
/// opcode stack machine that the CPU-side OBJ exporter and the GPU
/// raymarcher both implement.
///
/// Returns Err for malformed bytecode (e.g. a CSG op with fewer than two
/// shapes on the stack, which happens mid-edit when a script references a
/// shape that doesn't exist yet). The caller keeps the last working shader.
pub fn generate_program(bytecode: &[f32]) -> Result<SceneProgram, String> {
    let mut body = String::new();
    let mut prm = Params(extract_light(bytecode).to_vec());
    let mut pc = 0;

    let mut sp: i32 = 0;
    let mut tsp: i32 = 0;
    let mut csp: i32 = 0;

    // Peak depth of each stack: every slot ever used becomes a declared
    // GLSL register. Tracking the peak (not the final depth) is what keeps
    // deep scenes from referencing undeclared variables.
    let mut max_sp: i32 = 0;
    let mut max_tsp: i32 = 0;
    let mut max_csp: i32 = 0;

    while pc < bytecode.len() {
        let iop = bytecode[pc] as i32;

        if (20..=26).contains(&iop) && iop != 25 {
            body.push_str(&format!(
                "p{} = current_p; sc{} = current_scale;\n",
                tsp, tsp
            ));
            tsp += 1;
        }

        // Stride check: a truncated instruction at the end of the buffer
        // would otherwise index out of bounds below.
        let width = if matches!(iop, 2 | 4 | 23) { 8 } else { 4 };
        if iop != 0 && pc + width > bytecode.len() {
            return Err(format!("truncated instruction {} at offset {}", iop, pc));
        }
        let a = |i: usize| bytecode[pc + i];

        match iop {
            0 => break, // OP_HALT
            1 => {
                // SPHERE(radius, material)
                let (r, m) = (prm.f(a(1)), prm.mat(a(2)));
                body.push_str(&format!(
                    "s{} = Hit(sdSphere(current_p, {}) * current_scale, current_col, {});\n",
                    sp, r, m
                ));
                sp += 1;
            }
            2 => {
                // BOX(half extents, material)
                let (b, m) = (prm.v3(a(1), a(2), a(3)), prm.mat(a(4)));
                body.push_str(&format!(
                    "s{} = Hit(sdBox(current_p, {}) * current_scale, current_col, {});\n",
                    sp, b, m
                ));
                sp += 1;
            }
            3 => {
                // CYLINDER(radius, half height, material)
                let (h, r, m) = (prm.f(a(2)), prm.f(a(1)), prm.mat(a(3)));
                body.push_str(&format!(
                    "s{} = Hit(sdCylinder(current_p, {}, {}) * current_scale, current_col, {});\n",
                    sp, h, r, m
                ));
                sp += 1;
            }
            4 => {
                // CONE(r1, r2, half height, material)
                let (h, r1, r2, m) = (prm.f(a(3)), prm.f(a(1)), prm.f(a(2)), prm.mat(a(4)));
                body.push_str(&format!(
                    "s{} = Hit(sdCappedCone(current_p, {}, {}, {}) * current_scale, current_col, {});\n",
                    sp, h, r1, r2, m
                ));
                sp += 1;
            }
            5 => {
                // TORUS(major, minor, material)
                let (t1, t2, m) = (prm.f(a(1)), prm.f(a(2)), prm.mat(a(3)));
                body.push_str(&format!(
                    "s{} = Hit(sdTorus(current_p, vec2({}, {})) * current_scale, current_col, {});\n",
                    sp, t1, t2, m
                ));
                sp += 1;
            }
            10..=12 => {
                // UNION / DIFF / INTERSECT
                if sp < 2 {
                    return Err(format!(
                        "CSG op {} needs two shapes but the stack has {}",
                        iop, sp
                    ));
                }
                let op = match iop {
                    10 => "opU",
                    11 => "opS",
                    _ => "opI",
                };
                sp -= 1;
                let b = sp;
                sp -= 1;
                let a = sp;
                body.push_str(&format!("s{} = {}(s{}, s{});\n", a, op, a, b));
                sp += 1;
            }
            20..=22 => {
                // ROTATE_X / Y / Z by (cos, sin)
                let (c, s) = (prm.f(a(1)), prm.f(a(2)));
                let m = match iop {
                    20 => format!("mat3(1.0, 0.0, 0.0, 0.0, {c}, {s}, 0.0, -{s}, {c})"),
                    21 => format!("mat3({c}, 0.0, -{s}, 0.0, 1.0, 0.0, {s}, 0.0, {c})"),
                    _ => format!("mat3({c}, {s}, 0.0, -{s}, {c}, 0.0, 0.0, 0.0, 1.0)"),
                };
                body.push_str(&format!("current_p = {} * current_p;\n", m));
            }
            23 => {
                // SCALE(inverse per-axis factors, distance factor)
                let (k, v) = (prm.f(a(4)), prm.v3(a(1), a(2), a(3)));
                body.push_str(&format!("current_scale *= {};\ncurrent_p *= {};\n", k, v));
            }
            24 => {
                // MOVE
                let v = prm.v3(a(1), a(2), a(3));
                body.push_str(&format!("current_p -= {};\n", v));
            }
            26 => {
                // REPEAT
                let v = prm.v3(a(1), a(2), a(3));
                body.push_str(&format!("current_p = opRep(current_p, {});\n", v));
            }
            25 => {
                // POP_TRANSFORM
                if tsp < 1 {
                    return Err("transform pop with no transform pushed".to_string());
                }
                tsp -= 1;
                body.push_str(&format!(
                    "current_p = p{}; current_scale = sc{};\n",
                    tsp, tsp
                ));
            }
            30 => {
                // PUSH_COLOR
                let v = prm.v3(a(1), a(2), a(3));
                body.push_str(&format!("c{} = current_col; current_col = {};\n", csp, v));
                csp += 1;
            }
            31 => {
                // POP_COLOR
                if csp < 1 {
                    return Err("color pop with no color pushed".to_string());
                }
                csp -= 1;
                body.push_str(&format!("current_col = c{};\n", csp));
            }
            _ => {} // lights (40/41, read by extract_light) and unknown ops
        }
        pc += width;

        max_sp = max_sp.max(sp);
        max_tsp = max_tsp.max(tsp);
        max_csp = max_csp.max(csp);
    }

    // Sanity cap: legitimate scenes stay far below this (the Haskell
    // compiler orders commutative ops to keep the hit stack shallow), and a
    // shader with thousands of live registers would only stall the driver.
    if max_sp > MAX_REGISTERS || max_tsp > MAX_REGISTERS || max_csp > MAX_REGISTERS {
        return Err(format!(
            "scene too deeply nested: {} shapes, {} transforms, {} colors on the stack at once (limit {})",
            max_sp, max_tsp, max_csp, MAX_REGISTERS
        ));
    }

    if sp > 0 {
        body.push_str("    return s0;\n");
    } else {
        body.push_str("    return Hit(999999.0, vec3(0.0), 0);\n");
    }

    let mut decls = String::new();
    declare_registers(&mut decls, "Hit", "s", max_sp);
    declare_registers(&mut decls, "vec3", "p", max_tsp);
    declare_registers(&mut decls, "float", "sc", max_tsp);
    declare_registers(&mut decls, "vec3", "c", max_csp);
    decls.push_str(&body);

    Ok(SceneProgram {
        body: decls,
        params: prm.0,
    })
}

/// Upper bound on any one register stack's depth in a generated shader.
const MAX_REGISTERS: i32 = 256;

/// Append `ty prefix0, prefix1, ...;` for `count` registers (nothing if 0).
fn declare_registers(out: &mut String, ty: &str, prefix: &str, count: i32) {
    if count > 0 {
        let names: Vec<String> = (0..count).map(|i| format!("{}{}", prefix, i)).collect();
        out.push_str(&format!("{} {};\n", ty, names.join(", ")));
    }
}

/// Build the complete scene fragment shader for a program's structure.
/// `param_count` is `SceneProgram::params.len()`, which is a function of
/// the structure alone (every opcode always stores the same number of
/// params), so it never varies for a given cache key.
pub fn build_scene_source(body: &str, param_count: usize) -> String {
    SHADER_TEMPLATE
        .replace("{PARAM_COUNT}", &param_count.max(HEADER_PARAMS).to_string())
        .replace("{GENERATED_BODY}", body)
}

/// Parse and validate GLSL into naga IR without touching wgpu, so syntax
/// errors surface as values instead of the internal unwrap-panic wgpu's
/// GLSL path has (fatal in release builds, where panic = "abort").
pub fn parse_glsl_fragment(source: &str) -> Result<naga::Module, String> {
    let mut frontend = naga::front::glsl::Frontend::default();
    let options = naga::front::glsl::Options::from(naga::ShaderStage::Fragment);
    let module = frontend
        .parse(&options, source)
        .map_err(|e| format!("{:?}", e))?;

    let mut validator = naga::valid::Validator::new(
        naga::valid::ValidationFlags::all(),
        naga::valid::Capabilities::all(),
    );
    validator
        .validate(&module)
        .map_err(|e| format!("{:?}", e))?;

    Ok(module)
}

/// Build a fragment-shader render pipeline without any panic path: the GLSL
/// is parsed/validated by naga first (errors returned as values), and the
/// remaining wgpu calls run inside a validation error scope, which also
/// works in release builds where panic = "abort" makes catch_unwind useless.
pub fn build_pipeline_checked(
    device: &wgpu::Device,
    layout: &wgpu::PipelineLayout,
    format: wgpu::TextureFormat,
    label: &str,
    fragment_source: &str,
) -> Result<wgpu::RenderPipeline, String> {
    let fs_ir = parse_glsl_fragment(fragment_source)?;

    device.push_error_scope(wgpu::ErrorFilter::Validation);

    let fs_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some(label),
        source: wgpu::ShaderSource::Naga(std::borrow::Cow::Owned(fs_ir)),
    });

    let vs_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(std::borrow::Cow::Borrowed(
            "
            @vertex fn main(@builtin(vertex_index) i: u32) -> @builtin(position) vec4<f32> {
                var pos = array<vec2<f32>, 3>(vec2(-1.0, -1.0), vec2(3.0, -1.0), vec2(-1.0, 3.0));
                return vec4<f32>(pos[i], 0.0, 1.0);
            }
        ",
        )),
    });

    let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some(label),
        layout: Some(layout),
        vertex: wgpu::VertexState {
            module: &vs_module,
            entry_point: "main",
            buffers: &[],
        },
        fragment: Some(wgpu::FragmentState {
            module: &fs_module,
            entry_point: "main",
            targets: &[Some(wgpu::ColorTargetState {
                format,
                blend: Some(wgpu::BlendState::REPLACE),
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        primitive: wgpu::PrimitiveState::default(),
        depth_stencil: None,
        multisample: wgpu::MultisampleState::default(),
        multiview: None,
    });

    match pollster::block_on(device.pop_error_scope()) {
        None => Ok(pipeline),
        Some(e) => Err(e.to_string()),
    }
}

/// Compiled scene pipelines keyed by their structure (`SceneProgram::body`),
/// shared between the renderer (instant installs on a hit) and the compile
/// worker (never compiles the same structure twice). Least-recently-used
/// entries are dropped beyond `capacity`.
pub struct PipelineCache {
    entries: HashMap<Arc<str>, (Arc<wgpu::RenderPipeline>, u64)>,
    clock: u64,
    capacity: usize,
}

/// How many distinct scene structures stay compiled. Each costs a little
/// GPU memory; 32 covers undo/redo across a long editing session.
pub const PIPELINE_CACHE_CAPACITY: usize = 32;

pub type SharedPipelineCache = Arc<Mutex<PipelineCache>>;

impl PipelineCache {
    pub fn new(capacity: usize) -> Self {
        Self {
            entries: HashMap::new(),
            clock: 0,
            capacity: capacity.max(1),
        }
    }

    pub fn shared(capacity: usize) -> SharedPipelineCache {
        Arc::new(Mutex::new(Self::new(capacity)))
    }

    pub fn get(&mut self, key: &str) -> Option<Arc<wgpu::RenderPipeline>> {
        self.clock += 1;
        let now = self.clock;
        self.entries.get_mut(key).map(|(p, used)| {
            *used = now;
            p.clone()
        })
    }

    pub fn insert(&mut self, key: Arc<str>, pipeline: Arc<wgpu::RenderPipeline>) {
        self.clock += 1;
        self.entries.insert(key, (pipeline, self.clock));
        while self.entries.len() > self.capacity {
            let oldest = self
                .entries
                .iter()
                .min_by_key(|(_, (_, used))| *used)
                .map(|(k, _)| k.clone());
            match oldest {
                Some(k) => self.entries.remove(&k),
                None => break,
            };
        }
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }
}

/// A scene the renderer needs a pipeline for.
pub struct SceneJob {
    pub generation: u64,
    pub key: Arc<str>,
    pub params: Vec<f32>,
}

/// The outcome of a `SceneJob`. `generation` is the newest scene request
/// this result covers (older queued requests it superseded are covered
/// too), so callers waiting on generation N can treat any result with
/// generation >= N as "your scene is done". On success the pipeline is
/// meant to be installed together with `params`.
pub struct SceneResult {
    pub generation: u64,
    pub key: Arc<str>,
    pub params: Vec<f32>,
    pub pipeline: Result<Arc<wgpu::RenderPipeline>, String>,
}

/// Start the background scene compiler and return the queue that feeds it.
///
/// A single long-lived worker handles jobs one at a time. Before each one
/// it drains the queue and keeps only the newest, so edits made while a
/// slow driver compile is running collapse into one follow-up instead of
/// piling up. Because jobs run sequentially, results arrive on `tx` in edit
/// order. A job whose structure is already in `cache` (e.g. the same shape
/// was queued twice while it compiled) is answered without compiling.
pub fn spawn_scene_compiler(
    device: Arc<wgpu::Device>,
    layout: Arc<wgpu::PipelineLayout>,
    format: wgpu::TextureFormat,
    cache: SharedPipelineCache,
    tx: mpsc::Sender<SceneResult>,
    log_path: PathBuf,
) -> mpsc::Sender<SceneJob> {
    let (job_tx, job_rx) = mpsc::channel::<SceneJob>();

    std::thread::spawn(move || {
        while let Ok(mut job) = job_rx.recv() {
            while let Ok(newer) = job_rx.try_recv() {
                job = newer;
            }

            let cached = cache.lock().ok().and_then(|mut c| c.get(&job.key));
            let pipeline = match cached {
                Some(p) => Ok(p),
                None => {
                    let started = std::time::Instant::now();
                    let source = build_scene_source(&job.key, job.params.len());
                    let result = build_pipeline_checked(
                        &device,
                        &layout,
                        format,
                        "IronSmith Scene",
                        &source,
                    )
                    .map(Arc::new)
                    .map_err(|e| format!("Scene shader rejected: {}", e));
                    match &result {
                        Ok(p) => {
                            let cached = cache
                                .lock()
                                .map(|mut c| {
                                    c.insert(job.key.clone(), p.clone());
                                    c.len()
                                })
                                .unwrap_or(0);
                            log_line(
                                &log_path,
                                &format!(
                                    "Scene compiled in {:.2}s ({} params, {} structures cached)",
                                    started.elapsed().as_secs_f32(),
                                    job.params.len(),
                                    cached
                                ),
                            );
                        }
                        Err(e) => log_line(&log_path, e),
                    }
                    result
                }
            };

            let result = SceneResult {
                generation: job.generation,
                key: job.key,
                params: job.params,
                pipeline,
            };
            if tx.send(result).is_err() {
                break; // renderer is gone
            }
        }
    });

    job_tx
}

#[cfg(test)]
mod tests {
    use super::*;

    fn body(bytecode: &[f32]) -> String {
        generate_program(bytecode).unwrap().body
    }

    /// Full shader source for a bytecode program, validated by naga.
    fn validate(bytecode: &[f32]) -> String {
        let prog = generate_program(bytecode).unwrap();
        let source = build_scene_source(&prog.body, prog.params.len());
        parse_glsl_fragment(&source).unwrap();
        source
    }

    #[test]
    fn empty_bytecode_returns_a_miss() {
        let b = body(&[]);
        assert!(b.contains("return Hit(999999.0, vec3(0.0), 0);"));
        assert!(!b.contains("return s0;"));
    }

    #[test]
    fn halt_opcode_stops_generation() {
        assert!(!body(&[0.0, 0.0, 0.0, 0.0, 1.0, 5.0, 0.0, 0.0]).contains("sdSphere"));
    }

    #[test]
    fn sphere_reads_its_radius_and_material_from_params() {
        let prog = generate_program(&[1.0, 5.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap();
        let (r, m) = (HEADER_PARAMS, HEADER_PARAMS + 1);
        assert!(prog.body.contains(&format!(
            "s0 = Hit(sdSphere(current_p, prm[{r}]) * current_scale, current_col, int(prm[{m}]));"
        )));
        assert!(prog.body.contains("return s0;"));
        assert_eq!(prog.params[r], 5.0);
        assert_eq!(prog.params[m], 1.0);
    }

    #[test]
    fn union_opcode_combines_two_shapes() {
        // sphere, sphere, union, halt
        let b = body(&[
            1.0, 1.0, 0.0, 0.0, 1.0, 2.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ]);
        assert!(b.contains("s0 = opU(s0, s1);"));
        assert!(b.contains("return s0;"));
    }

    #[test]
    fn rotate_and_pop_transform_restore_state() {
        // rotateX(c=1,s=0), sphere, pop-transform, halt
        let b = body(&[
            20.0, 1.0, 0.0, 0.0, 1.0, 5.0, 0.0, 0.0, 25.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ]);
        let (c, s) = (HEADER_PARAMS, HEADER_PARAMS + 1);
        assert!(b.contains("p0 = current_p; sc0 = current_scale;"));
        assert!(b.contains(&format!(
            "current_p = mat3(1.0, 0.0, 0.0, 0.0, prm[{c}], prm[{s}], 0.0, -prm[{s}], prm[{c}]) * current_p;"
        )));
        assert!(b.contains("current_p = p0; current_scale = sc0;"));
    }

    /// paint(rgb, move(x, y, z, rotateY(cos, sin, box(w, h, d)))) twice,
    /// unioned: every kind of number the language has.
    fn painted_scene(x: f32, rgb: [f32; 3], w: f32, angle: f32, mat: f32) -> Vec<f32> {
        let (c, s) = (angle.cos(), angle.sin());
        let one = |dx: f32| {
            vec![
                30.0,
                rgb[0],
                rgb[1],
                rgb[2], //
                24.0,
                x + dx,
                1.0,
                2.0, //
                21.0,
                c,
                s,
                0.0, //
                2.0,
                w,
                1.0,
                1.0,
                mat,
                0.0,
                0.0,
                0.0, //
                25.0,
                0.0,
                0.0,
                0.0, //
                25.0,
                0.0,
                0.0,
                0.0, //
                31.0,
                0.0,
                0.0,
                0.0,
            ]
        };
        let mut bc = one(0.0);
        bc.extend(one(3.0));
        bc.extend([10.0, 0.0, 0.0, 0.0]);
        bc
    }

    #[test]
    fn number_only_edits_keep_the_same_structure() {
        // The heart of the pipeline cache: positions, colours, sizes,
        // angles and materials change only the params, never the GLSL.
        let a = generate_program(&painted_scene(1.0, [1.0, 0.0, 0.0], 2.0, 0.3, 0.0)).unwrap();
        let b = generate_program(&painted_scene(7.5, [0.1, 0.9, 0.2], 4.0, 1.2, 3.0)).unwrap();
        assert_eq!(a.body, b.body);
        assert_ne!(a.params, b.params);
        assert_eq!(a.params.len(), b.params.len());
        // No literal scene numbers leak into the source.
        assert!(!a.body.contains("7.5") && !a.body.contains("1.0)") && !a.body.contains("0.3"));
    }

    #[test]
    fn structural_edits_change_the_key() {
        let base = painted_scene(1.0, [1.0, 0.0, 0.0], 2.0, 0.3, 0.0);
        let mut more = base.clone();
        more.extend([1.0, 1.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0]); // + sphere, union
        let mut diff = base.clone();
        let n = diff.len();
        diff[n - 4] = 11.0; // union -> difference
        let key = |bc: &[f32]| generate_program(bc).unwrap().body;
        assert_ne!(key(&base), key(&more));
        assert_ne!(key(&base), key(&diff));
    }

    #[test]
    fn params_line_up_with_the_bytecode() {
        let prog = generate_program(&painted_scene(1.5, [0.25, 0.5, 0.75], 2.0, 0.0, 2.0)).unwrap();
        let h = HEADER_PARAMS;
        // First draw: colour, move, rotate (cos, sin), box extents, material.
        assert_eq!(
            &prog.params[h..h + 12],
            &[0.25, 0.5, 0.75, 1.5, 1.0, 2.0, 1.0, 0.0, 2.0, 1.0, 1.0, 2.0]
        );
    }

    #[test]
    fn csg_with_missing_operand_errors_instead_of_panicking() {
        // One sphere then a difference: mid-edit state where the second
        // operand was an unresolved shape reference. Used to underflow
        // and abort the release viewer.
        assert!(generate_program(&[1.0, 5.0, 0.0, 0.0, 11.0, 0.0, 0.0, 0.0]).is_err());
    }

    #[test]
    fn unbalanced_pops_error_instead_of_panicking() {
        assert!(generate_program(&[25.0, 0.0, 0.0, 0.0]).is_err());
        assert!(generate_program(&[31.0, 0.0, 0.0, 0.0]).is_err());
    }

    /// Bytecode for `n` spheres pushed one after another, then joined by
    /// n-1 unions: the hit stack peaks at depth `n`.
    fn stacked_spheres(n: usize) -> Vec<f32> {
        let mut bc = Vec::new();
        for i in 0..n {
            bc.extend_from_slice(&[1.0, 1.0 + i as f32, 0.0, 0.0]);
        }
        for _ in 1..n {
            bc.extend_from_slice(&[10.0, 0.0, 0.0, 0.0]);
        }
        bc
    }

    #[test]
    fn full_scene_shader_validates() {
        // Covers the template itself (looped calcNormal, params buffer,
        // light header) and every opcode.
        validate(&painted_scene(1.0, [1.0, 0.0, 0.0], 2.0, 0.3, 1.0));
        validate(&[
            1.0, 5.0, 0.0, 0.0, // sphere
            3.0, 1.0, 2.0, 0.0, // cylinder
            11.0, 0.0, 0.0, 0.0, // difference
            4.0, 1.0, 0.5, 2.0, 0.0, 0.0, 0.0, 0.0, // cone
            12.0, 0.0, 0.0, 0.0, // intersection
            23.0, 0.5, 0.5, 0.5, 2.0, 0.0, 0.0, 0.0, // scale
            26.0, 4.0, 0.0, 0.0, // repeat
            20.0, 1.0, 0.0, 0.0, 22.0, 1.0, 0.0, 0.0, // rotate x, z
            5.0, 2.0, 0.5, 2.0, // torus
            25.0, 0.0, 0.0, 0.0, 25.0, 0.0, 0.0, 0.0, 25.0, 0.0, 0.0, 0.0, 25.0, 0.0, 0.0, 0.0,
            10.0, 0.0, 0.0, 0.0, // union
            40.0, 1.0, 2.0, 3.0, 41.0, 0.5, 0.0, 0.0, // light
        ]);
    }

    #[test]
    fn deep_hit_stack_declares_every_register_it_uses() {
        // Peaks at 20 live shapes. The old fixed s0..s15 register file
        // only checked the *final* depth, so this produced GLSL using an
        // undeclared `s16` and the scene was silently rejected.
        assert!(validate(&stacked_spheres(20)).contains("s19"));
    }

    #[test]
    fn deep_transform_nesting_validates() {
        // 20 nested move(...)s around a sphere.
        let mut bc = Vec::new();
        for _ in 0..20 {
            bc.extend_from_slice(&[24.0, 1.0, 0.0, 0.0]);
        }
        bc.extend_from_slice(&[1.0, 1.0, 0.0, 0.0]);
        for _ in 0..20 {
            bc.extend_from_slice(&[25.0, 0.0, 0.0, 0.0]);
        }
        validate(&bc);
    }

    #[test]
    fn absurdly_deep_stack_is_rejected() {
        assert!(generate_program(&stacked_spheres(MAX_REGISTERS as usize + 1)).is_err());
    }

    #[test]
    fn truncated_instruction_errors_instead_of_panicking() {
        // A box needs 8 floats; only 4 arrived.
        assert!(generate_program(&[2.0, 1.0, 1.0, 1.0]).is_err());
        assert_eq!(extract_light(&[2.0, 1.0, 1.0, 1.0])[0], 0.0);
    }

    #[test]
    fn light_opcode_fills_the_header() {
        // sphere, light(10, 20, 5), intensity 2, halt
        let bytecode = [
            1.0, 5.0, 0.0, 0.0, 40.0, 10.0, 20.0, 5.0, 41.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ];
        let header = extract_light(&bytecode);
        assert_eq!(&header[..5], &[1.0, 10.0, 20.0, 5.0, 2.0]);
        // And it reaches the params buffer the shader reads.
        assert_eq!(
            &generate_program(&bytecode).unwrap().params[..5],
            &header[..5]
        );
    }

    #[test]
    fn light_without_intensity_defaults_to_one() {
        assert_eq!(extract_light(&[40.0, 0.0, 1.0, 0.0])[4], 1.0);
    }

    #[test]
    fn no_light_opcode_keeps_default_light() {
        assert_eq!(extract_light(&[1.0, 5.0, 0.0, 0.0])[0], 0.0);
    }

    #[test]
    fn light_argument_floats_are_not_misread_as_opcodes() {
        // A box with a 40.0 argument must not be treated as a light op.
        assert_eq!(
            extract_light(&[2.0, 40.0, 40.0, 40.0, 0.0, 0.0, 0.0, 0.0])[0],
            0.0
        );
    }

    #[test]
    fn zero_direction_light_falls_back_to_default() {
        assert_eq!(extract_light(&[40.0, 0.0, 0.0, 0.0])[0], 0.0);
    }

    #[test]
    fn light_edits_do_not_change_structure() {
        let a = body(&[1.0, 5.0, 0.0, 0.0]);
        let b = body(&[1.0, 5.0, 0.0, 0.0, 40.0, 1.0, 1.0, 1.0]);
        assert_eq!(a, b);
    }
}
