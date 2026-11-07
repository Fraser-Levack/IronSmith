use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::mpsc; // Needed for Arc<wgpu::Device>

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

vec3 calcNormal(vec3 p) {
    // Tetrahedral normal sampled in a loop whose start the compiler can't
    // prove is 0, so it can't unroll the loop and inline the (potentially
    // huge) generated map() four times. Unrolled, a 250-draw scene took
    // the AMD Vulkan driver >2 minutes to compile; looped, ~2.6s.
    int ZERO = min(int(u_time), 0);
    vec3 n = vec3(0.0);
    for (int i = ZERO; i < 4; i++) {
        vec3 e = 0.5773 * (2.0 * vec3(float(((i + 3) >> 1) & 1), float((i >> 1) & 1), float(i & 1)) - 1.0);
        n += e * map(p + 0.0005 * e).d;
    }
    return normalize(n);
}

float calcShadow(vec3 ro, vec3 rd, float mint, float maxt, float k) {
    float res = 1.0;
    float t = mint;
    
    for(int i = 0; i < 30; i++) {
        float h = map(ro + rd * t).d;
        if(h < 0.001) return 0.1; 
        res = min(res, k * h / t);
        t += h;
        if(t > maxt) break;
    }
    return clamp(res, 0.1, 1.0);
}

void main() {
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    vec2 st = (2.0 * uv - 1.0) * vec2(u_resolution.x / u_resolution.y, -1.0);
    vec3 ro = u_target_pos.xyz + vec3(u_camera_dist * cos(u_rotation.x) * sin(u_rotation.y), u_camera_dist * sin(u_rotation.x), u_camera_dist * cos(u_rotation.x) * cos(u_rotation.y));
    vec3 ww = normalize(u_target_pos.xyz - ro);
    vec3 uu = normalize(cross(ww, vec3(0.0, 1.0, 0.0)));
    vec3 vv = normalize(cross(uu, ww));
    vec3 rd = normalize(st.x * uu + st.y * vv + 1.5 * ww);

    float t = 0.0; vec3 material_col = vec3(0.0); int material_id = 0; bool hit = false; 

    float scene_radius = max(35.0, u_camera_dist * 1.5); 
    vec3 oc = ro - vec3(0.0);
    float b = dot(oc, rd);
    float h = b * b - (dot(oc, oc) - scene_radius * scene_radius);

    if (h >= 0.0) {
        float t_out = -b + sqrt(h); 
        t = max(0.0, -b - sqrt(h));

        int _max_steps = int(u_march_steps);
        for(int i = 0; i < _max_steps; i++) {
            if(t > t_out) break; 
            vec3 p = ro + rd * t; Hit res = map(p);
            if(res.d < max(0.001, 0.0002 * t)) { material_col = res.col; material_id = res.mat; hit = true; break; }
            t += res.d; 
        }
    }

    vec3 bg_color = u_bg_color.rgb; vec3 col = bg_color;
    {LIGHT_SETUP}

    if(hit) {
        vec3 pos = ro + rd * t; vec3 normal = calcNormal(pos); vec3 view_dir = normalize(ro - pos);

        float shadow = (u_shadow_enabled > 0.5 ? calcShadow(pos, light_dir, 0.05, 20.0, 12.0) : 1.0) * light_intensity;

        if (material_id == 0) { 
            col = material_col * max(dot(normal, light_dir), 0.0) * shadow + material_col * 0.1; 
        } 
        else if (material_id == 1) { 
            vec3 half_dir = normalize(light_dir + view_dir); 
            float diff = max(dot(normal, light_dir), 0.0) * shadow;
            float spec = pow(max(dot(normal, half_dir), 0.0), 64.0) * shadow;
            col = (material_col * diff) + (material_col * 0.1) + vec3(1.0) * spec; 
        } 
        else if (material_id == 2) { 
            col = material_col * 1.8; 
        }
        else if (material_id == 3) {
            vec3 ref_rd = reflect(rd, normal); vec3 ref_ro = pos + normal * 0.01; 
            float ref_t = 0.0; bool ref_hit = false; vec3 ref_col = vec3(0.0);
            for(int i = 0; i < 30; i++) { 
                Hit ref_res = map(ref_ro + ref_rd * ref_t);
                if(ref_res.d < max(0.005, 0.001 * ref_t)) { ref_col = ref_res.col; ref_hit = true; break; }
                if(ref_t > 20.0) break; ref_t += ref_res.d;
            }
            vec3 shaded_ref = ref_hit ? (ref_col * max(dot(calcNormal(ref_ro + ref_rd * ref_t), light_dir), 0.0) + ref_col * 0.1) : (bg_color + max(ref_rd.y, 0.0) * 0.3);
            
            float spec = pow(max(dot(normal, normalize(light_dir + view_dir)), 0.0), 128.0) * shadow;
            col = mix(material_col * 0.2 * shadow, shaded_ref, 0.8) + vec3(1.0) * spec;
        }
    }
    out_color = vec4(pow(col * u_exposure.x, vec3(0.4545)), 1.0);
}
"#;

/// The camera-relative light used when the script has no light() statement.
const DEFAULT_LIGHT_SETUP: &str =
    "vec3 light_dir = normalize(-ww + uu * 0.4 + vv * 0.6); float light_intensity = 1.0;";

/// Walk the bytecode looking for lighting opcodes (40 = light direction,
/// 41 = light intensity) and produce the GLSL that sets up `light_dir` and
/// `light_intensity` in main(). Uses the same instruction strides as
/// `generate_shader_body` so argument floats are never misread as opcodes.
pub fn extract_light_setup(bytecode: &[f32]) -> String {
    let mut dir: Option<[f32; 3]> = None;
    let mut intensity: Option<f32> = None;
    let mut pc = 0;

    while pc < bytecode.len() {
        match bytecode[pc] as i32 {
            0 => break,
            40 => {
                let d = [bytecode[pc + 1], bytecode[pc + 2], bytecode[pc + 3]];
                if d.iter().any(|v| *v != 0.0) {
                    dir = Some(d);
                }
                pc += 4;
            }
            41 => {
                intensity = Some(bytecode[pc + 1].max(0.0));
                pc += 4;
            }
            2 | 4 | 23 => pc += 8, // two-vec4 instructions (BOX, CONE, SCALE)
            _ => pc += 4,
        }
    }

    match dir {
        None => DEFAULT_LIGHT_SETUP.to_string(),
        Some([x, y, z]) => format!(
            "vec3 light_dir = normalize(vec3({:.5}, {:.5}, {:.5})); float light_intensity = {:.5};",
            x,
            y,
            z,
            intensity.unwrap_or(1.0)
        ),
    }
}

/// Translates the bytecode emitted by the Haskell compiler into the body of
/// the GLSL `map()` function, walking the same opcode stack machine that the
/// CPU-side OBJ exporter and the GPU raymarcher both implement.
///
/// Returns Err for malformed bytecode (e.g. a CSG op with fewer than two
/// shapes on the stack, which happens mid-edit when a script references a
/// shape that doesn't exist yet). The caller keeps the last working shader.
pub fn generate_shader_body(bytecode: &[f32]) -> Result<String, String> {
    let mut body = String::new();
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

        match iop {
            0 => break, // OP_HALT
            1 => {
                // SPHERE
                body.push_str(&format!(
                    "s{} = Hit(sdSphere(current_p, {:.5}) * current_scale, current_col, {});\n",
                    sp,
                    bytecode[pc + 1],
                    bytecode[pc + 2] as i32
                ));
                sp += 1;
                pc += 4;
            }
            2 => {
                // BOX
                body.push_str(&format!("s{} = Hit(sdBox(current_p, vec3({:.5}, {:.5}, {:.5})) * current_scale, current_col, {});\n", sp, bytecode[pc+1], bytecode[pc+2], bytecode[pc+3], bytecode[pc+4] as i32));
                sp += 1;
                pc += 8;
            }
            3 => {
                // CYLINDER
                body.push_str(&format!("s{} = Hit(sdCylinder(current_p, {:.5}, {:.5}) * current_scale, current_col, {});\n", sp, bytecode[pc+2], bytecode[pc+1], bytecode[pc+3] as i32));
                sp += 1;
                pc += 4;
            }
            4 => {
                // CONE
                body.push_str(&format!("s{} = Hit(sdCappedCone(current_p, {:.5}, {:.5}, {:.5}) * current_scale, current_col, {});\n", sp, bytecode[pc+3], bytecode[pc+1], bytecode[pc+2], bytecode[pc+4] as i32));
                sp += 1;
                pc += 8;
            }
            5 => {
                // TORUS
                body.push_str(&format!("s{} = Hit(sdTorus(current_p, vec2({:.5}, {:.5})) * current_scale, current_col, {});\n", sp, bytecode[pc+1], bytecode[pc+2], bytecode[pc+3] as i32));
                sp += 1;
                pc += 4;
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
                pc += 4;
            }
            20 => {
                // ROTATE_X
                body.push_str(&format!("current_p = mat3(1.0, 0.0, 0.0, 0.0, {:.5}, {:.5}, 0.0, {:.5}, {:.5}) * current_p;\n", bytecode[pc+1], bytecode[pc+2], -bytecode[pc+2], bytecode[pc+1]));
                pc += 4;
            }
            21 => {
                // ROTATE_Y
                body.push_str(&format!("current_p = mat3({:.5}, 0.0, {:.5}, 0.0, 1.0, 0.0, {:.5}, 0.0, {:.5}) * current_p;\n", bytecode[pc+1], -bytecode[pc+2], bytecode[pc+2], bytecode[pc+1]));
                pc += 4;
            }
            22 => {
                // ROTATE_Z
                body.push_str(&format!("current_p = mat3({:.5}, {:.5}, 0.0, {:.5}, {:.5}, 0.0, 0.0, 0.0, 1.0) * current_p;\n", bytecode[pc+1], bytecode[pc+2], -bytecode[pc+2], bytecode[pc+1]));
                pc += 4;
            }
            23 => {
                // SCALE
                body.push_str(&format!(
                    "current_scale *= {:.5};\ncurrent_p *= vec3({:.5}, {:.5}, {:.5});\n",
                    bytecode[pc + 4],
                    bytecode[pc + 1],
                    bytecode[pc + 2],
                    bytecode[pc + 3]
                ));
                pc += 8;
            }
            24 => {
                // MOVE
                body.push_str(&format!(
                    "current_p -= vec3({:.5}, {:.5}, {:.5});\n",
                    bytecode[pc + 1],
                    bytecode[pc + 2],
                    bytecode[pc + 3]
                ));
                pc += 4;
            }
            26 => {
                // REPEAT
                body.push_str(&format!(
                    "current_p = opRep(current_p, vec3({:.5}, {:.5}, {:.5}));\n",
                    bytecode[pc + 1],
                    bytecode[pc + 2],
                    bytecode[pc + 3]
                ));
                pc += 4;
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
                pc += 4;
            }
            30 => {
                // PUSH_COLOR
                body.push_str(&format!(
                    "c{} = current_col; current_col = vec3({:.5}, {:.5}, {:.5});\n",
                    csp,
                    bytecode[pc + 1],
                    bytecode[pc + 2],
                    bytecode[pc + 3]
                ));
                csp += 1;
                pc += 4;
            }
            31 => {
                // POP_COLOR
                if csp < 1 {
                    return Err("color pop with no color pushed".to_string());
                }
                csp -= 1;
                body.push_str(&format!("current_col = c{};\n", csp));
                pc += 4;
            }
            _ => {
                pc += 4;
            }
        }

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

    Ok(decls)
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

/// Build the complete scene fragment shader for a bytecode program.
pub fn build_scene_source(bytecode: &[f32]) -> Result<String, String> {
    let body = generate_shader_body(bytecode)?;
    let light_setup = extract_light_setup(bytecode);
    Ok(SHADER_TEMPLATE
        .replace("{GENERATED_BODY}", &body)
        .replace("{LIGHT_SETUP}", &light_setup))
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

/// The outcome of compiling a scene. `generation` is the newest scene
/// request this result covers (older queued requests it superseded are
/// covered too), so callers waiting on generation N can treat any result
/// with generation >= N as "your scene is done".
pub struct SceneResult {
    pub generation: u64,
    pub pipeline: Result<wgpu::RenderPipeline, String>,
}

/// Start the background scene compiler and return the queue that feeds it.
/// Jobs are `(generation, bytecode)`, with generations increasing.
///
/// A single long-lived worker compiles scenes one at a time. Before each
/// compile it drains the queue and keeps only the newest bytecode, so edits
/// made while a slow driver compile is running collapse into one follow-up
/// compile instead of piling up as parallel threads. Because compiles are
/// sequential, results arrive on `tx` in edit order: an older, slower
/// compile can never replace a newer scene.
pub fn spawn_scene_compiler(
    device: Arc<wgpu::Device>,
    layout: Arc<wgpu::PipelineLayout>,
    format: wgpu::TextureFormat,
    tx: mpsc::Sender<SceneResult>,
    log_path: PathBuf,
) -> mpsc::Sender<(u64, Vec<f32>)> {
    let (job_tx, job_rx) = mpsc::channel::<(u64, Vec<f32>)>();

    std::thread::spawn(move || {
        while let Ok(mut job) = job_rx.recv() {
            while let Ok(newer) = job_rx.try_recv() {
                job = newer;
            }
            let (generation, bytecode) = job;

            let pipeline = build_scene_source(&bytecode)
                .map_err(|e| format!("Scene rejected (malformed bytecode): {}", e))
                .and_then(|source| {
                    build_pipeline_checked(&device, &layout, format, "IronSmith Scene", &source)
                        .map_err(|e| format!("Scene shader rejected: {}", e))
                });
            if let Err(e) = &pipeline {
                log_line(&log_path, e);
            }

            if tx
                .send(SceneResult {
                    generation,
                    pipeline,
                })
                .is_err()
            {
                break; // renderer is gone
            }
        }
    });

    job_tx
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_bytecode_returns_a_miss() {
        let body = generate_shader_body(&[]).unwrap();
        assert!(body.contains("return Hit(999999.0, vec3(0.0), 0);"));
        assert!(!body.contains("return s0;"));
    }

    #[test]
    fn halt_opcode_stops_generation() {
        let body = generate_shader_body(&[0.0, 0.0, 0.0, 0.0, 1.0, 5.0, 0.0, 0.0]).unwrap();
        assert!(!body.contains("sdSphere"));
    }

    #[test]
    fn sphere_opcode_emits_sdsphere_call() {
        let bytecode = [1.0, 5.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0];
        let body = generate_shader_body(&bytecode).unwrap();
        assert!(
            body.contains(
                "s0 = Hit(sdSphere(current_p, 5.00000) * current_scale, current_col, 1);"
            )
        );
        assert!(body.contains("return s0;"));
    }

    #[test]
    fn union_opcode_combines_two_shapes() {
        // sphere, sphere, union, halt
        let bytecode = [
            1.0, 1.0, 0.0, 0.0, 1.0, 2.0, 0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ];
        let body = generate_shader_body(&bytecode).unwrap();
        assert!(body.contains("s0 = opU(s0, s1);"));
        assert!(body.contains("return s0;"));
    }

    #[test]
    fn rotate_and_pop_transform_restore_state() {
        // rotateX(c=1,s=0), sphere, pop-transform, halt
        let bytecode = [
            20.0, 1.0, 0.0, 0.0, 1.0, 5.0, 0.0, 0.0, 25.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ];
        let body = generate_shader_body(&bytecode).unwrap();
        assert!(body.contains("p0 = current_p; sc0 = current_scale;"));
        assert!(body.contains("current_p = mat3(1.0, 0.0, 0.0, 0.0, 1.00000, 0.00000, 0.0, -0.00000, 1.00000) * current_p;"));
        assert!(body.contains("current_p = p0; current_scale = sc0;"));
    }

    #[test]
    fn csg_with_missing_operand_errors_instead_of_panicking() {
        // One sphere then a difference: mid-edit state where the second
        // operand was an unresolved shape reference. Used to underflow
        // and abort the release viewer.
        let bytecode = [1.0, 5.0, 0.0, 0.0, 11.0, 0.0, 0.0, 0.0];
        assert!(generate_shader_body(&bytecode).is_err());
    }

    #[test]
    fn unbalanced_pops_error_instead_of_panicking() {
        assert!(generate_shader_body(&[25.0, 0.0, 0.0, 0.0]).is_err());
        assert!(generate_shader_body(&[31.0, 0.0, 0.0, 0.0]).is_err());
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
        // Covers the template itself (including the looped calcNormal).
        let source = build_scene_source(&[1.0, 5.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap();
        parse_glsl_fragment(&source).unwrap();
    }

    #[test]
    fn deep_hit_stack_declares_every_register_it_uses() {
        // Peaks at 20 live shapes. The old fixed s0..s15 register file
        // only checked the *final* depth, so this produced GLSL using an
        // undeclared `s16` and the scene was silently rejected.
        let source = build_scene_source(&stacked_spheres(20)).unwrap();
        assert!(source.contains("s19"));
        parse_glsl_fragment(&source).unwrap();
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
        let source = build_scene_source(&bc).unwrap();
        parse_glsl_fragment(&source).unwrap();
    }

    #[test]
    fn absurdly_deep_stack_is_rejected() {
        assert!(generate_shader_body(&stacked_spheres(MAX_REGISTERS as usize + 1)).is_err());
    }

    #[test]
    fn truncated_instruction_errors_instead_of_panicking() {
        // A box needs 8 floats; only 4 arrived.
        assert!(generate_shader_body(&[2.0, 1.0, 1.0, 1.0]).is_err());
    }

    #[test]
    fn light_opcode_overrides_default_light() {
        // sphere, light(10, 20, 5), intensity 2, halt
        let bytecode = [
            1.0, 5.0, 0.0, 0.0, 40.0, 10.0, 20.0, 5.0, 41.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ];
        let setup = extract_light_setup(&bytecode);
        assert!(setup.contains("normalize(vec3(10.00000, 20.00000, 5.00000))"));
        assert!(setup.contains("light_intensity = 2.00000"));
    }

    #[test]
    fn no_light_opcode_keeps_default_light() {
        let bytecode = [1.0, 5.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0];
        assert_eq!(extract_light_setup(&bytecode), DEFAULT_LIGHT_SETUP);
    }

    #[test]
    fn light_argument_floats_are_not_misread_as_opcodes() {
        // A box with a 40.0 argument must not be treated as a light op.
        let bytecode = [2.0, 40.0, 40.0, 40.0, 0.0, 0.0, 0.0, 0.0];
        assert_eq!(extract_light_setup(&bytecode), DEFAULT_LIGHT_SETUP);
    }

    #[test]
    fn zero_direction_light_falls_back_to_default() {
        let bytecode = [40.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0];
        assert_eq!(extract_light_setup(&bytecode), DEFAULT_LIGHT_SETUP);
    }
}
