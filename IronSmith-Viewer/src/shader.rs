use std::sync::mpsc;
use std::sync::Arc; // Needed for Arc<wgpu::Device>

const SHADER_TEMPLATE: &str = r#"
#version 450
layout(set = 0, binding = 0) uniform Uniforms {
    vec2 u_resolution;
    float u_time;
    float u_camera_dist;
    vec2 u_rotation; 
    vec2 _padding; 
    vec4 u_target_pos; 
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
    Hit s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15;
    vec3 p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15;
    float sc0, sc1, sc2, sc3, sc4, sc5, sc6, sc7, sc8, sc9, sc10, sc11, sc12, sc13, sc14, sc15;
    vec3 c0, c1, c2, c3, c4, c5, c6, c7;
    
    vec3 current_p = p; float current_scale = 1.0; vec3 current_col = vec3(0.8, 0.4, 0.1);

    {GENERATED_BODY}
}

vec3 calcNormal(vec3 p) {
    vec2 e = vec2(1.0, -1.0) * 0.0005;
    return normalize(e.xyy * map(p + e.xyy).d + e.yyx * map(p + e.yyx).d + e.yxy * map(p + e.yxy).d + e.xxx * map(p + e.xxx).d);
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

        for(int i = 0; i < 150; i++) { 
            if(t > t_out) break; 
            vec3 p = ro + rd * t; Hit res = map(p);
            // Dynamic precision based on distance to fix grazing edge starvation
            if(res.d < max(0.001, 0.0002 * t)) { material_col = res.col; material_id = res.mat; hit = true; break; }
            t += res.d; 
        }
    }

    vec3 bg_color = vec3(0.02, 0.02, 0.05); vec3 col = bg_color;
    vec3 light_dir = normalize(vec3(1.0, 2.0, 1.0));
    
    if(hit) {
        vec3 pos = ro + rd * t; vec3 normal = calcNormal(pos); vec3 view_dir = normalize(ro - pos);
        
        if (material_id == 0) { col = material_col * max(dot(normal, light_dir), 0.0) + material_col * 0.1; } 
        else if (material_id == 1) { vec3 half_dir = normalize(light_dir + view_dir); col = (material_col * max(dot(normal, light_dir), 0.0)) + (material_col * 0.1) + vec3(1.0) * pow(max(dot(normal, half_dir), 0.0), 64.0); } 
        else if (material_id == 2) { col = material_col * 1.8; }
        else if (material_id == 3) {
            vec3 ref_rd = reflect(rd, normal); vec3 ref_ro = pos + normal * 0.01; 
            float ref_t = 0.0; bool ref_hit = false; vec3 ref_col = vec3(0.0);
            for(int i = 0; i < 30; i++) { 
                Hit ref_res = map(ref_ro + ref_rd * ref_t);
                // Dynamic precision for reflections too
                if(ref_res.d < max(0.005, 0.001 * ref_t)) { ref_col = ref_res.col; ref_hit = true; break; }
                if(ref_t > 20.0) break; ref_t += ref_res.d;
            }
            vec3 shaded_ref = ref_hit ? (ref_col * max(dot(calcNormal(ref_ro + ref_rd * ref_t), light_dir), 0.0) + ref_col * 0.1) : (bg_color + max(ref_rd.y, 0.0) * 0.3);
            col = mix(material_col * 0.2, shaded_ref, 0.8) + vec3(1.0) * pow(max(dot(normal, normalize(light_dir + view_dir)), 0.0), 128.0);
        }
    }
    out_color = vec4(pow(col, vec3(0.4545)), 1.0); 
}
"#;

pub fn compile_pipeline_async(
    device: Arc<wgpu::Device>,          // Changed to Arc
    layout: Arc<wgpu::PipelineLayout>,  // Changed to Arc
    format: wgpu::TextureFormat,
    bytecode: Vec<f32>,
    tx: mpsc::Sender<wgpu::RenderPipeline>,
) {
    std::thread::spawn(move || {
        let mut body = String::new();
        let mut pc = 0;
        
        let mut sp = 0;
        let mut tsp = 0;
        let mut csp = 0;

        while pc < bytecode.len() {
            let iop = bytecode[pc] as i32;
            
            if iop >= 20 && iop <= 26 && iop != 25 {
                body.push_str(&format!("p{} = current_p; sc{} = current_scale;\n", tsp, tsp));
                tsp += 1;
            }

            match iop {
                0 => break, // OP_HALT
                1 => { // SPHERE
                    body.push_str(&format!("s{} = Hit(sdSphere(current_p, {:.5}) * current_scale, current_col, {});\n", sp, bytecode[pc+1], bytecode[pc+2] as i32));
                    sp += 1;
                    pc += 4;
                }
                2 => { // BOX
                    body.push_str(&format!("s{} = Hit(sdBox(current_p, vec3({:.5}, {:.5}, {:.5})) * current_scale, current_col, {});\n", sp, bytecode[pc+1], bytecode[pc+2], bytecode[pc+3], bytecode[pc+4] as i32));
                    sp += 1;
                    pc += 8;
                }
                3 => { // CYLINDER
                    body.push_str(&format!("s{} = Hit(sdCylinder(current_p, {:.5}, {:.5}) * current_scale, current_col, {});\n", sp, bytecode[pc+2], bytecode[pc+1], bytecode[pc+3] as i32));
                    sp += 1;
                    pc += 4;
                }
                4 => { // CONE
                    body.push_str(&format!("s{} = Hit(sdCappedCone(current_p, {:.5}, {:.5}, {:.5}) * current_scale, current_col, {});\n", sp, bytecode[pc+3], bytecode[pc+1], bytecode[pc+2], bytecode[pc+4] as i32));
                    sp += 1;
                    pc += 8;
                }
                5 => { // TORUS
                    body.push_str(&format!("s{} = Hit(sdTorus(current_p, vec2({:.5}, {:.5})) * current_scale, current_col, {});\n", sp, bytecode[pc+1], bytecode[pc+2], bytecode[pc+3] as i32));
                    sp += 1;
                    pc += 4;
                }
                10 => { // UNION
                    sp -= 1; let b = sp;
                    sp -= 1; let a = sp;
                    body.push_str(&format!("s{} = opU(s{}, s{});\n", a, a, b));
                    sp += 1;
                    pc += 4;
                }
                11 => { // DIFF
                    sp -= 1; let b = sp;
                    sp -= 1; let a = sp;
                    body.push_str(&format!("s{} = opS(s{}, s{});\n", a, a, b));
                    sp += 1;
                    pc += 4;
                }
                12 => { // INTERSECT
                    sp -= 1; let b = sp;
                    sp -= 1; let a = sp;
                    body.push_str(&format!("s{} = opI(s{}, s{});\n", a, a, b));
                    sp += 1;
                    pc += 4;
                }
                20 => { // ROTATE_X
                    body.push_str(&format!("current_p = mat3(1.0, 0.0, 0.0, 0.0, {:.5}, {:.5}, 0.0, {:.5}, {:.5}) * current_p;\n", bytecode[pc+1], bytecode[pc+2], -bytecode[pc+2], bytecode[pc+1]));
                    pc += 4;
                }
                21 => { // ROTATE_Y
                    body.push_str(&format!("current_p = mat3({:.5}, 0.0, {:.5}, 0.0, 1.0, 0.0, {:.5}, 0.0, {:.5}) * current_p;\n", bytecode[pc+1], -bytecode[pc+2], bytecode[pc+2], bytecode[pc+1]));
                    pc += 4;
                }
                22 => { // ROTATE_Z
                    body.push_str(&format!("current_p = mat3({:.5}, {:.5}, 0.0, {:.5}, {:.5}, 0.0, 0.0, 0.0, 1.0) * current_p;\n", bytecode[pc+1], bytecode[pc+2], -bytecode[pc+2], bytecode[pc+1]));
                    pc += 4;
                }
                23 => { // SCALE
                    body.push_str(&format!("current_scale *= {:.5};\ncurrent_p *= vec3({:.5}, {:.5}, {:.5});\n", bytecode[pc+4], bytecode[pc+1], bytecode[pc+2], bytecode[pc+3]));
                    pc += 8;
                }
                24 => { // MOVE
                    body.push_str(&format!("current_p -= vec3({:.5}, {:.5}, {:.5});\n", bytecode[pc+1], bytecode[pc+2], bytecode[pc+3]));
                    pc += 4;
                }
                26 => { // REPEAT
                    body.push_str(&format!("current_p = opRep(current_p, vec3({:.5}, {:.5}, {:.5}));\n", bytecode[pc+1], bytecode[pc+2], bytecode[pc+3]));
                    pc += 4;
                }
                25 => { // POP_TRANSFORM
                    tsp -= 1;
                    body.push_str(&format!("current_p = p{}; current_scale = sc{};\n", tsp, tsp));
                    pc += 4;
                }
                30 => { // PUSH_COLOR
                    body.push_str(&format!("c{} = current_col; current_col = vec3({:.5}, {:.5}, {:.5});\n", csp, bytecode[pc+1], bytecode[pc+2], bytecode[pc+3]));
                    csp += 1;
                    pc += 4;
                }
                31 => { // POP_COLOR
                    csp -= 1;
                    body.push_str(&format!("current_col = c{};\n", csp));
                    pc += 4;
                }
                _ => { pc += 4; }
            }
        }
        
        if sp > 0 {
            body.push_str("    return s0;\n");
        } else {
            body.push_str("    return Hit(999999.0, vec3(0.0), 0);\n");
        }

        let full_source = SHADER_TEMPLATE.replace("{GENERATED_BODY}", &body);

        let fs_module_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("IronSmith Async Fragment"),
                source: wgpu::ShaderSource::Glsl {
                    shader: std::borrow::Cow::Borrowed(&full_source),
                    stage: naga::ShaderStage::Fragment,
                    defines: naga::FastHashMap::default(),
                },
            })
        }));

        if let Ok(fs_module) = fs_module_result {
            let vs_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: None,
                source: wgpu::ShaderSource::Wgsl(std::borrow::Cow::Borrowed("
                    @vertex fn main(@builtin(vertex_index) i: u32) -> @builtin(position) vec4<f32> {
                        var pos = array<vec2<f32>, 3>(vec2(-1.0, -1.0), vec2(3.0, -1.0), vec2(-1.0, 3.0));
                        return vec4<f32>(pos[i], 0.0, 1.0);
                    }
                ")),
            });

            let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Async Pipeline"),
                layout: Some(&layout),
                vertex: wgpu::VertexState { module: &vs_module, entry_point: "main", buffers: &[] },
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

            let _ = tx.send(pipeline);
        }
    });
}