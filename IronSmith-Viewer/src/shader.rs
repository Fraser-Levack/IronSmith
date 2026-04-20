use anyhow::Result;

pub fn create_pipeline(
    device: &wgpu::Device, 
    layout: &wgpu::PipelineLayout, 
    format: wgpu::TextureFormat, 
    code_input: Option<String>
) -> Result<wgpu::RenderPipeline> {
    
    // Updated fallback string to include the Hit struct so Rust can compile without Haskell
    let final_map_logic = code_input.unwrap_or_else(|| {
        "struct Hit { float d; vec3 col; int mat; };\nHit map(vec3 p) { return Hit(length(p) - (1.5 + sin(u_time*3.0)*0.2), vec3(0.8, 0.4, 0.1), 0); }".to_string()
    });

    let full_shader_source = format!(r#"
        #version 450
        layout(set = 0, binding = 0) uniform Uniforms {{
            vec2 u_resolution;
            float u_time;
            float u_camera_dist;
            vec2 u_rotation; 
            vec2 _padding; 
            vec4 u_target_pos; 
        }};
        layout(location = 0) out vec4 out_color;

        {code}

        vec3 calcNormal(vec3 p) {{
            vec2 e = vec2(0.001, 0.0);
            return normalize(vec3(
                map(p + e.xyy).d - map(p - e.xyy).d,
                map(p + e.yxy).d - map(p - e.yxy).d,
                map(p + e.yyx).d - map(p - e.yyx).d
            ));
        }}

        void main() {{
            vec2 uv = gl_FragCoord.xy / u_resolution.xy;
            vec2 st = (2.0 * uv - 1.0) * vec2(u_resolution.x / u_resolution.y, -1.0);
            
            float p_angle = u_rotation.x; 
            float y_angle = u_rotation.y;

            vec3 target_pos = u_target_pos.xyz;
            vec3 ro = target_pos + vec3(u_camera_dist * cos(p_angle) * sin(y_angle),
                                        u_camera_dist * sin(p_angle),
                                        u_camera_dist * cos(p_angle) * cos(y_angle));
            
            vec3 ww = normalize(target_pos - ro);
            vec3 uu = normalize(cross(ww, vec3(0.0, 1.0, 0.0)));
            vec3 vv = normalize(cross(uu, ww));
            vec3 rd = normalize(st.x * uu + st.y * vv + 1.5 * ww);

            float t = 0.0;
            vec3 material_col = vec3(0.0);
            int material_id = 0;
            bool hit = false; 

            // Primary Raymarch
            for(int i = 0; i < 256; i++) {{
                Hit res = map(ro + rd * t);
                if(res.d < 0.001) {{
                    material_col = res.col; 
                    material_id = res.mat;
                    hit = true; 
                    break;
                }}
                if(t > 100.0) break;
                t += res.d;
            }}

            vec3 bg_color = vec3(0.02, 0.02, 0.05);
            vec3 col = bg_color;
            vec3 light_dir = normalize(vec3(1.0, 2.0, 1.0));
            
            if(hit) {{
                vec3 pos = ro + rd * t;
                vec3 normal = calcNormal(pos);
                vec3 view_dir = normalize(ro - pos);
                
                if (material_id == 0) {{
                    // 0 = MATTE (Standard Diffuse)
                    float diff = max(dot(normal, light_dir), 0.0);
                    col = material_col * diff + material_col * 0.1; 
                }} 
                else if (material_id == 1) {{
                    // 1 = PLASTIC (Diffuse + Specular Highlight)
                    float diff = max(dot(normal, light_dir), 0.0);
                    vec3 half_dir = normalize(light_dir + view_dir);
                    float spec = pow(max(dot(normal, half_dir), 0.0), 64.0);
                    col = (material_col * diff) + (material_col * 0.1) + vec3(1.0) * spec;
                }} 
                else if (material_id == 2) {{
                    // 2 = NEON (Pure Emissive, Over-brightened)
                    col = material_col * 1.8; 
                }}
                else if (material_id == 3) {{
                    // 3 = METAL (Ray bounce reflection)
                    vec3 ref_rd = reflect(rd, normal);
                    vec3 ref_ro = pos + normal * 0.01; // Step slightly off surface
                    float ref_t = 0.0;
                    bool ref_hit = false;
                    vec3 ref_col = vec3(0.0);

                    // Secondary Raymarch for reflection
                    for(int i = 0; i < 100; i++) {{
                        Hit ref_res = map(ref_ro + ref_rd * ref_t);
                        if(ref_res.d < 0.001) {{
                            ref_col = ref_res.col;
                            ref_hit = true;
                            break;
                        }}
                        if(ref_t > 50.0) break;
                        ref_t += ref_res.d;
                    }}

                    // Apply reflection or sky background
                    if (ref_hit) {{
                        vec3 ref_pos = ref_ro + ref_rd * ref_t;
                        vec3 ref_normal = calcNormal(ref_pos);
                        float ref_diff = max(dot(ref_normal, light_dir), 0.0);
                        vec3 shaded_ref = ref_col * ref_diff + ref_col * 0.1;
                        col = mix(material_col * 0.2, shaded_ref, 0.8);
                    }} else {{
                        vec3 fake_sky = bg_color + max(ref_rd.y, 0.0) * 0.3;
                        col = mix(material_col * 0.2, fake_sky, 0.8);
                    }}

                    // Add sharp specular ping to metal
                    vec3 half_dir = normalize(light_dir + view_dir);
                    float spec = pow(max(dot(normal, half_dir), 0.0), 128.0);
                    col += vec3(1.0) * spec;
                }}
            }}
            
            // Gamma Correction
            out_color = vec4(pow(col, vec3(0.4545)), 1.0); 
        }}
    "#, code = final_map_logic);

    device.push_error_scope(wgpu::ErrorFilter::Validation);

    let vs_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(std::borrow::Cow::Borrowed("
            @vertex fn main(@builtin(vertex_index) i: u32) -> @builtin(position) vec4<f32> {
                var pos = array<vec2<f32>, 3>(vec2(-1.0, -1.0), vec2(3.0, -1.0), vec2(-1.0, 3.0));
                return vec4<f32>(pos[i], 0.0, 1.0);
            }
        ")),
    });

    let fs_module_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("IronSmith Fragment"),
            source: wgpu::ShaderSource::Glsl {
                shader: std::borrow::Cow::Owned(full_shader_source),
                stage: naga::ShaderStage::Fragment,
                defines: naga::FastHashMap::default(),
            },
        })
    }));

    let fs_module = match fs_module_result {
        Ok(module) => module,
        Err(_) => return Err(anyhow::anyhow!("wgpu encountered a fatal syntax panic in GLSL.")),
    };

    let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("Pipeline"),
        layout: Some(layout),
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

    let error_future = device.pop_error_scope();
    device.poll(wgpu::Maintain::Wait);
    
    if let Some(err) = pollster::block_on(error_future) {
        return Err(anyhow::anyhow!("wgpu Validation Error: {}", err));
    }

    Ok(pipeline)
}