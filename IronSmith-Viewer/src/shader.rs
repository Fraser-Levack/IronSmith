use anyhow::Result;

pub fn create_pipeline(
    device: &wgpu::Device, 
    layout: &wgpu::PipelineLayout, 
    format: wgpu::TextureFormat, 
    code_input: Option<String>
) -> Result<wgpu::RenderPipeline> {
    
    let final_map_logic = code_input.unwrap_or_else(|| {
        "float map(vec3 p) { return length(p) - (1.5 + sin(u_time*3.0)*0.2); }".to_string()
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
                map(p + e.xyy) - map(p - e.xyy),
                map(p + e.yxy) - map(p - e.yxy),
                map(p + e.yyx) - map(p - e.yyx)
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
            for(int i = 0; i < 100; i++) {{
                float d = map(ro + rd * t);
                if(d < 0.001 || t > 100.0) break;
                t += d;
            }}

            vec3 col = vec3(0.02, 0.02, 0.05);
            if(t < 100.0) {{
                vec3 pos = ro + rd * t;
                vec3 normal = calcNormal(pos);
                float diff = max(dot(normal, normalize(vec3(1.0, 2.0, 1.0))), 0.0);
                col = vec3(0.8, 0.4, 0.1) * diff + vec3(0.1);
            }}
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