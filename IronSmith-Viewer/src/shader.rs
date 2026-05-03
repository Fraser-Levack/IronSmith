use anyhow::Result;

pub fn create_pipeline(
    device: &wgpu::Device, 
    layout: &wgpu::PipelineLayout, 
    format: wgpu::TextureFormat
) -> Result<wgpu::RenderPipeline> {
    
    // NEW: Pull the GLSL code directly from the external file at compile time!
    let full_shader_source = include_str!("core.glsl");

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
                shader: std::borrow::Cow::Borrowed(full_shader_source),
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