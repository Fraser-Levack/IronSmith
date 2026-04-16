use anyhow::{Context, Result};
use std::sync::Arc;
use std::time::Instant;
use std::path::PathBuf;
use wgpu::util::DeviceExt;
use winit::window::Window;
use std::io::Write; 

// --- 1. MEMORY LAYOUT ---
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct ShaderUniforms {
    pub resolution: [f32; 2],
    pub time: f32,
    pub camera_dist: f32,
    pub rotation: [f32; 2], 
    pub padding: [f32; 2],
    pub target_pos: [f32; 4], // NEW: 16-byte aligned array for vec4 panning
}

pub struct Renderer<'a> {
    surface: wgpu::Surface<'a>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    pub config: wgpu::SurfaceConfiguration,
    pub size: winit::dpi::PhysicalSize<u32>,
    render_pipeline: wgpu::RenderPipeline,
    pipeline_layout: wgpu::PipelineLayout, 
    uniform_buffer: wgpu::Buffer,
    bind_group: wgpu::BindGroup,
    pub uniforms: ShaderUniforms,
    start_time: Instant,
    log_path: PathBuf, 
}

impl<'a> Renderer<'a> {
    pub async fn new(window: Arc<Window>, initial_glsl: String, log_path: PathBuf) -> Result<Self> {
        let size = window.inner_size();
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });
        
        let surface = instance.create_surface(window.clone())?;
        let adapter = instance.request_adapter(&wgpu::RequestAdapterOptions {
            compatible_surface: Some(&surface),
            ..Default::default()
        }).await.context("Failed to find wgpu adapter")?;

        let (device, queue) = adapter.request_device(&wgpu::DeviceDescriptor::default(), None).await?;
        let surface_caps = surface.get_capabilities(&adapter);
        let surface_format = surface_caps.formats[0];
        
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        let uniforms = ShaderUniforms {
            resolution: [size.width as f32, size.height as f32],
            time: 0.0,
            camera_dist: 20.0,
            rotation: [0.4, 0.0],
            padding: [0.0, 0.0],
            target_pos: [0.0, 0.0, 0.0, 0.0], // Initialized at origin
        };

        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Uniform Buffer"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            }],
            label: None,
        });

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: uniform_buffer.as_entire_binding(),
            }],
            label: None,
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        let render_pipeline = match Self::create_pipeline_internal(&device, &pipeline_layout, config.format, Some(initial_glsl)) {
            Ok(pipeline) => pipeline,
            Err(_) => {
                Self::create_pipeline_internal(&device, &pipeline_layout, config.format, None)?
            }
        };

        Ok(Self {
            surface, device, queue, config, size,
            render_pipeline, pipeline_layout, uniform_buffer,
            bind_group, uniforms, start_time: Instant::now(), log_path,
        })
    }

    pub fn log_message(&self, msg: &str) {
        if let Ok(mut file) = std::fs::OpenOptions::new().create(true).append(true).open(&self.log_path) {
            let _ = writeln!(file, "{}", msg);
        }
    }

    // --- 2. GLSL SHADER GENERATION ---
    fn create_pipeline_internal(
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
                vec4 u_target_pos; // NEW: Receives the pan coordinates
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

                // NEW: Calculate camera origin relative to the new target position
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

    pub fn reload_shader(&mut self, haskell_code: &str) -> Result<()> {
        match Self::create_pipeline_internal(&self.device, &self.pipeline_layout, self.config.format, Some(haskell_code.to_string())) {
            Ok(new_pipeline) => {
                self.render_pipeline = new_pipeline;
                Ok(())
            }
            Err(e) => Err(e)
        }
    }

    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
            self.size = new_size;
        }
    }

    // --- 3. UNIFORM UPDATE ---
    pub fn update(&mut self, yaw: f32, pitch: f32, dist: f32, pan_x: f32, pan_y: f32, pan_z: f32) {
        self.uniforms.time = self.start_time.elapsed().as_secs_f32();
        self.uniforms.resolution = [self.size.width as f32, self.size.height as f32];
        self.uniforms.camera_dist = dist;
        self.uniforms.rotation = [pitch, yaw];
        self.uniforms.target_pos = [pan_x, pan_y, pan_z, 0.0]; // Update the target pos on the GPU
        self.queue.write_buffer(&self.uniform_buffer, 0, bytemuck::cast_slice(&[self.uniforms]));
    }

    pub fn render(&mut self) -> Result<(), wgpu::SurfaceError> {
        let output = self.surface.get_current_texture()?;
        let view = output.texture.create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
        {
            let mut rp = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view, resolve_target: None,
                    ops: wgpu::Operations { load: wgpu::LoadOp::Clear(wgpu::Color::BLACK), store: wgpu::StoreOp::Store },
                })],
                depth_stencil_attachment: None, occlusion_query_set: None, timestamp_writes: None,
            });
            rp.set_pipeline(&self.render_pipeline);
            rp.set_bind_group(0, &self.bind_group, &[]);
            rp.draw(0..3, 0..1);
        }
        self.queue.submit(std::iter::once(encoder.finish()));
        output.present();
        Ok(())
    }
}