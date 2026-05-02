use anyhow::{Context, Result};
use std::sync::Arc;
use std::time::Instant;
use std::path::PathBuf;
use wgpu::util::DeviceExt;
use winit::window::Window;
use std::io::Write; 

use crate::camera::Camera;
use crate::shader;

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct ShaderUniforms {
    pub resolution: [f32; 2],
    pub time: f32,
    pub camera_dist: f32,
    pub rotation: [f32; 2], 
    pub padding: [f32; 2],
    pub target_pos: [f32; 4],
}

pub struct Renderer<'a> {
    surface: wgpu::Surface<'a>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    pub config: wgpu::SurfaceConfiguration,
    pub size: winit::dpi::PhysicalSize<u32>,
    render_pipeline: wgpu::RenderPipeline,
    uniform_buffer: wgpu::Buffer,
    scene_buffer: wgpu::Buffer, // NEW: Our SSBO buffer!
    bind_group: wgpu::BindGroup,
    pub uniforms: ShaderUniforms,
    start_time: Instant,
    log_path: PathBuf, 
}

impl<'a> Renderer<'a> {
    pub async fn new(window: Arc<Window>, log_path: PathBuf) -> Result<Self> {
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
            target_pos: [0.0, 0.0, 0.0, 0.0], 
        };

        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Uniform Buffer"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        // NEW: HARDCODED BYTECODE SCENE
        let scene_instructions: &[f32] = &[
            1.0, -1.5, 0.0, 0.0, 2.0, 0.0,      // OP_SPHERE, x, y, z, r, mat
            2.0,  1.5, 0.0, 0.0, 1.5, 1.5, 1.5, 1.0, // OP_BOX, x, y, z, w, h, d, mat
            10.0,                               // OP_UNION
            0.0                                 // OP_HALT
        ];

        // FIX: Create a massive 64KB buffer (holds ~16,000 float instructions)
        let scene_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Scene SSBO"),
            size: 65536, // 64 KB
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        // Write a single OP_HALT (0.0) float to it immediately so the GPU doesn't crash 
        // trying to read empty garbage memory before Haskell connects!
        queue.write_buffer(&scene_buffer, 0, bytemuck::cast_slice(&[0.0f32]));

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                // NEW: Add the Storage Buffer to the Layout
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Storage { read_only: true },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                }
            ],
            label: None,
        });

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: uniform_buffer.as_entire_binding(),
                },
                // NEW: Bind the Scene SSBO
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: scene_buffer.as_entire_binding(),
                }
            ],
            label: None,
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        // Create the pipeline using the static shader
        let render_pipeline = shader::create_pipeline(&device, &pipeline_layout, config.format)?;

        Ok(Self {
            surface, device, queue, config, size,
            render_pipeline, uniform_buffer, scene_buffer,
            bind_group, uniforms, start_time: Instant::now(), log_path,
        })
    }

    pub fn log_message(&self, msg: &str) {
        if let Ok(mut file) = std::fs::OpenOptions::new().create(true).append(true).open(&self.log_path) {
            let _ = writeln!(file, "{}", msg);
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

    pub fn update(&mut self, camera: &Camera) {
        self.uniforms.time = self.start_time.elapsed().as_secs_f32();
        self.uniforms.resolution = [self.size.width as f32, self.size.height as f32];
        self.uniforms.camera_dist = camera.dist;
        self.uniforms.rotation = [camera.pitch, camera.yaw];
        self.uniforms.target_pos = [camera.pan_x, camera.pan_y, camera.pan_z, 0.0]; 
        self.queue.write_buffer(&self.uniform_buffer, 0, bytemuck::cast_slice(&[self.uniforms]));
    }

    // NEW: Instantly overwrites the SSBO in GPU memory with the new AST instructions
    pub fn update_scene(&self, instructions: &[f32]) {
        self.queue.write_buffer(&self.scene_buffer, 0, bytemuck::cast_slice(instructions));
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