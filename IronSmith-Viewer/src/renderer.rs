use anyhow::{Context, Result};
use std::sync::Arc;
use std::time::Instant;
use std::path::PathBuf;
use wgpu::util::DeviceExt;
use winit::window::Window;
use std::io::Write; 
use std::sync::mpsc;

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
    device: Arc<wgpu::Device>, // Wrapped in Arc for safe thread sharing
    queue: wgpu::Queue,
    pub config: wgpu::SurfaceConfiguration,
    pub size: winit::dpi::PhysicalSize<u32>,
    
    render_pipeline: wgpu::RenderPipeline,
    pipeline_layout: Arc<wgpu::PipelineLayout>, // Wrapped in Arc
    
    uniform_buffer: wgpu::Buffer,
    bind_group: wgpu::BindGroup,
    pub uniforms: ShaderUniforms,
    start_time: Instant,
    log_path: PathBuf, 
    
    pipeline_rx: mpsc::Receiver<wgpu::RenderPipeline>,
    pipeline_tx: mpsc::Sender<wgpu::RenderPipeline>,
}

impl<'a> Renderer<'a> {
    pub async fn new(window: Arc<Window>, log_path: PathBuf) -> Result<Self> {
        let size = window.inner_size();
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor::default());
        let surface = instance.create_surface(window.clone())?;
        let adapter = instance.request_adapter(&wgpu::RequestAdapterOptions {
            compatible_surface: Some(&surface),
            ..Default::default()
        }).await.context("Failed to find wgpu adapter")?;

        let (device, queue) = adapter.request_device(&wgpu::DeviceDescriptor::default(), None).await?;
        let device = Arc::new(device); // Convert device to Arc
        
        let surface_caps = surface.get_capabilities(&adapter);
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_caps.formats[0],
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

        let pipeline_layout = Arc::new(device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        })); // Convert to Arc

        let (pipeline_tx, pipeline_rx) = mpsc::channel();

        shader::compile_pipeline_async(
            device.clone(),
            pipeline_layout.clone(), 
            config.format,
            vec![0.0], 
            pipeline_tx.clone()
        );

        let render_pipeline = pipeline_rx.recv().expect("Initial shader compilation failed");

        Ok(Self {
            surface, device, queue, config, size,
            render_pipeline, pipeline_layout, uniform_buffer,
            bind_group, uniforms, start_time: Instant::now(), log_path,
            pipeline_rx, pipeline_tx
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
        
        if let Ok(new_pipeline) = self.pipeline_rx.try_recv() {
            self.render_pipeline = new_pipeline;
        }
    }

    pub fn update_scene(&mut self, bytecode: Vec<f32>) {
        shader::compile_pipeline_async(
            self.device.clone(),
            self.pipeline_layout.clone(),
            self.config.format,
            bytecode,
            self.pipeline_tx.clone()
        );
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