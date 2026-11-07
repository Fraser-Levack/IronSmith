use anyhow::{Context, Result};
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::mpsc;
use std::time::Instant;
use wgpu::util::DeviceExt;
use winit::window::Window;

use crate::camera::Camera;
use crate::filter;
use crate::shader;
use crate::video;

/// Format of the offscreen texture the scene is rendered into before the
/// filter pass. Non-sRGB so filters sample exactly what the scene shader
/// wrote (it already applies gamma itself).
const SCENE_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8Unorm;

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct ShaderUniforms {
    pub resolution: [f32; 2],
    pub time: f32,
    pub camera_dist: f32,
    pub rotation: [f32; 2],
    pub shadow_enabled: f32, // was padding[0], reused
    pub march_steps: f32,    // was padding[1], reused
    pub target_pos: [f32; 4],
    pub bg_color: [f32; 4], // NEW: rgb + unused w, added at end
    pub exposure: [f32; 4], // x = exposure multiplier, yzw unused
}

pub struct Renderer<'a> {
    surface: wgpu::Surface<'a>,
    device: Arc<wgpu::Device>, // Wrapped in Arc for safe thread sharing
    queue: wgpu::Queue,
    pub config: wgpu::SurfaceConfiguration,
    pub size: winit::dpi::PhysicalSize<u32>,

    render_pipeline: wgpu::RenderPipeline,

    uniform_buffer: wgpu::Buffer,
    bind_group: wgpu::BindGroup,
    pub uniforms: ShaderUniforms,
    start_time: Instant,
    log_path: PathBuf,

    /// Finished scene compiles, in edit order (see `spawn_scene_compiler`).
    pipeline_rx: mpsc::Receiver<shader::SceneResult>,
    /// `(generation, bytecode)` queue feeding the background scene compiler.
    scene_jobs: mpsc::Sender<(u64, Vec<f32>)>,
    /// Generation of the most recently submitted scene.
    submitted_generation: u64,
    /// Newest generation whose compile has finished, and its error if it
    /// failed (the previous working pipeline stays active in that case).
    completed_generation: u64,
    last_scene_error: Option<String>,

    // Filter pass: the scene renders into `scene_view`, then the filter
    // pipeline samples it and writes the final image to the swapchain.
    scene_view: wgpu::TextureView,
    scene_sampler: wgpu::Sampler,
    filter_bind_group_layout: wgpu::BindGroupLayout,
    filter_bind_group: wgpu::BindGroup,
    filter_pipeline: wgpu::RenderPipeline,
    filter_pipeline_layout: Arc<wgpu::PipelineLayout>,
    filter_rx: mpsc::Receiver<wgpu::RenderPipeline>,
    filter_tx: mpsc::Sender<wgpu::RenderPipeline>,
}

/// Pick the swapchain format. The scene shader gamma-encodes its own output
/// (`pow(col, 1/2.2)`), so an sRGB swapchain would encode it a second time
/// and the live view would look faded/desaturated compared to screenshots
/// and video (which undo that second encode). Prefer a plain UNORM format;
/// only fall back to the first offered format (possibly sRGB, compensated
/// in captures by `srgb_decode`) if no non-sRGB format exists.
/// Float formats are avoided too: they are presented as linear (scRGB) and
/// would wash the image out the same way.
fn choose_surface_format(formats: &[wgpu::TextureFormat]) -> wgpu::TextureFormat {
    use wgpu::TextureFormat as F;
    formats
        .iter()
        .copied()
        .find(|f| matches!(f, F::Bgra8Unorm | F::Rgba8Unorm))
        .unwrap_or(formats[0])
}

/// Create the offscreen texture view the scene pass renders into.
fn create_scene_view(device: &wgpu::Device, width: u32, height: u32) -> wgpu::TextureView {
    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("Scene Texture"),
        size: wgpu::Extent3d {
            width: width.max(1),
            height: height.max(1),
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: SCENE_FORMAT,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    texture.create_view(&wgpu::TextureViewDescriptor::default())
}

fn create_filter_bind_group(
    device: &wgpu::Device,
    layout: &wgpu::BindGroupLayout,
    uniform_buffer: &wgpu::Buffer,
    scene_view: &wgpu::TextureView,
    scene_sampler: &wgpu::Sampler,
) -> wgpu::BindGroup {
    device.create_bind_group(&wgpu::BindGroupDescriptor {
        layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: uniform_buffer.as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::TextureView(scene_view),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: wgpu::BindingResource::Sampler(scene_sampler),
            },
        ],
        label: Some("Filter Bind Group"),
    })
}

impl Renderer<'_> {
    pub async fn new(window: Arc<Window>, log_path: PathBuf) -> Result<Self> {
        let size = window.inner_size();
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor::default());
        let surface = instance.create_surface(window.clone())?;
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                compatible_surface: Some(&surface),
                ..Default::default()
            })
            .await
            .context("Failed to find wgpu adapter")?;

        let (device, queue) = adapter
            .request_device(&wgpu::DeviceDescriptor::default(), None)
            .await?;
        let device = Arc::new(device); // Convert device to Arc

        // wgpu's default handler panics on validation errors, which aborts
        // the process in release builds (panic = "abort"). Log them instead;
        // shader compilation already captures its own errors via scopes.
        let uncaptured_log = log_path.clone();
        device.on_uncaptured_error(Box::new(move |e| {
            shader::log_line(&uncaptured_log, &format!("wgpu error: {}", e));
        }));

        let surface_caps = surface.get_capabilities(&adapter);
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: choose_surface_format(&surface_caps.formats),
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);
        shader::log_line(
            &log_path,
            &format!(
                "Surface format: {:?} (offered: {:?})",
                config.format, surface_caps.formats
            ),
        );

        let uniforms = ShaderUniforms {
            resolution: [size.width as f32, size.height as f32],
            time: 0.0,
            camera_dist: 20.0,
            rotation: [0.4, 0.0],
            shadow_enabled: 1.0,
            march_steps: 150.0,
            target_pos: [0.0, 0.0, 0.0, 0.0],
            bg_color: [0.02, 0.02, 0.05, 0.0],
            exposure: [1.0, 0.0, 0.0, 0.0],
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

        let pipeline_layout = Arc::new(device.create_pipeline_layout(
            &wgpu::PipelineLayoutDescriptor {
                label: None,
                bind_group_layouts: &[&bind_group_layout],
                push_constant_ranges: &[],
            },
        )); // Convert to Arc

        let (pipeline_tx, pipeline_rx) = mpsc::channel();

        let scene_jobs = shader::spawn_scene_compiler(
            device.clone(),
            pipeline_layout,
            SCENE_FORMAT,
            pipeline_tx,
            log_path.clone(),
        );
        scene_jobs
            .send((0, vec![0.0]))
            .expect("Scene compiler thread failed to start");

        let render_pipeline = pipeline_rx
            .recv()
            .expect("Scene compiler thread stopped")
            .pipeline
            .map_err(|e| anyhow::anyhow!("Initial shader compilation failed: {}", e))?;

        // ── Filter pass setup ────────────────────────────────────────────
        let scene_view = create_scene_view(&device, size.width, size.height);
        let scene_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Scene Sampler"),
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        let filter_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
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
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
                label: Some("Filter Bind Group Layout"),
            });

        let filter_bind_group = create_filter_bind_group(
            &device,
            &filter_bind_group_layout,
            &uniform_buffer,
            &scene_view,
            &scene_sampler,
        );

        let filter_pipeline_layout = Arc::new(device.create_pipeline_layout(
            &wgpu::PipelineLayoutDescriptor {
                label: Some("Filter Pipeline Layout"),
                bind_group_layouts: &[&filter_bind_group_layout],
                push_constant_ranges: &[],
            },
        ));

        let (filter_tx, filter_rx) = mpsc::channel();

        filter::compile_filter_pipeline_async(
            device.clone(),
            filter_pipeline_layout.clone(),
            config.format,
            filter::PASSTHROUGH.to_string(),
            filter_tx.clone(),
            log_path.clone(),
        );

        let filter_pipeline = filter_rx.recv().expect("Initial filter compilation failed");

        Ok(Self {
            surface,
            device,
            queue,
            config,
            size,
            render_pipeline,
            uniform_buffer,
            bind_group,
            uniforms,
            start_time: Instant::now(),
            log_path,
            pipeline_rx,
            scene_jobs,
            submitted_generation: 0,
            completed_generation: 0,
            last_scene_error: None,
            scene_view,
            scene_sampler,
            filter_bind_group_layout,
            filter_bind_group,
            filter_pipeline,
            filter_pipeline_layout,
            filter_rx,
            filter_tx,
        })
    }

    pub fn log_message(&self, msg: &str) {
        if let Ok(mut file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.log_path)
        {
            let _ = writeln!(file, "{}", msg);
        }
    }

    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
            self.size = new_size;

            // The offscreen scene texture must match the window size, so
            // rebuild it (and the bind group that samples it) on resize.
            self.scene_view = create_scene_view(&self.device, new_size.width, new_size.height);
            self.filter_bind_group = create_filter_bind_group(
                &self.device,
                &self.filter_bind_group_layout,
                &self.uniform_buffer,
                &self.scene_view,
                &self.scene_sampler,
            );
        }
    }

    pub fn update(&mut self, camera: &Camera) {
        let t = self.start_time.elapsed().as_secs_f32();
        self.write_uniforms_at(camera, t, self.size.width, self.size.height);

        // Results arrive in edit order, so the last one queued is newest.
        while let Ok(result) = self.pipeline_rx.try_recv() {
            self.completed_generation = result.generation;
            match result.pipeline {
                Ok(pipeline) => {
                    self.render_pipeline = pipeline;
                    self.last_scene_error = None;
                }
                Err(e) => self.last_scene_error = Some(e),
            }
        }

        if let Ok(new_filter) = self.filter_rx.try_recv() {
            self.filter_pipeline = new_filter;
        }
    }

    pub fn update_scene(&mut self, bytecode: Vec<f32>) {
        self.submitted_generation += 1;
        if self
            .scene_jobs
            .send((self.submitted_generation, bytecode))
            .is_err()
        {
            self.log_message("Scene compiler thread has stopped; scene not updated");
        }
    }

    /// Generation of the most recently submitted scene. A caller that wants
    /// to act once "the scene I just sent" is live records this, then polls
    /// `scene_status`.
    pub fn scene_generation(&self) -> u64 {
        self.submitted_generation
    }

    /// `None` while scene `generation` is still compiling; otherwise whether
    /// the newest finished compile (covering it) succeeded.
    pub fn scene_status(&self, generation: u64) -> Option<Result<(), String>> {
        if self.completed_generation < generation {
            return None;
        }
        Some(match &self.last_scene_error {
            Some(e) => Err(e.clone()),
            None => Ok(()),
        })
    }

    /// Swap the post-process filter. `body` is the GLSL snippet defining
    /// `vec4 apply(vec2 uv)`; on compile failure the current filter stays.
    pub fn update_filter(&mut self, body: String) {
        filter::compile_filter_pipeline_async(
            self.device.clone(),
            self.filter_pipeline_layout.clone(),
            self.config.format,
            body,
            self.filter_tx.clone(),
            self.log_path.clone(),
        );
    }

    /// Record the two render passes (scene raymarch -> offscreen texture,
    /// then the active filter -> `target`). Shared by the live render loop
    /// (target = swapchain) and video export (target = export texture).
    fn encode_passes(&self, encoder: &mut wgpu::CommandEncoder, target: &wgpu::TextureView) {
        {
            // Pass 1: raymarch the scene into the offscreen texture
            let mut rp = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Scene Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &self.scene_view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                occlusion_query_set: None,
                timestamp_writes: None,
            });
            rp.set_pipeline(&self.render_pipeline);
            rp.set_bind_group(0, &self.bind_group, &[]);
            rp.draw(0..3, 0..1);
        }
        {
            // Pass 2: apply the active filter to the target
            let mut rp = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Filter Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: target,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                occlusion_query_set: None,
                timestamp_writes: None,
            });
            rp.set_pipeline(&self.filter_pipeline);
            rp.set_bind_group(0, &self.filter_bind_group, &[]);
            rp.draw(0..3, 0..1);
        }
    }

    pub fn render(&mut self) -> Result<(), wgpu::SurfaceError> {
        let output = self.surface.get_current_texture()?;
        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
        self.encode_passes(&mut encoder, &view);
        self.queue.submit(std::iter::once(encoder.finish()));
        output.present();
        Ok(())
    }

    /// Write the frame's uniforms directly with a deterministic timeline
    /// time instead of wall-clock time, so exports are identical no matter
    /// how fast the GPU renders them.
    fn write_uniforms_at(&mut self, camera: &Camera, t: f32, width: u32, height: u32) {
        self.uniforms.time = t;
        self.uniforms.resolution = [width as f32, height as f32];
        self.uniforms.camera_dist = camera.dist;
        self.uniforms.rotation = [camera.pitch, camera.yaw];
        self.uniforms.shadow_enabled = if camera.shadow_enabled { 1.0 } else { 0.0 };
        self.uniforms.march_steps = camera.march_steps as f32;
        self.uniforms.target_pos = [camera.pan_x, camera.pan_y, camera.pan_z, 0.0];
        self.uniforms.bg_color = [
            camera.bg_color[0],
            camera.bg_color[1],
            camera.bg_color[2],
            0.0,
        ];
        self.uniforms.exposure = [camera.exposure, 0.0, 0.0, 0.0];
        self.queue.write_buffer(
            &self.uniform_buffer,
            0,
            bytemuck::cast_slice(&[self.uniforms]),
        );
    }

    /// Render the camera animation offline and encode it to a video file
    /// (`<base_path>.mp4` via ffmpeg, or `.gif` without it). Steps the
    /// timeline frame by frame at `fps`, so output smoothness is
    /// independent of live rendering speed. Blocks until done; `progress`
    /// is called after each frame with (done, total).
    pub fn export_video(
        &mut self,
        camera: &mut Camera,
        fps: u32,
        base_path: &str,
        mut progress: impl FnMut(u32, u32),
    ) -> Result<PathBuf> {
        let duration = camera.anim_duration();
        anyhow::ensure!(
            !camera.anim_track.is_empty() && duration > 0.0,
            "no animation to export: the script needs camera(...) keyframes spanning more than 0 seconds"
        );

        let width = self.size.width.max(1);
        let height = self.size.height.max(1);
        let fps = fps.clamp(1, 120);
        let total_frames = ((duration * fps as f32).round() as u32).max(1);

        let mut sink =
            video::VideoEncoder::new(base_path, width, height, fps, self.pixel_format())?;
        self.log_message(&format!(
            "Video export: {} frames @ {} fps -> {}",
            total_frames,
            fps,
            sink.path().display()
        ));

        let capture = self.create_frame_capture("Video Export");
        let mut frame_pixels = vec![0u8; (width * height * 4) as usize];

        for i in 0..total_frames {
            let t = i as f32 / fps as f32;
            camera.apply_anim(t);
            self.write_uniforms_at(camera, t, width, height);
            self.capture_frame(&capture, &mut frame_pixels)?;
            sink.write_frame(&mut frame_pixels)?;
            progress(i + 1, total_frames);
        }

        let path = sink.finish()?;
        self.log_message(&format!("Video export finished: {}", path.display()));
        Ok(path)
    }

    /// Channel order of pixels read back from the swapchain-format texture.
    fn pixel_format(&self) -> video::PixelFormat {
        match self.config.format {
            wgpu::TextureFormat::Bgra8Unorm | wgpu::TextureFormat::Bgra8UnormSrgb => {
                video::PixelFormat::Bgra
            }
            _ => video::PixelFormat::Rgba,
        }
    }

    /// Render the scene once from `camera` (a one-off pose; the live camera
    /// is untouched) and save it as a PNG at the window's resolution.
    pub fn screenshot(&mut self, camera: &Camera, path: &std::path::Path) -> Result<()> {
        let capture = self.create_frame_capture("Screenshot");
        let (width, height) = (capture.width, capture.height);
        let t = self.start_time.elapsed().as_secs_f32();
        self.write_uniforms_at(camera, t, width, height);

        let mut pixels = vec![0u8; (width * height * 4) as usize];
        self.capture_frame(&capture, &mut pixels)?;
        if self.pixel_format() == video::PixelFormat::Bgra {
            for px in pixels.chunks_exact_mut(4) {
                px.swap(0, 2);
            }
        }
        video::write_png(path, width, height, &pixels)?;
        self.log_message(&format!("Screenshot saved: {}", path.display()));
        Ok(())
    }

    /// Offscreen target + readback buffer sized to the window, used to pull
    /// rendered frames back to the CPU (video export and screenshots).
    fn create_frame_capture(&self, label: &str) -> FrameCapture {
        let width = self.size.width.max(1);
        let height = self.size.height.max(1);

        // Same format as the swapchain so the existing filter pipeline can
        // render into it, plus COPY_SRC for CPU readback.
        let texture = self.device.create_texture(&wgpu::TextureDescriptor {
            label: Some(label),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: self.config.format,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
            view_formats: &[],
        });
        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        // Texture-to-buffer copies need bytes_per_row aligned to 256.
        let padded_row = (width * 4).div_ceil(wgpu::COPY_BYTES_PER_ROW_ALIGNMENT)
            * wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
        let readback = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some(label),
            size: (padded_row * height) as u64,
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
            mapped_at_creation: false,
        });

        // On an sRGB swapchain the shader's output (which already applies
        // its own gamma) is sRGB-*encoded* on write, so the raw bytes are
        // brighter than what the monitor shows. Decode once so captures
        // match the viewport instead of being washed out.
        let srgb_decode = self.config.format.is_srgb().then(|| {
            let mut lut = [0u8; 256];
            for (b, out) in lut.iter_mut().enumerate() {
                let c = b as f32 / 255.0;
                let lin = if c <= 0.04045 {
                    c / 12.92
                } else {
                    ((c + 0.055) / 1.055).powf(2.4)
                };
                *out = (lin * 255.0 + 0.5) as u8;
            }
            lut
        });

        FrameCapture {
            texture,
            view,
            readback,
            width,
            height,
            padded_row,
            srgb_decode,
        }
    }

    /// Render both passes with the current uniforms into `capture` and copy
    /// the result into `out` as tightly packed 4-byte pixels (surface
    /// channel order, see `pixel_format`). Blocks until the GPU is done.
    fn capture_frame(&self, capture: &FrameCapture, out: &mut [u8]) -> Result<()> {
        let (width, height) = (capture.width, capture.height);
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Frame Capture Encoder"),
            });
        self.encode_passes(&mut encoder, &capture.view);
        encoder.copy_texture_to_buffer(
            wgpu::ImageCopyTexture {
                texture: &capture.texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            wgpu::ImageCopyBuffer {
                buffer: &capture.readback,
                layout: wgpu::ImageDataLayout {
                    offset: 0,
                    bytes_per_row: Some(capture.padded_row),
                    rows_per_image: Some(height),
                },
            },
            wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
        );
        self.queue.submit(std::iter::once(encoder.finish()));

        let slice = capture.readback.slice(..);
        let (tx, rx) = mpsc::channel();
        slice.map_async(wgpu::MapMode::Read, move |r| {
            let _ = tx.send(r);
        });
        self.device.poll(wgpu::Maintain::Wait);
        rx.recv()
            .context("readback callback dropped")?
            .context("failed to map readback buffer")?;
        {
            let data = slice.get_mapped_range();
            let row_bytes = (width * 4) as usize;
            for row in 0..height as usize {
                let src = row * capture.padded_row as usize;
                out[row * row_bytes..(row + 1) * row_bytes]
                    .copy_from_slice(&data[src..src + row_bytes]);
            }
        }
        capture.readback.unmap();

        if let Some(lut) = &capture.srgb_decode {
            for px in out.chunks_exact_mut(4) {
                px[0] = lut[px[0] as usize];
                px[1] = lut[px[1] as usize];
                px[2] = lut[px[2] as usize];
            }
        }
        Ok(())
    }
}

/// See `Renderer::create_frame_capture`.
struct FrameCapture {
    texture: wgpu::Texture,
    view: wgpu::TextureView,
    readback: wgpu::Buffer,
    width: u32,
    height: u32,
    padded_row: u32,
    srgb_decode: Option<[u8; 256]>,
}
