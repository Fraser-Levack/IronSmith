mod camera;
mod network;
mod shader;
mod renderer;

use anyhow::Result;
use std::sync::Arc;
use std::path::PathBuf;
use std::net::TcpListener;
use std::io::Write;
use winit::{
    event::*,
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

use camera::Camera;
use renderer::Renderer;
use network::NetMessage;

fn get_config_dir() -> PathBuf {
    dirs::config_dir().expect("Config dir error").join("ironsmith")
}

fn get_glsl_path() -> PathBuf {
    get_config_dir().join("output.glsl")
}

fn setup_panic_logger() {
    let log_path = get_config_dir().join("forge.log");
    std::panic::set_hook(Box::new(move |info| {
        if let Ok(mut file) = std::fs::OpenOptions::new().create(true).append(true).open(&log_path) {
            let _ = writeln!(file, "\n--- FATAL CRASH ---");
            let _ = writeln!(file, "{}", info);
        }
    }));
}

async fn run() -> Result<()> {
    let event_loop = EventLoop::new()?;
    let window = Arc::new(WindowBuilder::new()
        .with_title("IronSmith Forge")
        .with_inner_size(winit::dpi::LogicalSize::new(800, 600))
        .build(&event_loop)?);

    let log_path = get_config_dir().join("forge.log");
    let glsl_path = get_glsl_path();
    
    let initial_glsl = std::fs::read_to_string(&glsl_path).unwrap_or_default();
    let mut renderer = Renderer::new(window.clone(), initial_glsl, log_path).await?;
    
    let listener = TcpListener::bind("127.0.0.1:7878").expect("Failed to bind TCP port");
    listener.set_nonblocking(true).expect("Cannot set non-blocking");
    
    let mut camera = Camera::new();

    event_loop.run(move |event, elwt| {
        elwt.set_control_flow(ControlFlow::Poll);
        match event {
            Event::WindowEvent { event, window_id } if window_id == window.id() => match event {
                WindowEvent::CloseRequested => elwt.exit(),
                WindowEvent::Resized(ps) => renderer.resize(ps),
                
                WindowEvent::MouseWheel { delta, .. } => {
                    if let MouseScrollDelta::LineDelta(_, y) = delta {
                        camera.scroll_zoom(y);
                    }
                }
                
                WindowEvent::RedrawRequested => {
                    let mut latest_code = None;
                    
                    // Route TCP messages
                    for msg in network::drain_sockets(&listener) {
                        match msg {
                            NetMessage::Command(cmd) => camera.process_command(&cmd),
                            NetMessage::Shader(code) => latest_code = Some(code),
                        }
                    }
                    
                    if let Some(code) = latest_code {
                        match renderer.reload_shader(&code) {
                            Ok(_) => renderer.log_message("Shader updated via TCP socket!"),
                            Err(e) => renderer.log_message(&format!("Shader Error: {}", e)),
                        }
                    }
                    
                    // Update and Render
                    camera.update_lerp();
                    renderer.update(&camera);
                    
                    match renderer.render() {
                        Ok(_) => {}
                        Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => renderer.resize(renderer.size),
                        Err(e) => renderer.log_message(&format!("Render Error: {:?}", e)),
                    }
                }
                _ => {}
            },
            Event::AboutToWait => { window.request_redraw(); }
            _ => {}
        }
    })?;
    Ok(())
}

fn main() -> Result<()> {
    setup_panic_logger();
    pollster::block_on(run())
}