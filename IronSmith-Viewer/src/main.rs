mod renderer;
use anyhow::Result;
use std::sync::Arc;
use std::path::PathBuf;
use winit::{
    event::*,
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
    keyboard::{KeyCode, PhysicalKey},
};
use renderer::Renderer;

fn get_glsl_path() -> PathBuf {
    dirs::config_dir().expect("Config dir error").join("ironsmith").join("output.glsl")
}

async fn run() -> Result<()> {
    let event_loop = EventLoop::new()?;
    let window = Arc::new(WindowBuilder::new()
        .with_title("IronSmith Forge")
        .with_inner_size(winit::dpi::LogicalSize::new(800, 600))
        .build(&event_loop)?);

    let glsl_path = get_glsl_path();
    let mut renderer = Renderer::new(window.clone(), glsl_path.clone()).await?;

    let mut last_modified = std::fs::metadata(&glsl_path).map(|m| m.modified().unwrap()).unwrap_or(std::time::SystemTime::now());
    
    // Camera State
    let mut yaw: f32 = 0.0;
    let mut pitch: f32 = 0.4;
    let mut dist: f32 = 20.0;
    
    // Input state
    let mut key_up = false; let mut key_down = false;
    let mut key_left = false; let mut key_right = false;

    event_loop.run(move |event, elwt| {
        elwt.set_control_flow(ControlFlow::Poll);
        match event {
            Event::WindowEvent { event, window_id } if window_id == window.id() => match event {
                WindowEvent::CloseRequested => elwt.exit(),
                WindowEvent::Resized(ps) => renderer.resize(ps),
                WindowEvent::KeyboardInput { event: KeyEvent { physical_key, state, .. }, .. } => {
                    let pressed = state == ElementState::Pressed;
                    if let PhysicalKey::Code(code) = physical_key {
                        match code {
                            KeyCode::ArrowUp | KeyCode::KeyW => key_up = pressed,
                            KeyCode::ArrowDown | KeyCode::KeyS => key_down = pressed,
                            KeyCode::ArrowLeft | KeyCode::KeyA => key_left = pressed,
                            KeyCode::ArrowRight | KeyCode::KeyD => key_right = pressed,
                            _ => {}
                        }
                    }
                }
                WindowEvent::MouseWheel { delta, .. } => {
                    match delta {
                        MouseScrollDelta::LineDelta(_, y) => dist = (dist - y).clamp(2.0, 100.0),
                        _ => {}
                    }
                }
                WindowEvent::RedrawRequested => {
                    // 1. Update Camera (Keep this logic)
                    if key_up { pitch += 0.05; }
                    if key_down { pitch -= 0.05; }
                    if key_left { yaw -= 0.05; }
                    if key_right { yaw += 0.05; }
                    pitch = pitch.clamp(-1.5, 1.5);

                    // 2. HOT RELOAD CHECK (The critical fix)
                    if let Ok(m) = std::fs::metadata(&glsl_path) {
                        if let Ok(mod_time) = m.modified() {
                            if mod_time > last_modified {
                                // CHANGE: We call it and handle the result without crashing
                                match renderer.reload_shader() {
                                    Ok(_) => {
                                        // eprintln!("Shader successfully forged!");
                                        last_modified = mod_time;
                                    }
                                    Err(e) => {
                                        // If the user is mid-sentence typing "cube", 
                                        // this will trigger. We just print it and move on.
                                        eprintln!("Forge Error (Temporary): {}", e);
                                        // We DON'T update last_modified here so it tries again 
                                        // next frame until the code is valid.
                                    }
                                }
                            }
                        }
                    }

                    // 3. Update and Render
                    renderer.update(yaw, pitch, dist);
                    
                    // CHANGE: Handle render errors (like Surface Lost) gracefully too
                    match renderer.render() {
                        Ok(_) => {}
                        Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                            renderer.resize(renderer.size);
                        }
                        Err(e) => eprintln!("Render error: {:?}", e),
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
    pollster::block_on(run())
}