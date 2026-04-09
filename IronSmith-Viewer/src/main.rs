mod renderer;
use anyhow::Result;
use std::sync::Arc;
use std::path::PathBuf;
use std::time::{Instant, Duration}; // --- FIX: Added Duration for throttle ---
use std::io::Write; // --- FIX: Added Write for the logger ---
use winit::{
    event::*,
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
    keyboard::{KeyCode, PhysicalKey},
};
use renderer::Renderer;

fn get_config_dir() -> PathBuf {
    dirs::config_dir().expect("Config dir error").join("ironsmith")
}

fn get_glsl_path() -> PathBuf {
    get_config_dir().join("output.glsl")
}

// --- FIX: THE BLACK BOX LOGGER ---
// This catches catastrophic Rust crashes and writes them to a file 
// so they don't corrupt your Haskell TUI.
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

    let glsl_path = get_glsl_path();
    let mut renderer = Renderer::new(window.clone(), glsl_path.clone()).await?;

    let mut last_modified = std::fs::metadata(&glsl_path).map(|m| m.modified().unwrap()).unwrap_or(std::time::SystemTime::now());
    
    // --- FIX: THE DEBOUNCE TIMER ---
    let mut last_reload_time = Instant::now();
    
    // Camera State
    let mut yaw: f32 = 0.0;
    let mut pitch: f32 = 0.4;
    let mut dist: f32 = 20.0;
    
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
                    if key_up { pitch += 0.05; }
                    if key_down { pitch -= 0.05; }
                    if key_left { yaw -= 0.05; }
                    if key_right { yaw += 0.05; }
                    pitch = pitch.clamp(-1.5, 1.5);

                    // --- FIX: THROTTLED HOT RELOAD ---
                    if let Ok(m) = std::fs::metadata(&glsl_path) {
                        if let Ok(mod_time) = m.modified() {
                            // Check if file changed AND at least 150ms have passed since our last try
                            if mod_time > last_modified && last_reload_time.elapsed() > Duration::from_millis(30) {
                                last_reload_time = Instant::now(); // Reset timer
                                
                                match renderer.reload_shader() {
                                    Ok(_) => {
                                        // Success! Update the modification tracker
                                        last_modified = mod_time;
                                        renderer.log_message("Shader compiled successfully.");
                                    }
                                    Err(e) => {
                                        // Keep last_modified the same so it tries again in 150ms.
                                        // Write the specific shader error to the log file!
                                        renderer.log_message(&format!("Shader Error: {}", e));
                                    }
                                }
                            }
                        }
                    }
                    
                    renderer.update(yaw, pitch, dist);
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
    setup_panic_logger(); // Initialize the black box
    pollster::block_on(run())
}