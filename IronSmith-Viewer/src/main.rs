mod renderer;
use anyhow::Result;
use std::sync::Arc;
use std::path::PathBuf;
use std::net::TcpListener;
use std::io::{Read, Write};
use std::time::Duration;
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
    
    // Read the last hard-saved code so we don't boot into the emergency sphere
    let initial_glsl = std::fs::read_to_string(&glsl_path).unwrap_or_default();
    let mut renderer = Renderer::new(window.clone(), initial_glsl, log_path).await?;
    
    let listener = TcpListener::bind("127.0.0.1:7878").expect("Failed to bind TCP port");
    // MUST be non-blocking so the `while` loop can cleanly exit when the queue is empty
    listener.set_nonblocking(true).expect("Cannot set non-blocking");
    
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
                    if let MouseScrollDelta::LineDelta(_, y) = delta {
                        dist = (dist - y).clamp(2.0, 100.0);
                    }
                }
                WindowEvent::RedrawRequested => {
                    if key_up { pitch += 0.05; }
                    if key_down { pitch -= 0.05; }
                    if key_left { yaw -= 0.05; }
                    if key_right { yaw += 0.05; }
                    pitch = pitch.clamp(-1.5, 1.5);

                    // --- THE TCP BACKLOG DRAINER ---
                    let mut got_new_shader = false;
                    let mut latest_code = String::new();
                    
                    // This while loop pulls EVERY pending connection from the OS instantly
                    while let Ok((mut stream, _)) = listener.accept() {
                        // Make the individual stream blocking so we can read the whole message
                        let _ = stream.set_nonblocking(false);
                        // Add a strict timeout so a fragmented packet never freezes the window
                        let _ = stream.set_read_timeout(Some(Duration::from_millis(50)));
                        
                        let mut buffer = String::new();
                        if stream.read_to_string(&mut buffer).is_ok() {
                            if buffer.contains("map(") {
                                // Overwrite any previous code from this frame. We only care about the latest!
                                latest_code = buffer;
                                got_new_shader = true;
                            }
                        }
                    }
                    
                    // Only trigger the heavy GPU compiler ONCE per frame, even if 50 packets arrived
                    if got_new_shader {
                        match renderer.reload_shader(&latest_code) {
                            Ok(_) => renderer.log_message("Shader updated via TCP socket!"),
                            Err(e) => renderer.log_message(&format!("Shader Error: {}", e)),
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
            // This is crucial: it tells winit to immediately request another redraw, keeping the game loop spinning
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