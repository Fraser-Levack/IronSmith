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
    listener.set_nonblocking(true).expect("Cannot set non-blocking");
    
    let mut yaw: f32 = 0.0;
    let mut pitch: f32 = 0.4;
    let mut dist: f32 = 20.0;
    
    // NEW: Target variables for smooth interpolation
    let mut target_yaw: f32 = yaw;
    let mut target_pitch: f32 = pitch;
    let mut target_dist: f32 = dist;
    
    // Auto-orbit starts true to match the Haskell OrbitMode default
    let mut auto_orbit = true;

    event_loop.run(move |event, elwt| {
        elwt.set_control_flow(ControlFlow::Poll);
        match event {
            Event::WindowEvent { event, window_id } if window_id == window.id() => match event {
                WindowEvent::CloseRequested => elwt.exit(),
                WindowEvent::Resized(ps) => renderer.resize(ps),
                
                WindowEvent::MouseWheel { delta, .. } => {
                    if let MouseScrollDelta::LineDelta(_, y) = delta {
                        // NEW: Modify the target, increase the multiplier slightly for better feel
                        target_dist = (target_dist - y * 2.0).clamp(2.0, 100.0);
                    }
                }
                
                WindowEvent::RedrawRequested => {
                    let mut got_new_shader = false;
                    let mut latest_code = String::new();
                    
                    // --- THE TCP BACKLOG DRAINER & COMMAND ROUTER ---
                    while let Ok((mut stream, _)) = listener.accept() {
                        let _ = stream.set_nonblocking(false);
                        let _ = stream.set_read_timeout(Some(Duration::from_millis(50)));
                        
                        let mut buffer = String::new();
                        if stream.read_to_string(&mut buffer).is_ok() {

                            let movement_value = 0.2; // Adjust this for how much each command changes the target
                            
                            // Check if Haskell sent a Command or a Shader
                            if buffer.starts_with("CMD:") {
                                match buffer.trim() {
                                    "CMD:OrbitMode"  => auto_orbit = true,
                                    "CMD:StaticMode" => auto_orbit = false,
                                    "CMD:FlyMode"    => auto_orbit = false, 
                                    // NEW: Modify targets, and bump the speed to 0.1 for responsiveness
                                    "CMD:PITCH_UP"   => target_pitch += movement_value,
                                    "CMD:PITCH_DOWN" => target_pitch -= movement_value,
                                    "CMD:YAW_LEFT"   => target_yaw -= movement_value,
                                    "CMD:YAW_RIGHT"  => target_yaw += movement_value,
                                    "CMD:ZOOM_IN"    => target_dist = (target_dist - 2.0).clamp(2.0, 100.0),
                                    "CMD:ZOOM_OUT"   => target_dist = (target_dist + 2.0).clamp(2.0, 100.0),
                                    _ => {}
                                }
                            } else if buffer.contains("map(") {
                                latest_code = buffer;
                                got_new_shader = true;
                            }
                        }
                    } // <-- End of the TCP while loop
                    
                    if got_new_shader {
                        match renderer.reload_shader(&latest_code) {
                            Ok(_) => renderer.log_message("Shader updated via TCP socket!"),
                            Err(e) => renderer.log_message(&format!("Shader Error: {}", e)),
                        }
                    }
                    
                    // Drive the target continuously if Orbiting
                    if auto_orbit {
                        target_yaw -= 0.003; 
                    }
                    
                    // Constrain the targets so we don't flip the camera upside down
                    target_pitch = target_pitch.clamp(-1.5, 1.5);
                    
                    let interpolation_f = 0.1; // Adjust this for more/less smoothing
                    // NEW: The Smoothing Math (Linear Interpolation)
                    // The '0.1' is the stiffness. Lower (e.g., 0.05) is floatier, Higher (e.g., 0.3) is snappier.
                    yaw += (target_yaw - yaw) * interpolation_f;
                    pitch += (target_pitch - pitch) * interpolation_f;
                    dist += (target_dist - dist) * interpolation_f;
                    
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
    setup_panic_logger();
    pollster::block_on(run())
}