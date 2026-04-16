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
    
    // -- Camera State --
    let mut yaw: f32 = 0.0;
    let mut pitch: f32 = 0.4;
    let mut dist: f32 = 20.0;
    
    let mut pan_x: f32 = 0.0;
    let mut pan_y: f32 = 0.0;
    let mut pan_z: f32 = 0.0;

    // -- Smoothing Targets --
    let mut target_yaw: f32 = yaw;
    let mut target_pitch: f32 = pitch;
    let mut target_dist: f32 = dist;
    
    let mut target_pan_x: f32 = pan_x;
    let mut target_pan_y: f32 = pan_y;
    let mut target_pan_z: f32 = pan_z;
    
    // Auto-orbit starts true to match the Haskell OrbitMode default
    let mut auto_orbit = true;

    event_loop.run(move |event, elwt| {
        elwt.set_control_flow(ControlFlow::Poll);
        match event {
            Event::WindowEvent { event, window_id } if window_id == window.id() => match event {
                WindowEvent::CloseRequested => elwt.exit(),
                WindowEvent::Resized(ps) => renderer.resize(ps),
                
                // Keep scroll-wheel zooming as a nice backup to the keyboard
                WindowEvent::MouseWheel { delta, .. } => {
                    if let MouseScrollDelta::LineDelta(_, y) = delta {
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
                            
                            // Check if Haskell sent a Command or a Shader
                            if buffer.starts_with("CMD:") {
                                match buffer.trim() {
                                    "CMD:OrbitMode"  => auto_orbit = true,
                                    "CMD:StaticMode" => auto_orbit = false,
                                    "CMD:FlyMode"    => auto_orbit = false, 
                                    
                                    // Rotation
                                    "CMD:PITCH_UP"   => target_pitch += 0.1,
                                    "CMD:PITCH_DOWN" => target_pitch -= 0.1,
                                    "CMD:YAW_LEFT"   => target_yaw -= 0.1,
                                    "CMD:YAW_RIGHT"  => target_yaw += 0.1,
                                    
                                    // Zoom
                                    "CMD:ZOOM_IN"    => target_dist = (target_dist - 2.0).clamp(2.0, 100.0),
                                    "CMD:ZOOM_OUT"   => target_dist = (target_dist + 2.0).clamp(2.0, 100.0),
                                    
                                    // Camera-Relative Panning
                                    "CMD:PAN_FORWARD" => {
                                        target_pan_x -= target_yaw.sin() * 1.0;
                                        target_pan_z -= target_yaw.cos() * 1.0;
                                    },
                                    "CMD:PAN_BACKWARD" => {
                                        target_pan_x += target_yaw.sin() * 1.0;
                                        target_pan_z += target_yaw.cos() * 1.0;
                                    },
                                    "CMD:PAN_LEFT" => {
                                        target_pan_x -= target_yaw.cos() * 1.0;
                                        target_pan_z += target_yaw.sin() * 1.0;
                                    },
                                    "CMD:PAN_RIGHT" => {
                                        target_pan_x += target_yaw.cos() * 1.0;
                                        target_pan_z -= target_yaw.sin() * 1.0;
                                    },
                                    "CMD:PAN_UP"   => target_pan_y += 1.0,
                                    "CMD:PAN_DOWN" => target_pan_y -= 1.0,

                                    // Reset everything to origin
                                    "CMD:RESET_CAMERA" => {
                                        target_yaw = 0.0;
                                        target_pitch = 0.4;
                                        target_dist = 20.0;
                                        target_pan_x = 0.0;
                                        target_pan_y = 0.0;
                                        target_pan_z = 0.0;
                                    },
                                    _ => {}
                                }
                            } else if buffer.contains("map(") {
                                latest_code = buffer;
                                got_new_shader = true;
                            }
                        }
                    }
                    
                    if got_new_shader {
                        match renderer.reload_shader(&latest_code) {
                            Ok(_) => renderer.log_message("Shader updated via TCP socket!"),
                            Err(e) => renderer.log_message(&format!("Shader Error: {}", e)),
                        }
                    }
                    
                    // Apply the automatic spin if we are in OrbitMode
                    if auto_orbit {
                        target_yaw -= 0.003; 
                    }
                    
                    // Clamp pitch so we don't flip upside down
                    target_pitch = target_pitch.clamp(-1.5, 1.5);
                    
                    // --- THE LERP MATH (Smoothing) ---
                    yaw += (target_yaw - yaw) * 0.1;
                    pitch += (target_pitch - pitch) * 0.1;
                    dist += (target_dist - dist) * 0.1;
                    
                    pan_x += (target_pan_x - pan_x) * 0.1;
                    pan_y += (target_pan_y - pan_y) * 0.1;
                    pan_z += (target_pan_z - pan_z) * 0.1;
                    
                    renderer.update(yaw, pitch, dist, pan_x, pan_y, pan_z);
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