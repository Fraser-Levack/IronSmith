mod camera;
mod filter;
mod network;
mod renderer;
mod shader;
mod video;

use anyhow::Result;
use std::io::Write;
use std::net::TcpListener;
use std::path::PathBuf;
use std::sync::Arc;
use winit::{
    event::*,
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

use camera::Camera;
use network::NetMessage;
use renderer::Renderer;

fn get_config_dir() -> PathBuf {
    dirs::config_dir()
        .expect("Config dir error")
        .join("ironsmith")
}

fn get_filters_dir() -> PathBuf {
    get_config_dir().join("filters")
}

/// Resolve a filter name (from CMD:SET_FILTER:<name>) to its GLSL body.
/// Names are restricted to simple identifiers so a command can never
/// escape the filters directory.
fn load_filter(name: &str) -> Option<String> {
    if name.is_empty() || name == "none" {
        return Some(filter::PASSTHROUGH.to_string());
    }
    if !name
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
    {
        return None;
    }
    std::fs::read_to_string(get_filters_dir().join(format!("{}.glsl", name))).ok()
}

fn setup_panic_logger() {
    let log_path = get_config_dir().join("forge.log");
    std::panic::set_hook(Box::new(move |info| {
        if let Ok(mut file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&log_path)
        {
            let _ = writeln!(file, "\n--- FATAL CRASH ---");
            let _ = writeln!(file, "{}", info);
        }
    }));
}

async fn run() -> Result<()> {
    let event_loop = EventLoop::new()?;
    let window = Arc::new(
        WindowBuilder::new()
            .with_title("IronSmith Forge (JIT Compiler)")
            .with_inner_size(winit::dpi::LogicalSize::new(800, 600))
            .build(&event_loop)?,
    );

    let log_path = get_config_dir().join("forge.log");

    let mut renderer = Renderer::new(window.clone(), log_path).await?;

    // Ship the example filters so users have working files to copy and mod.
    if let Err(e) = filter::install_default_filters(&get_filters_dir()) {
        renderer.log_message(&format!("Failed to install default filters: {}", e));
    }

    let listener = TcpListener::bind("127.0.0.1:7878").expect("Failed to bind TCP port");
    listener
        .set_nonblocking(true)
        .expect("Cannot set non-blocking");

    let mut camera = Camera::new();
    let mut last_frame = std::time::Instant::now();

    event_loop.run(move |event, elwt| {
        elwt.set_control_flow(ControlFlow::Poll);
        match event {
            Event::WindowEvent { event, window_id } if window_id == window.id() => match event {
                WindowEvent::CloseRequested => elwt.exit(),
                WindowEvent::Resized(ps) => renderer.resize(ps),

                WindowEvent::MouseWheel {
                    delta: MouseScrollDelta::LineDelta(_, y),
                    ..
                } => {
                    camera.scroll_zoom(y);
                }

                WindowEvent::RedrawRequested => {
                    let mut latest_bytecode = None;

                    for msg in network::drain_sockets(&listener) {
                        match msg {
                            NetMessage::Command(cmd) => {
                                if let Some(name) = cmd.strip_prefix("CMD:SET_FILTER:") {
                                    match load_filter(name.trim()) {
                                        Some(body) => renderer.update_filter(body),
                                        None => renderer.log_message(&format!(
                                            "Unknown filter: {}",
                                            name.trim()
                                        )),
                                    }
                                } else if let Some(rest) = cmd.strip_prefix("CMD:EXPORT_VIDEO:") {
                                    // Payload is "<fps>:<base path>"; only split
                                    // the first ':' since paths contain colons.
                                    let (fps_str, base_path) =
                                        rest.split_once(':').unwrap_or(("30", rest));
                                    let fps = fps_str.trim().parse().unwrap_or(30);
                                    let win = window.clone();
                                    let result = renderer.export_video(
                                        &mut camera,
                                        fps,
                                        base_path.trim(),
                                        |done, total| {
                                            win.set_title(&format!(
                                                "IronSmith Forge - exporting video: frame {}/{}",
                                                done, total
                                            ));
                                        },
                                    );
                                    match result {
                                        Ok(path) => window.set_title(&format!(
                                            "IronSmith Forge - video saved: {}",
                                            path.display()
                                        )),
                                        Err(e) => {
                                            renderer.log_message(&format!(
                                                "Video export failed: {}",
                                                e
                                            ));
                                            window.set_title(
                                                "IronSmith Forge - video export failed (see forge.log)",
                                            );
                                        }
                                    }
                                } else {
                                    camera.process_command(&cmd)
                                }
                            }
                            NetMessage::Bytecode(code) => latest_bytecode = Some(code),
                        }
                    }

                    // FIX: Pass ownership by removing the `&`
                    if let Some(bytecode) = latest_bytecode {
                        renderer.update_scene(bytecode);
                    }

                    let now = std::time::Instant::now();
                    // Clamp dt so a stall (e.g. window drag) can't make the
                    // animation jump a whole loop in one frame.
                    let dt = (now - last_frame).as_secs_f32().min(0.1);
                    last_frame = now;

                    camera.update_lerp(dt);
                    renderer.update(&camera);

                    match renderer.render() {
                        Ok(_) => {}
                        Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                            renderer.resize(renderer.size)
                        }
                        Err(e) => renderer.log_message(&format!("Render Error: {:?}", e)),
                    }
                }
                _ => {}
            },
            Event::AboutToWait => {
                window.request_redraw();
            }
            _ => {}
        }
    })?;
    Ok(())
}

fn main() -> Result<()> {
    setup_panic_logger();
    pollster::block_on(run())
}
