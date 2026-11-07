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
use network::{Incoming, NetMessage};
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

/// How long a screenshot request may wait for its scene to compile before
/// the viewer gives up and replies with an error.
const SCREENSHOT_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(600);

/// A parsed `CMD:SCREENSHOT:<view>:<path>` request.
///
/// `<view>` is empty (use the live camera) or `yaw,pitch,dist,tx,ty,tz`
/// with angles in radians, like `CMD:SET_ANIM`. `<path>` must be absolute:
/// the viewer's working directory is not the client's.
#[derive(Debug, PartialEq)]
struct ScreenshotRequest {
    view: Option<[f32; 6]>,
    path: PathBuf,
}

fn parse_screenshot(rest: &str) -> Result<ScreenshotRequest, String> {
    // Split on the first ':' only: Windows paths contain colons.
    let (view_str, path_str) = rest
        .split_once(':')
        .ok_or("expected CMD:SCREENSHOT:<view>:<absolute path>")?;
    let view = if view_str.trim().is_empty() {
        None
    } else {
        let nums: Vec<f32> = view_str
            .split(',')
            .map(|v| v.trim().parse::<f32>())
            .collect::<Result<_, _>>()
            .map_err(|_| format!("bad view '{}': expected 6 numbers", view_str))?;
        let arr: [f32; 6] = nums
            .try_into()
            .map_err(|_| format!("bad view '{}': expected 6 numbers", view_str))?;
        Some(arr)
    };
    let path = PathBuf::from(path_str.trim());
    if !path.is_absolute() {
        return Err(format!(
            "screenshot path must be absolute: {}",
            path.display()
        ));
    }
    Ok(ScreenshotRequest { view, path })
}

/// A screenshot waiting for the scene generation it was requested after.
struct PendingShot {
    request: ScreenshotRequest,
    stream: std::net::TcpStream,
    generation: u64,
    since: std::time::Instant,
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
    let mut pending_shots: Vec<PendingShot> = Vec::new();

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
                    let mut new_shots = Vec::new();

                    for Incoming { message, stream } in network::drain_sockets(&listener) {
                        match message {
                            NetMessage::Command(cmd) => {
                                if let Some(rest) = cmd.strip_prefix("CMD:SCREENSHOT:") {
                                    match parse_screenshot(rest) {
                                        Ok(request) => new_shots.push((request, stream)),
                                        Err(e) => network::reply(stream, &format!("ERROR {}", e)),
                                    }
                                } else if let Some(name) = cmd.strip_prefix("CMD:SET_FILTER:") {
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

                    if let Some(bytecode) = latest_bytecode {
                        renderer.update_scene(bytecode);
                    }

                    // Screenshots wait for the newest scene submitted so far,
                    // which includes any bytecode that arrived just before
                    // them in this same batch.
                    for (request, stream) in new_shots {
                        pending_shots.push(PendingShot {
                            request,
                            stream,
                            generation: renderer.scene_generation(),
                            since: std::time::Instant::now(),
                        });
                    }

                    // Take ready screenshots before the live frame's
                    // uniforms are written below, so the window never shows
                    // the screenshot pose.
                    let mut still_waiting = Vec::new();
                    for shot in pending_shots.drain(..) {
                        match renderer.scene_status(shot.generation) {
                            None if shot.since.elapsed() < SCREENSHOT_TIMEOUT => {
                                still_waiting.push(shot)
                            }
                            None => network::reply(
                                shot.stream,
                                "ERROR timed out waiting for the scene to compile",
                            ),
                            Some(Err(e)) => network::reply(shot.stream, &format!("ERROR {}", e)),
                            Some(Ok(())) => {
                                let mut view_cam = camera.clone();
                                if let Some([yaw, pitch, dist, tx, ty, tz]) = shot.request.view {
                                    view_cam.set_pose(yaw, pitch, dist, [tx, ty, tz]);
                                }
                                let line = match renderer.screenshot(&view_cam, &shot.request.path) {
                                    Ok(()) => format!("OK {}", shot.request.path.display()),
                                    Err(e) => format!("ERROR screenshot failed: {:#}", e),
                                };
                                network::reply(shot.stream, &line);
                            }
                        }
                    }
                    pending_shots = still_waiting;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn screenshot_with_view_keeps_colons_in_path() {
        // On Windows the temp dir starts with a drive letter ("C:..."), so
        // this exercises splitting on the first ':' only.
        let path = std::env::temp_dir().join("car.png");
        let req = parse_screenshot(&format!("0.5,0.3,12,0,1.5,0:{}", path.display())).unwrap();
        assert_eq!(req.view, Some([0.5, 0.3, 12.0, 0.0, 1.5, 0.0]));
        assert_eq!(req.path, path);
    }

    #[test]
    fn screenshot_without_view_uses_live_camera() {
        let path = std::env::temp_dir().join("shot.png");
        let req = parse_screenshot(&format!(":{}", path.display())).unwrap();
        assert_eq!(req.view, None);
        assert_eq!(req.path, path);
    }

    #[test]
    fn screenshot_rejects_relative_paths_and_bad_views() {
        assert!(parse_screenshot(":car.png").is_err());
        assert!(parse_screenshot("1,2,3:/abs/car.png").is_err());
        assert!(parse_screenshot("no-colon").is_err());
    }
}
