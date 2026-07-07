//! Video encoding for animation export.
//!
//! Frames rendered by the viewer are handed to one of two encoders:
//! - **ffmpeg** (if found on PATH): raw frames are piped to its stdin and
//!   encoded to an H.264 .mp4 — best quality, no bundled codec.
//! - **GIF** (pure-Rust fallback via the `gif` crate): always available,
//!   and GIFs preview inline almost everywhere, which suits sharing.

use anyhow::{Context, Result};
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};

/// Channel order of the raw frames the renderer reads back. The swapchain
/// on most platforms is BGRA; ffmpeg consumes that directly, the GIF path
/// swizzles to RGBA on the CPU.
#[derive(Clone, Copy, PartialEq)]
pub enum PixelFormat {
    Rgba,
    Bgra,
}

pub enum VideoEncoder {
    Ffmpeg {
        child: Child,
        path: PathBuf,
    },
    Gif {
        encoder: gif::Encoder<File>,
        path: PathBuf,
        width: u16,
        height: u16,
        /// Frame delay in centiseconds (GIF's native time unit).
        delay: u16,
        format: PixelFormat,
    },
}

impl VideoEncoder {
    /// Open an encoder writing to `<base_path>.mp4` (ffmpeg) or
    /// `<base_path>.gif` (fallback).
    pub fn new(
        base_path: &str,
        width: u32,
        height: u32,
        fps: u32,
        format: PixelFormat,
    ) -> Result<Self> {
        let fps = fps.max(1);
        let mp4_path = PathBuf::from(format!("{}.mp4", base_path));
        let pix = match format {
            PixelFormat::Rgba => "rgba",
            PixelFormat::Bgra => "bgra",
        };

        let ffmpeg = Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "rawvideo",
                "-pixel_format",
                pix,
                "-video_size",
                &format!("{}x{}", width, height),
                "-framerate",
                &fps.to_string(),
                "-i",
                "-",
                // yuv420p needs even dimensions; crop at most one pixel.
                "-vf",
                "crop=trunc(iw/2)*2:trunc(ih/2)*2",
                "-pix_fmt",
                "yuv420p",
            ])
            .arg(&mp4_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn();

        match ffmpeg {
            Ok(child) => Ok(VideoEncoder::Ffmpeg {
                child,
                path: mp4_path,
            }),
            Err(_) => {
                // No ffmpeg on this machine: encode a GIF ourselves.
                let path = PathBuf::from(format!("{}.gif", base_path));
                let file = File::create(&path)
                    .with_context(|| format!("cannot create {}", path.display()))?;
                let mut encoder = gif::Encoder::new(file, width as u16, height as u16, &[])?;
                encoder.set_repeat(gif::Repeat::Infinite)?;
                Ok(VideoEncoder::Gif {
                    encoder,
                    path,
                    width: width as u16,
                    height: height as u16,
                    delay: (100 / fps).max(1) as u16,
                    format,
                })
            }
        }
    }

    /// Where the video is being written (extension depends on the encoder).
    pub fn path(&self) -> &PathBuf {
        match self {
            VideoEncoder::Ffmpeg { path, .. } => path,
            VideoEncoder::Gif { path, .. } => path,
        }
    }

    /// Append one frame. `pixels` is tightly-packed 4-bytes-per-pixel data
    /// in the format passed to `new`; it may be scribbled on (the GIF
    /// quantizer works in place).
    pub fn write_frame(&mut self, pixels: &mut [u8]) -> Result<()> {
        match self {
            VideoEncoder::Ffmpeg { child, .. } => {
                child
                    .stdin
                    .as_mut()
                    .context("ffmpeg stdin closed")?
                    .write_all(pixels)
                    .context("failed piping frame to ffmpeg")?;
                Ok(())
            }
            VideoEncoder::Gif {
                encoder,
                width,
                height,
                delay,
                format,
                ..
            } => {
                if *format == PixelFormat::Bgra {
                    for px in pixels.chunks_exact_mut(4) {
                        px.swap(0, 2);
                    }
                }
                let mut frame = gif::Frame::from_rgba_speed(*width, *height, pixels, 10);
                frame.delay = *delay;
                encoder
                    .write_frame(&frame)
                    .context("failed writing GIF frame")?;
                Ok(())
            }
        }
    }

    /// Flush and close the file, returning its path.
    pub fn finish(self) -> Result<PathBuf> {
        match self {
            VideoEncoder::Ffmpeg { mut child, path } => {
                // Closing stdin tells ffmpeg the stream is over.
                drop(child.stdin.take());
                let status = child.wait().context("waiting for ffmpeg")?;
                anyhow::ensure!(status.success(), "ffmpeg exited with {}", status);
                Ok(path)
            }
            VideoEncoder::Gif { encoder, path, .. } => {
                drop(encoder); // flushes the trailer
                Ok(path)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The GIF fallback must always work, ffmpeg or not, so exercise it
    /// directly: encode two frames and check a plausible GIF comes out.
    #[test]
    fn gif_encoder_writes_a_valid_file() {
        let dir = std::env::temp_dir().join("ironsmith_video_test");
        std::fs::create_dir_all(&dir).unwrap();
        let base = dir.join("clip");
        let base_str = base.to_str().unwrap();

        let (w, h) = (8u32, 6u32);
        let path = PathBuf::from(format!("{}.gif", base_str));
        let file = File::create(&path).unwrap();
        let mut encoder = VideoEncoder::Gif {
            encoder: gif::Encoder::new(file, w as u16, h as u16, &[]).unwrap(),
            path: path.clone(),
            width: w as u16,
            height: h as u16,
            delay: 4,
            format: PixelFormat::Bgra,
        };

        let mut frame = vec![200u8; (w * h * 4) as usize];
        encoder.write_frame(&mut frame).unwrap();
        let mut frame2 = vec![20u8; (w * h * 4) as usize];
        encoder.write_frame(&mut frame2).unwrap();
        let out = encoder.finish().unwrap();

        let bytes = std::fs::read(&out).unwrap();
        assert!(bytes.starts_with(b"GIF89a"));
        let _ = std::fs::remove_file(out);
    }
}
