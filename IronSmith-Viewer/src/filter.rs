use std::path::Path;
use std::sync::Arc;
use std::sync::mpsc;

/// Wraps a user filter body (which must define `vec4 apply(vec2 uv)`) in the
/// uniforms, scene sampler, and entry point shared by every filter pass.
const FILTER_TEMPLATE: &str = r#"
#version 450
layout(set = 0, binding = 0) uniform Uniforms {
    vec2 u_resolution;
    float u_time;
    float u_camera_dist;
    vec2 u_rotation;
    float u_shadow_enabled;
    float u_march_steps;
    vec4 u_target_pos;
    vec4 u_bg_color;
    vec4 u_exposure;
};
layout(set = 0, binding = 1) uniform texture2D u_scene_tex;
layout(set = 0, binding = 2) uniform sampler u_scene_smp;

layout(location = 0) out vec4 out_color;

vec4 scene(vec2 uv) {
    return texture(sampler2D(u_scene_tex, u_scene_smp), clamp(uv, vec2(0.0), vec2(1.0)));
}
float luma(vec3 c) { return dot(c, vec3(0.299, 0.587, 0.114)); }

{FILTER_BODY}

void main() {
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    out_color = apply(uv);
}
"#;

/// The identity filter, used when no filter is active and installed as a
/// documented starting point for people writing their own.
pub const PASSTHROUGH: &str = r#"// IronSmith filter: passthrough
// ------------------------------------------------------------------
// A filter is a GLSL snippet that defines one function:
//
//     vec4 apply(vec2 uv)
//
// It receives the screen coordinate (0..1) and returns the final
// pixel colour. Available helpers and uniforms:
//
//     scene(uv)     -> vec4  the rendered scene at uv
//     luma(rgb)     -> float perceptual brightness of a colour
//     u_resolution  -> vec2  window size in pixels
//     u_time        -> float seconds since the viewer launched
//
// Copy this file, rename it, and go wild. If your filter fails to
// compile the viewer just keeps the last working one.
// ------------------------------------------------------------------

vec4 apply(vec2 uv) {
    return scene(uv);
}
"#;

const EDGE_DETECTION: &str = r#"// IronSmith filter: edge_detection
// Sobel edge detection: traces the model's silhouette and creases
// in glowing lines over a dimmed scene.

vec4 apply(vec2 uv) {
    vec2 px = 1.0 / u_resolution;

    float tl = luma(scene(uv + px * vec2(-1.0,  1.0)).rgb);
    float tc = luma(scene(uv + px * vec2( 0.0,  1.0)).rgb);
    float tr = luma(scene(uv + px * vec2( 1.0,  1.0)).rgb);
    float ml = luma(scene(uv + px * vec2(-1.0,  0.0)).rgb);
    float mr = luma(scene(uv + px * vec2( 1.0,  0.0)).rgb);
    float bl = luma(scene(uv + px * vec2(-1.0, -1.0)).rgb);
    float bc = luma(scene(uv + px * vec2( 0.0, -1.0)).rgb);
    float br = luma(scene(uv + px * vec2( 1.0, -1.0)).rgb);

    float gx = (tr + 2.0 * mr + br) - (tl + 2.0 * ml + bl);
    float gy = (tl + 2.0 * tc + tr) - (bl + 2.0 * bc + br);
    float edge = clamp(length(vec2(gx, gy)) * 2.0, 0.0, 1.0);

    vec3 base = scene(uv).rgb * 0.15;
    return vec4(mix(base, vec3(0.85, 0.9, 1.0), edge), 1.0);
}
"#;

const HEATMAP: &str = r#"// IronSmith filter: heatmap
// Maps scene brightness onto a thermal-camera colour ramp:
// cold blue for dark pixels through green and yellow to hot red.

vec3 heat(float t) {
    return clamp(vec3(
        1.5 - abs(4.0 * t - 3.0),
        1.5 - abs(4.0 * t - 2.0),
        1.5 - abs(4.0 * t - 1.0)
    ), 0.0, 1.0);
}

vec4 apply(vec2 uv) {
    float t = luma(scene(uv).rgb);
    return vec4(heat(clamp(t, 0.0, 1.0)), 1.0);
}
"#;

const ASCII: &str = r#"// IronSmith filter: ascii
// Redraws the scene as live text. Each cell picks a glyph
// by brightness from " .:*o&8@#", so the model morphs through
// characters in real time as it moves.
// Glyphs are 5x5 bitmaps packed into integers (movAX13h's technique).

float character(int n, vec2 p) {
    p = floor(p * vec2(-4.0, 4.0) + 2.5);
    if (clamp(p.x, 0.0, 4.0) == p.x && clamp(p.y, 0.0, 4.0) == p.y) {
        int a = int(round(p.x) + 5.0 * round(p.y));
        if (((n >> a) & 1) == 1) return 1.0;
    }
    return 0.0;
}

vec4 apply(vec2 uv) {
    vec2 pix = uv * u_resolution;
    
    // --- SIZE ADJUSTMENT ---
    // Change this value to make the text larger or smaller.
    // Original was 8.0. Set to 16.0 for double size, 32.0 for quadruple.
    float size = 32.0;
    float half_size = size / 2.0;

    // Use the dynamic size for the cell layout and centering
    vec2 cell = floor(pix / size) * size + half_size;
    vec3 col = scene(cell / u_resolution).rgb;
    float gray = luma(col);

    int n = 4096;                 // .
    if (gray > 0.2) n = 65600;    // :
    if (gray > 0.3) n = 332772;   // *
    if (gray > 0.4) n = 15255086; // o
    if (gray > 0.5) n = 23385164; // &
    if (gray > 0.6) n = 15252014; // 8
    if (gray > 0.7) n = 13199452; // @
    if (gray > 0.8) n = 11512810; // #

    // Use half_size to map the local grid coordinates for the 5x5 bitmap
    vec2 p = mod(pix / half_size, 2.0) - vec2(1.0);
    return vec4(col * character(n, p), 1.0);
}
"#;

/// The example filters shipped with the viewer, written into the user's
/// filters directory on startup so they can be used, copied, and edited.
pub const BUILTIN_FILTERS: &[(&str, &str)] = &[
    ("passthrough", PASSTHROUGH),
    ("edge_detection", EDGE_DETECTION),
    ("heatmap", HEATMAP),
    ("ascii", ASCII),
];

/// Write the built-in example filters into `dir`, skipping any file that
/// already exists so user edits are never clobbered.
pub fn install_default_filters(dir: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dir)?;
    for (name, body) in BUILTIN_FILTERS {
        let path = dir.join(format!("{}.glsl", name));
        if !path.exists() {
            std::fs::write(path, body)?;
        }
    }
    Ok(())
}

pub fn compose_filter_source(body: &str) -> String {
    FILTER_TEMPLATE.replace("{FILTER_BODY}", body)
}

/// Compile a filter body into a render pipeline on a background thread,
/// mirroring `shader::compile_pipeline_async`: a filter that fails to
/// compile sends nothing, so the renderer keeps the last working one.
/// Rejections are logged to the forge log for filter authors.
pub fn compile_filter_pipeline_async(
    device: Arc<wgpu::Device>,
    layout: Arc<wgpu::PipelineLayout>,
    format: wgpu::TextureFormat,
    body: String,
    tx: mpsc::Sender<wgpu::RenderPipeline>,
    log_path: std::path::PathBuf,
) {
    std::thread::spawn(move || {
        let full_source = compose_filter_source(&body);

        match crate::shader::build_pipeline_checked(
            &device,
            &layout,
            format,
            "IronSmith Filter",
            &full_source,
        ) {
            Ok(pipeline) => {
                let _ = tx.send(pipeline);
            }
            Err(e) => crate::shader::log_line(&log_path, &format!("Filter shader rejected: {}", e)),
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_builtin_filters_compile() {
        for (name, body) in BUILTIN_FILTERS {
            crate::shader::parse_glsl_fragment(&compose_filter_source(body))
                .unwrap_or_else(|e| panic!("filter '{}' failed to compile: {}", name, e));
        }
    }

    #[test]
    fn broken_filter_source_errors_instead_of_panicking() {
        let result = crate::shader::parse_glsl_fragment(&compose_filter_source(
            "this is not valid glsl {{{",
        ));
        assert!(result.is_err());
    }

    #[test]
    fn filter_missing_apply_function_errors() {
        let result = crate::shader::parse_glsl_fragment(&compose_filter_source(
            "vec4 not_apply(vec2 uv) { return scene(uv); }",
        ));
        assert!(result.is_err());
    }
}
