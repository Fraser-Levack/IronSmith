/// One keyframe of the scripted camera animation. Angles are in radians
/// (the Haskell compiler converts from the degrees users write).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CamKey {
    pub t: f32,
    pub yaw: f32,
    pub pitch: f32,
    pub dist: f32,
    pub pan: [f32; 3],
}

pub struct Camera {
    pub yaw: f32,
    pub pitch: f32,
    pub dist: f32,
    pub pan_x: f32,
    pub pan_y: f32,
    pub pan_z: f32,

    target_yaw: f32,
    target_pitch: f32,
    target_dist: f32,
    target_pan_x: f32,
    target_pan_y: f32,
    target_pan_z: f32,

    pub auto_orbit: bool,

    // Scripted camera animation (CMD:SET_ANIM / CMD:ANIM_PLAY / CMD:ANIM_STOP)
    pub anim_track: Vec<CamKey>,
    pub anim_playing: bool,
    pub anim_time: f32,

    // NEW config fields
    pub bg_color: [f32; 3],
    pub shadow_enabled: bool,
    pub march_steps: i32,
    pub exposure: f32,
}

impl Camera {
    pub fn new() -> Self {
        Self {
            yaw: 0.0,
            pitch: 0.4,
            dist: 20.0,
            pan_x: 0.0,
            pan_y: 0.0,
            pan_z: 0.0,
            target_yaw: 0.0,
            target_pitch: 0.4,
            target_dist: 20.0,
            target_pan_x: 0.0,
            target_pan_y: 0.0,
            target_pan_z: 0.0,
            auto_orbit: true,
            anim_track: Vec::new(),
            anim_playing: false,
            anim_time: 0.0,
            // NEW
            bg_color: [0.02, 0.02, 0.05],
            shadow_enabled: true,
            march_steps: 150,
            exposure: 1.0,
        }
    }

    pub fn process_command(&mut self, cmd: &str) {
        match cmd {
            "CMD:OrbitMode" => self.auto_orbit = true,
            "CMD:StaticMode" => self.auto_orbit = false,
            "CMD:FlyMode" => self.auto_orbit = false,

            "CMD:PITCH_UP" => self.target_pitch += 0.1,
            "CMD:PITCH_DOWN" => self.target_pitch -= 0.1,
            "CMD:YAW_LEFT" => self.target_yaw -= 0.1,
            "CMD:YAW_RIGHT" => self.target_yaw += 0.1,

            "CMD:ZOOM_IN" => self.target_dist = (self.target_dist - 2.0).clamp(2.0, 100.0),
            "CMD:ZOOM_OUT" => self.target_dist = (self.target_dist + 2.0).clamp(2.0, 100.0),

            "CMD:PAN_FORWARD" => {
                self.target_pan_x -= self.target_yaw.sin() * 0.5;
                self.target_pan_z -= self.target_yaw.cos() * 0.5;
            }
            "CMD:PAN_BACKWARD" => {
                self.target_pan_x += self.target_yaw.sin() * 0.5;
                self.target_pan_z += self.target_yaw.cos() * 0.5;
            }
            "CMD:PAN_LEFT" => {
                self.target_pan_x -= self.target_yaw.cos() * 0.5;
                self.target_pan_z += self.target_yaw.sin() * 0.5;
            }
            "CMD:PAN_RIGHT" => {
                self.target_pan_x += self.target_yaw.cos() * 0.5;
                self.target_pan_z -= self.target_yaw.sin() * 0.5;
            }
            "CMD:PAN_UP" => self.target_pan_y += 0.5,
            "CMD:PAN_DOWN" => self.target_pan_y -= 0.5,

            "CMD:RESET_CAMERA" => {
                self.target_yaw = 0.0;
                self.target_pitch = 0.4;
                self.target_dist = 20.0;
                self.target_pan_x = 0.0;
                self.target_pan_y = 0.0;
                self.target_pan_z = 0.0;
            }

            "CMD:SHADOW_ON" => self.shadow_enabled = true,
            "CMD:SHADOW_OFF" => self.shadow_enabled = false,

            "CMD:ANIM_PLAY" => {
                self.anim_playing = true;
                self.anim_time = 0.0;
            }
            "CMD:ANIM_STOP" => self.anim_playing = false,

            _ => {
                if let Some(rest) = cmd.strip_prefix("CMD:SET_ANIM:") {
                    self.anim_track = parse_anim_track(rest);
                    if self.anim_track.is_empty() {
                        self.anim_playing = false;
                    }
                } else if let Some(rest) = cmd.strip_prefix("CMD:SET_BG:") {
                    let parts: Vec<f32> = rest
                        .split(',')
                        .filter_map(|s| s.trim().parse().ok())
                        .collect();
                    if parts.len() == 3 {
                        self.bg_color = [parts[0], parts[1], parts[2]];
                    }
                } else if let Some(d) = cmd
                    .strip_prefix("CMD:SET_CAMERA_DIST:")
                    .and_then(|s| s.trim().parse::<f32>().ok())
                {
                    self.target_dist = d;
                    self.dist = d;
                } else if let Some(n) = cmd
                    .strip_prefix("CMD:SET_MARCH_STEPS:")
                    .and_then(|s| s.trim().parse::<i32>().ok())
                {
                    self.march_steps = n;
                } else if let Some(e) = cmd
                    .strip_prefix("CMD:SET_EXPOSURE:")
                    .and_then(|s| s.trim().parse::<f32>().ok())
                {
                    self.exposure = e.clamp(0.0, 10.0);
                }
            }
        }
    }

    pub fn scroll_zoom(&mut self, y_delta: f32) {
        self.target_dist = (self.target_dist - y_delta * 2.0).clamp(2.0, 100.0);
    }

    /// Total length of the animation in seconds (time of the last keyframe).
    pub fn anim_duration(&self) -> f32 {
        self.anim_track.last().map(|k| k.t).unwrap_or(0.0)
    }

    /// Linearly interpolate the keyframe track at time `t` (clamped to the
    /// track's ends). Returns None when no track is loaded.
    pub fn sample_anim(&self, t: f32) -> Option<CamKey> {
        let first = self.anim_track.first()?;
        if t <= first.t {
            return Some(*first);
        }
        for pair in self.anim_track.windows(2) {
            let (a, b) = (pair[0], pair[1]);
            if t <= b.t {
                let span = b.t - a.t;
                let f = if span <= 0.0 { 1.0 } else { (t - a.t) / span };
                let lerp = |x: f32, y: f32| x + (y - x) * f;
                return Some(CamKey {
                    t,
                    yaw: lerp(a.yaw, b.yaw),
                    pitch: lerp(a.pitch, b.pitch),
                    dist: lerp(a.dist, b.dist),
                    pan: [
                        lerp(a.pan[0], b.pan[0]),
                        lerp(a.pan[1], b.pan[1]),
                        lerp(a.pan[2], b.pan[2]),
                    ],
                });
            }
        }
        self.anim_track.last().copied()
    }

    /// Snap the camera (current values and lerp targets alike, so stopping
    /// playback never causes a snap-back) to the animation pose at `t`.
    pub fn apply_anim(&mut self, t: f32) {
        if let Some(k) = self.sample_anim(t) {
            self.yaw = k.yaw;
            self.pitch = k.pitch;
            self.dist = k.dist;
            self.pan_x = k.pan[0];
            self.pan_y = k.pan[1];
            self.pan_z = k.pan[2];
            self.target_yaw = k.yaw;
            self.target_pitch = k.pitch;
            self.target_dist = k.dist;
            self.target_pan_x = k.pan[0];
            self.target_pan_y = k.pan[1];
            self.target_pan_z = k.pan[2];
        }
    }

    pub fn update_lerp(&mut self, dt: f32) {
        // Scripted animation playback overrides orbit and manual control.
        if self.anim_playing && !self.anim_track.is_empty() {
            let dur = self.anim_duration();
            self.anim_time += dt;
            if dur > 0.0 {
                self.anim_time %= dur;
            } else {
                self.anim_time = 0.0;
            }
            self.apply_anim(self.anim_time);
            return;
        }

        if self.auto_orbit {
            self.target_yaw -= 0.003;
        }

        self.target_pitch = self.target_pitch.clamp(-1.5, 1.5);

        // The Lerp Math
        self.yaw += (self.target_yaw - self.yaw) * 0.1;
        self.pitch += (self.target_pitch - self.pitch) * 0.1;
        self.dist += (self.target_dist - self.dist) * 0.1;

        self.pan_x += (self.target_pan_x - self.pan_x) * 0.1;
        self.pan_y += (self.target_pan_y - self.pan_y) * 0.1;
        self.pan_z += (self.target_pan_z - self.pan_z) * 0.1;
    }
}

/// Parse the CMD:SET_ANIM payload: semicolon-separated keyframes of seven
/// comma-separated floats (t, yaw, pitch, dist, tx, ty, tz). Malformed
/// keyframes are dropped; the result is sorted by time defensively.
fn parse_anim_track(data: &str) -> Vec<CamKey> {
    let mut keys: Vec<CamKey> = data
        .split(';')
        .filter_map(|kf| {
            let v: Vec<f32> = kf
                .split(',')
                .filter_map(|s| s.trim().parse().ok())
                .collect();
            if v.len() == 7 {
                Some(CamKey {
                    t: v[0],
                    yaw: v[1],
                    pitch: v[2],
                    dist: v[3],
                    pan: [v[4], v[5], v[6]],
                })
            } else {
                None
            }
        })
        .collect();
    keys.sort_by(|a, b| a.t.total_cmp(&b.t));
    keys
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zoom_commands_clamp_to_range() {
        let mut cam = Camera::new();
        for _ in 0..100 {
            cam.process_command("CMD:ZOOM_IN");
        }
        assert!((2.0..=100.0).contains(&cam.target_dist));

        for _ in 0..100 {
            cam.process_command("CMD:ZOOM_OUT");
        }
        assert!((2.0..=100.0).contains(&cam.target_dist));
    }

    #[test]
    fn scroll_zoom_clamps_to_range() {
        let mut cam = Camera::new();
        cam.scroll_zoom(1000.0);
        assert_eq!(cam.target_dist, 2.0);

        cam.scroll_zoom(-1000.0);
        assert_eq!(cam.target_dist, 100.0);
    }

    #[test]
    fn reset_camera_restores_defaults() {
        let mut cam = Camera::new();
        cam.process_command("CMD:ZOOM_OUT");
        cam.process_command("CMD:YAW_LEFT");
        cam.process_command("CMD:PAN_UP");

        cam.process_command("CMD:RESET_CAMERA");

        assert_eq!(cam.target_yaw, 0.0);
        assert_eq!(cam.target_pitch, 0.4);
        assert_eq!(cam.target_dist, 20.0);
        assert_eq!(cam.target_pan_x, 0.0);
        assert_eq!(cam.target_pan_y, 0.0);
        assert_eq!(cam.target_pan_z, 0.0);
    }

    #[test]
    fn shadow_toggle_commands() {
        let mut cam = Camera::new();
        cam.process_command("CMD:SHADOW_OFF");
        assert!(!cam.shadow_enabled);
        cam.process_command("CMD:SHADOW_ON");
        assert!(cam.shadow_enabled);
    }

    #[test]
    fn set_bg_command_parses_three_floats() {
        let mut cam = Camera::new();
        cam.process_command("CMD:SET_BG:0.1, 0.2, 0.3");
        assert_eq!(cam.bg_color, [0.1, 0.2, 0.3]);
    }

    #[test]
    fn set_bg_command_ignores_malformed_input() {
        let mut cam = Camera::new();
        let before = cam.bg_color;
        cam.process_command("CMD:SET_BG:not,enough");
        assert_eq!(cam.bg_color, before);
    }

    #[test]
    fn set_march_steps_command_parses_int() {
        let mut cam = Camera::new();
        cam.process_command("CMD:SET_MARCH_STEPS:42");
        assert_eq!(cam.march_steps, 42);
    }

    #[test]
    fn set_exposure_command_parses_and_clamps() {
        let mut cam = Camera::new();
        cam.process_command("CMD:SET_EXPOSURE:1.8");
        assert_eq!(cam.exposure, 1.8);

        cam.process_command("CMD:SET_EXPOSURE:999");
        assert_eq!(cam.exposure, 10.0);

        cam.process_command("CMD:SET_EXPOSURE:garbage");
        assert_eq!(cam.exposure, 10.0); // unchanged
    }

    #[test]
    fn set_camera_dist_updates_both_dist_and_target() {
        let mut cam = Camera::new();
        cam.process_command("CMD:SET_CAMERA_DIST:33.5");
        assert_eq!(cam.dist, 33.5);
        assert_eq!(cam.target_dist, 33.5);
    }

    #[test]
    fn update_lerp_moves_towards_target() {
        let mut cam = Camera::new();
        cam.auto_orbit = false;
        cam.target_dist = 30.0;
        cam.update_lerp(0.016);
        // Lerp by 0.1 towards target: 20 + (30-20)*0.1 = 21
        assert!((cam.dist - 21.0).abs() < 1e-5);
    }

    #[test]
    fn update_lerp_clamps_pitch() {
        let mut cam = Camera::new();
        cam.target_pitch = 10.0;
        cam.update_lerp(0.016);
        assert!(cam.target_pitch <= 1.5);
    }

    #[test]
    fn set_anim_command_parses_keyframes() {
        let mut cam = Camera::new();
        cam.process_command(
            "CMD:SET_ANIM:0.0,0.0,0.4,20.0,0.0,0.0,0.0;2.0,6.28,0.4,10.0,1.0,2.0,3.0",
        );
        assert_eq!(cam.anim_track.len(), 2);
        assert_eq!(cam.anim_track[1].dist, 10.0);
        assert_eq!(cam.anim_track[1].pan, [1.0, 2.0, 3.0]);
        assert_eq!(cam.anim_duration(), 2.0);
    }

    #[test]
    fn set_anim_sorts_out_of_order_keyframes_and_drops_malformed() {
        let mut cam = Camera::new();
        cam.process_command(
            "CMD:SET_ANIM:2.0,1.0,0.0,10.0,0.0,0.0,0.0;garbage;0.0,0.0,0.0,20.0,0.0,0.0,0.0",
        );
        assert_eq!(cam.anim_track.len(), 2);
        assert_eq!(cam.anim_track[0].t, 0.0);
        assert_eq!(cam.anim_track[1].t, 2.0);
    }

    #[test]
    fn empty_anim_track_stops_playback() {
        let mut cam = Camera::new();
        cam.process_command(
            "CMD:SET_ANIM:0.0,0.0,0.0,20.0,0.0,0.0,0.0;2.0,1.0,0.0,10.0,0.0,0.0,0.0",
        );
        cam.process_command("CMD:ANIM_PLAY");
        assert!(cam.anim_playing);
        cam.process_command("CMD:SET_ANIM:");
        assert!(!cam.anim_playing);
        assert!(cam.anim_track.is_empty());
    }

    #[test]
    fn sample_anim_interpolates_linearly() {
        let mut cam = Camera::new();
        cam.process_command(
            "CMD:SET_ANIM:0.0,0.0,0.0,20.0,0.0,0.0,0.0;2.0,2.0,1.0,10.0,4.0,0.0,0.0",
        );
        let k = cam.sample_anim(1.0).unwrap();
        assert!((k.yaw - 1.0).abs() < 1e-5);
        assert!((k.pitch - 0.5).abs() < 1e-5);
        assert!((k.dist - 15.0).abs() < 1e-5);
        assert!((k.pan[0] - 2.0).abs() < 1e-5);
    }

    #[test]
    fn sample_anim_clamps_to_track_ends() {
        let mut cam = Camera::new();
        cam.process_command(
            "CMD:SET_ANIM:1.0,0.0,0.0,20.0,0.0,0.0,0.0;2.0,1.0,0.0,10.0,0.0,0.0,0.0",
        );
        assert_eq!(cam.sample_anim(0.0).unwrap().dist, 20.0);
        assert_eq!(cam.sample_anim(99.0).unwrap().dist, 10.0);
    }

    #[test]
    fn playback_loops_and_overrides_manual_control() {
        let mut cam = Camera::new();
        cam.process_command(
            "CMD:SET_ANIM:0.0,0.0,0.0,20.0,0.0,0.0,0.0;1.0,1.0,0.0,10.0,0.0,0.0,0.0",
        );
        cam.process_command("CMD:ANIM_PLAY");
        cam.update_lerp(0.5);
        assert!((cam.yaw - 0.5).abs() < 1e-5);
        // Advancing past the end wraps around: 0.5 + 0.75 = 1.25 -> 0.25
        cam.update_lerp(0.75);
        assert!((cam.anim_time - 0.25).abs() < 1e-5);
        // Stopping keeps the pose (targets match current, so no snap-back)
        cam.process_command("CMD:ANIM_STOP");
        assert_eq!(cam.yaw, cam.target_yaw);
    }
}
