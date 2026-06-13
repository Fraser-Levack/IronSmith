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

    // NEW config fields
    pub bg_color: [f32; 3],
    pub shadow_enabled: bool,
    pub march_steps: i32,
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
            // NEW
            bg_color: [0.02, 0.02, 0.05],
            shadow_enabled: true,
            march_steps: 150,
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

            _ => {
                if let Some(rest) = cmd.strip_prefix("CMD:SET_BG:") {
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
                }
            }
        }
    }

    pub fn scroll_zoom(&mut self, y_delta: f32) {
        self.target_dist = (self.target_dist - y_delta * 2.0).clamp(2.0, 100.0);
    }

    pub fn update_lerp(&mut self) {
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
        cam.update_lerp();
        // Lerp by 0.1 towards target: 20 + (30-20)*0.1 = 21
        assert!((cam.dist - 21.0).abs() < 1e-5);
    }

    #[test]
    fn update_lerp_clamps_pitch() {
        let mut cam = Camera::new();
        cam.target_pitch = 10.0;
        cam.update_lerp();
        assert!(cam.target_pitch <= 1.5);
    }
}
