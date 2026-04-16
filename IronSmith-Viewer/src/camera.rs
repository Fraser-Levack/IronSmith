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
}

impl Camera {
    pub fn new() -> Self {
        Self {
            yaw: 0.0, pitch: 0.4, dist: 20.0,
            pan_x: 0.0, pan_y: 0.0, pan_z: 0.0,
            target_yaw: 0.0, target_pitch: 0.4, target_dist: 20.0,
            target_pan_x: 0.0, target_pan_y: 0.0, target_pan_z: 0.0,
            auto_orbit: true,
        }
    }

    pub fn process_command(&mut self, cmd: &str) {
        match cmd {
            "CMD:OrbitMode"  => self.auto_orbit = true,
            "CMD:StaticMode" => self.auto_orbit = false,
            "CMD:FlyMode"    => self.auto_orbit = false, 
            
            "CMD:PITCH_UP"   => self.target_pitch += 0.1,
            "CMD:PITCH_DOWN" => self.target_pitch -= 0.1,
            "CMD:YAW_LEFT"   => self.target_yaw -= 0.1,
            "CMD:YAW_RIGHT"  => self.target_yaw += 0.1,
            
            "CMD:ZOOM_IN"    => self.target_dist = (self.target_dist - 2.0).clamp(2.0, 100.0),
            "CMD:ZOOM_OUT"   => self.target_dist = (self.target_dist + 2.0).clamp(2.0, 100.0),
            
            "CMD:PAN_FORWARD" => {
                self.target_pan_x -= self.target_yaw.sin() * 0.5;
                self.target_pan_z -= self.target_yaw.cos() * 0.5;
            },
            "CMD:PAN_BACKWARD" => {
                self.target_pan_x += self.target_yaw.sin() * 0.5;
                self.target_pan_z += self.target_yaw.cos() * 0.5;
            },
            "CMD:PAN_LEFT" => {
                self.target_pan_x -= self.target_yaw.cos() * 0.5;
                self.target_pan_z += self.target_yaw.sin() * 0.5;
            },
            "CMD:PAN_RIGHT" => {
                self.target_pan_x += self.target_yaw.cos() * 0.5;
                self.target_pan_z -= self.target_yaw.sin() * 0.5;
            },
            "CMD:PAN_UP"   => self.target_pan_y += 0.5,
            "CMD:PAN_DOWN" => self.target_pan_y -= 0.5,

            "CMD:RESET_CAMERA" => {
                self.target_yaw = 0.0;
                self.target_pitch = 0.4;
                self.target_dist = 20.0;
                self.target_pan_x = 0.0;
                self.target_pan_y = 0.0;
                self.target_pan_z = 0.0;
            },
            _ => {}
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