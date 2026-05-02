use std::io::Read;
use std::net::TcpListener;
use std::time::Duration;

pub enum NetMessage {
    Command(String),
    Bytecode(Vec<f32>), // NEW: Replaced Shader(String) with Bytecode(Vec<f32>)
}

pub fn drain_sockets(listener: &TcpListener) -> Vec<NetMessage> {
    let mut messages = Vec::new();
    
    while let Ok((mut stream, _)) = listener.accept() {
        // We set blocking to true with a tiny timeout so `read_to_end` safely 
        // grabs the entire payload sent by Haskell before closing.
        let _ = stream.set_nonblocking(false);
        let _ = stream.set_read_timeout(Some(Duration::from_millis(50)));
        
        let mut buffer = Vec::new();
        let _ = stream.read_to_end(&mut buffer);
        
        if !buffer.is_empty() {
            // Check if the payload starts with the raw bytes for "CMD:"
            if buffer.starts_with(b"CMD:") {
                if let Ok(cmd_str) = String::from_utf8(buffer) {
                    messages.push(NetMessage::Command(cmd_str.trim().to_string()));
                }
            } else {
                // If it's not a command, it's our raw float bytecode!
                // Safety check: ensure the byte count is a perfect multiple of 4 (32-bit floats)
                if buffer.len() % 4 == 0 {
                    let floats: &[f32] = bytemuck::cast_slice(&buffer);
                    messages.push(NetMessage::Bytecode(floats.to_vec()));
                }
            }
        }
    }
    
    messages
}