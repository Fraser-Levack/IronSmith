use std::io::Read;
use std::net::TcpListener;
use std::time::Duration;

pub enum NetMessage {
    Command(String),
    Bytecode(Vec<f32>), 
}

pub fn drain_sockets(listener: &TcpListener) -> Vec<NetMessage> {
    let mut messages = Vec::new();
    
    while let Ok((mut stream, _)) = listener.accept() {
        let _ = stream.set_nonblocking(false);
        let _ = stream.set_read_timeout(Some(Duration::from_millis(1)));
        
        let mut buffer = Vec::new();
        let _ = stream.read_to_end(&mut buffer);
        
        if !buffer.is_empty() {
            if buffer.starts_with(b"CMD:") {
                if let Ok(cmd_str) = String::from_utf8(buffer) {
                    messages.push(NetMessage::Command(cmd_str.trim().to_string()));
                }
            } else if buffer.len() % 4 == 0 {
                let floats: &[f32] = bytemuck::cast_slice(&buffer);
                messages.push(NetMessage::Bytecode(floats.to_vec()));
            }
        }
    }
    messages
}