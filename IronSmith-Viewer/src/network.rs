use std::io::Read;
use std::net::TcpListener;
use std::time::Duration;

pub enum NetMessage {
    Command(String),
    Shader(String),
}

pub fn drain_sockets(listener: &TcpListener) -> Vec<NetMessage> {
    let mut messages = Vec::new();
    
    while let Ok((mut stream, _)) = listener.accept() {
        let _ = stream.set_nonblocking(false);
        let _ = stream.set_read_timeout(Some(Duration::from_millis(50)));
        
        let mut buffer = String::new();
        if stream.read_to_string(&mut buffer).is_ok() {
            if buffer.starts_with("CMD:") {
                messages.push(NetMessage::Command(buffer.trim().to_string()));
            } else if buffer.contains("map(") {
                // For shaders, we usually only care about the latest one, 
                // but we'll return it so the main thread decides what to keep
                messages.push(NetMessage::Shader(buffer));
            }
        }
    }
    
    messages
}