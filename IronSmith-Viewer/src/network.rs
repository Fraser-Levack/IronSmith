use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::time::Duration;

pub enum NetMessage {
    Command(String),
    Bytecode(Vec<f32>),
}

/// A message plus the connection it arrived on. Most senders (the TUI)
/// close their socket right after writing, so replies are optional: a
/// client that wants one (the CLI's `render`) half-closes its write side
/// and keeps reading until the viewer answers with `reply`.
pub struct Incoming {
    pub message: NetMessage,
    pub stream: TcpStream,
}

/// Send a one-line reply (`OK ...` / `ERROR ...`) and close the connection.
/// Errors are ignored: the client may not be listening.
pub fn reply(mut stream: TcpStream, line: &str) {
    let _ = stream.write_all(line.as_bytes());
    let _ = stream.write_all(b"\n");
}

pub fn drain_sockets(listener: &TcpListener) -> Vec<Incoming> {
    let mut messages = Vec::new();

    while let Ok((mut stream, _)) = listener.accept() {
        let _ = stream.set_nonblocking(false);
        // Senders (the Haskell TUI) connect, write, and close immediately,
        // so data normally arrives within a millisecond. The timeout only
        // caps how long we wait when it hasn't: 1ms was so tight it raced
        // the sender and randomly dropped whole commands. 250ms is still a
        // bounded worst-case stall but makes local delivery reliable.
        let _ = stream.set_read_timeout(Some(Duration::from_millis(250)));

        let mut buffer = Vec::new();
        let _ = stream.read_to_end(&mut buffer);

        if buffer.is_empty() {
            continue;
        }
        let message = if buffer.starts_with(b"CMD:") {
            match String::from_utf8(buffer) {
                Ok(cmd_str) => NetMessage::Command(cmd_str.trim().to_string()),
                Err(_) => continue,
            }
        } else if buffer.len() % 4 == 0 {
            // Decode explicitly: bytemuck::cast_slice would panic (= abort
            // in release) if the Vec<u8> allocation weren't 4-byte aligned.
            let floats = buffer
                .chunks_exact(4)
                .map(|b| f32::from_le_bytes([b[0], b[1], b[2], b[3]]))
                .collect();
            NetMessage::Bytecode(floats)
        } else {
            continue;
        };
        messages.push(Incoming { message, stream });
    }
    messages
}
