use crate::streamweave::source::Source;
use futures::Stream;
use std::net::SocketAddr;
use std::pin::Pin;
use tokio::io::{AsyncReadExt, BufReader};
use tokio::net::TcpStream;

// Reads data from a TCP stream, producing a stream of data chunks.
pub struct TcpStreamSource {
  addr: SocketAddr,
}

impl TcpStreamSource {
  pub fn new(addr: SocketAddr) -> Self {
    TcpStreamSource { addr }
  }
}

impl Source<Vec<u8>> for TcpStreamSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = Vec<u8>> + Send>> {
    let addr = self.addr;

    Box::pin(async_stream::stream! {
        match TcpStream::connect(addr).await {
            Ok(stream) => {
                let mut reader = BufReader::new(stream);
                let mut buf = vec![0; 1024]; // Buffer size for reads
                loop {
                    match reader.read(&mut buf).await {
                        Ok(0) => break, // EOF reached
                        Ok(n) => yield buf[..n].to_vec(),
                        Err(e) => {
                            eprintln!("Error reading from stream: {}", e);
                            break;
                        }
                    }
                }
            },
            Err(e) => {
                eprintln!("Failed to connect: {}", e);
            }
        }
    })
  }
}
