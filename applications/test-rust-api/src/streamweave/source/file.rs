use crate::streamweave::source::Source;
use futures::Stream;
use std::path::PathBuf;
use std::pin::Pin;
use tokio::fs::File;
use tokio::io::{AsyncBufReadExt, BufReader};

// Streams lines from a file asynchronously.
pub struct FileSource {
  path: PathBuf,
}

impl FileSource {
  pub fn new(path: impl Into<PathBuf>) -> Self {
    FileSource { path: path.into() }
  }
}

impl Source<String> for FileSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = String> + Send>> {
    let path = self.path.clone();

    Box::pin(async_stream::stream! {
        let file = match File::open(path).await {
            Ok(file) => file,
            Err(e) => {
                yield format!("Failed to open file: {}", e);
                return;
            },
        };

        let reader = BufReader::new(file);
        let mut lines = reader.lines();

        while let Ok(Some(line)) = lines.next_line().await {
            yield line;
        }
    })
  }
}
