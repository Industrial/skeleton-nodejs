use crate::streamweave::source::Source;
use futures::Stream;
use std::pin::Pin;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;

// Uses std::process::Command to stream output of shell commands.
pub struct CommandSource {
  command: String,
  args: Vec<String>,
}

impl CommandSource {
  pub fn new(
    command: impl Into<String>,
    args: Vec<String>,
  ) -> Self {
    CommandSource {
      command: command.into(),
      args,
    }
  }
}

impl Source<String> for CommandSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = String> + Send>> {
    let command = self.command.clone();
    let args = self.args.clone();

    Box::pin(async_stream::stream! {
      let mut cmd = Command::new(command);
      cmd.args(&args);

      let output = match cmd.output().await {
        Ok(output) => output,
        Err(e) => {
          yield format!("Failed to execute command: {}", e);
          return;
        },
      };

      if !output.status.success() {
        yield "Command returned non-zero exit status".to_string();
        return;
      }

      let stdout = BufReader::new(output.stdout.as_slice());
      let mut lines = stdout.lines();

      while let Some(line) = lines.next_line().await.transpose() {
        match line {
          Ok(line) => yield line,
          Err(e) => {
            yield format!("Error reading line: {}", e);
            break;
          }
        }
      }
    })
  }
}
