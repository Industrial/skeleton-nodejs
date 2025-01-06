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

#[cfg(test)]
mod tests {
  use super::*;
  use futures::stream::StreamExt;
  use proptest::prelude::*;
  use tokio_test::block_on;

  proptest! {
      // Testing with successful command execution
      #[test]
      fn test_command_source_yields_output_for_valid_command(command in "\\PC+", args in proptest::collection::vec("\\PC*", 0..10)) {
          block_on(async {
              let source = CommandSource::new(command, args);
              let mut stream = source.stream();

              // Assuming command and args are valid, check output
              while let Some(line) = stream.next().await {
                  prop_assert!(line.contains("success") || line.contains("non-zero exit status"));
              }

              Ok(())
          }).unwrap();
      }

      // Testing handling of non-zero exit status
      #[test]
      fn test_command_source_handles_non_zero_exit_status(command in "\\PC+", args in proptest::collection::vec("\\PC*", 0..10)) {
          block_on(async {
              let source = CommandSource::new(command, args);
              let mut stream = source.stream();

              // Expect error message due to invalid command producing non-zero exit status
              let mut found_error = false;
              while let Some(line) = stream.next().await {
                  if line.contains("non-zero exit status") {
                      found_error = true;
                      break;
                  }
              }

              prop_assert!(found_error);

              Ok(())
          }).unwrap();
      }

      // Testing execution failure
      #[test]
      fn test_command_source_handles_command_execution_failure(command in "[\\\\]*", args in proptest::collection::vec("[\\\\]*", 0..10)) {
          block_on(async {
              let source = CommandSource::new(command, args);
              let mut stream = source.stream();

              let mut found_execution_fail = false;
              while let Some(line) = stream.next().await {
                  if line.starts_with("Failed to execute command") {
                      found_execution_fail = true;
                      break;
                  }
              }

              prop_assert!(found_execution_fail);

              Ok(())
          }).unwrap();
      }
  }
}
