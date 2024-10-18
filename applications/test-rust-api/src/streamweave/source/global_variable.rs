use futures::stream::{Stream, StreamExt};
use wasm_streams::ReadableStream;

pub struct GlobalVariable {
  value: String,
}

impl GlobalVariable {
  pub fn new(value: String) -> Self {
    GlobalVariable { value }
  }

  pub fn create_stream(&self) -> ReadableStream {
    let data_stream = futures::stream::once(async { self.value.clone() });

    wasm_streams::ReadableStream::from_stream(data_stream)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[tokio::test]
  async fn test_global_variable_stream() {
    let global_var = GlobalVariable::new("test".to_string());
    let mut stream = global_var.create_stream();

    let result = stream.next().await;
    assert_eq!(result, Some("test".to_string()));
  }
}
