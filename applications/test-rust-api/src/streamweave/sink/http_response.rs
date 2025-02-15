use crate::streamweave::sink::Sink;
use axum::response::Html;
use futures::stream::{Stream, StreamExt};
use std::pin::Pin;

pub struct HttpResponseSink;

impl HttpResponseSink {
  pub fn new() -> Self {
    HttpResponseSink
  }
}

impl Sink<String, Html<String>> for HttpResponseSink {
  fn run(
    self,
    input: Pin<Box<dyn Stream<Item = String> + Send>>,
  ) -> Html<String> {
    let output = futures::executor::block_on(async {
      let mut output = String::new();
      let mut stream = input;
      while let Some(data) = stream.next().await {
        output.push_str(&data);
        // output.push(' ');
      }
      output.trim().to_string()
    });

    Html(output)
  }
}
