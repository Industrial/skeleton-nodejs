pub mod streamweave;

use axum::{Router, response::Html, routing::get};
use shuttle_axum::ShuttleAxum;
use std::{convert::Infallible, time::Duration};
use streamweave::operator::{Operator, map::MapOperator};
use streamweave::sink::{Sink, http_response::HttpResponseSink};
use streamweave::source::{Source, timeout::TimeoutSource, vec::VecSource};

async fn root() -> Html<&'static str> {
  Html("Hello, World!")
}

#[axum::debug_handler]
async fn timeout_example() -> Result<Html<String>, Infallible> {
  let source = TimeoutSource::new(Duration::from_secs(3), "Delayed Message");
  let sink = HttpResponseSink::new();

  let response = sink.run(source.stream());

  Ok(response)
}

#[axum::debug_handler]
async fn stream_example() -> Result<Html<String>, Infallible> {
  let source = VecSource::new(vec![
    "Hello".to_string(),
    "from".to_string(),
    "Rust".to_string(),
    "Streams!".to_string(),
  ]);

  let map = MapOperator::new(|s: String| s.to_uppercase());
  let sink = HttpResponseSink::new();

  let operated_stream = map.apply(source.stream());
  let response = sink.run(operated_stream);

  Ok(response)
}

#[shuttle_runtime::main]
async fn axum() -> ShuttleAxum {
  let router = Router::new()
    .route("/", get(root))
    .route("/stream", get(stream_example))
    .route("/timeout", get(timeout_example));

  Ok(router.into())
}
