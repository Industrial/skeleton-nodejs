pub mod streamweave;

use axum::{Router, response::Html, routing::get};
use futures::stream::StreamExt;
use shuttle_axum::ShuttleAxum;
use std::convert::Infallible;
use streamweave::operator::Operator;
use streamweave::operator::map::Map;
use streamweave::source::{Source, StaticSource};

async fn root() -> Html<&'static str> {
  Html("Hello, World!")
}

#[axum::debug_handler]
async fn stream_example() -> Result<Html<String>, Infallible> {
  let source = StaticSource::new(vec![
    "Hello".to_string(),
    "from".to_string(),
    "Rust".to_string(),
    "Streams!".to_string(),
  ]);

  let map = Map::new(|s: String| s.to_uppercase());

  let operated_stream = map.apply(source.stream());

  let mut output = String::new();
  let mut stream = operated_stream;

  while let Some(data) = stream.next().await {
    output.push_str(&data);
    output.push(' ');
  }

  Ok(Html(output.trim().to_string()))
}

#[shuttle_runtime::main]
async fn axum() -> ShuttleAxum {
  let router = Router::new()
    .route("/", get(root))
    .route("/stream", get(stream_example));

  Ok(router.into())
}
