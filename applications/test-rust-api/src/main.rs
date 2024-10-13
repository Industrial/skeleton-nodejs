// use axum::{routing::get, Router};
// use tokio::net::TcpListener;
// #[tokio::main]
// async fn main() {
//   let app = Router::new().route("/", get(root));
//   let listener = TcpListener::bind("0.0.0.0:3000").await.unwrap();
//   axum::serve(listener, app).await.unwrap();
// }
// async fn root() -> &'static str {
//   "Hello, World!"
// }

use axum::{routing::get, Router};
use shuttle_axum::ShuttleAxum;
// use shuttle_runtime::Error;

async fn root() -> &'static str {
  "Hello, World!"
}

#[shuttle_runtime::main]
async fn axum() -> ShuttleAxum {
  let router = Router::new().route("/", get(root));

  Ok(router.into())
}
