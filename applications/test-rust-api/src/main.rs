use axum::{Router, routing::get};

async fn hello_world() -> &'static str {
  "Hello world!"
}

fn init_router() -> Router {
  Router::new().route("/", get(hello_world))
}

// fn main() {
//     println!("Hello, world!");
// }
