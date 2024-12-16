pub mod streamweave;

use axum::{Router, response::Html, routing::post};
use leptos::*;
use leptos_axum::{LeptosRoutes, generate_route_list};
use std::path::Path;
use std::{convert::Infallible, net::SocketAddr, time::Duration};
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

#[cfg(feature = "ssr")]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  use crate::app::App;

  simple_logger::init_with_level(log::Level::Debug)?;

  // Get configuration from Cargo.toml
  let conf = get_configuration(Some("Cargo.toml")).await?;
  let leptos_options = conf.leptos_options;
  let addr = leptos_options.site_addr.parse::<SocketAddr>()?;

  // Handle WASM file renaming for compatibility
  let site_pkg_dir = format!(
    "{}/{}",
    &leptos_options.site_root, &leptos_options.site_pkg_dir
  );
  let wasm_path = format!("{}/{}.wasm", &site_pkg_dir, &leptos_options.output_name);
  let should_rename_wasm = Path::new(&wasm_path).exists();
  if should_rename_wasm {
    std::fs::rename(
      wasm_path,
      format!("{}/{}_bg.wasm", &site_pkg_dir, &leptos_options.output_name),
    )?;
  }

  // Generate routes for Leptos
  let routes = generate_route_list(App);

  // Build application with routes
  let app = Router::new()
    .route("/api/*fn_name", post(leptos_axum::handle_server_fns))
    .leptos_routes(&leptos_options, routes, App)
    .with_state(leptos_options);

  // Run the server
  println!("Server running on http://{}", addr);
  axum::Server::bind(&addr)
    .serve(app.into_make_service())
    .await?;

  Ok(())
}

#[cfg(not(feature = "ssr"))]
pub fn main() {
  // no client-side main function
  // unless we want this to work with e.g., Trunk for a purely client-side app
  // see lib.rs for hydration function instead
}
