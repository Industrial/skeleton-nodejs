use axum::{response::Html, routing::get, Router};
use shuttle_axum::ShuttleAxum;
use streamweave::source::GlobalVariable;

async fn root() -> Html<&'static str> {
  Html("Hello, World!")
}

async fn global_variable_demo() -> Html<String> {
  let global_var = GlobalVariable::new("Hello from StreamWeave!".to_string());
  let mut stream = global_var.create_stream();

  let mut result = String::new();
  while let Some(value) = stream.next().await {
    result.push_str(&value);
  }

  Html(result)
}

#[shuttle_runtime::main]
async fn axum() -> ShuttleAxum {
  let router = Router::new()
    .route("/", get(root))
    .route("/global", get(global_variable_demo));

  Ok(router.into())
}
