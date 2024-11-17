use crate::streamweave::source::Source;
use futures::{stream, Stream};
use std::env;
use std::pin::Pin;

// Streams values of environment variables as strings.
pub struct EnvVarSource {
  var_name: String,
}

impl EnvVarSource {
  pub fn new(var_name: impl Into<String>) -> Self {
    EnvVarSource {
      var_name: var_name.into(),
    }
  }
}

impl Source<String> for EnvVarSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = String> + Send>> {
    let var_name = self.var_name.clone();

    let value = match env::var(&var_name) {
      Ok(val) => vec![val],
      Err(err) => vec![format!("Error accessing env var {}: {}", var_name, err)],
    };

    Box::pin(stream::iter(value))
  }
}
