use futures::{Stream, stream};
use std::pin::Pin;

pub trait Source {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = String> + Send>>;
}

pub struct StaticSource {
  data: Vec<String>,
}

impl StaticSource {
  pub fn new(data: Vec<String>) -> Self {
    StaticSource { data }
  }
}

impl Source for StaticSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = String> + Send>> {
    let stream = stream::iter(self.data.clone());
    Box::pin(stream)
  }
}
