use crate::streamweave::source::Source;
use futures::{stream, Stream};
use std::collections::HashSet;
use std::pin::Pin;

// Streams elements from a HashSet.
pub struct HashSetSource<T> {
  data: HashSet<T>,
}

impl<T> HashSetSource<T> {
  pub fn new(data: HashSet<T>) -> Self {
    HashSetSource { data }
  }
}

impl<T> Source<T> for HashSetSource<T>
where
  T: Clone + Send + 'static,
{
  fn stream(&self) -> Pin<Box<dyn Stream<Item = T> + Send>> {
    // Clone to make sure data remains intact when streamed.
    let iter = self.data.clone().into_iter();
    let stream = stream::iter(iter);
    Box::pin(stream)
  }
}
