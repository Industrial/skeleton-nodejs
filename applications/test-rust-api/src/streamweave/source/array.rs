use crate::streamweave::source::Source;
use futures::{stream, Stream};
use std::pin::Pin;

// Converts an array into a stream of its elements.
pub struct ArraySource<I, const N: usize> {
  data: [I; N],
}

impl<I, const N: usize> ArraySource<I, N> {
  pub fn new(data: [I; N]) -> Self {
    ArraySource { data }
  }
}

impl<I, const N: usize> Source<I> for ArraySource<I, N>
where
  I: Clone + Send + 'static,
{
  fn stream(&self) -> Pin<Box<dyn Stream<Item = I> + Send>> {
    let stream = stream::iter(self.data.clone());
    Box::pin(stream)
  }
}
