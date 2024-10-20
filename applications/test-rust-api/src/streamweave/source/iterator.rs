use crate::streamweave::source::Source;
use futures::{Stream, stream};
use std::iter::Iterator;
use std::pin::Pin;

// Wraps any Iterator to produce a stream.
pub struct IteratorSource<I>
where
  I: Iterator + Clone, {
  iter: I,
}

impl<I> IteratorSource<I>
where
  I: Iterator + Clone,
{
  pub fn new(iter: I) -> Self {
    IteratorSource { iter }
  }
}

impl<I> Source<I::Item> for IteratorSource<I>
where
  I: Iterator + Clone + Send + 'static,
  I::Item: Send + 'static,
{
  fn stream(&self) -> Pin<Box<dyn Stream<Item = I::Item> + Send>> {
    let stream = stream::iter(self.iter.clone());
    Box::pin(stream)
  }
}
