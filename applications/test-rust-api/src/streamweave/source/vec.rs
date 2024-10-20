use crate::streamweave::source::Source;
use futures::{Stream, stream};
use std::pin::Pin;

// Produces a stream from a Vec.

pub struct VecSource<I> {
  data: Vec<I>,
}

impl<I> VecSource<I> {
  pub fn new(data: Vec<I>) -> Self {
    VecSource { data }
  }
}

impl<I> Source<I> for VecSource<I>
where
  I: Clone + Send + 'static,
{
  fn stream(&self) -> Pin<Box<dyn Stream<Item = I> + Send>> {
    let stream = stream::iter(self.data.clone());
    Box::pin(stream)
  }
}
