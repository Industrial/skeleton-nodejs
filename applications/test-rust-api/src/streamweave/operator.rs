pub mod map;

use futures::stream::Stream;
use std::pin::Pin;

pub trait Operator<I, O> {
  type Output: Stream<Item = O> + Send;
  fn apply(
    &self,
    input: Pin<Box<dyn Stream<Item = I> + Send>>,
  ) -> Self::Output;
}
