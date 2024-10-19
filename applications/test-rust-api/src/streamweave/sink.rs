pub mod http_response;

use futures::stream::Stream;
use std::pin::Pin;

pub trait Sink<I, O> {
  fn run(
    self,
    input: Pin<Box<dyn Stream<Item = I> + Send>>,
  ) -> O;
}
