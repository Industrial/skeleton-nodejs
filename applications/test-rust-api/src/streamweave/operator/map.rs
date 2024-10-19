use crate::streamweave::operator::Operator;
use futures::stream::{Stream, StreamExt};
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::Arc;

pub struct Map<F, I, O> {
  func: Arc<F>,
  _input: PhantomData<I>,
  _output: PhantomData<O>,
}

impl<F, I, O> Map<F, I, O>
where
  F: Fn(I) -> O + Send + Sync + 'static,
{
  pub fn new(func: F) -> Self {
    Map {
      func: Arc::new(func),
      _input: PhantomData,
      _output: PhantomData,
    }
  }
}

impl<F, I, O> Operator<I, O> for Map<F, I, O>
where
  F: Fn(I) -> O + Send + Sync + 'static,
  I: Send + 'static,
  O: Send + 'static,
{
  type Output = Pin<Box<dyn Stream<Item = O> + Send>>;

  fn apply(
    &self,
    input: Pin<Box<dyn Stream<Item = I> + Send>>,
  ) -> Self::Output {
    let func = Arc::clone(&self.func);
    let output_stream = input.map(move |item| (func)(item));
    Box::pin(output_stream)
  }
}
