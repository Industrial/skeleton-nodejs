use crate::streamweave::source::Source;
use futures::{Stream, stream};
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

#[cfg(test)]
mod tests {
  use super::ArraySource;
  use crate::streamweave::source::Source;
  use futures::executor::block_on;
  use futures::stream::StreamExt;
  use proptest::prelude::*;

  proptest! {
      #[test]
      fn test_array_source_stream(ref input in proptest::collection::vec(any::<i32>(), 1..=100)) {
          let mut arr = [0; 100];
          arr[..input.len()].clone_from_slice(&input[..]);

          let source = ArraySource::new(arr);

          let mut stream = source.stream();
          let mut collected_items = Vec::new();

          block_on(async {
              while let Some(item) = stream.next().await {
                  collected_items.push(item);
              }
          });

          assert_eq!(&collected_items[..input.len()], &input[..]);
      }
  }
}
