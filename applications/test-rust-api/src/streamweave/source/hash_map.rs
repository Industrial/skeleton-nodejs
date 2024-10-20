use crate::streamweave::source::Source;
use futures::{Stream, stream};
use std::collections::HashMap;
use std::pin::Pin;

// Streams key-value pairs from a HashMap.
pub struct HashMapSource<K, V> {
  data: HashMap<K, V>,
}

impl<K, V> HashMapSource<K, V> {
  pub fn new(data: HashMap<K, V>) -> Self {
    HashMapSource { data }
  }
}

impl<K, V> Source<(K, V)> for HashMapSource<K, V>
where
  K: Clone + Send + 'static,
  V: Clone + Send + 'static,
{
  fn stream(&self) -> Pin<Box<dyn Stream<Item = (K, V)> + Send>> {
    // Clone is necessary to ensure data integrity for each stream.
    let iter = self.data.clone().into_iter();
    let stream = stream::iter(iter);
    Box::pin(stream)
  }
}
