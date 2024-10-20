use crate::streamweave::source::Source;
use futures::Stream;
use serde::de::DeserializeOwned;
use serde_json::Deserializer;
use std::pin::Pin;

// Parses a JSON string and streams deserialized values.
pub struct JsonStringSource<T> {
  json_data: String,
  _marker: std::marker::PhantomData<T>,
}

impl<T> JsonStringSource<T> {
  pub fn new(json_data: impl Into<String>) -> Self {
    JsonStringSource {
      json_data: json_data.into(),
      _marker: std::marker::PhantomData,
    }
  }
}

impl<T> Source<T> for JsonStringSource<T>
where
  T: DeserializeOwned + Send + 'static,
{
  fn stream(&self) -> Pin<Box<dyn Stream<Item = T> + Send>> {
    let data = self.json_data.clone();

    Box::pin(async_stream::stream! {
        let stream = Deserializer::from_str(&data).into_iter::<T>();
        for item in stream {
            match item {
                Ok(parsed_item) => yield parsed_item,
                Err(e) => {
                    eprintln!("Failed to parse JSON item: {}", e);
                }
            }
        }
    })
  }
}
